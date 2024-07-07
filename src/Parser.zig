const std = @import("std");
const Tokenizer = @import("tokenizer.zig");
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("Value.zig").Value;
const Token = @import("Token.zig").Token;
const OpCode = @import("opcode.zig").OpCode;
const Config = @import("config.zig");
const Object = @import("Object.zig");
const VM = @import("VM.zig").VM;
const safeUnreachable = @import("util.zig").safeUnreachable;
const Parser = @This();

const DebugWriter = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) !void {
        return std.debug.print(fmt, args);
    }
};

current: Token,
previous: Token,
tokenizer: Tokenizer,
had_error: bool,
panic_mode: bool,
compiling_chunk: *Chunk,
vm: *VM,

const Precedence = enum(u8) {
    none,
    assignment,
    @"or",
    @"and",
    equality,
    comparison,
    term,
    factor,
    unary,
    call,
    primary,

    fn next(self: Precedence) Precedence {
        return @enumFromInt(@intFromEnum(self) + 1);
    }
};

const ParseFn = *const fn (self: *Parser, canAssign: bool) anyerror!void;

const Rule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

fn getRule(token: Token.Tag) Rule {
    return switch (token) {
        .left_paren => createRule(grouping, null, .call),
        .right_paren => createRule(null, null, .none),
        .left_brace => createRule(null, null, .none),
        .right_brace => createRule(null, null, .none),
        .comma => createRule(null, null, .none),
        .dot => createRule(null, null, .none),
        .minus => createRule(unary, binary, .term),
        .plus => createRule(null, binary, .term),
        .semicolon => createRule(null, null, .none),
        .slash => createRule(null, binary, .factor),
        .star => createRule(null, binary, .factor),
        .bang => createRule(unary, null, .none),
        .bang_equal => createRule(null, binary, .equality),
        .equal => createRule(null, null, .none),
        .equal_equal => createRule(null, binary, .equality),
        .greater => createRule(null, binary, .comparison),
        .greater_equal => createRule(null, binary, .comparison),
        .less => createRule(null, binary, .comparison),
        .less_equal => createRule(null, binary, .comparison),
        .identifier => createRule(variable, null, .none),
        .string => createRule(string, null, .none),
        .number => createRule(number, null, .none),
        .keyword_and => createRule(null, andFn, .@"and"),
        .keyword_class => createRule(null, null, .none),
        .keyword_else => createRule(null, null, .none),
        .keyword_false => createRule(literal, null, .none),
        .keyword_fun => createRule(null, null, .none),
        .keyword_for => createRule(null, null, .none),
        .keyword_if => createRule(null, null, .none),
        .keyword_nil => createRule(literal, null, .none),
        .keyword_or => createRule(null, orFn, .@"or"),
        .keyword_print => createRule(null, null, .none),
        .keyword_return => createRule(null, null, .none),
        .keyword_super => createRule(super, null, .none),
        .keyword_this => createRule(this, null, .none),
        .keyword_true => createRule(literal, null, .none),
        .keyword_var => createRule(null, null, .none),
        .keyword_while => createRule(null, null, .none),
        .invalid => createRule(null, null, .none),
        .eof => createRule(null, null, .none),
    };
}

fn createRule(prefix: ?ParseFn, infix: ?ParseFn, precedence: Precedence) Rule {
    return Rule{
        .prefix = prefix,
        .infix = infix,
        .precedence = precedence,
    };
}

pub fn compile(self: *Parser, vm: *VM, src: []const u8, chunk: *Chunk) !bool {
    self.tokenizer = Tokenizer.init(src);
    self.vm = vm;
    self.had_error = false;
    self.panic_mode = false;

    self.compiling_chunk = chunk;

    self.advance();
    while (!self.match(.eof)) {
        try self.declaration();
    }
    try self.endCompiler();
    return !self.had_error;
}

fn advance(self: *Parser) void {
    self.previous = self.current;

    while (true) {
        self.current = self.tokenizer.next();
        if (self.current.tag != .invalid) break;

        self.errorAtCurrent(self.current.lexeme);
    }
}

fn consume(self: *Parser, tag: Token.Tag, message: []const u8) void {
    if (self.current.tag == tag) {
        self.advance();
        return;
    }
    self.errorAtCurrent(message);
}

fn match(self: *Parser, tag: Token.Tag) bool {
    if (!self.check(tag)) return false;
    self.advance();
    return true;
}

fn check(self: *Parser, tag: Token.Tag) bool {
    return self.current.tag == tag;
}

fn declaration(self: *Parser) !void {
    if (self.match(.keyword_var)) {
        try self.varDeclaration();
    } else {
        try self.statement();
    }

    if (self.panic_mode) self.synchronize();
}

fn statement(self: *Parser) !void {
    if (self.match(.keyword_print)) {
        try self.printStatement();
    } else {
        try self.expressionStatement();
    }
}

fn printStatement(self: *Parser) !void {
    try self.expression();
    self.consume(.semicolon, "Expect ';' after value.");
    try self.emitOp(.print);
}

fn expressionStatement(self: *Parser) !void {
    try self.expression();
    self.consume(.semicolon, "Expect ';' after expression.");
    try self.emitOp(.pop);
}

fn expression(self: *Parser) !void {
    try self.parsePrecedence(.assignment);
}

fn varDeclaration(self: *Parser) !void {
    const global = try self.parseVariable("Expect variable name.");

    if (self.match(.equal)) {
        try self.expression();
    } else {
        try self.emitOp(.nil);
    }

    self.consume(.semicolon, "Expect ';' after variable declartion.");

    try self.defineVariable(global);
}

fn parsePrecedence(self: *Parser, precedence: Precedence) !void {
    self.advance();
    const prefix = getRule(self.previous.tag).prefix orelse {
        self.errorAtPrevious("Expect expression.");
        return;
    };
    const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);

    try prefix(self, canAssign);

    while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.tag).precedence)) {
        self.advance();
        const infix = getRule(self.previous.tag).infix;
        if (infix) |ifx| {
            try ifx(self, canAssign);
        } else {
            safeUnreachable(@src());
        }
    }

    if (canAssign and self.match(.equal)) {
        self.errorAtPrevious("Invalid assignment target.");
    }
}

fn identifierConstant(self: *Parser, token: Token) !u8 {
    const str = try Object.String.copy(self.vm, token.lexeme);
    return self.makeConstant(str.obj.asValue());
}

fn parseVariable(self: *Parser, error_message: []const u8) !u8 {
    self.consume(.identifier, error_message);
    return try self.identifierConstant(self.previous);
}

fn defineVariable(self: *Parser, global: u8) !void {
    try self.emitOp(.define_global);
    try self.emitByte(global);
}

fn variable(self: *Parser, canAssign: bool) !void {
    try self.namedVariable(self.previous, canAssign);
}

fn string(self: *Parser, canAssign: bool) !void {
    _ = canAssign;
    const str = try Object.String.copy(self.vm, self.previous.lexeme);
    try self.emitConstant(str.obj.asValue());
}

fn namedVariable(self: *Parser, token: Token, canAssign: bool) !void {
    const arg = try self.identifierConstant(token);

    if (canAssign and self.match(.equal)) {
        try self.expression();
        try self.emitOp(.set_global);
        try self.emitByte(arg);
    } else {
        try self.emitOp(.get_global);
        try self.emitByte(arg);
    }
}

fn andFn(self: *Parser, canAssign: bool) !void {
    _ = self;
    _ = canAssign;
}

fn orFn(self: *Parser, canAssign: bool) !void {
    _ = self;
    _ = canAssign;
}

fn literal(self: *Parser, canAssign: bool) !void {
    _ = canAssign;
    switch (self.previous.tag) {
        .keyword_false => try self.emitOp(.op_false),
        .keyword_true => try self.emitOp(.op_true),
        .keyword_nil => try self.emitOp(.nil),
        else => safeUnreachable(@src()),
    }
}

fn super(self: *Parser, canAssign: bool) !void {
    _ = self;
    _ = canAssign;
}

fn this(self: *Parser, canAssign: bool) !void {
    _ = self;
    _ = canAssign;
}

fn grouping(self: *Parser, canAssign: bool) !void {
    _ = canAssign;
    try self.expression();
    self.consume(.right_paren, "Expected ')' after expression.");
}

fn binary(self: *Parser, canAssign: bool) !void {
    _ = canAssign;
    const operator_tag = self.previous.tag;
    const rule = getRule(operator_tag);
    try self.parsePrecedence(rule.precedence.next());

    switch (operator_tag) {
        .bang_equal => try self.emitOps(.equal, .not),
        .equal_equal => try self.emitOp(.equal),
        .greater => try self.emitOp(.greater),
        .greater_equal => try self.emitOps(.less, .not),
        .less => try self.emitOp(.less),
        .less_equal => try self.emitOps(.greater, .not),
        .plus => try self.emitOp(.add),
        .minus => try self.emitOp(.subtract),
        .star => try self.emitOp(.multiply),
        .slash => try self.emitOp(.divide),
        else => safeUnreachable(@src()),
    }
}

fn unary(self: *Parser, canAssign: bool) !void {
    _ = canAssign;
    const operator_tag = self.previous.tag;

    try self.parsePrecedence(.unary);

    switch (operator_tag) {
        .minus => try self.emitOp(.negate),
        .bang => try self.emitOp(.not),
        else => safeUnreachable(@src()),
    }
}

fn number(self: *Parser, canAssign: bool) !void {
    _ = canAssign;
    const value = try std.fmt.parseFloat(f64, self.previous.lexeme);
    try self.emitConstant(Value{ .number = value });
}

fn synchronize(self: *Parser) void {
    self.panic_mode = false;
    while (self.current.tag != .eof) {
        if (self.previous.tag == .semicolon) return;
        switch (self.current.tag) {
            .keyword_class,
            .keyword_fun,
            .keyword_var,
            .keyword_for,
            .keyword_if,
            .keyword_while,
            .keyword_print,
            .keyword_return,
            => return,

            else => {},
        }

        self.advance();
    }
}

fn errorAtCurrent(self: *Parser, message: []const u8) void {
    self.errorAt(self.current, message);
}

fn errorAtPrevious(self: *Parser, message: []const u8) void {
    self.errorAt(self.previous, message);
}

fn errorAt(self: *Parser, token: Token, message: []const u8) void {
    if (self.panic_mode) return;
    self.panic_mode = false;
    std.debug.print("[line {}] Error", .{token.line});

    if (token.tag == .eof) {
        std.debug.print("  at end", .{});
    } else if (token.tag == .invalid) {} else {
        std.debug.print(" at {s}", .{token.lexeme});
    }

    std.debug.print(": {s}\n", .{message});
    self.had_error = true;
}

fn makeConstant(self: *Parser, val: Value) !u8 {
    const constant = try self.currentChunk().addConstant(val);
    if (constant > std.math.maxInt(Chunk.ConstantIndex)) {
        self.errorAtPrevious("Too many constants in one chunk.");
        return 0;
    }

    return @intCast(constant);
}

fn emitConstant(self: *Parser, val: Value) !void {
    try self.emitOp(.constant);
    try self.emitByte(try self.makeConstant(val));
}

fn emitByte(self: *Parser, byte: u8) !void {
    try self.currentChunk().write(u8, byte, self.previous.line);
}

fn emitBytes(self: *Parser, byte1: u8, byte2: u8) void {
    self.emitByte(byte1);
    self.emitByte(byte2);
}

fn emitOp(self: *Parser, opcode: OpCode) !void {
    try self.currentChunk().writeOp(opcode, self.previous.line);
}

fn emitOps(self: *Parser, op1: OpCode, op2: OpCode) !void {
    try self.currentChunk().writeOp(op1, self.previous.line);
    try self.currentChunk().writeOp(op2, self.previous.line);
}

fn emitReturn(self: *Parser) !void {
    try self.emitOp(.ret);
}

fn endCompiler(self: *Parser) !void {
    try self.emitReturn();

    if (Config.debug_print_code) {
        if (!self.had_error) {
            try self.currentChunk().disassemble(DebugWriter, "debug");
        }
    }
}

fn currentChunk(self: *Parser) *Chunk {
    return self.compiling_chunk;
}
