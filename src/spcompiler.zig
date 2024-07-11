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

const DebugWriter = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) !void {
        return std.debug.print(fmt, args);
    }
};

//FIXME: make errors more explicit
const SpcError = error{ EmitError, VmError, GenericParseError, InvalidCharacter };

const Local = struct {
    name: Token,
    depth: i32,
};

const Compiler = struct {
    locals: std.ArrayList(Local),
    scope_depth: u32,
};

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

const ParseFn = *const fn (self: *CompilationContext, canAssign: bool) anyerror!void;

const Rule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

fn getRule(token: Token.Tag) Rule {
    return switch (token) {
        .left_paren => createRule(CompilationContext.grouping, null, .call),
        .right_paren => createRule(null, null, .none),
        .left_brace => createRule(null, null, .none),
        .right_brace => createRule(null, null, .none),
        .comma => createRule(null, null, .none),
        .dot => createRule(null, null, .none),
        .minus => createRule(CompilationContext.unary, CompilationContext.binary, .term),
        .plus => createRule(null, CompilationContext.binary, .term),
        .semicolon => createRule(null, null, .none),
        .slash => createRule(null, CompilationContext.binary, .factor),
        .star => createRule(null, CompilationContext.binary, .factor),
        .bang => createRule(CompilationContext.unary, null, .none),
        .bang_equal => createRule(null, CompilationContext.binary, .equality),
        .equal => createRule(null, null, .none),
        .equal_equal => createRule(null, CompilationContext.binary, .equality),
        .greater => createRule(null, CompilationContext.binary, .comparison),
        .greater_equal => createRule(null, CompilationContext.binary, .comparison),
        .less => createRule(null, CompilationContext.binary, .comparison),
        .less_equal => createRule(null, CompilationContext.binary, .comparison),
        .identifier => createRule(CompilationContext.variable, null, .none),
        .string => createRule(CompilationContext.string, null, .none),
        .number => createRule(CompilationContext.number, null, .none),
        .keyword_and => createRule(null, CompilationContext.andFn, .@"and"),
        .keyword_class => createRule(null, null, .none),
        .keyword_else => createRule(null, null, .none),
        .keyword_false => createRule(CompilationContext.literal, null, .none),
        .keyword_fun => createRule(null, null, .none),
        .keyword_for => createRule(null, null, .none),
        .keyword_if => createRule(null, null, .none),
        .keyword_nil => createRule(CompilationContext.literal, null, .none),
        .keyword_or => createRule(null, CompilationContext.orFn, .@"or"),
        .keyword_print => createRule(null, null, .none),
        .keyword_return => createRule(null, null, .none),
        .keyword_super => createRule(CompilationContext.super, null, .none),
        .keyword_this => createRule(CompilationContext.this, null, .none),
        .keyword_true => createRule(CompilationContext.literal, null, .none),
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

pub const Parser = struct {
    current: Token,
    previous: Token,
    tokenizer: Tokenizer,
    had_error: bool,
    panic_mode: bool,

    fn init(src: []const u8) Parser {
        return .{
            .tokenizer = Tokenizer.init(src),
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
        };
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
};

pub const CompilationContext = struct {
    vm: *VM,
    parser: Parser,
    current: Compiler,
    compiling_chunk: *Chunk,

    pub fn init(vm: *VM) CompilationContext {
        return .{
            .vm = vm,
            .parser = undefined,
            .compiling_chunk = undefined,
            .current = .{
                .scope_depth = 0,
                .locals = std.ArrayList(Local).init(vm.alloc),
            },
        };
    }

    pub fn deinit(self: *CompilationContext) void {
        self.current.locals.deinit();
    }

    pub fn compile(self: *CompilationContext, src: []const u8, chunk: *Chunk) !bool {
        self.parser = Parser.init(src);

        self.compiling_chunk = chunk;

        self.parser.advance();
        while (!self.parser.match(.eof)) {
            try self.declaration();
        }
        try self.endCompiler();
        return !self.parser.had_error;
    }

    fn declaration(self: *CompilationContext) SpcError!void {
        if (self.parser.match(.keyword_var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.parser.panic_mode) self.synchronize();
    }

    fn statement(self: *CompilationContext) SpcError!void {
        if (self.parser.match(.keyword_print)) {
            try self.printStatement();
        } else if (self.parser.match(.keyword_if)) {
            try self.ifStatement();
        } else if (self.parser.match(.keyword_while)) {
            try self.whileStatement();
        } else if (self.parser.match(.left_brace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn printStatement(self: *CompilationContext) SpcError!void {
        try self.expression();
        self.parser.consume(.semicolon, "Expect ';' after value.");
        try self.emitOp(.print);
    }

    fn whileStatement(self: *CompilationContext) SpcError!void {
        const loopStart: i32 = @intCast(self.currentChunk().bytes.items.len);
        self.parser.consume(.left_paren, "Expect '(' after 'while'.");
        try self.expression();
        self.parser.consume(.right_paren, "Expect ')' after condition.");

        const exitJump = try self.emitJump(.jump_if_false);
        try self.emitOp(.pop);
        try self.statement();
        try self.emitLoop(loopStart);

        self.patchJump(exitJump);
        try self.emitOp(.pop);
    }

    fn expressionStatement(self: *CompilationContext) SpcError!void {
        try self.expression();
        self.parser.consume(.semicolon, "Expect ';' after expression.");
        try self.emitOp(.pop);
    }

    fn ifStatement(self: *CompilationContext) SpcError!void {
        self.parser.consume(.left_paren, "Expect '(' after 'if'.");
        try self.expression();
        self.parser.consume(.right_paren, "Expect ')' after condition.");

        const thenJump = try self.emitJump(.jump_if_false);
        try self.emitOp(.pop);
        try self.statement();

        const elseJump = try self.emitJump(.jump);

        self.patchJump(thenJump);
        try self.emitOp(.pop);

        if (self.parser.match(.keyword_else)) {
            try self.statement();
        }
        self.patchJump(elseJump);
    }

    fn expression(self: *CompilationContext) SpcError!void {
        try self.parsePrecedence(.assignment);
    }

    fn block(self: *CompilationContext) SpcError!void {
        while (!self.parser.check(.right_brace) and !self.parser.check(.eof)) {
            try self.declaration();
        }

        self.parser.consume(.right_brace, "Expect '}' after block");
    }

    fn varDeclaration(self: *CompilationContext) SpcError!void {
        const global = try self.parseVariable("Expect variable name.");

        if (self.parser.match(.equal)) {
            try self.expression();
        } else {
            try self.emitOp(.nil);
        }

        self.parser.consume(.semicolon, "Expect ';' after variable declartion.");

        try self.defineVariable(global);
    }

    fn parsePrecedence(self: *CompilationContext, precedence: Precedence) SpcError!void {
        self.parser.advance();
        const prefix = getRule(self.parser.previous.tag).prefix orelse {
            self.parser.errorAtPrevious("Expect expression.");
            return;
        };
        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);

        prefix(self, canAssign) catch {
            return SpcError.GenericParseError;
        };

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.parser.current.tag).precedence)) {
            self.parser.advance();
            const infix = getRule(self.parser.previous.tag).infix;
            if (infix) |ifx| {
                ifx(self, canAssign) catch {
                    return SpcError.GenericParseError;
                };
            } else {
                safeUnreachable(@src());
            }
        }

        if (canAssign and self.parser.match(.equal)) {
            self.parser.errorAtPrevious("Invalid assignment target.");
        }
    }

    fn identifierConstant(self: *CompilationContext, token: Token) SpcError!u8 {
        const str = Object.String.copy(self.vm, token.lexeme) catch {
            return SpcError.VmError;
        };
        return self.makeConstant(str.obj.asValue());
    }

    fn identifierEqual(a: Token, b: Token) bool {
        return std.mem.eql(u8, a.lexeme, b.lexeme);
    }

    fn resolveLocal(self: *CompilationContext, compiler: Compiler, name: Token) ?u8 {
        if (compiler.locals.items.len == 0) return null;

        var i: isize = @intCast(compiler.locals.items.len - 1);
        while (i >= 0) : (i -= 1) {
            const local = &compiler.locals.items[@intCast(i)];
            if (identifierEqual(name, local.name)) {
                if (local.depth == -1) {
                    self.parser.errorAtPrevious("Can't read local variable in its own initializer.");
                }
                return @intCast(i);
            }
        }

        return null;
    }

    fn addLocal(self: *CompilationContext, token: Token) !void {
        if (self.current.locals.items.len == std.math.maxInt(u8)) {
            self.parser.errorAtPrevious("Too many local variables in function.");
            return;
        }

        const local: Local = .{ .name = token, .depth = -1 };
        self.current.locals.append(local) catch return SpcError.VmError;
    }

    fn declareVariable(self: *CompilationContext) SpcError!void {
        if (self.current.scope_depth == 0) return;

        const name = self.parser.previous;

        var i = @as(isize, @intCast(self.current.locals.items.len)) - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.current.locals.items[@intCast(i)];
            if (local.depth != 1 and local.depth < self.current.scope_depth) {
                break;
            }

            if (identifierEqual(name, local.name)) {
                self.parser.errorAtPrevious("Already a variable with this name in this scope.");
            }
        }

        try self.addLocal(name);
    }

    fn parseVariable(self: *CompilationContext, error_message: []const u8) !u8 {
        self.parser.consume(.identifier, error_message);

        try self.declareVariable();
        if (self.current.scope_depth > 0) return 0;

        return try self.identifierConstant(self.parser.previous);
    }

    fn markInitialized(self: *CompilationContext) void {
        const idx: usize = @intCast(self.current.locals.items.len - 1);
        self.current.locals.items[idx].depth = @intCast(self.current.scope_depth);
    }

    fn defineVariable(self: *CompilationContext, global: u8) SpcError!void {
        if (self.current.scope_depth > 0) {
            self.markInitialized();
            return;
        }

        try self.emitOp(.define_global);
        try self.emitByte(global);
    }

    fn variable(self: *CompilationContext, canAssign: bool) SpcError!void {
        try self.namedVariable(self.parser.previous, canAssign);
    }

    fn string(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = canAssign;
        const str = Object.String.copy(self.vm, self.parser.previous.lexeme) catch {
            return SpcError.VmError;
        };
        try self.emitConstant(str.obj.asValue());
    }

    fn namedVariable(self: *CompilationContext, token: Token, canAssign: bool) SpcError!void {
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;
        var arg: u8 = undefined;

        if (self.resolveLocal(self.current, token)) |a| {
            get_op = .get_local;
            set_op = .set_local;
            arg = a;
        } else {
            arg = try self.identifierConstant(token);
            get_op = .get_global;
            set_op = .set_global;
        }

        if (canAssign and self.parser.match(.equal)) {
            try self.expression();
            try self.emitOp(set_op);
            try self.emitByte(arg);
        } else {
            try self.emitOp(get_op);
            try self.emitByte(arg);
        }
    }

    fn andFn(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = canAssign;
        const endJump = try self.emitJump(.jump_if_false);
        try self.emitOp(.pop);
        try self.parsePrecedence(.@"and");

        self.patchJump(endJump);
    }

    fn orFn(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = canAssign;
        const elseJump = try self.emitJump(.jump_if_false);
        const endJump = try self.emitJump(.jump);

        self.patchJump(elseJump);
        try self.emitOp(.pop);

        try self.parsePrecedence(.@"or");
        self.patchJump(endJump);
    }

    fn literal(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = canAssign;
        switch (self.parser.previous.tag) {
            .keyword_false => try self.emitOp(.op_false),
            .keyword_true => try self.emitOp(.op_true),
            .keyword_nil => try self.emitOp(.nil),
            else => safeUnreachable(@src()),
        }
    }

    fn super(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = self;
        _ = canAssign;
    }

    fn this(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = self;
        _ = canAssign;
    }

    fn grouping(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = canAssign;
        try self.expression();
        self.parser.consume(.right_paren, "Expected ')' after expression.");
    }

    fn binary(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = canAssign;
        const operator_tag = self.parser.previous.tag;
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

    fn unary(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = canAssign;
        const operator_tag = self.parser.previous.tag;

        try self.parsePrecedence(.unary);

        switch (operator_tag) {
            .minus => try self.emitOp(.negate),
            .bang => try self.emitOp(.not),
            else => safeUnreachable(@src()),
        }
    }

    fn number(self: *CompilationContext, canAssign: bool) SpcError!void {
        _ = canAssign;
        const value = try std.fmt.parseFloat(f64, self.parser.previous.lexeme);
        try self.emitConstant(Value{ .number = value });
    }

    fn synchronize(self: *CompilationContext) void {
        self.parser.panic_mode = false;
        while (self.parser.current.tag != .eof) {
            if (self.parser.previous.tag == .semicolon) return;
            switch (self.parser.current.tag) {
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

            self.parser.advance();
        }
    }

    fn makeConstant(self: *CompilationContext, val: Value) SpcError!u8 {
        const constant = self.currentChunk().addConstant(val) catch {
            return SpcError.VmError;
        };
        if (constant > std.math.maxInt(Chunk.ConstantIndex)) {
            self.parser.errorAtPrevious("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(constant);
    }

    fn emitConstant(self: *CompilationContext, val: Value) SpcError!void {
        self.emitOp(.constant) catch {
            return SpcError.EmitError;
        };
        self.emitByte(try self.makeConstant(val)) catch {
            return SpcError.EmitError;
        };
    }

    fn patchJump(self: *CompilationContext, offset: i32) void {
        const jump: i32 = @as(i32, @intCast(self.currentChunk().bytes.items.len)) - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            self.parser.errorAtPrevious("Too much code to jump over.");
        }

        const j16 = @as(u16, @truncate(@as(u32, @intCast(jump))));

        const bytes = std.mem.toBytes(j16);

        self.currentChunk().bytes.items[@intCast(offset)] = bytes[0];
        self.currentChunk().bytes.items[@intCast(offset + 1)] = bytes[1];
    }

    fn emitByte(self: *CompilationContext, byte: u8) SpcError!void {
        self.currentChunk().write(u8, byte, self.parser.previous.line) catch {
            return SpcError.EmitError;
        };
    }

    fn emitLoop(self: *CompilationContext, loopStart: i32) SpcError!void {
        try self.emitOp(.loop);

        const offset = @as(i32, @intCast(self.currentChunk().bytes.items.len)) - loopStart + 2;
        if (offset > std.math.maxInt(u16)) {
            self.parser.errorAtPrevious("Loop body too large:");
        }

        const l16 = @as(u16, @truncate(@as(u32, @intCast(offset))));
        const bytes = std.mem.toBytes(l16);

        try self.emitByte(bytes[0]);
        try self.emitByte(bytes[1]);
    }

    fn emitJump(self: *CompilationContext, opcode: OpCode) SpcError!i32 {
        try self.emitOp(opcode);
        try self.emitByte(0xff);
        try self.emitByte(0xff);
        return @intCast(self.currentChunk().bytes.items.len - 2);
    }

    fn emitOp(self: *CompilationContext, opcode: OpCode) SpcError!void {
        self.currentChunk().writeOp(opcode, self.parser.previous.line) catch {
            return SpcError.EmitError;
        };
    }

    fn emitOps(self: *CompilationContext, op1: OpCode, op2: OpCode) SpcError!void {
        self.currentChunk().writeOp(op1, self.parser.previous.line) catch {
            return SpcError.EmitError;
        };
        self.currentChunk().writeOp(op2, self.parser.previous.line) catch {
            return SpcError.EmitError;
        };
    }

    fn emitReturn(self: *CompilationContext) SpcError!void {
        self.emitOp(.ret) catch {
            return SpcError.EmitError;
        };
    }

    fn endCompiler(self: *CompilationContext) SpcError!void {
        try self.emitReturn();

        if (Config.debug_print_code) {
            if (!self.parser.had_error) {
                try self.currentChunk().disassemble(DebugWriter, "debug");
            }
        }
    }

    fn beginScope(self: *CompilationContext) void {
        self.current.scope_depth += 1;
    }

    fn endScope(self: *CompilationContext) SpcError!void {
        self.current.scope_depth -= 1;

        while (self.current.locals.items.len > 0 and
            self.current.locals.items[self.current.locals.items.len - 1].depth > self.current.scope_depth)
        {
            self.emitOp(.pop) catch {
                return SpcError.EmitError;
            };

            _ = self.current.locals.pop();
        }
    }

    fn currentChunk(self: *CompilationContext) *Chunk {
        return self.compiling_chunk;
    }
};
