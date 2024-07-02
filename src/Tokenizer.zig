const std = @import("std");
const Token = @import("Token.zig").Token;
const Self = @This();

//Convert to pointers

text: []const u8,
current: usize,
line: u32,

pub fn init(src: []const u8) Self {
    return .{
        .text = src,
        .current = 0,
        .line = 1,
    };
}

pub fn next(self: *Self) Token {
    self.skipWhitespace();
    self.text = self.text[self.current..];
    self.current = 0;

    if (self.isAtEnd()) return self.makeToken(.eof);

    const c = self.text[self.current];
    self.advance();

    const token = switch (c) {
        '(' => self.makeToken(.left_paren),
        ')' => self.makeToken(.right_paren),
        '{' => self.makeToken(.left_brace),
        '}' => self.makeToken(.right_brace),
        ';' => self.makeToken(.semicolon),
        ',' => self.makeToken(.comma),
        '.' => self.makeToken(.dot),
        '-' => self.makeToken(.minus),
        '+' => self.makeToken(.plus),
        '/' => self.makeToken(.slash),
        '*' => self.makeToken(.star),
        '!' => self.makeToken(if (self.match('=')) Token.Tag.bang_equal else Token.Tag.bang),
        '=' => self.makeToken(if (self.match('=')) Token.Tag.equal_equal else Token.Tag.equal),
        '<' => self.makeToken(if (self.match('=')) Token.Tag.less_equal else Token.Tag.less),
        '>' => self.makeToken(if (self.match('=')) Token.Tag.greater_equal else Token.Tag.greater),
        '"' => self.string(),
        else => if (std.ascii.isAlphabetic(c)) self.identifier() else (if (std.ascii.isDigit(c)) self.number() else (if (c == 0) self.makeToken(.eof) else self.errorToken("Unexpected character"))),
    };
    //std.debug.print("Emitted {any} :: \"{s}\" left\n", .{ token, self.text[self.current..] });
    return token;
}

fn isAtEnd(self: *Self) bool {
    return self.current == self.text.len;
}

fn advance(self: *Self) void {
    self.current += 1;
}

fn peek(self: *Self) u8 {
    if (self.isAtEnd()) return 0;
    return self.text[self.current];
}

fn peekNext(self: *Self) u8 {
    if (self.current + 1 >= self.text.len) return 0;
    return self.text[self.current + 1];
}

fn match(self: *Self, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.text[self.current] != expected) return false;
    self.current += 1;
    return true;
}

fn skipWhitespace(self: *Self) void {
    while (true) {
        switch (self.peek()) {
            ' ', '\r', '\t' => _ = self.advance(),
            '\n' => {
                self.line += 1;
                self.advance();
            },
            '/' => {
                if (self.peekNext() == '/') {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        self.advance();
                    }
                } else {
                    return;
                }
            },
            else => return,
        }
    }
}

fn string(self: *Self) Token {
    while (self.peek() != '"' and !self.isAtEnd()) {
        if (self.peek() == '\n') self.line += 1;
        self.advance();
    }

    if (self.isAtEnd()) return self.errorToken("Unterminated string");

    self.advance();
    return self.makeToken(.string);
}

fn number(self: *Self) Token {
    while (std.ascii.isDigit(self.peek())) {
        self.advance();
    }

    if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
        self.advance();

        while (std.ascii.isDigit(self.peek())) self.advance();
    }
    return self.makeToken(.number);
}

fn identifier(self: *Self) Token {
    while (std.ascii.isAlphabetic(self.peek()) or std.ascii.isDigit(self.peek())) {
        self.advance();
    }
    const lexeme = self.text[0..(self.current)];
    const tag = Token.getKeyword(lexeme);
    if (tag) |t| {
        return self.makeToken(t);
    }
    return self.makeToken(.identifier);
}

fn checkKeyword(self: *Self, offset: usize, rest: []const u8, token_type: Token.Tag) Token.Tag {
    const word = self.text[offset..self.current];
    return if (std.mem.eql(u8, rest, word)) token_type else .identifier;
}

fn makeToken(self: *Self, tag: Token.Tag) Token {
    return .{
        .tag = tag,
        .lexeme = self.text[0..(self.current)],
        .line = @intCast(self.line),
    };
}

fn errorToken(self: *Self, message: []const u8) Token {
    return .{
        .tag = .invalid,
        .lexeme = message,
        .line = self.line,
    };
}

fn createTestToken(name: []const u8, token_type: Token.Tag) Token {
    return .{ .tag = token_type, .line = 1, .name = name };
}

pub fn expectEqualTokens(expected: []const Token, actual: []const Token) !void {
    const t_alloc = std.testing.allocator;
    var expected_list = std.ArrayList(u8).init(t_alloc);
    defer expected_list.deinit();
    var actual_list = std.ArrayList(u8).init(t_alloc);
    defer actual_list.deinit();

    for (expected) |e| {
        try e.debugFormat("", .{}, expected_list.writer());
    }

    for (actual) |a| {
        try a.debugFormat("", .{}, actual_list.writer());
    }

    _ = try std.testing.expectEqualStrings(expected_list.items, actual_list.items);
}

fn parseTestTokens(alloc: std.mem.Allocator, src: []const u8) !std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(alloc);
    errdefer tokens.deinit();
    var tokenizer = Self.init(src);

    while (true) {
        const token = tokenizer.next();
        try tokens.append(token);
        if (token.tag == .eof) {
            break;
        }
    }
    return tokens;
}

test "plus token" {
    var tokenizer = Self.init("+");
    const correct_token = createTestToken("+", .plus);

    const token = tokenizer.next();

    _ = try std.testing.expectEqualDeep(correct_token, token);
}

test "eof token" {
    var tokenizer = Self.init("");
    const correct_token = createTestToken("", .eof);

    const token = tokenizer.next();

    _ = try std.testing.expectEqualDeep(correct_token, token);
}

test "1 1" {
    var tokens = try parseTestTokens(std.testing.allocator, "1 1");
    defer tokens.deinit();

    var correct_tokens: [3]Token = .{
        createTestToken("1", .number),
        createTestToken("1", .number),
        createTestToken("", .eof),
    };

    _ = try expectEqualTokens(&correct_tokens, tokens.items);
}

test "5 * 6 + 4" {
    var tokens = try parseTestTokens(std.testing.allocator, "5 * 6 + 4");
    defer tokens.deinit();

    var correct_tokens = [_]Token{
        createTestToken("5", .number),
        createTestToken("*", .star),
        createTestToken("6", .number),
        createTestToken("+", .plus),
        createTestToken("4", .number),
        createTestToken("", .eof),
    };

    _ = try expectEqualTokens(&correct_tokens, tokens.items);
}

test "\"testString\"" {
    var tokens = try parseTestTokens(std.testing.allocator, "\"testString\"");
    defer tokens.deinit();

    var correct_tokens = [_]Token{
        createTestToken("\"testString\"", .string),
        createTestToken("", .eof),
    };

    _ = try expectEqualTokens(&correct_tokens, tokens.items);
}
