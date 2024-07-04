pub const OpCode = enum(u8) {
    ret,
    constant,
    negate,
    add,
    subtract,
    multiply,
    divide,
    constant_long,
    nil,
    op_true,
    op_false,
    not,
    equal,
    greater,
    less,

    pub fn instructionLen(self: OpCode) u8 {
        return switch (self) {
            .ret,
            .negate,
            .add,
            .subtract,
            .multiply,
            .divide,
            .nil,
            .op_true,
            .op_false,
            .not,
            .equal,
            .greater,
            .less,
            => 1,
            .constant => 2,
            .constant_long => 4,
        };
    }
};
