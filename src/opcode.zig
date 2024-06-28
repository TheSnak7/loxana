pub const OpCode = enum(u8) {
    ret,
    constant,
    negate,
    add,
    subtract,
    multiply,
    divide,
    constant_long,

    pub fn instructionLen(self: OpCode) u8 {
        return switch (self) {
            .ret, .negate, .add, .subtract, .multiply, .divide => 1,
            .constant => 2,
            .constant_long => 4,
        };
    }
};
