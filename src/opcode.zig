pub const OpCode = enum(u8) {
    ret,
    jump,
    jump_if_false,
    loop,
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
    print,
    pop,
    get_local,
    get_global,
    define_global,
    set_local,
    set_global,

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
            .print,
            .pop,
            => 1,
            .define_global,
            .constant,
            .get_global,
            .set_global,
            .set_local,
            .get_local,
            => 2,
            .jump,
            .jump_if_false,
            .loop,
            => 3,
            .constant_long,
            => 4,
        };
    }
};
