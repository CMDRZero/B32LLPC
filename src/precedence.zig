const std = @import("std");

const ast = @import("ast.zig");
const MutSLIR = @import("slir_construction.zig").MutSLIR;
const slir = @import("slir.zig");
const SLIR = slir.SLIR;
const Guid = SLIR.Guid;
const Reference = SLIR.Reference;
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const type_indexed_array = @import("type_indexed_array.zig");
const TypeIndexArrayPointer = type_indexed_array.TypeIndexArrayPointer;
const TypeIndexArray = type_indexed_array.TypeIndexArray;

pub const PrecClass = struct {
    group: Group,
    assoc: Assoc = .left,

    const Assoc = enum {
        left,
        none,
    };

    const Group = enum {
        const num_distinct = std.meta.fields(Group).len;

        arithmetic_product_chainable,
        arithmetic_product_nonchainable,
        arithmetic_sum,

        bitwise_shift,
        bitwise_product,
        bitwise_xor,
        bitwise_sum,

        comparison,

        logical_product,
        logical_sum,

        coercion,

        root,

        /// For this to work correctly we require that all subgroups are strictly adjacent
        /// Major groups are defined by the first text before an underscore
        const Major: type = b: {
            var major_names: []const []const u8 = &.{};
            for (std.meta.fieldNames(Group)) |fieldname| {
                var major_name: []const u8 = fieldname[0..];
                for (fieldname, 0..) |c, i| {
                    if (c == '_') {
                        major_name = fieldname[0..i];
                        break;
                    }
                }
                if (major_names.len == 0 or !std.mem.eql(u8, major_name, major_names[major_names.len - 1])) {
                    major_names = major_names ++ &[1][]const u8{major_name};
                }
            }

            const IntType = std.math.IntFittingRange(0, major_names.len - 1);
            var values: [major_names.len]IntType = undefined;
            for (0..major_names.len) |i| values[i] = i;

            break :b @Enum(
                IntType,
                .exhaustive,
                major_names,
                &values,
            );
        };
    };

    pub const start: PrecClass = .{ .group = .root };

    pub const Rel = enum(i2) {
        lt = -1,
        eq = 0,
        gt = 1,

        fn inverted(self: Rel) Rel {
            return @enumFromInt(-@intFromEnum(self));
        }
    };

    const PrecedenceArray = [Group.num_distinct][Group.num_distinct]?Rel;

    fn setMajorRelation(prec_array: *PrecedenceArray, major_a: Group.Major, rel: Rel, major_b: Group.Major) void {
        for (std.meta.fieldNames(Group), 0..) |fieldname_i, i| {
            if (!std.mem.startsWith(u8, fieldname_i, @tagName(major_a))) continue;

            for (std.meta.fieldNames(Group), 0..) |fieldname_j, j| {
                if (!std.mem.startsWith(u8, fieldname_j, @tagName(major_b))) continue;

                prec_array.*[i][j] = rel;
            }
        }
    }

    fn setMinorRelation(prec_array: *PrecedenceArray, minor_a: Group, rel: Rel, minor_b: Group) void {
        prec_array.*[@intFromEnum(minor_a)][@intFromEnum(minor_b)] = rel;
    }

    /// Arithmetic_* > Coercion
    /// Bitwise_* > Coercion
    /// Coercion > Comparison > Logical_* > Root
    ///
    /// *_Product > *_Sum
    /// Bitwise_Shift > Bitwise_Product
    ///
    /// order[y][x] --> y cmp x
    const precedence_ordering = b: {
        @setEvalBranchQuota(20_000);
        const major_greater_than_relations = [_][2]Group.Major{
            .{ .arithmetic, .coercion },
            .{ .bitwise, .coercion },
            .{ .coercion, .comparison },
            .{ .comparison, .logical },
            .{ .logical, .root },
        };

        var order: PrecedenceArray = @splat(@splat(null));

        for (major_greater_than_relations) |relation| {
            setMajorRelation(&order, relation[0], .gt, relation[1]);
        }

        setMinorRelation(&order, .arithmetic_product_chainable, .eq, .arithmetic_product_nonchainable);
        setMinorRelation(&order, .arithmetic_product_nonchainable, .gt, .arithmetic_sum);
        setMinorRelation(&order, .arithmetic_product_chainable, .gt, .arithmetic_sum);
        //We have to add both >'s here since the Transative Order computer doesnt know that a == b > c --> a > c

        setMinorRelation(&order, .bitwise_shift, .gt, .bitwise_product);
        setMinorRelation(&order, .bitwise_product, .gt, .bitwise_sum);

        setMinorRelation(&order, .logical_product, .gt, .logical_sum);

        computeTransativeOrdering(&order);
        break :b order;
    };

    pub fn cmp(lhs: PrecClass, rhs: PrecClass) ?Rel {
        return PrecClass.precedence_ordering[@intFromEnum(lhs.group)][@intFromEnum(rhs.group)];
    }

    fn computeTransativeOrdering(order: *PrecedenceArray) void {
        for (0..Group.num_distinct) |i| for (0..Group.num_distinct) |j| for (0..Group.num_distinct) |k| {
            if (order[i][k] == order[k][j] and order[i][k] != null) order[i][j] = order[i][k];
        };
        for (0..Group.num_distinct) |i| for (0..Group.num_distinct) |j| {
            if (i == j) order[i][j] = .eq;
            if (order[i][j] == null and order[j][i] != null) order[i][j] = order[j][i].?.inverted();
        };
    }
};

const precedence_group: TypeIndexArray(tokenizer.Operator.Tag, PrecClass) = b: {
    var groups: TypeIndexArray(tokenizer.Operator.Tag, PrecClass) = undefined;
    groups.set(.@"+", .{ .group = .arithmetic_sum });
    groups.set(.@"-", .{ .group = .arithmetic_sum });
    groups.set(.@"*", .{ .group = .arithmetic_product_chainable });
    groups.set(.@"%", .{ .group = .arithmetic_product_nonchainable, .assoc = .none });
    groups.set(.@"/", .{ .group = .arithmetic_product_chainable });

    groups.set(.@"<<", .{ .group = .bitwise_shift });
    groups.set(.@">>", .{ .group = .bitwise_shift });

    groups.set(.@"&", .{ .group = .bitwise_product });
    groups.set(.@"&~", .{ .group = .bitwise_product });
    groups.set(.@"^", .{ .group = .bitwise_xor });
    groups.set(.@"^~", .{ .group = .bitwise_xor });
    groups.set(.@"|", .{ .group = .bitwise_sum });
    groups.set(.@"|~", .{ .group = .bitwise_sum });

    groups.set(.@"orelse", .{ .group = .coercion });

    groups.set(.@"==", .{ .group = .comparison, .assoc = .none });
    groups.set(.@"!=", .{ .group = .comparison, .assoc = .none });
    groups.set(.@"<", .{ .group = .comparison, .assoc = .none });
    groups.set(.@">", .{ .group = .comparison, .assoc = .none });
    groups.set(.@"<=", .{ .group = .comparison, .assoc = .none });
    groups.set(.@">", .{ .group = .comparison, .assoc = .none });

    groups.set(.@"and", .{ .group = .logical_product });
    groups.set(.@"or", .{ .group = .logical_sum });

    break :b groups;
};

pub fn parseExpression(state: MutSLIR, resT: Reference) !?Reference {
    const save = state.getState();
    return try parseExprPrecedence(state, .start, resT) orelse state.restoreThrow(save);
}

fn parseExprPrecedence(state: MutSLIR, min_exc_prec: PrecClass, resT: Reference) !?Reference {
    const span_start = state.startSpan();
    var node: Reference = try parsePrefixExpr(state, resT) orelse return null;

    var save = state.getState();
    while (true) : (save = state.getState()) {
        var operator: tokenizer.Operator = undefined;
        const info: PrecClass = b: {
            operator = (state.popOperator() catch break :b .start) orelse (break :b .start);
            break :b precedence_group.get(operator.tag);
        };
        const rel = info.cmp(min_exc_prec) orelse return error.ambiguous_precedence;

        if (rel == .lt) {
            state.setState(save);
            break;
        }

        if (min_exc_prec.group == info.group and min_exc_prec.assoc == .none and info.assoc == .none) {
            return error.illegal_chained_operators;
        }

        if (rel == .eq) {
            state.setState(save);
            break;
        }

        const rhs = try parseExprPrecedence(state, info, resT) orelse {
            return error.expected_expression;
        };

        const res = state.slir.getGuid();
        const span = state.endSpan(span_start);
        try state.addInstructionPoly(span, .{res}, .fromOperator(operator.tag), .{ node, rhs });
        node = try makeConstValue(state, .fromGuid(res), span);
    }

    return node;
}

//TODO: Woefully unfinished
fn parsePrefixExpr(state: MutSLIR, resT: Reference) !?Reference {
    return parsePrimative(state, resT);
}

//TODO: Woefully unfinished
fn parsePrimative(state: MutSLIR, resT: Reference) !?Reference {
    _ = resT;
    const span_start = state.startSpan();
    const token = state.popToken();
    const span = state.endSpan(span_start);
    const value: Reference = switch (token) {
        .kind => |kind| ret: {
            const res = state.slir.getGuid();
            try state.addInstructionPoly(span, .{res}, .fromKindTag(kind.tag), .{kind.bits});
            break :ret .fromGuid(res);
        },
        .literal => |lit| ret: {
            switch (lit.tag) {
                .dec_int => |str| {
                    const res = state.slir.getGuid();
                    try state.addInstructionPoly(span, .{res}, .op_decimal_integer_lit, .{try state.slir.intern.convert(str)});
                    break :ret .fromGuid(res);
                },
                else => @panic("TODO"),
            }
        },
        else => @panic("TODO"),
    };

    return try makeConstValue(state, value, span);
}

fn makeConstValue(state: MutSLIR, value: Reference, span: SLIR.Span) !Reference {
    var kind = state.slir.getGuid();
    try state.addInstructionPoly(span, .{kind}, .op_type_of, .{value});
    var new_kind = state.slir.getGuid();
    try state.addInstructionPoly(span, .{new_kind}, .qualify_type_const, .{kind});

    kind = new_kind;
    new_kind = state.slir.getGuid();
    try state.addInstructionPoly(span, .{new_kind}, .qualify_type_view, .{kind});

    const stored_value = state.slir.getGuid();
    try state.addInstructionPoly(span, .{stored_value}, .struct_value, .{ value, new_kind });
    return .fromGuid(stored_value);
}
