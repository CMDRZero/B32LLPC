const std = @import("std");

pub const PrecClass = struct {
    group: Group,

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