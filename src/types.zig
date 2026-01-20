const std = @import("std");

pub const qualifier = struct {
    pub const Access = enum {
        view,
        mut,
    };

    pub const Data = enum {
        @"const",
        @"var",
        @"volatile",
    };
};

pub const Alignment = usize;

pub const partial = struct {
    pub const Type = struct {
        access: ?qualifier.Access = null,
        data: ?qualifier.Data = null,
        bitalign: Alignment,
        isPinned: bool = false,
        aggregate: Aggregate,

        pub fn fromRoot(root: *Root) Type {
            const aggr: Aggregate = .{ .root = root };
            return .{
                .aggregate = aggr,
                .bitalign = root.bitalign(),
            };
        }

        pub fn format(self: Type, writer: *std.Io.Writer) !void {
            if (self.access) |access| {
                try writer.print("{t} ", .{access});
            } else {
                try writer.print("any_access ", .{});
            }

            if (self.data) |data| {
                try writer.print("{t} ", .{data});
            } else {
                try writer.print("any_variability ", .{});
            }

            try writer.print("bitalign({}) ", .{self.bitalign});

            if (self.isPinned) {
                try writer.print("pinned ", .{});
            }

            try writer.print("{f}", .{self.aggregate});
        }

        pub fn eqls(self: Type, other: Type) bool {
            if (self.access != other.access) return false;
            if (self.data != other.data) return false;
            if (self.bitalign != other.bitalign) return false;
            if (self.isPinned != other.isPinned) return false;
            return self.aggregate.eqls(other.aggregate);
        }
    };

    const Aggregate = union (enum) {
        root: *Root,

        pub fn format(self: Aggregate, writer: *std.Io.Writer) !void {
            switch (self) {
                .root => |root| try writer.print("{f}", .{root.*}),
            }
        }

        pub fn eqls(self: Aggregate, other: Aggregate) bool {
            if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
            switch (self) {
                .root => |s_root| {
                    const o_root = other.root;
                    return s_root.eqls(o_root.*);
                }
            }
        }
    };

    pub const Root = union (enum) {
        integer: struct {
            signed: bool,
            bits: ?usize,
            low: ?std.math.big.int.Mutable = null,
            high: ?std.math.big.int.Mutable = null,
        },
        kind: void,

        fn bitalign(self: Root) usize {
            switch (self) {
                .integer => |int| {
                    if (int.bits) |bits| {
                        return alignFromBitSize(bits);
                    } else {
                        // if (int.low) |low| if (int.high) |high| {
                        //     const bits = low.len
                        //     return alignFromBitSize(bits);
                        // };
                        return 0;
                    }
                },
                .kind => {
                    return 0;
                }
            }
            unreachable;
        }

        pub fn format(self: Root, writer: *std.Io.Writer) !void {
            switch (self) {
                .integer => |int| {
                    if (int.bits) |bits| {
                        if (int.signed) {
                            try writer.print("i", .{});
                        } else {
                            try writer.print("u", .{});
                        }
                        try writer.print("{}", .{bits});
                    } else {
                        try writer.print("int", .{});
                    }
                    if (int.low != null or int.high != null) {
                        try writer.print("[", .{});
                        if (int.low) |low| try writer.print("{f}", .{low});
                        try writer.print("..", .{});
                        if (int.high) |high| try writer.print("{f}", .{high});
                        try writer.print("]", .{});
                    }
                },
                .kind => try writer.print("type", .{}),
            }
        }

        pub fn eqls(self: Root, other: Root) bool {
            return std.meta.eql(self, other);
        }
    };
};

fn log2(int: *std.math.big.int.Mutable, alloc: std.mem.Allocator) !usize {
    var ret: usize = 0;
    const Limb = std.math.big.Limb;
    const copy_limbs = try alloc.dupe(Limb, int.limbs);
    var copy = int.clone(copy_limbs);
    while (!int.eqlZero()) {
        for (1..64) |i| {
            const x = @as(usize, 1) << @intCast(i);
            copy.shiftRight(int.toConst(), x);
            if (copy.eqlZero()) {
                const i_ = i - 1;
                const x_ = @as(usize, 1) << @intCast(i_);
                ret += x_;
                int.shiftRight(int.toConst(), x_);
                break;
            }
        }
    }
    return ret;
}

test "log2" {
    var int: std.math.big.int.Managed = try .initSet(std.testing.allocator, 16);
    var mut = int.toMutable();
    try std.testing.expectEqual(4, log2(&mut, std.testing.allocator));
    int = try .initSet(std.testing.allocator, 33);
    mut = int.toMutable();
    try std.testing.expectEqual(6, log2(&mut, std.testing.allocator));
    
}

fn alignFromBitSize(bits: usize) usize {
    if (bits == 0) {
        return 0;
    } else if (bits < 32) {
        return std.math.ceilPowerOfTwo(usize, bits) catch unreachable;
    } else {
        return 32 * (std.math.divCeil(usize, bits, 32) catch unreachable);
    }
}

pub fn deepCopy(object: anytype, alloc: std.mem.Allocator) !@TypeOf(object) {
    const T = @TypeOf(object);
    if (T == std.mem.Allocator) return object; //Special case to allow copying allocators in objects that use them
    switch (@typeInfo(T)) {
        .int,
        .float,
        .@"enum",
        .@"bool",
        .@"void",
        => return object,

        .optional,
        => return try deepCopy(object orelse return null, alloc),

        .@"union",
        => {
            var copy = object;
            switch (copy) {
                inline else => |*val| {
                    val.* = try deepCopy(val.*, alloc);
                }
            }
            return copy;
        },

        .pointer,
        //Slice is also here?
        => |ptr| {
            if (ptr.size == .one) {
                const new_ptr = try alloc.create(std.meta.Child(T));
                new_ptr.* = try deepCopy(object.*, alloc);
                return new_ptr;
            } else if (ptr.size == .slice) {
                const new_slice = try alloc.alloc(std.meta.Child(T), object.len);
                for (0..new_slice.len) |i| {
                    new_slice[i] = try deepCopy(object[i], alloc);
                }
                return new_slice;
            }
        },

        .@"struct",
        => {
            var copy: T = undefined;
            inline for (comptime std.meta.fieldNames(T)) |fieldname| {
                @field(copy, fieldname) = try deepCopy(@field(object, fieldname), alloc);
            }
            return copy;
        },

        else => @compileError(std.fmt.comptimePrint("Cannot copy type `{s}` of typeclass `{t}`.", .{@typeName(T), @typeInfo(T)}))
    }
}