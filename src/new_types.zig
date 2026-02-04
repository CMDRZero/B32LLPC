const std = @import("std");
const compute = @import("new_compute_pool.zig").compute;

var global_type_display_mode: enum {verbose, minimal} = .minimal;

pub const qualifier = struct {
    pub const Access = enum {
        mut,
        view,

        pub fn isSubset(lhs: Access, rhs: Access) bool {
            return @intFromEnum(lhs) <= @intFromEnum(rhs);
        }
    };

    pub const Data = enum {
        @"const",
        @"var",
        @"volatile",

        pub fn isSubset(lhs: Data, rhs: Data) bool {
            return @intFromEnum(lhs) <= @intFromEnum(rhs);
        }
    };
};

pub const Alignment = u6;

pub const partial = struct {
    pub const Type = struct {
        access: ?qualifier.Access = null,
        data: ?qualifier.Data = null,
        isPinned: bool = false,
        
        bitalign: Alignment,    //Always power of two (or 0) in range [0..32]
        pack_as: usize,         //Must be greater than or equal to the logical size
        
        aggregate: Aggregate,

        pub fn fromRoot(root: *Root) Type {
            const aggr: Aggregate = .{ .root = compute.Item(.root_type).from(root) catch unreachable };
            return .{
                .aggregate = aggr,
                .bitalign = alignOf(root.bitsize()),
                .pack_as = logicalSize(root.bitsize()),
            };
        }

        pub fn format(self: Type, writer: *std.Io.Writer) !void {
            switch (global_type_display_mode) {
                .minimal => try self.formatMinimal(writer),
                .verbose => try self.formatExplicit(writer),
            }
        }

        pub fn formatMinimal(self: Type, writer: *std.Io.Writer) !void {
            if (self.access) |access| {
                if (access != .view) try writer.print("{t} ", .{access});
            } else {
                try writer.print("_ ", .{});
            }

            if (self.data) |data| {
                if (data != .@"var") try writer.print("{t} ", .{data});
            } else {
                try writer.print("_ ", .{});
            }

            if (self.bitsize() != 0) {
                if (logicalSize(self.bitsize()) != self.bitalign) try writer.print("stride({}) ", .{self.pack_as});

                if (alignOf(self.bitsize()) != self.bitalign) try writer.print("bitalign({}) ", .{self.bitalign});
            }

            if (self.isPinned) {
                try writer.print("pinned ", .{});
            }

            try writer.print("{f}", .{self.aggregate});
        }

        pub fn formatExplicit(self: Type, writer: *std.Io.Writer) !void {
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

            try writer.print("stride({}) ", .{self.pack_as});

            try writer.print("bitalign({}) ", .{self.bitalign});

            try writer.print("bitsize({}) ", .{self.bitsize()});

            if (self.isPinned) {
                try writer.print("pinned ", .{});
            }

            try writer.print("{f}", .{self.aggregate});
        }

        pub fn bitsize(self: Type) usize {
            return self.aggregate.bitsize();
        }
    };

    const Aggregate = union (enum) {
        root: compute.Item(.root_type),

        pub fn format(self: Aggregate, writer: *std.Io.Writer) !void {
            switch (self) {
                .root => |root| try writer.print("{f}", .{root.unwrap()}),
            }
        }

        pub fn bitsize(self: Aggregate) usize {
            return switch (self) {
                .root => |root| root.unwrap().bitsize(),
            };
        }
    };

    pub const Root = union (enum) {
        integer: Integer,
        kind: void,

        pub const Integer = struct {
            backing: union (enum) {
                explicit: Impl,
                implicit: ?Impl,
            },
            low: ?compute.Item(.integer_big) = null,
            high: ?compute.Item(.integer_big) = null,

            pub const Impl = struct {
                signed: bool,
                bits: usize
            };

            /// Returns the integer value which this must be at least equal to. 
            /// Either implicitly from the backing bits or explicitly set from ranging
            /// Do not assume that because the return is managed that the allocator is live
            pub fn lowBound(self: Integer, alloc: std.mem.Allocator) !std.math.big.int.Managed {
                if (self.low) |low| {
                    return low.unwrap().toManaged(alloc);
                }
                if (self.backing.explicit.signed) {
                    var ret: std.math.big.int.Managed = try .initSet(alloc, 1);
                    try ret.shiftLeft(&ret, self.backing.explicit.bits - 1);
                    ret.negate();
                    return ret;
                } else {
                    const ret: std.math.big.int.Managed =  try .initSet(alloc, 0);
                    return ret;
                }
            }

            pub fn highBound(self: Integer, alloc: std.mem.Allocator) !std.math.big.int.Managed {
                if (self.high) |high| {
                    return high.unwrap().toManaged(alloc);
                }
                if (self.backing.explicit.signed) {
                    var ret: std.math.big.int.Managed = try .initSet(alloc, 1);
                    try ret.shiftLeft(&ret, self.backing.explicit.bits - 1);
                    try ret.addScalar(&ret, -1);
                    return ret;
                } else {
                    var ret: std.math.big.int.Managed = try .initSet(alloc, 1);
                    try ret.shiftLeft(&ret, self.backing.explicit.bits);
                    try ret.addScalar(&ret, -1);
                    return ret;
                }
            }
        };

        pub const MakeIntegerBacking = union (enum) {
            explicit: Integer.Impl, 
            implicit
        };

        /// The integer created owns the low and high bounds if supplied
        pub fn makeInteger(
            backing: MakeIntegerBacking,
            alloc: ?std.mem.Allocator,
            defaults: struct {
                low: ?compute.Item(.integer_big) = null,
                high: ?compute.Item(.integer_big) = null,
        }) !Root {
            switch (backing) {
                .explicit => |impl| {
                    return .{ .integer = .{
                        .backing = .{ .explicit = impl },
                        .low = defaults.low,
                        .high = defaults.high,
                    } };
                },
                .implicit => {
                    if (defaults.low) |low| if (defaults.high) |high| {
                        const lowbits = try bitcountSigned(low.unwrap(), alloc.?);
                        const highbits = try bitcountSigned(high.unwrap(), alloc.?);
                        const bits = @max(lowbits, highbits);
                        const signed = !low.unwrap().positive and !low.unwrap().eqlZero(); //If the low isnt negative the high cannot be
                        return .{ .integer = .{
                            .backing = .{ .implicit = .{.bits = bits, .signed = signed} },
                            .low = defaults.low,
                            .high = defaults.high,
                        } };
                    };
                    return .{ .integer = .{
                        .backing = .{ .implicit = null },
                        .low = defaults.low,
                        .high = defaults.high,
                    } };
                }
            }
        }

        fn bitsize(self: Root) usize {
            switch (self) {
                .integer => |int| switch (int.backing) {
                    .explicit => |impl| {
                        return impl.bits;
                    },
                    .implicit => |q_impl| {
                        return (q_impl orelse return 0).bits;
                    }
                },
                .kind => return 0,
            }
        }

        pub fn format(self: Root, writer: *std.Io.Writer) !void {
            switch (self) {
                .integer => |int| switch (int.backing) {
                    .explicit => |impl| {
                        if (impl.signed) {
                            try writer.print("i", .{});
                        } else {
                            try writer.print("u", .{});
                        }
                        try writer.print("{}", .{impl.bits});
                    },
                    .implicit => {
                        try writer.print("int", .{});
                        if (int.low != null or int.high != null) {
                            try writer.print("[", .{});
                            if (int.low) |low| try writer.print("{f}", .{low});
                            try writer.print("..", .{});
                            if (int.high) |high| try writer.print("{f}", .{high});
                            try writer.print("]", .{});
                        }
                    }
                },
                .kind => try writer.print("type", .{}),
            }
        }
    };
};

//TODO: Fix this docstring
/// Returns the minimum number of bits to represent this number. 
/// If the number is posative its bits in an unsigned int
/// If the number is negative its bits in a signed int
/// Returns 0 if supplied 0
fn bitcountSigned(int_const: *std.math.big.int.Const, alloc: std.mem.Allocator) !usize {
    if (int_const.eqlZero()) return 0;
    if (int_const.positive) return bitcount(int_const, alloc);

    var int: std.math.big.int.Managed = try int_const.toManaged(alloc);

    try int.addScalar(&int, 1);
    int.negate();
    return bitcount(@constCast(&int.toConst()), alloc);
}

/// Returns the minimum number of bits to represent this number
/// bitcount(0b1) == 1; bitcount(0b100) == 3
/// returns 0 bits if given 0
/// Equivilent to `len(bin(x)][2:])` in python if x > 0
fn bitcount(int_const: *std.math.big.int.Const, alloc: std.mem.Allocator) !usize {
    std.debug.assert(int_const.positive);
    if (int_const.eqlZero()) return 0;

    var int: std.math.big.int.Managed = try int_const.toManaged(alloc);

    var ret: usize = 0;
    //const Limb = std.math.big.Limb;
    //const copy_limbs = try alloc.dupe(Limb, int.limbs);
    var copy = try int.clone();
    while (!int.eqlZero()) {
        for (1..64) |i| {
            const x = @as(usize, 1) << @intCast(i);
            try copy.shiftRight(&int, x);
            if (copy.eqlZero()) {
                const i_ = i - 1;
                const x_ = @as(usize, 1) << @intCast(i_);
                ret += x_;
                try int.shiftRight(&int, x_);
                break;
            }
        }
    }
    return ret;
}

test "bitcount" {
    var int: std.math.big.int.Managed = try .initSet(std.testing.allocator, 16);
    var mut = int.toMutable();
    try std.testing.expectEqual(5, bitcount(&mut, std.testing.allocator));
    int = try .initSet(std.testing.allocator, 33);
    mut = int.toMutable();
    try std.testing.expectEqual(6, bitcount(&mut, std.testing.allocator));
}

fn alignOf(bits: usize) u6 {
    if (bits == 0) {
        return 0;
    } else if (bits < 32) {
        return std.math.ceilPowerOfTwo(u6, @intCast(bits)) catch unreachable;
    } else {
        return 32;
    }
}

fn logicalSize(bits: usize) usize {
    if (bits == 0) {
        return 0;
    } else if (bits < 32) {
        return std.math.ceilPowerOfTwo(usize, bits) catch unreachable;
    } else {
        return 32 * (std.math.divCeil(usize, bits, 32) catch unreachable);
    }
}

pub fn refDeepCopy(object: anytype, alloc: std.mem.Allocator) !*@TypeOf(object) {
    return try deepCopy(@constCast(&object), alloc);
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