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
        },
        kind: void,

        fn bitalign(self: Root) usize {
            switch (self) {
                .integer => |int| {
                    if (int.bits) |bit| {
                        if (bit == 0) {
                            return 0;
                        } else if (bit < 32) {
                            return std.math.ceilPowerOfTwo(usize, bit) catch unreachable;
                        } else {
                            return 32 * (std.math.divCeil(usize, bit, 32) catch unreachable);
                        }
                    } else unreachable;
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
                },
                .kind => try writer.print("type", .{}),
            }
        }

        pub fn eqls(self: Root, other: Root) bool {
            return std.meta.eql(self, other);
        }
    };
};

pub fn deepCopy(object: anytype, alloc: std.mem.Allocator) !@TypeOf(object) {
    const T = @TypeOf(object);
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
        => {
            const new_ptr = try alloc.create(std.meta.Child(T));
            new_ptr.* = try deepCopy(object.*, alloc);
            return new_ptr;
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