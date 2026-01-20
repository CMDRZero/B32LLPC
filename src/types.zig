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
    };

    const Aggregate = union (enum) {
        root: *Root,

        pub fn format(self: Aggregate, writer: *std.Io.Writer) !void {
            switch (self) {
                .root => |root| try writer.print("{f}", .{root.*}),
            }
        }
    };

    pub const Root = union (enum) {
        integer: struct {
            signed: bool,
            bits: ?usize,
        },

        fn bitalign(self: Root) usize {
            switch (self) {
                .integer => |int| {
                    if (int.bits) |bit| {
                        if (bit < 32) {
                            return std.math.ceilPowerOfTwo(usize, bit) catch unreachable;
                        } else {
                            return 32 * (std.math.divCeil(usize, bit, 32) catch unreachable);
                        }
                    } else unreachable;
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
            }
        }
    };
};