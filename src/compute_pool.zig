const std = @import("std");
const types = @import("types.zig");

var singleton_pool: ?*ComputePool = null;

pub const ComputePool = struct {
    alloc: std.mem.Allocator,
    array: std.ArrayList(Item),

    pub const Item = union (enum) {
        int_small: usize,
        kind: *types.partial.Type,

        pub fn format(self: Item, writer: *std.Io.Writer) !void {
            switch (self) {
                .int_small => |val| {
                    try writer.print("cs#{d}", .{val});
                },
                .kind => |partial| {
                    try writer.print("Type({f})", .{partial.*});
                }
            }
        }

        pub fn eqls(self: Item, other: Item) bool {
            if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
            switch (self) {
                .int_small => |s_int| {
                    const o_int = other.int_small;
                    return s_int == o_int;
                },
                .kind => |s_type| {
                    const o_type = other.kind;
                    return s_type.eqls(o_type.*);
                }
            }
        }

        pub fn fromAny(value: anytype) Item {
            return switch (@TypeOf(value)) {
                usize => .{
                    .int_small = value,
                },
                types.partial.Type => @compileError("Must take reference of partial Type."),
                *types.partial.Type => .{
                    .kind = value,
                },
                else => @compileError(std.fmt.comptimePrint("Cannot convert type `{s}` to a computed value.", .{@typeName(@TypeOf(value))})),
            };
        }
    };
    comptime {
        std.debug.assert(@sizeOf(Item) <= 2*@sizeOf(usize));
    }

    pub const Compute = struct {
        tag: enum (u32) {_},
        
        pub fn format(self: Compute, writer: *std.Io.Writer) !void {
            if (singleton_pool) |pool| {
                const item = pool.array.items[@intFromEnum(self.tag)];
                try writer.print("{f}", .{item});
            } else {
                try writer.print("Compute_{d}", .{self.tag});
            }
        }

        pub fn toItem(self: Compute, pool: ComputePool) Item {
            return pool.array.items[@intFromEnum(self.tag)];
        }
    };

    pub fn init(alloc: std.mem.Allocator) ComputePool {
        std.debug.assert(singleton_pool == null);
        return .{.array = .empty, .alloc = alloc};
    }

    pub fn globalize(self: *ComputePool) void {
        std.debug.assert(singleton_pool == null);
        singleton_pool = self;
    }

    pub fn convert(self: *ComputePool, val: anytype) !Compute {
        const item: Item = .fromAny(val);
        for (self.array.items, 0..) |o_item, idx| {
            if (item.eqls(o_item)) {
                return .{.tag = @enumFromInt(idx)};
            }
        }
        try self.array.append(self.alloc, item);
        return .{.tag = @enumFromInt(self.array.items.len - 1)};
    }
};