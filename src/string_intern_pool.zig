const std = @import("std");

pub const strings_can_print_themselves: bool = false;

var singleton_pool: ?*StringInternPool = null;

pub const StringInternPool = struct {
    alloc: std.mem.Allocator,
    array: std.ArrayList([] u8),

    pub const String = struct {
        tag: enum (u32) {_},
        pool: if (strings_can_print_themselves) (* const StringInternPool) else void,

        pub fn format(self: String, writer: *std.Io.Writer) !void {
            if (comptime strings_can_print_themselves) {
                const str = self.pool.array.items[@intFromEnum(self.tag)];
                try writer.print("{s}", .{str});
            } else if (singleton_pool) |pool| {
                const str = pool.array.items[@intFromEnum(self.tag)];
                try writer.print("{s}", .{str});
            } else {
                try writer.print("String_{}", .{@intFromEnum(self.tag)});
            }
        }
    };

    pub fn init(alloc: std.mem.Allocator) StringInternPool {
        std.debug.assert(singleton_pool == null);
        return .{.array = .empty, .alloc = alloc};
    }

    pub fn globalize(self: *StringInternPool) void {
        std.debug.assert(singleton_pool == null);
        singleton_pool = self;
    }

    pub fn convert(self: *StringInternPool, str: [] u8) !String {
        const maybeself = if (strings_can_print_themselves) self else {};
        for (self.array.items, 0..) |other_str, idx| {
            if (std.mem.eql(u8, str, other_str)) {
                return .{.tag = @enumFromInt(idx), .pool = maybeself};
            }
        }
        try self.array.append(self.alloc, str);
        return .{.tag = @enumFromInt(self.array.items.len - 1), .pool = maybeself};
    }
};