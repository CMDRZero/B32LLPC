const std = @import("std");

const Guid = @import("slir.zig").SLIR.Guid;
const types = @import("new_types.zig");

var singleton_pool: ?compute.Pool = null;

pub const compute = struct {
    pub const Pool = struct {
        arena: std.heap.ArenaAllocator,
        array: std.MultiArrayList(AnyItem.Tagged),

        /// Stores an internal arena allocator. Owns all interned values and are freed on deinit
        pub fn init(alloc: std.mem.Allocator) *Pool {
            std.debug.assert(singleton_pool == null);
            singleton_pool = .{ .array = .empty, .arena = .init(alloc) };

            return &singleton_pool.?;
        }

        pub fn deinit(self: *Pool) void {
            self.arena.deinit();
            singleton_pool = null;
        }

        fn allocator(self: *Pool) std.mem.Allocator {
            return self.arena.allocator();
        }

        fn get(self: Pool, index: compute.AnyItem.Index) AnyItem.Tagged {
            std.debug.assert(self.array.len > @intFromEnum(index));
            return self.array.get(@intFromEnum(index));
        }

        pub fn cvtAuto(self: *Pool, item: anytype) !AnyItem {
            const T = @TypeOf(item);
            const sT = @typeName(T);
            return switch (T) {
                AnyItem => return item,

                Guid => self.cvtTagged(.{ .instr_ref = item }),
                isize => self.cvtTagged(.{ .integer_small_signed = item }),
                usize => self.cvtTagged(.{ .integer_usize = item }),

                *[]const u8 => self.cvtTagged(.{ .bytes = item }),
                []const u8 => self.cvtTagged(.{ .bytes = try types.refDeepCopy(item, self.allocator()) }),
                []u8 => self.cvtTagged(.{ .bytes = try types.refDeepCopy(@as([]const u8, item), self.allocator()) }),

                @import("tokenizer.zig").Token.Identifier => self.cvtTagged(.{ .bytes = try types.refDeepCopy(@as([]const u8, item.str), self.allocator()) }),

                std.math.big.int.Managed => self.cvtTagged(.{ .integer_big = try types.refDeepCopy(item.toConst(), self.allocator()) }),
                *std.math.big.int.Const => self.cvtTagged(.{ .integer_big = item }),

                *types.partial.Root => self.cvtTagged(.{ .root_type = item }),
                types.partial.Root => self.cvtTagged(.{ .root_type = try types.refDeepCopy(item, self.allocator()) }),

                *types.partial.Type => self.cvtTagged(.{ .kind = item }),
                types.partial.Type => self.cvtTagged(.{ .kind = try types.refDeepCopy(item, self.allocator()) }),

                //struct {Item(.kind), AnyItem} => self.cvtTagged(.{.typed_value = try types.refDeepCopy(@as(TypedValue, item), self.allocator())}),

                else => {
                    const info = @typeInfo(T);
                    if (info == .comptime_int) {
                        return self.cvtTagged(.{ .integer_usize = @as(usize, item) });
                    }
                    if (info == .int and info.int.bits <= 64) {
                        return self.cvtTagged(.{ .integer_usize = item });
                    }
                    if (info == .pointer and info.pointer.size == .one and @typeInfo(info.pointer.child) == .array) {
                        return self.cvtTagged(.{ .bytes = try types.refDeepCopy(@as([]const u8, item), self.allocator()) });
                    }
                    //@compileLog(@typeInfo(T));
                    @compileError(std.fmt.comptimePrint("Cannot intern type `{s}`", .{sT}));
                },
            };
        }

        pub fn cvtTagged(self: *Pool, item: AnyItem.Tagged) !AnyItem {
            for (0..self.array.len) |idx| {
                const other = self.get(@enumFromInt(idx));
                if (item.eqls(other)) {
                    return .fromPrivate(.{ .index = @enumFromInt(idx), .tag = std.meta.activeTag(item) });
                }
            }
            try self.array.append(self.allocator(), item);
            return .fromPrivate(.{ .index = @enumFromInt(self.array.len - 1), .tag = std.meta.activeTag(item) });
        }

        pub fn cvtStr(self: *Pool, string: anytype) !Item(.bytes) {
            return (try self.cvtAuto(string)).bake(.bytes);
        }
    };

    pub const AnyItem = enum(u64) {
        _,

        const Private = packed struct(u64) {
            tag: Tag,
            index: Index,
        };

        pub const Tag = enum(u3) {
            instr_ref,
            bytes,
            integer_small_signed,
            integer_usize,
            integer_big,
            kind,
            root_type,
            typed_value,
        };

        const Tagged = union(Tag) {
            instr_ref: Guid,
            bytes: *[]const u8,
            integer_small_signed: isize,
            integer_usize: usize,
            integer_big: *std.math.big.int.Const,
            kind: *types.partial.Type,
            root_type: *types.partial.Root,
            typed_value: *TypedValue,

            pub fn format(self: Tagged, writer: *std.Io.Writer) !void {
                switch (self) {
                    .instr_ref => |guid| try writer.print("%{d}", .{guid}),
                    .bytes => |str| try writer.print("\"{s}\"", .{str.*}),
                    .integer_small_signed => |x| try writer.print("s#{d}", .{x}),
                    .integer_usize => |x| try writer.print("u#{d}", .{x}),
                    .integer_big => |x| try writer.print("##{f}", .{x.*}),
                    .kind => |kind| try writer.print("type({f})", .{kind}),
                    .root_type => |kind| try writer.print("{f}", .{kind}),
                    .typed_value => |tv| try writer.print("{f}: {f}", .{ tv.value, tv.kind }),
                }
            }

            pub fn eqls(self: Tagged, other: Tagged) bool {
                if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
                switch (self) {
                    inline else => |self_pay, tag| switch (other) {
                        inline else => |other_pay, other_tag| {
                            if (tag != other_tag) unreachable;
                            return switch (tag) {
                                .integer_small_signed,
                                .integer_usize,
                                .instr_ref,
                                .kind,
                                .root_type,
                                => std.meta.eql(self_pay, other_pay),

                                .typed_value,
                                => std.meta.eql(self_pay.*, other_pay.*),

                                .bytes,
                                => std.mem.eql(u8, self_pay.*, other_pay.*),

                                .integer_big,
                                => self_pay.*.eql(other_pay.*),
                            };
                        },
                    },
                }
            }
        };

        const Index = enum(@Int(.unsigned, 64 - @bitSizeOf(Tag))) {
            _,
        };

        fn private(self: AnyItem) Private {
            return @bitCast(@as(u64, @intFromEnum(self)));
        }

        fn fromPrivate(private_self: Private) AnyItem {
            return @enumFromInt(@as(u64, @bitCast(private_self)));
        }

        pub fn toTagged(self: AnyItem) Tagged {
            const pool = singleton_pool.?;
            const private_self = self.private();
            const data = pool.get(private_self.index);
            return data;
        }

        pub fn bake(self: AnyItem, comptime tag: Tag) Item(tag) {
            std.debug.assert(self.private().tag == tag);
            return .{ .any = self };
        }

        pub fn PayloadType(comptime tag: Tag) type {
            return @FieldType(AnyItem.Tagged, @tagName(tag));
        }

        pub fn hasTag(self: AnyItem, comptime tag: Tag) ?PayloadType(tag) {
            if (self.private().tag == tag) return @field(self.toTagged(), @tagName(tag));
            return null;
        }

        pub fn as(self: AnyItem, comptime tag: Tag) PayloadType(tag) {
            return self.hasTag(tag).?;
        }

        pub fn format(self: AnyItem, writer: *std.Io.Writer) !void {
            try writer.print("{f}", .{self.toTagged()});
        }

        pub fn fromAuto(value: anytype) !AnyItem {
            return try singleton_pool.?.cvtAuto(value);
        }

        pub fn fromTagged(value: AnyItem.Tagged) !AnyItem {
            return try singleton_pool.?.cvtTagged(value);
        }
    };

    pub fn Item(comptime tag: AnyItem.Tag) type {
        return struct {
            const Self = @This();
            const expected_tag_name = @tagName(tag);

            any: AnyItem,

            pub fn from(value: anytype) !Self {
                const resT = @FieldType(AnyItem.Tagged, Self.expected_tag_name);
                return .{ .any = try singleton_pool.?.cvtAuto(@as(resT, value)) };
            }

            pub fn fromAuto(value: anytype) !Self {
                return .{ .any = try singleton_pool.?.cvtAuto(value) };
            }

            pub fn unwrap(self: Self) @FieldType(AnyItem.Tagged, Self.expected_tag_name) {
                // if (self.any.toTagged() != Self.expected_tag) return error.expected_tag;
                return @field(self.any.toTagged(), Self.expected_tag_name);
            }

            pub fn forget(self: Self) AnyItem {
                return self.any;
            }

            pub fn format(self: Self, writer: *std.Io.Writer) !void {
                try writer.print("{f}", .{self.any.toTagged()});
            }
        };
    }
};

pub const TypedValue = struct {
    kind: compute.Item(.kind),
    value: compute.AnyItem,
};

test "compute pool test" {
    const alloc = std.testing.allocator;
    const pool: *compute.Pool = .init(alloc);
    defer pool.deinit();

    var str: []const u8 = "Hello World!";

    const x = try pool.cvtTagged(.{ .instr_ref = @enumFromInt(2) });
    const y = try pool.cvtTagged(.{ .bytes = &str });
    const z = try pool.cvtTagged(.{ .instr_ref = @enumFromInt(2) });
    const w = try pool.cvtTagged(.{ .bytes = &str });

    try std.testing.expectEqual(x, z);
    try std.testing.expectEqual(y, w);
    try std.testing.expect(x != y);
}

test "compute pool test auto" {
    const alloc = std.testing.allocator;
    const pool: *compute.Pool = .init(alloc);
    defer pool.deinit();

    var str: []const u8 = "Hello World!";

    const x = try pool.cvtAuto(@as(Guid, @enumFromInt(2)));
    const y = try pool.cvtAuto(&str);
    const z = try pool.cvtAuto(@as(Guid, @enumFromInt(2)));
    const w = try pool.cvtAuto(&str);

    try std.testing.expectEqual(x, z);
    try std.testing.expectEqual(y, w);
    try std.testing.expect(x != y);
}

test "compute pool test typed" {
    const alloc = std.testing.allocator;
    const pool: *compute.Pool = .init(alloc);
    defer pool.deinit();

    //var str: []const u8 = "Hello World!";

    const x = try pool.cvtAuto(3);
    const x_int: compute.Item(.integer_small) = x.bake(.integer_small);
    const x_new = x_int.unwrap();
    try std.testing.expectEqual(x.toTagged().integer_small_signed, x_new);
}
