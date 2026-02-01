const std = @import("std");
const types = @import("new_types.zig");
const Guid = @import("slir.zig").SLIR.Guid;

var singleton_pool: ?compute.Pool = null;

pub const compute = struct {
    pub const Pool = struct {
        arena: std.heap.ArenaAllocator,
        array: std.MultiArrayList(AnyItem.Tagged),

        /// Stores an internal arena allocator. Owns all interned values and are freed on deinit
        pub fn init(alloc: std.mem.Allocator) *Pool {
            std.debug.assert(singleton_pool == null);
            singleton_pool = .{.array = .empty, .arena = .init(alloc)};

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

        fn cvtAuto(self: *Pool, item: anytype) !AnyItem {
            const sT = @typeName(@TypeOf(item));
            return switch (@TypeOf(item)) {
                Guid => self.cvtTagged(.{.instr_ref = item}),
                isize => self.cvtTagged(.{.integer = item}),
                *[]const u8 => self.cvtTagged(.{ .bytes = item }),
                else => {
                    const info = @typeInfo(@TypeOf(item));
                    if (info == .comptime_int) {
                        return self.cvtTagged(.{.integer_small = @as(isize, item)});
                    }
                    if (info == .int and info.int.bits <= 63) {
                        return self.cvtTagged(.{.integer_small = item});
                    }
                    @compileError(std.fmt.comptimePrint("Cannot intern type `{s}`", .{sT}));
                }
            };
        }

        fn cvtTagged(self: *Pool, item: AnyItem.Tagged) !AnyItem {
            for (0..self.array.len) |idx| {
                const other = self.get(@enumFromInt(idx));
                if (item.eqls(other)) {
                    return .fromPrivate(.{.index = @enumFromInt(idx), .tag = std.meta.activeTag(item)});
                }
            }
            try self.array.append(self.allocator(), item);
            return .fromPrivate(.{.index = @enumFromInt(self.array.len - 1), .tag = std.meta.activeTag(item)});
        }
    };

    pub const AnyItem = enum (u64) {
        _,
        
        const Private = packed struct (u64) {
            tag: Tag,
            index: Index,
        };

        const Tag = enum (u3) {
            instr_ref,
            bytes,
            integer_small,
            integer_big,
            kind,
            root_type,
        };


        const Tagged = union (Tag) {
            instr_ref: Guid,
            bytes: *[] const u8,
            integer_small: isize,
            integer_big: *std.math.big.int.Const,
            kind: types.partial.Type,
            root_type: types.partial.Root,
            typed_value: *TypedValue,

            pub fn format(self: Tagged, writer: *std.Io.Writer) !void {
                switch (self) {
                    .instr_ref => |guid| try writer.print("%{d}", .{guid}),
                    .bytes => |str| try writer.print("\"{s}\"", .{str.*}),
                    .integer_small => |x| try writer.print("#{d}", .{x}),
                    .integer_big => |x| try writer.print("##{f}", .{x.*}),
                    .kind => |kind| try writer.print("type({f})", .{kind}),
                    .root_type => |kind| try writer.print("{f}", .{kind}),
                    .typed_value => |tv| try writer.print("%{d}: {f}", .{tv.name, tv.kind}),
                }
            }

            pub fn eqls(self: Tagged, other: Tagged) bool {
                if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
                switch (self) {
                    inline else => |self_pay, tag| switch (other) {
                        inline else => |other_pay, other_tag| {
                            if (tag != other_tag) unreachable;
                            return switch (tag) {
                                .integer_small,
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
                        }
                    }
                }
            }
        };

        const Index = enum (@Int(.unsigned, 64 - @bitSizeOf(Tag))) {
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
            return .{.any = self};
        }

        pub fn format(self: AnyItem, writer: *std.Io.Writer) !void {
            try writer.print("{f}", .{self.toTagged()});
        }
    };

    pub fn Item(comptime tag: AnyItem.Tag) type {
        return struct {
            const Self = @This();
            const expected_tag_name = @tagName(tag);

            any: AnyItem,

            pub fn from(value: anytype) !Self {
                const resT = @FieldType(AnyItem.Tagged, Self.expected_tag_name);
                return .{.any = try singleton_pool.?.cvtAuto(@as(resT, value))};
            }

            pub fn unwrap(self: Self) @FieldType(AnyItem.Tagged, Self.expected_tag_name) {
                // if (self.any.toTagged() != Self.expected_tag) return error.expected_tag;
                return @field(self.any.toTagged(), Self.expected_tag_name);
            }

            pub fn forget(self: Self) AnyItem {
                return self.any;
            }
        };
    }
};

pub const TypedValue = struct {
    kind: compute.Item(.kind),
    name: Guid,
};

test "compute pool test" {
    const alloc = std.testing.allocator;
    const pool: *compute.Pool = .init(alloc);
    defer pool.deinit();

    var str: []const u8 = "Hello World!";

    const x = try pool.cvtTagged(.{ .instr_ref = @enumFromInt(2)});
    const y = try pool.cvtTagged(.{.bytes = &str});
    const z = try pool.cvtTagged(.{.instr_ref = @enumFromInt(2)});
    const w = try pool.cvtTagged(.{.bytes = &str});
    
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
    try std.testing.expectEqual(x.toTagged().integer_small, x_new);
}