const std = @import("std");
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
const tokenizer = @import("tokenizer.zig");
const ArrayList = std.ArrayList;
const mem = std.mem;
const Allocator = mem.Allocator;
const String = StringInternPool.String;

pub const SLIR = struct {
    alloc: Allocator,
    intern: *StringInternPool,
    next_guid: Guid,
    functions: ArrayList(Function),

    //Either an instruction reference, a string intern pool reference, or a plain integer.
    pub const Reference = enum (u64) {
        _,

        const num_u32: u64 = std.math.maxInt(u32) + 1;

        pub const TagType = enum {
            instr_ref,
            string_ref,
            int,
        };

        pub const Tagged = union (TagType) {
            instr_ref: Guid,
            string_ref: String,
            int: u64,
        };

        pub fn toTaggedUnion(self: Reference) Reference.Tagged {
            var val: u64 = @intFromEnum(self);
            if (val <= std.math.maxInt(u32)) return .{.instr_ref = @enumFromInt(val)};
            val -= std.math.maxInt(u32) + 1;
            
            if (val <= std.math.maxInt(u32)) return .{.string_ref = .{.tag = @enumFromInt(val), .pool = undefined}};
            val -= std.math.maxInt(u32) + 1;

            return .{.int = val};
        }

        pub fn fromInt(value: u64) ?Reference {
            const offset_value = std.math.add(u64, value, 2 * num_u32) catch return null;
            return @enumFromInt(offset_value);
        }

        pub fn fromGuid(guid: Guid) Reference {
            return @enumFromInt(@intFromEnum(guid));
        }

        pub fn fromString(string: String) Reference {
            return @enumFromInt(num_u32 + @intFromEnum(string.tag));
        }

        pub fn fromAny(value: anytype) ?Reference {
            switch (@TypeOf(value)) {
                Reference => {
                    return value;
                },
                comptime_int => {
                    return .fromInt(value);
                },
                Guid => {
                    return .fromGuid(value);
                },
                String => {
                    return .fromString(value);
                },
                @import("tokenizer.zig").Token.Identifier => {
                    @compileError("To convert `Identifier` to ReferenceType, convert it in an internpool.");
                },
                else => |T| {
                    if (comptime @typeInfo(T) == .int and std.math.cast(u64, std.math.maxInt(T)) != null) return .fromInt(value);
                    @compileError("Type `"++@typeName(T)++"` cannot be converted to ReferenceType.");
                }
            }
        }

        pub fn format(self: Reference, writer: *std.Io.Writer) !void {
            switch (self.toTaggedUnion()) {
                .instr_ref => |guid| {
                    try writer.print("%{d}", .{guid});
                },
                .string_ref => |string| {
                    try writer.print("\"{f}\"", .{string});
                },
                .int => |int| {
                    try writer.print("#{d}", .{int});
                }
            }
        } 
    };

    pub const Guid = enum (u32) {
        _,
        const init: Guid = @enumFromInt(1);
    };

    pub const Function = struct {
        name: String,
        args: ArrayList(Arg),
        resT: Reference,
        blocks: ArrayList(Block) = .empty,
        

        const Arg = struct {
            name: String,
            kind: Guid,
        };

        pub const Block = struct {
            name: String,
            instrs: ArrayList(Instruction) = .empty,

            pub const Instruction = struct {
            tag: Tag,
            results: ArrayList(Reference),
            args: ArrayList(Reference),

            pub const Tag = enum {
                add,
                sub,
                mul,
                div,
                mod,

                bit_shift_left,
                bit_shift_right,

                bit_and,
                bit_andn,
                bit_xor,
                bit_xorn,
                bit_or,
                bit_orn,

                copy,
                slice_or_range_refine,

                const_value,
                type_of,

                signed_int,
                unsigned_int,
                floating_point,

                decimal_integer,

                debug_named_value,

                pub fn fromOperator(op: tokenizer.Operator.Tag) Tag {
                    return switch (op) {
                        .@"+" => .add,
                        .@"-" => .sub,
                        .@"*" => .mul,
                        .@"/" => .div,
                        .@"%" => .mod,
                        
                        .@"<<" => .bit_shift_left,
                        .@">>" => .bit_shift_right,
                        
                        .@"&" => .bit_and,
                        .@"&~" => .bit_andn,
                        .@"^" => .bit_xor,
                        .@"^~" => .bit_xorn,
                        .@"|" => .bit_or,
                        .@"|~" => .bit_orn,

                        else => unreachable,
                    };
                }
            };

            pub fn format(self: Instruction, writer: *std.Io.Writer) !void {
                if (self.results.items.len >= 1) {
                    try writer.print("{f}", .{self.results.items[0]});
                    for (self.results.items[1..]) |result| {
                        try writer.print(", {f}", .{result});
                    }
                    try writer.print(" = ", .{});
                }
                try writer.print("{t}", .{self.tag});
                if (self.args.items.len >= 1) {
                    try writer.print(" {f}", .{self.args.items[0]});
                    for (self.args.items[1..]) |arg| {
                        try writer.print(", {f}", .{arg});
                    }
                }
            }
        };
        };

        
    };

    pub fn init(alloc: Allocator, intern: *StringInternPool) SLIR {
        return .{
            .alloc = alloc,
            .intern = intern,
            .next_guid = .init,
            .functions = .empty,
        };
    }

    pub fn format(self: SLIR, writer: *std.Io.Writer) !void {
        try writer.print("┌─── SLIR\n", .{});
        try writer.print("│ Next Guid: ({})\n", .{@intFromEnum(self.next_guid)});
        for (self.functions.items) |func| {
            try writer.print("│ ┌─── fn {f} (", .{func.name});
            for (func.args.items) |arg| {
                try writer.print("{f}: %{d}, ", .{arg.name, arg.kind});
            }
            try writer.print(") -> {f}\n", .{func.resT});
            for (func.blocks.items) |block| {
                try writer.print("│ │ ┌─── block {f}:\n", .{block.name});
                for (block.instrs.items) |instr| {
                    try writer.print("│ │ │ {f}\n", .{instr});
                }
            }
            
        }
    }

    pub fn getGuid(self: *SLIR) Guid {
        defer self.next_guid = @enumFromInt(1 + @intFromEnum(self.next_guid));
        return self.next_guid;
    }

    pub fn currentFunction(self: *SLIR) *Function {
        std.debug.assert(self.functions.items.len > 0);
        const items = &self.functions.items;
        return &items.*[items.len - 1];
    }

    pub fn currentBlock(self: *SLIR) *Function.Block {
        const curr_fn = self.currentFunction();
        std.debug.assert(curr_fn.blocks.items.len > 0);
        const items = &curr_fn.blocks.items;
        return &items.*[items.len - 1];
    }

    pub fn appendBlock(self: *SLIR, name: ?String) !void {
        const curr_fn = self.currentFunction();
        const blockname: String = name orelse b: {
            const buf = try self.alloc.alloc(u8, 16);
            const slice = try std.fmt.bufPrint(buf, "%L{d}", .{self.getGuid()});
            break :b try self.intern.convert(slice);
        };
        try curr_fn.blocks.append(self.alloc, .{.name = blockname});
    }
};