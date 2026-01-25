const std = @import("std");
const ArrayList = std.ArrayList;
const mem = std.mem;
const Allocator = mem.Allocator;

const ComputePool = @import("compute_pool.zig").ComputePool;
const Compute = ComputePool.Compute;
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
const String = StringInternPool.String;
const tokenizer = @import("tokenizer.zig");

pub const SLIR = struct {
    alloc: Allocator,
    intern: *StringInternPool,
    computes: *ComputePool,
    next_guid: Guid,
    functions: ArrayList(Function),

    //Either an instruction reference, a string intern pool reference, or a plain integer.
    pub const Reference = enum(u64) {
        _,

        const num_u32: u64 = std.math.maxInt(u32) + 1;

        pub const TagType = enum {
            instr_ref,
            string_ref,
            compute_ref,
            int,
        };

        pub const Tagged = union(TagType) {
            instr_ref: Guid,
            string_ref: String,
            compute_ref: Compute,
            int: u64,
        };

        pub fn toTaggedUnion(self: Reference) Reference.Tagged {
            var val: u64 = @intFromEnum(self);
            if (val <= std.math.maxInt(u32)) return .{ .instr_ref = @enumFromInt(val) };
            val -= num_u32;

            if (val <= std.math.maxInt(u32)) return .{ .string_ref = .{ .tag = @enumFromInt(val), .pool = undefined } };
            val -= num_u32;

            if (val <= std.math.maxInt(u32)) return .{ .compute_ref = .{ .tag = @enumFromInt(val) } };
            val -= num_u32;

            return .{ .int = val };
        }

        pub fn fromInt(value: u64) ?Reference {
            const offset_value = std.math.add(u64, value, 3 * num_u32) catch return null;
            return @enumFromInt(offset_value);
        }

        pub fn fromGuid(guid: Guid) Reference {
            return @enumFromInt(@intFromEnum(guid));
        }

        pub fn fromString(string: String) Reference {
            return @enumFromInt(num_u32 + @intFromEnum(string.tag));
        }

        pub fn fromCompute(compute: Compute) Reference {
            return @enumFromInt(2 * num_u32 + @intFromEnum(compute.tag));
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
                Compute => {
                    return .fromCompute(value);
                },
                @import("tokenizer.zig").Token.Identifier => {
                    @compileError("To convert `Identifier` to ReferenceType, convert it in an internpool.");
                },
                else => |T| {
                    if (comptime @typeInfo(T) == .int and std.math.cast(u64, std.math.maxInt(T)) != null) return .fromInt(value);
                    @compileError("Type `" ++ @typeName(T) ++ "` cannot be converted to ReferenceType.");
                },
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
                .compute_ref => |compute| {
                    try writer.print("{f}", .{compute});
                },
                .int => |int| {
                    try writer.print("#{d}", .{int});
                },
            }
        }
    };

    pub const Guid = enum(u32) {
        _,
        const init: Guid = @enumFromInt(1);
    };
    
    pub const Span = struct {
        slice: []u8
    };

    pub const Function = struct {
        name: String,
        args: ArrayList(Arg),
        resT: Reference,
        blocks: ArrayList(Block) = .empty,
        scope_depth: u32 = 0,

        const Arg = struct {
            name: String,
            kind: Guid,
        };

        pub const Block = struct {
            name: String,
            instrs: ArrayList(Instruction) = .empty,
            scope_ids: ArrayList(u32) = .empty,

            pub const Instruction = struct {
                tag: Tag,
                results: ArrayList(Reference),
                args: ArrayList(Reference),
                span: Span,

                pub const Tag = enum {
                    op_add,
                    op_sub,
                    op_mul,
                    op_div,
                    op_mod,

                    op_bit_shift_left,
                    op_bit_shift_right,

                    op_bit_and,
                    op_bit_andn,
                    op_bit_xor,
                    op_bit_xorn,
                    op_bit_or,
                    op_bit_orn,

                    alias,
                    op_slice_or_range_refine,

                    struct_value,

                    op_type_of,

                    ensure_is_type,
                    ensure_may_cast,

                    qualify_type_const,
                    qualify_type_var,
                    qualify_type_view,
                    qualify_type_mut,

                    op_type_signed_int,
                    op_type_unsigned_int,
                    op_type_floating_point,

                    op_decimal_integer_lit,

                    struct_int_lit,
                    struct_type_lit,

                    info_named_value,
                    op_load_named_value,

                    pub fn fromOperator(op: tokenizer.Operator.Tag) Tag {
                        return switch (op) {
                            .@"+" => .op_add,
                            .@"-" => .op_sub,
                            .@"*" => .op_mul,
                            .@"/" => .op_div,
                            .@"%" => .op_mod,

                            .@"<<" => .op_bit_shift_left,
                            .@">>" => .op_bit_shift_right,

                            .@"&" => .op_bit_and,
                            .@"&~" => .op_bit_andn,
                            .@"^" => .op_bit_xor,
                            .@"^~" => .op_bit_xorn,
                            .@"|" => .op_bit_or,
                            .@"|~" => .op_bit_orn,

                            else => unreachable,
                        };
                    }

                    pub fn fromKindTag(op: tokenizer.Token.Kind.Tag) Tag {
                        return switch (op) {
                            .unsigned_int => .op_type_unsigned_int,
                            .signed_int => .op_type_signed_int,
                            .float => .op_type_floating_point,
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

    pub fn init(alloc: Allocator, intern: *StringInternPool, computes: *ComputePool) SLIR {
        return .{
            .alloc = alloc,
            .intern = intern,
            .computes = computes,
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
                try writer.print("{f}: %{d}, ", .{ arg.name, arg.kind });
            }
            try writer.print(") -> {f}\n", .{func.resT});
            for (func.blocks.items) |block| {
                try writer.print("│ │ ┌─── block {f}", .{block.name});
                if (block.scope_ids.items.len > 0) {
                    try writer.print(" {{{}", .{block.scope_ids.items[0]});
                    for (block.scope_ids.items[1..]) |scope_id| {
                        try writer.print(".{}", .{scope_id});
                    }
                    try writer.print("}}", .{});
                }
                try writer.print(":\n", .{});
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

    pub fn createOnInstance(self: *SLIR) !void {
        const curr_fn = self.currentFunction();
        std.debug.assert(curr_fn.scope_depth == 0);
        const blockname: String = try self.intern.convert("OnInstance");
        try curr_fn.blocks.append(self.alloc, .{ .name = blockname });
    }

    pub fn createEntry(self: *SLIR) !void {
        const curr_fn = self.currentFunction();
        std.debug.assert(curr_fn.scope_depth == 1);
        const blockname: String = try self.intern.convert("Entry");
        try curr_fn.blocks.append(self.alloc, .{ .name = blockname });
        const curr_block = self.currentBlock();
        try curr_block.scope_ids.append(self.alloc, 0);
    }

    pub fn appendBlock(self: *SLIR, name: ?String) !void {
        const curr_fn = self.currentFunction();
        const blockname: String = name orelse b: {
            const buf = try self.alloc.alloc(u8, 16);
            const slice = try std.fmt.bufPrint(buf, "%L{d}", .{self.getGuid()});
            break :b try self.intern.convert(slice);
        };
        try curr_fn.blocks.ensureUnusedCapacity(self.alloc, 1);
        const prev_block = self.currentBlock();
        curr_fn.blocks.appendAssumeCapacity(.{ .name = blockname });
        const curr_block = self.currentBlock();
        try curr_block.scope_ids.ensureUnusedCapacity(self.alloc, curr_fn.scope_depth);
        if (curr_fn.scope_depth > prev_block.scope_ids.items.len) {
            curr_block.scope_ids.appendSliceAssumeCapacity(prev_block.scope_ids.items);
            const diff = curr_fn.scope_depth - prev_block.scope_ids.items.len;
            curr_block.scope_ids.appendNTimesAssumeCapacity(0, diff);
        } else {
            curr_block.scope_ids.appendSliceAssumeCapacity(prev_block.scope_ids.items[0..curr_fn.scope_depth]);
            curr_block.scope_ids.items[curr_fn.scope_depth - 1] += 1;
        }
    }

    //Note for how scopes work, if we have some pattern A.B.C and matching to D.E.F (Is DEF > ABC?)
    //We match if the second is a subset of the first, starts the same.
    //Or, we match if all terms but the last are the same and the 2nd's final term if equal or less
    //0 < 0.0,      0 < 0.1,    0 < 1,      0.1 !< 1,       0 < 0
    //For top level unordered constructs, we have some special handling not shown here yet
    pub fn openScope(self: *SLIR) !void {
        const curr_fn = self.currentFunction();
        curr_fn.scope_depth += 1;
    }

    pub fn closeScope(self: *SLIR) !void {
        const curr_fn = self.currentFunction();
        curr_fn.scope_depth -= 1;
    }
};
