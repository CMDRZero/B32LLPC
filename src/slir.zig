const std = @import("std");
const ArrayList = std.ArrayList;
const mem = std.mem;
const Allocator = mem.Allocator;

const source_display = @import("source_display.zig");
// const ComputePool = @import("compute_pool.zig").ComputePo
const compute = @import("new_compute_pool.zig").compute;
// const Compute = ComputePool.Compute;
// const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
// const String = StringInternPool.String;
const tokenizer = @import("tokenizer.zig");

const warningsAreErrors = false;
const censuresAreErrors = true;

pub const SLIR = struct {
    alloc: Allocator,
    compute_pool: *compute.Pool,
    next_guid: Guid,
    functions: ArrayList(Function),

    pub const String = compute.Item(.bytes);
    pub const AnyItem = compute.AnyItem;

    pub fn showWarning(self: *SLIR, span: Span, comptime message: []const u8, args: anytype) !void {
        var disp_span: source_display.State.Span = .fromSlice(span.slice);
        var writer: std.Io.Writer.Allocating = .init(self.alloc);
        defer writer.deinit();
        try disp_span.display(&writer.writer, "WARNING: "++message, args);
        std.debug.print("WARNING:\n{s}\n", .{writer.written()});
        if (warningsAreErrors) return error.warning;
    }

    pub fn showCensure(self: *SLIR, span: Span, comptime message: []const u8, args: anytype) !void {
        var disp_span: source_display.State.Span = .fromSlice(span.slice);
        var writer: std.Io.Writer.Allocating = .init(self.alloc);
        defer writer.deinit();
        try disp_span.display(&writer.writer, "CENSURE: "++message, args);
        std.debug.print("CENSURE:\n{s}\n", .{writer.written()});
        if (censuresAreErrors) return error.censure;
    }

    pub fn showError(self: *SLIR, span: Span, comptime message: []const u8, args: anytype) !void {
        var disp_span: source_display.State.Span = .fromSlice(span.slice);
        var writer: std.Io.Writer.Allocating = .init(self.alloc);
        defer writer.deinit();
        try disp_span.display(&writer.writer, "ERROR: "++message, args);
        std.debug.print("{s}\nSPAN: {f}", .{writer.written(), disp_span});
        return error.@"error";
    }

    pub const Guid = enum(u32) {
        _,
        const init: Guid = @enumFromInt(1);
    };
    
    pub const Span = struct {
        slice: []u8,

        pub fn format(self: Span, writer: *std.Io.Writer) !void {
            const print_span: source_display.State.Span = .fromSlice(self.slice);
            try writer.print("{f}", .{print_span});
        }
    };

    pub const Function = struct {
        name: String,
        args: ArrayList(Arg),
        resT: AnyItem,
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
                results: ArrayList(AnyItem),
                args: ArrayList(AnyItem),
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
                    
                    op_slice_or_range_start,
                    op_slice_or_range_start_end,
                    op_slice_or_range_length,
                    
                    op_slice_start,
                    op_slice_start_end,
                    op_slice_length,
                    
                    op_range_start,
                    op_range_start_end,
                    op_range_length,

                    op_struct_value,
                    struct_value,

                    op_type_of,

                    op_cmp,

                    ensure_is_type,
                    ensure_may_cast,

                    qualify_type_const,
                    qualify_type_var,
                    qualify_type_view,
                    qualify_type_mut,

                    op_type_signed_int,
                    op_type_unsigned_int,
                    op_type_floating_point,
                    op_type_type,

                    op_decimal_integer_lit,

                    struct_int_lit,
                    struct_type_lit,

                    typed_value,

                    info_named_value,
                    op_load_named_value,

                    @"unreachable",
                    
                    /// eg) prove %0, "<", %1, "Slice may be out of bounds"
                    prove,

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
                    var buf: [1024]u8 = undefined;
                    var buf_writer: std.Io.Writer.Allocating = .initOwnedSlice(std.mem.Allocator.failing, &buf);
                    var bwriter = &buf_writer.writer;

                    if (self.results.items.len >= 1) {
                        try bwriter.print("{f}", .{self.results.items[0]});
                        for (self.results.items[1..]) |result| {
                            try bwriter.print(", {f}", .{result});
                        }
                        try bwriter.print(" = ", .{});
                    }
                    try bwriter.print("{t}", .{self.tag});
                    if (self.args.items.len >= 1) {
                        try bwriter.print(" {f}", .{self.args.items[0]});
                        for (self.args.items[1..]) |arg| {
                            try bwriter.print(", {f}", .{arg});
                        }
                    }
                    const written = buf_writer.written();
                    try writer.print("{s}", .{written});
                    const span_padding = 90;
                    if (written.len < span_padding) {
                        try writer.splatByteAll(' ', span_padding - written.len);
                    }
                    try writer.print(" Span: {f}", .{self.span});
                }
            };
        };
    };

    pub fn init(alloc: Allocator) SLIR {
        return .{
            .alloc = alloc,
            .compute_pool = .init(alloc),
            .next_guid = .init,
            .functions = .empty,
        };
    }

    pub fn format(self: SLIR, writer: *std.Io.Writer) !void {
        var refd_guids: ArrayList(Guid) = .empty;
        for (self.functions.items) |*func| {
            for (func.blocks.items) |*block| {
                for (block.instrs.items) |*instr| {
                    for (instr.args.items) |res| {
                        if (res.hasTag(.typed_value)) |tv| {
                            if (tv.value.hasTag(.instr_ref)) |guid|
                                refd_guids.append(self.alloc, guid) catch unreachable;
                        }
                        if (res.hasTag(.instr_ref)) |guid|
                            refd_guids.append(self.alloc, guid) catch unreachable;
                    }
                }
            }
        }

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
                    if (instr.tag == .struct_type_lit) {
                        if (instr.results.items[0].hasTag(.typed_value)) |tv| {
                            for (refd_guids.items) |ref_guid| {
                                if (ref_guid == tv.value.as(.instr_ref)) break;
                            } else continue;
                        }
                    }
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
        const blockname: String = (try self.compute_pool.cvtStr("OnInstance"));
        try curr_fn.blocks.append(self.alloc, .{ .name = blockname });
    }

    pub fn createEntry(self: *SLIR) !void {
        const curr_fn = self.currentFunction();
        std.debug.assert(curr_fn.scope_depth == 1);
        const blockname: String = try self.compute_pool.cvtStr("Entry");
        try curr_fn.blocks.append(self.alloc, .{ .name = blockname });
        const curr_block = self.currentBlock();
        try curr_block.scope_ids.append(self.alloc, 0);
    }

    pub fn appendBlock(self: *SLIR, name: ?String) !void {
        const curr_fn = self.currentFunction();
        const blockname: String = name orelse b: {
            const buf = try self.alloc.alloc(u8, 16);
            const slice = try std.fmt.bufPrint(buf, "%L{d}", .{self.getGuid()});
            break :b try self.compute_pool.cvtStr(slice);
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
