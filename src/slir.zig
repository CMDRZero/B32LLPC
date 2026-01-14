const std = @import("std");
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;

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
            const offset_value = std.math.add(u64, value, 2 * (std.math.maxInt(u32)+1)) catch return null;
            return @enumFromInt(offset_value);
        }

        pub fn fromGuid(guid: Guid) Reference {
            return @enumFromInt(@intFromEnum(guid));
        }

        pub fn fromString(string: String) Reference {
            return @enumFromInt(std.math.maxInt(u32) + 1 + @intFromEnum(string.tag));
        }

        pub fn fromAny(value: anytype) ?Reference {
            switch (@TypeOf(value)) {
                comptime_int => {
                    return .fromInt(value);
                },
                Guid => {
                    return .fromGuid(value);
                },
                String => {
                    return .fromString(value);
                },
                else => |T| {
                    if (comptime std.math.cast(u64, std.math.maxInt(T))) |_| return .fromInt(value);
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
        instrs: ArrayList(Instruction) = .empty,

        const Arg = struct {
            name: String,
            kind: Guid,
        };

        pub const Instruction = struct {
            tag: Tag,
            results: ArrayList(Reference),
            args: ArrayList(Reference),

            pub const Tag = enum {
                unsigned_int,
            };

            pub fn format(self: Instruction, writer: *std.Io.Writer) !void {
                if (self.results.items.len >= 1) {
                    try writer.print("{f}", .{self.results.items[0]});
                    for (self.results.items[1..]) |result| {
                        try writer.print(", {f}", .{result});
                    }
                }
                
                try writer.print(" = {t}", .{self.tag});
                
                if (self.args.items.len >= 1) {
                    try writer.print(" {f}", .{self.args.items[0]});
                    for (self.args.items[1..]) |arg| {
                        try writer.print(", {f}", .{arg});
                    }
                }
            }
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
        try writer.print("+--- SLIR\n", .{});
        try writer.print("| Next Guid: ({})\n", .{@intFromEnum(self.next_guid)});
        for (self.functions.items) |func| {
            try writer.print("| +--- fn {f} (", .{func.name});
            for (func.args.items) |arg| {
                try writer.print("{f}: %{d}, ", .{arg.name, arg.kind});
            }
            try writer.print(") -> {f}\n", .{func.resT});
            for (func.instrs.items) |instr| {
                try writer.print("| | {f}\n", .{instr});
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
};

pub const InParseSLIR = struct {
    source_file: [] u8,
    slir: SLIR,

    pub const SaveState = struct {
        source_file: [] u8,
        next_guid: SLIR.Guid,
        num_functions: usize,
    };


    pub fn getState(self: InParseSLIR) SaveState {
        return .{
            .source_file = self.source_file,
            .next_guid = self.slir.next_guid,
            .num_functions = self.slir.functions.items.len,
        };
    }

    pub fn setState(self: *InParseSLIR, state: SaveState) void {
        self.source_file = state.source_file;
        self.slir.next_guid = state.next_guid;
        self.slir.functions.shrinkRetainingCapacity(state.num_functions);
    }
};