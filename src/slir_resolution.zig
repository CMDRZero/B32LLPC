const std = @import("std");

const compute_pool = @import("compute_pool.zig");
const slir = @import("slir.zig");
const SLIR = slir.SLIR;
const Instr = SLIR.Function.Block.Instruction;
const slir_cons = @import("slir_construction.zig");
const MutSLIR = slir_cons.MutSLIR;
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
const String = StringInternPool.String;
const types = @import("types.zig");

pub fn resolve(ir: MutSLIR) !void {
    var main = getFunctionByName(ir, try ir.slir.intern.convert("Main")).?;
    try computeRef(ir, main.resT);
    for (main.blocks.items) |block| {
        for (block.instrs.items) |instr| {
            if (mustResolve(instr)) {
                if (!(instr.results.items.len >= 1)) std.debug.panic("Instruction `{}`, must have a result", .{instr});
                std.debug.assert(instr.results.items.len >= 1); //All must compute instructions must have a result even if unused
                try computeRef(ir, instr.results.items[0]);
            }
        }
    }
}

fn computeRef(ir: MutSLIR, target: SLIR.Reference) !void {
    var compute_queue: std.ArrayList(SLIR.Reference) = .empty;
    try compute_queue.append(ir.slir.alloc, target);
    errdefer {
        std.debug.print("Dependency queue is {any}\n", .{compute_queue.items});
    }
    while (compute_queue.items.len != 0) {
        const temp_goal = compute_queue.pop().?;
        const func, const block, const instr = getInstrByResult(ir, temp_goal).?;
        errdefer compute_queue.appendAssumeCapacity(temp_goal);
        const deps = try evaluateInstruction(ir, func, block, instr) orelse continue;
        compute_queue.appendAssumeCapacity(temp_goal);
        try compute_queue.ensureUnusedCapacity(ir.slir.alloc, deps.len);
        for (deps) |dep| {
            for (compute_queue.items) |item| if (dep == item) return error.cyclic;
            compute_queue.appendAssumeCapacity(dep);
        }
    }
}

//Return null to mean no dependencies
fn evaluateInstruction(ir: MutSLIR, func: *SLIR.Function, block: *SLIR.Function.Block, instr: *Instr) !?[]SLIR.Reference {
    switch (instr.tag) {
        .type_unsigned_int => {
            const root = try ir.slir.alloc.create(types.partial.Root);
            const bits = instr.args.items[0].toTaggedUnion().int;
            root.* = try .makeInteger(.{ .explicit = .{.bits = bits, .signed = false}}, null, .{});
            
            const partial: types.partial.Type = .fromRoot(root);
            // const heap_partial = try ir.slir.alloc.create(types.partial.Type);
            // heap_partial.* = partial;
            const heap_partial = try types.refDeepCopy(partial, ir.slir.alloc);
            
            const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(heap_partial));
            instr.args.items[0] = ref;
            instr.tag = .type_lit;
        },
        .type_signed_int => {
            const bits = instr.args.items[0].toTaggedUnion().int;
            const ref: SLIR.Reference = try makeInteger(ir, .{ 
                .explicit = .{
                    .bits = bits, 
                    .signed = true
                }, 
            }, .{},);
            instr.args.items[0] = ref;
            instr.tag = .type_lit;
        },
        .qualify_type_const => {
            _, _, const res_def = getInstrByResult(ir, instr.args.items[0]) orelse std.debug.panic("{}", .{instr});
            if (res_def.tag != .type_lit) return res_def.results.items;
            const partial = res_def.args.items[0].toTaggedUnion().compute_ref.toItem(ir.slir.computes.*).kind;
            const new_kind = try types.deepCopy(partial, ir.slir.alloc);
            std.debug.assert(new_kind.data == null or new_kind.data.? == .@"const");
            new_kind.data = .@"const";
            instr.tag = .type_lit;
            const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(new_kind));
            instr.args.items[0] = ref;
        },
        .qualify_type_view => {
            _, _, const res_def = getInstrByResult(ir, instr.args.items[0]).?;
            if (res_def.tag != .type_lit) return res_def.results.items;
            const partial = res_def.args.items[0].toTaggedUnion().compute_ref.toItem(ir.slir.computes.*).kind;
            const new_kind = try types.deepCopy(partial, ir.slir.alloc);
            std.debug.assert(new_kind.access == null or new_kind.access.? == .view or new_kind.access.? == .mut);
            new_kind.access = .view;
            instr.tag = .type_lit;
            const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(new_kind));
            instr.args.items[0] = ref;
        },
        .qualify_type_var => {
            _, _, const res_def = getInstrByResult(ir, instr.args.items[0]).?;
            if (res_def.tag != .type_lit) return res_def.results.items;
            const partial = res_def.args.items[0].toTaggedUnion().compute_ref.toItem(ir.slir.computes.*).kind;
            const new_kind = try types.deepCopy(partial, ir.slir.alloc);
            std.debug.assert(new_kind.data == null or new_kind.data.? == .@"const" or new_kind.data.? == .@"var");
            new_kind.data = .@"var";
            instr.tag = .type_lit;
            const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(new_kind));
            instr.args.items[0] = ref;
        },
        .qualify_type_mut => {
            _, _, const res_def = getInstrByResult(ir, instr.args.items[0]).?;
            if (res_def.tag != .type_lit) return res_def.results.items;
            const partial = res_def.args.items[0].toTaggedUnion().compute_ref.toItem(ir.slir.computes.*).kind;
            const new_kind = try types.deepCopy(partial, ir.slir.alloc);
            std.debug.assert(new_kind.access == null or new_kind.access.? == .mut);
            new_kind.access = .mut;
            instr.tag = .type_lit;
            const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(new_kind));
            instr.args.items[0] = ref;
        },
        .type_of => {
            _, _, const res_def = getInstrByResult(ir, instr.args.items[0]).?;
            switch (res_def.tag) {
                .type_lit => {
                    const root = try ir.slir.alloc.create(types.partial.Root);
                    root.* = .kind;
                    const partial: types.partial.Type = .fromRoot(root);
                    const heap_partial = try ir.slir.alloc.create(types.partial.Type);
                    heap_partial.* = partial;
                    const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(heap_partial));
                    instr.args.items[0] = ref;
                    instr.tag = .type_lit;
                },
                .value => {
                    instr.tag = .alias;
                    instr.args.items[0] = res_def.args.items[1];
                },
                .decimal_integer_lit => {
                    return res_def.results.items;
                },
                .int_lit => {
                    const val: std.math.big.int.Mutable = switch (res_def.args.items[0].toTaggedUnion().compute_ref.toItem(ir.slir.computes.*)) {
                        .int_small => |x| (try std.math.big.int.Managed.initSet(ir.slir.alloc, x)).toMutable(),
                        .int_big => |x| x.toMutable(),
                        else => unreachable,
                    };
                    const ref: SLIR.Reference = try makeInteger(ir, .implicit, .{ 
                        .alloc = ir.slir.alloc,
                        .low = val, 
                        .high = val,
                    });

                    instr.args.items[0] = ref;
                    instr.tag = .type_lit;
                },
                //return res_def.results.items;
                else => std.debug.panic("Bad tag `{t}` for typeof.", .{res_def.tag}),
            }
        },
        .decimal_integer_lit => {
            var int: std.math.big.int.Managed = try .initSet(ir.slir.alloc, 0);
            const str = instr.args.items[0].toTaggedUnion().string_ref.toStr(ir.slir.intern.*);
            var temp: std.math.big.int.Managed = try .init(ir.slir.alloc);
            for (str) |c| {
                switch (c) {
                    '0'...'9' => |x_| {
                        try temp.set(10);
                        try int.mul(&int, &temp);
                        try temp.set(x_ - '0');
                        try int.add(&int, &temp);
                    },
                    '_' => {},
                    else => std.debug.print("Bad symbol in intger `{c}`", .{c}),
                }
            }
            if (int.toInt(usize)) |x| {
                instr.args.items[0] = .fromCompute(try ir.slir.computes.convert(x));
            } else |_| {
                instr.args.items[0] = .fromCompute(try ir.slir.computes.convert(try types.deepCopy(&int, ir.slir.alloc)));
            }
            instr.tag = .int_lit;
        },
        .load_named_value => {
            const value = getNamedValue(func, block, instr.args.items[0].toTaggedUnion().string_ref) orelse @panic("Name no exisyt");
            instr.tag = .alias;
            instr.args.items[0] = value;
        },
        .value => {
            const arg0 = instr.args.items[1].toTaggedUnion();
            if (arg0 == .compute_ref) {
                return error.cyclic_resolution;
            } else if (arg0 == .instr_ref) {
                const oinstr = getInstrByResult(ir, instr.args.items[1]).?[2];
                if (oinstr.tag != .type_lit) {
                    std.debug.print("{f}\n", .{oinstr.*});
                    return oinstr.results.items;
                }
                instr.args.items[1] = oinstr.args.items[0];
            } else {
                std.debug.print("WARN: value with wierd type of {f}\n", .{instr});
                return error.bad;
            }
        },
        .ensure_cast => {
            const arg0 = instr.args.items[1].toTaggedUnion();
            if (arg0 == .compute_ref) {
                const item0 = arg0.compute_ref.toItem(ir.slir.computes.*);
                if (item0 == .kind and item0.kind.aggregate == .root and item0.kind.aggregate.root.* == .kind) {
                    instr.tag = .alias;
                    instr.args.shrinkRetainingCapacity(1);
                } else unreachable;
            } else if (arg0 == .instr_ref) {
                const oinstr = getInstrByResult(ir, instr.args.items[1]).?[2];
                if (oinstr.tag != .type_lit) return oinstr.results.items;
                instr.args.items[1] = oinstr.args.items[0];
                return &.{};
            } else {
                std.debug.print("WARN: cast of {}\n", .{instr});
                return error.bad;
            }
        },
        .ensure_is_type => {
            _, _, const res_def = getInstrByResult(ir, instr.args.items[0]).?;
            switch (res_def.tag) {
                .type_lit => {
                    instr.tag = .alias;
                },
                .value => {
                    // const valueref = res_def.args.items[0];
                    // std.debug.assert(value.args.items[0].toTaggedUnion().compute_ref.toItem(ir.slir.computes.*) == .kind);
                    instr.tag = .alias;
                    instr.args.items[0] = res_def.args.items[0];
                },
                .ensure_cast,
                .load_named_value => {
                    return res_def.results.items;
                },
                else => std.debug.panic("Bad tag `{t}` for ensure_is_type.", .{res_def.tag}),
            } 
        },
        else => {
            std.debug.print("WARN: Cannot resolve instruction {}.\n", .{instr.*});
            return error.unresolveable;
        },
    }
    return null;
}

fn getNamedValue(func: *SLIR.Function, origin: *SLIR.Function.Block, name: String) ?SLIR.Reference {
    const idx = origin - func.blocks.items.ptr;
    for (0..idx+1) |i| {
        const block = func.blocks.items[idx - i];
        for (block.instrs.items) |instr| {
            if (instr.tag == .named_value and instr.args.items[0].toTaggedUnion().string_ref.tag == name.tag) {
                return instr.args.items[1];
            }
        }
    }
    return null;
}

fn mustResolve(instr: Instr) bool {
    switch (instr.tag) {
        .type_unsigned_int,
        //.type_floating_point,
        .type_signed_int,
        .qualify_type_const,
        .qualify_type_mut,
        .qualify_type_var,
        .qualify_type_view,
        .load_named_value,
        .ensure_is_type,
        .value,
        .type_of,
        => return true,
        else => return false,
    }
}

fn makeInteger(
    ir: MutSLIR,
    backing: types.partial.Root.MakeIntegerBacking,
    defaults: struct {
        alloc: ?std.mem.Allocator = null,
        low: ?std.math.big.int.Mutable = null,
        high: ?std.math.big.int.Mutable = null,
    },
) !SLIR.Reference {
    const root = try ir.slir.alloc.create(types.partial.Root);
    root.* = try .makeInteger(backing, defaults.alloc, .{.low = defaults.low, .high = defaults.high});
    
    const partial: types.partial.Type = .fromRoot(root);
    const heap_partial = try types.refDeepCopy(partial, ir.slir.alloc);
    
    const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(heap_partial));
    return ref;
}

fn getFunctionByName(ir: MutSLIR, name: String) ?*SLIR.Function {
    for (ir.slir.functions.items) |*func| {
        if (func.name.tag == name.tag) {
            return func;
        }
    }
    return null;
}

fn getInstrByResult(ir: MutSLIR, result: SLIR.Reference) ?struct{*SLIR.Function, *SLIR.Function.Block, *Instr} {
    for (ir.slir.functions.items) |*func| {
        for (func.blocks.items) |*block| {
            for (block.instrs.items) |*instr| {
                for (instr.results.items) |res| {
                    if (res == result) {
                        if (instr.tag == .alias or instr.tag == .value) {
                            return getInstrByResult(ir, instr.args.items[0]);
                        }
                        return .{func, block, instr};
                    }
                }
            }
        }
    }
    return null;
}
