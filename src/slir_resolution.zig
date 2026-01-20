const std = @import("std");

const types = @import("types.zig");
const slir = @import("slir.zig");
const slir_cons = @import("slir_construction.zig");
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
const compute_pool = @import("compute_pool.zig");


const SLIR = slir.SLIR;
const MutSLIR = slir_cons.MutSLIR;
const String = StringInternPool.String;

const Instr = SLIR.Function.Block.Instruction;

pub fn resolve(ir: MutSLIR) !void {
    var main = getFunctionByName(ir, try ir.slir.intern.convert("Main")).?;
    try computeRef(ir, main.resT);
    for (main.blocks.items) |block| {
        for (block.instrs.items) |instr| {
            if (mustResolve(instr)) {
                std.debug.assert(instr.results.items.len >= 1); //All must compute instructions must have a result even if unused
                try computeRef(ir, instr.results.items[0]);
            }
        }
    }
}

fn computeRef(ir: MutSLIR, target: SLIR.Reference) !void {
    var compute_queue: std.ArrayList(SLIR.Reference) = .empty;
    try compute_queue.append(ir.slir.alloc, target);
    while (compute_queue.items.len != 0) {
        const temp_goal = compute_queue.pop().?;
        const instr = getInstrByResult(ir, temp_goal).?;
        const deps = try evaluateInstruction(ir, instr) orelse continue;
        try compute_queue.append(ir.slir.alloc, temp_goal);
        try compute_queue.ensureUnusedCapacity(ir.slir.alloc, deps.len);
        for (deps) |dep| {
            for (compute_queue.items) |item| if (dep == item) return error.cyclic;
            compute_queue.appendAssumeCapacity(dep);
        }
    }
}

//Return null to mean no dependencies
fn evaluateInstruction(ir: MutSLIR, instr: *Instr) !?[]SLIR.Reference {
    switch (instr.tag) {
        .type_unsigned_int => {
            const root = try ir.slir.alloc.create(types.partial.Root);
            root.* = .{ .integer = .{.bits = instr.args.items[0].toTaggedUnion().int, .signed = false} };
            const partial: types.partial.Type = .fromRoot(root);
            const heap_partial = try ir.slir.alloc.create(types.partial.Type);
            heap_partial.* = partial;
            const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(heap_partial));
            instr.args.items[0] = ref;
            instr.tag = .type_lit;
        },
        .type_signed_int => {
            const root = try ir.slir.alloc.create(types.partial.Root);
            root.* = .{ .integer = .{.bits = instr.args.items[0].toTaggedUnion().int, .signed = true} };
            const partial: types.partial.Type = .fromRoot(root);
            const heap_partial = try ir.slir.alloc.create(types.partial.Type);
            heap_partial.* = partial;
            const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(heap_partial));
            instr.args.items[0] = ref;
            instr.tag = .type_lit;
            
        },
        .qualify_type_const => {
            const res_def = getInstrByResult(ir, instr.args.items[0]).?;
            if (res_def.tag != .type_lit) return res_def.results.items;
            const partial = res_def.args.items[0].toTaggedUnion().compute_ref.toItem(ir.slir.computes.*).kind;
            const new_kind = try types.deepCopy(partial, ir.slir.alloc);
            new_kind.data = .@"const";
            instr.tag = .type_lit;
            const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(new_kind));
            instr.args.items[0] = ref;
        },
        .qualify_type_view => {
            const res_def = getInstrByResult(ir, instr.args.items[0]).?;
            if (res_def.tag != .type_lit) return res_def.results.items;
            const partial = res_def.args.items[0].toTaggedUnion().compute_ref.toItem(ir.slir.computes.*).kind;
            const new_kind = try types.deepCopy(partial, ir.slir.alloc);
            new_kind.access = .@"view";
            instr.tag = .type_lit;
            const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(new_kind));
            instr.args.items[0] = ref;
        },
        .type_of => {
            const res_def: *Instr = getInstrByResult(ir, instr.args.items[0]).?;
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
                    const root = try ir.slir.alloc.create(types.partial.Root);
                    const val: std.math.big.int.Mutable = switch (res_def.args.items[0].toTaggedUnion().compute_ref.toItem(ir.slir.computes.*)) {
                        .int_small => |x| (try std.math.big.int.Managed.initSet(ir.slir.alloc, x)).toMutable(),
                        .int_big => |x| x.toMutable(),
                        else => unreachable,
                    };
                    root.* = .{ .integer = .{.bits = null, .signed = false, .low = val, .high = val} };
                    const partial: types.partial.Type = .fromRoot(root);
                    const heap_partial = try ir.slir.alloc.create(types.partial.Type);
                    heap_partial.* = partial;
                    const ref: SLIR.Reference = .fromCompute(try ir.slir.computes.convert(heap_partial));
                    instr.args.items[0] = ref;
                    instr.tag = .type_lit;
                },
                //return res_def.results.items;
                else => std.debug.panic("Bad tag `{t}` for typeof.", .{res_def.tag})
            }
        },
        .decimal_integer_lit => {
            var int: std.math.big.int.Managed = try .initSet(ir.slir.alloc, 0);
            const str = instr.args.items[0].toTaggedUnion().string_ref.toStr(ir.slir.intern.*);
            var temp: std.math.big.int.Managed = try .init(ir.slir.alloc);
            for (0..str.len) |i| {
                const c = str[str.len - 1 - i];
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
        else => std.debug.panic("Cannot resolve instruction {}.", .{instr.*}),
    }
    return null;
}

fn mustResolve(instr: Instr) bool {
    switch (instr.tag) {
        .type_unsigned_int,
        //.type_floating_point,
        .type_signed_int,
        .qualify_type_const,
        //.qualify_type_mut,
        //.qualify_type_var,
        .qualify_type_view,
        //.named_value,
        //.load_named_value,
        //.ensure_is_type,
        .type_of,
             => return true,
        else => return false,
    }
}

fn getFunctionByName(ir: MutSLIR, name: String) ?*SLIR.Function {
    for (ir.slir.functions.items) |*func| {
        if (func.name.tag == name.tag) {
            return func;
        }
    }
    return null;
}

fn getInstrByResult(ir: MutSLIR, result: SLIR.Reference) ?*Instr {
    for (ir.slir.functions.items) |*func| {
        for (func.blocks.items) |*block| {
            for (block.instrs.items) |*instr| {
                for (instr.results.items) |res| {
                    if (res == result) {
                        if (instr.tag == .alias){
                            return getInstrByResult(ir, instr.args.items[0]);
                        }
                        return instr;
                    }
                }
            }
        }
    }
    return null;
}