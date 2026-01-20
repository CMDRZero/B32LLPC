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
            _ = try types.deepCopy(partial, ir.slir.alloc);
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
            
        },
        else => {},
    }
    return null;
}

fn mustResolve(instr: Instr) bool {
    switch (instr.tag) {
        .type_unsigned_int,
        .type_floating_point,
        .type_signed_int,
        .qualify_type_const,
        .qualify_type_mut,
        .qualify_type_var,
        .qualify_type_view,
        //.named_value,
        .load_named_value,
        .ensure_is_type,
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
                        return instr;
                    }
                }
            }
        }
    }
    return null;
}