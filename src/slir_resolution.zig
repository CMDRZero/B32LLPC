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
const extra_fmt = @import("extra_fmt.zig");

const big_int = std.math.big.int;

const Type = types.partial.Type;
const RootType = types.partial.Root;
const Compute = compute_pool.ComputePool.Compute;
const ComputeItem = compute_pool.ComputePool.Item;

const deepCopy = types.deepCopy;
const refDeepCopy = types.refDeepCopy;

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
        std.debug.print("Dependency queue is {any}\n", .{compute_queue.items});
        const temp_goal = compute_queue.pop().?;
        const func, const block, const instr = getInstrByResult(ir, temp_goal).?;
        errdefer compute_queue.appendAssumeCapacity(temp_goal);
        const context: EvalCtx = .{
            .instr = instr,
            .block = block,
            .func = func,
        };
        const deps = try evaluateInstruction(ir, context) orelse continue;
        compute_queue.appendAssumeCapacity(temp_goal);
        try compute_queue.ensureUnusedCapacity(ir.slir.alloc, deps.len);
        for (deps) |dep| {
            for (compute_queue.items) |item| if (dep == item) return error.cyclic;
            compute_queue.appendAssumeCapacity(dep);
        }
    }
}

const EvalCtx = struct {
    instr: *Instr,
    block: *SLIR.Function.Block, 
    func: *SLIR.Function, 
};

fn evaluateInstruction(ir: MutSLIR, context: EvalCtx) !?[]SLIR.Reference {
    std.debug.print("Evaled: `{f}`\n", .{context.instr});
    defer std.debug.print("Into:   `{f}`\n{f}\n", .{context.instr, ir.slir});
    switch (context.instr.tag) { inline else => |tag| {
        const fn_name = comptime extra_fmt.comptimeLowercaseToLowerCamel(@tagName(tag));
        if (!std.meta.hasFn(eval_impl, fn_name)) {
            std.debug.panic("Call to unimplemented comptime evaluation `{s}`.", .{fn_name});
        }
        const func = @field(eval_impl, fn_name);
        return func(ir, context);
    }} 
}

// Some notes to self:
// typed_op_ means it has 2 returns, the value and the type
// op_ typically has 1 return and the comptime eval just turns it into typed_op_
// 
// 
// 
// 
// 

const eval_impl = struct {

    pub fn opTypeSignedInt(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        return genericOpTypeInteger(ir, ctx, true);
    }

    pub fn opTypeUnsignedInt(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        return genericOpTypeInteger(ir, ctx, false);
    }

    fn genericOpTypeInteger(ir: MutSLIR, ctx: EvalCtx, signed: bool) !?[]SLIR.Reference {
        const bits = ctx.instr.args.items[0].toTaggedUnion().int;
        const ref: SLIR.Reference = try makeInteger(
            ir,
            .{ .explicit = .{ .bits = bits, .signed = signed }},
            .{},
        );
        ctx.instr.args.items[0] = ref;
        ctx.instr.tag = .struct_type_lit;
        return null;
    }

    pub fn opTypeOf(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        const res_def = getInstrByResult(ir, ctx.instr.args.items[0]).?[2];
        if (isOperator(res_def.tag)) return res_def.results.items;
        switch (res_def.tag) {
            .struct_type_lit => {
                const root: RootType = .kind;
                const ref: SLIR.Reference = try refFromRootType(ir, root);
                ctx.instr.args.items[0] = ref;
                ctx.instr.tag = .struct_type_lit;
            },
            .typed_op_add, => {
                ctx.instr.tag = .struct_type_lit;
                ctx.instr.args.items[0] = res_def.results.items[1];
            },
            .struct_value => {
                ctx.instr.tag = .alias;
                ctx.instr.args.items[0] = res_def.args.items[1];
            },
            .struct_int_lit => {
                const val = switch (computeItemFromInstrArg(ir, res_def, 0)) {
                    .int_small => |x| 
                        (
                            try big_int.Managed.initSet(ir.slir.alloc, x)
                        ).toMutable(),
                    
                    .int_big => |x| 
                        x.toMutable(),
                    
                    else => unreachable,
                };
                const ref: SLIR.Reference = try makeInteger(ir, .implicit, .{
                    .alloc = ir.slir.alloc,
                    .low = val,
                    .high = val,
                });

                ctx.instr.args.items[0] = ref;
                ctx.instr.tag = .struct_type_lit;
            },
            else => try ir.slir.showError(ctx.instr.span, "Bad tag `{t}` for typeof.", .{res_def.tag}),
                //std.debug.panic(),
        }
        return null;
    }

    fn typeOf(ir: MutSLIR, res_def: *Instr) !SLIR.Reference {
        switch (res_def.tag) {
            .struct_type_lit => {
                const root: RootType = .kind;
                const ref: SLIR.Reference = try refFromRootType(ir, root);
                return ref;
            },
            .typed_op_add, => {
                return res_def.results.items[1];
            },
            .struct_value => {
                //return res_def.args.items[1];
                return getInstrByResult(ir, res_def.args.items[1]).?[2].args.items[0];
            },
            .struct_int_lit => {
                const val = switch (computeItemFromInstrArg(ir, res_def, 0)) {
                    .int_small => |x| 
                        (
                            try big_int.Managed.initSet(ir.slir.alloc, x)
                        ).toMutable(),
                    
                    .int_big => |x| 
                        x.toMutable(),
                    
                    else => unreachable,
                };
                const ref: SLIR.Reference = try makeInteger(ir, .implicit, .{
                    .alloc = ir.slir.alloc,
                    .low = val,
                    .high = val,
                });

                return ref;
            },
            else => std.debug.panic("Bad tag `{t}` for typeof.", .{res_def.tag}),
        }
        unreachable;
    }

    pub fn opDecimalIntegerLit(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        const str = stringFromInstrArg(ir, ctx.instr, 0);

        ctx.instr.args.items[0] = try genericIntegerLit(ir, ctx.instr.*, str, 10);

        ctx.instr.tag = .struct_int_lit;
        return null;
    }

    fn genericIntegerLit(ir: MutSLIR, instr: Instr, str: []const u8, base: comptime_int) !SLIR.Reference {
        const alloc = ir.slir.alloc;
        
        var int: big_int.Managed = try .initSet(alloc, 0);
        var temp: big_int.Managed = try .init(alloc);
        for (str) |c| {
            switch (c) {
                '0'...'9' => |x_| {
                    if (x_ >= '0'+base) {
                        try ir.slir.showError(instr.span, "Bad symbol in intger `{c}` for base {d}", .{c, base});
                        //std.debug.print("Bad symbol in intger `{c}` for base {d}", .{c, base});
                    }
                    try temp.set(base);
                    try int.mul(&int, &temp);
                    try temp.set(x_ - '0');
                    try int.add(&int, &temp);
                },
                'a'...'f' => |x_| {
                    if (x_ >= 'a'+base-10) {
                        try ir.slir.showError(instr.span, "Bad symbol in intger `{c}` for base {d}", .{c, base});
                        //std.debug.print("Bad symbol in intger `{c}` for base {d}", .{c, base});
                    }
                    try temp.set(base);
                    try int.mul(&int, &temp);
                    try temp.set(x_ - 'a' + 10);
                    try int.add(&int, &temp);
                },
                '_' => {},
                else => std.debug.print("Bad symbol in intger `{c}` for base {d}", .{c, base}),
            }
        }
        if (int.toInt(usize)) |x| {
            return try refFromCompute(ir, x);
        } else |_| {
            return try refFromComputeCopied(ir, int);
        }
    }

    pub fn qualifyTypeConst(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        return genericQualifyType(ir, ctx, .{ .data = .@"const" });
    }

    pub fn qualifyTypeVar(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        return genericQualifyType(ir, ctx, .{ .data = .@"var" });
    }

    pub fn qualifyTypeView(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        return genericQualifyType(ir, ctx, .{ .access = .view });
    }

    pub fn qualifyTypeMut(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        return genericQualifyType(ir, ctx, .{ .access = .mut });
    }

    const GenericQualifier = union (enum) {
        data: types.qualifier.Data, 
        access: types.qualifier.Access
    };
    fn genericQualifyType(ir: MutSLIR, ctx: EvalCtx, qualifier: GenericQualifier) !?[]SLIR.Reference {
        var res_def = getInstrByResult(ir, ctx.instr.args.items[0]).?[2];
        if (isOperator(res_def.tag)) return res_def.results.items;

        if (res_def.tag == .struct_value) {
            res_def = getInstrByResult(ir, res_def.args.items[0]).?[2];
        }

        const kind = computeItemFromInstrArg(ir, res_def, 0).kind;
        const new_kind = try deepCopy(kind, ir.slir.alloc);

        switch (qualifier) {
            .data => |new_data| {
                if(!isDataSubset(new_kind.data, new_data)) {
                    try ir.slir.showError(ctx.instr.span, "Cannot coerce data qualifier {?} to {t}", .{new_kind.data, new_data});
                }
                new_kind.data = new_data;
            },
            .access => |new_access| {
                if(!isAccessSubset(new_kind.access, new_access)) {
                    try ir.slir.showError(ctx.instr.span, "Cannot coerce access qualifier {?} to {t}", .{new_kind.access, new_access});
                }
                new_kind.access = new_access;
            },
        }
        
        ctx.instr.tag = .struct_type_lit;
        const ref = try refFromCompute(ir, new_kind);
        ctx.instr.args.items[0] = ref;
        return null;
    }

    pub fn opLoadNamedValue(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        _ = ir;
        const value = getNamedValue(ctx.func, ctx.block, ctx.instr.args.items[0].toTaggedUnion().string_ref) orelse @panic("Name no exisyt");
        ctx.instr.tag = .alias;
        ctx.instr.args.items[0] = value;
        return null;
    }

    //TODO: ???????
    pub fn ensureMayCast(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        const arg1 = ctx.instr.args.items[1].toTaggedUnion();
        if (arg1 == .compute_ref) {
            const item1 = arg1.compute_ref.toItem(ir.slir.computes.*);
            if (item1 == .kind) {
                switch (item1.kind.aggregate) {
                    .root => |root| switch (root.*) {
                        .kind => {
                            ctx.instr.tag = .alias;
                            ctx.instr.args.shrinkRetainingCapacity(1);
                        },
                        .integer => |res_int| {
                            const src_type = switch (try getArgType(ir, ctx, 0)) {
                                .dep => |deps| return deps,
                                .types => |lhs_type| lhs_type.kind,
                            };
                            switch (src_type.aggregate) {
                                .root => |src_root| switch (src_root.*) {
                                    .integer => |src_int| {
                                        const src_low = try src_int.lowBound(ir.slir.alloc);
                                        const src_high = try src_int.highBound(ir.slir.alloc);

                                        var res_low = try res_int.lowBound(ir.slir.alloc);
                                        var res_high = try res_int.highBound(ir.slir.alloc);
                                        try res_low.sub(&src_low, &res_low);
                                        if (!(res_low.isPositive() or res_low.eqlZero())) {
                                            try ir.slir.showError(ctx.instr.span, "Cannot coerce {f} into narrower range {f}\n", .{src_root, root});
                                        }

                                        try res_high.sub(&res_high, &src_high);
                                        if (!(res_high.isPositive() or res_high.eqlZero())) {
                                            try ir.slir.showError(ctx.instr.span, "Cannot coerce {f} into narrower range {f}\n", .{src_root, root});
                                        }
                                    },
                                    else => try ir.slir.showError(ctx.instr.span, "Cannot coerce Non-Integer to integer\n", .{}),
                                }
                            }
                            //try ir.slir.showError(ctx.instr.span, "Unimplemented cast check\n", .{});
                        }
                    },
                }
                // if (item1.kind.aggregate == .root and item1.kind.aggregate.root.* == .kind) {
                    
                // } else {

                    
                //     //std.debug.print("WARN: Unimplemented cast check\n", .{});
                // }
            } else {
                try ir.slir.showError(ctx.instr.span, "Result is compute of nontype {}\n", .{item1});
                //std.debug.print("WARN: Result is compute of nontype {}\n", .{item0});
            }
        } else if (arg1 == .instr_ref) {
            const oinstr = getInstrByResult(ir, ctx.instr.args.items[1]).?[2];
            if (oinstr.tag != .struct_type_lit) return oinstr.results.items;
            ctx.instr.args.items[1] = oinstr.args.items[0];
            return &.{};
        } else {
            std.debug.print("WARN: cast of {}\n", .{ctx.instr});
            return error.bad;
        }
        return null;
    }

    pub fn ensureIsType(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        const res_def = getInstrByResult(ir, ctx.instr.args.items[0]).?[2];
        if (isOperator(res_def.tag)) return res_def.results.items;

        switch (res_def.tag) {
            .struct_type_lit => {
                ctx.instr.tag = .alias;
            },
            .struct_value => {
                ctx.instr.tag = .alias;
                ctx.instr.args.items[0] = res_def.args.items[0];
            },
            else => std.debug.panic("Bad tag `{t}` for ensure_is_type.", .{res_def.tag}),
        }
        return null;
    }

    const TypesOrDependencies = union (enum) {
        dep: []SLIR.Reference,
        types: struct {ComputeItem, ComputeItem},
    };
    fn getBinaryTypes(ir: MutSLIR, ctx: EvalCtx) !TypesOrDependencies {
        const lhs_instr = getInstrByResult(ir, ctx.instr.args.items[0]).?[2];
        if (isOperator(lhs_instr.tag)) return .{ .dep = lhs_instr.results.items};
        
        const rhs_instr = getInstrByResult(ir, ctx.instr.args.items[1]).?[2];
        if (isOperator(rhs_instr.tag)) return .{ .dep = rhs_instr.results.items};

        const lhs_type_ref = try typeOf(ir, lhs_instr);
        const rhs_type_ref = try typeOf(ir, rhs_instr);

        const lhs_type = computeItemFromReference(ir, lhs_type_ref);
        const rhs_type = computeItemFromReference(ir, rhs_type_ref);

        return .{ .types = .{lhs_type, rhs_type} };
    }

    const TypeOrDependency = union (enum) {
        dep: []SLIR.Reference,
        types: ComputeItem,
    };
    fn getArgType(ir: MutSLIR, ctx: EvalCtx, idx: usize) !TypeOrDependency {
        const lhs_instr = getInstrByResult(ir, ctx.instr.args.items[idx]).?[2];
        if (isOperator(lhs_instr.tag)) return .{ .dep = lhs_instr.results.items};

        const lhs_type_ref = try typeOf(ir, lhs_instr);

        const lhs_type = computeItemFromReference(ir, lhs_type_ref);

        return .{ .types = lhs_type };
    }

    ///Doesnt actually perform the add at comptime, just computes the type of the result
    pub fn opAdd(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        const lhs_type, 
        const rhs_type = switch (try getBinaryTypes(ir, ctx)) {
            .dep => |deps| return deps,
            .types => |lhs_rhs| lhs_rhs,
        };

        const alloc = ir.slir.alloc;
        switch (lhs_type.kind.aggregate) {
            .root => |lhs_root| switch (lhs_root.*) {
                .integer => |lhs_int| switch (rhs_type.kind.aggregate) {
                    .root => |rhs_root| switch (rhs_root.*) {
                        .integer => |rhs_int| {
                            const lhs_low = try lhs_int.lowBound(alloc);
                            const lhs_high = try lhs_int.highBound(alloc);

                            const rhs_low = try rhs_int.lowBound(alloc);
                            const rhs_high = try rhs_int.highBound(alloc);
                            
                            var res_low: big_int.Managed = try .init(alloc);
                            try res_low.add(&lhs_low, &rhs_low);

                            var res_high: big_int.Managed = try .init(alloc);
                            try res_high.add(&lhs_high, &rhs_high);

                            const ref: SLIR.Reference = try makeInteger(ir, .implicit, .{
                                .alloc = alloc,
                                .low = res_low.toMutable(),
                                .high = res_high.toMutable(),
                            });

                            try ctx.instr.results.append(alloc, ref);
                        },
                        else => unreachable,
                    },
                    //else => unreachable,
                },
                else => unreachable,
            },
            //else => unreachable,
        }
            

        //std.debug.print("Umm, heres some info?\n{f}\n{f}\n", .{lhs_type, rhs_type});
        ctx.instr.tag = .typed_op_add;
        return null;
    }

    pub fn opStructValue(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        const res_def = getInstrByResult(ir, ctx.instr.args.items[1]).?[2];
        if (isOperator(res_def.tag)) return res_def.results.items;
        ctx.instr.tag = .struct_value;
        return null;
    }

    //////////////////////////////////////////////////////////////////////
    //

    pub fn structIntLit(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        _ = ir;
        _ = ctx;
        return null;
    }

    pub fn structTypeLit(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        _ = ir;
        _ = ctx;
        return null;
    }

    pub fn structValue(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        _ = ir;
        _ = ctx;
        return null;
    }

    pub fn typedOpAdd(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        _ = ir;
        _ = ctx;
        return null;
    }

    //
    //////////////////////////////////////////////////////////////////////

    pub fn template(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.Reference {
        _ = ir;
        _ = ctx;
        return null;
    }

    fn isOperator(tag: Instr.Tag) bool {
        return std.mem.startsWith(u8, @tagName(tag), "op") or std.mem.startsWith(u8, @tagName(tag), "ensure");
    }

    fn refFromComputeCopied(ir: MutSLIR, val: anytype) !SLIR.Reference {
        return refFromCompute(ir, try refDeepCopy(val, ir.slir.alloc));
    }

    fn refFromCompute(ir: MutSLIR, val: anytype) !SLIR.Reference {
        return .fromCompute(try ir.slir.computes.convert(val));
    }

    fn refFromType(ir: MutSLIR, kind: Type) !SLIR.Reference {
        return refFromComputeCopied(ir, kind);
    }

    fn refFromRootType(ir: MutSLIR, root: RootType) !SLIR.Reference {
        return refFromType(ir, .fromRoot(try refDeepCopy(root, ir.slir.alloc)));
    }

    fn computeItemFromReference(ir: MutSLIR, ref: SLIR.Reference) ComputeItem {
        return ref.toTaggedUnion().compute_ref.toItem(ir.slir.computes.*);
    }

    fn computeItemFromInstrArg(ir: MutSLIR, instr: *Instr, arg_id: usize) ComputeItem {
        return computeItemFromReference(ir, instr.args.items[arg_id]);
    }

    fn stringFromInstrArg(ir: MutSLIR, instr: *Instr, arg_id: usize) []const u8 {
        return instr.args.items[arg_id].toTaggedUnion().string_ref.toStr(ir.slir.intern.*);
    }

    fn isDataSubset(lhs: ?types.qualifier.Data, rhs: types.qualifier.Data) bool {
        return (lhs orelse return true).isSubset(rhs);
    }

    fn isAccessSubset(lhs: ?types.qualifier.Access, rhs: types.qualifier.Access) bool {
        return (lhs orelse return true).isSubset(rhs);
    }
};

//Return null to mean no dependencies
fn _evaluateInstruction(ir: MutSLIR, func: *SLIR.Function, block: *SLIR.Function.Block, instr: *Instr) !?[]SLIR.Reference {
    switch (instr.tag) {
        .op_load_named_value => {
            const value = getNamedValue(func, block, instr.args.items[0].toTaggedUnion().string_ref) orelse @panic("Name no exisyt");
            instr.tag = .alias;
            instr.args.items[0] = value;
        },
        .struct_value => {
            const arg0 = instr.args.items[1].toTaggedUnion();
            if (arg0 == .compute_ref) {
                return error.cyclic_resolution;
            } else if (arg0 == .instr_ref) {
                const oinstr = getInstrByResult(ir, instr.args.items[1]).?[2];
                if (oinstr.tag != .struct_type_lit) {
                    std.debug.print("{f}\n", .{oinstr.*});
                    return oinstr.results.items;
                }
                instr.args.items[1] = oinstr.args.items[0];
            } else {
                std.debug.print("WARN: value with wierd type of {f}\n", .{instr});
                return error.bad;
            }
        },
        .ensure_is_type => {
            
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
    for (0..idx + 1) |i| {
        const block = func.blocks.items[idx - i];
        if (!blockPreceeds(block, origin)) continue;
        for (block.instrs.items) |instr| {
            if (instr.tag == .info_named_value and instr.args.items[0].toTaggedUnion().string_ref.tag == name.tag) {
                return instr.args.items[1];
            }
        }
    }
    return null;
}

fn blockPreceeds(lhs: SLIR.Function.Block, rhs: *SLIR.Function.Block) bool {
    if (lhs.scope_ids.items.len > rhs.scope_ids.items.len) return false;
    for (rhs.scope_ids.items[0..lhs.scope_ids.items.len - 1], 0..) |r_id, i| {
        const l_id = lhs.scope_ids.items[i];
        if (l_id != r_id) return false;
    }
    const i = lhs.scope_ids.items.len - 1;
    const r_id = rhs.scope_ids.items[i];
    const l_id = lhs.scope_ids.items[i];
    return l_id <= r_id;
}

fn mustResolve(instr: Instr) bool {
    switch (instr.tag) {
        .op_type_unsigned_int,
        //.type_floating_point,
        .op_type_signed_int,
        .qualify_type_const,
        .qualify_type_mut,
        .qualify_type_var,
        .qualify_type_view,
        .op_load_named_value,
        .ensure_is_type,
        .struct_value,
        .op_type_of,
        .ensure_may_cast,
        => return true,
        else => return false,
    }
}

fn makeInteger(
    ir: MutSLIR,
    backing: types.partial.Root.MakeIntegerBacking,
    defaults: struct {
        alloc: ?std.mem.Allocator = null,
        low: ?big_int.Mutable = null,
        high: ?big_int.Mutable = null,
    },
) !SLIR.Reference {
    const root = try ir.slir.alloc.create(types.partial.Root);
    root.* = try .makeInteger(backing, defaults.alloc, .{ .low = defaults.low, .high = defaults.high });

    const partial: types.partial.Type = .fromRoot(root);
    const heap_partial = try refDeepCopy(partial, ir.slir.alloc);

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

fn getInstrByResult(ir: MutSLIR, result: SLIR.Reference) ?struct { *SLIR.Function, *SLIR.Function.Block, *Instr } {
    for (ir.slir.functions.items) |*func| {
        for (func.blocks.items) |*block| {
            for (block.instrs.items) |*instr| {
                for (instr.results.items) |res| {
                    if (res == result) {
                        if (instr.tag == .alias){// or instr.tag == .struct_value) {
                            return getInstrByResult(ir, instr.args.items[0]);
                        }
                        return .{ func, block, instr };
                    }
                }
            }
        }
    }
    return null;
}

fn add(ir: MutSLIR, lhs: ComputeItem, rhs: ComputeItem) !Compute {
    const alloc = ir.slir.alloc;
    switch (lhs) {
        .int_small => |x| switch (rhs) {
            .int_small => |y| {
                return try ir.slir.computes.convert(x + y);
            },
            .int_big => |y| {
                var result: big_int.Managed = .init(alloc);
                try result.addScalar(y, x);
                return try ir.slir.computes.convert(try refDeepCopy(result, alloc));
            },
            else => unreachable,
        },
        .int_big => |x| switch (rhs) {
            .int_small => |y| {
                var result: big_int.Managed = .init(alloc);
                try result.addScalar(x, y);
                return try ir.slir.computes.convert(try refDeepCopy(result, alloc));
            },
            .int_big => |y| {
                var result: big_int.Managed = .init(alloc);
                try result.add(x, y);
                return try ir.slir.computes.convert(try refDeepCopy(result, alloc));
            },
            else => unreachable,
        },
        else => unreachable,
    }
}