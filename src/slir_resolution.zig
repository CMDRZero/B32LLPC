const std = @import("std");

const compute_pool = @import("compute_pool.zig");
const compute = new_compute_pool.compute;
const new_compute_pool = @import("new_compute_pool.zig");
const slir = @import("slir.zig");
const SLIR = slir.SLIR;
const Instr = SLIR.Function.Block.Instruction;
const slir_cons = @import("slir_construction.zig");
const MutSLIR = slir_cons.MutSLIR;
//const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
const String = SLIR.String;
const types = @import("types.zig");
const new_types = @import("new_types.zig");
const extra_fmt = @import("extra_fmt.zig");

const big_int = std.math.big.int;

const Type = new_types.partial.Type;
const RootType = new_types.partial.Root;
// const Compute = compute_pool.ComputePool.Compute;
// const SLIR.AnyItem = compute_pool.ComputePool.Item;

///Symbolic type, must always be null, but has some extra info to allow type coercion
const Null = ?[]SLIR.AnyItem;

const deepCopy = types.deepCopy;
const refDeepCopy = types.refDeepCopy;

var canon_type_type: compute.Item(.kind) = undefined;

pub fn resolve(ir: MutSLIR) !void {
    try initTypeType();
    var main = getFunctionByName(ir, try ir.slir.compute_pool.cvtStr("Main")).?;
    try computeRef(ir, main.resT);
    for (main.blocks.items) |block| {
        for (block.instrs.items) |instr| {
            if (mustResolve(instr)) {
                if (!(instr.results.items.len >= 1)) std.debug.panic("Instruction `{}`, must have a result", .{instr});
                std.debug.assert(instr.results.items.len >= 1); //All must-compute instructions must have a result even if unused
                try computeRef(ir, instr.results.items[0]);
            }
        }
    }
}

fn initTypeType() !void {
    const root_type_type: RootType = .kind;
    const type_type: Type = .{
        .access = .view, 
        .data = .@"const", 
        .pack_as = 0, .bitalign = 0, 
        .aggregate = .{ 
            .root = try .fromAuto(root_type_type) 
        }
    };
    canon_type_type = try .fromAuto(type_type);
}

fn computeRef(ir: MutSLIR, target: SLIR.AnyItem) !void {
    var compute_queue: std.ArrayList(SLIR.AnyItem) = .empty;
    try compute_queue.append(ir.slir.alloc, target);
    errdefer {
        std.debug.print("Dependency queue is {any}\n", .{compute_queue.items});
    }
    while (compute_queue.items.len != 0) {
        //std.debug.print("Dependency queue is {any}\n", .{compute_queue.items});
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

fn evaluateInstruction(ir: MutSLIR, context: EvalCtx) !?[]SLIR.AnyItem {
    //std.debug.print("Evaled: `{f}`\n", .{context.instr});
    //defer std.debug.print("Into:   `{f}`\n{f}\n", .{context.instr, ir.slir});
    switch (context.instr.tag) { inline else => |tag| {
        const fn_name = comptime extra_fmt.comptimeLowercaseToLowerCamel(@tagName(tag));
        if (!std.meta.hasFn(eval_impl, fn_name)) {
            std.debug.panic("Call to unimplemented comptime evaluation `{s}`.", .{fn_name});
        }
        const func = @field(eval_impl, fn_name);
        return func(ir, context);
    }} 
}

const eval_impl = struct {
    const TypeRef = compute.Item(.kind);
    const BigIntRef = compute.Item(.integer_big);

    pub fn opTypeSignedInt(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        return genericOpTypeInteger(ir, ctx, true);
    }

    pub fn opTypeUnsignedInt(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        return genericOpTypeInteger(ir, ctx, false);
    }

    fn genericOpTypeInteger(ir: MutSLIR, ctx: EvalCtx, signed: bool) !Null {
        const bits = ctx.instr.args.items[0].toTagged().integer_usize;
        const value: TypeRef = try makeIntegerType(
            ir,
            .{ .explicit = .{ .bits = bits, .signed = signed }},
            .{},
        );
        ctx.instr.args.items[0] = try refTypedValue(canon_type_type, value.forget(), ir.slir.alloc);
        ctx.instr.tag = .struct_type_lit;
        ctx.instr.results.items[0] = try refTypedValue(canon_type_type, ctx.instr.results.items[0], ir.slir.alloc);
        return null;
    }

    pub fn opTypeType(ir: MutSLIR, ctx: EvalCtx) !Null {
        try ctx.instr.args.append(ir.slir.alloc, try refTypedValue(canon_type_type, canon_type_type.forget(), ir.slir.alloc));
        ctx.instr.tag = .struct_type_lit;
        ctx.instr.results.items[0] = try refTypedValue(canon_type_type, ctx.instr.results.items[0], ir.slir.alloc);
        return null;
    }

    pub fn opTypeOf(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        const res_def = getInstrByResult(ir, ctx.instr.args.items[0]).?[2];
        if (res_def.results.items[0].hasTag(.typed_value) == null) {
            return res_def.results.items;
        }
        ctx.instr.tag = .struct_type_lit;
        ctx.instr.args.items[0] = try typeOf(res_def, ir.slir.alloc);
        ctx.instr.results.items[0] = try refTypedValue(canon_type_type, ctx.instr.results.items[0], ir.slir.alloc);
        ctx.instr.args.shrinkRetainingCapacity(1);
        return null;
    }

    fn typeOf(res_def: *Instr, alloc: std.mem.Allocator) !SLIR.AnyItem {
        std.debug.assert(res_def.results.items.len == 1);
        std.debug.assert(res_def.results.items[0].toTagged() == .typed_value);
        const kind: TypeRef = res_def.results.items[0].as(.typed_value).kind;
        return try refTypedValue(canon_type_type, kind.forget(), alloc);
    }

    pub fn opDecimalIntegerLit(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        const str = ctx.instr.args.items[0].toTagged().bytes;
        //const str = stringFromInstrArg(ir, ctx.instr, 0);

        ctx.instr.args.items[0] = try genericIntegerLit(ir, ctx.instr.*, str.*, 10);

        ctx.instr.tag = .struct_int_lit;

        const value_ref = ctx.instr.args.items[0].as(.integer_big);
        const kind = try makeIntegerType(ir, .implicit, .{.low = value_ref.*, .high = value_ref.*, .alloc = ir.slir.alloc});
        ctx.instr.results.items[0] = try refTypedValue(kind, ctx.instr.results.items[0], ir.slir.alloc);
        return null;
    }

    fn genericIntegerLit(ir: MutSLIR, instr: Instr, str: []const u8, base: comptime_int) !SLIR.AnyItem {
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
        // if (int.toInt(usize)) |x| {
        //     return .fromAuto(x);
        //     //return try refFromCompute(ir, x);
        // } else |_| {
            return .fromAuto(int);
            //return try refFromComputeCopied(ir, int);
        // }
    }

    pub fn qualifyTypeConst(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        return genericQualifyType(ir, ctx, .{ .data = .@"const" });
    }

    pub fn qualifyTypeVar(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        return genericQualifyType(ir, ctx, .{ .data = .@"var" });
    }

    pub fn qualifyTypeView(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        return genericQualifyType(ir, ctx, .{ .access = .view });
    }

    pub fn qualifyTypeMut(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        return genericQualifyType(ir, ctx, .{ .access = .mut });
    }

    const GenericQualifier = union (enum) {
        data: new_types.qualifier.Data, 
        access: new_types.qualifier.Access
    };
    fn genericQualifyType(ir: MutSLIR, ctx: EvalCtx, qualifier: GenericQualifier) !?[]SLIR.AnyItem {
        var res_def = getInstrByResult(ir, ctx.instr.args.items[0]).?[2];
        if (isOperator(res_def.tag)) return res_def.results.items;

        if (res_def.tag == .struct_value) {
            res_def = getInstrByResult(ir, res_def.args.items[0]).?[2];
        }

        var new_kind = getArgTyped(res_def, 0, .kind);
        //var new_kind = res_def.args.items[0].toTagged().kind;
        //var new_kind = computeItemFromInstrArg(ir, res_def, 0).toTagged().kind;

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
        const ref: SLIR.AnyItem = try .fromAuto(new_kind);
        ctx.instr.args.items[0] = ref;
        ctx.instr.results.items[0] = try refTypedValue(canon_type_type, ctx.instr.results.items[0], ir.slir.alloc);
        return null;
    }

    pub fn opLoadNamedValue(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        _ = ir;
        const value = getNamedValue(ctx.func, ctx.block, ctx.instr.args.items[0].bake(.bytes)) orelse @panic("Name no exisyt");
        ctx.instr.tag = .alias;
        ctx.instr.args.items[0] = value;
        return null;
    }

    //TODO: ???????
    pub fn ensureMayCast(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        const cast_res_name = getArg(ctx.instr, 1);
        const cast_res_def = getInstrByResult(ir, cast_res_name).?[2];
        const cast_res = getArg(cast_res_def, 0); //We want to strip away any incidental type_type info here just in case
        
        const cast_arg_name = getArg(ctx.instr, 0);
        const cast_arg_def = getInstrByResult(ir, cast_arg_name).?[2];
        const cast_arg_type: new_types.partial.Type = cast_arg_def.args.items[0].as(.typed_value).kind.unwrap().*;

        defer ctx.instr.tag = .alias;
        defer ctx.instr.args.shrinkRetainingCapacity(1);

        if (cast_res == canon_type_type.any) {
            if (cast_arg_def.args.items[0].as(.typed_value).kind.any != canon_type_type.any) {
                try ir.slir.showError(ctx.instr.span, "Cannot coerce `non-type {f}` into type `type`\n", .{cast_arg_type});
                unreachable;
            }
            return null;
        }

        if (cast_res.hasTag(.kind)) |cast_res_type| if (asIntegerType(cast_res_type.*)) |int_res| {
            const int_arg = asIntegerType(cast_arg_type) orelse {
                try ir.slir.showError(ctx.instr.span, "Cannot coerce non-integer {f} into integer range {f}\n", .{cast_arg_type, cast_res_type});
                unreachable;
            };

            const src_low = try int_arg.lowBound(ir.slir.alloc);
            const src_high = try int_arg.highBound(ir.slir.alloc);

            var res_low = try int_res.lowBound(ir.slir.alloc);
            var res_high = try int_res.highBound(ir.slir.alloc);

            var acc: std.math.big.int.Managed = try .init(ir.slir.alloc);

            try acc.sub(&src_low, &res_low);
            if (!(acc.isPositive() or acc.eqlZero())) {
                try ir.slir.showError(ctx.instr.span, "Cannot coerce {f} into narrower range {f}\n", .{cast_arg_type, cast_res_type});
            }

            try acc.sub(&res_high, &src_high);
            if (!(acc.isPositive() or acc.eqlZero())) {
                try ir.slir.showError(ctx.instr.span, "Cannot coerce {f} into narrower range {f}\n", .{cast_arg_type, cast_res_type});
            }
            //try ir.slir.showError(ctx.instr.span, "Cannot coerce non integer {f} into integer range {f}\n", .{cast_arg, cast_res});

            return null;
        };

        unreachable;

        // const arg1 = ctx.instr.args.items[1].toTagged();
        // if (arg1 == .kind) switch (arg1.kind.aggregate) {
        //     .root => |root| switch (root.unwrap()) {
        //         .kind => {
        //             ctx.instr.tag = .alias;
        //             ctx.instr.args.shrinkRetainingCapacity(1);
        //         },
        //         .integer => |res_int| {
        //             const src_type = switch (try getArgType(ir, ctx, 0)) {
        //                 .dep => |deps| return deps,
        //                 .types => |lhs_type| lhs_type.kind,
        //             };
        //             switch (src_type.aggregate) {
        //                 .root => |src_root| switch (src_root.unwrap()) {
        //                     .integer => |src_int| {
        //                         const src_low = try src_int.lowBound(ir.slir.alloc);
        //                         const src_high = try src_int.highBound(ir.slir.alloc);

        //                         var res_low = try res_int.lowBound(ir.slir.alloc);
        //                         var res_high = try res_int.highBound(ir.slir.alloc);
        //                         try res_low.sub(&src_low, &res_low);
        //                         if (!(res_low.isPositive() or res_low.eqlZero())) {
        //                             try ir.slir.showError(ctx.instr.span, "Cannot coerce {f} into narrower range {f}\n", .{src_root, root});
        //                         }

        //                         try res_high.sub(&res_high, &src_high);
        //                         if (!(res_high.isPositive() or res_high.eqlZero())) {
        //                             try ir.slir.showError(ctx.instr.span, "Cannot coerce {f} into narrower range {f}\n", .{src_root, root});
        //                         }
        //                     },
        //                     else => try ir.slir.showError(ctx.instr.span, "Cannot coerce Non-Integer to integer\n", .{}),
        //                 }
        //             }
        //             //try ir.slir.showError(ctx.instr.span, "Unimplemented cast check\n", .{});
        //         }
        //     },
        //     // }
        //         // if (item1.kind.aggregate == .root and item1.kind.aggregate.root.* == .kind) {
                    
        //         // } else {

                    
        //         //     //std.debug.print("WARN: Unimplemented cast check\n", .{});
        //         // }
        //     // } else {
        //     //     try ir.slir.showError(ctx.instr.span, "Result is compute of nontype {}\n", .{item1});
        //     //     //std.debug.print("WARN: Result is compute of nontype {}\n", .{item0});
        //     // }
        // } else if (arg1 == .instr_ref) {
        //     const oinstr = getInstrByResult(ir, ctx.instr.args.items[1]).?[2];
        //     if (oinstr.tag != .struct_type_lit) return oinstr.results.items;
        //     ctx.instr.args.items[1] = oinstr.args.items[0];
        //     return &.{};
        // } else {
        //     std.debug.print("WARN: cast of {}\n", .{ctx.instr});
        //     return error.bad;
        // }
        // return null;
    }

    fn asIntegerType(kind: Type) ?new_types.partial.Root.Integer {
        if (kind.aggregate != .root) return null;
        const root = kind.aggregate.root.unwrap().*;
        if (root != .integer) return null;
        return root.integer;
    }

    pub fn ensureIsType(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
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
        dep: []SLIR.AnyItem,
        types: struct {SLIR.AnyItem, SLIR.AnyItem},
    };
    fn getBinaryTypes(ir: MutSLIR, ctx: EvalCtx) !TypesOrDependencies {
        const lhs_instr = getInstrByResult(ir, ctx.instr.args.items[0]).?[2];
        if (isOperator(lhs_instr.tag)) return .{ .dep = lhs_instr.results.items};
        
        const rhs_instr = getInstrByResult(ir, ctx.instr.args.items[1]).?[2];
        if (isOperator(rhs_instr.tag)) return .{ .dep = rhs_instr.results.items};

        const lhs_type_ref = try typeOf(ir, lhs_instr);
        const rhs_type_ref = try typeOf(ir, rhs_instr);

        // const lhs_type = computeItemFromReference(ir, lhs_type_ref);
        // const rhs_type = computeItemFromReference(ir, rhs_type_ref);

        return .{ .types = .{lhs_type_ref, rhs_type_ref} };
    }

    const TypeOrDependency = union (enum) {
        dep: []SLIR.AnyItem,
        types: SLIR.AnyItem,
    };
    fn getArgType(ir: MutSLIR, ctx: EvalCtx, idx: usize) !TypeOrDependency {
        const lhs_instr = getInstrByResult(ir, ctx.instr.args.items[idx]).?[2];
        if (isOperator(lhs_instr.tag)) return .{ .dep = lhs_instr.results.items};

        const lhs_type_ref = try typeOf(ir, lhs_instr);

        // const lhs_type = computeItemFromReference(ir, lhs_type_ref);

        return .{ .types = lhs_type_ref };
    }

    ///Doesnt actually perform the add at comptime, just computes the type of the result
    // pub fn opAdd(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
    //     const lhs_type, 
    //     const rhs_type = switch (try getBinaryTypes(ir, ctx)) {
    //         .dep => |deps| return deps,
    //         .types => |lhs_rhs| lhs_rhs,
    //     };

    //     const alloc = ir.slir.alloc;
    //     switch (lhs_type.kind.aggregate) {
    //         .root => |lhs_root| switch (lhs_root.*) {
    //             .integer => |lhs_int| switch (rhs_type.kind.aggregate) {
    //                 .root => |rhs_root| switch (rhs_root.*) {
    //                     .integer => |rhs_int| {
    //                         const lhs_low = try lhs_int.lowBound(alloc);
    //                         const lhs_high = try lhs_int.highBound(alloc);

    //                         const rhs_low = try rhs_int.lowBound(alloc);
    //                         const rhs_high = try rhs_int.highBound(alloc);
                            
    //                         var res_low: big_int.Managed = try .init(alloc);
    //                         try res_low.add(&lhs_low, &rhs_low);

    //                         var res_high: big_int.Managed = try .init(alloc);
    //                         try res_high.add(&lhs_high, &rhs_high);

    //                         const ref: SLIR.AnyItem = try makeInteger(ir, .implicit, .{
    //                             .alloc = alloc,
    //                             .low = res_low.toMutable(),
    //                             .high = res_high.toMutable(),
    //                         });

    //                         try ctx.instr.results.append(alloc, ref);
    //                     },
    //                     else => unreachable,
    //                 },
    //                 //else => unreachable,
    //             },
    //             else => unreachable,
    //         },
    //         //else => unreachable,
    //     }
            

    //     //std.debug.print("Umm, heres some info?\n{f}\n{f}\n", .{lhs_type, rhs_type});
    //     ctx.instr.tag = .typed_op_add;
    //     return null;
    // }

    pub fn opStructValue(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        const res_def = getInstrByResult(ir, ctx.instr.args.items[1]).?[2];
        if (isOperator(res_def.tag)) return res_def.results.items;
        ctx.instr.tag = .struct_value;
        return null;
    }

    //////////////////////////////////////////////////////////////////////
    //

    pub fn structIntLit(ir: MutSLIR, ctx: EvalCtx) !Null {
        _ = ir;
        _ = ctx;
        return null;
    }

    pub fn structTypeLit(ir: MutSLIR, ctx: EvalCtx) !Null {
        _ = ir;
        _ = ctx;
        return null;
    }

    pub fn structValue(ir: MutSLIR, ctx: EvalCtx) !Null {
        const res_def = getInstrByResult(ir, getArg(ctx.instr, 1)).?[2];
        std.debug.assert(res_def.tag == .struct_type_lit);
        ctx.instr.args.items[0] = try refTypedValue(getArg(res_def, 0).bake(.kind), getArg(ctx.instr, 0), ir.slir.alloc);
        ctx.instr.args.shrinkRetainingCapacity(1);
        ctx.instr.tag = .struct_value;
        ctx.instr.results.items[0] = try refTypedValue(getArg(res_def, 0).bake(.kind), ctx.instr.results.items[0], ir.slir.alloc);
        
        return null;
    }

    pub fn typedOpAdd(ir: MutSLIR, ctx: EvalCtx) !Null {
        _ = ir;
        _ = ctx;
        return null;
    }

    //
    //////////////////////////////////////////////////////////////////////

    pub fn template(ir: MutSLIR, ctx: EvalCtx) !?[]SLIR.AnyItem {
        _ = ir;
        _ = ctx;
        return null;
    }

    fn isOperator(tag: Instr.Tag) bool {
        return std.mem.startsWith(u8, @tagName(tag), "op") or std.mem.startsWith(u8, @tagName(tag), "ensure");
    }

    // fn refFromComputeCopied(ir: MutSLIR, val: anytype) !SLIR.AnyItem {
    //     return refFromCompute(ir, try refDeepCopy(val, ir.slir.alloc));
    // }

    // fn refFromCompute(ir: MutSLIR, val: anytype) !SLIR.AnyItem {
    //     return .fromCompute(try ir.slir.computes.convert(val));
    // }

    // fn refFromType(ir: MutSLIR, kind: Type) !SLIR.AnyItem {
    //     return refFromComputeCopied(ir, kind);
    // }

    // fn refFromRootType(ir: MutSLIR, root: RootType) !SLIR.AnyItem {
    //     return refFromType(ir, .fromRoot(try refDeepCopy(root, ir.slir.alloc)));
    // }

    // fn computeItemFromReference(ir: MutSLIR, ref: SLIR.AnyItem) SLIR.AnyItem {
    //     return ref.toTaggedUnion().compute_ref.toItem(ir.slir.computes.*);
    // }

    // fn computeItemFromInstrArg(ir: MutSLIR, instr: *Instr, arg_id: usize) SLIR.AnyItem {
    //     return computeItemFromReference(ir, instr.args.items[arg_id]);
    // }

    // fn stringFromInstrArg(ir: MutSLIR, instr: *Instr, arg_id: usize) []const u8 {
    //     return instr.args.items[arg_id].toTaggedUnion().string_ref.toStr(ir.slir.intern.*);
    // }

    fn isDataSubset(lhs: ?new_types.qualifier.Data, rhs: new_types.qualifier.Data) bool {
        return (lhs orelse return true).isSubset(rhs);
    }

    fn isAccessSubset(lhs: ?new_types.qualifier.Access, rhs: new_types.qualifier.Access) bool {
        return (lhs orelse return true).isSubset(rhs);
    }

    fn refTypedValue(kind: TypeRef, value: SLIR.AnyItem, alloc: std.mem.Allocator) !compute.AnyItem {
        const typed_value: new_compute_pool.TypedValue = .{.kind = kind, .value = value};
        return try .fromTagged(.{.typed_value = try new_types.refDeepCopy(typed_value, alloc)});
    }

    fn getArgTyped(instr: *Instr, arg_id: usize, comptime tag: SLIR.AnyItem.Tag) compute.AnyItem.PayloadType(tag) {
        const tagged = instr.args.items[arg_id].toTagged();
        if (tagged == .typed_value) {
            return tagged.typed_value.value.as(tag);
        } else {
            return @field(tagged, @tagName(tag));
        }
    } 
    fn getArg(instr: *Instr, arg_id: usize) compute.AnyItem {
        const tagged = instr.args.items[arg_id].toTagged();
        if (tagged == .typed_value) {
            return tagged.typed_value.value;
        } else {
            return instr.args.items[arg_id];
        }
    } 
};

//Return null to mean no dependencies
fn _evaluateInstruction(ir: MutSLIR, func: *SLIR.Function, block: *SLIR.Function.Block, instr: *Instr) !?[]SLIR.AnyItem {
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

fn getNamedValue(func: *SLIR.Function, origin: *SLIR.Function.Block, name: String) ?SLIR.AnyItem {
    const idx = origin - func.blocks.items.ptr;
    for (0..idx + 1) |i| {
        const block = func.blocks.items[idx - i];
        if (!blockPreceeds(block, origin)) continue;
        for (block.instrs.items) |instr| {
            if (instr.tag == .info_named_value and std.meta.eql(instr.args.items[0].bake(.bytes), name)) {
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

fn makeIntegerType(
    ir: MutSLIR,
    backing: new_types.partial.Root.MakeIntegerBacking,
    defaults: struct {
        alloc: ?std.mem.Allocator = null,
        low: ?big_int.Const = null,
        high: ?big_int.Const = null,
    },
) !eval_impl.TypeRef {
    const root = try ir.slir.alloc.create(new_types.partial.Root);
    const low: ?compute.Item(.integer_big) = if (defaults.low) |real_low| try .from(try refDeepCopy(real_low, ir.slir.alloc)) else null;
    const high: ?compute.Item(.integer_big) = if (defaults.high) |real_high| try .from(try refDeepCopy(real_high, ir.slir.alloc)) else null;
    root.* = try .makeInteger(backing, defaults.alloc, .{ .low = low, .high = high });

    const partial: new_types.partial.Type = .fromRoot(root);
    const heap_partial = try refDeepCopy(partial, ir.slir.alloc);

    return try .from(heap_partial);
}

fn getFunctionByName(ir: MutSLIR, name: String) ?*SLIR.Function {
    for (ir.slir.functions.items) |*func| {
        if (func.name.any == name.any) {
            return func;
        }
    }
    return null;
}

fn getInstrByResult(ir: MutSLIR, result: SLIR.AnyItem) ?struct { *SLIR.Function, *SLIR.Function.Block, *Instr } {
    for (ir.slir.functions.items) |*func| {
        for (func.blocks.items) |*block| {
            for (block.instrs.items) |*instr| {
                for (instr.results.items) |res| {
                    if (res == result) {
                        if (instr.tag == .alias){
                            return getInstrByResult(ir, instr.args.items[0]);
                        }
                        return .{ func, block, instr };
                    }
                    if (res.hasTag(.typed_value)) |typed_value| if (typed_value.value == result) {
                        if (instr.tag == .alias){
                            return getInstrByResult(ir, instr.args.items[0]);
                        }
                        return .{ func, block, instr };
                    };
                }
            }
        }
    }
    return null;
}

// fn add(ir: MutSLIR, lhs: SLIR.AnyItem, rhs: SLIR.AnyItem) !Compute {
//     const alloc = ir.slir.alloc;
//     switch (lhs) {
//         .int_small => |x| switch (rhs) {
//             .int_small => |y| {
//                 return try ir.slir.computes.convert(x + y);
//             },
//             .int_big => |y| {
//                 var result: big_int.Managed = .init(alloc);
//                 try result.addScalar(y, x);
//                 return try ir.slir.computes.convert(try refDeepCopy(result, alloc));
//             },
//             else => unreachable,
//         },
//         .int_big => |x| switch (rhs) {
//             .int_small => |y| {
//                 var result: big_int.Managed = .init(alloc);
//                 try result.addScalar(x, y);
//                 return try ir.slir.computes.convert(try refDeepCopy(result, alloc));
//             },
//             .int_big => |y| {
//                 var result: big_int.Managed = .init(alloc);
//                 try result.add(x, y);
//                 return try ir.slir.computes.convert(try refDeepCopy(result, alloc));
//             },
//             else => unreachable,
//         },
//         else => unreachable,
//     }
// }