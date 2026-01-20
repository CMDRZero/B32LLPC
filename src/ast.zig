const std = @import("std");

const precedence = @import("precedence.zig");
const slir = @import("slir.zig");
const slir_res = @import("slir_resolution.zig");
const SLIR = slir.SLIR;
const Guid = SLIR.Guid;
const Reference = SLIR.Reference;
const slir_cons = @import("slir_construction.zig");
pub const MutSLIR = slir_cons.MutSLIR;
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const ComputePool = @import("compute_pool.zig").ComputePool;

//Typing guide: Use errors for a failure and null for a probe that didnt succeed. !?T means this might return something or it might fail gently or it might fail hard.
//Unless this function finds something that forces it to commit, speculatively return null, the caller can always promote null to an error

pub fn parse(alloc: std.mem.Allocator, intern: *StringInternPool, computes: *ComputePool, file: []u8) !SLIR {
    var state: slir_cons.InParseSLIR = .{
        .source_file = file,
        .slir = .init(alloc, intern, computes),
    };
    errdefer {
        //std.debug.print("Unrecoverable error at:\n`{s}`\n", .{state.source_file[0..20]});
        //std.debug.print("{f}\n", .{state.slir});
    }
    while (state.source_file.len > 0) {
        try parseFunction(&state) orelse return error.expected_function;
    }
    std.debug.print("{f}\n", .{state.slir});
    try slir_res.resolve(&state);
    return state.slir;
}

pub fn parseFunction(state: MutSLIR) !?void {
    errdefer std.debug.print("Unrecoverable error at:\n`{s}`\n", .{state.source_file[0..@min(20, state.source_file.len)]});
    errdefer std.debug.print("SLIR DUMP:\n{f}\n", .{state.slir});

    if (!state.eatSymbol(.@"fn")) return null; //Commit on `fn`

    const name = try state.popIdentifier();
    const name_str = try state.slir.intern.convert(name.str);
    try state.slir.functions.append(state.slir.alloc, .{
        .name = name_str,
        .args = .empty,
        .resT = undefined,
    });
    const curr_fn = state.slir.currentFunction();
    try state.expectSymbol(.@"(");
    try state.slir.createOnInstance();

    if (state.popIdentifier()) |ident| {
        try state.expectSymbol(.@":");
        const kind_ref = try parseType(state) orelse return error.expected_type;
        try curr_fn.args.append(state.slir.alloc, .{
            .name = try state.slir.intern.convert(ident.str),
            .kind = kind_ref,
        });
    } else |_| {}

    try state.expectSymbol(.@")");
    const resT = try parseType(state) orelse return error.expected_type;
    curr_fn.resT = .fromGuid(resT);

    try state.expectSymbol(.@"{");
    try state.slir.openScope();
    try state.slir.createEntry();

    ////Testing begin
    // try state.slir.appendBlock(try state.slir.intern.convert("Test_0"));
    // try state.slir.openScope();
    // try state.slir.appendBlock(try state.slir.intern.convert("Test_1"));
    // try state.slir.appendBlock(try state.slir.intern.convert("Test_2"));
    // try state.slir.appendBlock(try state.slir.intern.convert("Test_3"));
    // try state.slir.closeScope();
    // try state.slir.appendBlock(try state.slir.intern.convert("Test_4"));
    ////Testing end

    while (!state.eatSymbol(.@"}")) {
        try parseStatement(state) orelse return error.expected_statement;
    }
    try state.slir.closeScope();
}

fn parseType(state: MutSLIR) !?Guid {
    const save = state.getState();
    return try parseTypeAccessQualifier(state) orelse state.restoreThrow(save);
}

fn parseTypeAccessQualifier(state: MutSLIR) !?Guid {
    const save = state.getState();
    if (state.eatSymbol(.view)) {
        return error.unimplemented;
    } else if (state.eatSymbol(.mut)) {
        return error.unimplemented;
    }

    return try parseTypeCopyQualifier(state) orelse state.restoreThrow(save);
}

fn parseTypeCopyQualifier(state: MutSLIR) !?Guid {
    const save = state.getState();
    if (state.eatSymbol(.pinned)) {
        return error.unimplemented;
    }

    return try parseTypeDataQualifier(state) orelse state.restoreThrow(save);
}

fn parseTypeDataQualifier(state: MutSLIR) !?Guid {
    const save = state.getState();
    if (state.eatSymbol(.@"const")) {
        return error.unimplemented;
    } else if (state.eatSymbol(.@"var")) {
        return error.unimplemented;
    } else if (state.eatSymbol(.@"volatile")) {
        return error.unimplemented;
    }

    return try parseTypeAggregate(state) orelse state.restoreThrow(save);
}

fn parseTypeAggregate(state: MutSLIR) !?Guid {
    const save = state.getState();
    if (state.eatSymbol(.array)) {
        return error.unimplemented;
    } else if (state.eatSymbol(.@"?")) {
        return error.unimplemented;
    }

    return try parseTypeStructuralQualifier(state) orelse state.restoreThrow(save);
}

fn parseTypeStructuralQualifier(state: MutSLIR) !?Guid {
    const save = state.getState();
    if (state.eatSymbol(.bitalign)) {
        return error.unimplemented;
    }

    return try parseTypeReference(state) orelse state.restoreThrow(save);
}

fn parseTypeReference(state: MutSLIR) !?Guid {
    const save = state.getState();
    if (state.eatSymbol(.@"^")) {
        return error.unimplemented;
    }

    return try parseTypePrimative(state) orelse state.restoreThrow(save);
}

fn parseTypePrimative(state: MutSLIR) !?Guid {
    const save = state.getState();
    const next_token = state.popToken();
    const guid = state.slir.getGuid();
    switch (next_token) {
        .symbol => |sym| {
            if (sym == .void) {
                try state.addInstructionPoly(.{guid}, .type_unsigned_int, .{0});
                return guid;
            }
            return state.restoreThrow(save);
        },
        .kind => {
            try state.addInstructionPoly(.{guid}, .fromKindTag(next_token.kind.tag), .{next_token.kind.bits});
            return guid;
        },
        .identifier => |id| {
            try state.addInstructionPoly(.{guid}, .load_named_value, .{id});
            const throw_away_guid = state.slir.getGuid();
            try state.addInstructionPoly(.{throw_away_guid}, .ensure_is_type, .{guid});
            return guid;
        },
        else => return state.restoreThrow(save),
    }
}

fn parseStatement(state: MutSLIR) !?void {
    const save = state.getState();
    if (state.eatSymbol(.@"{")) {
        try state.slir.openScope();
        try state.slir.appendBlock(null);
        while (true) {
            const stmt = try parseStatement(state);
            if (stmt == null) break;
        }
        try state.expectSymbol(.@"}");
        try state.slir.closeScope();
        try state.slir.appendBlock(null);
    } else {
        return try parseDeclStatement(state) orelse state.restoreThrow(save);
    }
}

fn parseDeclStatement(state: MutSLIR) !?void {
    const save = state.getState();
    if (state.eatSymbol(.@"const")) {
        const var_decl = try parseVariableDecl(state) orelse return error.expected_variable_declaration;

        var kind = var_decl.kind;
        var new_kind = state.slir.getGuid();
        try state.addInstructionPoly(.{new_kind}, .qualify_type_const, .{kind});

        kind = new_kind;
        new_kind = state.slir.getGuid();
        try state.addInstructionPoly(.{new_kind}, .qualify_type_view, .{kind});

        const stored_value = state.slir.getGuid();
        try state.addInstructionPoly(.{stored_value}, .value, .{ var_decl.value, new_kind });
        try state.addInstructionPoly(.{}, .named_value, .{ var_decl.name, stored_value });
    } else if (state.eatSymbol(.@"var")) {
        const var_decl = try parseVariableDecl(state) orelse return error.expected_variable_declaration;

        var kind = var_decl.kind;
        var new_kind = state.slir.getGuid();
        try state.addInstructionPoly(.{new_kind}, .qualify_type_var, .{kind});

        kind = new_kind;
        new_kind = state.slir.getGuid();
        try state.addInstructionPoly(.{new_kind}, .qualify_type_mut, .{kind});

        const stored_value = state.slir.getGuid();
        try state.addInstructionPoly(.{stored_value}, .value, .{ var_decl.value, new_kind });
        try state.addInstructionPoly(.{}, .named_value, .{ var_decl.name, stored_value });
    } else if (state.eatSymbol(.@"volatile")) {
        return error.unimplemented;
    } else return state.restoreThrow(save);
}

const VarDecl = struct {
    name: Token.Identifier,
    value: Guid,
    kind: Guid,
};
//We assume if you're calling this its on purpose, so null never occurs
fn parseVariableDecl(state: MutSLIR) !?VarDecl {
    //const save = state.getState();
    const name = try state.popIdentifier();
    const kind, const didInfer = if (state.eatSymbol(.@":")) (.{ try parseType(state) orelse return error.expected_type, false }) else (.{ state.slir.getGuid(), true });
    try state.expectSymbol(.@"=");

    const expr_value = try parseExpression(state, .fromGuid(kind));

    if (didInfer) {
        try state.addInstructionPoly(.{kind}, .type_of, .{expr_value});
    }
    try state.expectSymbol(.@";");
    return .{ .name = name, .value = expr_value.toTaggedUnion().instr_ref, .kind = kind };
}

//TODO: Add support for result type inference
fn parseExpression(state: MutSLIR, resT: Reference) !Reference {
    return try precedence.parseExpression(state, resT) orelse error.expected_expression;
}
