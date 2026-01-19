const std = @import("std");
const slir = @import("slir.zig");
const tokenizer = @import("tokenizer.zig");
const SLIR = slir.SLIR;
const Token = tokenizer.Token;
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
const Guid = SLIR.Guid;
const Reference = SLIR.Reference;
const precedence = @import("precedence.zig");
const slir_cons = @import("slir_construction.zig");
pub const MutSLIR = slir_cons.MutSLIR;

//Typing guide: Use errors for a failure and null for a probe that didnt succeed. !?T means this might return something or it might fail gently or it might fail hard.
//Unless this function finds something that forces it to commit, speculatively return null, the caller can always promote null to an error

pub fn parse(alloc: std.mem.Allocator, intern: *StringInternPool, file: [] u8) !SLIR { 
    var state: slir_cons.InParseSLIR = .{
        .source_file = file,
        .slir = .init(alloc, intern),
    };
    errdefer {
        //std.debug.print("Unrecoverable error at:\n`{s}`\n", .{state.source_file[0..20]});
        //std.debug.print("{f}\n", .{state.slir});
    }
    while (state.source_file.len > 0) {
        try parseFunction(&state) orelse return error.expected_function;
    }
    return state.slir;
}

pub fn parseFunction(state: MutSLIR) !?void {
    errdefer std.debug.print("Unrecoverable error at:\n`{s}`\n", .{state.source_file[0..20]});
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
    try state.slir.appendBlock(try state.slir.intern.convert("OnInstance"));
    
    if (state.popIdentifier()) |ident| {
        try state.expectSymbol(.@":" );
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
    try state.slir.appendBlock(try state.slir.intern.convert("Entry"));

    while (!state.eatSymbol(.@"}")) {
        try parseStatement(state) orelse return error.expected_statement;
    }
}


fn parseType(state: MutSLIR) !?Guid {
    const save = state.getState();
    return try parseTypeAccessQualifier(state) orelse state.restoreThrow(save);
}

fn parseTypeAccessQualifier(state: MutSLIR) !?Guid {
    const save = state.getState();
    if (state.eatSymbol(.@"view")) {
        return error.unimplemented;
    } else if (state.eatSymbol(.@"mut")) {
        return error.unimplemented;
    }

    return try parseTypeCopyQualifier(state) orelse state.restoreThrow(save);
}

fn parseTypeCopyQualifier(state: MutSLIR) !?Guid {
    const save = state.getState();
    if (state.eatSymbol(.@"pinned")) {
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
    if (state.eatSymbol(.@"array")) {
        return error.unimplemented;
    } else if (state.eatSymbol(.@"?")) {
        return error.unimplemented;
    }

    return try parseTypeStructuralQualifier(state) orelse state.restoreThrow(save);
}

fn parseTypeStructuralQualifier(state: MutSLIR) !?Guid {
    const save = state.getState();
    if (state.eatSymbol(.@"bitalign")) {
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
    const next_token = state.popToken();
    const guid = state.slir.getGuid();
    if (next_token == .symbol and next_token.symbol == .@"void") {
        try state.addInstructionPoly(.{guid}, .unsigned_int, .{0});
        return guid;
    }
    if (next_token != .kind) return null;

    try state.addInstructionPoly(.{guid}, .unsigned_int, .{next_token.kind.bits});
    return guid;
}

fn parseStatement(state: MutSLIR) !?void {
    const save = state.getState();
    if (state.eatSymbol(.@"const")) {
        const val, const kind = try parseVariableDecl(state) orelse return error.expected_variable_declaration;
        try state.addInstructionPoly(.{}, .const_value, .{val, kind});
        // try state.expectSymbol(.@"=");
        // _ = try parseExpression(state);
    } else if (state.eatSymbol(.@"var")) {
        return error.unimplemented;
    } else if (state.eatSymbol(.@"volatile")) {
        return error.unimplemented;
    } else return state.restoreThrow(save);
}

//We assume if you're calling this its on purpose, so null never occurs
fn parseVariableDecl(state: MutSLIR) !?struct{Guid, Guid} {
    //const save = state.getState();
    const name = try state.popIdentifier();
    const kind, const didInfer = if (state.eatSymbol(.@":")) (
        .{try parseType(state) orelse return error.expected_type, false}
    ) else (
        .{state.slir.getGuid(), true}
    );
    try state.expectSymbol(.@"=");
    const unnamed_result = try parseExpression(state); 
    try state.addInstructionPoly(.{}, .debug_named_value, .{name, unnamed_result});
    if (didInfer) {
        try state.addInstructionPoly(.{kind}, .type_of, .{unnamed_result});
    }
    try state.expectSymbol(.@";");
    return .{unnamed_result.toTaggedUnion().instr_ref, kind};
}

//Look at Zig Parse.zig for guidance
fn parseExpression(state: MutSLIR) !Reference {
    return try precedence.parseExpression(state) orelse error.expected_expression;
}



