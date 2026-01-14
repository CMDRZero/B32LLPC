const std = @import("std");
const slir = @import("slir.zig");
const tokenizer = @import("tokenizer.zig");
const SLIR = slir.SLIR;
const InParseSLIR = slir.InParseSLIR;
const Token = tokenizer.Token;
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;
const Guid = SLIR.Guid;

pub fn parse(alloc: std.mem.Allocator, intern: *StringInternPool, file: [] u8) !SLIR { 
    var state: InParseSLIR = .{
        .source_file = file,
        .slir = .init(alloc, intern),
    };
    errdefer {
        //std.debug.print("Unrecoverable error at:\n`{s}`\n", .{state.source_file[0..20]});
        std.debug.print("{f}\n", .{state.slir});
    }
    while (state.source_file.len > 0) {
        try parseFunction(&state);
    }
    return state.slir;
}

pub fn parseFunction(state: *InParseSLIR) !void {
    const save = state.getState();
    errdefer state.setState(save);
    errdefer std.debug.print("Unrecoverable error at:\n`{s}`\n", .{state.source_file[0..20]});
    errdefer std.debug.print("SLIR DUMP:\n{f}\n", .{state.slir});

    try expectSymbol(state, .@"fn" );
    const name = try popIdentifier(state);
    const name_str = try state.slir.intern.convert(name.str);
    try state.slir.functions.append(state.slir.alloc, .{
        .name = name_str,
        .args = .empty, 
        .resT = undefined,
    });
    const curr_fn = state.slir.currentFunction();
    try expectSymbol(state, .@"(");
    
    if (popIdentifier(state)) |ident| {
        try expectSymbol(state, .@":" );
        const kind_ref = try parseType(state);
        try curr_fn.args.append(state.slir.alloc, .{
            .name = try state.slir.intern.convert(ident.str),
            .kind = kind_ref,
        });
    } else |_| {}

    try expectSymbol(state, .@")");
    const resT = try parseType(state);
    curr_fn.resT = .fromGuid(resT);

    try expectSymbol(state, .@"{");

    try expectSymbol(state, .@"}");
}


fn parseType(state: *InParseSLIR) !Guid {
    const save = state.getState();
    errdefer state.setState(save);

    return try parseTypeAccessQualifier(state);
}

fn parseTypeAccessQualifier(state: *InParseSLIR) !Guid {
    const save = state.getState();
    if (eatSymbol(state, .@"view")) {
        return error.unimplemented;
    } else if (eatSymbol(state, .@"mut")) {
        return error.unimplemented;
    } else {
        state.setState(save);
    }

    return try parseTypeCopyQualifier(state);
}

fn parseTypeCopyQualifier(state: *InParseSLIR) !Guid {
    const save = state.getState();
    if (eatSymbol(state, .@"pinned")) {
        return error.unimplemented;
    } else {
        state.setState(save);
    }

    return try parseTypeDataQualifier(state);
}

fn parseTypeDataQualifier(state: *InParseSLIR) !Guid {
    const save = state.getState();
    if (eatSymbol(state, .@"const")) {
        return error.unimplemented;
    } else if (eatSymbol(state, .@"var")) {
        return error.unimplemented;
    } else if (eatSymbol(state, .@"volatile")) {
        return error.unimplemented;
    } else {
        state.setState(save);
    }

    return try parseTypeAggregate(state);
}

fn parseTypeAggregate(state: *InParseSLIR) !Guid {
    const save = state.getState();
    if (eatSymbol(state, .@"array")) {
        return error.unimplemented;
    } else if (eatSymbol(state, .@"?")) {
        return error.unimplemented;
    } else {
        state.setState(save);
    }

    return try parseTypeStructuralQualifier(state);
}

fn parseTypeStructuralQualifier(state: *InParseSLIR) !Guid {
    const save = state.getState();
    if (eatSymbol(state, .@"bitalign")) {
        return error.unimplemented;
    } else {
        state.setState(save);
    }

    return try parseTypeReference(state);
}

fn parseTypeReference(state: *InParseSLIR) !Guid {
    const save = state.getState();
    if (eatSymbol(state, .@"^")) {
        return error.unimplemented;
    } else {
        state.setState(save);
    }

    return try parseTypePrimative(state);
}

fn parseTypePrimative(state: *InParseSLIR) !Guid {
    const next_token = popToken(state);
    const guid = state.slir.getGuid();
    if (next_token == .symbol and next_token.symbol == .@"void") {
        try addInstructionPoly(state, .{guid}, .unsigned_int, .{0});
        return guid;
    }
    if (next_token != .kind) return error.invalid_primative_symbol;

    try addInstructionPoly(state, .{guid}, .unsigned_int, .{next_token.kind.bits});
    return guid;
}


fn addInstructionPoly(state: *InParseSLIR, results: anytype, tag: SLIR.Function.Instruction.Tag, args: anytype) !void {
    const num_results = std.meta.fields(@TypeOf(results)).len;
    const heap_results = try state.slir.alloc.alloc(SLIR.Reference, num_results);
    inline for (0..num_results) |i| {
        heap_results[i] = SLIR.Reference.fromAny(results[i]).?;
    }

    const num_args = std.meta.fields(@TypeOf(args)).len;
    const heap_args = try state.slir.alloc.alloc(SLIR.Reference, num_args);
    inline for (0..num_results) |i| {
        heap_args[i] = SLIR.Reference.fromAny(args[i]) orelse return error.int_too_big;
    }

    try state.slir.currentFunction().instrs.append(state.slir.alloc, .{
        .tag = tag,
        .results = .fromOwnedSlice(heap_results),
        .args = .fromOwnedSlice(heap_args),
    });
}

//Naming convention:
//pop- = get of a specific type and error if type is wrong -> !T
//peek- = do not consume but try to get type -> !T
//eat- = try consume and emit boolean success -> !bool
//expect- = eat but error on failure -> !void
//All of these undo the consumption if an error occured

fn popToken(state: *InParseSLIR) Token {
    return tokenizer.Token.popFrom(&state.source_file);
}

fn peekToken(state: *InParseSLIR) Token {
    const src = state.source_file;
    defer state.source_file = src;
    return tokenizer.Token.popFrom(&state.source_file);
}

fn eatTokenExact(state: *InParseSLIR, token: Token) bool {
    const src = state.source_file;

    const next_token = popToken(state);
    if (!std.meta.eql(next_token, token)) {
        state.source_file = src;
        return false;
    } else return true;
}

fn eatTokenType(state: *InParseSLIR, token: Token) bool {
    const src = state.source_file;

    const next_token = popToken(state);
    const succ = switch (token) {
        .identifier => next_token == .identifier,
        .kind => next_token == .kind,
        .literal => |lit| next_token == .literal and next_token.literal.tag == lit.tag,
        .symbol => |sym| next_token == .symbol and next_token.symbol == sym,
    };
    if (!succ) {
        state.source_file = src;
        return false;
    } else return true;
}

fn expectTokenExact(state: *InParseSLIR, token: Token) !void {
    if (!eatTokenExact(state, token)) error.unexpected_token;
}

fn expectTokenType(state: *InParseSLIR, token: Token) !void {
    if (!eatTokenType(state, token)) error.unexpected_token;
}


fn eatSymbol(state: *InParseSLIR, sym: Token.Symbol) bool {
    return eatTokenExact(state, .{.symbol = sym});
}

fn expectSymbol(state: *InParseSLIR, sym: Token.Symbol) !void {
    if (!eatSymbol(state, sym)) return error.unexpected_symbol;
}


fn popIdentifier(state: *InParseSLIR) !Token.Identifier {
    if (peekToken(state) != .identifier) return error.expected_identifier;
    return popToken(state).identifier;
}