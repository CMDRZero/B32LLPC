//! A wrapper over the tokenizer that provides new functions that are more useful

const std = @import("std");

const slir_cons = @import("slir_construction.zig");
const slir = @import("slir.zig");
const tokenizer = @import("tokenizer.zig");

const MutSLIR = slir_cons.MutSLIR;
const Token = tokenizer.Token;

//Naming convention:
//pop- = get of a specific type and error if type is wrong -> !T
//peek- = do not consume but try to get type -> !T
//eat- = try consume and emit boolean success -> !bool
//expect- = eat but error on failure -> !void
//All of these undo the consumption if an error occured

pub fn popToken(state: MutSLIR) Token {
    return tokenizer.Token.popFrom(&state.source_file);
}

pub fn peekToken(state: MutSLIR) Token {
    const src = state.source_file;
    defer state.source_file = src;
    return tokenizer.Token.popFrom(&state.source_file);
}

pub fn eatTokenExact(state: MutSLIR, token: Token) bool {
    const src = state.source_file;

    const next_token = popToken(state);
    if (!std.meta.eql(next_token, token)) {
        state.source_file = src;
        return false;
    } else return true;
}

pub fn eatTokenType(state: MutSLIR, token: Token) bool {
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

pub fn expectTokenExact(state: MutSLIR, token: Token) !void {
    if (!eatTokenExact(state, token)) error.unexpected_token;
}

pub fn expectTokenType(state: MutSLIR, token: Token) !void {
    if (!eatTokenType(state, token)) error.unexpected_token;
}


pub fn popSymbol(state: MutSLIR) !Token.Symbol {
    if (peekToken(state) != .symbol) return error.expected_symbol;
    return popToken(state).symbol;
}

pub fn eatSymbol(state: MutSLIR, sym: Token.Symbol) bool {
    return eatTokenExact(state, .{.symbol = sym});
}

pub fn expectSymbol(state: MutSLIR, sym: Token.Symbol) !void {
    if (!eatSymbol(state, sym)) return error.unexpected_symbol;
}


pub fn popIdentifier(state: MutSLIR) !Token.Identifier {
    if (peekToken(state) != .identifier) return error.expected_identifier;
    return popToken(state).identifier;
}

pub fn popOperator(state: MutSLIR) !?tokenizer.Operator {
    const save = state.getState();

    var ret: tokenizer.Operator = .{.isInplace = false, .isOverloaded = false, .postMod = .none, .tag = undefined};
    if (eatSymbol(state, .@".")) ret.isOverloaded = true;
    const opsym = try popSymbol(state);
    ret.tag = std.meta.stringToEnum(tokenizer.Operator.Tag, @tagName(opsym)) orelse return state.restoreThrow(save);
    if (eatSymbol(state, .@"%")) {
        ret.postMod = .wrapping;
    } else if (eatSymbol(state, .@"|")) {
        ret.postMod = .clamping;
    }
    if (eatSymbol(state, .@"=")) ret.isOverloaded = true;
    try ret.validate();
    return ret;
}