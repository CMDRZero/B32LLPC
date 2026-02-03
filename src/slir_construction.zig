//! A file dedicated to functions to help build the SLIR
//! Also dives a bit into implementation details

const std = @import("std");

const slir = @import("slir.zig");
const adv_tkn = @import("advanced_tokenizer.zig");
const tokenizer = @import("tokenizer.zig");

const SLIR = slir.SLIR;
const Token = tokenizer.Token;

pub const MutSLIR = *InParseSLIR;
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

    pub inline fn restoreThrow(self: *InParseSLIR, save: SaveState) @TypeOf(null) {
        self.setState(save);
        return null;
    }

    pub fn getGuid(self: *InParseSLIR) SLIR.Guid {
        return self.slir.getGuid();
    }

    pub fn startSpan(self: *InParseSLIR) PartialSpan {
        return .{ .slice = self.source_file };
    }
    pub fn endSpan(self: *InParseSLIR, span: PartialSpan) SLIR.Span {
        return .{ .slice = span.slice[0..self.source_file.ptr - span.slice.ptr]};
    }
    const PartialSpan = struct {
        slice: []u8,
    };

    //Now for convience we will import the advanced tokenizer functions
    pub fn popToken(state: MutSLIR) Token {
        return adv_tkn.popToken(state);
    }

    pub fn peekToken(state: MutSLIR) Token {
        return adv_tkn.peekToken(state);
    }

    pub fn eatTokenExact(state: MutSLIR, token: Token) bool {
        return adv_tkn.eatTokenExact(state, token);
    }

    pub fn eatTokenType(state: MutSLIR, token: Token) bool {
        return adv_tkn.eatTokenType(state, token);
    }

    pub fn expectTokenExact(state: MutSLIR, token: Token) !void {
        return adv_tkn.expectTokenExact(state, token);
    }

    pub fn expectTokenType(state: MutSLIR, token: Token) !void {
        return adv_tkn.expectTokenType(state, token);
    }

    pub fn popSymbol(state: MutSLIR) !Token.Symbol {
        return adv_tkn.popSymbol(state);
    }

    pub fn eatSymbol(state: MutSLIR, sym: Token.Symbol) bool {
        return adv_tkn.eatSymbol(state, sym);
    }

    pub fn expectSymbol(state: MutSLIR, sym: Token.Symbol) !void {
        return adv_tkn.expectSymbol(state, sym);
    }

    pub fn popIdentifier(state: MutSLIR) !Token.Identifier {
        return adv_tkn.popIdentifier(state);
    }

    pub fn popOperator(state: MutSLIR) !?tokenizer.Operator {
        return adv_tkn.popOperator(state);
    }

    pub fn addInstructionPoly(state: MutSLIR, span: SLIR.Span, results: anytype, tag: SLIR.Function.Block.Instruction.Tag, args: anytype) !void {
        const num_results = std.meta.fields(@TypeOf(results)).len;
        const heap_results = try state.slir.alloc.alloc(SLIR.AnyItem, num_results);
        inline for (0..num_results) |i| {
            heap_results[i] = try state.slir.compute_pool.cvtAuto(results[i]);
        }

        const num_args = std.meta.fields(@TypeOf(args)).len;
        const heap_args = try state.slir.alloc.alloc(SLIR.AnyItem, num_args);
        inline for (0..num_args) |i| {
            heap_args[i] = try state.slir.compute_pool.cvtAuto(args[i]);
        }

        try state.slir.currentBlock().instrs.append(state.slir.alloc, .{
            .tag = tag,
            .results = .fromOwnedSlice(heap_results),
            .args = .fromOwnedSlice(heap_args),
            .span = span,
        });
    }
};