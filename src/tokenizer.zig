const std = @import("std");

pub const TokenTag = enum {
    symbol,
    identifier,
    literal,
    kind,
};

pub const Operator = struct {
    tag: Tag,
    isOverloaded: bool,
    isInplace: bool,
    postMod: PostMod,

    pub const PostMod = enum {
        none,
        wrapping,
        clamping,
    };

    pub const Tag = enum {
        @"+",
        @"-",
        @"*",
        @"%",
        @"/",

        @"<<",
        @">>",

        @"&",
        @"&~",
        @"|",
        @"|~",
        @"^",
        @"^~",

        @"orelse",
        
        @"==",
        @"!=",
        @"<",
        @">",
        @"<=",
        @">=",
        
        @"and",
        @"or",
    };

    pub fn validate(self: Operator) !void {
        if (self.isOverloaded) {
            switch (self.tag) {
                .@"orelse", .@"and", .@"or", 
                .@"==", .@"!=", .@"<", .@">", .@"<=", .@">=" => return error.not_overloadable,
                else => {},
            }
            if (self.postMod != .none) return error.cannot_postmodify_overload;
        }
        if (self.postMod != .none) switch (self.tag) {
            .@"+", .@"-", .@"*", .@"<<" => {},
            else => return error.not_postmodifiable,
        };
    }
};

pub const Token = union (TokenTag) {
    symbol: Symbol,
    identifier: Identifier,
    literal: Literal,
    kind: Kind,

    pub const Symbol = enum {
        @"break",
        @"continue",

        @"yield",
        @"return",

        @"const",
        @"var",
        @"volatile",

        @"if",
        @"else",
        @"while",
        @"for",
        @"do",

        @"fn",
        @"struct",
        @"enum",
        @"union",

        @"inline",

        @"assume",
        @"assert",
        @"checked",

        @"unreachable",

        @"+",
        @"-",
        @"*",
        @"/",
        @"%",
        
        @"<<",
        @">>",
        
        @"&",
        @"&~",
        @"|",
        @"|~",
        @"^",
        @"^~",

        @"orelse",

        @"==",
        @"!=",
        @"<",
        @"<=",
        @">",
        @">=",

        @"and",
        @"or",

        @";",
        @".",
        @":",
        @"\"",
        @"'",
        @"$",
        @",",
        @"!",
        @"(",
        @")",
        @"_",
        @"=",
        @"{",
        @"}",
        @"[",
        @"]",
        @"~",
        @"?",
        @"..",
        
        @"mut",
        @"view",
        @"pinned",

        @"array",
        @"slice",
        @"type",
        @"void",
        @"null",
        @"bitalign",
        
        invalid,
        end_of_file,

        const maxlen = b: {
            var max = 0;
            for (std.meta.fieldNames(@This())) |name| {
                max = @max(name.len, max);
            }
            break :b max;
        };

        pub fn getFromString(str: [] u8) ?struct {usize, Symbol} {
            @setEvalBranchQuota(20_000);
            inline for (0..@This().maxlen) |x_| block: {
                const len = maxlen - x_;
                if (len > str.len) break :block;
                inline for (comptime std.meta.fieldNames(@This())) |fieldName| {
                    if (comptime fieldName.len != len) continue;
                    if (std.mem.startsWith(u8, str, fieldName)) return .{len, @field(@This(), fieldName)};
                }
            }
            return null;
        }
    };
    pub const Identifier = struct {
        str: [] u8,

        pub fn getFromString(str: [] u8) ?struct {usize, Identifier} {
            var len: usize = 0;
            if (str.len > 0 and isUAlph(str[0])) {
                len += 1;
                if (str[0] == '_' and str.len == 1) return null; //Special case `_` since its a special token not an identifier
                for (str[1..]) |c| {
                    if (!isUAlphNum(c)) break;
                    len += 1;
                }
                return .{len, .{.str = str[0..len]}};
            }
            return null;
        }

    };
    pub const Literal = struct {
        tag: union (enum) {
            character: u8,
            string: []u8,
            z85_int: []u8,
            dec_int: []u8,
            hex_int: []u8,
            bin_int: []u8,
            multiline_string: []u8,
        },

        pub fn getFromString(str: *[] u8) ?Literal {
            if (readCharLit(str)) |char| {
                return .{.tag = .{.character = char}};
            } else |_| {}

            if (readDecInt(str)) |substr| {
                return .{.tag = .{.dec_int = substr}};
            } else |_| {}

            return null;
        }

        //Improvement: Allow commiting to errors, eg dont fail to parse 'ab', commit it and note its invalid;
        fn readCharLit(str: *[] u8) !u8 {
            const err_copy = str.*;
            errdefer str.* = err_copy;

            try consumeChar(str, single_quote);
            const body = try popFullChar(str);
            try consumeChar(str, backslash);
            return body;
        }

        fn readDecInt(str: *[] u8) ![]u8 {
            const err_copy = str.*;
            errdefer str.* = err_copy;

            const char_0 = try peekChar(str);
            if (char_0 == '0') {
                if (peekChar(str)) |char_1| {
                    if (char_1 == 'x' or char_1 == 'b') return error.not_base_10;
                } else |_| {}
            }
            var count: usize = 0;
            var char = char_0;
            while (isUNum(char)) {
                _ = try popChar(str);
                count += 1;
                char = try peekChar(str);
            }
            if (count >= 1) return err_copy[0..count];
            return error.non_digit;
        }

        //Consumes the characters on success
        fn popFullChar(str: *[] u8) !u8 {
            const first_char = try popChar(str);
            if (first_char != backslash) { //Single character
                if (!std.ascii.isPrint(first_char)) return error.invalid_char;
                return first_char;
            }
            const escape = try popChar(str);
            if (escape != 'x') { //Single character escape
                const body: u8 = switch (escape) {
                    backslash => backslash,
                    single_quote => single_quote,
                    '"' => '"',
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    else => return error.unsupported_escape,
                };
                try consumeChar(str, backslash);
                return body;
            }
            //Hex escape
            const char_0 = try popChar(str);
            const char_1 = try popChar(str);
            if (!std.ascii.isHex(char_0)) return error.invalid_hex_escape;
            if (!std.ascii.isHex(char_1)) return error.invalid_hex_escape;
            return @as(u8, decodeHex(char_0)) << 4 | decodeHex(char_1);
        }

    };
    pub const Kind = struct {
        tag: Tag,
        bits: usize,

        const Tag = enum {
            unsigned_int,
            signed_int,
            float,
        };

        pub fn getFromString(str_: [] u8) ?struct {usize, Kind} {
            var str = str_;
            const char_0 = popChar(&str) catch return null;
            const tag: Tag = switch (char_0) {
                'u' => .unsigned_int,
                'i' => .signed_int,
                'f' => .float,
                else => return null,
            };

            var count: usize = 0;
            var value: usize = 0;
            var char = popChar(&str) catch return null;
            while (std.ascii.isDigit(char)) {
                value = 10 * value + (char - '0');
                count += 1;
                char = popChar(&str) catch return null;
            }
            if (count >= 1) return .{1+count, .{.tag = tag, .bits = value}};
            return null;
        }
    };

    pub fn format(self: Token, writer: *std.Io.Writer) !void {
        switch (self) {
            .symbol => |sym| try writer.print("symbol: {t}", .{sym}),
            .identifier => |id| try writer.print("identifier: {s}", .{id.str}),
            .literal => |lit| {
                try writer.print("literal.", .{});
                switch (lit.tag) {
                    .character => |char| try writer.print("character: {c}", .{char}),
                    .dec_int => |str| try writer.print("dec_int: {s}", .{str}),
                    else => unreachable,
                }
            },
            .kind => |kind| try writer.print("kind: {t}({})", .{kind.tag, kind.bits}),
            
        }
    }

    pub fn popFrom(str: *[] u8) Token {
        while (std.ascii.isWhitespace(try peekChar(str))) {
            str.* = str.*[1..];
        }
        
        //Note, this function commits changes to the slice if it succeeds
        if (Token.Literal.getFromString(str)) |literal| {
            return .{.literal = literal};
        }

        var bestlen: usize = 0;
        var ret: ?Token = null;
        
        if (Token.Symbol.getFromString(str.*)) |data| {
            const len, const sym = data;
            if (len > bestlen) {
                bestlen = len;
                ret = .{.symbol = sym};
            }
        }

        if (Token.Kind.getFromString(str.*)) |data| {
            const len, const kind = data;
            if (len > bestlen) {
                bestlen = len;
                ret = .{.kind = kind};
            }
        }

        if (Token.Identifier.getFromString(str.*)) |data| {
            const len, const id = data;
            if (len > bestlen) {
                bestlen = len;
                ret = .{.identifier = id};
            }
        }

        if (ret) |token| {
            str.* = str.*[bestlen..];
            while (std.ascii.isWhitespace(try peekChar(str))) {
                str.* = str.*[1..];
            }
            return token;
        }
        if (str.len == 0) return .{.symbol = .end_of_file};
        return .{.symbol = .invalid};
    }

    fn consumeChar(str: *[] u8, char: u8) !void {
        if (str.len > 0) {
            if (str.*[0] == char) {
                str.* = str.*[1..];
            } else return error.mismatch;
        } else return error.out_of_bounds;
    }

    fn popChar(str: *[] u8) !u8 {
        if (str.len > 0) {
            defer str.* = str.*[1..];
            return str.*[0];
        }
        return '\x00';
        //return error.out_of_bounds;
    }

    fn peekChar(str: *[] u8) !u8 {
        if (str.len > 0) {
            return str.*[0];
        }
        return '\x00';
        //return error.out_of_bounds;
    }

    
};

inline fn isUAlph(x: u8) bool {
    return x == '_' 
    or 'a' <= x and x <= 'z' 
    or 'A' <= x and x <= 'Z';
}
inline fn isUAlphNum(x: u8) bool {
    return x == '_' 
    or 'a' <= x and x <= 'z' 
    or 'A' <= x and x <= 'Z' 
    or '0' <= x and x <= '9';
}
inline fn isUNum(x: u8) bool {
    return x == '_' 
    or '0' <= x and x <= '9';
}

fn decodeHex(x: u8) u4 {
    std.debug.assert(std.ascii.isHex(x));
    return @intCast(switch (x) {
        '0'...'9' => x - '0',
        'a'...'f' => x - 'a' + 10,
        'A'...'F' => x - 'A' + 10,
        else => unreachable,
    });
}

const single_quote = '\'';
const backslash = '\\';