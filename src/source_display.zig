const std = @import("std");

var state: State = undefined;

pub const State = struct {
    source: []u8,
    lines: std.ArrayList(Line) = .empty,

    const Line = struct {
        ///Index to the start of the first non-whitespace character
        index_content: usize,
        ///Index to the start of the line
        index_start: usize,
        ///A normalized count of the amount of whitespace, tabs = 4, space = 1
        amt_whitespace: usize = 0,
        ///Length from the true start to the newline character. Eg) `ab\n` would be 2
        line_length: usize = 0,

    };

    pub const Span = struct {
        line_start: usize,  //Inclusive
        line_end: usize,    //Inclusive

        col_start: usize,   //Inclusive
        col_end: usize,     //Inclusive

        pub fn format(self: Span, writer: *std.Io.Writer) !void {
            try writer.print("(", .{});
            if (self.line_start != self.line_end){
                try writer.print("{}:{}–{}:{}", .{self.line_start+1, self.col_start+1, self.line_end+1, self.col_end+1});
            } else if (self.col_start != self.col_end) {
                try writer.print("{}:{}–{}", .{self.line_start+1, self.col_start+1, self.col_end+1});
            } else {
                try writer.print("{}:{}", .{self.line_start+1, self.col_start+1});
            }
            try writer.print(")", .{});
        }

        pub fn fromSlice(slice: []u8) Span {
            var run = slice.len - 1;
            while (std.ascii.isWhitespace(slice[run])) {
                run -= 1;
            }
            const start_idx = slice.ptr - state.source.ptr;
            const end_idx = start_idx + run;

            const line_start = lineFromIndex(start_idx);
            const line_end = lineFromIndex(end_idx);

            const col_start = start_idx - state.lines.items[line_start].index_start;
            const col_end = end_idx - state.lines.items[line_end].index_start;

            return .{
                .line_start = line_start,
                .line_end = line_end,
                .col_start = col_start,
                .col_end = col_end,
            };
        }

        pub fn display(self: Span, writer: *std.Io.Writer, comptime msg: []const u8, args: anytype) !void {
            var min_whitespace: usize = std.math.maxInt(usize);
            for (self.line_start..self.line_end+1) |line_id| {
                const line = state.lines.items[line_id];
                min_whitespace = @min(min_whitespace, line.amt_whitespace);
            }

            const line_end_len: usize = 1 + std.math.log10_int(self.line_end + 1);
            for (self.line_start..self.line_end+1) |line_id| {
                const line = state.lines.items[line_id];
                var line_buf: [32]u8 = undefined;
                const line_str = try std.fmt.bufPrint(&line_buf, "{}", .{line_id + 1});
                var align_buf: [1024]u8 = @splat(' ');
                const int_align = align_buf[0..line_end_len-line_str.len];
                const content_length = line.index_start + line.line_length - line.index_content;
                const content = state.source[line.index_content..][0..content_length];
                const reduced_whitespace = align_buf[0..line.amt_whitespace - min_whitespace];
                //std.debug.print("{}\n", .{line});
                try writer.print("{s}{s} | {s}{s}\n", .{line_str, int_align, reduced_whitespace, content});
                
                var col_start: usize = 0;
                if (line_id == self.line_start) {
                    col_start = self.col_start;
                }

                var col_end: usize = reduced_whitespace.len + content.len;
                if (line_id == self.line_end) {
                    col_end = reduced_whitespace.len + self.col_end;
                }
                const indic: [1024]u8 = @splat('^');
                try writer.print("{s}{s} | {s}\n", .{align_buf[0..line_str.len], align_buf[0..int_align.len], indic[col_start..col_end+1]});
                //try writer.print("{s}{s} | `{s}`\n", .{line_str, int_align, state.source[line.index_start..][0..line.line_length]});
            }
            var align_buf: [1024]u8 = @splat(' ');
            try writer.print("{s}   ", .{align_buf[0..line_end_len]});
            try writer.print(msg, args);
            try writer.print("\n", .{});
        }
    };

    pub fn lineFromIndex(idx: usize) usize {
        var low: usize = 0;
        var high = state.lines.items.len - 1;
        while (low != high) {
            const mid = (low + high) / 2;
            const mid_idx = state.lines.items[mid].index_start;
            if (mid_idx == idx) return mid;
            if (mid_idx > idx) high = mid;
            if (mid_idx < idx) low = mid;
            if (high == low + 1) return low;
        }
        return low;
    }

    pub fn init(alloc: std.mem.Allocator, sourcefile: []u8) !void {
        var self: State = .{.source = sourcefile};
        try self.lines.append(alloc, .{.index_content = 0, .index_start = 0});
        var index: usize = 0;
        while (index < sourcefile.len) {
            var char: u8 = sourcefile[index];
            while (std.ascii.isWhitespace(char)) {
                switch (char) {
                    ' ' => self.lines.items[self.lines.items.len - 1].amt_whitespace += 1,
                    '\t' => self.lines.items[self.lines.items.len - 1].amt_whitespace += 4,
                    '\n' => break,
                    else => {},
                }
                self.lines.items[self.lines.items.len - 1].line_length += 1;
                self.lines.items[self.lines.items.len - 1].index_content += 1;

                index += 1;
                if (index >= sourcefile.len) break;
                char = sourcefile[index];
            }
            while (char != '\n') {
                self.lines.items[self.lines.items.len - 1].line_length += 1;
                index += 1;
                if (index >= sourcefile.len) break;
                char = sourcefile[index];
            }
            index += 1;
            try self.lines.append(alloc, .{.index_content = index, .index_start = index});
            continue;
        }
        const lastline = &self.lines.items[self.lines.items.len - 1];
        if (lastline.line_length == 0) {
            _ = self.lines.pop().?;
        }
        state = self;
    }
};