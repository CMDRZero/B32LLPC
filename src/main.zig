const std = @import("std");
const B32LLPC = @import("B32LLPC");

const temp_ast = @import("ast.zig");
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;

const max_bytes = 1_000_000;

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const alloc = gpa.allocator();
    const arena = std.heap.ArenaAllocator.init(alloc);

    const filepath = "code/test_0.llpc";
    _ = arena;
    const filetext = try std.fs.cwd().readFileAlloc(alloc, filepath, max_bytes);
    defer alloc.free(filetext);
    
    var intern: StringInternPool = .init(alloc);
    intern.globalize();

    const slir = try temp_ast.parse(alloc, &intern, filetext);
    std.debug.print("{f}", .{slir});
}