const std = @import("std");
const B32LLPC = @import("B32LLPC");

const temp_ast = @import("ast.zig");
const StringInternPool = @import("string_intern_pool.zig").StringInternPool;

const max_bytes = 1_000_000;

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const arena = init.arena.allocator();

    const filepath = "code/test_type_val.llpc";
    const filetext = try std.Io.Dir.cwd().readFileAlloc(init.io, filepath, gpa, .limited(max_bytes)); //0.16.0-dev.2146+98db4570b version
    defer gpa.free(filetext);
    
    //In the future, use the first nonself argument for what filepath to load...
    std.debug.print("Args are:\n", .{});
    for (try init.minimal.args.toSlice(arena)) |arg| {
        std.debug.print(" - {s}\n", .{arg});
    }

    var intern: StringInternPool = .init(arena);
    intern.globalize();

    const slir = try temp_ast.parse(arena, &intern, filetext);
    std.debug.print("{f}\n", .{slir});

    //var root: @import("types.zig").partial.Root = .{.integer = .{.signed = false, .bits = 32}};
    //std.debug.print("{f}\n", .{@import("types.zig").partial.Type.fromRoot(&root)});
}