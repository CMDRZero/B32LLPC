const std = @import("std");

/// Converts the message string from lowercase to lowerCamelCase. 
/// Asserts the buffer is at long enough.
/// eg) abc_def_ghi --> abcDefGhi
pub fn lowercaseToLowerCamel(buf: []u8, msg: []const u8) []u8 {
    var ret: []u8 = buf[0..0];
    var splits = std.mem.splitScalar(u8, msg, '_');
    if (splits.next()) |first| {
        ret.len += first.len;
        std.debug.assert(ret.len <= buf.len);
        @memcpy(ret, first);
    } else return msg;
    while (splits.next()) |segment| {
        const old_len = ret.len;
        ret.len += segment.len;
        std.debug.assert(ret.len <= buf.len);
        @memcpy(ret[old_len..], segment);
        ret[old_len] = std.ascii.toUpper(ret[old_len]);
    }
    return ret;
}

pub fn comptimeLowercaseToLowerCamel(comptime msg: []const u8) *const [msg.len - std.mem.countScalar(u8, msg, '_')]u8 {
    @setEvalBranchQuota(20_000);
    comptime var buf: [msg.len - std.mem.countScalar(u8, msg, '_')]u8 = undefined;
    _ = comptime lowercaseToLowerCamel(&buf, msg);
    const final = buf;
    return &final;
}