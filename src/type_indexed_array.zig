const std = @import("std");

pub fn PackedEnumArray(IndexType: type, ValueType: type) type {
    if (@typeInfo(IndexType) != .@"enum") @compileError("Expected an enum for key of EnumArray");
    return struct {
        const Self = @This();
        
        array: [numEnumFields(IndexType)] ValueType,

        pub fn initFilled(default_value: ValueType) Self {
            return .{ .array = @splat(default_value) };
        }

        pub fn get(self: Self, index: IndexType) ValueType {
            return self.array[enumFieldIndex(IndexType, index)];
        }

        pub fn getMut(self: *Self, index: IndexType) *ValueType {
            return &self.array[enumFieldIndex(IndexType, index)];
        }

        pub fn set(self: *Self, index: IndexType, value: ValueType) void {
            self.array[enumFieldIndex(IndexType, index)] = value;
        }
    };
}

fn numEnumFields(Enum: type) comptime_int {
    return @typeInfo(Enum).@"enum".fields.len;
}

fn EnumInt(Enum: type) type {
    return std.math.IntFittingRange(0, numEnumFields(Enum) - 1);
}

fn enumFieldIndex(Enum: type, field: Enum) EnumInt(Enum) {
    switch (field) {
        inline else => |comptime_field| {
            return comptimeEnumFieldIndex(Enum, comptime_field);
        }
    }
    comptime unreachable;
}

fn comptimeEnumFieldIndex(Enum: type, comptime field: Enum) EnumInt(Enum) {
    inline for (@typeInfo(Enum).@"enum".fields, 0..) |enumfield, i| {
        if (@intFromEnum(field) == enumfield.value) return i;
    }
    comptime unreachable;
}

test "Simple PackedEnum Test" {
    const Direction = enum (u4) {
        north = 0b0001,
        south = 0b0010,
        west  = 0b0100,
        east  = 0b1000,
    };

    const Walls = PackedEnumArray(Direction, u8);

    var walls: Walls = .initFilled(0);
    walls.set(.north, 1);
    try std.testing.expectEqual(0, walls.get(.south));
    try std.testing.expectEqual(4, @sizeOf(Walls));
}

pub fn TypeIndexArray(IndexType: type, ValueType: type) type {
    if (!(isEnum(IndexType) or isInt(IndexType) or isStruct(IndexType))) @compileError("Expected an enum, int, or struct for key of TypeIndexArray");
    return struct {
        const Self = @This();

        array: [numValues(IndexType)] ValueType,

        pub fn initFilled(default_value: ValueType) Self {
            return .{ .array = @splat(default_value) };
        }

        pub fn get(self: Self, index: IndexType) ValueType {
            return self.array[toInt(IndexType, index)];
        }

        pub fn getMut(self: *Self, index: IndexType) *ValueType {
            return &self.array[toInt(IndexType, index)];
        }

        pub fn set(self: *Self, index: IndexType, value: ValueType) void {
            self.array[toInt(IndexType, index)] = value;
        }
    };
}

pub fn TypeIndexArrayPointer(IndexType: type, ValueType: type) type {
    if (!(isEnum(IndexType) or isInt(IndexType) or isStruct(IndexType))) @compileError("Expected an enum, int, or struct for key of TypeIndexArray");
    return struct {
        const Self = @This();

        ptrarray: * [numValues(IndexType)] ValueType,

        pub fn initFromPtr(ptrarray: * [numValues(IndexType)] ValueType) Self {
            return .{ .ptrarray = ptrarray};
        }

        pub fn get(self: Self, index: IndexType) ValueType {
            return self.ptrarray.*[toInt(IndexType, index)];
        }

        pub fn getMut(self: *Self, index: IndexType) *ValueType {
            return &self.ptrarray.*[toInt(IndexType, index)];
        }

        pub fn set(self: *Self, index: IndexType, value: ValueType) void {
            self.ptrarray.*[toInt(IndexType, index)] = value;
        }
    };
}

pub fn TypeIndexSlice(IndexType: type, ValueType: type) type {
    if (!(isEnum(IndexType) or isInt(IndexType) or isStruct(IndexType))) @compileError("Expected an enum, int, or struct for key of TypeIndexSlice");
    return struct {
        const Self = @This();

        slice: [] ValueType,

        pub fn initFilled(default_value: ValueType) Self {
            return .{ .slice = @splat(default_value) };
        }

        pub fn get(self: Self, index: IndexType) ValueType {
            return self.slice[toInt(IndexType, index)];
        }

        pub fn getMut(self: *Self, index: IndexType) *ValueType {
            return &self.slice[toInt(IndexType, index)];
        }

        pub fn set(self: *Self, index: IndexType, value: ValueType) void {
            self.slice[toInt(IndexType, index)] = value;
        }
    };
}

fn isEnum(Type: type) bool {
    return @typeInfo(Type) == .@"enum";
}

fn isInt(Type: type) bool {
    return @typeInfo(Type) == .int;
}

fn isStruct(Type: type) bool {
    return @typeInfo(Type) == .@"struct";
}

fn BackingInt(Type: type) type {
    if (comptime isEnum(Type)) {
        return @typeInfo(Type).@"enum".tag_type;
    } else if (comptime isInt(Type)) {
        return Type;
    } else if (comptime std.meta.hasMethod(Type, "toIndex")) {
        return Type.IndexType;
    } else {
        return @typeInfo(Type).@"struct".backing_integer.?;
    }
}

fn toInt(Type: type, value: Type) BackingInt(Type) {
    if (comptime isEnum(Type)) {
        return @intFromEnum(value);
    } else if (comptime isInt(Type)) {
        return value;
    } else if (comptime std.meta.hasMethod(Type, "toIndex")) {
        return Type.toIndex(value);
    } else {
        return @bitCast(value);
    }
}

fn numValues(Type: type) comptime_int {
    if (comptime isEnum(Type)) {
        var max: comptime_int = -1_000_000;
        var min: comptime_int = 1_000_000;
        for (std.meta.fields(Type)) |enumfield| {
            max = @max(max, enumfield.value);
            min = @min(min, enumfield.value);
        }
        return max - min + 1;
    } else if (comptime isInt(Type)) {
        return std.math.maxInt(Type) - std.math.minInt(Type) + 1;
    } else if (comptime std.meta.hasMethod(Type, "toIndex")) {
        return Type.max_index - Type.min_index + 1;
    } else {
        const BackingInteger = @typeInfo(Type).@"struct".backing_integer.?;
        return std.math.maxInt(BackingInteger) - std.math.minInt(BackingInteger) + 1;
    }
}

pub fn assertIsCompactIndexStruct(Type: type) void {
    if (!std.meta.hasMethod(Type, "toIndex")) @compileError(std.fmt.comptimePrint("Missing method `toIndex` on type `{s}`, is it marked `pub`?", .{@typeName(Type)}));
    if (!@hasDecl(Type, "min_index")) @compileError(std.fmt.comptimePrint("Missing const declaration `min_index` on type `{s}`, is it marked `pub`?", .{@typeName(Type)}));
    if (!@hasDecl(Type, "max_index")) @compileError(std.fmt.comptimePrint("Missing const declaration `max_index` on type `{s}`, is it marked `pub`?", .{@typeName(Type)}));
    if (!@hasDecl(Type, "IndexType")) @compileError(std.fmt.comptimePrint("Missing const declaration `IndexType` on type `{s}`, is it marked `pub`?", .{@typeName(Type)}));
    
}

test "IndexTypeArray" {
    const Color = enum (u2) {
        white = 0,
        black = 2,
    };
    const ColorStruct = packed struct (u2) {
        color: Color,
    };

    const WinningStruct = TypeIndexArray(ColorStruct, bool);
    var winning_struct: WinningStruct = .initFilled(false);
    winning_struct.set(.{.color = .white}, true);
    winning_struct.set(.{.color = .black}, true);
    try std.testing.expectEqual([4]bool{true, false, true, false}, winning_struct.array);

    const WinningEnum = TypeIndexArray(Color, bool);
    var winning_enum: WinningEnum = .initFilled(false);
    winning_enum.set(.white, true);
    winning_enum.set(.black, true);
    try std.testing.expectEqual([3]bool{true, false, true}, winning_enum.array);

    const CompactColorStruct = packed struct (u2) {
        color: Color,

        const min_index = 0;
        const max_index = 1;
        const IndexType = u1;

        pub fn toIndex(self: @This()) IndexType {
            if (self.color == .black) return 1;
            return 0;
        }
    };

    const WinningCompactStruct = TypeIndexArray(CompactColorStruct, bool);
    var winning_compact_struct: WinningCompactStruct = .initFilled(false);
    winning_compact_struct.set(.{.color = .white}, true);
    winning_compact_struct.set(.{.color = .black}, true);
    try std.testing.expectEqual([2]bool{true, true}, winning_compact_struct.array);


    const WinningCompactEnum = PackedEnumArray(Color, bool);
    var winning_compact_enum: WinningCompactEnum = .initFilled(false);
    winning_compact_enum.set(.white, true);
    winning_compact_enum.set(.black, true);
    try std.testing.expectEqual([2]bool{true, true}, winning_compact_enum.array);
}