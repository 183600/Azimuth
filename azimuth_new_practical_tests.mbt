// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含实用测试场景

test "integer_arithmetic" {
  // 整数算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(9, @azimuth.subtract(20, 11))
  @azimuth.assert_eq(7, @azimuth.divide(21, 3))
}

test "string_operations" {
  // 字符串操作测试
  @azimuth.assert_eq_string("MoonBit Test", @azimuth.concat("MoonBit", " Test"))
  @azimuth.assert_eq_string("2026", @azimuth.int_to_string(2026))
  @azimuth.assert_eq(42, @azimuth.string_to_int("42"))
}

test "boolean_logic" {
  // 布尔逻辑测试
  @azimuth.assert_eq(true, @azimuth.or(true, false))
  @azimuth.assert_eq(false, @azimuth.and(true, false))
  @azimuth.assert_eq(true, @azimuth.not(false))
  @azimuth.assert_eq(true, @azimuth.xor(true, false))
}

test "comparison_tests" {
  // 比较测试
  @azimuth.assert_eq(true, @azimuth.greater_than(100, 50))
  @azimuth.assert_eq(true, @azimuth.less_than(10, 20))
  @azimuth.assert_eq(true, @azimuth.equal(42, 42))
  @azimuth.assert_eq(false, @azimuth.not_equal(42, 42))
}

test "mathematical_functions" {
  // 数学函数测试
  @azimuth.assert_eq(25, @azimuth.square(5))
  @azimuth.assert_eq(125, @azimuth.cube(5))
  @azimuth.assert_eq(0, @azimuth.abs(-0))
  @azimuth.assert_eq(10, @azimuth.abs(-10))
}

test "array_functionality" {
  // 数组功能测试
  let empty_array = @azimuth.create_array(0)
  @azimuth.assert_eq(0, @azimuth.array_length(empty_array))
  
  let filled_array = @azimuth.create_array(3)
  @azimuth.assert_eq(3, @azimuth.array_length(filled_array))
  
  let new_array = @azimuth.array_push(filled_array, 99)
  @azimuth.assert_eq(4, @azimuth.array_length(new_array))
  @azimuth.assert_eq(99, @azimuth.array_get(new_array, 3))
}

test "error_conditions" {
  // 错误条件测试
  let normal_result = @azimuth.safe_divide(20, 4)
  @azimuth.assert_eq(true, @azimuth.is_success(normal_result))
  @azimuth.assert_eq(5, @azimuth.get_value(normal_result))
  
  let error_result = @azimuth.safe_divide(20, 0)
  @azimuth.assert_eq(false, @azimuth.is_success(error_result))
  @azimuth.assert_eq(true, @azimuth.is_error(error_result))
}

test "boundary_cases" {
  // 边界情况测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 999))
  @azimuth.assert_eq(999, @azimuth.subtract(999, 0))
}