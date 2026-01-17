// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，包含实用的测试场景

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_arithmetic_subtraction" {
  // 基本减法运算测试
  @azimuth.assert_eq(5, @azimuth.subtract(8, 3))
  @azimuth.assert_eq(-10, @azimuth.subtract(5, 15))
  @azimuth.assert_eq(0, @azimuth.subtract(7, 7))
  @azimuth.assert_eq(20, @azimuth.subtract(30, 10))
}

test "string_length_operations" {
  // 字符串长度操作测试
  @azimuth.assert_eq(5, @azimuth.string_length("Hello"))
  @azimuth.assert_eq(0, @azimuth.string_length(""))
  @azimuth.assert_eq(11, @azimuth.string_length("MoonBit Test"))
  @azimuth.assert_eq(1, @azimuth.string_length("A"))
}

test "array_basic_operations" {
  // 数组基本操作测试
  let empty_array = @azimuth.create_empty_array()
  @azimuth.assert_eq(0, @azimuth.array_length(empty_array))
  
  let single_element = @azimuth.array_push(empty_array, 42)
  @azimuth.assert_eq(1, @azimuth.array_length(single_element))
  @azimuth.assert_eq(42, @azimuth.array_get(single_element, 0))
  
  let multiple_elements = @azimuth.array_push(single_element, 84)
  @azimuth.assert_eq(2, @azimuth.array_length(multiple_elements))
  @azimuth.assert_eq(84, @azimuth.array_get(multiple_elements, 1))
}

test "boolean_logic_operations" {
  // 布尔逻辑运算测试
  @azimuth.assert_eq(true, @azimuth.and(true, true))
  @azimuth.assert_eq(false, @azimuth.and(true, false))
  @azimuth.assert_eq(false, @azimuth.and(false, true))
  @azimuth.assert_eq(false, @azimuth.and(false, false))
  
  @azimuth.assert_eq(true, @azimuth.or(true, true))
  @azimuth.assert_eq(true, @azimuth.or(true, false))
  @azimuth.assert_eq(true, @azimuth.or(false, true))
  @azimuth.assert_eq(false, @azimuth.or(false, false))
}

test "comparison_equality_tests" {
  // 比较相等性测试
  @azimuth.assert_eq(true, @azimuth.equal(5, 5))
  @azimuth.assert_eq(false, @azimuth.equal(5, 3))
  @azimuth.assert_eq(true, @azimuth.equal(0, 0))
  @azimuth.assert_eq(false, @azimuth.equal(-1, 1))
  
  @azimuth.assert_eq(false, @azimuth.not_equal(5, 5))
  @azimuth.assert_eq(true, @azimuth.not_equal(5, 3))
  @azimuth.assert_eq(false, @azimuth.not_equal(0, 0))
  @azimuth.assert_eq(true, @azimuth.not_equal(-1, 1))
}

test "mathematical_absolute_value" {
  // 数学绝对值测试
  @azimuth.assert_eq(5, @azimuth.abs(5))
  @azimuth.assert_eq(5, @azimuth.abs(-5))
  @azimuth.assert_eq(0, @azimuth.abs(0))
  @azimuth.assert_eq(100, @azimuth.abs(-100))
  @azimuth.assert_eq(42, @azimuth.abs(42))
}

test "error_handling_division_by_zero" {
  // 错误处理：除零测试
  let normal_result = @azimuth.safe_divide(10, 2)
  @azimuth.assert_eq(true, @azimuth.is_success(normal_result))
  @azimuth.assert_eq(5, @azimuth.get_value(normal_result))
  
  let zero_division = @azimuth.safe_divide(10, 0)
  @azimuth.assert_eq(false, @azimuth.is_success(zero_division))
  @azimuth.assert_eq_string("Division by zero error", @azimuth.get_error(zero_division))
}

test "string_case_conversion" {
  // 字符串大小写转换测试
  @azimuth.assert_eq_string("HELLO", @azimuth.to_uppercase("Hello"))
  @azimuth.assert_eq_string("world", @azimuth.to_lowercase("WORLD"))
  @azimuth.assert_eq_string("MOONBIT", @azimuth.to_uppercase("MoonBit"))
  @azimuth.assert_eq_string("test", @azimuth.to_lowercase("TEST"))
}