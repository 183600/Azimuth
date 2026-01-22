// 标准 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "basic_addition_functionality" {
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(2, -7))
}

test "basic_multiplication_functionality" {
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(-21, @azimuth.multiply(7, -3))
}

test "string_greeting_functionality" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_basic_cases" {
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "mathematical_properties" {
  let a = 8
  let b = 13
  
  // 交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 单位元
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "boundary_value_testing" {
  let max_int = 2147483647
  let min_int = -2147483648
  
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
}

test "complex_calculation_scenario" {
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(96, area)
  @azimuth.assert_eq(40, perimeter)
}

test "edge_case_divisions" {
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}