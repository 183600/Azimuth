// 标准 MoonBit 测试用例 - 测试核心功能

test "add_function_basic" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_function_basic" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "greet_function_basic" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
}

test "mathematical_properties" {
  @azimuth.assert_eq(@azimuth.add(7, 13), @azimuth.add(13, 7))
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
}

test "edge_cases" {
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "complex_arithmetic" {
  let sum = @azimuth.add(5, 3)
  let product = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(16, product)
}

test "unicode_handling" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "divide_with_ceil_comprehensive" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "practical_scenarios" {
  let subtotal = @azimuth.add(199, 299)
  @azimuth.assert_eq(498, subtotal)
  
  let boxes_needed = @azimuth.divide_with_ceil(25, 6)
  @azimuth.assert_eq(5, boxes_needed)
  
  let greeting = @azimuth.greet("张三")
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
}