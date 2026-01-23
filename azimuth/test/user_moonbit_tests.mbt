// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_basic" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_basic" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "greet_basic" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "divide_with_ceil_basic" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "math_properties" {
  @azimuth.assert_eq(@azimuth.add(7, 13), @azimuth.add(13, 7))
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
}

test "edge_cases" {
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "complex_ops" {
  let result = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result)
}

test "negative_numbers" {
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "large_numbers" {
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(10001, 10))
}

test "business_logic" {
  let items = 25
  let capacity = 6
  let boxes = @azimuth.divide_with_ceil(items, capacity)
  @azimuth.assert_eq(5, boxes)
}