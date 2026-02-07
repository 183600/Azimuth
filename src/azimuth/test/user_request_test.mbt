// 用户请求的 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "basic_multiplication" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "string_greeting" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "ceiling_division" {
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
}

test "subtraction" {
  @azimuth.assert_eq(2, @azimuth.subtract(5, 3))
}

test "edge_case_zero" {
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "negative_numbers" {
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "large_numbers" {
  @azimuth.assert_eq(2000, @azimuth.add(1000, 1000))
}

test "mixed_operations" {
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "complex_calculation" {
  let x = 10
  let y = 4
  let sum = @azimuth.add(x, y)
  @azimuth.assert_eq(14, sum)
}