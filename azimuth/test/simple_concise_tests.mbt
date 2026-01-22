// 简洁的标准 MoonBit 测试用例
// 覆盖 azimuth 项目的主要功能

test "basic_add_functionality" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "basic_multiply_functionality" {
  let result = @azimuth.multiply(3, 4)
  @azimuth.assert_eq(12, result)
}

test "basic_greet_functionality" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "basic_divide_with_ceil" {
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
}

test "add_negative_numbers" {
  let result = @azimuth.add(-5, 3)
  @azimuth.assert_eq(-2, result)
}

test "multiply_negative_numbers" {
  let result = @azimuth.multiply(-3, 4)
  @azimuth.assert_eq(-12, result)
}

test "divide_with_ceil_negative" {
  let result = @azimuth.divide_with_ceil(-10, 3)
  @azimuth.assert_eq(-3, result)
}

test "greet_empty_string" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

test "complex_calculation" {
  let sum = @azimuth.add(5, 3)
  let product = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(16, product)
}

test "zero_operations" {
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}