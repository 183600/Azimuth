// 新的 MoonBit 测试用例
// 包含10个基本的测试用例

test "add_basic_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "multiply_basic_test" {
  let result = @azimuth.multiply(3, 4)
  @azimuth.assert_eq(12, result)
}

test "greet_basic_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "add_negative_test" {
  let result = @azimuth.add(-5, -3)
  @azimuth.assert_eq(-8, result)
}

test "multiply_negative_test" {
  let result = @azimuth.multiply(-3, 4)
  @azimuth.assert_eq(-12, result)
}

test "add_zero_test" {
  let result = @azimuth.add(5, 0)
  @azimuth.assert_eq(5, result)
}

test "multiply_zero_test" {
  let result = @azimuth.multiply(5, 0)
  @azimuth.assert_eq(0, result)
}

test "greet_empty_test" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

test "assert_functions_test" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(10, 10)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_calculation_test" {
  let result = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result)
}