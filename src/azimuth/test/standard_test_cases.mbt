// 标准 MoonBit 测试用例

test "add_positive_numbers" {
  let result = @azimuth.add(15, 25)
  @azimuth.assert_eq(40, result)
}

test "add_negative_numbers" {
  let result = @azimuth.add(-10, -20)
  @azimuth.assert_eq(-30, result)
}

test "add_mixed_numbers" {
  let result = @azimuth.add(50, -30)
  @azimuth.assert_eq(20, result)
}

test "multiply_positive_numbers" {
  let result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, result)
}

test "multiply_negative_numbers" {
  let result = @azimuth.multiply(-4, -5)
  @azimuth.assert_eq(20, result)
}

test "multiply_mixed_numbers" {
  let result = @azimuth.multiply(8, -3)
  @azimuth.assert_eq(-24, result)
}

test "greet_with_name" {
  let result = @azimuth.greet("Alice")
  @azimuth.assert_eq_string("Hello, Alice!", result)
}

test "greet_with_empty_string" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

test "boundary_value_addition" {
  let max_val = 2147483647
  let result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result)
}

test "boundary_value_multiplication" {
  let min_val = -2147483648
  let result = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result)
}