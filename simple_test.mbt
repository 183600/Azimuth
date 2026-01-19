// 简单的 MoonBit 测试用例
// 测试基本功能

test "simple_add_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(5, -6))
}

test "simple_multiply_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
}

test "simple_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "overflow_test" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_test" {
  let length = 10
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
}

test "string_special_chars_test" {
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, $100!", @azimuth.greet("$100"))
}

test "math_properties_test" {
  let a = 7
  let b = 11
  
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "real_world_test" {
  let price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(617, total)
}

test "boundary_test" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}