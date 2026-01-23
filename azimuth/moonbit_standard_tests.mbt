// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "multiply_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
}

test "greet_basic" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "math_properties" {
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "edge_cases" {
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "complex_ops" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
}

test "negative_numbers" {
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "large_numbers" {
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1001, divide_with_ceil(10001, 10))
}

test "business_logic" {
  let items = 25
  let capacity = 6
  let boxes = divide_with_ceil(items, capacity)
  assert_eq(5, boxes)
}