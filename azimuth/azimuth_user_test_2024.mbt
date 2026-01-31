// Azimuth 用户测试用例 - 2024年新增10个标准 MoonBit 测试用例

test "add_function_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_function_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "subtract_function_test" {
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_function_test" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_test" {
  let x = 10
  let y = 5
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "negative_numbers_test" {
  assert_eq(-1, add(-3, 2))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-5, subtract(0, 5))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "edge_cases_test" {
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(5, 1))
}

test "complex_calculation_test" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
}

test "business_logic_test" {
  let items = 25
  let capacity = 6
  let boxes_needed = divide_with_ceil(items, capacity)
  assert_eq(5, boxes_needed)
}