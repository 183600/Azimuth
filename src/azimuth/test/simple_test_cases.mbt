// 简化的 MoonBit 测试用例
// 包含10个基本的测试用例

test "simple_add_test" {
  let result = add(2, 3)
  assert_eq(5, result)
}

test "simple_multiply_test" {
  let result = multiply(3, 4)
  assert_eq(12, result)
}

test "simple_greet_test" {
  let result = greet("World")
  assert_eq_string("Hello, World!", result)
}

test "add_negative_numbers" {
  let result = add(-5, -3)
  assert_eq(-8, result)
}

test "multiply_negative_numbers" {
  let result = multiply(-3, 4)
  assert_eq(-12, result)
}

test "add_zero" {
  let result = add(5, 0)
  assert_eq(5, result)
}

test "multiply_by_zero" {
  let result = multiply(5, 0)
  assert_eq(0, result)
}

test "greet_empty_string" {
  let result = greet("")
  assert_eq_string("Hello, !", result)
}

test "assert_functions_test" {
  assert_eq(1, 1)
  assert_eq(1, 1)
  assert_eq(10, 10)
  assert_eq_string("test", "test")
}

test "complex_calculation" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
}