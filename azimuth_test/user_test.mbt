// 用户请求的 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition" {
  assert_eq(5, add(2, 3))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "string_greeting" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "ceiling_division" {
  assert_eq(3, divide_with_ceil(10, 4))
}

test "subtraction" {
  assert_eq(2, subtract(5, 3))
}

test "edge_case_zero" {
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_numbers" {
  assert_eq(-5, add(-2, -3))
}

test "large_numbers" {
  assert_eq(2000, add(1000, 1000))
}

test "mixed_operations" {
  assert_eq(0, add(5, -5))
}

test "complex_calculation" {
  let x = 10
  let y = 4
  let sum = add(x, y)
  assert_eq(14, sum)
}