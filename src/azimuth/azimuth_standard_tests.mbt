// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "basic_addition" {
  assert_eq(5, add(2, 3))
}

test "addition_with_zero" {
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
}

test "addition_overflow" {
  let max_val = 2147483647
  assert_eq(max_val, add(max_val, 1))
}

test "basic_multiplication" {
  assert_eq(12, multiply(3, 4))
}

test "multiplication_with_zero" {
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
}

test "multiplication_with_negative" {
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "greet_empty_string" {
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation" {
  // 测试复合运算：(2 + 3) * 4 = 20
  let sum = add(2, 3)
  let result = multiply(sum, 4)
  assert_eq(20, result)
}