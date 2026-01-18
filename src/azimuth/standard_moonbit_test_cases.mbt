// 标准 MoonBit 测试用例
// 测试 azimuth 项目的主要功能

test "add_positive_numbers" {
  let result = add(10, 20)
  assert_eq(30, result)
}

test "add_with_zero" {
  assert_eq(15, add(15, 0))
  assert_eq(15, add(0, 15))
  assert_eq(0, add(0, 0))
}

test "add_negative_numbers" {
  let result = add(-10, -20)
  assert_eq(-30, result)
}

test "add_mixed_signs" {
  assert_eq(5, add(15, -10))
  assert_eq(-5, add(-15, 10))
  assert_eq(0, add(100, -100))
}

test "multiply_positive_numbers" {
  let result = multiply(6, 7)
  assert_eq(42, result)
}

test "multiply_with_zero_and_one" {
  assert_eq(0, multiply(25, 0))
  assert_eq(0, multiply(0, 25))
  assert_eq(25, multiply(25, 1))
  assert_eq(25, multiply(1, 25))
}

test "multiply_negative_numbers" {
  assert_eq(35, multiply(-5, -7))
  assert_eq(-35, multiply(5, -7))
  assert_eq(-35, multiply(-5, 7))
}

test "greet_basic" {
  let result = greet("MoonBit")
  assert_eq_string("Hello, MoonBit!", result)
}

test "greet_empty_string" {
  let result = greet("")
  assert_eq_string("Hello, !", result)
}

test "greet_special_characters" {
  let result = greet("测试123!@#")
  assert_eq_string("Hello, 测试123!@#!", result)
}