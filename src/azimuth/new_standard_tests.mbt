// 新的标准 MoonBit 测试用例
// 测试 azimuth 项目的主要功能

test "test_add_positive" {
  let result = add(5, 7)
  assert_eq(12, result)
}

test "test_add_zero" {
  assert_eq(8, add(8, 0))
  assert_eq(8, add(0, 8))
}

test "test_add_negative" {
  let result = add(-3, -4)
  assert_eq(-7, result)
}

test "test_add_mixed" {
  assert_eq(2, add(5, -3))
  assert_eq(-2, add(-5, 3))
}

test "test_multiply_positive" {
  let result = multiply(4, 5)
  assert_eq(20, result)
}

test "test_multiply_zero" {
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
}

test "test_multiply_negative" {
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
}

test "test_greet_simple" {
  let result = greet("Test")
  assert_eq_string("Hello, Test!", result)
}

test "test_greet_empty" {
  let result = greet("")
  assert_eq_string("Hello, !", result)
}

test "test_greet_unicode" {
  let result = greet("测试")
  assert_eq_string("Hello, 测试!", result)
}