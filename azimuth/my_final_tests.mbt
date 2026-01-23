// 我的最终测试文件 - 包含10个标准 MoonBit 测试用例

test "add_basic" {
  assert_eq(5, add(2, 3))
}

test "multiply_basic" {
  assert_eq(6, multiply(2, 3))
}

test "greet_basic" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "add_negative" {
  assert_eq(-5, add(-2, -3))
}

test "multiply_negative" {
  assert_eq(-6, multiply(-2, 3))
}

test "add_zero" {
  assert_eq(5, add(5, 0))
}

test "multiply_zero" {
  assert_eq(0, multiply(5, 0))
}

test "complex_calculation" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
}

test "unicode_greeting" {
  assert_eq_string("Hello, 世界!", greet("世界"))
}