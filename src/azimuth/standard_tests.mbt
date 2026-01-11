// 标准测试用例
// 测试 add 函数
test "add_basic_positive" {
  assert_eq(5, add(2, 3))
  assert_eq(30, add(10, 20))
}

test "add_with_zero" {
  assert_eq(5, add(0, 5))
  assert_eq(7, add(7, 0))
  assert_eq(0, add(0, 0))
}

test "add_negative_numbers" {
  assert_eq(-5, add(-2, -3))
  assert_eq(-5, add(-10, 5))
  assert_eq(10, add(15, -5))
}

test "add_overflow_boundary" {
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

// 测试 multiply 函数
test "multiply_basic_positive" {
  assert_eq(6, multiply(2, 3))
  assert_eq(50, multiply(5, 10))
}

test "multiply_with_zero_and_one" {
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(7, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(7, multiply(7, 1))
}

test "multiply_negative_numbers" {
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_overflow_boundary" {
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

// 测试 greet 函数
test "greet_basic" {
  assert_eq("Hello, World!", greet("World"))
  assert_eq("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_empty_string" {
  assert_eq("Hello, !", greet(""))
  assert_eq("Hello,  !", greet(" "))
}