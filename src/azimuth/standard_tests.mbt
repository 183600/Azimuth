// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖主要功能

test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
}

test "addition_with_zero" {
  assert_eq(10, add(10, 0))
  assert_eq(10, add(0, 10))
  assert_eq(0, add(0, 0))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-6, multiply(-2, 3))
}

test "multiplication_negatives" {
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-8, multiply(2, -4))
}

test "greet_functionality" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
}

test "mathematical_operations" {
  // 测试复合运算：矩形面积和周长
  let length = 8
  let width = 4
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(32, area)
  assert_eq(24, perimeter)
}

test "unicode_support" {
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, aBc123!", greet("aBc123"))
}