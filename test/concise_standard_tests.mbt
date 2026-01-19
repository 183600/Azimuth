// 简洁标准 MoonBit 测试用例
// 测试核心功能：add, multiply, greet 函数

test "add_positive_numbers" {
  assert_eq(8, add(3, 5))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_negative_numbers" {
  assert_eq(-8, add(-3, -5))
  assert_eq(-2, add(5, -7))
  assert_eq(0, add(-10, 10))
}

test "multiply_basic" {
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
}

test "greet_simple_cases" {
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "add_overflow_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_overflow_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "mathematical_commutativity" {
  let a = 12
  let b = 17
  
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation" {
  // 计算 (a + b) * c
  let a = 4
  let b = 6
  let c = 3
  
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(30, result)
}

test "string_special_characters" {
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "assert_function_utilities" {
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}