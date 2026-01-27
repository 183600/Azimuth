// 标准 MoonBit 测试用例 - 为 azimuth 项目创建
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(15, add(7, 8))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "multiply_function_zero_cases" {
  // 测试乘法零值情况
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(456, 0))
  assert_eq(0, multiply(0, 0))
}

test "greet_function_simple_cases" {
  // 测试问候函数简单情况
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_division" {
  // 测试基本除法向上取整
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(2, 2))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "large_number_calculations" {
  // 测试大数计算
  assert_eq(2000, add(1000, 1000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100, divide_with_ceil(10000, 100))
}

test "mixed_sign_operations" {
  // 测试混合符号运算
  assert_eq(1, add(-5, 6))
  assert_eq(-10, multiply(2, -5))
  assert_eq(-1, divide_with_ceil(5, -10))
}

test "edge_case_divisions" {
  // 测试除法边缘情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "unicode_string_greetings" {
  // 测试Unicode字符串问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let step1 = add(10, 5)        // 15
  let step2 = multiply(step1, 2) // 30
  let step3 = divide_with_ceil(step2, 8) // 4
  let step4 = add(step3, 1)      // 5
  assert_eq(5, step4)
}