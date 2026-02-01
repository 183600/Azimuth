// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含基本数学运算和字符串处理的测试

test "basic_addition_function" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
  assert_eq(10, add(7, 3))
}

test "basic_multiplication_function" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(15, multiply(3, 5))
}

test "basic_subtraction_function" {
  // 测试基本减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-3, subtract(2, 5))
  assert_eq(0, subtract(7, 7))
  assert_eq(-5, subtract(-3, 2))
}

test "basic_greet_function" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-1, subtract(2, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "zero_handling_operations" {
  // 测试零的处理
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = subtract(multiply(10, 5), add(3, 2))
  assert_eq(45, result2)  // 10*5 - (3 + 2) = 50 - 5 = 45
}

test "edge_case_divisions" {
  // 测试边界情况的除法
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "unicode_string_greeting" {
  // 测试 Unicode 字符串问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
}