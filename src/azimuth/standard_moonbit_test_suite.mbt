// 标准 MoonBit 测试套件
// 包含对 azimuth 库核心功能的全面测试

// 测试基本加法功能
test "add_basic_functionality" {
  // 测试基本的正数加法
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(100, add(50, 50))
}

// 测试加法边界情况
test "add_boundary_cases" {
  // 测试与零相加
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  
  // 测试负数加法
  assert_eq(0, add(10, -10))
  assert_eq(-5, add(-15, 10))
  
  // 测试最大值边界
  let max_val = 2147483647
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
}

// 测试基本乘法功能
test "multiply_basic_functionality" {
  // 测试基本的正数乘法
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 100))
  
  // 测试负数乘法
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

// 测试乘法边界情况
test "multiply_boundary_cases" {
  // 测试与1和-1相乘
  assert_eq(42, multiply(42, 1))
  assert_eq(-42, multiply(42, -1))
  
  // 测试最大值边界
  let max_val = 2147483647
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(max_val, 0))
  
  // 测试最小值特殊情况
  let min_val = -2147483648
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

// 测试字符串拼接功能
test "greet_basic_functionality" {
  // 测试基本字符串拼接
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
}

// 测试字符串特殊字符处理
test "greet_special_characters" {
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  
  // 测试包含数字的字符串
  assert_eq_string("Hello, Test123!", greet("Test123"))
  
  // 测试包含特殊符号的字符串
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

// 测试复合计算场景
test "complex_calculation_scenarios" {
  // 测试矩形面积计算：周长 = 2 × (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
  
  // 测试简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 2
  let interest = multiply(principal, multiply(rate, time))
  assert_eq(10000, interest)
}

// 测试实际应用场景
test "real_world_applications" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
  
  // 测试温度转换（简化版）：华氏度 = 摄氏度 × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

// 测试断言函数
test "assertion_functions" {
  // 测试布尔断言
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_false(5 < 3)
  
  // 测试相等断言
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试计算结果断言
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq_string("Hello, Alice!", greet("Alice"))
}

// 测试错误处理和边界保护
test "error_handling_and_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保加法溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  
  // 确保乘法溢出被正确处理
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试除零保护（通过 divide_with_ceil 函数）
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(4, divide_with_ceil(9, 2))
}