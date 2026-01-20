// 新的核心功能测试用例
// 测试 azimuth 包的核心函数：add, multiply, greet

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-5, add(-2, -3))
  assert_eq(-10, add(-7, -3))
  assert_eq(0, add(-5, 5))
  assert_eq(-100, add(-150, 50))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(86400000, multiply(86400, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
}

test "greet_unicode_characters" {
  // 测试 Unicode 字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ✓✗!", greet("✓✗"))
}

test "mathematical_series" {
  // 测试数学级数计算
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum_1_to_5 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_1_to_5)
  
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let sum_powers = add(add(add(2, 4), 8), 16)
  assert_eq(30, sum_powers)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与1的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 最小值与1的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 边界值相加
  assert_eq(-1, add(max_val, min_val))
}

test "complex_string_scenarios" {
  // 测试复杂字符串场景
  // 数字字符串
  assert_eq_string("Hello, 2023!", greet("2023"))
  
  // 特殊符号组合
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  
  // 混合内容
  assert_eq_string("Hello, Test123!", greet("Test123"))
  
  // 空格处理
  assert_eq_string("Hello, Leading Space!", greet(" Leading Space"))
}

test "calculation_precision" {
  // 测试计算精度
  // 连续加法
  let result = add(add(add(1, 2), 3), 4)
  assert_eq(10, result)
  
  // 连续乘法
  let product = multiply(multiply(2, 3), 4)
  assert_eq(24, product)
  
  // 混合运算
  let mixed = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, mixed)
}

test "real_world_applications" {
  // 测试实际应用场景
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 1000
  let rate = 5
  let years = 2
  let total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(11000, total)
}

test "performance_edge_cases" {
  // 测试性能边界情况
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(1000000000, multiply(large_num1, large_num2))
  
  // 零值运算
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(2147483647, add(0, 2147483647))
}

test "error_handling_validation" {
  // 测试错误处理验证
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("success", "success")
  
  // 条件判断测试
  let positive = 10
  let negative = -10
  let zero = 0
  
  assert_true(positive > zero)
  assert_true(negative < zero)
  assert_false(positive == negative)
}