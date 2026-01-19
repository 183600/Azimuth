// Azimuth 核心功能增强测试用例
// 使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 全面测试加法功能
  // 正数相加
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
  
  // 负数相加
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-50, 50))
  
  // 混合符号相加
  assert_eq(25, add(100, -75))
  assert_eq(-25, add(-100, 75))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  // 正数相乘
  assert_eq(100, multiply(10, 10))
  assert_eq(0, multiply(100, 0))
  
  // 负数相乘
  assert_eq(100, multiply(-10, -10))
  assert_eq(-100, multiply(-10, 10))
  
  // 大数相乘
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  // 基本用法
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  
  // 特殊字符
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  
  // Unicode 字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "mathematical_expressions" {
  // 测试数学表达式计算
  let a = 12
  let b = 8
  let c = 5
  
  // (a + b) * c
  let sum_ab = add(a, b)
  let result1 = multiply(sum_ab, c)
  assert_eq(100, result1)
  
  // a * b + c
  let product_ab = multiply(a, b)
  let result2 = add(product_ab, c)
  assert_eq(101, result2)
  
  // (a - b) * (b - c)
  let diff_ab = add(a, -b)
  let diff_bc = add(b, -c)
  let result3 = multiply(diff_ab, diff_bc)
  assert_eq(12, result3)
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 面积计算：矩形面积 = 长 × 宽
  let length = 15
  let width = 8
  let area = multiply(length, width)
  assert_eq(120, area)
  
  // 周长计算：矩形周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(46, perimeter)
  
  // 体积计算：长方体体积 = 长 × 宽 × 高
  let height = 6
  let volume = multiply(area, height)
  assert_eq(720, volume)
}

test "financial_calculations" {
  // 测试金融计算
  // 简单利息：本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3   // 3年
  let interest = multiply(principal, multiply(rate, time))
  assert_eq(150000, interest)
  
  // 本息合计：本金 + 利息
  let total = add(principal, interest)
  assert_eq(160000, total)
  
  // 月供计算（简化）：总额 / 期数
  let months = 36
  let monthly_payment = total / months
  assert_eq(4444, monthly_payment)
}

test "boundary_value_stress" {
  // 边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(min_val, min_val))
  
  // 边界值乘法测试
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 长字符串处理
  let long_string = "ThisIsAVeryLongStringThatContainsMultipleWordsAndShouldBeHandledCorrectly"
  let result1 = greet(long_string)
  assert_eq_string("Hello, " + long_string + "!", result1)
  
  // 包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result2 = greet(special_chars)
  assert_eq_string("Hello, " + special_chars + "!", result2)
  
  // 混合语言字符串
  let mixed_lang = "Hello世界123"
  let result3 = greet(mixed_lang)
  assert_eq_string("Hello, " + mixed_lang + "!", result3)
}

test "algorithmic_calculations" {
  // 算法计算测试
  // 计算等差数列前n项和：n × (首项 + 末项) / 2
  let n = 10
  let first_term = 1
  let last_term = 10
  let arithmetic_sum = multiply(n, add(first_term, last_term)) / 2
  assert_eq(55, arithmetic_sum)
  
  // 计算等比数列前n项和（简化）：a × (r^n - 1) / (r - 1)
  let a = 2
  let r = 3
  let n_geo = 4
  // 简化计算：2 + 6 + 18 + 54 = 80
  let geometric_sum = add(add(add(2, multiply(2, 3)), multiply(2, multiply(3, 3))), multiply(2, multiply(3, multiply(3, 3))))
  assert_eq(80, geometric_sum)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, square_sum)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  assert_true(add(5, 3) == 8)
  assert_false(add(5, 3) == 9)
  assert_eq(8, add(5, 3))
  assert_eq_string("Hello, Test!", greet("Test"))
  
  // 测试溢出保护是否正确工作
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试特殊值处理
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
}