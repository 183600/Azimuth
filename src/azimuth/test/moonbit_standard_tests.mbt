// MoonBit 标准测试用例
// 测试 azimuth 模块的核心功能

// 导入被测试的模块
use azimuth

// 测试用例1: 基本算术运算
test "basic_arithmetic_operations" {
  // 测试加法基本功能
  assert_eq(10, azimuth.add(3, 7))
  assert_eq(0, azimuth.add(-5, 5))
  assert_eq(-10, azimuth.add(-3, -7))
  
  // 测试乘法基本功能
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-15, azimuth.multiply(-3, 5))
  assert_eq(15, azimuth.multiply(-3, -5))
}

// 测试用例2: 边界值测试
test "boundary_value_tests" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(-1, azimuth.add(max_val, min_val))
  
  // 测试边界值乘法
  assert_eq(0, azimuth.multiply(max_val, 0))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
}

// 测试用例3: 字符串处理测试
test "string_processing_tests" {
  // 测试基本字符串拼接
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  
  // 测试特殊字符处理
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, user@123!", azimuth.greet("user@123"))
}

// 测试用例4: 数学性质验证
test "mathematical_properties" {
  let a = 12
  let b = 8
  
  // 验证加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 验证乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 验证分配律
  assert_eq(
    azimuth.multiply(a, azimuth.add(b, 5)),
    azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, 5))
  )
}

// 测试用例5: 实际应用场景 - 温度转换
test "temperature_conversion" {
  // 摄氏度转华氏度（简化公式：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = azimuth.add(azimuth.multiply(0, 2), 30)
  assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = azimuth.add(azimuth.multiply(100, 2), 30)
  assert_eq(230, boiling)
}

// 测试用例6: 实际应用场景 - 简单金融计算
test "simple_financial_calculation" {
  // 计算简单利息：本金 + (本金 × 利率 × 年数)
  let principal = 1000
  let rate = 5
  let years = 3
  
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, years))
  let total = azimuth.add(principal, interest)
  assert_eq(16000, total)
}

// 测试用例7: 复合运算测试
test "compound_operations" {
  // 计算 ((a + b) × c) - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = azimuth.add(a, b)
  let product = azimuth.multiply(sum, c)
  let result = azimuth.add(product, -d)
  
  assert_eq(38, result)
}

// 测试用例8: 错误处理和边界条件
test "error_handling_and_edge_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_true(azimuth.add(max_val, 100) == max_val)
  assert_true(azimuth.add(min_val, -100) == min_val)
  assert_true(azimuth.multiply(max_val, 2) == max_val)
  
  // 测试零值特性
  assert_eq(0, azimuth.multiply(0, max_val))
  assert_eq(max_val, azimuth.add(0, max_val))
  assert_eq(min_val, azimuth.add(0, min_val))
}

// 测试用例9: 数值范围验证
test "numeric_range_validation" {
  // 测试正数运算
  assert_true(azimuth.add(100, 200) > 0)
  assert_true(azimuth.multiply(10, 20) > 0)
  
  // 测试负数运算
  assert_true(azimuth.add(-100, -200) < 0)
  assert_true(azimuth.multiply(-10, 20) < 0)
  
  // 测试混合符号运算
  assert_eq(0, azimuth.add(100, -100))
  assert_eq(-200, azimuth.multiply(-10, 20))
}

// 测试用例10: 性能相关测试
test "performance_related_tests" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  assert_eq(3000000, azimuth.add(large_num1, large_num2))
  assert_eq(2000000000000, azimuth.multiply(large_num1, large_num2))
  
  // 测试连续运算
  let result = azimuth.add(
    azimuth.multiply(100, 100),
    azimuth.multiply(200, 200)
  )
  assert_eq(50000, result)
}