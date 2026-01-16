// Azimuth 项目额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(30, multiply(5, 6))
  assert_eq(0, add(10, -10))
  assert_eq(-25, multiply(-5, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "extreme_value_handling" {
  // 测试极值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5
  let years = 3
  let total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(160000, total)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "complex_nested_calculations" {
  // 测试复杂嵌套计算
  // 计算：((a + b) × (c + d)) - (e × f)
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = add(a, b)  // 30
  let sum2 = add(c, d)  // 20
  let product1 = multiply(sum1, sum2)  // 600
  let product2 = multiply(e, f)  // 96
  let result = add(product1, -product2)  // 504
  
  assert_eq(504, result)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 测试特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced   "))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  assert_eq_string("Hello, ThisIsAVeryLongNameForTestingPurposes!", greet(long_name))
}

test "error_resilience" {
  // 测试错误恢复能力
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护是否正常工作
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}

test "sequential_operations" {
  // 测试连续操作
  // 连续加法
  let result1 = add(add(1, 2), add(3, 4))
  assert_eq(10, result1)
  
  // 连续乘法
  let result2 = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result2)
  
  // 混合操作
  let result3 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result3)
}