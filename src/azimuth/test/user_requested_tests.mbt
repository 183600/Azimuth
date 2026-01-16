// 新增 MoonBit 测试用例
// 测试 azimuth 包的核心功能和边界情况

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let c = -12345
  let d = 67890
  
  assert_eq(add(c, d), add(d, c))
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "greet_multilingual_support" {
  // 测试多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, le Monde!", greet("le Monde"))
  assert_eq_string("Hello, 世界🌍!", greet("世界🌍"))
}

test "calculation_sequence" {
  // 测试计算序列：(a + b) × (c + d) ÷ e
  let a = 10
  let b = 15
  let c = 20
  let d = 25
  let e = 5
  
  let sum1 = add(a, b)  // 25
  let sum2 = add(c, d)  // 45
  let product = multiply(sum1, sum2)  // 1125
  let result = product / e  // 225
  
  assert_eq(225, result)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的各种运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(0, multiply(max_val, 0))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, \"John\"!", greet("\"John\""))
  assert_eq_string("Hello, O'Reilly!", greet("O'Reilly"))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello,  leading space!", greet(" leading space"))
  assert_eq_string("Hello, trailing space !", greet("trailing space "))
}

test "financial_calculation" {
  // 测试金融计算场景
  // 复利计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "temperature_conversion" {
  // 测试温度转换：华氏度 = 摄氏度 × 2 + 30（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试负温度
  let negative_celsius = -10
  let negative_fahrenheit = add(multiply(negative_celsius, 2), 30)
  
  assert_eq(10, negative_fahrenheit)  // -10 × 2 + 30 = 10
}

test "geometric_calculations" {
  // 测试几何计算
  let radius = 7
  
  // 圆周长（简化）：2 × π × r ≈ 2 × 3 × r
  let circumference = multiply(multiply(2, 3), radius)
  assert_eq(42, circumference)
  
  // 圆面积（简化）：π × r² ≈ 3 × r²
  let area = multiply(3, multiply(radius, radius))
  assert_eq(147, area)
}

test "error_handling_verification" {
  // 测试错误处理验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 验证正常计算
  assert_true(add(100, 200) == 300)
  assert_true(multiply(15, 4) == 60)
}