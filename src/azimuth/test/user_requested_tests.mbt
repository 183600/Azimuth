// 用户请求的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法
// 共添加10个测试用例

test "addition_with_positive_numbers" {
  // 测试正数加法
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "addition_with_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "addition_with_mixed_numbers" {
  // 测试正负数混合加法
  @azimuth.assert_eq(-4, @azimuth.add(-10, 6))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-1, @azimuth.add(10, -11))
}

test "multiplication_basic_cases" {
  // 测试基本乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "multiplication_with_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
}

test "greet_function_standard_cases" {
  // 测试greet函数标准情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "greet_function_special_characters" {
  // 测试greet函数特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, line1\nline2!", @azimuth.greet("line1\nline2"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "combined_operations" {
  // 测试组合运算
  // 计算表达式：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 圆面积计算（简化）：π × r² ≈ 3 × r²
  let radius = 5
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
  
  // 温度转换：摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5
  let years = 3
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(160000, total)
}