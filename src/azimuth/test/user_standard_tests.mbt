// 用户指定的标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，包含不超过10个测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
}

test "string_operations" {
  // 测试字符串操作
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
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

test "compound_calculations" {
  // 测试复合计算
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(38, result)
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 圆形面积计算（简化）：π × r² ≈ 3 × r²
  let radius = 8
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(192, area)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 5000
  let rate = 8  // 8%
  let years = 2
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(85000, total)  // 5000 × (1 + 8×2) = 5000 × 17 = 85000
}

test "assertion_functions" {
  // 测试断言函数
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  @azimuth.assert_false(2 > 5)
  @azimuth.assert_false(@azimuth.add(2, 2) == 5)
}

test "edge_case_handling" {
  // 测试边缘情况处理
  // 连续运算
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 混合运算
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result3)
}

test "string_edge_cases" {
  // 测试字符串边缘情况
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  
  // 长字符串
  let long_name = "VeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, VeryLongNameForTestingPurposes!", @azimuth.greet(long_name))
}

test "mathematical_series" {
  // 测试数学数列
  // 等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let sum_1_to_10 = @azimuth.multiply(10, @azimuth.add(1, 10)) / 2
  @azimuth.assert_eq(55, sum_1_to_10)
  
  // 平方和：1² + 2² + 3² + 4² = 30
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), 
                @azimuth.add(@azimuth.multiply(3, 3), @azimuth.multiply(4, 4)))
  @azimuth.assert_eq(30, square_sum)
}