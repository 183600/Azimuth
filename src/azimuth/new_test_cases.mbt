// 新增的10个MoonBit测试用例
// 使用标准的MoonBit测试语法

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-5, azimuth::add(-2, -3))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-10, azimuth::add(-3, -7))
}

test "multiply_by_negative_numbers" {
  // 测试负数乘法
  assert_eq(-6, azimuth::multiply(2, -3))
  assert_eq(6, azimuth::multiply(-2, -3))
  assert_eq(0, azimuth::multiply(-2, 0))
}

test "greet_with_numbers" {
  // 测试包含数字的字符串
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, Year2023!", azimuth::greet("Year2023"))
  assert_eq_string("Hello, Test_001!", azimuth::greet("Test_001"))
}

test "add_large_positive_numbers" {
  // 测试大正数加法
  assert_eq(2000000000, azimuth::add(1000000000, 1000000000))
  assert_eq(1500000000, azimuth::add(750000000, 750000000))
  assert_eq(2147483647, azimuth::add(2147483646, 1))
}

test "multiply_decimal_simulation" {
  // 模拟小数运算（使用整数表示）
  // 计算1.5 × 2.4 = 3.6，表示为15 × 24 = 360，然后除以100
  let result = azimuth::multiply(15, 24) / 100
  assert_eq(3, result)  // 整数除法，结果为3
  
  // 计算2.5 × 4 = 10.0，表示为25 × 40 = 1000，然后除以100
  let result2 = azimuth::multiply(25, 40) / 100
  assert_eq(10, result2)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式计算：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = azimuth::add(a, b)
  let product = azimuth::multiply(sum_ab, c)
  let result = azimuth::add(product, -d)
  
  assert_eq(22, result)  // (5 + 3) * 4 - 10 = 22
}

test "greet_with_whitespace_handling" {
  // 测试空白字符处理
  assert_eq_string("Hello,  !", azimuth::greet(" "))
  assert_eq_string("Hello, 	!", azimuth::greet("\t"))
  assert_eq_string("Hello, 
!", azimuth::greet("\n"))
}

test "add_sequence_validation" {
  // 测试加法序列验证
  // 验证等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = azimuth::add(azimuth::add(1, 2), azimuth::add(3, 4))
  let total = azimuth::add(sum1, 5)
  assert_eq(15, total)
  
  // 验证偶数求和：2 + 4 + 6 + 8 = 20
  let even_sum = azimuth::add(azimuth::add(2, 4), azimuth::add(6, 8))
  assert_eq(20, even_sum)
}

test "multiply_power_calculation" {
  // 测试幂运算计算
  // 计算3的4次方：3^4 = 81
  let power_3_4 = azimuth::multiply(azimuth::multiply(3, 3), azimuth::multiply(3, 3))
  assert_eq(81, power_3_4)
  
  // 计算4的3次方：4^3 = 64
  let power_4_3 = azimuth::multiply(azimuth::multiply(4, 4), 4)
  assert_eq(64, power_4_3)
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  // 测试包含各种特殊字符的字符串
  assert_eq_string("Hello, @#$%!", azimuth::greet("@#$%"))
  assert_eq_string("Hello, []{}!", azimuth::greet("[]{}"))
  assert_eq_string("Hello, <>?!", azimuth::greet("<>?"))
  assert_eq_string("Hello, ||&&!", azimuth::greet("||&&"))
  assert_eq_string("Hello, **!!", azimuth::greet("**"))
}