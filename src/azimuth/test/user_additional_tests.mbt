// 用户添加的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能，使用标准的 MoonBit 测试语法

test "addition_with_large_numbers" {
  // 测试大数相加
  let result = @azimuth.add(1000000, 2000000)
  @azimuth.assert_eq(3000000, result)
  
  // 测试负大数相加
  let negative_result = @azimuth.add(-1000000, -2000000)
  @azimuth.assert_eq(-3000000, negative_result)
}

test "addition_mixed_positive_negative" {
  // 测试正负数混合相加
  @azimuth.assert_eq(0, @azimuth.add(1000, -1000))
  @azimuth.assert_eq(500, @azimuth.add(1000, -500))
  @azimuth.assert_eq(-500, @azimuth.add(-1000, 500))
}

test "multiplication_power_of_two" {
  // 测试2的幂次乘法
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
}

test "multiplication_with_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(-42, @azimuth.multiply(6, -7))
  @azimuth.assert_eq(42, @azimuth.multiply(-6, -7))
  @azimuth.assert_eq(-42, @azimuth.multiply(-6, 7))
}

test "greeting_function_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法恒等：a + 0 = a
  let a = 42
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法恒等：a × 1 = a
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列：(a + b) × (c + d)
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let result = @azimuth.multiply(sum1, sum2)
  
  @azimuth.assert_eq(900, result)  // (10+20) × (5+15) = 30 × 20 = 600
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试最大值与1相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试最小值与-1相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "real_world_financial_calculation" {
  // 测试实际金融计算：简单利息
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  // 计算简单利息：本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  // 计算总额：本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "assert_function_validation" {
  // 测试断言函数的有效性
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let add_result = @azimuth.add(25, 25)
  @azimuth.assert_eq(50, add_result)
  
  let mul_result = @azimuth.multiply(10, 10)
  @azimuth.assert_eq(100, mul_result)
  
  let greet_result = @azimuth.greet("Test")
  @azimuth.assert_eq_string("Hello, Test!", greet_result)
}