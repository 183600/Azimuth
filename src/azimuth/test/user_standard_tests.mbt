// 用户标准 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -10
  let d = 20
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(28, perimeter)
}

test "financial_calculation_simple_interest" {
  // 测试金融计算：简单利息
  let principal = 5000  // 本金
  let rate = 5  // 利率百分比
  let years = 3  // 年数
  
  // 简单利息计算：利息 = 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  @azimuth.assert_eq(75000, interest)  // 5000 × 5 × 3 = 75000
  
  // 计算总额
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(80000, total)  // 5000 + 75000 = 80000
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景
  // 简化公式：F = C × 2 + 30（避免小数）
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result)
  
  let greeting = @azimuth.greet("Test")
  @azimuth.assert_eq_string("Hello, Test!", greeting)
}

test "sequential_operations_test" {
  // 测试连续运算
  // 计算 (a + b) * (c + d)
  let a = 5
  let b = 3
  let c = 4
  let d = 2
  
  let sum1 = @azimuth.add(a, b)  // 8
  let sum2 = @azimuth.add(c, d)  // 6
  let result = @azimuth.multiply(sum1, sum2)  // 48
  
  @azimuth.assert_eq(48, result)
  
  // 计算 a * b + c * d
  let product1 = @azimuth.multiply(a, b)  // 15
  let product2 = @azimuth.multiply(c, d)  // 8
  let result2 = @azimuth.add(product1, product2)  // 23
  
  @azimuth.assert_eq(23, result2)
}

test "error_handling_and_overflow_protection" {
  // 测试错误处理和溢出保护
  let max_val = 2147483647
  
  // 测试加法溢出保护
  let overflow_result = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, overflow_result)
  
  // 测试乘法溢出保护
  let multiply_overflow = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, multiply_overflow)
  
  // 测试最小值特殊处理
  let min_val = -2147483648
  let min_times_neg_one = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, min_times_neg_one)
}