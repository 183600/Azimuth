// MoonBit 测试套件
// 为 azimuth 项目添加的标准测试用例

test "integer_arithmetic_operations" {
  // 测试整数算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-2, @azimuth.add(5, -7))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "string_processing_functions" {
  // 测试字符串处理功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let c = 5
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算: ((a + b) * c) - d
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(80, result)
}

test "real_world_application_calculations" {
  // 测试实际应用计算
  // 计算订单总价: 商品1(单价199, 数量2) + 商品2(单价99, 数量3) + 运费15
  let item1_total = @azimuth.multiply(199, 2)
  let item2_total = @azimuth.multiply(99, 3)
  let shipping = 15
  let total = @azimuth.add(@azimuth.add(item1_total, item2_total), shipping)
  
  @azimuth.assert_eq(710, total)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "sequential_operations_test" {
  // 测试连续操作
  // 计算: 1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  let total_sum = @azimuth.add(sum1, 5)
  @azimuth.assert_eq(15, total_sum)
  
  // 计算: 2 * 3 * 4
  let product1 = @azimuth.multiply(2, 3)
  let total_product = @azimuth.multiply(product1, 4)
  @azimuth.assert_eq(24, total_product)
}

test "temperature_conversion_calculation" {
  // 测试温度转换计算
  // 摄氏度转华氏度（简化版）: F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}