// 额外的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(5, -6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 15
  let c = 3
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)  // 25
  let product = @azimuth.multiply(sum_ab, c)  // 75
  let result = @azimuth.add(product, -d)  // 55
  
  @azimuth.assert_eq(55, result)
}

test "real_world_shopping_cart" {
  // 测试实际应用：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let discount = 50
  
  let subtotal = @azimuth.multiply(item_price, quantity)  // 597
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)  // 562
  
  @azimuth.assert_eq(562, total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)  // 80
  
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)  // 30
  @azimuth.assert_eq(30, freezing_f)
}

test "sequential_operations" {
  // 测试连续运算
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(1, 2)  // 3
  let sum2 = @azimuth.add(sum1, 3)  // 6
  let sum3 = @azimuth.add(sum2, 4)  // 10
  let final_sum = @azimuth.add(sum3, 5)  // 15
  
  @azimuth.assert_eq(15, final_sum)
  
  // 计算 2 * 3 * 4
  let product1 = @azimuth.multiply(2, 3)  // 6
  let final_product = @azimuth.multiply(product1, 4)  // 24
  
  @azimuth.assert_eq(24, final_product)
}

test "error_handling_validation" {
  // 测试错误处理验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列前5项的和：2 + 4 + 6 + 8 + 10
  let term1 = 2
  let term2 = @azimuth.add(term1, 2)  // 4
  let term3 = @azimuth.add(term2, 2)  // 6
  let term4 = @azimuth.add(term3, 2)  // 8
  let term5 = @azimuth.add(term4, 2)  // 10
  
  let series_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(30, series_sum)
}