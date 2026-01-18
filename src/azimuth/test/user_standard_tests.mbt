// 用户标准 MoonBit 测试用例
// 测试核心功能的各种使用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 25
  let b = 17
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 15
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(46, perimeter)
}

test "financial_calculation_simple" {
  // 测试简单金融计算：本金和利息
  let principal = 5000
  let rate = 8  // 8% 利率
  let years = 2
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(85000, total)  // 5000 + (5000 × 8 × 2) = 85000
}

test "temperature_conversion_simplified" {
  // 测试简化温度转换：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
}

test "sequential_operations" {
  // 测试连续运算
  let initial = 10
  
  // 连续加法
  let result1 = @azimuth.add(@azimuth.add(initial, 5), 3)
  @azimuth.assert_eq(18, result1)
  
  // 连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(initial, 2), 3)
  @azimuth.assert_eq(60, result2)
  
  // 混合运算
  let result3 = @azimuth.add(@azimuth.multiply(5, 4), @azimuth.multiply(3, 2))
  @azimuth.assert_eq(26, result3)
}

test "error_handling_verification" {
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

test "real_world_application_test" {
  // 测试实际应用场景：购物车计算
  let item_price = 99
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 应用折扣和运费
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(394, final_total)  // (99×4) - 20 + 12 = 394
}