// 用户增强测试用例
// 测试 Azimuth 项目的核心功能和实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
}

test "financial_calculation_simple_interest" {
  // 测试金融计算：单利
  let principal = 10000  // 本金
  let rate = 5          // 利率百分比
  let time = 3          // 时间（年）
  
  // 单利公式：利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time)
  @azimuth.assert_eq(150000, interest)  // 10000 × 5 × 3 = 150000
  
  // 总金额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 150000 = 160000
}

test "geometry_rectangle_calculations" {
  // 测试几何计算：矩形
  let length = 10
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(32, perimeter)
}

test "temperature_conversion_simplified" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let base = 5
  let multiplier = 3
  let offset = 10
  
  // 计算：(base × multiplier) + offset
  let step1 = @azimuth.multiply(base, multiplier)  // 15
  let result = @azimuth.add(step1, offset)         // 25
  
  @azimuth.assert_eq(25, result)
  
  // 验证计算链的不同组合方式
  let alternative = @azimuth.add(@azimuth.multiply(base, multiplier), offset)
  @azimuth.assert_eq(result, alternative)
}

test "error_handling_overflow_protection" {
  // 测试错误处理：溢出保护
  let max_val = 2147483647
  
  // 测试加法溢出保护
  let overflow_add = @azimuth.add(max_val, 1000)
  @azimuth.assert_eq(max_val, overflow_add)
  
  // 测试乘法溢出保护
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  // 测试最小值特殊情况
  let min_val = -2147483648
  let min_val_mul = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, min_val_mul)
}

test "real_world_shopping_cart" {
  // 测试实际应用：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)  // 299
  let subtotal = @azimuth.add(item1_total, item2_total)            // 697
  
  // 计算最终总价
  let total = @azimuth.add(subtotal, shipping)                     // 717
  
  @azimuth.assert_eq(717, total)
}