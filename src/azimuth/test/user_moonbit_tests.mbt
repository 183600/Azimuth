// 用户自定义的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
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
  
  // 测试最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 10
  let width = 5
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
}

test "financial_calculation_simple_interest" {
  // 测试金融计算：单利计算
  let principal = 1000  // 本金
  let rate = 5          // 利率百分比
  let time = 3          // 时间（年）
  
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time)
  @azimuth.assert_eq(15000, interest)
  
  // 总金额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(16000, total)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度（简化版）
  // 简化公式：F = C × 2 + 30（避免小数运算）
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "shopping_cart_calculation" {
  // 测试购物车计算
  let item_price = 99
  let quantity = 3
  let shipping = 10
  let discount = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 应用折扣和运费
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  // 验证结果：(99 × 3) - 20 + 10 = 297 - 20 + 10 = 287
  @azimuth.assert_eq(287, total)
}

test "statistical_calculation_average" {
  // 测试统计计算：平均值
  // 数据集：15, 25, 35, 45, 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(35, average)
}

test "time_calculation_duration" {
  // 测试时间计算：持续时间
  let start_hour = 9
  let start_minute = 30
  let end_hour = 14
  let end_minute = 15
  
  // 转换为分钟数
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_minute)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_minute)
  
  // 计算持续时间
  let duration = @azimuth.add(end_total, -start_total)
  
  // 验证结果：(14×60+15) - (9×60+30) = 855 - 570 = 285分钟
  @azimuth.assert_eq(285, duration)
}