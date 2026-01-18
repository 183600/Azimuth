// 用户标准测试用例 - 针对 Azimuth 项目的核心功能
// 使用标准的 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "add_function_overflow_protection" {
  // 测试加法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_overflow_protection" {
  // 测试乘法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 负数溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 最小值特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "real_world_shopping_cart" {
  // 实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(total1, total2)
  
  // 验证总价：199*2 + 299*1 = 398 + 299 = 697
  @azimuth.assert_eq(697, cart_total)
}

test "temperature_conversion_simplified" {
  // 实际应用场景：简化的温度转换
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)  // 25 × 2 + 30 = 80
  
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)  // 0 × 2 + 30 = 30
}

test "compound_interest_calculation" {
  // 实际应用场景：简化复利计算
  // 本金 × (1 + 利率 × 年数)
  
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  // 验证总额：10000 + 10000×5×3 = 10000 + 150000 = 160000
  @azimuth.assert_eq(160000, total)
}

test "geometry_area_calculations" {
  // 实际应用场景：几何面积计算
  
  // 矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "assert_functions_verification" {
  // 验证断言函数的正常工作
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 验证计算结果的断言
  let result = @azimuth.add(17, 25)
  @azimuth.assert_eq(42, result)
  
  let greeting = @azimuth.greet("Test")
  @azimuth.assert_eq_string("Hello, Test!", greeting)
}