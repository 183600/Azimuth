// 用户添加的 MoonBit 测试用例
// 覆盖 azimuth 项目的核心功能，使用标准 MoonBit 测试语法

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  let result1 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result1)
  
  // 测试负数溢出
  let result2 = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result2)
  
  // 测试最大值与最小值相加
  let result3 = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, result3)
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最小值与-1相乘（特殊情况）
  let result1 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result1)
  
  // 测试溢出情况
  let result2 = @azimuth.multiply(46341, 46341)
  @azimuth.assert_eq(max_val, result2)
  
  // 测试与0相乘
  let result3 = @azimuth.multiply(max_val, 0)
  @azimuth.assert_eq(0, result3)
}

test "greet_unicode_handling" {
  // 测试greet函数的Unicode处理
  let chinese_name = "张三"
  let result1 = @azimuth.greet(chinese_name)
  @azimuth.assert_eq_string("Hello, 张三!", result1)
  
  let emoji_name = "🌍"
  let result2 = @azimuth.greet(emoji_name)
  @azimuth.assert_eq_string("Hello, 🌍!", result2)
  
  let mixed_name = "User测试123"
  let result3 = @azimuth.greet(mixed_name)
  @azimuth.assert_eq_string("Hello, User测试123!", result3)
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  let a = 12
  let b = 8
  let c = 5
  
  // 计算 (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(100, result1)
  
  // 计算 a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(101, result2)
  
  // 计算 (a - b) * (b - c)
  let result3 = @azimuth.multiply(@azimuth.add(a, -b), @azimuth.add(b, -c))
  @azimuth.assert_eq(12, result3)
}

test "financial_calculation" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算简单利息：本金 + 本金 * 利率 * 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  // 验证结果：10000 + 10000*5*3 = 10000 + 150000 = 160000
  @azimuth.assert_eq(160000, total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let item3_price = 99
  let item3_qty = 3
  let shipping = 20
  
  // 计算各项总价
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let total3 = @azimuth.multiply(item3_price, item3_qty)
  
  // 计算总价
  let subtotal = @azimuth.add(@azimuth.add(total1, total2), total3)
  let final_total = @azimuth.add(subtotal, shipping)
  
  // 验证结果：199*2 + 299*1 + 99*3 + 20 = 398 + 299 + 297 + 20 = 1014
  @azimuth.assert_eq(1014, final_total)
}

test "geometric_calculations" {
  // 测试几何计算
  let length = 10
  let width = 6
  
  // 计算矩形周长：2 * (length + width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(32, perimeter)
  
  // 计算矩形面积：length * width
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
  
  // 计算三角形面积：底 * 高 / 2
  let base = 8
  let height = 5
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(20, triangle_area)
}

test "statistical_calculations" {
  // 测试统计计算
  let numbers = [@azimuth.add(10, 20), @azimuth.add(30, 40), @azimuth.add(50, 60)]
  // numbers = [30, 70, 110]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2])
  @azimuth.assert_eq(210, sum)
  
  // 计算平均值
  let average = sum / 3
  @azimuth.assert_eq(70, average)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(numbers[0], -average)
  let diff2 = @azimuth.add(numbers[1], -average)
  let diff3 = @azimuth.add(numbers[2], -average)
  let variance = @azimuth.add(@azimuth.add(@azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), @azimuth.multiply(diff3, diff3)) / 3
  @azimuth.assert_eq(1600, variance)
}

test "assert_function_tests" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let add_result = @azimuth.add(15, 25)
  @azimuth.assert_eq(40, add_result)
  
  let multiply_result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, multiply_result)
  
  let greet_result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", greet_result)
}