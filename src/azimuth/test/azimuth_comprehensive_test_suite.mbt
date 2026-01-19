// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(0, azimuth.add(-3, 3))
  assert_eq(-10, azimuth.add(-7, -3))
  
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(-12, azimuth.multiply(-3, 4))
  assert_eq(15, azimuth.multiply(-3, -5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 加法单位元
  assert_eq(a, azimuth.add(a, 0))
  assert_eq(b, azimuth.add(0, b))
  
  // 乘法单位元
  assert_eq(a, azimuth.multiply(a, 1))
  assert_eq(b, azimuth.multiply(1, b))
}

test "boundary_value_analysis" {
  // 测试边界值分析
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试最大值边界
  assert_eq(max_int, azimuth.add(max_int, 0))
  assert_eq(max_int, azimuth.multiply(max_int, 1))
  
  // 测试最小值边界
  assert_eq(min_int, azimuth.add(min_int, 0))
  assert_eq(min_int, azimuth.multiply(min_int, 1))
  
  // 测试零值边界
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(0, azimuth.multiply(0, 0))
  assert_eq(0, azimuth.multiply(5, 0))
}

test "business_calculation_scenarios" {
  // 测试商业计算场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let subtotal = azimuth.multiply(item_price, quantity)
  let total = azimuth.add(subtotal, shipping)
  assert_eq(617, total)
  
  // 折扣计算
  let original_price = 1000
  let discount_percentage = 15
  let discount_amount = azimuth.multiply(original_price, discount_percentage) / 100
  let discounted_price = azimuth.add(original_price, -discount_amount)
  assert_eq(850, discounted_price)
}

test "geometry_calculations" {
  // 测试几何计算
  // 矩形计算
  let length = 10
  let width = 5
  let area = azimuth.multiply(length, width)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  assert_eq(50, area)
  assert_eq(30, perimeter)
  
  // 三角形面积（简化）
  let base = 8
  let height = 6
  let triangle_area = azimuth.multiply(base, height) / 2
  assert_eq(24, triangle_area)
}

test "series_and_progressions" {
  // 测试数列和级数
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let n = 5
  let sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  assert_eq(15, sum)
  
  // 等比数列求和：2 + 4 + 8 = 14
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let geo_sum = azimuth.add(azimuth.add(term1, term2), term3)
  assert_eq(14, geo_sum)
  
  // 平方和：1² + 2² + 3² = 14
  let square1 = azimuth.multiply(1, 1)
  let square2 = azimuth.multiply(2, 2)
  let square3 = azimuth.multiply(3, 3)
  let square_sum = azimuth.add(azimuth.add(square1, square2), square3)
  assert_eq(14, square_sum)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) ÷ 2（简化公式）
  let fahrenheit_back = 80
  let celsius_back = azimuth.add(fahrenheit_back, -30) / 2
  assert_eq(25, celsius_back)
  
  // 冰点测试
  assert_eq(32, azimuth.add(azimuth.multiply(0, 2), 30))  // 0°C = 32°F
  assert_eq(0, azimuth.add(azimuth.multiply(32, -30), 30) / 2)  // 32°F = 0°C
}

test "time_calculations" {
  // 测试时间计算
  // 分钟转秒
  let minutes = 5
  let seconds = azimuth.multiply(minutes, 60)
  assert_eq(300, seconds)
  
  // 小时转分钟
  let hours = 3
  let mins = azimuth.multiply(hours, 60)
  assert_eq(180, mins)
  
  // 天转小时
  let days = 2
  let hrs = azimuth.multiply(days, 24)
  assert_eq(48, hrs)
  
  // 计算总秒数：2天3小时5分钟
  let total_seconds = azimuth.add(
    azimuth.add(azimuth.multiply(2, 24, 60), azimuth.multiply(3, 60)),
    azimuth.multiply(5, 60)
  )
  assert_eq(183000, total_seconds)
}

test "complex_expressions" {
  // 测试复杂表达式
  // (a + b) × (c + d)
  let a = 2
  let b = 3
  let c = 4
  let d = 5
  let result1 = azimuth.multiply(azimuth.add(a, b), azimuth.add(c, d))
  assert_eq(45, result1)  // (2+3)×(4+5) = 5×9 = 45
  
  // a×b + c×d
  let result2 = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(c, d))
  assert_eq(26, result2)  // 2×3 + 4×5 = 6 + 20 = 26
  
  // (a×b)×(c×d)
  let result3 = azimuth.multiply(azimuth.multiply(a, b), azimuth.multiply(c, d))
  assert_eq(120, result3)  // (2×3)×(4×5) = 6×20 = 120
  
  // 嵌套表达式：((a + b) × c) - d
  let result4 = azimuth.add(azimuth.multiply(azimuth.add(a, b), c), -d)
  assert_eq(15, result4)  // ((2+3)×4) - 5 = 20 - 5 = 15
}