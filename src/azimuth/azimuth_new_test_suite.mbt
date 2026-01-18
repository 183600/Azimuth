// Azimuth 项目新的测试套件
// 使用标准 MoonBit 测试语法，提供全面的测试覆盖

// 导入必要的模块
use azimuth

test "addition_with_various_ranges" {
  // 测试不同范围的加法运算
  // 小正数
  @assertion.assert_eq(7, azimuth.add(3, 4))?
  
  // 大正数
  @assertion.assert_eq(2000000, azimuth.add(1000000, 1000000))?
  
  // 小负数
  @assertion.assert_eq(-7, azimuth.add(-3, -4))?
  
  // 大负数
  @assertion.assert_eq(-2000000, azimuth.add(-1000000, -1000000))?
  
  // 混合符号
  @assertion.assert_eq(-1, azimuth.add(1000, -1001))?
}

test "multiplication_precision_tests" {
  // 测试乘法精度和边界情况
  // 正数乘法
  @assertion.assert_eq(15625, azimuth.multiply(125, 125))?
  
  // 负数乘法
  @assertion.assert_eq(15625, azimuth.multiply(-125, -125))?
  
  // 混合符号乘法
  @assertion.assert_eq(-15625, azimuth.multiply(125, -125))?
  
  // 与0相乘
  @assertion.assert_eq(0, azimuth.multiply(12345, 0))?
  
  // 与1相乘
  @assertion.assert_eq(12345, azimuth.multiply(12345, 1))?
}

test "greet_function_with_special_cases" {
  // 测试greet函数的特殊情况
  // 空字符串
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  
  // 单字符
  @assertion.assert_eq_string("Hello, A!", azimuth.greet("A"))?
  
  // 包含空格的字符串
  @assertion.assert_eq_string("Hello, John Doe!", azimuth.greet("John Doe"))?
  
  // 包含特殊字符
  @assertion.assert_eq_string("Hello, test@example.com!", azimuth.greet("test@example.com"))?
  
  // Unicode字符
  @assertion.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))?
}

test "complex_financial_calculations" {
  // 测试复杂金融计算场景
  let principal = 10000
  let rate1 = 5
  let rate2 = 3
  let years = 2
  
  // 计算复利：第一年
  let year1_interest = azimuth.multiply(principal, rate1) / 100
  let year1_total = azimuth.add(principal, year1_interest)
  
  // 计算复利：第二年（使用不同利率）
  let year2_interest = azimuth.multiply(year1_total, rate2) / 100
  let year2_total = azimuth.add(year1_total, year2_interest)
  
  // 验证最终金额
  @assertion.assert_eq(10815, year2_total)?
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  let length = 12
  let width = 8
  let radius = 5
  
  // 矩形面积
  let rectangle_area = azimuth.multiply(length, width)
  @assertion.assert_eq(96, rectangle_area)?
  
  // 正方形面积（边长为半径的2倍）
  let square_side = azimuth.multiply(radius, 2)
  let square_area = azimuth.multiply(square_side, square_side)
  @assertion.assert_eq(100, square_area)?
  
  // 三角形面积（底×高÷2）
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = azimuth.multiply(triangle_base, triangle_height) / 2
  @assertion.assert_eq(30, triangle_area)?
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 等差数列求和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum_first_n = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  @assertion.assert_eq(55, sum_first_n)?
  
  // 等比数列求和：2 + 4 + 8 + 16
  let geo_sum = azimuth.add(azimuth.add(azimuth.add(2, 4), 8), 16)
  @assertion.assert_eq(30, geo_sum)?
  
  // 平方和：1² + 2² + 3² + 4²
  let square_sum = azimuth.add(azimuth.add(azimuth.add(1, 4), 9), 16)
  @assertion.assert_eq(30, square_sum)?
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度：F = C × 9/5 + 32（简化为整数运算）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 9) / 5, 32)
  @assertion.assert_eq(77, fahrenheit)?
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  let fahrenheit_back = 77
  let celsius_back = azimuth.multiply(azimuth.add(fahrenheit_back, -32), 5) / 9
  @assertion.assert_eq(25, celsius_back)?
  
  // 冰点和沸点测试
  @assertion.assert_eq(32, azimuth.add(azimuth.multiply(0, 9) / 5, 32))?  // 0°C = 32°F
  @assertion.assert_eq(212, azimuth.add(azimuth.multiply(100, 9) / 5, 32))?  // 100°C = 212°F
}

test "shopping_cart_with_discounts" {
  // 测试带折扣的购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount_threshold = 500
  let discount_rate = 10
  let shipping = 20
  
  // 计算商品总价
  let item1_total = azimuth.multiply(item1_price, item1_quantity)
  let item2_total = azimuth.multiply(item2_price, item2_quantity)
  let subtotal = azimuth.add(item1_total, item2_total)
  
  // 计算折扣
  let discount = if (subtotal > discount_threshold) {
    azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let discounted_total = azimuth.add(subtotal, -discount)
  let final_total = azimuth.add(discounted_total, shipping)
  
  @assertion.assert_eq(888, final_total)?
}

test "time_and_speed_calculations" {
  // 测试时间和速度计算
  let distance = 300  // 公里
  let speed = 60      // 公里/小时
  
  // 计算时间（小时）
  let time_hours = distance / speed
  @assertion.assert_eq(5, time_hours)?
  
  // 转换为分钟
  let time_minutes = azimuth.multiply(time_hours, 60)
  @assertion.assert_eq(300, time_minutes)?
  
  // 计算不同速度下的时间
  let faster_speed = 75
  let faster_time = distance / faster_speed
  let time_saved = azimuth.add(time_hours, -faster_time)
  @assertion.assert_eq(1, time_saved)?  // 节省1小时
}

test "data_analysis_calculations" {
  // 测试数据分析计算
  // 数据集：10, 20, 30, 40, 50
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算总和
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(data1, data2), data3), data4), data5)
  @assertion.assert_eq(150, sum)?
  
  // 计算平均值
  let average = sum / 5
  @assertion.assert_eq(30, average)?
  
  // 计算方差（简化版）
  let diff1 = azimuth.add(data1, -average)
  let diff2 = azimuth.add(data2, -average)
  let diff3 = azimuth.add(data3, -average)
  let diff4 = azimuth.add(data4, -average)
  let diff5 = azimuth.add(data5, -average)
  
  let sq_diff1 = azimuth.multiply(diff1, diff1)
  let sq_diff2 = azimuth.multiply(diff2, diff2)
  let sq_diff3 = azimuth.multiply(diff3, diff3)
  let sq_diff4 = azimuth.multiply(diff4, diff4)
  let sq_diff5 = azimuth.multiply(diff5, diff5)
  
  let variance = azimuth.add(azimuth.add(azimuth.add(azimuth.add(sq_diff1, sq_diff2), sq_diff3), sq_diff4), sq_diff5) / 5
  @assertion.assert_eq(200, variance)?
}