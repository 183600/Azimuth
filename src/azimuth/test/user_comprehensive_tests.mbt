// 用户综合测试用例
// 测试 Azimuth 项目的核心功能和边界情况

test "addition_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值相加应保持最大值
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 最小值相加应保持最小值
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiplication_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最小值与-1相乘应保持最小值（特殊情况）
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 最大值与2相乘应保持最大值
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值与2相乘应保持最小值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法分配律：a × (b + c) = a × b + a × c
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 包含特殊字符
  @azimuth.assert_eq_string("Hello, \"Quoted\"!", @azimuth.greet("\"Quoted\""))
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  // 验证：10000 + (10000 × 5 × 3) = 10000 + 150000 = 160000
  @azimuth.assert_eq(160000, total)
}

test "temperature_conversion" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

test "geometry_calculations" {
  // 测试几何计算
  let radius = 7
  
  // 圆周长（简化版）：2 × π × r ≈ 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(42, circumference)
  
  // 圆面积（简化版）：π × r² ≈ 3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(147, area)
  
  // 球体体积（简化版）：4 × r³
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(1372, volume)
}

test "shopping_cart_calculations" {
  // 测试购物车计算
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let shipping = 20
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  
  // 计算购物车总价
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, shipping)
  
  // 验证：(299×2) + (199×3) + 20 = 598 + 597 + 20 = 1215
  @azimuth.assert_eq(1215, final_total)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值：(10 + 20 + 30 + 40 + 50) / 5 = 30
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算方差（简化版）
  let data_point = 35
  let variance = @azimuth.multiply(data_point - average, data_point - average)
  @azimuth.assert_eq(25, variance)
}

test "time_calculations" {
  // 测试时间计算
  let hours = 3
  let minutes = 45
  
  // 转换为总分钟数：3 × 60 + 45 = 225
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(225, total_minutes)
  
  // 转换为总秒数：225 × 60 = 13500
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(13500, total_seconds)
  
  // 计算时间差（从9:00到17:30）
  let start_time = 540  // 9:00 AM in minutes
  let end_time = 1050   // 5:30 PM in minutes
  let duration = @azimuth.add(end_time, -start_time)
  @azimuth.assert_eq(510, duration)
}