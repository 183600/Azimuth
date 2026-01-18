// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "integer_overflow_protection" {
  // 整数溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  let overflow_add = azimuth.add(max_val, 1)
  @assertion.assert_eq(max_val, overflow_add)?
  
  let underflow_add = azimuth.add(min_val, -1)
  @assertion.assert_eq(min_val, underflow_add)?
  
  // 测试乘法溢出保护
  let overflow_mul = azimuth.multiply(max_val, 2)
  @assertion.assert_eq(max_val, overflow_mul)?
  
  let underflow_mul = azimuth.multiply(min_val, -1)
  @assertion.assert_eq(min_val, underflow_mul)?
}

test "string_concatenation_unicode" {
  // 字符串连接与Unicode处理测试
  let english_name = "World"
  let chinese_name = "世界"
  let emoji_name = "🌍"
  let mixed_name = "Hello世界🌍"
  
  // 测试英文字符串
  let english_result = azimuth.greet(english_name)
  @assertion.assert_eq_string("Hello, World!", english_result)?
  
  // 测试中文字符串
  let chinese_result = azimuth.greet(chinese_name)
  @assertion.assert_eq_string("Hello, 世界!", chinese_result)?
  
  // 测试emoji字符串
  let emoji_result = azimuth.greet(emoji_name)
  @assertion.assert_eq_string("Hello, 🌍!", emoji_result)?
  
  // 测试混合字符串
  let mixed_result = azimuth.greet(mixed_name)
  @assertion.assert_eq_string("Hello, Hello世界🌍!", mixed_result)?
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 15
  let b = 7
  let c = 3
  
  // 验证加法交换律
  let commutative_add1 = azimuth.add(a, b)
  let commutative_add2 = azimuth.add(b, a)
  @assertion.assert_eq(commutative_add1, commutative_add2)?
  
  // 验证乘法交换律
  let commutative_mul1 = azimuth.multiply(a, b)
  let commutative_mul2 = azimuth.multiply(b, a)
  @assertion.assert_eq(commutative_mul1, commutative_mul2)?
  
  // 验证分配律
  let distributive1 = azimuth.multiply(a, azimuth.add(b, c))
  let distributive2 = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  @assertion.assert_eq(distributive1, distributive2)?
}

test "financial_calculations" {
  // 金融计算场景测试
  let principal = 10000  // 本金
  let rate = 5          // 利率（百分比）
  let years = 3         // 年数
  
  // 简单利息计算：利息 = 本金 × 利率 × 年数
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, years))
  
  // 本息和 = 本金 + 利息
  let total = azimuth.add(principal, interest)
  
  // 验证计算结果：10000 + 10000×5×3 = 160000
  @assertion.assert_eq(160000, total)?
  
  // 测试复利计算（简化版）：本息和 = 本金 × (1 + 利率×年数)
  let compound_total = azimuth.multiply(principal, azimuth.add(1, azimuth.multiply(rate, years)))
  @assertion.assert_eq(160000, compound_total)?
}

test "geometry_calculations" {
  // 几何计算测试
  let length = 12  // 矩形长
  let width = 8    // 矩形宽
  let radius = 5   // 圆半径
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(40, perimeter)?
  
  // 计算矩形面积：长 × 宽
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(96, area)?
  
  // 计算圆周长（简化）：2 × π × r ≈ 2 × 3 × r
  let circumference = azimuth.multiply(azimuth.multiply(2, 3), radius)
  @assertion.assert_eq(30, circumference)?
  
  // 计算圆面积（简化）：π × r² ≈ 3 × r²
  let circle_area = azimuth.multiply(3, azimuth.multiply(radius, radius))
  @assertion.assert_eq(75, circle_area)?
}

test "temperature_conversion" {
  // 温度转换测试
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth.add(azimuth.multiply(freezing_celsius, 2), 30)
  @assertion.assert_eq(30, freezing_fahrenheit)?
  
  // 测试室温
  let room_celsius = 20
  let room_fahrenheit = azimuth.add(azimuth.multiply(room_celsius, 2), 30)
  @assertion.assert_eq(70, room_fahrenheit)?
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = azimuth.add(azimuth.multiply(boiling_celsius, 2), 30)
  @assertion.assert_eq(230, boiling_fahrenheit)?
}

test "shopping_cart_calculation" {
  // 购物车计算测试
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  let shipping_fee = 20
  let tax_rate = 8  // 8%
  
  // 计算每种商品的总价
  let total1 = azimuth.multiply(item1_price, item1_quantity)
  let total2 = azimuth.multiply(item2_price, item2_quantity)
  let total3 = azimuth.multiply(item3_price, item3_quantity)
  
  // 计算商品总价
  let subtotal = azimuth.add(azimuth.add(total1, total2), total3)
  
  // 计算税费
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let final_total = azimuth.add(azimuth.add(subtotal, tax), shipping_fee)
  
  // 验证总价：(299×2 + 199×3 + 99×1) + 税费 + 运费
  // = (598 + 597 + 99) + 1294×8/100 + 20 = 1294 + 103 + 20 = 1417
  @assertion.assert_eq(1417, final_total)?
}

test "time_calculations" {
  // 时间计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总分钟数
  let total_minutes = azimuth.add(azimuth.multiply(hours, 60), minutes)
  @assertion.assert_eq(165, total_minutes)?
  
  // 转换为总秒数
  let total_seconds = azimuth.multiply(total_minutes, 60)
  @assertion.assert_eq(9900, total_seconds)?
  
  // 计算时间差
  let start_hour = 8   // 8:00 AM
  let start_minute = 0
  let end_hour = 17    // 5:00 PM
  let end_minute = 30
  
  let start_total = azimuth.add(azimuth.multiply(start_hour, 60), start_minute)
  let end_total = azimuth.add(azimuth.multiply(end_hour, 60), end_minute)
  let duration = azimuth.add(end_total, -start_total)
  
  @assertion.assert_eq(570, duration)?  // 9.5小时 = 570分钟
}

test "statistical_analysis" {
  // 统计分析测试
  let data1 = 85
  let data2 = 90
  let data3 = 78
  let data4 = 92
  let data5 = 80
  
  // 计算总和
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(data1, data2), data3), data4), data5)
  @assertion.assert_eq(425, sum)?
  
  // 计算平均值
  let mean = sum / 5
  @assertion.assert_eq(85, mean)?
  
  // 计算极差
  let max_val = 92
  let min_val = 78
  let range = azimuth.add(max_val, -min_val)
  @assertion.assert_eq(14, range)?
  
  // 计算方差（简化）
  let diff1 = azimuth.add(data1, -mean)
  let diff2 = azimuth.add(data2, -mean)
  let diff3 = azimuth.add(data3, -mean)
  let diff4 = azimuth.add(data4, -mean)
  let diff5 = azimuth.add(data5, -mean)
  
  let variance = azimuth.add(azimuth.add(azimuth.add(azimuth.add(
    azimuth.multiply(diff1, diff1), 
    azimuth.multiply(diff2, diff2)), 
    azimuth.multiply(diff3, diff3)), 
    azimuth.add(azimuth.multiply(diff4, diff4), azimuth.multiply(diff5, diff5))) / 5
  
  @assertion.assert_eq(26, variance)?
}

test "game_logic_simulation" {
  // 游戏逻辑模拟测试
  let base_score = 1000
  let level = 5
  let combo_multiplier = 3
  let bonus_points = 500
  let penalty = 200
  
  // 计算等级加成
  let level_bonus = azimuth.multiply(base_score, level)
  
  // 计算连击加成
  let combo_bonus = azimuth.multiply(level_bonus, combo_multiplier)
  
  // 计算最终得分
  let final_score = azimuth.add(azimuth.add(combo_bonus, bonus_points), -penalty)
  
  // 验证得分：(1000×5)×3 + 500 - 200 = 15000 + 300 = 15300
  @assertion.assert_eq(15300, final_score)?
  
  // 测试排名计算（前10%额外加分）
  let top_percentage = 10
  let rank_bonus = azimuth.multiply(final_score, top_percentage) / 100
  let rank_total = azimuth.add(final_score, rank_bonus)
  
  @assertion.assert_eq(16830, rank_total)?
}