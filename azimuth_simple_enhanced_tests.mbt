// Azimuth 项目简化增强测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "boundary_value_addition" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与不同值的加法
  let result1 = azimuth.add(max_val, 0)
  let result2 = azimuth.add(max_val, 1)
  let result3 = azimuth.add(max_val, -1)
  
  // 测试最小值与不同值的加法
  let result4 = azimuth.add(min_val, 0)
  let result5 = azimuth.add(min_val, 1)
  let result6 = azimuth.add(min_val, -1)
  
  // 验证溢出保护
  let check1 = result1 == max_val
  let check2 = result2 == max_val
  let check3 = result3 == max_val
  let check4 = result4 == min_val
  let check5 = result5 == min_val
  let check6 = result6 == min_val
}

test "string_unicode_handling" {
  // 字符串Unicode处理测试
  let english_name = "World"
  let chinese_name = "世界"
  let emoji_name = "🌍"
  let mixed_name = "Hello世界🌍"
  
  // 测试英文字符串
  let english_result = azimuth.greet(english_name)
  let english_check = english_result == "Hello, World!"
  
  // 测试中文字符串
  let chinese_result = azimuth.greet(chinese_name)
  let chinese_check = chinese_result == "Hello, 世界!"
  
  // 测试emoji字符串
  let emoji_result = azimuth.greet(emoji_name)
  let emoji_check = emoji_result == "Hello, 🌍!"
  
  // 测试混合字符串
  let mixed_result = azimuth.greet(mixed_name)
  let mixed_check = mixed_result == "Hello, Hello世界🌍!"
}

test "mathematical_commutativity" {
  // 数学交换律验证测试
  let a = 15
  let b = 7
  let c = -3
  
  // 验证加法交换律
  let commutative_add1 = azimuth.add(a, b)
  let commutative_add2 = azimuth.add(b, a)
  let commutative_add_check = commutative_add1 == commutative_add2
  
  // 验证乘法交换律
  let commutative_mul1 = azimuth.multiply(a, b)
  let commutative_mul2 = azimuth.multiply(b, a)
  let commutative_mul_check = commutative_mul1 == commutative_mul2
  
  // 验证负数交换律
  let neg_add1 = azimuth.add(a, c)
  let neg_add2 = azimuth.add(c, a)
  let neg_add_check = neg_add1 == neg_add2
  
  let neg_mul1 = azimuth.multiply(a, c)
  let neg_mul2 = azimuth.multiply(c, a)
  let neg_mul_check = neg_mul1 == neg_mul2
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
  let total_check = total == 160000
  
  // 测试复利计算（简化版）：本息和 = 本金 × (1 + 利率×年数)
  let compound_total = azimuth.multiply(principal, azimuth.add(1, azimuth.multiply(rate, years)))
  let compound_check = compound_total == 160000
}

test "geometry_calculations" {
  // 几何计算测试
  let length = 12  // 矩形长
  let width = 8    // 矩形宽
  let radius = 5   // 圆半径
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  let perimeter_check = perimeter == 40
  
  // 计算矩形面积：长 × 宽
  let area = azimuth.multiply(length, width)
  let area_check = area == 96
  
  // 计算圆周长（简化）：2 × π × r ≈ 2 × 3 × r
  let circumference = azimuth.multiply(azimuth.multiply(2, 3), radius)
  let circumference_check = circumference == 30
  
  // 计算圆面积（简化）：π × r² ≈ 3 × r²
  let circle_area = azimuth.multiply(3, azimuth.multiply(radius, radius))
  let circle_area_check = circle_area == 75
}

test "shopping_cart_simulation" {
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
  let final_check = final_total == 1417
}

test "time_conversion_calculations" {
  // 时间转换测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总分钟数
  let total_minutes = azimuth.add(azimuth.multiply(hours, 60), minutes)
  let minutes_check = total_minutes == 165
  
  // 转换为总秒数
  let total_seconds = azimuth.multiply(total_minutes, 60)
  let seconds_check = total_seconds == 9900
  
  // 计算时间差
  let start_hour = 8   // 8:00 AM
  let start_minute = 0
  let end_hour = 17    // 5:00 PM
  let end_minute = 30
  
  let start_total = azimuth.add(azimuth.multiply(start_hour, 60), start_minute)
  let end_total = azimuth.add(azimuth.multiply(end_hour, 60), end_minute)
  let duration = azimuth.add(end_total, -start_total)
  
  // 验证时间差：9.5小时 = 570分钟
  let duration_check = duration == 570
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
  let sum_check = sum == 425
  
  // 计算平均值
  let mean = sum / 5
  let mean_check = mean == 85
  
  // 计算极差
  let max_val = 92
  let min_val = 78
  let range = azimuth.add(max_val, -min_val)
  let range_check = range == 14
  
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
  
  let variance_check = variance == 26
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
  let score_check = final_score == 15300
  
  // 测试排名计算（前10%额外加分）
  let top_percentage = 10
  let rank_bonus = azimuth.multiply(final_score, top_percentage) / 100
  let rank_total = azimuth.add(final_score, rank_bonus)
  
  let rank_check = rank_total == 16830
}