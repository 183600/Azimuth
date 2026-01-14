// 增强的 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "enhanced_add_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = add(a, b)
  let result2 = add(b, a)
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -12345
  let d = 67890
  
  let result3 = add(c, d)
  let result4 = add(d, c)
  assert_eq(result3, result4)
  
  // 测试两个负数的情况
  let e = -12345
  let f = -67890
  
  let result5 = add(e, f)
  let result6 = add(f, e)
  assert_eq(result5, result6)
}

test "enhanced_multiply_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 15
  let b = 20
  let c = 25
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let d = -10
  let e = 30
  let f = -40
  
  let left_side2 = multiply(d, add(e, f))
  let right_side2 = add(multiply(d, e), multiply(d, f))
  assert_eq(left_side2, right_side2)
}

test "enhanced_string_multilingual" {
  // 测试多语言字符串处理
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
}

test "enhanced_financial_compound_interest" {
  // 测试复利计算：A = P(1 + r/n)^(nt)
  // 简化版本：A = P(1 + r×t)
  let principal = 10000  // 本金
  let rate = 5          // 年利率（百分比）
  let time = 3          // 年数
  
  // 简化的复利计算
  let growth_factor = add(100, multiply(rate, time))
  let amount = multiply(principal, growth_factor) / 100
  
  assert_eq(115000, amount)  // 10000 × (100 + 5×3) / 100 = 115000
  
  // 测试不同本金和利率
  let principal2 = 50000
  let rate2 = 8
  let time2 = 5
  
  let growth_factor2 = add(100, multiply(rate2, time2))
  let amount2 = multiply(principal2, growth_factor2) / 100
  
  assert_eq(250000, amount2)  // 50000 × (100 + 8×5) / 100 = 250000
}

test "enhanced_temperature_conversions" {
  // 测试温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化版本：F = C × 2 + 30
  
  // 测试常见温度点
  let celsius0 = 0      // 冰点
  let fahrenheit0 = add(multiply(celsius0, 2), 30)
  assert_eq(30, fahrenheit0)
  
  let celsius100 = 100  // 沸点
  let fahrenheit100 = add(multiply(celsius100, 2), 30)
  assert_eq(230, fahrenheit100)
  
  let celsius25 = 25    // 室温
  let fahrenheit25 = add(multiply(celsius25, 2), 30)
  assert_eq(80, fahrenheit25)
  
  let celsius_10 = -10  // 低温
  let fahrenheit_10 = add(multiply(celsius_10, 2), 30)
  assert_eq(10, fahrenheit_10)
  
  // 华氏度转摄氏度（简化版本）：C = (F - 30) / 2
  let fahrenheit70 = 70
  let celsius70 = add(fahrenheit70, -30) / 2
  assert_eq(20, celsius70)
}

test "enhanced_geometry_perimeter_calculations" {
  // 测试几何图形周长计算
  
  // 矩形周长：2 × (长 + 宽)
  let length = 15
  let width = 8
  let rectangle_perimeter = multiply(2, add(length, width))
  assert_eq(46, rectangle_perimeter)
  
  // 正方形周长：4 × 边长
  let side = 12
  let square_perimeter = multiply(4, side)
  assert_eq(48, square_perimeter)
  
  // 三角形周长：三条边之和
  let side_a = 7
  let side_b = 9
  let side_c = 12
  let triangle_perimeter = add(add(side_a, side_b), side_c)
  assert_eq(28, triangle_perimeter)
  
  // 梯形周长：四条边之和
  let top_base = 10
  let bottom_base = 18
  let left_side = 13
  let right_side = 15
  let trapezoid_perimeter = add(add(add(top_base, bottom_base), left_side), right_side)
  assert_eq(56, trapezoid_perimeter)
}

test "enhanced_shopping_cart_with_discounts" {
  // 测试购物车计算，包含折扣
  
  // 商品价格和数量
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品的总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let total3 = multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let subtotal = add(add(total1, total2), total3)
  
  // 计算折扣（满1000减100）
  let discount = if (subtotal >= 1000) { 100 } else { 0 }
  
  // 计算最终价格
  let final_total = add(subtotal, -discount)
  
  assert_eq(1194, final_total)  // 1294 - 100 = 1194
  
  // 测试小额订单（无折扣）
  let small_item_price = 50
  let small_item_quantity = 3
  let small_total = multiply(small_item_price, small_item_quantity)
  let small_discount = if (small_total >= 1000) { 100 } else { 0 }
  let small_final = add(small_total, -small_discount)
  
  assert_eq(150, small_final)
}

test "enhanced_statistical_calculations" {
  // 测试统计计算
  
  // 数据集：10, 20, 30, 40, 50, 60, 70, 80, 90, 100
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  let data6 = 60
  let data7 = 70
  let data8 = 80
  let data9 = 90
  let data10 = 100
  
  // 计算总和
  let sum = add(add(add(add(add(add(add(add(add(data1, data2), data3), data4), data5), data6), data7), data8), data9), data10)
  assert_eq(550, sum)
  
  // 计算平均值
  let average = sum / 10
  assert_eq(55, average)
  
  // 计算中位数（偶数个数据，取中间两个的平均值）
  let median = add(data5, data6) / 2
  assert_eq(55, median)
  
  // 计算范围（最大值 - 最小值）
  let range = add(data10, -data1)
  assert_eq(90, range)
}

test "enhanced_time_zone_calculations" {
  // 测试时区计算
  
  // 基准时间（UTC时间的小时数）
  let utc_time = 15  // 下午3点
  
  // 不同时区的偏移量（小时）
  let est_offset = -5  // 东部标准时间
  let pst_offset = -8  // 太平洋标准时间
  let cet_offset = 1   // 中欧时间
  let jst_offset = 9   // 日本标准时间
  
  // 计算各时区的时间
  let est_time = add(utc_time, est_offset)
  let pst_time = add(utc_time, pst_offset)
  let cet_time = add(utc_time, cet_offset)
  let jst_time = add(utc_time, jst_offset)
  
  assert_eq(10, est_time)  // UTC-5
  assert_eq(7, pst_time)   // UTC-8
  assert_eq(16, cet_time)  // UTC+1
  assert_eq(24, jst_time)  // UTC+9（次日零点）
  
  // 测试时间跨日期处理
  let late_utc = 23  // 晚上11点
  let est_late = add(late_utc, est_offset)
  let jst_early = add(late_utc, jst_offset)
  
  assert_eq(18, est_late)   // 同一天晚上6点
  assert_eq(32, jst_early)  // 次日上午8点（24+8）
}

test "enhanced_energy_efficiency_calculations" {
  // 测试能效计算
  
  // 设备参数
  let power_rating = 100      // 额定功率（瓦特）
  let hours_per_day = 8       // 每天使用小时数
  let days_per_month = 30     // 每月天数
  let electricity_rate = 15   // 电价（分/千瓦时）
  
  // 计算每日能耗（瓦时）
  let daily_consumption = multiply(power_rating, hours_per_day)
  
  // 计算每月能耗（瓦时）
  let monthly_consumption = multiply(daily_consumption, days_per_month)
  
  // 转换为千瓦时
  let monthly_kwh = monthly_consumption / 1000
  
  // 计算每月电费（分）
  let monthly_cost = multiply(monthly_kwh, electricity_rate)
  
  assert_eq(24, monthly_kwh)      // 100×8×30/1000 = 24 kWh
  assert_eq(360, monthly_cost)    // 24×15 = 360 分
  
  // 计算能效等级（基于能耗）
  let efficiency_grade = if (monthly_kwh <= 20) { "A" } 
                        else if (monthly_kwh <= 30) { "B" }
                        else if (monthly_kwh <= 40) { "C" }
                        else { "D" }
  
  assert_eq_string("B", efficiency_grade)
  
  // 测试高能耗设备
  let high_power_device = 200
  let high_daily_consumption = multiply(high_power_device, hours_per_day)
  let high_monthly_consumption = multiply(high_daily_consumption, days_per_month)
  let high_monthly_kwh = high_monthly_consumption / 1000
  let high_efficiency_grade = if (high_monthly_kwh <= 20) { "A" } 
                             else if (high_monthly_kwh <= 30) { "B" }
                             else if (high_monthly_kwh <= 40) { "C" }
                             else { "D" }
  
  assert_eq(48, high_monthly_kwh)     // 200×8×30/1000 = 48 kWh
  assert_eq_string("D", high_efficiency_grade)
}