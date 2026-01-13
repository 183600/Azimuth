// 实用测试用例集合
// 这些测试用例模拟真实世界的应用场景

test "bank_interest_calculation" {
  // 银行利息计算：本金 + (本金 × 利率 × 时间)
  let principal = 10000  // 本金 10000元
  let rate = 3          // 年利率 3%
  let years = 5         // 存期 5年
  
  // 计算利息：10000 × 3 × 5 = 150000
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 计算本息和：10000 + 150000 = 160000
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)
}

test "shopping_cart_with_discount" {
  // 购物车计算与折扣
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount_rate = 10  // 10% 折扣
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)  // 299 × 2 = 598
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)  // 199 × 1 = 199
  let subtotal = @azimuth.add(item1_total, item2_total)            // 598 + 199 = 797
  
  // 计算折扣金额：797 × 10 ÷ 100 = 79（整数除法）
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  // 计算最终价格：797 - 79 = 718
  let final_price = @azimuth.add(subtotal, -discount_amount)
  
  @azimuth.assert_eq(718, final_price)
}

test "temperature_conversion_batch" {
  // 批量温度转换：摄氏度转华氏度（近似公式：F = C × 2 + 30）
  let celsius_temps = [0, 10, 20, 30, 40]  // 摄氏度数组
  let expected_fahrenheit = [30, 50, 70, 90, 110]  // 期望的华氏度结果
  
  // 转换第一个温度：0 × 2 + 30 = 30
  let temp1 = @azimuth.add(@azimuth.multiply(celsius_temps[0], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], temp1)
  
  // 转换第二个温度：10 × 2 + 30 = 50
  let temp2 = @azimuth.add(@azimuth.multiply(celsius_temps[1], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[1], temp2)
  
  // 转换第三个温度：20 × 2 + 30 = 70
  let temp3 = @azimuth.add(@azimuth.multiply(celsius_temps[2], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], temp3)
}

test "area_perimeter_calculation" {
  // 房间面积和周长计算
  let room_length = 6  // 房间长度 6米
  let room_width = 4   // 房间宽度 4米
  let room_height = 3  // 房间高度 3米
  
  // 计算房间面积：6 × 4 = 24 平方米
  let floor_area = @azimuth.multiply(room_length, room_width)
  
  // 计算房间周长：2 × (6 + 4) = 2 × 10 = 20 米
  let perimeter = @azimuth.multiply(2, @azimuth.add(room_length, room_width))
  
  // 计算墙面面积：周长 × 高度 = 20 × 3 = 60 平方米
  let wall_area = @azimuth.multiply(perimeter, room_height)
  
  @azimuth.assert_eq(24, floor_area)
  @azimuth.assert_eq(20, perimeter)
  @azimuth.assert_eq(60, wall_area)
}

test "score_statistics_calculation" {
  // 学生成绩统计
  let math_score = 85
  let english_score = 92
  let science_score = 78
  let history_score = 88
  
  // 计算总分：85 + 92 + 78 + 88 = 343
  let total_score = @azimuth.add(@azimuth.add(math_score, english_score), 
                                   @azimuth.add(science_score, history_score))
  
  // 计算平均分：343 ÷ 4 = 85（整数除法）
  let average_score = total_score / 4
  
  // 计算最高分与最低分的差值：92 - 78 = 14
  let score_range = @azimuth.add(92, -78)
  
  @azimuth.assert_eq(343, total_score)
  @azimuth.assert_eq(85, average_score)
  @azimuth.assert_eq(14, score_range)
}

test "fuel_consumption_calculation" {
  // 燃油消耗计算
  let distance = 500      // 行驶距离 500公里
  let fuel_efficiency = 8 // 油耗 8升/100公里
  let fuel_price = 7      // 油价 7元/升
  
  // 计算总油耗：500 × 8 ÷ 100 = 40 升
  let total_fuel = @azimuth.multiply(distance, fuel_efficiency) / 100
  
  // 计算总油费：40 × 7 = 280 元
  let total_cost = @azimuth.multiply(total_fuel, fuel_price)
  
  // 计算每公里油费：280 ÷ 500 = 0（整数除法，实际为0.56元）
  let cost_per_km = total_cost / distance
  
  @azimuth.assert_eq(40, total_fuel)
  @azimuth.assert_eq(280, total_cost)
}

test "project_time_estimation" {
  // 项目时间估算
  let task1_days = 5
  let task2_days = 3
  let task3_days = 7
  let buffer_percentage = 20  // 20% 缓冲时间
  
  // 计算基础任务时间：5 + 3 + 7 = 15 天
  let base_time = @azimuth.add(@azimuth.add(task1_days, task2_days), task3_days)
  
  // 计算缓冲时间：15 × 20 ÷ 100 = 3 天
  let buffer_time = @azimuth.multiply(base_time, buffer_percentage) / 100
  
  // 计算总预估时间：15 + 3 = 18 天
  let total_estimated_time = @azimuth.add(base_time, buffer_time)
  
  @azimuth.assert_eq(15, base_time)
  @azimuth.assert_eq(3, buffer_time)
  @azimuth.assert_eq(18, total_estimated_time)
}

test "batch_greeting_processing" {
  // 批量问候处理
  let user_names = ["Alice", "Bob", "Charlie", "Diana"]
  
  // 处理每个用户的问候
  let greeting1 = @azimuth.greet(user_names[0])
  let greeting2 = @azimuth.greet(user_names[1])
  let greeting3 = @azimuth.greet(user_names[2])
  let greeting4 = @azimuth.greet(user_names[3])
  
  // 验证问候语
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  @azimuth.assert_eq_string("Hello, Charlie!", greeting3)
  @azimuth.assert_eq_string("Hello, Diana!", greeting4)
}

test "compound_growth_calculation" {
  // 复合增长计算（简化版）
  let initial_value = 1000  // 初始值
  let growth_rate = 5       // 增长率 5%
  let periods = 3           // 周期数
  
  // 第一期：1000 + (1000 × 5) = 1000 + 5000 = 6000
  let period1 = @azimuth.add(initial_value, @azimuth.multiply(initial_value, growth_rate))
  
  // 第二期：6000 + (6000 × 5) = 6000 + 30000 = 36000
  let period2 = @azimuth.add(period1, @azimuth.multiply(period1, growth_rate))
  
  // 第三期：36000 + (36000 × 5) = 36000 + 180000 = 216000
  let period3 = @azimuth.add(period2, @azimuth.multiply(period2, growth_rate))
  
  @azimuth.assert_eq(6000, period1)
  @azimuth.assert_eq(36000, period2)
  @azimuth.assert_eq(216000, period3)
}

test "nested_calculations" {
  // 嵌套计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 4
  
  // 计算 (a + b)：10 + 5 = 15
  let sum_ab = @azimuth.add(a, b)
  
  // 计算 (c - d)：20 - 8 = 12
  let diff_cd = @azimuth.add(c, -d)
  
  // 计算 (a + b) × (c - d)：15 × 12 = 180
  let product1 = @azimuth.multiply(sum_ab, diff_cd)
  
  // 计算 e × f：3 × 4 = 12
  let product2 = @azimuth.multiply(e, f)
  
  // 计算最终结果：180 + 12 = 192
  let final_result = @azimuth.add(product1, product2)
  
  @azimuth.assert_eq(192, final_result)
}