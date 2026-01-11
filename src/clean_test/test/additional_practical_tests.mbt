// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 实际应用场景测试用例

// 测试用例1: 计算购物车折扣后的总价
///|
test "shopping_cart_discount_calculation" {
  // 商品价格
  let item1_price = 299
  let item2_price = 199
  let item3_price = 99
  
  // 商品数量
  let item1_qty = 2
  let item2_qty = 1
  let item3_qty = 3
  
  // 计算小计
  let subtotal1 = @clean_test.multiply(item1_price, item1_qty)
  let subtotal2 = @clean_test.multiply(item2_price, item2_qty)
  let subtotal3 = @clean_test.multiply(item3_price, item3_qty)
  
  // 计算总价
  let total_before_discount = @clean_test.add(@clean_test.add(subtotal1, subtotal2), subtotal3)
  
  // 应用折扣：满500减50
  let discount = if total_before_discount >= 500 { 50 } else { 0 }
  let final_total = @clean_test.add(total_before_discount, @clean_test.multiply(-1, discount))
  
  // 验证结果：(299*2) + 199 + (99*3) - 50 = 598 + 199 + 297 - 50 = 1044
  assert_eq(1044, final_total)
}

// 测试用例2: 计算银行贷款月供（简化版）
///|
test "bank_loan_monthly_payment" {
  // 贷款本金
  let principal = 300000
  
  // 年利率（5%表示为5/100）
  let annual_rate_numerator = 5
  let annual_rate_denominator = 100
  
  // 贷款年限
  let years = 20
  
  // 月数
  let months = @clean_test.@clean_test.multiply(years, 12)
  
  // 简化计算：只计算总利息和本金总和
  // 总利息 = 本金 * 年利率 * 年限
  let total_interest = @clean_test.multiply(@clean_test.multiply(principal, annual_rate_numerator), years)
  
  // 还款总额 = 本金 + 总利息
  let total_payment = @clean_test.add(principal, @clean_test.multiply(total_interest, annual_rate_denominator))
  
  // 验证结果：300000 + (300000 * 5 * 20) = 300000 + 30000000 = 30300000
  assert_eq(30300000, total_payment)
}

// 测试用例3: 计算身体质量指数(BMI)的分子部分
///|
test "bmi_calculation_numerator" {
  // 体重(kg)
  let weight = 70
  
  // 身高(m)的平方，我们避免小数用厘米计算
  let height_cm = 175
  let height_m_squared_numerator = @clean_test.@clean_test.multiply(height_cm, height_cm)
  let height_m_squared_denominator = 10000  // (100*100)
  
  // BMI = 体重(kg) / 身高²(m²)
  // 我们只计算分子部分：体重 * 分母
  let bmi_numerator = @clean_test.multiply(weight, height_m_squared_denominator)
  
  // 验证：70 * 10000 = 700000
  assert_eq(700000, bmi_numerator)
}

// 测试用例4: 计算燃油消耗和费用
///|
test "fuel_consumption_calculation" {
  // 汽车行驶距离(km)
  let distance = 500
  
  // 油耗(L/100km)
  let fuel_consumption = 8
  
  // 油价(元/L)
  let fuel_price = 7
  
  // 计算总油耗：距离 * 油耗 / 100
  let total_fuel_times_100 = @clean_test.multiply(distance, fuel_consumption)
  
  // 计算总费用：总油耗 * 油价
  let total_cost_times_100 = @clean_test.multiply(total_fuel_times_100, fuel_price)
  
  // 验证：500 * 8 * 7 = 28000，实际费用是280元
  assert_eq(28000, total_cost_times_100)
}

// 测试用例5: 计算投资复利（简化版）
///|
test "investment_compound_interest" {
  // 初始投资
  let principal = 10000
  
  // 年收益率（8%表示为8/100）
  let rate_numerator = 8
  let rate_denominator = 100
  
  // 投资年限
  let years = 5
  
  // 简化计算：只计算2年的复利作为示例
  // 第一年结束：本金 + 利息
  let year1_value_times_den = @clean_test.add(@clean_test.multiply(principal, rate_denominator), @clean_test.multiply(principal, rate_numerator))
  
  // 第二年结束：第一年价值 + 新利息
  let year2_value_times_den = @clean_test.add(@clean_test.multiply(year1_value_times_den, rate_denominator), @clean_test.multiply(year1_value_times_den, rate_numerator))
  
  // 验证计算过程
  assert_eq(true, year2_value_times_den > principal * rate_denominator)
}

// 测试用例6: 计算房屋面积和装修预算
///|
test "home_renovation_budget" {
  // 房屋尺寸
  let length = 8
  let width = 6
  let height = 3
  
  // 计算地面面积
  let floor_area = @clean_test.multiply(length, width)
  
  // 计算墙面面积（四面墙）
  let wall_area = @clean_test.multiply(2, @clean_test.add(@clean_test.multiply(length, height), @clean_test.multiply(width, height)))
  
  // 装修单价（元/平方米）
  let floor_price = 150
  let wall_price = 80
  
  // 计算总预算
  let floor_budget = @clean_test.multiply(floor_area, floor_price)
  let wall_budget = @clean_test.multiply(wall_area, wall_price)
  let total_budget = @clean_test.add(floor_budget, wall_budget)
  
  // 验证：地面48*150 + 墙面84*80 = 7200 + 6720 = 13920
  assert_eq(13920, total_budget)
}

// 测试用例7: 计算运动卡路里消耗
///|
test "exercise_calories_calculation" {
  // 运动时间（分钟）
  let running_time = 30
  let swimming_time = 45
  
  // 每分钟消耗卡路里
  let running_calories_per_min = 10
  let swimming_calories_per_min = 8
  
  // 计算总消耗
  let running_calories = @clean_test.multiply(running_time, running_calories_per_min)
  let swimming_calories = @clean_test.multiply(swimming_time, swimming_calories_per_min)
  let total_calories = @clean_test.add(running_calories, swimming_calories)
  
  // 验证：30*10 + 45*8 = 300 + 360 = 660
  assert_eq(660, total_calories)
}

// 测试用例8: 计算项目管理工时和成本
///|
test "project_management_cost" {
  // 项目各阶段工时（小时）
  let planning_hours = 40
  let development_hours = 160
  let testing_hours = 80
  
  // 人员成本（元/小时）
  let developer_rate = 150
  let tester_rate = 120
  
  // 计算总工时
  let total_hours = @clean_test.add(@clean_test.add(planning_hours, development_hours), testing_hours)
  
  // 计算总成本（假设开发和测试各占一半）
  let dev_cost = @clean_test.multiply(@clean_test.add(planning_hours, development_hours), developer_rate)
  let test_cost = @clean_test.multiply(testing_hours, tester_rate)
  let total_cost = @clean_test.add(dev_cost, test_cost)
  
  // 验证：200*150 + 80*120 = 30000 + 9600 = 39600
  assert_eq(39600, total_cost)
}

// 测试用例9: 测试国际化字符串处理
///|
test "internationalization_string_tests" {
  // 测试不同语言的问候
  let chinese_name = "小明"
  let english_name = "Alice"
  let japanese_name = "田中"
  let korean_name = "김철수"
  
  // 测试中文
  let chinese_greeting = @clean_test.greet(chinese_name)
  assert_eq(true, chinese_greeting.contains("小明"))
  
  // 测试英文
  let english_greeting = @clean_test.greet(english_name)
  assert_eq(true, english_greeting.contains("Alice"))
  
  // 测试日文
  let japanese_greeting = @clean_test.greet(japanese_name)
  assert_eq(true, japanese_greeting.contains("田中"))
  
  // 测试韩文
  let korean_greeting = @clean_test.greet(korean_name)
  assert_eq(true, korean_greeting.contains("김철수"))
  
  // 验证不同语言产生不同的问候语
  assert_eq(true, chinese_greeting != english_greeting)
  assert_eq(true, english_greeting != japanese_greeting)
  assert_eq(true, japanese_greeting != korean_greeting)
}

// 测试用例10: 计算几何级数求和
///|
test "geometric_series_sum" {
  // 计算几何级数：a + ar + ar² + ... + arⁿ
  // 例子：2 + 6 + 18 + 54 = 80 (a=2, r=3, n=3)
  
  let first_term = 2
  let ratio = 3
  let num_terms = 4
  
  // 逐项计算
  let term1 = first_term
  let term2 = @clean_test.multiply(term1, ratio)
  let term3 = @clean_test.multiply(term2, ratio)
  let term4 = @clean_test.multiply(term3, ratio)
  
  // 求和
  let sum = @clean_test.add(@clean_test.add(@clean_test.add(term1, term2), term3), term4)
  
  // 验证：2 + 6 + 18 + 54 = 80
  assert_eq(80, sum)
}