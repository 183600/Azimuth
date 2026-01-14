// 额外的 MoonBit 测试用例
// 这些测试用例涵盖了不同的应用场景和边界情况

// 导入主模块中的函数
use azimuth

test "simple_calculator_operations" {
  // 简单计算器操作测试
  let a = 15
  let b = 7
  
  // 加法运算
  let sum = add(a, b)
  assert_eq(22, sum)
  
  // 乘法运算
  let product = multiply(a, b)
  assert_eq(105, product)
  
  // 组合运算：(a + b) * 2
  let combined = multiply(add(a, b), 2)
  assert_eq(44, combined)
}

test "discount_calculation" {
  // 折扣计算测试
  let original_price = 1000
  let discount_percentage = 20
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percentage) / 100
  assert_eq(200, discount_amount)
  
  // 计算折后价格
  let discounted_price = add(original_price, -discount_amount)
  assert_eq(800, discounted_price)
}

test "temperature_conversion" {
  // 温度转换测试（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) / 2（简化公式）
  let fahrenheit_back = 80
  let celsius_back = add(fahrenheit_back, -30) / 2
  assert_eq(25, celsius_back)
}

test "grade_average_calculation" {
  // 成绩平均分计算
  let math_score = 85
  let english_score = 90
  let science_score = 78
  let history_score = 92
  
  // 计算总分
  let total_score = add(add(add(math_score, english_score), science_score), history_score)
  assert_eq(345, total_score)
  
  // 计算平均分
  let average_score = total_score / 4
  assert_eq(86, average_score)
}

test "shopping_budget_management" {
  // 购物预算管理测试
  let monthly_budget = 5000
  let week1_spent = 1200
  let week2_spent = 800
  let week3_spent = 1500
  
  // 计算前三周总支出
  let total_spent = add(add(week1_spent, week2_spent), week3_spent)
  assert_eq(3500, total_spent)
  
  // 计算剩余预算
  let remaining_budget = add(monthly_budget, -total_spent)
  assert_eq(1500, remaining_budget)
  
  // 计算第四周可支出金额（不超过预算）
  let week4_budget = remaining_budget
  assert_eq(1500, week4_budget)
}

test "compound_interest_simplified" {
  // 简化的复利计算测试
  let principal = 10000
  let annual_rate = 5
  let years = 3
  
  // 简化复利计算：本金 × (1 + 利率 × 年数)
  let growth_factor = add(1, multiply(annual_rate, years))
  let total_amount = multiply(principal, growth_factor)
  assert_eq(160000, total_amount)
  
  // 计算利息收益
  let interest_earned = add(total_amount, -principal)
  assert_eq(150000, interest_earned)
}

test "string_greeting_variations" {
  // 字符串问候变体测试
  // 测试不同类型的名称
  let english_name = "Alice"
  let chinese_name = "张三"
  let mixed_name = "John李"
  let numeric_name = "User123"
  
  // 测试各种名称的问候
  assert_eq_string("Hello, Alice!", greet(english_name))
  assert_eq_string("Hello, 张三!", greet(chinese_name))
  assert_eq_string("Hello, John李!", greet(mixed_name))
  assert_eq_string("Hello, User123!", greet(numeric_name))
  
  // 测试空字符串和单字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
}

test "area_perimeter_calculations" {
  // 面积和周长计算测试
  let rectangle_length = 12
  let rectangle_width = 8
  
  // 计算矩形面积
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  assert_eq(96, rectangle_area)
  
  // 计算矩形周长
  let rectangle_perimeter = add(multiply(2, rectangle_length), multiply(2, rectangle_width))
  assert_eq(40, rectangle_perimeter)
  
  // 计算正方形（边长为5）的面积和周长
  let square_side = 5
  let square_area = multiply(square_side, square_side)
  let square_perimeter = multiply(4, square_side)
  
  assert_eq(25, square_area)
  assert_eq(20, square_perimeter)
}

test "speed_distance_time" {
  // 速度、距离、时间计算测试
  let speed = 60  // km/h
  let time = 2.5  // 小时（用整数表示：2小时30分钟 = 150分钟）
  
  // 使用整数计算：距离 = 速度 × 时间（分钟）/ 60
  let time_minutes = 150
  let distance = multiply(speed, time_minutes) / 60
  assert_eq(150, distance)
  
  // 反向计算：时间 = 距离 × 60 / 速度
  let calculated_time = multiply(distance, 60) / speed
  assert_eq(150, calculated_time)
}

test "array_sum_product_simulation" {
  // 数组求和和求积的模拟测试
  // 模拟数组 [2, 3, 4, 5] 的运算
  
  // 计算数组元素的和
  let sum1 = add(2, 3)
  let sum2 = add(sum1, 4)
  let total_sum = add(sum2, 5)
  assert_eq(14, total_sum)
  
  // 计算数组元素的积
  let product1 = multiply(2, 3)
  let product2 = multiply(product1, 4)
  let total_product = multiply(product2, 5)
  assert_eq(120, total_product)
  
  // 计算平均值
  let average = total_sum / 4
  assert_eq(3, average)
}