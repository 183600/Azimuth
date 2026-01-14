// 实用测试用例集 - 包含实际应用场景的测试

test "string_length_validation" {
  // 测试字符串长度验证
  let short_name = "Bob"
  let medium_name = "Elizabeth"
  let long_name = "AlexanderTheGreat"
  
  assert_eq_string("Hello, Bob!", greet(short_name))
  assert_eq_string("Hello, Elizabeth!", greet(medium_name))
  assert_eq_string("Hello, AlexanderTheGreat!", greet(long_name))
}

test "calculation_precision" {
  // 测试计算精度
  let result1 = add(1000000, 2000000)
  let result2 = multiply(1000, 1000)
  
  assert_eq(3000000, result1)
  assert_eq(1000000, result2)
  
  // 测试复合计算的精度
  let complex_result = add(multiply(100, 20), multiply(50, 10))
  assert_eq(2500, complex_result)
}

test "input_validation_edge_cases" {
  // 测试输入验证的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值的加法
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试极值的乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
}

test "real_world_financial_calculation" {
  // 实际金融计算场景
  let principal = 10000  // 本金
  let rate = 5           // 利率5%
  let years = 3          // 3年
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + (10000 × 5 × 3) = 160000
  
  // 测试复利计算（简化版）
  let compound_total = multiply(principal, add(100, multiply(rate, years)))
  assert_eq(1600000, compound_total)  // 10000 × (100 + 5×3) = 10000 × 115 = 1150000
}

test "temperature_conversion_scenarios" {
  // 温度转换场景测试
  // 摄氏度转华氏度（简化公式：F = C × 2 + 30）
  let freezing_celsius = 0
  let room_celsius = 20
  let boiling_celsius = 100
  
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  
  assert_eq(30, freezing_fahrenheit)   // 0 × 2 + 30 = 30
  assert_eq(70, room_fahrenheit)       // 20 × 2 + 30 = 70
  assert_eq(230, boiling_fahrenheit)   // 100 × 2 + 30 = 230
}

test "shopping_cart_total_calculation" {
  // 购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount = 50
  let shipping = 15
  
  // 计算商品总价
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let subtotal = add(subtotal1, subtotal2)
  
  // 应用折扣和运费
  let final_total = add(add(subtotal, -discount), shipping)
  
  assert_eq(1194, final_total)  // (299×2 + 199×3) - 50 + 15 = 1194
}

test "array_index_calculations" {
  // 数组索引计算测试
  let rows = 5
  let cols = 4
  let row_index = 2
  let col_index = 3
  
  // 二维数组转一维数组的索引计算
  let linear_index = add(multiply(row_index, cols), col_index)
  assert_eq(11, linear_index)  // 2×4 + 3 = 11
  
  // 测试边界情况
  let first_element = add(multiply(0, cols), 0)
  let last_element = add(multiply(rows - 1, cols), cols - 1)
  
  assert_eq(0, first_element)
  assert_eq(19, last_element)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 500
  let percentage = 25
  
  // 计算百分比值
  let percentage_value = multiply(total, percentage) / 100
  assert_eq(125, percentage_value)  // 500 × 25 / 100 = 125
  
  // 计算增长后的值
  let increased_value = add(total, percentage_value)
  assert_eq(625, increased_value)  // 500 + 125 = 625
  
  // 计算折扣后的值
  let discounted_value = add(total, -percentage_value)
  assert_eq(375, discounted_value)  // 500 - 125 = 375
}

test "time_conversion_calculations" {
  // 时间转换计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  assert_eq(9930, total_seconds)  // 2×3600 + 45×60 + 30 = 9930
  
  // 转换为总分钟数（忽略秒数）
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(165, total_minutes)  // 2×60 + 45 = 165
}

test "geometric_area_calculations" {
  // 几何面积计算测试
  let rectangle_length = 12
  let rectangle_width = 8
  let triangle_base = 10
  let triangle_height = 6
  let circle_radius = 5
  
  // 计算矩形面积
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  assert_eq(96, rectangle_area)  // 12 × 8 = 96
  
  // 计算三角形面积
  let triangle_area = multiply(triangle_base, triangle_height) / 2
  assert_eq(30, triangle_area)  // 10 × 6 / 2 = 30
  
  // 计算圆面积（简化：π ≈ 3）
  let circle_area = multiply(3, multiply(circle_radius, circle_radius))
  assert_eq(75, circle_area)  // 3 × 5² = 75
}