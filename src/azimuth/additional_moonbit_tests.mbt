// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "string_length_validation" {
  // 测试字符串长度验证功能
  let short_name = "Bob"
  let medium_name = "Alexander"
  let long_name = "VeryLongNameWithManyCharacters"
  
  // 通过greet函数验证不同长度字符串的处理
  assert_eq_string("Hello, Bob!", azimuth::greet(short_name))
  assert_eq_string("Hello, Alexander!", azimuth::greet(medium_name))
  assert_eq_string("Hello, VeryLongNameWithManyCharacters!", azimuth::greet(long_name))
}

test "percentage_calculation" {
  // 测试百分比计算
  let total = 1000
  let percentage = 15
  
  // 计算百分比值：总数 × 百分比 / 100
  let value = azimuth::multiply(total, percentage) / 100
  assert_eq(150, value)
  
  // 计算增加后的值：总数 + 百分比值
  let increased = azimuth::add(total, value)
  assert_eq(1150, increased)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度（简化）：C = (F - 30) / 2
  let converted_back = azimuth::add(fahrenheit, -30) / 2
  assert_eq(25, converted_back)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let tax_rate = 10  // 10%
  let shipping = 20
  
  // 计算商品总价
  let subtotal1 = azimuth::multiply(item1_price, item1_quantity)
  let subtotal2 = azimuth::multiply(item2_price, item2_quantity)
  let subtotal = azimuth::add(subtotal1, subtotal2)
  
  // 计算税费
  let tax = azimuth::multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = azimuth::add(azimuth::add(subtotal, tax), shipping)
  
  // 验证：199*2 + 299*1 = 398 + 299 = 697
  // 税费：697 * 10% = 69
  // 总价：697 + 69 + 20 = 786
  assert_eq(786, total)
}

test "loan_interest_calculation" {
  // 测试贷款利息计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简单利息计算：利息 = 本金 × 利率 × 年数
  let interest = azimuth::multiply(principal, azimuth::multiply(annual_rate, years))
  
  // 总还款额 = 本金 + 利息
  let total_payment = azimuth::add(principal, interest)
  
  // 验证：10000 + 10000*5*3 = 10000 + 150000 = 160000
  assert_eq(160000, total_payment)
}

test "array_index_calculation" {
  // 测试二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let width = 10
  
  // 计算一维数组索引：index = row * width + col
  let index = azimuth::add(azimuth::multiply(row, width), col)
  assert_eq(34, index)
  
  // 测试边界情况
  let first_index = azimuth::add(azimuth::multiply(0, width), 0)
  assert_eq(0, first_index)
  
  let last_index = azimuth::add(azimuth::multiply(9, width), 9)
  assert_eq(99, last_index)
}

test "game_score_system" {
  // 测试游戏得分系统
  let base_score = 100
  let combo_multiplier = 3
  let bonus = 500
  let penalty = 200
  
  // 计算基础得分
  let combo_score = azimuth::multiply(base_score, combo_multiplier)
  
  // 计算最终得分
  let final_score = azimuth::add(azimuth::add(combo_score, bonus), -penalty)
  
  // 验证：100*3 + 500 - 200 = 300 + 500 - 200 = 600
  assert_eq(600, final_score)
}

test "geometric_area_calculation" {
  // 测试几何面积计算
  let rectangle_length = 12
  let rectangle_width = 8
  let triangle_base = 10
  let triangle_height = 6
  
  // 计算矩形面积：长 × 宽
  let rectangle_area = azimuth::multiply(rectangle_length, rectangle_width)
  
  // 计算三角形面积：底 × 高 / 2
  let triangle_area = azimuth::multiply(triangle_base, triangle_height) / 2
  
  // 计算总面积
  let total_area = azimuth::add(rectangle_area, triangle_area)
  
  // 验证：12*8 + 10*6/2 = 96 + 30 = 126
  assert_eq(126, total_area)
}

test "time_conversion" {
  // 测试时间单位转换
  let hours = 2
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = azimuth::add(azimuth::multiply(hours, 60), minutes)
  assert_eq(165, total_minutes)
  
  // 转换为总秒数
  let total_seconds = azimuth::multiply(total_minutes, 60)
  assert_eq(9900, total_seconds)
  
  // 计算工作时长（8小时工作制）
  let work_hours = 8
  let work_minutes = 30
  let total_work_minutes = azimuth::add(azimuth::multiply(work_hours, 60), work_minutes)
  assert_eq(510, total_work_minutes)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值运算
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(max_val, -1))
  
  // 测试最小值运算
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(max_val, azimuth::multiply(min_val, -1))
  
  // 测试极值相加
  assert_eq(-1, azimuth::add(max_val, min_val))
  
  // 测试接近边界值的运算
  assert_eq(2147483646, azimuth::add(max_val, -1))
  assert_eq(-2147483647, azimuth::add(min_val, 1))
}