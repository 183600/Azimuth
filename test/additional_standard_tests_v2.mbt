// 额外的标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能，包含实用的测试场景

test "arithmetic_sequence_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = multiply(n, add(n, 1)) / 2
  assert_eq(55, sum)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试温度转换：华氏度 = 摄氏度 × 9/5 + 32
  // 使用整数近似：华氏度 = 摄氏度 × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = add(multiply(100, 2), 30)
  assert_eq(230, boiling)
}

test "simple_interest_calculation" {
  // 测试简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest)
  assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "geometric_area_calculations" {
  // 测试几何面积计算
  // 矩形面积 = 长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积 = 底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = multiply(base, height) / 2
  assert_eq(30, triangle_area)
  
  // 梯形面积 = (上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 15
  let trapezoid_height = 8
  let trapezoid_area = multiply(add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(80, trapezoid_area)
}

test "percentage_calculations" {
  // 测试百分比计算
  let total = 800
  let percentage = 25  // 25%
  
  // 计算百分比值
  let value = multiply(total, percentage) / 100
  assert_eq(200, value)
  
  // 计算增加后的值
  let increased = add(total, value)
  assert_eq(1000, increased)
  
  // 计算减少后的值
  let decreased = add(total, -value)
  assert_eq(600, decreased)
}

test "time_conversion_calculations" {
  // 测试时间转换计算
  let hours = 3
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(225, total_minutes)
  
  // 转换为总秒数
  let total_seconds = multiply(total_minutes, 60)
  assert_eq(13500, total_seconds)
  
  // 计算时间差
  let start_hour = 9   // 9:00
  let end_hour = 14    // 14:00
  let duration_hours = add(end_hour, -start_hour)
  assert_eq(5, duration_hours)
}

test "shopping_cart_with_discount" {
  // 测试购物车计算（含折扣）
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount_threshold = 500
  let discount_rate = 10  // 10%
  let shipping = 20
  
  // 计算商品总价
  let subtotal = add(multiply(item1_price, item1_quantity), multiply(item2_price, item2_quantity))
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终总价
  let final_total = add(add(subtotal, -discount), shipping)
  assert_eq(698, final_total)  // (299×2 + 199×1) - 79 + 20 = 698
}

test "average_calculation" {
  // 测试平均值计算
  let scores = [85, 90, 78, 92, 88]
  
  // 计算总分
  let total_score = add(add(add(add(85, 90), 78), 92), 88)
  
  // 计算平均分
  let average_score = total_score / 5
  assert_eq(86, average_score)  // (85 + 90 + 78 + 92 + 88) / 5 = 86
  
  // 测试带权重的平均值
  let weighted_sum = add(add(multiply(85, 3), multiply(90, 2)), multiply(78, 1))
  let total_weight = add(3, add(2, 1))
  let weighted_average = weighted_sum / total_weight
  assert_eq(86, weighted_average)  // (85×3 + 90×2 + 78×1) / 6 = 86
}

test "string_processing_scenarios" {
  // 测试字符串处理场景
  // 测试数字字符串
  let numeric_string = "12345"
  assert_eq_string("Hello, 12345!", greet(numeric_string))
  
  // 测试包含空格的字符串
  let spaced_string = "John Doe"
  assert_eq_string("Hello, John Doe!", greet(spaced_string))
  
  // 测试包含特殊字符的字符串
  let special_string = "user@example.com"
  assert_eq_string("Hello, user@example.com!", greet(special_string))
  
  // 测试Unicode字符串
  let unicode_string = "测试用户"
  assert_eq_string("Hello, 测试用户!", greet(unicode_string))
}

test "boundary_value_comprehensive" {
  // 测试边界值的综合应用
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值的算术运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  
  // 测试边界值的乘法运算
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试边界值与普通值的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
}