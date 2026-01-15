// 新增的 MoonBit 测试用例
// 测试 azimuth 包的核心功能，包含各种实际应用场景

test "arithmetic_sequence_calculation" {
  // 计算等差数列的和：2 + 5 + 8 + 11 + 14
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 2
  let last = 14
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(40, sum)
}

test "geometric_progression_calculation" {
  // 计算等比数列的和：3 + 6 + 12 + 24 + 48
  // 使用手动计算
  let term1 = 3
  let term2 = multiply(term1, 2)
  let term3 = multiply(term2, 2)
  let term4 = multiply(term3, 2)
  let term5 = multiply(term4, 2)
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(93, sum)
}

test "temperature_conversion_scenarios" {
  // 温度转换场景：摄氏度转华氏度（简化版：F = C × 2 + 30）
  // 测试常见温度点
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 20
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  assert_eq(70, room_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
}

test "bank_interest_calculation" {
  // 银行利息计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  let interest_rate = multiply(rate_percent, years)
  let growth_factor = add(100, interest_rate)
  let total_amount = multiply(principal, growth_factor)
  
  // 10000 × (100 + 5×3) = 10000 × 115 = 1150000
  assert_eq(1150000, total_amount)
}

test "shopping_cart_with_discount" {
  // 带折扣的购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount_threshold = 500
  let discount_rate = 10
  
  // 计算商品总价
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let subtotal = add(subtotal1, subtotal2)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let final_total = add(subtotal, -discount)
  assert_eq(697, final_total)
}

test "speed_distance_time_calculation" {
  // 速度、距离、时间计算
  let speed = 60  // km/h
  let time_hours = 2
  let time_minutes = 30
  
  // 计算总时间（分钟）
  let total_time_minutes = add(multiply(time_hours, 60), time_minutes)
  
  // 计算距离（公里）
  let distance = multiply(speed, total_time_minutes) / 60
  assert_eq(150, distance)
  
  // 计算返回所需时间（不同速度）
  let return_speed = 75
  let return_time = multiply(distance, 60) / return_speed
  assert_eq(120, return_time)
}

test "array_index_manipulation" {
  // 数组索引操作：二维数组转一维数组的索引计算
  let rows = 4
  let cols = 5
  let row = 2
  let col = 3
  
  // 计算一维数组中的索引
  let index = add(multiply(row, cols), col)
  assert_eq(13, index)
  
  // 测试边界情况
  let first_index = add(multiply(0, cols), 0)
  assert_eq(0, first_index)
  
  let last_index = add(multiply(add(rows, -1), add(cols, -1)), add(cols, -1))
  assert_eq(19, last_index)
}

test "string_processing_scenarios" {
  // 字符串处理场景
  // 测试不同类型的输入
  let username = "user123"
  let email = "test@example.com"
  let phone = "123-456-7890"
  let mixed = "User_2023@Project"
  
  // 测试greet函数处理各种字符串格式
  assert_eq_string("Hello, user123!", greet(username))
  assert_eq_string("Hello, test@example.com!", greet(email))
  assert_eq_string("Hello, 123-456-7890!", greet(phone))
  assert_eq_string("Hello, User_2023@Project!", greet(mixed))
}

test "mathematical_formula_validation" {
  // 数学公式验证：验证基本的数学恒等式
  let a = 6
  let b = 4
  
  // 验证平方差公式：(a + b) × (a - b) = a² - b²
  let sum = add(a, b)
  let diff = add(a, -b)
  let left_side = multiply(sum, diff)
  
  let a_squared = multiply(a, a)
  let b_squared = multiply(b, b)
  let right_side = add(a_squared, -b_squared)
  
  assert_eq(left_side, right_side)
  
  // 验证分配律：a × (b + c) = a × b + a × c
  let c = 3
  let distrib_left = multiply(a, add(b, c))
  let distrib_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distrib_left, distrib_right)
}

test "boundary_value_comprehensive" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法边界情况
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  
  // 测试乘法边界情况
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试连续边界运算
  let complex_calc = add(multiply(add(max_val, min_val), 2), max_val)
  assert_eq(max_val, complex_calc)
}