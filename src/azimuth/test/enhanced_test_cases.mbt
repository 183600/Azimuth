// 增强的 MoonBit 测试用例
// 包含实际应用场景和边界条件测试

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数运算近似：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)  // 0 × 2 + 30 = 30
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)  // 100 × 2 + 30 = 230
}

test "shopping_cart_total_with_discount" {
  // 购物车总价计算，包含折扣逻辑
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 1000
  let discount_rate = 10
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 应用折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  let final_total = @azimuth.add(subtotal, -discount)
  @azimuth.assert_eq(1094, final_total)  // (299×2 + 199×3) - 10% = 1194 - 119 = 1075
}

test "loan_repayment_calculation" {
  // 贷款还款计算（简化版）
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的单利计算：总还款 = 本金 + 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total_repayment = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total_repayment)  // 10000 + 10000×5×3 = 160000
  
  // 计算月供（简化）
  let monthly_payment = total_repayment / @azimuth.multiply(years, 12)
  @azimuth.assert_eq(4444, monthly_payment)  // 160000 / 36 = 4444 (整数除法)
}

test "array_index_2d_to_1d" {
  // 二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let width = 10
  
  // 计算一维索引：index = row × width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(34, index)  // 3 × 10 + 4 = 34
  
  // 测试边界情况
  let first_index = @azimuth.add(@azimuth.multiply(0, width), 0)
  @azimuth.assert_eq(0, first_index)
  
  let last_index = @azimuth.add(@azimuth.multiply(9, width), 9)
  @azimuth.assert_eq(99, last_index)
}

test "checksum_calculation" {
  // 简单的校验和计算
  let numbers = [12, 34, 56, 78, 90]
  
  // 计算校验和：所有数字相加后取模
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(12, 34), 56), 78), 90)
  let checksum = sum % 256  // 取模256
  @azimuth.assert_eq(16, checksum)  // 270 % 256 = 14
  
  // 测试另一个数据集
  let sum2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  let checksum2 = sum2 % 100
  @azimuth.assert_eq(15, checksum2)  // 15 % 100 = 15
}

test "velocity_distance_calculation" {
  // 物理计算：速度、距离、时间
  let initial_velocity = 10  // m/s
  let acceleration = 2      // m/s²
  let time = 5              // s
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(20, final_velocity)  // 10 + 2×5 = 20
  
  // 计算距离：s = ut + 0.5at²
  // 简化为整数：s = ut + at²/2
  let distance_part1 = @azimuth.multiply(initial_velocity, time)
  let distance_part2 = @azimuth.multiply(@azimuth.multiply(acceleration, time), time) / 2
  let total_distance = @azimuth.add(distance_part1, distance_part2)
  @azimuth.assert_eq(75, total_distance)  // 10×5 + 2×5×5/2 = 50 + 25 = 75
}

test "percentage_change_calculation" {
  // 百分比变化计算
  let old_value = 200
  let new_value = 250
  
  // 计算变化量：new - old
  let change = @azimuth.add(new_value, -old_value)
  @azimuth.assert_eq(50, change)
  
  // 计算百分比变化：(change / old) × 100
  let percentage_change = @azimuth.multiply(change, 100) / old_value
  @azimuth.assert_eq(25, percentage_change)  // 50 × 100 / 200 = 25%
  
  // 测试负变化
  let old_value2 = 300
  let new_value2 = 240
  let change2 = @azimuth.add(new_value2, -old_value2)
  let percentage_change2 = @azimuth.multiply(change2, 100) / old_value2
  @azimuth.assert_eq(-20, percentage_change2)  // -60 × 100 / 300 = -20%
}

test "data_validation_boundary" {
  // 数据验证的边界条件测试
  let max_input = 2147483647
  let min_input = -2147483648
  let valid_range_max = 1000
  let valid_range_min = -1000
  
  // 测试超出范围的处理
  let over_max = if (max_input > valid_range_max) { valid_range_max } else { max_input }
  @azimuth.assert_eq(1000, over_max)
  
  let under_min = if (min_input < valid_range_min) { valid_range_min } else { min_input }
  @azimuth.assert_eq(-1000, under_min)
  
  // 测试边界值本身
  let at_max = if (valid_range_max > valid_range_max) { valid_range_max } else { valid_range_max }
  @azimuth.assert_eq(1000, at_max)
  
  let at_min = if (valid_range_min < valid_range_min) { valid_range_min } else { valid_range_min }
  @azimuth.assert_eq(-1000, at_min)
}

test "string_template_with_variables" {
  // 字符串模板处理与变量替换
  let user_name = "Alice"
  let user_age = "25"
  let user_city = "Beijing"
  
  // 模拟模板处理："Hello, {name}! You are {age} years old and live in {city}."
  let greeting = @azimuth.greet(user_name)
  let age_part = " You are " + user_age + " years old"
  let city_part = " and live in " + user_city + "."
  let full_message = greeting + age_part + city_part
  
  @azimuth.assert_eq_string("Hello, Alice! You are 25 years old and live in Beijing.", full_message)
  
  // 测试空值处理
  let empty_name = ""
  let empty_greeting = @azimuth.greet(empty_name)
  @azimuth.assert_eq_string("Hello, !", empty_greeting)
}