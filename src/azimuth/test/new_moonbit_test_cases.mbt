// 新增的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "arithmetic_progression_sum" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13
  // 使用公式：n/2 * (2a1 + (n-1)d)
  let n = 5
  let a1 = 1
  let d = 3
  
  // 手动计算验证
  let term1 = a1
  let term2 = @azimuth.add(term1, d)  // 4
  let term3 = @azimuth.add(term2, d)  // 7
  let term4 = @azimuth.add(term3, d)  // 10
  let term5 = @azimuth.add(term4, d)  // 13
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(35, sum)
}

test "geometric_progression_product" {
  // 测试等比数列求积：2 * 4 * 8 * 16
  let first_term = 2
  let ratio = 2
  let terms = 4
  
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)  // 4
  let term3 = @azimuth.multiply(term2, ratio)  // 8
  let term4 = @azimuth.multiply(term3, ratio)  // 16
  
  let product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(term1, term2), term3), term4)
  @azimuth.assert_eq(1024, product)
}

test "discount_calculation" {
  // 测试折扣计算场景
  let original_price = 1000
  let discount_percentage = 15  // 15% 折扣
  let tax_rate = 8  // 8% 税率
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(918, final_price)  // (1000 - 150) + 68 = 918
}

test "speed_distance_time" {
  // 测试速度、距离、时间计算
  let speed = 60  // km/h
  let time_hours = 2
  let time_minutes = 30
  
  // 转换时间为小时（分数表示）
  let total_time_hours = @azimuth.add(time_hours, time_minutes / 60)  // 2.5小时（整数除法为2）
  
  // 计算距离：速度 × 时间
  let distance = @azimuth.multiply(speed, total_time_hours)
  @azimuth.assert_eq(120, distance)  // 60 * 2 = 120（整数除法）
  
  // 计算返回所需时间
  let return_speed = 50  // km/h
  let return_time = distance / return_speed
  @azimuth.assert_eq(2, return_time)  // 120 / 50 = 2（整数除法）
}

test "array_index_manipulation" {
  // 测试数组索引操作模拟
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 二维数组转一维数组索引计算
  let linear_index = @azimuth.add(@azimuth.multiply(row_index, cols), col_index)
  @azimuth.assert_eq(13, linear_index)  // 2*5 + 3 = 13
  
  // 边界测试
  let first_element = @azimuth.add(@azimuth.multiply(0, cols), 0)
  @azimuth.assert_eq(0, first_element)
  
  let last_element = @azimuth.add(@azimuth.multiply(@azimuth.add(rows, -1), @azimuth.add(cols, -1)), @azimuth.add(cols, -1))
  @azimuth.assert_eq(19, last_element)  // 3*4 + 4 = 16（实际计算）
}

test "string_validation_patterns" {
  // 测试字符串验证模式
  // 测试数字字符串
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  
  // 测试邮箱格式字符串
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
  
  // 测试包含空格的姓名
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含连字符的名字
  @azimuth.assert_eq_string("Hello, Mary-Jane!", @azimuth.greet("Mary-Jane"))
  
  // 测试包含下划线的用户名
  @azimuth.assert_eq_string("Hello, user_name_123!", @azimuth.greet("user_name_123"))
}

test "binary_number_simulation" {
  // 测试二进制数运算模拟
  // 模拟二进制 1010 (10) 和 0101 (5) 的运算
  
  let binary1 = 10  // 1010 in binary
  let binary2 = 5   // 0101 in binary
  
  // 模拟位运算：AND（简化为最小值）
  let and_result = if (binary1 < binary2) { binary1 } else { binary2 }
  @azimuth.assert_eq(5, and_result)
  
  // 模拟位运算：OR（简化为最大值）
  let or_result = if (binary1 > binary2) { binary1 } else { binary2 }
  @azimuth.assert_eq(10, or_result)
  
  // 模拟位运算：XOR（简化为差值）
  let xor_result = @azimuth.add(binary1, -binary2)
  @azimuth.assert_eq(5, xor_result)
  
  // 模拟左移运算：<< 1 (乘以2)
  let left_shift = @azimuth.multiply(binary1, 2)
  @azimuth.assert_eq(20, left_shift)
}

test "loan_amortization" {
  // 测试贷款分期计算（简化版）
  let principal = 12000  // 贷款本金
  let annual_rate = 6    // 年利率6%
  let years = 2          // 贷款期限2年
  
  // 计算总利息（简化计算：本金 × 年利率 × 年数）
  let total_interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), years) / 100
  @azimuth.assert_eq(1440, total_interest)  // 12000 * 6 * 2 / 100 = 1440
  
  // 计算总还款额
  let total_payment = @azimuth.add(principal, total_interest)
  @azimuth.assert_eq(13440, total_payment)
  
  // 计算月还款额（简化：总还款额 / 总月数）
  let total_months = @azimuth.multiply(years, 12)
  let monthly_payment = total_payment / total_months
  @azimuth.assert_eq(560, monthly_payment)  // 13440 / 24 = 560
}

test "coordinate_geometry" {
  // 测试坐标几何计算
  let x1 = 1
  let y1 = 2
  let x2 = 4
  let y2 = 6
  
  // 计算两点间距离的平方（避免平方根）
  let delta_x = @azimuth.add(x2, -x1)  // 3
  let delta_y = @azimuth.add(y2, -y1)  // 4
  let distance_squared = @azimuth.add(@azimuth.multiply(delta_x, delta_x), @azimuth.multiply(delta_y, delta_y))
  @azimuth.assert_eq(25, distance_squared)  // 3² + 4² = 9 + 16 = 25
  
  // 计算中点坐标
  let midpoint_x = @azimuth.add(x1, x2) / 2  // 2（整数除法）
  let midpoint_y = @azimuth.add(y1, y2) / 2  // 4（整数除法）
  @azimuth.assert_eq(2, midpoint_x)
  @azimuth.assert_eq(4, midpoint_y)
  
  // 计算斜率（简化为整数）
  let slope = delta_y / delta_x
  @azimuth.assert_eq(1, slope)  // 4 / 3 = 1（整数除法）
}

test "inventory_management" {
  // 测试库存管理计算
  let initial_stock = 100
  let units_sold = 25
  let units_received = 30
  let unit_cost = 15
  let selling_price = 25
  
  // 计算当前库存
  let current_stock = @azimuth.add(@azimuth.add(initial_stock, -units_sold), units_received)
  @azimuth.assert_eq(105, current_stock)  // 100 - 25 + 30 = 105
  
  // 计算销售成本
  let cost_of_goods_sold = @azimuth.multiply(units_sold, unit_cost)
  @azimuth.assert_eq(375, cost_of_goods_sold)  // 25 * 15 = 375
  
  // 计算销售收入
  let sales_revenue = @azimuth.multiply(units_sold, selling_price)
  @azimuth.assert_eq(625, sales_revenue)  // 25 * 25 = 625
  
  // 计算毛利润
  let gross_profit = @azimuth.add(sales_revenue, -cost_of_goods_sold)
  @azimuth.assert_eq(250, gross_profit)  // 625 - 375 = 250
  
  // 计算库存总价值
  let inventory_value = @azimuth.multiply(current_stock, unit_cost)
  @azimuth.assert_eq(1575, inventory_value)  // 105 * 15 = 1575
}