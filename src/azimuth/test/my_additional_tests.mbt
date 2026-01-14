// 额外的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_large_numbers_with_overflow" {
  // 测试大数相加的溢出处理
  let max_val = 2147483647
  
  // 测试接近最大值的相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1000))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000000, 1000000))
  
  // 测试最小值附近的相加
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000, -1000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000000, -1000000))
}

test "multiply_with_decimal_simulation" {
  // 通过整数运算模拟小数乘法（例如：计算百分比和折扣）
  let original_price = 299
  let discount_rate = 15  // 15% 折扣
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  @azimuth.assert_eq(44, discount_amount)  // 299 * 15 / 100 = 44.85 ≈ 44（整数除法）
  
  // 计算折后价格
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(255, final_price)  // 299 - 44 = 255
}

test "greet_with_unicode_and_emoji" {
  // 测试包含 Unicode 字符和 emoji 的字符串处理
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 👨‍💻!", @azimuth.greet("👨‍💻"))
  @azimuth.assert_eq_string("Hello, 🎉!", @azimuth.greet("🎉"))
  
  // 测试混合字符串
  @azimuth.assert_eq_string("Hello, MoonBit🌙!", @azimuth.greet("MoonBit🌙"))
  @azimuth.assert_eq_string("Hello, 你好世界🌍!", @azimuth.greet("你好世界🌍"))
}

test "complex_financial_calculation" {
  // 复杂的金融计算场景：贷款计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率 6%
  let years = 10          // 贷款年限
  
  // 简化的单利计算：总利息 = 本金 × 利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years)) / 100
  @azimuth.assert_eq(60000, total_interest)  // 100000 × 6 × 10 / 100 = 60000
  
  // 计算总还款额
  let total_payment = @azimuth.add(principal, total_interest)
  @azimuth.assert_eq(160000, total_payment)
  
  // 计算月供（简化）
  let monthly_payment = total_payment / @azimuth.multiply(years, 12)
  @azimuth.assert_eq(1333, monthly_payment)  // 160000 / 120 ≈ 1333
}

test "temperature_conversion_comprehensive" {
  // 全面的温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32（简化为整数运算：F = C × 2 + 30）
  
  // 常见温度点
  let celsius_temps = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 110, 230]
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试室温
  let room_c = 20
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  @azimuth.assert_eq(70, room_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

test "shopping_cart_with_tax_and_shipping" {
  // 购物车计算，包含税费和运费
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let tax_rate = 8      // 8% 税费
  let shipping_fee = 15 // 运费
  
  // 计算商品总价
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)  // 297
  let subtotal = @azimuth.add(subtotal1, subtotal2)              // 695
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100  // 695 * 8 / 100 = 55.6 ≈ 55
  @azimuth.assert_eq(55, tax)
  
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping_fee)  // 695 + 55 + 15 = 765
  @azimuth.assert_eq(765, total)
}

test "statistical_analysis" {
  // 统计分析测试
  // 数据集：[15, 25, 35, 45, 55, 65, 75, 85, 95, 105]
  let data_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  let data_sum2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(65, 75), 85), 95), 105)
  let total_sum = @azimuth.add(data_sum, data_sum2)
  
  // 计算平均值
  let count = 10
  let mean = total_sum / count
  @azimuth.assert_eq(60, mean)
  
  // 计算中位数（偶数个数据，取中间两个的平均）
  let median = @azimuth.add(55, 65) / 2
  @azimuth.assert_eq(60, median)
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(105, -15)
  @azimuth.assert_eq(90, range)
}

test "physics_projectile_motion" {
  // 物理抛物运动计算（简化版）
  let initial_velocity = 20  // m/s
  let angle = 45             // 度
  let gravity = 10           // m/s²（简化值）
  
  // 水平速度分量（简化：v × cos(45°) ≈ v × 0.7 ≈ v × 7 / 10）
  let vx = @azimuth.multiply(initial_velocity, 7) / 10
  @azimuth.assert_eq(14, vx)  // 20 * 7 / 10 = 14
  
  // 垂直速度分量（简化：v × sin(45°) ≈ v × 0.7 ≈ v × 7 / 10）
  let vy = @azimuth.multiply(initial_velocity, 7) / 10
  @azimuth.assert_eq(14, vy)  // 20 * 7 / 10 = 14
  
  // 飞行时间（简化：2 × vy / g）
  let flight_time = @azimuth.multiply(2, vy) / gravity
  @azimuth.assert_eq(2, flight_time)  // 2 * 14 / 10 = 2.8 ≈ 2
  
  // 水平距离
  let horizontal_distance = @azimuth.multiply(vx, flight_time)
  @azimuth.assert_eq(28, horizontal_distance)  // 14 * 2 = 28
}

test "data_structure_operations" {
  // 数据结构操作模拟
  // 模拟数组索引计算：二维数组转一维数组
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 计算一维数组索引：index = row × cols + col
  let index = @azimuth.add(@azimuth.multiply(row_index, cols), col_index)
  @azimuth.assert_eq(13, index)  // 2 × 5 + 3 = 13
  
  // 模拟栈操作：压入和弹出
  let stack_size = 0
  let push_count = 5
  let pop_count = 2
  
  // 压入操作
  let after_push = @azimuth.add(stack_size, push_count)
  @azimuth.assert_eq(5, after_push)
  
  // 弹出操作
  let after_pop = @azimuth.add(after_push, -pop_count)
  @azimuth.assert_eq(3, after_pop)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let n = 100  // 数据规模
  
  // O(1) 常数时间复杂度
  let constant_time_ops = 1
  @azimuth.assert_eq(1, constant_time_ops)
  
  // O(log n) 对数时间复杂度（简化计算）
  let log_time_ops = 7  // log₂(100) ≈ 6.64 ≈ 7
  @azimuth.assert_eq(7, log_time_ops)
  
  // O(n) 线性时间复杂度
  let linear_time_ops = n
  @azimuth.assert_eq(100, linear_time_ops)
  
  // O(n log n) 时间复杂度
  let n_log_n_ops = @azimuth.multiply(n, log_time_ops)
  @azimuth.assert_eq(700, n_log_n_ops)
  
  // O(n²) 平方时间复杂度
  let quadratic_ops = @azimuth.multiply(n, n)
  @azimuth.assert_eq(10000, quadratic_ops)
}