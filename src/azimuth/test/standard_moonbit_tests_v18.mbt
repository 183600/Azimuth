// 标准 MoonBit 测试用例 v18
// 为 azimuth 库添加的10个新测试用例

test "integer_division_simulation" {
  // 模拟整数除法运算，通过乘法和减法实现
  let dividend = 27
  let divisor = 5
  
  // 计算商和余数
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  // 验证结果：27 = 5 * 5 + 2
  @azimuth.assert_eq(5, quotient)
  @azimuth.assert_eq(2, remainder)
  
  // 验证除法恒等式：dividend = divisor * quotient + remainder
  let reconstructed = @azimuth.add(@azimuth.multiply(divisor, quotient), remainder)
  @azimuth.assert_eq(dividend, reconstructed)
}

test "percentage_calculation_practical" {
  // 实际百分比计算场景
  let original_price = 200
  let discount_percent = 15
  let tax_percent = 8
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_percent) / 100
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  // 验证结果：200 - 15% + 8% = 200 - 30 + 13.6 ≈ 183.6，取整为183
  @azimuth.assert_eq(183, final_price)
}

test "temperature_conversion_formula" {
  // 温度转换公式测试（精确版）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数运算：F = C × 9 + 160) / 5
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9), 160) / 5
  @azimuth.assert_eq(77, fahrenheit)  // (25 × 9 + 160) / 5 = 385 / 5 = 77
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 9), 160) / 5
  @azimuth.assert_eq(32, freezing_fahrenheit)  // (0 × 9 + 160) / 5 = 160 / 5 = 32
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 9), 160) / 5
  @azimuth.assert_eq(212, boiling_fahrenheit)  // (100 × 9 + 160) / 5 = 1060 / 5 = 212
}

test "compound_interest_precise_calculation" {
  // 精确复利计算（整数版本）
  // 公式：A = P × (1 + r/n)^(n×t)
  // 简化为整数：A = P × (100 + r)^t / 100^t
  
  let principal = 10000  // 本金
  let rate = 5           // 年利率5%
  let times = 3          // 3年
  
  // 计算增长因子：(100 + 5)^3 = 105^3
  let growth_factor = @azimuth.multiply(@azimuth.multiply(105, 105), 105)
  let total = @azimuth.multiply(principal, growth_factor) / @azimuth.multiply(@azimuth.multiply(100, 100), 100)
  
  @azimuth.assert_eq(11576, total)  // 10000 × 1157625 / 1000000 ≈ 11576
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  let first_name = "John"
  let last_name = "Doe"
  let age = "30"
  
  // 测试多部分字符串拼接
  let full_greeting = @azimuth.greet(first_name + " " + last_name + " (" + age + ")")
  @azimuth.assert_eq_string("Hello, John Doe (30)!", full_greeting)
  
  // 测试包含特殊字符的复杂字符串
  let complex_string = "User@2023#Project$Alpha"
  let complex_greeting = @azimuth.greet(complex_string)
  @azimuth.assert_eq_string("Hello, User@2023#Project$Alpha!", complex_greeting)
  
  // 测试Unicode字符组合
  let unicode_string = "🌍🚀MoonBit"
  let unicode_greeting = @azimuth.greet(unicode_string)
  @azimuth.assert_eq_string("Hello, 🌍🚀MoonBit!", unicode_greeting)
}

test "mathematical_series_summation" {
  // 数学级数求和测试
  // 计算等差数列：2 + 5 + 8 + ... + 29 (10项)
  // 公式：S = n × (2a + (n-1)d) / 2
  
  let n = 10          // 项数
  let a = 2           // 首项
  let d = 3           // 公差
  
  let sum = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, a), @azimuth.multiply(n - 1, d))) / 2
  @azimuth.assert_eq(155, sum)  // 10 × (4 + 27) / 2 = 155
  
  // 计算等比数列：3 + 6 + 12 + 24 + 48 (5项)
  // 公式：S = a × (r^n - 1) / (r - 1)
  let geo_a = 3       // 首项
  let r = 2           // 公比
  let geo_n = 5       // 项数
  
  let geo_sum = @azimuth.multiply(geo_a, @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2) - 1, 1)) / (r - 1)
  @azimuth.assert_eq(93, geo_sum)  // 3 × (32 - 1) / 1 = 93
}

test "business_logic_calculation" {
  // 业务逻辑计算测试
  let unit_price = 50
  let quantity = 15
  let bulk_discount_threshold = 10
  let bulk_discount_rate = 10
  let loyalty_discount = 5
  let shipping_cost = 25
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算批量折扣
  let bulk_discount = if (quantity > bulk_discount_threshold) {
    @azimuth.multiply(subtotal, bulk_discount_rate) / 100
  } else {
    0
  }
  
  // 计算折扣后价格
  let after_bulk_discount = @azimuth.add(subtotal, -bulk_discount)
  
  // 计算忠诚度折扣
  let loyalty_discount_amount = @azimuth.multiply(after_bulk_discount, loyalty_discount) / 100
  let after_loyalty_discount = @azimuth.add(after_bulk_discount, -loyalty_discount_amount)
  
  // 计算最终价格
  let final_price = @azimuth.add(after_loyalty_discount, shipping_cost)
  
  @azimuth.assert_eq(747, final_price)  // (750 - 75 - 33) + 25 = 747
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let n = 100
  
  // 模拟O(n)复杂度：线性求和
  let linear_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2  // 1 + 2 + ... + n
  @azimuth.assert_eq(5050, linear_sum)
  
  // 模拟O(n²)复杂度：平方和
  let square_sum = @azimuth.multiply(n, @azimuth.multiply(@azimuth.add(n, 1), @azimuth.add(@azimuth.multiply(2, n), 1))) / 6
  @azimuth.assert_eq(338350, square_sum)
  
  // 模拟O(log n)复杂度：对数运算（简化）
  let log_approx = 0
  let temp = n
  let log_result = if (temp >= 128) { 7 } else if (temp >= 64) { 6 } else if (temp >= 32) { 5 } else if (temp >= 16) { 4 } else if (temp >= 8) { 3 } else if (temp >= 4) { 2 } else if (temp >= 2) { 1 } else { 0 }
  @azimuth.assert_eq(6, log_result)  // log₂(100) ≈ 6.64，向下取整为6
}

test "data_structure_operations" {
  // 数据结构操作模拟测试
  // 模拟栈操作：push和pop
  let stack_size = 10
  let push_operations = 5
  let pop_operations = 3
  
  let final_stack_size = @azimuth.add(stack_size, push_operations - pop_operations)
  @azimuth.assert_eq(12, final_stack_size)
  
  // 模拟队列操作：enqueue和dequeue
  let queue_size = 8
  let enqueue_operations = 6
  let dequeue_operations = 4
  
  let final_queue_size = @azimuth.add(queue_size, enqueue_operations - dequeue_operations)
  @azimuth.assert_eq(10, final_queue_size)
  
  // 模拟哈希表负载因子计算
  let table_capacity = 16
  let occupied_slots = 12
  let load_factor = @azimuth.multiply(occupied_slots, 100) / table_capacity
  @azimuth.assert_eq(75, load_factor)  // 12/16 × 100% = 75%
}

test "scientific_calculation_scenario" {
  // 科学计算场景测试
  // 计算自由落体距离：s = 0.5 × g × t²
  // 使用整数运算：s = g × t² / 2
  
  let gravity = 98  // 9.8 m/s² 放大10倍
  let time = 5      // 5秒
  
  let distance = @azimuth.multiply(gravity, @azimuth.multiply(time, time)) / 20  // 除以20因为gravity放大了10倍且公式中有0.5
  @azimuth.assert_eq(122, distance)  // 98 × 25 / 20 = 122.5，取整为122
  
  // 计算动能：E = 0.5 × m × v²
  let mass = 10      // 10kg
  let velocity = 15  // 15m/s
  
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(1125, kinetic_energy)  // 10 × 225 / 2 = 1125
  
  // 计算势能：E = m × g × h
  let height = 20    // 20米
  
  let potential_energy = @azimuth.multiply(mass, @azimuth.multiply(gravity, height)) / 10  // 除以10因为gravity放大了10倍
  @azimuth.assert_eq(1960, potential_energy)  // 10 × 98 × 20 / 10 = 1960
}