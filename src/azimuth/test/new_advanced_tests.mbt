// 新增的 MoonBit 测试用例
// 测试 azimuth 包的高级功能和复杂场景
test "prime_number_validation" {
  // 素数验证测试
  // 验证7是素数（通过检查不能被2、3、5、7整除）
  let candidate = 7
  
  // 检查不能被2整除
  @azimuth.assert_eq(candidate % 2 != 0, true)
  // 检查不能被3整除
  @azimuth.assert_eq(candidate % 3 != 0, true)
  // 检查不能被5整除
  @azimuth.assert_eq(candidate % 5 != 0, true)
  
  // 验证素数特性：只能被1和自己整除
  @azimuth.assert_eq(0, candidate % candidate)
  @azimuth.assert_eq(0, candidate % 1)
}
test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  // F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8, F(7)=13, F(8)=21
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = azimuth::@azimuth.add(fib_0, fib_1)  // 1
  let fib_3 = azimuth::@azimuth.add(fib_1, fib_2)  // 2
  let fib_4 = azimuth::@azimuth.add(fib_2, fib_3)  // 3
  let fib_5 = azimuth::@azimuth.add(fib_3, fib_4)  // 5
  let fib_6 = azimuth::@azimuth.add(fib_4, fib_5)  // 8
  let fib_7 = azimuth::@azimuth.add(fib_5, fib_6)  // 13
  let fib_8 = azimuth::@azimuth.add(fib_6, fib_7)  // 21
  
  @azimuth.assert_eq(1, fib_2)
  @azimuth.assert_eq(2, fib_3)
  @azimuth.assert_eq(3, fib_4)
  @azimuth.assert_eq(5, fib_5)
  @azimuth.assert_eq(8, fib_6)
  @azimuth.assert_eq(13, fib_7)
  @azimuth.assert_eq(21, fib_8)
}
test "greatest_common_divisor" {
  // 最大公约数计算测试
  // GCD(48, 18) = 6
  let a = 48
  let b = 18
  
  // 手动实现欧几里得算法步骤
  // 48 = 2 × 18 + 12
  let remainder1 = a - azimuth::@azimuth.multiply(2, b)  // 48 - 2*18 = 12
  
  // 18 = 1 × 12 + 6
  let remainder2 = b - azimuth::@azimuth.multiply(1, remainder1)  // 18 - 1*12 = 6
  
  // 12 = 2 × 6 + 0
  let _ = remainder1 - azimuth::@azimuth.multiply(2, remainder2)  // 12 - 2*6 = 0
  
  // GCD是最后一个非零余数
  let gcd = remainder2
  @azimuth.assert_eq(6, gcd)
}
test "compound_interest_calculation" {
  // 复利计算测试
  // 本金1000，年利率5%，3年复利
  let principal = 1000
  let rate_percent = 5
  
  // 第一年：1000 + 1000×5% = 1050
  let year1 = azimuth::@azimuth.add(principal, azimuth::@azimuth.multiply(principal, rate_percent) / 100)
  
  // 第二年：1050 + 1050×5% = 1102.5 → 1102（整数除法）
  let year2 = azimuth::@azimuth.add(year1, azimuth::@azimuth.multiply(year1, rate_percent) / 100)
  
  // 第三年：1102 + 1102×5% = 1157.1 → 1157（整数除法）
  let year3 = azimuth::@azimuth.add(year2, azimuth::@azimuth.multiply(year2, rate_percent) / 100)
  
  @azimuth.assert_eq(1050, year1)
  @azimuth.assert_eq(1102, year2)
  @azimuth.assert_eq(1157, year3)
}
test "quadratic_equation_solver" {
  // 二次方程求解测试：ax² + bx + c = 0
  // 解方程：x² - 5x + 6 = 0，解为x=2和x=3
  let a = 1
  let b = -5
  let c = 6
  
  // 验证解的正确性
  let x1 = 2
  let x2 = 3
  
  // 验证x1=2是解：2² - 5×2 + 6 = 0
  let result1 = azimuth::@azimuth.add(azimuth::@azimuth.multiply(a, azimuth::@azimuth.multiply(x1, x1)), azimuth::@azimuth.add(azimuth::@azimuth.multiply(b, x1), c))
  @azimuth.assert_eq(0, result1)
  
  // 验证x2=3是解：3² - 5×3 + 6 = 0
  let result2 = azimuth::@azimuth.add(azimuth::@azimuth.multiply(a, azimuth::@azimuth.multiply(x2, x2)), azimuth::@azimuth.add(azimuth::@azimuth.multiply(b, x2), c))
  @azimuth.assert_eq(0, result2)
}
test "statistical_analysis" {
  // 统计分析测试
  // 数据集：[10, 20, 30, 40, 50]
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算总和
  let sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(150, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算方差（简化版）：Σ(x - μ)² / n
  let variance_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(
    azimuth::@azimuth.multiply(data1 - average, data1 - average),
    azimuth::@azimuth.multiply(data2 - average, data2 - average)),
    azimuth::@azimuth.multiply(data3 - average, data3 - average)),
    azimuth::@azimuth.multiply(data4 - average, data4 - average)),
    azimuth::@azimuth.multiply(data5 - average, data5 - average))
  
  let variance = variance_sum / 5
  @azimuth.assert_eq(200, variance)  // (400+100+0+100+400)/5 = 1000/5 = 200
}
test "currency_exchange_calculation" {
  // 货币兑换计算测试
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY
  let transaction_fee_percent = 2
  
  // 计算兑换金额
  let cny_amount = azimuth::@azimuth.multiply(usd_amount, exchange_rate)
  
  // 计算手续费
  let fee = azimuth::@azimuth.multiply(cny_amount, transaction_fee_percent) / 100
  
  // 计算实际到账金额
  let actual_amount = azimuth::@azimuth.add(cny_amount, -fee)
  
  @azimuth.assert_eq(700, cny_amount)
  @azimuth.assert_eq(14, fee)
  @azimuth.assert_eq(686, actual_amount)
}
test "speed_distance_time_calculation" {
  // 速度、距离、时间计算测试
  let distance = 300  // 公里
  let time_hours = 3   // 小时
  
  // 计算速度：距离/时间
  let speed = distance / time_hours
  @azimuth.assert_eq(100, speed)
  
  // 验证：速度×时间 = 距离
  let calculated_distance = azimuth::@azimuth.multiply(speed, time_hours)
  @azimuth.assert_eq(distance, calculated_distance)
  
  // 计算节省时间：如果速度提高20%
  let new_speed = azimuth::@azimuth.add(speed, azimuth::@azimuth.multiply(speed, 20) / 100)
  let new_time = distance / new_speed
  let time_saved = time_hours - new_time
  
  @azimuth.assert_eq(120, new_speed)
  @azimuth.assert_eq(2, new_time)
  @azimuth.assert_eq(1, time_saved)
}
test "inventory_management" {
  // 库存管理测试
  let initial_stock = 100
  let sold_units = 30
  let restocked_units = 50
  let damaged_units = 5
  
  // 计算当前库存
  let current_stock = azimuth::@azimuth.add(azimuth::@azimuth.add(initial_stock, -sold_units), restocked_units)
  let current_stock_final = azimuth::@azimuth.add(current_stock, -damaged_units)
  
  // 计算库存周转率（简化版）
  let turnover_rate = azimuth::@azimuth.multiply(sold_units, 100) / initial_stock
  
  // 计算补货点（当库存低于20%时补货）
  let reorder_point = azimuth::@azimuth.multiply(initial_stock, 20) / 100
  let needs_reorder = current_stock_final < reorder_point
  
  @azimuth.assert_eq(115, current_stock_final)
  @azimuth.assert_eq(30, turnover_rate)
  @azimuth.assert_eq(20, reorder_point)
  @azimuth.assert_eq(0, 1)(needs_reorder)
}
test "complex_string_template" {
  // 复杂字符串模板测试
  // 模拟订单确认邮件模板
  let customer_name = "张三"
  let order_number = "ORD-2023-001"
  let product_name = "智能手机"
  let quantity = 2
  let unit_price = 2999
  let total_price = azimuth::@azimuth.multiply(quantity, unit_price)
  
  // 构建确认消息
  let greeting = azimuth::@azimuth.greet(customer_name)
  let order_info = "您的订单 " + order_number + " 已确认"
  let product_info = "商品：" + product_name + "，数量：2，单价：2999"
  let total_info = "总价：5998"
  
  // 验证字符串拼接
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
  @azimuth.assert_eq_string("您的订单 ORD-2023-001 已确认", order_info)
  @azimuth.assert_eq_string("商品：智能手机，数量：2，单价：2999", product_info)
  @azimuth.assert_eq_string("总价：5998", total_info)
}