// Azimuth 项目综合新测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "boolean_logic_operations" {
  // 布尔逻辑运算测试
  let true_val = 1
  let false_val = 0
  
  // 逻辑与操作（使用乘法模拟）
  let and_result1 = azimuth.multiply(true_val, true_val)  // 1 && 1 = 1
  let and_result2 = azimuth.multiply(true_val, false_val) // 1 && 0 = 0
  let and_result3 = azimuth.multiply(false_val, false_val) // 0 && 0 = 0
  
  @assertion.assert_eq(1, and_result1)?
  @assertion.assert_eq(0, and_result2)?
  @assertion.assert_eq(0, and_result3)?
  
  // 逻辑或操作（使用加法模拟，结果限制为0或1）
  let or_result1 = azimuth.min(azimuth.add(true_val, true_val), 1)  // 1 || 1 = 1
  let or_result2 = azimuth.min(azimuth.add(true_val, false_val), 1) // 1 || 0 = 1
  let or_result3 = azimuth.min(azimuth.add(false_val, false_val), 1) // 0 || 0 = 0
  
  @assertion.assert_eq(1, or_result1)?
  @assertion.assert_eq(1, or_result2)?
  @assertion.assert_eq(0, or_result3)?
}

test "array_summation_patterns" {
  // 数组求和模式测试
  // 使用累加变量模拟数组求和
  
  // 等差数列求和: 2 + 4 + 6 + 8 + 10 = 30
  let sum1 = azimuth.add(azimuth.add(azimuth.add(azimuth.add(2, 4), 6), 8), 10)
  @assertion.assert_eq(30, sum1)?
  
  // 奇数求和: 1 + 3 + 5 + 7 + 9 = 25
  let sum2 = azimuth.add(azimuth.add(azimuth.add(azimuth.add(1, 3), 5), 7), 9)
  @assertion.assert_eq(25, sum2)?
  
  // 阶乘求和: 1! + 2! + 3! + 4! = 1 + 2 + 6 + 24 = 33
  let fact1 = 1
  let fact2 = azimuth.multiply(2, 1)
  let fact3 = azimuth.multiply(3, azimuth.multiply(2, 1))
  let fact4 = azimuth.multiply(4, azimuth.multiply(3, azimuth.multiply(2, 1)))
  let sum3 = azimuth.add(azimuth.add(azimuth.add(fact1, fact2), fact3), fact4)
  @assertion.assert_eq(33, sum3)?
}

test "energy_consumption_calculations" {
  // 能源消耗计算测试
  let power_rating = 1500  // 设备功率（瓦特）
  let hours_per_day = 8    // 每天使用小时数
  let days_per_month = 30  // 每月天数
  let price_per_kwh = 12   // 每千瓦时价格（分）
  
  // 计算每日耗电量（瓦时）
  let daily_consumption_wh = azimuth.multiply(power_rating, hours_per_day)
  @assertion.assert_eq(12000, daily_consumption_wh)?
  
  // 转换为千瓦时
  let daily_consumption_kwh = daily_consumption_wh / 1000
  @assertion.assert_eq(12, daily_consumption_kwh)?
  
  // 计算每月耗电量
  let monthly_consumption_kwh = azimuth.multiply(daily_consumption_kwh, days_per_month)
  @assertion.assert_eq(360, monthly_consumption_kwh)?
  
  // 计算每月电费
  let monthly_cost = azimuth.multiply(monthly_consumption_kwh, price_per_kwh)
  @assertion.assert_eq(4320, monthly_cost)?
}

test "speed_distance_time_calculations" {
  // 速度、距离、时间计算测试
  let speed = 60  // 速度（公里/小时）
  let time = 2.5  // 时间（小时）
  let distance = 150  // 距离（公里）
  
  // 计算距离：距离 = 速度 × 时间
  let calculated_distance = azimuth.multiply(speed, azimuth.multiply(time, 10)) / 10
  @assertion.assert_eq(150, calculated_distance)?
  
  // 计算时间：时间 = 距离 / 速度
  let calculated_time = azimuth.multiply(distance, 10) / speed
  @assertion.assert_eq(25, calculated_time)?  // 2.5小时 = 25（十分之一小时）
  
  // 计算速度：速度 = 距离 / 时间
  let calculated_speed = azimuth.multiply(distance, 10) / azimuth.multiply(time, 10)
  @assertion.assert_eq(60, calculated_speed)?
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 500
  let part = 125
  let percentage = 25
  let increase = 20
  
  // 计算百分比：百分比 = 部分 / 总数 × 100
  let calculated_percentage = azimuth.multiply(part, 100) / total
  @assertion.assert_eq(25, calculated_percentage)?
  
  // 计算百分比值：值 = 总数 × 百分比 / 100
  let calculated_value = azimuth.multiply(total, percentage) / 100
  @assertion.assert_eq(125, calculated_value)?
  
  // 计算增长后的值：新值 = 原值 × (1 + 增长率/100)
  let increased_value = azimuth.multiply(total, azimuth.add(100, increase)) / 100
  @assertion.assert_eq(600, increased_value)?
  
  // 计算减少后的值：新值 = 原值 × (1 - 减少率/100)
  let decreased_value = azimuth.multiply(total, azimuth.add(100, -increase)) / 100
  @assertion.assert_eq(400, decreased_value)?
}

test "unit_conversion_calculations" {
  // 单位转换计算测试
  
  // 长度转换：1米 = 100厘米 = 1000毫米
  let meters = 5
  let centimeters = azimuth.multiply(meters, 100)
  let millimeters = azimuth.multiply(meters, 1000)
  
  @assertion.assert_eq(500, centimeters)?
  @assertion.assert_eq(5000, millimeters)?
  
  // 重量转换：1千克 = 1000克
  let kilograms = 3.5
  let grams = azimuth.multiply(kilograms, 1000)
  @assertion.assert_eq(3500, grams)?
  
  // 数据存储转换：1GB = 1024MB
  let gigabytes = 2
  let megabytes = azimuth.multiply(gigabytes, 1024)
  @assertion.assert_eq(2048, megabytes)?
}

test "loan_repayment_calculations" {
  // 贷款还款计算测试
  let principal = 10000  // 本金
  let annual_rate = 6    // 年利率（百分比）
  let loan_term = 3      // 贷款期限（年）
  
  // 计算总利息：利息 = 本金 × 利率 × 年数
  let total_interest = azimuth.multiply(principal, azimuth.multiply(annual_rate, loan_term)) / 100
  @assertion.assert_eq(1800, total_interest)?
  
  // 计算还款总额：总额 = 本金 + 利息
  let total_repayment = azimuth.add(principal, total_interest)
  @assertion.assert_eq(11800, total_repayment)?
  
  // 计算月还款额：月还款 = 总额 / (年数 × 12)
  let monthly_payment = azimuth.multiply(total_repayment, 100) / azimuth.multiply(loan_term, 12)
  @assertion.assert_eq(327, monthly_payment)?  // 向下取整
  
  // 计算总还款月数
  let total_months = azimuth.multiply(loan_term, 12)
  @assertion.assert_eq(36, total_months)?
}

test "inventory_management_calculations" {
  // 库存管理计算测试
  let initial_stock = 500
  let units_sold = 120
  let units_received = 200
  let units_returned = 15
  let units_damaged = 5
  
  // 计算当前库存：当前库存 = 初始库存 - 售出 + 收到 - 退货 - 损坏
  let current_stock = azimuth.add(azimuth.add(azimuth.subtract(
    azimuth.subtract(initial_stock, units_sold), units_returned), units_received), -units_damaged)
  @assertion.assert_eq(560, current_stock)?
  
  // 计算库存周转率：周转率 = 售出 / 平均库存
  let average_stock = azimuth.add(initial_stock, current_stock) / 2
  let turnover_rate = azimuth.multiply(units_sold, 100) / average_stock
  @assertion.assert_eq(23, turnover_rate)?  // 向下取整
  
  // 计算库存价值（假设每单位成本50）
  let unit_cost = 50
  let inventory_value = azimuth.multiply(current_stock, unit_cost)
  @assertion.assert_eq(28000, inventory_value)?
}

test "data_validation_patterns" {
  // 数据验证模式测试
  let min_value = 10
  let max_value = 100
  let input1 = 5    // 低于最小值
  let input2 = 50   // 在范围内
  let input3 = 150  // 高于最大值
  
  // 验证输入是否在范围内
  // 使用min和max函数限制值在范围内
  let validated1 = azimuth.max(min_value, azimuth.min(max_value, input1))
  let validated2 = azimuth.max(min_value, azimuth.min(max_value, input2))
  let validated3 = azimuth.max(min_value, azimuth.min(max_value, input3))
  
  @assertion.assert_eq(10, validated1)?   // 被限制为最小值
  @assertion.assert_eq(50, validated2)?   // 保持原值
  @assertion.assert_eq(100, validated3)?  // 被限制为最大值
  
  // 计算验证偏差
  let deviation1 = azimuth.add(input1, -validated1)
  let deviation2 = azimuth.add(input2, -validated2)
  let deviation3 = azimuth.add(input3, -validated3)
  
  @assertion.assert_eq(-5, deviation1)?
  @assertion.assert_eq(0, deviation2)?
  @assertion.assert_eq(50, deviation3)?
}

test "cryptographic_hash_simulation" {
  // 加密哈希模拟测试（简化版）
  let message = 12345
  let prime1 = 17
  let prime2 = 31
  let prime3 = 53
  
  // 简单的哈希算法：哈希 = (消息 × 质数1 + 质数2) × 质数3
  let hash1 = azimuth.multiply(azimuth.add(azimuth.multiply(message, prime1), prime2), prime3)
  @assertion.assert_eq(11074875, hash1)?
  
  // 测试不同消息产生不同哈希
  let message2 = 12346
  let hash2 = azimuth.multiply(azimuth.add(azimuth.multiply(message2, prime1), prime2), prime3)
  @assertion.assert_eq(11074942, hash2)?
  
  // 验证哈希值不同
  let hash_difference = azimuth.add(hash2, -hash1)
  @assertion.assert_eq(67, hash_difference)?
  
  // 测试哈希值的模运算（用于创建固定长度的哈希）
  let mod_value = 1000000
  let normalized_hash1 = hash1 % mod_value
  let normalized_hash2 = hash2 % mod_value
  
  @assertion.assert_eq(74875, normalized_hash1)?
  @assertion.assert_eq(74942, normalized_hash2)?
}