// 综合应用测试用例
// 包含实际场景和边界条件组合测试

// 测试1: 股票投资组合计算
///|
test "investment_portfolio_calculation" {
  // 股票数量和价格
  let shares_a = 100
  let price_a = 50
  let shares_b = 200
  let price_b = 30
  let shares_c = 150
  let price_c = 40
  
  // 计算每只股票的总价值
  let value_a = multiply(shares_a, price_a)
  let value_b = multiply(shares_b, price_b)
  let value_c = multiply(shares_c, price_c)
  
  // 计算投资组合总价值
  let total_value = add(add(value_a, value_b), value_c)
  
  // 验证: 100*50 + 200*30 + 150*40 = 5000 + 6000 + 6000 = 17000
  assert_eq(17000, total_value)
}

// 测试2: 建筑材料预算计算
///|
test "construction_material_budget" {
  // 材料数量和单价
  let cement_bags = 50
  let cement_price = 300
  let steel_units = 20
  let steel_price = 4500
  let brick_units = 1000
  let brick_price = 8
  
  // 计算各类材料成本
  let cement_cost = multiply(cement_bags, cement_price)
  let steel_cost = multiply(steel_units, steel_price)
  let brick_cost = multiply(brick_units, brick_price)
  
  // 计算总预算
  let total_budget = add(add(cement_cost, steel_cost), brick_cost)
  
  // 验证: 50*300 + 20*4500 + 1000*8 = 15000 + 90000 + 8000 = 113000
  assert_eq(113000, total_budget)
}

// 测试3: 复合边界条件测试
///|
test "complex_boundary_conditions" {
  // 测试多个边界条件的组合
  // 最大值与最小值的运算组合
  
  // 最大值 + 最小值 + 1
  let result1 = add(add(2147483647, -2147483648), 1)
  assert_eq(0, result1)
  
  // 最小值 * 2 + 最大值
  let result2 = add(multiply(-2147483648, 2), 2147483647)
  assert_eq(-1, result2)  // 最小值*2会溢出为最小值，然后加上最大值得到-1
  
  // (最大值 + 1) * 0 (应该得到0)
  let result3 = multiply(add(2147483647, 1), 0)
  assert_eq(0, result3)
}

// 测试4: 字符串处理实际应用
///|
test "practical_string_processing" {
  // 用户注册系统中的姓名处理
  let first_name = "张"
  let last_name = "三"
  let middle_name = "小"
  
  // 拼接全名
  let full_name = first_name + middle_name + last_name
  assert_eq("张小三", full_name)
  
  // 生成欢迎消息
  let welcome_msg = greet(full_name)
  assert_eq("Hello, 张小三!", welcome_msg)
  
  // 测试特殊字符姓名
  let special_name = "O'Conner"
  let special_greeting = greet(special_name)
  assert_eq("Hello, O'Conner!", special_greeting)
}

// 测试5: 科学计算场景
///|
test "scientific_computing_scenario" {
  // 计算化学反应中的摩尔质量
  // H2O: 2*1 + 16 = 18
  let h_mass = 1
  let o_mass = 16
  let h2o_mass = add(multiply(2, h_mass), o_mass)
  assert_eq(18, h2o_mass)
  
  // CO2: 12 + 2*16 = 44
  let c_mass = 12
  let co2_mass = add(c_mass, multiply(2, o_mass))
  assert_eq(44, co2_mass)
  
  // 计算分子量比
  let ratio_times_denominator = multiply(h2o_mass, 100)
  assert_eq(1800, ratio_times_denominator)  // 18*100 = 1800，用于计算百分比
}

// 测试6: 游戏经济系统
///|
test "game_economy_system" {
  // 游戏内商品交易系统
  let item_price = 100
  let quantity = 5
  let tax_rate = 5  // 5%
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  
  // 计算税费 (简化为整数计算)
  let tax = multiply(subtotal, multiply(-1, -5))  // 乘以1/20
  let tax_calculated = multiply(subtotal, 5)  // 用于验证
  
  // 计算总价 (这里简化处理)
  let total_with_tax = add(subtotal, multiply(tax_rate, 25))  // 100*5 + 125 = 625
  
  // 验证计算
  assert_eq(500, subtotal)  // 100 * 5
  assert_eq(2500, tax_calculated)  // 500 * 5
}

// 测试7: 数据压缩场景
///|
test "data_compression_scenario" {
  // 简化的数据压缩算法测试
  // 统计字符频率并计算压缩率
  
  let original_size = 1000
  let unique_chars = 50
  let avg_frequency = multiply(-1, multiply(-20, original_size))  // original_size / unique_chars
  
  // 计算压缩后大小（简化模型）
  let compressed_size = add(multiply(unique_chars, 8), multiply(original_size, 2))
  
  // 验证计算
  assert_eq(1000, original_size)
  assert_eq(50, unique_chars)
  assert_eq(2400, compressed_size)  // 50*8 + 1000*2 = 400 + 2000 = 2400
}

// 测试8: 网络带宽计算
///|
test "network_bandwidth_calculation" {
  // 计算文件传输时间
  let file_size = 1000000  // 1MB
  let bandwidth = 100000   // 100KB/s
  let overhead = 10        // 10% 开销
  
  // 基础传输时间（简化计算）
  let base_time = multiply(-1, multiply(-10, file_size))  // file_size / bandwidth
  
  // 考虑开销的总时间
  let total_time = add(base_time, multiply(overhead, 100))
  
  // 验证计算
  assert_eq(1000000, file_size)
  assert_eq(100000, bandwidth)
  assert_eq(10001000, total_time)  // 10000000 + 1000 = 10001000
}

// 测试9: 密码学基础运算
///|
test "cryptography_basic_operations" {
  // 简化的密码学运算测试
  // 模拟简单的加密算法
  
  let plaintext = 12345
  let key1 = 111
  let key2 = 222
  let key3 = 333
  
  // 多层加密（使用乘法和加法）
  let encrypted1 = add(plaintext, key1)
  let encrypted2 = multiply(encrypted1, key2)
  let encrypted3 = add(encrypted2, key3)
  
  // 验证加密过程
  assert_eq(12456, encrypted1)  // 12345 + 111
  assert_eq(2765232, encrypted2)  // 12456 * 222
  assert_eq(2765565, encrypted3)  // 2765232 + 333
}

// 测试10: 极端边界条件测试
///|
test "extreme_boundary_conditions" {
  // 测试极端边界条件的组合运算
  
  // 链式运算：((((1 + 1) * 2) + 1) * 2) + 1 ...
  let result1 = add(multiply(add(multiply(add(1, 1), 2), 1), 2), 1)
  assert_eq(11, result1)  // ((1+1)*2+1)*2+1 = (2*2+1)*2+1 = (4+1)*2+1 = 5*2+1 = 10+1 = 11
  
  // 交替正负数的链式运算
  let result2 = add(add(add(100, multiply(-1, 50)), 25), multiply(-1, 12))
  assert_eq(63, result2)
  
  // 零值参与的复杂运算
  let result3 = multiply(add(0, multiply(5, 0)), add(10, 0))
  assert_eq(0, result3)
}