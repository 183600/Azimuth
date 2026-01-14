// 用户定义的 MoonBit 测试用例
// 这些测试用例展示了 MoonBit 的标准测试语法

// 测试用例1：基本算术运算组合
test "arithmetic_combination" {
  // 测试 (a + b) * c - d 的复合运算
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum_ab = add(a, b)  // 15
  let product = multiply(sum_ab, c)  // 45
  let result = add(product, -d)  // 38
  
  assert_eq(38, result)
}

// 测试用例2：字符串处理边界情况
test "string_edge_cases" {
  // 测试包含特殊字符的字符串
  let result1 = greet("用户123")
  assert_eq_string("Hello, 用户123!", result1)
  
  // 测试包含引号的字符串
  let result2 = greet("\"测试\"")
  assert_eq_string("Hello, \"测试\"!", result2)
  
  // 测试空字符串
  let result3 = greet("")
  assert_eq_string("Hello, !", result3)
}

// 测试用例3：数学函数结合实际场景
test "real_world_calculation" {
  // 计算折扣价格：原价 * (1 - 折扣率)
  let original_price = 200
  let discount_rate = 15  // 15% 折扣
  let discounted_price = multiply(original_price, add(100, -discount_rate)) / 100
  
  assert_eq(170, discounted_price)
  
  // 计算最终价格（含税）：折扣价 * (1 + 税率)
  let tax_rate = 8  // 8% 税率
  let final_price = multiply(discounted_price, add(100, tax_rate)) / 100
  
  assert_eq(183, final_price)
}

// 测试用例4：条件逻辑测试
test "conditional_logic" {
  // 模拟绝对值函数
  let x = -15
  let abs_x = if x < 0 { multiply(x, -1) } else { x }
  assert_eq(15, abs_x)
  
  // 模拟符号函数
  let y = 0
  let sign_y = if y > 0 { 1 } else if y < 0 { -1 } else { 0 }
  assert_eq(0, sign_y)
  
  // 模拟最大值函数
  let a = 25
  let b = 17
  let max_ab = if a > b { a } else { b }
  assert_eq(25, max_ab)
}

// 测试用例5：数列计算
test "sequence_calculation" {
  // 计算等差数列前n项和：n * (a1 + an) / 2
  let n = 10
  let first_term = 3
  let last_term = 30
  
  let sum = multiply(n, add(first_term, last_term)) / 2
  assert_eq(165, sum)
  
  // 验证手动计算
  let manual_sum = add(add(add(add(add(add(add(add(add(3, 6), 9), 12), 15), 18), 21), 24), 27), 30)
  assert_eq(sum, manual_sum)
}

// 测试用例6：几何计算
test "geometric_calculation" {
  // 计算梯形面积：(上底 + 下底) * 高 / 2
  let top_base = 8
  let bottom_base = 12
  let height = 5
  
  let trapezoid_area = multiply(add(top_base, bottom_base), height) / 2
  assert_eq(50, trapezoid_area)
  
  // 计算圆柱体积（简化）：底面积 * 高
  let radius = 4
  let cylinder_height = 10
  let base_area = multiply(3, multiply(radius, radius))  // π ≈ 3
  let volume = multiply(base_area, cylinder_height)
  assert_eq(480, volume)
}

// 测试用例7：时间计算
test "time_calculation" {
  // 计算两个时间点之间的分钟差
  let start_hour = 9
  let start_minute = 30
  let end_hour = 14
  let end_minute = 45
  
  let start_total = add(multiply(start_hour, 60), start_minute)
  let end_total = add(multiply(end_hour, 60), end_minute)
  let duration = add(end_total, -start_total)
  
  assert_eq(315, duration)  // 5小时15分钟 = 315分钟
}

// 测试用例8：商业计算场景
test "business_calculation" {
  // 计算利润：收入 - 成本
  let revenue = 5000
  let cost = 3200
  let profit = add(revenue, -cost)
  assert_eq(1800, profit)
  
  // 计算利润率：利润 / 成本 * 100%
  let profit_rate = multiply(profit, 100) / cost
  assert_eq(56, profit_rate)  // 1800/3200*100 = 56.25%，整数除法得到56
  
  // 计算税费：利润 * 税率
  let tax_rate = 20  // 20%
  let tax = multiply(profit, tax_rate) / 100
  assert_eq(360, tax)
  
  // 计算净利润：利润 - 税费
  let net_profit = add(profit, -tax)
  assert_eq(1440, net_profit)
}

// 测试用例9：科学计算
test "scientific_calculation" {
  // 计算动能：1/2 * m * v²
  let mass = 10
  let velocity = 15
  
  let velocity_squared = multiply(velocity, velocity)
  let kinetic_energy = multiply(multiply(velocity_squared, mass), 1) / 2
  assert_eq(1125, kinetic_energy)
  
  // 计算势能：m * g * h
  let height = 20
  let gravity = 10  // 简化的重力加速度
  let potential_energy = multiply(mass, multiply(gravity, height))
  assert_eq(2000, potential_energy)
}

// 测试用例10：综合应用测试
test "comprehensive_application" {
  // 场景：计算购物车总价，包括折扣、税费和运费
  
  // 商品价格和数量
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算商品小计
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let subtotal = add(item1_total, item2_total)
  
  // 计算折扣（满300减50）
  let discount = if subtotal >= 300 { 50 } else { 0 }
  let discounted_total = add(subtotal, -discount)
  
  // 计算税费（8%）
  let tax_rate = 8
  let tax = multiply(discounted_total, tax_rate) / 100
  
  // 计算运费（满500免运费）
  let shipping = if discounted_total >= 500 { 0 } else { 20 }
  
  // 计算最终总价
  let final_total = add(add(discounted_total, tax), shipping)
  
  // 验证结果：(199*2 + 299*1 - 50) * 1.08 + 20 = 697 * 1.08 + 20 = 752.76 + 20 = 772.76
  // 由于整数运算，实际结果是 752 * 108 / 100 + 20 = 812 + 20 = 832
  assert_eq(832, final_total)
}