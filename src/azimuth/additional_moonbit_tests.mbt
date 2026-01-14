// 额外的 MoonBit 测试用例
// 为 azimuth 模块提供额外的测试覆盖，使用标准 MoonBit 测试语法

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = azimuth::add(a, b)
  let result2 = azimuth::add(b, a)
  assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -123
  let d = 456
  
  let result3 = azimuth::add(c, d)
  let result4 = azimuth::add(d, c)
  assert_eq(result3, result4)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left_side, right_side)
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  // 测试单个字符
  assert_eq_string("Hello, A!", azimuth::greet("A"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  
  // 测试包含数字的字符串
  assert_eq_string("Hello, 2023!", azimuth::greet("2023"))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", azimuth::greet("@#$%"))
}

test "overflow_protection_comprehensive" {
  // 全面测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, max_val))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, min_val))
  
  // 测试乘法溢出保护
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  assert_eq(min_val, azimuth::multiply(min_val, 2))
}

test "real_world_financial_calculation" {
  // 实际金融计算场景：贷款计算
  let principal = 100000  // 本金（单位：元，简化表示）
  let annual_rate = 6     // 年利率6%
  let loan_years = 10     // 贷款年限
  
  // 计算总利息：本金 × 年利率 × 贷款年限
  let total_interest = azimuth::multiply(principal, azimuth::multiply(annual_rate, loan_years))
  
  // 计算还款总额：本金 + 总利息
  let total_payment = azimuth::add(principal, total_interest)
  
  assert_eq(7000000, total_payment)  // 100000 + 100000×6×10 = 100000 + 6000000 = 6100000
}

test "mathematical_series_calculation" {
  // 数学序列计算：斐波那契数列前几项
  // F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8, F(7)=13, F(8)=21
  
  // 计算前8项的和
  let f1 = 1
  let f2 = 1
  let f3 = azimuth::add(f1, f2)  // 2
  let f4 = azimuth::add(f2, f3)  // 3
  let f5 = azimuth::add(f3, f4)  // 5
  let f6 = azimuth::add(f4, f5)  // 8
  let f7 = azimuth::add(f5, f6)  // 13
  let f8 = azimuth::add(f6, f7)  // 21
  
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(f1, f2), f3), f4), f5), f6), azimuth::add(f7, f8))
  assert_eq(54, sum)  // 1+1+2+3+5+8+13+21 = 54
}

test "temperature_conversion_precise" {
  // 精确温度转换：摄氏度转华氏度和开尔文
  // 摄氏度转华氏度：F = C × 9/5 + 32（简化为：F = C × 2 + 30）
  // 摄氏度转开尔文：K = C + 273.15（简化为：K = C + 273）
  
  let celsius = 25
  
  // 转换为华氏度
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 转换为开尔文
  let kelvin = azimuth::add(celsius, 273)
  assert_eq(298, kelvin)  // 25 + 273 = 298
  
  // 测试水的冰点
  let freezing_c = 0
  let freezing_f = azimuth::add(azimuth::multiply(freezing_c, 2), 30)
  let freezing_k = azimuth::add(freezing_c, 273)
  
  assert_eq(30, freezing_f)  // 0 × 2 + 30 = 30
  assert_eq(273, freezing_k)  // 0 + 273 = 273
}

test "complex_business_logic" {
  // 复杂业务逻辑：库存管理和销售计算
  let initial_stock = 1000
  let units_sold = 250
  let units_received = 150
  let unit_cost = 50
  let selling_price = 75
  let tax_rate = 10  // 10%
  
  // 计算当前库存
  let current_stock = azimuth::add(azimuth::add(initial_stock, -units_sold), units_received)
  assert_eq(900, current_stock)  // 1000 - 250 + 150 = 900
  
  // 计算销售收入
  let revenue = azimuth::multiply(units_sold, selling_price)
  assert_eq(18750, revenue)  // 250 × 75 = 18750
  
  // 计算成本
  let cost = azimuth::multiply(units_sold, unit_cost)
  assert_eq(12500, cost)  // 250 × 50 = 12500
  
  // 计算毛利润
  let gross_profit = azimuth::add(revenue, -cost)
  assert_eq(6250, gross_profit)  // 18750 - 12500 = 6250
  
  // 计算税费
  let tax = azimuth::multiply(gross_profit, tax_rate) / 100
  assert_eq(625, tax)  // 6250 × 10 / 100 = 625
  
  // 计算净利润
  let net_profit = azimuth::add(gross_profit, -tax)
  assert_eq(5625, net_profit)  // 6250 - 625 = 5625
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 测试零值运算
  assert_eq(zero, azimuth::multiply(zero, max_val))
  assert_eq(zero, azimuth::multiply(zero, min_val))
  assert_eq(zero, azimuth::multiply(zero, zero))
  assert_eq(max_val, azimuth::add(max_val, zero))
  assert_eq(min_val, azimuth::add(min_val, zero))
  
  // 测试单位元运算
  assert_eq(max_val, azimuth::multiply(max_val, one))
  assert_eq(min_val, azimuth::multiply(min_val, one))
  assert_eq(max_val, azimuth::add(max_val, zero))
  assert_eq(min_val, azimuth::add(min_val, zero))
  
  // 测试负单位元运算
  assert_eq(azimuth::add(max_val, -max_val), zero)
  assert_eq(azimuth::multiply(max_val, neg_one), min_val)
  assert_eq(azimuth::multiply(min_val, neg_one), min_val)  // 特殊情况：最小值 × -1 = 最小值
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟：模拟不同时间复杂度的算法
  let n = 100
  
  // O(1) 常数时间：直接访问
  let constant_time = 42
  
  // O(n) 线性时间：遍历数组求和
  // 模拟：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let linear_time = azimuth::multiply(n, azimuth::add(n, 1)) / 2
  assert_eq(5050, linear_time)  // 100 × 101 / 2 = 5050
  
  // O(n²) 平方时间：嵌套循环
  // 模拟：1² + 2² + 3² + ... + n² = n × (n + 1) × (2n + 1) / 6
  let square_time = azimuth::multiply(n, azimuth::multiply(azimuth::add(n, 1), azimuth::add(azimuth::multiply(2, n), 1))) / 6
  assert_eq(338350, square_time)  // 100 × 101 × 201 / 6 = 338350
  
  // O(log n) 对数时间：二分查找
  // 模拟：计算以2为底的对数（简化为整数）
  let log_time = 7  // 2^7 = 128 > 100, 所以 log2(100) ≈ 7
  
  // 验证计算结果
  assert_eq(42, constant_time)
  assert_eq(5050, linear_time)
  assert_eq(338350, square_time)
  assert_eq(7, log_time)
}