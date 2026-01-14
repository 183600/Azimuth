// MoonBit 测试套件 - 标准测试用例
// 包含 10 个全面的测试用例，覆盖各种功能和场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, azimuth::add(7, 3))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-8, azimuth::add(-3, -5))
  assert_eq(15, azimuth::multiply(3, 5))
  assert_eq(-12, azimuth::multiply(-3, 4))
  assert_eq(0, azimuth::multiply(0, 100))
}

test "boundary_value_handling" {
  // 边界值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  
  // 测试最小值边界
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 测试边界值组合
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "string_processing_comprehensive" {
  // 综合字符串处理测试
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, special@#$%!", azimuth::greet("special@#$%"))
  assert_eq_string("Hello, Line1\nLine2!", azimuth::greet("Line1\nLine2"))
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景1: 计算矩形面积和周长
  let length = 8
  let width = 5
  let area = azimuth::multiply(length, width)
  let perimeter = azimuth::multiply(azimuth::add(length, width), 2)
  assert_eq(40, area)
  assert_eq(26, perimeter)
  
  // 场景2: 计算平均分
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let total = azimuth::add(azimuth::add(score1, score2), score3)
  let average = total / 3
  assert_eq(85, average)
  
  // 场景3: 计算折扣价格
  let original_price = 200
  let discount_rate = 15  // 15% 折扣
  let discount_amount = azimuth::multiply(original_price, discount_rate) / 100
  let final_price = azimuth::add(original_price, -discount_amount)
  assert_eq(170, final_price)
}

test "real_world_business_calculations" {
  // 实际业务计算测试
  // 场景1: 订单总价计算
  let item_price = 99
  let quantity = 3
  let shipping_fee = 12
  let tax_rate = 8  // 8% 税率
  
  let subtotal = azimuth::multiply(item_price, quantity)
  let tax_amount = azimuth::multiply(subtotal, tax_rate) / 100
  let total = azimuth::add(azimuth::add(subtotal, tax_amount), shipping_fee)
  assert_eq(332, total)  // 99*3 + 99*3*8/100 + 12 = 297 + 23 + 12 = 332
  
  // 场景2: 货币转换
  let usd_amount = 150
  let exchange_rate = 7  // 1 USD = 7 CNY
  let conversion_fee = 10  // 固定手续费
  let cny_amount = azimuth::multiply(usd_amount, exchange_rate)
  let final_amount = azimuth::add(cny_amount, conversion_fee)
  assert_eq(1060, final_amount)  // 150*7 + 10 = 1050 + 10 = 1060
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 验证加法交换律
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 验证乘法交换律
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 验证分配律
  let distributive_left = azimuth::multiply(a, azimuth::add(b, c))
  let distributive_right = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 验证结合律
  let associative_left = azimuth::add(azimuth::add(a, b), c)
  let associative_right = azimuth::add(a, azimuth::add(b, c))
  assert_eq(associative_left, associative_right)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_true(azimuth::add(max_val, 100) == max_val)
  assert_true(azimuth::add(min_val, -100) == min_val)
  assert_true(azimuth::multiply(max_val, 2) == max_val)
  assert_true(azimuth::multiply(min_val, 2) == min_val)
  
  // 测试断言函数
  azimuth::assert_true(true)
  azimuth::assert_false(false)
  azimuth::assert_eq(42, 42)
  azimuth::assert_eq_string("test", "test")
  
  // 测试特殊情况
  assert_eq(0, azimuth::multiply(0, max_val))
  assert_eq(0, azimuth::multiply(0, min_val))
  assert_eq(max_val, azimuth::add(0, max_val))
  assert_eq(min_val, azimuth::add(0, min_val))
}

test "performance_and_efficiency" {
  // 性能和效率测试
  // 测试大量数据计算
  let base_value = 100
  let multiplier = 1000
  
  // 计算等差数列和：1 + 2 + ... + 100 = 100 * 101 / 2 = 5050
  let series_sum = azimuth::multiply(multiplier, azimuth::add(multiplier, 1)) / 2
  assert_eq(500500, series_sum)
  
  // 测试连续运算
  let consecutive_result = azimuth::add(
    azimuth::multiply(10, 20),
    azimuth::multiply(30, 40)
  )
  assert_eq(1400, consecutive_result)  // 10*20 + 30*40 = 200 + 1200 = 1400
  
  // 测试嵌套运算
  let nested_result = azimuth::multiply(
    azimuth::add(5, 3),
    azimuth::add(7, 2)
  )
  assert_eq(72, nested_result)  // (5+3)*(7+2) = 8*9 = 72
}

test "data_type_operations" {
  // 数据类型操作测试
  // 模拟布尔运算
  let true_as_int = 1
  let false_as_int = 0
  
  // 模拟逻辑与：a && b = a * b
  let logical_and = azimuth::multiply(true_as_int, false_as_int)
  assert_eq(0, logical_and)
  
  // 模拟逻辑或：a || b = a + b - a * b
  let logical_or = azimuth::add(
    azimuth::add(true_as_int, false_as_int),
    azimuth::multiply(true_as_int, false_as_int)
  )
  assert_eq(1, logical_or)
  
  // 测试字符串到数字的间接操作
  let number_str = "42"
  let greeting_result = azimuth::greet(number_str)
  assert_eq_string("Hello, 42!", greeting_result)
  
  // 测试特殊字符处理
  let special_chars = "!@#$%^&*()"
  let special_result = azimuth::greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()!", special_result)
}

test "comprehensive_application_scenario" {
  // 综合应用场景测试
  // 场景：计算一个项目的总成本和收益
  
  // 项目参数
  let initial_cost = 10000
  let monthly_cost = 1500
  let duration_months = 12
  let monthly_revenue = 2500
  
  // 计算总成本
  let total_cost = azimuth::add(
    initial_cost,
    azimuth::multiply(monthly_cost, duration_months)
  )
  
  // 计算总收入
  let total_revenue = azimuth::multiply(monthly_revenue, duration_months)
  
  // 计算净利润
  let net_profit = azimuth::add(total_revenue, -total_cost)
  
  // 计算投资回报率（简化为百分比）
  let roi_percentage = azimuth::multiply(net_profit, 100) / total_cost
  
  // 验证结果
  assert_eq(28000, total_cost)    // 10000 + 1500*12 = 10000 + 18000 = 28000
  assert_eq(30000, total_revenue)  // 2500*12 = 30000
  assert_eq(2000, net_profit)      // 30000 - 28000 = 2000
  assert_eq(7, roi_percentage)     // 2000*100/28000 ≈ 7.14 ≈ 7 (整数除法)
  
  // 验证项目是否盈利
  azimuth::assert_true(net_profit > 0)
  
  // 测试项目报告生成
  let project_name = "MoonBit Project"
  let report = azimuth::greet(project_name)
  assert_eq_string("Hello, MoonBit Project!", report)
}