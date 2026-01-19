// Azimuth 综合测试套件
// 使用标准 MoonBit 测试语法编写的10个测试用例

// 测试1: 基本算术运算验证
test "test_basic_arithmetic_operations" {
  // 测试基本的加法和乘法运算
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(5, -3))
  
  // 测试与0和1的运算
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
}

// 测试2: 边界值和溢出处理
test "test_boundary_values_and_overflow" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出处理
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试乘法溢出处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

// 测试3: 字符串处理功能
test "test_string_processing_functionality" {
  // 测试基本字符串处理
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   spaced  !", @azimuth.greet("  spaced  "))
}

// 测试4: 数学性质验证
test "test_mathematical_properties" {
  let a = 15
  let b = 25
  let c = 10
  
  // 验证加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证加法结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 验证乘法结合律: (a * b) * c = a * (b * c)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 验证分配律: a * (b + c) = a * b + a * c
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

// 测试5: 实际应用场景 - 购物车计算
test "test_shopping_cart_calculation" {
  // 商品价格和数量
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let total3 = @azimuth.multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  // 验证总价: 199*2 + 299*1 + 99*3 = 398 + 299 + 297 = 994
  @azimuth.assert_eq(994, cart_total)
}

// 测试6: 实际应用场景 - 温度转换
test "test_temperature_conversion_scenario" {
  // 摄氏度转华氏度（简化公式: F = C * 2 + 30）
  let celsius_temperatures = [0, 10, 25, 100]
  let expected_fahrenheit = [30, 50, 80, 230]
  
  // 测试冰点
  let freezing_f = @azimuth.add(@azimuth.multiply(celsius_temperatures[0], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], freezing_f)
  
  // 测试室温
  let room_f = @azimuth.add(@azimuth.multiply(celsius_temperatures[2], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], room_f)
  
  // 测试沸点
  let boiling_f = @azimuth.add(@azimuth.multiply(celsius_temperatures[3], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[3], boiling_f)
  
  // 测试反向计算：华氏度转摄氏度（简化公式: C = (F - 30) / 2）
  let fahrenheit = 80
  let celsius = @azimuth.divide_with_ceil(@azimuth.add(fahrenheit, -30), 2)
  @azimuth.assert_eq(25, celsius)
}

// 测试7: 复合计算场景
test "test_complex_calculation_scenarios" {
  // 计算矩形的面积和周长
  let length = 12
  let width = 8
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算三角形的面积（使用海伦公式的简化版本）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.divide_with_ceil(@azimuth.multiply(base, height), 2)
  @azimuth.assert_eq(30, triangle_area)
}

// 测试8: 数列和级数计算
test "test_series_and_progression_calculations" {
  // 计算等差数列的和：1 + 3 + 5 + ... + 19（前10个奇数）
  let first_term = 1
  let last_term = 19
  let number_of_terms = 10
  
  // 使用公式：和 = n × (首项 + 末项) / 2
  let arithmetic_sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(first_term, last_term)), 
    2
  )
  @azimuth.assert_eq(100, arithmetic_sum)
  
  // 计算等比数列的和：2 + 4 + 8 + 16（前4项）
  let ratio = 2
  let first = 2
  
  // 手动计算：2 + 4 + 8 + 16 = 30
  let term2 = @azimuth.multiply(first, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(first, term2), term3), term4)
  @azimuth.assert_eq(30, geometric_sum)
}

// 测试9: 金融计算场景
test "test_financial_calculation_scenarios" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate_percent = 5  // 5%
  let years = 3
  
  // 计算总利息
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(rate_percent, years))
  // 本息和 = 本金 + 利息
  let total_amount = @azimuth.add(principal, total_interest)
  
  // 验证结果：10000 + 10000×5×3 = 10000 + 150000 = 160000（表示1600.00元）
  @azimuth.assert_eq(160000, total_amount)
  
  // 折扣计算：折后价 = 原价 × (1 - 折扣率)
  let original_price = 500
  let discount_rate = 20  // 20%折扣
  let discount_amount = @azimuth.divide_with_ceil(@azimuth.multiply(original_price, discount_rate), 100)
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 验证结果：500 - 500×20% = 500 - 100 = 400
  @azimuth.assert_eq(400, discounted_price)
}

// 测试10: 错误处理和边界情况
test "test_error_handling_and_edge_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值运算不会导致未定义行为
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试连续运算的稳定性
  let chain_result = @azimuth.add(
    @azimuth.multiply(@azimuth.add(10, 5), 3),
    @azimuth.multiply(@azimuth.add(20, 15), 2)
  )
  // (10+5)*3 + (20+15)*2 = 15*3 + 35*2 = 45 + 70 = 115
  @azimuth.assert_eq(115, chain_result)
  
  // 测试字符串函数的稳定性
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, x!", @azimuth.greet("x"))
  @azimuth.assert_eq_string("Hello, very_long_name_for_testing_purposes!", @azimuth.greet("very_long_name_for_testing_purposes"))
}