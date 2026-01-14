// 新的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试 azimuth 包的核心功能
// 包含不超过10个测试用例

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 12
  let b = 8
  let c = 5
  
  // 计算 (a + b) * c - a
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -a)
  
  @azimuth.assert_eq(88, result)  // (12 + 8) * 5 - 12 = 20 * 5 - 12 = 100 - 12 = 88
}

test "sequence_calculations" {
  // 测试序列计算
  let numbers = [10, 20, 30, 40, 50]
  
  // 计算序列的和
  let sum1 = @azimuth.add(numbers[0], numbers[1])
  let sum2 = @azimuth.add(sum1, numbers[2])
  let sum3 = @azimuth.add(sum2, numbers[3])
  let total_sum = @azimuth.add(sum3, numbers[4])
  
  @azimuth.assert_eq(150, total_sum)
  
  // 计算序列的乘积
  let product1 = @azimuth.multiply(numbers[0], numbers[1])
  let product2 = @azimuth.multiply(product1, numbers[2])
  
  @azimuth.assert_eq(6000, product2)  // 10 * 20 * 30 = 6000
}

test "temperature_conversion_formula" {
  // 测试温度转换公式（简化版）
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 简化为整数运算：F = C * 2 + 30
  
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试第一个温度转换
  let f1 = @azimuth.add(@azimuth.multiply(celsius_temps[0], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], f1)
  
  // 测试第三个温度转换
  let f3 = @azimuth.add(@azimuth.multiply(celsius_temps[2], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], f3)
  
  // 测试最后一个温度转换
  let f5 = @azimuth.add(@azimuth.multiply(celsius_temps[4], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[4], f5)
}

test "financial_calculations" {
  // 测试金融计算
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算：本金 + (本金 * 利率 * 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + (10000 * 5 * 3) = 10000 + 150000 = 160000
  
  // 复利计算（简化版）：本金 * (1 + 利率 * 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let compound_total = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(160000, compound_total)  // 10000 * (1 + 5 * 3) = 10000 * 16 = 160000
}

test "geometric_calculations" {
  // 测试几何计算
  let length = 10
  let width = 6
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
  
  // 计算矩形周长
  let perimeter = @azimuth.add(@azimuth.add(length, width), @azimuth.add(length, width))
  @azimuth.assert_eq(32, perimeter)
  
  // 计算三角形面积（底 * 高 / 2）
  let base = 8
  let height = 5
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(20, triangle_area)
}

test "string_processing_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  let special_names = ["John@Doe", "User_123", "Test#Example", "Data$Value"]
  
  // 测试各种特殊字符
  @azimuth.assert_eq_string("Hello, John@Doe!", @azimuth.greet(special_names[0]))
  @azimuth.assert_eq_string("Hello, User_123!", @azimuth.greet(special_names[1]))
  @azimuth.assert_eq_string("Hello, Test#Example!", @azimuth.greet(special_names[2]))
  @azimuth.assert_eq_string("Hello, Data$Value!", @azimuth.greet(special_names[3]))
}

test "edge_case_boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "algorithmic_calculations" {
  // 测试算法计算
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 计算等比数列的和：2 + 4 + 8 + 16
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 2)
  let term3 = @azimuth.multiply(term2, 2)
  let term4 = @azimuth.multiply(term3, 2)
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(30, geo_sum)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_prices = [199, 299, 399]
  let quantities = [2, 1, 3]
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item_prices[0], quantities[0])
  let total2 = @azimuth.multiply(item_prices[1], quantities[1])
  let total3 = @azimuth.multiply(item_prices[2], quantities[2])
  
  // 计算购物车总价
  let subtotal = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  // 计算税费（10%）
  let tax = @azimuth.multiply(subtotal, 10) / 100
  
  // 计算最终总价（含运费50）
  let shipping = 50
  let final_total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(2187, final_total)  // (199*2 + 299*1 + 399*3) + 10% + 50 = 398 + 299 + 1197 + 189 + 50 = 2133
}

test "performance_optimization_calculations" {
  // 测试性能优化相关的计算
  let base_value = 1000
  let multiplier = 2
  let iterations = 10
  
  // 模拟循环累加：base_value * (1 + 2 + ... + iterations)
  // 使用公式：base_value * iterations * (iterations + 1) / 2
  let sum_formula = @azimuth.multiply(base_value, @azimuth.multiply(iterations, @azimuth.add(iterations, 1)) / 2)
  
  @azimuth.assert_eq(55000, sum_formula)  // 1000 * 10 * 11 / 2 = 55000
  
  // 模拟指数增长：base_value * multiplier^iterations
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(2, 2))
  let exponential_growth = @azimuth.multiply(base_value, power_of_2)
  
  @azimuth.assert_eq(256000, exponential_growth)  // 1000 * 256 = 256000
}