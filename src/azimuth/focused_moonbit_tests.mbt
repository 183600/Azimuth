// 专注的 MoonBit 测试用例
// 覆盖 azimuth 项目的核心功能和实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-6, @azimuth.add(-10, 4))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "overflow_protection_mechanisms" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_capabilities" {
  // 测试字符串处理能力
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "mathematical_formula_implementation" {
  // 测试数学公式实现
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(96, area)
  @azimuth.assert_eq(40, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  // 复合利息计算：本金 × (1 + 利率 × 年数)
  let principal = 5000
  let rate = 8  // 8%
  let years = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(125000, total_amount)  // 5000 + 5000×8×3 = 125000
}

test "data_analysis_operations" {
  // 测试数据分析操作
  // 计算一组数据的平均值、最大值、最小值
  let data1 = 85
  let data2 = 92
  let data3 = 78
  let data4 = 96
  let data5 = 89
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  let average = sum / 5
  
  @azimuth.assert_eq(440, sum)
  @azimuth.assert_eq(88, average)
  
  // 验证数据范围
  @azimuth.assert_true(average >= data3)
  @azimuth.assert_true(average <= data4)
}

test "unit_conversion_calculations" {
  // 测试单位转换计算
  // 公里转米：1公里 = 1000米
  let kilometers = 5
  let meters = @azimuth.multiply(kilometers, 1000)
  @azimuth.assert_eq(5000, meters)
  
  // 小时转秒：1小时 = 3600秒
  let hours = 2
  let seconds = @azimuth.multiply(hours, 3600)
  @azimuth.assert_eq(7200, seconds)
  
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius = 15
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(60, fahrenheit)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  // 订单总价计算：商品价格 × 数量 + 税费 - 折扣
  let item_price = 199
  let quantity = 3
  let tax_rate = 10  // 10%
  let discount = 50
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  let total_before_discount = @azimuth.add(subtotal, tax_amount)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(616, final_total)  // (199×3) + (199×3×10/100) - 50 = 597 + 59 - 50 = 616
}

test "algorithmic_implementation" {
  // 测试算法实现
  // 计算等比数列前n项和：a + ar + ar² + ... + ar^(n-1)
  let a = 2  // 首项
  let r = 3  // 公比
  let n = 4  // 项数
  
  // 手动计算：2 + 6 + 18 + 54 = 80
  let term1 = a
  let term2 = @azimuth.multiply(a, r)
  let term3 = @azimuth.multiply(term2, r)
  let term4 = @azimuth.multiply(term3, r)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(80, sum)
}