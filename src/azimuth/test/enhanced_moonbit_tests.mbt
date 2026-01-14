// 增强的 MoonBit 测试用例
// 测试 azimuth 项目的核心功能和边界情况

test "enhanced_add_commutativity" {
  // 测试加法交换律的多种情况
  let a = 12345
  let b = -6789
  
  // 验证交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试包含0的情况
  @azimuth.assert_eq(@azimuth.add(a, 0), @azimuth.add(0, a))
  
  // 测试两个负数
  let x = -1000
  let y = -2000
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
}

test "enhanced_multiply_properties" {
  // 测试乘法的数学性质
  let a = 15
  let b = 7
  let c = 3
  
  // 测试结合律 (a * b) * c = a * (b * c)
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试分配律 a * (b + c) = a * b + a * c
  let result3 = @azimuth.multiply(a, @azimuth.add(b, c))
  let result4 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(result3, result4)
}

test "enhanced_string_scenarios" {
  // 测试字符串处理的各种场景
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@domain.com!", @azimuth.greet("test@domain.com"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Smith!", @azimuth.greet("John Smith"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  
  // 测试包含制表符的字符串
  @azimuth.assert_eq_string("Hello, Name\t!", @azimuth.greet("Name\t"))
}

test "enhanced_financial_calculations" {
  // 测试金融计算场景
  // 计算贷款月供（简化版）
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限
  
  // 简化的月供计算：本金 + 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total_payment = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(1900000, total_payment)
  
  // 计算投资回报
  let investment = 50000
  let return_rate = 8     // 8%年回报率
  let investment_years = 5
  
  let returns = @azimuth.multiply(investment, @azimuth.multiply(return_rate, investment_years))
  let final_value = @azimuth.add(investment, returns)
  
  @azimuth.assert_eq(250000, final_value)
}

test "enhanced_temperature_conversions" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius_temperatures = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 110, 230]
  
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))
  @azimuth.assert_eq(50, @azimuth.add(@azimuth.multiply(10, 2), 30))
  @azimuth.assert_eq(70, @azimuth.add(@azimuth.multiply(20, 2), 30))
  @azimuth.assert_eq(90, @azimuth.add(@azimuth.multiply(30, 2), 30))
  @azimuth.assert_eq(110, @azimuth.add(@azimuth.multiply(40, 2), 30))
  @azimuth.assert_eq(230, @azimuth.add(@azimuth.multiply(100, 2), 30))
  
  // 华氏度转摄氏度（简化版：C = (F - 30) ÷ 2）
  @azimuth.assert_eq(0, @azimuth.add(@azimuth.multiply(30, -1), 30) / 2)
  @azimuth.assert_eq(20, @azimuth.add(@azimuth.multiply(70, -1), 30) / 2)
}

test "enhanced_geometry_calculations" {
  // 测试几何计算
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(96, area)
  @azimuth.assert_eq(40, perimeter)
  
  // 计算三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  
  @azimuth.assert_eq(30, triangle_area)
  
  // 计算梯形面积（（上底+下底）×高÷2）
  let top_base = 6
  let bottom_base = 10
  let trapezoid_height = 4
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  
  @azimuth.assert_eq(32, trapezoid_area)
}

test "enhanced_boundary_value_analysis" {
  // 测试边界值分析
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试极值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "enhanced_statistical_calculations" {
  // 测试统计计算
  // 计算一组数据的总和、平均值、最大值、最小值
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(150, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(data1, -average)
  let diff2 = @azimuth.add(data2, -average)
  let diff3 = @azimuth.add(data3, -average)
  let diff4 = @azimuth.add(data4, -average)
  let diff5 = @azimuth.add(data5, -average)
  
  let variance = (@azimuth.multiply(diff1, diff1) + @azimuth.multiply(diff2, diff2) + 
                  @azimuth.multiply(diff3, diff3) + @azimuth.multiply(diff4, diff4) + 
                  @azimuth.multiply(diff5, diff5)) / 5
  
  @azimuth.assert_eq(200, variance)
}

test "enhanced_complex_expressions" {
  // 测试复杂表达式计算
  // 计算：(a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product_ef = @azimuth.multiply(e, f)
  
  let result = @azimuth.add(@azimuth.multiply(sum_ab, diff_cd), product_ef)
  
  // (10 + 5) * (20 - 8) + 3 * 7 = 15 * 12 + 21 = 180 + 21 = 201
  @azimuth.assert_eq(201, result)
  
  // 计算：a² + b² + 2*a*b (完全平方公式)
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let two_ab = @azimuth.multiply(2, @azimuth.multiply(a, b))
  
  let perfect_square = @azimuth.add(@azimuth.add(a_squared, b_squared), two_ab)
  
  // 10² + 5² + 2*10*5 = 100 + 25 + 100 = 225
  @azimuth.assert_eq(225, perfect_square)
}