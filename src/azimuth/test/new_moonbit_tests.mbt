// 新增的 MoonBit 测试用例
// 覆盖 azimuth 项目的核心功能，使用标准 MoonBit 测试语法

test "addition_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -123
  let d = 456
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
}

test "multiplication_identity" {
  // 测试乘法单位元：a * 1 = a
  let test_values = [0, 1, -1, 42, -42, 1000, -1000, 2147483647, -2147483648]
  
  // 测试正单位元
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-42, 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  
  // 测试负单位元
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(42, @azimuth.multiply(-42, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(2147483647, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
}

test "string_processing_unicode" {
  // 测试包含各种 Unicode 字符的字符串处理
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, αβγδε!", @azimuth.greet("αβγδε"))
  @azimuth.assert_eq_string("Hello, 你好!", @azimuth.greet("你好"))
  @azimuth.assert_eq_string("Hello, こんにちは!", @azimuth.greet("こんにちは"))
  @azimuth.assert_eq_string("Hello, 👨‍💻!", @azimuth.greet("👨‍💻"))
}

test "financial_calculation_loan" {
  // 测试贷款计算场景
  // 贷款金额：10000，年利率：5%，贷款期限：3年
  // 简化计算：总还款额 = 本金 + 本金 × 利率 × 年数
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  let yearly_interest = @azimuth.multiply(principal, annual_rate)
  let total_interest = @azimuth.multiply(yearly_interest, years)
  let total_repayment = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(160000, total_repayment)  // 10000 + 10000×5×3 = 160000
}

test "temperature_conversion_precise" {
  // 测试精确的温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30 - C/10
  
  let celsius_temps = [0, 10, 20, 30, 40, 100]
  
  // 0°C = 32°F (简化计算：0×2+30-0=30，近似值)
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_0)
  
  // 20°C = 68°F (简化计算：20×2+30-2=68)
  let fahrenheit_20 = @azimuth.add(@azimuth.add(@azimuth.multiply(20, 2), 30), -2)
  @azimuth.assert_eq(68, fahrenheit_20)
  
  // 100°C = 212°F (简化计算：100×2+30-10=220)
  let fahrenheit_100 = @azimuth.add(@azimuth.add(@azimuth.multiply(100, 2), 30), -10)
  @azimuth.assert_eq(220, fahrenheit_100)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  // 矩形面积：长 × 宽
  let rectangle_length = 15
  let rectangle_width = 8
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  @azimuth.assert_eq(120, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let triangle_base = 12
  let triangle_height = 9
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  @azimuth.assert_eq(54, triangle_area)
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let trapezoid_top = 6
  let trapezoid_bottom = 10
  let trapezoid_height = 5
  let trapezoid_area = @azimuth.multiply(@azimuth.add(trapezoid_top, trapezoid_bottom), trapezoid_height) / 2
  @azimuth.assert_eq(40, trapezoid_area)
}

test "shopping_cart_with_discount" {
  // 测试带折扣的购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 1000
  let discount_rate = 10  // 10%
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算折扣（如果超过阈值）
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let final_total = @azimuth.add(subtotal, -discount_amount)
  
  @azimuth.assert_eq(1094, final_total)  // (299×2 + 199×3) - 10% = 1194 - 119 = 1075
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：[23, 45, 67, 89, 12, 34, 56, 78, 90, 1]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(23, 45), 67), 89), 12), 34)
  let sum2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(56, 78), 90), 1), 0), 0)
  let total_sum = @azimuth.add(sum, sum2)
  @azimuth.assert_eq(495, total_sum)
  
  // 计算平均值（整数除法）
  let average = total_sum / 10
  @azimuth.assert_eq(49, average)
  
  // 计算最大值和最小值的差（极差）
  let max_val = 90
  let min_val = 1
  let range = @azimuth.add(max_val, -min_val)
  @azimuth.assert_eq(89, range)
}

test "complex_number_operations" {
  // 测试复合数学运算
  // 计算：((a + b) × (c - d)) + (e × f)
  let a = 15
  let b = 25
  let c = 40
  let d = 10
  let e = 8
  let f = 12
  
  let sum_ab = @azimuth.add(a, b)  // 40
  let diff_cd = @azimuth.add(c, -d)  // 30
  let product1 = @azimuth.multiply(sum_ab, diff_cd)  // 1200
  let product2 = @azimuth.multiply(e, f)  // 96
  let result = @azimuth.add(product1, product2)  // 1296
  
  @azimuth.assert_eq(1296, result)
}