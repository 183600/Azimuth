// 新增 MoonBit 测试用例
// 测试 azimuth 模块的各种功能和边界情况

test "add_zero_properties" {
  // 测试加法的零元素性质
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
}

test "multiply_identity_properties" {
  // 测试乘法的单位元性质
  @azimuth.assert_eq(25, @azimuth.multiply(25, 1))
  @azimuth.assert_eq(25, @azimuth.multiply(1, 25))
  @azimuth.assert_eq(0, @azimuth.multiply(25, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 25))
  @azimuth.assert_eq(-30, @azimuth.multiply(-30, 1))
  @azimuth.assert_eq(-30, @azimuth.multiply(1, -30))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试最小值乘法特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, 	\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, émoji🚀!", @azimuth.greet("émoji🚀"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：计算订单总价
  let item_price = 199
  let quantity = 3
  let discount = 50
  let tax_rate = 8
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 应用折扣
  let discounted_total = @azimuth.add(subtotal, -discount)
  // 计算税费
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax), shipping)
  
  @azimuth.assert_eq(622, final_total)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算：计算斐波那契数列前10项的和
  // 斐波那契数列：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  // 和为：143
  
  // 通过加法和乘法计算斐波那契数
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9) // 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f1, f2), f3), f4), f5), f6), f7), f8), f9), f10)
  
  @azimuth.assert_eq(143, sum)
}

test "temperature_conversion_calculation" {
  // 测试温度转换计算：摄氏度转华氏度
  // 使用简化公式：F = C * 9/5 + 32，由于整数运算，我们使用 F = C * 2 + 30
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
  
  // 测试室温
  let room_c = 20
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  @azimuth.assert_eq(70, room_f)
  
  // 测试零下温度
  let below_zero_c = -10
  let below_zero_f = @azimuth.add(@azimuth.multiply(below_zero_c, 2), 30)
  @azimuth.assert_eq(10, below_zero_f)
}

test "compound_interest_calculation" {
  // 测试复利计算：本金 × (1 + 利率)^年数
  // 简化为整数计算：10000 × (1 + 5%)^3 ≈ 10000 × 116 = 1160000
  
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利公式，避免浮点数
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))
  let total = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(1150000, total)  // 10000 × (100 + 5×3) = 10000 × 115 = 1150000
  
  // 测试不同利率
  let high_rate = 10
  let high_growth = @azimuth.add(100, @azimuth.multiply(high_rate, years))
  let high_total = @azimuth.multiply(principal, high_growth)
  @azimuth.assert_eq(1300000, high_total)  // 10000 × (100 + 10×3) = 10000 × 130 = 1300000
}

test "geometric_area_calculation" {
  // 测试几何面积计算
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
  
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 15
  let trapezoid_height = 8
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  @azimuth.assert_eq(80, trapezoid_area)
}