// 用户综合测试用例
// 测试 azimuth 库的核心功能，覆盖各种场景

test "addition_with_various_ranges" {
  // 测试不同范围的加法运算
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(1000, -1000))
  @azimuth.assert_eq(-200, @azimuth.add(-100, -100))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

test "multiplication_edge_cases" {
  // 测试乘法边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_function_with_special_inputs" {
  // 测试 greet 函数的特殊输入
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "mathematical_associative_properties" {
  // 测试数学结合律性质
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_add = @azimuth.add(@azimuth.add(a, b), c)
  let right_add = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_add, right_add)
  
  // 乘法结合律: (a × b) × c = a × (b × c)
  let left_mul = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul, right_mul)
}

test "overflow_protection_mechanisms" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 负数溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 特殊情况：最小值乘以-1
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let item_price = 299
  let quantity = 3
  let discount = 50
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 应用折扣
  let discounted_total = @azimuth.add(subtotal, -discount)
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  // 计算最终总价
  let final_total = @azimuth.add(discounted_total, tax_amount)
  
  @azimuth.assert_eq(925, final_total)
}

test "string_concatenation_performance" {
  // 测试字符串连接性能
  let short_name = "AI"
  let medium_name = "MoonBit"
  let long_name = "ArtificialIntelligenceLanguage"
  
  @azimuth.assert_eq_string("Hello, AI!", @azimuth.greet(short_name))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(medium_name))
  @azimuth.assert_eq_string("Hello, ArtificialIntelligenceLanguage!", @azimuth.greet(long_name))
}

test "arithmetical_sequence_calculations" {
  // 测试等差数列计算
  // 计算前n个自然数的和：1 + 2 + 3 + ... + n
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 计算前n个偶数的和：2 + 4 + 6 + ... + 2n
  let even_sum = @azimuth.multiply(2, sum)
  @azimuth.assert_eq(110, even_sum)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化公式）：F = C × 2 + 30
  
  // 测试常见温度点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  let room_c = 20
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  @azimuth.assert_eq(70, room_f)
  
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算正方形面积和周长
  let side = 8
  let square_area = @azimuth.multiply(side, side)
  let square_perimeter = @azimuth.multiply(4, side)
  
  @azimuth.assert_eq(64, square_area)
  @azimuth.assert_eq(32, square_perimeter)
  
  // 计算矩形面积和对角线平方（简化）
  let length = 6
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  let diagonal_squared = @azimuth.add(@azimuth.multiply(length, length), @azimuth.multiply(width, width))
  
  @azimuth.assert_eq(48, rectangle_area)
  @azimuth.assert_eq(100, diagonal_squared)
}