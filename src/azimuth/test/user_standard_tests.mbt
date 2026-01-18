// 用户标准测试用例
// 测试 azimuth 库的核心功能

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 123
  let b = 456
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -789
  let d = 321
  
  let result3 = @azimuth.add(c, d)
  let result4 = @azimuth.add(d, c)
  @azimuth.assert_eq(result3, result4)
}

test "multiply_function_commutative_property" {
  // 测试乘法交换律：a × b = b × a
  let a = 12
  let b = 15
  
  let result1 = @azimuth.multiply(a, b)
  let result2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -20
  let d = 30
  
  let result3 = @azimuth.multiply(c, d)
  let result4 = @azimuth.multiply(d, c)
  @azimuth.assert_eq(result3, result4)
}

test "distributive_property_verification" {
  // 测试分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 11
  let c = 13
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试第一个温度点
  let result1 = @azimuth.add(@azimuth.multiply(celsius_temps[0], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], result1)
  
  // 测试中间温度点
  let result2 = @azimuth.add(@azimuth.multiply(celsius_temps[2], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], result2)
  
  // 测试最后一个温度点
  let result3 = @azimuth.add(@azimuth.multiply(celsius_temps[4], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[4], result3)
}

test "shopping_cart_total_calculation" {
  // 测试购物车总价计算
  let item_prices = [199, 299, 99]
  let quantities = [2, 1, 3]
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item_prices[0], quantities[0])
  let total2 = @azimuth.multiply(item_prices[1], quantities[1])
  let total3 = @azimuth.multiply(item_prices[2], quantities[2])
  
  // 计算购物车总价
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  // 验证结果：199*2 + 299*1 + 99*3 = 398 + 299 + 297 = 994
  @azimuth.assert_eq(994, cart_total)
}

test "greet_function_with_various_inputs" {
  // 测试 greet 函数的各种输入
  // 测试普通名字
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  
  // 测试包含空格的名字
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的名字
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊字符的名字
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 测试最大值与零的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(max_val, zero))
  
  // 测试最小值与零的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(min_val, zero))
  
  // 测试最大值与最小值的加法
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "compound_interest_calculation" {
  // 测试复利计算（简化版）
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算：本息和 = 本金 + 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  // 验证结果：1000 + 1000*5*3 = 1000 + 15000 = 16000（表示1600元）
  @azimuth.assert_eq(16000, total)
}

test "area_and_perimeter_calculations" {
  // 测试面积和周长计算
  let length = 10
  let width = 5
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 计算矩形周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
  
  // 测试正方形（长宽相等）
  let side = 7
  let square_area = @azimuth.multiply(side, side)
  let square_perimeter = @azimuth.multiply(4, side)
  
  @azimuth.assert_eq(49, square_area)
  @azimuth.assert_eq(28, square_perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 测试包含引号的字符串
  let quoted_name = "\"John\""
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet(quoted_name))
}