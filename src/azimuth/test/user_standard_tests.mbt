// 用户标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试 azimuth 库的核心功能

test "addition_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值相加
  let result = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, result)
  
  // 测试大数相加
  let large_sum = @azimuth.add(1000000, 2000000)
  @azimuth.assert_eq(3000000, large_sum)
  
  // 测试连续相加
  let sequential = @azimuth.add(@azimuth.add(100, 200), 300)
  @azimuth.assert_eq(600, sequential)
}

test "multiplication_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘
  let large_product = @azimuth.multiply(10000, 1000)
  @azimuth.assert_eq(10000000, large_product)
  
  // 测试连续相乘
  let sequential = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, sequential)
  
  // 测试负数相乘
  let negative_product = @azimuth.multiply(-15, 20)
  @azimuth.assert_eq(-300, negative_product)
}

test "string_processing_comprehensive" {
  // 测试字符串处理的综合情况
  // 测试长字符串
  let long_name = "VeryLongNameThatExceedsNormalLength"
  let long_result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongNameThatExceedsNormalLength!", long_result)
  
  // 测试包含数字的字符串
  let numeric_name = "User123"
  let numeric_result = @azimuth.greet(numeric_name)
  @azimuth.assert_eq_string("Hello, User123!", numeric_result)
  
  // 测试包含特殊符号的字符串
  let symbolic_name = "test@example.com"
  let symbolic_result = @azimuth.greet(symbolic_name)
  @azimuth.assert_eq_string("Hello, test@example.com!", symbolic_result)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 42
  let b = 17
  let c = 23
  
  // 测试加法结合律
  let associative1 = @azimuth.add(@azimuth.add(a, b), c)
  let associative2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(associative1, associative2)
  
  // 测试乘法结合律
  let mul_associative1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let mul_associative2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(mul_associative1, mul_associative2)
  
  // 测试分配律
  let distributive1 = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive1, distributive2)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 计算简单利息
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 测试折扣计算
  let original_price = 500
  let discount_percent = 20
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(400, discounted_price)
}

test "geometry_calculations" {
  // 测试几何计算
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算三角形面积（简化）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  let fahrenheit1 = @azimuth.add(@azimuth.multiply(celsius_temps[0], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], fahrenheit1)
  
  let fahrenheit2 = @azimuth.add(@azimuth.multiply(celsius_temps[1], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[1], fahrenheit2)
  
  let fahrenheit3 = @azimuth.add(@azimuth.multiply(celsius_temps[2], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], fahrenheit3)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值
  let numbers = [10, 20, 30, 40, 50]
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算加权平均
  let values = [80, 90, 75]
  let weights = [2, 3, 1]
  let weighted_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(values[0], weights[0]), @azimuth.multiply(values[1], weights[1])), @azimuth.multiply(values[2], weights[2]))
  let total_weight = @azimuth.add(@azimuth.add(weights[0], weights[1]), weights[2])
  let weighted_average = weighted_sum / total_weight
  @azimuth.assert_eq(85, weighted_average)
}

test "error_handling_validation" {
  // 测试错误处理和验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = @azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, overflow_add)
  
  let underflow_add = @azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, underflow_add)
  
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_business_scenario" {
  // 测试复杂业务场景：电商订单计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let total3 = @azimuth.multiply(item3_price, item3_quantity)
  
  // 计算小计
  let subtotal = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  // 计算税费（8%）
  let tax_rate = 8
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算运费
  let shipping = 20
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping)
  
  // 验证结果：199*2 + 299*1 + 99*3 = 398 + 299 + 297 = 994
  // 税费：994 * 8% = 79（整数除法）
  // 总价：994 + 79 + 20 = 1093
  @azimuth.assert_eq(1093, total)
}