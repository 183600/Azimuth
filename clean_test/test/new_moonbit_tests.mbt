// 新增的 MoonBit 测试用例
// 测试 clean_test 模块的功能

test "add_associative_property" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = @clean_test.add(@clean_test.add(a, b), c)
  let result2 = @clean_test.add(a, @clean_test.add(b, c))
  @clean_test.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -150
  let e = 250
  let f = -100
  
  let result3 = @clean_test.add(@clean_test.add(d, e), f)
  let result4 = @clean_test.add(d, @clean_test.add(e, f))
  @clean_test.assert_eq(result3, result4)
}

test "multiply_commutative_property" {
  // 测试乘法交换律: a * b = b * a
  let a = 12
  let b = 15
  
  let result1 = @clean_test.multiply(a, b)
  let result2 = @clean_test.multiply(b, a)
  @clean_test.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -20
  let d = 30
  
  let result3 = @clean_test.multiply(c, d)
  let result4 = @clean_test.multiply(d, c)
  @clean_test.assert_eq(result3, result4)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  let perimeter = @clean_test.add(@clean_test.multiply(2, length), @clean_test.multiply(2, width))
  @clean_test.assert_eq(30, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：复利计算（简化版）
  let principal = 1000
  let rate = 5
  let years = 3
  // 简化的复利计算：本息和 = 本金 + 本金 × 利率 × 年数
  let interest = @clean_test.multiply(principal, @clean_test.multiply(rate, years))
  let total = @clean_test.add(principal, interest)
  @clean_test.assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000，表示1600元
}

test "shopping_cart_calculation" {
  // 测试实际应用场景：购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品的总价
  let total1 = @clean_test.multiply(item1_price, item1_quantity)
  let total2 = @clean_test.multiply(item2_price, item2_quantity)
  let total3 = @clean_test.multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = @clean_test.add(@clean_test.add(total1, total2), total3)
  
  // 验证总价：299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  @clean_test.assert_eq(1294, cart_total)
}

test "temperature_conversion" {
  // 测试实际应用场景：温度转换（简化版）
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 由于我们只有整数运算，我们使用近似值：F = C * 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = @clean_test.add(@clean_test.multiply(celsius_temp, 2), 30)
  @clean_test.assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @clean_test.add(@clean_test.multiply(freezing_celsius, 2), 30)
  @clean_test.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @clean_test.add(@clean_test.multiply(boiling_celsius, 2), 30)
  @clean_test.assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result = @clean_test.greet(long_name)
  @clean_test.assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含引号的字符串
  let quoted_name = "\"John Doe\""
  let result2 = @clean_test.greet(quoted_name)
  @clean_test.assert_eq_string("Hello, \"John Doe\"!", result2)
  
  // 测试包含换行符的字符串
  let newline_name = "Line1\nLine2"
  let result3 = @clean_test.greet(newline_name)
  @clean_test.assert_eq_string("Hello, Line1\nLine2!", result3)
}

test "mathematical_series_sum" {
  // 计算等差数列的和：1 + 3 + 5 + ... + 19（前10个奇数）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 1
  let last = 19
  
  let sum = @clean_test.multiply(n, @clean_test.add(first, last)) / 2
  @clean_test.assert_eq(100, sum)  // 10 × (1 + 19) / 2 = 100
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @clean_test.assert_true(@clean_test.add(max_val, 1) == max_val)
  @clean_test.assert_true(@clean_test.add(min_val, -1) == min_val)
  @clean_test.assert_true(@clean_test.multiply(max_val, 2) == max_val)
  @clean_test.assert_true(@clean_test.multiply(min_val, -1) == min_val)
  
  // 测试正常情况
  @clean_test.assert_true(@clean_test.add(5, 3) == 8)
  @clean_test.assert_true(@clean_test.multiply(4, 6) == 24)
  @clean_test.assert_true(@clean_test.greet("Test") == "Hello, Test!")
}