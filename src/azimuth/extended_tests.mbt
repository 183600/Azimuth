// 扩展的 MoonBit 测试用例
// 这些测试用例提供了对 azimuth 库更全面的验证

// 测试加法的交换律和结合律
test "add_properties_verification" {
  // 测试加法交换律: a + b = b + a
  let a = 12345
  let b = 67890
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let c = 24680
  let left = @azimuth.add(@azimuth.add(a, b), c)
  let right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left, right)
}

// 测试乘法的交换律和结合律
test "multiply_properties_verification" {
  // 测试乘法交换律: a * b = b * a
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let c = 78
  let left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left, right)
}

// 测试乘法分配律: a * (b + c) = a * b + a * c
test "multiply_distributive_property" {
  let a = 12
  let b = 34
  let c = 56
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

// 测试大数运算的稳定性
test "large_number_operations_stability" {
  // 测试接近边界值的大数运算
  let large_num = 1000000000  // 10亿
  let another_large = 900000000  // 9亿
  
  // 大数加法
  @azimuth.assert_eq(1900000000, @azimuth.add(large_num, another_large))
  
  // 大数乘法（但不会溢出）
  @azimuth.assert_eq(1000000000, @azimuth.multiply(10000, 100000))
  @azimuth.assert_eq(-1000000000, @azimuth.multiply(-10000, 100000))
}

// 测试负数运算的全面性
test "comprehensive_negative_operations" {
  // 测试负数加法的各种组合
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(4, @azimuth.add(-3, 7))
  @azimuth.assert_eq(-4, @azimuth.add(3, -7))
  
  // 测试负数乘法的各种组合
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
}

// 测试字符串处理的各种边界情况
test "string_processing_edge_cases" {
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let expected = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_name))
  
  // 测试只包含空格的字符串
  @azimuth.assert_eq_string("Hello,    !", @azimuth.greet("    "))
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
}

// 测试实际应用场景：温度转换
test "temperature_conversion_scenario" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)  // 25°C = 77°F
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 212  // 沸点
  let celsius2 = @azimuth.multiply(@azimuth.add(fahrenheit2, -32), 5) / 9
  @azimuth.assert_eq(100, celsius2)  // 212°F = 100°C
}

// 测试实际应用场景：购物车计算
test "shopping_cart_calculation" {
  // 商品价格和数量
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品的小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal3 = @azimuth.multiply(item3_price, item3_quantity)
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), subtotal3)
  @azimuth.assert_eq(1194, total)  // 299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
}

// 测试实际应用场景：时间计算
test "time_calculation_scenario" {
  // 计算总秒数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = @azimuth.add(@azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.multiply(minutes, 60)), seconds)
  @azimuth.assert_eq(9045, total_seconds)  // 2*3600 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
  
  // 计算天数差
  let days1 = 15
  let days2 = 8
  let day_difference = @azimuth.add(days1, -days2)
  @azimuth.assert_eq(7, day_difference)
}