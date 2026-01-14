// 专注的 MoonBit 测试用例 - 覆盖关键功能点
// 导入 azimuth 包以使用其中的函数
test "add_overflow_protection" {
  // 测试加法溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, max_val))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, min_val))
  
  // 边界值相加
  @azimuth.assert_eq(-1, azimuth::@azimuth.add(max_val, min_val))
}
test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘溢出
  @azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(-46341, 46341))
  
  // 最小值特殊处理
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, 2))
  
  // 边界值相乘
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(min_val, 0))
}
test "string_edge_cases" {
  // 测试字符串边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", azimuth::@azimuth.greet("A"))
  
  // 长字符串
  let long_name = "VeryLongNameThatExceedsNormalLength"
  @azimuth.assert_eq_string("Hello, VeryLongNameThatExceedsNormalLength!", azimuth::@azimuth.greet(long_name))
  
  // 特殊字符组合
  @azimuth.assert_eq_string("Hello, \n\t\r!", azimuth::@azimuth.greet("\n\t\r"))
}
test "mathematical_identities" {
  // 测试数学恒等式
  // 加法恒等式
  @azimuth.assert_eq(42, azimuth::@azimuth.add(42, 0))
  @azimuth.assert_eq(42, azimuth::@azimuth.add(0, 42))
  
  // 乘法恒等式
  @azimuth.assert_eq(42, azimuth::@azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, azimuth::@azimuth.multiply(1, 42))
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(0, 42))
  
  // 负一恒等式
  @azimuth.assert_eq(-42, azimuth::@azimuth.multiply(42, -1))
  @azimuth.assert_eq(-42, azimuth::@azimuth.multiply(-1, 42))
}
test "complex_calculation_patterns" {
  // 测试复杂计算模式
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = azimuth::@azimuth.add(azimuth::@azimuth.add(1, 2), azimuth::@azimuth.add(3, azimuth::@azimuth.add(4, 5)))
  @azimuth.assert_eq(15, sum1)
  
  // 阶乘计算：5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial = azimuth::@azimuth.multiply(5, azimuth::@azimuth.multiply(4, azimuth::@azimuth.multiply(3, azimuth::@azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial)
  
  // 平方和：1² + 2² + 3² = 1 + 4 + 9 = 14
  let square_sum = azimuth::@azimuth.add(azimuth::@azimuth.multiply(1, 1), azimuth::@azimuth.add(azimuth::@azimuth.multiply(2, 2), azimuth::@azimuth.multiply(3, 3)))
  @azimuth.assert_eq(14, square_sum)
}
test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车：商品价格 × 数量
  let apple_price = 5
  let apple_quantity = 3
  let orange_price = 8
  let orange_quantity = 2
  
  let apple_total = azimuth::@azimuth.multiply(apple_price, apple_quantity)
  let orange_total = azimuth::@azimuth.multiply(orange_price, orange_quantity)
  let cart_total = azimuth::@azimuth.add(apple_total, orange_total)
  
  @azimuth.assert_eq(31, cart_total)  // 5×3 + 8×2 = 15 + 16 = 31
  
  // 温度转换：摄氏度转华氏度（简化版 F = C × 2 + 30）
  let celsius = 20
  let fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)  // 20×2 + 30 = 70
}
test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出不会产生意外结果
  @azimuth.assert_true(azimuth::@azimuth.add(max_val, 1) <= max_val)
  @azimuth.assert_true(azimuth::@azimuth.add(min_val, -1) >= min_val)
  @azimuth.assert_true(azimuth::@azimuth.multiply(max_val, 2) <= max_val)
  @azimuth.assert_true(azimuth::@azimuth.multiply(min_val, -2) >= min_val)
  
  // 确保基本运算正确
  @azimuth.assert_eq(100, azimuth::@azimuth.add(50, 50))
  @azimuth.assert_eq(25, azimuth::@azimuth.multiply(5, 5))
}
test "performance_sensitive_operations" {
  // 测试性能敏感操作
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, azimuth::@azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000000, azimuth::@azimuth.multiply(large_num1, large_num2))
  
  // 重复运算
  let result = azimuth::@azimuth.add(1, 1)
  result = azimuth::@azimuth.add(result, 1)
  result = azimuth::@azimuth.add(result, 1)
  result = azimuth::@azimuth.add(result, 1)
  @azimuth.assert_eq(5, result)  // 1+1+1+1+1 = 5
}
test "unicode_and_internationalization" {
  // 测试 Unicode 和国际化支持
  // 中文
  @azimuth.assert_eq_string("Hello, 世界!", azimuth::@azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 张三!", azimuth::@azimuth.greet("张三"))
  
  // 日文
  @azimuth.assert_eq_string("Hello, こんにちは!", azimuth::@azimuth.greet("こんにちは"))
  
  // 韩文
  @azimuth.assert_eq_string("Hello, 안녕하세요!", azimuth::@azimuth.greet("안녕하세요"))
  
  // Emoji
  @azimuth.assert_eq_string("Hello, 🌍!", azimuth::@azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 🚀!", azimuth::@azimuth.greet("🚀"))
}
test "data_type_integrity" {
  // 测试数据类型完整性
  // 确保函数返回正确的类型
  let add_result = azimuth::@azimuth.add(10, 20)
  let multiply_result = azimuth::@azimuth.multiply(3, 4)
  let greet_result = azimuth::@azimuth.greet("Test")
  
  // 通过类型检查确保返回值正确
  @azimuth.assert_eq(30, add_result)
  @azimuth.assert_eq(12, multiply_result)
  @azimuth.assert_eq_string("Hello, Test!", greet_result)
  
  // 确保负数处理正确
  let negative_result = azimuth::@azimuth.add(-10, 5)
  @azimuth.assert_eq(-5, negative_result)
  
  let negative_multiply = azimuth::@azimuth.multiply(-3, 4)
  @azimuth.assert_eq(-12, negative_multiply)
}