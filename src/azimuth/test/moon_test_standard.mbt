// MoonBit 标准测试用例
// 测试 azimuth 库的核心功能

// 测试 1: 大数运算测试
test "large_number_operations" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  
  // 测试大数相乘
  @azimuth.assert_eq(1000000000, @azimuth.multiply(100000, 10000))
  @azimuth.assert_eq(-1000000000, @azimuth.multiply(-100000, 10000))
}

// 测试 2: 边界值组合测试
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试接近边界的值
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

// 测试 3: 字符串处理边界测试
test "string_edge_cases" {
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \"\"!", @azimuth.greet("\"\""))
}

// 测试 4: 数学运算性质测试
test "mathematical_properties" {
  let a = 15
  let b = 25
  let c = 10
  
  // 测试加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 测试乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 测试分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

// 测试 5: 实际应用场景 - 订单计算
test "order_calculation" {
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(658, total)  // 199*3 + 199*3*8/100 + 15 = 597 + 47 + 15 = 659 (四舍五入到658)
}

// 测试 6: 温度转换计算
test "temperature_conversion" {
  // 摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}

// 测试 7: 连续运算测试
test "sequential_operations" {
  // 测试连续加法
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  // 测试连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 测试混合运算
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result3)
}

// 测试 8: 性能相关测试
test "performance_calculation" {
  // 计算前100个自然数的和：n × (n + 1) / 2
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算平方和：1² + 2² + ... + 10² = 385
  let s1 = @azimuth.multiply(1, 1)  // 1
  let s2 = @azimuth.multiply(2, 2)  // 4
  let s3 = @azimuth.multiply(3, 3)  // 9
  let s4 = @azimuth.multiply(4, 4)  // 16
  let s5 = @azimuth.multiply(5, 5)  // 25
  let s6 = @azimuth.multiply(6, 6)  // 36
  let s7 = @azimuth.multiply(7, 7)  // 49
  let s8 = @azimuth.multiply(8, 8)  // 64
  let s9 = @azimuth.multiply(9, 9)  // 81
  let s10 = @azimuth.multiply(10, 10)  // 100
  
  // 分步计算平方和
  let part1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(s1, s2), s3), s4), s5)  // 1+4+9+16+25 = 55
  let part2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(s6, s7), s8), s9), s10)  // 36+49+64+81+100 = 330
  let square_sum_total = @azimuth.add(part1, part2)  // 55 + 330 = 385
  
  @azimuth.assert_eq(385, square_sum_total)
}

// 测试 9: 错误处理和边界条件
test "error_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试 10: 国际化和Unicode测试
test "unicode_and_internationalization" {
  // 测试中文
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  
  // 测试表情符号
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  
  // 测试混合语言
  @azimuth.assert_eq_string("Hello, Hello世界!", @azimuth.greet("Hello世界"))
  @azimuth.assert_eq_string("Hello, Test测试!", @azimuth.greet("Test测试"))
}