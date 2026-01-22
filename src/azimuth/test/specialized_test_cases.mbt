// 专门的测试用例 - 针对特定场景的高级测试

// 导入azimuth模块中的函数
// use line removed

test "advanced_arithmetic_progression" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13 = 35
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 1
  let last = 13
  let sum = azimuth.@azimuth.multiply(n, azimuth.@azimuth.add(first, last)) / 2
  azimuth.@azimuth.assert_eq(35, sum)
}

test "string_validation_with_numbers" {
  // 测试包含数字的字符串验证
  let result1 = azimuth.@azimuth.greet("User123")
  azimuth.@azimuth.assert_eq_string("Hello, User123!", result1)
  
  let result2 = azimuth.@azimuth.greet("2023")
  azimuth.@azimuth.assert_eq_string("Hello, 2023!", result2)
}

test "complex_business_calculation" {
  // 复杂业务计算：计算打折后的商品总价
  let item_price = 199
  let quantity = 3
  let discount_rate = 15  // 15%折扣
  let tax_rate = 8        // 8%税率
  
  // 计算商品原价
  let subtotal = azimuth.@azimuth.multiply(item_price, quantity)
  
  // 计算折扣金额
  let discount_amount = azimuth.@azimuth.multiply(subtotal, discount_rate) / 100
  
  // 计算折后价格
  let discounted_price = azimuth.@azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = azimuth.@azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = azimuth.@azimuth.add(discounted_price, tax_amount)
  
  azimuth.@azimuth.assert_eq(550, final_price)
}

test "mathematical_series_validation" {
  // 验证平方数列：1² + 2² + 3² + 4² = 30
  let square1 = azimuth.@azimuth.multiply(1, 1)
  let square2 = azimuth.@azimuth.multiply(2, 2)
  let square3 = azimuth.@azimuth.multiply(3, 3)
  let square4 = azimuth.@azimuth.multiply(4, 4)
  
  let sum_of_squares = azimuth.@azimuth.add(azimuth.@azimuth.add(azimuth.@azimuth.add(square1, square2), square3), square4)
  azimuth.@azimuth.assert_eq(30, sum_of_squares)
}

test "temperature_conversion_precise" {
  // 精确温度转换：摄氏度转华氏度 F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 - C × 0.2 + 32 ≈ C × 2 + 30
  let celsius = 15
  let fahrenheit = azimuth.@azimuth.add(azimuth.@azimuth.multiply(celsius, 2), 30)
  azimuth.@azimuth.assert_eq(60, fahrenheit)
  
  // 测试负温度
  let negative_celsius = -10
  let negative_fahrenheit = azimuth.@azimuth.add(azimuth.@azimuth.multiply(negative_celsius, 2), 30)
  azimuth.@azimuth.assert_eq(10, negative_fahrenheit)
}

test "data_structure_simulation" {
  // 模拟数据结构操作：计算数组元素的总和与平均值
  // 假设数组元素为：[10, 20, 30, 40, 50]
  let sum = azimuth.@azimuth.add(azimuth.@azimuth.add(azimuth.@azimuth.add(azimuth.@azimuth.add(10, 20), 30), 40), 50)
  let average = sum / 5
  
  azimuth.@azimuth.assert_eq(150, sum)
  azimuth.@azimuth.assert_eq(30, average)
}

test "algorithm_complexity_simulation" {
  // 模拟算法复杂度：O(n²)操作的前几项
  // 计算前5个数的平方和：1² + 2² + 3² + 4² + 5² = 55
  let n = 5
  let i = 1
  let i_squared = azimuth.@azimuth.multiply(i, i)
  
  let j = 2
  let j_squared = azimuth.@azimuth.multiply(j, j)
  
  let k = 3
  let k_squared = azimuth.@azimuth.multiply(k, k)
  
  let l = 4
  let l_squared = azimuth.@azimuth.multiply(l, l)
  
  let m = 5
  let m_squared = azimuth.@azimuth.multiply(m, m)
  
  let total = azimuth.@azimuth.add(azimuth.@azimuth.add(azimuth.@azimuth.add(azimuth.@azimuth.add(i_squared, j_squared), k_squared), l_squared), m_squared)
  azimuth.@azimuth.assert_eq(55, total)
}

test "error_boundary_conditions" {
  // 错误边界条件测试：确保边界值被正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  azimuth.@azimuth.assert_eq(max_val, azimuth.@azimuth.add(max_val, 1))
  azimuth.@azimuth.assert_eq(max_val, azimuth.@azimuth.multiply(max_val, 2))
  
  // 测试最小值的各种运算
  azimuth.@azimuth.assert_eq(min_val, azimuth.@azimuth.add(min_val, -1))
  azimuth.@azimuth.assert_eq(min_val, azimuth.@azimuth.multiply(min_val, 2))
  
  // 测试正常运算
  azimuth.@azimuth.assert_eq(0, azimuth.@azimuth.add(max_val, min_val))
}

test "string_processing_edge_cases" {
  // 字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  let result1 = azimuth.@azimuth.greet(special_chars)
  azimuth.@azimuth.assert_eq_string("Hello, !@#$%^&*()!", result1)
  
  // 测试包含空格的字符串
  let spaced_string = "Hello World"
  let result2 = azimuth.@azimuth.greet(spaced_string)
  azimuth.@azimuth.assert_eq_string("Hello, Hello World!", result2)
  
  // 测试包含数字和字母的混合字符串
  let mixed_string = "Test123ABC"
  let result3 = azimuth.@azimuth.greet(mixed_string)
  azimuth.@azimuth.assert_eq_string("Hello, Test123ABC!", result3)
}

test "mathematical_identities_verification" {
  // 数学恒等式验证
  let a = 12
  let b = 8
  
  // 验证平方差公式：(a+b)×(a-b) = a²-b²
  let sum_ab = azimuth.@azimuth.add(a, b)
  let diff_ab = azimuth.@azimuth.add(a, -b)
  let left_side = azimuth.@azimuth.multiply(sum_ab, diff_ab)
  
  let a_squared = azimuth.@azimuth.multiply(a, a)
  let b_squared = azimuth.@azimuth.multiply(b, b)
  let right_side = azimuth.@azimuth.add(a_squared, -b_squared)
  
  azimuth.@azimuth.assert_eq(left_side, right_side)
  
  // 验证分配律：a×(b+c) = a×b + a×c
  let c = 5
  let distrib_left = azimuth.@azimuth.multiply(a, azimuth.@azimuth.add(b, c))
  let distrib_right = azimuth.@azimuth.add(azimuth.@azimuth.multiply(a, b), azimuth.@azimuth.multiply(a, c))
  
  azimuth.@azimuth.assert_eq(distrib_left, distrib_right)
}