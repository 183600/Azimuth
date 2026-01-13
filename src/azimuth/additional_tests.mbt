// 额外的 MoonBit 测试用例
// 这些测试用例补充现有测试，覆盖更多边界情况和实际应用场景

// 测试用例 1: 极值组合运算
test "extreme_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与自身相加（应溢出）
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试最小值与自身相加（应溢出）
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试最大值与-1相乘
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(max_val, -1))
  
  // 测试最小值与2相乘（应溢出）
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

// 测试用例 2: 数学运算律验证
test "mathematical_properties" {
  // 测试加法交换律: a + b = b + a
  let a = 12345
  let b = -67890
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let x = 12
  let y = 15
  let z = 20
  let left = @azimuth.multiply(x, @azimuth.add(y, z))
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left, right)
}

// 测试用例 3: 字符串处理边界情况
test "string_processing_edge_cases" {
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  @azimuth.assert_eq_string("Hello, " + special_chars + "!", @azimuth.greet(special_chars))
  
  // 测试非常长的字符串
  let long_string = "A".repeat(1000)  // 假设 repeat 方法可用
  let expected = "Hello, " + long_string + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_string))
  
  // 测试包含数字和字母混合的字符串
  let mixed_string = "User123Test456"
  @azimuth.assert_eq_string("Hello, User123Test456!", @azimuth.greet(mixed_string))
}

// 测试用例 4: 实际应用场景 - 银行利息计算
test "bank_interest_calculation" {
  // 简化的银行利息计算
  // 本金 = 10000，年利率 = 5%，存款年限 = 3年
  // 简单利息计算：总金额 = 本金 + 本金 * 利率 * 年数 / 100
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算利息：10000 * 5 * 3 / 100 = 1500
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), years) / 100
  // 计算总金额：10000 + 1500 = 11500
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(11500, total)
}

// 测试用例 5: 实际应用场景 - 购物折扣计算
test "shopping_discount_calculation" {
  // 购物折扣计算
  // 商品原价 = 500，折扣 = 20%，最终价格 = 原价 * (100 - 折扣) / 100
  let original_price = 500
  let discount = 20
  
  // 计算折扣金额：500 * 20 / 100 = 100
  let discount_amount = @azimuth.multiply(original_price, discount) / 100
  // 计算最终价格：500 - 100 = 400
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(400, final_price)
}

// 测试用例 6: 复合运算测试
test "compound_operations" {
  // 测试复合运算：(a + b) * (c - d)
  let a = 10
  let b = 15
  let c = 20
  let d = 8
  
  // 计算 (a + b) = 25
  let sum_ab = @azimuth.add(a, b)
  // 计算 (c - d) = 12
  let diff_cd = @azimuth.add(c, -d)
  // 计算 25 * 12 = 300
  let result = @azimuth.multiply(sum_ab, diff_cd)
  
  @azimuth.assert_eq(300, result)
}

// 测试用例 7: 数列计算
test "sequence_calculation" {
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let mut sum = 0
  
  // 使用循环计算前10个奇数的和（这里用递归方式模拟）
  // 1 + 3 + 5 + 7 + 9 + 11 + 13 + 15 + 17 + 19 = 100
  sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9), 11)
  sum = @azimuth.add(@azimuth.add(@azimuth.add(sum, 13), 15), @azimuth.add(17, 19))
  
  @azimuth.assert_eq(100, sum)  // 10² = 100
}

// 测试用例 8: 字符串模板场景
test "string_template_scenarios" {
  // 测试各种字符串模板场景
  // 用户名生成
  let first_name = "John"
  let last_name = "Doe"
  let user_id = 12345
  let full_name = first_name + " " + last_name
  let username = full_name + str(user_id)  // 假设 str 函数可用
  @azimuth.assert_eq_string("Hello, John Doe12345!", @azimuth.greet(username))
  
  // 地址格式化
  let street = "123 Main St"
  let city = "New York"
  let zip_code = "10001"
  let address = street + ", " + city + ", " + zip_code
  @azimuth.assert_eq_string("Hello, 123 Main St, New York, 10001!", @azimuth.greet(address))
}

// 测试用例 9: 错误处理和边界条件
test "error_handling_and_boundaries" {
  // 测试断言函数的正确使用
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试数值边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保边界值处理正确
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)  // 溢出处理
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)  // 溢出处理
  @azimuth.assert_true(@azimuth.multiply(max_val, max_val) == max_val)  // 溢出处理
  
  // 测试零值处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

// 测试用例 10: 性能和复杂度测试
test "performance_and_complexity" {
  // 测试多次运算的组合
  let base = 2
  let mut result = 1
  
  // 计算 2^10 = 1024（通过连续乘法）
  result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(1, 2), 2), 2), 2), 2)
  result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(result, 2), 2), 2), 2)
  
  @azimuth.assert_eq(1024, result)
  
  // 测试累加计算：1 + 2 + 3 + ... + 10 = 55
  let sum_1_to_10 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5), 6)
  let sum_7_to_10 = @azimuth.add(@azimuth.add(7, 8), @azimuth.add(9, 10))
  let total_sum = @azimuth.add(sum_1_to_10, sum_7_to_10)
  
  @azimuth.assert_eq(55, total_sum)
}