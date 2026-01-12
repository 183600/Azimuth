// 额外的综合测试用例

test "large_number_stability" {
  // 测试大数相加的稳定性
  let large_num1 = 100000000
  let large_num2 = 200000000
  let large_num3 = 300000000
  
  // 测试连续大数加法
  let result1 = @clean_test.add(@clean_test.add(large_num1, large_num2), large_num3)
  let result2 = @clean_test.add(large_num1, @clean_test.add(large_num2, large_num3))
  @clean_test.assert_eq(result1, result2)  // 验证结合律
  
  // 测试大数相乘的边界
  let large_multiply = @clean_test.multiply(large_num1, 10)
  @clean_test.assert_eq(1000000000, large_multiply)
}

// 测试用例 2: 负数运算的深度测试
///|
test "negative_number_deep_test" {
  // 测试负数乘法的各种组合
  @clean_test.assert_eq(100, @clean_test.multiply(-10, -10))  // 负负得正
  @clean_test.assert_eq(-100, @clean_test.multiply(-10, 10))   // 负正得负
  @clean_test.assert_eq(-100, @clean_test.multiply(10, -10))   // 正负得负
  @clean_test.assert_eq(0, @clean_test.multiply(-10, 0))       // 乘0得0
  
  // 测试负数加法的边界情况
  @clean_test.assert_eq(-200, @clean_test.add(-100, -100))     // 负数相加
  @clean_test.assert_eq(0, @clean_test.add(-100, 100))         // 正负抵消
  @clean_test.assert_eq(-50, @clean_test.add(-100, 50))        // 部分抵消
}

// 测试用例 3: 字符串处理的高级测试
///|
test "advanced_string_processing" {
  // 测试包含数字和特殊字符的字符串
  @clean_test.assert_eq_string("Hello, User123!", @clean_test.greet("User123"))
  @clean_test.assert_eq_string("Hello, test@example.com!", @clean_test.greet("test@example.com"))
  @clean_test.assert_eq_string("Hello, 123-456-7890!", @clean_test.greet("123-456-7890"))
  
  // 测试空格和特殊字符
  @clean_test.assert_eq_string("Hello, John Doe!", @clean_test.greet("John Doe"))
  @clean_test.assert_eq_string("Hello,   !", @clean_test.greet("   "))  // 只有空格
  @clean_test.assert_eq_string("Hello, \\t\\n!", @clean_test.greet("\\t\\n"))  // 制表符和换行符
}

// 测试用例 4: 数学恒等式的验证
///|
test "mathematical_identities" {
  // 验证平方差公式: a² - b² = (a + b)(a - b)
  let a = 15
  let b = 5
  
  // 直接计算 a² - b²
  let a_squared = @clean_test.multiply(a, a)
  let b_squared = @clean_test.multiply(b, b)
  let difference = @clean_test.add(a_squared, -b_squared)
  
  // 使用因式分解计算 (a + b)(a - b)
  let sum = @clean_test.add(a, b)
  let diff = @clean_test.add(a, -b)
  let factored_form = @clean_test.multiply(sum, diff)
  
  @clean_test.assert_eq(difference, factored_form)
  
  // 验证结果正确性
  @clean_test.assert_eq(200, difference)  // 15² - 5² = 225 - 25 = 200
}

// 测试用例 5: 边界值测试
///|
test "boundary_values" {
  // 测试接近Int32边界的值
  let near_max = 2147483646  // 接近最大值
  let near_min = -2147483647 // 接近最小值
  
  // 测试加法的边界
  @clean_test.assert_eq(2147483647, @clean_test.add(near_max, 1))     // 达到最大值
  @clean_test.assert_eq(2147483647, @clean_test.add(near_max, 2))     // 溢出
  @clean_test.assert_eq(-2147483648, @clean_test.add(near_min, -1))   // 达到最小值
  @clean_test.assert_eq(-2147483648, @clean_test.add(near_min, -2))   // 溢出
  
  // 测试乘法的边界
  @clean_test.assert_eq(2147483646, @clean_test.multiply(near_max, 1))  // 保持不变
  @clean_test.assert_eq(-2147483647, @clean_test.multiply(near_min, 1)) // 保持不变
}

// 测试用例 6: 组合运算测试
///|
test "combined_operations" {
  // 测试 (a + b) * c - d 的复杂运算
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  // 分步计算
  let sum_ab = @clean_test.add(a, b)           // 10 + 20 = 30
  let product_abc = @clean_test.multiply(sum_ab, c)  // 30 * 3 = 90
  let result = @clean_test.add(product_abc, -d)     // 90 - 15 = 75
  
  @clean_test.assert_eq(75, result)
  
  // 测试另一个组合: a * (b + c) - d
  let sum_bc = @clean_test.add(b, c)           // 20 + 3 = 23
  let product_a_bc = @clean_test.multiply(a, sum_bc)  // 10 * 23 = 230
  let result2 = @clean_test.add(product_a_bc, -d)     // 230 - 15 = 215
  
  @clean_test.assert_eq(215, result2)
}

// 测试用例 7: 递归运算模拟
///|
test "recursive_simulation" {
  // 模拟递归计算数列的和: 1 + 2 + 3 + ... + n
  
  // 计算1到6的和
  let sum1 = @clean_test.add(1, 2)           // 3
  let sum2 = @clean_test.add(sum1, 3)        // 6
  let sum3 = @clean_test.add(sum2, 4)        // 10
  let sum4 = @clean_test.add(sum3, 5)        // 15
  let sum5 = @clean_test.add(sum4, 6)        // 21
  
  @clean_test.assert_eq(21, sum5)
  
  // 验证公式: n(n+1)/2 = 6*7/2 = 21
  let n = 6
  let n_plus_1 = @clean_test.add(n, 1)        // 7
  let numerator = @clean_test.multiply(n, n_plus_1)  // 42
  let formula_result = @clean_test.multiply(numerator, 1) / 2  // 这里简化为除以2
  
  @clean_test.assert_eq(21, formula_result)
}

// 测试用例 8: 实际应用场景 - 温度转换
///|
test "temperature_conversion" {
  // 摄氏度转华氏度: F = C * 9/5 + 32
  
  let celsius = 25
  let celsius_times_9 = @clean_test.multiply(celsius, 9)  // 225
  let divided_by_5 = @clean_test.multiply(celsius_times_9, 1) / 5  // 45
  let fahrenheit = @clean_test.add(divided_by_5, 32)  // 77
  
  @clean_test.assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9
  let fahrenheit2 = 77
  let f_minus_32 = @clean_test.add(fahrenheit2, -32)  // 45
  let times_5 = @clean_test.multiply(f_minus_32, 5)  // 225
  let celsius2 = @clean_test.multiply(times_5, 1) / 9  // 25
  
  @clean_test.assert_eq(25, celsius2)
}

// 测试用例 9: 单位换算
///|
test "unit_conversion" {
  // 公里转英里: 1公里 = 0.621371英里
  // 这里简化为整数运算: 100公里 = 62英里
  
  let kilometers = 100
  let miles = @clean_test.multiply(kilometers, 62) / 100  // 62
  
  @clean_test.assert_eq(62, miles)
  
  // 英里转公里: 1英里 = 1.60934公里
  // 这里简化为整数运算: 62英里 = 100公里
  let miles2 = 62
  let kilometers2 = @clean_test.multiply(miles2, 100) / 62  // 100
  
  @clean_test.assert_eq(100, kilometers2)
}

// 测试用例 10: 实际应用场景 - 财务计算
///|
test "financial_calculations" {
  // 复利计算: A = P(1 + r/n)^(nt)
  // 简化版本: 计算两年后的本金翻倍
  
  let principal = 10000      // 本金
  let rate_percent = 50      // 年利率50%
  
  // 第一年后的金额: P + P * r = P * (1 + r)
  let year1_amount = @clean_test.add(principal, @clean_test.multiply(principal, rate_percent))
  
  // 第二年后的金额: year1_amount + year1_amount * r
  let year2_amount = @clean_test.add(year1_amount, @clean_test.multiply(year1_amount, rate_percent))
  
  // 验证结果: 10000 * (1 + 0.5)^2 = 10000 * 2.25 = 22500
  @clean_test.assert_eq(22500, year2_amount)
}
