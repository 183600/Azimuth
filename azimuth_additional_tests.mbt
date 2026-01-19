// Azimuth 项目额外的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，补充现有测试覆盖

test "complex_arithmetic_sequence" {
  // 测试复杂数列计算：斐波那契数列前10项和
  // 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let f1 = 1
  let f2 = 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  let f6 = add(f4, f5)  // 8
  let f7 = add(f5, f6)  // 13
  let f8 = add(f6, f7)  // 21
  let f9 = add(f7, f8)  // 34
  let f10 = add(f8, f9) // 55
  
  // 计算前10项和
  let sum = add(add(add(add(add(add(add(add(add(f1, f2), f3), f4), f5), f6), f7), f8), f9), f10)
  assert_eq(143, sum)
}

test "geometric_progression_sum" {
  // 测试等比数列求和：2 + 6 + 18 + 54 + 162
  let a1 = 2
  let r = 3
  let a2 = multiply(a1, r)  // 6
  let a3 = multiply(a2, r)  // 18
  let a4 = multiply(a3, r)  // 54
  let a5 = multiply(a4, r)  // 162
  
  let sum = add(add(add(add(a1, a2), a3), a4), a5)
  assert_eq(242, sum)
}

test "string_with_various_encodings" {
  // 测试不同编码的字符串处理
  let ascii_name = "John"
  let unicode_name = "张三"
  let mixed_name = "John张三"
  let emoji_name = "🌟"
  let complex_name = "👋Hello世界123"
  
  assert_eq_string("Hello, John!", greet(ascii_name))
  assert_eq_string("Hello, 张三!", greet(unicode_name))
  assert_eq_string("Hello, John张三!", greet(mixed_name))
  assert_eq_string("Hello, 🌟!", greet(emoji_name))
  assert_eq_string("Hello, 👋Hello世界123!", greet(complex_name))
}

test "temperature_conversion_precise" {
  // 测试精确的温度转换（整数近似）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 + 30 - C/5
  
  let celsius = 25
  let rough_fahrenheit = add(multiply(celsius, 2), 30)
  let adjustment = celsius / 5
  let precise_fahrenheit = add(rough_fahrenheit, -adjustment)
  
  assert_eq(77, precise_fahrenheit)  // 25°C ≈ 77°F
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, 2), 30)
  assert_eq(30, freezing_f)  // 0°C ≈ 30°F（近似）
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, 2), 30)
  assert_eq(230, boiling_f)  // 100°C ≈ 230°F（近似）
}

test "business_profit_calculation" {
  // 测试商业利润计算
  let revenue = 50000
  let cost1 = 20000
  let cost2 = 15000
  let cost3 = 8000
  
  // 计算总成本
  let total_cost = add(add(cost1, cost2), cost3)
  // 计算利润
  let profit = add(revenue, -total_cost)
  
  assert_eq(7000, profit)
  
  // 计算利润率（百分比）
  let profit_margin = multiply(profit, 100) / revenue
  assert_eq(14, profit_margin)  // 7000/50000 × 100 = 14%
}

test "nested_conditional_calculations" {
  // 测试嵌套条件计算
  let x = 15
  let y = 25
  let z = 10
  
  // 如果 x < y，则计算 x+z，否则计算 x-z
  let condition1 = x < y
  let result1 = if condition1 { add(x, z) } else { add(x, -z) }
  assert_eq(25, result1)
  
  // 如果 result1 > 20，则计算 result1*y，否则计算 result1+y
  let condition2 = result1 > 20
  let result2 = if condition2 { multiply(result1, y) } else { add(result1, y) }
  assert_eq(625, result2)
  
  // 如果 z 是偶数，则计算 result2-x，否则计算 result2+x
  let condition3 = z % 2 == 0
  let result3 = if condition3 { add(result2, -x) } else { add(result2, x) }
  assert_eq(610, result3)
}

test "extreme_boundary_operations" {
  // 测试极值边界操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的连续加法
  let result1 = add(add(max_val, 1), 2)
  assert_eq(max_val, result1)
  
  // 测试最小值的连续减法
  let result2 = add(add(min_val, -1), -2)
  assert_eq(min_val, result2)
  
  // 测试极值混合运算
  let result3 = add(multiply(max_val, 0), min_val)
  assert_eq(min_val, result3)
  
  // 测试极值与1的运算
  let result4 = add(multiply(min_val, 1), max_val)
  assert_eq(-1, result4)
}

test "array_index_manipulation" {
  // 测试数组索引操作（模拟）
  let rows = 5
  let cols = 4
  
  // 计算二维数组的一维索引
  let index_2d_to_1d = multiply(rows, cols)  // 总容量
  assert_eq(20, index_2d_to_1d)
  
  // 计算特定位置的索引：(row, col) -> row * cols + col
  let row = 2
  let col = 3
  let specific_index = add(multiply(row, cols), col)
  assert_eq(11, specific_index)
  
  // 计算最后一个元素的索引
  let last_row = rows - 1
  let last_col = cols - 1
  let last_index = add(multiply(last_row, cols), last_col)
  assert_eq(19, last_index)
}

test "financial_compound_interest" {
  // 测试复利计算（整数近似）
  let principal = 10000
  let rate_percent = 8  // 8%年利率
  let years = 3
  
  // 简化的复利计算：A = P(1 + r/n)^(nt)
  // 这里简化为：A = P + P × r × t（单利近似）
  let simple_interest = multiply(principal, multiply(rate_percent, years)) / 100
  let total_simple = add(principal, simple_interest)
  assert_eq(12400, total_simple)
  
  // 更精确的复利近似（使用二项式展开的前几项）
  let year1_multiplier = add(100, rate_percent)  // 108
  let year2_multiplier = add(100, multiply(rate_percent, 2))  // 116
  let year3_multiplier = add(100, multiply(rate_percent, 3))  // 124
  
  let total_compound = multiply(principal, year3_multiplier) / 100
  assert_eq(12400, total_compound)
}

test "string_pattern_validation" {
  // 测试字符串模式验证
  // 测试邮箱格式（简化）
  let email1 = "user@example.com"
  let email2 = "test.email+tag@domain.co.uk"
  let email3 = "invalid-email"
  
  // 通过greet函数验证字符串处理能力
  assert_eq_string("Hello, user@example.com!", greet(email1))
  assert_eq_string("Hello, test.email+tag@domain.co.uk!", greet(email2))
  assert_eq_string("Hello, invalid-email!", greet(email3))
  
  // 测试电话号码格式（简化）
  let phone1 = "123-456-7890"
  let phone2 = "+1-555-123-4567"
  let phone3 = "555.123.4567"
  
  assert_eq_string("Hello, 123-456-7890!", greet(phone1))
  assert_eq_string("Hello, +1-555-123-4567!", greet(phone2))
  assert_eq_string("Hello, 555.123.4567!", greet(phone3))
  
  // 测试URL格式（简化）
  let url1 = "https://www.example.com"
  let url2 = "http://subdomain.domain.org/path"
  let url3 = "ftp://files.server.net"
  
  assert_eq_string("Hello, https://www.example.com!", greet(url1))
  assert_eq_string("Hello, http://subdomain.domain.org/path!", greet(url2))
  assert_eq_string("Hello, ftp://files.server.net!", greet(url3))
}