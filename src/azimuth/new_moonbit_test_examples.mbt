// 新的 MoonBit 测试用例示例
// 包含各种实际应用场景和边界条件测试

test "large_number_calculations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 大数相加
  let sum_result = add(large_num1, large_num2)
  assert_eq(3000000, sum_result)
  
  // 大数相乘
  let product_result = multiply(large_num1, 2)
  assert_eq(2000000, product_result)
  
  // 复合大数运算
  let complex_result = add(multiply(large_num1, 3), multiply(large_num2, 2))
  assert_eq(7000000, complex_result)
}

test "mathematical_formulas" {
  // 测试数学公式计算
  
  // 计算圆的周长（简化）：2 × π × r ≈ 2 × 3 × r
  let radius = 10
  let circumference = multiply(multiply(2, 3), radius)
  assert_eq(60, circumference)
  
  // 计算圆的面积（简化）：π × r² ≈ 3 × r²
  let area = multiply(3, multiply(radius, radius))
  assert_eq(300, area)
  
  // 计算等差数列和：n × (a1 + an) / 2
  let n = 10
  let first_term = 1
  let last_term = 10
  let series_sum = multiply(n, add(first_term, last_term)) / 2
  assert_eq(55, series_sum)
}

test "financial_calculations" {
  // 测试金融计算场景
  
  // 简单利息计算：本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3   // 3年
  let simple_interest = multiply(principal, multiply(rate, time))
  assert_eq(150000, simple_interest)  // 10000 × 5 × 3 = 150000
  
  // 复利计算（简化）：本金 × (1 + 利率 × 时间)
  let compound_amount = multiply(principal, add(1, multiply(rate, time)))
  assert_eq(160000, compound_amount)  // 10000 × (1 + 5×3) = 160000
  
  // 贷款月供计算（简化）：贷款总额 / 月数 + 利息
  let loan_amount = 120000
  let months = 12
  let monthly_interest = multiply(loan_amount, rate) / 100
  let monthly_payment = add(loan_amount / months, monthly_interest / months)
  assert_eq(10500, monthly_payment)
}

test "boundary_value_testing" {
  // 测试边界值
  
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(max_val, -1))
  
  // 最小值边界测试
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 零值边界测试
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  
  // 空字符串处理
  let empty_result = greet("")
  assert_eq_string("Hello, !", empty_result)
  
  // 单字符字符串
  let single_char = greet("A")
  assert_eq_string("Hello, A!", single_char)
  
  // 长字符串处理
  let long_name = "ThisIsAVeryLongNameThatTestsStringHandlingCapabilities"
  let long_result = greet(long_name)
  assert_eq_string("Hello, " + long_name + "!", long_result)
  
  // 特殊字符处理
  let special_chars = greet("!@#$%^&*()")
  assert_eq_string("Hello, !@#$%^&*()!", special_chars)
  
  // Unicode字符处理
  let unicode_chars = greet("测试中文")
  assert_eq_string("Hello, 测试中文!", unicode_chars)
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  
  // 计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = add(a, b)           // 15
  let diff_cd = add(c, -d)         // 12
  let product_ef = multiply(e, f)  // 21
  let product_ab_cd = multiply(sum_ab, diff_cd)  // 180
  let final_result = add(product_ab_cd, product_ef)  // 201
  
  assert_eq(201, final_result)
  
  // 计算平方和：a² + b² + c²
  let square_a = multiply(a, a)    // 100
  let square_b = multiply(b, b)    // 25
  let square_c = multiply(c, c)    // 400
  let sum_of_squares = add(add(square_a, square_b), square_c)
  
  assert_eq(525, sum_of_squares)
}

test "conditional_logic_calculations" {
  // 测试条件逻辑计算
  
  let x = 15
  let y = 20
  let z = 25
  
  // 条件1：如果 x < y，则计算 x + y，否则计算 x × y
  let condition1 = x < y
  let result1 = if condition1 { add(x, y) } else { multiply(x, y) }
  assert_eq(35, result1)  // 15 < 20 为真，所以 15 + 20 = 35
  
  // 条件2：如果 y > z，则计算 y - z，否则计算 y + z
  let condition2 = y > z
  let result2 = if condition2 { add(y, -z) } else { add(y, z) }
  assert_eq(45, result2)  // 20 > 25 为假，所以 20 + 25 = 45
  
  // 嵌套条件：如果 x < y 且 y < z，则计算 x + y + z
  let condition3 = x < y && y < z
  let result3 = if condition3 { add(add(x, y), z) } else { multiply(x, y) }
  assert_eq(60, result3)  // 15 < 20 且 20 < 25 为真，所以 15 + 20 + 25 = 60
}

test "data_conversion_scenarios" {
  // 测试数据转换场景
  
  // 数字到字符串的间接转换
  let year = 2024
  let year_string = "2024"
  let greeting = greet(year_string)
  assert_eq_string("Hello, 2024!", greeting)
  
  // 布尔值到字符串的转换
  let true_string = "true"
  let false_string = "false"
  assert_eq_string("Hello, true!", greet(true_string))
  assert_eq_string("Hello, false!", greet(false_string))
  
  // 模拟浮点数表示
  let pi_integer = 3
  let pi_decimal = "14"
  let pi_string = "3.14"
  assert_eq_string("Hello, 3.14!", greet(pi_string))
  
  // 日期格式字符串
  let date_string = "2024-01-15"
  assert_eq_string("Hello, 2024-01-15!", greet(date_string))
}

test "performance_related_calculations" {
  // 测试性能相关计算
  
  // 大量数据累加：1 + 2 + 3 + ... + 100
  // 使用公式：n × (n + 1) / 2
  let n = 100
  let sum_1_to_100 = multiply(n, add(n, 1)) / 2
  assert_eq(5050, sum_1_to_100)
  
  // 等比数列求和：2 + 4 + 8 + 16 + 32
  // 手动计算
  let geo_sum = add(add(add(add(2, 4), 8), 16), 32)
  assert_eq(62, geo_sum)
  
  // 阶乘计算（5!）：5 × 4 × 3 × 2 × 1
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_5)
  
  // 幂运算（2^10）：通过连续相乘实现
  let power_2_8 = multiply(multiply(multiply(2, 2), multiply(2, 2)), multiply(multiply(2, 2), multiply(2, 2)))
  assert_eq(256, power_2_8)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护验证
  assert_true(add(max_val, 100) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, 2) == min_val)
  
  // 特殊值处理验证
  assert_true(multiply(min_val, -1) == min_val)  // 最小值乘以-1的特殊情况
  assert_true(add(0, max_val) == max_val)
  assert_true(add(0, min_val) == min_val)
  assert_true(multiply(0, max_val) == 0)
  assert_true(multiply(0, min_val) == 0)
  
  // 断言函数验证
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 函数结果验证
  let add_result = add(10, 20)
  let multiply_result = multiply(5, 6)
  let greet_result = greet("验证")
  
  assert_true(add_result == 30)
  assert_true(multiply_result == 30)
  assert_true(greet_result == "Hello, 验证!")
}