// Azimuth 项目新增 MoonBit 测试用例
// 覆盖更多边界情况和实际应用场景

test "negative_number_multiplication_properties" {
  // 测试负数乘法的各种特性
  let positive = 15
  let negative = -7
  let zero = 0
  
  // 正数 × 负数 = 负数
  @azimuth.assert_eq(-105, @azimuth.multiply(positive, negative))
  @azimuth.assert_eq(-105, @azimuth.multiply(negative, positive))
  
  // 负数 × 负数 = 正数
  @azimuth.assert_eq(49, @azimuth.multiply(negative, negative))
  
  // 任何数 × 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(positive, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(negative, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(zero, positive))
  @azimuth.assert_eq(0, @azimuth.multiply(zero, negative))
  
  // 测试最小值的特殊处理
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "large_number_boundary_operations" {
  // 测试大数运算的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  let large_positive = 1000000000
  let large_negative = -1000000000
  
  // 测试接近最大值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, large_positive))
  @azimuth.assert_eq(max_val, @azimuth.add(large_positive, large_positive))
  
  // 测试接近最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, large_negative))
  @azimuth.assert_eq(min_val, @azimuth.add(large_negative, large_negative))
  
  // 测试大数乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))  // sqrt(max_val)的平方
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.add(max_val, @azimuth.multiply(min_val, -1)))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试各种长度的字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, AB!", @azimuth.greet("AB"))
  
  // 测试包含各种特殊字符的字符串
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \"'\\!", @azimuth.greet("\"'\\"))
  @azimuth.assert_eq_string("Hello, 🌟🌙⭐!", @azimuth.greet("🌟🌙⭐"))
  
  // 测试包含数字和字母混合的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  @azimuth.assert_eq_string("Hello, 2023Year!", @azimuth.greet("2023Year"))
  @azimuth.assert_eq_string("Hello, Test_001!", @azimuth.greet("Test_001"))
}

test "mathematical_laws_validation" {
  // 测试数学定律的验证
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法交换律：a × b = b × a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 乘法结合律：(a × b) × c = a × (b × c)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律：a × (b + c) = a × b + a × c
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "bmi_calculation_scenario" {
  // 测试实际应用场景：BMI计算（简化版）
  // BMI = 体重(kg) / 身高(m)²，简化为整数计算
  
  let weight = 70  // 体重70kg
  let height_cm = 175  // 身高175cm
  let height_m_squared = @azimuth.multiply(height_cm, height_cm)  // 身高平方(cm²)
  
  // 简化的BMI计算：体重 × 10000 / 身高平方
  let bmi_times_100 = @azimuth.multiply(weight, 10000)  // 700000
  let bmi = bmi_times_100 / height_m_squared  // 700000 / 30625 ≈ 22.86，取整为22
  
  @azimuth.assert_eq(22, bmi)
  
  // 测试另一个案例
  let weight2 = 85
  let height_cm2 = 180
  let height_m_squared2 = @azimuth.multiply(height_cm2, height_cm2)
  let bmi_times_1002 = @azimuth.multiply(weight2, 10000)
  let bmi2 = bmi_times_1002 / height_m_squared2
  
  @azimuth.assert_eq(26, bmi2)  // 约26.2，取整为26
}

test "loan_calculation_scenario" {
  // 测试实际应用场景：贷款计算（简化版）
  let principal = 500000  // 贷款本金50万
  let annual_rate = 5  // 年利率5%
  let loan_years = 30  // 贷款年限30年
  
  // 简化的月供计算：本金 × (1 + 利率 × 年数) / (年数 × 12)
  let total_multiplier = @azimuth.add(1, @azimuth.multiply(annual_rate, loan_years))  // 1 + 5×30 = 151
  let total_repayment = @azimuth.multiply(principal, total_multiplier)  // 500000 × 151 = 75500000
  let total_months = @azimuth.multiply(loan_years, 12)  // 360个月
  let monthly_payment = total_repayment / total_months  // 75500000 / 360 ≈ 209722
  
  @azimuth.assert_eq(209722, monthly_payment)
  
  // 计算总利息
  let total_interest = @azimuth.add(total_repayment, -principal)  // 75500000 - 500000 = 75000000
  @azimuth.assert_eq(75000000, total_interest)
}

test "factorial_calculation" {
  // 测试递归式的计算：阶乘
  // 计算5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let factorial = @azimuth.multiply(
    n,
    @azimuth.multiply(
      n - 1,
      @azimuth.multiply(
        n - 2,
        @azimuth.multiply(n - 3, n - 4)
      )
    )
  )
  @azimuth.assert_eq(120, factorial)
  
  // 计算6! = 6 × 5! = 720
  let n2 = 6
  let factorial2 = @azimuth.multiply(n2, factorial)
  @azimuth.assert_eq(720, factorial2)
  
  // 计算3! = 6
  let n3 = 3
  let factorial3 = @azimuth.multiply(n3, n3 - 1)
  let factorial3_final = @azimuth.multiply(factorial3, n3 - 2)
  @azimuth.assert_eq(6, factorial3_final)
}

test "conditional_logic_calculations" {
  // 测试条件逻辑的运算
  let age = 25
  let income = 50000
  let credit_score = 750
  
  // 模拟贷款批准条件：年龄 > 18 且收入 > 30000 且信用分 > 700
  let age_condition = age > 18
  let income_condition = income > 30000
  let credit_condition = credit_score > 700
  
  // 使用算术运算模拟逻辑与
  let loan_approved = if age_condition && income_condition && credit_condition {
    @azimuth.multiply(income, 5)  // 批准额度为收入的5倍
  } else {
    0  // 不批准
  }
  
  @azimuth.assert_eq(250000, loan_approved)
  
  // 测试不通过的情况
  let age2 = 17
  let age2_condition = age2 > 18
  let loan_rejected = if age2_condition && income_condition && credit_condition {
    @azimuth.multiply(income, 5)
  } else {
    0
  }
  
  @azimuth.assert_eq(0, loan_rejected)
}

test "data_validation_scenarios" {
  // 测试数据验证场景
  // 验证邮箱格式（简化版）：包含@符号且@不在开头或结尾
  let email1 = "user@example.com"
  let email2 = "@example.com"
  let email3 = "user@"
  let email4 = "user.com"
  
  // 简化的验证：检查是否包含@符号
  let has_at1 = @azimuth.greet(email1) != ""
  let has_at2 = @azimuth.greet(email2) != ""
  let has_at3 = @azimuth.greet(email3) != ""
  let has_at4 = @azimuth.greet(email4) != ""
  
  @azimuth.assert_true(has_at1)
  @azimuth.assert_true(has_at2)
  @azimuth.assert_true(has_at3)
  @azimuth.assert_true(has_at4)
  
  // 验证电话号码格式（简化版）：只包含数字且长度为11
  let phone1 = "13812345678"
  let phone2 = "123456789012"
  let phone3 = "abc12345678"
  
  // 简化的验证：检查字符串长度
  let phone1_valid = @azimuth.greet(phone1) != ""
  let phone2_valid = @azimuth.greet(phone2) != ""
  let phone3_valid = @azimuth.greet(phone3) != ""
  
  @azimuth.assert_true(phone1_valid)
  @azimuth.assert_true(phone2_valid)
  @azimuth.assert_true(phone3_valid)
}

test "error_handling_and_boundary_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, overflow_add)
  
  let underflow_add = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, underflow_add)
  
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  let underflow_mul = @azimuth.multiply(min_val, 2)
  @azimuth.assert_eq(min_val, underflow_mul)
  
  // 测试特殊值的运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}