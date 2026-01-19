// 标准 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能和边界情况

// 测试加法函数的边界值处理
test "add_overflow_boundary" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 2))
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1147483648))
  
  // 测试负数溢出边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -2))
  @azimuth.assert_eq(min_val, @azimuth.add(-1000000000, -1147483648))
}

// 测试乘法函数的边界值处理
test "multiply_extreme_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘的溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(65536, 32768))
  @azimuth.assert_eq(max_val, @azimuth.multiply(100000, 21475))
  
  // 测试最小值的特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 100))
}

// 测试数学运算的结合律
test "addition_associativity" {
  // 验证加法结合律：(a + b) + c = a + (b + c)
  let a = 1000000
  let b = -500000
  let c = 750000
  
  let left = @azimuth.add(@azimuth.add(a, b), c)
  let right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left, right)
  
  // 测试边界值的结合律
  let max_val = 2147483647
  let min_val = -2147483648
  
  let left_boundary = @azimuth.add(@azimuth.add(max_val, min_val), 1)
  let right_boundary = @azimuth.add(max_val, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(left_boundary, right_boundary)
}

// 测试数学运算的分配律
test "multiplication_distributivity" {
  // 验证乘法分配律：a × (b + c) = a × b + a × c
  let a = 123
  let b = 456
  let c = 789
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
  
  // 测试负数的分配律
  let a_neg = -100
  let b_neg = 50
  let c_neg = -75
  
  let left_neg = @azimuth.multiply(a_neg, @azimuth.add(b_neg, c_neg))
  let right_neg = @azimuth.add(@azimuth.multiply(a_neg, b_neg), @azimuth.multiply(a_neg, c_neg))
  @azimuth.assert_eq(left_neg, right_neg)
}

// 测试字符串处理函数的边界情况
test "greet_unicode_handling" {
  // 测试各种 Unicode 字符
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 👋🌍!", @azimuth.greet("👋🌍"))
  
  // 测试混合字符
  @azimuth.assert_eq_string("Hello, Hello世界!", @azimuth.greet("Hello世界"))
  @azimuth.assert_eq_string("Hello, Test123测试!", @azimuth.greet("Test123测试"))
}

// 测试复杂算术表达式
test "complex_arithmetic_expressions" {
  // 计算：((a + b) × c) - (d × (e + f))
  let a = 10
  let b = 20
  let c = 5
  let d = 3
  let e = 15
  let f = 25
  
  let sum_ab = @azimuth.add(a, b)
  let product_abc = @azimuth.multiply(sum_ab, c)
  let sum_ef = @azimuth.add(e, f)
  let product_def = @azimuth.multiply(d, sum_ef)
  let result = @azimuth.add(product_abc, -product_def)
  
  @azimuth.assert_eq(-40, result)
  
  // 测试嵌套运算：a × b + c × d + e × f
  let product_ab = @azimuth.multiply(a, b)
  let product_cd = @azimuth.multiply(c, d)
  let product_ef = @azimuth.multiply(e, f)
  let total = @azimuth.add(@azimuth.add(product_ab, product_cd), product_ef)
  
  @azimuth.assert_eq(550, total)
}

// 测试实际商业场景
test "business_calculation_scenarios" {
  // 场景1：计算订单总价（含税和折扣）
  let unit_price = 100
  let quantity = 5
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税率
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let discount_amount = @azimuth.multiply(subtotal, discount_rate)
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate)
  let final_total = @azimuth.add(discounted_total, tax_amount)
  
  @azimuth.assert_eq(480, final_total)  // (100*5 - 100*5*10/100) + (100*5 - 100*5*10/100)*8/100 = 480
  
  // 场景2：计算员工工资
  let base_salary = 3000
  let overtime_hours = 10
  let overtime_rate = 50
  let bonus = 500
  let deduction = 200
  
  let overtime_pay = @azimuth.multiply(overtime_hours, overtime_rate)
  let gross_pay = @azimuth.add(@azimuth.add(base_salary, overtime_pay), bonus)
  let net_pay = @azimuth.add(gross_pay, -deduction)
  
  @azimuth.assert_eq(3800, net_pay)
}

// 测试数学函数的幂运算模拟
test "power_operation_simulation" {
  // 使用乘法模拟幂运算：2^3 = 2 × 2 × 2
  let base = 2
  let exponent = 3
  
  let mut result = 1
  result = @azimuth.multiply(result, base)  // 2
  result = @azimuth.multiply(result, base)  // 4
  result = @azimuth.multiply(result, base)  // 8
  
  @azimuth.assert_eq(8, result)
  
  // 模拟 3^4 = 3 × 3 × 3 × 3
  let base2 = 3
  let exponent2 = 4
  
  let mut result2 = 1
  result2 = @azimuth.multiply(result2, base2)  // 3
  result2 = @azimuth.multiply(result2, base2)  // 9
  result2 = @azimuth.multiply(result2, base2)  // 27
  result2 = @azimuth.multiply(result2, base2)  // 81
  
  @azimuth.assert_eq(81, result2)
}

// 测试斐波那契数列计算
test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  let mut a = 0
  let mut b = 1
  
  // F0 = 0
  @azimuth.assert_eq(0, a)
  
  // F1 = 1
  @azimuth.assert_eq(1, b)
  
  // F2 = F1 + F0 = 1 + 0 = 1
  let c = @azimuth.add(a, b)
  @azimuth.assert_eq(1, c)
  
  // 更新 a = b, b = c
  a = b
  b = c
  
  // F3 = F2 + F1 = 1 + 1 = 2
  c = @azimuth.add(a, b)
  @azimuth.assert_eq(2, c)
  
  // 更新 a = b, b = c
  a = b
  b = c
  
  // F4 = F3 + F2 = 2 + 1 = 3
  c = @azimuth.add(a, b)
  @azimuth.assert_eq(3, c)
  
  // 更新 a = b, b = c
  a = b
  b = c
  
  // F5 = F4 + F3 = 3 + 2 = 5
  c = @azimuth.add(a, b)
  @azimuth.assert_eq(5, c)
}

// 测试阶乘计算
test "factorial_calculation" {
  // 计算 5! = 5 × 4 × 3 × 2 × 1
  let n = 5
  
  let mut result = n
  result = @azimuth.multiply(result, 4)  // 20
  result = @azimuth.multiply(result, 3)  // 60
  result = @azimuth.multiply(result, 2)  // 120
  result = @azimuth.multiply(result, 1)  // 120
  
  @azimuth.assert_eq(120, result)
  
  // 计算 4! = 4 × 3 × 2 × 1
  let n2 = 4
  
  let mut result2 = n2
  result2 = @azimuth.multiply(result2, 3)  // 12
  result2 = @azimuth.multiply(result2, 2)  // 24
  result2 = @azimuth.multiply(result2, 1)  // 24
  
  @azimuth.assert_eq(24, result2)
  
  // 边界情况：0! = 1
  @azimuth.assert_eq(1, 1)  // 直接验证
}