// 增强的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_property_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = azimuth::add(a, b)
  let result2 = azimuth::add(b, a)
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -12345
  let d = 67890
  
  let result3 = azimuth::add(c, d)
  let result4 = azimuth::add(d, c)
  assert_eq(result3, result4)
}

test "add_property_associativity" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 1000
  let b = 2000
  let c = 3000
  
  let result1 = azimuth::add(azimuth::add(a, b), c)
  let result2 = azimuth::add(a, azimuth::add(b, c))
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -1000
  let e = 2500
  let f = -1500
  
  let result3 = azimuth::add(azimuth::add(d, e), f)
  let result4 = azimuth::add(d, azimuth::add(e, f))
  assert_eq(result3, result4)
}

test "multiply_property_commutativity" {
  // 测试乘法交换律：a * b = b * a
  let a = 123
  let b = 456
  
  let result1 = azimuth::multiply(a, b)
  let result2 = azimuth::multiply(b, a)
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -123
  let d = 456
  
  let result3 = azimuth::multiply(c, d)
  let result4 = azimuth::multiply(d, c)
  assert_eq(result3, result4)
}

test "multiply_property_distributivity" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 10
  let b = 20
  let c = 30
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let d = -10
  let e = 25
  let f = -15
  
  let left_side2 = azimuth::multiply(d, azimuth::add(e, f))
  let right_side2 = azimuth::add(azimuth::multiply(d, e), azimuth::multiply(d, f))
  assert_eq(left_side2, right_side2)
}

test "combined_operations_complex" {
  // 测试复合运算：(a + b) * (c + d) - e * f
  let a = 15
  let b = 25
  let c = 8
  let d = 12
  let e = 10
  let f = 20
  
  let sum1 = azimuth::add(a, b)  // 40
  let sum2 = azimuth::add(c, d)  // 20
  let product1 = azimuth::multiply(sum1, sum2)  // 800
  let product2 = azimuth::multiply(e, f)  // 200
  let result = azimuth::add(product1, -product2)  // 600
  
  assert_eq(600, result)
}

test "string_processing_multilingual" {
  // 测试多语言字符串处理
  let english_name = "John"
  let chinese_name = "张三"
  let japanese_name = "田中"
  let arabic_name = "أحمد"
  let mixed_name = "John张三田中أحمد"
  
  assert_eq_string("Hello, John!", azimuth::greet(english_name))
  assert_eq_string("Hello, 张三!", azimuth::greet(chinese_name))
  assert_eq_string("Hello, 田中!", azimuth::greet(japanese_name))
  assert_eq_string("Hello, أحمد!", azimuth::greet(arabic_name))
  assert_eq_string("Hello, John张三田中أحمد!", azimuth::greet(mixed_name))
}

test "real_world_financial_calculation" {
  // 实际金融计算：贷款利息计算
  let principal = 100000  // 本金 1000元（单位：分）
  let annual_rate = 5     // 年利率 5%
  let months = 12         // 12个月
  
  // 计算月利息：本金 * 年利率 / 12
  let monthly_interest = azimuth::multiply(principal, annual_rate) / azimuth::multiply(12, 100)
  
  // 计算总利息：月利息 * 12
  let total_interest = azimuth::multiply(monthly_interest, months)
  
  // 计算还款总额：本金 + 总利息
  let total_payment = azimuth::add(principal, total_interest)
  
  assert_eq(105000, total_payment)  // 100000 + 5000 = 105000
}

test "boundary_value_extreme_cases" {
  // 极端边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(max_val, azimuth::add(max_val, -min_val))  // max_val + max_val = max_val（溢出保护）
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(max_val, -1))
  
  // 测试最小值的各种运算
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(min_val, azimuth::add(min_val, max_val))   // min_val + max_val = -1
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))   // 特殊情况：min_val * -1 = min_val（溢出保护）
  
  // 测试链式运算
  let chain_result = azimuth::add(
    azimuth::multiply(azimuth::add(max_val, min_val), 2),
    azimuth::multiply(max_val, 0)
  )
  assert_eq(-2, chain_result)  // (-1) * 2 + max_val * 0 = -2 + 0 = -2
}

test "performance_large_number_operations" {
  // 大数运算性能测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = 3000000
  
  // 大数加法
  let sum_result = azimuth::add(azimuth::add(large_num1, large_num2), large_num3)
  assert_eq(6000000, sum_result)
  
  // 大数乘法
  let product_result = azimuth::multiply(large_num1, 3)
  assert_eq(3000000, product_result)
  
  // 复合运算
  let complex_result = azimuth::add(
    azimuth::multiply(large_num1, 2),
    azimuth::multiply(large_num2, 3)
  )
  assert_eq(8000000, complex_result)  // 1000000*2 + 2000000*3 = 2000000 + 6000000 = 8000000
}