// 额外测试用例 - 补充现有测试的覆盖范围

///|
test "fibonacci_sequence_operations" {
  // 测试斐波那契数列中的运算
  let fib_10 = 55
  let fib_11 = 89
  let fib_12 = 144
  
  // 斐波那契数列性质：F(n) + F(n+1) = F(n+2)
  assert_eq(fib_12, add(fib_10, fib_11))
  
  // 斐波那契数列乘法性质
  assert_eq(4895, multiply(fib_10, fib_11))
}

///|
test "prime_number_operations" {
  // 测试质数运算
  let prime_7 = 7
  let prime_11 = 11
  let prime_13 = 13
  let prime_17 = 17
  
  // 质数加法
  assert_eq(18, add(prime_7, prime_11))
  assert_eq(24, add(prime_11, prime_13))
  
  // 质数乘法
  assert_eq(77, multiply(prime_7, prime_11))
  assert_eq(143, multiply(prime_11, prime_13))
  assert_eq(221, multiply(prime_13, prime_17))
}

///|
test "geometric_progression" {
  // 测试等比数列运算
  let first_term = 3
  let ratio = 4
  
  // 生成等比数列：3, 12, 48, 192, 768
  let term_2 = multiply(first_term, ratio)      // 12
  let term_3 = multiply(term_2, ratio)          // 48
  let term_4 = multiply(term_3, ratio)          // 192
  let term_5 = multiply(term_4, ratio)          // 768
  
  assert_eq(12, term_2)
  assert_eq(48, term_3)
  assert_eq(192, term_4)
  assert_eq(768, term_5)
  
  // 测试等比数列求和（前5项）
  let sum = add(first_term, add(term_2, add(term_3, add(term_4, term_5))))
  assert_eq(1023, sum)
}

///|
test "alternating_sign_sequence" {
  // 测试交替符号序列的运算
  let mut result = 0
  
  // 交替加减：+1 -2 +3 -4 +5 -6 +7 -8 +9 -10
  result = add(result, 1)    // 1
  result = add(result, -2)   // -1
  result = add(result, 3)    // 2
  result = add(result, -4)   // -2
  result = add(result, 5)    // 3
  result = add(result, -6)   // -3
  result = add(result, 7)    // 4
  result = add(result, -8)   // -4
  result = add(result, 9)    // 5
  result = add(result, -10)  // -5
  
  assert_eq(-5, result)
}

///|
test "nested_function_calls" {
  // 测试嵌套函数调用
  let a = 5
  let b = 10
  let c = 15
  let d = 20
  
  // 复杂嵌套：add(multiply(a, b), multiply(c, d))
  let result1 = add(multiply(a, b), multiply(c, d))
  assert_eq(350, result1)  // 5*10 + 15*20 = 50 + 300 = 350
  
  // 更复杂的嵌套：multiply(add(a, b), add(c, d))
  let result2 = multiply(add(a, b), add(c, d))
  assert_eq(525, result2)  // (5+10)*(15+20) = 15*35 = 525
  
  // 三层嵌套：add(multiply(a, add(b, c)), d)
  let result3 = add(multiply(a, add(b, c)), d)
  assert_eq(145, result3)  // 5*(10+15) + 20 = 5*25 + 20 = 125 + 20 = 145
}

///|
test "string_length_edge_cases" {
  // 测试字符串长度边界情况
  let empty = ""
  let single_char = "A"
  let long_string = "This is a very long string to test the greet function with various length inputs"
  
  assert_eq("Hello, !", greet(empty))
  assert_eq("Hello, A!", greet(single_char))
  assert_eq("Hello, This is a very long string to test the greet function with various length inputs!", greet(long_string))
}

///|
test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // 平方差公式：(x+y)*(x-y) = x² - y²
  let diff_of_squares = multiply(add(x, y), add(x, -y))
  let x_squared = multiply(x, x)
  let y_squared = multiply(y, y)
  let expected = add(x_squared, -y_squared)
  
  assert_eq(expected, diff_of_squares)
  
  // 两数和的平方：(x+y)² = x² + 2xy + y²
  let sum_squared = multiply(add(x, y), add(x, y))
  let two_xy = multiply(2, multiply(x, y))
  let sum_expected = add(add(x_squared, two_xy), y_squared)
  
  assert_eq(sum_expected, sum_squared)
}

///|
test "overflow_recovery_patterns" {
  // 测试溢出恢复模式
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 从溢出状态恢复
  let mut value = max_val
  value = add(value, 100)  // 应该保持最大值
  assert_eq(max_val, value)
  
  value = add(value, -100)  // 应该可以正常减法
  assert_eq(max_val - 100, value)
  
  // 从负溢出状态恢复
  value = min_val
  value = add(value, -100)  // 应该保持最小值
  assert_eq(min_val, value)
  
  value = add(value, 100)  // 应该可以正常加法
  assert_eq(min_val + 100, value)
}

///|
test "circular_reference_patterns" {
  // 测试循环引用模式
  let a = 10
  let b = 20
  let c = 30
  
  // 创建循环计算：a -> b -> c -> a
  let step1 = add(a, b)     // 30
  let step2 = multiply(step1, c)  // 900
  let step3 = add(step2, -a)  // 890
  let final_result = multiply(step3, b)  // 17800
  
  assert_eq(17800, final_result)
}