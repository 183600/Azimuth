// Azimuth 项目高级测试用例
// 使用标准 MoonBit 测试语法，包含高级数学运算和边界条件测试

test "mathematical_associative_law" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 12
  let b = 15
  let c = 23
  
  let left_side = azimuth::add(azimuth::add(a, b), c)  // (12 + 15) + 23 = 50
  let right_side = azimuth::add(a, azimuth::add(b, c))  // 12 + (15 + 23) = 50
  
  assert_eq(left_side, right_side)
  
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let x = 3
  let y = 4
  let z = 5
  
  let mult_left = azimuth::multiply(azimuth::multiply(x, y), z)  // (3 * 4) * 5 = 60
  let mult_right = azimuth::multiply(x, azimuth::multiply(y, z))  // 3 * (4 * 5) = 60
  
  assert_eq(mult_left, mult_right)
}

test "string_unicode_handling" {
  // 测试 Unicode 字符串处理
  let unicode_inputs = ["测试", "🚀", "Café", "Москва", "العربية"]
  
  for input in unicode_inputs {
    let result = azimuth::greet(input)
    let expected = "Hello, " + input + "!"
    assert_eq(result, expected)
  }
}

test "large_number_arithmetic" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 大数加法
  let sum_result = azimuth::add(large_num1, large_num2)
  assert_eq(sum_result, 3000000)
  
  // 大数乘法（使用较小的数避免溢出）
  let mult_result = azimuth::multiply(10000, 5000)
  assert_eq(mult_result, 50000000)
  
  // 大数与零的运算
  assert_eq(azimuth::add(large_num1, 0), large_num1)
  assert_eq(azimuth::multiply(large_num1, 0), 0)
}

test "prime_number_calculation" {
  // 测试素数相关计算
  // 使用加法验证素数性质
  let primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
  
  // 验证素数序列的连续差值
  for i in 1..length(primes) {
    let diff = azimuth::add(primes[i], -primes[i-1])
    // 素数差值应该大于0（除了2和3之间差1）
    assert_eq(diff > 0, true)
  }
  
  // 验证特定素数性质
  assert_eq(azimuth::add(primes[0], primes[1]), 5)  // 2 + 3 = 5 (也是素数)
  assert_eq(azimuth::multiply(primes[1], primes[2]), 15)  // 3 * 5 = 15
}

test "recursive_calculation_simulation" {
  // 测试递归计算模拟（使用迭代方式）
  // 计算阶乘: 5! = 5 * 4 * 3 * 2 * 1 = 120
  let n = 5
  let factorial = 1
  
  // 模拟递归计算
  factorial = azimuth::multiply(factorial, 5)  // 1 * 5 = 5
  factorial = azimuth::multiply(factorial, 4)  // 5 * 4 = 20
  factorial = azimuth::multiply(factorial, 3)  // 20 * 3 = 60
  factorial = azimuth::multiply(factorial, 2)  // 60 * 2 = 120
  factorial = azimuth::multiply(factorial, 1)  // 120 * 1 = 120
  
  assert_eq(factorial, 120)
}

test "string_length_boundary_cases" {
  // 测试字符串长度边界情况
  let empty_string = ""
  let single_char = "a"
  let long_string = "this_is_a_very_long_string_for_testing_purposes"
  
  let result1 = azimuth::greet(empty_string)
  assert_eq(result1, "Hello, !")
  
  let result2 = azimuth::greet(single_char)
  assert_eq(result2, "Hello, a!")
  
  let result3 = azimuth::greet(long_string)
  assert_eq(result3, "Hello, this_is_a_very_long_string_for_testing_purposes!")
}

test "power_operation_simulation" {
  // 测试幂运算模拟（使用重复乘法）
  // 计算 2^4 = 2 * 2 * 2 * 2 = 16
  let base = 2
  let exponent = 4
  let result = 1
  
  result = azimuth::multiply(result, base)  // 1 * 2 = 2
  result = azimuth::multiply(result, base)  // 2 * 2 = 4
  result = azimuth::multiply(result, base)  // 4 * 2 = 8
  result = azimuth::multiply(result, base)  // 8 * 2 = 16
  
  assert_eq(result, 16)
  
  // 计算 3^3 = 3 * 3 * 3 = 27
  let base2 = 3
  let result2 = 1
  
  result2 = azimuth::multiply(result2, base2)  // 1 * 3 = 3
  result2 = azimuth::multiply(result2, base2)  // 3 * 3 = 9
  result2 = azimuth::multiply(result2, base2)  // 9 * 3 = 27
  
  assert_eq(result2, 27)
}

test "negative_number_boundary_tests" {
  // 测试负数边界情况
  let max_negative = -2147483648  // 32位有符号整数最小值
  let min_negative = -1
  
  // 负数加法测试
  let result1 = azimuth::add(max_negative, 1)
  let result2 = azimuth::add(min_negative, -1)
  
  // 负数乘法测试
  let result3 = azimuth::multiply(max_negative, 1)
  let result4 = azimuth::multiply(min_negative, -1)
  
  // 验证负数运算性质
  assert_eq(azimuth::add(-10, 20), 10)  // 负数加正数
  assert_eq(azimuth::multiply(-5, -5), 25)  // 负数乘负数得正数
  assert_eq(azimuth::multiply(-5, 3), -15)  // 负数乘正数得负数
}

test "string_special_character_processing" {
  // 测试字符串特殊字符处理
  let special_chars = [
    "line\nbreak",
    "tab\tcharacter", 
    "quote\"test",
    "apostrophe'test",
    "back\\slash",
    "emoji😀test",
    "space test",
    "bracket(test)",
    "curly{test}",
    "square[test]"
  ]
  
  for char_string in special_chars {
    let result = azimuth::greet(char_string)
    let expected = "Hello, " + char_string + "!"
    assert_eq(result, expected)
  }
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // 表达式1: (a + b) * (c - d) + e
  let a = 8
  let b = 12
  let c = 20
  let d = 5
  let e = 15
  
  let step1 = azimuth::add(a, b)           // 8 + 12 = 20
  let step2 = azimuth::add(c, -d)          // 20 - 5 = 15
  let step3 = azimuth::multiply(step1, step2)  // 20 * 15 = 300
  let result1 = azimuth::add(step3, e)     // 300 + 15 = 315
  
  assert_eq(result1, 315)
  
  // 表达式2: a * b + c * d - e * f
  let f = 2
  
  let expr1 = azimuth::multiply(a, b)      // 8 * 12 = 96
  let expr2 = azimuth::multiply(c, d)      // 20 * 5 = 100
  let expr3 = azimuth::multiply(e, f)      // 15 * 2 = 30
  let result2 = azimuth::add(azimuth::add(expr1, expr2), -expr3)  // 96 + 100 - 30 = 166
  
  assert_eq(result2, 166)
}