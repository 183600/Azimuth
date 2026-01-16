// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含多样化测试场景

test "basic_addition_commutativity" {
  // 测试加法交换律
  let a = 15
  let b = 27
  let result1 = azimuth::add(a, b)
  let result2 = azimuth::add(b, a)
  assert_eq(result1, result2)
}

test "multiplication_identity_property" {
  // 测试乘法单位元性质
  let test_values = [0, 1, 5, 10, 100]
  for value in test_values {
    let result = azimuth::multiply(value, 1)
    assert_eq(result, value)
  }
}

test "string_greeting_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  let special_inputs = ["user@domain.com", "test-name", "12345", "hello_world"]
  for input in special_inputs {
    let expected = "Hello, " + input + "!"
    let result = azimuth::greet(input)
    assert_eq(result, expected)
  }
}

test "arithmetic_sequence_fibonacci" {
  // 测试斐波那契数列计算
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth::add(fib1, fib2)  // 2
  let fib4 = azimuth::add(fib2, fib3)  // 3
  let fib5 = azimuth::add(fib3, fib4)  // 5
  let fib6 = azimuth::add(fib4, fib5)  // 8
  
  assert_eq(fib3, 2)
  assert_eq(fib4, 3)
  assert_eq(fib5, 5)
  assert_eq(fib6, 8)
}

test "complex_calculation_pipeline" {
  // 测试复杂计算管道
  // 计算: (5 * 3) + (10 * 2) - 15
  let step1 = azimuth::multiply(5, 3)  // 15
  let step2 = azimuth::multiply(10, 2) // 20
  let step3 = azimuth::add(step1, step2) // 35
  let result = azimuth::add(step3, -15)  // 20
  
  assert_eq(result, 20)
}

test "negative_number_multiplication" {
  // 测试负数乘法
  let positive = 7
  let negative = -4
  
  let result1 = azimuth::multiply(positive, negative)  // -28
  let result2 = azimuth::multiply(negative, positive)  // -28
  let result3 = azimuth::multiply(negative, negative)  // 16
  
  assert_eq(result1, -28)
  assert_eq(result2, -28)
  assert_eq(result3, 16)
}

test "zero_addition_property" {
  // 测试加法零元性质
  let test_values = [-10, -1, 0, 1, 42]
  for value in test_values {
    let result1 = azimuth::add(value, 0)
    let result2 = azimuth::add(0, value)
    assert_eq(result1, value)
    assert_eq(result2, value)
  }
}

test "string_greeting_edge_cases" {
  // 测试字符串问候边界情况
  let edge_cases = ["", " ", "a", "A", "very_long_name_with_multiple_words"]
  let expected_results = ["Hello, !", "Hello,  !", "Hello, a!", "Hello, A!", "Hello, very_long_name_with_multiple_words!"]
  
  for i in 0..length(edge_cases) {
    let result = azimuth::greet(edge_cases[i])
    assert_eq(result, expected_results[i])
  }
}

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))  // 3 * (4 + 5) = 27
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))  // 3 * 4 + 3 * 5 = 27
  
  assert_eq(left_side, right_side)
}

test "geometric_progression_calculation" {
  // 测试等比数列计算
  let first_term = 2
  let ratio = 3
  
  let term1 = first_term  // 2
  let term2 = azimuth::multiply(term1, ratio)  // 6
  let term3 = azimuth::multiply(term2, ratio)  // 18
  let term4 = azimuth::multiply(term3, ratio)  // 54
  
  assert_eq(term1, 2)
  assert_eq(term2, 6)
  assert_eq(term3, 18)
  assert_eq(term4, 54)
  
  // 计算前4项和
  let sum = azimuth::add(azimuth::add(azimuth::add(term1, term2), term3), term4)
  assert_eq(sum, 80)
}