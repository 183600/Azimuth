// 标准核心功能测试用例
// 覆盖 add, multiply, greet 函数的各种场景

test "add_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 12345
  let b = 67890
  
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数的交换律
  let c = -123
  let d = 456
  assert_eq(add(c, d), add(d, c))
}

test "multiply_distributive_property" {
  // 测试乘法分配律: a × (b + c) = a × b + a × c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "greet_function_with_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
}

test "boundary_value_calculations" {
  // 测试边界值的计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的值
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  
  // 测试边界值的乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "sequential_operations" {
  // 测试连续运算
  // 计算: ((1 + 2) × 3) + 4 = 13
  let step1 = add(1, 2)        // 3
  let step2 = multiply(step1, 3)  // 9
  let result = add(step2, 4)    // 13
  
  assert_eq(13, result)
  
  // 计算: (10 × 5) + (3 × 2) = 56
  let result2 = add(multiply(10, 5), multiply(3, 2))
  assert_eq(56, result2)
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  // 测试极长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetFunctionsWithExtendedInputLength"
  let result = greet(long_string)
  assert_eq_string("Hello, " + long_string + "!", result)
  
  // 测试包含数字和字母混合
  let mixed_string = "User123ABC"
  let result2 = greet(mixed_string)
  assert_eq_string("Hello, User123ABC!", result2)
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let test_value = 42
  
  // 加法恒等: a + 0 = a
  assert_eq(test_value, add(test_value, 0))
  assert_eq(test_value, add(0, test_value))
  
  // 乘法恒等: a × 1 = a
  assert_eq(test_value, multiply(test_value, 1))
  assert_eq(test_value, multiply(1, test_value))
  
  // 乘法零元: a × 0 = 0
  assert_eq(0, multiply(test_value, 0))
  assert_eq(0, multiply(0, test_value))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  // 计算: (a + b) × (c - d) + e
  let a = 15
  let b = 25
  let c = 30
  let d = 10
  let e = 50
  
  let sum_ab = add(a, b)        // 40
  let diff_cd = add(c, -d)      // 20
  let product = multiply(sum_ab, diff_cd)  // 800
  let result = add(product, e)  // 850
  
  assert_eq(850, result)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试正常运算
  assert_true(add(100, 200) == 300)
  assert_true(multiply(10, 20) == 200)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1: 计算购物车总价
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
  
  // 场景2: 计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let score4 = 80
  
  let sum_scores = add(add(add(score1, score2), score3), score4)
  let average = sum_scores / 4
  
  assert_eq(87, average)  // (85+90+95+80)/4 = 350/4 = 87
}