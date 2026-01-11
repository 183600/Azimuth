// 新的综合测试用例 - 补充测试覆盖

///|
test "additive_associativity" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = -50
  
  let left = add(add(a, b), c)
  let right = add(a, add(b, c))
  assert_eq(left, right)
  
  // 使用边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  let left_boundary = add(add(max_val, 0), min_val)
  let right_boundary = add(max_val, add(0, min_val))
  assert_eq(left_boundary, right_boundary)
}

///|
test "multiplicative_associativity" {
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let a = 10
  let b = 20
  let c = 3
  
  let left = multiply(multiply(a, b), c)
  let right = multiply(a, multiply(b, c))
  assert_eq(left, right)
  
  // 使用负数测试
  let x = -5
  let y = 4
  let z = -2
  
  let left_neg = multiply(multiply(x, y), z)
  let right_neg = multiply(x, multiply(y, z))
  assert_eq(left_neg, right_neg)
}

///|
test "distributive_property" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
  
  // 使用负数测试
  let x = -3
  let y = 7
  let z = -2
  
  let left_neg = multiply(x, add(y, z))
  let right_neg = add(multiply(x, y), multiply(x, z))
  assert_eq(left_neg, right_neg)
}

///|
test "string_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  assert_eq("Hello, 🌙!", greet("🌙")) // emoji
  assert_eq("Hello, \"quoted\"!", greet("\"quoted\"")) // 引号
  assert_eq("Hello, \\backslash\\!", greet("\\backslash\\")) // 反斜杠
  assert_eq("Hello, \nnewline\ttab!", greet("\nnewline\ttab")) // 换行和制表符
  assert_eq("Hello, 🚀🌟💻!", greet("🚀🌟💻")) // 多个emoji
}

///|
test "extreme_boundary_combinations" {
  // 测试极端边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与接近最大值的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(max_val, add(max_val - 1, 1))
  assert_eq(max_val, add(max_val - 100, 100))
  
  // 最小值与接近最小值的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, add(min_val + 1, -1))
  assert_eq(min_val, add(min_val + 100, -100))
}

///|
test "zero_in_complex_expressions" {
  // 测试零值在复杂表达式中的行为
  let a = 42
  let b = -17
  let c = 0
  
  // 加法中的零
  assert_eq(a, add(a, c))
  assert_eq(b, add(b, c))
  assert_eq(c, add(c, c))
  
  // 乘法中的零
  assert_eq(c, multiply(a, c))
  assert_eq(c, multiply(b, c))
  assert_eq(c, multiply(c, c))
  assert_eq(c, multiply(c, a))
  
  // 复杂表达式中的零
  let complex1 = add(multiply(a, c), add(b, c))
  assert_eq(b, complex1)
  
  let complex2 = multiply(add(a, c), add(b, c))
  assert_eq(multiply(a, b), complex2)
}

///|
test "subtraction_via_addition" {
  // 通过加负数实现减法: a - b = a + (-b)
  let a = 100
  let b = 30
  
  // a - b = a + (-b)
  let subtraction = add(a, -b)
  assert_eq(70, subtraction)
  
  // 使用边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值减正数
  let max_sub = add(max_val, -100)
  assert_eq(2147483547, max_sub)
  
  // 最小值减负数
  let min_sub = add(min_val, 100)
  assert_eq(-2147483548, min_sub)
  
  // 边界减法
  let boundary_sub = add(min_val, -max_val)
  assert_eq(min_val, boundary_sub) // 由于溢出，结果应该是 min_val
}

///|
test "mathematical_series" {
  // 测试数学级数运算
  // 等差数列: 1 + 2 + 3 + 4 + 5 = 15
  let sum1 = add(1, 2)
  let sum2 = add(sum1, 3)
  let sum3 = add(sum2, 4)
  let sum4 = add(sum3, 5)
  assert_eq(15, sum4)
  
  // 等比数列: 1 * 2 * 3 * 4 = 24
  let prod1 = multiply(1, 2)
  let prod2 = multiply(prod1, 3)
  let prod3 = multiply(prod2, 4)
  assert_eq(24, prod3)
  
  // 混合运算: (1 + 2) * (3 + 4) = 3 * 7 = 21
  let mixed1 = add(1, 2)
  let mixed2 = add(3, 4)
  let mixed_result = multiply(mixed1, mixed2)
  assert_eq(21, mixed_result)
}

///|
test "commutative_property_verification" {
  // 验证交换律 - 测试几组典型值
  // 正数对
  let a1 = 10
  let b1 = 20
  assert_eq(add(a1, b1), add(b1, a1))
  assert_eq(multiply(a1, b1), multiply(b1, a1))
  
  // 负数对
  let a2 = -5
  let b2 = 7
  assert_eq(add(a2, b2), add(b2, a2))
  assert_eq(multiply(a2, b2), multiply(b2, a2))
  
  // 零值对
  let a3 = 0
  let b3 = 100
  assert_eq(add(a3, b3), add(b3, a3))
  assert_eq(multiply(a3, b3), multiply(b3, a3))
  
  // 边界值对
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(add(max_val, 1), add(1, max_val))
  assert_eq(multiply(max_val, 1), multiply(1, max_val))
  assert_eq(add(min_val, 1), add(1, min_val))
  assert_eq(multiply(min_val, 1), multiply(1, min_val))
}

///|
test "string_edge_cases" {
  // 测试字符串边界情况
  // 空字符串
  assert_eq("Hello, !", greet(""))
  
  // 单个字符
  assert_eq("Hello, A!", greet("A"))
  assert_eq("Hello, 1!", greet("1"))
  assert_eq("Hello, !!", greet("!")) // greet函数会在输入后添加!
  
  // 长字符串
  let long_string = "This is a very long string that contains many words and characters to test the greet function's ability to handle longer inputs properly"
  let expected = "Hello, " + long_string + "!"
  assert_eq(expected, greet(long_string))
  
  // 包含空格的字符串
  assert_eq("Hello,  leading space!", greet(" leading space")) // 保留前导空格
  assert_eq("Hello, trailing space !", greet("trailing space "))
  assert_eq("Hello, multiple   spaces!", greet("multiple   spaces"))
}

// 额外添加的10个新测试用例
///|
test "new_add_symmetry_properties" {
  // 测试加法的交换律
  assert_eq(add(100, 200), add(200, 100))
  assert_eq(add(-100, 200), add(200, -100))
  assert_eq(add(-100, -200), add(-200, -100))
  
  // 测试加法的结合律（在溢出保护下的表现）
  let a = 1000000
  let b = 2000000
  let c = 3000000
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}

///|
test "new_multiply_distributive_properties" {
  // 测试乘法对加法的分配律（在溢出保护下的表现）
  let a = 100
  let b = 200
  let c = 3
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 负数情况
  assert_eq(multiply(-100, add(200, -50)), add(multiply(-100, 200), multiply(-100, -50)))
}

///|
test "new_extreme_boundary_combinations" {
  // 测试极端边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值的多次运算
  let result1 = add(max_val, add(max_val, max_val))
  assert_eq(max_val, result1) // 应该溢出
  
  // 最小值的多次运算
  let result2 = add(min_val, add(min_val, min_val))
  assert_eq(min_val, result2) // 应该溢出
  
  // 混合边界运算
  let result3 = add(add(max_val, min_val), 1)
  assert_eq(0, result3) // max_val + min_val = -1, then + 1 = 0
}

///|
test "new_power_of_two_operations" {
  // 测试2的幂次方的运算
  assert_eq(multiply(2, 1024), 2048)
  assert_eq(multiply(4, 512), 2048)
  assert_eq(multiply(8, 256), 2048)
  assert_eq(multiply(16, 128), 2048)
  assert_eq(multiply(32, 64), 2048)
  
  // 2的幂次方相加
  assert_eq(add(1024, 2048), 3072)
  assert_eq(add(4096, 8192), 12288)
  
  // 大的2的幂次方
  assert_eq(multiply(32768, 32768), 1073741824) // 32768 * 32768 = 1073741824
}

///|
test "new_greet_unicode_and_special_chars" {
  // 测试包含特殊字符和Unicode的字符串
  assert_eq("Hello, 中文!", greet("中文"))
  assert_eq("Hello, 🌙!", greet("🌙"))
  assert_eq("Hello, Café!", greet("Café"))
  assert_eq("Hello, test@example.com!", greet("test@example.com"))
  assert_eq("Hello, Hello World!", greet("Hello World"))
  
  // 测试包含空格的字符串
  assert_eq("Hello,   !", greet("  "))
  assert_eq("Hello, a b c!", greet("a b c"))
}

///|
test "new_sequential_operations_stability" {
  // 测试连续运算的稳定性
  let mut result = 0
  result = add(result, 100)
  assert_eq(100, result)
  result = multiply(result, 2)
  assert_eq(200, result)
  result = add(result, -50)
  assert_eq(150, result)
  result = multiply(result, 3)
  assert_eq(450, result)
  
  // 测试接近边界的连续运算
  let mut large_result = 1000000
  large_result = multiply(large_result, 1000)
  assert_eq(1000000000, large_result) // 1000000 * 1000 = 1000000000
  large_result = add(large_result, -1000000)
  assert_eq(999000000, large_result) // 1000000000 - 1000000 = 999000000
}

///|
test "new_negative_zero_handling" {
  // 测试负零的处理（虽然Int中没有负零，但测试相关边界）
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, -1))
  assert_eq(0, multiply(-1, 0))
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(0, -2147483648))
}

///|
test "new_alternating_sign_patterns" {
  // 测试交替符号的模式
  assert_eq(0, add(100, -100))
  assert_eq(0, add(-100, 100))
  assert_eq(200, add(-100, 300))
  assert_eq(-200, add(100, -300))
  
  // 乘法的交替符号
  assert_eq(-10000, multiply(100, -100))
  assert_eq(10000, multiply(-100, -100))
  assert_eq(10000, multiply(100, 100))
  
  // 复杂的符号组合
  let result = multiply(add(10, -5), add(-3, 8))
  assert_eq(25, result) // (10-5)*( -3+8) = 5*5 = 25
}

///|
test "new_boundary_precision_tests" {
  // 测试边界精度相关的测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的减法（通过加负数实现）
  assert_eq(-1, add(max_val, multiply(min_val, -1))) // 2147483647 + (-2147483648 * -1) = 2147483647 + 2147483648 = 溢出
  
  // 测试接近边界的精确计算
  assert_eq(2147483646, add(1073741823, 1073741823))
  assert_eq(-2147483647, add(-1073741824, -1073741823))
  
  // 测试边界值的乘法
  assert_eq(2147441940, multiply(46340, 46341)) // 接近溢出但不溢出
  assert_eq(2147441940, multiply(46341, 46340)) // 反过来
}

///|
test "new_mathematical_identities" {
  // 测试数学恒等式
  let test_values = [0, 1, -1, 100, -100, 2147483647, -2147483648]
  
  for value in test_values {
    // 加法恒等式：a + 0 = a
    assert_eq(value, add(value, 0))
    assert_eq(value, add(0, value))
    
    // 乘法恒等式：a * 1 = a
    assert_eq(value, multiply(value, 1))
    assert_eq(value, multiply(1, value))
    
    // 零乘法：a * 0 = 0
    assert_eq(0, multiply(value, 0))
    assert_eq(0, multiply(0, value))
    
    // 负一乘法：a * -1 = -a（除了最小值）
    if value != -2147483648 {
      assert_eq(multiply(value, -1), multiply(-1, value))
    }
  }
}