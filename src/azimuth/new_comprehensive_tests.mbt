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