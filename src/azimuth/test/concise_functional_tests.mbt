// 简洁功能测试

// 测试加法基本功能
///|
test "add_basic_functionality" {
  assert_eq(10, add(3, 7))
  assert_eq(-4, add(-2, -2))
  assert_eq(0, add(5, -5))
}

// 测试乘法基本功能
///|
test "multiply_basic_functionality" {
  assert_eq(15, multiply(3, 5))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(100, 0))
}

// 测试字符串拼接功能
///|
test "greet_basic_functionality" {
  assert_eq("Hello, Alice!", greet("Alice"))
  assert_eq("Hello, Bob!", greet("Bob"))
}

// 测试加法边界值
///|
test "add_boundary_values" {
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(-2147483647, add(-2147483648, 1))
}

// 测试乘法边界值
///|
test "multiply_boundary_values" {
  assert_eq(2147483647, multiply(46341, 46341))
  assert_eq(-2147483648, multiply(-2147483648, 1))
  assert_eq(0, multiply(-2147483648, 0))
}

// 测试数学运算组合
///|
test "combined_math_operations" {
  // (2 + 3) * 4 = 20
  let sum = add(2, 3)
  let result = multiply(sum, 4)
  assert_eq(20, result)
  
  // 5 * (6 + 7) = 65
  let sum2 = add(6, 7)
  let result2 = multiply(5, sum2)
  assert_eq(65, result2)
}

// 测试加法交换律
///|
test "addition_commutativity" {
  assert_eq(add(12, 34), add(34, 12))
  assert_eq(add(-5, 10), add(10, -5))
}

// 测试乘法交换律
///|
test "multiplication_commutativity" {
  assert_eq(multiply(6, 7), multiply(7, 6))
  assert_eq(multiply(-3, 8), multiply(8, -3))
}

// 测试字符串特殊字符处理
///|
test "string_special_chars" {
  assert_eq("Hello, 世界!", greet("世界"))
  assert_eq("Hello, 123!", greet("123"))
  assert_eq("Hello, !", greet(""))
}

// 测试数学恒等式
///|
test "mathematical_identities" {
  // 加法单位元
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  
  // 乘法单位元
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  
  // 乘法零元
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
}