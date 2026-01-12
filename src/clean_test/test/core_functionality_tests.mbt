test "add_identity_property" {
  // 对于任意整数 a，a + 0 = a
  @clean_test.assert_eq(42, @clean_test.add(42, 0))
  @clean_test.assert_eq(-17, @clean_test.add(-17, 0))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483647, 0))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, 0))
}

// 测试乘法的零性质
///|
test "multiply_zero_property" {
  // 对于任意整数 a，a * 0 = 0
  @clean_test.assert_eq(0, @clean_test.multiply(123, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(-456, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(2147483647, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(-2147483648, 0))
}

// 测试加法的反元素性质
///|
test "add_inverse_property" {
  // 对于任意整数 a，a + (-a) = 0
  @clean_test.assert_eq(0, @clean_test.add(10, @clean_test.multiply(-1, 10)))
  @clean_test.assert_eq(0, @clean_test.add(-25, @clean_test.multiply(-1, -25)))
  @clean_test.assert_eq(0, @clean_test.add(0, @clean_test.multiply(-1, 0)))
  @clean_test.assert_eq(0, @clean_test.add(1000, @clean_test.multiply(-1, 1000)))
  @clean_test.assert_eq(0, @clean_test.add(-1000, @clean_test.multiply(-1, -1000)))
}

// 测试乘法的幂等性质
///|
test "multiply_idempotent_property" {
  // 1 * 1 = 1
  @clean_test.assert_eq(1, @clean_test.multiply(1, 1))
  // (-1) * (-1) = 1
  @clean_test.assert_eq(1, @clean_test.multiply(-1, -1))
  // 0 * 0 = 0
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
}

// 测试字符串处理的边界情况
///|
test "greet_edge_cases" {
  // 空字符串
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
  
  // 单个字符
  @clean_test.assert_eq_string("Hello, a!", @clean_test.greet("a"))
  @clean_test.assert_eq_string("Hello, 1!", @clean_test.greet("1"))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet("!"))
  
  // 包含空格的字符串
  @clean_test.assert_eq_string("Hello, John Doe!", @clean_test.greet("John Doe"))
  @clean_test.assert_eq_string("Hello,   !", @clean_test.greet("   "))
}

// 测试负数乘法的特殊情况
///|
test "negative_multiply_special_cases" {
  // 负数乘以正数
  @clean_test.assert_eq(-6, @clean_test.multiply(-2, 3))
  @clean_test.assert_eq(-15, @clean_test.multiply(3, -5))
  @clean_test.assert_eq(-100, @clean_test.multiply(-10, 10))
  
  // 负数乘以负数
  @clean_test.assert_eq(6, @clean_test.multiply(-2, -3))
  @clean_test.assert_eq(15, @clean_test.multiply(-3, -5))
  @clean_test.assert_eq(100, @clean_test.multiply(-10, -10))
}

// 测试加法的结合律
///|
test "add_associative_law" {
  // (a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  
  let left_assoc = @clean_test.add(@clean_test.add(a, b), c)
  let right_assoc = @clean_test.add(a, @clean_test.add(b, c))
  
  @clean_test.assert_eq(left_assoc, right_assoc)
  
  // 测试负数情况
  let x = -10
  let y = -20
  let z = 30
  
  let left_assoc_neg = @clean_test.add(@clean_test.add(x, y), z)
  let right_assoc_neg = @clean_test.add(x, @clean_test.add(y, z))
  
  @clean_test.assert_eq(left_assoc_neg, right_assoc_neg)
}

// 测试乘法的分配律
///|
test "multiply_distributive_law" {
  // a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left_dist = @clean_test.multiply(a, @clean_test.add(b, c))
  let right_dist = @clean_test.add(@clean_test.multiply(a, b), @clean_test.multiply(a, c))
  
  @clean_test.assert_eq(left_dist, right_dist)
  
  // 测试负数情况
  let x = -3
  let y = 7
  let z = -2
  
  let left_dist_neg = @clean_test.multiply(x, @clean_test.add(y, z))
  let right_dist_neg = @clean_test.add(@clean_test.multiply(x, y), @clean_test.multiply(x, z))
  
  @clean_test.assert_eq(left_dist_neg, right_dist_neg)
}

// 测试大数运算的安全性
///|
test "large_number_safety" {
  // 测试接近边界的大数运算
  let large1 = 1000000
  let large2 = 2000000
  
  // 大数加法
  @clean_test.assert_eq(3000000, @clean_test.add(large1, large2))
  
  // 大数乘法（确保不溢出）
  // 2000000 * 1000 = 2000000000 (在Int32范围内)
  @clean_test.assert_eq(2000000000, @clean_test.multiply(large1, 1000))
  
  // 测试中等大小的数相乘不会溢出
  @clean_test.assert_eq(46340 * 46340, @clean_test.multiply(46340, 46340))
  
  // 测试刚好会溢出的情况
  @clean_test.assert_eq(2147483647, @clean_test.multiply(46341, 46341))
}

// 测试字符串处理的实际应用
///|
test "greet_practical_usage" {
  // 用户名拼接
  let username = "moonbit_dev"
  let greeting = @clean_test.greet(username)
  @clean_test.assert_eq_string("Hello, moonbit_dev!", greeting)
  
  // 测试包含数字的用户名
  let username_with_num = "user123"
  let greeting_with_num = @clean_test.greet(username_with_num)
  @clean_test.assert_eq_string("Hello, user123!", greeting_with_num)
  
  // 测试包含下划线的用户名
  let username_with_underscore = "first_last"
  let greeting_with_underscore = @clean_test.greet(username_with_underscore)
  @clean_test.assert_eq_string("Hello, first_last!", greeting_with_underscore)
}
