// 核心功能测试文件

// 测试加法的恒等性质
///|
test "add_identity_property" {
  // 对于任意整数 a，a + 0 = a
  assert_eq(42, @azimuth.add(42, 0))
  assert_eq(-17, @azimuth.add(-17, 0))
  assert_eq(0, @azimuth.add(0, 0))
  assert_eq(2147483647, @azimuth.add(2147483647, 0))
  assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

// 测试乘法的零性质
///|
test "multiply_zero_property" {
  // 对于任意整数 a，a * 0 = 0
  assert_eq(0, @azimuth.multiply(123, 0))
  assert_eq(0, @azimuth.multiply(-456, 0))
  assert_eq(0, @azimuth.multiply(0, 0))
  assert_eq(0, @azimuth.multiply(2147483647, 0))
  assert_eq(0, @azimuth.multiply(-2147483648, 0))
}

// 测试加法的反元素性质
///|
test "add_inverse_property" {
  // 对于任意整数 a，a + (-a) = 0
  assert_eq(0, @azimuth.add(10, @azimuth.multiply(-1, 10)))
  assert_eq(0, @azimuth.add(-25, @azimuth.multiply(-1, -25)))
  assert_eq(0, @azimuth.add(0, @azimuth.multiply(-1, 0)))
  assert_eq(0, @azimuth.add(1000, @azimuth.multiply(-1, 1000)))
  assert_eq(0, @azimuth.add(-1000, @azimuth.multiply(-1, -1000)))
}

// 测试乘法的幂等性质
///|
test "multiply_idempotent_property" {
  // 1 * 1 = 1
  assert_eq(1, @azimuth.multiply(1, 1))
  // (-1) * (-1) = 1
  assert_eq(1, @azimuth.multiply(-1, -1))
  // 0 * 0 = 0
  assert_eq(0, @azimuth.multiply(0, 0))
}

// 测试字符串处理的边界情况
///|
test "greet_edge_cases" {
  // 空字符串
  assert_eq("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  assert_eq("Hello, a!", @azimuth.greet("a"))
  assert_eq("Hello, 1!", @azimuth.greet("1"))
  assert_eq("Hello, !", @azimuth.greet("!"))
  
  // 包含空格的字符串
  assert_eq("Hello, John Doe!", @azimuth.greet("John Doe"))
  assert_eq("Hello,   !", @azimuth.greet("   "))
}

// 测试负数乘法的特殊情况
///|
test "negative_multiply_special_cases" {
  // 负数乘以正数
  assert_eq(-6, @azimuth.multiply(-2, 3))
  assert_eq(-15, @azimuth.multiply(3, -5))
  assert_eq(-100, @azimuth.multiply(-10, 10))
  
  // 负数乘以负数
  assert_eq(6, @azimuth.multiply(-2, -3))
  assert_eq(15, @azimuth.multiply(-3, -5))
  assert_eq(100, @azimuth.multiply(-10, -10))
}

// 测试加法的结合律
///|
test "add_associative_law" {
  // (a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  
  assert_eq(left_assoc, right_assoc)
  
  // 测试负数情况
  let x = -10
  let y = -20
  let z = 30
  
  let left_assoc_neg = @azimuth.add(@azimuth.add(x, y), z)
  let right_assoc_neg = @azimuth.add(x, @azimuth.add(y, z))
  
  assert_eq(left_assoc_neg, right_assoc_neg)
}

// 测试乘法的分配律
///|
test "multiply_distributive_law" {
  // a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left_dist = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_dist = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  assert_eq(left_dist, right_dist)
  
  // 测试负数情况
  let x = -3
  let y = 7
  let z = -2
  
  let left_dist_neg = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_dist_neg = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  
  assert_eq(left_dist_neg, right_dist_neg)
}

// 测试大数运算的安全性
///|
test "large_number_safety" {
  // 测试接近边界的大数运算
  let large1 = 1000000
  let large2 = 2000000
  
  // 大数加法
  assert_eq(3000000, @azimuth.add(large1, large2))
  
  // 大数乘法（确保不溢出）
  assert_eq(2000000000000, @azimuth.multiply(large1, large2))
  
  // 测试中等大小的数相乘不会溢出
  assert_eq(46340 * 46340, @azimuth.multiply(46340, 46340))
  
  // 测试刚好会溢出的情况
  assert_eq(2147483647, @azimuth.multiply(46341, 46341))
}

// 测试字符串处理的实际应用
///|
test "greet_practical_usage" {
  // 用户名拼接
  let username = "moonbit_dev"
  let greeting = @azimuth.greet(username)
  assert_eq("Hello, moonbit_dev!", greeting)
  
  // 测试包含数字的用户名
  let username_with_num = "user123"
  let greeting_with_num = @azimuth.greet(username_with_num)
  assert_eq("Hello, user123!", greeting_with_num)
  
  // 测试包含下划线的用户名
  let username_with_underscore = "first_last"
  let greeting_with_underscore = @azimuth.greet(username_with_underscore)
  assert_eq("Hello, first_last!", greeting_with_underscore)
}