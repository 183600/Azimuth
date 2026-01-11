// 额外的标准测试用例

// 测试加法的零值特性
///|
test "add_zero_properties" {
  // 测试 0 + x = x
  assert_eq(42, @azimuth.add(0, 42))
  assert_eq(-17, @azimuth.add(0, -17))
  assert_eq(0, @azimuth.add(0, 0))
  
  // 测试 x + 0 = x
  assert_eq(42, @azimuth.add(42, 0))
  assert_eq(-17, @azimuth.add(-17, 0))
}

// 测试乘法的零值特性
///|
test "multiply_zero_properties" {
  // 测试 0 * x = 0
  assert_eq(0, @azimuth.multiply(0, 42))
  assert_eq(0, @azimuth.multiply(0, -17))
  assert_eq(0, @azimuth.multiply(0, 0))
  
  // 测试 x * 0 = 0
  assert_eq(0, @azimuth.multiply(42, 0))
  assert_eq(0, @azimuth.multiply(-17, 0))
}

// 测试乘法的单位元特性
///|
test "multiply_identity_properties" {
  // 测试 1 * x = x
  assert_eq(42, @azimuth.multiply(1, 42))
  assert_eq(-17, @azimuth.multiply(1, -17))
  assert_eq(0, @azimuth.multiply(1, 0))
  
  // 测试 x * 1 = x
  assert_eq(42, @azimuth.multiply(42, 1))
  assert_eq(-17, @azimuth.multiply(-17, 1))
  assert_eq(0, @azimuth.multiply(0, 1))
}

// 测试负数的特殊性质
///|
test "negative_number_properties" {
  // 测试 -1 * x = -x
  assert_eq(-42, @azimuth.multiply(-1, 42))
  assert_eq(17, @azimuth.multiply(-1, -17))
  assert_eq(0, @azimuth.multiply(-1, 0))
  
  // 测试 x * -1 = -x
  assert_eq(-42, @azimuth.multiply(42, -1))
  assert_eq(17, @azimuth.multiply(-17, -1))
  assert_eq(0, @azimuth.multiply(0, -1))
}

// 测试字符串拼接的边界情况
///|
test "string_concatenation_edge_cases" {
  // 测试空字符串
  assert_eq("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  assert_eq("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  assert_eq("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  assert_eq("Hello, 12345!", @azimuth.greet("12345"))
}

// 测试大数相加的边界情况
///|
test "large_number_addition_boundaries" {
  // 测试接近最大值的加法
  let max_val = 2147483647
  assert_eq(max_val, @azimuth.add(max_val, 0))
  assert_eq(max_val, @azimuth.add(0, max_val))
  assert_eq(max_val, @azimuth.add(max_val, 1))  // 应该溢出
  
  // 测试接近最小值的加法
  let min_val = -2147483648
  assert_eq(min_val, @azimuth.add(min_val, 0))
  assert_eq(min_val, @azimuth.add(0, min_val))
  assert_eq(min_val, @azimuth.add(min_val, -1))  // 应该溢出
}

// 测试大数相乘的边界情况
///|
test "large_number_multiplication_boundaries" {
  // 测试接近最大值的乘法
  let max_val = 2147483647
  assert_eq(max_val, @azimuth.multiply(max_val, 1))
  assert_eq(max_val, @azimuth.multiply(1, max_val))
  
  // 测试接近最小值的乘法
  let min_val = -2147483648
  assert_eq(min_val, @azimuth.multiply(min_val, 1))
  assert_eq(min_val, @azimuth.multiply(1, min_val))
  assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 应该溢出
}

// 测试数学运算的结合律
///|
test "mathematical_associative_laws" {
  // 加法结合律: (a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

// 测试数学运算的交换律
///|
test "mathematical_commutative_laws" {
  // 加法交换律: a + b = b + a
  assert_eq(@azimuth.add(15, 25), @azimuth.add(25, 15))
  assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
  
  // 乘法交换律: a * b = b * a
  assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  assert_eq(@azimuth.multiply(-3, 8), @azimuth.multiply(8, -3))
}

// 测试数学运算的分配律
///|
test "mathematical_distributive_law" {
  // 乘法对加法的分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}