// 简洁测试用例 - 10个精心设计的测试

// 测试1: 加法单位元性质
///|
test "add_identity_element" {
  assert_eq(42, @azimuth.add(42, 0))
  assert_eq(42, @azimuth.add(0, 42))
  assert_eq(-17, @azimuth.add(-17, 0))
  assert_eq(0, @azimuth.add(0, 0))
}

// 测试2: 乘法单位元性质
///|
test "multiply_identity_element" {
  assert_eq(42, @azimuth.multiply(42, 1))
  assert_eq(42, @azimuth.multiply(1, 42))
  assert_eq(-17, @azimuth.multiply(-17, 1))
  assert_eq(0, @azimuth.multiply(0, 1))
}

// 测试3: 乘法零元素性质
///|
test "multiply_zero_element" {
  assert_eq(0, @azimuth.multiply(42, 0))
  assert_eq(0, @azimuth.multiply(0, 42))
  assert_eq(0, @azimuth.multiply(-17, 0))
  assert_eq(0, @azimuth.multiply(0, 0))
}

// 测试4: 加法交换律
///|
test "addition_commutative" {
  assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  assert_eq(@azimuth.add(-10, 25), @azimuth.add(25, -10))
  assert_eq(@azimuth.add(-8, -12), @azimuth.add(-12, -8))
}

// 测试5: 乘法交换律
///|
test "multiplication_commutative" {
  assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  assert_eq(@azimuth.multiply(-4, 9), @azimuth.multiply(9, -4))
  assert_eq(@azimuth.multiply(-3, -5), @azimuth.multiply(-5, -3))
}

// 测试6: 加法结合律
///|
test "addition_associative" {
  let a = 10, b = 20, c = 30
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  let x = -5, y = 15, z = -10
  assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
}

// 测试7: 乘法结合律
///|
test "multiplication_associative" {
  let a = 2, b = 3, c = 4
  assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  let x = -2, y = 5, z = -3
  assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
}

// 测试8: 乘法分配律
///|
test "multiplication_distributive" {
  let a = 5, b = 7, c = 3
  assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  let x = -4, y = 6, z = -2
  assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
}

// 测试9: 字符串处理基础
///|
test "string_handling_basic" {
  assert_eq("Hello, Alice!", @azimuth.greet("Alice"))
  assert_eq("Hello, Bob!", @azimuth.greet("Bob"))
  assert_eq("Hello, !", @azimuth.greet(""))
  assert_eq("Hello, 123!", @azimuth.greet("123"))
}

// 测试10: 数学恒等式验证
///|
test "mathematical_identities" {
  // 验证 (a+b)² = a² + 2ab + b²
  let a = 3, b = 4
  let left = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(a, b))
  let right = @azimuth.add(@azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(2, @azimuth.multiply(a, b))), @azimuth.multiply(b, b))
  assert_eq(left, right)
  
  // 验证 a² - b² = (a+b)(a-b)
  let x = 5, y = 2
  let diff_left = @azimuth.add(@azimuth.multiply(x, x), @azimuth.multiply(-1, @azimuth.multiply(y, y)))
  let diff_right = @azimuth.multiply(@azimuth.add(x, y), @azimuth.add(x, @azimuth.multiply(-1, y)))
  assert_eq(diff_left, diff_right)
}