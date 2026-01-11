// 修正版简洁测试用例 - 10个精心设计的测试

// 测试1: 加法单位元性质
///|
test "correct_add_identity_element" {
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
}

// 测试2: 乘法单位元性质
///|
test "correct_multiply_identity_element" {
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(0, 1))
}

// 测试3: 乘法零元素性质
///|
test "correct_multiply_zero_element" {
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
  assert_eq(0, multiply(-17, 0))
  assert_eq(0, multiply(0, 0))
}

// 测试4: 加法交换律
///|
test "correct_addition_commutative" {
  assert_eq(add(15, 27), add(27, 15))
  assert_eq(add(-10, 25), add(25, -10))
  assert_eq(add(-8, -12), add(-12, -8))
}

// 测试5: 乘法交换律
///|
test "correct_multiplication_commutative" {
  assert_eq(multiply(6, 7), multiply(7, 6))
  assert_eq(multiply(-4, 9), multiply(9, -4))
  assert_eq(multiply(-3, -5), multiply(-5, -3))
}

// 测试6: 加法结合律
///|
test "correct_addition_associative" {
  let a = 10
  let b = 20
  let c = 30
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  let x = -5
  let y = 15
  let z = -10
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
}

// 测试7: 乘法结合律
///|
test "correct_multiplication_associative" {
  let a = 2
  let b = 3
  let c = 4
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  let x = -2
  let y = 5
  let z = -3
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
}

// 测试8: 乘法分配律
///|
test "correct_multiplication_distributive" {
  let a = 5
  let b = 7
  let c = 3
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  let x = -4
  let y = 6
  let z = -2
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

// 测试9: 字符串处理基础
///|
test "correct_string_handling_basic" {
  assert_eq("Hello, Alice!", greet("Alice"))
  assert_eq("Hello, Bob!", greet("Bob"))
  assert_eq("Hello, !", greet(""))
  assert_eq("Hello, 123!", greet("123"))
}

// 测试10: 数学恒等式验证
///|
test "correct_mathematical_identities" {
  // 验证 (a+b)² = a² + 2ab + b²
  let a = 3
  let b = 4
  let left = multiply(add(a, b), add(a, b))
  let right = add(add(multiply(a, a), multiply(2, multiply(a, b))), multiply(b, b))
  assert_eq(left, right)
  
  // 验证 a² - b² = (a+b)(a-b)
  let x = 5
  let y = 2
  let diff_left = add(multiply(x, x), multiply(-1, multiply(y, y)))
  let diff_right = multiply(add(x, y), add(x, multiply(-1, y)))
  assert_eq(diff_left, diff_right)
}