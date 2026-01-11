// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 简洁测试用例 - 10个精心设计的测试

// 测试1: 加法单位元性质
///|
test "add_identity_element" {
  assert_eq(42, @clean_test.@clean_test.add(42, 0))
  assert_eq(42, @clean_test.@clean_test.add(0, 42))
  assert_eq(-17, @clean_test.@clean_test.add(-17, 0))
  assert_eq(0, @clean_test.@clean_test.add(0, 0))
}

// 测试2: 乘法单位元性质
///|
test "multiply_identity_element" {
  assert_eq(42, @clean_test.@clean_test.multiply(42, 1))
  assert_eq(42, @clean_test.@clean_test.multiply(1, 42))
  assert_eq(-17, @clean_test.@clean_test.multiply(-17, 1))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 1))
}

// 测试3: 乘法零元素性质
///|
test "multiply_zero_element" {
  assert_eq(0, @clean_test.@clean_test.multiply(42, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 42))
  assert_eq(0, @clean_test.@clean_test.multiply(-17, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 0))
}

// 测试4: 加法交换律
///|
test "addition_commutative" {
  assert_eq(@clean_test.@clean_test.add(15, 27), @clean_test.@clean_test.add(27, 15))
  assert_eq(@clean_test.@clean_test.add(-10, 25), @clean_test.@clean_test.add(25, -10))
  assert_eq(@clean_test.@clean_test.add(-8, -12), @clean_test.@clean_test.add(-12, -8))
}

// 测试5: 乘法交换律
///|
test "multiplication_commutative" {
  assert_eq(@clean_test.@clean_test.multiply(6, 7), @clean_test.@clean_test.multiply(7, 6))
  assert_eq(@clean_test.@clean_test.multiply(-4, 9), @clean_test.@clean_test.multiply(9, -4))
  assert_eq(@clean_test.@clean_test.multiply(-3, -5), @clean_test.@clean_test.multiply(-5, -3))
}

// 测试6: 加法结合律
///|
test "addition_associative" {
  let a = 10, b = 20, c = 30
  assert_eq(@clean_test.@clean_test.add(@clean_test.@clean_test.add(a, b), c), @clean_test.@clean_test.add(a, @clean_test.@clean_test.add(b, c)))
  
  let x = -5, y = 15, z = -10
  assert_eq(@clean_test.@clean_test.add(@clean_test.@clean_test.add(x, y), z), @clean_test.@clean_test.add(x, @clean_test.@clean_test.add(y, z)))
}

// 测试7: 乘法结合律
///|
test "multiplication_associative" {
  let a = 2, b = 3, c = 4
  assert_eq(@clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(a, b), c), @clean_test.@clean_test.multiply(a, @clean_test.@clean_test.multiply(b, c)))
  
  let x = -2, y = 5, z = -3
  assert_eq(@clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(x, y), z), @clean_test.@clean_test.multiply(x, @clean_test.@clean_test.multiply(y, z)))
}

// 测试8: 乘法分配律
///|
test "multiplication_distributive" {
  let a = 5, b = 7, c = 3
  assert_eq(@clean_test.@clean_test.multiply(a, @clean_test.@clean_test.add(b, c)), @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(a, b), @clean_test.@clean_test.multiply(a, c)))
  
  let x = -4, y = 6, z = -2
  assert_eq(@clean_test.@clean_test.multiply(x, @clean_test.@clean_test.add(y, z)), @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(x, y), @clean_test.@clean_test.multiply(x, z)))
}

// 测试9: 字符串处理基础
///|
test "string_handling_basic" {
  assert_eq_string("Hello, Alice!", @clean_test.@clean_test.greet("Alice"))
  assert_eq_string("Hello, Bob!", @clean_test.@clean_test.greet("Bob"))
  assert_eq_string("Hello, !", @clean_test.@clean_test.greet(""))
  assert_eq_string("Hello, 123!", @clean_test.@clean_test.greet("123"))
}

// 测试10: 数学恒等式验证
///|
test "mathematical_identities" {
  // 验证 (a+b)² = a² + 2ab + b²
  let a = 3, b = 4
  let left = @clean_test.@clean_test.multiply(@clean_test.@clean_test.add(a, b), @clean_test.@clean_test.add(a, b))
  let right = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.multiply(a, a), @clean_test.@clean_test.multiply(2, @clean_test.@clean_test.multiply(a, b))), @clean_test.@clean_test.multiply(b, b))
  assert_eq(left, right)
  
  // 验证 a² - b² = (a+b)(a-b)
  let x = 5, y = 2
  let diff_left = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(x, x), @clean_test.@clean_test.multiply(-1, @clean_test.@clean_test.multiply(y, y)))
  let diff_right = @clean_test.@clean_test.multiply(@clean_test.@clean_test.add(x, y), @clean_test.@clean_test.add(x, @clean_test.@clean_test.multiply(-1, y)))
  assert_eq(diff_left, diff_right)
}