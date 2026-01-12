test "addition_commutative" {
  let a = 15
  let b = 27
  @clean_test.assert_eq(@clean_test.add(a, b), @clean_test.add(b, a))
  
  let x = -10
  let y = 25
  @clean_test.assert_eq(@clean_test.add(x, y), @clean_test.add(y, x))
}

// 测试2: 加法结合律
///|
test "addition_associative" {
  let a = 10
  let b = 20
  let c = 30
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(a, b), c), @clean_test.add(a, @clean_test.add(b, c)))
}

// 测试3: 乘法交换律
///|
test "multiplication_commutative" {
  let a = 6
  let b = 7
  @clean_test.assert_eq(@clean_test.multiply(a, b), @clean_test.multiply(b, a))
  
  let x = -4
  let y = 9
  @clean_test.assert_eq(@clean_test.multiply(x, y), @clean_test.multiply(y, x))
}

// 测试4: 乘法结合律
///|
test "multiplication_associative" {
  let a = 2
  let b = 3
  let c = 4
  @clean_test.assert_eq(@clean_test.multiply(@clean_test.multiply(a, b), c), @clean_test.multiply(a, @clean_test.multiply(b, c)))
}

// 测试5: 乘法分配律
///|
test "multiplication_distributive" {
  let a = 5
  let b = 7
  let c = 3
  @clean_test.assert_eq(@clean_test.multiply(a, @clean_test.add(b, c)), @clean_test.add(@clean_test.multiply(a, b), @clean_test.multiply(a, c)))
}

// 测试6: 零元素性质
///|
test "zero_element_properties" {
  let a = 42
  @clean_test.assert_eq(a, @clean_test.add(a, 0))
  @clean_test.assert_eq(a, @clean_test.add(0, a))
  @clean_test.assert_eq(0, @clean_test.multiply(a, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, a))
}

// 测试7: 单位元性质
///|
test "identity_element_properties" {
  let a = 42
  @clean_test.assert_eq(a, @clean_test.multiply(a, 1))
  @clean_test.assert_eq(a, @clean_test.multiply(1, a))
}

// 测试8: 负数性质
///|
test "negative_number_properties" {
  let a = 10
  let b = -5
  @clean_test.assert_eq(5, @clean_test.add(a, b))
  @clean_test.assert_eq(-50, @clean_test.multiply(a, b))
}
