// 高级数学性质测试

///|
test "addition_associative_property" {
  // 测试 (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  let x = -100
  let y = -200
  let z = 300
  assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
}

///|
test "multiplication_commutative_property" {
  // 测试 a * b = b * a
  assert_eq(@azimuth.multiply(12, 34), @azimuth.multiply(34, 12))
  assert_eq(@azimuth.multiply(-5, 7), @azimuth.multiply(7, -5))
  assert_eq(@azimuth.multiply(-8, -9), @azimuth.multiply(-9, -8))
  assert_eq(@azimuth.multiply(0, 12345), @azimuth.multiply(12345, 0))
}

///|
test "mathematical_identity_elements" {
  // 测试加法单位元
  assert_eq(123, @azimuth.add(123, 0))
  assert_eq(-456, @azimuth.add(-456, 0))
  assert_eq(0, @azimuth.add(0, 0))
  
  // 测试乘法单位元
  assert_eq(789, @azimuth.multiply(789, 1))
  assert_eq(-789, @azimuth.multiply(-789, 1))
  assert_eq(0, @azimuth.multiply(0, 1))
  
  // 测试乘法零元
  assert_eq(0, @azimuth.multiply(12345, 0))
  assert_eq(0, @azimuth.multiply(-12345, 0))
  assert_eq(0, @azimuth.multiply(0, 0))
}