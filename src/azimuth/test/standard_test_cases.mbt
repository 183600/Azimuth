// 标准 MoonBit 测试用例

// 测试基本加法功能
///|
test "basic_addition" {
  assert_eq(5, @azimuth.add(2, 3))
  assert_eq(0, @azimuth.add(0, 0))
  assert_eq(-3, @azimuth.add(-1, -2))
}

// 测试基本乘法功能
///|
test "basic_multiplication" {
  assert_eq(6, @azimuth.multiply(2, 3))
  assert_eq(0, @azimuth.multiply(5, 0))
  assert_eq(-4, @azimuth.multiply(-2, 2))
}

// 测试字符串拼接功能
///|
test "string_greeting" {
  assert_eq("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq("Hello, !", @azimuth.greet(""))
}

// 测试加法边界值
///|
test "addition_boundary_values" {
  assert_eq(2147483647, @azimuth.add(2147483647, 1))
  assert_eq(-2147483648, @azimuth.add(-2147483648, -1))
  assert_eq(-2147483647, @azimuth.add(-2147483648, 1))
}

// 测试乘法边界值
///|
test "multiplication_boundary_values" {
  assert_eq(2147483647, @azimuth.multiply(46342, 46342))
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 2))
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
}

// 测试数学交换律
///|
test "math_commutative_laws" {
  assert_eq(@azimuth.add(10, 20), @azimuth.add(20, 10))
  assert_eq(@azimuth.multiply(5, 7), @azimuth.multiply(7, 5))
}

// 测试数学结合律
///|
test "math_associative_laws" {
  assert_eq(@azimuth.add(@azimuth.add(1, 2), 3), @azimuth.add(1, @azimuth.add(2, 3)))
  assert_eq(@azimuth.multiply(@azimuth.multiply(2, 3), 4), @azimuth.multiply(2, @azimuth.multiply(3, 4)))
}

// 测试零元素性质
///|
test "zero_element_properties" {
  assert_eq(123, @azimuth.add(123, 0))
  assert_eq(123, @azimuth.add(0, 123))
  assert_eq(0, @azimuth.multiply(123, 0))
  assert_eq(0, @azimuth.multiply(0, 123))
}

// 测试单位元性质
///|
test "identity_element_properties" {
  assert_eq(123, @azimuth.multiply(123, 1))
  assert_eq(123, @azimuth.multiply(1, 123))
  assert_eq(-456, @azimuth.multiply(-456, 1))
}

// 测试实际应用场景
///|
test "practical_calculations" {
  // 计算矩形面积
  let length = 10
  let width = 5
  let area = @azimuth.multiply(length, width)
  assert_eq(50, area)
  
  // 计算周长
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  assert_eq(30, perimeter)
}