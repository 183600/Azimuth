// 新的标准 MoonBit 测试用例
// 测试 azimuth 包中的核心功能
test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, azimuth::@azimuth.add(2, 3))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, azimuth::@azimuth.add(-2, -3))
  @azimuth.assert_eq(100, azimuth::@azimuth.add(50, 50))
}
test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, azimuth::@azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, azimuth::@azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, azimuth::@azimuth.multiply(-2, -3))
}
test "add_with_zero" {
  // 测试与0相加的情况
  @azimuth.assert_eq(5, azimuth::@azimuth.add(5, 0))
  @azimuth.assert_eq(5, azimuth::@azimuth.add(0, 5))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
  @azimuth.assert_eq(-10, azimuth::@azimuth.add(-10, 0))
}
test "multiply_with_identity" {
  // 测试乘法单位元的情况
  @azimuth.assert_eq(7, azimuth::@azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, azimuth::@azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, azimuth::@azimuth.multiply(-7, 1))
  @azimuth.assert_eq(-7, azimuth::@azimuth.multiply(1, -7))
}
test "greet_simple_names" {
  // 测试简单名字的问候
  @azimuth.assert_eq_string("Hello, Alice!", azimuth::@azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", azimuth::@azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, World!", azimuth::@azimuth.greet("World"))
}
test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, azimuth::@azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, azimuth::@azimuth.add(-8, 3))
  @azimuth.assert_eq(5, azimuth::@azimuth.add(8, -3))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(-10, 10))
}
test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, azimuth::@azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, azimuth::@azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, azimuth::@azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, azimuth::@azimuth.multiply(-1, -15))
}
test "greet_empty_and_special" {
  // 测试空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, @!", azimuth::@azimuth.greet("@"))
  @azimuth.assert_eq_string("Hello, 123!", azimuth::@azimuth.greet("123"))
}
test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(2000000, azimuth::@azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, azimuth::@azimuth.add(1500000, -500000))
  @azimuth.assert_eq(-2000000, azimuth::@azimuth.add(-1000000, -1000000))
}
test "combined_operations" {
  // 测试组合运算
  // (a + b) * c
  let a = 3
  let b = 4
  let c = 5
  
  let sum = azimuth::@azimuth.add(a, b)  // 7
  let result = azimuth::@azimuth.multiply(sum, c)  // 35
  @azimuth.assert_eq(35, result)
  
  // a * b + c
  let product = azimuth::@azimuth.multiply(a, b)  // 12
  let result2 = azimuth::@azimuth.add(product, c)  // 17
  @azimuth.assert_eq(17, result2)
}