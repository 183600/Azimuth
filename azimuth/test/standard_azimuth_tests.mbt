// 标准 MoonBit 测试用例
test "add function with positive numbers" {
  @assertion.assert_eq(10, azimuth.add(3, 7))
  @assertion.assert_eq(100, azimuth.add(50, 50))
  @assertion.assert_eq(1, azimuth.add(0, 1))
}

test "add function with negative numbers" {
  @assertion.assert_eq(-10, azimuth.add(-3, -7))
  @assertion.assert_eq(-5, azimuth.add(-10, 5))
  @assertion.assert_eq(0, azimuth.add(-5, 5))
}

test "add function with boundary values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  @assertion.assert_eq(max_val, azimuth.add(max_val, 0))
  @assertion.assert_eq(min_val, azimuth.add(min_val, 0))
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))
}

test "multiply function with positive numbers" {
  @assertion.assert_eq(15, azimuth.multiply(3, 5))
  @assertion.assert_eq(0, azimuth.multiply(0, 10))
  @assertion.assert_eq(100, azimuth.multiply(10, 10))
  @assertion.assert_eq(1, azimuth.multiply(1, 100))
}

test "multiply function with negative numbers" {
  @assertion.assert_eq(-15, azimuth.multiply(3, -5))
  @assertion.assert_eq(15, azimuth.multiply(-3, -5))
  @assertion.assert_eq(0, azimuth.multiply(0, -10))
  @assertion.assert_eq(-100, azimuth.multiply(-10, 10))
}

test "multiply function with boundary values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  @assertion.assert_eq(0, azimuth.multiply(0, max_val))
  @assertion.assert_eq(0, azimuth.multiply(0, min_val))
  @assertion.assert_eq(max_val, azimuth.multiply(1, max_val))
  @assertion.assert_eq(min_val, azimuth.multiply(1, min_val))
}

test "greet function with various inputs" {
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))
  @assertion.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))
  @assertion.assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
}

test "complex arithmetic scenarios" {
  // 复合运算测试
  let a = 5
  let b = 10
  let c = -3
  
  @assertion.assert_eq(50, azimuth.multiply(azimuth.add(a, b), 3))
  @assertion.assert_eq(21, azimuth.add(azimuth.multiply(a, b), azimuth.multiply(c, -1)))
  @assertion.assert_eq(0, azimuth.add(azimuth.multiply(a, c), azimuth.multiply(b, 0)))
}

test "edge case combinations" {
  // 边界情况组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @assertion.assert_eq(max_val, azimuth.add(azimuth.multiply(1000, 1000), 1000))
  @assertion.assert_eq(min_val, azimuth.add(azimuth.multiply(-1000, 1000), -1000))
  @assertion.assert_eq(max_val, azimuth.multiply(azimuth.add(1000, 1000), 1000))
}

test "string operations and arithmetic" {
  // 字符串操作与算术运算结合
  let name = "Developer"
  let greeting = azimuth.greet(name)
  let name_length = 9
  
  @assertion.assert_eq_string("Hello, Developer!", greeting)
  @assertion.assert_eq(18, azimuth.multiply(name_length, 2))
  @assertion.assert_eq(10, azimuth.add(name_length, 1))
}