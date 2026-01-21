// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_addition" {
  // 测试基本加法运算
  let result = azimuth.add(3, 5)
  assert_eq(8, result)
}

test "addition_with_zero" {
  // 测试与零相加的情况
  assert_eq(5, azimuth.add(5, 0))
  assert_eq(7, azimuth.add(0, 7))
  assert_eq(0, azimuth.add(0, 0))
}

test "addition_with_negative" {
  // 测试包含负数的加法
  assert_eq(2, azimuth.add(5, -3))
  assert_eq(-2, azimuth.add(-5, 3))
  assert_eq(-8, azimuth.add(-5, -3))
}

test "basic_multiplication" {
  // 测试基本乘法运算
  let result = azimuth.multiply(4, 6)
  assert_eq(24, result)
}

test "multiplication_with_zero_and_one" {
  // 测试与零和一的乘法
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(5, azimuth.multiply(5, 1))
  assert_eq(5, azimuth.multiply(1, 5))
}

test "multiplication_with_negative" {
  // 测试包含负数的乘法
  assert_eq(-12, azimuth.multiply(4, -3))
  assert_eq(-12, azimuth.multiply(-4, 3))
  assert_eq(12, azimuth.multiply(-4, -3))
}

test "greet_function" {
  // 测试问候函数
  let result = azimuth.greet("MoonBit")
  assert_eq_string("Hello, MoonBit!", result)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, User123!", azimuth.greet("User123"))
}

test "complex_calculation" {
  // 测试复合计算：矩形面积计算
  let length = 8
  let width = 5
  let area = azimuth.multiply(length, width)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  let total = azimuth.add(area, perimeter)
  assert_eq(66, total) // 8*5 + 2*(8+5) = 40 + 26 = 66
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(max_val, azimuth.add(1000000000, 1500000000))
  
  // 测试负数溢出
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(min_val, azimuth.add(-1000000000, -1500000000))
  
  // 测试乘法溢出
  assert_eq(max_val, azimuth.multiply(46341, 46341))
  assert_eq(min_val, azimuth.multiply(-46341, 46341))
}