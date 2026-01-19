// 标准 MoonBit 单元测试
// 测试 azimuth 包的核心功能

test "add_normal_cases" {
  // 测试正常情况下的加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_zero_cases" {
  // 测试包含零的加法
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(0, -5))
}

test "add_overflow_cases" {
  // 测试溢出情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 负溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
}

test "multiply_normal_cases" {
  // 测试正常情况下的乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
  @azimuth.assert_eq(7, @azimuth.multiply(-7, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
}

test "multiply_overflow_cases" {
  // 测试乘法溢出
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 最小值乘法特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
}

test "combined_operations" {
  // 测试组合运算
  // (2 + 3) * 4 = 20
  let sum = @azimuth.add(2, 3)
  let result = @azimuth.multiply(sum, 4)
  @azimuth.assert_eq(20, result)
  
  // 5 + 6 + 7 = 18
  let sum2 = @azimuth.add(5, 6)
  let total = @azimuth.add(sum2, 7)
  @azimuth.assert_eq(18, total)
}