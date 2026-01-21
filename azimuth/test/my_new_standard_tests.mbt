// Azimuth 项目标准测试用例
// 使用标准的 MoonBit 测试语法

test "basic_addition" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
}

test "basic_multiplication" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
}

test "divide_with_ceil_negative" {
  // 测试向上取整除法负数处理
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-8, -4))
}

test "boolean_assertions" {
  // 测试布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(2 > 10)
}

test "complex_operations" {
  // 测试复合运算
  let a = 10
  let b = 20
  let product = @azimuth.multiply(a, b)
  let result = @azimuth.add(product, 5)
  @azimuth.assert_eq(205, result)
}

test "edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}