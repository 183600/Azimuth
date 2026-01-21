// 标准 MoonBit 测试用例 - Azimuth 库
// 包含10个测试用例，覆盖核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "add_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(1, @azimuth.add(max_val, min_val + 1))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(2, 0))
}

test "multiply_identity_elements" {
  // 测试乘法单位元
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "greet_edge_cases" {
  // 测试问候函数边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试基本向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
}

test "divide_with_ceil_zero_division" {
  // 测试除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(5, 6)
  let division = @azimuth.divide_with_ceil(25, 4)
  
  @azimuth.assert_eq(30, sum)
  @azimuth.assert_eq(30, product)
  @azimuth.assert_eq(7, division)
  
  // 测试函数组合
  let combined = @azimuth.multiply(@azimuth.add(2, 3), @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(20, combined)
}