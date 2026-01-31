// 标准 Azimuth 测试套件
// 包含10个标准 MoonBit 测试用例，覆盖主要功能

test "add_function_positive_numbers" {
  // 测试正数加法
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-7, @azimuth.add(-3, -4))
  @azimuth.assert_eq(-1, @azimuth.add(-5, 4))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
}

test "multiply_function_various_cases" {
  // 测试乘法的各种情况
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
}

test "subtract_function_basic" {
  // 测试基本减法功能
  @azimuth.assert_eq(2, @azimuth.subtract(5, 3))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(-5, -20))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 2))
}

test "divide_with_ceil_zero_division" {
  // 测试除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "combined_operations" {
  // 测试组合运算
  let a = 5
  let b = 3
  let c = 2
  @azimuth.assert_eq(17, @azimuth.add(@azimuth.multiply(a, b), c))  // 5*3 + 2 = 17
  @azimuth.assert_eq(3, @azimuth.subtract(@azimuth.divide_with_ceil(10, 3), 1))  // ceil(10/3) - 1 = 3
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
}

test "edge_cases_and_limits" {
  // 测试边界情况和限制
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))  // 接近 Int 最大值
  @azimuth.assert_eq(-2147483648, @azimuth.subtract(-2147483647, 1))  // 接近 Int 最小值
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
}