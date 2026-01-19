// Azimuth 增强测试套件
// 使用标准的 MoonBit 测试语法，包含全面的测试用例

test "add_zero_handling" {
  // 测试加法中的零值处理
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
  @azimuth.assert_eq(-100, @azimuth.add(0, -100))
}

test "add_negative_numbers" {
  // 测试负数加法的各种情况
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(5, @azimuth.add(-3, 8))
  @azimuth.assert_eq(-1000, @azimuth.add(-500, -500))
}

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(-2000000, @azimuth.add(-1000000, -1000000))
  @azimuth.assert_eq(0, @azimuth.add(1000000, -1000000))
}

test "multiply_by_zero_and_one" {
  // 测试乘以0和1的特殊情况
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(1, -12345))
}

test "multiply_negative_numbers" {
  // 测试负数乘法的各种情况
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello,     !", @azimuth.greet("     "))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "complex_mathematical_expressions" {
  // 测试复杂的数学表达式
  let x = 15
  let y = -3
  let z = 7
  
  // 计算 (x + y) * z
  let sum = @azimuth.add(x, y)
  let result = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(84, result)
  
  // 计算 x * y + z
  let product = @azimuth.multiply(x, y)
  let result2 = @azimuth.add(product, z)
  @azimuth.assert_eq(-38, result2)
}

test "overflow_boundary_conditions" {
  // 测试溢出边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(1, max_val - 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1, min_val + 1))
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
}