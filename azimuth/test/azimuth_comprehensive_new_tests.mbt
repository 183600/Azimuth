// Azimuth 综合新测试用例
// 使用标准的 MoonBit 测试语法

test "add_zero_handling" {
  // 测试加法中0的处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-100, @azimuth.add(0, -100))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-50, @azimuth.add(-20, -30))
  @azimuth.assert_eq(0, @azimuth.add(-100, 100))
  @azimuth.assert_eq(90, @azimuth.add(-10, 100))
}

test "add_large_numbers" {
  // 测试大数加法
  let max_val = 2147483647
  let half_max = 1073741823
  
  @azimuth.assert_eq(max_val, @azimuth.add(half_max, half_max + 1))
  @azimuth.assert_eq(2147483646, @azimuth.add(half_max, half_max))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "multiply_by_zero" {
  // 测试乘以0的情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -100))
}

test "multiply_by_one" {
  // 测试乘以1的情况
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-100, @azimuth.multiply(-100, 1))
  @azimuth.assert_eq(-100, @azimuth.multiply(1, -100))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(-6, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
}

test "multiply_large_values" {
  // 测试大数值乘法
  let max_val = 2147483647
  let sqrt_max = 46340  // sqrt(2147395600) ≈ 46340
  
  @azimuth.assert_eq(2147395600, @azimuth.multiply(sqrt_max, sqrt_max))
  @azimuth.assert_eq(max_val, @azimuth.multiply(sqrt_max, sqrt_max + 1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_empty_and_special_cases" {
  // 测试问候函数的特殊情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "combined_operations" {
  // 测试组合运算
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