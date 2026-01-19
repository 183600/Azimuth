// Azimuth 增强测试套件
// 使用标准的 MoonBit 测试语法

test "divide_with_ceil_basic_positive" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))
}

test "divide_with_ceil_negative_dividend" {
  // 测试负数被除数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-4, 4))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-9, 2))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(10, -4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(11, -4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(4, -4))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(9, -2))
}

test "divide_with_ceil_both_negative" {
  // 测试两个负数的向上取整除法
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-10, -4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-11, -4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-4, -4))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-9, -2))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(999, 1))
}

test "multiply_zero_properties" {
  // 测试乘法的零性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  @azimuth.assert_eq(99980001, @azimuth.multiply(9999, 9999))
}

test "add_distributive_property" {
  // 测试加法与乘法的分配律
  let a = 5
  let b = 3
  let c = 2
  
  // 测试 a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "greet_unicode_handling" {
  // 测试问候函数的Unicode处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}