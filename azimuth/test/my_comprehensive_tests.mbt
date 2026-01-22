// 我添加的综合测试用例
// 专注于核心功能和边界条件测试

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "add_commutative" {
  // 测试加法交换律
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  let c = -123
  let d = 456
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
}

test "multiply_commutative" {
  // 测试乘法交换律
  let a = 12
  let b = 34
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  let c = -12
  let d = 34
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "greet_multilingual" {
  // 测试多语言支持
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, English!", @azimuth.greet("English"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
}

test "complex_calculation" {
  // 测试复杂计算序列
  // 计算 (5 + 3) * (10 - 2) / 3 向上取整
  let sum = @azimuth.add(5, 3)  // 8
  let diff = @azimuth.add(10, -2)  // 8
  let product = @azimuth.multiply(sum, diff)  // 64
  let result = @azimuth.divide_with_ceil(product, 3)  // 22
  @azimuth.assert_eq(22, result)
}