// 简单增强的 azimuth 测试用例
// 测试核心功能的各种场景

test "add_function_enhanced" {
  // 测试加法函数的增强场景
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(1000, -1000))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "multiply_function_enhanced" {
  // 测试乘法函数的增强场景
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
}

test "greet_function_enhanced" {
  // 测试问候函数的增强场景
  @azimuth.assert_eq_string("Hello, Enhanced!", @azimuth.greet("Enhanced"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_enhanced" {
  // 测试向上取整除法的增强场景
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "complex_calculation_enhanced" {
  // 测试复杂计算场景
  let step1 = @azimuth.add(10, 20)  // 30
  let step2 = @azimuth.multiply(step1, 2)  // 60
  let step3 = @azimuth.divide_with_ceil(step2, 25)  // 3
  @azimuth.assert_eq(3, step3)
}