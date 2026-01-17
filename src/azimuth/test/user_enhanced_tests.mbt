// 用户增强测试用例
// 使用标准 MoonBit 测试语法，覆盖各种场景

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
}

test "multiply_by_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, John@Doe!", @azimuth.greet("John@Doe"))
  @azimuth.assert_eq_string("Hello, 用户#123!", @azimuth.greet("用户#123"))
  @azimuth.assert_eq_string("Hello, test$%^!", @azimuth.greet("test$%^"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let a = 10
  let b = 5
  let c = 3
  
  // 计算：(a + b) * c - a
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -a)
  
  @azimuth.assert_eq(35, result)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与负数相加
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  
  // 最小值与正数相加
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 最大值与0相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  
  // x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  
  // x * 1 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  
  // x * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}