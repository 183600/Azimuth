// Azimuth 新增标准测试用例
// 使用标准的 MoonBit 测试语法

test "add_positive_numbers" {
  // 测试正数相加
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "add_mixed_numbers" {
  // 测试正负数混合相加
  @azimuth.assert_eq(4, @azimuth.add(10, -6))
  @azimuth.assert_eq(-4, @azimuth.add(-10, 6))
  @azimuth.assert_eq(1, @azimuth.add(100, -99))
}

test "multiply_positive_numbers" {
  // 测试正数相乘
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "greet_simple_names" {
  // 测试简单名称的问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_with_spaces" {
  // 测试包含空格的名称
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, Moon Bit!", @azimuth.greet("Moon Bit"))
}

test "edge_case_zero_operations" {
  // 测试零的边界情况
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
}

test "combined_operations" {
  // 测试组合运算
  let x = 5
  let y = 3
  
  // 计算 (x + y) * y
  let sum = @azimuth.add(x, y)
  let result = @azimuth.multiply(sum, y)
  @azimuth.assert_eq(24, result)
  
  // 计算 x * (x + y)
  let sum2 = @azimuth.add(x, y)
  let result2 = @azimuth.multiply(x, sum2)
  @azimuth.assert_eq(40, result2)
}