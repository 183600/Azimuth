// 标准 MoonBit 测试用例集
// 测试 azimuth 模块的核心功能

test "add_positive_numbers" {
  // 测试正数相加
  let result = @azimuth.add(15, 25)
  @azimuth.assert_eq(40, result)
}

test "add_negative_numbers" {
  // 测试负数相加
  let result = @azimuth.add(-10, -20)
  @azimuth.assert_eq(-30, result)
}

test "add_mixed_numbers" {
  // 测试正负数相加
  let result = @azimuth.add(50, -30)
  @azimuth.assert_eq(20, result)
}

test "multiply_positive_numbers" {
  // 测试正数相乘
  let result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, result)
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  let result = @azimuth.multiply(-4, -5)
  @azimuth.assert_eq(20, result)
}

test "multiply_mixed_numbers" {
  // 测试正负数相乘
  let result = @azimuth.multiply(8, -3)
  @azimuth.assert_eq(-24, result)
}

test "greet_standard_name" {
  // 测试标准名称问候
  let result = @azimuth.greet("Alice")
  @azimuth.assert_eq_string("Hello, Alice!", result)
}

test "greet_numeric_string" {
  // 测试数字字符串问候
  let result = @azimuth.greet("123")
  @azimuth.assert_eq_string("Hello, 123!", result)
}

test "complex_calculation" {
  // 测试复合计算：(a + b) * c
  let a = 10
  let b = 5
  let c = 3
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(45, result)
}

test "boundary_value_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与0相加
  let result1 = @azimuth.add(max_val, 0)
  @azimuth.assert_eq(max_val, result1)
  
  // 测试最小值与0相加
  let result2 = @azimuth.add(min_val, 0)
  @azimuth.assert_eq(min_val, result2)
  
  // 测试最大值与1相乘
  let result3 = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, result3)
}