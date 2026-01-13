// 新增的 MoonBit 测试用例
// 这些测试用例覆盖了 azimuth 库的各种功能和边界情况

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
}

test "add_large_positive_numbers" {
  // 测试大正数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(2147483647, @azimuth.add(2000000000, 14748367))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(5, -5))
}

test "multiply_boundary_values" {
  // 测试边界值相乘
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "greet_with_numbers" {
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet("Test123"))
  @azimuth.assert_eq_string("Hello, 2023-12-25!", @azimuth.greet("2023-12-25"))
}

test "greet_with_spaces" {
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,  Leading Space!", @azimuth.greet(" Leading Space"))
  @azimuth.assert_eq_string("Hello, Trailing Space !", @azimuth.greet("Trailing Space "))
}

test "complex_calculation" {
  // 测试复杂计算场景
  // 计算：(5 + 3) * (10 - 2) = 8 * 8 = 64
  let add_result = @azimuth.add(5, 3)
  let subtract_result = @azimuth.add(10, -2)  // 使用加法实现减法
  let final_result = @azimuth.multiply(add_result, subtract_result)
  @azimuth.assert_eq(64, final_result)
}

test "area_calculation" {
  // 测试面积计算场景
  // 计算矩形面积：长 × 宽
  let length = 15
  let width = 8
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
}

test "sequence_operations" {
  // 测试序列运算
  // 计算：1 + 2 + 3 + 4 + 5
  let step1 = @azimuth.add(1, 2)
  let step2 = @azimuth.add(step1, 3)
  let step3 = @azimuth.add(step2, 4)
  let step4 = @azimuth.add(step3, 5)
  @azimuth.assert_eq(15, step4)
  
  // 计算：2 * 3 * 4
  let mult_step1 = @azimuth.multiply(2, 3)
  let mult_step2 = @azimuth.multiply(mult_step1, 4)
  @azimuth.assert_eq(24, mult_step2)
}