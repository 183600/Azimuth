// 新增测试用例，使用标准的 MoonBit 测试语法
// 测试 azimuth 模块的各种功能

test "add_positive_numbers" {
  let result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result)
}

test "add_negative_numbers" {
  let result = @azimuth.add(-10, -20)
  @azimuth.assert_eq(-30, result)
}

test "add_mixed_numbers" {
  let result = @azimuth.add(-10, 20)
  @azimuth.assert_eq(10, result)
}

test "multiply_positive_numbers" {
  let result = @azimuth.multiply(5, 6)
  @azimuth.assert_eq(30, result)
}

test "multiply_negative_numbers" {
  let result = @azimuth.multiply(-5, -6)
  @azimuth.assert_eq(30, result)
}

test "multiply_mixed_numbers" {
  let result = @azimuth.multiply(-5, 6)
  @azimuth.assert_eq(-30, result)
}

test "greet_with_name" {
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "greet_with_numbers" {
  let result = @azimuth.greet("12345")
  @azimuth.assert_eq_string("Hello, 12345!", result)
}

test "complex_calculation" {
  // 测试复杂计算：(2 + 3) * 4 = 20
  let add_result = @azimuth.add(2, 3)
  let final_result = @azimuth.multiply(add_result, 4)
  @azimuth.assert_eq(20, final_result)
}

test "edge_case_zero_operations" {
  // 测试零的运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}