// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能的不同场景

test "add_zero_handling" {
  // 测试加法的零值处理
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(0, -3))
  @azimuth.assert_eq(-3, @azimuth.add(-3, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-1, @azimuth.add(-5, 4))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000001, @azimuth.add(1000000, 1))
  @azimuth.assert_eq(-2000000, @azimuth.add(-1000000, -1000000))
  @azimuth.assert_eq(-999999, @azimuth.add(-1000000, 1))
}

test "multiply_zero_and_one" {
  // 测试乘法的0和1处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.multiply(1000, 2000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
}

test "greet_empty_and_whitespace" {
  // 测试空字符串和空白字符的问候语
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
}

test "greet_long_names" {
  // 测试长名称的问候语
  let long_name = "VeryLongNameThatExceedsNormalLength"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  let very_long_name = "ThisIsAnExtremelyLongNameThatWouldBeUnusualInPracticeButShouldStillWork"
  @azimuth.assert_eq_string("Hello, " + very_long_name + "!", @azimuth.greet(very_long_name))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  // (a + b) * (c - d)
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let result = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(c, -d))
  @azimuth.assert_eq(120, result)  // (10 + 5) * (20 - 8) = 15 * 12 = 180
  
  // a * b + c * d
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(210, result2)  // 10 * 5 + 20 * 8 = 50 + 160 = 210
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近最大值的加法
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 100, 99))
  
  // 接近最小值的加法
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 100, -99))
  
  // 负数乘法测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 组合运算
  let result = @azimuth.add(@azimuth.multiply(100, 100), @azimuth.multiply(200, 50))
  @azimuth.assert_eq(20000, result)  // 10000 + 10000 = 20000
}