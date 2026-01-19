// Azimuth 综合测试套件
// 使用标准的 MoonBit 测试语法，覆盖更多边界情况

test "add_zero_handling" {
  // 测试加法的零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
}

test "add_boundary_values" {
  // 测试加法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
}

test "multiply_zero_and_one" {
  // 测试乘法的0和1处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(123, @azimuth.multiply(1, 123))
  @azimuth.assert_eq(-123, @azimuth.multiply(123, -1))
  @azimuth.assert_eq(-123, @azimuth.multiply(-1, 123))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-3, 0))
}

test "multiply_boundary_values" {
  // 测试乘法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, abcdef!", @azimuth.greet("abcdef"))
}

test "greet_special_characters" {
  // 测试问候函数的特殊字符
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "complex_add_operations" {
  // 测试复杂的加法运算
  @azimuth.assert_eq(100, @azimuth.add(25, 75))
  @azimuth.assert_eq(-100, @azimuth.add(-25, -75))
  @azimuth.assert_eq(50, @azimuth.add(100, -50))
  @azimuth.assert_eq(-50, @azimuth.add(-100, 50))
}

test "complex_multiply_operations" {
  // 测试复杂的乘法运算
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(1000, @azimuth.multiply(25, 40))
}

test "assertion_functionality" {
  // 测试断言功能
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 < 1)
  @azimuth.assert_true(-1 < 0)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 > 1)
  @azimuth.assert_false(-1 > 0)
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq_string("", "")
}