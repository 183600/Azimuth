// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，覆盖边界条件和特殊场景

test "add_zero_edge_cases" {
  // 测试加法的零值边界情况
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::add(0, -2147483648), -2147483648)
  assert_eq(azimuth::add(-2147483648, 0), -2147483648)
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  assert_eq(azimuth::add(2147483647, 1), 2147483647)
  assert_eq(azimuth::add(2147483647, 2147483647), 2147483647)
  assert_eq(azimuth::add(1000000000, 2000000000), 2147483647)
}

test "add_underflow_protection" {
  // 测试加法下溢保护
  assert_eq(azimuth::add(-2147483648, -1), -2147483648)
  assert_eq(azimuth::add(-2147483648, -2147483648), -2147483648)
  assert_eq(azimuth::add(-1000000000, -2000000000), -2147483648)
}

test "multiply_zero_and_one" {
  // 测试乘法的零值和一值情况
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(0, 42), 0)
  assert_eq(azimuth::multiply(42, 0), 0)
  assert_eq(azimuth::multiply(1, 42), 42)
  assert_eq(azimuth::multiply(42, 1), 42)
  assert_eq(azimuth::multiply(-1, 42), -42)
  assert_eq(azimuth::multiply(42, -1), -42)
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  assert_eq(azimuth::multiply(2147483647, 2), 2147483647)
  assert_eq(azimuth::multiply(1000000, 3000), 2147483647)
  assert_eq(azimuth::multiply(-2147483648, -1), -2147483648)
}

test "multiply_min_value_special_cases" {
  // 测试最小值乘法的特殊情况
  assert_eq(azimuth::multiply(-2147483648, 0), 0)
  assert_eq(azimuth::multiply(-2147483648, 1), -2147483648)
  assert_eq(azimuth::multiply(-2147483648, -1), -2147483648)
  assert_eq(azimuth::multiply(-2147483648, 2), -2147483648)
  assert_eq(azimuth::multiply(2, -2147483648), -2147483648)
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string(azimuth::greet("123"), "Hello, 123!")
  assert_eq_string(azimuth::greet("!@#$"), "Hello, !@#$!")
  assert_eq_string(azimuth::greet("中文字符"), "Hello, 中文字符!")
  assert_eq_string(azimuth::greet("emoji😊"), "Hello, emoji😊!")
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string(azimuth::greet(""), "Hello, !")
  assert_eq_string(azimuth::greet(" "), "Hello, !")
  assert_eq_string(azimuth::greet("\n"), "Hello, \n!")
  assert_eq_string(azimuth::greet("\t"), "Hello, \t!")
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  // (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(sum, c)
  let result = azimuth::add(product, -d)
  
  assert_eq(result, 75)
  
  // 测试嵌套运算
  let x = azimuth::multiply(azimuth::add(5, 3), azimuth::add(2, 4))
  assert_eq(x, 48)
}