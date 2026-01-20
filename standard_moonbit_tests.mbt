// MoonBit 标准测试用例
// 测试 azimuth 包的主要功能

test "add_basic_functionality" {
  // 测试基本加法功能
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  azimuth::assert_eq(max_val, azimuth::add(max_val, 1))
  azimuth::assert_eq(min_val, azimuth::add(min_val, -1))
  azimuth::assert_eq(-1, azimuth::add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(0, azimuth::multiply(0, 5))
  azimuth::assert_eq(-6, azimuth::multiply(2, -3))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  azimuth::assert_eq(max_val, azimuth::multiply(1, max_val))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试 greet 函数
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  azimuth::assert_eq(3, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(6, 3))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(2, 5))
}

test "divide_with_ceil_negative" {
  // 测试向上取整除法负数情况
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-6, 3))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-2, 5))
}

test "complex_calculation" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 5
  // 计算 (a + b) * c
  let sum = azimuth::add(a, b)
  let result = azimuth::multiply(sum, c)
  azimuth::assert_eq(150, result)
}

test "string_processing" {
  // 测试字符串处理
  let name1 = "Alice"
  let name2 = "Bob"
  let greeting1 = azimuth::greet(name1)
  let greeting2 = azimuth::greet(name2)
  azimuth::assert_eq_string("Hello, Alice!", greeting1)
  azimuth::assert_eq_string("Hello, Bob!", greeting2)
}