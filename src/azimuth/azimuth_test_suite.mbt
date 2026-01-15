// 新增的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

// 测试基本加法运算
test "add_basic_operation" {
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-10, azimuth::add(-3, -7))
}

// 测试加法零值特殊情况
test "add_zero_special_case" {
  assert_eq(42, azimuth::add(42, 0))
  assert_eq(42, azimuth::add(0, 42))
  assert_eq(0, azimuth::add(0, 0))
}

// 测试加法边界值处理
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(-1, azimuth::add(max_val, min_val))
}

// 测试基本乘法运算
test "multiply_basic_operation" {
  assert_eq(15, azimuth::multiply(3, 5))
  assert_eq(-15, azimuth::multiply(-3, 5))
  assert_eq(15, azimuth::multiply(-3, -5))
}

// 测试乘法零值和单位值
test "multiply_zero_and_identity" {
  assert_eq(0, azimuth::multiply(123, 0))
  assert_eq(0, azimuth::multiply(0, 123))
  assert_eq(123, azimuth::multiply(123, 1))
  assert_eq(123, azimuth::multiply(1, 123))
  assert_eq(-123, azimuth::multiply(123, -1))
}

// 测试乘法边界值处理
test "multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

// 测试字符串拼接功能
test "greet_basic_functionality" {
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
}

// 测试字符串特殊字符处理
test "greet_special_characters" {
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, @#$!", azimuth::greet("@#$"))
}

// 测试向上取整除法
test "divide_with_ceil_functionality" {
  // 正数测试
  assert_eq(3, azimuth::divide_with_ceil(10, 4))  // 10/4 = 2.5 -> 3
  assert_eq(3, azimuth::divide_with_ceil(12, 4))  // 12/4 = 3 -> 3
  assert_eq(1, azimuth::divide_with_ceil(1, 2))   // 1/2 = 0.5 -> 1
  
  // 负数测试
  assert_eq(-2, azimuth::divide_with_ceil(-10, 4))  // -10/4 = -2.5 -> -2
  assert_eq(-3, azimuth::divide_with_ceil(-10, 3))  // -10/3 = -3.33 -> -3
}