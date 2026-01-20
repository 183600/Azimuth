// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_functionality" {
  // 测试基本加法功能
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
  azimuth::assert_eq(1, azimuth::add(5, -4))
  azimuth::assert_eq(-1, azimuth::add(-5, 4))
}

test "add_function_overflow_handling" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  azimuth::assert_eq(max_val, azimuth::add(max_val, 1))
  azimuth::assert_eq(max_val, azimuth::add(max_val, max_val))
  
  // 负数溢出
  azimuth::assert_eq(min_val, azimuth::add(min_val, -1))
  azimuth::assert_eq(min_val, azimuth::add(min_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试基本乘法功能
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(0, azimuth::multiply(0, 5))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(-6, azimuth::multiply(2, -3))
  azimuth::assert_eq(-6, azimuth::multiply(-2, 3))
  azimuth::assert_eq(6, azimuth::multiply(-2, -3))
}

test "multiply_function_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  azimuth::assert_eq(max_val, azimuth::multiply(max_val, 2))
  
  // 最小值特殊情况
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, -1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 2))
}

test "greet_function_string_operations" {
  // 测试字符串拼接功能
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正数除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(11, 3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(12, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  
  // 负数除法
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-8, 3))
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-6, 3))
  
  // 混合符号
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(5, -3))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(-5, 3))
}

test "assert_functions_functionality" {
  // 测试断言函数功能
  // assert_true 和 assert_false 测试
  azimuth::assert_true(true)
  azimuth::assert_true(1 == 1)
  azimuth::assert_true(0 < 1)
  
  azimuth::assert_false(false)
  azimuth::assert_false(1 == 2)
  azimuth::assert_false(0 > 1)
  
  // assert_eq 和 assert_eq_string 测试
  azimuth::assert_eq(42, 42)
  azimuth::assert_eq(-42, -42)
  azimuth::assert_eq_string("test", "test")
  azimuth::assert_eq_string("", "")
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 10
  let b = 3
  let c = -2
  
  // 组合运算
  azimuth::assert_eq(39, azimuth::multiply(azimuth::add(a, b), 3))  // (10 + 3) * 3 = 39
  azimuth::assert_eq(24, azimuth::add(azimuth::multiply(a, b), c))  // 10 * 3 + (-6) = 24
  azimuth::assert_eq(4, azimuth::divide_with_ceil(azimuth::multiply(a, b), 8))  // ceil(30 / 8) = 4
  
  // 嵌套运算
  azimuth::assert_eq(15, azimuth::add(azimuth::multiply(2, 5), azimuth::divide_with_ceil(10, 3)))  // 10 + 4 = 14
}

test "edge_cases_and_boundary_conditions" {
  // 测试边界条件和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值测试
  azimuth::assert_eq(max_val, azimuth::add(max_val, 0))
  azimuth::assert_eq(min_val, azimuth::add(min_val, 0))
  azimuth::assert_eq(max_val, azimuth::multiply(max_val, 1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 1))
  
  // 特殊值测试
  azimuth::assert_eq(0, azimuth::multiply(0, min_val))
  azimuth::assert_eq(0, azimuth::multiply(0, max_val))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))
}