// MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值处理
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let length = 10
  let width = 5
  let height = 3
  
  // 计算表面积：2 × (长×宽 + 长×高 + 宽×高)
  let side1 = multiply(length, width)
  let side2 = multiply(length, height)
  let side3 = multiply(width, height)
  let surface_area = multiply(2, add(add(side1, side2), side3))
  
  assert_eq(190, surface_area)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 7
  let b = 11
  let c = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_application_calculations" {
  // 测试实际应用计算
  // 银行利息计算：本金 × (1 + 利率 × 时间)
  let principal = 10000
  let rate = 5  // 5%
  let time = 2  // 2年
  
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest)
  
  assert_eq(110000, total)  // 10000 + 10000×5×2 = 110000
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced  "))
  
  // 测试非常长的字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  assert_eq_string("Hello, " + long_string + "!", greet(long_string))
}

test "comprehensive_function_integration" {
  // 测试函数综合集成
  let base_value = 100
  let multiplier = 5
  let addition = 20
  let name = "User"
  
  // 计算结果：(base_value × multiplier) + addition
  let calculated_value = add(multiply(base_value, multiplier), addition)
  
  // 验证计算结果
  assert_eq(520, calculated_value)
  
  // 使用计算结果生成问候语
  let value_string = "520"
  let personalized_greeting = greet(name + " (score: " + value_string + ")")
  
  assert_eq_string("Hello, User (score: 520)!", personalized_greeting)
}