// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_boundary_values" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_operations" {
  // 基本乘法运算测试
  assert_eq(12, multiply(3, 4))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(0, multiply(7, 0))
}

test "multiply_special_cases" {
  // 乘法特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_variations" {
  // greet 函数变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(16, result)
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 8
  let y = 6
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "real_world_scenario" {
  // 实际应用场景测试：计算矩形周长
  let length = 10
  let width = 5
  
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
}

test "error_boundary_handling" {
  // 错误边界处理测试
  let max_val = 2147483647
  
  // 溢出保护验证
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  
  // 断言功能验证
  assert_true(true)
  assert_false(false)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 包含特殊字符的字符串
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  
  // 长字符串处理
  let long_name = "VeryLongNameForTestingPurposes"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}