// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
  assert_eq(-1, add(2, -3))
}

test "add_boundary_values" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, add(max_val, min_val))
  assert_eq(max_val, add(max_val - 1, 1))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_special_cases" {
  // 乘法特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(5, multiply(1, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(5, multiply(-1, -5))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(-1, min_val))
}

test "greet_function_basic" {
  // 基本问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic" {
  // 基本向上取整除法测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
}

test "combined_operations" {
  // 组合运算测试
  let a = 10
  let b = 3
  
  // 加法和乘法组合
  assert_eq(39, multiply(add(a, b), 3))
  assert_eq(33, add(multiply(a, b), 3))
  
  // 复杂表达式
  assert_eq(16, add(multiply(2, 5), divide_with_ceil(10, 3)))
  assert_eq(9, divide_with_ceil(multiply(add(5, 1), 3), 2))
}

test "mathematical_properties" {
  // 数学性质验证测试
  // 加法交换律
  assert_eq(add(5, 3), add(3, 5))
  assert_eq(add(-5, 3), add(3, -5))
  
  // 乘法交换律
  assert_eq(multiply(5, 3), multiply(3, 5))
  assert_eq(multiply(-5, 3), multiply(3, -5))
  
  // 分配律
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}

test "string_unicode_handling" {
  // Unicode 字符串处理测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, émoji😊!", greet("émoji😊"))
}

test "edge_case_comprehensive" {
  // 综合边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值组合
  assert_eq(max_val - 1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 特殊数值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1073741824, divide_with_ceil(max_val, 2))
  assert_eq(-1073741824, divide_with_ceil(min_val, 2))
}