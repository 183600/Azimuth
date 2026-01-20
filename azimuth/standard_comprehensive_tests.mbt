test "add_edge_cases_and_min_max_values" {
  // 测试加法的边界情况和极值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值测试
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, add(max_val, min_val + 1))
  
  // 边界值测试
  assert_eq(1, add(min_val, max_val))
  assert_eq(-1, add(min_val + 1, max_val))
}

test "multiply_special_values_and_edge_cases" {
  // 测试乘法的特殊值和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 特殊值测试
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(0, multiply(min_val, 0))
  
  // 边界值测试
  assert_eq(max_val, multiply(46340, 46340))
  assert_eq(-max_val, multiply(-46340, 46340))
}

test "greet_function_unicode_and_special_characters" {
  // 测试问候函数的Unicode和特殊字符处理
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_comprehensive_cases" {
  // 测试向上取整除法的综合情况
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-9, 3))
  assert_eq(-1, divide_with_ceil(-1, 100))
  
  // 混合符号情况
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "add_function_negative_numbers_and_zero" {
  // 测试加法的负数和零值处理
  assert_eq(-5, add(-2, -3))
  assert_eq(-10, add(-7, -3))
  assert_eq(0, add(-5, 5))
  assert_eq(-100, add(-100, 0))
  assert_eq(100, add(0, 100))
}

test "multiply_function_negative_combinations" {
  // 测试乘法的负数组合
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, -5))
}

test "comprehensive_boundary_value_tests" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界测试
  assert_eq(max_val, add(max_val - 100, 100))
  assert_eq(min_val, add(min_val + 100, -100))
  
  // 乘法边界测试
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(-max_val, multiply(-1, max_val))
}

test "string_operations_edge_cases" {
  // 测试字符串操作的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, very long name!", greet("very long name"))
}

test "mathematical_operations_consistency" {
  // 测试数学运算的一致性
  let a = 100
  let b = 200
  let c = -50
  
  // 加法交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(a, c), add(c, a))
  
  // 乘法交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(a, c), multiply(c, a))
  
  // 分配律验证
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "extreme_value_robustness" {
  // 测试极值处理的健壮性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法测试
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, min_val))
  
  // 极值乘法测试
  assert_eq(min_val, multiply(min_val, min_val))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 极值除法测试
  assert_eq(1, divide_with_ceil(max_val, max_val))
  assert_eq(1, divide_with_ceil(min_val, min_val))
}