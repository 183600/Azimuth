// 新增的 MoonBit 测试用例 - 简化版本
// 覆盖边界情况和特殊场景

test "add_function_boundary_test" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(0, max_val))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
}

test "multiply_function_special_values" {
  // 测试乘法函数的特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与2的乘法（常见溢出情况）
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试与-2的乘法
  assert_eq(min_val, multiply(max_val, -2))
  assert_eq(max_val, multiply(min_val, -2))
}

test "greet_function_unicode_test" {
  // 测试字符串函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-3, 3))
  
  // 混合符号测试
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "combined_operations_simple" {
  // 测试简单的组合运算
  let a = 10
  let b = 20
  
  // 加法组合
  let sum = add(a, b)
  assert_eq(30, sum)
  
  // 乘法组合
  let product = multiply(a, b)
  assert_eq(200, product)
  
  // 混合运算
  let temp = add(a, b)
  let result = multiply(temp, 2)
  assert_eq(60, result)
}

test "overflow_protection_basic" {
  // 基本溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, min_val))
  
  // 乘法溢出保护
  assert_eq(max_val, multiply(100000, 30000))
  assert_eq(min_val, multiply(-100000, 30000))
}

test "performance_optimized_operations" {
  // 性能优化操作测试
  // 零值优化
  assert_eq(12345, add(0, 12345))
  assert_eq(12345, add(12345, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  
  // 单位元优化
  assert_eq(12345, multiply(1, 12345))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(-12345, multiply(12345, -1))
}

test "mathematical_properties_basic" {
  // 基本数学性质测试
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 小数测试（避免溢出）
  let small_x = 5
  let small_y = 3
  let small_z = 2
  let left = multiply(small_x, add(small_y, small_z))
  let right = add(multiply(small_x, small_y), multiply(small_x, small_z))
  assert_eq(left, right)
}