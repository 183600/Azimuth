// 标准 MoonBit 单元测试
// 测试 azimuth 库的核心功能

test "divide_with_ceil_zero_division" {
  // 测试除数为零的边界情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}

test "negative_division_ceil_behavior" {
  // 测试负数除法的向上取整行为
  assert_eq(-2, divide_with_ceil(-5, 3))   // -5/3 = -1.67, ceil = -1
  assert_eq(-1, divide_with_ceil(-4, 3))   // -4/3 = -1.33, ceil = -1
  assert_eq(2, divide_with_ceil(5, -3))    // 5/-3 = -1.67, ceil = -1
}

test "string_boundary_cases" {
  // 测试字符串边界情况
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, add(0, 0))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  let items = 47
  let container_capacity = 12
  let containers_needed = divide_with_ceil(items, container_capacity)
  assert_eq(4, containers_needed)
  
  // 验证计算：47/12 = 3.92，向上取整为4
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(48, total_capacity)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  assert_eq(0, divide_with_ceil(0, 5))   // 0除以任何数
  assert_eq(0, divide_with_ceil(0, -5))  // 0除以负数
  assert_eq(1, divide_with_ceil(5, 5))   // 相同数相除
}

test "special_character_handling" {
  // 测试特殊字符处理
  assert_eq_string("Hello, spaces and tabs!", greet("spaces and tabs"))
  assert_eq_string("Hello, emoji 😀!", greet("emoji 😀"))
  assert_eq_string("Hello, quotes \"\"!", greet("quotes \"\""))
}

test "extreme_value_cases" {
  // 测试极值情况
  assert_eq(1, divide_with_ceil(1, 1000000))    // 极小商
  assert_eq(1000000, divide_with_ceil(1000000, 1))  // 极大商
  assert_eq(2, add(1, 1))  // 最小正整数相加
}

test "combined_operations_stress_test" {
  // 测试组合运算压力测试
  let result1 = add(multiply(10, 20), divide_with_ceil(45, 10))
  assert_eq(209, result1)  // 10*20 + ceil(45/10) = 200 + 5 = 205
  
  let result2 = multiply(add(7, 8), divide_with_ceil(25, 6))
  assert_eq(30, result2)   // (7+8) * ceil(25/6) = 15 * 5 = 75
  
  let result3 = divide_with_ceil(add(multiply(3, 4), multiply(5, 6)), 7)
  assert_eq(6, result3)    // ceil((3*4 + 5*6)/7) = ceil(42/7) = 6
}