// 增强测试用例集合
// 包含更多边界条件和特殊情况的测试

///|
test "greet_special_strings" {
  // 测试特殊字符串输入
  assert_eq("Hello, 星空!", greet("星空"))
  assert_eq("Hello, 🌙!", greet("🌙"))
  assert_eq("Hello, space   test!", greet("space   test"))
  assert_eq("Hello, !", greet(""))
  assert_eq("Hello, !", greet(""))
  assert_eq("Hello, a!", greet("a"))
  assert_eq("Hello, 1234567890!", greet("1234567890"))
}

///|
test "add_sequence_patterns" {
  // 测试连续加法的模式
  assert_eq(15, add(add(5, 5), 5))
  assert_eq(-15, add(add(-5, -5), -5))
  assert_eq(0, add(add(2147483647, -2147483647), 0))
  assert_eq(-2147483648, add(add(-2147483647, -1), 0))
}

///|
test "multiply_power_patterns" {
  // 测试幂次相关的乘法模式
  assert_eq(16, multiply(multiply(2, 2), multiply(2, 2)))
  assert_eq(0, multiply(multiply(0, 5), multiply(3, 7)))
  assert_eq(-2147483648, multiply(multiply(-2, 2), multiply(-2, 1073741824)))
}

///|
test "add_fibonacci_like" {
  // 测试类似斐波那契序列的加法
  assert_eq(1, add(0, 1))
  assert_eq(2, add(1, 1))
  assert_eq(3, add(1, 2))
  assert_eq(5, add(2, 3))
  assert_eq(8, add(3, 5))
  assert_eq(13, add(5, 8))
}

///|
test "multiply_geometric_series" {
  // 测试几何级数乘法
  assert_eq(2, multiply(1, 2))
  assert_eq(4, multiply(2, 2))
  assert_eq(8, multiply(4, 2))
  assert_eq(16, multiply(8, 2))
  assert_eq(32, multiply(16, 2))
}

///|
test "boundary_value_combinations" {
  // 测试边界值的各种组合
  assert_eq(0, add(multiply(0, 2147483647), multiply(0, -2147483648)))
  assert_eq(2147483647, add(multiply(1, 2147483647), multiply(0, -2147483648)))
  assert_eq(-2147483648, add(multiply(0, 2147483647), multiply(1, -2147483648)))
}

///|
test "negative_zero_scenarios" {
  // 测试涉及负零的场景（虽然MoonBit中可能没有负零概念）
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, -2147483648))
  assert_eq(0, multiply(0, 2147483647))
}

///|
test "large_number_patterns" {
  // 测试大数模式
  assert_eq(2147483647, add(1073741823, 1073741824))
  assert_eq(2147483647, add(1000000000, 1147483647))
  assert_eq(-2147483648, add(-1073741824, -1073741824))
  assert_eq(-2147483648, add(-1000000000, -1147483648))
}

///|
test "mixed_operations_stress" {
  // 混合运算压力测试
  let result1 = add(100, multiply(5, 10))
  assert_eq(150, result1)
  
  let result2 = multiply(add(2, 3), add(4, 5))
  assert_eq(45, result2)
  
  let result3 = add(multiply(-2, 3), multiply(4, -5))
  assert_eq(-26, result3)
}

///|
test "extreme_boundary_transitions" {
  // 极端边界转换测试
  assert_eq(2147483646, add(2147483647, -1))
  assert_eq(2147483645, add(2147483647, -2))
  assert_eq(-2147483647, add(-2147483648, 1))
  assert_eq(-2147483646, add(-2147483648, 2))
}