test "add_function_edge_cases" {
  // 测试边界值情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值相加
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, add(max_val, -max_val))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(5, add(-3, 8))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_negative_numbers" {
  // 测试负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(100, multiply(-10, -10))
}

test "multiply_function_large_numbers" {
  // 测试大数乘法
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2147483647, multiply(46340, 46340))  // 接近溢出边界
  assert_eq(-2147483648, multiply(-2147483648, 1))  // 最小值测试
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @MoonBit!", greet("@MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello,   !", greet("  "))  // 空格测试
}

test "divide_with_ceil_function_various_cases" {
  // 测试各种向上取整除法情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-3, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "combined_operations" {
  // 测试组合运算
  let a = 10
  let b = 5
  let c = 3
  
  // 先乘后加
  assert_eq(25, add(multiply(a, b), c))
  
  // 先加后乘
  assert_eq(45, multiply(add(a, b), c))
  
  // 复杂表达式
  assert_eq(8, divide_with_ceil(add(multiply(2, 3), 2), 3))
}

test "extreme_boundary_values" {
  // 测试极端边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值相关运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(0, multiply(max_val, 0))
  
  // 最小值相关运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, -1))  // 特殊情况：最小值乘以-1会溢出
}