// 综合单元测试 - 覆盖所有主要功能和边界情况

test "add_function_edge_cases" {
  // 测试边界值和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(0, max_val))
  
  // 最小值边界测试
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  
  // 相同数相加
  assert_eq(10, add(5, 5))
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(5, -5))
}

test "add_function_large_numbers" {
  // 测试大数相加
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(1000000, add(1500000, -500000))
}

test "multiply_function_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最小值特殊处理测试
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
  assert_eq(min_val, multiply(1, min_val))
  
  // 大数乘法测试
  assert_eq(max_val, multiply(46340, 46341))  // 接近sqrt(max_val)
}

test "multiply_function_negative_numbers" {
  // 测试负数乘法的各种组合
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(-3, 0))
  assert_eq(0, multiply(0, -5))
}

test "greet_function_edge_cases" {
  // 测试字符串拼接的边界情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello,   !", greet("   "))  // 空格测试
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(5, divide_with_ceil(13, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(9, -3))
  assert_eq(4, divide_with_ceil(11, -3))
}

test "divide_with_ceil_special_cases" {
  // 测试特殊情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(10, divide_with_ceil(10, 1))
  assert_eq(-10, divide_with_ceil(-10, 1))
  assert_eq(-10, divide_with_ceil(10, -1))
  assert_eq(10, divide_with_ceil(-10, -1))
}

test "assert_functions_comprehensive" {
  // 综合测试所有断言函数
  assert_true(1 == 1)
  assert_true(0 == 0)
  assert_true(-1 == -1)
  assert_true(true)
  
  assert_false(1 == 2)
  assert_false(false)
  
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(50, -50))
  
  assert_eq_string("Hello, Test!", greet("Test"))
  assert_eq_string("Hello, !", greet(""))
}