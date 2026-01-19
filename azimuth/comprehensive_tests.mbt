// 综合测试用例 - 测试各种边界情况和特殊场景

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加1应该溢出
  assert_eq(max_val, add(max_val, 1))
  
  // 最小值加-1应该溢出
  assert_eq(min_val, add(min_val, -1))
  
  // 最大值加最小值
  assert_eq(-1, add(max_val, min_val))
  
  // 最小值加1
  assert_eq(min_val + 1, add(min_val, 1))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值乘以2应该溢出
  assert_eq(max_val, multiply(max_val, 2))
  
  // 最小值乘以2应该溢出
  assert_eq(min_val, multiply(min_val, 2))
  
  // 最小值乘以-1的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试大数相乘
  assert_eq(max_val, multiply(46340, 46341))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(2, add(-5, 7))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(-18, multiply(-2, 9))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(5, divide_with_ceil(13, 3))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-3, divide_with_ceil(-12, 3))
  assert_eq(-4, divide_with_ceil(-13, 3))
  
  // 除数为负数
  assert_eq(-3, divide_with_ceil(11, -3))
  assert_eq(-4, divide_with_ceil(13, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
  
  // 整除情况
  assert_eq(5, divide_with_ceil(15, 3))
  assert_eq(-5, divide_with_ceil(-15, 3))
  assert_eq(-5, divide_with_ceil(15, -3))
  assert_eq(5, divide_with_ceil(-15, -3))
}

test "greet_function_variations" {
  // 测试greet函数的各种情况
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 特殊字符!", greet("特殊字符"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 5
  let c = -3
  
  // 加法和乘法混合
  assert_eq(50, multiply(a, add(b, c)))
  assert_eq(20, add(multiply(a, b), multiply(a, c)))
  
  // 连续加法
  assert_eq(12, add(add(a, b), c))
  
  // 连续乘法
  assert_eq(-150, multiply(multiply(a, b), c))
}

test "zero_and_one_operations" {
  // 测试0和1的特殊运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  
  assert_eq(12345, add(0, 12345))
  assert_eq(12345, add(12345, 0))
  assert_eq(0, add(0, 0))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num = 1000000
  let another_large = 2000000
  
  assert_eq(3000000, add(large_num, another_large))
  assert_eq(2000000000000, multiply(large_num, another_large))
  
  // 接近边界的运算
  let near_max = 2147483000
  assert_eq(2147483647, add(near_max, 647))
}

test "assert_functions_comprehensive" {
  // 测试各种断言函数
  assert_true(1 < 2)
  assert_true(0 <= 0)
  assert_true(-5 < 0)
  
  assert_false(1 > 2)
  assert_false(0 < 0)
  assert_false(-5 > 0)
  
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(50, -50))
  
  assert_eq_string("Hello, Test!", greet("Test"))
  assert_eq_string("Hello, !", greet(""))
}