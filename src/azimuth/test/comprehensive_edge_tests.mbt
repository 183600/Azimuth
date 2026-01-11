// 边界值和特殊情况测试

// 测试 1: 加法溢出处理 - 正数溢出
///|
test "add_positive_overflow" {
  let max_val = 2147483647
  // 接近最大值的正数相加应该返回最大值
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(1000000000, 1500000000))
}

// 测试 2: 加法溢出处理 - 负数溢出
///|
test "add_negative_overflow" {
  let min_val = -2147483648
  // 最小值加负数应该返回最小值
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(-1000000000, -1500000000))
}

// 测试 3: 乘法溢出处理 - 大数相乘
///|
test "multiply_large_numbers" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大正数相乘应该溢出
  assert_eq(max_val, multiply(50000, 50000))
  assert_eq(max_val, multiply(100000, 30000))
  
  // 大负数相乘应该溢出
  assert_eq(min_val, multiply(-50000, 50000))
  assert_eq(min_val, multiply(50000, -50000))
}

// 测试 4: 最小值特殊处理
///|
test "min_value_special_cases" {
  let min_val = -2147483648
  
  // 最小值加法测试
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  assert_eq(min_val + 1, add(min_val, 1))
  assert_eq(min_val + 100, add(min_val, 100))
  
  // 最小值乘法测试
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
}

// 测试 5: 负数运算验证
///|
test "negative_operations" {
  // 负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-50, 50))
  
  // 负数乘法
  assert_eq(21, multiply(-3, -7))
  assert_eq(2500, multiply(-50, -50))
  assert_eq(-150, multiply(-3, 50))
  assert_eq(-150, multiply(3, -50))
}

// 测试 6: 字符串拼接边界情况
///|
test "string_edge_cases" {
  // 空字符串和特殊字符
  assert_eq("Hello, !", greet(""))
  assert_eq("Hello, !", greet(" "))
  assert_eq("Hello, \n!", greet("\n"))
  assert_eq("Hello, \t!", greet("\t"))
  
  // 长字符串
  let long_name = "这是一个非常长的名字用来测试字符串拼接功能是否正常工作"
  assert_eq("Hello, " + long_name + "!", greet(long_name))
}

// 测试 7: 混合符号运算
///|
test "mixed_sign_operations" {
  // 正负数混合加法
  assert_eq(0, add(100, -100))
  assert_eq(-50, add(50, -100))
  assert_eq(50, add(-50, 100))
  
  // 正负数混合乘法
  assert_eq(-100, multiply(10, -10))
  assert_eq(100, multiply(-10, -10))
  assert_eq(0, multiply(0, -100))
}

// 测试 8: 边界值精确测试
///|
test "precise_boundary_tests" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值的精确测试
  assert_eq(max_val, add(max_val - 1, 1))
  assert_eq(max_val - 1, add(max_val - 2, 1))
  
  // 最小值的精确测试
  assert_eq(min_val + 1, add(min_val, 1))
  assert_eq(min_val + 2, add(min_val, 2))
  
  // 乘法的精确边界测试
  assert_eq(max_val, multiply(46340, 46340))  // 46340*46340 接近溢出
  assert_eq(46340, multiply(46340, 1))
}