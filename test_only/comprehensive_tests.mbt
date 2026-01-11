// 综合测试用例 - 覆盖azimuth库的各种场景

// 测试用例1: 极端边界值组合测试
///|
test "extreme_boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的加法
  assert_eq(-1, add(max_val, min_val))
  
  // 测试最大值与自身的加法（应该溢出）
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试最小值与自身的加法（应该溢出）
  assert_eq(min_val, add(min_val, min_val))
}

// 测试用例2: 乘法溢出的详细测试
///|
test "multiplication_overflow_detailed" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘溢出
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试负数相乘的正溢出
  assert_eq(max_val, multiply(-46340, -46341))
  
  // 测试最小值与最小值的乘法
  assert_eq(min_val, multiply(min_val, min_val))
}

// 测试用例3: 零值和单位元的特殊处理
///|
test "zero_and_identity_special_handling" {
  // 测试0的各种组合
  assert_eq(0, add(0, 0))
  assert_eq(42, add(0, 42))
  assert_eq(42, add(42, 0))
  
  // 测试乘法中的0处理
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 42))
  assert_eq(0, multiply(42, 0))
  
  // 测试1的单位元特性
  assert_eq(42, multiply(1, 42))
  assert_eq(42, multiply(42, 1))
  
  // 测试-1的特殊情况
  assert_eq(-42, multiply(-1, 42))
  assert_eq(-42, multiply(42, -1))
  assert_eq(42, multiply(-1, -42))
}

// 测试用例4: 字符串处理的边界情况
///|
test "string_processing_edge_cases" {
  // 测试单个字符
  assert_eq("Hello, A!", greet("A"))
  
  // 测试数字字符串
  assert_eq("Hello, 12345!", greet("12345"))
  
  // 测试包含Unicode字符的字符串
  assert_eq("Hello, 测试!", greet("测试"))
  
  // 测试包含引号的字符串
  assert_eq("Hello, \"Quote\"!", greet("\"Quote\""))
}

// 测试用例5: 连续运算的测试
///|
test "sequential_operations" {
  // 测试连续加法
  let result1 = add(add(1, 2), add(3, 4))
  assert_eq(10, result1)
  
  // 测试连续乘法
  let result2 = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result2)
  
  // 测试混合运算
  let result3 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result3)
  
  let result4 = multiply(add(2, 3), add(4, 5))
  assert_eq(45, result4)
}

// 测试用例6: 负数和正数的混合运算
///|
test "mixed_positive_negative_operations" {
  // 正负数加法
  assert_eq(0, add(100, -100))
  assert_eq(-50, add(50, -100))
  assert_eq(50, add(-50, 100))
  
  // 正负数乘法
  assert_eq(-100, multiply(10, -10))
  assert_eq(100, multiply(-10, -10))
  assert_eq(0, multiply(0, -100))
  
  // 混合运算链
  let result = add(multiply(5, -2), multiply(-3, 4))
  assert_eq(-22, result)
}

// 测试用例7: 中等大小数值的精确性测试
///|
test "medium_range_precision" {
  // 测试中等大小的数值运算
  assert_eq(1000000, add(500000, 500000))
  assert_eq(999999, add(500000, 499999))
  
  // 测试中等大小的乘法
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(99980001, multiply(9999, 9999))
  
  // 测试连续中等大小运算
  let result = multiply(add(10000, 20000), add(30000, 40000))
  assert_eq(2100000000, result)  // 30000 * 70000
}

// 测试用例8: 特殊数学性质验证
///|
test "mathematical_properties" {
  // 验证加法交换律（在非溢出情况下）
  assert_eq(add(17, 29), add(29, 17))
  assert_eq(add(-17, 29), add(29, -17))
  
  // 验证乘法交换律（在非溢出情况下）
  assert_eq(multiply(6, 7), multiply(7, 6))
  assert_eq(multiply(-6, 7), multiply(7, -6))
  
  // 验证分配律（在非溢出情况下）
  let a = 5
  let b = 3
  let c = 2
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}