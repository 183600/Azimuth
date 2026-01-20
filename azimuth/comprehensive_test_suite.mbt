// 新增的 MoonBit 测试用例
// 覆盖边界情况和特殊场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(0, max_val))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  
  // 测试最小值 + 1
  assert_eq(min_val + 1, add(min_val, 1))
  assert_eq(min_val + 1, add(1, min_val))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与2的乘法（常见溢出情况）
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试与-2的乘法
  assert_eq(min_val, multiply(max_val, -2))
  assert_eq(max_val, multiply(min_val, -2))
  
  // 测试与自身的乘法
  assert_eq(max_val, multiply(46340, 46341))  // sqrt(max_val) * sqrt(max_val + 1)
}

test "greet_function_special_cases" {
  // 测试字符串函数的特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(""))
  
  // 测试包含特殊字符的名字
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, John-Doe!", greet("John-Doe"))
  assert_eq_string("Hello, 123!", greet("123"))
  
  // 测试长字符串
  let long_name = "This is a very long name that should still work properly"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-3, 3))
  
  // 混合符号
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "combined_operations_test" {
  // 测试组合运算
  let a = 10
  let b = 20
  let c = 30
  
  // 加法组合
  let sum1 = add(a, b)
  let sum2 = add(sum1, c)
  assert_eq(60, sum2)
  
  // 乘法组合
  let product1 = multiply(a, b)
  let product2 = multiply(product1, 1)  // 乘以1不改变值
  assert_eq(200, product2)
  
  // 混合运算
  let temp = add(a, b)
  let result = multiply(temp, c)
  assert_eq(900, result)
}

test "overflow_protection_comprehensive" {
  // 全面测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护的多种情况
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 乘法溢出保护的多种情况
  assert_eq(max_val, multiply(100000, 30000))
  assert_eq(min_val, multiply(-100000, 30000))
  assert_eq(max_val, multiply(-100000, -30000))
}

test "performance_sensitive_operations" {
  // 测试性能敏感的操作
  // 大数运算
  assert_eq(1000000, add(500000, 500000))
  assert_eq(250000000, multiply(5000, 50000))
  
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

test "mathematical_properties" {
  // 测试数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 加法结合律（近似）
  let z = 10
  let result1 = add(add(x, y), z)
  let result2 = add(x, add(y, z))
  assert_eq(result1, result2)
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律（近似，在无溢出的情况下）
  let small_x = 5
  let small_y = 3
  let small_z = 2
  let left = multiply(small_x, add(small_y, small_z))
  let right = add(multiply(small_x, small_y), multiply(small_x, small_z))
  assert_eq(left, right)
}