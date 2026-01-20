// 增强的 Azimuth 测试用例
// 覆盖更多边界情况和实际应用场景

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的向上取整除法
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(-2, divide_with_ceil(6, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "add_overflow_protection" {
  // 测试加法的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_overflow_protection" {
  // 测试乘法的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负溢出
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(-46341, 46341))
}

test "string_operations_edge_cases" {
  // 测试字符串操作的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(""))
  
  // 测试长字符串
  let long_name = "这是一个非常长的名字用来测试greet函数处理长字符串的能力"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 12345
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  assert_eq(-x, multiply(x, -1))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算贷款利息
  let principal = 10000  // 本金
  let rate = 5           // 利率百分比
  let years = 3          // 年数
  
  // 计算总利息：principal * rate * years / 100
  let total_interest = divide_with_ceil(multiply(multiply(principal, rate), years), 100)
  let total_amount = add(principal, total_interest)
  
  assert_eq(11500, total_amount)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 这些测试验证断言函数在正常情况下的工作
  assert_true(1 == 1)
  assert_false(1 == 2)
  assert_eq(0, add(0, 0))
  assert_eq_string("", greet(""))
}

test "performance_critical_operations" {
  // 性能关键操作的测试
  let large_num = 1000000
  
  // 大数运算
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(2147483647, multiply(large_num, large_num))
  
  // 连续运算
  let result = add(multiply(100, 100), multiply(200, 200))
  assert_eq(50000, result)
}