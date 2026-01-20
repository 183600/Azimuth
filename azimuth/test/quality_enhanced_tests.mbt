// 高质量测试用例 - 覆盖核心功能和边界情况
// 专注于测试尚未充分覆盖的场景

// 测试大数运算和溢出保护
test "large_number_overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试减法溢出保护（通过加负数）
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

// 测试除法函数的边界情况
test "divide_with_ceil_edge_cases" {
  // 测试零除数错误处理
  // 注意：这里不能直接测试除零，因为会导致运行时错误
  
  // 测试被除数为零
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  
  // 测试除数为1和-1
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(5, 1))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-5, 1))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(5, -1))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(-5, -1))
  
  // 测试精确整除
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
}

// 测试字符串处理的特殊情况
test "greet_function_special_characters" {
  // 测试特殊字符和转义序列
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \"!'!", @azimuth.greet("\"!'"))
  @azimuth.assert_eq_string("Hello, \u{1F600}!", @azimuth.greet("\u{1F600}"))
  
  // 测试长字符串
  let long_name = "This_is_a_very_long_name_that_tests_the_greet_function_with_long_inputs"
  let expected = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_name))
  
  // 测试数字字符串
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 0!", @azimuth.greet("0"))
  @azimuth.assert_eq_string("Hello, -42!", @azimuth.greet("-42"))
}

// 测试数学恒等式和性质
test "mathematical_identities" {
  // 测试加法恒等式
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试乘法恒等式
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  // 测试负数性质（在安全范围内）
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(-1, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-1, 0))
}

// 测试复合运算和表达式
test "complex_expressions" {
  // 测试嵌套运算
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.multiply(3, 4), 3))
  @azimuth.assert_eq(20, @azimuth.multiply(@azimuth.add(2, 3), 4))
  
  // 测试链式运算
  let result = @azimuth.add(@azimuth.add(1, 2), @azimuth.multiply(3, 4))
  @azimuth.assert_eq(15, result)
  
  // 测试混合运算
  let a = 10
  let b = 3
  let c = -2
  @azimuth.assert_eq(29, @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, -1)))
  
  // 测试与除法结合
  @azimuth.assert_eq(8, @azimuth.add(@azimuth.divide_with_ceil(10, 3), @azimuth.multiply(2, 3)))
}

// 测试断言函数本身
test "assertion_functions_behavior" {
  // 测试布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_false(1 != 1)
  
  // 测试数值断言
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(-1, -1)
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  
  // 测试字符串断言
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("single", "single")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}

// 测试边界值组合
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与各种值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 测试最小值与各种值的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试接近边界的值
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 2, 1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 2, -1))
}

// 测试实际应用场景
test "practical_application_scenarios" {
  // 计算平均值的向上取整
  let total = 10
  let count = 3
  let average_ceil = @azimuth.divide_with_ceil(total, count)
  @azimuth.assert_eq(4, average_ceil)
  
  // 计算页数（每页固定项目数）
  let items = 25
  let items_per_page = 10
  let pages_needed = @azimuth.divide_with_ceil(items, items_per_page)
  @azimuth.assert_eq(3, pages_needed)
  
  // 价格计算（含税）
  let price = 99
  let tax_rate = 3
  let total_with_tax = @azimuth.add(price, @azimuth.divide_with_ceil(@azimuth.multiply(price, tax_rate), 100))
  @azimuth.assert_eq(101, total_with_tax)
}