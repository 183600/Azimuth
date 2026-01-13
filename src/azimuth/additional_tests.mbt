// 附加测试用例 - Azimuth 项目
// 这些测试用例补充了现有的测试，覆盖更多边缘情况

// 测试加法函数的极端边界情况
test "add_extreme_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与正数相加
  assert_eq(max_val, add(max_val, 100))
  
  // 测试最小值与负数相加
  assert_eq(min_val, add(min_val, -100))
  
  // 测试接近边界的值
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
}

// 测试乘法函数的极端边界情况
test "multiply_extreme_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘的溢出处理
  assert_eq(max_val, multiply(65536, 32768))  // 2^16 * 2^15 = 2^31，接近溢出边界
  
  // 测试最小值与2的乘法
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试负数的大数相乘
  assert_eq(min_val, multiply(-65536, 32768))
}

// 测试加法的零值处理
test "add_zero_handling" {
  // 测试各种零值组合
  assert_eq(0, add(0, 0))
  assert_eq(100, add(0, 100))
  assert_eq(-100, add(-100, 0))
  assert_eq(2147483647, add(0, 2147483647))
  assert_eq(-2147483648, add(-2147483648, 0))
}

// 测试乘法的零值和负一处理
test "multiply_special_values" {
  // 测试零值的各种组合
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(0, -2147483648))
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(0, multiply(-2147483648, 0))
  
  // 测试负一的特殊情况
  assert_eq(1, multiply(-1, -1))
  assert_eq(-2147483648, multiply(-2147483648, -1))
  assert_eq(2147483647, multiply(-1, -2147483647))
}

// 测试字符串处理的边缘情况
test "greet_edge_cases" {
  // 测试包含各种特殊字符的字符串
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quote\"!", greet("\"quote\""))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
  
  // 测试非常长的字符串
  let long_name = "This_is_a_very_long_name_that_tests_the_greet_function_with_extended_input"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}

// 测试复杂的数学运算组合
test "complex_mathematical_operations" {
  // 测试连续加法运算
  let result1 = add(add(10, 20), add(30, 40))
  assert_eq(100, result1)
  
  // 测试连续乘法运算
  let result2 = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result2)
  
  // 测试混合运算
  let result3 = add(multiply(10, 20), multiply(30, 40))
  assert_eq(1400, result3)
}

// 测试实际应用场景
test "real_world_scenarios" {
  // 测试温度转换场景：摄氏度转华氏度 F = C * 9/5 + 32
  let celsius = 100
  let fahrenheit = add(multiply(celsius, 9/5), 32)  // 简化为整数运算
  assert_eq(212, fahrenheit)
  
  // 测试购物车计算：商品总价
  let item_price = 199
  let quantity = 5
  let tax_rate = 8  // 8%
  let subtotal = multiply(item_price, quantity)
  let tax = multiply(subtotal, tax_rate) / 100  // 简化为整数运算
  let total = add(subtotal, tax)
  assert_eq(1073, total)  // 199*5 + (199*5*8)/100 = 995 + 78 = 1073
}

// 测试断言函数的边缘情况
test "assert_functions_edge_cases" {
  // 测试边界值的断言
  assert_eq(2147483647, 2147483647)
  assert_eq(-2147483648, -2147483648)
  assert_eq(0, 0)
  
  // 测试布尔值的边界情况
  assert_true(1 == 1)
  assert_true(0 != 1)
  assert_false(1 == 0)
  assert_false(0 != 0)
  
  // 测试字符串断言的边缘情况
  assert_eq_string("", "")
  assert_eq_string(" ", " ")
  assert_eq_string("a", "a")
}

// 测试错误处理和边界条件
test "error_handling_and_boundaries" {
  // 测试接近溢出的计算
  let near_max = 2147483640
  let small_add = 7
  assert_eq(2147483647, add(near_max, small_add))
  
  // 测试大数乘法的安全处理
  let large_num = 1000000
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 测试负数的大数乘法
  assert_eq(-1000000000000, multiply(-large_num, large_num))
  assert_eq(1000000000000, multiply(-large_num, -large_num))
}