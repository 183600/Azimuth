// 标准 MoonBit 综合测试用例
// 测试 azimuth 包的核心功能

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值加法
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  
  // 测试极值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0和1的乘法
  assert_eq(0, multiply(123, 0))
  assert_eq(123, multiply(123, 1))
  assert_eq(-123, multiply(123, -1))
  
  // 测试极值乘法
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(46341, 46341))
}

test "greet_comprehensive" {
  // 测试问候函数的综合情况
  // 基本测试
  assert_eq_string("Hello, World!", greet("World"))
  
  // 特殊字符测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  
  // Unicode测试
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 5)), add(multiply(a, b), multiply(a, 5)))
}

test "complex_calculations" {
  // 测试复合计算
  // 计算矩形面积和周长
  let length = 15
  let width = 8
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(120, area)
  assert_eq(46, perimeter)
  
  // 计算平均值
  let scores = add(add(85, 90), 95)
  let average = scores / 3
  assert_eq(90, average)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 银行利息计算
  let principal = 10000
  let rate = 5
  let years = 3
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(160000, total)
  
  // 购物车计算
  let item1 = multiply(199, 2)
  let item2 = multiply(299, 1)
  let shipping = 20
  let cart_total = add(add(item1, item2), shipping)
  assert_eq(717, cart_total)
}

test "error_handling" {
  // 测试错误处理
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("success", "success")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}

test "performance_calculations" {
  // 测试性能相关计算
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000, multiply(large_num1, 2))
  
  // 连续运算
  let result = add(multiply(100, 20), multiply(50, 10))
  assert_eq(2500, result)
}

test "string_processing" {
  // 测试字符串处理
  // 长字符串处理
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
  
  // 特殊格式字符串
  let formatted_name = "user_2023_project"
  assert_eq_string("Hello, " + formatted_name + "!", greet(formatted_name))
  
  // 混合语言字符串
  let mixed_name = "Test用户123"
  assert_eq_string("Hello, " + mixed_name + "!", greet(mixed_name))
}

test "boundary_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 各种边界值组合
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 连续边界运算
  let chain_result = add(multiply(add(max_val, min_val), 1), max_val)
  assert_eq(max_val, chain_result)
}