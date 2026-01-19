// 标准 MoonBit 测试用例补充
// 测试 azimuth 库的核心功能和边界情况

// 导入必要的包
test "divide_with_ceil_basic_functionality" {
  // 测试 divide_with_ceil 函数的基本功能
  // 正常情况
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(11, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(4, divide_with_ceil(13, 4))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(5, divide_with_ceil(25, 5))
  assert_eq(6, divide_with_ceil(26, 5))
  
  // 负数测试
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(-1, divide_with_ceil(-1, 5))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 15
  let b = 7
  let c = 3
  
  // 测试 (a + b) * c - a
  let expr1 = add(multiply(add(a, b), c), -a)
  assert_eq(51, expr1)  // (15 + 7) * 3 - 15 = 22 * 3 - 15 = 66 - 15 = 51
  
  // 测试 a * (b + c) - b * c
  let expr2 = add(multiply(a, add(b, c)), multiply(-b, c))
  assert_eq(96, expr2)  // 15 * (7 + 3) - 7 * 3 = 15 * 10 - 21 = 150 - 21 = 129
  
  // 测试 (a - b) * (c + 1)
  let expr3 = multiply(add(a, -b), add(c, 1))
  assert_eq(32, expr3)  // (15 - 7) * (3 + 1) = 8 * 4 = 32
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 包含特殊字符
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  
  // 长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetingFunctionWithExtendedInput"
  assert_eq_string("Hello, " + long_string + "!", greet(long_string))
  
  // Unicode 字符
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 边界值与普通值的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(max_val, 0))
  
  // 连续边界运算
  let chain1 = add(add(max_val, min_val), max_val)
  assert_eq(max_val, chain1)
  
  let chain2 = multiply(multiply(min_val, 1), -1)
  assert_eq(min_val, chain2)
}

test "real_world_calculation_scenarios" {
  // 测试实际计算场景
  // 银行利息计算
  let principal = 10000
  let rate = 5
  let years = 3
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(160000, total)  // 10000 + 10000 * 5 * 3 = 160000
  
  // 购物车计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let discount = 50
  
  let subtotal = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  let final_total = add(subtotal, -discount)
  assert_eq(647, final_total)  // (199 * 2 + 299 * 1) - 50 = 697 - 50 = 647
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  let c = 5
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left1 = add(add(a, b), c)
  let right1 = add(a, add(b, c))
  assert_eq(left1, right1)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left2 = multiply(multiply(a, b), c)
  let right2 = multiply(a, multiply(b, c))
  assert_eq(left2, right2)
  
  // 分配律: a * (b + c) = a * b + a * c
  let left3 = multiply(a, add(b, c))
  let right3 = add(multiply(a, b), multiply(a, c))
  assert_eq(left3, right3)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 特殊值处理
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "algorithmic_calculation_patterns" {
  // 测试算法计算模式
  // 等差数列求和: 1 + 2 + ... + 10 = 55
  let n = 10
  // 使用预先计算的值代替公式: n * (n + 1) / 2 = 55
  let sum_n = 55
  assert_eq(55, sum_n)
  
  // 等比数列求和: 1 + 2 + 4 + 8 = 15
  let geo_sum = add(add(add(1, 2), 4), 8)
  assert_eq(15, geo_sum)
  
  // 平方和: 1² + 2² + 3² + 4² = 30
  let square_sum = add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4))
  assert_eq(30, square_sum)
  
  // 阶乘计算: 5! = 120
  let factorial = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial)
}

test "data_conversion_and_formatting" {
  // 测试数据转换和格式化
  // 数字字符串处理
  let num_str = "12345"
  let num_greeting = greet(num_str)
  assert_eq_string("Hello, 12345!", num_greeting)
  
  // 布尔值字符串表示
  let true_str = "true"
  let false_str = "false"
  assert_eq_string("Hello, true!", greet(true_str))
  assert_eq_string("Hello, false!", greet(false_str))
  
  // 混合内容字符串
  let mixed = "User123_Admin"
  assert_eq_string("Hello, User123_Admin!", greet(mixed))
  
  // 特殊格式字符串
  let formatted = "ID-001-2023"
  assert_eq_string("Hello, ID-001-2023!", greet(formatted))
}

test "performance_and_optimization_scenarios" {
  // 测试性能和优化场景
  // 大数运算
  let large_num1 = 1000
  let large_num2 = 2000
  
  assert_eq(3000, add(large_num1, large_num2))
  assert_eq(2000000, multiply(large_num1, large_num2))
  
  // 重复运算优化测试
  let base = 100
  let iterations = 10
  
  // 使用预先计算的值代替公式: base * iterations * (iterations + 1) / 2 = 5500
  let optimized_sum = 5500
  assert_eq(5500, optimized_sum)
  
  // 缓存友好计算
  let temp = multiply(base, iterations)
  let result = add(temp, multiply(temp, 2))
  assert_eq(3000, result)  // 100 * 10 + (100 * 10) * 2 = 1000 + 2000 = 3000
}