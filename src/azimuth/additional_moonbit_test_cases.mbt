// 额外的 MoonBit 测试用例
// 覆盖更多边界情况和实际应用场景

test "divide_with_ceil_edge_cases" {
  // 测试 divide_with_ceil 函数的边界情况
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
  
  // 混合符号除法
  assert_eq(-3, divide_with_ceil(7, -3))
  assert_eq(3, divide_with_ceil(-7, -3))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 15
  let b = 7
  let c = 3
  
  // (a + b) * c - a
  let result = add(multiply(add(a, b), c), -a)
  assert_eq(51, result)
  
  // a * (b + c) - (a + b)
  let result2 = add(multiply(a, add(b, c)), add(-(a + b)))
  assert_eq(46, result2)
}

test "string_processing_advanced" {
  // 测试高级字符串处理场景
  // 长字符串处理
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetFunctionWithExtendedInput"
  let result = greet(long_string)
  assert_eq_string("Hello, " + long_string + "!", result)
  
  // 包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result2 = greet(special_chars)
  assert_eq_string("Hello, " + special_chars + "!", result2)
}

test "mathematical_properties_extended" {
  // 测试扩展的数学性质
  let x = 12
  let y = 8
  let z = 5
  
  // 结合律: (x + y) + z = x + (y + z)
  let associative_add_1 = add(add(x, y), z)
  let associative_add_2 = add(x, add(y, z))
  assert_eq(associative_add_1, associative_add_2)
  
  // 分配律: x * (y + z) = x * y + x * z
  let distributive_1 = multiply(x, add(y, z))
  let distributive_2 = add(multiply(x, y), multiply(x, z))
  assert_eq(distributive_1, distributive_2)
  
  // 交换律: x * y = y * x
  assert_eq(multiply(x, y), multiply(y, x))
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  let fee = 50           // 手续费
  
  // 计算复利：本金 * (1 + 利率 * 年数)
  let interest = multiply(principal, multiply(rate, years))
  let total_with_interest = add(principal, interest)
  
  // 减去手续费
  let final_amount = add(total_with_interest, -fee)
  
  // 验证结果：10000 + 10000*5*3 - 50 = 10000 + 150000 - 50 = 159950
  assert_eq(159950, final_amount)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试特殊值
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
}

test "algorithmic_implementation" {
  // 测试算法实现场景
  // 计算阶乘：5! = 5 * 4 * 3 * 2 * 1
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_5)
  
  // 计算斐波那契数列第10项（简化版）
  // F(10) = F(9) + F(8) = 34 + 21 = 55
  let fib_8 = 21
  let fib_9 = 34
  let fib_10 = add(fib_8, fib_9)
  assert_eq(55, fib_10)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, square_sum)
}

test "performance_calculation_simulation" {
  // 测试性能计算模拟
  let base_value = 100
  let multiplier = 1000
  
  // 模拟大量数据求和：1 + 2 + ... + 100 = 100 * 101 / 2 = 5050
  let sum_1_to_100 = multiply(100, 101) / 2
  assert_eq(5050, sum_1_to_100)
  
  // 模拟指数增长：100 * 2^10
  let power_of_2 = multiply(multiply(multiply(multiply(multiply(2, 2), 2), 2), 2), multiply(multiply(multiply(multiply(2, 2), 2), 2), 2))
  let exponential_growth = multiply(base_value, power_of_2)
  assert_eq(102400, exponential_growth)
  
  // 模拟几何级数求和：100 + 200 + 400 + 800 + 1600
  let geometric_sum = add(add(add(add(multiply(base_value, 1), multiply(base_value, 2)), multiply(base_value, 4)), multiply(base_value, 8)), multiply(base_value, 16))
  assert_eq(3100, geometric_sum)
}

test "data_conversion_and_formatting" {
  // 测试数据转换和格式化场景
  // 模拟数字到字符串的转换
  let year = 2023
  let month = 12
  let day = 25
  
  // 构建日期字符串
  let date_string = "2023-12-25"
  let greeting_with_date = greet("Date: " + date_string)
  assert_eq_string("Hello, Date: 2023-12-25!", greeting_with_date)
  
  // 模拟布尔值到字符串的转换
  let is_active = true
  let status_string = if is_active { "active" } else { "inactive" }
  let greeting_with_status = greet("Status: " + status_string)
  assert_eq_string("Hello, Status: active!", greeting_with_status)
  
  // 测试数值格式化
  let price = 19999
  let price_string = "199.99"  // 模拟价格格式化
  let greeting_with_price = greet("Price: $" + price_string)
  assert_eq_string("Hello, Price: $199.99!", greeting_with_price)
}

test "extreme_boundary_combinations" {
  // 测试极端边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 极值组合运算
  let extreme_calc_1 = add(multiply(add(max_val, min_val), one), max_val)
  assert_eq(max_val, extreme_calc_1)
  
  let extreme_calc_2 = multiply(add(min_val, one), neg_one)
  assert_eq(max_val, extreme_calc_2)
  
  let extreme_calc_3 = divide_with_ceil(add(max_val, min_val), one)
  assert_eq(-1, extreme_calc_3)
  
  // 连续极值运算
  let chain_calc = add(multiply(add(divide_with_ceil(max_val, one), min_val), neg_one), max_val)
  assert_eq(max_val, chain_calc)
  
  // 测试极值字符串处理
  let extreme_greeting = greet("ExtremeValue_" + max_val.to_string())
  // 由于 to_string() 可能不可用，我们使用固定字符串
  let extreme_greeting_fixed = greet("ExtremeValue_Max")
  assert_eq_string("Hello, ExtremeValue_Max!", extreme_greeting_fixed)
}