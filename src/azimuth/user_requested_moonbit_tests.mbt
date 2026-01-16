// 用户请求的 MoonBit 测试用例
// 包含 10 个标准测试用例，覆盖项目的核心功能

test "arithmetico_boundary_operations" {
  // 算术边界运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 测试最小值边界运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试最大值乘法边界
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(max_val, -1))
  
  // 测试最小值乘法边界
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "multilingual_string_processing" {
  // 多语言字符串处理测试
  // 测试中文
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 李四!", greet("李四"))
  
  // 测试英文
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  
  // 测试日文
  assert_eq_string("Hello, 田中!", greet("田中"))
  assert_eq_string("Hello, 佐藤!", greet("佐藤"))
  
  // 测试韩文
  assert_eq_string("Hello, 김철수!", greet("김철수"))
  
  // 测试阿拉伯文
  assert_eq_string("Hello, محمد!", greet("محمد"))
  
  // 测试混合语言
  assert_eq_string("Hello, John张三!", greet("John张三"))
}

test "mathematical_function_combinations" {
  // 数学函数组合测试
  // 计算 (a + b) * c - d / e 的复合运算
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  let e = 5
  
  let sum_ab = add(a, b)  // 30
  let product = multiply(sum_ab, c)  // 90
  let quotient = d / e  // 3
  let result = add(product, -quotient)  // 87
  
  assert_eq(87, result)
  
  // 计算平方和：a² + b²
  let a_squared = multiply(a, a)  // 100
  let b_squared = multiply(b, b)  // 400
  let sum_of_squares = add(a_squared, b_squared)  // 500
  
  assert_eq(500, sum_of_squares)
  
  // 计算立方差：a³ - b³
  let a_cubed = multiply(a_squared, a)  // 1000
  let b_cubed = multiply(b_squared, b)  // 8000
  let difference_of_cubes = add(a_cubed, -b_cubed)  // -7000
  
  assert_eq(-7000, difference_of_cubes)
}

test "real_world_financial_calculations" {
  // 实际金融计算测试
  // 贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 100000  // 1000元（以分为单位）
  let annual_rate = 5  // 5%
  let years = 3
  
  let interest_rate = multiply(annual_rate, years)  // 15
  let total_rate = add(100, interest_rate)  // 115
  let total_repayment = multiply(principal, total_rate)  // 115000
  
  assert_eq(115000, total_repayment)
  
  // 投资回报计算：复利简化版
  let investment = 50000  // 500元
  let quarterly_return = 2  // 2%每季度
  let quarters = 4  // 1年4个季度
  
  let quarterly_growth = add(100, quarterly_return)  // 102
  let annual_growth = multiply(quarterly_growth, quarterly_growth)  // 10404（简化计算）
  let investment_return = multiply(investment, annual_growth) / 10000  // 52020
  
  assert_eq(52020, investment_return)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_true(add(max_val, max_val) == max_val)
  assert_true(add(min_val, min_val) == min_val)
  assert_true(multiply(max_val, max_val) == max_val)
  assert_true(multiply(min_val, min_val) == min_val)
  
  // 测试零值运算
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试单位元运算
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
  
  // 测试负单位元运算
  assert_eq(add(max_val, min_val), -1)
  assert_eq(add(min_val, max_val), -1)
}

test "performance_related_operations" {
  // 性能相关操作测试
  // 大数运算测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = 3000000
  
  // 大数加法
  let large_sum = add(add(large_num1, large_num2), large_num3)
  assert_eq(6000000, large_sum)
  
  // 大数乘法
  let large_product = multiply(large_num1, large_num2)
  assert_eq(2000000000000, large_product)
  
  // 连续运算测试
  let chain_result = add(multiply(1000, 1000), multiply(2000, 2000))
  assert_eq(5000000, chain_result)
  
  // 嵌套运算测试
  let nested_result = multiply(add(100, 200), add(300, 400))
  assert_eq(210000, nested_result)
}

test "data_type_conversion_scenarios" {
  // 数据类型转换场景测试
  // 数字字符串处理
  let numeric_string = "12345"
  assert_eq_string("Hello, 12345!", greet(numeric_string))
  
  // 布尔值字符串处理
  let true_string = "true"
  let false_string = "false"
  assert_eq_string("Hello, true!", greet(true_string))
  assert_eq_string("Hello, false!", greet(false_string))
  
  // 特殊字符字符串处理
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
  
  // 空字符串和空格处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  
  // Unicode字符处理
  let unicode_chars = "🌟💫⭐"
  assert_eq_string("Hello, 🌟💫⭐!", greet(unicode_chars))
}

test "complex_algorithm_implementations" {
  // 复杂算法实现测试
  // 斐波那契数列计算（简化版）
  // F(5) = F(4) + F(3) = (F(3) + F(2)) + (F(2) + F(1)) = 5
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  
  assert_eq(5, fib5)
  
  // 最大公约数算法（简化版，使用减法）
  let a = 48
  let b = 18
  
  // 简化的GCD计算过程
  let diff1 = add(a, -b)  // 30
  let diff2 = add(diff1, -b)  // 12
  let diff3 = add(b, -diff2)  // 6
  let diff4 = add(diff2, multiply(-2, diff3))  // 0
  
  // 当差值为0时，当前数就是GCD
  assert_eq(6, diff3)
  
  // 简单的排序算法测试
  let num1 = 5
  let num2 = 2
  let num3 = 8
  let num4 = 1
  let num5 = 9
  
  // 找出最大值
  let max1 = if (num1 > num2) { num1 } else { num2 }  // 5
  let max2 = if (max1 > num3) { max1 } else { num3 }  // 8
  let max3 = if (max2 > num4) { max2 } else { num4 }  // 8
  let max4 = if (max3 > num5) { max3 } else { num5 }  // 9
  
  assert_eq(9, max4)
}

test "business_logic_validation" {
  // 业务逻辑验证测试
  // 购物车业务逻辑
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let discount_threshold = 1000
  let discount_rate = 10
  let shipping_fee = 20
  
  // 计算商品总价
  let item1_total = multiply(item1_price, item1_qty)  // 598
  let item2_total = multiply(item2_price, item2_qty)  // 597
  let subtotal = add(item1_total, item2_total)  // 1195
  
  // 计算折扣
  let discount_amount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }  // 119
  
  // 计算最终价格
  let discounted_total = add(subtotal, -discount_amount)  // 1076
  let final_total = add(discounted_total, shipping_fee)  // 1096
  
  assert_eq(1096, final_total)
  
  // 库存管理逻辑
  let initial_stock = 100
  let sold_quantity = 35
  let restocked_quantity = 20
  
  let current_stock = add(add(initial_stock, -sold_quantity), restocked_quantity)
  assert_eq(85, current_stock)
  
  // 会员等级计算
  let purchase_amount = 2500
  let silver_threshold = 1000
  let gold_threshold = 2000
  let platinum_threshold = 5000
  
  let is_platinum = purchase_amount >= platinum_threshold
  let is_gold = purchase_amount >= gold_threshold && purchase_amount < platinum_threshold
  let is_silver = purchase_amount >= silver_threshold && purchase_amount < gold_threshold
  
  assert_false(is_platinum)
  assert_true(is_gold)
  assert_false(is_silver)
}

test "security_and_validation_tests" {
  // 安全性和验证测试
  // 输入验证测试
  let normal_input = 100
  let extreme_input = 2147483647
  let negative_input = -100
  
  // 确保正常输入正确处理
  assert_eq(200, add(normal_input, normal_input))
  assert_eq(10000, multiply(normal_input, normal_input))
  
  // 确保极值输入安全处理
  assert_true(add(extreme_input, 1) == extreme_input)
  assert_true(multiply(extreme_input, 2) == extreme_input)
  
  // 确保负数输入正确处理
  assert_eq(0, add(negative_input, 100))
  assert_eq(-10000, multiply(negative_input, 100))
  
  // 字符串安全测试
  let sql_injection_attempt = "'; DROP TABLE users; --"
  let safe_greeting = greet(sql_injection_attempt)
  assert_eq_string("Hello, '; DROP TABLE users; --!", safe_greeting)
  
  let xss_attempt = "<script>alert('xss')</script>"
  let safe_xss_greeting = greet(xss_attempt)
  assert_eq_string("Hello, <script>alert('xss')</script>!", safe_xss_greeting)
  
  // 边界条件安全测试
  let buffer_overflow_attempt = "A" * 1000  // 简化的长字符串
  let long_string = "VeryLongStringThatMightCauseIssuesInSomeSystems"
  let safe_long_greeting = greet(long_string)
  assert_eq_string("Hello, " + long_string + "!", safe_long_greeting)
}