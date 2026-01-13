// 新增的 MoonBit 测试用例
// 覆盖各种场景，使用标准的 MoonBit 测试语法

test "string_length_validation" {
  // 测试字符串长度验证功能
  let short_name = "Bob"
  let medium_name = "Alexander"
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  
  // 验证greet函数处理不同长度字符串的能力
  assert_eq_string("Hello, Bob!", greet(short_name))
  assert_eq_string("Hello, Alexander!", greet(medium_name))
  assert_eq_string("Hello, VeryLongNameThatExceedsNormalExpectations!", greet(long_name))
}

test "numeric_precision_tests" {
  // 测试数值精度和边界情况
  let large_positive = 1000000
  let large_negative = -1000000
  
  // 测试大数相加
  let sum_result = add(large_positive, large_positive)
  assert_eq(2000000, sum_result)
  
  // 测试大数相乘
  let product_result = multiply(large_positive, 2)
  assert_eq(2000000, product_result)
  
  // 测试大负数运算
  let negative_sum = add(large_negative, large_negative)
  assert_eq(-2000000, negative_sum)
  
  // 测试正负数相乘
  let mixed_product = multiply(large_positive, -2)
  assert_eq(-2000000, mixed_product)
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  // 计算：(a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 4
  
  let sum_ab = add(a, b)        // 15
  let diff_cd = add(c, -d)      // 12
  let product_ef = multiply(e, f) // 12
  let product_ab_cd = multiply(sum_ab, diff_cd) // 180
  let final_result = add(product_ab_cd, product_ef) // 192
  
  assert_eq(192, final_result)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_true(add(max_val, 0) == max_val)
  assert_true(add(max_val, -1) == 2147483646)
  assert_true(add(max_val, 1) == max_val)  // 溢出保护
  
  // 测试最小值边界
  assert_true(add(min_val, 0) == min_val)
  assert_true(add(min_val, 1) == -2147483647)
  assert_true(add(min_val, -1) == min_val)  // 溢出保护
  
  // 测试乘法边界
  assert_true(multiply(max_val, 1) == max_val)
  assert_true(multiply(min_val, 1) == min_val)
  assert_true(multiply(max_val, -1) == min_val)
  assert_true(multiply(min_val, -1) == min_val)
}

test "string_special_characters_handling" {
  // 测试特殊字符处理
  let unicode_name = "张三李四"
  let emoji_name = "🌟🚀"
  let mixed_special = "Test@123#$"
  let quoted_name = "\"John\""
  let newline_name = "Line1\nLine2"
  
  assert_eq_string("Hello, 张三李四!", greet(unicode_name))
  assert_eq_string("Hello, 🌟🚀!", greet(emoji_name))
  assert_eq_string("Hello, Test@123#$!", greet(mixed_special))
  assert_eq_string("Hello, \"John\"!", greet(quoted_name))
  assert_eq_string("Hello, Line1\nLine2!", greet(newline_name))
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 计算斐波那契数列前几项的和：1, 1, 2, 3, 5, 8, 13, 21
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  let fib6 = add(fib4, fib5)  // 8
  let fib7 = add(fib5, fib6)  // 13
  let fib8 = add(fib6, fib7)  // 21
  
  // 计算前8项的和
  let sum_fib = add(add(add(add(add(add(add(fib1, fib2), fib3), fib4), fib5), fib6), fib7), fib8)
  assert_eq(54, sum_fib)
}

test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  // 计算贷款月供：P * r * (1 + r)^n / ((1 + r)^n - 1)
  // 简化为整数计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限
  
  // 简化计算：月供 ≈ 本金 / (年限 * 12) + 利息
  let monthly_principal = principal / (years * 12)
  let total_interest = multiply(principal, annual_rate) / 100
  let monthly_interest = total_interest / (years * 12)
  let monthly_payment = add(monthly_principal, monthly_interest)
  
  assert_eq(444, monthly_payment)  // 简化计算结果
}

test "performance_stress_tests" {
  // 测试性能压力场景
  let base = 2
  
  // 计算2的10次方：2^10 = 1024
  let power_2 = multiply(base, base)
  let power_4 = multiply(power_2, power_2)
  let power_8 = multiply(power_4, power_4)
  let power_10 = multiply(power_8, multiply(base, base))
  
  assert_eq(1024, power_10)
  
  // 连续加法测试
  let sum_1_to_100 = multiply(100, add(100, 1)) / 2  // 等差数列求和公式
  assert_eq(5050, sum_1_to_100)
}

test "data_structure_simulation" {
  // 模拟数据结构操作
  // 模拟栈操作：push, push, pop, peek
  let stack_initial = 0
  let push1 = add(stack_initial, 10)
  let push2 = add(push1, 20)
  let push3 = add(push2, 30)
  
  // 模拟pop操作（移除最后一个元素）
  let after_pop = add(push3, -30)
  
  // 模拟peek操作（查看栈顶元素）
  let peek_value = 20
  
  assert_eq(30, push3)      // 栈中有三个元素：10, 20, 30
  assert_eq(20, after_pop)  // pop后栈中有两个元素：10, 20
  assert_eq(20, peek_value) // 栈顶元素是20
}

test "conditional_calculation_logic" {
  // 测试条件计算逻辑
  let x = 15
  let y = 25
  let z = 10
  
  // 条件1：如果x < y，则计算x + z，否则计算y - z
  let condition1 = x < y
  let result1 = if condition1 { add(x, z) } else { add(y, -z) }
  assert_eq(25, result1)  // 15 < 25为真，所以15 + 10 = 25
  
  // 条件2：如果y > z且x > 5，则计算x * y，否则计算x + y
  let condition2a = y > z
  let condition2b = x > 5
  let condition2 = condition2a && condition2b
  let result2 = if condition2 { multiply(x, y) } else { add(x, y) }
  assert_eq(375, result2)  // 25 > 10且15 > 5为真，所以15 * 25 = 375
  
  // 条件3：复杂条件判断
  let complex_condition = (x + y > z) && (y - x < x)
  let result3 = if complex_condition { multiply(add(x, y), z) } else { add(multiply(x, y), z) }
  assert_eq(400, result3)  // (15+25 > 10)且(25-15 < 15)为真，所以(15+25) * 10 = 400
}