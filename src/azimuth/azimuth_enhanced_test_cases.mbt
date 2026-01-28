// Azimuth 增强测试用例 - 使用标准 MoonBit 测试语法
// 包含边界情况、实际应用场景和数学性质验证

test "addition_commutative_property" {
  // 验证加法交换律: a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let x = -15
  let y = 8
  assert_eq(add(x, y), add(y, x))
}

test "multiplication_distributive_property" {
  // 验证乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法的各种情况
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.33... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.33... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // 10/-3 = -3.33... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.33... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0
  assert_eq(0, divide_with_ceil(0, 5))     // 被除数为0
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 85
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算最终总价
  let total_before_discount = add(add(subtotal, tax), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(1020, subtotal)     // 85 * 12 = 1020
  assert_eq(82, tax)           // ceil(1020 * 8 / 100) = ceil(81.6) = 82
  assert_eq(1127, total_before_discount)  // 1020 + 82 + 25 = 1127
  assert_eq(1077, final_total)  // 1127 - 50 = 1077
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(78, sum)  // 6/2 * (6 + 20) = 3 * 26 = 78
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算剩余预算
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以支持的小项目数量（每个小项目成本250）
  let small_projects = divide_with_ceil(remaining, 250)
  
  assert_eq(3900, allocated)    // 1200 + 1800 + 900 = 3900
  assert_eq(1100, remaining)    // 5000 - 3900 = 1100
  assert_eq(5, small_projects)  // ceil(1100 / 250) = ceil(4.4) = 5
}

test "time_management_scenario" {
  // 时间管理场景：任务完成时间计算
  let daily_work_hours = 8
  let total_task_hours = 35
  let break_duration = 1  // 每天休息1小时
  
  // 计算实际工作小时数
  let effective_daily_hours = add(daily_work_hours, -break_duration)
  
  // 计算需要的工作天数
  let work_days = divide_with_ceil(total_task_hours, effective_daily_hours)
  
  // 计算总日历天数（包括周末）
  let calendar_days = multiply(work_days, 7) / 5  // 假设5天工作制
  
  assert_eq(7, effective_daily_hours)  // 8 - 1 = 7
  assert_eq(5, work_days)              // ceil(35 / 7) = 5
  assert_eq(7, calendar_days)          // 5 * 7 / 5 = 7
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种边界情况和异常输入
  
  // 大数运算
  let large_num1 = 100000
  let large_num2 = 200000
  assert_eq(300000, add(large_num1, large_num2))
  assert_eq(20000000000, multiply(large_num1, large_num2))
  
  // 零值运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(0, 12345))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
  
  // 负数运算验证
  assert_eq(-100, add(-50, -50))
  assert_eq(100, multiply(-10, -10))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  
  // 长字符串
  let long_name = "This is a very long name that contains many words and characters"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
  
  // 特殊符号和数字组合
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
  assert_eq_string("Hello, 100%!", greet("100%"))
}