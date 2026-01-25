// 标准 MoonBit 测试用例 - 针对 azimuth 库的额外测试
// 包含10个高质量的测试用例，覆盖各种使用场景

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(add(0, 100), add(100, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "greet_function_with_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, test+123!", greet("test+123"))
  assert_eq_string("Hello, a_b_c!", greet("a_b_c"))
  assert_eq_string("Hello, 100%!", greet("100%"))
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 全面测试向上取整除法的边界情况
  // 被除数小于除数
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-15, 8))
  assert_eq(3, divide_with_ceil(-20, -7))
  
  // 大数情况
  assert_eq(1001, divide_with_ceil(1001, 1))
  assert_eq(1, divide_with_ceil(1000, 1000))
}

test "complex_business_calculation" {
  // 复杂业务计算：计算订单总价
  let unit_price = 35
  let quantity = 12
  let tax_rate = 8
  let shipping = 18
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(420, subtotal)
  assert_eq(34, tax)
  assert_eq(472, total)
}

test "mathematical_series_summation" {
  // 数学级数求和：计算等差数列的和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 8
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(124, sum)
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算剩余预算
  let remaining = add(add(add(total_budget, -project_a_cost), -project_b_cost), -project_c_cost)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 350
  let small_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(1100, remaining)
  assert_eq(4, small_projects)
}

test "time_management_scheduling" {
  // 时间管理调度问题
  let daily_work_hours = 8
  let total_tasks = 75
  let average_task_duration = 2  // 每个任务平均2小时
  
  // 计算需要的总工作小时数
  let total_hours_needed = multiply(total_tasks, average_task_duration)
  
  // 计算需要的工作天数
  let work_days_needed = divide_with_ceil(total_hours_needed, daily_work_hours)
  
  assert_eq(150, total_hours_needed)
  assert_eq(19, work_days_needed)
}

test "string_validation_and_greeting" {
  // 字符串验证和问候组合测试
  let valid_username = "user123"
  let invalid_username = ""
  let special_username = "admin_2024"
  
  // 测试不同类型的用户名问候
  assert_eq_string("Hello, user123!", greet(valid_username))
  assert_eq_string("Hello, !", greet(invalid_username))
  assert_eq_string("Hello, admin_2024!", greet(special_username))
  
  // 测试数字用户名
  assert_eq_string("Hello, 2024!", greet("2024"))
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 10000
  let annual_rate = 6  // 6%
  let years = 4
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  
  // 第四年
  let year4 = add(year3, divide_with_ceil(multiply(year3, annual_rate), 100))
  
  assert_eq(10600, year1)
  assert_eq(11236, year2)
  assert_eq(11910, year3)
  assert_eq(12625, year4)
}