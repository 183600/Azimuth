// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 4 * (6 + 8) = 4 * 6 + 4 * 8 = 56
}

test "divide_with_ceil_boundary_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(1, divide_with_ceil(1, 2))  // 0.5 -> 1
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(999, 10))  // 99.9 -> 100
  assert_eq(0, divide_with_ceil(0, 100))  // 零除以任何数
  assert_eq(0, divide_with_ceil(100, 0))  // 除数为零的安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(add(subtotal, tax), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(540, subtotal)      // 45 * 12 = 540
  assert_eq(44, tax)            // ceil(540 * 8 / 100) = ceil(43.2) = 44
  assert_eq(609, total_before_discount)  // 540 + 44 + 25 = 609
  assert_eq(559, final_total)   // 609 - 50 = 559
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 8
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(132, sum)  // 8/2 * (6 + 28) = 4 * 34 = 136，使用向上取整可能会有差异
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 300
  let small_projects_count = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(3500, allocated_budget)  // 1200 + 800 + 1500 = 3500
  assert_eq(1500, remaining_budget)  // 5000 - 3500 = 1500
  assert_eq(5, small_projects_count)  // ceil(1500 / 300) = 5
}

test "time_management_calculation" {
  // 时间管理计算
  let daily_working_hours = 8
  let tasks_per_hour = 3
  let total_tasks = 75
  
  // 计算每日任务数
  let daily_tasks = multiply(daily_working_hours, tasks_per_hour)
  // 计算需要的工作日数
  let workdays_needed = divide_with_ceil(total_tasks, daily_tasks)
  // 计算总工作小时数
  let total_hours = multiply(workdays_needed, daily_working_hours)
  
  assert_eq(24, daily_tasks)  // 8 * 3 = 24
  assert_eq(4, workdays_needed)  // ceil(75 / 24) = 4
  assert_eq(32, total_hours)  // 4 * 8 = 32
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 测试大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 测试负数运算
  assert_eq(-200000, add(-large_num, -large_num))
  assert_eq(10000000000, multiply(-large_num, -large_num))
  assert_eq(-10000000000, multiply(large_num, -large_num))
  
  // 测试除法的边界情况
  assert_eq(1, divide_with_ceil(1, large_num))
  assert_eq(large_num, divide_with_ceil(large_num, 1))
  assert_eq(0, divide_with_ceil(0, large_num))
  assert_eq(0, divide_with_ceil(large_num, 0))
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let prefix = "Dr."
  let first_name = "Jane"
  let last_name = "Smith"
  
  // 构建完整名称
  let full_name = prefix + " " + first_name + " " + last_name
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Jane Smith!", formal_greeting)
  
  // 测试特殊字符组合
  let special_chars = "@#$%^&*()"
  let special_greeting = greet(special_chars)
  assert_eq_string("Hello, @#$%^&*()!", special_greeting)
  
  // 测试数字和字母混合
  let mixed_string = "User123_Admin"
  let mixed_greeting = greet(mixed_string)
  assert_eq_string("Hello, User123_Admin!", mixed_greeting)
}