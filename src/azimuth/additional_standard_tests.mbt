// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 8
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法功能
  // 正数除法
  assert_eq(4, divide_with_ceil(13, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(3, 5))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-11, 4))
  assert_eq(3, divide_with_ceil(-11, -4))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_multilingual_support" {
  // 测试多语言问候功能
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  let discount = 25
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(subtotal, add(tax, shipping))
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(304, total_before_discount)
  assert_eq(279, final_total)
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 5
  let common_diff = 3
  let num_terms = 8
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(124, sum)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算剩余预算
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以启动的小项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(3900, allocated)
  assert_eq(1100, remaining)
  assert_eq(4, additional_projects)
}

test "time_management_calculation" {
  // 时间管理计算
  let daily_working_hours = 8
  let total_tasks = 67
  let average_task_time = 2  // 每个任务平均2小时
  
  // 计算需要的工作日
  let total_hours_needed = multiply(total_tasks, average_task_time)
  let workdays_needed = divide_with_ceil(total_hours_needed, daily_working_hours)
  
  assert_eq(134, total_hours_needed)
  assert_eq(17, workdays_needed)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(50, 0))
  assert_eq(0, divide_with_ceil(-75, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 50000
  assert_eq(100000, add(large_num, large_num))
  assert_eq(2500000000, multiply(large_num, 50000))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
  
  // 测试特殊字符组合
  let complex_name = "John-Doe O'Reilly"
  let complex_greeting = greet(complex_name)
  assert_eq_string("Hello, John-Doe O'Reilly!", complex_greeting)
}