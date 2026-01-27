// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  assert_eq(7, divide_with_ceil(20, 3))  // 6.666... -> 7
  assert_eq(3, divide_with_ceil(25, 8))  // 3.125 -> 3
  assert_eq(1, divide_with_ceil(3, 5))   // 0.6 -> 1
  assert_eq(0, divide_with_ceil(0, 10))  // 0/10 = 0 -> 0
  assert_eq(0, divide_with_ceil(50, 0))  // 除数为0的安全处理
}

test "greet_function_special_characters_and_numbers" {
  // 测试问候函数处理特殊字符和数字
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, User-123!", greet("User-123"))
  assert_eq_string("Hello, test@domain.com!", greet("test@domain.com"))
  assert_eq_string("Hello, a+b=c!", greet("a+b=c"))
}

test "combined_operations_business_scenario" {
  // 测试组合运算的业务场景
  // 计算订单总价：商品价格 × 数量 + 税费 - 折扣
  let unit_price = 45
  let quantity = 6
  let tax_rate = 10
  let discount = 25
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(27, tax_amount)
  assert_eq(297, total_before_discount)
  assert_eq(272, final_total)
}

test "mathematical_series_summation" {
  // 测试数学级数求和
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 8
  let sum = 0
  let i = 1
  while i <= n {
    sum = add(sum, multiply(2, i) - 1)
    i = i + 1
  }
  assert_eq(multiply(n, n), sum)  // 8² = 64
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 1000
  let project_a_cost = 150
  let project_b_cost = 200
  let project_c_cost = 180
  
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的额外小项目数量
  let small_project_cost = 50
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(530, allocated_budget)
  assert_eq(470, remaining_budget)
  assert_eq(10, additional_projects)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let tasks_per_hour = 4
  let working_hours_per_day = 8
  let total_tasks = 75
  
  let daily_capacity = multiply(tasks_per_hour, working_hours_per_day)
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  assert_eq(32, daily_capacity)
  assert_eq(3, days_needed)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(2147483647, -2147483646))
}