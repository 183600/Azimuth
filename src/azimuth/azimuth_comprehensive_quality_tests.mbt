// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库的全面测试，包含数学性质、实际应用场景和边界情况

test "mathematical_identities_verification" {
  // 验证基本数学恒等式
  // 加法恒等式：a + 0 = a
  let test_value = 42
  assert_eq(test_value, add(test_value, 0))
  
  // 乘法恒等式：a * 1 = a
  assert_eq(test_value, multiply(test_value, 1))
  
  // 乘法零性质：a * 0 = 0
  assert_eq(0, multiply(test_value, 0))
  
  // 加法逆元：a + (-a) = 0
  assert_eq(0, add(test_value, -test_value))
}

test "real_world_shopping_cart" {
  // 模拟真实购物车计算
  let apple_price = 8
  let apple_qty = 3
  let banana_price = 5
  let banana_qty = 4
  let orange_price = 12
  let orange_qty = 2
  
  // 计算每种水果的小计
  let apple_subtotal = multiply(apple_price, apple_qty)
  let banana_subtotal = multiply(banana_price, banana_qty)
  let orange_subtotal = multiply(orange_price, orange_qty)
  
  // 计算总价
  let total_cost = add(add(apple_subtotal, banana_subtotal), orange_subtotal)
  
  assert_eq(24, apple_subtotal)
  assert_eq(20, banana_subtotal)
  assert_eq(24, orange_subtotal)
  assert_eq(68, total_cost)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试接近整数边界的除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(2, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 测试极值加法
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multilingual_greeting_scenarios" {
  // 多语言问候场景测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款利息计算
  let principal = 10000
  let annual_rate = 6
  let years = 3
  
  // 简化的复利计算
  let year1_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let year1_total = add(principal, year1_interest)
  
  let year2_interest = divide_with_ceil(multiply(year1_total, annual_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  let year3_interest = divide_with_ceil(multiply(year2_total, annual_rate), 100)
  let final_amount = add(year2_total, year3_interest)
  
  assert_eq(600, year1_interest)
  assert_eq(10600, year1_total)
  assert_eq(636, year2_interest)
  assert_eq(11236, year2_total)
  assert_eq(675, year3_interest)
  assert_eq(11911, final_amount)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 测试大数运算的稳定性
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000000000, multiply(big_num, big_num))
}

test "negative_number_arithmetic" {
  // 负数算术运算测试
  // 负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(-10, 10))
  
  // 负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(-5, 0))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
}

test "zero_value_operations" {
  // 零值运算测试
  // 与零的加法
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, add(0, 0))
  
  // 与零的乘法
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, 0))
  
  // 包含零的除法
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let reserve_percentage = 10
  
  // 计算项目总成本
  let total_project_cost = add(project_a_cost, project_b_cost)
  
  // 计算储备金
  let reserve_amount = divide_with_ceil(multiply(total_budget, reserve_percentage), 100)
  
  // 计算可用预算
  let available_budget = add(total_budget, -reserve_amount)
  
  // 计算剩余资金
  let remaining_funds = add(available_budget, -total_project_cost)
  
  // 计算可以启动的小项目数量
  let small_project_cost = 300
  let small_projects_count = divide_with_ceil(remaining_funds, small_project_cost)
  
  assert_eq(3000, total_project_cost)
  assert_eq(500, reserve_amount)
  assert_eq(4500, available_budget)
  assert_eq(1500, remaining_funds)
  assert_eq(5, small_projects_count)
}

test "time_management_scheduling" {
  // 时间管理调度测试
  let total_tasks = 47
  let working_hours_per_day = 8
  let task_completion_time = 2  // 每个任务需要2小时
  
  // 计算每天能完成的任务数
  let tasks_per_day = divide_with_ceil(working_hours_per_day, task_completion_time)
  
  // 计算需要的工作天数
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作时间
  let total_work_hours = multiply(total_tasks, task_completion_time)
  
  // 计算剩余时间
  let total_available_hours = multiply(work_days_needed, working_hours_per_day)
  let remaining_hours = add(total_available_hours, -total_work_hours)
  
  assert_eq(4, tasks_per_day)
  assert_eq(12, work_days_needed)
  assert_eq(94, total_work_hours)
  assert_eq(96, total_available_hours)
  assert_eq(2, remaining_hours)
}