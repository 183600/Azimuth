// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库核心功能的全面测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-5, 2))
  assert_eq(100000, add(50000, 50000))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(64, multiply(8, 8))
  assert_eq(0, multiply(0, 12345))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(1, 1))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(-3, divide_with_ceil(-11, 4))
  assert_eq(3, divide_with_ceil(-11, -4))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "greet_function_international_support" {
  // 测试问候函数的国际化和特殊字符支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 计算复利（简化版本）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  
  let total_sold = multiply(daily_sales, days)
  let remaining_stock = add(initial_stock, -total_sold)
  let restock_boxes = divide_with_ceil(remaining_stock, 100)
  
  assert_eq(175, total_sold)
  assert_eq(325, remaining_stock)
  assert_eq(4, restock_boxes)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算
  let first_term = 3
  let difference = 4
  let terms = 6
  
  // 计算等差数列第n项：a + (n-1)d
  let sixth_term = add(first_term, multiply(terms - 1, difference))
  
  // 计算前n项和：n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(multiply(terms, add(multiply(2, first_term), multiply(terms - 1, difference))), 2)
  
  assert_eq(23, sixth_term)
  assert_eq(78, sum)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -allocated_budget)
  let additional_projects = divide_with_ceil(remaining_budget, 600)
  
  assert_eq(3500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(3, additional_projects)
}

test "time_and_work_calculation" {
  // 时间与工作量计算
  let total_tasks = 95
  let workers = 4
  let tasks_per_worker_per_day = 6
  let work_days_per_week = 5
  
  let days_needed = divide_with_ceil(total_tasks, multiply(workers, tasks_per_worker_per_day))
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  let total_work_hours = multiply(days_needed, multiply(workers, 8))
  
  assert_eq(4, days_needed)
  assert_eq(1, weeks_needed)
  assert_eq(128, total_work_hours)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试除零安全处理
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(0, -2147483648))
  assert_eq(0, divide_with_ceil(0, 2147483647))
  
  // 测试边界加法
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}