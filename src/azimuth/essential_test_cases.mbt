// Azimuth 核心测试用例集
// 使用标准 MoonBit 测试语法，包含8个核心测试用例

test "edge_case_zero_division" {
  // 零除法边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(2, subtract(-3, -5))
  assert_eq(-2, divide_with_ceil(-7, 3))
}

test "large_number_computations" {
  // 大数计算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(0, subtract(1000000, 1000000))
  assert_eq(100000, divide_with_ceil(1000000, 10))
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "mathematical_identity_properties" {
  // 数学恒等性质测试
  let x = 42
  
  // 加法恒等元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 减法恒等元
  assert_eq(0, subtract(x, x))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 1000
  let project_cost = 275
  let contingency_rate = 20
  
  // 计算应急资金
  let contingency = divide_with_ceil(multiply(project_cost, contingency_rate), 100)
  let total_project_cost = add(project_cost, contingency)
  
  // 计算可支持的项目数量
  let max_projects = divide_with_ceil(total_budget, total_project_cost)
  
  assert_eq(55, contingency)
  assert_eq(330, total_project_cost)
  assert_eq(4, max_projects)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 35
  let days_in_month = 30
  let reorder_threshold = 200
  
  // 计算月度总销售
  let monthly_sales = multiply(daily_sales, days_in_month)
  
  // 计算剩余库存
  let remaining_stock = subtract(initial_stock, monthly_sales)
  
  // 计算需要补货的数量（假设每次补货400件）
  let restock_batches = divide_with_ceil(subtract(reorder_threshold, remaining_stock), 400)
  
  assert_eq(1050, monthly_sales)
  assert_eq(-550, remaining_stock)
  assert_eq(2, restock_batches)
}

test "time_calculation_scenarios" {
  // 时间计算场景测试
  let hours_per_day = 24
  let minutes_per_hour = 60
  let seconds_per_minute = 60
  let days = 3
  
  // 计算总分钟数
  let total_minutes = multiply(days, multiply(hours_per_day, minutes_per_hour))
  
  // 计算总秒数
  let total_seconds = multiply(total_minutes, seconds_per_minute)
  
  // 计算需要的计时器数量（每个计时器覆盖10000秒）
  let timers_needed = divide_with_ceil(total_seconds, 10000)
  
  assert_eq(4320, total_minutes)
  assert_eq(259200, total_seconds)
  assert_eq(26, timers_needed)
}