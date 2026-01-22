// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能测试

test "add_boundary_values" {
  // 测试加法边界值
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(2147483646, -2147483645))
  assert_eq(-1, add(-2147483647, 2147483646))
}

test "multiply_zero_properties" {
  // 测试乘法零性质
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(-2147483648, 0))
  assert_eq(0, multiply(0, 0))
}

test "divide_precision_handling" {
  // 测试除法精度处理
  assert_eq(1, divide_with_ceil(1, 2147483647))
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
  assert_eq(0, divide_with_ceil(0, 2147483647))
}

test "greet_unicode_comprehensive" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóúü!", greet("ñáéíóúü"))
  assert_eq_string("Hello, 🌟🌙⭐!", greet("🌟🌙⭐"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算管道
  let base = 15
  let multiplier = 3
  let divisor = 4
  let offset = 7
  
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, offset)
  
  assert_eq(45, step1)
  assert_eq(12, step2)
  assert_eq(19, result)
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 1000
  let rate = 5
  let periods = 3
  
  let period1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let period2 = add(period1, divide_with_ceil(multiply(period1, rate), 100))
  let period3 = add(period2, divide_with_ceil(multiply(period2, rate), 100))
  
  assert_eq(1050, period1)
  assert_eq(1103, period2)
  assert_eq(1158, period3)
}

test "inventory_optimization" {
  // 测试库存优化算法
  let demand = 127
  let batch_size = 25
  let safety_stock = 15
  
  let batches_needed = divide_with_ceil(demand, batch_size)
  let total_inventory = add(multiply(batches_needed, batch_size), safety_stock)
  
  assert_eq(6, batches_needed)
  assert_eq(165, total_inventory)
}

test "time_scheduling_efficiency" {
  // 测试时间调度效率
  let tasks_total = 37
  let tasks_per_hour = 5
  let work_hours = 8
  
  let hours_needed = divide_with_ceil(tasks_total, tasks_per_hour)
  let days_needed = divide_with_ceil(hours_needed, work_hours)
  
  assert_eq(8, hours_needed)
  assert_eq(1, days_needed)
}

test "mathematical_series_validation" {
  // 测试数学级数验证
  let n = 10
  let first_term = 3
  let difference = 2
  
  // 等差数列和公式: n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(n, add(multiply(2, first_term), multiply(n - 1, difference))),
    2
  )
  
  assert_eq(120, series_sum)
}

test "resource_allocation_optimal" {
  // 测试最优资源分配
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let contingency_percent = 10
  
  let allocated_projects = add(project_a_cost, project_b_cost)
  let contingency = divide_with_ceil(multiply(allocated_projects, contingency_percent), 100)
  let total_allocated = add(allocated_projects, contingency)
  let remaining = add(total_budget, -total_allocated)
  
  assert_eq(3000, allocated_projects)
  assert_eq(300, contingency)
  assert_eq(3300, total_allocated)
  assert_eq(1700, remaining)
}