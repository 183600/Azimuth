// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multiply_function_boundary_values" {
  // 测试乘法函数的边界值
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
}

test "divide_with_ceil_special_cases" {
  // 测试向上取整除法的特殊情况
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "greet_function_unicode_comprehensive" {
  // 测试问候函数的 Unicode 字符处理
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c - a * b
  let result1 = add(multiply(add(a, b), c), -multiply(a, b))
  assert_eq(800, result1)  // (10 + 20) * 30 - 10 * 20 = 30 * 30 - 200 = 900 - 200 = 700
  
  // 计算 a * b + b * c + c * a
  let result2 = add(add(multiply(a, b), multiply(b, c)), multiply(c, a))
  assert_eq(1100, result2)  // 10 * 20 + 20 * 30 + 30 * 10 = 200 + 600 + 300 = 1100
}

test "negative_number_comprehensive_operations" {
  // 测试负数的全面运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律: x * (y + y) = x * y + x * y
  assert_eq(multiply(x, add(y, y)), add(multiply(x, y), multiply(x, y)))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

test "real_world_financial_calculation" {
  // 实际金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简单的复利计算（每年计算一次）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(2147483647, -2147483646))
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以支持的小项目数量（每个300）
  let small_projects = divide_with_ceil(remaining, 300)
  
  assert_eq(3500, allocated)
  assert_eq(1500, remaining)
  assert_eq(5, small_projects)
}

test "time_and_scheduling_calculation" {
  // 时间和调度计算
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  assert_eq(11, work_days_needed)
  assert_eq(3, weeks_needed)
}