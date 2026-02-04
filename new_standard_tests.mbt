// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_edge_cases" {
  // 测试加法的边界情况
  assert_eq(2147483647, add(2147483646, 1))  // 接近最大值
  assert_eq(-2147483647, add(-2147483646, -1))  // 接近最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数
  assert_eq(1, add(0, 1))  // 零加一
  assert_eq(-1, add(0, -1))  // 零减一
}

test "multiplication_distributive_law" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7 * (12 + 8) = 7 * 12 + 7 * 8 = 140
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(34, divide_with_ceil(100, 3))  // 33.333... -> 34
  assert_eq(1000, divide_with_ceil(999999, 1000))  // 999.999 -> 1000
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(-2, divide_with_ceil(-4, 2))  // -2 -> -2
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let x = 15
  let y = 25
  let z = 8
  
  // 测试 (x + y) * z - y / x
  let result1 = add(multiply(add(x, y), z), -divide_with_ceil(y, x))
  assert_eq(319, result1)  // (15 + 25) * 8 - ceil(25/15) = 40*8 - 2 = 320 - 2 = 318
  
  // 测试嵌套表达式
  let result2 = divide_with_ceil(multiply(add(x, multiply(y, z)), 3), 5)
  assert_eq(123, result2)  // ceil((15 + 25*8) * 3 / 5) = ceil(215 * 3 / 5) = ceil(645/5) = 129
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10800, year1)  // 10000 + 800
  assert_eq(11664, year2)  // 10800 + 864
  assert_eq(12597, year3)  // 11664 + 933
}

test "inventory_management_system" {
  // 测试库存管理系统
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 150
  let days = 7
  
  // 计算一周的销售量
  let weekly_sales = multiply(daily_sales, days)
  // 计算补货前的库存
  let stock_before_restock = subtract(initial_stock, weekly_sales)
  // 计算最终库存
  let final_stock = add(stock_before_restock, restock_amount)
  
  assert_eq(175, weekly_sales)  // 25 * 7
  assert_eq(325, stock_before_restock)  // 500 - 175
  assert_eq(475, final_stock)  // 325 + 150
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, allocated_budget)
  // 计算可以支持的小项目数量（每个500）
  let small_projects = divide_with_ceil(remaining_budget, 500)
  
  assert_eq(8500, allocated_budget)  // 3500 + 2800 + 2200
  assert_eq(1500, remaining_budget)  // 10000 - 8500
  assert_eq(3, small_projects)  // ceil(1500/500)
}

test "time_management_calculations" {
  // 测试时间管理计算
  let tasks_per_hour = 4
  let working_hours_per_day = 8
  let total_tasks = 150
  let break_hours_per_day = 2
  
  // 计算有效工作时间
  let effective_hours_per_day = subtract(working_hours_per_day, break_hours_per_day)
  // 计算每天完成的任务数
  let tasks_per_day = multiply(tasks_per_hour, effective_hours_per_day)
  // 计算需要的天数
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(6, effective_hours_per_day)  // 8 - 2
  assert_eq(24, tasks_per_day)  // 4 * 6
  assert_eq(7, days_needed)  // ceil(150/24)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 测试除零情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 测试极小数运算
  let small_num = 1
  assert_eq(2, add(small_num, small_num))
  assert_eq(1, multiply(small_num, small_num))
}