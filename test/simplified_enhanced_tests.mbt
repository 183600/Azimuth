// 新增的 MoonBit 测试用例 - 简洁版本
// 包含 10 个高质量的测试用例，使用标准的 MoonBit 测试语法

test "enhanced_boundary_values" {
  // 增强的边界值测试
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
  assert_eq(0, add(2147483647, -2147483647))
}

test "enhanced_division_precision" {
  // 增强的除法精度测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "enhanced_unicode_support" {
  // 增强的Unicode支持测试
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "enhanced_math_expressions" {
  // 增强的数学表达式测试
  let x = 10
  let y = 20
  let z = 5
  
  let result1 = multiply(add(x, y), z)
  assert_eq(150, result1)
  
  let result2 = add(multiply(x, y), z)
  assert_eq(205, result2)
}

test "enhanced_financial_calculation" {
  // 增强的金融计算测试
  let principal = 1000
  let rate_percent = 5
  
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, rate_percent), 100))
  
  assert_eq(1050, amount_after_year1)
  assert_eq(1103, amount_after_year2)
}

test "enhanced_inventory_management" {
  // 增强的库存管理测试
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  
  let total_sold = multiply(daily_sales, days_to_simulate)
  let final_stock = add(initial_stock, -total_sold)
  
  assert_eq(84, total_sold)
  assert_eq(66, final_stock)
}

test "enhanced_project_timeline" {
  // 增强的项目时间线测试
  let tasks_per_day = 5
  let total_tasks = 47
  
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(10, workdays_needed)
}

test "enhanced_energy_calculation" {
  // 增强的能源计算测试
  let daily_consumption = 120
  let days_in_month = 30
  
  let monthly_consumption = multiply(daily_consumption, days_in_month)
  
  assert_eq(3600, monthly_consumption)
}

test "enhanced_fibonacci_sequence" {
  // 增强的斐波那契数列测试
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}

test "enhanced_network_calculation" {
  // 增强的网络计算测试
  let file_size_mb = 850
  let network_speed_mbps = 50
  
  let transfer_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  assert_eq(17, transfer_time)
}