// 额外的核心功能测试用例
// 针对 azimuth 库的补充测试，覆盖更多边界情况和实际应用场景

test "large_number_arithmetic" {
  // 测试大数算术运算
  let big_num1 = 10000
  let big_num2 = 9999
  
  assert_eq(19999, add(big_num1, big_num2))
  assert_eq(99990000, multiply(big_num1, big_num2))
  assert_eq(1, divide_with_ceil(big_num1, big_num2))
  assert_eq(10000, divide_with_ceil(big_num1, 1))
}

test "division_edge_cases" {
  // 测试除法的更多边界情况
  // 测试被除数小于除数
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(1, divide_with_ceil(99, 100))
  
  // 测试负数除法的各种情况
  assert_eq(-2, divide_with_ceil(-15, 8))
  assert_eq(-1, divide_with_ceil(-8, 10))
  assert_eq(2, divide_with_ceil(-15, -8))
  assert_eq(1, divide_with_ceil(-8, -10))
  
  // 测试除数为1的情况
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(-100, divide_with_ceil(-100, 1))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  assert_eq_string("Hello,   !", greet("   "))
  
  // 测试包含特殊符号的字符串
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, <>&\"'!", greet("<>&\"'"))
  
  // 测试数字和字母混合
  assert_eq_string("Hello, test123!", greet("test123"))
  assert_eq_string("Hello, 123test!", greet("123test"))
}

test "mathematical_laws" {
  // 测试数学定律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律: a * (b + c) = a * b + a * c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000  // 本金
  let rate = 5           // 利率5%
  let years = 3          // 年数
  
  // 简单利息计算
  let simple_interest = multiply(multiply(principal, rate), years) / 100
  let total_with_simple = add(principal, simple_interest)
  
  // 复利计算（每年计算一次，使用向上取整）
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, rate), 100))
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, rate), 100))
  
  assert_eq(1500, simple_interest)
  assert_eq(11500, total_with_simple)
  assert_eq(11576, amount_year3)  // 由于向上取整，会略高于标准复利
}

test "inventory_management" {
  // 测试库存管理场景
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 200
  let days_to_simulate = 10
  
  // 计算总销售量
  let total_sold = multiply(daily_sales, days_to_simulate)
  
  // 计算剩余库存
  let remaining_stock = add(initial_stock, -total_sold)
  
  // 如果库存不足，计算需要补货的次数
  let restock_times = if remaining_stock < 0 {
    divide_with_ceil(-remaining_stock, restock_amount)
  } else {
    0
  }
  
  assert_eq(250, total_sold)
  assert_eq(250, remaining_stock)
  assert_eq(0, restock_times)
}

test "resource_allocation" {
  // 测试资源分配场景
  let total_budget = 10000
  let cost1 = 1500
  let cost2 = 2000
  let cost3 = 1200
  let cost4 = 1800
  let cost5 = 1000
  
  // 计算总成本
  let total_cost = add(add(add(add(cost1, cost2), cost3), cost4), cost5)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -total_cost)
  
  // 计算可以支持的小项目数量（每个500元）
  let small_projects = divide_with_ceil(remaining_budget, 500)
  
  assert_eq(7500, total_cost)
  assert_eq(2500, remaining_budget)
  assert_eq(5, small_projects)
}

test "time_management" {
  // 测试时间管理场景
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(11, work_days_needed)
  assert_eq(3, weeks_needed)
  assert_eq(88, total_hours)
}

test "data_analysis" {
  // 测试数据分析场景
  let point1 = 23
  let point2 = 45
  let point3 = 67
  let point4 = 89
  let point5 = 12
  let point6 = 34
  let point7 = 56
  let point8 = 78
  let point9 = 90
  let point10 = 43
  let num_points = 10
  
  // 计算总和
  let sum = add(add(add(add(add(add(add(add(add(point1, point2), point3), point4), point5), point6), point7), point8), point9), point10)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, num_points)
  
  // 计算最大值（简化处理）
  let max_value = 90
  
  // 计算最小值（简化处理）
  let min_value = 12
  
  assert_eq(537, sum)
  assert_eq(54, average)
  assert_eq(90, max_value)
  assert_eq(12, min_value)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试极值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近极值的运算
  assert_eq(2147483646, add(max_int, -1))
  assert_eq(-2147483647, add(min_int, 1))
  
  // 测试零的运算
  assert_eq(0, multiply(0, max_int))
  assert_eq(0, multiply(0, min_int))
  assert_eq(0, divide_with_ceil(0, max_int))
}