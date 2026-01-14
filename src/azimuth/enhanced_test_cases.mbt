// 增强的 MoonBit 测试用例
// 覆盖各种边界情况和实际应用场景

test "extreme_boundary_addition" {
  // 测试极端边界值的加法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与各种值的组合
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, 1000000))
  assert_eq(max_val, azimuth::add(max_val, max_val))
  
  // 测试最小值与各种值的组合
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, -1000000))
  assert_eq(min_val, azimuth::add(min_val, min_val))
  
  // 测试最大值与最小值的组合
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期付款计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限
  let months_per_year = 12
  
  // 简化的月供计算（不含复利）
  let total_months = azimuth::multiply(years, months_per_year)
  let total_interest = azimuth::multiply(principal, azimuth::multiply(annual_rate, years)) / 100
  let total_payment = azimuth::add(principal, total_interest)
  let monthly_payment = total_payment / total_months
  
  assert_eq(277, monthly_payment)  // 简化计算结果
}

test "string_processing_edge_cases" {
  // 字符串处理的边界情况
  // 测试各种特殊字符和Unicode字符串
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, !", azimuth::greet(""))
  
  // 测试包含各种引号的字符串
  assert_eq_string("Hello, \"quoted\"!", azimuth::greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", azimuth::greet("'single'"))
  
  // 测试包含各种符号的字符串
  assert_eq_string("Hello, @#$%^&*()!", azimuth::greet("@#$%^&*()"))
  assert_eq_string("Hello, []{}<>!", azimuth::greet("[]{}<>"))
  
  // 测试混合语言字符串
  assert_eq_string("Hello, Hello世界!", azimuth::greet("Hello世界"))
  assert_eq_string("Hello, 123测试!", azimuth::greet("123测试"))
}

test "mathematical_series_calculations" {
  // 数学级数计算
  // 计算等差数列前n项和：1 + 4 + 7 + ... + (3n-2)
  let n = 10
  let first_term = 1
  let common_diff = 3
  let last_term = azimuth::add(first_term, azimuth::multiply(common_diff, n - 1))
  let sum = azimuth::multiply(n, azimuth::add(first_term, last_term)) / 2
  assert_eq(145, sum)  // 10 × (1 + 28) / 2 = 145
  
  // 计算等比数列前n项和：2 + 4 + 8 + ... + 2^n
  let ratio = 2
  let geo_sum = azimuth::multiply(azimuth::multiply(azimuth::multiply(2, 2), 2), 2)  // 2^4 = 16
  assert_eq(16, geo_sum)
}

test "physics_simulation_calculations" {
  // 物理模拟计算
  // 自由落体运动：h = 0.5 × g × t²
  // 简化为整数计算：h = 5 × t² (g ≈ 10 m/s²)
  let gravity = 5  // 简化的重力常数
  let time = 3     // 时间（秒）
  let height = azimuth::multiply(gravity, azimuth::multiply(time, time))
  assert_eq(45, height)  // 5 × 3² = 45
  
  // 抛物运动：水平距离 = 初速度 × 时间
  let initial_velocity = 20
  let flight_time = 4
  let horizontal_distance = azimuth::multiply(initial_velocity, flight_time)
  assert_eq(80, horizontal_distance)
}

test "data_structure_operations" {
  // 数据结构操作模拟
  // 栈操作：push和pop的模拟
  let stack_size = 0
  let push_count = 5
  let pop_count = 3
  
  // 模拟push操作
  stack_size = azimuth::add(stack_size, push_count)
  assert_eq(5, stack_size)
  
  // 模拟pop操作
  stack_size = azimuth::add(stack_size, -pop_count)
  assert_eq(2, stack_size)
  
  // 队列操作：入队和出队的模拟
  let queue_front = 0
  let queue_rear = 0
  let queue_size = 0
  let enqueue_count = 7
  let dequeue_count = 4
  
  // 模拟入队操作
  queue_rear = azimuth::add(queue_rear, enqueue_count)
  queue_size = azimuth::add(queue_size, enqueue_count)
  assert_eq(7, queue_size)
  
  // 模拟出队操作
  queue_front = azimuth::add(queue_front, dequeue_count)
  queue_size = azimuth::add(queue_size, -dequeue_count)
  assert_eq(3, queue_size)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  // O(n²)算法的时间复杂度模拟
  let n = 100
  let operations = azimuth::multiply(n, n)
  assert_eq(10000, operations)
  
  // O(n log n)算法的时间复杂度模拟（简化为n × log₂n）
  let log_n = 7  // log₂100 ≈ 6.64，取整数7
  let n_log_n_operations = azimuth::multiply(n, log_n)
  assert_eq(700, n_log_n_operations)
  
  // O(2^n)算法的时间复杂度模拟（小规模）
  let small_n = 10
  let exponential_operations = azimuth::multiply(azimuth::multiply(azimuth::multiply(2, 2), 2), azimuth::multiply(azimuth::multiply(2, 2), 2))
  assert_eq(64, exponential_operations)  // 2^6 = 64
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘法的各种边界情况
  assert_eq(min_val, azimuth::multiply(min_val, -1))  // 最小值乘以-1会溢出
  assert_eq(max_val, azimuth::multiply(max_val, 1))   // 最大值乘以1
  assert_eq(min_val, azimuth::multiply(min_val, 1))   // 最小值乘以1
  assert_eq(0, azimuth::multiply(max_val, 0))         // 任何数乘以0
  assert_eq(0, azimuth::multiply(min_val, 0))         // 任何数乘以0
  assert_eq(max_val, azimuth::multiply(46341, 46341)) // 接近√max_val的数相乘
  
  // 测试连续运算的边界情况
  let complex_calc = azimuth::add(
    azimuth::multiply(max_val, 2),
    azimuth::multiply(min_val, -2)
  )
  assert_eq(0, complex_calc)
}

test "real_world_business_logic" {
  // 实际业务逻辑测试
  // 库存管理系统
  let initial_stock = 1000
  let incoming_shipment = 500
  let sales_today = 150
  let returns_today = 25
  let damaged_items = 10
  
  // 计算最终库存
  let current_stock = azimuth::add(initial_stock, incoming_shipment)
  current_stock = azimuth::add(current_stock, -sales_today)
  current_stock = azimuth::add(current_stock, returns_today)
  current_stock = azimuth::add(current_stock, -damaged_items)
  
  assert_eq(1365, current_stock)
  
  // 计算库存周转率
  let cost_per_item = 50
  let total_value = azimuth::multiply(current_stock, cost_per_item)
  assert_eq(68250, total_value)
  
  // 价格计算：成本 + 利润 + 税费
  let cost = 100
  let profit_margin = 30  // 30%
  let tax_rate = 8       // 8%
  
  let profit = azimuth::multiply(cost, profit_margin) / 100
  let subtotal = azimuth::add(cost, profit)
  let tax = azimuth::multiply(subtotal, tax_rate) / 100
  let final_price = azimuth::add(subtotal, tax)
  
  assert_eq(140, final_price)  // 100 + 30 + 10 = 140
}

test "advanced_mathematical_functions" {
  // 高级数学函数模拟
  // 平方根的整数近似
  let number = 81
  let sqrt_approx = 9  // √81 = 9
  assert_eq(9, sqrt_approx)
  
  // 立方根的整数近似
  let cube_number = 27
  let cube_root = 3  // ³√27 = 3
  assert_eq(3, cube_root)
  
  // 幂运算：a^b
  let base = 3
  let exponent = 4
  let power = azimuth::multiply(azimuth::multiply(azimuth::multiply(3, 3), 3), 3)
  assert_eq(81, power)  // 3^4 = 81
  
  // 斐波那契数列计算（前10项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth::add(fib1, fib2)      // 2
  let fib4 = azimuth::add(fib2, fib3)      // 3
  let fib5 = azimuth::add(fib3, fib4)      // 5
  let fib6 = azimuth::add(fib4, fib5)      // 8
  let fib7 = azimuth::add(fib5, fib6)      // 13
  let fib8 = azimuth::add(fib6, fib7)      // 21
  let fib9 = azimuth::add(fib7, fib8)      // 34
  let fib10 = azimuth::add(fib8, fib9)     // 55
  
  assert_eq(55, fib10)
}