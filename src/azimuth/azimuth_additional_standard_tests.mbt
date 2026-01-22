// Azimuth 库的标准测试用例
// 测试核心功能：加法、乘法、问候函数和向上取整除法

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)  // 5 * (8 + 12) = 5 * 20 = 100
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(7, divide_with_ceil(20, 3))  // 20/3 = 6.666... -> 7
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-8, 3))   // -8/3 = -2.666... -> -2
  assert_eq(-2, divide_with_ceil(-6, 3))   // -6/3 = -2 -> -2
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
}

test "combined_business_calculations" {
  // 测试组合业务计算：订单总价计算
  let unit_price = 45
  let quantity = 8
  let tax_rate = 10  // 10%
  let shipping = 15
  
  // 计算商品小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(360, subtotal)  // 45 * 8 = 360
  assert_eq(36, tax)        // ceil(360 * 10 / 100) = ceil(36) = 36
  assert_eq(411, total)     // 360 + 36 + 15 = 411
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：等差数列求和
  let first_term = 3
  let common_diff = 4
  let num_terms = 6
  
  // 手动计算等差数列：3, 7, 11, 15, 19, 23
  let term1 = first_term
  let term2 = add(term1, common_diff)
  let term3 = add(term2, common_diff)
  let term4 = add(term3, common_diff)
  let term5 = add(term4, common_diff)
  let term6 = add(term5, common_diff)
  
  let sum = add(add(add(add(add(term1, term2), term3), term4), term5), term6)
  
  assert_eq(3, term1)
  assert_eq(7, term2)
  assert_eq(11, term3)
  assert_eq(15, term4)
  assert_eq(19, term5)
  assert_eq(23, term6)
  assert_eq(78, sum)  // 3 + 7 + 11 + 15 + 19 + 23 = 78
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 1000
  let team_a_cost = 150
  let team_b_cost = 200
  let team_c_cost = 180
  
  // 计算已分配预算
  let allocated_budget = add(add(team_a_cost, team_b_cost), team_c_cost)
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个项目80元）
  let small_projects = divide_with_ceil(remaining_budget, 80)
  
  assert_eq(530, allocated_budget)  // 150 + 200 + 180 = 530
  assert_eq(470, remaining_budget)  // 1000 - 530 = 470
  assert_eq(6, small_projects)      // ceil(470 / 80) = ceil(5.875) = 6
}

test "time_management_calculations" {
  // 测试时间管理计算
  let tasks_per_hour = 4
  let working_hours = 8
  let total_tasks = 35
  
  // 计算每日任务容量
  let daily_capacity = multiply(tasks_per_hour, working_hours)
  // 计算需要的工作日数
  let workdays_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  // 计算总工作小时数
  let total_hours = multiply(workdays_needed, working_hours)
  
  assert_eq(32, daily_capacity)  // 4 * 8 = 32
  assert_eq(2, workdays_needed)   // ceil(35 / 32) = ceil(1.09375) = 2
  assert_eq(16, total_hours)      // 2 * 8 = 16
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  // 测试极值运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(100, 0))
  assert_eq(0, multiply(0, -100))
  
  // 测试边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试字符串边界
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
}

test "performance_benchmark_simulation" {
  // 测试性能基准模拟
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 12
  
  // 计算优化后的操作数
  let optimized_ops = multiply(base_operations, optimization_factor)
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percentage), 100)
  // 计算实际性能
  let actual_performance = add(optimized_ops, -overhead)
  
  assert_eq(3000, optimized_ops)
  assert_eq(360, overhead)  // ceil(3000 * 12 / 100) = ceil(360) = 360
  assert_eq(2640, actual_performance)  // 3000 - 360 = 2640
}