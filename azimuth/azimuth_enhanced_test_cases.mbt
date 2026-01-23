// Azimuth 增强测试用例
// 包含高质量、实用的 MoonBit 测试用例

test "add_function_performance_validation" {
  // 验证加法函数在大量计算中的性能和准确性
  let base = 1000
  let increment = 47
  let iterations = 21
  
  let mut result = base
  for i = 0; i < iterations; i = i + 1 {
    result = add(result, increment)
  }
  
  let expected = base + multiply(iterations, increment)
  assert_eq(expected, result)
}

test "divide_with_ceil_logistics_optimization" {
  // 物流优化场景：计算最优装载方案
  let total_weight = 1875  // 总重量(kg)
  let truck_capacity = 500  // 卡车载重(kg)
  
  let trucks_needed = divide_with_ceil(total_weight, truck_capacity)
  assert_eq(4, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(2000, total_capacity)
  
  // 计算剩余运力
  let unused_capacity = subtract(total_capacity, total_weight)
  assert_eq(125, unused_capacity)
}

test "multiply_function_scaling_properties" {
  // 测试乘法函数的缩放特性
  let base_value = 7
  let scale_factor_2x = 2
  let scale_factor_10x = 10
  let scale_factor_100x = 100
  
  let doubled = multiply(base_value, scale_factor_2x)
  let ten_times = multiply(base_value, scale_factor_10x)
  let hundred_times = multiply(base_value, scale_factor_100x)
  
  assert_eq(14, doubled)
  assert_eq(70, ten_times)
  assert_eq(700, hundred_times)
  
  // 验证缩放关系
  assert_eq(multiply(doubled, 5), ten_times)
  assert_eq(multiply(ten_times, 10), hundred_times)
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  let names = [
    "Alice", "Bob", "Charlie", "张三", "李四", 
    "José", "François", "Владимир", "محمد", "田中"
  ]
  
  let expected_greetings = [
    "Hello, Alice!", "Hello, Bob!", "Hello, Charlie!", 
    "Hello, 张三!", "Hello, 李四!", "Hello, José!", 
    "Hello, François!", "Hello, Владимир!", "Hello, محمد!", 
    "Hello, 田中!"
  ]
  
  for i = 0; i < names.length(); i = i + 1 {
    assert_eq_string(expected_greetings[i], greet(names[i]))
  }
}

test "complex_resource_allocation" {
  // 复杂资源分配场景
  let projects = 15
  let team_size = 4
  let duration_weeks = 8
  let budget_per_team_per_week = 2500
  
  // 计算总团队数需求
  let teams_needed = divide_with_ceil(projects, team_size)
  assert_eq(4, teams_needed)
  
  // 计算项目总预算
  let total_weeks = multiply(duration_weeks, teams_needed)
  let total_budget = multiply(total_weeks, budget_per_team_per_week)
  assert_eq(80000, total_budget)
  
  // 计算每个项目的平均预算
  let avg_budget_per_project = divide_with_ceil(total_budget, projects)
  assert_eq(5334, avg_budget_per_project)
}

test "mathematical_sequence_validation" {
  // 数学序列验证
  let start_value = 3
  let common_difference = 7
  let terms = 12
  
  // 计算等差数列的和
  let mut sum = 0
  let mut current = start_value
  for i = 0; i < terms; i = i + 1 {
    sum = add(sum, current)
    current = add(current, common_difference)
  }
  
  // 使用公式验证: n/2 * (2a + (n-1)d)
  let formula_result = divide_with_ceil(
    multiply(terms, add(multiply(2, start_value), multiply(terms - 1, common_difference))),
    2
  )
  
  assert_eq(formula_result, sum)
}

test "divide_with_ceil_precision_analysis" {
  // 向上取整除法精度分析
  let test_cases = [
    (100, 33),   // 100/33 ≈ 3.03 → 4
    (999, 100),  // 999/100 = 9.99 → 10
    (1000, 3),   // 1000/3 ≈ 333.33 → 334
    (12345, 123), // 12345/123 ≈ 100.36 → 101
    (50, 49)     // 50/49 ≈ 1.02 → 2
  ]
  
  let expected_results = [4, 10, 334, 101, 2]
  
  for i = 0; i < test_cases.length(); i = i + 1 {
    let (dividend, divisor) = test_cases[i]
    let result = divide_with_ceil(dividend, divisor)
    assert_eq(expected_results[i], result)
  }
}

test "edge_case_comprehensive_validation" {
  // 边界情况综合验证
  // 测试极值情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 测试单位值
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, divide_with_ceil(5, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 测试负值边界
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(-5, divide_with_ceil(-5, 1))
  assert_eq(1, divide_with_ceil(-5, -5))
}

test "real_world_business_calculation" {
  // 真实世界业务计算
  // 场景：电商订单处理
  let orders_per_day = 342
  let items_per_order = 3
  let processing_capacity_per_worker = 85
  let work_days_per_month = 22
  
  // 计算日处理总量
  let daily_items = multiply(orders_per_day, items_per_order)
  
  // 计算需要的工人数量
  let workers_needed = divide_with_ceil(daily_items, processing_capacity_per_worker)
  assert_eq(13, workers_needed)
  
  // 计算月处理能力
  let monthly_capacity = multiply(workers_needed, multiply(processing_capacity_per_worker, work_days_per_month))
  assert_eq(24310, monthly_capacity)
  
  // 计算月订单处理量
  let monthly_orders = multiply(orders_per_day, work_days_per_month)
  assert_eq(7524, monthly_orders)
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}