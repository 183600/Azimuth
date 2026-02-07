// Azimuth 项目新增标准测试用例
// 包含 8 个高质量的 MoonBit 测试用例
// 覆盖边界情况、数学性质、真实场景和组合运算

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边界情况
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  
  // 大数精度测试
  assert_eq(100000, divide_with_ceil(199999, 2))
  assert_eq(66667, divide_with_ceil(200000, 3))
  assert_eq(33334, divide_with_ceil(100000, 3))
}

test "mathematical_operator_precedence" {
  // 测试数学运算符优先级和组合
  let a = 10
  let b = 3
  let c = 2
  
  // 测试不同运算顺序
  let result1 = add(multiply(a, b), c)  // (10 * 3) + 2 = 32
  let result2 = multiply(a, add(b, c))  // 10 * (3 + 2) = 50
  let result3 = divide_with_ceil(subtract(a, b), c)  // ceil((10-3)/2) = 4
  
  assert_eq(32, result1)
  assert_eq(50, result2)
  assert_eq(4, result3)
}

test "negative_number_comprehensive_operations" {
  // 负数综合运算测试
  let positive = 15
  let negative = -8
  let zero = 0
  
  // 加法与负数
  assert_eq(7, add(positive, negative))
  assert_eq(-23, add(negative, subtract(0, positive)))
  assert_eq(-8, add(negative, zero))
  
  // 乘法与负数
  assert_eq(-120, multiply(positive, negative))
  assert_eq(64, multiply(negative, negative))
  assert_eq(0, multiply(negative, zero))
  
  // 除法与负数
  assert_eq(-2, divide_with_ceil(negative, positive))
  assert_eq(-2, divide_with_ceil(positive, negative))
  assert_eq(2, divide_with_ceil(negative, negative))
  assert_eq(0, divide_with_ceil(negative, zero))
}

test "greet_function_unicode_and_special_cases" {
  // 问候函数的 Unicode 和特殊情况测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  
  // 特殊字符和空白
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, spaces in middle!", greet("spaces in middle"))
}

test "real_world_shipping_calculation" {
  // 真实世界物流计算场景
  let total_weight = 127  // 总重量(kg)
  let max_weight_per_box = 15  // 每箱最大重量(kg)
  let cost_per_box = 25  // 每箱成本
  let budget = 250  // 预算
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_weight, max_weight_per_box)
  assert_eq(9, boxes_needed)  // ceil(127/15) = 9
  
  // 计算总成本
  let total_cost = multiply(boxes_needed, cost_per_box)
  assert_eq(225, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(budget, total_cost)
  assert_eq(25, remaining_budget)
  
  // 计算实际容量
  let actual_capacity = multiply(boxes_needed, max_weight_per_box)
  let unused_capacity = subtract(actual_capacity, total_weight)
  assert_eq(8, unused_capacity)
}

test "time_calculation_scenarios" {
  // 时间计算场景测试
  let total_minutes = 475  // 总分钟数
  let minutes_per_hour = 60
  
  // 计算小时数
  let hours = divide_with_ceil(total_minutes, minutes_per_hour)
  assert_eq(8, hours)  // ceil(475/60) = 8
  
  // 计算实际覆盖的分钟数
  let covered_minutes = multiply(hours, minutes_per_hour)
  assert_eq(480, covered_minutes)
  
  // 计算额外分钟数
  let extra_minutes = subtract(covered_minutes, total_minutes)
  assert_eq(5, extra_minutes)
  
  // 复杂时间计算：工作日安排
  let tasks = 23
  let tasks_per_day = 5
  let work_days_needed = divide_with_ceil(tasks, tasks_per_day)
  assert_eq(5, work_days_needed)
  
  // 计算工作日内的总任务容量
  let total_task_capacity = multiply(work_days_needed, tasks_per_day)
  let spare_capacity = subtract(total_task_capacity, tasks)
  assert_eq(2, spare_capacity)
}

test "error_boundary_and_extreme_values" {
  // 错误边界和极值测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 极值加法测试
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  assert_eq(0, add(max_int, add(min_int, 1)))  // 避免溢出
  
  // 极值乘法测试
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
  assert_eq(max_int, multiply(max_int, 1))
  assert_eq(min_int, multiply(min_int, 1))
  
  // 极值除法测试
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
  assert_eq(0, divide_with_ceil(max_int, 0))
  assert_eq(0, divide_with_ceil(min_int, 0))
  assert_eq(0, divide_with_ceil(0, max_int))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "comprehensive_business_workflow" {
  // 综合业务工作流测试
  // 场景：活动策划预算分配
  let total_budget = 10000
  let participants = 85
  let venue_cost_per_person = 45
  let catering_cost_per_person = 35
  let materials_budget = 2000
  
  // 计算人均成本
  let cost_per_person = add(venue_cost_per_person, catering_cost_per_person)
  assert_eq(80, cost_per_person)
  
  // 计算人员相关总成本
  let personnel_costs = multiply(participants, cost_per_person)
  assert_eq(6800, personnel_costs)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, personnel_costs)
  assert_eq(3200, remaining_budget)
  
  // 验证材料预算充足
  assert_true(materials_budget <= remaining_budget)
  
  // 计算最终剩余预算
  let final_remaining = subtract(remaining_budget, materials_budget)
  assert_eq(1200, final_remaining)
  
  // 计算预算利用率
  let budget_utilization = divide_with_ceil(multiply(subtract(total_budget, final_remaining), 100), total_budget)
  assert_eq(88, budget_utilization)  // 8800/10000 = 88%
}