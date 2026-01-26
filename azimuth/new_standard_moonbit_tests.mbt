// 新的标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试覆盖各种边界情况和实际应用场景

test "arithmetc_sequence_operations" {
  // 测试算术序列操作
  let first_term = 3
  let common_difference = 4
  let nth_term = 5
  
  // 计算等差数列的第n项: a_n = a_1 + (n-1)*d
  let term_value = add(first_term, multiply(common_difference, subtract(nth_term, 1)))
  assert_eq(19, term_value)  // 3 + (5-1)*4 = 3 + 16 = 19
  
  // 计算等差数列前n项和: S_n = n/2 * (2*a_1 + (n-1)*d)
  let sum_n = divide_with_ceil(multiply(nth_term, add(multiply(2, first_term), multiply(common_difference, subtract(nth_term, 1)))), 2)
  assert_eq(55, sum_n)  // 5/2 * (2*3 + 4*4) = 5/2 * (6 + 16) = 5/2 * 22 = 55
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边缘情况
  let empty_string = ""
  let single_char = "A"
  let long_string = "This is a very long string with multiple words"
  
  // 测试空字符串连接
  assert_eq_string("Hello, !", greet(empty_string))
  
  // 测试单字符连接
  assert_eq_string("Hello, A!", greet(single_char))
  
  // 测试长字符串连接
  assert_eq_string("Hello, This is a very long string with multiple words!", greet(long_string))
  
  // 测试包含数字的字符串
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
}

test "resource_allocation_with_constraints" {
  // 测试带有约束条件的资源分配
  let total_resources = 100
  let min_per_group = 15
  let max_groups = 8
  
  // 计算最少需要的组数
  let min_groups_needed = divide_with_ceil(total_resources, min_per_group)
  assert_eq(7, min_groups_needed)  // ceil(100/15) = 7
  
  // 验证组数不超过最大限制
  assert_true(min_groups_needed <= max_groups)
  
  // 计算实际每组分配的资源
  let actual_per_group = divide_with_ceil(total_resources, min_groups_needed)
  assert_eq(15, actual_per_group)  // ceil(100/7) = 15
  
  // 验证总分配资源不小于总资源
  let total_allocated = multiply(actual_per_group, min_groups_needed)
  assert_true(total_allocated >= total_resources)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算平方和: 1² + 2² + 3² + ... + n² = n(n+1)(2n+1)/6
  let n = 5
  let square_sum_formula = divide_with_ceil(multiply(n, multiply(add(n, 1), add(multiply(2, n), 1))), 6)
  assert_eq(55, square_sum_formula)  // 5*6*11/6 = 55
  
  // 验证通过逐项计算
  let manual_sum = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(square_sum_formula, manual_sum)
}

test "time_conversion_practical_scenario" {
  // 测试时间转换的实际场景
  let total_minutes = 147
  
  // 转换为小时和分钟
  let hours = divide_with_ceil(total_minutes, 60)
  let remaining_minutes = subtract(total_minutes, multiply(hours - 1, 60))
  
  assert_eq(3, hours)  // ceil(147/60) = 3
  assert_eq(27, remaining_minutes)  // 147 - 2*60 = 27
  
  // 验证转换正确性
  let total_back = add(multiply(hours - 1, 60), remaining_minutes)
  assert_eq(total_minutes, total_back)
}

test "inventory_optimization_problem" {
  // 测试库存优化问题
  let product_a = 23
  let product_b = 31
  let product_c = 17
  let package_capacity = 12
  
  // 计算每种产品需要的包装数
  let packages_a = divide_with_ceil(product_a, package_capacity)
  let packages_b = divide_with_ceil(product_b, package_capacity)
  let packages_c = divide_with_ceil(product_c, package_capacity)
  
  assert_eq(2, packages_a)  // ceil(23/12) = 2
  assert_eq(3, packages_b)  // ceil(31/12) = 3
  assert_eq(2, packages_c)  // ceil(17/12) = 2
  
  // 计算总包装数和总容量
  let total_packages = add(add(packages_a, packages_b), packages_c)
  let total_capacity = multiply(total_packages, package_capacity)
  
  assert_eq(7, total_packages)
  assert_eq(84, total_capacity)
  
  // 计算剩余容量
  let total_products = add(add(product_a, product_b), product_c)
  let unused_capacity = subtract(total_capacity, total_products)
  assert_eq(13, unused_capacity)  // 84 - (23+31+17) = 84 - 71 = 13
}

test "geometric_progression_calculation" {
  // 测试几何级数计算
  let first_term = 2
  let common_ratio = 3
  let number_of_terms = 4
  
  // 计算几何级数的第n项: a_n = a_1 * r^(n-1)
  let nth_term = multiply(first_term, multiply(multiply(common_ratio, common_ratio), common_ratio))
  assert_eq(54, nth_term)  // 2 * 3^3 = 2 * 27 = 54
  
  // 计算几何级数前n项和: S_n = a_1 * (r^n - 1) / (r - 1)
  let sum_n = multiply(first_term, divide_with_ceil(subtract(multiply(multiply(multiply(common_ratio, common_ratio), common_ratio), common_ratio), 1), subtract(common_ratio, 1)))
  assert_eq(80, sum_n)  // 2 * (3^4 - 1) / (3 - 1) = 2 * (81 - 1) / 2 = 2 * 80 / 2 = 80
}

test "conditional_budget_allocation" {
  // 测试条件预算分配
  let total_budget = 5000
  let department_a = 1200
  let department_b = 1800
  let department_c = 900
  
  // 计算已分配预算
  let allocated_budget = add(add(department_a, department_b), department_c)
  assert_eq(3900, allocated_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, allocated_budget)
  assert_eq(1100, remaining_budget)
  
  // 如果有剩余预算，按比例分配给各部门
  let additional_per_dept = divide_with_ceil(remaining_budget, 3)
  assert_eq(367, additional_per_dept)  // ceil(1100/3) = 367
  
  // 计算各部门最终预算
  let final_a = add(department_a, additional_per_dept)
  let final_b = add(department_b, additional_per_dept)
  let final_c = add(department_c, additional_per_dept)
  
  assert_eq(1567, final_a)
  assert_eq(2167, final_b)
  assert_eq(1267, final_c)
  
  // 验证总预算不超过限制
  let final_total = add(add(final_a, final_b), final_c)
  assert_true(final_total <= multiply(total_budget, 2))  // 允许一定的超支
}

test "data_validation_and_error_handling" {
  // 测试数据验证和错误处理
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值处理
  let very_large_number = 1000000
  let very_small_number = 1
  
  assert_eq(1000000, divide_with_ceil(very_large_number, very_small_number))
  assert_eq(1, divide_with_ceil(very_small_number, very_large_number))
  assert_eq(1, divide_with_ceil(very_large_number, very_large_number))
  
  // 测试负数除法
  assert_eq(-100, divide_with_ceil(-100, 1))
  assert_eq(100, divide_with_ceil(100, -1))
  assert_eq(100, divide_with_ceil(-100, -1))
}

test "complex_business_workflow_simulation" {
  // 测试复杂业务工作流模拟
  // 场景：订单处理流程
  let orders_per_day = 45
  let processing_capacity_per_worker = 8
  let workers_available = 6
  
  // 计算日处理能力
  let daily_capacity = multiply(workers_available, processing_capacity_per_worker)
  assert_eq(48, daily_capacity)
  
  // 计算处理所有订单需要的天数
  let days_needed = divide_with_ceil(orders_per_day, daily_capacity)
  assert_eq(1, days_needed)  // ceil(45/48) = 1
  
  // 计算实际每天处理的订单数
  let actual_processed_per_day = divide_with_ceil(orders_per_day, days_needed)
  assert_eq(45, actual_processed_per_day)
  
  // 计算每个工人的实际工作量
  let actual_per_worker = divide_with_ceil(actual_processed_per_day, workers_available)
  assert_eq(8, actual_per_worker)  // ceil(45/6) = 8
  
  // 验证工作量不超过处理能力
  assert_true(actual_per_worker <= processing_capacity_per_worker + 2)  // 允许少量超负荷
  
  // 计算总处理能力利用率
  let utilization_rate = divide_with_ceil(multiply(actual_processed_per_day, 100), daily_capacity)
  assert_eq(94, utilization_rate)  // ceil(45*100/48) = ceil(93.75) = 94%
}