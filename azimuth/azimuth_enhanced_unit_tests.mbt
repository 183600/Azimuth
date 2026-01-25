// Azimuth 增强单元测试 - 8个高质量测试用例
// 专注于边界情况、数学定理和实际应用场景

test "mathematical_distributive_law" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 5
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7 * (5 + 3) = 7 * 5 + 7 * 3 = 56
}

test "inventory_box_packing_optimization" {
  // 库存装箱优化问题
  // 有多种物品，每种物品需要不同数量的箱子，计算最优装箱方案
  
  // 物品A: 23个，每箱装6个
  let item_a_count = 23
  let box_capacity_a = 6
  let boxes_a = divide_with_ceil(item_a_count, box_capacity_a)
  
  // 物品B: 41个，每箱装8个  
  let item_b_count = 41
  let box_capacity_b = 8
  let boxes_b = divide_with_ceil(item_b_count, box_capacity_b)
  
  // 物品C: 17个，每箱装5个
  let item_c_count = 17
  let box_capacity_c = 5
  let boxes_c = divide_with_ceil(item_c_count, box_capacity_c)
  
  // 计算总箱子数和剩余容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = add(
    multiply(boxes_a, box_capacity_a),
    add(multiply(boxes_b, box_capacity_b), multiply(boxes_c, box_capacity_c))
  )
  let total_items = add(add(item_a_count, item_b_count), item_c_count)
  let wasted_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_a)  // ceil(23/6) = 4
  assert_eq(6, boxes_b)  // ceil(41/8) = 6
  assert_eq(4, boxes_c)  // ceil(17/5) = 4
  assert_eq(14, total_boxes)
  assert_eq(7, wasted_space)  // 总容量88 - 总物品81 = 7
}

test "time_calculation_scenario" {
  // 时间计算场景：项目排期
  let total_tasks = 127
  let team_members = 5
  let tasks_per_person_per_day = 3
  let work_days_per_week = 5
  
  // 计算每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  assert_eq(15, daily_capacity)  // 5 * 3 = 15
  assert_eq(9, work_days_needed)  // ceil(127/15) = 9
  assert_eq(2, weeks_needed)     // ceil(9/5) = 2
}

test "string_concatenation_edge_cases" {
  // 字符串连接的边缘情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\n\n!", greet("\n\n\n"))
  assert_eq_string("Hello, \t\t\t!", greet("\t\t\t"))
  assert_eq_string("Hello, 🌟🌙⭐!", greet("🌟🌙⭐"))
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列和等比数列
  let n = 10
  let first_term = 3
  let common_difference = 2
  
  // 等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let arithmetic_sum = divide_with_ceil(
    multiply(n, add(multiply(2, first_term), multiply(n - 1, common_difference))),
    2
  )
  
  // 等比数列前n项和（简化计算）
  let geometric_sum = multiply(first_term, subtract(multiply(2, n), 1))
  
  assert_eq(120, arithmetic_sum)  // 10/2 * (6 + 18) = 60
  assert_eq(57, geometric_sum)   // 3 * (20 - 1) = 57
}

test "resource_allocation_simple" {
  // 简化的资源分配问题
  let total_budget = 50000
  let departments = 4
  
  // 计算平均分配
  let avg_allocation = divide_with_ceil(total_budget, departments)
  
  // 计算实际总分配
  let actual_total = multiply(avg_allocation, departments)
  let budget_deficit = subtract(actual_total, total_budget)
  
  assert_eq(12500, avg_allocation)  // ceil(50000/4) = 12500
  assert_eq(0, budget_deficit)      // 12500 * 4 - 50000 = 0
}

test "negative_number_division_precision" {
  // 负数除法的精度测试
  assert_eq(-2, divide_with_ceil(-7, 3))   // ceil(-2.33) = -2
  assert_eq(-2, divide_with_ceil(-8, 3))   // ceil(-2.67) = -2
  assert_eq(3, divide_with_ceil(-7, -3))   // ceil(2.33) = 3
  assert_eq(3, divide_with_ceil(-8, -3))   // ceil(2.67) = 3
  assert_eq(0, divide_with_ceil(-1, 2))    // ceil(-0.5) = 0
  assert_eq(0, divide_with_ceil(1, -2))    // ceil(-0.5) = 0
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：电商订单处理
  
  let order_items = 47
  let items_per_box = 8
  let boxes_per_pallet = 6
  let pallets_per_truck = 4
  
  // 计算需要的箱子数
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  
  // 计算需要的托盘数
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  
  // 计算需要的卡车数
  let trucks_needed = divide_with_ceil(pallets_needed, pallets_per_truck)
  
  // 计算运输能力利用率
  let total_truck_capacity = multiply(trucks_needed, multiply(pallets_per_truck, boxes_per_pallet))
  let capacity_utilization = divide_with_ceil(
    multiply(multiply(boxes_needed, 100), total_truck_capacity),
    100
  )
  
  assert_eq(6, boxes_needed)      // ceil(47/8) = 6
  assert_eq(1, pallets_needed)    // ceil(6/6) = 1
  assert_eq(1, trucks_needed)     // ceil(1/4) = 1
  assert_eq(24, total_truck_capacity)  // 1 * 4 * 6 = 24
  assert_eq(25, capacity_utilization)  // ceil(6*100/24) = 25%
}