// Azimuth 项目标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "mathematical_associative_laws" {
  // 测试数学结合律
  let a = 5
  let b = 3
  let c = 2
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 验证具体数值
  assert_eq(10, add(add(5, 3), 2))
  assert_eq(10, add(5, add(3, 2)))
  assert_eq(30, multiply(multiply(5, 3), 2))
  assert_eq(30, multiply(5, multiply(3, 2)))
}

test "complex_budget_allocation" {
  // 复杂预算分配场景测试
  let total_budget = 50000
  let departments = 4
  let min_budget_per_dept = divide_with_ceil(total_budget, departments)
  
  assert_eq(12500, min_budget_per_dept)
  
  // 验证分配后的总预算
  let allocated_total = multiply(min_budget_per_dept, departments)
  assert_eq(50000, allocated_total)
  
  // 计算预留缓冲资金
  let buffer = subtract(allocated_total, total_budget)
  assert_eq(0, buffer)
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  let total_minutes = 157
  let minutes_per_hour = 60
  let hours_needed = divide_with_ceil(total_minutes, minutes_per_hour)
  
  assert_eq(3, hours_needed)
  
  // 计算总分钟容量
  let total_capacity = multiply(hours_needed, minutes_per_hour)
  assert_eq(180, total_capacity)
  
  // 计算剩余分钟
  let remaining_minutes = subtract(total_capacity, total_minutes)
  assert_eq(23, remaining_minutes)
}

test "string_edge_case_handling" {
  // 字符串边缘情况处理测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC123!", greet("ABC123"))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "inventory_optimization_problem" {
  // 库存优化问题测试
  let product_a_qty = 45
  let product_b_qty = 67
  let product_c_qty = 23
  let box_capacity = 20
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  assert_eq(3, boxes_a)  // ceil(45/20) = 3
  assert_eq(4, boxes_b)  // ceil(67/20) = 4
  assert_eq(2, boxes_c)  // ceil(23/20) = 2
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(9, total_boxes)
  
  // 计算总存储容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(180, total_capacity)
}

test "performance_calculation_metrics" {
  // 性能计算指标测试
  let operations = 1000
  let time_per_operation = 3
  let operations_per_batch = 75
  
  // 计算总时间
  let total_time = multiply(operations, time_per_operation)
  assert_eq(3000, total_time)
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(operations, operations_per_batch)
  assert_eq(14, batches_needed)  // ceil(1000/75) = 14
  
  // 计算批次处理能力
  let batch_capacity = multiply(batches_needed, operations_per_batch)
  assert_eq(1050, batch_capacity)
  
  // 计算剩余处理能力
  let spare_capacity = subtract(batch_capacity, operations)
  assert_eq(50, spare_capacity)
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  assert_eq(42, left_side)  // 6 * (4 + 3) = 6 * 7 = 42
  assert_eq(42, right_side)  // 6 * 4 + 6 * 3 = 24 + 18 = 42
}

test "real_world_shipping_calculation" {
  // 真实世界运输计算测试
  let packages = 127
  let weight_per_package = 15
  let max_weight_per_truck = 200
  let trucks_available = 12
  
  // 计算总重量
  let total_weight = multiply(packages, weight_per_package)
  assert_eq(1905, total_weight)
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(total_weight, max_weight_per_truck)
  assert_eq(10, trucks_needed)  // ceil(1905/200) = 10
  
  // 验证卡车数量是否足够
  assert_true(trucks_needed <= trucks_available)
  
  // 计算实际运输能力
  let actual_capacity = multiply(trucks_needed, max_weight_per_truck)
  assert_eq(2000, actual_capacity)
  
  // 计算剩余运输能力
  let remaining_capacity = subtract(actual_capacity, total_weight)
  assert_eq(95, remaining_capacity)
}