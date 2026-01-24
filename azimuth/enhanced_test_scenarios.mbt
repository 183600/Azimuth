// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "financial_loan_calculation" {
  // 金融贷款计算场景
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(23, months_needed)
  
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  let final_payment = loan_amount - multiply(months_needed - 1, monthly_payment)
  assert_eq(200, final_payment)
}

test "warehouse_packing_optimization" {
  // 仓库装箱优化问题
  let product_a = 127
  let box_capacity_a = 15
  let boxes_a = divide_with_ceil(product_a, box_capacity_a)
  
  let product_b = 89
  let box_capacity_b = 12
  let boxes_b = divide_with_ceil(product_b, box_capacity_b)
  
  let total_boxes = add(boxes_a, boxes_b)
  assert_eq(17, total_boxes)
  
  let total_capacity = add(
    multiply(boxes_a, box_capacity_a),
    multiply(boxes_b, box_capacity_b)
  )
  assert_eq(171, total_capacity)
}

test "event_seating_arrangement" {
  // 活动座位安排场景
  let participants = 245
  let chairs_per_table = 6
  
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(41, tables_needed)
  
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(246, total_seats)
}

test "development_team_sprint_planning" {
  // 开发团队冲刺规划
  let team_members = 7
  let story_points_per_person = 5
  let sprint_capacity = 20
  
  let total_story_points = multiply(team_members, story_points_per_person)
  assert_eq(35, total_story_points)
  
  let sprints_needed = divide_with_ceil(total_story_points, sprint_capacity)
  assert_eq(2, sprints_needed)
  
  let final_sprint_points = total_story_points - sprint_capacity
  assert_eq(15, final_sprint_points)
}

test "manufacturing_production_batch" {
  // 制造业生产批次计算
  let order_quantity = 1500
  let machine_capacity_per_hour = 85
  
  let production_hours = divide_with_ceil(order_quantity, machine_capacity_per_hour)
  assert_eq(18, production_hours)
  
  let total_capacity = multiply(production_hours, machine_capacity_per_hour)
  assert_eq(1530, total_capacity)
}

test "educational_class_scheduling" {
  // 教育课程排课场景
  let students = 156
  let max_students_per_class = 25
  
  let classes_needed = divide_with_ceil(students, max_students_per_class)
  assert_eq(7, classes_needed)
  
  let avg_students_per_class = divide_with_ceil(students, classes_needed)
  assert_eq(23, avg_students_per_class)
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算
  let current_stock = 450
  let daily_demand = 28
  let lead_time_days = 5
  let safety_stock = 50
  
  let lead_time_demand = multiply(daily_demand, lead_time_days)
  assert_eq(140, lead_time_demand)
  
  let reorder_point = add(lead_time_demand, safety_stock)
  assert_eq(190, reorder_point)
  
  let days_supported = divide_with_ceil(current_stock, daily_demand)
  assert_eq(17, days_supported)
  
  let should_reorder = current_stock <= reorder_point
  assert_false(should_reorder)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)
  
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
}