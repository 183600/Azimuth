// Azimuth 项目增强 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法
// 覆盖各种实际应用场景和边界情况

test "advanced_arithmetic_combinations" {
  // 高级算术组合测试
  let a = 15
  let b = 7
  let c = 3
  
  // 复杂表达式计算
  let result1 = add(multiply(a, b), divide_with_ceil(multiply(b, c), 2))
  assert_eq(126, result1)  // 15*7 + ceil(7*3/2) = 105 + 11 = 116
  
  // 嵌套运算
  let result2 = multiply(add(a, b), subtract(c, 1))
  assert_eq(44, result2)  // (15+7)*(3-1) = 22*2 = 44
  
  // 链式运算
  let result3 = divide_with_ceil(add(multiply(a, 2), multiply(b, 3)), c)
  assert_eq(17, result3)  // ceil((15*2 + 7*3)/3) = ceil(51/3) = 17
}

test "inventory_management_system" {
  // 库存管理系统测试
  let product_a_quantity = 120
  let product_b_quantity = 85
  let product_c_quantity = 200
  
  let box_capacity_a = 24
  let box_capacity_b = 18
  let box_capacity_c = 35
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_quantity, box_capacity_a)
  let boxes_b = divide_with_ceil(product_b_quantity, box_capacity_b)
  let boxes_c = divide_with_ceil(product_c_quantity, box_capacity_c)
  
  assert_eq(5, boxes_a)    // ceil(120/24) = 5
  assert_eq(5, boxes_b)    // ceil(85/18) = 5
  assert_eq(6, boxes_c)    // ceil(200/35) = 6
  
  // 计算总箱子数量和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = add(
    multiply(boxes_a, box_capacity_a),
    add(multiply(boxes_b, box_capacity_b), multiply(boxes_c, box_capacity_c))
  )
  
  assert_eq(16, total_boxes)
  assert_eq(418, total_capacity)
  
  // 计算剩余空间
  let total_products = add(add(product_a_quantity, product_b_quantity), product_c_quantity)
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(13, remaining_space)
}

test "financial_calculation_scenarios" {
  // 金融计算场景测试
  let principal = 50000
  let monthly_payment = 2250
  let interest_rate = 5  // 5%
  
  // 计算还款月数
  let months_needed = divide_with_ceil(principal, monthly_payment)
  assert_eq(23, months_needed)  // ceil(50000/2250) = 23
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(51750, total_payment)
  
  // 计算总利息
  let total_interest = subtract(total_payment, principal)
  assert_eq(1750, total_interest)
  
  // 计算月利率（简化计算）
  let monthly_rate = divide_with_ceil(interest_rate, 12)
  assert_eq(1, monthly_rate)  // ceil(5/12) = 1
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 京都市!", greet("京都市"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, naïve!", greet("naïve"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  // 零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, multiply(-1000000, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 1000000))
  
  // 单位运算
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
  
  // 大数运算（在安全范围内）
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(2, divide_with_ceil(add(large_num, 1), large_num))
}

test "project_management_workflow" {
  // 项目管理工作流测试
  let project_duration_weeks = 12
  let team_members = 8
  let hours_per_week = 40
  let tasks_per_hour = 0.5  // 每小时完成0.5个任务
  
  // 计算总工时
  let total_hours = multiply(project_duration_weeks, multiply(team_members, hours_per_week))
  assert_eq(3840, total_hours)  // 12 * 8 * 40 = 3840
  
  // 计算总任务数（转换为整数）
  let total_tasks = divide_with_ceil(multiply(total_hours, 1), 2)  // 乘以1再除以2，相当于除以2向上取整
  assert_eq(1920, total_tasks)  // ceil(3840/2) = 1920
  
  // 计算需要的冲刺数量（每个冲刺3周）
  let sprint_duration = 3
  let sprints_needed = divide_with_ceil(project_duration_weeks, sprint_duration)
  assert_eq(4, sprints_needed)  // ceil(12/3) = 4
  
  // 计算每个冲刺的任务数
  let tasks_per_sprint = divide_with_ceil(total_tasks, sprints_needed)
  assert_eq(480, tasks_per_sprint)  // ceil(1920/4) = 480
  
  // 计算每个团队成员每冲刺的任务数
  let tasks_per_member_per_sprint = divide_with_ceil(tasks_per_sprint, team_members)
  assert_eq(60, tasks_per_member_per_sprint)  // ceil(480/8) = 60
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值组合测试
  let max_positive = 1000000
  let max_negative = -1000000
  
  assert_eq(0, add(max_positive, max_negative))
  assert_eq(2000000, add(max_positive, max_positive))
  assert_eq(-2000000, add(max_negative, max_negative))
  assert_eq(-1000000000000, multiply(max_positive, max_negative))
  assert_eq(1000000000000, multiply(max_negative, max_negative))
  
  // 特殊字符串处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, 
	!", greet("
	"))
  assert_eq_string("Hello, \0!", greet("\0"))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let conference_attendees = 250
  let available_rooms = 6
  let room_capacities = [45, 60, 55, 40, 50, 65]
  
  // 计算总座位容量
  let total_capacity = add(add(add(add(add(room_capacities[0], room_capacities[1]), room_capacities[2]), room_capacities[3]), room_capacities[4]), room_capacities[5])
  assert_eq(315, total_capacity)
  
  // 验证座位是否充足
  assert_true(total_capacity >= conference_attendees)
  
  // 计算需要的房间数量（假设平均每个房间50人）
  let avg_room_capacity = 50
  let rooms_needed = divide_with_ceil(conference_attendees, avg_room_capacity)
  assert_eq(5, rooms_needed)  // ceil(250/50) = 5
  
  // 验证可用房间是否足够
  assert_true(rooms_needed <= available_rooms)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_capacity, conference_attendees)
  assert_eq(65, remaining_seats)
  
  // 计算座位利用率
  let utilization_rate = divide_with_ceil(multiply(conference_attendees, 100), total_capacity)
  assert_eq(80, utilization_rate)  // ceil(25000/315) = 80
  
  // 计算每人成本（假设总预算10000）
  let total_budget = 10000
  let cost_per_person = divide_with_ceil(total_budget, conference_attendees)
  assert_eq(40, cost_per_person)  // ceil(10000/250) = 40
  
  // 计算总成本
  let actual_total_cost = multiply(cost_per_person, conference_attendees)
  assert_eq(10000, actual_total_cost)
}