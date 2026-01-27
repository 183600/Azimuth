// Azimuth 综合真实世界测试用例
// 包含10个高质量的 MoonBit 测试用例，专注于实际应用场景

test "inventory_management_system" {
  // 库存管理系统测试
  // 场景：计算仓库存储需求
  let product_a = 120  // 产品A数量
  let product_b = 85   // 产品B数量
  let product_c = 200  // 产品C数量
  let box_capacity = 24  // 每箱容量
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  // 验证箱子数量计算
  assert_eq(5, boxes_a)   // ceil(120/24) = 5
  assert_eq(4, boxes_b)   // ceil(85/24) = 4
  assert_eq(9, boxes_c)   // ceil(200/24) = 9
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(18, total_boxes)
  assert_eq(432, total_capacity)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  // 场景：计算项目完成所需的人力和时间
  let total_tasks = 150
  let team_members = 8
  let working_days = 15
  let tasks_per_person_per_day = 2
  
  // 计算团队每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的天数
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(10, days_needed)  // ceil(150/16) = 10
  
  // 验证是否在规定工作日内完成
  assert_true(days_needed <= working_days)
  
  // 计算实际处理能力
  let actual_capacity = multiply(days_needed, daily_capacity)
  assert_eq(160, actual_capacity)
}

test "financial_budget_calculation" {
  // 财务预算计算测试
  // 场景：部门预算分配和支出跟踪
  let total_budget = 50000
  let departments = 4
  let emergency_fund = 5000
  
  // 计算每个部门的基础预算
  let available_budget = subtract(total_budget, emergency_fund)
  let budget_per_dept = divide_with_ceil(available_budget, departments)
  
  assert_eq(11250, budget_per_dept)  // ceil(45000/4) = 11250
  
  // 计算总分配预算
  let total_allocated = multiply(budget_per_dept, departments)
  assert_eq(45000, total_allocated)
  
  // 验证紧急基金未被使用
  let remaining_budget = subtract(total_budget, total_allocated)
  assert_eq(5000, remaining_budget)
}

test "event_planning_seating_arrangement" {
  // 活动规划座位安排测试
  // 场景：会议或活动的座位安排
  let attendees = 267
  let tables_per_room = 12
  let chairs_per_table = 8
  let rooms_available = 3
  
  // 计算每个房间可容纳人数
  let capacity_per_room = multiply(tables_per_room, chairs_per_table)
  
  // 计算需要的房间数
  let rooms_needed = divide_with_ceil(attendees, capacity_per_room)
  assert_eq(3, rooms_needed)  // ceil(267/96) = 3
  
  // 验证房间是否足够
  assert_true(rooms_needed <= rooms_available)
  
  // 计算总可用座位数
  let total_seats = multiply(rooms_needed, capacity_per_room)
  assert_eq(288, total_seats)
  
  // 计算剩余座位数
  let empty_seats = subtract(total_seats, attendees)
  assert_eq(21, empty_seats)
}

test "manufacturing_production_batch" {
  // 制造业生产批次测试
  // 场景：计算生产批次和原材料需求
  let order_quantity = 1250
  let batch_size = 100
  let defect_rate = 5  // 5%的次品率
  let safety_margin = 10  // 10%的安全边际
  
  // 计算基础生产批次数
  let base_batches = divide_with_ceil(order_quantity, batch_size)
  assert_eq(13, base_batches)  // ceil(1250/100) = 13
  
  // 计算考虑次品率后的额外生产量
  let defect_adjustment = divide_with_ceil(multiply(order_quantity, defect_rate), 100)
  assert_eq(63, defect_adjustment)  // ceil(1250*5/100) = 63
  
  // 计算考虑安全边际后的总生产量
  let safety_adjustment = divide_with_ceil(multiply(add(order_quantity, defect_adjustment), safety_margin), 100)
  let total_production = add(add(order_quantity, defect_adjustment), safety_adjustment)
  
  assert_eq(1440, total_production)  // 1250 + 63 + 127
  
  // 计算最终需要的批次数
  let final_batches = divide_with_ceil(total_production, batch_size)
  assert_eq(15, final_batches)  // ceil(1440/100) = 15
}

test "transportation_logistics_optimization" {
  // 运输物流优化测试
  // 场景：货物配送路线和车辆调度
  let total_packages = 487
  let truck_capacity = 24
  let delivery_routes = 5
  let average_delivery_time_per_route = 4  // 小时
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  assert_eq(21, trucks_needed)  // ceil(487/24) = 21
  
  // 计算每条路线分配的卡车数
  let trucks_per_route = divide_with_ceil(trucks_needed, delivery_routes)
  assert_eq(5, trucks_per_route)  // ceil(21/5) = 5
  
  // 计算实际使用的卡车总数
  let actual_trucks = multiply(trucks_per_route, delivery_routes)
  assert_eq(25, actual_trucks)
  
  // 计算总运输能力
  let total_capacity = multiply(actual_trucks, truck_capacity)
  assert_eq(600, total_capacity)
  
  // 计算剩余运力
  let unused_capacity = subtract(total_capacity, total_packages)
  assert_eq(113, unused_capacity)
}

test "software_development_sprint_planning" {
  // 软件开发冲刺规划测试
  // 场景：敏捷开发冲刺计划和任务分配
  let total_story_points = 85
  let team_velocity = 20
  let sprint_duration = 2  // 周
  let team_members = 6
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_story_points, team_velocity)
  assert_eq(5, sprints_needed)  // ceil(85/20) = 5
  
  // 计算项目总持续时间
  let total_duration = multiply(sprints_needed, sprint_duration)
  assert_eq(10, total_duration)  // 5冲刺 * 2周 = 10周
  
  // 计算每人每周需要完成的任务点数
  let weekly_capacity = multiply(team_members, sprint_duration)
  let points_per_person_per_week = divide_with_ceil(team_velocity, weekly_capacity)
  assert_eq(2, points_per_person_per_week)  // ceil(20/12) = 2
  
  // 验证团队总处理能力
  let total_capacity = multiply(sprints_needed, team_velocity)
  assert_eq(100, total_capacity)
}

test "healthcare_staff_scheduling" {
  // 医疗保健人员排班测试
  // 场景：医院护士排班和人员配置
  let patients_per_day = 120
  let nurses_per_shift = 8
  let shifts_per_day = 3
  let max_patients_per_nurse = 6
  
  // 计算每班需要护士数
  let nurses_needed_per_shift = divide_with_ceil(patients_per_day, multiply(shifts_per_day, max_patients_per_nurse))
  assert_eq(7, nurses_needed_per_shift)  // ceil(120/(3*6)) = 7
  
  // 计算每日总护士需求
  let total_nurses_per_day = multiply(nurses_needed_per_shift, shifts_per_day)
  assert_eq(21, total_nurses_per_day)  // 7 * 3 = 21
  
  // 验证现有护士是否满足需求
  let available_nurses = multiply(nurses_per_shift, shifts_per_day)
  assert_true(available_nurses >= total_nurses_per_day)
  
  // 计算护士处理能力利用率
  let total_capacity = multiply(available_nurses, max_patients_per_nurse)
  let utilization_rate = divide_with_ceil(multiply(patients_per_day, 100), total_capacity)
  assert_eq(83, utilization_rate)  // ceil(120*100/144) = 83%
}

test "educational_classroom_allocation" {
  // 教育课堂分配测试
  // 场景：学校教室和课程安排
  let total_students = 450
  let classrooms_available = 15
  let max_students_per_classroom = 30
  let subjects_per_student = 6
  let periods_per_day = 8
  
  // 计算需要的教室数
  let classrooms_needed = divide_with_ceil(total_students, max_students_per_classroom)
  assert_eq(15, classrooms_needed)  // ceil(450/30) = 15
  
  // 验证教室是否足够
  assert_true(classrooms_needed <= classrooms_available)
  
  // 计算每个教室的平均学生数
  let avg_students_per_classroom = divide_with_ceil(total_students, classrooms_available)
  assert_eq(30, avg_students_per_classroom)  // ceil(450/15) = 30
  
  // 计算每日总课时需求
  let total_periods_needed = multiply(total_students, subjects_per_student)
  assert_eq(2700, total_periods_needed)  // 450 * 6 = 2700
  
  // 计算每个教室每日可提供的课时数
  let periods_per_classroom_per_day = periods_per_day
  let total_periods_available = multiply(classrooms_available, periods_per_classroom_per_day)
  assert_eq(120, total_periods_available)  // 15 * 8 = 120
  
  // 计算需要的轮次
  let rounds_needed = divide_with_ceil(total_periods_needed, total_periods_available)
  assert_eq(23, rounds_needed)  // ceil(2700/120) = 23
}

test "retail_inventory_replenishment" {
  // 零售库存补充测试
  // 场景：零售店库存管理和补货策略
  let current_stock = 245
  let daily_sales = 35
  let lead_time_days = 7
  let safety_stock_days = 3
  let reorder_point = 100
  
  // 计算在途库存消耗
  let lead_time_consumption = multiply(daily_sales, lead_time_days)
  assert_eq(245, lead_time_consumption)  // 35 * 7 = 245
  
  // 计算安全库存
  let safety_stock = multiply(daily_sales, safety_stock_days)
  assert_eq(105, safety_stock)  // 35 * 3 = 105
  
  // 计算再订货点
  let calculated_reorder_point = add(lead_time_consumption, safety_stock)
  assert_eq(350, calculated_reorder_point)  // 245 + 105 = 350
  
  // 判断是否需要补货
  let need_reorder = current_stock <= reorder_point
  assert_true(need_reorder)  // 245 <= 100，需要补货
  
  // 计算建议的补货量
  let suggested_reorder_quantity = subtract(calculated_reorder_point, current_stock)
  assert_eq(105, suggested_reorder_quantity)  // 350 - 245 = 105
  
  // 计算补货后的库存水平
  let stock_after_reorder = add(current_stock, suggested_reorder_quantity)
  assert_eq(350, stock_after_reorder)
}