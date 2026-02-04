// Azimuth 高级增强测试用例
// 包含8个高质量的标准 MoonBit 测试用例，涵盖新的测试场景

test "statistical_analysis_calculations" {
  // 统计分析计算测试
  let data_set = [15, 23, 8, 42, 16, 33, 27, 19]
  let data_count = 8
  
  // 计算总和
  let total_sum = add(add(add(add(15, 23), add(8, 42)), add(add(16, 33), add(27, 19)))
  assert_eq(183, total_sum)
  
  // 计算平均值（使用整数除法向上取整）
  let average = divide_with_ceil(total_sum, data_count)
  assert_eq(23, average)  // ceil(183/8) = 23
  
  // 计算最大值和最小值的差
  let max_value = 42
  let min_value = 8
  let range = subtract(max_value, min_value)
  assert_eq(34, range)
}

test "geometric_area_calculations" {
  // 几何面积计算测试
  // 矩形面积计算
  let rectangle_length = 12
  let rectangle_width = 8
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积计算（底×高÷2，使用向上取整除法）
  let triangle_base = 15
  let triangle_height = 10
  let triangle_area_double = multiply(triangle_base, triangle_height)
  let triangle_area = divide_with_ceil(triangle_area_double, 2)
  assert_eq(75, triangle_area)  // ceil(150/2) = 75
  
  // 梯形面积计算（（上底+下底）×高÷2）
  let trapezoid_top = 8
  let trapezoid_bottom = 12
  let trapezoid_height = 6
  let trapezoid_area_double = multiply(add(trapezoid_top, trapezoid_bottom), trapezoid_height)
  let trapezoid_area = divide_with_ceil(trapezoid_area_double, 2)
  assert_eq(60, trapezoid_area)  // ceil((8+12)*6/2) = ceil(120/2) = 60
}

test "time_based_project_planning" {
  // 基于时间的项目规划测试
  let project_tasks = 45
  let team_members = 6
  let work_hours_per_day = 8
  let work_days_per_week = 5
  let tasks_per_person_per_day = 2
  
  // 计算每日团队处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  assert_eq(12, daily_capacity)
  
  // 计算每周团队处理能力
  let weekly_capacity = multiply(daily_capacity, work_days_per_week)
  assert_eq(60, weekly_capacity)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(project_tasks, weekly_capacity)
  assert_eq(1, weeks_needed)  // ceil(45/60) = 1
  
  // 计算实际需要的工作日
  let work_days_needed = divide_with_ceil(project_tasks, daily_capacity)
  assert_eq(4, work_days_needed)  // ceil(45/12) = 4
  
  // 计算总工作小时数
  let total_work_hours = multiply(work_days_needed, work_hours_per_day)
  assert_eq(32, total_work_hours)  // 4 * 8 = 32
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 50000
  let departments = 4
  let min_budget_per_dept = 8000
  let remaining_budget = 0
  
  // 计算每个部门的最低预算总额
  let total_min_budget = multiply(min_budget_per_dept, departments)
  assert_eq(32000, total_min_budget)
  
  // 计算剩余可分配预算
  remaining_budget = subtract(total_budget, total_min_budget)
  assert_eq(18000, remaining_budget)
  
  // 平均分配剩余预算
  let additional_per_dept = divide_with_ceil(remaining_budget, departments)
  assert_eq(4500, additional_per_dept)  // ceil(18000/4) = 4500
  
  // 计算每个部门的最终预算
  let final_budget_per_dept = add(min_budget_per_dept, additional_per_dept)
  assert_eq(12500, final_budget_per_dept)
  
  // 验证总预算分配
  let total_allocated = multiply(final_budget_per_dept, departments)
  assert_eq(50000, total_allocated)
}

test "physics_simulation_calculations" {
  // 物理模拟计算测试
  // 自由落体运动：h = 0.5 * g * t^2，这里用整数模拟
  let gravity = 10  // 简化的重力加速度
  let time_seconds = 5
  let height_double = multiply(divide_with_ceil(multiply(gravity, multiply(time_seconds, time_seconds)), 2), 1)
  assert_eq(125, height_double)  // ceil(10*25/2) = ceil(250/2) = 125
  
  // 速度计算：v = g * t
  let velocity = multiply(gravity, time_seconds)
  assert_eq(50, velocity)  // 10 * 5 = 50
  
  // 动能计算：KE = 0.5 * m * v^2
  let mass = 10
  let kinetic_energy_double = multiply(divide_with_ceil(multiply(mass, multiply(velocity, velocity)), 2), 1)
  assert_eq(12500, kinetic_energy_double)  // ceil(10*2500/2) = ceil(25000/2) = 12500
}

test "compound_interest_financial" {
  // 复利金融计算测试
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 第一年：P * (1 + r)
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  assert_eq(10500, year1_amount)  // 10000 + ceil(10000*5/100) = 10000 + 500 = 10500
  
  // 第二年：year1 * (1 + r)
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate), 100))
  assert_eq(11025, year2_amount)  // 10500 + ceil(10500*5/100) = 10500 + 525 = 11025
  
  // 第三年：year2 * (1 + r)
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate), 100))
  assert_eq(11576, year3_amount)  // 11025 + ceil(11025*5/100) = 11025 + 551 = 11576
  
  // 计算总利息
  let total_interest = subtract(year3_amount, principal)
  assert_eq(1576, total_interest)  // 11576 - 10000 = 1576
}

test "data_structures_stack_simulation" {
  // 数据结构栈模拟测试
  // 使用简单的数值模拟栈操作
  let stack_size = 0
  let max_stack_size = 10
  let operations = [3, 7, 2, 9, 1]  // 要推入栈的值
  
  // 模拟推入操作
  stack_size = add(stack_size, 1)  // push 3
  stack_size = add(stack_size, 1)  // push 7
  stack_size = add(stack_size, 1)  // push 2
  assert_eq(3, stack_size)
  
  // 模拟弹出操作
  stack_size = subtract(stack_size, 1)  // pop 2
  stack_size = subtract(stack_size, 1)  // pop 7
  assert_eq(1, stack_size)
  
  // 继续推入剩余元素
  stack_size = add(stack_size, 1)  // push 9
  stack_size = add(stack_size, 1)  // push 1
  assert_eq(3, stack_size)
  
  // 检查栈是否满了
  let is_full = stack_size >= max_stack_size
  assert_false(is_full)
  
  // 计算栈的利用率
  let utilization_percent = divide_with_ceil(multiply(stack_size, 100), max_stack_size)
  assert_eq(30, utilization_percent)  // ceil(3*100/10) = 30%
}

test "pattern_sequence_generation" {
  // 模式序列生成测试
  // 生成斐波那契数列的前几项（简化版本）
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)   // 1 + 1 = 2
  assert_eq(3, fib4)   // 1 + 2 = 3
  assert_eq(5, fib5)   // 2 + 3 = 5
  
  // 生成等差数列
  let arithmetic_start = 3
  let arithmetic_diff = 4
  let term1 = arithmetic_start
  let term2 = add(term1, arithmetic_diff)
  let term3 = add(term2, arithmetic_diff)
  let term4 = add(term3, arithmetic_diff)
  
  assert_eq(3, term1)   // 第一项
  assert_eq(7, term2)   // 3 + 4 = 7
  assert_eq(11, term3)  // 7 + 4 = 11
  assert_eq(15, term4)  // 11 + 4 = 15
  
  // 计算等差数列前4项的和
  let arithmetic_sum = add(add(add(term1, term2), term3), term4)
  assert_eq(36, arithmetic_sum)  // 3 + 7 + 11 + 15 = 36
  
  // 生成等比数列
  let geometric_start = 2
  let geometric_ratio = 3
  let geo_term1 = geometric_start
  let geo_term2 = multiply(geo_term1, geometric_ratio)
  let geo_term3 = multiply(geo_term2, geometric_ratio)
  let geo_term4 = multiply(geo_term3, geometric_ratio)
  
  assert_eq(2, geo_term1)   // 第一项
  assert_eq(6, geo_term2)   // 2 * 3 = 6
  assert_eq(18, geo_term3)  // 6 * 3 = 18
  assert_eq(54, geo_term4)  // 18 * 3 = 54
  
  // 计算等比数列前4项的和
  let geometric_sum = add(add(add(geo_term1, geo_term2), geo_term3), geo_term4)
  assert_eq(80, geometric_sum)  // 2 + 6 + 18 + 54 = 80
}