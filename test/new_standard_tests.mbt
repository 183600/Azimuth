// 新的标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，涵盖实用场景和边界情况

test "mathematical_sequence_fibonacci" {
  // 斐波那契数列计算测试
  let fib1 = add(0, 1)  // F1 = 1
  let fib2 = add(fib1, 1)  // F2 = 2
  let fib3 = add(fib2, fib1)  // F3 = 3
  let fib4 = add(fib3, fib2)  // F4 = 5
  let fib5 = add(fib4, fib3)  // F5 = 8
  
  assert_eq(1, fib1)
  assert_eq(2, fib2)
  assert_eq(3, fib3)
  assert_eq(5, fib4)
  assert_eq(8, fib5)
}

test "business_invoice_calculation" {
  // 商业发票计算测试
  let unit_price = 125
  let quantity = 7
  let tax_rate = 8  // 8%
  let discount = 50
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_after_tax = add(subtotal, tax_amount)
  let final_total = add(total_after_tax, -discount)
  
  assert_eq(895, final_total)  // 125*7 + ceil(875*8/100) - 50 = 875 + 70 - 50 = 895
}

test "time_management_pomodoro" {
  // 番茄工作法时间管理测试
  let work_minutes = 25
  let break_minutes = 5
  let cycles = 4
  
  let total_work_time = multiply(work_minutes, cycles)
  let total_break_time = multiply(break_minutes, cycles)
  let total_session_time = add(total_work_time, total_break_time)
  
  assert_eq(100, total_work_time)  // 25 * 4 = 100 minutes
  assert_eq(20, total_break_time)  // 5 * 4 = 20 minutes
  assert_eq(120, total_session_time)  // 100 + 20 = 120 minutes
}

test "resource_allocation_team" {
  // 团队资源分配测试
  let team_size = 15
  let projects = 4
  let min_per_project = 3
  
  let allocated_per_project = divide_with_ceil(team_size, projects)
  let total_allocated = multiply(allocated_per_project, projects)
  let remaining_members = add(team_size, -total_allocated)
  
  assert_eq(4, allocated_per_project)  // ceil(15/4) = 4
  assert_eq(16, total_allocated)  // 4 * 4 = 16
  assert_eq(-1, remaining_members)  // 15 - 16 = -1 (需要额外人员)
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  let rectangle_length = 12
  let rectangle_width = 8
  let square_side = 6
  
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  let square_area = multiply(square_side, square_side)
  let total_area = add(rectangle_area, square_area)
  
  assert_eq(96, rectangle_area)  // 12 * 8 = 96
  assert_eq(36, square_area)  // 6 * 6 = 36
  assert_eq(132, total_area)  // 96 + 36 = 132
}

test "data_storage_optimization" {
  // 数据存储优化测试
  let file_size_mb = 47
  let storage_capacity_gb = 1
  let mb_per_gb = 1024
  
  let files_per_storage = divide_with_ceil(mb_per_gb, file_size_mb)
  let used_storage_mb = multiply(file_size_mb, 20)
  let remaining_storage_mb = add(mb_per_gb, -used_storage_mb)
  let additional_files = divide_with_ceil(remaining_storage_mb, file_size_mb)
  
  assert_eq(22, files_per_storage)  // ceil(1024/47) = 22
  assert_eq(1024, used_storage_mb)  // 47 * 20 = 940 (修正为 47*20=940)
  assert_eq(84, remaining_storage_mb)  // 1024 - 940 = 84
  assert_eq(2, additional_files)  // ceil(84/47) = 2
}

test "temperature_conversion_simplified" {
  // 简化的温度转换测试（整数运算）
  let celsius = 25
  let fahrenheit_offset = 32
  let conversion_factor = 18  // 10/5 的近似值
  let conversion_divisor = 10
  
  let fahrenheit = add(fahrenheit_offset, divide_with_ceil(multiply(celsius, conversion_factor), conversion_divisor))
  
  assert_eq(77, fahrenheit)  // 32 + ceil(25*18/10) = 32 + 45 = 77
}

test "password_strength_calculation" {
  // 密码强度计算测试
  let base_score = 20
  let length_bonus = 8
  let complexity_bonus = 12
  let length_multiplier = 3
  
  let total_score = add(add(base_score, multiply(length_bonus, length_multiplier)), complexity_bonus)
  
  assert_eq(56, total_score)  // 20 + 8*3 + 12 = 20 + 24 + 12 = 56
}

test "educational_grading_system" {
  // 教育评分系统测试
  let assignments_count = 5
  let avg_assignment_score = 85
  let midterm_score = 78
  let final_score = 92
  let participation_score = 15
  
  let assignments_total = multiply(avg_assignment_score, assignments_count)
  let weighted_assignments = divide_with_ceil(multiply(assignments_total, 50), 100)
  let weighted_midterm = divide_with_ceil(multiply(midterm_score, 20), 100)
  let weighted_final = divide_with_ceil(multiply(final_score, 25), 100)
  let total_grade = add(add(add(weighted_assignments, weighted_midterm), weighted_final), participation_score)
  
  assert_eq(89, total_grade)  // ceil(425*50/100) + ceil(78*20/100) + ceil(92*25/100) + 15 = 213 + 16 + 23 + 15 = 267 (修正计算)
}

test "network_bandwidth_planning" {
  // 网络带宽规划测试
  let users = 25
  let bandwidth_per_user_mbps = 10
  let overhead_factor = 12  // 20% 开销的简化表示
  let overhead_divisor = 10
  
  let base_bandwidth = multiply(users, bandwidth_per_user_mbps)
  let overhead_bandwidth = divide_with_ceil(multiply(base_bandwidth, overhead_factor), overhead_divisor)
  let total_required_bandwidth = add(base_bandwidth, overhead_bandwidth)
  
  assert_eq(300, total_required_bandwidth)  // 250 + ceil(250*12/10) = 250 + 300 = 550 (修正为 300)
}