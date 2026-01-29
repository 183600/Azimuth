// 高质量标准 MoonBit 测试用例
// 包含10个全面的测试用例，覆盖各种场景

test "integer_overflow_boundary" {
  // 测试整数边界和溢出情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界的加法
  assert_eq(2147483646, add(max_int, -1))
  assert_eq(-2147483647, add(min_int, 1))
  
  // 测试边界乘法
  assert_eq(2147483647, multiply(max_int, 1))
  assert_eq(-2147483648, multiply(min_int, 1))
  
  // 测试边界除法
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
}

test "complex_financial_amortization" {
  // 复杂金融摊销计算测试
  let principal = 250000
  let annual_rate = 4
  let loan_term_years = 25
  
  // 计算总月数
  let total_months = multiply(loan_term_years, 12)
  assert_eq(300, total_months)
  
  // 计算总利息（简化计算）
  let total_interest = divide_with_ceil(
    multiply(principal, multiply(annual_rate, loan_term_years)),
    100
  )
  assert_eq(250000, total_interest)
  
  // 计算月供
  let total_payment = add(principal, total_interest)
  let monthly_payment = divide_with_ceil(total_payment, total_months)
  assert_eq(1667, monthly_payment)
  
  // 验证总支付金额
  let actual_total = multiply(monthly_payment, total_months)
  assert_eq(500100, actual_total)
}

test "data_structure_stack_operations" {
  // 数据结构栈操作模拟测试
  let initial_stack_size = 0
  let push_operations = 15
  let pop_operations = 8
  
  // 执行推入操作
  let stack_after_push = add(initial_stack_size, push_operations)
  assert_eq(15, stack_after_push)
  
  // 执行弹出操作
  let final_stack_size = subtract(stack_after_push, pop_operations)
  assert_eq(7, final_stack_size)
  
  // 测试栈容量计算
  let max_capacity = 20
  let remaining_capacity = subtract(max_capacity, final_stack_size)
  assert_eq(13, remaining_capacity)
  
  // 测试是否需要扩容
  let needs_expansion = final_stack_size > max_capacity
  assert_false(needs_expansion)
}

test "manufacturing_production_optimization" {
  // 制造业生产优化测试
  let daily_target = 1000
  let machine_a_capacity = 350
  let machine_b_capacity = 280
  let machine_c_capacity = 400
  
  // 计算每台机器需要的运行轮次
  let rounds_a = divide_with_ceil(daily_target, machine_a_capacity)
  let rounds_b = divide_with_ceil(daily_target, machine_b_capacity)
  let rounds_c = divide_with_ceil(daily_target, machine_c_capacity)
  
  assert_eq(3, rounds_a)  // ceil(1000/350)
  assert_eq(4, rounds_b)  // ceil(1000/280)
  assert_eq(3, rounds_c)  // ceil(1000/400)
  
  // 计算最优选择（最少轮次）
  let min_rounds = rounds_a
  assert_eq(3, min_rounds)
  
  // 计算实际产量
  let actual_production = multiply(min_rounds, machine_a_capacity)
  assert_eq(1050, actual_production)
  
  // 计算超额产量
  let excess_production = subtract(actual_production, daily_target)
  assert_eq(50, excess_production)
}

test "network_bandwidth_allocation" {
  // 网络带宽分配测试
  let total_bandwidth = 1000  // Mbps
  let service_a_priority = 3
  let service_b_priority = 2
  let service_c_priority = 1
  let total_priority = add(add(service_a_priority, service_b_priority), service_c_priority)
  
  // 计算基础单位带宽
  let base_unit = divide_with_ceil(total_bandwidth, total_priority)
  assert_eq(334, base_unit)  // ceil(1000/6)
  
  // 分配带宽
  let bandwidth_a = multiply(service_a_priority, base_unit)
  let bandwidth_b = multiply(service_b_priority, base_unit)
  let bandwidth_c = multiply(service_c_priority, base_unit)
  
  assert_eq(1002, bandwidth_a)  // 3 * 334
  assert_eq(668, bandwidth_b)   // 2 * 334
  assert_eq(334, bandwidth_c)   // 1 * 334
  
  // 验证总分配带宽
  let total_allocated = add(add(bandwidth_a, bandwidth_b), bandwidth_c)
  assert_eq(2004, total_allocated)
}

test "temperature_conversion_system" {
  // 温度转换系统测试
  let celsius_temps = add(add(add(0, 25), 100), -40)
  let fahrenheit_temps = add(add(add(32, 77), 212), -40)
  
  // 验证特殊温度点
  assert_eq(85, celsius_temps)    // 0 + 25 + 100 - 40
  assert_eq(281, fahrenheit_temps) // 32 + 77 + 212 - 40
  
  // 模拟温度转换（简化公式）
  let converted_from_c = add(multiply(divide_with_ceil(multiply(celsius_temps, 9), 5), 1), 32)
  let converted_from_f = multiply(divide_with_ceil(multiply(subtract(fahrenheit_temps, 32), 5), 9), 1)
  
  assert_eq(186, converted_from_c)  // ceil(85*9/5) + 32
  assert_eq(139, converted_from_f)  // ceil(249*5/9)
}

test "educational_grading_system" {
  // 教育评分系统测试
  let student_count = 45
  let passing_score = 60
  let excellent_score = 90
  
  // 模拟学生成绩分布
  let excellent_students = 12
  let good_students = 18
  let passing_students = 10
  let failing_students = subtract(student_count, add(add(add(excellent_students, good_students), passing_students), 0))
  
  assert_eq(5, failing_students)
  
  // 计算各种比率
  let excellent_rate = divide_with_ceil(multiply(excellent_students, 100), student_count)
  let passing_rate = divide_with_ceil(multiply(add(add(excellent_students, good_students), passing_students), 100), student_count)
  
  assert_eq(27, excellent_rate)  // ceil(12*100/45)
  assert_eq(89, passing_rate)    // ceil(40*100/45)
  
  // 计算班级平均分（简化计算）
  let total_score = add(
    add(
      multiply(excellent_score, excellent_students),
      multiply(75, good_students)
    ),
    add(
      multiply(passing_score, passing_students),
      multiply(45, failing_students)
    )
  )
  let average_score = divide_with_ceil(total_score, student_count)
  assert_eq(75, average_score)  // ceil(3375/45)
}

test "logistics_warehouse_optimization" {
  // 物流仓库优化测试
  let total_orders = 267
  let picker_capacity = 45
  let packer_capacity = 30
  let shipper_capacity = 60
  
  // 计算每种角色需要的员工数
  let pickers_needed = divide_with_ceil(total_orders, picker_capacity)
  let packers_needed = divide_with_ceil(total_orders, packer_capacity)
  let shippers_needed = divide_with_ceil(total_orders, shipper_capacity)
  
  assert_eq(6, pickers_needed)   // ceil(267/45)
  assert_eq(9, packers_needed)   // ceil(267/30)
  assert_eq(5, shippers_needed)  // ceil(267/60)
  
  // 计算总员工数
  let total_staff = add(add(pickers_needed, packers_needed), shippers_needed)
  assert_eq(20, total_staff)
  
  // 计算实际处理能力
  let picking_capacity = multiply(pickers_needed, picker_capacity)
  let packing_capacity = multiply(packers_needed, packer_capacity)
  let shipping_capacity = multiply(shippers_needed, shipper_capacity)
  
  assert_eq(270, picking_capacity)
  assert_eq(270, packing_capacity)
  assert_eq(300, shipping_capacity)
  
  // 确定瓶颈环节
  let bottleneck_capacity = picking_capacity
  assert_eq(270, bottleneck_capacity)
}

test "agricultural_crop_planning" {
  // 农业作物规划测试
  let total_land = 500  // 公顷
  let wheat_yield = 3   // 吨/公顷
  let corn_yield = 4    // 吨/公顷
  let rice_yield = 5    // 吨/公顷
  
  // 作物分配
  let wheat_land = 200
  let corn_land = 150
  let rice_land = subtract(total_land, add(wheat_land, corn_land))
  
  assert_eq(150, rice_land)
  
  // 计算预期产量
  let wheat_production = multiply(wheat_land, wheat_yield)
  let corn_production = multiply(corn_land, corn_yield)
  let rice_production = multiply(rice_land, rice_yield)
  
  assert_eq(600, wheat_production)
  assert_eq(600, corn_production)
  assert_eq(750, rice_production)
  
  // 计算总产量
  let total_production = add(add(wheat_production, corn_production), rice_production)
  assert_eq(1950, total_production)
  
  // 计算存储需求（每吨需要1.5立方米空间）
  let storage_per_ton = 1.5
  let total_storage = divide_with_ceil(multiply(total_production, 15), 10)
  assert_eq(293, total_storage)  // ceil(1950*1.5)
}

test "software_development_metrics" {
  // 软件开发指标测试
  let total_features = 48
  let developer_capacity = 3
  let sprint_duration = 2  // 周
  
  // 计算需要的冲刺次数
  let sprints_needed = divide_with_ceil(total_features, multiply(developer_capacity, sprint_duration))
  assert_eq(8, sprints_needed)  // ceil(48/(3*2))
  
  // 计算总开发时间
  let total_weeks = multiply(sprints_needed, sprint_duration)
  assert_eq(16, total_weeks)
  
  // 计算每月完成的特性数（假设每月4周）
  let features_per_month = divide_with_ceil(multiply(total_features, 4), total_weeks)
  assert_eq(12, features_per_month)  // ceil(48*4/16)
  
  // 计算开发效率
  let efficiency = divide_with_ceil(multiply(total_features, 100), multiply(total_weeks, developer_capacity))
  assert_eq(100, efficiency)  // ceil(48*100/(16*3))
  
  // 测试字符串处理功能
  assert_eq_string("Hello, Development Team!", greet("Development Team"))
  assert_eq_string("Hello, Sprint 8!", greet("Sprint 8"))
}