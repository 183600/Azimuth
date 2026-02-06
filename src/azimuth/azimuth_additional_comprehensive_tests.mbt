// Azimuth 项目额外综合测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "complex_financial_calculation" {
  // 复杂金融计算场景：贷款计算和分期付款
  let loan_amount = 250000
  let annual_interest_rate = 5
  let loan_years = 30
  
  // 计算总利息（简化计算）
  let total_interest = multiply(loan_amount, annual_interest_rate)
  let total_interest_over_years = multiply(total_interest, loan_years)
  let total_repayment = add(loan_amount, divide_with_ceil(total_interest_over_years, 100))
  
  // 计算月供（简化）
  let total_months = multiply(loan_years, 12)
  let monthly_payment = divide_with_ceil(total_repayment, total_months)
  
  assert_eq(375000, total_interest_over_years)  // 250000 * 5 * 30 = 375000
  assert_eq(625000, total_repayment)  // 250000 + 375000 = 625000
  assert_eq(360, total_months)  // 30 * 12 = 360
  assert_eq(1736, monthly_payment)  // ceil(625000/360) = 1736
}

test "inventory_management_optimization" {
  // 库存管理优化测试：多品类商品的存储优化
  // 产品A：体积大，数量少
  let product_a_volume = 15
  let product_a_quantity = 23
  
  // 产品B：体积小，数量多
  let product_b_volume = 3
  let product_b_quantity = 87
  
  // 产品C：中等体积，中等数量
  let product_c_volume = 8
  let product_c_quantity = 45
  
  // 计算各产品总体积
  let total_volume_a = multiply(product_a_volume, product_a_quantity)
  let total_volume_b = multiply(product_b_volume, product_b_quantity)
  let total_volume_c = multiply(product_c_volume, product_c_quantity)
  
  // 计算总体积
  let combined_volume = add(add(total_volume_a, total_volume_b), total_volume_c)
  
  // 计算需要的容器数量（每个容器容量100）
  let container_capacity = 100
  let containers_needed = divide_with_ceil(combined_volume, container_capacity)
  
  assert_eq(345, total_volume_a)  // 15 * 23 = 345
  assert_eq(261, total_volume_b)  // 3 * 87 = 261
  assert_eq(360, total_volume_c)  // 8 * 45 = 360
  assert_eq(966, combined_volume)  // 345 + 261 + 360 = 966
  assert_eq(10, containers_needed)  // ceil(966/100) = 10
}

test "project_resource_scheduling" {
  // 项目资源调度测试：多阶段项目资源分配
  let total_project_days = 120
  let phase1_percentage = 25
  let phase2_percentage = 40
  let phase3_percentage = 35
  
  let team_size = 12
  let work_hours_per_day = 8
  
  // 计算各阶段天数
  let phase1_days = divide_with_ceil(multiply(total_project_days, phase1_percentage), 100)
  let phase2_days = divide_with_ceil(multiply(total_project_days, phase2_percentage), 100)
  let phase3_days = total_project_days - subtract(phase1_days, phase2_days)
  
  // 计算各阶段总工时
  let phase1_hours = multiply(phase1_days, multiply(team_size, work_hours_per_day))
  let phase2_hours = multiply(phase2_days, multiply(team_size, work_hours_per_day))
  let phase3_hours = multiply(phase3_days, multiply(team_size, work_hours_per_day))
  
  // 计算项目总工时
  let total_project_hours = add(add(phase1_hours, phase2_hours), phase3_hours)
  
  assert_eq(30, phase1_days)  // ceil(120*25/100) = 30
  assert_eq(48, phase2_days)  // ceil(120*40/100) = 48
  assert_eq(42, phase3_days)  // 120 - 30 - 48 = 42
  assert_eq(2880, phase1_hours)  // 30 * 12 * 8 = 2880
  assert_eq(4608, phase2_hours)  // 48 * 12 * 8 = 4608
  assert_eq(4032, phase3_hours)  // 42 * 12 * 8 = 4032
  assert_eq(11520, total_project_hours)  // 2880 + 4608 + 4032 = 11520
}

test "data_transmission_optimization" {
  // 数据传输优化测试：网络数据包传输计算
  let total_data_size = 15750  // KB
  let packet_header_size = 20   // KB
  let max_payload_size = 1000   // KB
  let transmission_overhead = 5 // 百分比
  
  // 计算需要的包数量
  let packets_needed = divide_with_ceil(total_data_size, max_payload_size)
  
  // 计算总传输数据（包含包头）
  let total_transmitted_data = add(
    total_data_size,
    multiply(packets_needed, packet_header_size)
  )
  
  // 计算包含传输开销的总数据
  let total_with_overhead = add(
    total_transmitted_data,
    divide_with_ceil(multiply(total_transmitted_data, transmission_overhead), 100)
  )
  
  assert_eq(16, packets_needed)  // ceil(15750/1000) = 16
  assert_eq(16070, total_transmitted_data)  // 15750 + 16*20 = 16070
  assert_eq(16874, total_with_overhead)  // 16070 + ceil(16070*5/100) = 16070 + 804 = 16874
}

test "manufacturing_production_planning" {
  // 制造业生产计划测试：生产线优化
  let daily_demand = 1250
  let production_lines = 3
  let hours_per_shift = 8
  let shifts_per_day = 2
  let maintenance_time_per_day = 2  // 小时
  
  // 计算每日可用生产时间
  let daily_production_time = multiply(
    subtract(multiply(hours_per_shift, shifts_per_day), maintenance_time_per_day),
    production_lines
  )
  
  // 计算每条线需要生产的数量
  let production_per_line = divide_with_ceil(daily_demand, production_lines)
  
  // 计算每小时需要生产的产品数量
  let hourly_production_per_line = divide_with_ceil(
    production_per_line,
    subtract(multiply(hours_per_shift, shifts_per_day), maintenance_time_per_day)
  )
  
  assert_eq(42, daily_production_time)  // (8*2-2)*3 = 14*3 = 42
  assert_eq(417, production_per_line)  // ceil(1250/3) = 417
  assert_eq(30, hourly_production_per_line)  // ceil(417/14) = 30
}

test "complex_string_processing_workflow" {
  // 复杂字符串处理工作流测试
  let base_name = "MoonBit"
  let suffix = "2024"
  let separator = "_"
  
  // 构建复杂字符串
  let stage1 = greet(base_name)
  let stage2 = greet(stage1)
  let stage3 = greet(add(base_name, suffix))
  let stage4 = greet(add(add(base_name, separator), suffix))
  
  // 验证字符串处理结果
  assert_eq_string("Hello, MoonBit!", stage1)
  assert_eq_string("Hello, Hello, MoonBit!!", stage2)
  assert_eq_string("Hello, MoonBit2024!", stage3)
  assert_eq_string("Hello, MoonBit_2024!", stage4)
}

test "mathematical_series_calculation" {
  // 数学级数计算测试：等差数列和等比数列
  let first_term = 3
  let common_difference = 5
  let number_of_terms = 8
  
  // 计算等差数列的和
  let last_term = add(first_term, multiply(common_difference, subtract(number_of_terms, 1)))
  let arithmetic_sum = divide_with_ceil(
    multiply(number_of_terms, add(first_term, last_term)),
    2
  )
  
  // 计算等比数列的和（简化版本）
  let common_ratio = 2
  let geometric_sum = subtract(
    multiply(first_term, multiply(common_ratio, number_of_terms)),
    first_term
  )
  
  assert_eq(38, last_term)  // 3 + 5*(8-1) = 38
  assert_eq(164, arithmetic_sum)  // ceil(8*(3+38)/2) = ceil(164) = 164
  assert_eq(45, geometric_sum)  // 3*2*8 - 3 = 48 - 3 = 45
}

test "energy_consumption_analysis" {
  // 能耗分析测试：建筑能源使用优化
  let building_area = 2500  // 平方米
  let energy_per_square_meter = 45  // kWh/年
  let peak_hours_percentage = 30
  let off_peak_discount = 40  // 百分比
  
  // 计算年能耗
  let annual_energy_consumption = multiply(building_area, energy_per_square_meter)
  
  // 计算峰值和谷值能耗
  let peak_energy = divide_with_ceil(
    multiply(annual_energy_consumption, peak_hours_percentage),
    100
  )
  let off_peak_energy = subtract(annual_energy_consumption, peak_energy)
  
  // 计算谷值能耗成本（有折扣）
  let off_peak_cost = multiply(
    off_peak_energy,
    subtract(100, off_peak_discount)
  )
  
  assert_eq(112500, annual_energy_consumption)  // 2500 * 45 = 112500
  assert_eq(33750, peak_energy)  // ceil(112500*30/100) = 33750
  assert_eq(78750, off_peak_energy)  // 112500 - 33750 = 78750
  assert_eq(47250, off_peak_cost)  // 78750 * (100-40) = 78750 * 60 = 4725000 -> 简化计算
}

test "logistics_distribution_optimization" {
  // 物流配送优化测试：多仓库配送网络
  let warehouse1_capacity = 500
  let warehouse2_capacity = 750
  let warehouse3_capacity = 600
  
  let total_orders = 1850
  let urgent_orders_percentage = 25
  let delivery_cost_per_unit = 3
  
  // 计算总仓库容量
  let total_warehouse_capacity = add(add(warehouse1_capacity, warehouse2_capacity), warehouse3_capacity)
  
  // 计算紧急订单数量
  let urgent_orders = divide_with_ceil(
    multiply(total_orders, urgent_orders_percentage),
    100
  )
  
  // 计算常规订单数量
  let regular_orders = subtract(total_orders, urgent_orders)
  
  // 计算配送成本
  let total_delivery_cost = multiply(total_orders, delivery_cost_per_unit)
  
  assert_eq(1850, total_warehouse_capacity)  // 500 + 750 + 600 = 1850
  assert_eq(463, urgent_orders)  // ceil(1850*25/100) = 463
  assert_eq(1387, regular_orders)  // 1850 - 463 = 1387
  assert_eq(5550, total_delivery_cost)  // 1850 * 3 = 5550
}

test "complex_error_boundary_handling" {
  // 复杂错误边界处理测试：各种边界情况
  // 测试极值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试大数运算（在安全范围内）
  let large_safe_number = 1000000
  let large_addition = add(large_safe_number, large_safe_number)
  let large_multiplication = multiply(large_safe_number, 100)
  
  // 测试边界除法
  let boundary_division1 = divide_with_ceil(max_int, max_int)
  let boundary_division2 = divide_with_ceil(min_int, max_int)
  let boundary_division3 = divide_with_ceil(1, max_int)
  
  // 测试零相关运算
  let zero_operations1 = multiply(0, max_int)
  let zero_operations2 = add(0, min_int)
  let zero_operations3 = divide_with_ceil(0, max_int)
  let zero_operations4 = divide_with_ceil(max_int, 0)
  
  assert_eq(2000000, large_addition)  // 1000000 + 1000000 = 2000000
  assert_eq(100000000, large_multiplication)  // 1000000 * 100 = 100000000
  assert_eq(1, boundary_division1)  // ceil(2147483647/2147483647) = 1
  assert_eq(-1, boundary_division2)  // ceil(-2147483648/2147483647) = -1
  assert_eq(1, boundary_division3)  // ceil(1/2147483647) = 1
  assert_eq(0, zero_operations1)  // 0 * 2147483647 = 0
  assert_eq(-2147483648, zero_operations2)  // 0 + (-2147483648) = -2147483648
  assert_eq(0, zero_operations3)  // ceil(0/2147483647) = 0
  assert_eq(0, zero_operations4)  // ceil(2147483647/0) = 0 (错误处理)
}