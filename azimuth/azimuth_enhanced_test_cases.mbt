// Azimuth 增强测试用例 - 标准MoonBit测试语法
// 包含8个高质量测试用例，覆盖各种实际应用场景

test "arithmeti_sequence_operations" {
  // 测试算术序列操作
  let first_term = 3
  let common_diff = 4
  let n_terms = 5
  
  // 计算等差数列第n项: a_n = a_1 + (n-1)*d
  let nth_term = add(first_term, multiply(n_terms - 1, common_diff))
  assert_eq(19, nth_term)  // 3 + (5-1)*4 = 19
  
  // 计算前n项和: S_n = n/2 * (2*a_1 + (n-1)*d)
  let sum_first_n = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  assert_eq(55, sum_first_n)  // 5/2 * (6 + 16) = 55
}

test "inventory_management_system" {
  // 库存管理系统测试
  let product_a_qty = 120
  let product_b_qty = 85
  let product_c_qty = 200
  
  let box_capacity_a = 25
  let box_capacity_b = 20
  let box_capacity_c = 45
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity_a)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity_b)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity_c)
  
  assert_eq(5, boxes_a)   // ceil(120/25) = 5
  assert_eq(5, boxes_b)   // ceil(85/20) = 5
  assert_eq(5, boxes_c)   // ceil(200/45) = 5
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = add(
    multiply(boxes_a, box_capacity_a),
    add(multiply(boxes_b, box_capacity_b), multiply(boxes_c, box_capacity_c))
  )
  
  assert_eq(15, total_boxes)
  assert_eq(450, total_capacity)
  
  // 计算总产品数
  let total_products = add(add(product_a_qty, product_b_qty), product_c_qty)
  assert_eq(405, total_products)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, total_products)
  assert_eq(45, remaining_capacity)
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  let total_minutes = 475
  let minutes_per_hour = 60
  let hours_per_day = 8
  let days_per_week = 5
  
  // 计算小时数
  let hours = divide_with_ceil(total_minutes, minutes_per_hour)
  assert_eq(8, hours)  // ceil(475/60) = 8
  
  // 计算工作天数
  let work_days = divide_with_ceil(hours, hours_per_day)
  assert_eq(1, work_days)  // ceil(8/8) = 1
  
  // 计算工作周数
  let work_weeks = divide_with_ceil(work_days, days_per_week)
  assert_eq(1, work_weeks)  // ceil(1/5) = 1
  
  // 验证总工作时间
  let total_work_hours = multiply(work_days, hours_per_day)
  let total_work_minutes = multiply(total_work_hours, minutes_per_hour)
  assert_eq(480, total_work_minutes)
}

test "string_concatenation_validation" {
  // 字符串连接验证测试
  let first_name = "John"
  let last_name = "Doe"
  let age = 30
  let city = "New York"
  
  // 测试各种字符串组合
  assert_eq_string("Hello, John!", greet(first_name))
  assert_eq_string("Hello, John Doe!", greet(first_name + " " + last_name))
  assert_eq_string("Hello, JD!", greet("JD"))
  assert_eq_string("Hello, 30!", greet(age.to_string()))
  assert_eq_string("Hello, NYC!", greet("NYC"))
  
  // 测试特殊字符
  assert_eq_string("Hello, John-Doe!", greet("John-Doe"))
  assert_eq_string("Hello, O'Reilly!", greet("O'Reilly"))
  assert_eq_string("Hello, John & Jane!", greet("John & Jane"))
}

test "financial_budget_allocation" {
  // 财务预算分配测试
  let total_budget = 50000
  let departments = 4
  
  // 平均分配预算
  let base_allocation = divide_with_ceil(total_budget, departments)
  assert_eq(12500, base_allocation)  // ceil(50000/4) = 12500
  
  // 各部门实际需求
  let dept_a_need = 12000
  let dept_b_need = 15000
  let dept_c_need = 10000
  let dept_d_need = 13000
  
  // 计算总需求
  let total_need = add(add(dept_a_need, dept_b_need), add(dept_c_need, dept_d_need))
  assert_eq(50000, total_need)
  
  // 验证预算充足性
  assert_true(total_need <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_need)
  assert_eq(0, remaining_budget)
}

test "geometric_calculations" {
  // 几何计算测试
  let length = 12
  let width = 8
  let height = 5
  
  // 计算面积和体积
  let area = multiply(length, width)
  let volume = multiply(area, height)
  
  assert_eq(96, area)    // 12 * 8 = 96
  assert_eq(480, volume) // 96 * 5 = 480
  
  // 计算表面积（简化：2*(lw + lh + wh)）
  let surface_area = multiply(2, add(
    add(multiply(length, width), multiply(length, height)),
    multiply(width, height)
  ))
  assert_eq(392, surface_area)  // 2*(96 + 60 + 40) = 392
  
  // 计算包装需要的立方体数量
  let package_capacity = 100
  let packages_needed = divide_with_ceil(volume, package_capacity)
  assert_eq(5, packages_needed)  // ceil(480/100) = 5
}

test "temperature_conversion_validation" {
  // 温度转换验证测试
  let celsius_temp = 25
  let fahrenheit_temp = 77
  
  // 简化的温度转换验证（使用整数运算）
  // F = C * 9/5 + 32, C = (F - 32) * 5/9
  let converted_f = add(multiply(celsius_temp, 9), 160)
  let converted_f_final = divide_with_ceil(converted_f, 5)
  assert_eq(77, converted_f_final)
  
  let converted_c = multiply(fahrenheit_temp - 32, 5)
  let converted_c_final = divide_with_ceil(converted_c, 9)
  assert_eq(25, converted_c_final)
  
  // 测试极端温度
  let freezing_c = 0
  let freezing_f = 32
  let converted_f_freezing = add(multiply(freezing_c, 9), 160)
  let converted_f_freezing_final = divide_with_ceil(converted_f_freezing, 5)
  assert_eq(32, converted_f_freezing_final)
  
  let boiling_c = 100
  let boiling_f = 212
  let converted_f_boiling = add(multiply(boiling_c, 9), 160)
  let converted_f_boiling_final = divide_with_ceil(converted_f_boiling, 5)
  assert_eq(212, converted_f_boiling_final)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  let total_items = 150
  let container_types = 3
  
  // 不同容器的容量
  let large_capacity = 50
  let medium_capacity = 30
  let small_capacity = 15
  
  // 优先使用大容器
  let large_containers = divide_with_ceil(total_items, large_capacity)
  let remaining_after_large = subtract(total_items, multiply(large_containers - 1, large_capacity))
  
  assert_eq(3, large_containers)  // ceil(150/50) = 3
  
  // 如果最后一个大容器未满，尝试混合策略
  let medium_containers = divide_with_ceil(remaining_after_large, medium_capacity)
  let remaining_after_medium = subtract(remaining_after_large, multiply(medium_containers - 1, medium_capacity))
  
  let small_containers = divide_with_ceil(remaining_after_medium, small_capacity)
  
  // 验证总容量
  let total_capacity = add(
    multiply(large_containers, large_capacity),
    add(multiply(medium_containers, medium_capacity), multiply(small_containers, small_capacity))
  )
  
  assert_true(total_capacity >= total_items)
  
  // 计算空间利用率
  let utilization = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(100, utilization)  // 100%利用率
}