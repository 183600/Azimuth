// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_combinations" {
  // 基本算术运算组合测试
  let a = 15
  let b = 4
  let c = 7
  
  // 测试加法和乘法的组合
  @azimuth.assert_eq(67, @azimuth.add(@azimuth.multiply(a, b), c))  // 15*4 + 7 = 67
  @azimuth.assert_eq(133, @azimuth.multiply(@azimuth.add(a, b), c))  // (15+4)*7 = 133
  
  // 测试减法和除法的组合
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(@azimuth.subtract(a, b), c))  // ceil((15-4)/7) = 2
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(@azimuth.subtract(b, a), c))  // ceil((4-15)/7) = -2
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试零值边界
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  
  // 测试单位值边界
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  
  // 测试极值边界（在安全范围内）
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试：贷款分期付款
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  
  @azimuth.assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 验证总支付金额
  let total_payment = @azimuth.multiply(months_needed, monthly_payment)
  @azimuth.assert_eq(50600, total_payment)
  
  // 验证最后一个月的实际支付金额
  let final_payment = @azimuth.subtract(total_payment, @azimuth.multiply(months_needed - 1, monthly_payment))
  @azimuth.assert_eq(200, final_payment)
}

test "inventory_management_system" {
  // 库存管理系统测试
  // 产品A：125个，每箱装15个
  let product_a_quantity = 125
  let product_a_box_capacity = 15
  let boxes_a = @azimuth.divide_with_ceil(product_a_quantity, product_a_box_capacity)
  
  // 产品B：87个，每箱装12个
  let product_b_quantity = 87
  let product_b_box_capacity = 12
  let boxes_b = @azimuth.divide_with_ceil(product_b_quantity, product_b_box_capacity)
  
  // 产品C：234个，每箱装25个
  let product_c_quantity = 234
  let product_c_box_capacity = 25
  let boxes_c = @azimuth.divide_with_ceil(product_c_quantity, product_c_box_capacity)
  
  // 验证计算结果
  @azimuth.assert_eq(9, boxes_a)   // ceil(125/15) = 9
  @azimuth.assert_eq(8, boxes_b)   // ceil(87/12) = 8
  @azimuth.assert_eq(10, boxes_c)  // ceil(234/25) = 10
  
  // 计算总箱子数
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  @azimuth.assert_eq(27, total_boxes)
  
  // 计算总存储容量
  let total_capacity = @azimuth.add(
    @azimuth.multiply(boxes_a, product_a_box_capacity),
    @azimuth.add(@azimuth.multiply(boxes_b, product_b_box_capacity), @azimuth.multiply(boxes_c, product_c_box_capacity))
  )
  @azimuth.assert_eq(403, total_capacity)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
  
  // 极值运算测试
  let large_positive = 100000
  let large_negative = -100000
  
  @azimuth.assert_eq(0, @azimuth.multiply(large_positive, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(large_negative, 0))
  @azimuth.assert_eq(large_positive, @azimuth.add(large_positive, 0))
  @azimuth.assert_eq(large_negative, @azimuth.add(large_negative, 0))
  
  // 边界除法测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_positive, large_positive))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_negative, large_negative))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(large_positive, large_negative))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 基本字符串测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, éspanol!", @azimuth.greet("éspanol"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  
  // 数字和字母混合测试
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
  @azimuth.assert_eq_string("Hello, 123test!", @azimuth.greet("123test"))
  @azimuth.assert_eq_string("Hello, a1b2c3!", @azimuth.greet("a1b2c3"))
  
  // 空白字符测试
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, leading space!", @azimuth.greet(" leading space"))
  @azimuth.assert_eq_string("Hello, trailing space !", @azimuth.greet("trailing space "))
}

test "project_management_workflow" {
  // 项目管理工作流测试
  let team_size = 8
  let sprint_duration = 2  // 周
  let tasks_per_person_per_week = 5
  let total_tasks = 95
  
  // 计算团队每周处理能力
  let weekly_capacity = @azimuth.multiply(team_size, tasks_per_person_per_week)
  @azimuth.assert_eq(40, weekly_capacity)
  
  // 计算每个冲刺的处理能力
  let sprint_capacity = @azimuth.multiply(weekly_capacity, sprint_duration)
  @azimuth.assert_eq(80, sprint_capacity)
  
  // 计算需要的冲刺数
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  @azimuth.assert_eq(2, sprints_needed)
  
  // 计算实际处理能力
  let actual_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  @azimuth.assert_eq(160, actual_capacity)
  
  // 计算剩余处理能力
  let remaining_capacity = @azimuth.subtract(actual_capacity, total_tasks)
  @azimuth.assert_eq(65, remaining_capacity)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  let z = 5
  
  // 加法交换律：x + y = y + x
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律：x * y = y * x
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法结合律：(x + y) + z = x + (y + z)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 乘法结合律：(x * y) * z = x * (y * z)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
  
  // 分配律：x * (y + z) = (x * y) + (x * z)
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, z)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  )
  
  // 恒等式验证
  @azimuth.assert_eq(x, @azimuth.add(x, 0))      // x + 0 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1)) // x * 1 = x
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0)) // x * 0 = 0
  @azimuth.assert_eq(0, @azimuth.subtract(x, x)) // x - x = 0
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：运输公司需要将不同数量的货物分配到不同容量的车辆中
  
  // 货物类型A：156件，车辆容量：20件/车
  let cargo_a = 156
  let vehicle_capacity_a = 20
  let vehicles_a = @azimuth.divide_with_ceil(cargo_a, vehicle_capacity_a)
  
  // 货物类型B：89件，车辆容量：15件/车
  let cargo_b = 89
  let vehicle_capacity_b = 15
  let vehicles_b = @azimuth.divide_with_ceil(cargo_b, vehicle_capacity_b)
  
  // 货物类型C：237件，车辆容量：25件/车
  let cargo_c = 237
  let vehicle_capacity_c = 25
  let vehicles_c = @azimuth.divide_with_ceil(cargo_c, vehicle_capacity_c)
  
  // 验证车辆需求
  @azimuth.assert_eq(8, vehicles_a)   // ceil(156/20) = 8
  @azimuth.assert_eq(6, vehicles_b)   // ceil(89/15) = 6
  @azimuth.assert_eq(10, vehicles_c)  // ceil(237/25) = 10
  
  // 计算总车辆数
  let total_vehicles = @azimuth.add(@azimuth.add(vehicles_a, vehicles_b), vehicles_c)
  @azimuth.assert_eq(24, total_vehicles)
  
  // 计算总运输能力
  let total_capacity = @azimuth.add(
    @azimuth.multiply(vehicles_a, vehicle_capacity_a),
    @azimuth.add(@azimuth.multiply(vehicles_b, vehicle_capacity_b), @azimuth.multiply(vehicles_c, vehicle_capacity_c))
  )
  @azimuth.assert_eq(475, total_capacity)
  
  // 计算剩余运输能力
  let total_cargo = @azimuth.add(@azimuth.add(cargo_a, cargo_b), cargo_c)
  let remaining_capacity = @azimuth.subtract(total_capacity, total_cargo)
  @azimuth.assert_eq(7, remaining_capacity)
  
  // 计算运输利用率
  let utilization_rate = @azimuth.divide_with_ceil(@azimuth.multiply(total_cargo, 100), total_capacity)
  @azimuth.assert_eq(99, utilization_rate)  // floor(48200/47500) = 98%
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  // 模拟一个复杂的业务计算流程
  
  let initial_value = 100
  let growth_factor = 15
  let periods = 4
  let divisor = 3
  let adjustment = 25
  
  // 第一步：计算增长值
  let growth_amount = @azimuth.multiply(initial_value, growth_factor)
  @azimuth.assert_eq(1500, growth_amount)
  
  // 第二步：计算总增长（考虑周期）
  let total_growth = @azimuth.multiply(growth_amount, periods)
  @azimuth.assert_eq(6000, total_growth)
  
  // 第三步：计算平均值
  let average_growth = @azimuth.divide_with_ceil(total_growth, periods)
  @azimuth.assert_eq(1500, average_growth)
  
  // 第四步：应用调整
  let adjusted_value = @azimuth.add(average_growth, adjustment)
  @azimuth.assert_eq(1525, adjusted_value)
  
  // 第五步：最终分配计算
  let final_allocation = @azimuth.divide_with_ceil(adjusted_value, divisor)
  @azimuth.assert_eq(509, final_allocation)
  
  // 验证整个计算链的一致性
  let verification = @azimuth.multiply(final_allocation, divisor)
  @azimuth.assert_eq(1527, verification)  // 由于向上取整，会略大于原值
  
  // 验证差异在可接受范围内
  let difference = @azimuth.subtract(verification, adjusted_value)
  @azimuth.assert_eq(2, difference)  // 差异应该很小
}