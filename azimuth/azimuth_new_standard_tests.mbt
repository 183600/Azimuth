// Azimuth 项目新增标准测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖核心功能和业务场景

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, multiply(0, 100))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "business_logistics_calculation" {
  // 物流业务计算测试
  // 场景：计算运输需求
  let total_packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(total_packages, capacity_per_truck)
  
  assert_eq(5, trucks_needed)
  
  // 验证总运输能力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
  
  // 计算剩余空间
  let unused_space = subtract(total_capacity, total_packages)
  assert_eq(3, unused_space)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 15000
  let monthly_payment = 1200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(13, months_needed)
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(15600, total_payment)
  
  // 计算最后一个月的实际支付
  let regular_payments = multiply(months_needed - 1, monthly_payment)
  let final_payment = subtract(total_payment, regular_payments)
  assert_eq(600, final_payment)
}

test "string_processing_international" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源规划
  let team_members = 6
  let daily_hours = 8
  let project_days = 15
  let hourly_rate = 50
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(daily_hours, project_days))
  assert_eq(720, total_hours)
  
  // 计算项目成本
  let total_cost = multiply(total_hours, hourly_rate)
  assert_eq(36000, total_cost)
  
  // 计算需要的支付周期（每周期不超过10000）
  let payment_cycles = divide_with_ceil(total_cost, 10000)
  assert_eq(4, payment_cycles)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：仓库存储优化
  let item_a_quantity = 25
  let item_b_quantity = 47
  let box_capacity = 12
  
  // 计算每种物品需要的箱子数
  let boxes_a = divide_with_ceil(item_a_quantity, box_capacity)
  let boxes_b = divide_with_ceil(item_b_quantity, box_capacity)
  
  assert_eq(3, boxes_a)  // ceil(25/12) = 3
  assert_eq(4, boxes_b)  // ceil(47/12) = 4
  
  // 计算总箱子数和总容量
  let total_boxes = add(boxes_a, boxes_b)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(7, total_boxes)
  assert_eq(84, total_capacity)
  
  // 计算剩余空间
  let total_items = add(item_a_quantity, item_b_quantity)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(12, remaining_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议座位安排
  let participants = 87
  let tables = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  let seating_sufficient = available_seats >= participants
  assert_true(seating_sufficient)
  
  // 计算桌子使用率
  let table_utilization = divide_with_ceil(multiply(participants, 100), available_seats)
  assert_eq(97, table_utilization)  // floor(8700/90) = 96.67% ≈ 97%
}