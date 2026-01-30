// Azimuth 项目高质量测试用例
// 使用标准 MoonBit 测试语法编写

test "basic_arithmetic_comprehensive" {
  // 基础算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(5, -5))
  assert_eq(42, multiply(6, 7))
  assert_eq(-42, multiply(-6, 7))
  assert_eq(0, multiply(0, 12345))
}

test "divide_with_ceil_precision_edge_cases" {
  // 向上取整除法的精度和边缘情况测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(50, 0))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-3, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "string_processing_unicode_support" {
  // 字符串处理和 Unicode 支持测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 24
  let y = 18
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法交换律和分配律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
  
  // 减法性质
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "real_world_inventory_management" {
  // 真实世界库存管理场景测试
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(7, boxes_needed)  // ceil(158/24) = 7
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(10, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源分配计算
  let team_members = 8
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(1920, total_tasks)  // 960 * 2 = 1920
  
  // 计算需要的冲刺数（每冲刺处理500个任务）
  let sprint_capacity = 500
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(4, sprints_needed)  // ceil(1920/500) = 4
}

test "error_handling_and_boundary_values" {
  // 错误处理和边界值测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment_max = 2500
  let months_needed = divide_with_ceil(loan_amount, monthly_payment_max)
  
  // 验证需要的月份数
  assert_eq(20, months_needed)  // ceil(50000/2500) = 20
  
  // 计算实际总支付
  let actual_monthly_payment = divide_with_ceil(loan_amount, months_needed)
  assert_eq(2500, actual_monthly_payment)
  
  let total_payment = multiply(months_needed, actual_monthly_payment)
  assert_eq(50000, total_payment)
  
  // 计算最后一个月的实际支付
  let regular_payments = subtract(months_needed, 1)
  let regular_total = multiply(regular_payments, actual_monthly_payment)
  let final_payment = subtract(total_payment, regular_total)
  assert_eq(2500, final_payment)
}

test "event_planning_resource_optimization" {
  // 活动规划资源优化测试
  let participants = 245
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(31, tables_needed)  // ceil(245/8) = 31
  
  // 计算实际可用的座位数
  let available_seats = multiply(tables_available, chairs_per_table)
  assert_eq(240, available_seats)
  
  // 计算需要额外租用的椅子
  let additional_chairs = subtract(participants, available_seats)
  assert_eq(5, additional_chairs)
  
  // 计算总成本
  let total_chairs = multiply(tables_available, chairs_per_table)
  let chair_cost = multiply(total_chairs, cost_per_chair)
  let additional_cost = multiply(additional_chairs, cost_per_chair)
  let total_cost = add(chair_cost, additional_cost)
  
  // 验证预算
  assert_eq(6125, total_cost)  // 30*8*25 + 5*25 = 6000 + 125 = 6125
}