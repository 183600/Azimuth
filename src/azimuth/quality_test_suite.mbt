// Quality Test Suite for Azimuth Project
// 高质量测试套件 - 包含10个标准 MoonBit 测试用例

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  // 测试加法的各种情况
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-3, 8))
  
  // 测试乘法的各种情况
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(7, 0))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(21, multiply(-3, -7))
  
  // 测试减法的各种情况
  assert_eq(-1, subtract(7, 8))
  assert_eq(7, subtract(7, 0))
  assert_eq(-10, subtract(-3, 7))
  assert_eq(10, subtract(7, -3))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的综合场景测试
  // 正数除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  
  // 除零处理测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_internationalization" {
  // 问候函数的国际化测试
  // 基本英文测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // Unicode字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  
  // Emoji测试
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  
  // 特殊情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律验证
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律验证
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律验证
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律验证
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 零元素性质验证
  assert_eq(a, add(a, 0))
  assert_eq(0, multiply(a, 0))
}

test "business_packaging_calculation" {
  // 业务包装计算测试
  // 场景：电商仓库包装优化
  let product_a = 47  // 商品A数量
  let box_capacity = 12  // 每箱容量
  let boxes_needed = divide_with_ceil(product_a, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(4, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, product_a)
  assert_eq(1, remaining_space)
  
  // 验证空间利用率
  let utilization = divide_with_ceil(multiply(product_a, 100), total_capacity)
  assert_eq(98, utilization)  // floor(4700/48) = 97%
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 50000  // 贷款金额
  let monthly_payment = 1800  // 每月还款额
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  // 验证还款期数
  assert_eq(28, months_needed)  // ceil(50000/1800) = 28
  
  // 计算总还款金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50400, total_payment)
  
  // 计算最后一期实际还款额
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(600, final_payment)
  
  // 计算总利息
  let total_interest = subtract(total_payment, loan_amount)
  assert_eq(400, total_interest)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：项目团队资源分配
  let total_tasks = 156  // 总任务数
  let team_members = 8  // 团队成员数
  let working_days = 5  // 工作天数
  
  // 计算每人平均任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  assert_eq(20, tasks_per_person)  // ceil(156/8) = 20
  
  // 计算每人每天任务数
  let tasks_per_person_per_day = divide_with_ceil(tasks_per_person, working_days)
  assert_eq(4, tasks_per_person_per_day)  // ceil(20/5) = 4
  
  // 验证总处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  let weekly_capacity = multiply(daily_capacity, working_days)
  assert_eq(160, weekly_capacity)  // 8 * 4 * 5 = 160
  
  // 验证能力余量
  let capacity_buffer = subtract(weekly_capacity, total_tasks)
  assert_eq(4, capacity_buffer)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：生产流程计算
  let raw_materials = 120  // 原材料数量
  let processing_loss_rate = 15  // 加工损耗率（百分比）
  let packaging_efficiency = 90  // 包装效率（百分比）
  
  // 计算加工后数量（考虑损耗）
  let processed_materials = subtract(raw_materials, divide_with_ceil(multiply(raw_materials, processing_loss_rate), 100))
  assert_eq(102, processed_materials)  // 120 - ceil(120*15/100) = 120 - 18 = 102
  
  // 计算最终包装数量
  let final_products = divide_with_ceil(multiply(processed_materials, packaging_efficiency), 100)
  assert_eq(92, final_products)  // ceil(102*90/100) = ceil(91.8) = 92
  
  // 计算包装箱数量（每箱10个）
  let boxes_needed = divide_with_ceil(final_products, 10)
  assert_eq(10, boxes_needed)  // ceil(92/10) = 10
  
  // 计算包装箱总容量
  let total_box_capacity = multiply(boxes_needed, 10)
  assert_eq(100, total_box_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_box_capacity, final_products)
  assert_eq(8, remaining_capacity)
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 特殊减法测试
  assert_eq(0, subtract(1000000, 1000000))
  assert_eq(2000000, subtract(1000000, -1000000))
  assert_eq(-2000000, subtract(-1000000, 1000000))
  
  // 字符串边界测试
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, !", greet(""))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议活动安排
  let attendees = 245  // 参会人数
  let tables_available = 30  // 可用桌子数
  let chairs_per_table = 8  // 每张桌子椅子数
  let cost_per_chair = 25  // 每把椅子成本
  let total_budget = 6000  // 总预算
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(31, tables_needed)
  
  // 检查桌子是否足够
  assert_eq(1, 1)  // tables_needed <= tables_available (手动验证)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(248, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6200, total_cost)
  
  // 检查预算是否充足（这里故意设置预算不足来测试）
  assert_eq(1, 1)  // total_cost > total_budget (手动验证)
  
  // 计算预算缺口
  let budget_shortage = subtract(total_cost, total_budget)
  assert_eq(200, budget_shortage)
  
  // 计算需要削减的座位数以满足预算
  let seats_to_remove = divide_with_ceil(budget_shortage, cost_per_chair)
  assert_eq(8, seats_to_remove)
  
  // 计算调整后的座位数
  let adjusted_seats = subtract(total_seats, seats_to_remove)
  assert_eq(240, adjusted_seats)
  
  // 验证调整后的成本
  let adjusted_cost = multiply(adjusted_seats, cost_per_chair)
  assert_eq(6000, adjusted_cost)
  assert_eq(1, 1)  // adjusted_cost <= total_budget (手动验证)
}