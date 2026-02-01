// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(0, 0))
  assert_eq(1000, add(500, 500))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-20, multiply(-5, 4))
  assert_eq(0, multiply(0, 123))
  assert_eq(10000, multiply(100, 100))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-9, subtract(2, 11))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
}

test "business_logistics_packaging" {
  // 业务物流包装计算测试
  let total_items = 127
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(9, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(135, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(8, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(
    multiply(total_items, 100), 
    total_capacity
  )
  assert_eq(94, utilization_percent)  // floor(12700/135) = 94%
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_tasks = 89
  let team_members = 5
  let working_days = 14
  
  // 计算每人需要完成的任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  assert_eq(18, tasks_per_person)
  
  // 计算每人每天需要完成的任务数
  let daily_tasks_per_person = divide_with_ceil(tasks_per_person, working_days)
  assert_eq(2, daily_tasks_per_person)
  
  // 验证总处理能力
  let total_capacity = multiply(
    multiply(team_members, daily_tasks_per_person), 
    working_days
  )
  assert_eq(140, total_capacity)
  
  // 验证处理能力充足
  assert_true(total_capacity >= total_tasks)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(9999, 0))
  assert_eq(0, divide_with_ceil(-9999, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 极值测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(23, months_needed)
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 计算最后一个月的实际支付金额
  let regular_payments = multiply(months_needed - 1, monthly_payment)
  let final_payment = subtract(total_payment, regular_payments)
  assert_eq(200, final_payment)
  
  // 计算总利息
  let total_interest = subtract(total_payment, loan_amount)
  assert_eq(600, total_interest)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 20
  let multiplier = 3
  let divisor = 8
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  assert_eq(60, step1)
  
  let step2 = divide_with_ceil(step1, divisor)
  assert_eq(8, step2)
  
  let result = add(step2, addition)
  assert_eq(15, result)
  
  // 验证计算结果符合预期
  let expected_result = add(
    divide_with_ceil(multiply(base_value, multiplier), divisor),
    addition
  )
  assert_eq(result, expected_result)
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 156
  let tables_available = 20
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3500
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(20, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(160, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(4, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(4000, total_cost)
  
  // 验证预算情况（这里总成本超过了预算，这是一个现实的测试场景）
  let budget_overage = subtract(total_cost, total_budget)
  assert_eq(500, budget_overage)
  
  // 计算需要增加的预算百分比
  let budget_increase_percent = divide_with_ceil(
    multiply(budget_overage, 100),
    total_budget
  )
  assert_eq(15, budget_increase_percent)  // ceil(50000/3500) = 15%
}