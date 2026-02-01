// 标准 MoonBit 测试套件 - 为 Azimuth 项目添加
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

// 导入 azimuth 库的函数
use "../azimuth/lib"

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(0, 0))
  assert_eq(100, add(50, 50))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(7, 0))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "string_greeting_international" {
  // 国际化字符串问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 交换律
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
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
  let utilization = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(94, utilization)  // floor(12700/135) = 94%
}

test "complex_workflow_resource_allocation" {
  // 复杂工作流资源分配测试
  // 场景：项目管理中的资源分配
  let team_members = 9
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let daily_capacity = 20
  let sprint_capacity = multiply(sprint_duration, daily_capacity)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)  // 9 * 7 = 63
  assert_eq(40, sprint_capacity)  // 2 * 20 = 40
  assert_eq(2, sprints_needed)  // ceil(63/40) = 2
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(80, total_capacity)
  
  // 验证剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(17, remaining_capacity)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极小值
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(0, 1))
  
  // 极大值（但在安全范围内）
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(10001, divide_with_ceil(10000, 1))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
  assert_eq(1, divide_with_ceil(10000, 10000))
  assert_eq(2, divide_with_ceil(10001, 10000))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期付款计算
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 验证最后一个月的实际支付金额
  let regular_payments = multiply(months_needed - 1, monthly_payment)
  let final_payment = subtract(total_payment, regular_payments)
  assert_eq(400, final_payment)
  
  // 验证总利息
  let total_interest = subtract(total_payment, loan_amount)
  assert_eq(600, total_interest)
}

test "event_planning_resource_optimization" {
  // 活动规划资源优化测试
  let attendees = 245
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(31, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(248, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6200, total_cost)
  
  // 验证预算（这里会超预算，这是测试的一部分）
  assert_true(total_cost > budget)
  
  // 计算超出预算的金额
  let budget_overrun = subtract(total_cost, budget)
  assert_eq(200, budget_overrun)
}

test "algorithm_complexity_validation" {
  // 算法复杂度验证测试
  // 验证不同算法策略的性能特征
  let data_size = 1000
  
  // 线性算法：O(n)
  let linear_operations = multiply(data_size, 5)
  assert_eq(5000, linear_operations)
  
  // 对数算法：O(log n) - 使用向上取整除法模拟
  let log_operations = divide_with_ceil(data_size, 100)
  assert_eq(10, log_operations)
  
  // 平方算法：O(n²) - 小规模测试
  let small_size = 20
  let quadratic_operations = multiply(small_size, small_size)
  assert_eq(400, quadratic_operations)
  
  // 验证算法效率差异
  let linear_vs_log_ratio = divide_with_ceil(linear_operations, log_operations)
  assert_eq(500, linear_vs_log_ratio)
  
  // 验证数据规模对平方算法的影响
  let medium_size = 30
  let medium_quadratic = multiply(medium_size, medium_size)
  let complexity_increase = divide_with_ceil(medium_quadratic, quadratic_operations)
  assert_eq(3, complexity_increase)  // 30²/20² = 900/400 = 2.25 -> ceil = 3
}