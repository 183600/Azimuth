// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

// 导入必要的包
test {
  // 导入 azimuth 库函数
  // 注意：在测试文件中，可以直接使用主包中导出的函数
} derive(Debug, Show)

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-10, 5))
  assert_eq(1000, add(500, 500))
  
  assert_eq(42, multiply(6, 7))
  assert_eq(0, multiply(123, 0))
  assert_eq(-24, multiply(-4, 6))
  assert_eq(10000, multiply(100, 100))
  
  assert_eq(5, subtract(12, 7))
  assert_eq(-5, subtract(7, 12))
  assert_eq(0, subtract(50, 50))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(15, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, éspanol!", greet("éspanol"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, space !", greet("space "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 17
  let b = 23
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  assert_eq(0, multiply(a, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(a, a))
  assert_eq(a, subtract(a, 0))
}

test "business_logistics_scenario" {
  // 业务物流场景测试
  // 场景：计算运输需求
  let total_packages = 157
  let capacity_per_truck = 24
  let trucks_needed = divide_with_ceil(total_packages, capacity_per_truck)
  
  assert_eq(7, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(11, remaining_space)
  
  // 计算空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_packages, 100), total_capacity)
  assert_eq(94, utilization_percent)  // floor(15700/168) = 93%
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 12
  let sprint_capacity = multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证计算结果
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(24, sprint_capacity)  // 2 * 12 = 24
  assert_eq(2, sprints_needed)  // ceil(48/24) = 2
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(48, total_capacity)
  
  // 计算剩余处理能力
  let extra_capacity = subtract(total_capacity, total_tasks)
  assert_eq(0, extra_capacity)
}

test "boundary_conditions_and_edge_cases" {
  // 边界条件和边缘情况测试
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 1000))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000001, divide_with_ceil(1000000, 1))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
  
  // 特殊字符串处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 0!", greet("0"))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  // 场景：会议座位安排
  let participants = 125
  let tables_available = 15
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3200, total_cost)
  
  // 验证预算是否充足（这里应该为false，因为成本超过预算）
  assert_false(total_cost <= total_budget)
  
  // 计算预算缺口
  let budget_shortage = subtract(total_cost, total_budget)
  assert_eq(200, budget_shortage)
}