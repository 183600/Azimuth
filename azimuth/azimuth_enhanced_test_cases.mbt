// Azimuth 增强测试用例 - 10个高质量标准 MoonBit 测试用例
// 测试覆盖基本功能、边界情况、错误处理和实际应用场景

test "add_function_comprehensive_coverage" {
  // 全面测试加法函数的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边缘情况和特殊值
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符和特殊符号支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
}

test "divide_with_ceil_business_logistics" {
  // 测试向上取整除法的业务物流场景
  // 场景：计算仓库存储需要的货架数量
  let products = 157
  let capacity_per_shelf = 24
  let shelves_needed = divide_with_ceil(products, capacity_per_shelf)
  assert_eq(7, shelves_needed)
  
  // 验证总存储容量
  let total_capacity = multiply(shelves_needed, capacity_per_shelf)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, products)
  assert_eq(11, remaining_space)
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流程
  // 场景：计算项目资源需求和分配
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(3, sprints_needed)
  
  // 计算实际完成的任务数
  let completed_tasks = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(45, completed_tasks)
  
  // 计算额外容量
  let extra_capacity = subtract(completed_tasks, total_tasks)
  assert_eq(5, extra_capacity)
}

test "mathematical_properties_validation" {
  // 验证基本数学性质和恒等式
  let x = 42
  let y = 17
  let z = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
  
  // 结合律验证
  assert_eq(
    add(add(x, y), z),
    add(x, add(y, z))
  )
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  let min_budget_per_member = 1500
  
  // 计算基本分配
  let base_allocation = multiply(team_members, min_budget_per_member)
  assert_eq(9000, base_allocation)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, base_allocation)
  assert_eq(1000, remaining_budget)
  
  // 使用向上取整进行公平分配
  let equal_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, equal_share)
  
  // 验证总分配
  let total_allocated = multiply(equal_share, team_members)
  assert_eq(10002, total_allocated)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello,   spaces!", greet("  spaces  "))
}

test "real_world_event_planning" {
  // 真实世界活动规划场景测试
  let participants = 87
  let tables = 8
  let chairs_per_table = 10
  let budget_per_person = 25
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 计算总预算
  let total_budget = multiply(participants, budget_per_person)
  assert_eq(2175, total_budget)
  
  // 计算每桌平均预算
  let budget_per_table = divide_with_ceil(total_budget, tables_needed)
  assert_eq(242, budget_per_table)
  
  // 验证座位充足性
  assert_true(available_seats >= participants)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}