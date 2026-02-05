// Azimuth 标准增强测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖各种场景

// 导入 azimuth 库
use "azimuth"

test "basic_arithmetic_comprehensive" {
  // 综合算术运算测试
  assert_eq(10, add(6, 4))
  assert_eq(24, multiply(6, 4))
  assert_eq(2, divide_with_ceil(6, 4))
  assert_eq(1, divide_with_ceil(4, 4))
  assert_eq(2, subtract(6, 4))
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 5
  let b = 3
  let c = 2
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "negative_numbers_comprehensive" {
  // 负数综合测试
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
  assert_eq(-1, add(2, -3))
  
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-6, multiply(2, -3))
  
  assert_eq(-1, divide_with_ceil(-5, 3))
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "business_inventory_management" {
  // 库存管理业务场景测试
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(10, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 5
  let sprint_capacity = multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(10, sprint_capacity)
  assert_eq(5, sprints_needed)
}

test "boundary_values_and_limits" {
  // 边界值和限制测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
}

test "mathematical_associative_properties" {
  // 数学结合律测试
  let a = 2
  let b = 3
  let c = 4
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "event_planning_scenario" {
  // 活动规划场景测试
  let participants = 125
  let tables_available = 15
  let chairs_per_table = 8
  let cost_per_chair = 20
  let total_budget = 2500
  
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
  assert_eq(2560, total_cost)
  
  // 验证预算
  assert_true(total_cost <= total_budget)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}