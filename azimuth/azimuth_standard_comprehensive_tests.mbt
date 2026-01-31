// Azimuth 标准综合测试用例 - 10个高质量 MoonBit 测试用例
// 覆盖核心功能、边界条件、错误处理和业务逻辑场景

test "basic_arithmetic_functionality" {
  // 基本算术功能测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(7, 5))
  assert_eq(3, divide_with_ceil(8, 3))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 999))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(2, add(-5, 7))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_business_calculation" {
  // 复杂业务计算测试 - 物流装箱场景
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "mathematical_distributive_property" {
  // 数学分配律验证
  let a = 6
  let b = 4
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值运算
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  let max_share = divide_with_ceil(total_budget, team_members)
  
  assert_eq(1667, max_share)
  
  // 验证总分配
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
  
  // 计算溢出量
  let overflow = subtract(total_allocated, total_budget)
  assert_eq(2, overflow)
}

test "comprehensive_workflow_validation" {
  // 综合工作流验证测试 - 项目管理场景
  let project_tasks = 87
  let team_size = 5
  let tasks_per_person_per_day = 3
  let work_days = 10
  
  // 计算团队日产能
  let daily_capacity = multiply(team_size, tasks_per_person_per_day)
  assert_eq(15, daily_capacity)
  
  // 计算总产能
  let total_capacity = multiply(daily_capacity, work_days)
  assert_eq(150, total_capacity)
  
  // 计算需要的额外天数
  let remaining_tasks = subtract(project_tasks, total_capacity)
  let extra_days = divide_with_ceil(remaining_tasks, daily_capacity)
  assert_eq(5, extra_days)
  
  // 计算总工期
  let total_days = add(work_days, extra_days)
  assert_eq(15, total_days)
}