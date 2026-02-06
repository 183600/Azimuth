// Azimuth 项目增强测试用例
// 标准 MoonBit 测试语法，包含 8 个高质量测试用例

test "mathematical_associative_properties" {
  // 测试数学结合律
  let a = 2
  let b = 3
  let c = 4
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试：项目资源规划
  let team_size = 8
  let story_points_per_person = 13
  let sprint_capacity = 40
  let project_story_points = multiply(team_size, story_points_per_person)
  
  let sprints_needed = divide_with_ceil(project_story_points, sprint_capacity)
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let buffer_capacity = subtract(total_capacity, project_story_points)
  
  assert_eq(104, project_story_points)  // 8 * 13 = 104
  assert_eq(3, sprints_needed)         // ceil(104/40) = 3
  assert_eq(120, total_capacity)       // 3 * 40 = 120
  assert_eq(16, buffer_capacity)       // 120 - 104 = 16
}

test "advanced_string_processing" {
  // 高级字符串处理测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 空格!", greet(" 空格"))
  assert_eq_string("Hello, 数字123!", greet("数字123"))
  assert_eq_string("Hello, 特殊@#$%!", greet("特殊@#$%"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mixed_arithmetic_operations" {
  // 混合算术运算测试
  let base = 10
  let multiplier = 3
  let divisor = 4
  let offset = 5
  
  let step1 = multiply(base, multiplier)     // 10 * 3 = 30
  let step2 = divide_with_ceil(step1, divisor)  // ceil(30/4) = 8
  let step3 = add(step2, offset)             // 8 + 5 = 13
  let step4 = subtract(step3, base)          // 13 - 10 = 3
  
  assert_eq(3, step4)
}

test "edge_case_boundary_conditions" {
  // 边界条件和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(100, multiply(100, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(10, add(-10, 20))
  
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-25, multiply(5, -5))
  
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(-3, divide_with_ceil(10, -4))
  assert_eq(3, divide_with_ceil(-10, -4))
  
  assert_eq(-15, subtract(-10, 5))
  assert_eq(15, subtract(10, -5))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_tasks = 67
  let team_members = 5
  let work_days = 10
  let tasks_per_member_per_day = 2
  
  let daily_capacity = multiply(team_members, tasks_per_member_per_day)
  let total_period_capacity = multiply(daily_capacity, work_days)
  let additional_days_needed = divide_with_ceil(
    subtract(total_tasks, total_period_capacity), 
    daily_capacity
  )
  
  assert_eq(10, daily_capacity)              // 5 * 2 = 10
  assert_eq(100, total_period_capacity)      // 10 * 10 = 100
  assert_eq(0, additional_days_needed)       // ceil((67-100)/10) = 0
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6 * (4 + 3) = 6 * 4 + 6 * 3 = 42
}