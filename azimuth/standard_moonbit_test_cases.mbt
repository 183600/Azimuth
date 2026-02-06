// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "azimuth_standard_addition_comprehensive" {
  // 综合加法测试
  assert_eq(42, add(20, 22))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-15, 5))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "azimuth_standard_multiplication_various" {
  // 各种乘法测试
  assert_eq(36, multiply(6, 6))
  assert_eq(0, multiply(123, 0))
  assert_eq(-45, multiply(-9, 5))
  assert_eq(45, multiply(-9, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "azimuth_standard_divide_ceil_scenarios" {
  // 向上取整除法场景测试
  assert_eq(5, divide_with_ceil(21, 5))
  assert_eq(4, divide_with_ceil(16, 4))
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(8, 0))
}

test "azimuth_standard_divide_ceil_negative_cases" {
  // 向上取整除法负数情况测试
  assert_eq(-4, divide_with_ceil(-18, 5))
  assert_eq(-4, divide_with_ceil(18, -5))
  assert_eq(4, divide_with_ceil(-18, -5))
  assert_eq(-1, divide_with_ceil(-3, 5))
  assert_eq(1, divide_with_ceil(-3, -5))
}

test "azimuth_standard_greet_international" {
  // 国际化问候测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, !", greet(""))
}

test "azimuth_standard_subtract_operations" {
  // 减法运算测试
  assert_eq(15, subtract(25, 10))
  assert_eq(-15, subtract(10, 25))
  assert_eq(0, subtract(20, 20))
  assert_eq(30, subtract(15, -15))
  assert_eq(-30, subtract(-15, 15))
}

test "azimuth_standard_mathematical_laws" {
  // 数学定律验证测试
  let x = 18
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(25, add(18, 7))
  assert_eq(126, multiply(18, 7))
}

test "azimuth_standard_resource_allocation" {
  // 资源分配场景测试
  let total_resources = 89
  let team_capacity = 12
  let teams_needed = divide_with_ceil(total_resources, team_capacity)
  
  assert_eq(8, teams_needed)
  
  // 验证总容量
  let total_capacity = multiply(teams_needed, team_capacity)
  assert_eq(96, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, total_resources)
  assert_eq(7, remaining_capacity)
}

test "azimuth_standard_project_planning" {
  // 项目规划场景测试
  // 场景：计算项目时间和资源需求
  let developers = 9
  let tasks_per_developer = 7
  let total_tasks = multiply(developers, tasks_per_developer)
  
  let sprint_duration = 10
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(63, total_tasks)  // 9 * 7 = 63
  assert_eq(5, sprints_needed)  // ceil(63/15) = 5
  
  // 计算总项目时间
  let total_project_days = multiply(sprints_needed, sprint_duration)
  assert_eq(50, total_project_days)  // 5 * 10 = 50
}

test "azimuth_standard_boundary_comprehensive" {
  // 综合边界情况测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(7, 7))
  assert_eq(1, divide_with_ceil(-7, -7))
  assert_eq(0, divide_with_ceil(0, 7))
  
  // 大数运算
  assert_eq(3000000, add(1500000, 1500000))
  assert_eq(25000000, multiply(5000, 5000))
  
  // 特殊字符串情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, line break!", greet("line break"))
  assert_eq_string("Hello, tab character!", greet("tab character"))
}