// Azimuth 额外标准测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "azimuth_add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(42, add(20, 22))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-15, 5))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-60, -40))
}

test "azimuth_multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(144, multiply(12, 12))
  assert_eq(0, multiply(12345, 0))
  assert_eq(-49, multiply(-7, 7))
  assert_eq(49, multiply(-7, -7))
  assert_eq(1000000, multiply(1000, 1000))
}

test "azimuth_divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 42))
  assert_eq(0, divide_with_ceil(99, 0))
}

test "azimuth_divide_with_ceil_negative_comprehensive" {
  // 向上取整除法负数综合测试
  assert_eq(-3, divide_with_ceil(-11, 4))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(3, divide_with_ceil(-11, -4))
  assert_eq(-1, divide_with_ceil(-5, 10))
  assert_eq(1, divide_with_ceil(-5, -5))
}

test "azimuth_greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, émoji 🚀!", greet("émoji 🚀"))
}

test "azimuth_subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(25, subtract(50, 25))
  assert_eq(-25, subtract(25, 50))
  assert_eq(0, subtract(75, 75))
  assert_eq(100, subtract(50, -50))
  assert_eq(-100, subtract(-50, 50))
}

test "azimuth_business_logistics_scenario" {
  // 业务物流场景测试
  let total_packages = 87
  let truck_capacity = 20
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(5, trucks_needed)
  
  // 验证总运输能力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  let unused_capacity = subtract(total_capacity, total_packages)
  assert_eq(13, unused_capacity)
}

test "azimuth_complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：计算项目资源分配
  let team_members = 7
  let hours_per_day = 8
  let working_days = 15
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  
  let tasks_per_hour = 2
  let total_tasks = multiply(total_hours, tasks_per_hour)
  
  let tasks_per_sprint = 50
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(840, total_hours)  // 7 * 8 * 15 = 840
  assert_eq(1680, total_tasks)  // 840 * 2 = 1680
  assert_eq(34, sprints_needed)  // ceil(1680/50) = 34
}