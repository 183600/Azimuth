// Azimuth 项目额外的标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "add_function_commutative_property" {
  // 测试加法交换律性质
  let a = 17
  let b = 29
  assert_eq(add(a, b), add(b, a))
  assert_eq(46, add(17, 29))
  assert_eq(46, add(29, 17))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律性质
  let a = 6
  let b = 7
  let c = 8
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(90, left_side)  // 6 * (7 + 8) = 6 * 15 = 90
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  assert_eq(3, divide_with_ceil(14, 5))
  assert_eq(5, divide_with_ceil(21, 4))
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(25, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 東京!", greet("東京"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let total_packages = 156
  let truck_capacity = 25
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(7, trucks_needed)
  
  // 验证总运输能力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(19, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算项目资源需求
  let team_members = 9
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 12
  let sprint_capacity = multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)     // 9 * 7 = 63
  assert_eq(24, sprint_capacity) // 2 * 12 = 24
  assert_eq(3, sprints_needed)   // ceil(63/24) = 3
}

test "boundary_and_precision_testing" {
  // 边界和精度测试
  // 除法精度测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000000, divide_with_ceil(1999999, 2))
  assert_eq(666667, divide_with_ceil(2000000, 3))
  
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
}