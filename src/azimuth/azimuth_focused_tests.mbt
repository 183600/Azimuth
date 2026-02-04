// Azimuth 项目专注测试用例
// 包含 8 个高质量的标准 MoonBit 测试用例
// 覆盖核心功能和边界情况

test "basic_arithmetic_comprehensive" {
  // 基本算术综合测试
  // 测试加法、减法、乘法的各种组合
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(8, 0))
  assert_eq(-12, multiply(-3, 4))
  
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-9, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  // 基本英文问候
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // Unicode 字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  
  // 特殊字符和数字
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 3
  
  // 交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律验证
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律验证
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 恒等式验证
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
}

test "real_world_business_packaging" {
  // 真实世界业务包装场景测试
  // 电商包装计算
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(7, boxes_needed)
  
  // 计算总容量和剩余空间
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, total_products)
  
  assert_eq(168, total_capacity)
  assert_eq(11, remaining_space)
  
  // 验证箱子数量合理性
  assert_true(boxes_needed >= divide_with_ceil(total_products, box_capacity + 1))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 项目资源管理场景
  let team_members = 8
  let tasks_per_member = 6
  let sprint_duration = 2
  let total_tasks = 90
  
  // 计算团队总任务能力
  let team_capacity = multiply(team_members, tasks_per_member)
  assert_eq(48, team_capacity)
  
  // 计算每个冲刺的处理能力
  let sprint_capacity = multiply(team_capacity, sprint_duration)
  assert_eq(96, sprint_capacity)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(1, sprints_needed)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_tasks, 100), sprint_capacity)
  assert_eq(94, utilization)  // floor(9000/96) = 93%
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值处理
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 大数运算（在合理范围内）
  assert_eq(20000, add(10000, 10000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(10001, divide_with_ceil(10000, 1))
  
  // 极值边界
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的全面处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  
  // 极值组合运算
  let max_test = 100000
  let min_test = -100000
  
  assert_eq(0, add(max_test, min_test))
  assert_eq(200000, subtract(max_test, min_test))
  assert_eq(-10000000000, multiply(max_test, min_test))
  
  // 字符串边界情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, space !", greet("space "))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
}