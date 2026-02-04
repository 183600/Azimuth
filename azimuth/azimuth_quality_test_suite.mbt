// Azimuth 质量测试套件 - 10个高质量标准 MoonBit 测试用例
// 覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 999))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 50))
  assert_eq(0, divide_with_ceil(25, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_function_validation" {
  // 减法函数验证测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源分配
  let team_members = 7
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(42, total_tasks)  // 7 * 6 = 42
  assert_eq(3, sprints_needed)  // ceil(42/20) = 3
  
  // 验证最后一轮的任务数
  let last_sprint_tasks = subtract(total_tasks, multiply(sprints_needed - 1, sprint_capacity))
  assert_eq(2, last_sprint_tasks)  // 42 - 2*20 = 2
}

test "boundary_and_error_conditions" {
  // 边界和错误条件测试
  // 除零边界情况
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  
  // 恒等式验证
  let x = 25
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  
  // 特殊字符串处理
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}