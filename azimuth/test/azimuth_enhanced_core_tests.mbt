// Azimuth 增强核心测试用例
// 包含10个标准 MoonBit 测试用例，覆盖核心功能

test "enhanced_addition_comprehensive" {
  // 增强加法综合测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  assert_eq(100000, add(50000, 50000))
  assert_eq(-1, add(-5, 4))
  assert_eq(1, add(5, -4))
}

test "enhanced_multiplication_varied_cases" {
  // 增强乘法多样化测试
  assert_eq(42, multiply(6, 7))
  assert_eq(0, multiply(15, 0))
  assert_eq(-24, multiply(-3, 8))
  assert_eq(24, multiply(-3, -8))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(1000, -1000))
}

test "enhanced_divide_with_ceil_precision" {
  // 增强向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(2, divide_with_ceil(1001, 500))
}

test "enhanced_divide_with_ceil_negative_handling" {
  // 增强向上取整除法负数处理测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "enhanced_greet_function_internationalization" {
  // 增强问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, éspanol!", greet("éspanol"))
}

test "enhanced_subtraction_comprehensive" {
  // 增强减法综合测试
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
  assert_eq(100, subtract(50, -50))
  assert_eq(-100, subtract(-50, 50))
}

test "enhanced_mathematical_properties_validation" {
  // 增强数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  let c = 5
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "enhanced_business_packaging_scenario" {
  // 增强业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
  
  // 验证空间利用率
  let utilization = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(88, utilization)  // floor(3500/40) = 87%
}

test "enhanced_complex_workflow_calculation" {
  // 增强复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
  
  // 计算总工作量
  let total_workload = multiply(sprints_needed, sprint_capacity)
  assert_eq(60, total_workload)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_workload, total_tasks)
  assert_eq(12, remaining_capacity)
}

test "enhanced_edge_cases_and_boundary_values" {
  // 增强边界情况和极值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, newline\n!", greet("newline\n"))
  assert_eq_string("Hello, tab\t!", greet("tab\t"))
  
  // 数学恒等式
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(-x, multiply(x, -1))
}