// Azimuth 项目标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(7, 5))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(3, divide_with_ceil(12, 4))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 17
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "negative_numbers_handling" {
  // 负数处理测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-8, add(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-8, subtract(-3, 5))
  assert_eq(2, subtract(5, -3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(100000000, multiply(10000, 10000))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello,   !", greet("  "))
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
  // 场景：项目资源计算
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 计算总工作量
  let total_work = multiply(sprints_needed, sprint_capacity)
  assert_eq(60, total_work)  // 3 * 20 = 60
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界测试
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "sequential_operations_chain" {
  // 连续操作链测试
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 1)     // 3
  let step5 = add(step4, multiply(2, 3))  // 9
  
  assert_eq(3, step4)
  assert_eq(9, step5)
}

test "simple_real_world_scenario" {
  // 简化的真实世界场景测试
  let total_items = 100
  let package_size = 15
  let packages = divide_with_ceil(total_items, package_size)
  
  assert_eq(7, packages)
  
  let total_capacity = multiply(packages, package_size)
  assert_eq(105, total_capacity)
  
  let unused = subtract(total_capacity, total_items)
  assert_eq(5, unused)
}