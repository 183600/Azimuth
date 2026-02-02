// Azimuth 项目标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "arith_add_comprehensive" {
  // 加法综合测试
  assert_eq(0, add(0, 0))
  assert_eq(10, add(6, 4))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-3, 8))
  assert_eq(10000, add(5000, 5000))
}

test "arith_multiply_edge_cases" {
  // 乘法边界情况测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(25, multiply(-5, -5))
}

test "divide_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_ceil_negative_comprehensive" {
  // 向上取整除法负数综合测试
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(2, divide_with_ceil(-3, -2))
  assert_eq(-1, divide_with_ceil(1, -2))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "greet_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, space !", greet("space "))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(0, subtract(5, 5))
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(15, subtract(5, -10))
  assert_eq(-15, subtract(-5, 10))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(18, add(7, 11))
  assert_eq(77, multiply(7, 11))
}

test "business_packaging_logic" {
  // 业务包装逻辑测试
  let total_items = 43
  let items_per_package = 10
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(7, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目管理计算
  let team_size = 6
  let tasks_per_member = 9
  let total_tasks = multiply(team_size, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(54, total_tasks)  // 6 * 9 = 54
  assert_eq(3, sprints_needed)  // ceil(54/20) = 3
  
  // 验证最后一个冲刺的任务数
  let full_sprints_tasks = multiply(2, sprint_capacity)
  let last_sprint_tasks = subtract(total_tasks, full_sprints_tasks)
  assert_eq(14, last_sprint_tasks)  // 54 - 40 = 14
}

test "performance_large_numbers" {
  // 大数性能测试
  let large_num1 = 10000
  let large_num2 = 20000
  
  // 大数加法
  assert_eq(30000, add(large_num1, large_num2))
  
  // 大数乘法
  assert_eq(200000000, multiply(large_num1, large_num2))
  
  // 大数除法
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
  assert_eq(2, divide_with_ceil(large_num2, large_num1))
  
  // 大数减法
  assert_eq(10000, subtract(large_num2, large_num1))
}