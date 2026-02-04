// Azimuth 项目新增标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "arithmetric_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(42, add(17, 25))
  assert_eq(-8, add(5, -13))
  assert_eq(0, add(100, -100))
  
  assert_eq(144, multiply(12, 12))
  assert_eq(-36, multiply(-9, 4))
  assert_eq(0, multiply(999, 0))
  
  assert_eq(7, divide_with_ceil(20, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 7))
}

test "mathematical_laws_validation" {
  // 数学定律验证测试
  let a = 15
  let b = 7
  let c = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_greeting_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, emoji 🚀!", greet("emoji 🚀"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   leading!", greet("  leading"))
  assert_eq_string("Hello, trailing   !", greet("trailing   "))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试：项目管理场景
  let project_tasks = 125
  let team_members = 8
  let tasks_per_member = divide_with_ceil(project_tasks, team_members)
  
  assert_eq(16, tasks_per_member)  // ceil(125/8) = 16
  
  // 计算实际总任务数
  let actual_total = multiply(team_members, tasks_per_member)
  assert_eq(128, actual_total)
  
  // 计算剩余任务容量
  let remaining_capacity = subtract(actual_total, project_tasks)
  assert_eq(3, remaining_capacity)
}

test "mathematical_zero_properties" {
  // 数学零性质测试
  let x = 42
  
  // 加法零元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 减法零元
  assert_eq(x, subtract(x, 0))
  assert_eq(-x, subtract(0, x))
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-500000, add(-300000, -200000))
  assert_eq(500000, add(-300000, 800000))
  
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
  
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试：计算最优配置
  let total_items = 487
  let container_capacity = 24
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  
  assert_eq(21, containers_needed)  // ceil(487/24) = 21
  
  // 计算总容量
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(504, total_capacity)
  
  // 计算空间利用率
  let utilization = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(97, utilization)  // floor(48700/504) = 96%
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(17, remaining_space)
}

test "sequential_calculation_chain" {
  // 连续计算链测试
  let initial = 5
  
  // 第一步：加法
  let step1 = add(initial, 3)  // 8
  
  // 第二步：乘法
  let step2 = multiply(step1, 4)  // 32
  
  // 第三步：除法
  let step3 = divide_with_ceil(step2, 7)  // 5
  
  // 第四步：减法
  let step4 = subtract(step3, 2)  // 3
  
  // 第五步：再次加法
  let final_result = add(step4, 7)  // 10
  
  assert_eq(10, final_result)
}

test "error_handling_and_boundary_validation" {
  // 错误处理和边界验证测试
  // 极值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(0, multiply(-1, 0))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  
  // 特殊输入处理
  assert_eq_string("Hello, newline\n!", greet("newline\n"))
  assert_eq_string("Hello, tab\t!", greet("tab\t"))
  assert_eq_string("Hello, mixed\t\n!", greet("mixed\t\n"))
}