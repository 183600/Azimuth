// Azimuth 项目新增标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，涵盖核心功能和边界情况

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法恒等式：x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式：x * 1 = x, x * 0 = 0
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式：x - x = 0, x - 0 = x
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "divide_with_ceil_comprehensive_coverage" {
  // 向上取整除法综合覆盖测试
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  let a = 5
  let b = 3
  let c = 2
  
  // 分配律：a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 结合律：(a + b) + c = a + (b + c)
  let associative_left = add(add(a, b), c)
  let associative_right = add(a, add(b, c))
  assert_eq(associative_left, associative_right)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  
  // Unicode 和表情符号
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  
  // 数字和特殊符号
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "business_inventory_management" {
  // 业务库存管理场景测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 验证空间充足性
  assert_true(total_capacity >= total_products)
}

test "negative_number_operations_comprehensive" {
  // 负数运算综合测试
  // 加法负数测试
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(0, add(-10, 10))
  
  // 乘法负数测试
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
  
  // 减法负数测试
  assert_eq(-2, subtract(-5, -3))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(8, subtract(3, -5))
}

test "large_number_operations" {
  // 大数运算测试
  let large_num1 = 100000
  let large_num2 = 50000
  
  // 大数加法
  assert_eq(150000, add(large_num1, large_num2))
  
  // 大数乘法（调整数值以避免溢出）
  assert_eq(100000000, multiply(10000, 10000))
  
  // 大数除法
  assert_eq(3, divide_with_ceil(large_num1, 40000))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
  
  // 大数减法
  assert_eq(50000, subtract(large_num1, large_num2))
}

test "boolean_assertion_functionality" {
  // 布尔断言功能测试
  let true_condition = add(2, 3) == 5
  let false_condition = add(2, 3) == 6
  
  // 测试 assert_true 和 assert_false
  assert_true(true_condition)
  assert_false(false_condition)
  
  // 复杂布尔表达式
  assert_true(multiply(4, 5) > add(10, 8))
  assert_false(divide_with_ceil(10, 3) < 3)
  
  // 边界布尔条件
  assert_true(divide_with_ceil(0, 5) == 0)
  assert_false(divide_with_ceil(5, 0) != 0)
}

test "project_resource_allocation" {
  // 项目资源分配场景测试
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证总任务数
  assert_eq(48, total_tasks)
  
  // 验证需要的冲刺数
  assert_eq(4, sprints_needed)
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(60, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, remaining_capacity)
  
  // 验证容量充足性
  assert_true(total_capacity >= total_tasks)
}

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  // 零值运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 单位运算
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, subtract(1, 1))
  
  // 极值比较
  assert_true(add(1000000, 1000000) > multiply(1000, 1000))
  assert_false(divide_with_ceil(10, 3) == divide_with_ceil(9, 3))
}