// 标准 MoonBit 测试套件 - 新增测试用例
// 为 azimuth 库添加高质量测试覆盖

test "arithmetic_commutative_law" {
  // 测试算术交换律
  let a = 17
  let b = 29
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "division_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(199, 2))
  assert_eq(67, divide_with_ceil(200, 3))
}

test "string_greeting_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
}

test "complex_business_calculation" {
  // 复杂业务计算测试：项目资源分配
  let team_size = 12
  let tasks_per_person = 8
  let sprint_capacity = 25
  
  let total_tasks = multiply(team_size, tasks_per_person)
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(4, sprints_needed)
  assert_eq(96, total_tasks)
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
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

test "error_handling_zero_division" {
  // 除零错误处理测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
}

test "resource_optimization_packaging" {
  // 资源优化包装问题测试
  let items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = subtract(total_capacity, items)
  
  assert_eq(168, total_capacity)
  assert_eq(11, unused_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(3, divide_with_ceil(-7, -3))
}

test "edge_case_boundary_values" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "workflow_sequential_operations" {
  // 工作流连续操作测试
  let initial = 15
  let step1 = multiply(initial, 3)
  let step2 = add(step1, 20)
  let step3 = divide_with_ceil(step2, 7)
  let final_result = subtract(step3, 2)
  
  assert_eq(45, step1)
  assert_eq(65, step2)
  assert_eq(10, step3)
  assert_eq(8, final_result)
}