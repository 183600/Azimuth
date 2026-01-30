// 标准 MoonBit 测试用例 - 新增8个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_operations" {
  // 测试加法函数的基本操作
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(100, add(45, 55))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(3, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边缘情况
  assert_eq(-2, subtract(3, 5))
  assert_eq(8, subtract(3, -5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
  assert_eq(1000, subtract(1500, 500))
}

test "divide_with_ceil_function_precision" {
  // 测试向上取整除法函数的精度
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源分配
  let team_members = 6
  let tasks_per_person = 8
  let total_tasks = multiply(team_members, tasks_per_person)
  assert_eq(48, total_tasks)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(4, sprints_needed)
  
  let actual_capacity = multiply(sprints_needed, sprint_capacity)
  let buffer_capacity = subtract(actual_capacity, total_tasks)
  assert_eq(12, buffer_capacity)
}