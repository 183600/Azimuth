// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(21, multiply(-7, -3))
  assert_eq(100, multiply(10, 10))
}

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(4, subtract(7, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-10, subtract(3, 13))
  assert_eq(10, subtract(-5, -15))
  assert_eq(100, subtract(150, 50))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(5, divide_with_ceil(25, 5))
  assert_eq(100, divide_with_ceil(999, 10))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(-5, 10))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
}

test "business_logic_packaging" {
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