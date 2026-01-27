// 标准 MoonBit 测试用例 - 为 azimuth 库创建
// 包含8个高质量测试用例，覆盖主要功能

test "add_function_various_scenarios" {
  // 测试加法函数的各种场景
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-3, 8))
  assert_eq(2000, add(1000, 1000))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(42, multiply(6, 7))
  assert_eq(0, multiply(0, 123))
  assert_eq(-24, multiply(-3, 8))
  assert_eq(24, multiply(-3, -8))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_packaging_logic" {
  // 测试向上取整除法的包装逻辑
  let items = 37
  let box_capacity = 10
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(40, total_capacity)
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边缘情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(20, 5))
  assert_eq(-1000, subtract(0, 1000))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算项目资源需求
  let team_size = 12
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  let sprint_capacity = 25
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(96, total_tasks)
  assert_eq(4, sprints_needed)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 7
  
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

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}