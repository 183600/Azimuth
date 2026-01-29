// 标准 MoonBit 测试用例 - 为 azimuth 包创建
// 包含 8 个高质量测试用例，覆盖主要功能和边缘情况

test "arithmetics_add_edge_cases" {
  // 测试加法函数的边缘情况
  assert_eq(0, add(0, 0))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-50, 50))
  assert_eq(2147483647, add(2147483646, 1))
}

test "arithmetics_multiply_properties" {
  // 测试乘法函数的数学性质
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "arithmetics_subtract_comprehensive" {
  // 测试减法函数的综合情况
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(15, 5))
  assert_eq(-10, subtract(5, 15))
  assert_eq(2147483647, subtract(2147483647, 0))
  assert_eq(-2147483648, subtract(-2147483647, 1))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1000, divide_with_ceil(1999, 2))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "business_logic_inventory_calculation" {
  // 测试库存计算业务逻辑
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 42
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(59, add(42, 17))
  assert_eq(714, multiply(42, 17))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(45, total_capacity)
  
  // 验证剩余能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(5, remaining_capacity)
}