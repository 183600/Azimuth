// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(5, add(2, 3))
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
}

test "multiply_function_properties" {
  // 测试乘法函数的性质
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(1, multiply(1, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "subtract_function_comprehensive" {
  // 测试减法函数的综合情况
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
}

test "mathematical_commutative_law" {
  // 测试数学交换律
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let items = 25
  let boxes = 6
  let boxes_needed = divide_with_ceil(items, boxes)
  assert_eq(5, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, boxes)
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和边缘情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "business_logic_application" {
  // 测试业务逻辑应用
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(3, sprints_needed)
}