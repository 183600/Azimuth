// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法编写的高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(-2, subtract(3, 5))
}

test "zero_element_properties" {
  // 零元素性质测试
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(5, subtract(5, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-1, divide_with_ceil(-7, 4))
  assert_eq(2, subtract(-3, -5))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "division_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 场景：计算装盒需求
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
  
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "mathematical_distributive_property" {
  // 数学分配律性质测试
  let a = 6
  let b = 4
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "business_logic_calculation" {
  // 业务逻辑计算测试
  // 场景：项目资源分配
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(3, sprints_needed)
}

test "input_validation_and_error_handling" {
  // 输入验证和错误处理测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 空字符串处理
  assert_eq_string("Hello, !", greet(""))
}