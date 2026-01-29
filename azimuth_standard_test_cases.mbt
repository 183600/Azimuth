// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法编写的高质量测试用例

///|
test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(5, @azimuth.add(2, 3))
  assert_eq(0, @azimuth.add(0, 0))
  assert_eq(-5, @azimuth.add(-2, -3))
  assert_eq(10, @azimuth.add(15, -5))
}

///|
test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(6, @azimuth.multiply(2, 3))
  assert_eq(0, @azimuth.multiply(5, 0))
  assert_eq(-15, @azimuth.multiply(-3, 5))
  assert_eq(25, @azimuth.multiply(-5, -5))
}

///|
test "greet_function_basic" {
  // 测试问候函数功能
  assert_eq_string("Hello, World!", @azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
}

///|
test "divide_with_ceil_function" {
  // 测试向上取整除法功能
  assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

///|
test "subtract_function_basic" {
  // 测试减法功能
  assert_eq(2, @azimuth.subtract(5, 3))
  assert_eq(-2, @azimuth.subtract(3, 5))
  assert_eq(0, @azimuth.subtract(5, 5))
  assert_eq(10, @azimuth.subtract(-5, -15))
}

///|
test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 零元素性质
  assert_eq(x, @azimuth.add(x, 0))
  assert_eq(0, @azimuth.multiply(x, 0))
}

///|
test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 项目资源分配计算
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
}

///|
test "boundary_values_and_edge_cases" {
  // 测试边界值和边缘情况
  assert_eq(@azimuth.add(@azimuth.subtract(10, 5), @azimuth.multiply(2, 3)), 11)
  assert_eq(@azimuth.divide_with_ceil(@azimuth.multiply(3, 4), 5), 3)
  assert_eq(@azimuth.divide_with_ceil(1, 1), 1)
  assert_eq(@azimuth.divide_with_ceil(0, 100), 0)
}

///|
test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符支持
  assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  assert_eq_string("Hello, café!", @azimuth.greet("café"))
  assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

///|
test "business_logic_packaging" {
  // 测试业务逻辑：包装计算
  let items = 47
  let capacity_per_box = 12
  
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity_per_box)
  let total_capacity = @azimuth.multiply(boxes_needed, capacity_per_box)
  let remaining_space = @azimuth.subtract(total_capacity, items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, remaining_space)
}