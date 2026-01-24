// 新增的高质量 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(10, add(3, 7))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "multiply_function_negative_numbers" {
  // 测试负数乘法
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(-10, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  // 场景：计算活动需要的桌子数量
  let guests = 35
  let chairs_per_table = 6
  let tables_needed = divide_with_ceil(guests, chairs_per_table)
  assert_eq(6, tables_needed)
  
  // 验证总座位数足够
  let total_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(36, total_chairs)
}

test "mathematical_properties_commutative" {
  // 验证数学交换律
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_calculation_inventory" {
  // 业务计算：库存管理场景
  let products = 125
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(products, box_capacity)
  assert_eq(9, boxes_needed)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, products)
  assert_eq(10, remaining_space)
}

test "edge_cases_zero_values" {
  // 边缘情况：零值处理
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线：项目资源规划
  let team_members = 7
  let tasks_per_member = 6
  let sprint_capacity = 20
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(42, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)
}

test "string_processing_special_characters" {
  // 字符串处理：特殊字符测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, line\nbreak!", greet("line\nbreak"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}