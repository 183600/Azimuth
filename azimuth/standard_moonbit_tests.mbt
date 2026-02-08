// MoonBit 标准测试用例 - 高质量测试集合
// 为 Azimuth 项目添加的 10 个标准测试用例

test "standard_addition_commutative" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(0, add(-5, 5))
}

test "standard_multiplication_properties" {
  // 测试乘法性质
  assert_eq(0, multiply(0, 123))
  assert_eq(123, multiply(123, 1))
  assert_eq(-123, multiply(123, -1))
  assert_eq(36, multiply(6, 6))
}

test "standard_divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(7, divide_with_ceil(40, 6))
}

test "standard_greet_function_international" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "standard_subtraction_properties" {
  // 测试减法性质
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(20, 10))
  assert_eq(-10, subtract(10, 20))
  assert_eq(20, subtract(10, -10))
}

test "standard_complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算项目资源需求
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 12
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(40, total_tasks)
  assert_eq(4, sprints_needed)
}

test "standard_mathematical_distributive" {
  // 测试数学分配律
  let a = 5
  let b = 3
  let c = 2
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "standard_inventory_management" {
  // 测试库存管理场景
  let total_items = 58
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(60, total_capacity)
  assert_eq(2, remaining_space)
}

test "standard_negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, subtract(-5, -5))
}

test "standard_edge_boundary_cases" {
  // 测试边界和极端情况
  // 大数运算
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(10000, divide_with_ceil(10000, 1))
  assert_eq(1, divide_with_ceil(9999, 10000))
  
  // 特殊字符串处理
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}