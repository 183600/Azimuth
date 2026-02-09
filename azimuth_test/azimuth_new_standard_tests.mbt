// Azimuth 新标准测试用例 - 10个高质量 MoonBit 测试
// 测试基本数学运算和字符串处理功能

test "azimuth_add_commutative_property" {
  // 测试加法交换律
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(59, add(a, b))
}

test "azimuth_multiply_distributive_property" {
  // 测试乘法分配律
  let a = 6
  let b = 4
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "azimuth_divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(100, divide_with_ceil(199, 2))
}

test "azimuth_subtract_negative_result" {
  // 测试减法产生负数结果
  assert_eq(-5, subtract(3, 8))
  assert_eq(-10, subtract(0, 10))
  assert_eq(-100, subtract(-50, 50))
}

test "azimuth_greet_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "azimuth_complex_calculation_workflow" {
  // 测试复杂计算工作流
  let items = 47
  let boxes = divide_with_ceil(items, 12)
  let total_capacity = multiply(boxes, 12)
  let remaining = subtract(total_capacity, items)
  
  assert_eq(4, boxes)
  assert_eq(48, total_capacity)
  assert_eq(1, remaining)
}

test "azimuth_zero_element_properties" {
  // 测试零元素性质
  let x = 25
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  assert_eq(x, subtract(x, 0))
}

test "azimuth_large_number_operations" {
  // 测试大数运算
  assert_eq(1000000, add(500000, 500000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(10001, divide_with_ceil(20000001, 2000))
}

test "azimuth_negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-8, subtract(5, 13))
}

test "azimuth_business_scenario_calculation" {
  // 测试业务场景计算
  // 场景：计算项目资源需求
  let team_size = 7
  let tasks_per_member = 6
  let total_tasks = multiply(team_size, tasks_per_member)
  let sprint_capacity = 10
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(42, total_tasks)
  assert_eq(5, sprints_needed)
}