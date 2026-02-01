// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_positive" {
  // 测试正数向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_zero_handling" {
  // 测试除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(x, multiply(x, 1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "packaging_calculation_business" {
  // 测试包装计算业务场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

// 新增的测试用例 - 为 Azimuth 项目添加
test "enhanced_addition_cases" {
  // 增强加法测试用例
  assert_eq(8, add(3, 5))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-100, 100))
}

test "enhanced_multiplication_cases" {
  // 增强乘法测试用例
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000, multiply(-100, 10))
  assert_eq(1000, multiply(-100, -10))
  assert_eq(0, multiply(12345, 0))
}

test "international_greet_support" {
  // 国际化问候支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：项目资源管理
  let team_size = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_size, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(60, total_capacity)  // 3 * 20 = 60
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, remaining_capacity)  // 60 - 48 = 12
}