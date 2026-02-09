// 新增的 MoonBit 测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含9个高质量测试用例

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 17
  let b = 29
  assert_eq(add(a, b), add(b, a))
  assert_eq(46, add(17, 29))
  assert_eq(46, add(29, 17))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边缘情况
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(999999, 1))
  assert_eq(4, divide_with_ceil(13, 4))
  assert_eq(4, divide_with_ceil(14, 4))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(4, divide_with_ceil(16, 4))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  // 场景：计算项目完成所需的时间和资源
  let team_size = 7
  let story_points = 120
  let velocity_per_sprint = 20
  let sprints_needed = divide_with_ceil(story_points, velocity_per_sprint)
  
  assert_eq(6, sprints_needed)
  
  // 计算总人天（每个sprint 2周，10个工作日）
  let total_work_days = multiply(sprints_needed, multiply(team_size, 10))
  assert_eq(420, total_work_days)
}

test "mathematical_boundary_conditions" {
  // 测试数学边界条件
  // 测试大数运算
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  
  // 测试零的性质
  assert_eq(0, multiply(123456, 0))
  assert_eq(123456, add(123456, 0))
  assert_eq(123456, subtract(123456, 0))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-1, divide_with_ceil(-1, 3))
  assert_eq(0, divide_with_ceil(0, -3))
  assert_eq(1, divide_with_ceil(-3, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "subtract_function_properties" {
  // 测试减法函数的性质
  let a = 42
  let b = 17
  
  // 测试减法的基本性质
  assert_eq(25, subtract(a, b))
  assert_eq(-25, subtract(b, a))
  assert_eq(0, subtract(a, a))
  
  // 测试与加法的关系
  assert_eq(a, add(subtract(a, b), b))
  assert_eq(b, subtract(a, subtract(a, b)))
}

test "real_world_business_scenario" {
  // 测试真实世界业务场景
  // 场景：电商订单包装和配送计算
  let total_orders = 157
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_orders, items_per_package)
  
  assert_eq(14, packages_needed)
  
  // 计算总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_orders)
  assert_eq(11, remaining_space)
  
  // 计算配送车辆需求（每辆车最多装50个包裹）
  let vehicles_needed = divide_with_ceil(packages_needed, 50)
  assert_eq(1, vehicles_needed)
}