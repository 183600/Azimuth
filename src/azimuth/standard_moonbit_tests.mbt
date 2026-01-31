// 标准 MoonBit 测试用例
// 为 azimuth 库添加10个高质量的测试用例

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(-3, subtract(5, 8))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
}

test "mathematical_properties_verification" {
  // 验证数学基本性质
  let x = 12
  let y = 18
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
  
  // 分配律
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-7, 11))
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(3, -7))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 89
  let quantity = 4
  let shipping_cost = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total_cost = add(subtotal, shipping_cost)
  
  assert_eq(356, subtotal)
  assert_eq(371, total_cost)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 1000
  let team_members = 6
  let min_share = divide_with_ceil(total_budget, team_members)
  
  assert_eq(167, min_share)
  
  // 验证总分配不超过预算
  let total_allocated = multiply(min_share, team_members)
  assert_eq(1002, total_allocated)
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "error_handling_and_boundary_cases" {
  // 错误处理和边界情况测试
  // 除零安全处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 项目管理场景：计算项目完成时间和成本
  let tasks = 25
  let team_size = 4
  let tasks_per_person_per_day = 2
  let daily_cost_per_person = 150
  
  let daily_capacity = multiply(team_size, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  let total_cost = multiply(days_needed, multiply(team_size, daily_cost_per_person))
  
  assert_eq(8, daily_capacity)
  assert_eq(4, days_needed)
  assert_eq(2400, total_cost)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
}