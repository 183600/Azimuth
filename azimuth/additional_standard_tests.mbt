// 额外的标准 MoonBit 测试用例
// 为 azimuth 项目添加的高质量补充测试

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
}

test "complex_inventory_calculation" {
  // 复杂库存计算场景
  let initial_stock = 150
  let sold_items = 47
  let returned_items = 8
  let new_shipment = 75
  
  let current_stock = subtract(subtract(add(initial_stock, returned_items), sold_items), new_shipment)
  assert_eq(86, current_stock)
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  let project_duration = 45
  let team_members = 8
  let work_days_per_week = 5
  
  let total_work_days = divide_with_ceil(project_duration, 7) * work_days_per_week
  let person_days_needed = multiply(total_work_days, team_members)
  assert_eq(320, person_days_needed)
}

test "mathematical_identity_elements" {
  // 测试数学恒等元素
  let x = 42
  
  // 加法恒等元素
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等元素
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 减法恒等元素
  assert_eq(0, subtract(x, x))
}

test "error_handling_division_by_zero" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  let products_per_batch = 24
  let batches_per_day = 3
  let operating_days = 15
  let defect_rate = 2
  
  let total_produced = multiply(products_per_batch, multiply(batches_per_day, operating_days))
  let expected_defects = divide_with_ceil(multiply(total_produced, defect_rate), 100)
  let quality_products = subtract(total_produced, expected_defects)
  
  assert_eq(1080, total_produced)
  assert_eq(22, expected_defects)
  assert_eq(1058, quality_products)
}

test "string_edge_cases_comprehensive" {
  // 字符串边缘情况综合测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
}