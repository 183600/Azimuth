// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(-10, add(-15, 5))
}

test "multiply_function_identity_property" {
  // 测试乘法单位元性质
  let x = 42
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  assert_eq(-42, multiply(-42, 1))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  assert_eq(4, divide_with_ceil(10, 3))  // 3.33 -> 4
  assert_eq(3, divide_with_ceil(6, 2))   // 3 -> 3
  assert_eq(7, divide_with_ceil(20, 3))  // 6.66 -> 7
  assert_eq(0, divide_with_ceil(5, 0))   // 除零保护
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 89
  let quantity = 4
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(371, total_before_discount)
  assert_eq(351, final_total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-14, 3))
}

test "mathematical_distributive_property" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 5 * (8 + 12) = 5 * 8 + 5 * 12 = 100
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 6
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let work_hours = multiply(days_needed, 8)
  
  assert_eq(6, days_needed)  // ceil(35/6) = 6
  assert_eq(48, work_hours)  // 6 * 8 = 48
}

test "string_and_math_integration" {
  // 字符串和数学函数集成测试
  let team_size = 8
  let project_days = 12
  let total_hours = multiply(team_size, multiply(project_days, 8))
  let team_name = "开发团队"
  let greeting = greet(team_name)
  
  assert_eq(768, total_hours)  // 8 * 12 * 8 = 768
  assert_eq_string("Hello, 开发团队!", greeting)
}