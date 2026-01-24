// 10个高质量 MoonBit 测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边缘情况
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_unicode" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_addition" {
  // 加法交换律
  assert_eq(add(5, 7), add(7, 5))
  assert_eq(add(-3, 8), add(8, -3))
  assert_eq(add(0, 10), add(10, 0))
}

test "mathematical_properties_multiplication" {
  // 乘法交换律
  assert_eq(multiply(4, 6), multiply(6, 4))
  assert_eq(multiply(-2, 5), multiply(5, -2))
  assert_eq(multiply(1, 15), multiply(15, 1))
}

test "complex_calculation_scenario" {
  // 复杂计算场景
  let base = 100
  let multiplier = 3
  let addition = 25
  
  let step1 = multiply(base, multiplier)
  let step2 = add(step1, addition)
  let result = divide_with_ceil(step2, 7)
  
  assert_eq(65, result)
}

test "real_world_application_packaging" {
  // 现实应用：包装计算
  let items_per_box = 12
  let total_items = 47
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  let total_weight_per_item = 2
  let total_weight = multiply(total_items, total_weight_per_item)
  assert_eq(94, total_weight)
}

test "real_world_application_transportation" {
  // 现实应用：运输计算
  let distance = 150
  let fuel_efficiency = 12
  let fuel_needed = divide_with_ceil(distance, fuel_efficiency)
  
  assert_eq(13, fuel_needed)
  
  let fuel_price = 5
  let total_cost = multiply(fuel_needed, fuel_price)
  assert_eq(65, total_cost)
}

test "real_world_application_event_planning" {
  // 现实应用：活动规划
  let guests = 85
  let seats_per_table = 8
  let tables_needed = divide_with_ceil(guests, seats_per_table)
  
  assert_eq(11, tables_needed)
  
  let additional_guests = 15
  let total_guests = add(guests, additional_guests)
  let total_tables = divide_with_ceil(total_guests, seats_per_table)
  
  assert_eq(13, total_tables)
  
  let greeting_message = greet("活动参与者")
  assert_eq_string("Hello, 活动参与者!", greeting_message)
}