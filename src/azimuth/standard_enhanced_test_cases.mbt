// 标准 MoonBit 增强测试用例
// 针对 azimuth 库核心功能的全面测试

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(0, add(1000000, -1000000))
}

test "multiply_function_zero_properties" {
  // 测试乘法函数的零元素性质
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(1000, 0))
  assert_eq(0, multiply(0, -500))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
}

test "divide_with_ceil_precision_tests" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(5, divide_with_ceil(21, 5))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 12
  let y = 8
  let z = 5
  
  // 测试分配律: a * (b + c) = a * b + a * c
  let distributive_result = multiply(x, add(y, z))
  let manual_result = add(multiply(x, y), multiply(x, z))
  assert_eq(distributive_result, manual_result)
  
  // 测试结合律: (a + b) * c
  let associative_result = multiply(add(x, y), z)
  assert_eq(100, associative_result)
}

test "real_world_inventory_calculation" {
  // 测试实际库存计算场景
  let items_per_box = 24
  let total_items_a = 150
  let total_items_b = 200
  let total_items_c = 75
  
  let boxes_a = divide_with_ceil(total_items_a, items_per_box)
  let boxes_b = divide_with_ceil(total_items_b, items_per_box)
  let boxes_c = divide_with_ceil(total_items_c, items_per_box)
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(7, boxes_a)
  assert_eq(9, boxes_b)
  assert_eq(4, boxes_c)
  assert_eq(20, total_boxes)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  let principal = 1000
  let interest_rate = 8
  let years = 3
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(1080, year1_amount)
  assert_eq(1167, year2_amount)
  assert_eq(1261, year3_amount)
}

test "negative_number_comprehensive_tests" {
  // 测试负数的综合运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(-2, divide_with_ceil(-5, 2))
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, Special@Chars#123!", greet("Special@Chars#123"))
}

test "algorithm_performance_simulation" {
  // 测试算法性能模拟
  let base_operations = 100
  let multiplier = 50
  let iterations = 10
  
  // 模拟算法复杂度计算
  let total_operations = multiply(base_operations, multiplier)
  let per_iteration_cost = divide_with_ceil(total_operations, iterations)
  let optimized_total = add(total_operations, multiply(per_iteration_cost, 2))
  
  assert_eq(5000, total_operations)
  assert_eq(500, per_iteration_cost)
  assert_eq(6000, optimized_total)
}