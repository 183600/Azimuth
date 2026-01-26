// 新增的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_commutativity" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
}

test "multiply_function_distributive" {
  // 测试乘法分配律
  let a = 3
  let b = 4
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "mathematical_series_sum" {
  // 测试数学级数求和
  let n = 10
  let sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, sum)
}

test "inventory_packaging_optimization" {
  // 测试库存包装优化
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  let remaining_items = add(total_items, -multiply(boxes_needed - 1, box_capacity))
  
  assert_eq(4, boxes_needed)
  assert_eq(11, remaining_items)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(5, days_needed)
  assert_eq(40, total_hours)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
}