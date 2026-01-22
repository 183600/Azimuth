// 简洁标准的 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行精确测试

test "basic_arithmetic" {
  // 基础算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "math_properties" {
  // 数学性质验证
  let x = 8
  let y = 12
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 单位元性质
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
}

test "negative_operations" {
  // 负数计算测试
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
}

test "greeting_functionality" {
  // 问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "business_calculation" {
  // 业务计算场景
  let unit_price = 25
  let quantity = 7
  let tax_rate = 8
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(175, subtotal)
  assert_eq(14, tax)
  assert_eq(189, total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 58
  let box_capacity = 15
  
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  let remaining_space = multiply(boxes_needed, box_capacity) - total_items
  
  assert_eq(4, boxes_needed)
  assert_eq(2, remaining_space)
}

test "time_management" {
  // 时间管理计算
  let tasks_per_hour = 6
  let total_tasks = 35
  
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let total_minutes = multiply(hours_needed, 60)
  
  assert_eq(6, hours_needed)
  assert_eq(360, total_minutes)
}

test "complex_expression" {
  // 复杂表达式求值
  let a = 5
  let b = 8
  let c = 3
  
  // 计算 (a + b) * c - a / b
  let result = add(multiply(add(a, b), c), -divide_with_ceil(a, b))
  
  assert_eq(38, result)  // (5 + 8) * 3 - ceil(5/8) = 13 * 3 - 1 = 39 - 1 = 38
}