// 标准 Azimuth 测试套件 - 使用标准 MoonBit 测试语法
// 覆盖 azimuth 库的核心功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
  assert_eq(-1, add(5, -6))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(5, 5))
  assert_eq(-8, multiply(-2, 4))
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 6
  let tax_rate = 10
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(27, tax)
  assert_eq(312, total)
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算场景
  let total_items = 38
  let items_per_box = 8
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let cost_per_box = 12
  let total_packaging_cost = multiply(boxes_needed, cost_per_box)
  
  assert_eq(5, boxes_needed)
  assert_eq(60, total_packaging_cost)
}

test "time_and_resource_management" {
  // 时间和资源管理计算
  let tasks_per_day = 7
  let total_tasks = 45
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  let hours_per_day = 8
  let total_hours = multiply(days_needed, hours_per_day)
  
  assert_eq(7, days_needed)
  assert_eq(56, total_hours)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 测试除零安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "unicode_and_special_characters_handling" {
  // Unicode 和特殊字符处理
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 東京!", greet("東京"))
}