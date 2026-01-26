// 标准 MoonBit 测试用例 - 针对 azimuth 库的高质量测试
// 测试覆盖基本功能、边界情况和实际应用场景

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(0, add(100, -100))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(-3, -4))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法的各种情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  assert_eq(0, divide_with_ceil(5, 0))   // 除零保护
  assert_eq(-3, divide_with_ceil(-10, 3)) // -3.33 -> -3
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
}

test "mathematical_properties" {
  // 数学性质验证
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "time_and_resource_calculation" {
  // 时间和资源计算场景
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(5, days_needed)  // ceil(35/8) = 5
  
  // 计算总工作小时数
  let hours_per_day = 8
  let total_hours = multiply(days_needed, hours_per_day)
  assert_eq(40, total_hours)
}