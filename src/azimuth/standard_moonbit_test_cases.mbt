// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的高质量测试

test "add_function_comprehensive" {
  // 测试加法功能的各种情况
  assert_eq(8, add(5, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_comprehensive" {
  // 测试乘法功能的各种情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(10000, multiply(100, 100))
}

test "greet_function_comprehensive" {
  // 测试问候功能的各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "mathematical_properties_verification" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_scenario" {
  // 测试复杂业务场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_cost = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total_cost)
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "time_calculation_scenario" {
  // 测试时间计算场景
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(5, days_needed)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "error_handling_robustness" {
  // 测试错误处理健壮性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, divide_with_ceil(0, 1000000))
}