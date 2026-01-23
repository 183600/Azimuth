// Azimuth 库的标准 MoonBit 测试用例
// 包含核心功能的全面测试，不超过10个测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 10))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 2)), add(multiply(a, b), multiply(a, 2)))
}

test "edge_case_boundary_values" {
  // 测试边界值情况
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2147483647, add(2147483646, 1))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_cost = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total_cost)
}

test "resource_planning_scenarios" {
  // 测试资源规划场景
  let total_tasks = 47
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  assert_eq(6, work_days_needed)
  assert_eq(2, weeks_needed)
}

test "string_processing_with_unicode" {
  // 测试字符串处理和Unicode支持
  assert_eq_string("Hello, 🚀 MoonBit!", greet("🚀 MoonBit"))
  assert_eq_string("Hello, María García!", greet("María García"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello,   spaced   name!", greet("   spaced   name"))
}

test "error_handling_robustness" {
  // 测试错误处理健壮性
  // 除零安全处理
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值运算
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, divide_with_ceil(0, 2147483647))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}