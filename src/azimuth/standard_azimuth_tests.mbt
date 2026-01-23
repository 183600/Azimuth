// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(1, 1))
}

test "divide_with_ceil_advanced" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_laws_verification" {
  // 验证数学定律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_tasks = 47
  let tasks_per_day = 8
  let work_days = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(work_days, 8)
  
  assert_eq(6, work_days)
  assert_eq(48, total_hours)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, <>&\"'!", greet("<>&\"'"))
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, multiply(0, 1000000))
}

test "performance_efficiency_calculation" {
  // 性能效率计算
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  let optimized_operations = multiply(base_operations, optimization_factor)
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  let actual_performance = add(optimized_operations, -overhead)
  
  assert_eq(3000, optimized_operations)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
}