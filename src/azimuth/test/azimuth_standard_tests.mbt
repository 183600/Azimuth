// Azimuth 库的标准 MoonBit 测试用例
// 包含核心功能的全面测试，不超过10个测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 2)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2)))
}

test "edge_case_boundary_values" {
  // 测试边界值情况
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total_cost = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax_amount)
  @azimuth.assert_eq(231, total_cost)
}

test "resource_planning_scenarios" {
  // 测试资源规划场景
  let total_tasks = 47
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  let work_days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = @azimuth.divide_with_ceil(work_days_needed, work_days_per_week)
  
  @azimuth.assert_eq(6, work_days_needed)
  @azimuth.assert_eq(2, weeks_needed)
}

test "string_processing_with_unicode" {
  // 测试字符串处理和Unicode支持
  @azimuth.assert_eq_string("Hello, 🚀 MoonBit!", @azimuth.greet("🚀 MoonBit"))
  @azimuth.assert_eq_string("Hello, María García!", @azimuth.greet("María García"))
  @azimuth.assert_eq_string("Hello, αβγδε!", @azimuth.greet("αβγδε"))
  @azimuth.assert_eq_string("Hello,   spaced   name!", @azimuth.greet("   spaced   name"))
}

test "error_handling_robustness" {
  // 测试错误处理健壮性
  // 除零安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  
  // 极值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 2147483647))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}