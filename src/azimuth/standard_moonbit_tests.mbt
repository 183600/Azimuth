// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的综合测试

test "arithmetic_operations_comprehensive" {
  // 综合测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(-10, 7))
  assert_eq(0, add(5, -5))
  
  assert_eq(42, multiply(6, 7))
  assert_eq(-24, multiply(-4, 6))
  assert_eq(0, multiply(0, 123))
  
  assert_eq(10, subtract(25, 15))
  assert_eq(-20, subtract(5, 25))
  assert_eq(0, subtract(100, 100))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(a, add(a, 0))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
  
  // 减法性质
  assert_eq(0, subtract(a, a))
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 12
  let discount = 50
  let tax_rate = 8
  
  let subtotal = multiply(unit_price, quantity)
  let discounted_total = add(subtotal, -discount)
  let tax_amount = divide_with_ceil(multiply(discounted_total, tax_rate), 100)
  let final_total = add(discounted_total, tax_amount)
  
  assert_eq(540, subtotal)
  assert_eq(490, discounted_total)
  assert_eq(40, tax_amount)
  assert_eq(530, final_total)
}

test "large_number_precision" {
  // 大数精度测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(0, subtract(large_num, large_num))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-15, add(-7, -8))
  assert_eq(3, add(-10, 13))
  assert_eq(24, multiply(-4, -6))
  assert_eq(-18, multiply(3, -6))
  assert_eq(-5, subtract(-10, -5))
  assert_eq(-15, subtract(5, 20))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "real_world_packaging_scenario" {
  // 实际包装场景计算
  let product_count = 157
  let box_capacity = 25
  let boxes_needed = divide_with_ceil(product_count, box_capacity)
  
  let remaining_items = subtract(product_count, multiply(boxes_needed - 1, box_capacity))
  
  assert_eq(7, boxes_needed)
  assert_eq(7, remaining_items)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟测试
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 10
  
  let optimized_operations = multiply(base_operations, optimization_factor)
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  let effective_operations = add(optimized_operations, -overhead)
  
  assert_eq(3000, optimized_operations)
  assert_eq(300, overhead)
  assert_eq(2700, effective_operations)
}