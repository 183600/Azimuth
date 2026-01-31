// 标准 MoonBit 综合测试用例
// 针对 azimuth 库核心功能的高质量测试

test "comprehensive_arithmetic_operations" {
  // 综合测试基本算术运算
  azimuth::assert_eq(15, azimuth::add(7, 8))
  azimuth::assert_eq(-3, azimuth::add(-10, 7))
  azimuth::assert_eq(0, azimuth::add(5, -5))
  
  azimuth::assert_eq(42, azimuth::multiply(6, 7))
  azimuth::assert_eq(-24, azimuth::multiply(-4, 6))
  azimuth::assert_eq(0, azimuth::multiply(0, 123))
  
  azimuth::assert_eq(10, azimuth::subtract(25, 15))
  azimuth::assert_eq(-20, azimuth::subtract(5, 25))
  azimuth::assert_eq(0, azimuth::subtract(100, 100))
}

test "comprehensive_divide_with_ceil" {
  // 全面测试向上取整除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-10, -3))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(4, 2))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(8, 3))
}

test "comprehensive_greet_function" {
  // 全面测试问候函数
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
  azimuth::assert_eq_string("Hello, Test!", azimuth::greet("Test"))
}

test "comprehensive_mathematical_properties" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 加法单位元
  azimuth::assert_eq(a, azimuth::add(a, 0))
  
  // 乘法单位元
  azimuth::assert_eq(a, azimuth::multiply(a, 1))
  
  // 减法性质
  azimuth::assert_eq(0, azimuth::subtract(a, a))
}

test "comprehensive_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 12
  let discount = 50
  let tax_rate = 8
  
  let subtotal = azimuth::multiply(unit_price, quantity)
  let discounted_total = azimuth::add(subtotal, -discount)
  let tax_amount = azimuth::divide_with_ceil(azimuth::multiply(discounted_total, tax_rate), 100)
  let final_total = azimuth::add(discounted_total, tax_amount)
  
  azimuth::assert_eq(540, subtotal)
  azimuth::assert_eq(490, discounted_total)
  azimuth::assert_eq(40, tax_amount)
  azimuth::assert_eq(530, final_total)
}

test "comprehensive_large_number_operations" {
  // 大数精度测试
  let large_num = 100000
  azimuth::assert_eq(200000, azimuth::add(large_num, large_num))
  azimuth::assert_eq(10000000000, azimuth::multiply(large_num, large_num))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(large_num, large_num))
  azimuth::assert_eq(0, azimuth::subtract(large_num, large_num))
}

test "comprehensive_negative_operations" {
  // 负数运算测试
  azimuth::assert_eq(-15, azimuth::add(-7, -8))
  azimuth::assert_eq(3, azimuth::add(-10, 13))
  azimuth::assert_eq(24, azimuth::multiply(-4, -6))
  azimuth::assert_eq(-18, azimuth::multiply(3, -6))
  azimuth::assert_eq(-5, azimuth::subtract(-10, -5))
  azimuth::assert_eq(-15, azimuth::subtract(5, 20))
  azimuth::assert_eq(-4, azimuth::divide_with_ceil(-11, 3))
}

test "comprehensive_packaging_scenario" {
  // 实际包装场景计算
  let product_count = 157
  let box_capacity = 25
  let boxes_needed = azimuth::divide_with_ceil(product_count, box_capacity)
  
  let remaining_items = azimuth::subtract(product_count, azimuth::multiply(boxes_needed - 1, box_capacity))
  
  azimuth::assert_eq(7, boxes_needed)
  azimuth::assert_eq(7, remaining_items)
}

test "comprehensive_string_processing" {
  // 字符串处理边界情况
  azimuth::assert_eq_string("Hello, A!", azimuth::greet("A"))
  azimuth::assert_eq_string("Hello,   !", azimuth::greet("   "))
  azimuth::assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
  azimuth::assert_eq_string("Hello, αβγδε!", azimuth::greet("αβγδε"))
  azimuth::assert_eq_string("Hello, @#$%^&*()!", azimuth::greet("@#$%^&*()"))
}

test "comprehensive_algorithm_efficiency" {
  // 算法效率模拟测试
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 10
  
  let optimized_operations = azimuth::multiply(base_operations, optimization_factor)
  let overhead = azimuth::divide_with_ceil(azimuth::multiply(optimized_operations, overhead_percentage), 100)
  let effective_operations = azimuth::add(optimized_operations, -overhead)
  
  azimuth::assert_eq(3000, optimized_operations)
  azimuth::assert_eq(300, overhead)
  azimuth::assert_eq(2700, effective_operations)
}