// 标准 Azimuth 新增测试用例 - 10个高质量的 MoonBit 测试
// 测试覆盖所有主要功能：add, multiply, greet, divide_with_ceil

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(-2, add(-5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(0, 100))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_variations" {
  // 问候函数变化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "combined_operations" {
  // 组合运算测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_calculation" {
  // 包装计算测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(999999, add(500000, 499999))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "comprehensive_error_boundary" {
  // 综合错误边界测试
  // 测试极值和异常情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000000000, multiply(big_num, big_num))
  
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
}

// 新增的10个高质量测试用例
test "enhanced_add_edge_cases" {
  // 测试加法的边界情况
  assert_eq(1000, add(999, 1))
  assert_eq(-1000, add(-999, -1))
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(0, 1))
}

test "enhanced_multiply_properties" {
  // 测试乘法的数学性质
  let x = 12
  let y = 15
  
  // 交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 与1相乘
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(1, y))
  
  // 与0相乘
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, y))
}

test "enhanced_divide_precision" {
  // 测试除法的精度和边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(100, divide_with_ceil(999, 10))
  assert_eq(-1, divide_with_ceil(-5, 5))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "enhanced_greet_special_cases" {
  // 测试问候函数的特殊情况
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC123!", greet("ABC123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
}

test "enhanced_financial_calculations" {
  // 测试金融计算场景
  let price = 199
  let quantity = 3
  let discount = 50
  let tax_rate = 8
  
  let subtotal = multiply(price, quantity)
  let discounted_total = add(subtotal, -discount)
  let tax = divide_with_ceil(multiply(discounted_total, tax_rate), 100)
  let final_total = add(discounted_total, tax)
  
  assert_eq(597, subtotal)
  assert_eq(547, discounted_total)
  assert_eq(44, tax)
  assert_eq(591, final_total)
}

test "enhanced_inventory_management" {
  // 测试库存管理场景
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 200
  
  let total_sold = multiply(daily_sales, days)
  let stock_before_shipment = add(initial_stock, -total_sold)
  let final_stock = add(stock_before_shipment, new_shipment)
  
  assert_eq(175, total_sold)
  assert_eq(325, stock_before_shipment)
  assert_eq(525, final_stock)
}

test "enhanced_engineering_calculations" {
  // 测试工程计算场景
  let length = 10
  let width = 8
  let height = 3
  
  let floor_area = multiply(length, width)
  let wall_area = multiply(2, multiply(add(length, width), height))
  let total_surface = add(floor_area, wall_area)
  
  assert_eq(80, floor_area)
  assert_eq(108, wall_area)
  assert_eq(188, total_surface)
}

test "enhanced_performance_metrics" {
  // 测试性能指标计算
  let baseline = 1000
  let improvement_factor = 25
  let overhead = 50
  
  let improved = add(baseline, divide_with_ceil(multiply(baseline, improvement_factor), 100))
  let net_improvement = add(improved, -overhead)
  
  assert_eq(1250, improved)
  assert_eq(1200, net_improvement)
}

test "enhanced_data_analysis" {
  // 测试数据分析场景
  let data_points = 1000
  let sample_size = 50
  let confidence_level = 95
  
  let samples_needed = divide_with_ceil(multiply(data_points, confidence_level), 100)
  let actual_samples = if samples_needed > sample_size { sample_size } else { samples_needed }
  
  assert_eq(950, samples_needed)
  assert_eq(50, actual_samples)
}

test "enhanced_error_boundary_tests" {
  // 测试错误边界情况
  // 测试极值
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000000000, multiply(big_num, big_num))
  
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
}