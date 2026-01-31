// Azimuth 测试用例 - 10个高质量测试
// 测试核心功能的各种场景和边界情况

test "test_add_edge_cases" {
  // 测试加法的边界情况
  assert_eq(1000, add(999, 1))
  assert_eq(-1000, add(-999, -1))
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(0, 1))
}

test "test_multiply_properties" {
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

test "test_divide_precision" {
  // 测试除法的精度和边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(100, divide_with_ceil(999, 10))
  assert_eq(-1, divide_with_ceil(-5, 5))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "test_greet_special_cases" {
  // 测试问候函数的特殊情况
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC123!", greet("ABC123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
}

test "test_financial_calculations" {
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

test "test_inventory_management" {
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

test "test_engineering_calculations" {
  // 测试工程计算场景
  let length = 10
  let width = 8
  let height = 3
  let material_thickness = 0.5  // 向上取整为1
  
  let floor_area = multiply(length, width)
  let wall_area = multiply(2, multiply(add(length, width), height))
  let total_surface = add(floor_area, wall_area)
  let material_units = divide_with_ceil(total_surface, 1)
  
  assert_eq(80, floor_area)
  assert_eq(108, wall_area)
  assert_eq(188, total_surface)
  assert_eq(188, material_units)
}

test "test_performance_metrics" {
  // 测试性能指标计算
  let baseline = 1000
  let improvement_factor = 25
  let overhead = 50
  
  let improved = add(baseline, divide_with_ceil(multiply(baseline, improvement_factor), 100))
  let net_improvement = add(improved, -overhead)
  
  assert_eq(1250, improved)
  assert_eq(1200, net_improvement)
}

test "test_data_analysis" {
  // 测试数据分析场景
  let data_points = 1000
  let sample_size = 50
  let confidence_level = 95
  
  let samples_needed = divide_with_ceil(multiply(data_points, confidence_level), 100)
  let actual_samples = if samples_needed > sample_size { sample_size } else { samples_needed }
  
  assert_eq(950, samples_needed)
  assert_eq(50, actual_samples)
}

test "test_error_boundary_tests" {
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