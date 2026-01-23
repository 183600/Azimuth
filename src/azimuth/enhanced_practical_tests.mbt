// 增强 MoonBit 测试用例
// 覆盖 azimuth 库的高级功能

test "add_advanced_functionality" {
  // 测试高级加法功能
  assert_eq(100, add(50, 50))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(100, -100))
}

test "multiply_advanced_functionality" {
  // 测试高级乘法功能
  assert_eq(100, multiply(10, 10))
  assert_eq(1000, multiply(100, 10))
  assert_eq(-100, multiply(-10, 10))
  assert_eq(100, multiply(-10, -10))
}

test "greet_advanced_functionality" {
  // 测试高级问候功能
  assert_eq_string("Hello, Advanced MoonBit!", greet("Advanced MoonBit"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, 12345!", greet("12345"))
}

test "divide_with_ceil_advanced_functionality" {
  // 测试高级向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let product_price = 120
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(subtotal, add(shipping, tax))
  // 计算最终价格
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(960, subtotal)
  assert_eq(77, tax)
  assert_eq(1052, total_before_discount)
  assert_eq(1032, final_total)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, rate_percent), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_total, rate_percent), 100)
  let year3_total = add(year2_total, year3_interest)
  
  assert_eq(500, year1_interest)
  assert_eq(10500, year1_total)
  assert_eq(525, year2_interest)
  assert_eq(11025, year2_total)
  assert_eq(552, year3_interest)
  assert_eq(11577, year3_total)
}

test "inventory_management_calculation" {
  // 库存管理计算测试
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let safety_stock = 100
  let reorder_point = add(multiply(daily_sales, days), safety_stock)
  
  // 计算一周后的库存
  let weekly_sales = multiply(daily_sales, days)
  let remaining_stock = add(initial_stock, -weekly_sales)
  
  // 计算需要的补货数量
  let reorder_quantity = multiply(2, initial_stock)
  
  assert_eq(175, weekly_sales)
  assert_eq(325, remaining_stock)
  assert_eq(275, reorder_point)
  assert_eq(1000, reorder_quantity)
}

test "packaging_optimization_calculation" {
  // 包装优化计算测试
  let total_items = 250
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, box_capacity)
  let wasted_space = add(total_capacity, -total_items)
  
  assert_eq(11, boxes_needed)  // ceil(250/24) = 11
  assert_eq(264, total_capacity)  // 11 * 24 = 264
  assert_eq(14, wasted_space)  // 264 - 250 = 14
}

test "data_transfer_calculation" {
  // 数据传输计算测试
  let file_size_mb = 850
  let network_speed_mbps = 50
  let compression_ratio = 70
  
  // 计算压缩后大小
  let compressed_size = divide_with_ceil(multiply(file_size_mb, compression_ratio), 100)
  // 计算传输时间
  let transfer_time = divide_with_ceil(compressed_size, network_speed_mbps)
  
  assert_eq(595, compressed_size)  // ceil(850 * 70 / 100) = 595
  assert_eq(12, transfer_time)  // ceil(595/50) = 12
}

test "error_boundary_test" {
  // 错误边界测试
  let normal_value = 100
  let extreme_value = 2147483647
  
  // 测试正常值
  assert_eq(200, add(normal_value, normal_value))
  assert_eq(10000, multiply(normal_value, normal_value))
  
  // 测试除零安全性
  assert_eq(0, divide_with_ceil(normal_value, 0))
  assert_eq(0, divide_with_ceil(extreme_value, 0))
  
  // 测试空字符串问候
  assert_eq_string("Hello, !", greet(""))
}