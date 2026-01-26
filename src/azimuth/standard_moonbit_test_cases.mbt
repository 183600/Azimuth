// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的高质量测试

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  azimuth::assert_eq(1000000, azimuth::add(500000, 500000))
  azimuth::assert_eq(-1000000, azimuth::add(-500000, -500000))
  azimuth::assert_eq(0, azimuth::add(1000000, -1000000))
  azimuth::assert_eq(1, azimuth::add(2147483647, -2147483646))
}

test "multiply_function_precision" {
  // 测试乘法函数的精度
  azimuth::assert_eq(1000000, azimuth::multiply(1000, 1000))
  azimuth::assert_eq(-1000000, azimuth::multiply(1000, -1000))
  azimuth::assert_eq(0, azimuth::multiply(2147483647, 0))
  azimuth::assert_eq(2147483647, azimuth::multiply(1, 2147483647))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-11, -3)) // -11/-3 = 3.666... -> 4
  azimuth::assert_eq(0, azimuth::divide_with_ceil(100, 0))  // 除数为0的安全处理
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
  azimuth::assert_eq_string("Hello, Español!", azimuth::greet("Español"))
  azimuth::assert_eq_string("Hello, Москва!", azimuth::greet("Москва"))
  azimuth::assert_eq_string("Hello, العربية!", azimuth::greet("العربية"))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  let product_cost = 120
  let quantity = 15
  let tax_rate = 8
  let shipping_cost_per_item = 5
  let discount_threshold = 1000
  let discount_amount = 50
  
  // 计算小计
  let subtotal = azimuth::multiply(product_cost, quantity)
  
  // 计算运费
  let total_shipping = azimuth::multiply(shipping_cost_per_item, quantity)
  
  // 计算税费
  let tax_amount = azimuth::divide_with_ceil(azimuth::multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = azimuth::add(azimuth::add(subtotal, total_shipping), tax_amount)
  
  // 应用折扣
  let final_total = if pre_discount_total > discount_threshold {
    azimuth::add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  azimuth::assert_eq(1800, subtotal)
  azimuth::assert_eq(75, total_shipping)
  azimuth::assert_eq(144, tax_amount)
  azimuth::assert_eq(2019, pre_discount_total)
  azimuth::assert_eq(1969, final_total)
}

test "mathematical_composition" {
  // 测试数学函数的组合
  let x = 12
  let y = 8
  let z = 5
  
  // 测试分配律: x * (y + z) = x * y + x * z
  let distributive_left = azimuth::multiply(x, azimuth::add(y, z))
  let distributive_right = azimuth::add(azimuth::multiply(x, y), azimuth::multiply(x, z))
  azimuth::assert_eq(distributive_left, distributive_right)
  
  // 测试结合律: (x + y) + z = x + (y + z)
  let associative_left = azimuth::add(azimuth::add(x, y), z)
  let associative_right = azimuth::add(x, azimuth::add(y, z))
  azimuth::assert_eq(associative_left, associative_right)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 1000
  let rate_percent = 5
  let _years = 3  // 未使用的变量
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = azimuth::add(principal, azimuth::divide_with_ceil(azimuth::multiply(principal, rate_percent), 100))
  let amount_after_year2 = azimuth::add(amount_after_year1, azimuth::divide_with_ceil(azimuth::multiply(amount_after_year1, rate_percent), 100))
  let amount_after_year3 = azimuth::add(amount_after_year2, azimuth::divide_with_ceil(azimuth::multiply(amount_after_year2, rate_percent), 100))
  
  azimuth::assert_eq(1050, amount_after_year1)
  azimuth::assert_eq(1103, amount_after_year2)
  azimuth::assert_eq(1158, amount_after_year3)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let weekly_restock = 200
  let days_to_simulate = 14
  
  // 计算总销售量
  let total_sold = azimuth::multiply(daily_sales, days_to_simulate)
  
  // 计算补货次数（每7天补货一次）
  let restock_times = azimuth::divide_with_ceil(days_to_simulate, 7)
  
  // 计算总补货量
  let total_restocked = azimuth::multiply(weekly_restock, restock_times)
  
  // 计算最终库存
  let final_stock = azimuth::add(azimuth::add(initial_stock, -total_sold), total_restocked)
  
  azimuth::assert_eq(350, total_sold)
  azimuth::assert_eq(2, restock_times)
  azimuth::assert_eq(400, total_restocked)
  azimuth::assert_eq(550, final_stock)
}

test "data_processing_pipeline" {
  // 数据处理管道测试
  let raw_data_points = 1000
  let valid_data_ratio = 85
  let processing_batches = 20
  let error_rate = 3
  
  // 计算有效数据点
  let valid_data = azimuth::divide_with_ceil(azimuth::multiply(raw_data_points, valid_data_ratio), 100)
  
  // 计算每批处理的数据点
  let batch_size = azimuth::divide_with_ceil(valid_data, processing_batches)
  
  // 计算预期错误数
  let expected_errors = azimuth::divide_with_ceil(azimuth::multiply(valid_data, error_rate), 100)
  
  // 计算成功处理的数据点
  let successful_data = azimuth::add(valid_data, -expected_errors)
  
  azimuth::assert_eq(850, valid_data)
  azimuth::assert_eq(43, batch_size)
  azimuth::assert_eq(26, expected_errors)
  azimuth::assert_eq(824, successful_data)
}

test "performance_benchmark_simulation" {
  // 性能基准测试模拟
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  let concurrent_threads = 4
  
  // 计算优化后的操作数
  let optimized_operations = azimuth::multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = azimuth::divide_with_ceil(azimuth::multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = azimuth::add(optimized_operations, -overhead)
  
  // 计算每线程操作数
  let operations_per_thread = azimuth::divide_with_ceil(actual_performance, concurrent_threads)
  
  // 计算性能提升倍数
  let performance_improvement = azimuth::divide_with_ceil(actual_performance, base_operations)
  
  azimuth::assert_eq(3000, optimized_operations)
  azimuth::assert_eq(450, overhead)
  azimuth::assert_eq(2550, actual_performance)
  azimuth::assert_eq(638, operations_per_thread)
  azimuth::assert_eq(3, performance_improvement)
}