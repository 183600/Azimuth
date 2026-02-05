// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

// 引入 azimuth 库
use azimuth

test "mathematical_commutativity_laws" {
  // 测试数学交换律
  let a = 42
  let b = 17
  
  // 加法交换律: a + b = b + a
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
}

test "mathematical_associativity_laws" {
  // 测试数学结合律
  let a = 5
  let b = 10
  let c = 15
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_add = azimuth.add(azimuth.add(a, b), c)
  let right_add = azimuth.add(a, azimuth.add(b, c))
  assert_eq(left_add, right_add)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mul = azimuth.multiply(azimuth.multiply(a, b), c)
  let right_mul = azimuth.multiply(a, azimuth.multiply(b, c))
  assert_eq(left_mul, right_mul)
}

test "distributive_property_verification" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  assert_eq(left_side, right_side)
}

test "boundary_value_testing" {
  // 边界值测试
  // 测试零值
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(5, azimuth.add(5, 0))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(0, azimuth.multiply(0, 5))
  
  // 测试单位元
  assert_eq(5, azimuth.add(5, 0))
  assert_eq(5, azimuth.multiply(5, 1))
  
  // 测试负数边界
  assert_eq(0, azimuth.add(5, -5))
  assert_eq(-25, azimuth.multiply(5, -5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, azimuth.divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, azimuth.divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(3, azimuth.divide_with_ceil(10, -3))   // -3.333... -> -3
  assert_eq(4, azimuth.divide_with_ceil(-10, -3))  // 3.333... -> 4
  
  // 零除数情况
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 空字符串
  assert_eq_string("Hello, !", azimuth.greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", azimuth.greet("A"))
  
  // 长字符串
  let long_name = "This is a very long name that tests the greet function with extended input"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, azimuth.greet(long_name))
  
  // 特殊字符
  assert_eq_string("Hello, 	
\r!", azimuth.greet("	
\r"))
  assert_eq_string("Hello, 🌟🚀!", azimuth.greet("🌟🚀"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  // 计算贷款还款：本金 + 利息 - 提前还款
  let principal = 100000
  let interest_rate = 5  // 5%
  let years = 3
  let early_payment = 20000
  
  // 计算总利息（简化计算）
  let total_interest = azimuth.divide_with_ceil(
    azimuth.multiply(principal, azimuth.multiply(interest_rate, years)), 
    100
  )
  
  // 计算总还款额
  let total_payment = azimuth.add(principal, total_interest)
  
  // 计算最终还款额
  let final_payment = azimuth.add(total_payment, -early_payment)
  
  assert_eq(15000, total_interest)
  assert_eq(115000, total_payment)
  assert_eq(95000, final_payment)
}

test "inventory_optimization_scenario" {
  // 库存优化场景
  let current_stock = 250
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 50
  
  // 计算Lead Time期间的需求
  let lead_time_demand = azimuth.multiply(daily_demand, lead_time_days)
  
  // 计算再订货点
  let reorder_point = azimuth.add(lead_time_demand, safety_stock)
  
  // 计算经济订货量（简化版本）
  let economic_order_quantity = azimuth.divide_with_ceil(
    azimuth.multiply(2, lead_time_demand), 
    3
  )
  
  assert_eq(105, lead_time_demand)
  assert_eq(155, reorder_point)
  assert_eq(70, economic_order_quantity)
}

test "data_processing_pipeline" {
  // 数据处理管道模拟
  let raw_data_count = 1000
  let error_rate_percent = 5
  let processing_batches = 8
  
  // 计算错误数据量
  let error_count = azimuth.divide_with_ceil(
    azimuth.multiply(raw_data_count, error_rate_percent), 
    100
  )
  
  // 计算有效数据量
  let valid_data = azimuth.add(raw_data_count, -error_count)
  
  // 计算每批处理的数据量
  let batch_size = azimuth.divide_with_ceil(valid_data, processing_batches)
  
  // 计算总处理时间（假设每批处理需要10分钟）
  let total_processing_time = azimuth.multiply(processing_batches, 10)
  
  assert_eq(50, error_count)
  assert_eq(950, valid_data)
  assert_eq(119, batch_size)
  assert_eq(80, total_processing_time)
}

test "performance_benchmark_simulation" {
  // 性能基准测试模拟
  let baseline_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  // 计算优化后的操作数
  let optimized_operations = azimuth.multiply(baseline_operations, optimization_factor)
  
  // 计算开销
  let overhead = azimuth.divide_with_ceil(
    azimuth.multiply(optimized_operations, overhead_percentage), 
    100
  )
  
  // 计算实际性能
  let actual_performance = azimuth.add(optimized_operations, -overhead)
  
  // 计算性能提升倍数
  let performance_improvement = azimuth.divide_with_ceil(actual_performance, baseline_operations)
  
  assert_eq(3000, optimized_operations)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
  assert_eq(3, performance_improvement)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试极端数值
  let large_positive = 2147483647
  let large_negative = -2147483647
  
  // 测试大数运算不会溢出（在合理范围内）
  let sum_large = azimuth.add(large_positive, -1000000)
  let product_large = azimuth.divide_with_ceil(large_positive, 1000)
  
  // 测试除零保护
  let divide_zero_1 = azimuth.divide_with_ceil(100, 0)
  let divide_zero_2 = azimuth.divide_with_ceil(-100, 0)
  let divide_zero_3 = azimuth.divide_with_ceil(0, 0)
  
  // 测试减法边界情况
  let subtract_result = azimuth.subtract(-100, -200)
  
  assert_eq(2147482647, sum_large)
  assert_eq(2147484, product_large)
  assert_eq(0, divide_zero_1)
  assert_eq(0, divide_zero_2)
  assert_eq(0, divide_zero_3)
  assert_eq(100, subtract_result)
}