// 高质量 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 使用标准的 MoonBit 测试语法

test "numeric_overflow_boundary" {
  // 测试数值溢出边界情况
  let max_safe = 2147483647 / 2
  let result = @azimuth.add(max_safe, max_safe)
  @azimuth.assert_eq(2147483646, result)
}

test "divide_with_ceil_decimal_precision" {
  // 测试向上取整除法的小数精度处理
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))  // 0.5 -> 1
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))  // 3.5 -> 4
  @azimuth.assert_eq(34, @azimuth.divide_with_ceil(100, 3))  // 33.33... -> 34
}

test "greet_function_unicode_validation" {
  // 测试问候函数的 Unicode 字符处理
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγδε!", @azimuth.greet("αβγδε"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

test "compound_financial_calculation" {
  // 测试复合金融计算场景
  let principal = 1000
  let rate1 = 5
  let rate2 = 3
  let rate3 = 4
  
  // 计算三年复利
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate1), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate2), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate3), 100))
  
  @azimuth.assert_eq(1050, year1)  // 1000 + 5% = 1050
  @azimuth.assert_eq(1082, year2)  // 1050 + 3% = 1081.5 -> 1082
  @azimuth.assert_eq(1126, year3)  // 1082 + 4% = 1125.28 -> 1126
}

test "inventory_optimization_problem" {
  // 测试库存优化问题
  let demand_forecast = 157
  let batch_size = 25
  let safety_stock = 20
  
  // 计算订货数量（向上取整到最近的批次）
  let base_orders = @azimuth.divide_with_ceil(demand_forecast, batch_size)
  let total_order_quantity = @azimuth.multiply(base_orders, batch_size)
  let final_inventory = @azimuth.add(total_order_quantity, safety_stock)
  
  @azimuth.assert_eq(7, base_orders)  // ceil(157/25) = 7
  @azimuth.assert_eq(175, total_order_quantity)  // 7 * 25 = 175
  @azimuth.assert_eq(195, final_inventory)  // 175 + 20 = 195
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, ABC123!", @azimuth.greet("ABC123"))
  @azimuth.assert_eq_string("Hello, special@chars#123!", @azimuth.greet("special@chars#123"))
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算
  // 计算等差数列：3, 7, 11, 15, 19 的和
  let term1 = 3
  let difference = 4
  let num_terms = 5
  
  let sum = @azimuth.multiply(num_terms, @azimuth.add(term1, @azimuth.multiply(num_terms - 1, difference))) / 2
  @azimuth.assert_eq(55, sum)  // 5 * (3 + 16) / 2 = 55
}

test "resource_allocation_algorithm" {
  // 测试资源分配算法
  let total_resources = 100
  let project1_needs = 23
  let project2_needs = 31
  let project3_needs = 19
  
  let allocated1 = project1_needs
  let allocated2 = project2_needs
  let allocated3 = project3_needs
  let total_allocated = @azimuth.add(@azimuth.add(allocated1, allocated2), allocated3)
  let remaining = @azimuth.add(total_resources, -total_allocated)
  
  @azimuth.assert_eq(73, total_allocated)  // 23 + 31 + 19 = 73
  @azimuth.assert_eq(27, remaining)  // 100 - 73 = 27
  
  // 计算可以支持的新项目数量（每个需要10个资源）
  let new_projects = @azimuth.divide_with_ceil(remaining, 10)
  @azimuth.assert_eq(3, new_projects)  // ceil(27/10) = 3
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 测试除数为零的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  
  // 测试极值运算
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(1, @azimuth.add(2147483646, -2147483645))
}

test "performance_benchmark_simulation" {
  // 测试性能基准模拟
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percent = 15
  
  // 计算优化后的性能
  let optimized_ops = @azimuth.multiply(base_operations, optimization_factor)
  let overhead = @azimuth.divide_with_ceil(@azimuth.multiply(optimized_ops, overhead_percent), 100)
  let actual_performance = @azimuth.add(optimized_ops, -overhead)
  
  @azimuth.assert_eq(3000, optimized_ops)
  @azimuth.assert_eq(450, overhead)  // ceil(3000 * 15 / 100) = 450
  @azimuth.assert_eq(2550, actual_performance)  // 3000 - 450 = 2550
}