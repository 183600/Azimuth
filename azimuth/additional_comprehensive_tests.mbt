// 额外的高质量 MoonBit 测试用例
// 为 azimuth 库提供额外的测试覆盖，专注于边界条件和实际应用场景

// 导入被测试的模块
use azimuth

test "extreme_boundary_conditions" {
  // 测试极端边界条件
  // 接近整数边界的加法测试
  assert_eq(2147483646, azimuth.add(2147483645, 1))
  assert_eq(-2147483647, azimuth.add(-2147483646, -1))
  
  // 大数乘法测试
  assert_eq(46340, azimuth.multiply(215, 215))  // 215^2 = 46225，在安全范围内
  assert_eq(-46340, azimuth.multiply(215, -215))
}

test "financial_calculation_scenarios" {
  // 金融计算场景测试
  // 场景：计算贷款分期
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = azimuth.divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 验证总支付金额
  let total_payment = azimuth.multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 计算最后一期的实际支付金额
  let previous_payments = azimuth.multiply(months_needed - 1, monthly_payment)
  let final_payment = total_payment - previous_payments
  assert_eq(600, final_payment)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：云服务器资源分配
  let total_cpu_cores = 127
  let instances = 8
  let cores_per_instance = azimuth.divide_with_ceil(total_cpu_cores, instances)
  assert_eq(16, cores_per_instance)  // ceil(127/8) = 16
  
  // 计算实际分配的总核心数
  let total_allocated = azimuth.multiply(cores_per_instance, instances)
  assert_eq(128, total_allocated)
  
  // 计算资源浪费
  let resource_waste = total_allocated - total_cpu_cores
  assert_eq(1, resource_waste)
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  // 模拟多层业务逻辑计算
  let base_value = 100
  let growth_rate = 15
  let periods = 3
  
  // 复利计算（简化版）
  let period1 = azimuth.add(base_value, azimuth.divide_with_ceil(azimuth.multiply(base_value, growth_rate), 100))
  let period2 = azimuth.add(period1, azimuth.divide_with_ceil(azimuth.multiply(period1, growth_rate), 100))
  let period3 = azimuth.add(period2, azimuth.divide_with_ceil(azimuth.multiply(period2, growth_rate), 100))
  
  assert_eq(115, period1)  // 100 + 15%
  assert_eq(133, period2)  // 115 + 15% = 132.25 -> 133
  assert_eq(154, period3)  // 133 + 15% = 152.95 -> 153 -> 154 (向上取整)
}

test "internationalization_comprehensive" {
  // 国际化综合测试
  // 测试各种语言和特殊字符的问候
  assert_eq_string("Hello, 中文测试!", azimuth.greet("中文测试"))
  assert_eq_string("Hello, العربية!", azimuth.greet("العربية"))
  assert_eq_string("Hello, हिन्दी!", azimuth.greet("हिन्दी"))
  assert_eq_string("Hello, 한국어!", azimuth.greet("한국어"))
  assert_eq_string("Hello, ελληνικά!", azimuth.greet("ελληνικά"))
  assert_eq_string("Hello, 🌍🌎🌏!", azimuth.greet("🌍🌎🌏"))
}

test "error_boundary_and_resilience" {
  // 错误边界和恢复能力测试
  // 测试各种除零情况的处理
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  assert_eq(0, azimuth.divide_with_ceil(100, 0))
  assert_eq(0, azimuth.divide_with_ceil(-100, 0))
  assert_eq(0, azimuth.divide_with_ceil(2147483647, 0))
  assert_eq(0, azimuth.divide_with_ceil(-2147483648, 0))
  
  // 测试极小除数
  assert_eq(100000, azimuth.divide_with_ceil(99999, 1))
  assert_eq(-99999, azimuth.divide_with_ceil(-99999, 1))
  
  // 测试边界条件的布尔验证
  let zero_division_result = azimuth.divide_with_ceil(50, 0)
  assert_eq(0, zero_division_result)  // 验证除零返回0
  
  let positive_result = azimuth.divide_with_ceil(10, 3)
  assert_eq(0, positive_result - 4)  // 验证结果为4 (使用减法来验证相等)
}

test "batch_processing_efficiency" {
  // 批处理效率测试
  // 场景：批量数据处理
  let total_records = 1047
  let batch_size = 50
  let batches_needed = azimuth.divide_with_ceil(total_records, batch_size)
  assert_eq(21, batches_needed)  // ceil(1047/50) = 21
  
  // 计算完整批次和最后一批的大小
  let full_batches = batches_needed - 1
  let records_in_full_batches = azimuth.multiply(full_batches, batch_size)
  let final_batch_size = total_records - records_in_full_batches
  
  assert_eq(20, full_batches)
  assert_eq(1000, records_in_full_batches)
  assert_eq(47, final_batch_size)
}

test "mathematical_sequence_operations" {
  // 数学序列操作测试
  // 测试等差数列和等比数列的简单计算
  let first_term = 3
  let common_difference = 4
  let terms_count = 5
  
  // 计算等差数列的第n项：a_n = a_1 + (n-1)d
  let nth_term = azimuth.add(first_term, azimuth.multiply(terms_count - 1, common_difference))
  assert_eq(19, nth_term)  // 3 + (5-1)*4 = 3 + 16 = 19
  
  // 计算等差数列的和：S_n = n/2 * (a_1 + a_n)
  let sum = azimuth.divide_with_ceil(azimuth.multiply(terms_count, azimuth.add(first_term, nth_term)), 2)
  assert_eq(55, sum)  // 5/2 * (3 + 19) = 2.5 * 22 = 55
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  // 场景：多产品库存优化
  let product_a = 89
  let product_b = 156
  let product_c = 234
  let box_capacity = 50
  
  // 计算每种产品需要的箱子数量
  let boxes_a = azimuth.divide_with_ceil(product_a, box_capacity)
  let boxes_b = azimuth.divide_with_ceil(product_b, box_capacity)
  let boxes_c = azimuth.divide_with_ceil(product_c, box_capacity)
  
  assert_eq(2, boxes_a)   // ceil(89/50) = 2
  assert_eq(4, boxes_b)   // ceil(156/50) = 4
  assert_eq(5, boxes_c)   // ceil(234/50) = 5
  
  // 计算总箱子数和总容量
  let total_boxes = azimuth.add(azimuth.add(boxes_a, boxes_b), boxes_c)
  let total_capacity = azimuth.multiply(total_boxes, box_capacity)
  let total_products = azimuth.add(azimuth.add(product_a, product_b), product_c)
  
  assert_eq(11, total_boxes)
  assert_eq(550, total_capacity)
  assert_eq(479, total_products)
  
  // 计算剩余空间
  let remaining_space = total_capacity - total_products
  assert_eq(71, remaining_space)
}

test "performance_benchmark_operations" {
  // 性能基准操作测试
  // 测试大量运算的组合性能
  let base_number = 1000
  let iterations = 100
  
  // 连续加法操作
  let addition_result = azimuth.add(base_number, iterations)
  assert_eq(1100, addition_result)
  
  // 连续乘法操作
  let multiplication_result = azimuth.multiply(base_number, iterations)
  assert_eq(100000, multiplication_result)
  
  // 混合运算：((base + iterations) * 10) / (base / 100)
  let mixed_step1 = azimuth.add(base_number, iterations)
  let mixed_step2 = azimuth.multiply(mixed_step1, 10)
  let mixed_step3 = azimuth.divide_with_ceil(base_number, 100)
  let final_result = azimuth.divide_with_ceil(mixed_step2, mixed_step3)
  
  assert_eq(1100, mixed_step1)      // 1000 + 100
  assert_eq(11000, mixed_step2)     // 1100 * 10
  assert_eq(10, mixed_step3)        // ceil(1000/100)
  assert_eq(1100, final_result)     // ceil(11000/10)
}