// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库核心功能的全面测试

test "add_function_mathematical_properties" {
  // 测试加法的数学性质
  // 交换律测试
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(add(-5, 10), add(10, -5))
  
  // 结合律测试
  assert_eq(add(add(2, 3), 5), add(2, add(3, 5)))
  
  // 单位元测试
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  
  // 相反数测试
  assert_eq(0, add(15, -15))
  assert_eq(0, add(-8, 8))
}

test "multiply_function_comprehensive" {
  // 测试乘法的全面功能
  // 交换律测试
  assert_eq(multiply(6, 7), multiply(7, 6))
  assert_eq(multiply(-3, 8), multiply(8, -3))
  
  // 结合律测试
  assert_eq(multiply(multiply(2, 3), 4), multiply(2, multiply(3, 4)))
  
  // 分配律测试
  assert_eq(multiply(5, add(3, 2)), add(multiply(5, 3), multiply(5, 2)))
  
  // 零元素测试
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(-456, 0))
  
  // 单位元测试
  assert_eq(25, multiply(25, 1))
  assert_eq(-17, multiply(-17, 1))
}

test "divide_with_ceil_advanced_scenarios" {
  // 测试向上取整除法的高级场景
  // 正数除法测试
  assert_eq(4, divide_with_ceil(11, 3))  // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2))  // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(-2, divide_with_ceil(-4, 2))   // -2 -> -2
  assert_eq(4, divide_with_ceil(-11, -3))  // 3.666... -> 4
  
  // 边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))  // 除数为零的安全处理
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(100, divide_with_ceil(999, 10))  // 99.9 -> 100
}

test "greet_function_unicode_and_special_chars" {
  // 测试问候函数的 Unicode 和特殊字符处理
  // Unicode 字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  
  // 特殊字符测试
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
  assert_eq_string("Hello, spaced name!", greet("spaced name"))
  
  // 边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "real_world_business_calculations" {
  // 测试真实世界的业务计算场景
  // 订单总价计算
  let unit_price = 85
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 25
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(1020, subtotal)
  assert_eq(82, tax_amount)  // ceil(1020 * 8 / 100) = ceil(81.6) = 82
  assert_eq(1127, total)
  
  // 折扣计算
  let discount_threshold = 1000
  let discount_amount = 50
  let final_total = if total > discount_threshold {
    add(total, -discount_amount)
  } else {
    total
  }
  
  assert_eq(1077, final_total)
}

test "inventory_and_packaging_optimization" {
  // 测试库存和包装优化计算
  let product_a_qty = 150
  let product_b_qty = 87
  let product_c_qty = 234
  
  let box_capacity = 25
  
  // 计算每种产品需要的包装箱数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  assert_eq(6, boxes_a)   // ceil(150/25) = 6
  assert_eq(4, boxes_b)   // ceil(87/25) = 4
  assert_eq(10, boxes_c)  // ceil(234/25) = 10
  
  // 计算总箱数和剩余容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  let total_items = add(add(product_a_qty, product_b_qty), product_c_qty)
  let unused_capacity = add(total_capacity, -total_items)
  
  assert_eq(20, total_boxes)
  assert_eq(500, total_capacity)
  assert_eq(471, total_items)
  assert_eq(29, unused_capacity)
}

test "energy_consumption_analysis" {
  // 测试能源消耗分析计算
  let daily_consumption_kwh = 120
  let days_in_month = 30
  let peak_hours_percentage = 40
  let off_peak_rate = 8
  let peak_rate = 15
  
  // 计算月度总消耗
  let monthly_consumption = multiply(daily_consumption_kwh, days_in_month)
  
  // 计算峰值和非峰值的消耗量
  let peak_consumption = divide_with_ceil(multiply(monthly_consumption, peak_hours_percentage), 100)
  let off_peak_consumption = add(monthly_consumption, -peak_consumption)
  
  // 计算电费
  let peak_cost = multiply(peak_consumption, peak_rate)
  let off_peak_cost = multiply(off_peak_consumption, off_peak_rate)
  let total_cost = add(peak_cost, off_peak_cost)
  
  assert_eq(3600, monthly_consumption)
  assert_eq(1440, peak_consumption)   // ceil(3600 * 40 / 100) = ceil(1440) = 1440
  assert_eq(2160, off_peak_consumption)
  assert_eq(21600, peak_cost)         // 1440 * 15
  assert_eq(17280, off_peak_cost)     // 2160 * 8
  assert_eq(38880, total_cost)
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 等差数列求和：1 + 3 + 5 + ... + 19 (10个奇数)
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果：1 + 3 + 5 + 7 + 9 + 11 + 13 + 15 + 17 + 19 = 100
  assert_eq(100, sum_formula)
  
  // 等比数列求和：2 + 4 + 8 + 16 + 32
  let geometric_sum = add(add(add(add(2, 4), 8), 16), 32)
  assert_eq(62, geometric_sum)
  
  // 阶乘计算：5! = 5 * 4 * 3 * 2 * 1
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_5)
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  // 极值测试
  let large_positive = 1000000
  let large_negative = -1000000
  
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  assert_eq(0, add(large_positive, large_negative))
  
  assert_eq(1000000000000, multiply(large_positive, large_positive))
  assert_eq(1000000000000, multiply(large_negative, large_negative))
  assert_eq(-1000000000000, multiply(large_positive, large_negative))
  
  // 除法的边界情况
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, divide_with_ceil(large_positive, 1))
  
  // 字符串边界测试
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  let very_long_name = "This is a very long name that might cause issues in some systems"
  assert_eq_string("Hello, " + very_long_name + "!", greet(very_long_name))
}

test "performance_and_efficiency_metrics" {
  // 测试性能和效率指标计算
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  let target_improvement = 250
  
  // 计算优化后的性能
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  // 计算系统开销
  let system_overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_operations, -system_overhead)
  
  // 计算性能改进百分比
  let improvement_percentage = divide_with_ceil(multiply(actual_performance - base_operations, 100), base_operations)
  
  // 验证是否达到目标
  let target_met = if actual_performance >= target_improvement { 1 } else { 0 }
  
  assert_eq(3000, optimized_operations)
  assert_eq(450, system_overhead)  // ceil(3000 * 15 / 100) = 450
  assert_eq(2550, actual_performance)
  assert_eq(155, improvement_percentage)  // ceil((2550-1000) * 100 / 1000) = 155
  assert_eq(1, target_met)  // 目标达成
}