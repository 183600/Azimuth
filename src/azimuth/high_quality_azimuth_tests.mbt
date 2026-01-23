// 高质量 MoonBit 测试用例
// 针对 azimuth 库的全面测试，包含边界情况、实际应用场景和数学性质验证

test "add_function_comprehensive_test" {
  // 全面测试加法功能，包括正数、负数和零
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, add(2147483647, -2147483646))
  assert_eq(-1, add(-2147483648, 2147483647))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况和特殊情况
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(24, multiply(-8, -3))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(1, 2147483647))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 全面测试向上取整除法的各种场景
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-3, divide_with_ceil(10, -3))
  
  // 特殊情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言和特殊字符支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "mathematical_properties_verification" {
  // 验证基本数学性质
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_business_calculations" {
  // 实际业务计算场景：订单总价计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping_cost = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(subtotal, add(tax_amount, shipping_cost))
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(400, total_before_discount)
  assert_eq(380, final_total)
}

test "complex_inventory_management" {
  // 复杂库存管理场景
  let initial_stock = 1000
  let daily_sales = 45
  let days_to_simulate = 7
  let restock_threshold = 200
  let restock_amount = 500
  
  // 计算总销售量
  let total_sold = multiply(daily_sales, days_to_simulate)
  
  // 计算剩余库存
  let remaining_stock = add(initial_stock, -total_sold)
  
  // 判断是否需要补货
  let needs_restock = if remaining_stock < restock_threshold { true } else { false }
  
  // 计算补货后的库存
  let final_stock = if needs_restock { add(remaining_stock, restock_amount) } else { remaining_stock }
  
  assert_eq(315, total_sold)
  assert_eq(685, remaining_stock)
  assert_eq(true, needs_restock)
  assert_eq(1185, final_stock)
}

test "engineering_calculations_packaging" {
  // 工程计算：包装优化
  let total_items = 247
  let box_capacity = 24
  let pallet_capacity = 10
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, pallet_capacity)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = add(total_capacity, -total_items)
  
  assert_eq(11, boxes_needed)
  assert_eq(2, pallets_needed)
  assert_eq(264, total_capacity)
  assert_eq(17, unused_space)
}

test "data_processing_statistics" {
  // 数据处理和统计分析
  let data_count = 8
  
  // 计算总和（使用累加）
  let sum = add(add(add(add(add(add(add(150, 180), 220), 250), 190), 210), 230), 170)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, data_count)
  
  // 计算最大值和最小值的差
  let max_value = 250
  let min_value = 150
  let range = add(max_value, -min_value)
  
  // 计算方差估计（简化版）
  let variance_estimate = divide_with_ceil(multiply(range, range), 4)
  
  assert_eq(1600, sum)
  assert_eq(200, average)
  assert_eq(100, range)
  assert_eq(2500, variance_estimate)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端数值
  let large_positive = 1000000
  let large_negative = -1000000
  
  // 大数运算
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  assert_eq(0, add(large_positive, large_negative))
  
  // 混合运算
  let mixed_result = add(multiply(large_positive, 2), divide_with_ceil(large_negative, 3))
  assert_eq(1999998, mixed_result)
  
  // 字符串边界测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, This is a very long name!", greet("This is a very long name"))
}