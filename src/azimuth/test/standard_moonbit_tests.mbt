// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))  // 接近Int最大值
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483647, -1))  // 接近Int最小值
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))  // 最大值与相反数相加
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))  // 大数相加
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))  // 任何数乘以0
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))  // 任何数乘以1
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(2147483647, -1))  // 正数乘以-1
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))  // 大数相乘
}

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  @azimuth.assert_eq(5, @azimuth.subtract(8, 3))
  @azimuth.assert_eq(-5, @azimuth.subtract(3, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
  @azimuth.assert_eq(-10, @azimuth.subtract(-10, 0))
  @azimuth.assert_eq(20, @azimuth.subtract(-5, -25))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 3 -> 3
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))   // 1.5 -> 2
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))  // 3.333... -> 4
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))    // 除数为0返回0
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))  // 空格
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))  // Emoji
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))  // 希腊字母
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 12
  let discount = 20
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  // 计算税费（向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = @azimuth.add(subtotal, @azimuth.add(tax, shipping))
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(356, subtotal)
  @azimuth.assert_eq(29, tax)  // ceil(356 * 8 / 100) = ceil(28.48) = 29
  @azimuth.assert_eq(397, total_before_discount)  // 356 + 29 + 12 = 397
  @azimuth.assert_eq(377, final_total)  // 397 - 20 = 377
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "inventory_management_scenario" {
  // 库存管理场景
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  let new_shipment = 50
  
  // 计算总销售量
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  // 计算发货前库存
  let stock_before_shipment = @azimuth.subtract(initial_stock, total_sold)
  // 计算最终库存
  let final_stock = @azimuth.add(stock_before_shipment, new_shipment)
  
  @azimuth.assert_eq(84, total_sold)  // 12 * 7 = 84
  @azimuth.assert_eq(66, stock_before_shipment)  // 150 - 84 = 66
  @azimuth.assert_eq(116, final_stock)  // 66 + 50 = 116
}

test "performance_benchmark_simulation" {
  // 性能基准测试模拟
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  // 计算优化后的操作数
  let optimized_operations = @azimuth.multiply(base_operations, optimization_factor)
  // 计算开销
  let overhead = @azimuth.divide_with_ceil(@azimuth.multiply(optimized_operations, overhead_percentage), 100)
  // 计算实际性能
  let actual_performance = @azimuth.add(optimized_operations, -overhead)
  
  @azimuth.assert_eq(3000, optimized_operations)
  @azimuth.assert_eq(450, overhead)  // ceil(3000 * 15 / 100) = ceil(450) = 450
  @azimuth.assert_eq(2550, actual_performance)  // 3000 - 450 = 2550
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  let large_num = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(large_num, large_num))
}