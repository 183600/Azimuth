// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))  // 接近Int最大值
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))  // 接近Int最小值
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))  // 最大值与相反数相加
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))  // 大数相加
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))  // 大负数相加
}

test "multiply_function_precision" {
  // 测试乘法函数的精度和边界情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))  // 单位元测试
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))  // 负数乘法
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))  // 负负得正
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))  // 零乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))  // 大数乘法
}

test "divide_with_ceil_mathematical_properties" {
  // 测试向上取整除法的数学性质
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))  // 完全相等
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(9999, 100))  // 99.99 -> 100
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))  // 负数相等
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))  // 零除法
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))  // 除数为零
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))  // 空格字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))  // 中文
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))  // Emoji
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))  // 邮箱格式
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  let product_cost = 120
  let quantity = 15
  let tax_rate = 8
  let shipping_cost_per_item = 5
  let discount_threshold = 1000
  let discount_amount = 50
  
  // 步骤1: 计算商品总价
  let subtotal = @azimuth.multiply(product_cost, quantity)
  
  // 步骤2: 计算运费
  let total_shipping = @azimuth.multiply(shipping_cost_per_item, quantity)
  
  // 步骤3: 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 步骤4: 计算折扣前总价
  let pre_discount_total = @azimuth.add(@azimuth.add(subtotal, total_shipping), tax_amount)
  
  // 步骤5: 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    @azimuth.add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  @azimuth.assert_eq(1800, subtotal)
  @azimuth.assert_eq(75, total_shipping)
  @azimuth.assert_eq(144, tax_amount)
  @azimuth.assert_eq(2019, pre_discount_total)
  @azimuth.assert_eq(1969, final_total)
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果
  @azimuth.assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "inventory_management_scenario" {
  // 库存管理场景测试
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  let new_shipment = 50
  
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  let stock_before_shipment = @azimuth.add(initial_stock, -total_sold)
  let final_stock = @azimuth.add(stock_before_shipment, new_shipment)
  
  @azimuth.assert_eq(84, total_sold)
  @azimuth.assert_eq(66, stock_before_shipment)
  @azimuth.assert_eq(116, final_stock)
}

test "engineering_calculation_scenario" {
  // 工程计算场景测试
  let room_length = 12
  let room_width = 8
  let room_height = 3
  let tile_area = 2  // 每块瓷砖2平方米
  
  // 计算房间面积
  let floor_area = @azimuth.multiply(room_length, room_width)
  let wall_area = @azimuth.multiply(2, @azimuth.multiply(@azimuth.add(room_length, room_width), room_height))
  let total_surface_area = @azimuth.add(floor_area, wall_area)
  
  // 计算需要的瓷砖数量
  let tiles_needed = @azimuth.divide_with_ceil(total_surface_area, tile_area)
  
  // 计算油漆用量（假设每升油漆覆盖10平方米）
  let paint_needed = @azimuth.divide_with_ceil(wall_area, 10)
  
  @azimuth.assert_eq(96, floor_area)
  @azimuth.assert_eq(120, wall_area)
  @azimuth.assert_eq(216, total_surface_area)
  @azimuth.assert_eq(108, tiles_needed)
  @azimuth.assert_eq(12, paint_needed)
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
  
  // 计算性能提升倍数
  let performance_improvement = @azimuth.divide_with_ceil(actual_performance, base_operations)
  
  @azimuth.assert_eq(3000, optimized_operations)
  @azimuth.assert_eq(450, overhead)
  @azimuth.assert_eq(2550, actual_performance)
  @azimuth.assert_eq(3, performance_improvement)
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  let sales_q1 = 15000
  let sales_q2 = 18000
  let sales_q3 = 22000
  let sales_q4 = 25000
  
  // 计算年度总销售额
  let annual_sales = @azimuth.add(@azimuth.add(@azimuth.add(sales_q1, sales_q2), sales_q3), sales_q4)
  
  // 计算季度平均销售额（向上取整）
  let quarterly_average = @azimuth.divide_with_ceil(annual_sales, 4)
  
  // 计算增长率百分比
  let growth_q2_q1 = @azimuth.divide_with_ceil(@azimuth.multiply(sales_q2 - sales_q1, 100), sales_q1)
  let growth_q3_q2 = @azimuth.divide_with_ceil(@azimuth.multiply(sales_q3 - sales_q2, 100), sales_q2)
  let growth_q4_q3 = @azimuth.divide_with_ceil(@azimuth.multiply(sales_q4 - sales_q3, 100), sales_q3)
  
  @azimuth.assert_eq(80000, annual_sales)
  @azimuth.assert_eq(20000, quarterly_average)
  @azimuth.assert_eq(20, growth_q2_q1)
  @azimuth.assert_eq(23, growth_q3_q2)
  @azimuth.assert_eq(14, growth_q4_q3)
}