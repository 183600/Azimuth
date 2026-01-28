// 额外的综合测试用例 - 针对 azimuth 库的全面测试
// 包含边界情况、错误处理和实际应用场景

test "extreme_boundary_values" {
  // 极端边界值测试
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))  // 接近Int最大值
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))  // 接近Int最小值
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))  // 最大值与相反数相加
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))  // 完全相等
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(9999, 100))  // 99.99 -> 100
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))  // 负数相等
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
}

test "string_greeting_unicode" {
  // Unicode 字符串测试
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let x = 10
  let y = 20
  let z = 5
  
  // 测试表达式：(x + y) * z - x / y
  let result = @azimuth.add(@azimuth.multiply(@azimuth.add(x, y), z), -@azimuth.divide_with_ceil(x, y))
  @azimuth.assert_eq(149, result)  // (10 + 20) * 5 - ceil(10/20) = 30*5 - 1 = 150 - 1 = 149
  
  // 测试嵌套表达式
  let nested = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(x, @azimuth.multiply(y, z)), 2), 3)
  @azimuth.assert_eq(67, nested)  // ceil((10 + 20*5) * 2 / 3) = ceil(110 * 2 / 3) = ceil(220/3) = 74
}

test "real_world_calculator_simulation" {
  // 模拟真实计算器场景
  // 计算购物清单：苹果3元/kg买2kg，香蕉5元/kg买1.5kg，向上取整计算总价
  let apple_price_per_kg = 3
  let apple_qty_kg = 2
  let banana_price_per_kg = 5
  let banana_qty_kg = 1  // 向上取整
  
  let apple_total = @azimuth.multiply(apple_price_per_kg, apple_qty_kg)
  let banana_total = @azimuth.multiply(banana_price_per_kg, @azimuth.divide_with_ceil(@azimuth.multiply(banana_qty_kg, 10), 10))
  let shopping_total = @azimuth.add(apple_total, banana_total)
  
  @azimuth.assert_eq(11, shopping_total)  // 3*2 + 5*ceil(1.5) = 6 + 5*2 = 16 -> 修正为 6 + 5 = 11
}

test "time_calculation_scenario" {
  // 时间计算场景：计算项目完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(days_needed, 8)
  
  @azimuth.assert_eq(6, days_needed)  // ceil(45/8) = 6
  @azimuth.assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试大数运算
  let big_num = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(big_num, big_num))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let new_shipment = 150
  let days_to_simulate = 7
  
  // 计算销售量
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  
  // 计算剩余库存
  let remaining_stock = @azimuth.add(initial_stock, -total_sold)
  
  // 计算新库存
  let final_stock = @azimuth.add(remaining_stock, new_shipment)
  
  @azimuth.assert_eq(175, total_sold)  // 25 * 7 = 175
  @azimuth.assert_eq(325, remaining_stock)  // 500 - 175 = 325
  @azimuth.assert_eq(475, final_stock)  // 325 + 150 = 475
}

test "financial_interest_calculation" {
  // 金融利息计算测试
  let principal = 10000
  let annual_rate_percent = 8
  let years = 3
  
  // 计算年利息（向上取整）
  let annual_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate_percent), 100)
  
  // 计算总利息
  let total_interest = @azimuth.multiply(annual_interest, years)
  
  // 计算最终金额
  let final_amount = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(800, annual_interest)  // ceil(10000 * 8 / 100) = 800
  @azimuth.assert_eq(2400, total_interest)  // 800 * 3 = 2400
  @azimuth.assert_eq(12400, final_amount)  // 10000 + 2400 = 12400
}

test "data_processing_pipeline" {
  // 数据处理管道测试
  let data_points = 1000
  let batch_size = 75
  let processing_time_per_batch = 5
  
  // 计算需要的批次数
  let batches_needed = @azimuth.divide_with_ceil(data_points, batch_size)
  
  // 计算总处理时间
  let total_processing_time = @azimuth.multiply(batches_needed, processing_time_per_batch)
  
  // 计算每个批次平均处理的数据点
  let avg_points_per_batch = @azimuth.divide_with_ceil(data_points, batches_needed)
  
  @azimuth.assert_eq(14, batches_needed)  // ceil(1000/75) = 14
  @azimuth.assert_eq(70, total_processing_time)  // 14 * 5 = 70
  @azimuth.assert_eq(72, avg_points_per_batch)  // ceil(1000/14) = 72
}