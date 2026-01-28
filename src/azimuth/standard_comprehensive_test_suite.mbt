// 标准 MoonBit 综合测试套件
// 包含10个高质量的测试用例，全面测试 azimuth 库的核心功能

test "boundary_value_testing" {
  // 边界值测试：测试极大值和极小值
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))  // 接近Int最大值
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))  // 接近Int最小值
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))  // 最大值与相反数相加
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2147483647, 2147483647))  // 大数相除
}

test "mathematical_laws_verification" {
  // 数学定律验证：结合律和分配律
  let x = 8
  let y = 12
  let z = 15
  
  // 加法结合律：(x + y) + z = x + (y + z)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 分配律：x * (y + z) = x * y + x * z
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
}

test "financial_calculation_scenario" {
  // 金融计算场景：复利计算
  let principal = 10000
  let rate_percent = 5
  let _years = 3  // 使用下划线前缀表示未使用的变量
  
  // 第一年
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  
  @azimuth.assert_eq(10500, year1)
  @azimuth.assert_eq(11025, year2)
  @azimuth.assert_eq(11576, year3)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零安全性
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极端除法情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2147483647))  // 极小除以极大
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))  // 极大除以极小
}

test "string_processing_comprehensive" {
  // 综合字符串处理测试
  let username = "张三"
  let role = "开发者"
  let company = "MoonBit"
  
  // 复合字符串处理
  let full_title = username + " - " + role + " @ " + company
  let greeting = @azimuth.greet(full_title)
  
  @azimuth.assert_eq_string("Hello, 张三 - 开发者 @ MoonBit!", greeting)
  
  // 特殊字符和数字混合
  @azimuth.assert_eq_string("Hello, User2024!", @azimuth.greet("User2024"))
  @azimuth.assert_eq_string("Hello, test+example@domain.com!", @azimuth.greet("test+example@domain.com"))
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟测试
  let data_size = 10000
  let operations_per_item = 5
  let overhead = 150
  
  // 计算总操作数
  let total_operations = @azimuth.multiply(data_size, operations_per_item)
  let adjusted_operations = @azimuth.add(total_operations, overhead)
  
  // 计算处理时间（模拟）
  let base_time = @azimuth.divide_with_ceil(adjusted_operations, 1000)  // 毫秒
  let total_time = @azimuth.add(base_time, @azimuth.divide_with_ceil(overhead, 100))
  
  @azimuth.assert_eq(50000, total_operations)
  @azimuth.assert_eq(50150, adjusted_operations)
  @azimuth.assert_eq(52, total_time)
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let warehouse_capacity = 5000
  let current_stock = 3250
  let daily_sales = 75
  let weekly_shipment = 400
  
  // 计算剩余容量
  let remaining_capacity = @azimuth.add(warehouse_capacity, -current_stock)
  
  // 计算一周后的库存
  let weekly_sales = @azimuth.multiply(daily_sales, 7)
  let stock_after_week = @azimuth.add(current_stock, @azimuth.add(weekly_shipment, -weekly_sales))
  
  // 计算需要多少天到达容量限制
  let days_until_full = @azimuth.divide_with_ceil(remaining_capacity, daily_sales)
  
  @azimuth.assert_eq(1750, remaining_capacity)
  @azimuth.assert_eq(3775, stock_after_week)
  @azimuth.assert_eq(24, days_until_full)
}

test "complex_number_theory_operations" {
  // 复杂数论运算测试
  let start_number = 17
  
  // 计算前5个质数的和（简化版）
  let prime1 = 2
  let prime2 = 3
  let prime3 = 5
  let prime4 = 7
  let prime5 = 11
  
  let prime_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(prime1, prime2), prime3), prime4), prime5)
  
  // 计算平均值（向上取整）
  let prime_average = @azimuth.divide_with_ceil(prime_sum, 5)
  
  // 与起始数字的组合运算
  let complex_result = @azimuth.multiply(start_number, prime_average)
  
  @azimuth.assert_eq(28, prime_sum)
  @azimuth.assert_eq(6, prime_average)
  @azimuth.assert_eq(102, complex_result)
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  let q1_sales = 45000
  let q2_sales = 52000
  let q3_sales = 48000
  let q4_sales = 61000
  
  // 计算年度总销售额
  let annual_sales = @azimuth.add(@azimuth.add(@azimuth.add(q1_sales, q2_sales), q3_sales), q4_sales)
  
  // 计算季度平均销售额
  let quarterly_average = @azimuth.divide_with_ceil(annual_sales, 4)
  
  // 计算同比增长率（简化）
  let year_over_year_growth = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(annual_sales, -@azimuth.multiply(q1_sales, 4)), 100), @azimuth.multiply(q1_sales, 4))
  
  @azimuth.assert_eq(206000, annual_sales)
  @azimuth.assert_eq(51500, quarterly_average)
  @azimuth.assert_eq(15, year_over_year_growth)
}

test "engineering_calculation_precision" {
  // 工程计算精度测试
  let beam_length = 12000  // 毫米
  let support_count = 4
  let load_per_meter = 500  // 千克/米
  
  // 计算支撑点间距
  let support_spacing = @azimuth.divide_with_ceil(beam_length, support_count)
  
  // 计算总载荷
  let total_load = @azimuth.multiply(@azimuth.divide_with_ceil(beam_length, 1000), load_per_meter)
  
  // 计算每个支撑点的载荷（向上取整）
  let load_per_support = @azimuth.divide_with_ceil(total_load, support_count)
  
  @azimuth.assert_eq(3000, support_spacing)
  @azimuth.assert_eq(6000, total_load)
  @azimuth.assert_eq(1500, load_per_support)
}