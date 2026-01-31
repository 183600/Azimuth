// 新的高质量 MoonBit 测试用例 - 针对 azimuth 库
// 使用完全限定的函数名来引用 azimuth 库中的函数

test "mathematical_identity_properties" {
  // 测试数学单位元性质
  // 加法单位元：a + 0 = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法单位元：a * 1 = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 完全整除的情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(21, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))
  
  // 需要向上取整的情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(8, @azimuth.divide_with_ceil(15, 2))
  
  // 零除数安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-50, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  // 空字符串和单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  
  // 特殊字符和数字
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(500, result1)  // (10 + 15) * 20 = 25 * 20 = 500
  
  // 测试 x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(170, result2)  // 10 * 15 + 20 = 150 + 20 = 170
}

test "real_world_financial_calculation" {
  // 实际金融计算场景：计算贷款月供
  let principal = 200000  // 本金
  let annual_rate = 5     // 年利率百分比
  let years = 30          // 贷款年限
  
  // 简化计算：月供 = (本金 + 总利息) / (年数 * 12)
  let total_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let total_with_interest = @azimuth.add(principal, total_interest)
  let total_months = @azimuth.multiply(years, 12)
  let monthly_payment = @azimuth.divide_with_ceil(total_with_interest, total_months)
  
  @azimuth.assert_eq(10000, total_interest)
  @azimuth.assert_eq(210000, total_with_interest)
  @azimuth.assert_eq(360, total_months)
  @azimuth.assert_eq(584, monthly_payment)  // ceil(210000/360) = 584
}

test "inventory_management_calculation" {
  // 库存管理计算：计算补货需求
  let current_stock = 150
  let daily_sales = 12
  let lead_time_days = 7  // 供应商交货时间
  let safety_stock = 20   // 安全库存
  
  // 计算在交货期间的销售量
  let sales_during_lead = @azimuth.multiply(daily_sales, lead_time_days)
  
  // 计算再订货点
  let reorder_point = @azimuth.add(sales_during_lead, safety_stock)
  
  // 计算需要补货的数量
  let reorder_quantity = if current_stock <= reorder_point {
    @azimuth.multiply(daily_sales, 30)  // 订购30天的库存
  } else {
    0
  }
  
  @azimuth.assert_eq(84, sales_during_lead)
  @azimuth.assert_eq(104, reorder_point)
  @azimuth.assert_eq(360, reorder_quantity)
}

test "data_processing_simulation" {
  // 数据处理模拟：批量处理计算
  let batch_size = 1000
  let processing_time_per_item = 3  // 毫秒
  let overhead_per_batch = 50       // 每批次开销（毫秒）
  
  // 计算处理时间
  let base_processing_time = @azimuth.multiply(batch_size, processing_time_per_item)
  let total_batch_time = @azimuth.add(base_processing_time, overhead_per_batch)
  
  // 计算每小时可以处理的批次
  let ms_per_hour = 3600000
  let batches_per_hour = @azimuth.divide_with_ceil(ms_per_hour, total_batch_time)
  
  // 计算每小时可以处理的项目数
  let items_per_hour = @azimuth.multiply(batches_per_hour, batch_size)
  
  @azimuth.assert_eq(3000, base_processing_time)
  @azimuth.assert_eq(3050, total_batch_time)
  @azimuth.assert_eq(1181, batches_per_hour)
  @azimuth.assert_eq(1181000, items_per_hour)
}

test "engineering_tolerance_calculation" {
  // 工程容差计算：零件尺寸验证
  let nominal_dimension = 100  // 公称尺寸
  let positive_tolerance = 2   // 正公差
  let negative_tolerance = 1   // 负公差
  let measured_value = 101     // 测量值
  
  // 计算上限和下限
  let upper_limit = @azimuth.add(nominal_dimension, positive_tolerance)
  let lower_limit = @azimuth.add(nominal_dimension, -negative_tolerance)
  
  // 检查是否在容差范围内
  let is_within_tolerance = 
    if measured_value >= lower_limit && measured_value <= upper_limit {
      1  // 合格
    } else {
      0  // 不合格
    }
  
  // 计算偏差
  let deviation = @azimuth.add(measured_value, -nominal_dimension)
  
  @azimuth.assert_eq(102, upper_limit)
  @azimuth.assert_eq(99, lower_limit)
  @azimuth.assert_eq(1, is_within_tolerance)
  @azimuth.assert_eq(1, deviation)
}

test "statistical_analysis_basic" {
  // 基础统计分析
  let data_values = [15, 22, 18, 25, 20]  // 数据集
  let n = 5  // 数据点数量
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data_values[0], data_values[1]), data_values[2]), data_values[3]), data_values[4])
  
  // 计算平均值（向上取整）
  let average = @azimuth.divide_with_ceil(sum, n)
  
  // 计算最大值和最小值
  let max_value = 25
  let min_value = 15
  
  // 计算范围
  let range = @azimuth.add(max_value, -min_value)
  
  @azimuth.assert_eq(100, sum)
  @azimuth.assert_eq(20, average)
  @azimuth.assert_eq(10, range)
}

test "energy_efficiency_calculation" {
  // 能效计算：设备能耗评估
  let power_rating_watts = 1500    // 额定功率（瓦）
  let hours_per_day = 8           // 每天使用小时数
  let days_per_month = 30         // 每月天数
  let cost_per_kwh = 12           // 每千瓦时费用（分）
  
  // 计算每日能耗（瓦时）
  let daily_consumption_wh = @azimuth.multiply(power_rating_watts, hours_per_day)
  
  // 转换为千瓦时
  let daily_consumption_kwh = @azimuth.divide_with_ceil(daily_consumption_wh, 1000)
  
  // 计算每月能耗
  let monthly_consumption_kwh = @azimuth.multiply(daily_consumption_kwh, days_per_month)
  
  // 计算每月费用
  let monthly_cost_cents = @azimuth.multiply(monthly_consumption_kwh, cost_per_kwh)
  
  // 转换为元（向上取整）
  let monthly_cost_yuan = @azimuth.divide_with_ceil(monthly_cost_cents, 100)
  
  @azimuth.assert_eq(12000, daily_consumption_wh)
  @azimuth.assert_eq(12, daily_consumption_kwh)
  @azimuth.assert_eq(360, monthly_consumption_kwh)
  @azimuth.assert_eq(4320, monthly_cost_cents)
  @azimuth.assert_eq(44, monthly_cost_yuan)
}