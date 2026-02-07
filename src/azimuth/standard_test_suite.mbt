// Azimuth 项目标准测试套件
// 包含10个高质量的 MoonBit 测试用例，使用标准 MoonBit 测试语法

test "arithmetic_sequence_operations" {
  // 等差数列运算测试
  let first_term = 3
  let common_diff = 4
  let n_terms = 5
  
  // 计算第n项: a_n = a_1 + (n-1)d
  let nth_term = @azimuth.add(first_term, @azimuth.multiply(@azimuth.subtract(n_terms, 1), common_diff))
  
  // 计算前n项和: S_n = n/2 * (2a_1 + (n-1)d)
  let sum_first_last = @azimuth.multiply(n_terms, @azimuth.add(first_term, nth_term))
  let series_sum = @azimuth.divide_with_ceil(sum_first_last, 2)
  
  @azimuth.assert_eq(nth_term, 19)
  @azimuth.assert_eq(series_sum, 55)
}

test "cartesian_coordinate_distance" {
  // 笛卡尔坐标系距离计算测试
  let x1 = 1
  let y1 = 2
  let x2 = 4
  let y2 = 6
  
  // 计算x和y方向的差值
  let delta_x = @azimuth.subtract(x2, x1)
  let delta_y = @azimuth.subtract(y2, y1)
  
  // 计算平方和（简化版本，不使用开方）
  let distance_squared = @azimuth.add(@azimuth.multiply(delta_x, delta_x), @azimuth.multiply(delta_y, delta_y))
  
  // 计算曼哈顿距离
  let manhattan_distance = @azimuth.add(abs_int(delta_x), abs_int(delta_y))
  
  @azimuth.assert_eq(delta_x, 3)
  @azimuth.assert_eq(delta_y, 4)
  @azimuth.assert_eq(distance_squared, 25)
  @azimuth.assert_eq(manhattan_distance, 7)
}

test "time_conversion_calculations" {
  // 时间单位转换计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = @azimuth.add(@azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.multiply(minutes, 60)), seconds)
  
  // 转换为总分钟数（向上取整）
  let total_minutes = @azimuth.divide_with_ceil(total_seconds, 60)
  
  // 转换为总小时数（向上取整）
  let total_hours = @azimuth.divide_with_ceil(total_seconds, 3600)
  
  @azimuth.assert_eq(total_seconds, 9930)
  @azimuth.assert_eq(total_minutes, 166)
  @azimuth.assert_eq(total_hours, 3)
}

test "percentage_calculations" {
  // 百分比计算测试
  let original_value = 250
  let percentage = 15
  
  // 计算百分比值
  let percentage_value = @azimuth.divide_with_ceil(@azimuth.multiply(original_value, percentage), 100)
  
  // 计算增加后的值
  let increased_value = @azimuth.add(original_value, percentage_value)
  
  // 计算减少后的值
  let decreased_value = @azimuth.subtract(original_value, percentage_value)
  
  // 计算增长百分比
  let growth_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(percentage_value, 100), original_value)
  
  @azimuth.assert_eq(percentage_value, 38)
  @azimuth.assert_eq(increased_value, 288)
  @azimuth.assert_eq(decreased_value, 212)
  @azimuth.assert_eq(growth_percentage, 15)
}

test "array_index_operations" {
  // 数组索引操作测试（模拟数组操作）
  let array_length = 10
  let start_index = 2
  let end_index = 7
  
  // 计算元素数量
  let element_count = @azimuth.subtract(end_index, start_index)
  
  // 计算中间索引
  let middle_index = @azimuth.divide_with_ceil(@azimuth.add(start_index, end_index), 2)
  
  // 计算从开始到结尾的步数
  let steps = @azimuth.subtract(end_index, start_index)
  
  // 验证索引边界
  let is_valid_start = start_index >= 0
  let is_valid_end = end_index < array_length
  
  @azimuth.assert_eq(element_count, 5)
  @azimuth.assert_eq(middle_index, 5)
  @azimuth.assert_eq(steps, 5)
  @azimuth.assert_eq(is_valid_start, true)
  @azimuth.assert_eq(is_valid_end, true)
}

test "loan_repayment_calculation" {
  // 贷款还款计算测试
  let principal = 10000
  let annual_rate = 6
  let loan_term_years = 3
  
  // 计算年利率金额
  let yearly_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  
  // 计算总利息
  let total_interest = @azimuth.multiply(yearly_interest, loan_term_years)
  
  // 计算总还款额
  let total_repayment = @azimuth.add(principal, total_interest)
  
  // 计算月还款额（向上取整）
  let total_months = @azimuth.multiply(loan_term_years, 12)
  let monthly_payment = @azimuth.divide_with_ceil(total_repayment, total_months)
  
  @azimuth.assert_eq(yearly_interest, 600)
  @azimuth.assert_eq(total_interest, 1800)
  @azimuth.assert_eq(total_repayment, 11800)
  @azimuth.assert_eq(monthly_payment, 328)
}

test "temperature_analysis" {
  // 温度分析测试
  let temps_week = [18, 22, 25, 19, 23, 20, 21]  // 一周的温度
  
  // 计算平均温度（模拟）
  let sum_temps = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(18, 22), 25), 19), 23), 20), 21)
  let avg_temp = @azimuth.divide_with_ceil(sum_temps, 7)
  
  // 计算最高温和最低温的差值
  let max_temp = 25
  let min_temp = 18
  let temp_range = @azimuth.subtract(max_temp, min_temp)
  
  // 计算温度波动指数
  let fluctuation_index = @azimuth.divide_with_ceil(@azimuth.multiply(temp_range, 100), avg_temp)
  
  @azimuth.assert_eq(sum_temps, 148)
  @azimuth.assert_eq(avg_temp, 22)
  @azimuth.assert_eq(temp_range, 7)
  @azimuth.assert_eq(fluctuation_index, 32)
}

test "shopping_cart_calculations" {
  // 购物车计算测试
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let item3_price = 8
  let item3_qty = 4
  
  // 计算各商品小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal3 = @azimuth.multiply(item3_price, item3_qty)
  
  // 计算商品总计
  let items_total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), subtotal3)
  
  // 计算折扣（满100减15）
  let discount_threshold = 100
  let discount_amount = 15
  let applied_discount = if(items_total >= discount_threshold) { discount_amount } else { 0 }
  
  // 计算最终总价
  let final_total = @azimuth.subtract(items_total, applied_discount)
  
  @azimuth.assert_eq(subtotal1, 58)
  @azimuth.assert_eq(subtotal2, 45)
  @azimuth.assert_eq(subtotal3, 32)
  @azimuth.assert_eq(items_total, 135)
  @azimuth.assert_eq(applied_discount, 15)
  @azimuth.assert_eq(final_total, 120)
}

test "speed_conversion_calculations" {
  // 速度单位转换计算测试
  let km_per_hour = 72
  
  // 转换为米每秒: m/s = km/h × 1000/3600
  let meters_per_second = @azimuth.divide_with_ceil(@azimuth.multiply(km_per_hour, 1000), 3600)
  
  // 转换为米每分钟: m/min = km/h × 1000/60
  let meters_per_minute = @azimuth.divide_with_ceil(@azimuth.multiply(km_per_hour, 1000), 60)
  
  // 计算行驶一定距离所需时间
  let distance_km = 180
  let hours_needed = @azimuth.divide_with_ceil(distance_km, km_per_hour)
  let minutes_needed = @azimuth.multiply(hours_needed, 60)
  
  @azimuth.assert_eq(meters_per_second, 20)
  @azimuth.assert_eq(meters_per_minute, 1200)
  @azimuth.assert_eq(hours_needed, 3)
  @azimuth.assert_eq(minutes_needed, 180)
}

test "data_storage_calculations" {
  // 数据存储计算测试
  let file_size_mb = 15
  let storage_capacity_gb = 2
  
  // 转换存储容量为MB
  let storage_capacity_mb = @azimuth.multiply(storage_capacity_gb, 1024)
  
  // 计算可存储文件数量
  let files_count = @azimuth.divide_with_ceil(storage_capacity_mb, file_size_mb)
  
  // 计算剩余空间
  let used_space = @azimuth.multiply(files_count, file_size_mb)
  let remaining_space = @azimuth.subtract(storage_capacity_mb, used_space)
  
  // 计算空间利用率
  let utilization_percent = @azimuth.divide_with_ceil(@azimuth.multiply(used_space, 100), storage_capacity_mb)
  
  @azimuth.assert_eq(storage_capacity_mb, 2048)
  @azimuth.assert_eq(files_count, 137)
  @azimuth.assert_eq(used_space, 2055)
  @azimuth.assert_eq(remaining_space, -7)
  @azimuth.assert_eq(utilization_percent, 101)
}

// 辅助函数：整数绝对值
fn abs_int(x : Int) -> Int {
  if x < 0 { @azimuth.subtract(0, x) } else { x }
}