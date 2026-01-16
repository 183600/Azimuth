// 用户自定义 MoonBit 测试用例
// 包含实际应用场景和有趣的测试案例

test "binary_number_simulation" {
  // 模拟二进制数运算
  // 将二进制 1010 (10) 和 1100 (12) 转换为十进制并进行运算
  
  // 模拟二进制转十进制：1010 = 1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  let binary1 = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(0, 4)), @azimuth.add(@azimuth.multiply(1, 2), 0))
  @azimuth.assert_eq(10, binary1)
  
  // 模拟二进制转十进制：1100 = 1×2³ + 1×2² + 0×2¹ + 0×2⁰ = 8 + 4 + 0 + 0 = 12
  let binary2 = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(1, 4)), @azimuth.add(@azimuth.multiply(0, 2), 0))
  @azimuth.assert_eq(12, binary2)
  
  // 二进制加法：1010 + 1100 = 10110 (22)
  let sum = @azimuth.add(binary1, binary2)
  @azimuth.assert_eq(22, sum)
  
  // 二进制乘法：1010 × 1100 = 1111000 (120)
  let product = @azimuth.multiply(binary1, binary2)
  @azimuth.assert_eq(120, product)
}

test "calendar_date_calculations" {
  // 测试日历日期计算
  // 假设一年有365天，一周有7天
  
  let days_in_year = 365
  let days_in_week = 7
  let current_day = 45  // 第45天
  
  // 计算当前是第几周（整数除法）
  let week_number = current_day / days_in_week
  @azimuth.assert_eq(6, week_number)  // 第6周
  
  // 计算当前是周几（取余）
  let day_of_week = current_day % days_in_week
  @azimuth.assert_eq(3, day_of_week)  // 第3天（周四，如果周日是0）
  
  // 计算距离年底还有多少天
  let days_until_year_end = @azimuth.add(days_in_year, -current_day)
  @azimuth.assert_eq(320, days_until_year_end)
  
  // 计算距离下个周年还有多少天
  let days_until_next_anniversary = days_until_year_end
  @azimuth.assert_eq(320, days_until_next_anniversary)
}

test "game_inventory_management" {
  // 测试游戏物品栏管理
  let max_stack_size = 64
  let item_count = 150
  let stacks_needed = @azimuth.divide_with_ceil(item_count, max_stack_size)
  
  // 验证需要多少个堆栈
  @azimuth.assert_eq(3, stacks_needed)
  
  // 计算最后一个堆栈的物品数量
  let full_stacks = item_count / max_stack_size
  let last_stack_count = item_count - @azimuth.multiply(full_stacks, max_stack_size)
  @azimuth.assert_eq(22, last_stack_count)
  
  // 测试添加物品
  let additional_items = 50
  let new_total = @azimuth.add(item_count, additional_items)
  let new_stacks_needed = @azimuth.divide_with_ceil(new_total, max_stack_size)
  @azimuth.assert_eq(4, new_stacks_needed)
}

test "network_data_packet_simulation" {
  // 模拟网络数据包处理
  let header_size = 20
  let max_payload_size = 1460
  let total_data_size = 5000
  
  // 计算需要多少个数据包
  let payload_per_packet = max_payload_size
  let packets_needed = @azimuth.divide_with_ceil(total_data_size, payload_per_packet)
  @azimuth.assert_eq(4, packets_needed)
  
  // 计算最后一个数据包的载荷大小
  let full_packets = total_data_size / payload_per_packet
  let last_packet_payload = total_data_size - @azimuth.multiply(full_packets, payload_per_packet)
  @azimuth.assert_eq(620, last_packet_payload)
  
  // 计算总传输大小（包括头部）
  let total_transferred = @azimuth.add(@azimuth.multiply(packets_needed, header_size), total_data_size)
  @azimuth.assert_eq(5080, total_transferred)
}

test "battery_life_calculation" {
  // 测试电池续航计算
  let battery_capacity_mah = 4000  // 电池容量：4000mAh
  let device_consumption_ma = 200  // 设备功耗：200mA
  let screen_on_consumption_ma = 300  // 屏幕开启时功耗：300mA
  let screen_on_hours = 4  // 每天屏幕开启4小时
  
  // 计算屏幕关闭时的续航时间
  let screen_off_hours = 24 - screen_on_hours
  let daily_consumption = @azimuth.add(@azimuth.multiply(screen_on_hours, screen_on_consumption_ma), @azimuth.multiply(screen_off_hours, device_consumption_ma))
  @azimuth.assert_eq(5200, daily_consumption)  // 4×300 + 20×200 = 1200 + 4000 = 5200mAh
  
  // 计算实际续航天数
  let battery_life_days = battery_capacity_mah / daily_consumption
  @azimuth.assert_eq(0, battery_life_days)  // 不足一天
  
  // 计算实际续航小时数
  let battery_life_hours = @azimuth.multiply(battery_life_days, 24)
  let remaining_hours = @azimuth.divide_with_ceil(@azimuth.add(battery_capacity_mah, -@azimuth.multiply(battery_life_days, daily_consumption)), device_consumption_ma)
  let total_hours = @azimuth.add(battery_life_hours, remaining_hours)
  @azimuth.assert_eq(20, total_hours)  // 约20小时
}

test "color_rgb_conversion" {
  // 测试RGB颜色转换
  // RGB(255, 128, 64) 转换为十六进制表示
  
  let red = 255
  let green = 128
  let blue = 64
  
  // 计算十六进制值（简化为整数计算）
  // 每个颜色分量可以表示为两位十六进制数
  let red_hex_high = red / 16  // 高位
  let red_hex_low = red % 16   // 低位
  @azimuth.assert_eq(15, red_hex_high)  // F
  @azimuth.assert_eq(15, red_hex_low)   // F
  
  let green_hex_high = green / 16
  let green_hex_low = green % 16
  @azimuth.assert_eq(8, green_hex_high)   // 8
  @azimuth.assert_eq(0, green_hex_low)    // 0
  
  let blue_hex_high = blue / 16
  let blue_hex_low = blue % 16
  @azimuth.assert_eq(4, blue_hex_high)    // 4
  @azimuth.assert_eq(0, blue_hex_low)     // 0
  
  // 计算整数值（模拟十六进制转换）
  let color_value = @azimuth.add(@azimuth.add(@azimuth.multiply(red, 65536), @azimuth.multiply(green, 256)), blue)
  @azimuth.assert_eq(16744448, color_value)
}

test "compression_ratio_calculation" {
  // 测试压缩比计算
  let original_size = 1024  // 原始大小：1KB
  let compressed_size = 256 // 压缩后大小：256B
  
  // 计算压缩比
  let compression_ratio = @azimuth.multiply(original_size, 100) / compressed_size
  @azimuth.assert_eq(400, compression_ratio)  // 4:1压缩比，即75%压缩率
  
  // 计算节省的空间
  let space_saved = @azimuth.add(original_size, -compressed_size)
  @azimuth.assert_eq(768, space_saved)
  
  // 计算节省的百分比
  let space_saved_percent = @azimuth.multiply(space_saved, 100) / original_size
  @azimuth.assert_eq(75, space_saved_percent)  // 节省75%空间
  
  // 测试多个文件的压缩
  let file1_original = 2048
  let file2_original = 1536
  let file1_compressed = 512
  let file2_compressed = 384
  
  let total_original = @azimuth.add(file1_original, file2_original)
  let total_compressed = @azimuth.add(file1_compressed, file2_compressed)
  let total_compression_ratio = @azimuth.multiply(total_original, 100) / total_compressed
  
  @azimuth.assert_eq(400, total_compression_ratio)  // 总体压缩比仍为4:1
}

test "loan_amortization_calculation" {
  // 测试贷款分期计算
  let principal = 120000  // 贷款本金：12万
  let annual_rate = 6     // 年利率：6%
  let loan_term_years = 30 // 贷款期限：30年
  
  // 简化的月供计算（不使用复利公式）
  // 总利息 = 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), loan_term_years)
  @azimuth.assert_eq(216000, total_interest)  // 120000 × 6 × 30 = 216000
  
  // 总还款额
  let total_payment = @azimuth.add(principal, total_interest)
  @azimuth.assert_eq(336000, total_payment)
  
  // 月供（简化计算）
  let total_months = @azimuth.multiply(loan_term_years, 12)
  let monthly_payment = total_payment / total_months
  @azimuth.assert_eq(933, monthly_payment)  // 336000 ÷ 360 = 933.33，取整数部分
  
  // 计算前5年还款总额
  let first_5_years_months = @azimuth.multiply(5, 12)
  let first_5_years_payment = @azimuth.multiply(monthly_payment, first_5_years_months)
  @azimuth.assert_eq(55980, first_5_years_payment)
}

test "fitness_calorie_calculation" {
  // 测试健身卡路里计算
  let weight_kg = 70      // 体重：70kg
  let exercise_minutes = 30 // 运动时间：30分钟
  let met_value = 8        // MET值（代谢当量）：8（中等强度运动）
  
  // 简化的卡路里计算：卡路里 = MET × 体重(kg) × 时间(小时)
  let exercise_hours = exercise_minutes / 60
  let calories_burned = @azimuth.multiply(@azimuth.multiply(met_value, weight_kg), exercise_hours)
  @azimuth.assert_eq(280, calories_burned)  // 8 × 70 × 0.5 = 280卡路里
  
  // 计算每周运动消耗（假设每周运动3次）
  let weekly_sessions = 3
  let weekly_calories = @azimuth.multiply(calories_burned, weekly_sessions)
  @azimuth.assert_eq(840, weekly_calories)
  
  // 计算每月运动消耗（假设每月4周）
  let monthly_calories = @azimuth.multiply(weekly_calories, 4)
  @azimuth.assert_eq(3360, monthly_calories)
  
  // 计算减重所需时间（简化：1kg脂肪≈7700卡路里）
  let target_weight_loss_kg = 2
  let calories_needed = @azimuth.multiply(target_weight_loss_kg, 7700)
  let days_needed = @azimuth.divide_with_ceil(calories_needed, weekly_calories / 7)
  @azimuth.assert_eq(16, days_needed)  // 约16天
}

test "digital_storage_conversion" {
  // 测试数字存储单位转换
  let file_size_mb = 1024  // 文件大小：1024MB
  
  // 转换为GB
  let file_size_gb = file_size_mb / 1024
  @azimuth.assert_eq(1, file_size_gb)
  
  // 转换为KB
  let file_size_kb = @azimuth.multiply(file_size_mb, 1024)
  @azimuth.assert_eq(1048576, file_size_kb)
  
  // 转换为字节
  let file_size_bytes = @azimuth.multiply(file_size_kb, 1024)
  @azimuth.assert_eq(1073741824, file_size_bytes)
  
  // 计算传输时间（假设传输速度为10MB/s）
  let transfer_speed_mb_per_sec = 10
  let transfer_time_seconds = file_size_mb / transfer_speed_mb_per_sec
  @azimuth.assert_eq(102, transfer_time_seconds)
  
  // 转换为分钟和秒
  let transfer_time_minutes = transfer_time_seconds / 60
  let remaining_seconds = transfer_time_seconds % 60
  @azimuth.assert_eq(1, transfer_time_minutes)
  @azimuth.assert_eq(42, remaining_seconds)
}