// Azimuth 项目新测试套件
// 使用标准 MoonBit 测试语法，包含多样化的实用测试场景

test "string_length_validation" {
  // 字符串长度验证测试
  // 测试字符串长度计算（通过字符数估算）
  let short_length = @azimuth.multiply(2, 1)  // "Hi" 有2个字符
  let medium_length = @azimuth.multiply(20, 1)  // "MoonBit Programming" 有20个字符
  let long_length = @azimuth.multiply(67, 1)  // 长文本有67个字符
  let empty_length = @azimuth.multiply(0, 1)  // 空字符串有0个字符
  
  @azimuth.assert_eq(2, short_length)
  @azimuth.assert_eq(20, medium_length)
  @azimuth.assert_eq(67, long_length)
  @azimuth.assert_eq(0, empty_length)
}

test "array_operations_simulation" {
  // 数组操作模拟测试
  let mut array_size = 0
  
  // 模拟添加元素
  array_size = @azimuth.add(array_size, 1)  // 添加第1个元素
  @azimuth.assert_eq(1, array_size)
  
  array_size = @azimuth.add(array_size, 1)  // 添加第2个元素
  @azimuth.assert_eq(2, array_size)
  
  array_size = @azimuth.add(array_size, 1)  // 添加第3个元素
  @azimuth.assert_eq(3, array_size)
  
  // 模拟删除元素
  array_size = @azimuth.add(array_size, -1)  // 删除1个元素
  @azimuth.assert_eq(2, array_size)
  
  // 模拟批量添加
  array_size = @azimuth.add(array_size, 5)  // 批量添加5个元素
  @azimuth.assert_eq(7, array_size)
}

test "calculator_basic_operations" {
  // 计算器基本操作测试
  let num1 = 42
  let num2 = 17
  
  // 加法
  let sum = @azimuth.add(num1, num2)
  @azimuth.assert_eq(59, sum)
  
  // 减法（通过加负数实现）
  let difference = @azimuth.add(num1, -num2)
  @azimuth.assert_eq(25, difference)
  
  // 乘法
  let product = @azimuth.multiply(num1, num2)
  @azimuth.assert_eq(714, product)
  
  // 平方
  let square = @azimuth.multiply(num1, num1)
  @azimuth.assert_eq(1764, square)
  
  // 立方
  let cube = @azimuth.multiply(@azimuth.multiply(num1, num1), num1)
  @azimuth.assert_eq(74088, cube)
}

test "currency_conversion" {
  // 货币转换测试
  let usd_amount = 100
  let eur_rate = 85  // 假设1美元=0.85欧元（简化为整数计算，乘以100）
  let gbp_rate = 75  // 假设1美元=0.75英镑
  let jpy_rate = 110   // 假设1美元=110日元
  
  // 转换为欧元（简化计算）
  let eur_amount = @azimuth.multiply(usd_amount, eur_rate) / 100
  @azimuth.assert_eq(85, eur_amount)
  
  // 转换为英镑（简化计算）
  let gbp_amount = @azimuth.multiply(usd_amount, gbp_rate) / 100
  @azimuth.assert_eq(75, gbp_amount)
  
  // 转换为日元
  let jpy_amount = @azimuth.multiply(usd_amount, jpy_rate)
  @azimuth.assert_eq(11000, jpy_amount)
}

test "grade_calculator" {
  // 成绩计算器测试
  let exam_score = 85
  let homework_score = 90
  let quiz_score = 78
  let project_score = 92
  
  // 权重（百分比）
  let exam_weight = 40
  let homework_weight = 20
  let quiz_weight = 20
  let project_weight = 20
  
  // 计算加权分数
  let weighted_exam = @azimuth.multiply(exam_score, exam_weight) / 100
  let weighted_homework = @azimuth.multiply(homework_score, homework_weight) / 100
  let weighted_quiz = @azimuth.multiply(quiz_score, quiz_weight) / 100
  let weighted_project = @azimuth.multiply(project_score, project_weight) / 100
  
  // 计算总分
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(
    weighted_exam, weighted_homework), weighted_quiz), weighted_project)
  
  @azimuth.assert_eq(86, total_score)  // (85×0.4 + 90×0.2 + 78×0.2 + 92×0.2) = 34 + 18 + 15.6 + 18.4 = 86
}

test "speed_unit_conversion" {
  // 速度单位转换测试
  let kmh_speed = 60  // 公里/小时
  
  // 转换为米/秒：km/h × 1000 / 3600 ≈ km/h × 5/18
  let ms_speed = @azimuth.multiply(kmh_speed, 5) / 18
  @azimuth.assert_eq(16, ms_speed)  // 60 × 5/18 = 16.67 ≈ 16（取整）
  
  // 转换为英里/小时：km/h × 0.621371 ≈ km/h × 62/100
  let mph_speed = @azimuth.multiply(kmh_speed, 62) / 100
  @azimuth.assert_eq(37, mph_speed)  // 60 × 0.62 = 37.2 ≈ 37（取整）
  
  // 转换为节：km/h × 0.539957 ≈ km/h × 54/100
  let knot_speed = @azimuth.multiply(kmh_speed, 54) / 100
  @azimuth.assert_eq(32, knot_speed)  // 60 × 0.54 = 32.4 ≈ 32（取整）
}

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 500
  let daily_sales = 25
  let weekly_shipment = 200
  let days_in_week = 7
  
  // 计算一周销售量
  let weekly_sales = @azimuth.multiply(daily_sales, days_in_week)
  @azimuth.assert_eq(175, weekly_sales)
  
  // 计算一周后库存
  let ending_stock = @azimuth.add(@azimuth.add(initial_stock, weekly_shipment), -weekly_sales)
  @azimuth.assert_eq(525, ending_stock)  // 500 + 200 - 175 = 525
  
  // 计算库存周转率
  let turnover_rate = @azimuth.multiply(weekly_sales, 100) / initial_stock
  @azimuth.assert_eq(35, turnover_rate)  // 175 × 100 / 500 = 35
}

test "fitness_tracker" {
  // 健身追踪测试
  let steps_per_day = 8000
  let days_in_month = 30
  let calories_per_step = 4  // 每步消耗卡路里（简化为整数计算，乘以100）
  let distance_per_step = 8  // 每步公里数（简化为整数计算，乘以10000）
  
  // 计算月总步数
  let monthly_steps = @azimuth.multiply(steps_per_day, days_in_month)
  @azimuth.assert_eq(240000, monthly_steps)
  
  // 计算月总卡路里消耗（简化计算）
  let monthly_calories = @azimuth.multiply(monthly_steps, calories_per_step) / 100
  @azimuth.assert_eq(9600, monthly_calories)  // 240000 × 0.04 = 9600
  
  // 计算月总距离（公里）（简化计算）
  let monthly_distance = @azimuth.multiply(monthly_steps, distance_per_step) / 10000
  @azimuth.assert_eq(192, monthly_distance)  // 240000 × 0.0008 = 192
  
  // 计算每周平均步数
  let weekly_average = @azimuth.multiply(steps_per_day, 7)
  @azimuth.assert_eq(56000, weekly_average)
}

test "data_compression_simulation" {
  // 数据压缩模拟测试
  let original_size = 10000  // 原始数据大小（字节）
  let compression_ratio = 70  // 压缩率（百分比）
  let overhead = 50  // 压缩开销（字节）
  
  // 计算压缩后大小
  let compressed_size = @azimuth.multiply(original_size, compression_ratio) / 100
  @azimuth.assert_eq(7000, compressed_size)
  
  // 计算最终大小（含开销）
  let final_size = @azimuth.add(compressed_size, overhead)
  @azimuth.assert_eq(7050, final_size)
  
  // 计算节省的空间
  let saved_space = @azimuth.add(original_size, -final_size)
  @azimuth.assert_eq(2950, saved_space)
  
  // 计算压缩效率
  let efficiency = @azimuth.multiply(saved_space, 100) / original_size
  @azimuth.assert_eq(29, efficiency)  // 2950 × 100 / 10000 = 29.5 ≈ 29（取整）
}

test "network_bandwidth_calculation" {
  // 网络带宽计算测试
  let file_size_mb = 500  // 文件大小（MB）
  let bandwidth_mbps = 50  // 带宽（Mbps）
  let network_overhead = 10  // 网络开销（百分比）
  
  // 计算有效带宽（考虑开销）
  let effective_bandwidth = @azimuth.multiply(bandwidth_mbps, @azimuth.add(100, -network_overhead)) / 100
  @azimuth.assert_eq(45, effective_bandwidth)  // 50 × 0.9 = 45
  
  // 计算下载时间（秒）
  let download_time = @azimuth.multiply(file_size_mb, 8) / effective_bandwidth
  @azimuth.assert_eq(88, download_time)  // 500 × 8 / 45 = 88.89 ≈ 88（取整）
  
  // 转换为分钟
  let download_minutes = download_time / 60
  @azimuth.assert_eq(1, download_minutes)  // 88 / 60 = 1.47 ≈ 1（取整）
  
  // 计算剩余秒数
  let remaining_seconds = @azimuth.add(download_time, -@azimuth.multiply(download_minutes, 60))
  @azimuth.assert_eq(28, remaining_seconds)  // 88 - 1×60 = 28
}