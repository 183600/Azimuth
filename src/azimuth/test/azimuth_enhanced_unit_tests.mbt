// 增强的单元测试用例 - Azimuth 项目
// 包含边界测试、实际应用场景和数学性质验证

test "temperature_conversion" {
  // 温度转换测试：摄氏度到华氏度
  // F = C * 9/5 + 32，使用向上取整除法模拟 9/5
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, @azimuth.divide_with_ceil(9, 5)), 32)
  @azimuth.assert_eq(77, fahrenheit)  // 25 * 2 + 32 = 82 (使用ceil(9/5)=2)
  
  // 测试零度
  let freezing = @azimuth.add(@azimuth.multiply(0, @azimuth.divide_with_ceil(9, 5)), 32)
  @azimuth.assert_eq(32, freezing)
  
  // 测试负温度
  let negative_celsius = -10
  let negative_fahrenheit = @azimuth.add(@azimuth.multiply(negative_celsius, @azimuth.divide_with_ceil(9, 5)), 32)
  @azimuth.assert_eq(12, negative_fahrenheit)  // -10 * 2 + 32 = 12
}

test "array_index_calculation" {
  // 数组索引计算测试
  let page_size = 10
  let total_items = 47
  
  // 计算最后一页的起始索引
  let last_page_start = @azimuth.divide_with_ceil(total_items, page_size) - 1
  @azimuth.assert_eq(4, last_page_start)  // ceil(47/10) - 1 = 5 - 1 = 4
  
  // 计算总页数
  let total_pages = @azimuth.divide_with_ceil(total_items, page_size)
  @azimuth.assert_eq(5, total_pages)
  
  // 计算最后一页的项目数
  let last_page_items = total_items - @azimuth.multiply(last_page_start, page_size)
  @azimuth.assert_eq(7, last_page_items)  // 47 - 4*10 = 7
}

test "loan_interest_calculation" {
  // 贷款利息计算测试
  let principal = 10000
  let annual_rate_percent = 6
  let years = 3
  
  // 简单利息计算（使用向上取整）
  let yearly_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate_percent), 100)
  let total_interest = @azimuth.multiply(yearly_interest, years)
  let total_amount = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(600, yearly_interest)  // ceil(10000*6/100) = 600
  @azimuth.assert_eq(1800, total_interest)  // 600*3 = 1800
  @azimuth.assert_eq(11800, total_amount)   // 10000 + 1800 = 11800
}

test "game_score_system" {
  // 游戏评分系统测试
  let base_score = 100
  let level_multiplier = 3
  let bonus_per_level = 50
  let current_level = 7
  
  // 计算基础分数（等级倍数）
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  
  // 计算奖励分数
  let bonus_score = @azimuth.multiply(bonus_per_level, current_level)
  
  // 计算总分
  let total_score = @azimuth.add(level_score, bonus_score)
  
  @azimuth.assert_eq(300, level_score)   // 100 * 3 = 300
  @azimuth.assert_eq(350, bonus_score)   // 50 * 7 = 350
  @azimuth.assert_eq(650, total_score)   // 300 + 350 = 650
}

test "resource_allocation" {
  // 资源分配测试
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 42
  
  // 计算总需求
  let total_needed = @azimuth.add(@azimuth.add(project_a_needs, project_b_needs), project_c_needs)
  
  // 计算剩余资源
  let remaining = @azimuth.add(total_resources, 0 - total_needed)
  
  // 计算每个项目获得的资源比例（向上取整）
  let project_a_ratio = @azimuth.divide_with_ceil(@azimuth.multiply(project_a_needs, 100), total_needed)
  let project_b_ratio = @azimuth.divide_with_ceil(@azimuth.multiply(project_b_needs, 100), total_needed)
  let project_c_ratio = @azimuth.divide_with_ceil(@azimuth.multiply(project_c_needs, 100), total_needed)
  
  @azimuth.assert_eq(105, total_needed)   // 35 + 28 + 42 = 105
  @azimuth.assert_eq(-5, remaining)       // 100 - 105 = -5
  @azimuth.assert_eq(34, project_a_ratio)  // ceil(35*100/105) = 34
  @azimuth.assert_eq(27, project_b_ratio)  // ceil(28*100/105) = 27
  @azimuth.assert_eq(41, project_c_ratio)  // ceil(42*100/105) = 41
}

test "time_zone_conversion" {
  // 时区转换测试
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与UTC的时差
  
  // 计算UTC时间
  let utc_time = @azimuth.add(local_time, 0 - time_difference)
  let adjusted_utc = if utc_time < 0 { @azimuth.add(utc_time, 24) } else { utc_time }
  
  // 计算其他时区时间
  let timezone_plus3 = @azimuth.add(adjusted_utc, 3)
  let adjusted_plus3 = if timezone_plus3 >= 24 { @azimuth.add(timezone_plus3, 0 - 24) } else { timezone_plus3 }
  
  let timezone_minus5 = @azimuth.add(adjusted_utc, 0 - 5)
  let adjusted_minus5 = if timezone_minus5 < 0 { @azimuth.add(timezone_minus5, 24) } else { timezone_minus5 }
  
  @azimuth.assert_eq(6, adjusted_utc)    // 14 - 8 = 6
  @azimuth.assert_eq(9, adjusted_plus3)  // 6 + 3 = 9
  @azimuth.assert_eq(1, adjusted_minus5) // 6 - 5 = 1
}

test "batch_processing" {
  // 批处理计算测试
  let total_records = 1250
  let batch_size = 100
  
  // 计算批次数
  let total_batches = @azimuth.divide_with_ceil(total_records, batch_size)
  
  // 计算完整批次
  let full_batches = total_records / batch_size
  
  // 计算最后一批的记录数
  let last_batch_size = @azimuth.add(total_records, 0 - @azimuth.multiply(full_batches, batch_size))
  
  // 计算处理时间（假设每批次5分钟）
  let processing_time_minutes = @azimuth.multiply(total_batches, 5)
  let processing_time_hours = @azimuth.divide_with_ceil(processing_time_minutes, 60)
  
  @azimuth.assert_eq(13, total_batches)      // ceil(1250/100) = 13
  @azimuth.assert_eq(12, full_batches)       // 1250/100 = 12
  @azimuth.assert_eq(50, last_batch_size)    // 1250 - 12*100 = 50
  @azimuth.assert_eq(65, processing_time_minutes)  // 13*5 = 65
  @azimuth.assert_eq(2, processing_time_hours)     // ceil(65/60) = 2
}

test "greeting_template_system" {
  // 问候模板系统测试
  let user_name = "张三"
  let time_of_day = "早上好"
  let location = "北京"
  
  // 创建完整的问候语
  let time_greeting = @azimuth.greet(time_of_day)
  let name_part = ", " + user_name + "!"
  let location_part = "欢迎来到" + location + "。"
  
  // 组合完整问候（简化版本，实际字符串拼接在MoonBit中可能需要其他方式）
  let full_greeting = @azimuth.greet(time_of_day + user_name)
  
  @azimuth.assert_eq_string("Hello, 早上好张三!", full_greeting)
  
  // 测试不同时间段的问候
  let morning_greeting = @azimuth.greet("早上好")
  let afternoon_greeting = @azimuth.greet("下午好")
  let evening_greeting = @azimuth.greet("晚上好")
  
  @azimuth.assert_eq_string("Hello, 早上好!", morning_greeting)
  @azimuth.assert_eq_string("Hello, 下午好!", afternoon_greeting)
  @azimuth.assert_eq_string("Hello, 晚上好!", evening_greeting)
}