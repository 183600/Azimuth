// 专业级 MoonBit 测试用例 - 8个高质量测试用例
// 覆盖算法、数据结构、金融计算、科学计算等领域

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  // 使用基本的算术运算模拟递归计算
  
  // F(0) = 0, F(1) = 1
  let f0 = 0
  let f1 = 1
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let f2 = add(f1, f0)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = add(f2, f1)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = add(f3, f2)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = add(f4, f3)
  
  // F(6) = F(5) + F(4) = 5 + 3 = 8
  let f6 = add(f5, f4)
  
  // F(7) = F(6) + F(5) = 8 + 5 = 13
  let f7 = add(f6, f5)
  
  // F(8) = F(7) + F(6) = 13 + 8 = 21
  let f8 = add(f7, f6)
  
  assert_eq(f2, 1)
  assert_eq(f3, 2)
  assert_eq(f4, 3)
  assert_eq(f5, 5)
  assert_eq(f6, 8)
  assert_eq(f7, 13)
  assert_eq(f8, 21)
}

test "compound_interest_calculation" {
  // 复利计算测试 - 金融应用场景
  let principal = 10000  // 本金
  let rate = 5           // 年利率百分比
  let years = 3          // 投资年限
  
  // 第一年：本金 + 利息
  let year1_interest = divide_with_ceil(multiply(principal, rate), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年：第一年总额 + 第二年利息
  let year2_interest = divide_with_ceil(multiply(year1_total, rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年：第二年总额 + 第三年利息
  let year3_interest = divide_with_ceil(multiply(year2_total, rate), 100)
  let year3_total = add(year2_total, year3_interest)
  
  // 计算总收益
  let total_profit = subtract(year3_total, principal)
  
  assert_eq(year1_total, 10500)
  assert_eq(year2_total, 11025)
  assert_eq(year3_total, 11576)
  assert_eq(total_profit, 1576)
}

test "binary_representation_simulation" {
  // 二进制表示模拟测试 - 计算机科学基础
  let decimal_number = 42
  
  // 分解为二进制位：42 = 32 + 8 + 2 = 2^5 + 2^3 + 2^1
  let bit5 = multiply(1, 32)  // 2^5
  let bit4 = multiply(0, 16)  // 2^4
  let bit3 = multiply(1, 8)   // 2^3
  let bit2 = multiply(0, 4)   // 2^2
  let bit1 = multiply(1, 2)   // 2^1
  let bit0 = multiply(0, 1)   // 2^0
  
  // 重新组合验证
  let reconstructed = add(add(add(add(add(bit5, bit4), bit3), bit2), bit1), bit0)
  
  // 计算设置位的数量
  let set_bits = add(add(1, 1), 1)  // bit5, bit3, bit1 被设置
  
  assert_eq(reconstructed, 42)
  assert_eq(set_bits, 3)
}

test "temperature_conversion" {
  // 温度转换测试 - 科学计算应用
  let celsius = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit_step1 = multiply(celsius, 9)
  let fahrenheit_step2 = divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = add(fahrenheit_step2, 32)
  
  // 华氏度转开尔文：K = (F - 32) × 5/9 + 273.15
  // 简化计算，使用整数近似
  let kelvin_step1 = subtract(fahrenheit, 32)
  let kelvin_step2 = multiply(kelvin_step1, 5)
  let kelvin_step3 = divide_with_ceil(kelvin_step2, 9)
  let kelvin = add(kelvin_step3, 273)
  
  // 验证转换的一致性（简化验证）
  let celsius_back = divide_with_ceil(multiply(subtract(kelvin, 273), 9), 5)
  
  assert_eq(fahrenheit, 77)
  assert_eq(kelvin, 298)
  assert_eq(celsius_back, 25)
}

test "game_score_calculation" {
  // 游戏得分计算测试 - 游戏逻辑应用
  let base_score = 100
  let level_multiplier = 3
  let combo_bonus = 50
  let penalty = 20
  
  // 基础得分 × 等级倍数
  let level_score = multiply(base_score, level_multiplier)
  
  // 加入连击奖励
  let score_with_combo = add(level_score, combo_bonus)
  
  // 减去惩罚
  let final_score = subtract(score_with_combo, penalty)
  
  // 计算星级评分（每100分一星）
  let star_rating = divide_with_ceil(final_score, 100)
  
  // 计算剩余分数
  let remainder = subtract(final_score, multiply(star_rating - 1, 100))
  
  assert_eq(level_score, 300)
  assert_eq(score_with_combo, 350)
  assert_eq(final_score, 330)
  assert_eq(star_rating, 4)
  assert_eq(remainder, 30)
}

test "inventory_management" {
  // 库存管理测试 - 商业应用
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 200
  
  // 计算一周销售量
  let weekly_sales = multiply(daily_sales, days)
  
  // 计算销售后库存
  let stock_after_sales = subtract(initial_stock, weekly_sales)
  
  // 加入新货
  let final_stock = add(stock_after_sales, new_shipment)
  
  // 计算库存周转率（假设月需求为400）
  let monthly_demand = 400
  let turnover_rate = divide_with_ceil(monthly_demand, final_stock)
  
  // 计算安全库存天数
  let safety_days = divide_with_ceil(final_stock, daily_sales)
  
  assert_eq(weekly_sales, 175)
  assert_eq(stock_after_sales, 325)
  assert_eq(final_stock, 525)
  assert_eq(turnover_rate, 1)
  assert_eq(safety_days, 21)
}

test "data_compression_simulation" {
  // 数据压缩模拟测试 - 算法应用
  let original_size = 1000
  let repetition_factor = 5  // 重复因子
  
  // 计算压缩后的理论大小
  let compressed_size = divide_with_ceil(original_size, repetition_factor)
  
  // 计算压缩比
  let compression_ratio = divide_with_ceil(multiply(original_size, 100), compressed_size)
  
  // 计算节省的空间
  let space_saved = subtract(original_size, compressed_size)
  
  // 计算节省的百分比
  let savings_percentage = divide_with_ceil(multiply(space_saved, 100), original_size)
  
  // 模拟多次压缩的效果
  let second_compression = divide_with_ceil(compressed_size, repetition_factor)
  let total_compression = divide_with_ceil(original_size, second_compression)
  
  assert_eq(compressed_size, 200)
  assert_eq(compression_ratio, 500)
  assert_eq(space_saved, 800)
  assert_eq(savings_percentage, 80)
  assert_eq(second_compression, 40)
  assert_eq(total_compression, 25)
}

test "project_timeline_calculation" {
  // 项目时间线计算测试 - 项目管理应用
  let total_tasks = 48
  let team_size = 6
  let work_days_per_week = 5
  let hours_per_task = 8
  
  // 计算每人需要完成的任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_size)
  
  // 计算每人需要的工作小时数
  let hours_per_person = multiply(tasks_per_person, hours_per_task)
  
  // 计算每人需要的工作天数
  let days_per_person = divide_with_ceil(hours_per_person, 8)
  
  // 计算项目持续周数
  let project_weeks = divide_with_ceil(days_per_person, work_days_per_week)
  
  // 计算总工作小时数
  let total_hours = multiply(total_tasks, hours_per_task)
  
  // 计算项目效率（每人每周完成的任务数）
  let tasks_per_week_per_person = divide_with_ceil(tasks_per_person, project_weeks)
  
  assert_eq(tasks_per_person, 8)
  assert_eq(hours_per_person, 64)
  assert_eq(days_per_person, 8)
  assert_eq(project_weeks, 2)
  assert_eq(total_hours, 384)
  assert_eq(tasks_per_week_per_person, 4)
}