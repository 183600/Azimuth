// 用户定义的增强 MoonBit 测试用例
// 包含8个高质量测试用例，覆盖核心功能和实际应用场景

test "scientific_calculator_simulation" {
  // 科学计算器模拟测试
  // 模拟计算器进行复合运算
  
  let base = 10
  let exponent = 3
  let multiplier = 2
  let offset = 15
  
  // 计算 (base^exponent) * multiplier + offset
  // 使用基本算术运算模拟幂运算: 10^3 = 10 * 10 * 10
  let power_step1 = @azimuth.multiply(base, base)      // 10 * 10 = 100
  let power_result = @azimuth.multiply(power_step1, base)  // 100 * 10 = 1000
  
  // 应用乘法
  let multiplied = @azimuth.multiply(power_result, multiplier)  // 1000 * 2 = 2000
  
  // 应用偏移
  let final_result = @azimuth.add(multiplied, offset)  // 2000 + 15 = 2015
  
  @azimuth.assert_eq(power_step1, 100)
  @azimuth.assert_eq(power_result, 1000)
  @azimuth.assert_eq(multiplied, 2000)
  @azimuth.assert_eq(final_result, 2015)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let annual_rate = 8
  let years = 3
  
  // 第一年计算
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  // 第二年计算（基于第一年的总额）
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, annual_rate), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  // 第三年计算（基于第二年的总额）
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year2_total, annual_rate), 100)
  let year3_total = @azimuth.add(year2_total, year3_interest)
  
  // 计算总收益
  let total_profit = @azimuth.subtract(year3_total, principal)
  
  @azimuth.assert_eq(year1_total, 1080)
  @azimuth.assert_eq(year2_total, 1167)
  @azimuth.assert_eq(year3_total, 1261)
  @azimuth.assert_eq(total_profit, 261)
}

test "inventory_optimization" {
  // 库存优化算法测试
  let current_stock = 150
  let daily_demand = 12
  let lead_time = 5  // 补货前置时间（天）
  let safety_factor = 2  // 安全因子
  
  // 计算在补货期间的需求
  let lead_time_demand = @azimuth.multiply(daily_demand, lead_time)
  
  // 计算安全库存
  let safety_stock = @azimuth.multiply(lead_time_demand, safety_factor)
  
  // 计算再订货点
  let reorder_point = @azimuth.add(lead_time_demand, safety_stock)
  
  // 计算经济订货量（简化版）
  let eoq = @azimuth.divide_with_ceil(@azimuth.multiply(lead_time_demand, 3), 2)
  
  @azimuth.assert_eq(lead_time_demand, 60)
  @azimuth.assert_eq(safety_stock, 120)
  @azimuth.assert_eq(reorder_point, 180)
  @azimuth.assert_eq(eoq, 90)
}

test "game_character_stats" {
  // 游戏角色属性计算测试
  let base_hp = 100
  let base_attack = 20
  let level = 5
  let equipment_bonus = 15
  let skill_multiplier = 2
  
  // 计算等级加成
  let level_hp_bonus = @azimuth.multiply(level, 10)
  let level_attack_bonus = @azimuth.multiply(level, 3)
  
  // 计算最终属性
  let final_hp = @azimuth.add(base_hp, level_hp_bonus)
  let final_attack = @azimuth.add(@azimuth.add(base_attack, level_attack_bonus), equipment_bonus)
  
  // 应用技能倍数
  let skill_attack = @azimuth.multiply(final_attack, skill_multiplier)
  
  // 计算战斗力评分
  let combat_power = @azimuth.add(final_hp, @azimuth.divide_with_ceil(skill_attack, 2))
  
  @azimuth.assert_eq(level_hp_bonus, 50)
  @azimuth.assert_eq(level_attack_bonus, 15)
  @azimuth.assert_eq(final_hp, 150)
  @azimuth.assert_eq(final_attack, 50)
  @azimuth.assert_eq(skill_attack, 100)
  @azimuth.assert_eq(combat_power, 200)
}

test "data_transmission_calculation" {
  // 数据传输计算测试
  let file_size = 1024  // KB
  let network_speed = 128  // KB/s
  let overhead_factor = 10  // 10% 开销
  let packet_size = 32  // KB
  
  // 计算原始传输时间
  let raw_time = @azimuth.divide_with_ceil(file_size, network_speed)
  
  // 计算开销时间
  let overhead_time = @azimuth.divide_with_ceil(@azimuth.multiply(raw_time, overhead_factor), 100)
  
  // 计算总传输时间
  let total_time = @azimuth.add(raw_time, overhead_time)
  
  // 计算数据包数量
  let packet_count = @azimuth.divide_with_ceil(file_size, packet_size)
  
  // 计算效率
  let efficiency = @azimuth.divide_with_ceil(@azimuth.multiply(raw_time, 100), total_time)
  
  @azimuth.assert_eq(raw_time, 8)
  @azimuth.assert_eq(overhead_time, 1)
  @azimuth.assert_eq(total_time, 9)
  @azimuth.assert_eq(packet_count, 32)
  @azimuth.assert_eq(efficiency, 89)
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  let rectangle_length = 12
  let rectangle_width = 8
  let triangle_base = 10
  let triangle_height = 6
  let circle_radius = 5
  
  // 计算矩形面积
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  
  // 计算三角形面积（简化为整数计算）
  let triangle_area = @azimuth.divide_with_ceil(@azimuth.multiply(triangle_base, triangle_height), 2)
  
  // 计算圆面积（π≈3.14，简化为整数计算）
  let circle_area = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.multiply(circle_radius, circle_radius), 314), 100)
  
  // 计算总面积
  let total_area = @azimuth.add(@azimuth.add(rectangle_area, triangle_area), circle_area)
  
  @azimuth.assert_eq(rectangle_area, 96)
  @azimuth.assert_eq(triangle_area, 30)
  @azimuth.assert_eq(circle_area, 79)
  @azimuth.assert_eq(total_area, 205)
}

test "temperature_conversion_comprehensive" {
  // 综合温度转换测试
  let celsius_freezing = 0
  let celsius_boiling = 100
  let celsius_room = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit_freezing = @azimuth.add(@azimuth.multiply(celsius_freezing, 9) / 5, 32)
  let fahrenheit_boiling = @azimuth.add(@azimuth.multiply(celsius_boiling, 9) / 5, 32)
  let fahrenheit_room = @azimuth.add(@azimuth.multiply(celsius_room, 9) / 5, 32)
  
  // 华氏度转开尔文：K = (F - 32) × 5/9 + 273.15
  // 简化为整数计算
  let kelvin_freezing = @azimuth.add(@azimuth.multiply(@azimuth.subtract(fahrenheit_freezing, 32), 5) / 9, 273)
  let kelvin_boiling = @azimuth.add(@azimuth.multiply(@azimuth.subtract(fahrenheit_boiling, 32), 5) / 9, 273)
  let kelvin_room = @azimuth.add(@azimuth.multiply(@azimuth.subtract(fahrenheit_room, 32), 5) / 9, 273)
  
  @azimuth.assert_eq(fahrenheit_freezing, 32)
  @azimuth.assert_eq(fahrenheit_boiling, 212)
  @azimuth.assert_eq(fahrenheit_room, 77)
  @azimuth.assert_eq(kelvin_freezing, 273)
  @azimuth.assert_eq(kelvin_boiling, 373)
  @azimuth.assert_eq(kelvin_room, 298)
}

test "project_management_metrics" {
  // 项目管理指标计算测试
  let total_tasks = 48
  let completed_tasks = 32
  let team_members = 6
  let days_elapsed = 15
  let budget_total = 100000
  let budget_spent = 45000
  
  // 计算完成率
  let completion_rate = @azimuth.divide_with_ceil(@azimuth.multiply(completed_tasks, 100), total_tasks)
  
  // 计算剩余任务
  let remaining_tasks = @azimuth.subtract(total_tasks, completed_tasks)
  
  // 计算每人平均完成任务
  let tasks_per_member = @azimuth.divide_with_ceil(completed_tasks, team_members)
  
  // 计算平均每天完成任务
  let tasks_per_day = @azimuth.divide_with_ceil(completed_tasks, days_elapsed)
  
  // 计算预算使用率
  let budget_usage = @azimuth.divide_with_ceil(@azimuth.multiply(budget_spent, 100), budget_total)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.subtract(budget_total, budget_spent)
  
  // 预估完成剩余任务所需天数
  let days_needed = @azimuth.divide_with_ceil(remaining_tasks, tasks_per_day)
  
  @azimuth.assert_eq(completion_rate, 67)
  @azimuth.assert_eq(remaining_tasks, 16)
  @azimuth.assert_eq(tasks_per_member, 6)
  @azimuth.assert_eq(tasks_per_day, 3)
  @azimuth.assert_eq(budget_usage, 45)
  @azimuth.assert_eq(remaining_budget, 55000)
  @azimuth.assert_eq(days_needed, 6)
}