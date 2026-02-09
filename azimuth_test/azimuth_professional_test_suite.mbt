// Azimuth 项目专业测试用例
// 使用标准 MoonBit 测试语法编写

test "azimuth_financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 第一年：
  let year1_interest = divide_with_ceil(multiply(principal, rate), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年：
  let year2_interest = divide_with_ceil(multiply(year1_total, rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年：
  let year3_interest = divide_with_ceil(multiply(year2_total, rate), 100)
  let year3_total = add(year2_total, year3_interest)
  
  assert_eq(10500, year1_total)
  assert_eq(11025, year2_total)
  assert_eq(11576, year3_total)
}

test "azimuth_geometric_volume_calculation" {
  // 几何体积计算测试
  let length = 8
  let width = 6
  let height = 4
  
  // 计算表面积
  let area1 = multiply(length, width)
  let area2 = multiply(length, height)
  let area3 = multiply(width, height)
  let surface_area = multiply(2, add(add(area1, area2), area3))
  
  // 计算体积
  let volume = multiply(area1, height)
  
  assert_eq(208, surface_area)
  assert_eq(192, volume)
}

test "azimuth_temperature_conversion" {
  // 温度转换测试
  let celsius = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit_step1 = multiply(celsius, 9)
  let fahrenheit_step2 = divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = add(fahrenheit_step2, 32)
  
  assert_eq(77, fahrenheit)
}

test "azimuth_inventory_optimization" {
  // 库存优化测试
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
  
  assert_eq(175, weekly_sales)
  assert_eq(325, stock_after_sales)
  assert_eq(525, final_stock)
}

test "azimuth_project_management" {
  // 项目管理计算测试
  let total_tasks = 48
  let team_size = 6
  let work_days_per_week = 5
  
  // 计算每人需要完成的任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_size)
  
  // 计算项目持续周数
  let project_weeks = divide_with_ceil(tasks_per_person, work_days_per_week)
  
  assert_eq(8, tasks_per_person)
  assert_eq(2, project_weeks)
}

test "azimuth_game_scoring_system" {
  // 游戏评分系统测试
  let base_score = 100
  let level_multiplier = 3
  let combo_bonus = 50
  let penalty = 20
  
  // 计算最终得分
  let level_score = multiply(base_score, level_multiplier)
  let score_with_combo = add(level_score, combo_bonus)
  let final_score = subtract(score_with_combo, penalty)
  
  // 计算星级评分
  let star_rating = divide_with_ceil(final_score, 100)
  
  assert_eq(300, level_score)
  assert_eq(350, score_with_combo)
  assert_eq(330, final_score)
  assert_eq(4, star_rating)
}

test "azimuth_data_compression_ratio" {
  // 数据压缩比计算测试
  let original_size = 1000
  let compression_factor = 5
  
  // 计算压缩后大小
  let compressed_size = divide_with_ceil(original_size, compression_factor)
  
  // 计算压缩比
  let compression_ratio = divide_with_ceil(multiply(original_size, 100), compressed_size)
  
  // 计算节省空间百分比
  let space_saved = subtract(original_size, compressed_size)
  let savings_percentage = divide_with_ceil(multiply(space_saved, 100), original_size)
  
  assert_eq(200, compressed_size)
  assert_eq(500, compression_ratio)
  assert_eq(800, space_saved)
  assert_eq(80, savings_percentage)
}

test "azimuth_string_processing" {
  // 字符串处理测试
  let name1 = "MoonBit"
  let name2 = "开发者"
  let name3 = "Azimuth"
  
  // 测试greet函数
  let greeting1 = greet(name1)
  let greeting2 = greet(name2)
  let greeting3 = greet(name3)
  
  assert_eq_string("Hello, MoonBit!", greeting1)
  assert_eq_string("Hello, 开发者!", greeting2)
  assert_eq_string("Hello, Azimuth!", greeting3)
}