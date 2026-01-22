// 标准 MoonBit 测试用例 - 额外补充
// 针对 azimuth 库的核心功能进行实际应用场景测试

test "financial_interest_calculation" {
  // 金融利息计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 计算单利：principal * rate * years / 100
  let simple_interest = multiply(multiply(principal, rate_percent), years) / 100
  let total_amount = add(principal, simple_interest)
  
  assert_eq(1500, simple_interest)
  assert_eq(11500, total_amount)
}

test "inventory_reorder_point" {
  // 库存重新订购点计算
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 20
  
  // 重新订购点 = 日需求量 * 交货时间 + 安全库存
  let reorder_point = add(multiply(daily_demand, lead_time_days), safety_stock)
  
  assert_eq(125, reorder_point)
}

test "data_transfer_calculation" {
  // 数据传输时间计算
  let file_size_mb = 750
  let network_speed_mbps = 50
  
  // 传输时间 = 文件大小 / 网络速度（向上取整）
  let transfer_time_seconds = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  assert_eq(15, transfer_time_seconds)
}

test "construction_material_estimation" {
  // 建筑材料估算
  let wall_length = 12
  let wall_height = 3
  let bricks_per_square_meter = 60
  
  // 所需砖数 = 墙面积 * 每平方米砖数
  let wall_area = multiply(wall_length, wall_height)
  let total_bricks = multiply(wall_area, bricks_per_square_meter)
  
  assert_eq(36, wall_area)
  assert_eq(2160, total_bricks)
}

test "cooking_recipe_scaling" {
  // 烹饪食谱缩放
  let original_servings = 4
  let desired_servings = 7
  let flour_per_serving = 150
  
  // 缩放因子 = 期望份数 / 原始份数（向上取整）
  let scale_factor = divide_with_ceil(desired_servings, original_servings)
  let total_flour = multiply(flour_per_serving, scale_factor)
  
  assert_eq(2, scale_factor)
  assert_eq(300, total_flour)
}

test "fitness_calorie_calculation" {
  // 健身卡路里计算
  let calories_per_minute_running = 10
  let calories_per_minute_cycling = 8
  let running_minutes = 20
  let cycling_minutes = 30
  
  // 总卡路里消耗
  let running_calories = multiply(calories_per_minute_running, running_minutes)
  let cycling_calories = multiply(calories_per_minute_cycling, cycling_minutes)
  let total_calories = add(running_calories, cycling_calories)
  
  assert_eq(200, running_calories)
  assert_eq(240, cycling_calories)
  assert_eq(440, total_calories)
}

test "educational_classroom_planning" {
  // 教育教室规划
  let total_students = 87
  let students_per_classroom = 25
  
  // 所需教室数量
  let classrooms_needed = divide_with_ceil(total_students, students_per_classroom)
  
  // 计算剩余学生数
  let filled_capacity = multiply(classrooms_needed - 1, students_per_classroom)
  let remaining_students = add(total_students, 0 - filled_capacity)
  
  assert_eq(4, classrooms_needed)
  assert_eq(12, remaining_students)
}

test "agricultural_irrigation_planning" {
  // 农业灌溉规划
  let field_area_hectares = 15
  let water_requirement_per_hectare = 1000
  let irrigation_efficiency_percent = 85
  
  // 实际需水量 = 面积 * 单位需水量 / 效率
  let gross_water_requirement = multiply(field_area_hectares, water_requirement_per_hectare)
  let actual_water_needed = divide_with_ceil(gross_water_requirement, irrigation_efficiency_percent) * 100
  
  assert_eq(15000, gross_water_requirement)
  assert_eq(17647, actual_water_needed)
}

test "telecommunications_bandwidth_planning" {
  // 电信带宽规划
  let users_per_tower = 500
  let bandwidth_per_user_mbps = 2
  let overhead_percentage = 20
  
  // 总带宽需求 = 用户数 * 每用户带宽 + 开销
  let base_bandwidth = multiply(users_per_tower, bandwidth_per_user_mbps)
  let overhead_bandwidth = divide_with_ceil(multiply(base_bandwidth, overhead_percentage), 100)
  let total_bandwidth = add(base_bandwidth, overhead_bandwidth)
  
  assert_eq(1000, base_bandwidth)
  assert_eq(200, overhead_bandwidth)
  assert_eq(1200, total_bandwidth)
}

test "healthcare_patient_scheduling" {
  // 医疗患者排程
  let patients_per_day = 24
  let consultation_time_minutes = 15
  let break_time_minutes = 60
  
  // 总工作时间 = 患者数 * 诊疗时间 + 休息时间
  let patient_time = multiply(patients_per_day, consultation_time_minutes)
  let total_working_time = add(patient_time, break_time_minutes)
  
  // 计算工作小时数（向上取整）
  let working_hours = divide_with_ceil(total_working_time, 60)
  
  assert_eq(360, patient_time)
  assert_eq(420, total_working_time)
  assert_eq(7, working_hours)
}