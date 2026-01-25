// Enhanced test suite for Azimuth library
// 标准MoonBit测试语法，包含10个测试用例

// 导入azimuth库
use azimuth

test "enhanced_mathematical_series" {
  // 数学级数计算：计算等比数列的前n项和
  let first_term = 2
  let common_ratio = 3
  let number_of_terms = 5
  
  // 计算等比数列和：a * (r^n - 1) / (r - 1)
  let sum_manual = add(add(add(add(first_term, multiply(first_term, common_ratio)), 
    multiply(first_term, multiply(common_ratio, common_ratio))), 
    multiply(first_term, multiply(common_ratio, multiply(common_ratio, common_ratio)))), 
    multiply(first_term, multiply(common_ratio, multiply(common_ratio, multiply(common_ratio, common_ratio)))))
  
  assert_eq(242, sum_manual)  // 2 + 6 + 18 + 54 + 162 = 242
}

test "enhanced_temperature_conversion" {
  // 温度转换计算：摄氏度到华氏度
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  
  assert_eq(77, fahrenheit)  // (25 * 9/5) + 32 = 45 + 32 = 77
}

test "enhanced_loan_calculation" {
  // 贷款计算：计算月供
  let principal = 200000
  let annual_rate = 6
  let years = 30
  
  // 简化计算：总利息 + 本金 / 月数
  let total_interest = divide_with_ceil(multiply(principal, annual_rate * years), 100)
  let total_payment = add(principal, total_interest)
  let monthly_payment = divide_with_ceil(total_payment, years * 12)
  
  assert_eq(833, monthly_payment)  // ceil((200000 + 360000) / 360) = 833
}

test "enhanced_digital_storage_conversion" {
  // 数字存储转换：GB到MB
  let gigabytes = 5
  let megabytes = multiply(gigabytes, 1024)
  
  assert_eq(5120, megabytes)  // 5 * 1024 = 5120
}

test "enhanced_fitness_tracking" {
  // 健身追踪：计算卡路里消耗
  let calories_per_minute = 8
  let exercise_minutes = 45
  let rest_minutes = 10
  let total_session_time = add(exercise_minutes, rest_minutes)
  
  let total_calories = multiply(calories_per_minute, exercise_minutes)
  let session_efficiency = divide_with_ceil(multiply(total_calories, 100), total_session_time)
  
  assert_eq(360, total_calories)  // 8 * 45 = 360
  assert_eq(69, session_efficiency)  // ceil(360 * 100 / 55) = 69
}

test "enhanced_cooking_recipe_scaling" {
  // 烹饪食谱缩放
  let original_servings = 4
  let desired_servings = 7
  let flour_per_serving = 150
  let sugar_per_serving = 50
  
  let scaling_factor = divide_with_ceil(desired_servings, original_servings)
  let total_flour = multiply(flour_per_serving, scaling_factor)
  let total_sugar = multiply(sugar_per_serving, scaling_factor)
  
  assert_eq(2, scaling_factor)  // ceil(7/4) = 2
  assert_eq(300, total_flour)   // 150 * 2 = 300
  assert_eq(100, total_sugar)   // 50 * 2 = 100
}

test "enhanced_travel_planning" {
  // 旅行计划：计算总时间和费用
  let distance_km = 450
  let speed_kmh = 90
  let fuel_consumption_per_100km = 8
  let fuel_price_per_liter = 6
  
  let travel_hours = divide_with_ceil(multiply(distance_km, 60), speed_kmh * 60)
  let fuel_needed = divide_with_ceil(multiply(distance_km, fuel_consumption_per_100km), 100)
  let total_fuel_cost = multiply(fuel_needed, fuel_price_per_liter)
  
  assert_eq(5, travel_hours)      // ceil(450/90) = 5
  assert_eq(36, fuel_needed)      // ceil(450*8/100) = 36
  assert_eq(216, total_fuel_cost) // 36 * 6 = 216
}

test "enhanced_data_analysis" {
  // 数据分析：计算统计指标
  let data_points = [15, 22, 18, 25, 20, 17, 23]
  let sum = add(add(add(add(add(add(15, 22), 18), 25), 20), 17), 23)
  let count = 7
  let mean = divide_with_ceil(sum, count)
  
  // 计算方差（简化版）
  let variance_sum = add(add(add(add(add(add(multiply(15 - mean, 15 - mean), 
    multiply(22 - mean, 22 - mean)), multiply(18 - mean, 18 - mean)), 
    multiply(25 - mean, 25 - mean)), multiply(20 - mean, 20 - mean)), 
    multiply(17 - mean, 17 - mean)), multiply(23 - mean, 23 - mean))
  let variance = divide_with_ceil(variance_sum, count)
  
  assert_eq(140, sum)   // 15 + 22 + 18 + 25 + 20 + 17 + 23 = 140
  assert_eq(20, mean)   // ceil(140/7) = 20
}

test "enhanced_communication_cost" {
  // 通信费用计算
  let base_minutes = 100
  let additional_minutes = 45
  let base_rate = 30
  let additional_rate = 2
  
  let total_minutes = add(base_minutes, additional_minutes)
  let base_cost = base_rate
  let additional_cost = multiply(additional_minutes, additional_rate)
  let total_cost = add(base_cost, additional_cost)
  
  assert_eq(145, total_minutes)  // 100 + 45 = 145
  assert_eq(120, additional_cost) // 45 * 2 = 90
  assert_eq(120, total_cost)     // 30 + 90 = 120
}

test "enhanced_greeting_with_context" {
  // 带上下文的问候功能
  let time_of_day = "morning"
  let user_name = "Alice"
  let context = " on this beautiful " + time_of_day
  let full_greeting = greet(user_name + context)
  
  assert_eq_string("Hello, Alice on this beautiful morning!", full_greeting)
  
  // 测试不同时区的问候
  let timezone_greeting = greet("Bob" + " from Tokyo")
  assert_eq_string("Hello, Bob from Tokyo!", timezone_greeting)
}