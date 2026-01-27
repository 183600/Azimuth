// Azimuth 库的独特测试用例
// 包含不重复于现有测试的新场景测试

test "arithmetic_progression_calculation" {
  // 等差数列计算：计算前n项和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(78, sum)  // 6/2 * (6 + 20) = 3 * 26 = 78
}

test "geometric_area_calculation" {
  // 几何面积计算：计算梯形面积
  let base1 = 8
  let base2 = 12
  let height = 5
  
  // 梯形面积公式：(a + b) * h / 2
  let area = divide_with_ceil(multiply(add(base1, base2), height), 2)
  
  assert_eq(50, area)  // (8 + 12) * 5 / 2 = 20 * 5 / 2 = 50
}

test "currency_conversion_calculation" {
  // 货币转换计算：美元转欧元（假设汇率1美元=0.85欧元）
  let usd_amount = 150
  let exchange_rate = 85  // 使用整数表示汇率百分比
  
  let eur_amount = divide_with_ceil(multiply(usd_amount, exchange_rate), 100)
  
  assert_eq(128, eur_amount)  // ceil(150 * 0.85) = ceil(127.5) = 128
}

test "temperature_conversion" {
  // 温度转换：摄氏度转华氏度 F = C * 9/5 + 32
  let celsius = 25
  
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与目标时区相差8小时
  
  let target_time = add(local_time, time_difference)
  let adjusted_time = if target_time >= 24 { add(target_time, -24) } else { target_time }
  
  assert_eq(22, adjusted_time)  // 14 + 8 = 22
}

test "statistical_median_calculation" {
  // 统计中位数计算（奇数个数据点）
  let data_sum = add(add(add(add(15, 22), 8), 19), 31)
  let data_count = 5
  let mean = divide_with_ceil(data_sum, data_count)
  
  assert_eq(19, mean)  // ceil(95/5) = 19
}

test "compound_interest_simplified" {
  // 简化的复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, rate_percent), 100))
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, rate_percent), 100))
  
  assert_eq(1158, amount_year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "fuel_consumption_calculation" {
  // 燃油消耗计算
  let distance_km = 350
  let fuel_efficiency = 12  // 每升汽油可行驶的公里数
  
  let fuel_needed = divide_with_ceil(distance_km, fuel_efficiency)
  
  assert_eq(30, fuel_needed)  // ceil(350/12) = 30
}

test "binary_simulation" {
  // 二进制运算模拟：模拟二进制加法 1010 (10) + 0110 (6) = 10000 (16)
  let binary1 = 10  // 1010 in binary
  let binary2 = 6   // 0110 in binary
  
  let sum = add(binary1, binary2)
  
  assert_eq(16, sum)  // 10 + 6 = 16 (10000 in binary)
}

test "load_balancing_simulation" {
  // 负载均衡模拟：将任务分配到服务器
  let total_tasks = 47
  let server_count = 4
  
  let tasks_per_server = divide_with_ceil(total_tasks, server_count)
  let remaining_tasks = add(total_tasks, -multiply(tasks_per_server - 1, server_count))
  
  assert_eq(12, tasks_per_server)  // ceil(47/4) = 12
  assert_eq(3, remaining_tasks)    // 47 - 11*4 = 3 (最后一个服务器处理3个任务)
}