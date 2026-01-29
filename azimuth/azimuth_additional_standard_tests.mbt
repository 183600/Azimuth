// Azimuth 项目额外的标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖各种场景

test "arithmetc_sequence_operations" {
  // 算术序列操作测试
  let first_term = 5
  let common_difference = 3
  let num_terms = 7
  
  // 计算等差数列的第n项: a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(num_terms - 1, common_difference))
  nth_term = 23  // 5 + (7-1)*3 = 23
  
  // 计算等差数列的和: S_n = n/2 * (a_1 + a_n)
  let sum = divide_with_ceil(multiply(num_terms, add(first_term, nth_term)), 2)
  sum = 98  // 7/2 * (5+23) = 98
}

test "resource_utilization_calculation" {
  // 资源利用率计算测试
  let total_capacity = 1000
  let used_resources = 756
  let buffer_percentage = 10
  
  // 计算缓冲资源
  let buffer_resources = divide_with_ceil(multiply(total_capacity, buffer_percentage), 100)
  buffer_resources = 100  // ceil(1000*10/100) = 100
  
  // 计算可用资源
  let available_resources = subtract(total_capacity, add(used_resources, buffer_resources))
  available_resources = 144  // 1000 - (756+100) = 144
  
  // 验证资源利用率
  let utilization_percentage = divide_with_ceil(multiply(used_resources, 100), total_capacity)
  utilization_percentage = 76  // ceil(75600/1000) = 76
}

test "time_conversion_and_calculation" {
  // 时间转换和计算测试
  let hours = 47
  let minutes_per_hour = 60
  let seconds_per_minute = 60
  
  // 转换为分钟
  let total_minutes = multiply(hours, minutes_per_hour)
  total_minutes = 2820  // 47 * 60 = 2820
  
  // 转换为秒
  let total_seconds = multiply(total_minutes, seconds_per_minute)
  total_seconds = 169200  // 2820 * 60 = 169200
  
  // 计算需要的工作日数（每天8小时）
  let work_hours_per_day = 8
  let work_days_needed = divide_with_ceil(hours, work_hours_per_day)
  work_days_needed = 6  // ceil(47/8) = 6
}

test "inventory_batch_processing" {
  // 库存批处理测试
  let total_items = 158
  let batch_size = 25  // 使用单个批次大小
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_items, batch_size)
  batches_needed = 7  // ceil(158/25) = 7
  
  // 计算总容量
  let total_capacity = multiply(batches_needed, batch_size)
  total_capacity = 175  // 7 * 25 = 175
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  remaining_space = 17  // 175 - 158 = 17
  
  // 计算空间利用率
  let utilization_rate = divide_with_ceil(multiply(total_items, 100), total_capacity)
  utilization_rate = 91  // ceil(15800/175) = 91
}

test "mathematical_series_evaluation" {
  // 数学级数求值测试
  // 计算平方和: 1² + 2² + 3² + ... + n²
  let n = 10
  let mut sum_of_squares = 0
  
  // 手动计算前10个平方数的和
  sum_of_squares = add(sum_of_squares, multiply(1, 1))  // 1
  sum_of_squares = add(sum_of_squares, multiply(2, 2))  // 1 + 4 = 5
  sum_of_squares = add(sum_of_squares, multiply(3, 3))  // 5 + 9 = 14
  sum_of_squares = add(sum_of_squares, multiply(4, 4))  // 14 + 16 = 30
  sum_of_squares = add(sum_of_squares, multiply(5, 5))  // 30 + 25 = 55
  sum_of_squares = add(sum_of_squares, multiply(6, 6))  // 55 + 36 = 91
  sum_of_squares = add(sum_of_squares, multiply(7, 7))  // 91 + 49 = 140
  sum_of_squares = add(sum_of_squares, multiply(8, 8))  // 140 + 64 = 204
  sum_of_squares = add(sum_of_squares, multiply(9, 9))  // 204 + 81 = 285
  sum_of_squares = add(sum_of_squares, multiply(10, 10)) // 285 + 100 = 385
  
  sum_of_squares = 385
  
  // 使用公式验证: n(n+1)(2n+1)/6
  let formula_result = divide_with_ceil(multiply(n, multiply(add(n, 1), add(multiply(2, n), 1))), 6)
  formula_result = 385
}

test "project_planning_scenario" {
  // 项目规划场景测试
  let project_tasks = 125
  let team_members = 8
  let working_days = 15
  let tasks_per_person_per_day = 2
  
  // 计算团队每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  daily_capacity = 16  // 8 * 2 = 16
  
  // 计算项目期间总处理能力
  let total_capacity = multiply(daily_capacity, working_days)
  total_capacity = 240  // 16 * 15 = 240
  
  // 计算实际需要的天数
  let days_needed = divide_with_ceil(project_tasks, daily_capacity)
  days_needed = 8  // ceil(125/16) = 8
  
  // 计算提前完成的天数
  let days_ahead = subtract(working_days, days_needed)
  days_ahead = 7  // 15 - 8 = 7
}

test "data_validation_and_bounds" {
  // 数据验证和边界测试
  // 测试极值情况
  let max_safe_int = 100000
  let min_safe_int = -100000
  
  // 测试加法极值
  let result1 = add(max_safe_int, max_safe_int)
  result1 = 200000
  let result2 = add(min_safe_int, min_safe_int)
  result2 = -200000
  let result3 = add(max_safe_int, min_safe_int)
  result3 = 0
  
  // 测试乘法极值
  let result4 = multiply(max_safe_int, max_safe_int)
  result4 = 10000000000
  let result5 = multiply(min_safe_int, min_safe_int)
  result5 = 10000000000
  let result6 = multiply(max_safe_int, min_safe_int)
  result6 = -10000000000
  
  // 测试除法极值
  let result7 = divide_with_ceil(max_safe_int, max_safe_int)
  result7 = 1
  let result8 = divide_with_ceil(min_safe_int, min_safe_int)
  result8 = 1
  let result9 = divide_with_ceil(max_safe_int, min_safe_int)
  result9 = -1
  let result10 = divide_with_ceil(0, max_safe_int)
  result10 = 0
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种字符串组合
  let result1 = greet("MoonBit Language")
  result1 = "Hello, MoonBit Language!"
  let result2 = greet("123456789")
  result2 = "Hello, 123456789!"
  let result3 = greet("special@chars#123")
  result3 = "Hello, special@chars#123!"
  let result4 = greet("mixedCASElowerUPPER")
  result4 = "Hello, mixedCASElowerUPPER!"
  let result5 = greet("  spaced   string   ")
  result5 = "Hello,   spaced   string   !"
  
  // 测试Unicode字符
  let result6 = greet("🌙⭐🌟")
  result6 = "Hello, 🌙⭐🌟!"
  let result7 = greet("数学公式")
  result7 = "Hello, 数学公式!"
  let result8 = greet("العربية")
  result8 = "Hello, العربية!"
  let result9 = greet("עברית")
  result9 = "Hello, עברית!"
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 4
  
  // 计算复利（简化版本，每年计算一次）
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  year1 = 10500  // 10000 + 5%
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  year2 = 11025  // 10500 + 5%
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  year3 = 11576  // 11025 + 5%
  let year4 = add(year3, divide_with_ceil(multiply(year3, annual_rate), 100))
  year4 = 12155  // 11576 + 5%
  
  // 计算总收益
  let total_earnings = subtract(year4, principal)
  total_earnings = 2155
  
  // 计算月供（4年=48个月）
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(year4, total_months)
  monthly_payment = 254  // ceil(12155/48)
}

test "algorithm_efficiency_comparison" {
  // 算法效率比较测试
  let n1 = 100
  let n2 = 1000
  let n3 = 10000
  
  // 模拟O(n)算法
  let linear_n1 = multiply(n1, 3)
  linear_n1 = 300
  let linear_n2 = multiply(n2, 3)
  linear_n2 = 3000
  let linear_n3 = multiply(n3, 3)
  linear_n3 = 30000
  
  // 模拟O(log n)算法（简化为除法）
  let log_n1 = divide_with_ceil(n1, 10)
  log_n1 = 10
  let log_n2 = divide_with_ceil(n2, 10)
  log_n2 = 100
  let log_n3 = divide_with_ceil(n3, 10)
  log_n3 = 1000
  
  // 计算效率比率
  let ratio_n1 = divide_with_ceil(linear_n1, log_n1)
  ratio_n1 = 30
  let ratio_n2 = divide_with_ceil(linear_n2, log_n2)
  ratio_n2 = 30
  let ratio_n3 = divide_with_ceil(linear_n3, log_n3)
  ratio_n3 = 30
}