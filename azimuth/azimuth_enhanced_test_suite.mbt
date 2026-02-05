// Azimuth 增强测试套件
// 包含 8 个高质量的标准 MoonBit 测试用例
// 测试各种实际应用场景和边界情况

test "time_calculations_scenario" {
  // 时间计算场景测试
  // 场景：计算项目完成时间
  let total_hours_needed = 157
  let work_hours_per_day = 8
  let days_needed = divide_with_ceil(total_hours_needed, work_hours_per_day)
  
  assert_eq(20, days_needed)
  
  // 计算总工作日数（每周5天）
  let work_days_per_week = 5
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  assert_eq(4, weeks_needed)
  
  // 计算实际工作天数
  let actual_work_days = multiply(weeks_needed - 1, work_days_per_week)
  let remaining_days = days_needed - actual_work_days
  assert_eq(5, remaining_days)
}

test "inventory_batch_processing" {
  // 库存批处理测试
  // 场景：批量处理库存物品
  let total_items = 487
  let batch_size = 25
  let batches_needed = divide_with_ceil(total_items, batch_size)
  
  assert_eq(20, batches_needed)
  
  // 计算总处理能力
  let total_capacity = multiply(batches_needed, batch_size)
  assert_eq(500, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, total_items)
  assert_eq(13, remaining_capacity)
  
  // 计算利用率
  let utilization_rate = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(98, utilization_rate)
}

test "nested_calculations_complex" {
  // 嵌套复杂计算测试
  // 场景：多层嵌套计算
  let base_value = 15
  let multiplier_a = 3
  let multiplier_b = 4
  let divisor = 7
  let offset = 10
  
  // 复杂计算：((base * a) + (base * b)) / divisor + offset
  let calc_a = multiply(base_value, multiplier_a)
  let calc_b = multiply(base_value, multiplier_b)
  let sum_result = add(calc_a, calc_b)
  let div_result = divide_with_ceil(sum_result, divisor)
  let final_result = add(div_result, offset)
  
  assert_eq(35, final_result)  // ceil((15*3 + 15*4)/7) + 10 = ceil(105/7) + 10 = 15 + 10 = 25
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  // 测试各种语言的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, español!", greet("español"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  // 测试各种负数运算组合
  let negative_a = -15
  let negative_b = -8
  let positive_c = 12
  
  // 负数加法
  assert_eq(-23, add(negative_a, negative_b))
  assert_eq(-3, add(negative_a, positive_c))
  
  // 负数乘法
  assert_eq(120, multiply(negative_a, negative_b))
  assert_eq(-180, multiply(negative_a, positive_c))
  
  // 负数除法
  assert_eq(-1, divide_with_ceil(negative_a, positive_c))
  assert_eq(2, divide_with_ceil(positive_c, negative_b))
  assert_eq(-2, divide_with_ceil(negative_a, negative_b))
  
  // 负数减法
  assert_eq(-27, subtract(negative_a, positive_c))
  assert_eq(27, subtract(positive_c, negative_a))
}

test "performance_metrics_calculation" {
  // 性能指标计算测试
  // 场景：计算系统性能指标
  let total_requests = 1250
  let error_count = 37
  let time_period_minutes = 45
  
  // 计算成功率
  let success_count = subtract(total_requests, error_count)
  let success_rate = divide_with_ceil(multiply(success_count, 100), total_requests)
  assert_eq(98, success_rate)
  
  // 计算每分钟请求数
  let requests_per_minute = divide_with_ceil(total_requests, time_period_minutes)
  assert_eq(28, requests_per_minute)
  
  // 计算每小时错误数
  let errors_per_hour = divide_with_ceil(multiply(error_count, 60), time_period_minutes)
  assert_eq(50, errors_per_hour)
}

test "resource_planning_optimization" {
  // 资源规划优化测试
  // 场景：优化资源分配
  let project_duration_days = 30
  let team_members = 8
  let work_hours_per_day = 8
  let total_task_hours = 1800
  
  // 计算团队总工作能力
  let daily_capacity = multiply(team_members, work_hours_per_day)
  let total_capacity = multiply(daily_capacity, project_duration_days)
  
  assert_eq(64, daily_capacity)
  assert_eq(1920, total_capacity)
  
  // 计算需要的额外天数
  let current_capacity = multiply(daily_capacity, project_duration_days)
  let additional_days_needed = divide_with_ceil(
    subtract(total_task_hours, current_capacity), 
    daily_capacity
  )
  
  // 由于当前能力足够，不需要额外天数
  assert_true(total_task_hours <= total_capacity)
  
  // 计算剩余能力
  let remaining_capacity = subtract(total_capacity, total_task_hours)
  assert_eq(120, remaining_capacity)
}

test "mathematical_sequence_validation" {
  // 数学序列验证测试
  // 场景：验证数学序列和模式
  let initial_value = 5
  let common_difference = 3
  let sequence_length = 7
  
  // 计算等差数列的和
  // 公式：S_n = n/2 * (2a + (n-1)d)
  let last_term = add(initial_value, multiply(sequence_length - 1, common_difference))
  let sequence_sum = divide_with_ceil(
    multiply(sequence_length, add(initial_value, last_term)), 
    2
  )
  
  assert_eq(17, last_term)  // 5 + (7-1)*3 = 5 + 18 = 23
  assert_eq(77, sequence_sum)  // 7/2 * (5 + 23) = 3.5 * 28 = 98
  
  // 验证序列的乘法性质
  let doubled_sum = multiply(2, sequence_sum)
  let product_check = multiply(sequence_length, add(initial_value, last_term))
  assert_eq(doubled_sum, product_check)
}