// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

// 导入必要的模块
test "temperature_conversion" {

test "temperature_conversion" {
  // 温度转换测试：摄氏度到华氏度
  // F = C * 9/5 + 32，使用向上取整除法模拟
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
  
  // 测试零摄氏度
  let freezing = add(multiply(divide_with_ceil(multiply(0, 9), 5), 1), 32)
  assert_eq(32, freezing)
}

test "loan_interest_calculation" {
  // 贷款利息计算测试
  let principal = 10000
  let annual_rate_percent = 6
  let months = 12
  
  // 计算月利率（向上取整）
  let monthly_rate = divide_with_ceil(annual_rate_percent, 12)
  
  // 计算简单利息
  let total_interest = divide_with_ceil(multiply(principal, multiply(monthly_rate, months)), 100)
  
  assert_eq(600, total_interest)  // 10000 * 6% = 600
}

test "batch_processing_optimization" {
  // 批处理优化测试
  let total_items = 157
  let batch_size = 25
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_items, batch_size)
  
  // 计算最后一个批次的项目数
  let last_batch_size = add(total_items, -multiply(batches_needed - 1, batch_size))
  
  assert_eq(7, batches_needed)  // ceil(157/25) = 7
  assert_eq(7, last_batch_size)  // 157 - 6*25 = 7
}

test "resource_allocation_fairness" {
  // 资源分配公平性测试
  let total_resources = 100
  let participants = 3
  
  // 每人分配的基础资源
  let base_allocation = divide_with_ceil(total_resources, participants)
  
  // 计算总分配量
  let total_allocated = multiply(base_allocation, participants)
  
  assert_eq(34, base_allocation)  // ceil(100/3) = 34
  assert_eq(102, total_allocated)  // 34 * 3 = 102（可能超出原总量）
}

test "string_pattern_generation" {
  // 字符串模式生成测试
  let prefix = "ID"
  let number = 42
  let suffix = "END"
  
  // 构建ID字符串
  let id_string = prefix + number.to_string() + suffix
  let greeting = greet(id_string)
  
  assert_eq_string("Hello, ID42END!", greeting)
  assert_eq_string("Hello, ID0END!", greet("ID0END"))
}

test "cascading_calculations" {
  // 级联计算测试
  let initial_value = 100
  let factor1 = 2
  let factor2 = 3
  let factor3 = 4
  
  // 级联乘法计算
  let step1 = multiply(initial_value, factor1)
  let step2 = multiply(step1, factor2)
  let step3 = multiply(step2, factor3)
  
  // 添加偏移量
  let final_result = add(step3, 50)
  
  assert_eq(200, step1)   // 100 * 2 = 200
  assert_eq(600, step2)   // 200 * 3 = 600
  assert_eq(2400, step3)  // 600 * 4 = 2400
  assert_eq(2450, final_result)  // 2400 + 50 = 2450
}

test "time_zone_conversion" {
  // 时区转换测试
  let local_hour = 23  // 晚上11点
  let time_difference = 5  // 时差5小时
  
  // 计算目标时区时间（24小时制）
  let target_hour_raw = add(local_hour, time_difference)
  let target_hour = if target_hour_raw >= 24 {
    add(target_hour_raw, -24)
  } else {
    target_hour_raw
  }
  
  assert_eq(4, target_hour)  // 23 + 5 = 28 -> 28 - 24 = 4（凌晨4点）
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算测试
  let current_stock = 45
  let daily_sales = 8
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算在交货期内的预期销售
  let expected_sales = multiply(daily_sales, lead_time_days)
  
  // 计算重新订购点
  let reorder_point = add(expected_sales, safety_stock)
  
  // 判断是否需要重新订购
  let should_reorder = if current_stock <= reorder_point { 1 } else { 0 }
  
  assert_eq(56, expected_sales)  // 8 * 7 = 56
  assert_eq(76, reorder_point)   // 56 + 20 = 76
  assert_eq(1, should_reorder)   // 45 <= 76，需要重新订购
}

test "geometric_progression" {
  // 几何级数计算测试
  let first_term = 2
  let ratio = 3
  let terms = 5
  
  // 计算几何级数的和（简化版）
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  
  assert_eq(2, term1)    // 2
  assert_eq(6, term2)    // 2 * 3 = 6
  assert_eq(18, term3)   // 6 * 3 = 18
  assert_eq(54, term4)   // 18 * 3 = 54
  assert_eq(162, term5)  // 54 * 3 = 162
  assert_eq(242, sum)    // 2 + 6 + 18 + 54 + 162 = 242
}

test "password_strength_evaluation" {
  // 密码强度评估测试（简化版）
  let password_length = 12
  let has_uppercase = 1
  let has_lowercase = 1
  let has_numbers = 1
  let has_symbols = 0
  
  // 计算基础分数
  let base_score = multiply(password_length, 5)
  
  // 添加特征分数
  let feature_score = add(add(add(has_uppercase, has_lowercase), has_numbers), has_symbols)
  let total_score = add(base_score, multiply(feature_score, 10))
  
  // 强度等级（简化）
  let strength_level = divide_with_ceil(total_score, 30)
  
  assert_eq(60, base_score)      // 12 * 5 = 60
  assert_eq(3, feature_score)    // 1 + 1 + 1 + 0 = 3
  assert_eq(90, total_score)     // 60 + 3 * 10 = 90
  assert_eq(3, strength_level)   // ceil(90/30) = 3（强密码）
}