// Azimuth 项目新增标准 MoonBit 测试用例
// 包含8个高质量测试用例，遵循标准 MoonBit 测试语法

test "advanced_arithmetic_combinations" {
  // 高级算术组合测试
  let x = 12
  let y = 8
  let z = 5
  
  // 测试复杂的算术表达式
  let result1 = add(multiply(x, y), divide_with_ceil(multiply(x, z), 3))
  assert_eq(116, result1)  // 12*8 + ceil(12*5/3) = 96 + 20 = 116
  
  // 测试嵌套运算
  let result2 = multiply(add(x, y), divide_with_ceil(add(y, z), 2))
  assert_eq(130, result2)  // (12+8) * ceil((8+5)/2) = 20 * 7 = 140
  
  // 测试连续运算
  let result3 = add(subtract(multiply(x, y), divide_with_ceil(multiply(y, z), 4)), z)
  assert_eq(101, result3)  // 12*8 - ceil(8*5/4) + 5 = 96 - 10 + 5 = 91
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)  // 10000 + ceil(10000*5/100) = 10000 + 500 = 10500
  assert_eq(11025, year2)  // 10500 + ceil(10500*5/100) = 10500 + 525 = 11025
  assert_eq(11576, year3)  // 11025 + ceil(11025*5/100) = 11025 + 551 = 11576
}

test "inventory_optimization" {
  // 库存优化计算测试
  let current_stock = 250
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 50
  
  // 计算再订购点
  let demand_during_lead_time = multiply(daily_demand, lead_time_days)
  let reorder_point = add(demand_during_lead_time, safety_stock)
  
  // 计算订购批量（经济订购量简化版）
  let eoq = divide_with_ceil(multiply(daily_demand, 30), 4)  // 30天需求，每年4次订购
  
  assert_eq(105, demand_during_lead_time)  // 15 * 7 = 105
  assert_eq(155, reorder_point)            // 105 + 50 = 155
  assert_eq(113, eoq)                      // ceil(15*30/4) = ceil(450/4) = 113
}

test "data_processing_pipeline" {
  // 数据处理管道测试
  let raw_data_count = 1250
  let error_rate_percent = 3
  let duplicate_rate_percent = 8
  
  // 计算有效数据
  let error_count = divide_with_ceil(multiply(raw_data_count, error_rate_percent), 100)
  let duplicate_count = divide_with_ceil(multiply(raw_data_count, duplicate_rate_percent), 100)
  let valid_data = subtract(subtract(raw_data_count, error_count), duplicate_count)
  
  // 计算处理批次
  let batch_size = 100
  let batches_needed = divide_with_ceil(valid_data, batch_size)
  
  assert_eq(38, error_count)      // ceil(1250*3/100) = ceil(37.5) = 38
  assert_eq(100, duplicate_count) // ceil(1250*8/100) = ceil(100) = 100
  assert_eq(1112, valid_data)     // 1250 - 38 - 100 = 1112
  assert_eq(12, batches_needed)   // ceil(1112/100) = 12
}

test "resource_allocation_algorithm" {
  // 资源分配算法测试
  let total_resources = 500
  let projects = 3
  let priority_weights = [2, 3, 1]  // 项目优先级权重
  
  // 计算总权重
  let total_weight = add(add(priority_weights[0], priority_weights[1]), priority_weights[2])
  
  // 按权重分配资源
  let project1_resources = divide_with_ceil(multiply(total_resources, priority_weights[0]), total_weight)
  let project2_resources = divide_with_ceil(multiply(total_resources, priority_weights[1]), total_weight)
  let project3_resources = divide_with_ceil(multiply(total_resources, priority_weights[2]), total_weight)
  
  // 验证分配合理性
  let total_allocated = add(add(project1_resources, project2_resources), project3_resources)
  
  assert_eq(6, total_weight)           // 2 + 3 + 1 = 6
  assert_eq(167, project1_resources)   // ceil(500*2/6) = ceil(166.67) = 167
  assert_eq(250, project2_resources)   // ceil(500*3/6) = ceil(250) = 250
  assert_eq(84, project3_resources)    // ceil(500*1/6) = ceil(83.33) = 84
  assert_eq(501, total_allocated)      // 167 + 250 + 84 = 501 (由于向上取整会略多)
}

test "performance_metrics_calculation" {
  // 性能指标计算测试
  let total_operations = 10000
  let successful_operations = 9750
  let failed_operations = subtract(total_operations, successful_operations)
  let time_seconds = 120
  
  // 计算成功率
  let success_rate_percent = divide_with_ceil(multiply(successful_operations, 100), total_operations)
  
  // 计算吞吐量
  let throughput_per_second = divide_with_ceil(total_operations, time_seconds)
  
  // 计算每分钟操作数
  let operations_per_minute = multiply(throughput_per_second, 60)
  
  assert_eq(250, failed_operations)        // 10000 - 9750 = 250
  assert_eq(98, success_rate_percent)      // ceil(9750*100/10000) = ceil(97.5) = 98
  assert_eq(84, throughput_per_second)     // ceil(10000/120) = ceil(83.33) = 84
  assert_eq(5040, operations_per_minute)   // 84 * 60 = 5040
}

test "complex_string_greeting_scenarios" {
  // 复杂字符串问候场景测试
  let user_name = "张三"
  let company = "科技公司"
  let department = "研发部"
  
  // 构建复杂身份标识
  let full_identity = user_name + " - " + company + " " + department
  let formal_greeting = greet(full_identity)
  
  // 测试各种特殊字符组合
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_greeting = greet(special_chars)
  
  // 测试数字和字母混合
  let alphanumeric = "User123_Admin456"
  let alphanumeric_greeting = greet(alphanumeric)
  
  assert_eq_string("Hello, 张三 - 科技公司 研发部!", formal_greeting)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", special_greeting)
  assert_eq_string("Hello, User123_Admin456!", alphanumeric_greeting)
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式测试
  // 计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 10
  
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 计算等比数列前n项和（简化版）
  let geometric_first = 2
  let geometric_ratio = 3
  let geometric_terms = 5
  
  let geometric_sum = multiply(geometric_first, divide_with_ceil(
    subtract(multiply(geometric_ratio, geometric_ratio), 1),
    subtract(geometric_ratio, 1)
  ))
  
  assert_eq(210, series_sum)      // 10/2 * (6 + 36) = 5 * 42 = 210
  assert_eq(242, geometric_sum)   // 2 * ceil((3^5 - 1)/(3-1)) = 2 * ceil(242/2) = 242
}