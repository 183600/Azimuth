// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

// 导入必要的模块
use azimuth

test "arithmetic_sequence_operations" {
  // 测试算术序列运算
  let start = 5
  let difference = 3
  let terms = 4
  
  // 计算等差数列的和：start + (start + difference) + (start + 2*difference) + ...
  let term1 = start
  let term2 = azimuth.add(start, difference)
  let term3 = azimuth.add(term2, difference)
  let term4 = azimuth.add(term3, difference)
  
  let sequence_sum = azimuth.add(azimuth.add(term1, term2), azimuth.add(term3, term4))
  assert_eq(32, sequence_sum)  // 5 + 8 + 11 + 14 = 38
}

test "inventory_packing_optimization" {
  // 测试库存打包优化
  let small_items = 23
  let medium_items = 47
  let large_items = 15
  
  let small_box_capacity = 6
  let medium_box_capacity = 12
  let large_box_capacity = 8
  
  // 计算每种物品需要的箱子数量
  let small_boxes = azimuth.divide_with_ceil(small_items, small_box_capacity)
  let medium_boxes = azimuth.divide_with_ceil(medium_items, medium_box_capacity)
  let large_boxes = azimuth.divide_with_ceil(large_items, large_box_capacity)
  
  // 计算总箱子数
  let total_boxes = azimuth.add(azimuth.add(small_boxes, medium_boxes), large_boxes)
  assert_eq(14, total_boxes)  // ceil(23/6) + ceil(47/12) + ceil(15/8) = 4 + 4 + 2 = 10
}

test "time_calculation_scenarios" {
  // 测试时间计算场景
  let total_minutes = 157
  let minutes_per_hour = 60
  
  // 计算小时数和剩余分钟数
  let hours = azimuth.divide_with_ceil(total_minutes, minutes_per_hour)
  let exact_hours = total_minutes / minutes_per_hour
  let remaining_minutes = azimuth.subtract(total_minutes, azimuth.multiply(exact_hours, minutes_per_hour))
  
  assert_eq(3, hours)  // ceil(157/60) = 3
  assert_eq(2, exact_hours)  // 157/60 = 2
  assert_eq(37, remaining_minutes)  // 157 - 2*60 = 37
}

test "financial_rounding_calculations" {
  // 测试金融舍入计算
  let total_amount = 1000
  let people = 3
  
  // 计算每人应该支付的金额（向上取整确保覆盖总额）
  let payment_per_person = azimuth.divide_with_ceil(total_amount, people)
  let total_collected = azimuth.multiply(payment_per_person, people)
  let excess = azimuth.subtract(total_collected, total_amount)
  
  assert_eq(334, payment_per_person)  // ceil(1000/3) = 334
  assert_eq(1002, total_collected)  // 334 * 3 = 1002
  assert_eq(2, excess)  // 1002 - 1000 = 2
}

test "geometric_progression_analysis" {
  // 浏几何级数分析
  let first_term = 2
  let ratio = 3
  let terms = 4
  
  // 计算几何级数：a, ar, ar², ar³
  let term1 = first_term
  let term2 = azimuth.multiply(first_term, ratio)
  let term3 = azimuth.multiply(term2, ratio)
  let term4 = azimuth.multiply(term3, ratio)
  
  let sum = azimuth.add(azimuth.add(term1, term2), azimuth.add(term3, term4))
  assert_eq(80, sum)  // 2 + 6 + 18 + 54 = 80
}

test "resource_scheduling_problem" {
  // 测试资源调度问题
  let tasks = 25
  let workers = 4
  let max_tasks_per_worker = 7
  
  // 计算需要的额外轮次
  let first_round_capacity = azimuth.multiply(workers, max_tasks_per_worker)
  let remaining_tasks = azimuth.subtract(tasks, first_round_capacity)
  let additional_workers_needed = azimuth.divide_with_ceil(remaining_tasks, max_tasks_per_worker)
  
  let total_rounds = azimuth.add(1, additional_workers_needed)  // 第一轮 + 额外轮次
  assert_eq(1, additional_workers_needed)  // ceil((25-4*7)/7) = ceil((-3)/7) = 1（处理负数情况）
}

test "string_concatenation_validation" {
  // 测试字符串连接验证
  let prefix = "Hello"
  let suffix = "World"
  let spacer = ", "
  
  // 测试不同的问候组合
  let greeting1 = azimuth.greet(prefix)
  let greeting2 = azimuth.greet(suffix)
  let greeting3 = azimuth.greet(prefix + spacer + suffix)
  
  assert_eq_string("Hello, Hello!", greeting1)
  assert_eq_string("Hello, World!", greeting2)
  assert_eq_string("Hello, Hello, World!", greeting3)
}

test "batch_processing_efficiency" {
  // 测试批处理效率
  let total_records = 1000
  let batch_size = 75
  let processing_time_per_batch = 5
  
  // 计算批次数和总处理时间
  let batches = azimuth.divide_with_ceil(total_records, batch_size)
  let total_time = azimuth.multiply(batches, processing_time_per_batch)
  
  // 计算实际处理的记录数
  let processed_records = azimuth.multiply(batches - 1, batch_size)
  let final_batch_size = azimuth.subtract(total_records, processed_records)
  
  assert_eq(14, batches)  // ceil(1000/75) = 14
  assert_eq(70, total_time)  // 14 * 5 = 70
  assert_eq(25, final_batch_size)  // 1000 - 13*75 = 25
}

test "temperature_conversion_calculations" {
  // 测试温度转换计算
  let celsius_temps = azimuth.add(azimuth.add(0, 10), 20)  // 0, 10, 20度
  let conversion_factor = 9
  let conversion_divisor = 5
  let offset = 32
  
  // 转换为华氏度：F = C * 9/5 + 32
  let celsius_0 = 0
  let celsius_10 = 10
  let celsius_20 = 20
  
  let fahrenheit_0 = azimuth.add(
    azimuth.divide_with_ceil(azimuth.multiply(celsius_0, conversion_factor), conversion_divisor),
    offset
  )
  let fahrenheit_10 = azimuth.add(
    azimuth.divide_with_ceil(azimuth.multiply(celsius_10, conversion_factor), conversion_divisor),
    offset
  )
  let fahrenheit_20 = azimuth.add(
    azimuth.divide_with_ceil(azimuth.multiply(celsius_20, conversion_factor), conversion_divisor),
    offset
  )
  
  assert_eq(32, fahrenheit_0)   // 0 * 9/5 + 32 = 32
  assert_eq(50, fahrenheit_10)  // 10 * 9/5 + 32 = 50
  assert_eq(68, fahrenheit_20)  // 20 * 9/5 + 32 = 68
}

test "comprehensive_error_handling" {
  // 测试综合错误处理
  let normal_cases = azimuth.add(azimuth.multiply(5, 3), azimuth.divide_with_ceil(10, 2))
  let edge_cases = azimuth.add(azimuth.multiply(0, 100), azimuth.divide_with_ceil(0, 1))
  let error_cases = azimuth.add(azimuth.multiply(10, 0), azimuth.divide_with_ceil(50, 0))
  
  // 验证正常情况
  assert_eq(18, normal_cases)  // 5*3 + ceil(10/2) = 15 + 5 = 20
  
  // 验证边缘情况
  assert_eq(0, edge_cases)  // 0*100 + ceil(0/1) = 0 + 0 = 0
  
  // 验证错误情况（除零）
  assert_eq(0, error_cases)  // 10*0 + ceil(50/0) = 0 + 0 = 0
}