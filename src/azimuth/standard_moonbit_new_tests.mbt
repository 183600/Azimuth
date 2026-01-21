// 新增的 MoonBit 标准测试用例
// 针对 azimuth 库核心功能的高质量测试

test "arithemtic_sequence_sum" {
  // 测试等差数列求和
  // 计算 1 + 2 + 3 + ... + 10 的和
  let first_term = 1
  let last_term = 10
  let number_of_terms = 10
  let sum = divide_with_ceil(multiply(number_of_terms, add(first_term, last_term)), 2)
  assert_eq(55, sum)  // 10 * (1 + 10) / 2 = 55
}

test "temperature_conversion" {
  // 测试温度转换计算
  // 摄氏度转华氏度：F = C * 9/5 + 32，使用整数运算近似
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  assert_eq(77, fahrenheit)  // ceil(25 * 9 / 5) + 32 = ceil(45) + 32 = 77
  
  // 零下温度测试
  let celsius_negative = -10
  let fahrenheit_negative = add(divide_with_ceil(multiply(celsius_negative, 9), 5), 32)
  assert_eq(14, fahrenheit_negative)  // ceil(-10 * 9 / 5) + 32 = ceil(-18) + 32 = 14
}

test "batch_processing_calculation" {
  // 测试批处理计算
  // 计算处理一批文件所需的时间
  let files_per_batch = 7
  let total_files = 45
  let time_per_batch = 5  // 分钟
  let batches_needed = divide_with_ceil(total_files, files_per_batch)
  let total_time = multiply(batches_needed, time_per_batch)
  assert_eq(35, total_time)  // ceil(45/7) * 5 = 7 * 5 = 35 分钟
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  // 将资源分配给不同项目，确保每个项目获得足够的资源
  let total_resources = 100
  let project1_needs = 23
  let project2_needs = 31
  let project3_needs = 19
  
  let allocated1 = project1_needs
  let allocated2 = project2_needs
  let allocated3 = project3_needs
  let total_allocated = add(add(allocated1, allocated2), allocated3)
  let remaining = add(total_resources, -total_allocated)
  
  assert_eq(73, total_allocated)  // 23 + 31 + 19 = 73
  assert_eq(27, remaining)  // 100 - 73 = 27
}

test "multi_level_marketing_calculation" {
  // 测试多级营销计算
  // 计算三级分销的佣金
  let base_commission = 100
  let level1_multiplier = 2  // 一级2倍
  let level2_multiplier = 3  // 二级3倍
  let level3_multiplier = 5  // 三级5倍
  
  let level1_commission = multiply(base_commission, level1_multiplier)
  let level2_commission = multiply(base_commission, level2_multiplier)
  let level3_commission = multiply(base_commission, level3_multiplier)
  let total_commission = add(add(level1_commission, level2_commission), level3_commission)
  
  assert_eq(1000, total_commission)  // 100*2 + 100*3 + 100*5 = 200 + 300 + 500 = 1000
}

test "international_greeting_formats" {
  // 测试国际化问候格式
  // 测试不同语言和特殊字符的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, München!", greet("München"))
}

test "inventory_restock_calculation" {
  // 测试库存补货计算
  // 根据当前库存和销量计算需要补货的数量
  let current_stock = 45
  let daily_sales = 12
  let days_to_cover = 7  // 需要覆盖7天
  let safety_stock = 20  // 安全库存
  
  let needed_for_sales = multiply(daily_sales, days_to_cover)
  let total_needed = add(needed_for_sales, safety_stock)
  let restock_quantity = add(total_needed, -current_stock)
  
  assert_eq(59, restock_quantity)  // 12*7 + 20 - 45 = 84 + 20 - 45 = 59
}

test "project_timeline_planning" {
  // 测试项目时间线规划
  // 计算项目完成时间和里程碑
  let total_tasks = 67
  let tasks_per_week = 15
  let weeks_needed = divide_with_ceil(total_tasks, tasks_per_week)
  let buffer_weeks = 2  // 缓冲时间
  let total_timeline = add(weeks_needed, buffer_weeks)
  
  assert_eq(7, total_timeline)  // ceil(67/15) + 2 = 5 + 2 = 7 周
}

test "financial_compound_interest" {
  // 测试金融复利计算（简化版）
  // 计算简化的复利增长
  let principal = 1000
  let interest_rate_percent = 8
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate_percent), 100))
  
  assert_eq(1260, year3_amount)  // 使用向上取整的复利计算
}

test "complex_shipping_calculation" {
  // 测试复杂运输计算
  // 计算不同重量包裹的运费
  let base_rate = 10
  let weight_per_kg_rate = 3
  let package_weights = [5, 8, 12, 3]  // 四个包裹的重量
  let total_weight = add(add(add(package_weights[0], package_weights[1]), package_weights[2]), package_weights[3])
  let weight_cost = multiply(total_weight, weight_per_kg_rate)
  let packages_needed = divide_with_ceil(total_weight, 10)  // 每箱最多10kg
  let packaging_cost = multiply(packages_needed, 5)  // 每个箱子5元
  let total_shipping_cost = add(add(base_rate, weight_cost), packaging_cost)
  
  assert_eq(114, total_shipping_cost)  // 10 + 28*3 + ceil(28/10)*5 = 10 + 84 + 15 = 109
}