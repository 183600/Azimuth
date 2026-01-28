// Azimuth 标准测试套件
// 使用标准 MoonBit 测试语法的精选测试用例

test "arithmetic_sequence_calculation" {
  // 等差数列计算：计算 3 + 7 + 11 + 15 + 19
  let term1 = 3
  let diff = 4
  let terms = 5
  
  // 使用等差数列求和公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(terms, add(multiply(2, term1), multiply(terms - 1, diff))),
    2
  )
  
  assert_eq(55, sum)  // 3 + 7 + 11 + 15 + 19 = 55
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_resources = 100
  let project_a_needs = 23
  let project_b_needs = 31
  let project_c_needs = 19
  
  let allocated_a = project_a_needs
  let allocated_b = project_b_needs
  let allocated_c = project_c_needs
  let total_allocated = add(add(allocated_a, allocated_b), allocated_c)
  let remaining = add(total_resources, -total_allocated)
  
  assert_eq(73, total_allocated)
  assert_eq(27, remaining)
}

test "temperature_conversion" {
  // 温度转换模拟（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  assert_eq(77, fahrenheit)  // 25°C = 77°F
}

test "digital_clock_calculation" {
  // 数字时钟计算：计算一天中的总秒数
  let hours = 24
  let minutes_per_hour = 60
  let seconds_per_minute = 60
  
  let seconds_per_day = multiply(hours, multiply(minutes_per_hour, seconds_per_minute))
  
  assert_eq(86400, seconds_per_day)
}

test "compound_interest_simplified" {
  // 简化的复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "geometric_area_calculation" {
  // 几何面积计算：计算梯形面积
  let base1 = 8
  let base2 = 12
  let height = 5
  
  // 梯形面积公式：(上底 + 下底) × 高 ÷ 2
  let area = divide_with_ceil(multiply(add(base1, base2), height), 2)
  
  assert_eq(50, area)  // (8 + 12) × 5 ÷ 2 = 50
}

test "batch_processing_efficiency" {
  // 批处理效率计算
  let total_items = 157
  let batch_size = 25
  
  let full_batches = divide_with_ceil(total_items, batch_size)
  let last_batch_size = total_items - multiply(full_batches - 1, batch_size)
  
  assert_eq(7, full_batches)
  assert_eq(7, last_batch_size)
}

test "multilingual_greeting_system" {
  // 多语言问候系统测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算
  let current_stock = 45
  let daily_demand = 8
  let lead_time_days = 7
  let safety_stock = 15
  
  let demand_during_lead = multiply(daily_demand, lead_time_days)
  let reorder_point = add(demand_during_lead, safety_stock)
  let need_reorder = current_stock < reorder_point
  
  assert_eq(56, demand_during_lead)
  assert_eq(71, reorder_point)
  
  // 用断言表示需要重新订购
  assert_eq(1, if need_reorder { 1 } else { 0 })
}

test "network_packet_optimization" {
  // 网络数据包优化计算
  let data_size = 2048
  let header_size = 32
  let max_packet_size = 512
  
  let total_size = add(data_size, header_size)
  let packets_needed = divide_with_ceil(total_size, max_packet_size)
  
  assert_eq(2080, total_size)
  assert_eq(5, packets_needed)
}