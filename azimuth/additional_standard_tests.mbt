// 额外的标准 MoonBit 测试用例
// 为 azimuth 库提供更多测试覆盖

test "arithmetc_sequence_operations" {
  // 等差数列操作测试
  let first_term = 3
  let common_diff = 2
  let n_terms = 5
  
  // 计算第n项: a_n = a_1 + (n-1)*d
  let nth_term = add(first_term, multiply(subtract(n_terms, 1), common_diff))
  assert_eq(11, nth_term)  // 3 + (5-1)*2 = 11
  
  // 计算前n项和: S_n = n/2 * (a_1 + a_n)
  let sum = divide_with_ceil(multiply(n_terms, add(first_term, nth_term)), 2)
  assert_eq(35, sum)  // 5/2 * (3+11) = 35
}

test "time_conversion_calculations" {
  // 时间转换计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(multiply(hours, 3600), add(multiply(minutes, 60), seconds))
  assert_eq(9930, total_seconds)  // 2*3600 + 45*60 + 30 = 9930
  
  // 计算需要的分钟数（向上取整）
  let minutes_needed = divide_with_ceil(total_seconds, 60)
  assert_eq(166, minutes_needed)  // ceil(9930/60) = 166
}

test "resource_allocation_fair_division" {
  // 资源公平分配测试
  let total_resources = 23
  let groups = 5
  
  // 每组分配的基础资源
  let base_per_group = divide_with_ceil(total_resources, groups)
  assert_eq(5, base_per_group)  // ceil(23/5) = 5
  
  // 计算总分配量
  let total_allocated = multiply(base_per_group, groups)
  assert_eq(25, total_allocated)
  
  // 计算剩余资源
  let remaining = subtract(total_allocated, total_resources)
  assert_eq(2, remaining)
}

test "geometric_progression_basic" {
  // 基本等比数列测试
  let first_term = 2
  let ratio = 3
  let n_terms = 4
  
  // 计算第n项: a_n = a_1 * r^(n-1)
  let second_term = multiply(first_term, ratio)
  let third_term = multiply(second_term, ratio)
  let fourth_term = multiply(third_term, ratio)
  
  assert_eq(6, second_term)   // 2 * 3 = 6
  assert_eq(18, third_term)   // 6 * 3 = 18
  assert_eq(54, fourth_term)  // 18 * 3 = 54
}

test "temperature_conversion" {
  // 温度转换测试（简化版本）
  let celsius = 25
  
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  assert_eq(77, fahrenheit)  // 25*9/5 + 32 = 77
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9
  let celsius_back = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  assert_eq(25, celsius_back)  // (77-32)*5/9 = 25
}

test "pricing_tier_calculation" {
  // 定价层级计算测试
  let units_consumed = 750
  
  // 第一层级: 0-100 单位，每单位 10
  let tier1_units = 100
  let tier1_cost = multiply(tier1_units, 10)
  
  // 第二层级: 101-500 单位，每单位 8
  let tier2_units = subtract(500, 100)
  let tier2_cost = multiply(tier2_units, 8)
  
  // 第三层级: 501+ 单位，每单位 6
  let tier3_units = subtract(units_consumed, 500)
  let tier3_cost = multiply(tier3_units, 6)
  
  // 总成本
  let total_cost = add(add(tier1_cost, tier2_cost), tier3_cost)
  assert_eq(5500, total_cost)  // 100*10 + 400*8 + 250*6 = 5500
}

test "load_balancing_simulation" {
  // 负载均衡模拟测试
  let total_requests = 47
  let servers = 4
  
  // 每台服务器的基础负载
  let base_load = divide_with_ceil(total_requests, servers)
  assert_eq(12, base_load)  // ceil(47/4) = 12
  
  // 计算总处理能力
  let total_capacity = multiply(base_load, servers)
  assert_eq(48, total_capacity)
  
  // 计算剩余处理能力
  let spare_capacity = subtract(total_capacity, total_requests)
  assert_eq(1, spare_capacity)
}

test "compound_interest_simple" {
  // 简单复利计算测试
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年结束
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  assert_eq(1050, year1)
  
  // 第二年结束
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  assert_eq(1155, year2)
  
  // 第三年结束
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  assert_eq(1213, year3)
}

test "batch_job_optimization" {
  // 批处理作业优化测试
  let total_jobs = 127
  let max_batch_size = 15
  let processing_time_per_batch = 3
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_jobs, max_batch_size)
  assert_eq(9, batches_needed)  // ceil(127/15) = 9
  
  // 计算总处理时间
  let total_time = multiply(batches_needed, processing_time_per_batch)
  assert_eq(27, total_time)
  
  // 计算平均每批处理的作业数
  let avg_jobs_per_batch = divide_with_ceil(total_jobs, batches_needed)
  assert_eq(15, avg_jobs_per_batch)  // ceil(127/9) = 15
}

test "storage_optimization_problem" {
  // 存储优化问题测试
  let items_small = 23  // 每个占用1单位空间
  let items_medium = 15  // 每个占用2单位空间
  let items_large = 8   // 每个占用4单位空间
  
  // 计算总空间需求
  let space_small = items_small
  let space_medium = multiply(items_medium, 2)
  let space_large = multiply(items_large, 4)
  let total_space = add(add(space_small, space_medium), space_large)
  assert_eq(79, total_space)  // 23 + 15*2 + 8*4 = 79
  
  // 计算需要的存储单元数量（每个单元容量为10）
  let storage_units = divide_with_ceil(total_space, 10)
  assert_eq(8, storage_units)  // ceil(79/10) = 8
  
  // 计算总存储容量
  let total_capacity = multiply(storage_units, 10)
  assert_eq(80, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_space)
  assert_eq(1, remaining_space)
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}