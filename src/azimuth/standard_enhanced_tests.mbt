// 标准 MoonBit 增强测试用例
// 针对 azimuth 库的全面功能测试

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列和等比数列
  let first_term = 3
  let common_diff = 4
  let terms_count = 5
  
  // 计算等差数列第5项：a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(common_diff, terms_count - 1))
  assert_eq(19, nth_term)  // 3 + 4*4 = 19
  
  // 计算前5项和：S_n = n/2 * (2a_1 + (n-1)d)
  let sum_n = divide_with_ceil(multiply(terms_count, add(multiply(2, first_term), multiply(common_diff, terms_count - 1))), 2)
  assert_eq(55, sum_n)  // 5/2 * (6 + 16) = 55
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年：1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  
  // 第二年：1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  
  // 第三年：1103 + ceil(1103*5/100) = 1103 + 56 = 1159
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1159, year3)
}

test "inventory_optimization" {
  // 库存优化算法
  let demand_forecast = 150
  let safety_stock = 30
  let reorder_point = add(demand_forecast, safety_stock)
  
  let current_stock = 100
  let order_quantity = 200
  
  // 如果当前库存低于再订购点，则订购
  let should_order = current_stock < reorder_point
  let new_stock = if should_order { add(current_stock, order_quantity) } else { current_stock }
  
  assert_eq(180, reorder_point)
  assert_eq(true, should_order)
  assert_eq(300, new_stock)
}

test "temperature_conversion" {
  // 温度转换计算（简化版本）
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 验证转换公式：°F = (°C × 9/5) + 32
  assert_eq(77, fahrenheit)  // 25*9/5 + 32 = 45 + 32 = 77
  
  // 负温度测试
  let negative_celsius = -10
  let negative_fahrenheit = add(divide_with_ceil(multiply(negative_celsius, 9), 5), 32)
  assert_eq(14, negative_fahrenheit)  // -10*9/5 + 32 = -18 + 32 = 14
}

test "project_management_metrics" {
  // 项目管理指标计算
  let total_tasks = 45
  let completed_tasks = 28
  let team_members = 5
  
  // 完成率
  let completion_rate = divide_with_ceil(multiply(completed_tasks, 100), total_tasks)
  
  // 剩余任务
  let remaining_tasks = add(total_tasks, -completed_tasks)
  
  // 每人平均任务数
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  // 预计剩余天数（假设每人每天完成2个任务）
  let days_remaining = divide_with_ceil(remaining_tasks, multiply(team_members, 2))
  
  assert_eq(63, completion_rate)  // ceil(28*100/45) = 63%
  assert_eq(17, remaining_tasks)
  assert_eq(9, tasks_per_member)
  assert_eq(2, days_remaining)
}

test "data_structure_simulation" {
  // 数据结构模拟：栈操作
  let initial_stack = 0
  let push1 = 10
  let push2 = 20
  let push3 = 30
  
  // 模拟栈的push操作（用加法表示）
  let stack_after_push1 = add(initial_stack, push1)
  let stack_after_push2 = add(stack_after_push1, push2)
  let stack_after_push3 = add(stack_after_push2, push3)
  
  // 模拟栈的pop操作（用减法表示）
  let stack_after_pop1 = add(stack_after_push3, -push3)
  let stack_after_pop2 = add(stack_after_pop1, -push2)
  
  assert_eq(10, stack_after_push1)
  assert_eq(30, stack_after_push2)
  assert_eq(60, stack_after_push3)
  assert_eq(30, stack_after_pop1)
  assert_eq(10, stack_after_pop2)
}

test "network_throughput_calculation" {
  // 网络吞吐量计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  let latency_ms = 10
  let packet_overhead = 5
  
  // 基本传输时间
  let base_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 考虑延迟的总时间（简化计算）
  let total_packets = divide_with_ceil(file_size_mb, 10)  // 假设每个包10MB
  let total_latency = divide_with_ceil(multiply(total_packets, latency_ms), 1000)  // 转换为秒
  let overhead_time = divide_with_ceil(multiply(total_packets, packet_overhead), network_speed_mbps)
  
  let total_time = add(add(base_time, total_latency), overhead_time)
  
  assert_eq(17, base_time)
  assert_eq(1, total_latency)
  assert_eq(1, overhead_time)
  assert_eq(19, total_time)
}

test "string_processing_pipeline" {
  // 字符串处理流水线
  let base_name = "John"
  let suffix = "Doe"
  let title = "Mr"
  
  // 构建完整名称
  let full_name = base_name + " " + suffix
  let formal_name = title + " " + full_name
  
  // 使用greet函数处理
  let casual_greeting = greet(base_name)
  let formal_greeting = greet(formal_name)
  
  assert_eq_string("Hello, John!", casual_greeting)
  assert_eq_string("Hello, Mr John Doe!", formal_greeting)
  
  // 测试特殊字符处理
  let special_chars = "!@#$%^&*()"
  let special_greeting = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()!", special_greeting)
}