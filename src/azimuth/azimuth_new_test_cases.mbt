// 新增的 MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "divide_with_ceil_basic_functionality" {
  // 测试 divide_with_ceil 基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(5, divide_with_ceil(11, 2))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_edge_cases" {
  // 测试 divide_with_ceil 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}

test "divide_with_ceil_negative_numbers" {
  // 测试 divide_with_ceil 负数处理
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "complex_business_calculation" {
  // 复杂业务计算：计算订单需要的包装箱数量
  let items_per_box = 12
  let total_items = 45
  
  // 计算需要的箱子数量（向上取整）
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 验证计算：45个物品，每箱12个，需要4箱
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let last_term = 15
  let common_diff = 2
  
  // 计算项数：n = (last - first) / diff + 1
  let num_terms = add(divide_with_ceil(add(last_term, -first_term), common_diff), 1)
  assert_eq(7, num_terms)
  
  // 计算总和：n * (first + last) / 2
  let sum = multiply(num_terms, add(first_term, last_term)) / 2
  assert_eq(63, sum)
}

test "resource_allocation_optimization" {
  // 资源分配优化：服务器负载均衡
  let total_requests = 157
  let servers_count = 8
  
  // 计算每台服务器的基本负载
  let base_load = total_requests / servers_count
  // 计算需要额外处理请求的服务器数量
  let extra_servers = divide_with_ceil(total_requests % servers_count, 1)
  
  assert_eq(19, base_load)
  assert_eq(5, extra_servers)
}

test "time_based_scheduling" {
  // 基于时间的调度：计算完成任务需要的轮次
  let tasks_total = 37
  let tasks_per_round = 5
  
  // 计算需要的轮次
  let rounds_needed = divide_with_ceil(tasks_total, tasks_per_round)
  assert_eq(8, rounds_needed)
  
  // 最后一轮的任务数
  let last_round_tasks = add(tasks_total, multiply(-tasks_per_round, add(rounds_needed, -1)))
  assert_eq(2, last_round_tasks)
}

test "financial_calculation_precision" {
  // 金融计算精度：分期付款计算
  let total_amount = 1250
  let months = 12
  
  // 计算每月基本付款
  let monthly_payment = total_amount / months
  // 计算需要额外付款的月数
  let extra_payment_months = divide_with_ceil(total_amount % months, 1)
  
  assert_eq(104, monthly_payment)
  assert_eq(2, extra_payment_months)
}

test "data_transmission_planning" {
  // 数据传输规划：计算传输数据包数量
  let data_size = 1024
  let packet_capacity = 75
  
  // 计算需要的包数量
  let packets_needed = divide_with_ceil(data_size, packet_capacity)
  assert_eq(14, packets_needed)
  
  // 最后一个包的数据量
  let last_packet_size = add(data_size, multiply(-packet_capacity, add(packets_needed, -1)))
  assert_eq(49, last_packet_size)
}

test "comprehensive_integration_test" {
  // 综合集成测试：结合所有导出函数
  let base_value = 10
  let multiplier = 3
  let increment = 7
  
  // 使用 multiply 和 add
  let calculated_value = add(multiply(base_value, multiplier), increment)
  assert_eq(37, calculated_value)
  
  // 使用 divide_with_ceil 进行分组
  let group_size = 8
  let groups_needed = divide_with_ceil(calculated_value, group_size)
  assert_eq(5, groups_needed)
  
  // 使用 greet 生成消息
  let message = greet("MoonBit")
  assert_eq_string("Hello, MoonBit!", message)
  
  // 验证所有断言函数
  assert_true(calculated_value > 30)
  assert_false(groups_needed < 3)
  assert_eq(37, calculated_value)
  assert_eq_string("Hello, MoonBit!", message)
}