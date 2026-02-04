// 新增的 MoonBit 标准测试用例 - 8个高质量测试用例
// 为 azimuth 项目添加额外的测试覆盖

test "performance_large_number_operations" {
  // 大数运算性能测试
  let large_num = 10000
  let another_large = 20000
  
  assert_eq(30000, add(large_num, another_large))
  assert_eq(200000000, multiply(large_num, another_large))
  assert_eq(1, divide_with_ceil(large_num, another_large))
  assert_eq(-10000, subtract(large_num, another_large))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试：电商订单处理
  let order_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, order_items)
  assert_eq(1, remaining_space)
  
  // 验证订单处理效率
  let efficiency_percentage = divide_with_ceil(
    multiply(order_items, 100), 
    total_capacity
  )
  assert_eq(98, efficiency_percentage)
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  let n = 10
  
  // 计算等差数列和：1 + 2 + ... + 10 = 55
  let arithmetic_sum = divide_with_ceil(
    multiply(n, add(n, 1)), 
    2
  )
  assert_eq(55, arithmetic_sum)
  
  // 计算等比数列和：2^0 + 2^1 + ... + 2^3 = 15
  let geometric_sum = subtract(
    multiply(2, multiply(2, multiply(2, 2))), 
    1
  )
  assert_eq(15, geometric_sum)
}

test "unicode_and_multilingual_support" {
  // Unicode和多语言支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
}

test "error_resilience_and_edge_cases" {
  // 错误恢复能力和边缘情况测试
  // 极值除法测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  
  // 极值加法测试
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  
  // 极值乘法测试
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(0, multiply(-1, 0))
  assert_eq(0, multiply(999999, 0))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_tasks = 125
  let team_members = 8
  let working_days = 5
  
  // 计算每人每天需要完成的任务数
  let total_person_days = multiply(team_members, working_days)
  let tasks_per_person_day = divide_with_ceil(total_tasks, total_person_days)
  assert_eq(4, tasks_per_person_day)
  
  // 验证总处理能力
  let total_capacity = multiply(tasks_per_person_day, total_person_days)
  assert_eq(160, total_capacity)
  
  // 计算剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(35, remaining_capacity)
}

test "financial_calculation_precision" {
  // 金融计算精度测试
  let principal = 100000
  let monthly_payment = 3500
  let months_needed = divide_with_ceil(principal, monthly_payment)
  
  assert_eq(29, months_needed)
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(101500, total_payment)
  
  // 计算利息
  let total_interest = subtract(total_payment, principal)
  assert_eq(1500, total_interest)
  
  // 计算最后一个月的实际支付金额
  let last_month_payment = subtract(
    total_payment, 
    multiply(subtract(months_needed, 1), monthly_payment)
  )
  assert_eq(1500, last_month_payment)
}

test "scientific_computation_workflow" {
  // 科学计算工作流测试
  let samples = 1000
  let sample_rate = 44100
  let buffer_size = 512
  
  // 计算缓冲区数量
  let buffers_needed = divide_with_ceil(samples, buffer_size)
  assert_eq(2, buffers_needed)
  
  // 计算总采样时间（秒）
  let total_duration_seconds = divide_with_ceil(samples, sample_rate)
  assert_eq(1, total_duration_seconds)
  
  // 计算总缓冲区大小
  let total_buffer_size = multiply(buffers_needed, buffer_size)
  assert_eq(1024, total_buffer_size)
  
  // 计算剩余缓冲区空间
  let remaining_buffer_space = subtract(total_buffer_size, samples)
  assert_eq(24, remaining_buffer_space)
  
  // 计算内存利用率
  let memory_utilization = divide_with_ceil(
    multiply(samples, 100), 
    total_buffer_size
  )
  assert_eq(98, memory_utilization)
}