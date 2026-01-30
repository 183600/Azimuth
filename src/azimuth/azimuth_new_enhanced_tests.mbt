// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "string_greeting_with_emoji" {
  // 测试包含表情符号的字符串问候
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 💻!", greet("💻"))
  assert_eq_string("Hello, 🎯!", greet("🎯"))
}

test "complex_financial_calculation" {
  // 复杂的金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 计算复利（简化版本，使用向上取整）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(10500, year1_amount)
  assert_eq(11025, year2_amount)
  assert_eq(11576, year3_amount)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(3500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(5, additional_projects)
}

test "time_zone_conversion" {
  // 时区转换计算模拟
  let local_time = 14  // 下午2点
  let time_difference = 8  // 8小时时差
  
  // 计算远程时间
  let remote_time = add(local_time, time_difference)
  let adjusted_remote_time = if remote_time > 24 {
    add(remote_time, -24)
  } else {
    remote_time
  }
  
  assert_eq(22, adjusted_remote_time)
}

test "batch_processing_simulation" {
  // 批处理模拟计算
  let total_records = 1047
  let batch_size = 100
  
  // 计算需要的批次数
  let full_batches = divide_with_ceil(total_records, batch_size)
  
  // 计算最后一批的记录数
  let last_batch_size = if total_records % batch_size == 0 {
    batch_size
  } else {
    total_records % batch_size
  }
  
  assert_eq(11, full_batches)
  assert_eq(47, last_batch_size)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算
  let beginning_inventory = 500
  let ending_inventory = 300
  let cost_of_goods_sold = 2400
  
  // 计算平均库存
  let average_inventory = divide_with_ceil(add(beginning_inventory, ending_inventory), 2)
  
  // 计算库存周转率
  let inventory_turnover = divide_with_ceil(cost_of_goods_sold, average_inventory)
  
  assert_eq(400, average_inventory)
  assert_eq(6, inventory_turnover)
}

test "data_structure_simulation" {
  // 数据结构模拟 - 栈操作
  let initial_stack_size = 0
  
  // 模拟压栈操作
  let push_operations = 5
  let final_stack_size = add(initial_stack_size, push_operations)
  
  // 模拟弹栈操作
  let pop_operations = 3
  let remaining_stack_size = add(final_stack_size, -pop_operations)
  
  assert_eq(5, final_stack_size)
  assert_eq(2, remaining_stack_size)
}

test "circular_buffer_calculation" {
  // 循环缓冲区计算
  let buffer_size = 10
  let current_position = 8
  let steps_forward = 5
  
  // 计算新位置（循环）
  let total_steps = add(current_position, steps_forward)
  let new_position = if total_steps >= buffer_size {
    add(total_steps, -multiply(divide_with_ceil(total_steps, buffer_size), buffer_size))
  } else {
    total_steps
  }
  
  assert_eq(3, new_position)
}

test "temperature_conversion" {
  // 温度转换计算模拟
  let celsius = 25
  let fahrenheit_offset = 32
  let conversion_factor = 5
  
  // 摄氏度转华氏度（简化计算）
  let fahrenheit = add(multiply(celsius, 9), divide_with_ceil(fahrenheit_offset * conversion_factor, conversion_factor))
  let adjusted_fahrenheit = divide_with_ceil(fahrenheit, 5)
  
  assert_eq(77, adjusted_fahrenheit)
}

test "load_balancing_simulation" {
  // 负载均衡模拟
  let total_requests = 157
  let server_count = 4
  
  // 计算每台服务器的基本负载
  let base_load_per_server = divide_with_ceil(total_requests, server_count)
  
  // 计算剩余请求数
  let distributed_requests = multiply(base_load_per_server, server_count)
  let remaining_requests = add(total_requests, -distributed_requests)
  
  assert_eq(40, base_load_per_server)
  assert_eq(-3, remaining_requests)
}