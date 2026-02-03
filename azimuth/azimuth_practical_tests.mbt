// Azimuth 实用测试用例 - 包含8个高质量的标准 MoonBit 测试用例
// 专注于实际应用场景和业务逻辑测试

test "data_processing_pipeline" {
  // 数据处理流水线测试
  // 场景：处理传感器数据，计算平均值和向上取整
  let sensor_readings = 125
  let sensor_count = 8
  let average = divide_with_ceil(sensor_readings, sensor_count)
  assert_eq(16, average)  // ceil(125/8) = 16
  
  // 计算总容量与实际读数的差异
  let total_capacity = multiply(average, sensor_count)
  let difference = subtract(total_capacity, sensor_readings)
  assert_eq(3, difference)  // 16*8 - 125 = 128 - 125 = 3
}

test "resource_allocation_efficiency" {
  // 资源分配效率测试
  // 场景：计算服务器资源分配的最优方案
  let total_requests = 1000
  let server_capacity = 150
  let servers_needed = divide_with_ceil(total_requests, server_capacity)
  assert_eq(7, servers_needed)  // ceil(1000/150) = 7
  
  // 计算资源利用率
  let total_capacity = multiply(servers_needed, server_capacity)
  let utilization = divide_with_ceil(multiply(total_requests, 100), total_capacity)
  assert_eq(96, utilization)  // floor(100000/1050) = 95%
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, total_requests)
  assert_eq(50, remaining_capacity)  // 7*150 - 1000 = 1050 - 1000 = 50
}

test "batch_processing_workflow" {
  // 批处理工作流测试
  // 场景：批量处理文件，计算需要的处理轮次
  let total_files = 247
  let batch_size = 32
  let batches_needed = divide_with_ceil(total_files, batch_size)
  assert_eq(8, batches_needed)  // ceil(247/32) = 8
  
  // 计算最后一轮处理的文件数
  let full_batches = subtract(batches_needed, 1)
  let files_in_full_batches = multiply(full_batches, batch_size)
  let files_in_last_batch = subtract(total_files, files_in_full_batches)
  assert_eq(27, files_in_last_batch)  // 247 - 7*32 = 247 - 224 = 23
  
  // 验证总处理量
  let total_processed = add(files_in_full_batches, files_in_last_batch)
  assert_eq(total_files, total_processed)
}

test "time_management_calculation" {
  // 时间管理计算测试
  // 场景：计算项目完成时间和资源需求
  let project_tasks = 89
  let daily_capacity = 12
  let days_needed = divide_with_ceil(project_tasks, daily_capacity)
  assert_eq(8, days_needed)  // ceil(89/12) = 8
  
  // 计算工作日和工作周数
  let work_days_per_week = 5
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  assert_eq(2, weeks_needed)  // ceil(8/5) = 2
  
  // 计算总可用工作日
  let total_work_days = multiply(weeks_needed, work_days_per_week)
  assert_eq(10, total_work_days)  // 2*5 = 10
  
  // 计算剩余工作日
  let remaining_days = subtract(total_work_days, days_needed)
  assert_eq(2, remaining_days)  // 10 - 8 = 2
}

test "inventory_optimization" {
  // 库存优化测试
  // 场景：优化库存分配，计算最佳订购策略
  let monthly_demand = 450
  let package_size = 24
  let packages_needed = divide_with_ceil(monthly_demand, package_size)
  assert_eq(19, packages_needed)  // ceil(450/24) = 19
  
  // 计算总库存量
  let total_inventory = multiply(packages_needed, package_size)
  assert_eq(456, total_inventory)  // 19*24 = 456
  
  // 计算安全库存
  let safety_stock = subtract(total_inventory, monthly_demand)
  assert_eq(6, safety_stock)  // 456 - 450 = 6
  
  // 计算库存覆盖率
  let coverage_months = divide_with_ceil(total_inventory, monthly_demand)
  assert_eq(2, coverage_months)  // ceil(456/450) = 2
}

test "financial_budget_planning" {
  // 财务预算规划测试
  // 场景：部门预算分配和支出控制
  let total_budget = 50000
  let departments = 7
  let base_allocation = divide_with_ceil(total_budget, departments)
  assert_eq(7143, base_allocation)  // ceil(50000/7) = 7143
  
  // 计算总分配金额
  let total_allocated = multiply(base_allocation, departments)
  assert_eq(50001, total_allocated)  // 7143*7 = 50001
  
  // 计算预算超支
  let over_budget = subtract(total_allocated, total_budget)
  assert_eq(1, over_budget)  // 50001 - 50000 = 1
  
  // 调整分配方案
  let adjusted_allocation = subtract(base_allocation, 1)
  let adjusted_total = multiply(adjusted_allocation, departments)
  let remaining_budget = subtract(total_budget, adjusted_total)
  assert_eq(7, remaining_budget)  // 50000 - 7142*7 = 50000 - 49994 = 6
}

test "network_bandwidth_planning" {
  // 网络带宽规划测试
  // 场景：计算网络带宽需求和分配策略
  let total_users = 1250
  let bandwidth_per_user = 5  // Mbps
  let total_bandwidth_needed = multiply(total_users, bandwidth_per_user)
  assert_eq(6250, total_bandwidth_needed)  // 1250*5 = 6250
  
  // 计算需要的连接数
  let connection_capacity = 100  // Mbps per connection
  let connections_needed = divide_with_ceil(total_bandwidth_needed, connection_capacity)
  assert_eq(63, connections_needed)  // ceil(6250/100) = 63
  
  // 计算实际可用带宽
  let actual_bandwidth = multiply(connections_needed, connection_capacity)
  assert_eq(6300, actual_bandwidth)  // 63*100 = 6300
  
  // 计算带宽余量
  let bandwidth_buffer = subtract(actual_bandwidth, total_bandwidth_needed)
  assert_eq(50, bandwidth_buffer)  // 6300 - 6250 = 50
}

test "educational_class_scheduling" {
  // 教育课程安排测试
  // 场景：学校课程安排和教室分配
  let total_students = 285
  let classroom_capacity = 30
  let classrooms_needed = divide_with_ceil(total_students, classroom_capacity)
  assert_eq(10, classrooms_needed)  // ceil(285/30) = 10
  
  // 计算总座位数
  let total_seats = multiply(classrooms_needed, classroom_capacity)
  assert_eq(300, total_seats)  // 10*30 = 300
  
  // 计算空余座位
  let empty_seats = subtract(total_seats, total_students)
  assert_eq(15, empty_seats)  // 300 - 285 = 15
  
  // 计算座位利用率
  let utilization_rate = divide_with_ceil(multiply(total_students, 100), total_seats)
  assert_eq(95, utilization_rate)  // floor(28500/300) = 95%
}