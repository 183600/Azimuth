// Azimuth 增强标准测试用例
// 包含 8 个高质量的 MoonBit 测试用例，覆盖各种实际应用场景

use azimuth

test "statistical_calculations" {
  // 统计计算测试：平均值、中位数等场景
  let data_points = 45
  let sample_size = 7
  let groups_needed = azimuth.divide_with_ceil(data_points, sample_size)
  azimuth.assert_eq(7, groups_needed)  // ceil(45/7) = 7
  
  // 计算每组的平均数据点
  let avg_per_group = azimuth.divide_with_ceil(data_points, groups_needed)
  azimuth.assert_eq(7, avg_per_group)  // ceil(45/7) = 7
  
  // 验证总容量
  let total_capacity = azimuth.multiply(groups_needed, sample_size)
  azimuth.assert_eq(49, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = azimuth.subtract(total_capacity, data_points)
  azimuth.assert_eq(4, remaining_capacity)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算测试
  let annual_sales = 12000
  let average_inventory = 3000
  let turnover_rate = azimuth.divide_with_ceil(annual_sales, average_inventory)
  azimuth.assert_eq(4, turnover_rate)  // ceil(12000/3000) = 4
  
  // 计算周转天数
  let days_per_year = 365
  let days_per_turnover = azimuth.divide_with_ceil(days_per_year, turnover_rate)
  azimuth.assert_eq(92, days_per_turnover)  // ceil(365/4) = 92
  
  // 验证年度覆盖
  let total_coverage = azimuth.multiply(turnover_rate, days_per_turnover)
  azimuth.assert_eq(368, total_coverage)  // 略多于365天，符合向上取整特性
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_tasks = 127
  let team_members = 8
  let working_days = 15
  
  // 计算每人需要处理的任务数
  let tasks_per_person = azimuth.divide_with_ceil(total_tasks, team_members)
  azimuth.assert_eq(16, tasks_per_person)  // ceil(127/8) = 16
  
  // 计算每天需要完成的任务数
  let daily_tasks = azimuth.divide_with_ceil(total_tasks, working_days)
  azimuth.assert_eq(9, daily_tasks)  // ceil(127/15) = 9
  
  // 验证总处理能力
  let total_capacity = azimuth.multiply(tasks_per_person, team_members)
  azimuth.assert_eq(128, total_capacity)
  
  // 计算资源利用率
  let utilization = azimuth.divide_with_ceil(azimuth.multiply(total_tasks, 100), total_capacity)
  azimuth.assert_eq(100, utilization)  // ceil(12700/128) = 100%
}

test "network_bandwidth_planning" {
  // 网络带宽规划测试
  let users = 250
  let bandwidth_per_user_mbps = 5
  let total_bandwidth_needed = azimuth.multiply(users, bandwidth_per_user_mbps)
  azimuth.assert_eq(1250, total_bandwidth_needed)
  
  // 计算需要的连接数（每个连接最大100Mbps）
  let max_bandwidth_per_connection = 100
  let connections_needed = azimuth.divide_with_ceil(total_bandwidth_needed, max_bandwidth_per_connection)
  azimuth.assert_eq(13, connections_needed)  // ceil(1250/100) = 13
  
  // 计算实际可用带宽
  let available_bandwidth = azimuth.multiply(connections_needed, max_bandwidth_per_connection)
  azimuth.assert_eq(1300, available_bandwidth)
  
  // 计算冗余带宽
  let redundant_bandwidth = azimuth.subtract(available_bandwidth, total_bandwidth_needed)
  azimuth.assert_eq(50, redundant_bandwidth)
}

test "manufacturing_production_batch" {
  // 制造业生产批次计算测试
  let order_quantity = 987
  let batch_size = 50
  let production_batches = azimuth.divide_with_ceil(order_quantity, batch_size)
  azimuth.assert_eq(20, production_batches)  // ceil(987/50) = 20
  
  // 计算实际生产数量
  let actual_production = azimuth.multiply(production_batches, batch_size)
  azimuth.assert_eq(1000, actual_production)
  
  // 计算额外生产数量
  let extra_production = azimuth.subtract(actual_production, order_quantity)
  azimuth.assert_eq(13, extra_production)
  
  // 计算生产成本（假设每批次成本200元）
  let cost_per_batch = 200
  let total_cost = azimuth.multiply(production_batches, cost_per_batch)
  azimuth.assert_eq(4000, total_cost)
  
  // 计算单位成本
  let unit_cost = azimuth.divide_with_ceil(total_cost, actual_production)
  azimuth.assert_eq(4, unit_cost)  // ceil(4000/1000) = 4
}

test "educational_class_scheduling" {
  // 教育课程排程测试
  let students = 156
  let max_students_per_class = 25
  let classes_needed = azimuth.divide_with_ceil(students, max_students_per_class)
  azimuth.assert_eq(7, classes_needed)  // ceil(156/25) = 7
  
  // 计算总座位数
  let total_seats = azimuth.multiply(classes_needed, max_students_per_class)
  azimuth.assert_eq(175, total_seats)
  
  // 计算空余座位
  let empty_seats = azimuth.subtract(total_seats, students)
  azimuth.assert_eq(19, empty_seats)
  
  // 计算教师需求（每名教师最多教3个班）
  let max_classes_per_teacher = 3
  let teachers_needed = azimuth.divide_with_ceil(classes_needed, max_classes_per_teacher)
  azimuth.assert_eq(3, teachers_needed)  // ceil(7/3) = 3
  
  // 验证教师承载能力
  let teacher_capacity = azimuth.multiply(teachers_needed, max_classes_per_teacher)
  azimuth.assert_eq(9, teacher_capacity)
}

test "cloud_resource_scaling" {
  // 云资源扩展测试
  let current_load = 75
  let server_capacity = 20
  let current_servers = azimuth.divide_with_ceil(current_load, server_capacity)
  azimuth.assert_eq(4, current_servers)  // ceil(75/20) = 4
  
  // 预期负载增长
  let projected_load = azimuth.add(current_load, 60)  // 增长到135
  let projected_servers = azimuth.divide_with_ceil(projected_load, server_capacity)
  azimuth.assert_eq(7, projected_servers)  // ceil(135/20) = 7
  
  // 计算需要增加的服务器
  let additional_servers = azimuth.subtract(projected_servers, current_servers)
  azimuth.assert_eq(3, additional_servers)
  
  // 计算扩展后的总容量
  let total_capacity = azimuth.multiply(projected_servers, server_capacity)
  azimuth.assert_eq(140, total_capacity)
  
  // 计算扩展后的利用率
  let utilization = azimuth.divide_with_ceil(azimuth.multiply(projected_load, 100), total_capacity)
  azimuth.assert_eq(97, utilization)  // ceil(13500/140) = 97%
}

test "logistics_delivery_optimization" {
  // 物流配送优化测试
  let packages = 342
  let truck_capacity = 28
  let delivery_days = 6
  
  // 计算需要的卡车数量
  let trucks_needed = azimuth.divide_with_ceil(packages, truck_capacity)
  azimuth.assert_eq(13, trucks_needed)  // ceil(342/28) = 13
  
  // 计算每天需要派送的卡车数
  let daily_trucks = azimuth.divide_with_ceil(trucks_needed, delivery_days)
  azimuth.assert_eq(3, daily_trucks)  // ceil(13/6) = 3
  
  // 验证每日派送能力
  let daily_capacity = azimuth.multiply(daily_trucks, truck_capacity)
  azimuth.assert_eq(84, daily_capacity)
  
  // 计算总派送能力
  let total_capacity = azimuth.multiply(daily_trucks, truck_capacity, delivery_days)
  azimuth.assert_eq(504, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = azimuth.subtract(total_capacity, packages)
  azimuth.assert_eq(162, remaining_capacity)
  
  // 计算配送效率
  let efficiency = azimuth.divide_with_ceil(azimuth.multiply(packages, 100), total_capacity)
  azimuth.assert_eq(68, efficiency)  // ceil(34200/504) = 68%
}