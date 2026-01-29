// 新增的 MoonBit 测试用例 - 专注于实际应用场景
// 使用标准的 MoonBit 测试语法

test "financial_compound_interest" {
  // 金融复利计算场景
  let principal = 1000
  let rate = 5  // 5%
  let _years = 3
  
  // 简化的复利计算
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate), 100))
  
  @azimuth.assert_eq(1158, year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated = @azimuth.add(@azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = @azimuth.add(total_budget, -allocated)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 200
  let additional_projects = @azimuth.divide_with_ceil(remaining, small_project_cost)
  
  @azimuth.assert_eq(3500, allocated)
  @azimuth.assert_eq(1500, remaining)
  @azimuth.assert_eq(8, additional_projects)
}

test "time_management_workflow" {
  // 时间管理工作流测试
  let daily_hours = 8
  let meeting_hours = 2
  let coding_hours = 4
  let review_hours = 2
  
  // 验证时间分配
  let total_allocated = @azimuth.add(@azimuth.add(meeting_hours, coding_hours), review_hours)
  @azimuth.assert_eq(daily_hours, total_allocated)
  
  // 计算完成任务所需天数
  let total_tasks = 96
  let tasks_per_hour = 6
  let daily_capacity = @azimuth.multiply(tasks_per_hour, daily_hours)
  let days_needed = @azimuth.divide_with_ceil(total_tasks, daily_capacity)
  
  @azimuth.assert_eq(48, daily_capacity)
  @azimuth.assert_eq(2, days_needed)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算
  let initial_inventory = 500
  let purchases = 300
  let final_inventory = 200
  let sales_period = 30  // 天
  
  // 计算销售的商品数量
  let goods_sold = @azimuth.add(@azimuth.add(initial_inventory, purchases), -final_inventory)
  
  // 计算日均销售量
  let daily_sales = @azimuth.divide_with_ceil(goods_sold, sales_period)
  
  // 计算库存周转天数
  let turnover_days = @azimuth.divide_with_ceil(final_inventory, daily_sales)
  
  @azimuth.assert_eq(600, goods_sold)
  @azimuth.assert_eq(20, daily_sales)
  @azimuth.assert_eq(10, turnover_days)
}

test "data_batch_processing" {
  // 数据批处理测试
  let total_records = 1047
  let batch_size = 100
  
  // 计算需要的批次数
  let full_batches = @azimuth.divide_with_ceil(total_records, batch_size)
  
  // 计算最后一批的记录数
  let last_batch_size = total_records - @azimuth.multiply(full_batches - 1, batch_size)
  
  @azimuth.assert_eq(11, full_batches)
  @azimuth.assert_eq(47, last_batch_size)
}

test "network_load_balancing" {
  // 网络负载均衡测试
  let total_requests = 1250
  let server_count = 4
  
  // 计算每台服务器的基本负载
  let base_load = @azimuth.divide_with_ceil(total_requests, server_count)
  
  // 计算剩余需要分配的请求
  let distributed_requests = @azimuth.multiply(base_load, server_count)
  let remaining_requests = @azimuth.add(total_requests, -distributed_requests)
  
  @azimuth.assert_eq(313, base_load)
  @azimuth.assert_eq(1252, distributed_requests)
  @azimuth.assert_eq(-2, remaining_requests)  // 由于向上取整，可能会多分配
}

test "energy_efficiency_calculation" {
  // 能效计算测试
  let power_rating = 150  // 瓦特
  let hours_per_day = 8
  let days_per_month = 30
  let cost_per_kwh = 12  // 分钱
  
  // 计算月耗电量（向上取整到千瓦时）
  let daily_wh = @azimuth.multiply(power_rating, hours_per_day)
  let monthly_wh = @azimuth.multiply(daily_wh, days_per_month)
  let monthly_kwh = @azimuth.divide_with_ceil(monthly_wh, 1000)
  
  // 计算月电费
  let monthly_cost = @azimuth.divide_with_ceil(@azimuth.multiply(monthly_kwh, cost_per_kwh), 100)
  
  @azimuth.assert_eq(1200, daily_wh)
  @azimuth.assert_eq(36000, monthly_wh)
  @azimuth.assert_eq(36, monthly_kwh)
  @azimuth.assert_eq(5, monthly_cost)
}

test "educational_performance_tracking" {
  // 教育绩效跟踪测试
  let _student_count = 32
  let assignment_score = 85
  let midterm_score = 78
  let final_score = 92
  let participation_score = 88
  
  // 计算加权平均分
  let assignment_weight = 20  // 20%
  let midterm_weight = 30     // 30%
  let final_weight = 40       // 40%
  let participation_weight = 10 // 10%
  
  let weighted_sum = @azimuth.add(@azimuth.add(
    @azimuth.divide_with_ceil(@azimuth.multiply(assignment_score, assignment_weight), 100),
    @azimuth.divide_with_ceil(@azimuth.multiply(midterm_score, midterm_weight), 100)
  ), @azimuth.add(
    @azimuth.divide_with_ceil(@azimuth.multiply(final_score, final_weight), 100),
    @azimuth.divide_with_ceil(@azimuth.multiply(participation_score, participation_weight), 100)
  ))
  
  @azimuth.assert_eq(17, @azimuth.divide_with_ceil(@azimuth.multiply(assignment_score, assignment_weight), 100))
  @azimuth.assert_eq(24, @azimuth.divide_with_ceil(@azimuth.multiply(midterm_score, midterm_weight), 100))
  @azimuth.assert_eq(37, @azimuth.divide_with_ceil(@azimuth.multiply(final_score, final_weight), 100))
  @azimuth.assert_eq(9, @azimuth.divide_with_ceil(@azimuth.multiply(participation_score, participation_weight), 100))
  @azimuth.assert_eq(87, weighted_sum)
}

test "agricultural_planning" {
  // 农业规划测试
  let field_area = 150  // 亩
  let wheat_yield_per_acre = 400  // 公斤/亩
  let corn_yield_per_acre = 600   // 公斤/亩
  let wheat_acres = 90
  let corn_acres = 60
  
  // 验证总面积
  @azimuth.assert_eq(field_area, @azimuth.add(wheat_acres, corn_acres))
  
  // 计算总产量
  let wheat_total = @azimuth.multiply(wheat_yield_per_acre, wheat_acres)
  let corn_total = @azimuth.multiply(corn_yield_per_acre, corn_acres)
  let total_yield = @azimuth.add(wheat_total, corn_total)
  
  // 计算存储需要的仓库数量（每个仓库可存储10000公斤）
  let storage_capacity = 10000
  let warehouses_needed = @azimuth.divide_with_ceil(total_yield, storage_capacity)
  
  @azimuth.assert_eq(36000, wheat_total)
  @azimuth.assert_eq(36000, corn_total)
  @azimuth.assert_eq(72000, total_yield)
  @azimuth.assert_eq(8, warehouses_needed)
}

test "digital_content_distribution" {
  // 数字内容分发测试
  let content_size_mb = 850
  let user_count = 1250
  let bandwidth_mbps = 100
  let cache_hit_rate = 60  // 60%
  
  // 计算需要从源服务器传输的数据量
  let total_demand_mb = @azimuth.multiply(content_size_mb, user_count)
  let cache_served_mb = @azimuth.divide_with_ceil(@azimuth.multiply(total_demand_mb, cache_hit_rate), 100)
  let origin_server_mb = @azimuth.add(total_demand_mb, -cache_served_mb)
  
  // 计算传输时间（秒）
  let transfer_time_seconds = @azimuth.divide_with_ceil(origin_server_mb, bandwidth_mbps)
  
  // 计算需要的CDN节点数（每个节点可处理50000MB）
  let cdn_capacity = 50000
  let cdn_nodes_needed = @azimuth.divide_with_ceil(origin_server_mb, cdn_capacity)
  
  @azimuth.assert_eq(1062500, total_demand_mb)
  @azimuth.assert_eq(637500, cache_served_mb)
  @azimuth.assert_eq(425000, origin_server_mb)
  @azimuth.assert_eq(4250, transfer_time_seconds)
  @azimuth.assert_eq(9, cdn_nodes_needed)
}