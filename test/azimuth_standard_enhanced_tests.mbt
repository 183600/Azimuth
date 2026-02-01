// Azimuth 项目标准增强测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "statistical_calculations" {
  // 统计计算测试
  let numbers_sum = @azimuth.add(15, @azimuth.add(25, @azimuth.add(35, 45)))
  let count = 4
  let average = @azimuth.divide_with_ceil(numbers_sum, count)
  
  @azimuth.assert_eq(120, numbers_sum)  // 15 + 25 + 35 + 45 = 120
  @azimuth.assert_eq(30, average)       // ceil(120/4) = 30
}

test "time_calculations_scenario" {
  // 时间计算场景测试
  let hours_per_day = 24
  let days_per_week = 7
  let hours_per_week = @azimuth.multiply(hours_per_day, days_per_week)
  
  let work_hours_per_day = 8
  let work_days_per_week = 5
  let work_hours_per_week = @azimuth.multiply(work_hours_per_day, work_days_per_week)
  
  let overtime_hours = @azimuth.subtract(hours_per_week, work_hours_per_week)
  
  @azimuth.assert_eq(168, hours_per_week)      // 24 * 7 = 168
  @azimuth.assert_eq(40, work_hours_per_week)  // 8 * 5 = 40
  @azimuth.assert_eq(128, overtime_hours)      // 168 - 40 = 128
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  let length = 12
  let width = 8
  
  // 矩形面积
  let rectangle_area = @azimuth.multiply(length, width)
  
  // 正方形面积（边长为矩形周长的一半）
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  let square_side = @azimuth.divide_with_ceil(perimeter, 4)
  let square_area = @azimuth.multiply(square_side, square_side)
  
  @azimuth.assert_eq(96, rectangle_area)    // 12 * 8 = 96
  @azimuth.assert_eq(40, perimeter)        // 2*12 + 2*8 = 40
  @azimuth.assert_eq(10, square_side)      // ceil(40/4) = 10
  @azimuth.assert_eq(100, square_area)     // 10 * 10 = 100
}

test "inventory_batch_processing" {
  // 库存批次处理测试
  let total_items = 234
  let batch_size = 25
  let batches_needed = @azimuth.divide_with_ceil(total_items, batch_size)
  
  @azimuth.assert_eq(10, batches_needed)  // ceil(234/25) = 10
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(batches_needed, batch_size)
  @azimuth.assert_eq(250, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(16, remaining_capacity)
}

test "temperature_conversion_scenario" {
  // 温度转换场景测试（简化的摄氏度到华氏度）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9), @azimuth.divide_with_ceil(@azimuth.multiply(5, 32), 5))
  
  // 简化公式：F = (C * 9/5) + 32 = (25 * 9/5) + 32 = 45 + 32 = 77
  let step1 = @azimuth.multiply(celsius, 9)
  let step2 = @azimuth.divide_with_ceil(step1, 5)
  let result = @azimuth.add(step2, 32)
  
  @azimuth.assert_eq(225, step1)  // 25 * 9 = 225
  @azimuth.assert_eq(45, step2)   // ceil(225/5) = 45
  @azimuth.assert_eq(77, result)  // 45 + 32 = 77
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let project_duration_days = 45
  let team_members = 6
  let total_person_days = @azimuth.multiply(project_duration_days, team_members)
  
  let critical_tasks = 120
  let people_per_critical_task = @azimuth.divide_with_ceil(total_person_days, critical_tasks)
  
  @azimuth.assert_eq(270, total_person_days)      // 45 * 6 = 270
  @azimuth.assert_eq(3, people_per_critical_task) // ceil(270/120) = 3
  
  // 验证资源分配合理性
  let allocated_resources = @azimuth.multiply(people_per_critical_task, critical_tasks)
  @azimuth.assert_eq(360, allocated_resources)    // 3 * 120 = 360
}

test "financial_compound_interest" {
  // 金融复利计算测试（简化版本）
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化计算：每年增加本金的 5%
  let year1_end = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let year2_end = @azimuth.add(year1_end, @azimuth.divide_with_ceil(@azimuth.multiply(year1_end, rate_percent), 100))
  let year3_end = @azimuth.add(year2_end, @azimuth.divide_with_ceil(@azimuth.multiply(year2_end, rate_percent), 100))
  
  @azimuth.assert_eq(1050, year1_end)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  @azimuth.assert_eq(1103, year2_end)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  @azimuth.assert_eq(1158, year3_end)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}

test "network_bandwidth_planning" {
  // 网络带宽规划测试
  let users = 150
  let bandwidth_per_user_mbps = 10
  let total_bandwidth_needed = @azimuth.multiply(users, bandwidth_per_user_mbps)
  
  let available_bandwidth_per_link_mbps = 1000
  let links_needed = @azimuth.divide_with_ceil(total_bandwidth_needed, available_bandwidth_per_link_mbps)
  
  @azimuth.assert_eq(1500, total_bandwidth_needed)  // 150 * 10 = 1500 Mbps
  @azimuth.assert_eq(2, links_needed)              // ceil(1500/1000) = 2
  
  // 验证总带宽容量
  let total_capacity = @azimuth.multiply(links_needed, available_bandwidth_per_link_mbps)
  @azimuth.assert_eq(2000, total_capacity)
  
  // 验证冗余带宽
  let redundant_bandwidth = @azimuth.subtract(total_capacity, total_bandwidth_needed)
  @azimuth.assert_eq(500, redundant_bandwidth)
}