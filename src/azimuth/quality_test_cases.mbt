// 高质量 MoonBit 测试用例
// 为 azimuth 库提供全面的测试覆盖

test "mathematical_series_sum" {
  // 计算等差数列的和：1 + 3 + 5 + 7 + 9 = 25
  let term1 = 1
  let term2 = 3
  let term3 = 5
  let term4 = 7
  let term5 = 9
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(25, sum)
}

test "business_profit_calculation" {
  // 计算业务利润：收入 - 成本 = 利润
  let revenue = 15000
  let cost = 8500
  let profit = @azimuth.add(revenue, -cost)
  @azimuth.assert_eq(6500, profit)
}

test "resource_allocation_efficiency" {
  // 资源分配效率计算
  let total_resources = 100
  let projects = 3
  let resources_per_project = @azimuth.divide_with_ceil(total_resources, projects)
  let actual_allocation = @azimuth.multiply(resources_per_project, projects)
  @azimuth.assert_eq(34, resources_per_project)
  @azimuth.assert_eq(102, actual_allocation)
}

test "temperature_conversion" {
  // 温度转换模拟：摄氏度到华氏度 F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)
}

test "compound_interest_simple" {
  // 简单复利计算
  let principal = 1000
  let rate = 5
  
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate), 100))
  
  @azimuth.assert_eq(1158, year3)
}

test "batch_processing_calculation" {
  // 批处理计算
  let items_per_batch = 47
  let total_items = 1000
  let batches_needed = @azimuth.divide_with_ceil(total_items, items_per_batch)
  let last_batch_size = @azimuth.add(total_items, -@azimuth.multiply(batches_needed - 1, items_per_batch))
  
  @azimuth.assert_eq(22, batches_needed)
  @azimuth.assert_eq(16, last_batch_size)
}

test "digital_storage_conversion" {
  // 数字存储转换：GB 到 MB
  let gigabytes = 5
  let megabytes_per_gb = 1024
  let total_megabytes = @azimuth.multiply(gigabytes, megabytes_per_gb)
  
  @azimuth.assert_eq(5120, total_megabytes)
}

test "network_packet_calculation" {
  // 网络数据包计算
  let data_size = 8000
  let packet_size = 1500
  let packets_needed = @azimuth.divide_with_ceil(data_size, packet_size)
  let last_packet_size = @azimuth.add(data_size, -@azimuth.multiply(packets_needed - 1, packet_size))
  
  @azimuth.assert_eq(6, packets_needed)
  @azimuth.assert_eq(500, last_packet_size)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = -8  // 相差8小时
  let remote_time = @azimuth.add(local_time, time_difference)
  
  // 处理负数时间
  let adjusted_time = if remote_time < 0 {
    @azimuth.add(remote_time, 24)
  } else {
    remote_time
  }
  
  @azimuth.assert_eq(6, adjusted_time)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算
  let annual_sales = 120000
  let average_inventory = 30000
  let turnover_rate = @azimuth.divide_with_ceil(annual_sales, average_inventory)
  
  @azimuth.assert_eq(4, turnover_rate)
}