// 遥测资源利用率测试用例

test "cpu_utilization_monitoring" {
  // 测试CPU利用率监控
  
  let cpu_metrics = [
    ("user_time", 35.2),
    ("system_time", 12.8),
    ("idle_time", 45.5),
    ("iowait_time", 6.5)
  ]
  
  // 验证CPU指标数量
  assert_eq(cpu_metrics.length(), 4)
  
  // 验证用户时间
  assert_eq(cpu_metrics[0].0, "user_time")
  assert_eq(cpu_metrics[0].1 > 30.0, true)
  assert_eq(cpu_metrics[0].1 < 40.0, true)
  
  // 验证系统时间
  assert_eq(cpu_metrics[1].0, "system_time")
  assert_eq(cpu_metrics[1].1 > 10.0, true)
  assert_eq(cpu_metrics[1].1 < 15.0, true)
  
  // 验证空闲时间
  assert_eq(cpu_metrics[2].0, "idle_time")
  assert_eq(cpu_metrics[2].1 > 40.0, true)
  
  // 验证IO等待时间
  assert_eq(cpu_metrics[3].0, "iowait_time")
  assert_eq(cpu_metrics[3].1 < 10.0, true)
  
  // 计算总CPU利用率
  let total_utilization = cpu_metrics[0].1 + cpu_metrics[1].1 + cpu_metrics[3].1
  assert_eq(total_utilization, 54.5) // 35.2 + 12.8 + 6.5
  
  // 验证CPU利用率合理性
  assert_eq(total_utilization < 80.0, true)
  assert_eq(total_utilization > 40.0, true)
  
  // 计算CPU负载分布
  let user_percentage = (cpu_metrics[0].1 * 100) / total_utilization
  let system_percentage = (cpu_metrics[1].1 * 100) / total_utilization
  let iowait_percentage = (cpu_metrics[3].1 * 100) / total_utilization
  
  // 验证负载分布
  assert_eq(user_percentage, 64) // 35.2/54.5 * 100 ≈ 64%
  assert_eq(system_percentage, 23) // 12.8/54.5 * 100 ≈ 23%
  assert_eq(iowait_percentage, 11) // 6.5/54.5 * 100 ≈ 11%
}

test "memory_utilization_monitoring" {
  // 测试内存利用率监控
  
  let memory_metrics = [
    ("total_gb", 16.0),
    ("used_gb", 12.8),
    ("free_gb", 3.2),
    ("cached_gb", 2.1),
    ("buffers_gb", 0.8)
  ]
  
  // 验证内存指标数量
  assert_eq(memory_metrics.length(), 5)
  
  // 验证总内存
  assert_eq(memory_metrics[0].0, "total_gb")
  assert_eq(memory_metrics[0].1, 16.0)
  
  // 验证已用内存
  assert_eq(memory_metrics[1].0, "used_gb")
  assert_eq(memory_metrics[1].1 > 10.0, true)
  
  // 验证空闲内存
  assert_eq(memory_metrics[2].0, "free_gb")
  assert_eq(memory_metrics[2].1 > 3.0, true)
  
  // 验证缓存内存
  assert_eq(memory_metrics[3].0, "cached_gb")
  assert_eq(memory_metrics[3].1 > 2.0, true)
  
  // 验证缓冲区内存
  assert_eq(memory_metrics[4].0, "buffers_gb")
  assert_eq(memory_metrics[4].1 < 1.0, true)
  
  // 验证内存总量平衡
  let memory_balance = memory_metrics[1].1 + memory_metrics[2].1
  assert_eq(memory_balance, 16.0) // 12.8 + 3.2 = 16.0
  
  // 计算内存利用率
  let memory_utilization = (memory_metrics[1].1 * 100) / memory_metrics[0].1
  assert_eq(memory_utilization, 80) // 12.8/16.0 * 100 = 80%
  
  // 计算实际可用内存 (包括缓存和缓冲区)
  let available_memory = memory_metrics[2].1 + memory_metrics[3].1 + memory_metrics[4].1
  assert_eq(available_memory, 6.1) // 3.2 + 2.1 + 0.8
  
  // 计算实际内存压力
  let memory_pressure = (memory_metrics[1].1 * 100) / (memory_metrics[0].1 - memory_metrics[3].1 - memory_metrics[4].1)
  assert_eq(memory_pressure > 80.0, true)
}

test "disk_utilization_monitoring" {
  // 测试磁盘利用率监控
  
  let disk_metrics = [
    ("/var", 100.0, 75.5, 24.5),    // (挂载点, 总容量GB, 已用GB, 可用GB)
    ("/home", 500.0, 350.2, 149.8),
    ("/tmp", 50.0, 15.3, 34.7),
    ("/opt", 200.0, 120.8, 79.2)
  ]
  
  // 验证磁盘指标数量
  assert_eq(disk_metrics.length(), 4)
  
  // 验证/var分区
  assert_eq(disk_metrics[0].0, "/var")
  assert_eq(disk_metrics[0].1, 100.0)
  assert_eq(disk_metrics[0].2 + disk_metrics[0].3, disk_metrics[0].1)
  
  // 验证/home分区
  assert_eq(disk_metrics[1].0, "/home")
  assert_eq(disk_metrics[1].1, 500.0)
  assert_eq(disk_metrics[1].2 + disk_metrics[1].3, disk_metrics[1].1)
  
  // 计算/var分区利用率
  let var_utilization = (disk_metrics[0].2 * 100) / disk_metrics[0].1
  assert_eq(var_utilization, 75) // 75.5/100.0 * 100 ≈ 75%
  
  // 计算/home分区利用率
  let home_utilization = (disk_metrics[1].2 * 100) / disk_metrics[1].1
  assert_eq(home_utilization, 70) // 350.2/500.0 * 100 ≈ 70%
  
  // 计算/tmp分区利用率
  let tmp_utilization = (disk_metrics[2].2 * 100) / disk_metrics[2].1
  assert_eq(tmp_utilization, 30) // 15.3/50.0 * 100 ≈ 30%
  
  // 计算总磁盘容量
  let mut total_capacity = 0.0
  let mut total_used = 0.0
  for disk in disk_metrics {
    total_capacity = total_capacity + disk.1
    total_used = total_used + disk.2
  }
  
  // 验证总容量
  assert_eq(total_capacity, 850.0) // 100 + 500 + 50 + 200
  
  // 验证总使用量
  assert_eq(total_used, 561.8) // 75.5 + 350.2 + 15.3 + 120.8
  
  // 计算总体磁盘利用率
  let overall_utilization = (total_used * 100) / total_capacity
  assert_eq(overall_utilization, 66) // 561.8/850.0 * 100 ≈ 66%
  
  // 检测磁盘空间不足的分区
  let low_space_partitions = []
  for disk in disk_metrics {
    let utilization = (disk.2 * 100) / disk.1
    if utilization > 80 {
      low_space_partitions.push(disk.0)
    }
  }
  
  // 验证低空间分区检测
  assert_eq(low_space_partitions.length(), 0) // 没有分区超过80%
}

test "network_utilization_monitoring" {
  // 测试网络利用率监控
  
  let network_interfaces = [
    ("eth0", 1000.0, 450.2, 380.5),  // (接口名, 带宽Mbps, 入站Mbps, 出站Mbps)
    ("wlan0", 300.0, 120.8, 95.3),
    ("lo", 1000.0, 25.6, 25.6)
  ]
  
  // 验证网络接口数量
  assert_eq(network_interfaces.length(), 3)
  
  // 验证eth0接口
  assert_eq(network_interfaces[0].0, "eth0")
  assert_eq(network_interfaces[0].1, 1000.0)
  
  // 验证wlan0接口
  assert_eq(network_interfaces[1].0, "wlan0")
  assert_eq(network_interfaces[1].1, 300.0)
  
  // 验证lo接口
  assert_eq(network_interfaces[2].0, "lo")
  assert_eq(network_interfaces[2].2, network_interfaces[2].3) // 回环接口入站出站相等
  
  // 计算eth0利用率
  let eth0_utilization = ((network_interfaces[0].2 + network_interfaces[0].3) * 100) / network_interfaces[0].1
  assert_eq(eth0_utilization, 83) // (450.2 + 380.5)/1000.0 * 100 ≈ 83%
  
  // 计算wlan0利用率
  let wlan0_utilization = ((network_interfaces[1].2 + network_interfaces[1].3) * 100) / network_interfaces[1].1
  assert_eq(wlan0_utilization, 72) // (120.8 + 95.3)/300.0 * 100 ≈ 72%
  
  // 计算总网络流量
  let mut total_inbound = 0.0
  let mut total_outbound = 0.0
  for interface in network_interfaces {
    total_inbound = total_inbound + interface.2
    total_outbound = total_outbound + interface.3
  }
  
  // 验证总流量
  assert_eq(total_inbound, 596.6) // 450.2 + 120.8 + 25.6
  assert_eq(total_outbound, 501.4) // 380.5 + 95.3 + 25.6
  
  // 计算网络流量比率
  let traffic_ratio = total_inbound / total_outbound
  assert_eq(traffic_ratio > 1.0, true)
  assert_eq(traffic_ratio < 1.2, true)
  
  // 检测高利用率接口
  let high_utilization_interfaces = []
  for interface in network_interfaces {
    let utilization = ((interface.2 + interface.3) * 100) / interface.1
    if utilization > 80 {
      high_utilization_interfaces.push(interface.0)
    }
  }
  
  // 验证高利用率接口检测
  assert_eq(high_utilization_interfaces.length(), 1)
  assert_eq(high_utilization_interfaces[0], "eth0")
}

test "resource_utilization_trends" {
  // 测试资源利用率趋势
  
  let hourly_utilization = [
    (0, 25.5),   // (小时, CPU利用率%)
    (1, 22.3),
    (2, 20.1),
    (3, 18.7),
    (4, 19.2),
    (5, 24.8),
    (6, 35.6),
    (7, 45.2),
    (8, 65.8),
    (9, 78.3),
    (10, 82.1),
    (11, 79.6)
  ]
  
  // 验证小时数据数量
  assert_eq(hourly_utilization.length(), 12)
  
  // 验证低峰期 (凌晨0-5点)
  let mut low_hour_sum = 0.0
  let mut i = 0
  while i < 6 {
    low_hour_sum = low_hour_sum + hourly_utilization[i].1
    i = i + 1
  }
  let low_hour_avg = low_hour_sum / 6.0
  assert_eq(low_hour_avg < 25.0, true)
  
  // 验证高峰期 (上午8-11点)
  let mut peak_hour_sum = 0.0
  i = 8
  while i < 12 {
    peak_hour_sum = peak_hour_sum + hourly_utilization[i].1
    i = i + 1
  }
  let peak_hour_avg = peak_hour_sum / 4.0
  assert_eq(peak_hour_avg > 75.0, true)
  
  // 计算最大利用率
  let mut max_utilization = hourly_utilization[0].1
  i = 1
  while i < hourly_utilization.length() {
    if hourly_utilization[i].1 > max_utilization {
      max_utilization = hourly_utilization[i].1
    }
    i = i + 1
  }
  assert_eq(max_utilization, 82.1)
  
  // 计算最小利用率
  let mut min_utilization = hourly_utilization[0].1
  i = 1
  while i < hourly_utilization.length() {
    if hourly_utilization[i].1 < min_utilization {
      min_utilization = hourly_utilization[i].1
    }
    i = i + 1
  }
  assert_eq(min_utilization, 18.7)
  
  // 计算利用率范围
  let utilization_range = max_utilization - min_utilization
  assert_eq(utilization_range, 63.4) // 82.1 - 18.7
  
  // 检测增长率 (从6点到10点)
  let growth_rate = hourly_utilization[10].1 - hourly_utilization[6].1
  assert_eq(growth_rate, 46.5) // 82.1 - 35.6
  
  // 预测下一小时利用率 (简单线性预测)
  let last_two_diff = hourly_utilization[11].1 - hourly_utilization[10].1
  let predicted_next_hour = hourly_utilization[11].1 + last_two_diff
  assert_eq(predicted_next_hour, 77.1) // 79.6 + (79.6 - 82.1)
}