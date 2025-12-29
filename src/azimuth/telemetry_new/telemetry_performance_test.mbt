// 遥测性能监控测试用例

test "telemetry_cpu_performance_monitoring" {
  // 测试遥测CPU性能监控
  
  let cpu_metrics = {
    "cpu_usage_percent": 75.2,
    "cpu_load_average_1m": 2.5,
    "cpu_load_average_5m": 2.1,
    "cpu_load_average_15m": 1.8,
    "cpu_cores": 8,
    "cpu_context_switches": 125000,
    "cpu_interrupts": 45000,
    "measurement_timestamp": 1672531200
  }
  
  // 验证CPU指标完整性
  assert_eq(cpu_metrics.contains("cpu_usage_percent"), true)
  assert_eq(cpu_metrics.contains("cpu_load_average_1m"), true)
  assert_eq(cpu_metrics.contains("cpu_cores"), true)
  assert_eq(cpu_metrics.contains("measurement_timestamp"), true)
  
  // 验证CPU使用率
  let cpu_usage = cpu_metrics.get("cpu_usage_percent", 0.0)
  assert_eq(cpu_usage >= 0.0, true)
  assert_eq(cpu_usage <= 100.0, true)
  
  // 验证负载平均值
  let load_1m = cpu_metrics.get("cpu_load_average_1m", 0.0)
  let load_5m = cpu_metrics.get("cpu_load_average_5m", 0.0)
  let load_15m = cpu_metrics.get("cpu_load_average_15m", 0.0)
  let cpu_cores = cpu_metrics.get("cpu_cores", 0)
  
  assert_eq(load_1m >= 0.0, true)
  assert_eq(load_5m >= 0.0, true)
  assert_eq(load_15m >= 0.0, true)
  assert_eq(load_1m >= load_5m, true)  // 1分钟负载应该 >= 5分钟负载
  assert_eq(load_5m >= load_15m, true)  // 5分钟负载应该 >= 15分钟负载
  assert_eq(load_1m <= cpu_cores.to_double(), true)  // 负载不应该超过CPU核心数
  
  // 验证上下文切换和中断
  let context_switches = cpu_metrics.get("cpu_context_switches", 0)
  let interrupts = cpu_metrics.get("cpu_interrupts", 0)
  
  assert_eq(context_switches > 0, true)
  assert_eq(interrupts > 0, true)
  assert_eq(context_switches > interrupts, true)  // 通常上下文切换比中断多
  
  // 计算CPU性能评分
  let performance_score = 100.0 - cpu_usage  // 简化的性能评分
  assert_eq(performance_score >= 0.0, true)
  assert_eq(performance_score <= 100.0, true)
}

test "telemetry_memory_performance_monitoring" {
  // 测试遥测内存性能监控
  
  let memory_metrics = {
    "total_memory_gb": 16.0,
    "used_memory_gb": 12.5,
    "free_memory_gb": 3.5,
    "cached_memory_gb": 2.1,
    "buffer_memory_gb": 0.8,
    "swap_total_gb": 8.0,
    "swap_used_gb": 1.2,
    "swap_free_gb": 6.8,
    "memory_usage_percent": 78.1,
    "swap_usage_percent": 15.0,
    "page_faults": 15000,
    "major_page_faults": 250
  }
  
  // 验证内存指标完整性
  assert_eq(memory_metrics.contains("total_memory_gb"), true)
  assert_eq(memory_metrics.contains("used_memory_gb"), true)
  assert_eq(memory_metrics.contains("free_memory_gb"), true)
  assert_eq(memory_metrics.contains("memory_usage_percent"), true)
  
  // 验证内存总量一致性
  let total_memory = memory_metrics.get("total_memory_gb", 0.0)
  let used_memory = memory_metrics.get("used_memory_gb", 0.0)
  let free_memory = memory_metrics.get("free_memory_gb", 0.0)
  
  assert_eq(total_memory > 0.0, true)
  assert_eq(used_memory > 0.0, true)
  assert_eq(free_memory > 0.0, true)
  assert_eq(total_memory, used_memory + free_memory)  // 总内存 = 已用 + 空闲
  
  // 验证内存使用率
  let memory_usage_percent = memory_metrics.get("memory_usage_percent", 0.0)
  let calculated_usage = (used_memory / total_memory) * 100.0
  
  assert_eq(memory_usage_percent >= 0.0, true)
  assert_eq(memory_usage_percent <= 100.0, true)
  assert_eq(abs(memory_usage_percent - calculated_usage) < 1.0, true)  // 允许1%的误差
  
  // 验证Swap内存
  let swap_total = memory_metrics.get("swap_total_gb", 0.0)
  let swap_used = memory_metrics.get("swap_used_gb", 0.0)
  let swap_free = memory_metrics.get("swap_free_gb", 0.0)
  
  assert_eq(swap_total > 0.0, true)
  assert_eq(swap_total, swap_used + swap_free)  // Swap总量 = 已用 + 空闲
  
  // 验证页面错误
  let page_faults = memory_metrics.get("page_faults", 0)
  let major_page_faults = memory_metrics.get("major_page_faults", 0)
  
  assert_eq(page_faults > 0, true)
  assert_eq(major_page_faults > 0, true)
  assert_eq(page_faults >= major_page_faults, true)  // 总页面错误 >= 主要页面错误
  
  // 评估内存压力
  let memory_pressure = if memory_usage_percent > 90.0 {
    "high"
  } else if memory_usage_percent > 70.0 {
    "medium"
  } else {
    "low"
  }
  
  assert_eq(memory_pressure == "high" || memory_pressure == "medium" || memory_pressure == "low", true)
}

test "telemetry_disk_performance_monitoring" {
  // 测试遥测磁盘性能监控
  
  let disk_metrics = {
    "disk_total_gb": 500.0,
    "disk_used_gb": 350.0,
    "disk_free_gb": 150.0,
    "disk_usage_percent": 70.0,
    "disk_read_bytes_per_sec": 52428800,  // 50MB/s
    "disk_write_bytes_per_sec": 31457280, // 30MB/s
    "disk_read_ops_per_sec": 1250,
    "disk_write_ops_per_sec": 800,
    "disk_io_wait_percent": 5.2,
    "disk_queue_depth": 8,
    "avg_response_time_ms": 12.5
  }
  
  // 验证磁盘指标完整性
  assert_eq(disk_metrics.contains("disk_total_gb"), true)
  assert_eq(disk_metrics.contains("disk_used_gb"), true)
  assert_eq(disk_metrics.contains("disk_usage_percent"), true)
  assert_eq(disk_metrics.contains("disk_read_bytes_per_sec"), true)
  
  // 验证磁盘空间
  let disk_total = disk_metrics.get("disk_total_gb", 0.0)
  let disk_used = disk_metrics.get("disk_used_gb", 0.0)
  let disk_free = disk_metrics.get("disk_free_gb", 0.0)
  
  assert_eq(disk_total > 0.0, true)
  assert_eq(disk_used > 0.0, true)
  assert_eq(disk_free > 0.0, true)
  assert_eq(disk_total, disk_used + disk_free)  // 总容量 = 已用 + 空闲
  
  // 验证磁盘使用率
  let disk_usage_percent = disk_metrics.get("disk_usage_percent", 0.0)
  let calculated_usage = (disk_used / disk_total) * 100.0
  
  assert_eq(disk_usage_percent >= 0.0, true)
  assert_eq(disk_usage_percent <= 100.0, true)
  assert_eq(abs(disk_usage_percent - calculated_usage) < 1.0, true)  // 允许1%误差
  
  // 验证磁盘I/O
  let read_bytes_per_sec = disk_metrics.get("disk_read_bytes_per_sec", 0)
  let write_bytes_per_sec = disk_metrics.get("disk_write_bytes_per_sec", 0)
  let read_ops_per_sec = disk_metrics.get("disk_read_ops_per_sec", 0)
  let write_ops_per_sec = disk_metrics.get("disk_write_ops_per_sec", 0)
  
  assert_eq(read_bytes_per_sec > 0, true)
  assert_eq(write_bytes_per_sec > 0, true)
  assert_eq(read_ops_per_sec > 0, true)
  assert_eq(write_ops_per_sec > 0, true)
  
  // 计算平均I/O大小
  let avg_read_size = read_bytes_per_sec.to_double() / read_ops_per_sec.to_double()
  let avg_write_size = write_bytes_per_sec.to_double() / write_ops_per_sec.to_double()
  
  assert_eq(avg_read_size > 0.0, true)
  assert_eq(avg_write_size > 0.0, true)
  
  // 验证I/O等待时间
  let io_wait_percent = disk_metrics.get("disk_io_wait_percent", 0.0)
  assert_eq(io_wait_percent >= 0.0, true)
  assert_eq(io_wait_percent <= 100.0, true)
  
  // 验证队列深度
  let queue_depth = disk_metrics.get("disk_queue_depth", 0)
  assert_eq(queue_depth >= 0, true)
  
  // 验证响应时间
  let avg_response_time = disk_metrics.get("avg_response_time_ms", 0.0)
  assert_eq(avg_response_time > 0.0, true)
  
  // 评估磁盘性能
  let disk_performance = if avg_response_time > 20.0 || io_wait_percent > 10.0 {
    "poor"
  } else if avg_response_time > 10.0 || io_wait_percent > 5.0 {
    "moderate"
  } else {
    "good"
  }
  
  assert_eq(disk_performance == "poor" || disk_performance == "moderate" || disk_performance == "good", true)
}

test "telemetry_network_performance_monitoring" {
  // 测试遥测网络性能监控
  
  let network_metrics = {
    "network_bytes_sent_per_sec": 104857600,  // 100MB/s
    "network_bytes_received_per_sec": 52428800, // 50MB/s
    "network_packets_sent_per_sec": 80000,
    "network_packets_received_per_sec": 45000,
    "network_packets_dropped_per_sec": 50,
    "network_errors_per_sec": 25,
    "network_interface": "eth0",
    "network_link_speed_mbps": 1000,
    "network_duplex": "full",
    "network_mtu": 1500,
    "network_collisions": 0,
    "network_rx_errors": 10,
    "network_tx_errors": 5
  }
  
  // 验证网络指标完整性
  assert_eq(network_metrics.contains("network_bytes_sent_per_sec"), true)
  assert_eq(network_metrics.contains("network_bytes_received_per_sec"), true)
  assert_eq(network_metrics.contains("network_packets_sent_per_sec"), true)
  assert_eq(network_metrics.contains("network_interface"), true)
  
  // 验证网络吞吐量
  let bytes_sent_per_sec = network_metrics.get("network_bytes_sent_per_sec", 0)
  let bytes_received_per_sec = network_metrics.get("network_bytes_received_per_sec", 0)
  
  assert_eq(bytes_sent_per_sec > 0, true)
  assert_eq(bytes_received_per_sec > 0, true)
  
  // 验证包速率
  let packets_sent_per_sec = network_metrics.get("network_packets_sent_per_sec", 0)
  let packets_received_per_sec = network_metrics.get("network_packets_received_per_sec", 0)
  
  assert_eq(packets_sent_per_sec > 0, true)
  assert_eq(packets_received_per_sec > 0, true)
  
  // 计算平均包大小
  let avg_tx_packet_size = bytes_sent_per_sec.to_double() / packets_sent_per_sec.to_double()
  let avg_rx_packet_size = bytes_received_per_sec.to_double() / packets_received_per_sec.to_double()
  
  assert_eq(avg_tx_packet_size > 0.0, true)
  assert_eq(avg_rx_packet_size > 0.0, true)
  
  // 验证网络错误
  let packets_dropped = network_metrics.get("network_packets_dropped_per_sec", 0)
  let network_errors = network_metrics.get("network_errors_per_sec", 0)
  
  assert_eq(packets_dropped >= 0, true)
  assert_eq(network_errors >= 0, true)
  
  // 计算错误率
  let total_packets = packets_sent_per_sec + packets_received_per_sec
  let packet_loss_rate = (packets_dropped.to_double() / total_packets.to_double()) * 100.0
  let error_rate = (network_errors.to_double() / total_packets.to_double()) * 100.0
  
  assert_eq(packet_loss_rate >= 0.0, true)
  assert_eq(error_rate >= 0.0, true)
  
  // 验证网络接口配置
  let link_speed = network_metrics.get("network_link_speed_mbps", 0)
  let duplex = network_metrics.get("network_duplex", "")
  let mtu = network_metrics.get("network_mtu", 0)
  
  assert_eq(link_speed > 0, true)
  assert_eq(duplex == "full" || duplex == "half", true)
  assert_eq(mtu > 0, true)
  
  // 计算网络利用率
  let max_throughput_bps = link_speed * 1000000 / 8  // Mbps to bytes/sec
  let current_throughput = bytes_sent_per_sec + bytes_received_per_sec
  let network_utilization = (current_throughput.to_double() / max_throughput_bps.to_double()) * 100.0
  
  assert_eq(network_utilization >= 0.0, true)
  assert_eq(network_utilization <= 100.0, true)
  
  // 评估网络质量
  let network_quality = if packet_loss_rate > 1.0 || error_rate > 0.5 {
    "poor"
  } else if packet_loss_rate > 0.1 || error_rate > 0.1 {
    "moderate"
  } else {
    "good"
  }
  
  assert_eq(network_quality == "poor" || network_quality == "moderate" || network_quality == "good", true)
}

test "telemetry_application_performance_monitoring" {
  // 测试遥测应用性能监控
  
  let application_metrics = {
    "app_response_time_p50_ms": 125.5,
    "app_response_time_p95_ms": 450.2,
    "app_response_time_p99_ms": 850.7,
    "app_throughput_rps": 1250.5,
    "app_error_rate_percent": 2.3,
    "app_active_connections": 850,
    "app_memory_usage_mb": 512.8,
    "app_cpu_usage_percent": 45.6,
    "app_gc_pause_time_ms": 25.3,
    "app_thread_count": 45,
    "app_database_connections": 20,
    "app_cache_hit_rate_percent": 92.5
  }
  
  // 验证应用指标完整性
  assert_eq(application_metrics.contains("app_response_time_p50_ms"), true)
  assert_eq(application_metrics.contains("app_response_time_p95_ms"), true)
  assert_eq(application_metrics.contains("app_throughput_rps"), true)
  assert_eq(application_metrics.contains("app_error_rate_percent"), true)
  
  // 验证响应时间百分位数
  let p50_response = application_metrics.get("app_response_time_p50_ms", 0.0)
  let p95_response = application_metrics.get("app_response_time_p95_ms", 0.0)
  let p99_response = application_metrics.get("app_response_time_p99_ms", 0.0)
  
  assert_eq(p50_response > 0.0, true)
  assert_eq(p95_response > 0.0, true)
  assert_eq(p99_response > 0.0, true)
  assert_eq(p95_response >= p50_response, true)  // P95应该 >= P50
  assert_eq(p99_response >= p95_response, true)  // P99应该 >= P95
  
  // 验证吞吐量
  let throughput = application_metrics.get("app_throughput_rps", 0.0)
  assert_eq(throughput > 0.0, true)
  
  // 验证错误率
  let error_rate = application_metrics.get("app_error_rate_percent", 0.0)
  assert_eq(error_rate >= 0.0, true)
  assert_eq(error_rate <= 100.0, true)
  
  // 验证连接数
  let active_connections = application_metrics.get("app_active_connections", 0)
  assert_eq(active_connections > 0, true)
  
  // 验证资源使用
  let memory_usage = application_metrics.get("app_memory_usage_mb", 0.0)
  let cpu_usage = application_metrics.get("app_cpu_usage_percent", 0.0)
  
  assert_eq(memory_usage > 0.0, true)
  assert_eq(cpu_usage >= 0.0, true)
  assert_eq(cpu_usage <= 100.0, true)
  
  // 验证GC暂停时间
  let gc_pause_time = application_metrics.get("app_gc_pause_time_ms", 0.0)
  assert_eq(gc_pause_time >= 0.0, true)
  
  // 验证线程和数据库连接
  let thread_count = application_metrics.get("app_thread_count", 0)
  let db_connections = application_metrics.get("app_database_connections", 0)
  
  assert_eq(thread_count > 0, true)
  assert_eq(db_connections > 0, true)
  assert_eq(db_connections <= thread_count, true)  // 数据库连接不应该超过线程数
  
  // 验证缓存命中率
  let cache_hit_rate = application_metrics.get("app_cache_hit_rate_percent", 0.0)
  assert_eq(cache_hit_rate >= 0.0, true)
  assert_eq(cache_hit_rate <= 100.0, true)
  
  // 计算应用性能评分
  let response_time_score = if p95_response < 200.0 {
    100.0
  } else if p95_response < 500.0 {
    75.0
  } else if p95_response < 1000.0 {
    50.0
  } else {
    25.0
  }
  
  let error_rate_score = if error_rate < 1.0 {
    100.0
  } else if error_rate < 5.0 {
    75.0
  } else if error_rate < 10.0 {
    50.0
  } else {
    25.0
  }
  
  let throughput_score = if throughput > 1000.0 {
    100.0
  } else if throughput > 500.0 {
    75.0
  } else if throughput > 100.0 {
    50.0
  } else {
    25.0
  }
  
  let overall_score = (response_time_score + error_rate_score + throughput_score) / 3.0
  
  assert_eq(overall_score >= 0.0, true)
  assert_eq(overall_score <= 100.0, true)
}

test "telemetry_performance_benchmarking" {
  // 测试遥测性能基准测试
  
  let benchmark_results = {
    "test_name": "telemetry_processing_throughput",
    "test_duration_seconds": 60,
    "total_events_processed": 1250000,
    "events_per_second": 20833.3,
    "avg_processing_time_us": 48.0,
    "p95_processing_time_us": 125.5,
    "p99_processing_time_us": 250.8,
    "memory_peak_mb": 1024.5,
    "cpu_peak_percent": 85.2,
    "disk_io_peak_mb_s": 150.8,
    "network_io_peak_mb_s": 75.3,
    "error_count": 12,
    "timeout_count": 3
  }
  
  // 验证基准测试结果完整性
  assert_eq(benchmark_results.contains("test_name"), true)
  assert_eq(benchmark_results.contains("test_duration_seconds"), true)
  assert_eq(benchmark_results.contains("total_events_processed"), true)
  assert_eq(benchmark_results.contains("events_per_second"), true)
  
  // 验证测试配置
  let test_name = benchmark_results.get("test_name", "")
  let test_duration = benchmark_results.get("test_duration_seconds", 0)
  
  assert_eq(test_name.length() > 0, true)
  assert_eq(test_duration > 0, true)
  
  // 验证吞吐量指标
  let total_events = benchmark_results.get("total_events_processed", 0)
  let events_per_second = benchmark_results.get("events_per_second", 0.0)
  
  assert_eq(total_events > 0, true)
  assert_eq(events_per_second > 0.0, true)
  
  // 验证计算的事件率
  let calculated_eps = total_events.to_double() / test_duration.to_double()
  assert_eq(abs(events_per_second - calculated_eps) < 1.0, true)  // 允许1的误差
  
  // 验证处理时间
  let avg_processing_time = benchmark_results.get("avg_processing_time_us", 0.0)
  let p95_processing_time = benchmark_results.get("p95_processing_time_us", 0.0)
  let p99_processing_time = benchmark_results.get("p99_processing_time_us", 0.0)
  
  assert_eq(avg_processing_time > 0.0, true)
  assert_eq(p95_processing_time > 0.0, true)
  assert_eq(p99_processing_time > 0.0, true)
  assert_eq(p95_processing_time >= avg_processing_time, true)
  assert_eq(p99_processing_time >= p95_processing_time, true)
  
  // 验证资源使用峰值
  let memory_peak = benchmark_results.get("memory_peak_mb", 0.0)
  let cpu_peak = benchmark_results.get("cpu_peak_percent", 0.0)
  let disk_io_peak = benchmark_results.get("disk_io_peak_mb_s", 0.0)
  let network_io_peak = benchmark_results.get("network_io_peak_mb_s", 0.0)
  
  assert_eq(memory_peak > 0.0, true)
  assert_eq(cpu_peak >= 0.0, true)
  assert_eq(cpu_peak <= 100.0, true)
  assert_eq(disk_io_peak >= 0.0, true)
  assert_eq(network_io_peak >= 0.0, true)
  
  // 验证错误统计
  let error_count = benchmark_results.get("error_count", 0)
  let timeout_count = benchmark_results.get("timeout_count", 0)
  
  assert_eq(error_count >= 0, true)
  assert_eq(timeout_count >= 0, true)
  
  // 计算错误率
  let error_rate = (error_count.to_double() / total_events.to_double()) * 100.0
  let timeout_rate = (timeout_count.to_double() / total_events.to_double()) * 100.0
  
  assert_eq(error_rate >= 0.0, true)
  assert_eq(timeout_rate >= 0.0, true)
  
  // 比较与基准性能
  let baseline_eps = 15000.0  // 基准事件率
  let baseline_avg_time = 60.0  // 基准平均处理时间
  
  let eps_improvement = ((events_per_second - baseline_eps) / baseline_eps) * 100.0
  let time_improvement = ((baseline_avg_time - avg_processing_time) / baseline_avg_time) * 100.0
  
  assert_eq(eps_improvement > 0.0, true)  // 事件率应该有所提升
  assert_eq(time_improvement > 0.0, true)  // 处理时间应该有所减少
  
  // 评估基准测试结果
  let benchmark_grade = if eps_improvement > 30.0 && time_improvement > 20.0 && error_rate < 0.1 {
    "excellent"
  } else if eps_improvement > 15.0 && time_improvement > 10.0 && error_rate < 0.5 {
    "good"
  } else if eps_improvement > 0.0 && time_improvement > 0.0 && error_rate < 1.0 {
    "acceptable"
  } else {
    "needs_improvement"
  }
  
  assert_eq(benchmark_grade == "excellent" || benchmark_grade == "good" || benchmark_grade == "acceptable" || benchmark_grade == "needs_improvement", true)
}

test "telemetry_performance_alerting" {
  // 测试遥测性能告警
  
  let alert_config = {
    "cpu_threshold_percent": 80.0,
    "memory_threshold_percent": 85.0,
    "disk_threshold_percent": 90.0,
    "response_time_p95_threshold_ms": 500.0,
    "error_rate_threshold_percent": 5.0,
    "throughput_min_threshold_rps": 100.0,
    "alert_cooldown_seconds": 300,
    "alert_severity_levels": ["critical", "warning", "info"]
  }
  
  // 验证告警配置
  assert_eq(alert_config.contains("cpu_threshold_percent"), true)
  assert_eq(alert_config.contains("memory_threshold_percent"), true)
  assert_eq(alert_config.contains("response_time_p95_threshold_ms"), true)
  assert_eq(alert_config.contains("alert_severity_levels"), true)
  
  // 验证阈值配置
  let cpu_threshold = alert_config.get("cpu_threshold_percent", 0.0)
  let memory_threshold = alert_config.get("memory_threshold_percent", 0.0)
  let disk_threshold = alert_config.get("disk_threshold_percent", 0.0)
  
  assert_eq(cpu_threshold > 0.0, true)
  assert_eq(cpu_threshold <= 100.0, true)
  assert_eq(memory_threshold > 0.0, true)
  assert_eq(memory_threshold <= 100.0, true)
  assert_eq(disk_threshold > 0.0, true)
  assert_eq(disk_threshold <= 100.0, true)
  
  // 验证性能阈值
  let response_time_threshold = alert_config.get("response_time_p95_threshold_ms", 0.0)
  let error_rate_threshold = alert_config.get("error_rate_threshold_percent", 0.0)
  let throughput_threshold = alert_config.get("throughput_min_threshold_rps", 0.0)
  
  assert_eq(response_time_threshold > 0.0, true)
  assert_eq(error_rate_threshold >= 0.0, true)
  assert_eq(throughput_threshold > 0.0, true)
  
  // 验证告警冷却时间
  let cooldown_seconds = alert_config.get("alert_cooldown_seconds", 0)
  assert_eq(cooldown_seconds > 0, true)
  
  // 验证告警严重级别
  let severity_levels = alert_config.get("alert_severity_levels", [])
  assert_eq(severity_levels.length(), 3)
  assert_eq(severity_levels.contains("critical"), true)
  assert_eq(severity_levels.contains("warning"), true)
  assert_eq(severity_levels.contains("info"), true)
  
  // 模拟当前性能指标
  let current_metrics = {
    "cpu_usage_percent": 85.5,
    "memory_usage_percent": 78.2,
    "disk_usage_percent": 92.1,
    "response_time_p95_ms": 650.3,
    "error_rate_percent": 7.8,
    "throughput_rps": 85.5
  }
  
  // 检查告警条件
  let active_alerts = []
  
  // CPU告警
  if current_metrics.get("cpu_usage_percent", 0.0) > cpu_threshold {
    active_alerts.push({
      "metric": "cpu_usage_percent",
      "current_value": current_metrics.get("cpu_usage_percent", 0.0),
      "threshold": cpu_threshold,
      "severity": if current_metrics.get("cpu_usage_percent", 0.0) > 90.0 { "critical" } else { "warning" }
    })
  }
  
  // 内存告警
  if current_metrics.get("memory_usage_percent", 0.0) > memory_threshold {
    active_alerts.push({
      "metric": "memory_usage_percent",
      "current_value": current_metrics.get("memory_usage_percent", 0.0),
      "threshold": memory_threshold,
      "severity": if current_metrics.get("memory_usage_percent", 0.0) > 95.0 { "critical" } else { "warning" }
    })
  }
  
  // 磁盘告警
  if current_metrics.get("disk_usage_percent", 0.0) > disk_threshold {
    active_alerts.push({
      "metric": "disk_usage_percent",
      "current_value": current_metrics.get("disk_usage_percent", 0.0),
      "threshold": disk_threshold,
      "severity": if current_metrics.get("disk_usage_percent", 0.0) > 95.0 { "critical" } else { "warning" }
    })
  }
  
  // 响应时间告警
  if current_metrics.get("response_time_p95_ms", 0.0) > response_time_threshold {
    active_alerts.push({
      "metric": "response_time_p95_ms",
      "current_value": current_metrics.get("response_time_p95_ms", 0.0),
      "threshold": response_time_threshold,
      "severity": if current_metrics.get("response_time_p95_ms", 0.0) > 1000.0 { "critical" } else { "warning" }
    })
  }
  
  // 错误率告警
  if current_metrics.get("error_rate_percent", 0.0) > error_rate_threshold {
    active_alerts.push({
      "metric": "error_rate_percent",
      "current_value": current_metrics.get("error_rate_percent", 0.0),
      "threshold": error_rate_threshold,
      "severity": if current_metrics.get("error_rate_percent", 0.0) > 10.0 { "critical" } else { "warning" }
    })
  }
  
  // 吞吐量告警
  if current_metrics.get("throughput_rps", 0.0) < throughput_threshold {
    active_alerts.push({
      "metric": "throughput_rps",
      "current_value": current_metrics.get("throughput_rps", 0.0),
      "threshold": throughput_threshold,
      "severity": if current_metrics.get("throughput_rps", 0.0) < 50.0 { "critical" } else { "warning" }
    })
  }
  
  // 验证告警结果
  assert_eq(active_alerts.length(), 5)  // CPU、磁盘、响应时间、错误率、吞吐量都超过阈值
  assert_eq(active_alerts[0].get("metric", ""), "cpu_usage_percent")
  assert_eq(active_alerts[1].get("metric", ""), "disk_usage_percent")
  assert_eq(active_alerts[2].get("metric", ""), "response_time_p95_ms")
  assert_eq(active_alerts[3].get("metric", ""), "error_rate_percent")
  assert_eq(active_alerts[4].get("metric", ""), "throughput_rps")
  
  // 验证告警严重级别
  let mut critical_count = 0
  let mut warning_count = 0
  let mut i = 0
  while i < active_alerts.length() {
    let severity = active_alerts[i].get("severity", "")
    if severity == "critical" {
      critical_count = critical_count + 1
    } else if severity == "warning" {
      warning_count = warning_count + 1
    }
    i = i + 1
  }
  
  assert_eq(critical_count + warning_count, active_alerts.length())
}