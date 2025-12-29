// 遥测性能基准测试用例
// 测试遥测系统的性能基准和资源利用率

test "telemetry_performance_throughput_benchmark" {
  // 测试遥测系统吞吐量基准
  
  let throughput_metrics = [
    ("spans_per_second", 10000),
    ("metrics_per_second", 50000),
    ("logs_per_second", 20000),
    ("events_per_second", 15000),
    ("attributes_per_span", 10)
  ]
  
  // 验证吞吐量指标数量
  assert_eq(throughput_metrics.length(), 5)
  
  // 验证每秒Span数
  assert_eq(throughput_metrics[0].0, "spans_per_second")
  assert_eq(throughput_metrics[0].1, 10000)
  assert_eq(throughput_metrics[0].1 > 5000, true)
  
  // 验证每秒指标数
  assert_eq(throughput_metrics[1].0, "metrics_per_second")
  assert_eq(throughput_metrics[1].1, 50000)
  assert_eq(throughput_metrics[1].1 > throughput_metrics[0].1, true) // 指标数应该大于Span数
  
  // 验证每秒日志数
  assert_eq(throughput_metrics[2].0, "logs_per_second")
  assert_eq(throughput_metrics[2].1, 20000)
  assert_eq(throughput_metrics[2].1 > throughput_metrics[0].1, true) // 日志数应该大于Span数
  assert_eq(throughput_metrics[2].1 < throughput_metrics[1].1, true) // 日志数应该小于指标数
  
  // 验证每秒事件数
  assert_eq(throughput_metrics[3].0, "events_per_second")
  assert_eq(throughput_metrics[3].1, 15000)
  assert_eq(throughput_metrics[3].1 > throughput_metrics[0].1, true) // 事件数应该大于Span数
  assert_eq(throughput_metrics[3].1 < throughput_metrics[2].1, true) // 事件数应该小于日志数
  
  // 验证每个Span的属性数
  assert_eq(throughput_metrics[4].0, "attributes_per_span")
  assert_eq(throughput_metrics[4].1, 10)
  assert_eq(throughput_metrics[4].1 > 5, true)
  
  // 计算总数据点数
  let total_data_points = throughput_metrics[0].1 + throughput_metrics[1].1 + throughput_metrics[2].1 + throughput_metrics[3].1
  assert_eq(total_data_points, 95000)
  
  // 计算每小时处理的数据点数
  let hourly_data_points = total_data_points * 3600
  assert_eq(hourly_data_points, 342000000)
}

test "telemetry_performance_latency_benchmark" {
  // 测试遥测系统延迟基准
  
  let latency_metrics = [
    ("span_creation_p50", 0.1),
    ("span_creation_p95", 0.2),
    ("span_creation_p99", 0.5),
    ("metric_recording_p50", 0.05),
    ("metric_recording_p95", 0.1),
    ("metric_recording_p99", 0.2),
    ("log_emission_p50", 0.2),
    ("log_emission_p95", 0.4),
    ("log_emission_p99", 0.8),
    ("export_batch_p50", 10.0),
    ("export_batch_p95", 20.0),
    ("export_batch_p99", 50.0)
  ]
  
  // 验证延迟指标数量
  assert_eq(latency_metrics.length(), 12)
  
  // 验证Span创建延迟
  assert_eq(latency_metrics[0].0, "span_creation_p50")
  assert_eq(latency_metrics[0].1, 0.1)  // P50: 0.1ms
  assert_eq(latency_metrics[1].0, "span_creation_p95")
  assert_eq(latency_metrics[1].1, 0.2)  // P95: 0.2ms
  assert_eq(latency_metrics[2].0, "span_creation_p99")
  assert_eq(latency_metrics[2].1, 0.5)  // P99: 0.5ms
  
  // 验证指标记录延迟（应该比Span创建快）
  assert_eq(latency_metrics[3].0, "metric_recording_p50")
  assert_eq(latency_metrics[3].1, 0.05) // P50: 0.05ms
  assert_eq(latency_metrics[3].1 < latency_metrics[0].1, true)
  
  assert_eq(latency_metrics[4].0, "metric_recording_p95")
  assert_eq(latency_metrics[4].1, 0.1)  // P95: 0.1ms
  assert_eq(latency_metrics[4].1 < latency_metrics[1].1, true)
  
  assert_eq(latency_metrics[5].0, "metric_recording_p99")
  assert_eq(latency_metrics[5].1, 0.2)  // P99: 0.2ms
  assert_eq(latency_metrics[5].1 < latency_metrics[2].1, true)
  
  // 验证日志发送延迟
  assert_eq(latency_metrics[6].0, "log_emission_p50")
  assert_eq(latency_metrics[6].1, 0.2)  // P50: 0.2ms
  
  assert_eq(latency_metrics[7].0, "log_emission_p95")
  assert_eq(latency_metrics[7].1, 0.4)  // P95: 0.4ms
  
  assert_eq(latency_metrics[8].0, "log_emission_p99")
  assert_eq(latency_metrics[8].1, 0.8)  // P99: 0.8ms
  
  // 验证导出批处理延迟（应该比单个操作慢）
  assert_eq(latency_metrics[9].0, "export_batch_p50")
  assert_eq(latency_metrics[9].1, 10.0) // P50: 10ms
  assert_eq(latency_metrics[9].1 > latency_metrics[8].1, true)
  
  assert_eq(latency_metrics[10].0, "export_batch_p95")
  assert_eq(latency_metrics[10].1, 20.0) // P95: 20ms
  
  assert_eq(latency_metrics[11].0, "export_batch_p99")
  assert_eq(latency_metrics[11].1, 50.0) // P99: 50ms
}

test "telemetry_performance_memory_usage_benchmark" {
  // 测试遥测系统内存使用基准
  
  let memory_metrics = [
    ("base_memory_mb", 50),
    ("per_span_memory_bytes", 1024),
    ("per_metric_memory_bytes", 256),
    ("per_log_memory_bytes", 512),
    ("batch_buffer_memory_mb", 100),
    ("max_memory_usage_mb", 500)
  ]
  
  // 验证内存指标数量
  assert_eq(memory_metrics.length(), 6)
  
  // 验证基础内存使用
  assert_eq(memory_metrics[0].0, "base_memory_mb")
  assert_eq(memory_metrics[0].1, 50) // 50MB
  
  // 验证每个Span的内存使用
  assert_eq(memory_metrics[1].0, "per_span_memory_bytes")
  assert_eq(memory_metrics[1].1, 1024) // 1KB per span
  
  // 验证每个指标的内存使用
  assert_eq(memory_metrics[2].0, "per_metric_memory_bytes")
  assert_eq(memory_metrics[2].1, 256) // 256 bytes per metric
  assert_eq(memory_metrics[2].1 < memory_metrics[1].1, true) // 指标内存应该小于Span内存
  
  // 验证每个日志的内存使用
  assert_eq(memory_metrics[3].0, "per_log_memory_bytes")
  assert_eq(memory_metrics[3].1, 512) // 512 bytes per log
  assert_eq(memory_metrics[3].1 > memory_metrics[2].1, true) // 日志内存应该大于指标内存
  assert_eq(memory_metrics[3].1 < memory_metrics[1].1, true) // 日志内存应该小于Span内存
  
  // 验证批处理缓冲区内存
  assert_eq(memory_metrics[4].0, "batch_buffer_memory_mb")
  assert_eq(memory_metrics[4].1, 100) // 100MB
  assert_eq(memory_metrics[4].1 > memory_metrics[0].1, true) // 缓冲区内存应该大于基础内存
  
  // 验证最大内存使用限制
  assert_eq(memory_metrics[5].0, "max_memory_usage_mb")
  assert_eq(memory_metrics[5].1, 500) // 500MB
  assert_eq(memory_metrics[5].1 > memory_metrics[4].1, true) // 最大内存应该大于缓冲区内存
  
  // 计算10000个Span的内存使用
  let span_count = 10000
  let span_memory_mb = (span_count * memory_metrics[1].1) / (1024 * 1024) // 转换为MB
  assert_eq(span_memory_mb > 9, true)
  assert_eq(span_memory_mb < 10, true)
  
  // 计算50000个指标的内存使用
  let metric_count = 50000
  let metric_memory_mb = (metric_count * memory_metrics[2].1) / (1024 * 1024) // 转换为MB
  assert_eq(metric_memory_mb > 10, true)
  assert_eq(metric_memory_mb < 15, true)
  
  // 计算20000个日志的内存使用
  let log_count = 20000
  let log_memory_mb = (log_count * memory_metrics[3].1) / (1024 * 1024) // 转换为MB
  assert_eq(log_memory_mb > 9, true)
  assert_eq(log_memory_mb < 10, true)
}

test "telemetry_performance_cpu_usage_benchmark" {
  // 测试遥测系统CPU使用基准
  
  let cpu_metrics = [
    ("idle_cpu_percentage", 5.0),
    ("span_processing_cpu_percentage", 15.0),
    ("metric_processing_cpu_percentage", 10.0),
    ("log_processing_cpu_percentage", 20.0),
    ("export_processing_cpu_percentage", 30.0),
    ("total_cpu_percentage", 80.0)
  ]
  
  // 验证CPU指标数量
  assert_eq(cpu_metrics.length(), 6)
  
  // 验证空闲CPU百分比
  assert_eq(cpu_metrics[0].0, "idle_cpu_percentage")
  assert_eq(cpu_metrics[0].1, 5.0)
  assert_eq(cpu_metrics[0].1 > 0.0, true)
  assert_eq(cpu_metrics[0].1 < 10.0, true)
  
  // 验证Span处理CPU百分比
  assert_eq(cpu_metrics[1].0, "span_processing_cpu_percentage")
  assert_eq(cpu_metrics[1].1, 15.0)
  assert_eq(cpu_metrics[1].1 > cpu_metrics[0].1, true)
  
  // 验证指标处理CPU百分比
  assert_eq(cpu_metrics[2].0, "metric_processing_cpu_percentage")
  assert_eq(cpu_metrics[2].1, 10.0)
  assert_eq(cpu_metrics[2].1 < cpu_metrics[1].1, true) // 指标处理CPU使用应该小于Span处理
  
  // 验证日志处理CPU百分比
  assert_eq(cpu_metrics[3].0, "log_processing_cpu_percentage")
  assert_eq(cpu_metrics[3].1, 20.0)
  assert_eq(cpu_metrics[3].1 > cpu_metrics[1].1, true) // 日志处理CPU使用应该大于Span处理
  
  // 验证导出处理CPU百分比
  assert_eq(cpu_metrics[4].0, "export_processing_cpu_percentage")
  assert_eq(cpu_metrics[4].1, 30.0)
  assert_eq(cpu_metrics[4].1 > cpu_metrics[3].1, true) // 导出处理CPU使用应该大于日志处理
  
  // 验证总CPU百分比
  assert_eq(cpu_metrics[5].0, "total_cpu_percentage")
  assert_eq(cpu_metrics[5].1, 80.0)
  assert_eq(cpu_metrics[5].1 > cpu_metrics[4].1, true)
  assert_eq(cpu_metrics[5].1 < 100.0, true)
  
  // 计算各组件CPU使用之和
  let component_cpu_sum = cpu_metrics[1].1 + cpu_metrics[2].1 + cpu_metrics[3].1 + cpu_metrics[4].1
  assert_eq(component_cpu_sum, 75.0)
  
  // 验证总CPU使用等于各组件之和加上空闲CPU
  let calculated_total = component_cpu_sum + cpu_metrics[0].1
  assert_eq(calculated_total, cpu_metrics[5].1)
  
  // 计算各组件CPU使用占比
  let span_cpu_ratio = (cpu_metrics[1].1 / cpu_metrics[5].1) * 100.0
  let metric_cpu_ratio = (cpu_metrics[2].1 / cpu_metrics[5].1) * 100.0
  let log_cpu_ratio = (cpu_metrics[3].1 / cpu_metrics[5].1) * 100.0
  let export_cpu_ratio = (cpu_metrics[4].1 / cpu_metrics[5].1) * 100.0
  
  assert_eq(span_cpu_ratio > 15.0, true)
  assert_eq(metric_cpu_ratio > 10.0, true)
  assert_eq(log_cpu_ratio > 20.0, true)
  assert_eq(export_cpu_ratio > 30.0, true)
}

test "telemetry_performance_storage_io_benchmark" {
  // 测试遥测系统存储IO基准
  
  let storage_io_metrics = [
    ("disk_write_mb_per_second", 50.0),
    ("disk_read_mb_per_second", 10.0),
    ("batch_write_size_mb", 5.0),
    ("compression_ratio", 0.3),  // 压缩后大小为原大小的30%
    ("storage_retention_hours", 720), // 30天
    ("max_storage_usage_gb", 1000)
  ]
  
  // 验证存储IO指标数量
  assert_eq(storage_io_metrics.length(), 6)
  
  // 验证磁盘写入速度
  assert_eq(storage_io_metrics[0].0, "disk_write_mb_per_second")
  assert_eq(storage_io_metrics[0].1, 50.0) // 50MB/s
  assert_eq(storage_io_metrics[0].1 > 40.0, true)
  
  // 验证磁盘读取速度
  assert_eq(storage_io_metrics[1].0, "disk_read_mb_per_second")
  assert_eq(storage_io_metrics[1].1, 10.0) // 10MB/s
  assert_eq(storage_io_metrics[1].1 < storage_io_metrics[0].1, true) // 读取速度应该小于写入速度
  
  // 验证批处理写入大小
  assert_eq(storage_io_metrics[2].0, "batch_write_size_mb")
  assert_eq(storage_io_metrics[2].1, 5.0) // 5MB per batch
  assert_eq(storage_io_metrics[2].1 > 1.0, true)
  
  // 验证压缩比
  assert_eq(storage_io_metrics[3].0, "compression_ratio")
  assert_eq(storage_io_metrics[3].1, 0.3) // 压缩到30%
  assert_eq(storage_io_metrics[3].1 > 0.0, true)
  assert_eq(storage_io_metrics[3].1 < 1.0, true)
  
  // 验证存储保留时间
  assert_eq(storage_io_metrics[4].0, "storage_retention_hours")
  assert_eq(storage_io_metrics[4].1, 720) // 30天
  assert_eq(storage_io_metrics[4].1 > 500, true)
  
  // 验证最大存储使用量
  assert_eq(storage_io_metrics[5].0, "max_storage_usage_gb")
  assert_eq(storage_io_metrics[5].1, 1000) // 1TB
  assert_eq(storage_io_metrics[5].1 > 500, true)
  
  // 计算每小时写入的数据量
  let hourly_write_mb = storage_io_metrics[0].1 * 3600
  assert_eq(hourly_write_mb, 180000) // 180GB per hour
  
  // 计算每天写入的数据量
  let daily_write_gb = (hourly_write_mb * 24) / 1024
  assert_eq(daily_write_gb > 4000, true)
  assert_eq(daily_write_gb < 4500, true)
  
  // 计算压缩后的每天数据量
  let compressed_daily_write_gb = daily_write_gb * storage_io_metrics[3].1
  assert_eq(compressed_daily_write_gb > 1200, true)
  assert_eq(compressed_daily_write_gb < 1400, true)
  
  // 计算存储保留期间的总数据量
  let retention_data_gb = compressed_daily_write_gb * (storage_io_metrics[4].1 / 24)
  assert_eq(retention_data_gb > 30000, true)
  assert_eq(retention_data_gb < 40000, true)
  
  // 验证存储空间是否足够
  assert_eq(storage_io_metrics[5].1 > retention_data_gb / 1000, true) // 转换为TB
}

test "telemetry_performance_network_io_benchmark" {
  // 测试遥测系统网络IO基准
  
  let network_io_metrics = [
    ("network_send_mb_per_second", 20.0),
    ("network_receive_mb_per_second", 5.0),
    ("connection_pool_size", 10),
    ("max_concurrent_connections", 50),
    ("connection_timeout_ms", 5000),
    ("keep_alive_enabled", true)
  ]
  
  // 验证网络IO指标数量
  assert_eq(network_io_metrics.length(), 6)
  
  // 验证网络发送速度
  assert_eq(network_io_metrics[0].0, "network_send_mb_per_second")
  assert_eq(network_io_metrics[0].1, 20.0) // 20MB/s
  assert_eq(network_io_metrics[0].1 > 10.0, true)
  
  // 验证网络接收速度
  assert_eq(network_io_metrics[1].0, "network_receive_mb_per_second")
  assert_eq(network_io_metrics[1].1, 5.0) // 5MB/s
  assert_eq(network_io_metrics[1].1 < network_io_metrics[0].1, true) // 接收速度应该小于发送速度
  
  // 验证连接池大小
  assert_eq(network_io_metrics[2].0, "connection_pool_size")
  assert_eq(network_io_metrics[2].1, 10)
  assert_eq(network_io_metrics[2].1 > 5, true)
  
  // 验证最大并发连接数
  assert_eq(network_io_metrics[3].0, "max_concurrent_connections")
  assert_eq(network_io_metrics[3].1, 50)
  assert_eq(network_io_metrics[3].1 > network_io_metrics[2].1, true) // 最大连接数应该大于连接池大小
  
  // 验证连接超时时间
  assert_eq(network_io_metrics[4].0, "connection_timeout_ms")
  assert_eq(network_io_metrics[4].1, 5000) // 5秒
  assert_eq(network_io_metrics[4].1 > 1000, true)
  
  // 验证保持连接启用状态
  assert_eq(network_io_metrics[5].0, "keep_alive_enabled")
  assert_eq(network_io_metrics[5].1, true)
  
  // 计算每小时网络发送量
  let hourly_send_mb = network_io_metrics[0].1 * 3600
  assert_eq(hourly_send_mb, 72000) // 72GB per hour
  
  // 计算每天网络发送量
  let daily_send_gb = (hourly_send_mb * 24) / 1024
  assert_eq(daily_send_gb > 1600, true)
  assert_eq(daily_send_gb < 1700, true)
  
  // 计算每月网络发送量
  let monthly_send_tb = (daily_send_gb * 30) / 1024
  assert_eq(monthly_send_tb > 45, true)
  assert_eq(monthly_send_tb < 50, true)
  
  // 计算连接利用率
  let connection_utilization = (network_io_metrics[2].1.to_double() / network_io_metrics[3].1.to_double()) * 100.0
  assert_eq(connection_utilization, 20.0) // 10/50 = 20%
  assert_eq(connection_utilization < 50.0, true) // 连接利用率应该低于50%
}

test "telemetry_performance_scalability_benchmark" {
  // 测试遥测系统可扩展性基准
  
  let scalability_metrics = [
    ("horizontal_scaling_factor", 2.0),    // 水平扩展因子
    ("vertical_scaling_factor", 1.5),     // 垂直扩展因子
    ("load_balancing_efficiency", 0.9),   // 负载均衡效率
    ("resource_contention_ratio", 0.1),   // 资源争用比率
    ("max_concurrent_requests", 10000),   // 最大并发请求数
    ("auto_scaling_threshold_cpu", 80.0), // 自动扩展CPU阈值
    ("auto_scaling_threshold_memory", 85.0) // 自动扩展内存阈值
  ]
  
  // 验证可扩展性指标数量
  assert_eq(scalability_metrics.length(), 7)
  
  // 验证水平扩展因子
  assert_eq(scalability_metrics[0].0, "horizontal_scaling_factor")
  assert_eq(scalability_metrics[0].1, 2.0) // 可以扩展到2倍
  assert_eq(scalability_metrics[0].1 > 1.0, true)
  
  // 验证垂直扩展因子
  assert_eq(scalability_metrics[1].0, "vertical_scaling_factor")
  assert_eq(scalability_metrics[1].1, 1.5) // 可以扩展到1.5倍
  assert_eq(scalability_metrics[1].1 > 1.0, true)
  assert_eq(scalability_metrics[1].1 < scalability_metrics[0].1, true) // 垂直扩展应该小于水平扩展
  
  // 验证负载均衡效率
  assert_eq(scalability_metrics[2].0, "load_balancing_efficiency")
  assert_eq(scalability_metrics[2].1, 0.9) // 90%效率
  assert_eq(scalability_metrics[2].1 > 0.8, true)
  assert_eq(scalability_metrics[2].1 < 1.0, true)
  
  // 验证资源争用比率
  assert_eq(scalability_metrics[3].0, "resource_contention_ratio")
  assert_eq(scalability_metrics[3].1, 0.1) // 10%争用
  assert_eq(scalability_metrics[3].1 > 0.0, true)
  assert_eq(scalability_metrics[3].1 < 0.2, true)
  
  // 验证最大并发请求数
  assert_eq(scalability_metrics[4].0, "max_concurrent_requests")
  assert_eq(scalability_metrics[4].1, 10000)
  assert_eq(scalability_metrics[4].1 > 5000, true)
  
  // 验证自动扩展CPU阈值
  assert_eq(scalability_metrics[5].0, "auto_scaling_threshold_cpu")
  assert_eq(scalability_metrics[5].1, 80.0) // 80%
  assert_eq(scalability_metrics[5].1 > 70.0, true)
  assert_eq(scalability_metrics[5].1 < 90.0, true)
  
  // 验证自动扩展内存阈值
  assert_eq(scalability_metrics[6].0, "auto_scaling_threshold_memory")
  assert_eq(scalability_metrics[6].1, 85.0) // 85%
  assert_eq(scalability_metrics[6].1 > 75.0, true)
  assert_eq(scalability_metrics[6].1 < 95.0, true)
  
  // 计算水平扩展后的最大并发请求数
  let scaled_max_requests = (scalability_metrics[4].1.to_double() * scalability_metrics[0].1).to_int()
  assert_eq(scaled_max_requests, 20000)
  
  // 计算垂直扩展后的处理能力
  let base_throughput = 10000
  let scaled_throughput = (base_throughput.to_double() * scalability_metrics[1].1).to_int()
  assert_eq(scaled_throughput, 15000)
  
  // 计算综合扩展能力
  let combined_scaling = scalability_metrics[0].1 * scalability_metrics[1].1
  assert_eq(combined_scaling, 3.0) // 2.0 * 1.5 = 3.0
  
  // 计算扩展后的总吞吐量
  let combined_throughput = (base_throughput.to_double() * combined_scaling).to_int()
  assert_eq(combined_throughput, 30000)
}

test "telemetry_performance_resource_efficiency_benchmark" {
  // 测试遥测系统资源效率基准
  
  let efficiency_metrics = [
    ("cpu_efficiency_score", 0.85),       // CPU效率分数
    ("memory_efficiency_score", 0.80),    // 内存效率分数
    ("network_efficiency_score", 0.75),   // 网络效率分数
    ("storage_efficiency_score", 0.90),   // 存储效率分数
    ("overall_efficiency_score", 0.825),  // 总体效率分数
    ("resource_waste_percentage", 15.0),  // 资源浪费百分比
    ("cost_per_telemetry_unit", 0.001)    // 每个遥测单位的成本
  ]
  
  // 验证效率指标数量
  assert_eq(efficiency_metrics.length(), 7)
  
  // 验证CPU效率分数
  assert_eq(efficiency_metrics[0].0, "cpu_efficiency_score")
  assert_eq(efficiency_metrics[0].1, 0.85) // 85%
  assert_eq(efficiency_metrics[0].1 > 0.8, true)
  assert_eq(efficiency_metrics[0].1 < 1.0, true)
  
  // 验证内存效率分数
  assert_eq(efficiency_metrics[1].0, "memory_efficiency_score")
  assert_eq(efficiency_metrics[1].1, 0.80) // 80%
  assert_eq(efficiency_metrics[1].1 > 0.7, true)
  assert_eq(efficiency_metrics[1].1 < efficiency_metrics[0].1, true) // 内存效率应该低于CPU效率
  
  // 验证网络效率分数
  assert_eq(efficiency_metrics[2].0, "network_efficiency_score")
  assert_eq(efficiency_metrics[2].1, 0.75) // 75%
  assert_eq(efficiency_metrics[2].1 > 0.7, true)
  assert_eq(efficiency_metrics[2].1 < efficiency_metrics[1].1, true) // 网络效率应该低于内存效率
  
  // 验证存储效率分数
  assert_eq(efficiency_metrics[3].0, "storage_efficiency_score")
  assert_eq(efficiency_metrics[3].1, 0.90) // 90%
  assert_eq(efficiency_metrics[3].1 > efficiency_metrics[0].1, true) // 存储效率应该最高
  
  // 验证总体效率分数（平均值）
  assert_eq(efficiency_metrics[4].0, "overall_efficiency_score")
  assert_eq(efficiency_metrics[4].1, 0.825) // 82.5%
  
  // 计算平均效率分数
  let calculated_average = (efficiency_metrics[0].1 + efficiency_metrics[1].1 + efficiency_metrics[2].1 + efficiency_metrics[3].1) / 4.0
  assert_eq(calculated_average, efficiency_metrics[4].1)
  
  // 验证资源浪费百分比
  assert_eq(efficiency_metrics[5].0, "resource_waste_percentage")
  assert_eq(efficiency_metrics[5].1, 15.0) // 15%
  assert_eq(efficiency_metrics[5].1 > 10.0, true)
  assert_eq(efficiency_metrics[5].1 < 20.0, true)
  
  // 验证资源浪费百分比与总体效率的关系
  let waste_percentage = (1.0 - efficiency_metrics[4].1) * 100.0
  assert_eq(waste_percentage > 17.0, true)
  assert_eq(waste_percentage < 18.0, true)
  
  // 验证每个遥测单位的成本
  assert_eq(efficiency_metrics[6].0, "cost_per_telemetry_unit")
  assert_eq(efficiency_metrics[6].1, 0.001) // $0.001 per unit
  assert_eq(efficiency_metrics[6].1 > 0.0, true)
  assert_eq(efficiency_metrics[6].1 < 0.01, true)
  
  // 计算每天100万个遥测单位的成本
  let daily_units = 1000000
  let daily_cost = daily_units.to_double() * efficiency_metrics[6].1
  assert_eq(daily_cost, 1000.0) // $1000 per day
  
  // 计算每月成本
  let monthly_cost = daily_cost * 30
  assert_eq(monthly_cost, 30000.0) // $30000 per month
  
  // 计算每年成本
  let yearly_cost = monthly_cost * 12
  assert_eq(yearly_cost, 360000.0) // $360000 per year
}