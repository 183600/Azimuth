// 遥测资源监控测试用例

test "telemetry_system_resource_monitoring" {
  // 测试遥测系统资源监控
  
  // 创建资源监控器
  let resource_monitor = azimuth::telemetry::api::monitoring::ResourceMonitor::new()
    .with_monitoring_interval(5000)  // 5秒监控间隔
    .with_metrics_collection(["cpu", "memory", "disk", "network"])
    .with_alert_thresholds({
      "cpu_usage": {"warning": 70.0, "critical": 90.0},
      "memory_usage": {"warning": 80.0, "critical": 95.0},
      "disk_usage": {"warning": 85.0, "critical": 95.0},
      "network_latency": {"warning": 100.0, "critical": 500.0}
    })
  
  // 模拟系统资源数据
  let system_resources = azimuth::telemetry::api::monitoring::SystemResources::new()
    .with_cpu_usage(65.5)  // 正常范围
    .with_memory_usage(2048.0, 8192.0)  // 2GB已用，总共8GB (25%)
    .with_disk_usage(500.0, 1000.0)     // 500GB已用，总共1TB (50%)
    .with_network_io(1000000.0, 500000.0)  // 1MB/s发送，500KB/s接收
    .with_load_average([1.2, 1.5, 1.8])    // 1分钟、5分钟、15分钟负载
    .with_process_count(156)
    .with_uptime(86400)  // 1天
  
  // 收集资源指标
  let resource_metrics = resource_monitor.collect_metrics(system_resources)
  assert_eq(resource_metrics.timestamp > 0, true)
  assert_eq(resource_metrics.cpu_usage, 65.5)
  assert_eq(resource_metrics.memory_usage_percentage, 25.0)
  assert_eq(resource_metrics.disk_usage_percentage, 50.0)
  assert_eq(resource_metrics.network_throughput, 1500000.0)
  assert_eq(resource_metrics.process_count, 156)
  
  // 检查告警状态
  let alert_status = resource_monitor.check_alerts(resource_metrics)
  assert_eq(alert_status.cpu_alert, "normal")      // 65.5% < 70%
  assert_eq(alert_status.memory_alert, "normal")   // 25% < 80%
  assert_eq(alert_status.disk_alert, "normal")     // 50% < 85%
  assert_eq(alert_status.overall_status, "healthy")
  
  // 模拟高负载情况
  let high_load_resources = azimuth::telemetry::api::monitoring::SystemResources::new()
    .with_cpu_usage(92.0)  // 超过临界阈值
    .with_memory_usage(7800.0, 8192.0)  // 95.2% 超过临界阈值
    .with_disk_usage(920.0, 1000.0)     // 92% 超过警告阈值
    .with_network_io(500000.0, 2000000.0)
  
  let high_load_metrics = resource_monitor.collect_metrics(high_load_resources)
  let high_load_alerts = resource_monitor.check_alerts(high_load_metrics)
  
  assert_eq(high_load_alerts.cpu_alert, "critical")    // 92% > 90%
  assert_eq(high_load_alerts.memory_alert, "critical") // 95.2% > 95%
  assert_eq(high_load_alerts.disk_alert, "warning")    // 92% > 85%
  assert_eq(high_load_alerts.overall_status, "critical")
}

test "telemetry_service_resource_monitoring" {
  // 测试遥测服务资源监控
  
  // 创建服务资源监控器
  let service_monitor = azimuth::telemetry::api::monitoring::ServiceResourceMonitor::new()
    .with_monitored_services([
      "telemetry-collector",
      "metrics-aggregator",
      "trace-processor",
      "log-analyzer"
    ])
    .with_service_metrics([
      "request_rate",
      "response_time",
      "error_rate",
      "throughput",
      "queue_size",
      "connection_count"
    ])
  
  // 模拟服务资源数据
  let service_resources = [
    azimuth::telemetry::api::monitoring::ServiceResources::new("telemetry-collector")
      .with_cpu_usage(45.0)
      .with_memory_usage(512.0, 2048.0)  // 25%
      .with_request_rate(1000.0)  // 1000 req/s
      .with_response_time(25.5)   // 25.5ms
      .with_error_rate(0.01)      // 1%
      .with_queue_size(50)
      .with_connection_count(25),
    
    azimuth::telemetry::api::monitoring::ServiceResources::new("metrics-aggregator")
      .with_cpu_usage(78.0)  // 较高CPU使用率
      .with_memory_usage(1536.0, 2048.0)  // 75%
      .with_request_rate(2500.0)  // 2500 req/s
      .with_response_time(85.0)   // 85ms 较慢
      .with_error_rate(0.02)      // 2%
      .with_queue_size(200)       // 队列积压
      .with_connection_count(100),
    
    azimuth::telemetry::api::monitoring::ServiceResources::new("trace-processor")
      .with_cpu_usage(35.0)
      .with_memory_usage(256.0, 2048.0)  // 12.5%
      .with_request_rate(500.0)   // 500 req/s
      .with_response_time(15.0)   // 15ms 很快
      .with_error_rate(0.005)     // 0.5%
      .with_queue_size(10)
      .with_connection_count(15),
    
    azimuth::telemetry::api::monitoring::ServiceResources::new("log-analyzer")
      .with_cpu_usage(55.0)
      .with_memory_usage(1024.0, 2048.0)  // 50%
      .with_request_rate(800.0)   // 800 req/s
      .with_response_time(45.0)   // 45ms
      .with_error_rate(0.015)     // 1.5%
      .with_queue_size(75)
      .with_connection_count(40)
  ]
  
  // 收集服务指标
  let service_metrics = service_monitor.collect_service_metrics(service_resources)
  assert_eq(service_metrics.length(), 4)
  
  // 验证收集的指标
  let collector_metrics = service_metrics.find(|m| m.service_name == "telemetry-collector").unwrap()
  assert_eq(collector_metrics.cpu_usage, 45.0)
  assert_eq(collector_metrics.request_rate, 1000.0)
  assert_eq(collector_metrics.error_rate, 0.01)
  
  let aggregator_metrics = service_metrics.find(|m| m.service_name == "metrics-aggregator").unwrap()
  assert_eq(aggregator_metrics.cpu_usage, 78.0)
  assert_eq(aggregator_metrics.queue_size, 200)
  
  // 分析服务性能
  let performance_analysis = service_monitor.analyze_performance(service_metrics)
  assert_eq(performance_analysis.healthy_services.length(), 2)  // collector和trace-processor
  assert_eq(performance_analysis.degraded_services.length(), 2)  // aggregator和log-analyzer
  assert_eq(performance_analysis.overall_system_health, "degraded")
  
  // 识别瓶颈服务
  let bottleneck_analysis = service_monitor.identify_bottlenecks(service_metrics)
  assert_eq(bottleneck_analysis.primary_bottleneck, "metrics-aggregator")
  assert_eq(bottleneck_analysis.bottleneck_reasons.contains("high_cpu_usage"), true)
  assert_eq(bottleneck_analysis.bottleneck_reasons.contains("large_queue_size"), true)
  assert_eq(bottleneck_analysis.bottleneck_reasons.contains("slow_response_time"), true)
}

test "telemetry_storage_resource_monitoring" {
  // 测试遥测存储资源监控
  
  // 创建存储资源监控器
  let storage_monitor = azimuth::telemetry::api::monitoring::StorageResourceMonitor::new()
    .with_storage_backends([
      "elasticsearch-cluster",
      "influxdb-timeseries",
      "redis-cache",
      "s3-cold-storage"
    ])
    .with_storage_metrics([
      "disk_usage",
      "index_size",
      "document_count",
      "query_performance",
      "write_throughput",
      "compression_ratio"
    ])
  
  // 模拟存储资源数据
  let storage_resources = [
    azimuth::telemetry::api::monitoring::StorageResources::new("elasticsearch-cluster")
      .with_total_capacity(1000000.0)  // 1TB
      .with_used_space(750000.0)       // 750GB
      .with_index_size(600000.0)       // 600GB索引
      .with_document_count(50000000)   // 5000万文档
      .with_query_latency(150.0)       // 150ms平均查询延迟
      .with_write_throughput(5000.0)   // 5000 writes/s
      .with_compression_ratio(0.4),    // 40%压缩率
    
    azimuth::telemetry::api::monitoring::StorageResources::new("influxdb-timeseries")
      .with_total_capacity(500000.0)   // 500GB
      .with_used_space(200000.0)       // 200GB
      .with_index_size(150000.0)       // 150GB索引
      .with_document_count(100000000)  // 1亿时间序列点
      .with_query_latency(25.0)        // 25ms平均查询延迟
      .with_write_throughput(10000.0)  // 10000 writes/s
      .with_compression_ratio(0.25),   // 25%压缩率
    
    azimuth::telemetry::api::monitoring::StorageResources::new("redis-cache")
      .with_total_capacity(100000.0)   // 100GB
      .with_used_space(45000.0)        // 45GB
      .with_memory_usage(45000.0)      // 45GB内存使用
      .with_key_count(1000000)         // 100万键
      .with_query_latency(2.5)         // 2.5ms平均查询延迟
      .with_write_throughput(50000.0)  // 50000 ops/s
      .with_hit_rate(0.95),            // 95%命中率
    
    azimuth::telemetry::api::monitoring::StorageResources::new("s3-cold-storage")
      .with_total_capacity(10000000.0) // 10TB
      .with_used_space(2000000.0)      // 2TB
      .with_object_count(5000000)      // 500万对象
      .with_write_throughput(1000.0)   // 1000 objects/s
      .with_read_throughput(2000.0)    // 2000 objects/s
      .with_storage_class("standard")  // 标准存储类别
  ]
  
  // 收集存储指标
  let storage_metrics = storage_monitor.collect_storage_metrics(storage_resources)
  assert_eq(storage_metrics.length(), 4)
  
  // 分析存储使用情况
  let storage_analysis = storage_monitor.analyze_storage_usage(storage_metrics)
  
  // 验证存储使用分析
  let elasticsearch_usage = storage_analysis.backend_usage.find(|u| u.backend == "elasticsearch-cluster").unwrap()
  assert_eq(elasticsearch_usage.usage_percentage, 75.0)  // 750GB/1TB
  assert_eq(elasticsearch_usage.status, "warning")      // 超过70%阈值
  
  let influxdb_usage = storage_analysis.backend_usage.find(|u| u.backend == "influxdb-timeseries").unwrap()
  assert_eq(influxdb_usage.usage_percentage, 40.0)  // 200GB/500GB
  assert_eq(influxdb_usage.status, "healthy")
  
  let redis_usage = storage_analysis.backend_usage.find(|u| u.backend == "redis-cache").unwrap()
  assert_eq(redis_usage.usage_percentage, 45.0)  // 45GB/100GB
  assert_eq(redis_usage.status, "healthy")
  
  let s3_usage = storage_analysis.backend_usage.find(|u| u.backend == "s3-cold-storage").unwrap()
  assert_eq(s3_usage.usage_percentage, 20.0)  // 2TB/10TB
  assert_eq(s3_usage.status, "healthy")
  
  // 存储性能分析
  let performance_analysis = storage_monitor.analyze_storage_performance(storage_metrics)
  assert_eq(performance_analysis.fastest_backend, "redis-cache")      // 2.5ms延迟
  assert_eq(performance_analysis.slowest_backend, "elasticsearch-cluster")  // 150ms延迟
  assert_eq(performance_analysis.highest_throughput, "redis-cache")    // 50000 ops/s
  
  // 存储优化建议
  let optimization_recommendations = storage_monitor.generate_optimization_recommendations(storage_metrics)
  assert_eq(optimization_recommendations.length() > 0, true)
  assert_eq(optimization_recommendations.any(|r| r.contains("elasticsearch")), true)
  assert_eq(optimization_recommendations.any(|r| r.contains("data_retention")), true)
}

test "telemetry_network_resource_monitoring" {
  // 测试遥测网络资源监控
  
  // 创建网络资源监控器
  let network_monitor = azimuth::telemetry::api::monitoring::NetworkResourceMonitor::new()
    .with_monitored_interfaces(["eth0", "eth1", "lo"])
    .with_network_metrics([
      "bandwidth_usage",
      "packet_loss",
      "latency",
      "throughput",
      "connection_count",
      "error_rate"
    ])
    .with_monitoring_targets([
      "telemetry-collector-01:4317",
      "elasticsearch-cluster:9200",
      "influxdb-cluster:8086"
    ])
  
  // 模拟网络资源数据
  let network_resources = [
    azimuth::telemetry::api::monitoring::NetworkResources::new("eth0")
      .with_rx_bytes(1000000000)    // 1GB接收
      .with_tx_bytes(500000000)     // 500MB发送
      .with_rx_packets(1000000)     // 100万包接收
      .with_tx_packets(500000)      // 50万包发送
      .with_bandwidth_capacity(1000000000.0)  // 1Gbps
      .with_packet_loss_rate(0.001) // 0.1%丢包率
      .with_latency(5.5)            // 5.5ms延迟
      .with_error_rate(0.0001),     // 0.01%错误率
    
    azimuth::telemetry::api::monitoring::NetworkResources::new("eth1")
      .with_rx_bytes(2000000000)    // 2GB接收
      .with_tx_bytes(1500000000)    // 1.5GB发送
      .with_rx_packets(2000000)     // 200万包接收
      .with_tx_packets(1500000)     // 150万包发送
      .with_bandwidth_capacity(10000000000.0) // 10Gbps
      .with_packet_loss_rate(0.0005) // 0.05%丢包率
      .with_latency(2.0)            // 2ms延迟
      .with_error_rate(0.00005),    // 0.005%错误率
    
    azimuth::telemetry::api::monitoring::NetworkResources::new("lo")
      .with_rx_bytes(100000000)     // 100MB接收
      .with_tx_bytes(100000000)     // 100MB发送
      .with_rx_packets(200000)      // 20万包接收
      .with_tx_packets(200000)      // 20万包发送
      .with_bandwidth_capacity(1000000000.0)  // 1Gbps
      .with_packet_loss_rate(0.0)   // 0%丢包率
      .with_latency(0.1)            // 0.1ms延迟
      .with_error_rate(0.0)         // 0%错误率
  ]
  
  // 收集网络指标
  let network_metrics = network_monitor.collect_network_metrics(network_resources)
  assert_eq(network_metrics.length(), 3)
  
  // 分析网络使用情况
  let network_analysis = network_monitor.analyze_network_usage(network_metrics)
  
  // 验证网络使用分析
  let eth0_analysis = network_analysis.interface_usage.find(|u| u.interface == "eth0").unwrap()
  assert_eq(eth0_analysis.bandwidth_utilization > 0.0, true)
  assert_eq(eth0_analysis.bandwidth_utilization < 100.0, true)
  assert_eq(eth0_analysis.latency_ms, 5.5)
  assert_eq(eth0_analysis.packet_loss_percentage, 0.1)
  
  let eth1_analysis = network_analysis.interface_usage.find(|u| u.interface == "eth1").unwrap()
  assert_eq(eth1_analysis.latency_ms, 2.0)  // 更好的延迟
  assert_eq(eth1_analysis.packet_loss_percentage, 0.05)
  
  // 网络连通性测试
  let connectivity_tests = network_monitor.test_connectivity([
    "telemetry-collector-01:4317",
    "elasticsearch-cluster:9200",
    "influxdb-cluster:8086"
  ])
  
  assert_eq(connectivity_tests.length(), 3)
  assert_eq(connectivity_tests.all(|t| t.is_reachable), true)
  assert_eq(connectivity_tests.all(|t| t.latency_ms < 100), true)  // 所有连接延迟小于100ms
  
  // 网络性能基准测试
  let performance_benchmark = network_monitor.benchmark_network_performance([
    "telemetry-collector-01:4317",
    "elasticsearch-cluster:9200"
  ])
  
  assert_eq(performance_benchmark.length(), 2)
  assert_eq(performance_benchmark.all(|b| b.throughput_mbps > 0), true)
  assert_eq(performance_benchmark.all(|b| b.jitter_ms < 10), true)  // 抖动小于10ms
}

test "telemetry_resource_capacity_planning" {
  // 测试遥测资源容量规划
  
  // 创建容量规划器
  let capacity_planner = azimuth::telemetry::api::monitoring::CapacityPlanner::new()
    .with_planning_horizon(90)  // 90天规划周期
    .with_growth_factors({
      "data_volume": 1.2,        // 每月20%增长
      "request_rate": 1.15,      // 每月15%增长
      "user_count": 1.1          // 每月10%增长
    })
    .with_utilization_targets({
      "cpu": 70.0,               // 目标CPU利用率70%
      "memory": 75.0,            // 目标内存利用率75%
      "storage": 80.0,           // 目标存储利用率80%
      "network": 60.0            // 目标网络利用率60%
    })
  
  // 当前资源使用情况
  let current_resources = azimuth::telemetry::api::monitoring::ResourceSnapshot::new()
    .with_cpu_cores(16)
    .with_cpu_usage(65.0)         // 当前65%使用率
    .with_memory_gb(128)
    .with_memory_usage(96.0)      // 96GB使用 (75%)
    .with_storage_gb(2000)
    .with_storage_usage(1200.0)   // 1.2TB使用 (60%)
    .with_network_bandwidth_mbps(10000)
    .with_network_usage(4000.0)   // 4Gbps使用 (40%)
    .with_daily_data_volume(500.0) // 每天500GB
    .with_daily_request_count(1000000) // 每天100万请求
  
  // 生成容量预测
  let capacity_forecast = capacity_planner.generate_forecast(current_resources)
  
  // 验证容量预测
  assert_eq(capacity_forecast.forecast_period_days, 90)
  assert_eq(capacity_forecast.predictions.length() > 0, true)
  
  // 检查30天后的预测
  let day_30_prediction = capacity_forecast.predictions.find(|p| p.day == 30).unwrap()
  assert_eq(day_30_prediction.cpu_usage > 65.0, true)  // CPU使用率应该增长
  assert_eq(day_30_prediction.memory_usage > 96.0, true)  // 内存使用率应该增长
  assert_eq(day_30_prediction.storage_usage > 1200.0, true)  // 存储使用率应该增长
  
  // 检查90天后的预测
  let day_90_prediction = capacity_forecast.predictions.find(|p| p.day == 90).unwrap()
  assert_eq(day_90_prediction.cpu_usage > day_30_prediction.cpu_usage, true)
  assert_eq(day_90_prediction.memory_usage > day_30_prediction.memory_usage, true)
  assert_eq(day_90_prediction.storage_usage > day_30_prediction.storage_usage, true)
  
  // 识别资源瓶颈
  let bottleneck_analysis = capacity_planner.identify_future_bottlenecks(capacity_forecast)
  assert_eq(bottleneck_analysis.bottlenecks.length() > 0, true)
  
  // 验证瓶颈分析
  let cpu_bottleneck = bottleneck_analysis.bottlenecks.find(|b| b.resource_type == "cpu")
  let memory_bottleneck = bottleneck_analysis.bottlenecks.find(|b| b.resource_type == "memory")
  let storage_bottleneck = bottleneck_analysis.bottlenecks.find(|b| b.resource_type == "storage")
  
  // 至少应该有一个瓶颈
  assert_eq(cpu_bottleneck.is_some() || memory_bottleneck.is_some() || storage_bottleneck.is_some(), true)
  
  // 生成扩容建议
  let scaling_recommendations = capacity_planner.generate_scaling_recommendations(capacity_forecast, bottleneck_analysis)
  assert_eq(scaling_recommendations.length() > 0, true)
  
  // 验证扩容建议
  for recommendation in scaling_recommendations {
    assert_eq(recommendation.resource_type.length() > 0, true)
    assert_eq(recommendation.current_capacity > 0, true)
    assert_eq(recommendation.recommended_capacity >= recommendation.current_capacity, true)
    assert_eq(recommendation.justification.length() > 0, true)
    assert_eq(recommendation.timeline_days > 0, true)
  }
  
  // 成本效益分析
  let cost_benefit_analysis = capacity_planner.analyze_cost_benefit(scaling_recommendations)
  assert_eq(cost_benefit_analysis.total_investment > 0, true)
  assert_eq(cost_benefit_analysis.expected_benefit > 0, true)
  assert_eq(cost_benefit_analysis.roi_percentage > 0, true)
  assert_eq(cost_benefit_analysis.payback_period_days > 0, true)
}