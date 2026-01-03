// Azimuth 性能监控测试
// 专注于测试系统的性能监控、指标收集和分析功能

// 测试1: 系统资源监控
test "系统资源监控测试" {
  // 1. 创建系统资源监控器
  let resource_monitor = SystemResourceMonitor({
    monitoring_interval: 1000, // 1秒
    metrics_to_collect: [
      "cpu.usage",
      "memory.usage",
      "disk.usage",
      "network.io"
    ]
  })
  
  // 2. 验证监控器配置
  assert_eq(resource_monitor.monitoring_interval, 1000)
  assert_eq(resource_monitor.metrics_to_collect.length(), 4)
  assert_true(resource_monitor.metrics_to_collect.contains("cpu.usage"))
  assert_true(resource_monitor.metrics_to_collect.contains("memory.usage"))
  assert_true(resource_monitor.metrics_to_collect.contains("disk.usage"))
  assert_true(resource_monitor.metrics_to_collect.contains("network.io"))
  
  // 3. 模拟收集系统资源指标
  let cpu_metric = Metric({
    name: "cpu.usage",
    value: FloatValue(75.5),
    unit: "percent",
    timestamp: 1640995600000,
    tags: [("core", "0"), ("host", "server-1")]
  })
  
  let memory_metric = Metric({
    name: "memory.usage",
    value: FloatValue(60.2),
    unit: "percent",
    timestamp: 1640995600000,
    tags: [("type", "ram"), ("host", "server-1")]
  })
  
  let disk_metric = Metric({
    name: "disk.usage",
    value: FloatValue(45.8),
    unit: "percent",
    timestamp: 1640995600000,
    tags: [("device", "/dev/sda1"), ("host", "server-1")]
  })
  
  let network_metric = Metric({
    name: "network.io",
    value: FloatValue(1024.5),
    unit: "mbps",
    timestamp: 1640995600000,
    tags: [("interface", "eth0"), ("host", "server-1")]
  })
  
  // 4. 验证指标数据
  match cpu_metric.value {
    FloatValue(v) => assert_eq(v, 75.5)
    _ => assert_true(false)
  }
  
  match memory_metric.value {
    FloatValue(v) => assert_eq(v, 60.2)
    _ => assert_true(false)
  }
  
  match disk_metric.value {
    FloatValue(v) => assert_eq(v, 45.8)
    _ => assert_true(false)
  }
  
  match network_metric.value {
    FloatValue(v) => assert_eq(v, 1024.5)
    _ => assert_true(false)
  }
  
  // 5. 创建指标集合
  let metrics = [cpu_metric, memory_metric, disk_metric, network_metric]
  
  // 6. 验证指标集合
  assert_eq(metrics.length(), 4)
  
  // 7. 计算资源使用率统计
  let resource_usage_stats = calculate_resource_usage_stats(metrics)
  
  assert_eq(resource_usage_stats.cpu_usage, 75.5)
  assert_eq(resource_usage_stats.memory_usage, 60.2)
  assert_eq(resource_usage_stats.disk_usage, 45.8)
  assert_eq(resource_usage_stats.network_throughput, 1024.5)
}

// 测试2: 应用程序性能监控
test "应用程序性能监控测试" {
  // 1. 创建应用程序性能监控器
  let app_monitor = ApplicationPerformanceMonitor({
    application_name: "azimuth-api",
    monitoring_endpoints: [
      "/api/users",
      "/api/orders",
      "/api/products"
    ],
    performance_thresholds: [
      ("response.time.p95", 500.0),
      ("error.rate", 0.05),
      ("throughput", 1000.0)
    ]
  })
  
  // 2. 验证应用监控器配置
  assert_eq(app_monitor.application_name, "azimuth-api")
  assert_eq(app_monitor.monitoring_endpoints.length(), 3)
  assert_eq(app_monitor.performance_thresholds.length(), 3)
  
  // 3. 模拟收集端点性能数据
  let endpoint_metrics = [
    EndpointMetric({
      endpoint: "/api/users",
      request_count: 1500,
      error_count: 30,
      total_response_time: 75000.0, // 毫秒
      p95_response_time: 80.0,
      p99_response_time: 150.0,
      timestamp: 1640995700000
    }),
    EndpointMetric({
      endpoint: "/api/orders",
      request_count: 800,
      error_count: 40,
      total_response_time: 64000.0,
      p95_response_time: 120.0,
      p99_response_time: 200.0,
      timestamp: 1640995700000
    }),
    EndpointMetric({
      endpoint: "/api/products",
      request_count: 2000,
      error_count: 20,
      total_response_time: 100000.0,
      p95_response_time: 75.0,
      p99_response_time: 120.0,
      timestamp: 1640995700000
    })
  ]
  
  // 4. 验证端点指标数据
  assert_eq(endpoint_metrics.length(), 3)
  
  // 5. 计算应用程序整体性能指标
  let total_requests = endpoint_metrics.reduce(fn(acc, metric) { acc + metric.request_count }, 0)
  let total_errors = endpoint_metrics.reduce(fn(acc, metric) { acc + metric.error_count }, 0)
  let total_response_time = endpoint_metrics.reduce(fn(acc, metric) { acc + metric.total_response_time }, 0.0)
  
  assert_eq(total_requests, 4300)
  assert_eq(total_errors, 90)
  assert_eq(total_response_time, 239000.0)
  
  // 6. 计算整体错误率和平均响应时间
  let overall_error_rate = total_errors.to_float() / total_requests.to_float()
  let avg_response_time = total_response_time / total_requests.to_float()
  
  assert_eq(overall_error_rate, 0.020930232558139537) // 90 / 4300
  assert_eq(avg_response_time, 55.58139534883721) // 239000 / 4300
  
  // 7. 检查性能阈值违规
  let threshold_violations = check_performance_thresholds(endpoint_metrics, app_monitor.performance_thresholds)
  
  // 验证没有P95响应时间超过阈值
  let p95_violations = threshold_violations.filter(fn(violation) { violation.metric == "response.time.p95" })
  assert_eq(p95_violations.length(), 0)
  
  // 验证错误率是否超过阈值
  let error_rate_violations = threshold_violations.filter(fn(violation) { violation.metric == "error.rate" })
  assert_eq(error_rate_violations.length(), 0) // 整体错误率2.09%低于5%阈值
}

// 测试3: 数据库性能监控
test "数据库性能监控测试" {
  // 1. 创建数据库性能监控器
  let db_monitor = DatabasePerformanceMonitor({
    database_type: "postgresql",
    connection_string: "postgresql://localhost:5432/azimuth",
    monitoring_queries: [
      "SELECT COUNT(*) FROM users",
      "SELECT * FROM orders WHERE created_at > NOW() - INTERVAL '1 hour'",
      "UPDATE products SET stock = stock - 1 WHERE id = $1"
    ],
    performance_metrics: [
      "query.execution.time",
      "connection.pool.usage",
      "database.size",
      "index.usage"
    ]
  })
  
  // 2. 验证数据库监控器配置
  assert_eq(db_monitor.database_type, "postgresql")
  assert_eq(db_monitor.monitoring_queries.length(), 3)
  assert_eq(db_monitor.performance_metrics.length(), 4)
  
  // 3. 模拟收集数据库性能指标
  let db_metrics = [
    DatabaseMetric({
      query_type: "SELECT",
      execution_time: 25.5, // 毫秒
      rows_affected: 100,
      index_used: true,
      timestamp: 1640995800000
    }),
    DatabaseMetric({
      query_type: "SELECT",
      execution_time: 150.2, // 毫秒
      rows_affected: 500,
      index_used: false,
      timestamp: 1640995800000
    }),
    DatabaseMetric({
      query_type: "UPDATE",
      execution_time: 45.8, // 毫秒
      rows_affected: 1,
      index_used: true,
      timestamp: 1640995800000
    }),
    DatabaseMetric({
      query_type: "INSERT",
      execution_time: 30.3, // 毫秒
      rows_affected: 1,
      index_used: true,
      timestamp: 1640995800000
    })
  ]
  
  // 4. 验证数据库指标数据
  assert_eq(db_metrics.length(), 4)
  
  // 5. 按查询类型分组统计
  let select_metrics = db_metrics.filter(fn(metric) { metric.query_type == "SELECT" })
  let update_metrics = db_metrics.filter(fn(metric) { metric.query_type == "UPDATE" })
  let insert_metrics = db_metrics.filter(fn(metric) { metric.query_type == "INSERT" })
  
  assert_eq(select_metrics.length(), 2)
  assert_eq(update_metrics.length(), 1)
  assert_eq(insert_metrics.length(), 1)
  
  // 6. 计算每种查询类型的平均执行时间
  let select_avg_time = select_metrics.reduce(fn(acc, metric) { acc + metric.execution_time }, 0.0) / select_metrics.length().to_float()
  let update_avg_time = update_metrics[0].execution_time
  let insert_avg_time = insert_metrics[0].execution_time
  
  assert_eq(select_avg_time, 87.85) // (25.5 + 150.2) / 2
  assert_eq(update_avg_time, 45.8)
  assert_eq(insert_avg_time, 30.3)
  
  // 7. 识别慢查询和未使用索引的查询
  let slow_queries = db_metrics.filter(fn(metric) { metric.execution_time > 100.0 })
  let queries_without_index = db_metrics.filter(fn(metric) { !metric.index_used })
  
  assert_eq(slow_queries.length(), 1)
  assert_eq(queries_without_index.length(), 1)
  
  // 8. 验证慢查询和未使用索引的查询是同一个查询
  assert_eq(slow_queries[0].query_type, "SELECT")
  assert_eq(slow_queries[0].execution_time, 150.2)
  assert_false(slow_queries[0].index_used)
}

// 测试4: 缓存性能监控
test "缓存性能监控测试" {
  // 1. 创建缓存性能监控器
  let cache_monitor = CachePerformanceMonitor({
    cache_type: "redis",
    cache_nodes: [
      "redis-node-1:6379",
      "redis-node-2:6379",
      "redis-node-3:6379"
    ],
    monitoring_metrics: [
      "hit.rate",
      "miss.rate",
      "eviction.rate",
      "memory.usage"
    ]
  })
  
  // 2. 验证缓存监控器配置
  assert_eq(cache_monitor.cache_type, "redis")
  assert_eq(cache_monitor.cache_nodes.length(), 3)
  assert_eq(cache_monitor.monitoring_metrics.length(), 4)
  
  // 3. 模拟收集缓存性能指标
  let cache_metrics = [
    CacheMetric({
      node: "redis-node-1:6379",
      hits: 8500,
      misses: 1500,
      evictions: 50,
      memory_usage: 512.0, // MB
      timestamp: 1640995900000
    }),
    CacheMetric({
      node: "redis-node-2:6379",
      hits: 9200,
      misses: 800,
      evictions: 20,
      memory_usage: 480.0, // MB
      timestamp: 1640995900000
    }),
    CacheMetric({
      node: "redis-node-3:6379",
      hits: 7800,
      misses: 2200,
      evictions: 100,
      memory_usage: 640.0, // MB
      timestamp: 1640995900000
    })
  ]
  
  // 4. 验证缓存指标数据
  assert_eq(cache_metrics.length(), 3)
  
  // 5. 计算每个节点的命中率
  let node_hit_rates = cache_metrics.map(fn(metric) {
    let total_requests = metric.hits + metric.misses
    let hit_rate = metric.hits.to_float() / total_requests.to_float()
    (metric.node, hit_rate)
  })
  
  // 6. 验证节点命中率
  for (node, hit_rate) in node_hit_rates {
    match node {
      "redis-node-1:6379" => assert_eq(hit_rate, 0.85) // 8500 / (8500 + 1500)
      "redis-node-2:6379" => assert_eq(hit_rate, 0.92) // 9200 / (9200 + 800)
      "redis-node-3:6379" => assert_eq(hit_rate, 0.78) // 7800 / (7800 + 2200)
      _ => assert_true(false)
    }
  }
  
  // 7. 计算集群整体性能指标
  let total_hits = cache_metrics.reduce(fn(acc, metric) { acc + metric.hits }, 0)
  let total_misses = cache_metrics.reduce(fn(acc, metric) { acc + metric.misses }, 0)
  let total_evictions = cache_metrics.reduce(fn(acc, metric) { acc + metric.evictions }, 0)
  let total_memory = cache_metrics.reduce(fn(acc, metric) { acc + metric.memory_usage }, 0.0)
  
  assert_eq(total_hits, 25500)
  assert_eq(total_misses, 4500)
  assert_eq(total_evictions, 170)
  assert_eq(total_memory, 1632.0)
  
  // 8. 计算集群整体命中率
  let cluster_hit_rate = total_hits.to_float() / (total_hits + total_misses).to_float()
  assert_eq(cluster_hit_rate, 0.85) // 25500 / (25500 + 4500)
  
  // 9. 识别性能问题节点
  let low_hit_rate_nodes = node_hit_rates.filter(fn(node_rate) {
    let (_, hit_rate) = node_rate
    hit_rate < 0.8
  })
  
  assert_eq(low_hit_rate_nodes.length(), 1)
  assert_eq(low_hit_rate_nodes[0].0, "redis-node-3:6379")
}

// 测试5: 网络性能监控
test "网络性能监控测试" {
  // 1. 创建网络性能监控器
  let network_monitor = NetworkPerformanceMonitor({
    interfaces: [
      "eth0",
      "eth1",
      "lo"
    ],
    monitoring_metrics: [
      "throughput",
      "packet.loss",
      "latency",
      "connection.count"
    ]
  })
  
  // 2. 验证网络监控器配置
  assert_eq(network_monitor.interfaces.length(), 3)
  assert_eq(network_monitor.monitoring_metrics.length(), 4)
  
  // 3. 模拟收集网络性能指标
  let network_metrics = [
    NetworkMetric({
      interface: "eth0",
      direction: "outbound",
      throughput: 1024.5, // Mbps
      packet_loss: 0.01, // 0.01%
      latency: 5.2, // ms
      connection_count: 150,
      timestamp: 1640996000000
    }),
    NetworkMetric({
      interface: "eth0",
      direction: "inbound",
      throughput: 2048.3, // Mbps
      packet_loss: 0.02, // 0.02%
      latency: 8.7, // ms
      connection_count: 200,
      timestamp: 1640996000000
    }),
    NetworkMetric({
      interface: "eth1",
      direction: "outbound",
      throughput: 512.8, // Mbps
      packet_loss: 0.005, // 0.005%
      latency: 3.1, // ms
      connection_count: 75,
      timestamp: 1640996000000
    }),
    NetworkMetric({
      interface: "eth1",
      direction: "inbound",
      throughput: 1024.1, // Mbps
      packet_loss: 0.01, // 0.01%
      latency: 4.5, // ms
      connection_count: 100,
      timestamp: 1640996000000
    })
  ]
  
  // 4. 验证网络指标数据
  assert_eq(network_metrics.length(), 4)
  
  // 5. 按接口分组统计
  let eth0_metrics = network_metrics.filter(fn(metric) { metric.interface == "eth0" })
  let eth1_metrics = network_metrics.filter(fn(metric) { metric.interface == "eth1" })
  
  assert_eq(eth0_metrics.length(), 2)
  assert_eq(eth1_metrics.length(), 2)
  
  // 6. 计算每个接口的总吞吐量和平均延迟
  let eth0_total_throughput = eth0_metrics.reduce(fn(acc, metric) { acc + metric.throughput }, 0.0)
  let eth0_avg_latency = eth0_metrics.reduce(fn(acc, metric) { acc + metric.latency }, 0.0) / eth0_metrics.length().to_float()
  
  let eth1_total_throughput = eth1_metrics.reduce(fn(acc, metric) { acc + metric.throughput }, 0.0)
  let eth1_avg_latency = eth1_metrics.reduce(fn(acc, metric) { acc + metric.latency }, 0.0) / eth1_metrics.length().to_float()
  
  assert_eq(eth0_total_throughput, 3072.8) // 1024.5 + 2048.3
  assert_eq(eth0_avg_latency, 6.95) // (5.2 + 8.7) / 2
  
  assert_eq(eth1_total_throughput, 1536.9) // 512.8 + 1024.1
  assert_eq(eth1_avg_latency, 3.8) // (3.1 + 4.5) / 2
  
  // 7. 识别网络性能问题
  let high_latency_metrics = network_metrics.filter(fn(metric) { metric.latency > 5.0 })
  let high_packet_loss_metrics = network_metrics.filter(fn(metric) { metric.packet_loss > 0.01 })
  
  assert_eq(high_latency_metrics.length(), 1)
  assert_eq(high_latency_metrics[0].interface, "eth0")
  assert_eq(high_latency_metrics[0].direction, "inbound")
  assert_eq(high_latency_metrics[0].latency, 8.7)
  
  assert_eq(high_packet_loss_metrics.length(), 1)
  assert_eq(high_packet_loss_metrics[0].interface, "eth0")
  assert_eq(high_packet_loss_metrics[0].direction, "inbound")
  assert_eq(high_packet_loss_metrics[0].packet_loss, 0.02)
}

// 测试6: 性能基准测试
test "性能基准测试测试" {
  // 1. 创建性能基准测试配置
  let benchmark_config = BenchmarkConfig({
    test_name: "api.performance.baseline",
    test_duration: 300, // 5分钟
    concurrent_users: 100,
    ramp_up_time: 60, // 1分钟
    target_endpoints: [
      "/api/users",
      "/api/orders",
      "/api/products"
    ]
  })
  
  // 2. 验证基准测试配置
  assert_eq(benchmark_config.test_name, "api.performance.baseline")
  assert_eq(benchmark_config.test_duration, 300)
  assert_eq(benchmark_config.concurrent_users, 100)
  assert_eq(benchmark_config.ramp_up_time, 60)
  assert_eq(benchmark_config.target_endpoints.length(), 3)
  
  // 3. 模拟基准测试结果
  let benchmark_results = [
    BenchmarkResult({
      endpoint: "/api/users",
      total_requests: 10000,
      successful_requests: 9950,
      failed_requests: 50,
      min_response_time: 10.5,
      max_response_time: 250.8,
      avg_response_time: 45.6,
      p50_response_time: 40.2,
      p95_response_time: 85.3,
      p99_response_time: 150.7,
      throughput: 33.33 // 请求/秒
    }),
    BenchmarkResult({
      endpoint: "/api/orders",
      total_requests: 8000,
      successful_requests: 7920,
      failed_requests: 80,
      min_response_time: 15.2,
      max_response_time: 320.5,
      avg_response_time: 65.8,
      p50_response_time: 60.1,
      p95_response_time: 120.4,
      p99_response_time: 200.3,
      throughput: 26.67 // 请求/秒
    }),
    BenchmarkResult({
      endpoint: "/api/products",
      total_requests: 12000,
      successful_requests: 11880,
      failed_requests: 120,
      min_response_time: 8.7,
      max_response_time: 180.3,
      avg_response_time: 35.2,
      p50_response_time: 32.5,
      p95_response_time: 70.8,
      p99_response_time: 120.5,
      throughput: 40.0 // 请求/秒
    })
  ]
  
  // 4. 验证基准测试结果
  assert_eq(benchmark_results.length(), 3)
  
  // 5. 计算整体基准测试指标
  let total_requests = benchmark_results.reduce(fn(acc, result) { acc + result.total_requests }, 0)
  let total_successful = benchmark_results.reduce(fn(acc, result) { acc + result.successful_requests }, 0)
  let total_failed = benchmark_results.reduce(fn(acc, result) { acc + result.failed_requests }, 0)
  let total_throughput = benchmark_results.reduce(fn(acc, result) { acc + result.throughput }, 0.0)
  
  assert_eq(total_requests, 30000)
  assert_eq(total_successful, 29750)
  assert_eq(total_failed, 270)
  assert_eq(total_throughput, 100.0)
  
  // 6. 计算整体成功率和错误率
  let overall_success_rate = total_successful.to_float() / total_requests.to_float()
  let overall_error_rate = total_failed.to_float() / total_requests.to_float()
  
  assert_eq(overall_success_rate, 0.9916666666666667) // 29750 / 30000
  assert_eq(overall_error_rate, 0.009) // 270 / 30000
  
  // 7. 识别性能瓶颈
  let slowest_endpoint = benchmark_results.reduce(fn(acc, result) {
    if result.avg_response_time > acc.avg_response_time { result } else { acc }
  }, benchmark_results[0])
  
  let fastest_endpoint = benchmark_results.reduce(fn(acc, result) {
    if result.avg_response_time < acc.avg_response_time { result } else { acc }
  }, benchmark_results[0])
  
  assert_eq(slowest_endpoint.endpoint, "/api/orders")
  assert_eq(slowest_endpoint.avg_response_time, 65.8)
  
  assert_eq(fastest_endpoint.endpoint, "/api/products")
  assert_eq(fastest_endpoint.avg_response_time, 35.2)
}

// 测试7: 性能趋势分析
test "性能趋势分析测试" {
  // 1. 创建时间序列性能数据
  let performance_trends = [
    PerformanceDataPoint({
      timestamp: 1640990000000, // 1小时前
      cpu_usage: 65.5,
      memory_usage: 55.2,
      response_time: 45.8,
      error_rate: 0.02,
      throughput: 1200.0
    }),
    PerformanceDataPoint({
      timestamp: 1640993600000, // 30分钟前
      cpu_usage: 72.3,
      memory_usage: 58.7,
      response_time: 52.4,
      error_rate: 0.025,
      throughput: 1150.0
    }),
    PerformanceDataPoint({
      timestamp: 1640995400000, // 10分钟前
      cpu_usage: 78.9,
      memory_usage: 62.1,
      response_time: 65.3,
      error_rate: 0.035,
      throughput: 1080.0
    }),
    PerformanceDataPoint({
      timestamp: 1640996000000, // 当前
      cpu_usage: 85.2,
      memory_usage: 68.5,
      response_time: 78.6,
      error_rate: 0.045,
      throughput: 980.0
    })
  ]
  
  // 2. 验证性能趋势数据
  assert_eq(performance_trends.length(), 4)
  
  // 3. 计算性能指标的变化趋势
  let cpu_trend = calculate_trend(performance_trends.map(fn(point) { point.cpu_usage }))
  let memory_trend = calculate_trend(performance_trends.map(fn(point) { point.memory_usage }))
  let response_time_trend = calculate_trend(performance_trends.map(fn(point) { point.response_time }))
  let error_rate_trend = calculate_trend(performance_trends.map(fn(point) { point.error_rate }))
  let throughput_trend = calculate_trend(performance_trends.map(fn(point) { point.throughput }))
  
  // 4. 验证趋势分析结果（所有指标都呈上升趋势，除了吞吐量）
  assert_true(cpu_trend > 0) // CPU使用率上升
  assert_true(memory_trend > 0) // 内存使用率上升
  assert_true(response_time_trend > 0) // 响应时间上升
  assert_true(error_rate_trend > 0) // 错误率上升
  assert_true(throughput_trend < 0) // 吞吐量下降
  
  // 5. 计算性能指标的变化率
  let cpu_change_rate = (performance_trends[3].cpu_usage - performance_trends[0].cpu_usage) / performance_trends[0].cpu_usage
  let memory_change_rate = (performance_trends[3].memory_usage - performance_trends[0].memory_usage) / performance_trends[0].memory_usage
  let response_time_change_rate = (performance_trends[3].response_time - performance_trends[0].response_time) / performance_trends[0].response_time
  let error_rate_change_rate = (performance_trends[3].error_rate - performance_trends[0].error_rate) / performance_trends[0].error_rate
  let throughput_change_rate = (performance_trends[3].throughput - performance_trends[0].throughput) / performance_trends[0].throughput
  
  // 6. 验证变化率
  assert_eq(cpu_change_rate, 0.3015267175572519) // (85.2 - 65.5) / 65.5
  assert_eq(memory_change_rate, 0.24057971014492753) // (68.5 - 55.2) / 55.2
  assert_eq(response_time_change_rate, 0.716593842136855) // (78.6 - 45.8) / 45.8
  assert_eq(error_rate_change_rate, 1.25) // (0.045 - 0.02) / 0.02
  assert_eq(throughput_change_rate, -0.18333333333333335) // (980 - 1200) / 1200
  
  // 7. 识别性能异常
  let performance_anomalies = detect_performance_anomalies(performance_trends)
  
  // 验证检测到的异常
  assert_eq(performance_anomalies.length(), 2) // CPU使用率和错误率异常
  
  let cpu_anomaly = performance_anomalies.filter(fn(anomaly) { anomaly.metric == "cpu.usage" })
  let error_rate_anomaly = performance_anomalies.filter(fn(anomaly) { anomaly.metric == "error_rate" })
  
  assert_eq(cpu_anomaly.length(), 1)
  assert_eq(cpu_anomaly[0].severity, "warning")
  assert_true(cpu_anomaly[0].message.contains("85.2"))
  
  assert_eq(error_rate_anomaly.length(), 1)
  assert_eq(error_rate_anomaly[0].severity, "critical")
  assert_true(error_rate_anomaly[0].message.contains("0.045"))
}

// 测试8: 性能告警系统
test "性能告警系统测试" {
  // 1. 创建性能告警配置
  let alert_config = AlertConfig({
    alert_rules: [
      AlertRule({
        name: "high.cpu.usage",
        metric: "cpu.usage",
        operator: ">",
        threshold: 80.0,
        severity: "warning",
        duration: 300 // 5分钟
      }),
      AlertRule({
        name: "high.memory.usage",
        metric: "memory.usage",
        operator: ">",
        threshold: 85.0,
        severity: "warning",
        duration: 300 // 5分钟
      }),
      AlertRule({
        name: "high.error.rate",
        metric: "error.rate",
        operator: ">",
        threshold: 0.05,
        severity: "critical",
        duration: 60 // 1分钟
      }),
      AlertRule({
        name: "low.throughput",
        metric: "throughput",
        operator: "<",
        threshold: 1000.0,
        severity: "warning",
        duration: 300 // 5分钟
      })
    ]
  })
  
  // 2. 验证告警配置
  assert_eq(alert_config.alert_rules.length(), 4)
  
  // 3. 模拟当前性能指标
  let current_metrics = [
    ("cpu.usage", 85.2),
    ("memory.usage", 68.5),
    ("error.rate", 0.045),
    ("throughput", 980.0)
  ]
  
  // 4. 检查告警规则
  let triggered_alerts = check_alert_rules(current_metrics, alert_config.alert_rules)
  
  // 5. 验证触发的告警
  assert_eq(triggered_alerts.length(), 2) // CPU使用率和吞吐量告警
  
  // 6. 验证告警内容
  let mut cpu_alert_found = false
  let mut throughput_alert_found = false
  
  for alert in triggered_alerts {
    match alert.rule_name {
      "high.cpu.usage" => {
        assert_eq(alert.severity, "warning")
        assert_eq(alert.current_value, 85.2)
        assert_eq(alert.threshold, 80.0)
        cpu_alert_found = true
      }
      "low.throughput" => {
        assert_eq(alert.severity, "warning")
        assert_eq(alert.current_value, 980.0)
        assert_eq(alert.threshold, 1000.0)
        throughput_alert_found = true
      }
      _ => assert_true(false)
    }
  }
  
  assert_true(cpu_alert_found)
  assert_true(throughput_alert_found)
  
  // 7. 测试告警抑制和恢复
  let recovered_metrics = [
    ("cpu.usage", 75.0),
    ("memory.usage", 68.5),
    ("error.rate", 0.045),
    ("throughput", 1100.0)
  ]
  
  let recovered_alerts = check_alert_rules(recovered_metrics, alert_config.alert_rules)
  
  // 8. 验证告警恢复
  assert_eq(recovered_alerts.length(), 0) // 所有告警都已恢复
}