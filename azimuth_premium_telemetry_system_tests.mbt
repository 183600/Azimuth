// Azimuth 高级遥测系统测试用例
// 专注于遥测系统的核心功能和高级特性

// 测试1: 遥测数据生命周期管理
test "遥测数据生命周期管理测试" {
  // 创建生命周期管理器
  let lifecycle_manager = TelemetryLifecycleManager::new()
  
  // 配置数据保留策略
  LifecycleManager::set_retention_policy(lifecycle_manager, {
    raw_data: 7 * 24 * 60 * 60,      // 原始数据保留7天
    aggregated_data: 30 * 24 * 60 * 60,  // 聚合数据保留30天
    archived_data: 365 * 24 * 60 * 60    // 归档数据保留1年
  })
  
  // 创建测试遥测数据
  let current_time = 1640995200  // 2022-01-01 00:00:00 UTC
  let old_data_time = current_time - 10 * 24 * 60 * 60  // 10天前
  let very_old_data_time = current_time - 40 * 24 * 60 * 60  // 40天前
  
  let recent_data = TelemetryData::new("trace-1", "span-1", current_time, "service.a")
  let old_data = TelemetryData::new("trace-2", "span-2", old_data_time, "service.b")
  let very_old_data = TelemetryData::new("trace-3", "span-3", very_old_data_time, "service.c")
  
  // 存储数据
  LifecycleManager::store_data(lifecycle_manager, recent_data)
  LifecycleManager::store_data(lifecycle_manager, old_data)
  LifecycleManager::store_data(lifecycle_manager, very_old_data)
  
  // 验证数据存储
  let all_data = LifecycleManager::get_all_data(lifecycle_manager)
  assert_eq(all_data.length(), 3)
  
  // 执行生命周期清理
  let cleanup_result = LifecycleManager::execute_cleanup(lifecycle_manager, current_time)
  
  // 验证清理结果
  assert_true(cleanup_result.deleted_count > 0)
  assert_eq(cleanup_result.archived_count, 1)  // 10天前的数据被归档
  assert_eq(cleanup_result.purged_count, 1)    // 40天前的数据被清除
  
  // 验证剩余数据
  let remaining_data = LifecycleManager::get_all_data(lifecycle_manager)
  assert_eq(remaining_data.length(), 2)  // 只保留最近的数据和归档数据
  
  // 测试数据恢复
  let archived_data = LifecycleManager::get_archived_data(lifecycle_manager)
  assert_eq(archived_data.length(), 1)
  
  let restored_data = LifecycleManager::restore_from_archive(lifecycle_manager, archived_data[0].id)
  assert_true(restored_data.is_some)
  assert_eq(restored_data.unwrap().trace_id, "trace-2")
}

// 测试2: 遥测数据质量保证
test "遥测数据质量保证测试" {
  // 创建数据质量管理器
  let quality_manager = DataQualityManager::new()
  
  // 配置质量规则
  QualityManager::add_rule(quality_manager, "trace_id_format", {
    pattern: "^[a-zA-Z0-9-]{10,50}$",
    description: "追踪ID必须是10-50位字母数字和连字符"
  })
  
  QualityManager::add_rule(quality_manager, "span_duration_range", {
    min_value: 0,
    max_value: 3600000,  // 最大1小时
    description: "Span持续时间必须在0-1小时之间"
  })
  
  QualityManager::add_rule(quality_manager, "service_name_format", {
    pattern: "^[a-z][a-z0-9-]*[a-z0-9]$",
    description: "服务名必须以小写字母开头，只包含小写字母、数字和连字符"
  })
  
  // 测试有效数据
  let valid_data = TelemetryData::new("trace-12345", "span-67890", 1640995200, "payment-service")
  valid_data.duration = 1500  // 1.5秒
  valid_data.status = "ok"
  
  let valid_quality_result = QualityManager::validate_data(quality_manager, valid_data)
  assert_true(valid_quality_result.is_valid)
  assert_eq(valid_quality_result.violations.length(), 0)
  
  // 测试无效数据
  let invalid_data = TelemetryData::new("", "span-67890", 1640995200, "123InvalidService")
  invalid_data.duration = -100  // 负数持续时间
  invalid_data.status = "ok"
  
  let invalid_quality_result = QualityManager::validate_data(quality_manager, invalid_data)
  assert_false(invalid_quality_result.is_valid)
  assert_true(invalid_quality_result.violations.length() >= 2)
  
  // 验证具体违规项
  let trace_id_violation = invalid_quality_result.violations.find(fn(v) { v.rule == "trace_id_format" })
  assert_true(trace_id_violation.is_some)
  
  let service_name_violation = invalid_quality_result.violations.find(fn(v) { v.rule == "service_name_format" })
  assert_true(service_name_violation.is_some)
  
  let duration_violation = invalid_quality_result.violations.find(fn(v) { v.rule == "span_duration_range" })
  assert_true(duration_violation.is_some)
  
  // 测试数据修复
  let repair_result = QualityManager::repair_data(quality_manager, invalid_data)
  assert_true(repair_result.success)
  assert_true(repair_result.repaired_data.is_some)
  
  let repaired_data = repair_result.repaired_data.unwrap()
  assert_true(repaired_data.trace_id.length() > 0)
  assert_true(repaired_data.duration >= 0)
  
  // 验证修复后的数据质量
  let repaired_quality_result = QualityManager::validate_data(quality_manager, repaired_data)
  assert_true(repaired_quality_result.is_valid)
}

// 测试3: 遥测系统安全性测试
test "遥测系统安全性测试" {
  // 创建安全管理器
  let security_manager = SecurityManager::new()
  
  // 配置加密设置
  SecurityManager::set_encryption_config(security_manager, {
    algorithm: "AES-256-GCM",
    key_rotation_interval: 7 * 24 * 60 * 60,  // 7天轮换一次密钥
    key_derivation: "PBKDF2"
  })
  
  // 配置访问控制
  SecurityManager::add_access_policy(security_manager, {
    name: "admin_policy",
    permissions: ["read", "write", "delete", "admin"],
    resource_patterns: ["*"]
  })
  
  SecurityManager::add_access_policy(security_manager, {
    name: "readonly_policy",
    permissions: ["read"],
    resource_patterns: ["telemetry.data.*", "telemetry.metrics.*"]
  })
  
  // 创建测试用户
  let admin_user = SecurityUser::new("admin", ["admin_policy"])
  let readonly_user = SecurityUser::new("analyst", ["readonly_policy"])
  
  // 测试数据加密
  let sensitive_data = TelemetryData::new("trace-sensitive", "span-sensitive", 1640995200, "auth-service")
  sensitive_data.add_attribute("user.email", "user@example.com")
  sensitive_data.add_attribute("user.id", "12345")
  sensitive_data.add_attribute("api.key", "secret-api-key-12345")
  
  // 加密敏感属性
  let encrypted_data = SecurityManager::encrypt_sensitive_attributes(security_manager, sensitive_data)
  
  // 验证加密
  assert_true(encrypted_data.attributes.length() == sensitive_data.attributes.length())
  
  let email_attr = encrypted_data.attributes.find(fn(a) { a.key == "user.email" })
  assert_true(email_attr.is_some)
  assert_ne(email_attr.unwrap().value, "user@example.com")  // 应该是加密后的值
  assert_true(email_attr.unwrap().value.starts_with("enc:"))
  
  let api_key_attr = encrypted_data.attributes.find(fn(a) { a.key == "api.key" })
  assert_true(api_key_attr.is_some)
  assert_ne(api_key_attr.unwrap().value, "secret-api-key-12345")
  assert_true(api_key_attr.unwrap().value.starts_with("enc:"))
  
  // 解密敏感属性
  let decrypted_data = SecurityManager::decrypt_sensitive_attributes(security_manager, encrypted_data)
  
  // 验证解密
  let decrypted_email_attr = decrypted_data.attributes.find(fn(a) { a.key == "user.email" })
  assert_true(decrypted_email_attr.is_some)
  assert_eq(decrypted_email_attr.unwrap().value, "user@example.com")
  
  // 测试访问控制
  let admin_access = SecurityManager::check_access(security_manager, admin_user, "telemetry.data.trace-sensitive", "read")
  assert_true(admin_access)
  
  let readonly_access = SecurityManager::check_access(security_manager, readonly_user, "telemetry.data.trace-sensitive", "read")
  assert_true(readonly_access)
  
  let readonly_write_access = SecurityManager::check_access(security_manager, readonly_user, "telemetry.data.trace-sensitive", "write")
  assert_false(readonly_write_access)
  
  // 测试数据脱敏
  let masked_data = SecurityManager::mask_pii_data(security_manager, sensitive_data, readonly_user)
  
  // 验证脱敏
  let masked_email_attr = masked_data.attributes.find(fn(a) { a.key == "user.email" })
  assert_true(masked_email_attr.is_some)
  assert_eq(masked_email_attr.unwrap().value, "u***@example.com")
  
  let masked_id_attr = masked_data.attributes.find(fn(a) { a.key == "user.id" })
  assert_true(masked_id_attr.is_some)
  assert_eq(masked_id_attr.unwrap().value, "*****")
  
  // 管理员不应该看到脱敏数据
  let admin_masked_data = SecurityManager::mask_pii_data(security_manager, sensitive_data, admin_user)
  let admin_email_attr = admin_masked_data.attributes.find(fn(a) { a.key == "user.email" })
  assert_true(admin_email_attr.is_some)
  assert_eq(admin_email_attr.unwrap().value, "user@example.com")
}

// 测试4: 遥测数据可视化测试
test "遥测数据可视化测试" {
  // 创建可视化管理器
  let visualization_manager = VisualizationManager::new()
  
  // 创建时间序列图表
  let time_series_chart = VisualizationManager::create_time_series_chart(visualization_manager, {
    title: "服务请求率趋势",
    metric: "http.requests.total",
    aggregation: "rate",
    time_range: "1h",
    group_by: ["service.name"],
    chart_type: "line"
  })
  
  // 添加时间序列数据点
  let base_time = 1640995200
  for i in 0..=60 {
    let timestamp = base_time + i * 60  // 每分钟一个数据点
    
    VisualizationManager::add_data_point(visualization_manager, time_series_chart.id, timestamp, 100.0 + (i % 10) * 5.0, [
      ("service.name", "api.gateway")
    ])
    
    VisualizationManager::add_data_point(visualization_manager, time_series_chart.id, timestamp, 80.0 + (i % 8) * 4.0, [
      ("service.name", "auth.service")
    ])
    
    VisualizationManager::add_data_point(visualization_manager, time_series_chart.id, timestamp, 120.0 + (i % 12) * 6.0, [
      ("service.name", "payment.service")
    ])
  }
  
  // 生成图表配置
  let chart_config = VisualizationManager::generate_chart_config(visualization_manager, time_series_chart.id)
  
  // 验证图表配置
  assert_true(chart_config.series.length() >= 3)
  assert_true(chart_config.series.any(fn(s) { s.name == "api.gateway" }))
  assert_true(chart_config.series.any(fn(s) { s.name == "auth.service" }))
  assert_true(chart_config.series.any(fn(s) { s.name == "payment.service" }))
  
  // 创建热力图
  let heatmap_chart = VisualizationManager::create_heatmap(visualization_manager, {
    title: "服务延迟热力图",
    metric: "http.request.duration",
    aggregation: "percentile",
    percentile: 95,
    time_range: "24h",
    x_axis: "hour",
    y_axis: "service.name",
    color_scale: "viridis"
  })
  
  // 添加热力图数据
  for hour in 0..=23 {
    for service in ["api.gateway", "auth.service", "payment.service", "notification.service"] {
      let latency = 50.0 + hour.to_float() * 2.0 + (service.length() * 5.0).to_float()
      VisualizationManager::add_heatmap_data(visualization_manager, heatmap_chart.id, hour, service, latency)
    }
  }
  
  // 生成热力图配置
  let heatmap_config = VisualizationManager::generate_chart_config(visualization_manager, heatmap_chart.id)
  
  // 验证热力图配置
  assert_true(heatmap_config.data_matrix.length() >= 4)  // 4个服务
  assert_true(heatmap_config.data_matrix[0].length() == 24)  // 24小时
  
  // 创建服务拓扑图
  let topology_chart = VisualizationManager::create_topology_chart(visualization_manager, {
    title: "服务调用拓扑",
    time_range: "1h",
    node_size_metric: "request_count",
    edge_width_metric: "call_count",
    layout_algorithm: "force_directed"
  })
  
  // 添加拓扑节点和边
  VisualizationManager::add_topology_node(visualization_manager, topology_chart.id, "api.gateway", {
    x: 100.0,
    y: 100.0,
    size: 30.0,
    color: "#4285F4"
  })
  
  VisualizationManager::add_topology_node(visualization_manager, topology_chart.id, "auth.service", {
    x: 200.0,
    y: 150.0,
    size: 20.0,
    color: "#34A853"
  })
  
  VisualizationManager::add_topology_node(visualization_manager, topology_chart.id, "payment.service", {
    x: 300.0,
    y: 100.0,
    size: 25.0,
    color: "#FBBC05"
  })
  
  // 添加服务调用边
  VisualizationManager::add_topology_edge(visualization_manager, topology_chart.id, "api.gateway", "auth.service", {
    width: 5.0,
    color: "#EA4335",
    calls: 1500
  })
  
  VisualizationManager::add_topology_edge(visualization_manager, topology_chart.id, "api.gateway", "payment.service", {
    width: 8.0,
    color: "#EA4335",
    calls: 2500
  })
  
  VisualizationManager::add_topology_edge(visualization_manager, topology_chart.id, "auth.service", "payment.service", {
    width: 3.0,
    color: "#EA4335",
    calls: 800
  })
  
  // 生成拓扑图配置
  let topology_config = VisualizationManager::generate_chart_config(visualization_manager, topology_chart.id)
  
  // 验证拓扑图配置
  assert_true(topology_config.nodes.length() == 3)
  assert_true(topology_config.edges.length() == 3)
  
  let gateway_node = topology_config.nodes.find(fn(n) { n.id == "api.gateway" })
  assert_true(gateway_node.is_some)
  assert_eq(gateway_node.unwrap().size, 30.0)
  
  let gateway_payment_edge = topology_config.edges.find(fn(e) { e.source == "api.gateway" and e.target == "payment.service" })
  assert_true(gateway_payment_edge.is_some)
  assert_eq(gateway_payment_edge.unwrap().width, 8.0)
  
  // 创建仪表板
  let dashboard = VisualizationManager::create_dashboard(visualization_manager, {
    title: "系统监控仪表板",
    layout: "grid",
    refresh_interval: 30000  // 30秒刷新
  })
  
  // 添加图表到仪表板
  VisualizationManager::add_chart_to_dashboard(visualization_manager, dashboard.id, time_series_chart.id, {
    position: { row: 0, col: 0, width: 12, height: 6 }
  })
  
  VisualizationManager::add_chart_to_dashboard(visualization_manager, dashboard.id, heatmap_chart.id, {
    position: { row: 6, col: 0, width: 6, height: 6 }
  })
  
  VisualizationManager::add_chart_to_dashboard(visualization_manager, dashboard.id, topology_chart.id, {
    position: { row: 6, col: 6, width: 6, height: 6 }
  })
  
  // 验证仪表板
  let dashboard_config = VisualizationManager::get_dashboard_config(visualization_manager, dashboard.id)
  assert_eq(dashboard_config.charts.length(), 3)
  assert_eq(dashboard_config.title, "系统监控仪表板")
  
  // 导出仪表板
  let exported_dashboard = VisualizationManager::export_dashboard(visualization_manager, dashboard.id, "json")
  assert_true(exported_dashboard.contains("系统监控仪表板"))
  assert_true(exported_dashboard.contains("time_series"))
  assert_true(exported_dashboard.contains("heatmap"))
  assert_true(exported_dashboard.contains("topology"))
}

// 测试5: 遥测系统性能基准测试
test "遥测系统性能基准测试" {
  // 创建性能基准测试管理器
  let benchmark_manager = BenchmarkManager::new()
  
  // 配置基准测试参数
  BenchmarkManager::configure(benchmark_manager, {
    concurrent_threads: 10,
    operations_per_thread: 1000,
    warmup_iterations: 100,
    measurement_iterations: 1000
  })
  
  // 测试数据插入性能
  let insert_benchmark = BenchmarkManager::create_benchmark(benchmark_manager, "data_insert", {
    setup: fn() {
      // 准备测试数据
      let test_data = []
      for i in 0..=1000 {
        test_data = test_data.push(TelemetryData::new(
          "trace-" + i.to_string(),
          "span-" + i.to_string(),
          1640995200 + i,
          "service-" + (i % 10).to_string()
        ))
      }
      test_data
    },
    operation: fn(data) {
      // 插入数据操作
      let storage = TelemetryStorage::new()
      for item in data {
        Storage::insert(storage, item)
      }
      data.length()
    },
    teardown: fn() {
      // 清理资源
      TelemetryStorage::cleanup_all()
    }
  })
  
  // 执行插入基准测试
  let insert_results = BenchmarkManager::run_benchmark(benchmark_manager, insert_benchmark)
  
  // 验证插入性能结果
  assert_true(insert_results.total_operations > 0)
  assert_true(insert_results.avg_latency_ms > 0)
  assert_true(insert_results.throughput_ops_per_sec > 0)
  assert_true(insert_results.p99_latency_ms >= insert_results.avg_latency_ms)
  
  // 测试数据查询性能
  let query_benchmark = BenchmarkManager::create_benchmark(benchmark_manager, "data_query", {
    setup: fn() {
      // 准备测试数据和索引
      let storage = TelemetryStorage::new()
      for i in 0..=10000 {
        let data = TelemetryData::new(
          "trace-" + i.to_string(),
          "span-" + i.to_string(),
          1640995200 + i,
          "service-" + (i % 10).to_string()
        )
        Storage::insert(storage, data)
      }
      Storage::build_indexes(storage)
      storage
    },
    operation: fn(storage) {
      // 查询操作
      let results = Storage::query_by_service(storage, "service-5", 100)
      results.length()
    },
    teardown: fn(storage) {
      // 清理资源
      Storage::cleanup(storage)
    }
  })
  
  // 执行查询基准测试
  let query_results = BenchmarkManager::run_benchmark(benchmark_manager, query_benchmark)
  
  // 验证查询性能结果
  assert_true(query_results.total_operations > 0)
  assert_true(query_results.avg_latency_ms > 0)
  assert_true(query_results.throughput_ops_per_sec > 0)
  
  // 查询应该比插入快
  assert_true(query_results.avg_latency_ms < insert_results.avg_latency_ms)
  
  // 测试聚合性能
  let aggregation_benchmark = BenchmarkManager::create_benchmark(benchmark_manager, "data_aggregation", {
    setup: fn() {
      // 准备大量测试数据
      let storage = TelemetryStorage::new()
      for i in 0..=50000 {
        let data = TelemetryData::new(
          "trace-" + i.to_string(),
          "span-" + i.to_string(),
          1640995200 + (i * 10),
          "service-" + (i % 20).to_string()
        )
        data.duration = 50 + (i % 500)
        data.status = if i % 50 == 0 { "error" } else { "ok" }
        Storage::insert(storage, data)
      }
      Storage::build_indexes(storage)
      storage
    },
    operation: fn(storage) {
      // 聚合操作
      let agg_config = AggregationConfig::new()
      AggregationConfig::add_group_by(agg_config, "service.name")
      AggregationConfig::add_metric(agg_config, "duration", "avg")
      AggregationConfig::add_metric(agg_config, "duration", "p95")
      AggregationConfig::add_metric(agg_config, "status", "error_rate")
      
      Storage::aggregate(storage, agg_config)
    },
    teardown: fn(storage) {
      // 清理资源
      Storage::cleanup(storage)
    }
  })
  
  // 执行聚合基准测试
  let aggregation_results = BenchmarkManager::run_benchmark(benchmark_manager, aggregation_benchmark)
  
  // 验证聚合性能结果
  assert_true(aggregation_results.total_operations > 0)
  assert_true(aggregation_results.avg_latency_ms > 0)
  assert_true(aggregation_results.throughput_ops_per_sec > 0)
  
  // 聚合应该比纯查询慢，但比批量插入快
  assert_true(aggregation_results.avg_latency_ms > query_results.avg_latency_ms)
  assert_true(aggregation_results.avg_latency_ms < insert_results.avg_latency_ms)
  
  // 生成性能报告
  let performance_report = BenchmarkManager::generate_report(benchmark_manager, [
    insert_results,
    query_results,
    aggregation_results
  ])
  
  // 验证性能报告
  assert_true(performance_report.benchmarks.length() == 3)
  assert_true(performance_report.summary.total_operations > 0)
  assert_true(performance_report.summary.avg_throughput > 0)
  
  // 验证报告包含所有基准测试
  assert_true(performance_report.benchmarks.any(fn(b) { b.name == "data_insert" }))
  assert_true(performance_report.benchmarks.any(fn(b) { b.name == "data_query" }))
  assert_true(performance_report.benchmarks.any(fn(b) { b.name == "data_aggregation" }))
  
  // 导出性能报告
  let exported_report = BenchmarkManager::export_report(benchmark_manager, performance_report, "json")
  assert_true(exported_report.contains("data_insert"))
  assert_true(exported_report.contains("data_query"))
  assert_true(exported_report.contains("data_aggregation"))
  assert_true(exported_report.contains("throughput"))
  assert_true(exported_report.contains("latency"))
}

// 测试6: 遥测系统容错能力测试
test "遥测系统容错能力测试" {
  // 创建容错管理器
  let resilience_manager = ResilienceManager::new()
  
  // 配置熔断器
  ResilienceManager::configure_circuit_breaker(resilience_manager, "telemetry_collector", {
    failure_threshold: 5,
    recovery_timeout: 30000,  // 30秒
    half_open_max_calls: 3,
    success_threshold: 2
  })
  
  // 配置重试策略
  ResilienceManager::configure_retry_policy(resilience_manager, "telemetry_storage", {
    max_attempts: 3,
    initial_backoff: 1000,  // 1秒
    max_backoff: 10000,     // 10秒
    backoff_multiplier: 2.0,
    retryable_errors: ["timeout", "connection_refused", "service_unavailable"]
  })
  
  // 配置限流
  ResilienceManager::configure_rate_limiter(resilience_manager, "telemetry_api", {
    requests_per_second: 1000,
    burst_size: 100,
    strategy: "token_bucket"
  })
  
  // 创建模拟遥测收集器
  let collector = MockTelemetryCollector::new()
  collector.set_failure_rate(0.3)  // 30%失败率
  
  // 测试熔断器功能
  let mut circuit_breaker_trips = 0
  let mut successful_calls = 0
  let mut failed_calls = 0
  
  for i in 0..=20 {
    let result = ResilienceManager::execute_with_circuit_breaker(
      resilience_manager,
      "telemetry_collector",
      fn() { collector.collect_telemetry() }
    )
    
    match result {
      Ok(_) => successful_calls = successful_calls + 1,
      Err(error) => {
        failed_calls = failed_calls + 1
        if error.type == "circuit_breaker_open" {
          circuit_breaker_trips = circuit_breaker_trips + 1
        }
      }
    }
  }
  
  // 验证熔断器触发
  assert_true(circuit_breaker_trips > 0)
  assert_true(failed_calls > 0)
  assert_true(successful_calls > 0)
  
  // 验证熔断器状态
  let circuit_breaker_state = ResilienceManager::get_circuit_breaker_state(resilience_manager, "telemetry_collector")
  assert_true(circuit_breaker_state.state == "open" or circuit_breaker_state.state == "half_open")
  
  // 测试重试策略
  let storage = MockTelemetryStorage::new()
  storage.set_failure_rate(0.5)  // 50%失败率，但前两次失败第三次成功
  
  let mut retry_attempts = 0
  let final_result = ResilienceManager::execute_with_retry(
    resilience_manager,
    "telemetry_storage",
    fn() {
      retry_attempts = retry_attempts + 1
      storage.store_telemetry()
    }
  )
  
  // 验证重试行为
  assert_true(retry_attempts >= 1 and retry_attempts <= 3)
  assert_true(final_result.is_ok)  // 最终应该成功
  
  // 测试限流功能
  let mut rate_limited_calls = 0
  let mut successful_api_calls = 0
  
  for i in 0..=150 {  // 超过限流阈值
    let result = ResilienceManager::execute_with_rate_limiter(
      resilience_manager,
      "telemetry_api",
      fn() { 
        successful_api_calls = successful_api_calls + 1
        "api_response"
      }
    )
    
    if result.is_err and result.unwrap_err().type == "rate_limited" {
      rate_limited_calls = rate_limited_calls + 1
    }
  }
  
  // 验证限流行为
  assert_true(rate_limited_calls > 0)
  assert_true(successful_api_calls <= 110)  // 允许一些突发，但不应超过太多
  
  // 测试故障恢复
  let fault_injector = FaultInjector::new()
  
  // 注入网络分区故障
  FaultInjector::inject_network_partition(fault_injector, ["service.a", "service.b"], 5000)
  
  // 测试系统在网络分区下的行为
  let partition_result = ResilienceManager::test_network_partition_resilience(
    resilience_manager,
    fault_injector,
    10000  // 10秒测试时间
  )
  
  // 验证网络分区恢复
  assert_true(partition_result.degraded_duration > 0)
  assert_true(partition_result.recovery_time > 0)
  assert_true(partition_result.lost_requests > 0)
  
  // 注入内存压力故障
  FaultInjector::inject_memory_pressure(fault_injector, 0.8)  // 80%内存使用率
  
  // 测试系统在内存压力下的行为
  let memory_pressure_result = ResilienceManager::test_memory_pressure_resilience(
    resilience_manager,
    fault_injector,
    5000  // 5秒测试时间
  )
  
  // 验证内存压力恢复
  assert_true(memory_pressure_result.degraded_duration > 0)
  assert_true(memory_pressure_result.recovery_time > 0)
  assert_true(memory_pressure_result.cache_evictions > 0)
  
  // 生成容错报告
  let resilience_report = ResilienceManager::generate_resilience_report(resilience_manager)
  
  // 验证容错报告
  assert_true(resilience_report.circuit_breakers.length() > 0)
  assert_true(resilience_report.retry_policies.length() > 0)
  assert_true(resilience_report.rate_limiters.length() > 0)
  assert_true(resilience_report.fault_injection_tests.length() >= 2)
  
  // 验证报告包含统计信息
  assert_true(resilience_report.summary.total_circuit_breaker_trips >= circuit_breaker_trips)
  assert_true(resilience_report.summary.total_retries > 0)
  assert_true(resilience_report.summary.total_rate_limited_requests >= rate_limited_calls)
}

// 测试7: 遥测数据多租户隔离测试
test "遥测数据多租户隔离测试" {
  // 创建多租户管理器
  let multi_tenant_manager = MultiTenantManager::new()
  
  // 配置租户
  let tenant_a = MultiTenantManager::create_tenant(multi_tenant_manager, {
    id: "tenant-a",
    name: "客户A",
    data_retention_days: 30,
    storage_quota_gb: 100,
    rate_limit_rps: 500
  })
  
  let tenant_b = MultiTenantManager::create_tenant(multi_tenant_manager, {
    id: "tenant-b",
    name: "客户B",
    data_retention_days: 60,
    storage_quota_gb: 200,
    rate_limit_rps: 1000
  })
  
  let tenant_c = MultiTenantManager::create_tenant(multi_tenant_manager, {
    id: "tenant-c",
    name: "客户C",
    data_retention_days: 90,
    storage_quota_gb: 50,
    rate_limit_rps: 200
  })
  
  // 验证租户创建
  assert_eq(tenant_a.id, "tenant-a")
  assert_eq(tenant_a.data_retention_days, 30)
  assert_eq(tenant_a.storage_quota_gb, 100)
  assert_eq(tenant_a.rate_limit_rps, 500)
  
  // 创建租户A的数据
  let tenant_a_data = []
  for i in 0..=100 {
    let data = TelemetryData::new("tenant-a-trace-" + i.to_string(), "span-" + i.to_string(), 1640995200 + i, "service.a")
    data.add_tenant_tag("tenant-a")
    tenant_a_data = tenant_a_data.push(data)
  }
  
  // 创建租户B的数据
  let tenant_b_data = []
  for i in 0..=200 {
    let data = TelemetryData::new("tenant-b-trace-" + i.to_string(), "span-" + i.to_string(), 1640995200 + i, "service.b")
    data.add_tenant_tag("tenant-b")
    tenant_b_data = tenant_b_data.push(data)
  }
  
  // 存储租户数据
  MultiTenantManager::store_tenant_data(multi_tenant_manager, tenant_a.id, tenant_a_data)
  MultiTenantManager::store_tenant_data(multi_tenant_manager, tenant_b.id, tenant_b_data)
  
  // 测试租户数据隔离
  let tenant_a_retrieved = MultiTenantManager::get_tenant_data(multi_tenant_manager, tenant_a.id)
  let tenant_b_retrieved = MultiTenantManager::get_tenant_data(multi_tenant_manager, tenant_b.id)
  
  // 验证数据隔离
  assert_eq(tenant_a_retrieved.length(), 101)
  assert_eq(tenant_b_retrieved.length(), 201)
  
  // 验证租户A只能访问自己的数据
  let tenant_a_only_data = tenant_a_retrieved.filter(fn(d) { d.has_tenant_tag("tenant-a") })
  assert_eq(tenant_a_only_data.length(), 101)
  
  let tenant_a_cross_tenant = tenant_a_retrieved.filter(fn(d) { d.has_tenant_tag("tenant-b") })
  assert_eq(tenant_a_cross_tenant.length(), 0)
  
  // 验证租户B只能访问自己的数据
  let tenant_b_only_data = tenant_b_retrieved.filter(fn(d) { d.has_tenant_tag("tenant-b") })
  assert_eq(tenant_b_only_data.length(), 201)
  
  let tenant_b_cross_tenant = tenant_b_retrieved.filter(fn(d) { d.has_tenant_tag("tenant-a") })
  assert_eq(tenant_b_cross_tenant.length(), 0)
  
  // 测试租户配额限制
  let storage_usage = MultiTenantManager::get_storage_usage(multi_tenant_manager, tenant_a.id)
  assert_true(storage_usage.used_gb < tenant_a.storage_quota_gb)
  
  // 测试租户速率限制
  let mut rate_limited_requests = 0
  let mut successful_requests = 0
  
  for i in 0..=600 {  // 超过租户A的速率限制
    let result = MultiTenantManager::execute_tenant_request(
      multi_tenant_manager,
      tenant_a.id,
      fn() { "request_result" }
    )
    
    match result {
      Ok(_) => successful_requests = successful_requests + 1,
      Err(error) => {
        if error.type == "rate_limited" {
          rate_limited_requests = rate_limited_requests + 1
        }
      }
    }
  }
  
  // 验证速率限制
  assert_true(rate_limited_requests > 0)
  assert_true(successful_requests <= tenant_a.rate_limit_rps + 10)  // 允许一些突发
  
  // 测试租户数据保留策略
  let old_data_time = 1640995200 - 35 * 24 * 60 * 60  // 35天前
  let old_tenant_a_data = TelemetryData::new("tenant-a-old-trace", "span-old", old_data_time, "service.a")
  old_tenant_a_data.add_tenant_tag("tenant-a")
  
  MultiTenantManager::store_tenant_data(multi_tenant_manager, tenant_a.id, [old_tenant_a_data])
  
  // 执行数据保留策略清理
  let cleanup_result = MultiTenantManager::execute_retention_cleanup(multi_tenant_manager)
  
  // 验证清理结果
  assert_true(cleanup_result.deleted_records > 0)
  
  let tenant_a_after_cleanup = MultiTenantManager::get_tenant_data(multi_tenant_manager, tenant_a.id)
  let old_data_exists = tenant_a_after_cleanup.any(fn(d) { d.trace_id == "tenant-a-old-trace" })
  assert_false(old_data_exists)  // 35天的数据应该被删除（租户A保留30天）
  
  // 测试租户聚合数据隔离
  let tenant_a_aggregation = MultiTenantManager::aggregate_tenant_data(
    multi_tenant_manager,
    tenant_a.id,
    { group_by: ["service.name"], metrics: ["duration"] }
  )
  
  let tenant_b_aggregation = MultiTenantManager::aggregate_tenant_data(
    multi_tenant_manager,
    tenant_b.id,
    { group_by: ["service.name"], metrics: ["duration"] }
  )
  
  // 验证聚合数据隔离
  assert_true(tenant_a_aggregation.length() > 0)
  assert_true(tenant_b_aggregation.length() > 0)
  
  let tenant_a_services = tenant_a_aggregation.map(fn(a) { a.group.service_name })
  let tenant_b_services = tenant_b_aggregation.map(fn(a) { a.group.service_name })
  
  assert_true(tenant_a_services.contains("service.a"))
  assert_false(tenant_a_services.contains("service.b"))
  
  assert_true(tenant_b_services.contains("service.b"))
  assert_false(tenant_b_services.contains("service.a"))
  
  // 测试租户配置管理
  let updated_tenant_a = MultiTenantManager::update_tenant_config(
    multi_tenant_manager,
    tenant_a.id,
    { data_retention_days: 45, storage_quota_gb: 150 }
  )
  
  // 验证配置更新
  assert_eq(updated_tenant_a.data_retention_days, 45)
  assert_eq(updated_tenant_a.storage_quota_gb, 150)
  assert_eq(updated_tenant_a.rate_limit_rps, 500)  // 未更新的字段保持不变
  
  // 生成多租户报告
  let tenant_report = MultiTenantManager::generate_tenant_report(multi_tenant_manager)
  
  // 验证多租户报告
  assert_true(tenant_report.tenants.length() == 3)
  assert_true(tenant_report.summary.total_tenants == 3)
  assert_true(tenant_report.summary.total_storage_used_gb > 0)
  assert_true(tenant_report.summary.total_requests > 0)
}

// 测试8: 遥测系统配置管理测试
test "遥测系统配置管理测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 配置默认值
  ConfigManager::set_default(config_manager, "telemetry.sampling_rate", 0.1)
  ConfigManager::set_default(config_manager, "telemetry.batch_size", 100)
  ConfigManager::set_default(config_manager, "telemetry.flush_interval_ms", 5000)
  ConfigManager::set_default(config_manager, "telemetry.max_spans_per_trace", 1000)
  
  // 验证默认值
  assert_eq(ConfigManager::get(config_manager, "telemetry.sampling_rate"), Some(0.1))
  assert_eq(ConfigManager::get(config_manager, "telemetry.batch_size"), Some(100))
  assert_eq(ConfigManager::get(config_manager, "telemetry.flush_interval_ms"), Some(5000))
  assert_eq(ConfigManager::get(config_manager, "telemetry.max_spans_per_trace"), Some(1000))
  
  // 从环境变量加载配置
  ConfigManager::load_from_env(config_manager, [
    ("TELEMETRY_SAMPLING_RATE", "telemetry.sampling_rate"),
    ("TELEMETRY_BATCH_SIZE", "telemetry.batch_size"),
    ("TELEMETRY_FLUSH_INTERVAL", "telemetry.flush_interval_ms")
  ])
  
  // 从配置文件加载配置
  let config_file_content = {
    "telemetry": {
      "sampling_rate": 0.2,
      "max_spans_per_trace": 2000,
      "exporters": {
        "jaeger": {
          "endpoint": "http://jaeger:14268/api/traces",
          "timeout": 5000
        },
        "prometheus": {
          "endpoint": "http://prometheus:9090/api/v1/write",
          "timeout": 3000
        }
      },
      "processors": {
        "batch": {
          "timeout": 5000,
          "max_size": 1000
        },
        "memory_limiter": {
          "limit_mib": 512
        }
      }
    },
    "logging": {
      "level": "info",
      "format": "json"
    }
  }
  
  ConfigManager::load_from_string(config_manager, config_file_content, "json")
  
  // 验证配置加载
  assert_eq(ConfigManager::get(config_manager, "telemetry.sampling_rate"), Some(0.2))
  assert_eq(ConfigManager::get(config_manager, "telemetry.batch_size"), Some(100))  // 未在文件中定义，保持默认或环境变量值
  assert_eq(ConfigManager::get(config_manager, "telemetry.max_spans_per_trace"), Some(2000))
  
  // 验证嵌套配置
  assert_eq(ConfigManager::get(config_manager, "telemetry.exporters.jaeger.endpoint"), Some("http://jaeger:14268/api/traces"))
  assert_eq(ConfigManager::get(config_manager, "telemetry.exporters.jaeger.timeout"), Some(5000))
  assert_eq(ConfigManager::get(config_manager, "telemetry.exporters.prometheus.endpoint"), Some("http://prometheus:9090/api/v1/write"))
  assert_eq(ConfigManager::get(config_manager, "telemetry.processors.batch.timeout"), Some(5000))
  assert_eq(ConfigManager::get(config_manager, "telemetry.processors.memory_limiter.limit_mib"), Some(512))
  
  // 验证不同类型的配置
  assert_eq(ConfigManager::get(config_manager, "logging.level"), Some("info"))
  assert_eq(ConfigManager::get(config_manager, "logging.format"), Some("json"))
  
  // 测试配置验证
  ConfigManager::add_validator(config_manager, "telemetry.sampling_rate", fn(value) {
    match value {
      Float(rate) => rate >= 0.0 and rate <= 1.0,
      _ => false
    }
  })
  
  ConfigManager::add_validator(config_manager, "telemetry.batch_size", fn(value) {
    match value {
      Int(size) => size > 0 and size <= 10000,
      _ => false
    }
  })
  
  ConfigManager::add_validator(config_manager, "telemetry.flush_interval_ms", fn(value) {
    match value {
      Int(interval) => interval >= 1000,
      _ => false
    }
  })
  
  // 测试有效配置更新
  let valid_update_result = ConfigManager::set(config_manager, "telemetry.sampling_rate", 0.3)
  assert_true(valid_update_result.is_ok)
  
  // 测试无效配置更新
  let invalid_sampling_result = ConfigManager::set(config_manager, "telemetry.sampling_rate", 1.5)
  assert_true(invalid_sampling_result.is_err)
  assert_eq(invalid_sampling_result.unwrap_err().message, "Invalid value for telemetry.sampling_rate")
  
  let invalid_batch_result = ConfigManager::set(config_manager, "telemetry.batch_size", -10)
  assert_true(invalid_batch_result.is_err)
  assert_eq(invalid_batch_result.unwrap_err().message, "Invalid value for telemetry.batch_size")
  
  let invalid_interval_result = ConfigManager::set(config_manager, "telemetry.flush_interval_ms", 500)
  assert_true(invalid_interval_result.is_err)
  assert_eq(invalid_interval_result.unwrap_err().message, "Invalid value for telemetry.flush_interval_ms")
  
  // 测试配置变更监听
  let mut config_changes = []
  
  ConfigManager::add_change_listener(config_manager, "telemetry.sampling_rate", fn(old_value, new_value) {
    config_changes = config_changes.push(("telemetry.sampling_rate", old_value, new_value))
  })
  
  ConfigManager::add_change_listener(config_manager, "telemetry.batch_size", fn(old_value, new_value) {
    config_changes = config_changes.push(("telemetry.batch_size", old_value, new_value))
  })
  
  // 触发配置变更
  ConfigManager::set(config_manager, "telemetry.sampling_rate", 0.4)
  ConfigManager::set(config_manager, "telemetry.batch_size", 200)
  
  // 验证配置变更监听
  assert_eq(config_changes.length(), 2)
  
  let sampling_change = config_changes.find(fn(c) { c.0 == "telemetry.sampling_rate" })
  assert_true(sampling_change.is_some)
  assert_eq(sampling_change.unwrap().1, 0.3)  // 旧值
  assert_eq(sampling_change.unwrap().2, 0.4)  // 新值
  
  let batch_change = config_changes.find(fn(c) { c.0 == "telemetry.batch_size" })
  assert_true(batch_change.is_some)
  assert_eq(batch_change.unwrap().1, 100)  // 旧值
  assert_eq(batch_change.unwrap().2, 200)  // 新值
  
  // 测试配置导出
  let exported_config = ConfigManager::export(config_manager, "json")
  assert_true(exported_config.contains("telemetry"))
  assert_true(exported_config.contains("sampling_rate"))
  assert_true(exported_config.contains("batch_size"))
  assert_true(exported_config.contains("exporters"))
  assert_true(exported_config.contains("processors"))
  
  // 测试配置导入
  let new_config_content = {
    "telemetry": {
      "sampling_rate": 0.5,
      "batch_size": 500,
      "new_feature": {
        "enabled": true,
        "threshold": 100
      }
    }
  }
  
  let import_result = ConfigManager::import_from_string(config_manager, new_config_content, "json")
  assert_true(import_result.is_ok)
  
  // 验证导入结果
  assert_eq(ConfigManager::get(config_manager, "telemetry.sampling_rate"), Some(0.5))
  assert_eq(ConfigManager::get(config_manager, "telemetry.batch_size"), Some(500))
  assert_eq(ConfigManager::get(config_manager, "telemetry.new_feature.enabled"), Some(true))
  assert_eq(ConfigManager::get(config_manager, "telemetry.new_feature.threshold"), Some(100))
  
  // 验证未覆盖的配置保持不变
  assert_eq(ConfigManager::get(config_manager, "telemetry.flush_interval_ms"), Some(5000))
  assert_eq(ConfigManager::get(config_manager, "telemetry.exporters.jaeger.endpoint"), Some("http://jaeger:14268/api/traces"))
  
  // 测试配置版本管理
  let config_version = ConfigManager::get_version(config_manager)
  assert_true(config_version > 0)
  
  // 创建配置快照
  let snapshot_id = ConfigManager::create_snapshot(config_manager, "Before major update")
  assert_true(snapshot_id.length() > 0)
  
  // 修改配置
  ConfigManager::set(config_manager, "telemetry.sampling_rate", 0.8)
  ConfigManager::set(config_manager, "telemetry.batch_size", 800)
  
  // 验证配置已修改
  assert_eq(ConfigManager::get(config_manager, "telemetry.sampling_rate"), Some(0.8))
  assert_eq(ConfigManager::get(config_manager, "telemetry.batch_size"), Some(800))
  
  // 恢复配置快照
  let restore_result = ConfigManager::restore_snapshot(config_manager, snapshot_id)
  assert_true(restore_result.is_ok)
  
  // 验证配置已恢复
  assert_eq(ConfigManager::get(config_manager, "telemetry.sampling_rate"), Some(0.5))
  assert_eq(ConfigManager::get(config_manager, "telemetry.batch_size"), Some(500))
  
  // 获取配置历史
  let config_history = ConfigManager::get_history(config_manager, "telemetry.sampling_rate")
  assert_true(config_history.length() >= 3)  // 默认 -> 0.3 -> 0.4 -> 0.5 -> 0.8 -> 0.5
  
  // 验证历史记录
  let latest_change = config_history[0]
  assert_eq(latest_change.value, 0.5)
  assert_eq(latest_change.operation, "restore")
}

// 测试9: 遥测数据导出导入测试
test "遥测数据导出导入测试" {
  // 创建数据导出导入管理器
  let data_transfer_manager = DataTransferManager::new()
  
  // 创建测试遥测数据
  let test_data = []
  let base_time = 1640995200
  
  for i in 0..=1000 {
    let data = TelemetryData::new(
      "trace-" + i.to_string(),
      "span-" + i.to_string(),
      base_time + i * 10,
      "service-" + (i % 10).to_string()
    )
    
    data.duration = 50 + (i % 500)
    data.status = if i % 50 == 0 { "error" } else { "ok" }
    
    data.add_attribute("http.method", if i % 2 == 0 { "GET" } else { "POST" })
    data.add_attribute("http.status_code", (200 + (i % 5) * 100).to_string())
    data.add_attribute("user.id", "user-" + (i % 100).to_string())
    
    test_data = test_data.push(data)
  }
  
  // 测试JSON格式导出
  let json_export_result = DataTransferManager::export_data(
    data_transfer_manager,
    test_data,
    "json",
    { include_attributes: true, compress: false }
  )
  
  assert_true(json_export_result.is_ok)
  let json_exported_data = json_export_result.unwrap()
  assert_true(json_exported_data.length() > 0)
  assert_true(json_exported_data.contains("trace-"))
  assert_true(json_exported_data.contains("span-"))
  assert_true(json_exported_data.contains("service-"))
  
  // 测试JSON格式导入
  let json_import_result = DataTransferManager::import_data(
    data_transfer_manager,
    json_exported_data,
    "json"
  )
  
  assert_true(json_import_result.is_ok)
  let json_imported_data = json_import_result.unwrap()
  assert_eq(json_imported_data.length(), test_data.length())
  
  // 验证导入数据的完整性
  let original_trace_ids = test_data.map(fn(d) { d.trace_id })
  let imported_trace_ids = json_imported_data.map(fn(d) { d.trace_id })
  
  assert_eq(original_trace_ids.sort(), imported_trace_ids.sort())
  
  let original_sample = test_data.find(fn(d) { d.trace_id == "trace-100" })
  let imported_sample = json_imported_data.find(fn(d) { d.trace_id == "trace-100" })
  
  assert_true(original_sample.is_some and imported_sample.is_some)
  assert_eq(original_sample.unwrap().service_name, imported_sample.unwrap().service_name)
  assert_eq(original_sample.unwrap().duration, imported_sample.unwrap().duration)
  assert_eq(original_sample.unwrap().status, imported_sample.unwrap().status)
  
  // 测试CSV格式导出
  let csv_export_result = DataTransferManager::export_data(
    data_transfer_manager,
    test_data,
    "csv",
    { include_attributes: true, compress: false }
  )
  
  assert_true(csv_export_result.is_ok)
  let csv_exported_data = csv_export_result.unwrap()
  assert_true(csv_exported_data.length() > 0)
  assert_true(csv_exported_data.contains("trace_id,span_id,timestamp,service_name,duration,status"))
  
  // 测试CSV格式导入
  let csv_import_result = DataTransferManager::import_data(
    data_transfer_manager,
    csv_exported_data,
    "csv"
  )
  
  assert_true(csv_import_result.is_ok)
  let csv_imported_data = csv_import_result.unwrap()
  assert_eq(csv_imported_data.length(), test_data.length())
  
  // 测试Parquet格式导出
  let parquet_export_result = DataTransferManager::export_data(
    data_transfer_manager,
    test_data,
    "parquet",
    { include_attributes: true, compress: true }
  )
  
  assert_true(parquet_export_result.is_ok)
  let parquet_exported_data = parquet_export_result.unwrap()
  assert_true(parquet_exported_data.length() > 0)
  
  // 测试Parquet格式导入
  let parquet_import_result = DataTransferManager::import_data(
    data_transfer_manager,
    parquet_exported_data,
    "parquet"
  )
  
  assert_true(parquet_import_result.is_ok)
  let parquet_imported_data = parquet_import_result.unwrap()
  assert_eq(parquet_imported_data.length(), test_data.length())
  
  // 测试压缩导出
  let compressed_export_result = DataTransferManager::export_data(
    data_transfer_manager,
    test_data,
    "json",
    { include_attributes: true, compress: true }
  )
  
  assert_true(compressed_export_result.is_ok)
  let compressed_exported_data = compressed_export_result.unwrap()
  
  // 验证压缩效果
  assert_true(compressed_exported_data.length() < json_exported_data.length())
  
  // 测试压缩导入
  let compressed_import_result = DataTransferManager::import_data(
    data_transfer_manager,
    compressed_exported_data,
    "json"
  )
  
  assert_true(compressed_import_result.is_ok)
  let compressed_imported_data = compressed_import_result.unwrap()
  assert_eq(compressed_imported_data.length(), test_data.length())
  
  // 测试增量导出
  let incremental_filter = DataTransferManager::create_time_range_filter(
    data_transfer_manager,
    base_time + 5000,  // 开始时间
    base_time + 7000   // 结束时间
  )
  
  let incremental_data = test_data.filter(incremental_filter)
  assert_true(incremental_data.length() > 0 and incremental_data.length() < test_data.length())
  
  let incremental_export_result = DataTransferManager::export_data(
    data_transfer_manager,
    incremental_data,
    "json",
    { include_attributes: true, compress: false }
  )
  
  assert_true(incremental_export_result.is_ok)
  let incremental_exported_data = incremental_export_result.unwrap()
  
  // 测试增量导入
  let incremental_import_result = DataTransferManager::import_data(
    data_transfer_manager,
    incremental_exported_data,
    "json"
  )
  
  assert_true(incremental_import_result.is_ok)
  let incremental_imported_data = incremental_import_result.unwrap()
  assert_eq(incremental_imported_data.length(), incremental_data.length())
  
  // 测试数据转换和映射
  let field_mapping = {
    "trace_id": "traceId",
    "span_id": "spanId",
    "timestamp": "@timestamp",
    "service_name": "service",
    "duration": "duration_ms"
  }
  
  let transformed_export_result = DataTransferManager::export_data_with_mapping(
    data_transfer_manager,
    test_data,
    "json",
    field_mapping,
    { include_attributes: true, compress: false }
  )
  
  assert_true(transformed_export_result.is_ok)
  let transformed_exported_data = transformed_export_result.unwrap()
  
  // 验证字段映射
  assert_true(transformed_exported_data.contains("traceId"))
  assert_true(transformed_exported_data.contains("spanId"))
  assert_true(transformed_exported_data.contains("@timestamp"))
  assert_true(transformed_exported_data.contains("service"))
  assert_true(transformed_exported_data.contains("duration_ms"))
  
  // 测试数据过滤导出
  let error_data_filter = fn(data: TelemetryData) {
    data.status == "error"
  }
  
  let error_data = test_data.filter(error_data_filter)
  assert_true(error_data.length() > 0)
  
  let filtered_export_result = DataTransferManager::export_data_with_filter(
    data_transfer_manager,
    test_data,
    "json",
    error_data_filter,
    { include_attributes: true, compress: false }
  )
  
  assert_true(filtered_export_result.is_ok)
  let filtered_exported_data = filtered_export_result.unwrap()
  
  // 测试过滤导入
  let filtered_import_result = DataTransferManager::import_data(
    data_transfer_manager,
    filtered_exported_data,
    "json"
  )
  
  assert_true(filtered_import_result.is_ok)
  let filtered_imported_data = filtered_import_result.unwrap()
  
  // 验证过滤导入只包含错误数据
  let all_error_data = filtered_imported_data.all(fn(d) { d.status == "error" })
  assert_true(all_error_data)
  
  // 测试导出导入性能
  let large_dataset = []
  for i in 0..=10000 {
    let data = TelemetryData::new(
      "large-trace-" + i.to_string(),
      "large-span-" + i.to_string(),
      base_time + i,
      "large-service-" + (i % 50).to_string()
    )
    large_dataset = large_dataset.push(data)
  }
  
  let export_start_time = Time::now()
  let large_export_result = DataTransferManager::export_data(
    data_transfer_manager,
    large_dataset,
    "parquet",
    { include_attributes: false, compress: true }
  )
  let export_duration = Time::now() - export_start_time
  
  assert_true(large_export_result.is_ok)
  assert_true(export_duration < 10000)  // 导出应在10秒内完成
  
  let large_exported_data = large_export_result.unwrap()
  
  let import_start_time = Time::now()
  let large_import_result = DataTransferManager::import_data(
    data_transfer_manager,
    large_exported_data,
    "parquet"
  )
  let import_duration = Time::now() - import_start_time
  
  assert_true(large_import_result.is_ok)
  assert_true(import_duration < 15000)  // 导入应在15秒内完成
  
  let large_imported_data = large_import_result.unwrap()
  assert_eq(large_imported_data.length(), large_dataset.length())
  
  // 生成数据传输报告
  let transfer_report = DataTransferManager::generate_transfer_report(data_transfer_manager, [
    { format: "json", operation: "export", records: test_data.length(), size_bytes: json_exported_data.length(), duration_ms: 100 },
    { format: "json", operation: "import", records: json_imported_data.length(), size_bytes: json_exported_data.length(), duration_ms: 150 },
    { format: "csv", operation: "export", records: test_data.length(), size_bytes: csv_exported_data.length(), duration_ms: 80 },
    { format: "csv", operation: "import", records: csv_imported_data.length(), size_bytes: csv_exported_data.length(), duration_ms: 120 },
    { format: "parquet", operation: "export", records: test_data.length(), size_bytes: parquet_exported_data.length(), duration_ms: 60 },
    { format: "parquet", operation: "import", records: parquet_imported_data.length(), size_bytes: parquet_exported_data.length(), duration_ms: 90 },
    { format: "json", operation: "export", records: test_data.length(), size_bytes: compressed_exported_data.length(), duration_ms: 120 },
    { format: "json", operation: "import", records: compressed_imported_data.length(), size_bytes: compressed_exported_data.length(), duration_ms: 180 },
    { format: "parquet", operation: "export", records: large_dataset.length(), size_bytes: large_exported_data.length(), duration_ms: export_duration },
    { format: "parquet", operation: "import", records: large_imported_data.length(), size_bytes: large_exported_data.length(), duration_ms: import_duration }
  ])
  
  // 验证数据传输报告
  assert_true(transfer_report.operations.length() == 10)
  assert_true(transfer_report.summary.total_records_exported > 0)
  assert_true(transfer_report.summary.total_records_imported > 0)
  assert_true(transfer_report.summary.total_bytes_transferred > 0)
  
  // 验证格式性能比较
  let json_performance = transfer_report.format_performance.get("json")
  let csv_performance = transfer_report.format_performance.get("csv")
  let parquet_performance = transfer_report.format_performance.get("parquet")
  
  assert_true(json_performance.is_some)
  assert_true(csv_performance.is_some)
  assert_true(parquet_performance.is_some)
  
  // 验证Parquet格式在压缩率方面的优势
  assert_true(parquet_performance.unwrap().avg_compression_ratio > json_performance.unwrap().avg_compression_ratio)
}