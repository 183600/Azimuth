// Azimuth New Comprehensive Test Suite
// 新的综合测试套件，覆盖多种遥测功能场景

test "遥测数据批处理和聚合" {
  // 测试遥测数据的批处理和聚合功能
  let batch_processor = @azimuth.BatchProcessor::new({
    "batch_size": 100,
    "flush_interval_ms": 5000,
    "max_batch_size": 500,
    "compression_enabled": true
  })
  
  let telemetry_spans = []
  
  // 创建多个遥测span用于批处理
  for i in 0..=49 {
    let span = @azimuth.Span {
      context: @azimuth.SpanContext {
        trace_id: "trace-" + i.to_string(),
        span_id: "span-" + i.to_string(),
        sampled: true,
        trace_state: ""
      },
      parent_span_id: None,
      operation_name: "batch.operation." + i.to_string(),
      start_time: 1640995200000L + (i as Int64) * 1000L,
      end_time: Some(1640995200100L + (i as Int64) * 1000L),
      status: @azimuth.SpanStatus::Ok,
      attributes: [
        ("batch.index", @azimuth.IntValue(i)),
        ("operation.type", @azimuth.StringValue("batch_test"))
      ],
      events: []
    }
    telemetry_spans = telemetry_spans.push(span)
  }
  
  // 测试批处理添加
  for span in telemetry_spans {
    @azimuth.BatchProcessor::add_span(batch_processor, span)
  }
  
  // 验证批处理状态
  let batch_status = @azimuth.BatchProcessor::get_status(batch_processor)
  assert_eq(batch_status.pending_count, 50)
  assert_eq(batch_status.processed_count, 0)
  
  // 测试手动刷新批处理
  let batch_result = @azimuth.BatchProcessor::flush(batch_processor)
  assert_true(batch_result.is_success)
  assert_eq(batch_result.processed_spans.length(), 50)
  
  // 验证批处理后的状态
  let post_flush_status = @azimuth.BatchProcessor::get_status(batch_processor)
  assert_eq(post_flush_status.pending_count, 0)
  assert_eq(post_flush_status.processed_count, 50)
  
  // 测试批处理聚合功能
  let aggregation_result = @azimuth.BatchProcessor::aggregate_metrics(batch_processor)
  assert_true(aggregation_result.contains("batch.operation"))
  assert_true(aggregation_result.contains("total_spans: 50"))
  assert_true(aggregation_result.contains("average_duration"))
}

test "采样策略和动态调整" {
  // 测试采样策略和动态调整功能
  let sampling_config = @azimuth.SamplingConfig {
    sampling_type: @azimuth.SamplingType::Adaptive,
    base_probability: 0.1,
    max_probability: 0.5,
    min_probability: 0.01,
    adjustment_threshold: 1000,
    adjustment_factor: 1.5
  }
  
  let sampling_manager = @azimuth.SamplingManager::new(sampling_config)
  
  // 测试基础采样决策
  let trace_id_high = "high-traffic-trace-12345"
  let trace_id_low = "low-traffic-trace-67890"
  
  let initial_sampling_decision = @azimuth.SamplingManager::should_sample(
    sampling_manager, 
    trace_id_high, 
    "high.traffic.operation"
  )
  assert_true(initial_sampling_decision.sampled)
  assert_true(initial_sampling_decision.probability >= 0.1)
  
  // 模拟高流量场景
  for i in 0..=1999 {
    @azimuth.SamplingManager::record_trace(
      sampling_manager, 
      "trace-" + i.to_string(), 
      "high.traffic.operation"
    )
  }
  
  // 测试高流量下的采样调整
  let high_volume_decision = @azimuth.SamplingManager::should_sample(
    sampling_manager, 
    trace_id_high, 
    "high.traffic.operation"
  )
  assert_true(high_volume_decision.probability < initial_sampling_decision.probability)
  
  // 测试低流量场景
  let low_volume_decision = @azimuth.SamplingManager::should_sample(
    sampling_manager, 
    trace_id_low, 
    "low.traffic.operation"
  )
  assert_true(low_volume_decision.probability >= sampling_config.min_probability)
  
  // 测试基于操作类型的采样策略
  let critical_operation_decision = @azimuth.SamplingManager::should_sample(
    sampling_manager, 
    "critical-trace-123", 
    "critical.business.operation"
  )
  assert_true(critical_operation_decision.sampled)
  assert_eq(critical_operation_decision.probability, 1.0) // 关键操作始终采样
  
  // 测试采样统计
  let sampling_stats = @azimuth.SamplingManager::get_statistics(sampling_manager)
  assert_true(sampling_stats.total_traces >= 2000)
  assert_true(sampling_stats.sampled_traces > 0)
  assert_true(sampling_stats.sampling_rate > 0.0)
  assert_true(sampling_stats.sampling_rate <= 1.0)
}

test "遥测数据导出和格式转换" {
  // 测试遥测数据导出和格式转换功能
  let exporter_config = @azimuth.ExporterConfig {
    exporter_type: @azimuth.ExporterType::MultiFormat,
    endpoint: "https://otel-collector.example.com:4317",
    headers: [("Authorization", "Bearer export-token-123")],
    timeout_ms: 10000,
    batch_size: 100,
    export_formats: ["json", "protobuf", "avro"],
    compression: "gzip"
  }
  
  let data_exporter = @azimuth.DataExporter::new(exporter_config)
  
  // 创建测试遥测数据
  let test_trace = @azimuth.TraceData {
    trace_id: "export-test-trace-12345",
    spans: [
      @azimuth.Span {
        context: @azimuth.SpanContext {
          trace_id: "export-test-trace-12345",
          span_id: "export-span-1",
          sampled: true,
          trace_state: "sampling.decision=true"
        },
        parent_span_id: None,
        operation_name: "http.request",
        start_time: 1640995200000L,
        end_time: Some(1640995200200L),
        status: @azimuth.SpanStatus::Ok,
        attributes: [
          ("http.method", @azimuth.StringValue("GET")),
          ("http.url", @azimuth.StringValue("/api/export/test")),
          ("http.status_code", @azimuth.IntValue(200))
        ],
        events: [
          @azimuth.SpanEvent {
            name: "http.response.headers",
            timestamp: 1640995200150L,
            attributes: [("content-type", @azimuth.StringValue("application/json"))]
          }
        ]
      }
    ],
    metrics: [
      @azimuth.MetricData {
        name: "http.request.duration",
        value: 200.0,
        unit: "ms",
        timestamp: 1640995200200L,
        attributes: [("endpoint", @azimuth.StringValue("/api/export/test"))]
      }
    ],
    logs: [
      @azimuth.LogData {
        timestamp: 1640995200100L,
        severity: @azimuth.LogSeverity::Info,
        message: "Export test request processed",
        attributes: [("trace.id", @azimuth.StringValue("export-test-trace-12345"))]
      }
    ]
  }
  
  // 测试JSON格式导出
  let json_export_result = @azimuth.DataExporter::export_as_json(data_exporter, test_trace)
  assert_true(json_export_result.is_success)
  assert_true(json_export_result.data.contains("export-test-trace-12345"))
  assert_true(json_export_result.data.contains("http.request"))
  assert_true(json_export_result.data.contains("http.request.duration"))
  
  // 测试Protobuf格式导出
  let protobuf_export_result = @azimuth.DataExporter::export_as_protobuf(data_exporter, test_trace)
  assert_true(protobuf_export_result.is_success)
  assert_true(protobuf_export_result.data.length() > 0)
  
  // 测试Avro格式导出
  let avro_export_result = @azimuth.DataExporter::export_as_avro(data_exporter, test_trace)
  assert_true(avro_export_result.is_success)
  assert_true(avro_export_result.data.length() > 0)
  
  // 测试压缩导出
  let compressed_export_result = @azimuth.DataExporter::export_compressed(data_exporter, test_trace, "gzip")
  assert_true(compressed_export_result.is_success)
  assert_true(compressed_export_result.data.length() < json_export_result.data.length())
  
  // 测试批量导出
  let batch_traces = [test_trace, test_trace, test_trace]
  let batch_export_result = @azimuth.DataExporter::export_batch(data_exporter, batch_traces)
  assert_true(batch_export_result.is_success)
  assert_eq(batch_export_result.exported_count, 3)
}

test "服务网格遥测集成" {
  // 测试服务网格遥测集成功能
  let mesh_config = @azimuth.MeshConfig {
    mesh_type: @azimuth.MeshType::Istio,
    service_name: "payment-service",
    service_version: "2.1.0",
    namespace: "production",
    mesh_telemetry_enabled: true,
    inbound_interception: true,
    outbound_interception: true
  }
  
  let mesh_telemetry = @azimuth.MeshTelemetry::new(mesh_config)
  
  // 测试服务网格入站遥测
  let inbound_request = @azimuth.MeshRequest {
    request_id: "req-inbound-12345",
    source_service: "api-gateway",
    source_namespace: "gateway",
    destination_service: "payment-service",
    destination_namespace: "production",
    method: "POST",
    path: "/api/payments/process",
    headers: [
      ("x-request-id", "req-inbound-12345"),
      ("x-b3-traceid", "trace-mesh-12345"),
      ("x-b3-spanid", "span-mesh-12345")
    ],
    timestamp: 1640995200000L
  }
  
  let inbound_telemetry = @azimuth.MeshTelemetry::process_inbound(mesh_telemetry, inbound_request)
  assert_true(inbound_telemetry.span_created)
  assert_eq(inbound_telemetry.trace_id, "trace-mesh-12345")
  assert_eq(inbound_telemetry.parent_span_id, "span-mesh-12345")
  assert_true(inbound_telemetry.attributes.contains(("mesh.direction", @azimuth.StringValue("inbound"))))
  
  // 测试服务网格出站遥测
  let outbound_request = @azimuth.MeshRequest {
    request_id: "req-outbound-67890",
    source_service: "payment-service",
    source_namespace: "production",
    destination_service: "user-service",
    destination_namespace: "user",
    method: "GET",
    path: "/api/users/12345",
    headers: [
      ("x-request-id", "req-outbound-67890"),
      ("x-b3-traceid", "trace-mesh-12345"),
      ("x-b3-spanid", "span-payment-67890")
    ],
    timestamp: 1640995200500L
  }
  
  let outbound_telemetry = @azimuth.MeshTelemetry::process_outbound(mesh_telemetry, outbound_request)
  assert_true(outbound_telemetry.span_created)
  assert_eq(outbound_telemetry.trace_id, "trace-mesh-12345")
  assert_true(outbound_telemetry.attributes.contains(("mesh.direction", @azimuth.StringValue("outbound"))))
  
  // 测试服务网格度量收集
  let mesh_metrics = @azimuth.MeshTelemetry::collect_metrics(mesh_telemetry)
  assert_true(mesh_metrics.contains("mesh.request.total"))
  assert_true(mesh_metrics.contains("mesh.request.duration"))
  assert_true(mesh_metrics.contains("mesh.request.success"))
  assert_true(mesh_metrics.contains("mesh.request.error"))
  
  // 测试服务网格拓扑映射
  let topology = @azimuth.MeshTelemetry::build_service_topology(mesh_telemetry)
  assert_true(topology.services.contains("payment-service"))
  assert_true(topology.services.contains("api-gateway"))
  assert_true(topology.services.contains("user-service"))
  assert_true(topology.connections.contains(("api-gateway", "payment-service")))
  assert_true(topology.connections.contains(("payment-service", "user-service")))
}

test "自定义度量仪表盘" {
  // 测试自定义度量仪表盘功能
  let dashboard_config = @azimuth.DashboardConfig {
    name: "Payment Service Dashboard",
    description: "Real-time metrics for payment service",
    refresh_interval_ms: 5000,
    time_range_ms: 3600000, // 1小时
    panels: [
      @azimuth.PanelConfig {
        title: "Request Rate",
        type: @azimuth.PanelType::LineChart,
        metrics: ["http.requests.total"],
        aggregation: @azimuth.AggregationType::Rate,
        group_by: ["endpoint"]
      },
      @azimuth.PanelConfig {
        title: "Response Time Distribution",
        type: @azimuth.PanelType::Histogram,
        metrics: ["http.request.duration"],
        aggregation: @azimuth.AggregationType::Percentile,
        percentiles: [50, 90, 95, 99]
      },
      @azimuth.PanelConfig {
        title: "Error Rate",
        type: @azimuth.PanelType::SingleStat,
        metrics: ["http.requests.error"],
        aggregation: @azimuth.AggregationType::Rate,
        thresholds: [(0.01, "green"), (0.05, "yellow"), (0.1, "red")]
      }
    ]
  }
  
  let dashboard = @azimuth.CustomDashboard::new(dashboard_config)
  
  // 生成测试度量数据
  let test_metrics = []
  let current_time = 1640995200000L
  
  for i in 0..=59 {
    let timestamp = current_time + (i as Int64) * 60000L // 每分钟一个数据点
    let request_metric = @azimuth.MetricPoint {
      name: "http.requests.total",
      value: 100.0 + (i as Float) * 2.0,
      timestamp: timestamp,
      tags: [("endpoint", "/api/payments/process"), ("status", "200")]
    }
    let latency_metric = @azimuth.MetricPoint {
      name: "http.request.duration",
      value: 150.0 + (i as Float) * 1.5,
      timestamp: timestamp,
      tags: [("endpoint", "/api/payments/process"), ("percentile", "p95")]
    }
    let error_metric = @azimuth.MetricPoint {
      name: "http.requests.error",
      value: if i % 10 == 0 { 5.0 } else { 0.0 },
      timestamp: timestamp,
      tags: [("endpoint", "/api/payments/process"), ("error_type", "timeout")]
    }
    
    test_metrics = test_metrics.push(request_metric)
    test_metrics = test_metrics.push(latency_metric)
    test_metrics = test_metrics.push(error_metric)
  }
  
  // 测试仪表盘数据更新
  for metric in test_metrics {
    @azimuth.CustomDashboard::add_metric(dashboard, metric)
  }
  
  // 测试面板数据生成
  let request_rate_panel = @azimuth.CustomDashboard::get_panel_data(dashboard, "Request Rate")
  assert_true(request_rate_panel.is_success)
  assert_true(request_rate_panel.data_points.length() > 0)
  assert_true(request_rate_panel.data_points[0].value >= 100.0)
  
  let latency_panel = @azimuth.CustomDashboard::get_panel_data(dashboard, "Response Time Distribution")
  assert_true(latency_panel.is_success)
  assert_true(latency_panel.data_points.length() > 0)
  assert_true(latency_panel.data_points[0].value >= 150.0)
  
  let error_rate_panel = @azimuth.CustomDashboard::get_panel_data(dashboard, "Error Rate")
  assert_true(error_rate_panel.is_success)
  assert_true(error_rate_panel.current_value >= 0.0)
  
  // 测试仪表盘导出
  let dashboard_export = @azimuth.CustomDashboard::export_dashboard(dashboard, "json")
  assert_true(dashboard_export.is_success)
  assert_true(dashboard_export.data.contains("Payment Service Dashboard"))
  assert_true(dashboard_export.data.contains("Request Rate"))
  assert_true(dashboard_export.data.contains("Response Time Distribution"))
  
  // 测试实时更新功能
  let realtime_update = @azimuth.CustomDashboard::enable_realtime_updates(dashboard)
  assert_true(realtime_update.enabled)
  assert_eq(realtime_update.refresh_interval_ms, 5000)
  
  // 添加新的实时数据点
  let new_metric = @azimuth.MetricPoint {
    name: "http.requests.total",
    value: 250.0,
    timestamp: current_time + 3600000L,
    tags: [("endpoint", "/api/payments/process"), ("status", "200")]
  }
  
  @azimuth.CustomDashboard::add_metric(dashboard, new_metric)
  let updated_panel = @azimuth.CustomDashboard::get_panel_data(dashboard, "Request Rate")
  assert_true(updated_panel.data_points.length() > request_rate_panel.data_points.length())
}

test "遥测数据生命周期管理" {
  // 测试遥测数据生命周期管理功能
  let lifecycle_config = @azimuth.LifecycleConfig {
    retention_period_days: 30,
    archival_period_days: 90,
    cleanup_interval_hours: 24,
    hot_storage_size_mb: 1024,
    cold_storage_size_mb: 10240,
    compression_enabled: true,
    encryption_enabled: true
  }
  
  let lifecycle_manager = @azimuth.LifecycleManager::new(lifecycle_config)
  
  // 创建不同时间点的测试数据
  let current_time = 1640995200000L // 2022-01-01
  let old_data_time = current_time - (40 * 24 * 60 * 60 * 1000) // 40天前
  let very_old_data_time = current_time - (100 * 24 * 60 * 60 * 1000) // 100天前
  
  let recent_telemetry = @azimuth.TelemetryData {
    timestamp: current_time,
    trace_id: "recent-trace-123",
    span_id: "recent-span-123",
    operation_name: "recent.operation",
    data_size: 1024,
    access_count: 10,
    last_accessed: current_time
  }
  
  let old_telemetry = @azimuth.TelemetryData {
    timestamp: old_data_time,
    trace_id: "old-trace-456",
    span_id: "old-span-456",
    operation_name: "old.operation",
    data_size: 2048,
    access_count: 2,
    last_accessed: old_data_time
  }
  
  let very_old_telemetry = @azimuth.TelemetryData {
    timestamp: very_old_data_time,
    trace_id: "very-old-trace-789",
    span_id: "very-old-span-789",
    operation_name: "very.old.operation",
    data_size: 4096,
    access_count: 0,
    last_accessed: very_old_data_time
  }
  
  // 添加测试数据到生命周期管理器
  @azimuth.LifecycleManager::add_data(lifecycle_manager, recent_telemetry)
  @azimuth.LifecycleManager::add_data(lifecycle_manager, old_telemetry)
  @azimuth.LifecycleManager::add_data(lifecycle_manager, very_old_telemetry)
  
  // 测试数据分类
  let data_classification = @azimuth.LifecycleManager::classify_data(lifecycle_manager)
  assert_eq(data_classification.hot_data.length(), 1) // 只有最近的数据在热存储
  assert_eq(data_classification.cold_data.length(), 1) // 旧数据在冷存储
  assert_eq(data_classification.archival_data.length(), 1) // 很旧的数据准备归档
  
  // 测试数据迁移
  let migration_result = @azimuth.LifecycleManager::migrate_data(lifecycle_manager)
  assert_true(migration_result.success)
  assert_eq(migration_result.migrated_to_cold, 1)
  assert_eq(migration_result.migrated_to_archive, 1)
  
  // 测试数据清理
  let cleanup_result = @azimuth.LifecycleManager::cleanup_expired_data(lifecycle_manager)
  assert_true(cleanup_result.success)
  assert_eq(cleanup_result.deleted_count, 0) // 100天的数据还未超过90天归档期
  
  // 模拟时间推进，测试过期数据清理
  let future_time = current_time + (120 * 24 * 60 * 60 * 1000) // 120天后
  @azimuth.LifecycleManager::set_current_time(lifecycle_manager, future_time)
  
  let future_cleanup_result = @azimuth.LifecycleManager::cleanup_expired_data(lifecycle_manager)
  assert_true(future_cleanup_result.success)
  assert_eq(future_cleanup_result.deleted_count, 1) // 100天的数据现在应该被清理
  
  // 测试存储使用情况
  let storage_stats = @azimuth.LifecycleManager::get_storage_statistics(lifecycle_manager)
  assert_true(storage_stats.hot_storage_used_mb > 0)
  assert_true(storage_stats.cold_storage_used_mb > 0)
  assert_true(storage_stats.archival_storage_used_mb > 0)
  assert_true(storage_stats.total_storage_saved_mb > 0) // 压缩节省的空间
}

test "多租户遥测隔离" {
  // 测试多租户遥测隔离功能
  let tenant_config = @azimuth.TenantConfig {
    isolation_level: @azimuth.IsolationLevel::Strict,
    max_tenants: 100,
    default_quota_mb: 1024,
    sampling_rate_per_tenant: 0.1,
    cross_tenant_access: false
  }
  
  let tenant_manager = @azimuth.TenantManager::new(tenant_config)
  
  // 创建多个租户
  let tenant_a = @azimuth.TenantManager::create_tenant(tenant_manager, "tenant-a", "Customer A")
  let tenant_b = @azimuth.TenantManager::create_tenant(tenant_manager, "tenant-b", "Customer B")
  let tenant_c = @azimuth.TenantManager::create_tenant(tenant_manager, "tenant-c", "Customer C")
  
  // 验证租户创建
  assert_true(tenant_a.is_success)
  assert_true(tenant_b.is_success)
  assert_true(tenant_c.is_success)
  assert_eq(tenant_a.tenant_id, "tenant-a")
  assert_eq(tenant_b.tenant_id, "tenant-b")
  assert_eq(tenant_c.tenant_id, "tenant-c")
  
  // 为每个租户添加遥测数据
  let tenant_a_trace = @azimuth.TenantTraceData {
    tenant_id: "tenant-a",
    trace_id: "trace-a-123",
    spans: [
      @azimuth.TenantSpan {
        tenant_id: "tenant-a",
        span_id: "span-a-123",
        operation_name: "tenant.a.operation",
        attributes: [("tenant.data", @azimuth.StringValue("sensitive-a"))]
      }
    ]
  }
  
  let tenant_b_trace = @azimuth.TenantTraceData {
    tenant_id: "tenant-b",
    trace_id: "trace-b-456",
    spans: [
      @azimuth.TenantSpan {
        tenant_id: "tenant-b",
        span_id: "span-b-456",
        operation_name: "tenant.b.operation",
        attributes: [("tenant.data", @azimuth.StringValue("sensitive-b"))]
      }
    ]
  }
  
  // 添加租户数据
  @azimuth.TenantManager::add_trace_data(tenant_manager, tenant_a_trace)
  @azimuth.TenantManager::add_trace_data(tenant_manager, tenant_b_trace)
  
  // 测试租户数据隔离查询
  let tenant_a_data = @azimuth.TenantManager::get_tenant_data(tenant_manager, "tenant-a")
  assert_true(tenant_a_data.is_success)
  assert_eq(tenant_a_data.traces.length(), 1)
  assert_eq(tenant_a_data.traces[0].trace_id, "trace-a-123")
  assert_false(tenant_a_data.traces[0].spans[0].attributes.contains(("tenant.data", @azimuth.StringValue("sensitive-b"))))
  
  let tenant_b_data = @azimuth.TenantManager::get_tenant_data(tenant_manager, "tenant-b")
  assert_true(tenant_b_data.is_success)
  assert_eq(tenant_b_data.traces.length(), 1)
  assert_eq(tenant_b_data.traces[0].trace_id, "trace-b-456")
  assert_false(tenant_b_data.traces[0].spans[0].attributes.contains(("tenant.data", @azimuth.StringValue("sensitive-a"))))
  
  // 测试跨租户访问限制
  let cross_tenant_access = @azimuth.TenantManager::get_tenant_data(tenant_manager, "tenant-a", "tenant-b")
  assert_false(cross_tenant_access.is_success)
  assert_true(cross_tenant_access.error.contains("cross-tenant access denied"))
  
  // 测试租户配额管理
  let tenant_a_quota = @azimuth.TenantManager::get_quota_usage(tenant_manager, "tenant-a")
  assert_true(tenant_a_quota.used_mb > 0)
  assert_true(tenant_a_quota.used_mb <= tenant_config.default_quota_mb)
  
  // 测试租户采样隔离
  let tenant_a_sampling = @azimuth.TenantManager::should_sample(tenant_manager, "tenant-a", "trace-a-789")
  let tenant_b_sampling = @azimuth.TenantManager::should_sample(tenant_manager, "tenant-b", "trace-b-789")
  
  assert_true(tenant_a_sampling.sampled)
  assert_true(tenant_b_sampling.sampled)
  assert_eq(tenant_a_sampling.sampling_rate, tenant_config.sampling_rate_per_tenant)
  assert_eq(tenant_b_sampling.sampling_rate, tenant_config.sampling_rate_per_tenant)
  
  // 测试租户统计信息
  let tenant_stats = @azimuth.TenantManager::get_tenant_statistics(tenant_manager)
  assert_eq(tenant_stats.total_tenants, 3)
  assert_eq(tenant_stats.active_tenants, 3)
  assert_true(tenant_stats.total_storage_used_mb > 0)
  assert_true(tenant_stats.quota_utilization > 0.0)
}

test "遥测数据压缩传输" {
  // 测试遥测数据压缩传输功能
  let compression_config = @azimuth.CompressionConfig {
    algorithm: @azimuth.CompressionAlgorithm::Gzip,
    compression_level: 6,
    min_size_threshold: 1024,
    enable_streaming: true,
    buffer_size: 8192
  }
  
  let compression_manager = @azimuth.CompressionManager::new(compression_config)
  
  // 创建大型遥测数据集
  let large_telemetry_data = @azimuth.TelemetryBatch {
    batch_id: "batch-compression-test",
    timestamp: 1640995200000L,
    traces: []
  }
  
  // 生成大量span数据
  for i in 0..=999 {
    let span = @azimuth.Span {
      context: @azimuth.SpanContext {
        trace_id: "compression-trace-" + i.to_string(),
        span_id: "compression-span-" + i.to_string(),
        sampled: true,
        trace_state: ""
      },
      parent_span_id: None,
      operation_name: "compression.test.operation." + i.to_string(),
      start_time: 1640995200000L + (i as Int64) * 100L,
      end_time: Some(1640995200100L + (i as Int64) * 100L),
      status: @azimuth.SpanStatus::Ok,
      attributes: [
        ("test.index", @azimuth.IntValue(i)),
        ("test.data", @azimuth.StringValue("large test data string for compression testing " + i.to_string())),
        ("test.payload", @azimuth.StringValue("x".repeat(100))) // 100字符的载荷
      ],
      events: [
        @azimuth.SpanEvent {
          name: "compression.test.event",
          timestamp: 1640995200050L + (i as Int64) * 100L,
          attributes: [("event.data", @azimuth.StringValue("event data " + i.to_string()))]
        }
      ]
    }
    large_telemetry_data.traces = large_telemetry_data.traces.push(span)
  }
  
  // 序列化数据
  let serialized_data = @azimuth.TelemetrySerializer::serialize(large_telemetry_data)
  assert_true(serialized_data.length() > 1024) // 确保数据足够大以触发压缩
  
  // 测试数据压缩
  let compression_result = @azimuth.CompressionManager::compress(compression_manager, serialized_data)
  assert_true(compression_result.is_success)
  assert_true(compression_result.compressed_data.length() < serialized_data.length())
  assert_true(compression_result.compression_ratio > 0.0)
  assert_true(compression_result.compression_ratio < 1.0)
  
  // 测试数据解压缩
  let decompression_result = @azimuth.CompressionManager::decompress(
    compression_manager, 
    compression_result.compressed_data
  )
  assert_true(decompression_result.is_success)
  assert_eq(decompression_result.decompressed_data.length(), serialized_data.length())
  assert_eq(decompression_result.decompressed_data, serialized_data)
  
  // 测试流式压缩
  let stream_data = serialized_data.substring(0, serialized_data.length() / 2)
  let streaming_compression = @azimuth.CompressionManager::compress_stream(compression_manager, stream_data)
  assert_true(streaming_compression.is_success)
  
  // 继续添加流数据
  let remaining_data = serialized_data.substring(serialized_data.length() / 2)
  let continue_streaming = @azimuth.CompressionManager::continue_stream(
    compression_manager, 
    streaming_compression.stream_id, 
    remaining_data
  )
  assert_true(continue_streaming.is_success)
  
  // 完成流式压缩
  let finalize_streaming = @azimuth.CompressionManager::finalize_stream(
    compression_manager, 
    streaming_compression.stream_id
  )
  assert_true(finalize_streaming.is_success)
  assert_true(finalize_streaming.compressed_data.length() > 0)
  
  // 测试不同压缩算法
  let lz4_config = @azimuth.CompressionConfig {
    algorithm: @azimuth.CompressionAlgorithm::LZ4,
    compression_level: 4,
    min_size_threshold: 1024,
    enable_streaming: false,
    buffer_size: 8192
  }
  
  let lz4_compression = @azimuth.CompressionManager::compress_with_algorithm(
    compression_manager, 
    serialized_data, 
    lz4_config
  )
  assert_true(lz4_compression.is_success)
  
  // 比较压缩算法性能
  let gzip_time = compression_result.compression_time_ms
  let lz4_time = lz4_compression.compression_time_ms
  let gzip_ratio = compression_result.compression_ratio
  let lz4_ratio = lz4_compression.compression_ratio
  
  assert_true(gzip_ratio > 0.0 && lz4_ratio > 0.0)
  assert_true(gzip_time > 0 && lz4_time > 0)
  
  // 测试压缩统计
  let compression_stats = @azimuth.CompressionManager::get_statistics(compression_manager)
  assert_true(compression_stats.total_compressions >= 3)
  assert_true(compression_stats.total_bytes_original > 0)
  assert_true(compression_stats.total_bytes_compressed > 0)
  assert_true(compression_stats.average_compression_ratio > 0.0)
  assert_true(compression_stats.average_compression_ratio < 1.0)
}

test "实时流处理遥测" {
  // 测试实时流处理遥测功能
  let stream_config = @azimuth.StreamConfig {
    stream_name: "telemetry-realtime-stream",
    buffer_size: 10000,
    batch_size: 100,
    flush_interval_ms: 1000,
    window_size_ms: 60000, // 1分钟窗口
    aggregation_enabled: true,
    alerting_enabled: true
  }
  
  let stream_processor = @azimuth.StreamProcessor::new(stream_config)
  
  // 创建实时流处理规则
  let rate_alert_rule = @azimuth.AlertRule {
    name: "high_error_rate",
    condition: "error_rate > 0.1",
    window_size_ms: 300000, // 5分钟窗口
    severity: @azimuth.AlertSeverity::Warning,
    threshold: 0.1,
    action: @azimuth.AlertAction::Notify
  }
  
  let latency_rule = @azimuth.AlertRule {
    name: "high_latency",
    condition: "p95_latency > 1000",
    window_size_ms: 300000,
    severity: @azimuth.AlertSeverity::Critical,
    threshold: 1000.0,
    action: @azimuth.AlertAction::Escalate
  }
  
  @azimuth.StreamProcessor::add_alert_rule(stream_processor, rate_alert_rule)
  @azimuth.StreamProcessor::add_alert_rule(stream_processor, latency_rule)
  
  // 生成实时遥测数据流
  let stream_events = []
  let base_time = 1640995200000L
  
  for i in 0..=999 {
    let timestamp = base_time + (i as Int64) * 100L // 每100ms一个事件
    
    // 大部分正常请求
    let status_code = if i % 50 == 0 { 500 } else { 200 } // 2%错误率
    let latency = if i % 100 == 0 { 1200.0 } else { 150.0 } // 偶尔高延迟
    
    let telemetry_event = @azimuth.TelemetryEvent {
      event_id: "event-" + i.to_string(),
      timestamp: timestamp,
      trace_id: "stream-trace-" + i.to_string(),
      span_id: "stream-span-" + i.to_string(),
      operation_name: "stream.processing.operation",
      attributes: [
        ("http.method", @azimuth.StringValue("GET")),
        ("http.status_code", @azimuth.IntValue(status_code)),
        ("latency_ms", @azimuth.FloatValue(latency)),
        ("service.name", @azimuth.StringValue("stream-service"))
      ]
    }
    
    stream_events = stream_events.push(telemetry_event)
  }
  
  // 处理实时数据流
  let processing_results = []
  for event in stream_events {
    let result = @azimuth.StreamProcessor::process_event(stream_processor, event)
    processing_results = processing_results.push(result)
  }
  
  // 验证流处理结果
  assert_eq(processing_results.length(), 1000)
  assert_true(processing_results.filter(fn(r) { r.processed }).length() > 0)
  
  // 测试时间窗口聚合
  let window_aggregations = @azimuth.StreamProcessor::get_window_aggregations(stream_processor)
  assert_true(window_aggregations.length() > 0)
  
  let latest_window = window_aggregations[window_aggregations.length() - 1]
  assert_true(latest_window.metrics.contains("request_count"))
  assert_true(latest_window.metrics.contains("error_rate"))
  assert_true(latest_window.metrics.contains("avg_latency"))
  assert_true(latest_window.metrics.contains("p95_latency"))
  
  // 验证聚合计算
  assert_eq(latest_window.metrics["request_count"], 1000.0)
  assert_eq(latest_window.metrics["error_rate"], 0.02) // 2%错误率
  assert_true(latest_window.metrics["p95_latency"] >= 1000.0) // 应该检测到高延迟
  
  // 测试告警触发
  let triggered_alerts = @azimuth.StreamProcessor::get_triggered_alerts(stream_processor)
  assert_true(triggered_alerts.length() > 0)
  
  let high_latency_alert = triggered_alerts.filter(fn(alert) { alert.rule_name == "high_latency" })
  assert_true(high_latency_alert.length() > 0)
  assert_eq(high_latency_alert[0].severity, @azimuth.AlertSeverity::Critical)
  
  // 测试实时指标查询
  let real_time_metrics = @azimuth.StreamProcessor::get_real_time_metrics(stream_processor)
  assert_true(real_time_metrics.contains("current_qps"))
  assert_true(real_time_metrics.contains("current_error_rate"))
  assert_true(real_time_metrics.contains("current_p95_latency"))
  
  // 测试流处理状态
  let stream_status = @azimuth.StreamProcessor::get_status(stream_processor)
  assert_true(stream_status.is_active)
  assert_eq(stream_status.events_processed, 1000)
  assert_eq(stream_status.events_per_second, stream_status.events_processed as Float / (stream_status.uptime_ms as Float / 1000.0))
  
  // 测试动态规则更新
  let updated_rate_rule = @azimuth.AlertRule {
    name: "high_error_rate",
    condition: "error_rate > 0.05", // 降低阈值
    window_size_ms: 300000,
    severity: @azimuth.AlertSeverity::Warning,
    threshold: 0.05,
    action: @azimuth.AlertAction::Notify
  }
  
  let rule_update_result = @azimuth.StreamProcessor::update_alert_rule(stream_processor, updated_rate_rule)
  assert_true(rule_update_result.is_success)
  
  // 生成更多事件以测试更新的规则
  let additional_events = []
  for i in 1000..=1099 {
    let timestamp = base_time + (i as Int64) * 100L
    let telemetry_event = @azimuth.TelemetryEvent {
      event_id: "event-" + i.to_string(),
      timestamp: timestamp,
      trace_id: "stream-trace-" + i.to_string(),
      span_id: "stream-span-" + i.to_string(),
      operation_name: "stream.processing.operation",
      attributes: [
        ("http.method", @azimuth.StringValue("GET")),
        ("http.status_code", @azimuth.IntValue(500)), // 全部错误请求
        ("latency_ms", @azimuth.FloatValue(200.0)),
        ("service.name", @azimuth.StringValue("stream-service"))
      ]
    }
    additional_events = additional_events.push(telemetry_event)
  }
  
  for event in additional_events {
    @azimuth.StreamProcessor::process_event(stream_processor, event)
  }
  
  let updated_alerts = @azimuth.StreamProcessor::get_triggered_alerts(stream_processor)
  let updated_rate_alerts = updated_alerts.filter(fn(alert) { alert.rule_name == "high_error_rate" })
  assert_true(updated_rate_alerts.length() > 0) // 应该触发更多告警
}

test "遥测数据安全加密" {
  // 测试遥测数据安全加密功能
  let encryption_config = @azimuth.EncryptionConfig {
    algorithm: @azimuth.EncryptionAlgorithm::AES256,
    key_rotation_interval_hours: 24,
    encryption_scope: @azimuth.EncryptionScope::All,
    field_level_encryption: true,
    sensitive_fields: ["user.id", "credit.card", "ssn", "api.key"],
    key_provider: @azimuth.KeyProvider::KMS
  }
  
  let encryption_manager = @azimuth.EncryptionManager::new(encryption_config)
  
  // 创建包含敏感信息的遥测数据
  let sensitive_telemetry = @azimuth.SensitiveTelemetryData {
    trace_id: "encryption-test-trace",
    spans: [
      @azimuth.SensitiveSpan {
        span_id: "encryption-test-span",
        operation_name: "payment.processing",
        regular_attributes: [
          ("http.method", @azimuth.StringValue("POST")),
          ("http.url", @azimuth.StringValue("/api/payments")),
          ("http.status_code", @azimuth.IntValue(200))
        ],
        sensitive_attributes: [
          ("user.id", @azimuth.StringValue("user-12345")),
          ("credit.card", @azimuth.StringValue("4111-1111-1111-1111")),
          ("transaction.id", @azimuth.StringValue("txn-abc123")),
          ("api.key", @azimuth.StringValue("sk-1234567890abcdef"))
        ],
        events: [
          @azimuth.SensitiveEvent {
            name: "authentication.success",
            regular_attributes: [("timestamp", @azimuth.StringValue("2022-01-01T12:00:00Z"))],
            sensitive_attributes: [
              ("auth.token", @azimuth.StringValue("jwt-token-12345")),
              ("session.id", @azimuth.StringValue("sess-67890"))
            ]
          }
        ]
      }
    ],
    logs: [
      @azimuth.SensitiveLog {
        timestamp: 1640995200000L,
        level: @azimuth.LogLevel::Info,
        message: "Payment processed successfully",
        regular_attributes: [("service.name", @azimuth.StringValue("payment-service"))],
        sensitive_attributes: [
          ("user.email", @azimuth.StringValue("user@example.com")),
          ("user.phone", @azimuth.StringValue("+1-555-123-4567"))
        ]
      }
    ]
  }
  
  // 测试数据加密
  let encryption_result = @azimuth.EncryptionManager::encrypt_telemetry(encryption_manager, sensitive_telemetry)
  assert_true(encryption_result.is_success)
  assert_true(encryption_result.encrypted_data.length() > 0)
  assert_not_eq(encryption_result.encrypted_data, sensitive_telemetry) // 数据应该已改变
  
  // 验证敏感字段已加密
  let encrypted_span = encryption_result.encrypted_data.spans[0]
  assert_eq(encrypted_span.regular_attributes[0], ("http.method", @azimuth.StringValue("POST"))) // 常规字段未加密
  assert_true(encrypted_span.sensitive_attributes[0].1.starts_with("enc:")) // 敏感字段已加密
  assert_true(encrypted_span.sensitive_attributes[1].1.starts_with("enc:"))
  
  // 测试数据解密
  let decryption_result = @azimuth.EncryptionManager::decrypt_telemetry(
    encryption_manager, 
    encryption_result.encrypted_data
  )
  assert_true(decryption_result.is_success)
  
  // 验证解密后的数据与原始数据一致
  let decrypted_span = decryption_result.decrypted_data.spans[0]
  assert_eq(decrypted_span.sensitive_attributes[0], ("user.id", @azimuth.StringValue("user-12345")))
  assert_eq(decrypted_span.sensitive_attributes[1], ("credit.card", @azimuth.StringValue("4111-1111-1111-1111")))
  
  // 测试字段级加密
  let field_encryption_result = @azimuth.EncryptionManager::encrypt_field(
    encryption_manager, 
    "user.id", 
    "user-12345"
  )
  assert_true(field_encryption_result.is_success)
  assert_true(field_encryption_result.encrypted_value.starts_with("enc:"))
  
  let field_decryption_result = @azimuth.EncryptionManager::decrypt_field(
    encryption_manager, 
    "user.id", 
    field_encryption_result.encrypted_value
  )
  assert_true(field_decryption_result.is_success)
  assert_eq(field_decryption_result.decrypted_value, "user-12345")
  
  // 测试密钥轮换
  let key_rotation_result = @azimuth.EncryptionManager::rotate_keys(encryption_manager)
  assert_true(key_rotation_result.is_success)
  assert_true(key_rotation_result.new_key_id != key_rotation_result.old_key_id)
  
  // 使用新密钥加密数据
  let new_key_encryption = @azimuth.EncryptionManager::encrypt_telemetry(encryption_manager, sensitive_telemetry)
  assert_true(new_key_encryption.is_success)
  assert_true(new_key_encryption.key_id == key_rotation_result.new_key_id)
  
  // 测试多密钥解密（新旧密钥都能解密）
  let old_key_decryption = @azimuth.EncryptionManager::decrypt_telemetry(
    encryption_manager, 
    encryption_result.encrypted_data
  )
  assert_true(old_key_decryption.is_success) // 旧密钥仍能解密
  
  let new_key_decryption = @azimuth.EncryptionManager::decrypt_telemetry(
    encryption_manager, 
    new_key_encryption.encrypted_data
  )
  assert_true(new_key_decryption.is_success) // 新密钥也能解密
  
  // 测试加密性能
  let performance_test_data = sensitive_telemetry
  let encryption_perf = @azimuth.EncryptionManager::benchmark_encryption(encryption_manager, performance_test_data)
  assert_true(encryption_perf.encryption_time_ms > 0)
  assert_true(encryption_perf.decryption_time_ms > 0)
  assert_true(encryption_perf.throughput_mbps > 0)
  
  // 测试加密统计
  let encryption_stats = @azimuth.EncryptionManager::get_statistics(encryption_manager)
  assert_true(encryption_stats.total_encryptions >= 2)
  assert_true(encryption_stats.total_decryptions >= 2)
  assert_true(encryption_stats.active_keys >= 1)
  assert_true(encryption_stats.encrypted_fields.contains("user.id"))
  assert_true(encryption_stats.encrypted_fields.contains("credit.card"))
}