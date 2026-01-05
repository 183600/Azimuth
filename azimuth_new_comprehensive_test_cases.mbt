// Azimuth New Comprehensive Test Cases
// 新的综合测试用例，涵盖各种功能场景

test "telemetry data validation and sanitization" {
  // 测试遥测数据验证和清理
  let raw_data = @azimuth.RawTelemetryData {
    timestamp : -1L, // 无效时间戳
    trace_id : "", // 空trace_id
    span_id : "invalid", // 无效span_id
    operation_name : "", // 空操作名
    attributes : [
      ("key.with.dots", @azimuth.StringValue("value1")), // 包含点的键
      ("key with spaces", @azimuth.StringValue("value2")), // 包含空格的键
      ("", @azimuth.StringValue("value3")), // 空键
      ("valid.key", @azimuth.StringValue("")) // 空值
    ]
  }
  
  // 数据验证和清理
  let validated_data = @azimuth.validate_and_sanitize(raw_data)
  
  // 验证清理后的数据
  assert_true(validated_data.timestamp >= 0L)
  assert_true(validated_data.trace_id.length() > 0)
  assert_true(validated_data.span_id.length() > 0)
  assert_true(validated_data.operation_name.length() > 0)
  
  // 验证属性清理
  let sanitized_attrs = validated_data.attributes
  assert_true(sanitized_attrs.length() <= raw_data.attributes.length())
  
  // 验证无效键被过滤或替换
  for attr in sanitized_attrs {
    assert_true(attr.0.length() > 0)
    assert_false(attr.0.contains("."))
    assert_false(attr.0.contains(" "))
  }
}

test "adaptive sampling strategy with dynamic thresholds" {
  // 测试具有动态阈值的自适应采样策略
  let sampling_strategy = @azimuth.AdaptiveSamplingStrategy {
    base_probability : 0.1,
    max_probability : 0.5,
    min_probability : 0.01,
    error_rate_threshold : 0.05,
    latency_threshold_ms : 1000L,
    throughput_threshold : 10000,
    adjustment_factor : 1.5,
    evaluation_window_ms : 60000L
  }
  
  // 模拟不同的系统状态
  let normal_state = @azimuth.SystemMetrics {
    error_rate : 0.02, // 低于阈值
    avg_latency_ms : 500L, // 低于阈值
    throughput : 5000 // 低于阈值
  }
  
  let high_error_state = @azimuth.SystemMetrics {
    error_rate : 0.1, // 高于阈值
    avg_latency_ms : 500L,
    throughput : 5000
  }
  
  let high_latency_state = @azimuth.SystemMetrics {
    error_rate : 0.02,
    avg_latency_ms : 2000L, // 高于阈值
    throughput : 5000
  }
  
  // 计算不同状态下的采样概率
  let normal_prob = @azimuth.calculate_sampling_probability(sampling_strategy, normal_state)
  let high_error_prob = @azimuth.calculate_sampling_probability(sampling_strategy, high_error_state)
  let high_latency_prob = @azimuth.calculate_sampling_probability(sampling_strategy, high_latency_state)
  
  // 验证采样概率调整
  assert_true(normal_prob >= sampling_strategy.min_probability)
  assert_true(normal_prob <= sampling_strategy.max_probability)
  assert_true(high_error_prob > normal_prob) // 高错误率应增加采样
  assert_true(high_latency_prob > normal_prob) // 高延迟应增加采样
}

test "telemetry data compression with multiple algorithms" {
  // 测试使用多种算法的遥测数据压缩
  let telemetry_batch = @azimuth.TelemetryBatch {
    batch_id : "batch-12345",
    timestamp : 1640995200000L,
    items : [
      @azimuth.TelemetryItem {
        trace_id : "trace1",
        span_id : "span1",
        operation_name : "http.get",
        attributes : [
          ("http.method", @azimuth.StringValue("GET")),
          ("http.url", @azimuth.StringValue("/api/users")),
          ("http.status_code", @azimuth.IntValue(200))
        ]
      },
      @azimuth.TelemetryItem {
        trace_id : "trace2",
        span_id : "span2",
        operation_name : "db.query",
        attributes : [
          ("db.type", @azimuth.StringValue("postgresql")),
          ("db.statement", @azimuth.StringValue("SELECT * FROM users")),
          ("db.duration_ms", @azimuth.IntValue(150))
        ]
      }
    ]
  }
  
  // 测试不同的压缩算法
  let gzip_compressed = @azimuth.compress_batch(telemetry_batch, @azimuth.CompressionAlgorithm::Gzip)
  let lz4_compressed = @azimuth.compress_batch(telemetry_batch, @azimuth.CompressionAlgorithm::LZ4)
  let zstd_compressed = @azimuth.compress_batch(telemetry_batch, @azimuth.CompressionAlgorithm::Zstd)
  
  // 验证压缩结果
  assert_true(gzip_compressed.compressed_size > 0)
  assert_true(lz4_compressed.compressed_size > 0)
  assert_true(zstd_compressed.compressed_size > 0)
  
  // 验证解压缩后数据一致性
  let gzip_decompressed = @azimuth.decompress_batch(gzip_compressed, @azimuth.CompressionAlgorithm::Gzip)
  let lz4_decompressed = @azimuth.decompress_batch(lz4_compressed, @azimuth.CompressionAlgorithm::LZ4)
  let zstd_decompressed = @azimuth.decompress_batch(zstd_compressed, @azimuth.CompressionAlgorithm::Zstd)
  
  assert_eq(gzip_decompressed.batch_id, telemetry_batch.batch_id)
  assert_eq(lz4_decompressed.batch_id, telemetry_batch.batch_id)
  assert_eq(zstd_decompressed.batch_id, telemetry_batch.batch_id)
  assert_eq(gzip_decompressed.items.length(), telemetry_batch.items.length())
  assert_eq(lz4_decompressed.items.length(), telemetry_batch.items.length())
  assert_eq(zstd_decompressed.items.length(), telemetry_batch.items.length())
}

test "metric aggregation with time window bucketing" {
  // 测试时间窗口桶的指标聚合
  let time_series_metrics = @azimuth.TimeSeriesMetrics {
    metric_name : "response.time",
    metric_type : @azimuth.MetricType::Histogram,
    time_buckets : [
      @azimuth.TimeBucket {
        start_time : 1640995200000L, // 2022-01-01 00:00:00
        end_time : 1640995260000L,   // 2022-01-01 00:01:00
        values : [100.0, 150.0, 200.0, 120.0, 180.0],
        count : 5,
        sum : 750.0
      },
      @azimuth.TimeBucket {
        start_time : 1640995260000L, // 2022-01-01 00:01:00
        end_time : 1640995320000L,   // 2022-01-01 00:02:00
        values : [90.0, 110.0, 130.0, 160.0, 140.0],
        count : 5,
        sum : 630.0
      },
      @azimuth.TimeBucket {
        start_time : 1640995320000L, // 2022-01-01 00:02:00
        end_time : 1640995380000L,   // 2022-01-01 00:03:00
        values : [80.0, 95.0, 115.0, 125.0, 135.0],
        count : 5,
        sum : 550.0
      }
    ]
  }
  
  // 计算聚合统计
  let aggregated_stats = @azimuth.calculate_aggregated_stats(time_series_metrics)
  
  // 验证聚合结果
  assert_eq(aggregated_stats.total_count, 15) // 3个桶，每个5个值
  assert_eq(aggregated_stats.total_sum, 1930.0) // 750 + 630 + 550
  assert_true(aggregated_stats.avg_value > 0.0)
  assert_true(aggregated_stats.min_value > 0.0)
  assert_true(aggregated_stats.max_value > 0.0)
  assert_true(aggregated_stats.p50_value > 0.0)
  assert_true(aggregated_stats.p95_value > 0.0)
  assert_true(aggregated_stats.p99_value > 0.0)
  
  // 验证百分位数计算
  assert_true(aggregated_stats.p50_value <= aggregated_stats.p95_value)
  assert_true(aggregated_stats.p95_value <= aggregated_stats.p99_value)
  assert_true(aggregated_stats.p99_value <= aggregated_stats.max_value)
}

test "distributed tracing with service mesh integration" {
  // 测试与服务网格集成的分布式追踪
  let service_mesh_trace = @azimuth.ServiceMeshTrace {
    trace_id : "mesh-trace-12345",
    mesh_protocol : "istio",
    ingress_gateway : @azimuth.GatewaySpan {
      gateway_name : "istio-ingressgateway",
      span_id : "ingress-span-123",
      start_time : 1640995200000L,
      end_time : Some(1640995200100L),
      attributes : [
        ("request.protocol", @azimuth.StringValue("http")),
        ("request.host", @azimuth.StringValue("api.example.com")),
        ("request.method", @azimuth.StringValue("POST")),
        ("request.path", @azimuth.StringValue("/api/v1/orders")),
        ("response.code", @azimuth.IntValue(201))
      ]
    },
    service_spans : [
      @azimuth.MeshServiceSpan {
        service_name : "orders-service",
        span_id : "orders-span-456",
        parent_span_id : Some("ingress-span-123"),
        start_time : 1640995200050L,
        end_time : Some(1640995200200L),
        attributes : [
          ("service.version", @azimuth.StringValue("v1.2.3")),
          ("service.namespace", @azimuth.StringValue("production")),
          ("operation", @azimuth.StringValue("createOrder")),
          ("db.query.count", @azimuth.IntValue(3))
        ]
      },
      @azimuth.MeshServiceSpan {
        service_name : "payment-service",
        span_id : "payment-span-789",
        parent_span_id : Some("orders-span-456"),
        start_time : 1640995200100L,
        end_time : Some(1640995200180L),
        attributes : [
          ("service.version", @azimuth.StringValue("v2.1.0")),
          ("service.namespace", @azimuth.StringValue("production")),
          ("operation", @azimuth.StringValue("processPayment")),
          ("payment.gateway", @azimuth.StringValue("stripe"))
        ]
      }
    ],
    egress_gateway : @azimuth.GatewaySpan {
      gateway_name : "istio-egressgateway",
      span_id : "egress-span-999",
      start_time : 1640995200200L,
      end_time : Some(1640995200250L),
      attributes : [
        ("external.service", @azimuth.StringValue("payment-gateway")),
        ("external.endpoint", @azimuth.StringValue("https://api.stripe.com")),
        ("request.method", @azimuth.StringValue("POST")),
        ("response.code", @azimuth.IntValue(200))
      ]
    }
  }
  
  // 验证服务网格追踪结构
  assert_eq(service_mesh_trace.trace_id, "mesh-trace-12345")
  assert_eq(service_mesh_trace.mesh_protocol, "istio")
  assert_eq(service_mesh_trace.service_spans.length(), 2)
  
  // 验证时间顺序
  assert_true(service_mesh_trace.ingress_gateway.start_time <= service_mesh_trace.service_spans[0].start_time)
  assert_true(service_mesh_trace.service_spans[0].start_time <= service_mesh_trace.service_spans[1].start_time)
  assert_true(service_mesh_trace.service_spans[1].start_time <= service_mesh_trace.egress_gateway.start_time)
  
  // 验证父子关系
  match service_mesh_trace.service_spans[0].parent_span_id {
    Some(parent) => assert_eq(parent, service_mesh_trace.ingress_gateway.span_id)
    None => assert_true(false)
  }
  
  match service_mesh_trace.service_spans[1].parent_span_id {
    Some(parent) => assert_eq(parent, service_mesh_trace.service_spans[0].span_id)
    None => assert_true(false)
  }
}

test "telemetry data retention and archival policies" {
  // 测试遥测数据保留和归档策略
  let retention_policy = @azimuth.RetentionPolicy {
    hot_storage_days : 7,
    warm_storage_days : 30,
    cold_storage_days : 365,
    archival_after_days : 365,
    delete_after_days : 2555, // 7年
    compression_enabled : true,
    encryption_enabled : true,
    sampling_for_long_term : true,
    long_term_sampling_rate : 0.01
  }
  
  // 模拟不同年龄的数据
  let current_time = 1640995200000L // 2022-01-01 00:00:00
  let hot_data = @azimuth.TelemetryData {
    timestamp : current_time - (3 * 24 * 60 * 60 * 1000), // 3天前
    trace_id : "hot-trace-123",
    data_size_bytes : 1024,
    storage_tier : @azimuth.StorageTier::Hot
  }
  
  let warm_data = @azimuth.TelemetryData {
    timestamp : current_time - (15 * 24 * 60 * 60 * 1000), // 15天前
    trace_id : "warm-trace-456",
    data_size_bytes : 1024,
    storage_tier : @azimuth.StorageTier::Warm
  }
  
  let cold_data = @azimuth.TelemetryData {
    timestamp : current_time - (100 * 24 * 60 * 60 * 1000), // 100天前
    trace_id : "cold-trace-789",
    data_size_bytes : 1024,
    storage_tier : @azimuth.StorageTier::Cold
  }
  
  let archival_data = @azimuth.TelemetryData {
    timestamp : current_time - (400 * 24 * 60 * 60 * 1000), // 400天前
    trace_id : "archival-trace-999",
    data_size_bytes : 1024,
    storage_tier : @azimuth.StorageTier::Archival
  }
  
  // 应用保留策略
  let hot_policy = @azimuth.apply_retention_policy(retention_policy, hot_data, current_time)
  let warm_policy = @azimuth.apply_retention_policy(retention_policy, warm_data, current_time)
  let cold_policy = @azimuth.apply_retention_policy(retention_policy, cold_data, current_time)
  let archival_policy = @azimuth.apply_retention_policy(retention_policy, archival_data, current_time)
  
  // 验证策略应用结果
  match hot_policy.action {
    @azimuth.RetentionAction::Keep => assert_true(true)
    _ => assert_true(false)
  }
  
  match warm_policy.action {
    @azimuth.RetentionAction::Keep => assert_true(true)
    _ => assert_true(false)
  }
  
  match cold_policy.action {
    @azimuth.RetentionAction::Keep => assert_true(true)
    _ => assert_true(false)
  }
  
  match archival_policy.action {
    @azimuth.RetentionAction::Archive => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证存储层分配
  assert_eq(hot_policy.target_tier, Some(@azimuth.StorageTier::Hot))
  assert_eq(warm_policy.target_tier, Some(@azimuth.StorageTier::Warm))
  assert_eq(cold_policy.target_tier, Some(@azimuth.StorageTier::Cold))
  assert_eq(archival_policy.target_tier, Some(@azimuth.StorageTier::Archival))
}

test "telemetry dashboard visualization data preparation" {
  // 测试遥测仪表板可视化数据准备
  let dashboard_config = @azimuth.DashboardConfig {
    dashboard_id : "main-operations",
    title : "Main Operations Dashboard",
    refresh_interval_seconds : 30,
    time_range_hours : 24,
    widgets : [
      @azimuth.WidgetConfig {
        widget_id : "request-rate",
        widget_type : @azimuth.WidgetType::TimeSeriesChart,
        title : "Request Rate",
        metrics : ["http.requests.total"],
        aggregation : @azimuth.AggregationType::Rate,
        group_by : ["service.name", "http.method"]
      },
      @azimuth.WidgetConfig {
        widget_id : "error-rate",
        widget_type : @azimuth.WidgetType::GaugeChart,
        title : "Error Rate",
        metrics : ["http.errors.total"],
        aggregation : @azimuth.AggregationType::Percentage,
        group_by : ["service.name"]
      },
      @azimuth.WidgetConfig {
        widget_id : "response-time",
        widget_type : @azimuth.WidgetType::Heatmap,
        title : "Response Time Distribution",
        metrics : ["http.request.duration"],
        aggregation : @azimuth.AggregationType::Percentile,
        group_by : ["service.name", "endpoint"]
      }
    ]
  }
  
  // 模拟原始遥测数据
  let raw_telemetry = @azimuth.RawTelemetryDataset {
    timestamp : 1640995200000L,
    data_points : [
      @azimuth.DataPoint {
        metric_name : "http.requests.total",
        value : 1000.0,
        attributes : [
          ("service.name", @azimuth.StringValue("api-gateway")),
          ("http.method", @azimuth.StringValue("GET")),
          ("http.status_code", @azimuth.IntValue(200))
        ]
      },
      @azimuth.DataPoint {
        metric_name : "http.requests.total",
        value : 500.0,
        attributes : [
          ("service.name", @azimuth.StringValue("api-gateway")),
          ("http.method", @azimuth.StringValue("POST")),
          ("http.status_code", @azimuth.IntValue(201))
        ]
      },
      @azimuth.DataPoint {
        metric_name : "http.errors.total",
        value : 50.0,
        attributes : [
          ("service.name", @azimuth.StringValue("api-gateway")),
          ("error.type", @azimuth.StringValue("timeout"))
        ]
      },
      @azimuth.DataPoint {
        metric_name : "http.request.duration",
        value : 150.0,
        attributes : [
          ("service.name", @azimuth.StringValue("user-service")),
          ("endpoint", @azimuth.StringValue("/api/users")),
          ("percentile", @azimuth.StringValue("p95"))
        ]
      }
    ]
  }
  
  // 准备仪表板数据
  let dashboard_data = @azimuth.prepare_dashboard_data(dashboard_config, raw_telemetry)
  
  // 验证仪表板数据准备
  assert_eq(dashboard_data.dashboard_id, dashboard_config.dashboard_id)
  assert_eq(dashboard_data.widgets.length(), dashboard_config.widgets.length())
  
  // 验证请求数率图表数据
  let request_rate_widget = dashboard_data.widgets.filter(fn(w) { w.widget_id == "request-rate" })
  assert_eq(request_rate_widget.length(), 1)
  assert_eq(request_rate_widget[0].data_series.length(), 2) // GET和POST
  
  // 验证错误率仪表数据
  let error_rate_widget = dashboard_data.widgets.filter(fn(w) { w.widget_id == "error-rate" })
  assert_eq(error_rate_widget.length(), 1)
  assert_true(error_rate_widget[0].current_value > 0.0)
  
  // 验证响应时间热力图数据
  let response_time_widget = dashboard_data.widgets.filter(fn(w) { w.widget_id == "response-time" })
  assert_eq(response_time_widget.length(), 1)
  assert_eq(response_time_widget[0].heatmap_data.length(), 1)
}

test "telemetry data export and import across formats" {
  // 测试跨格式的遥测数据导出和导入
  let telemetry_dataset = @azimuth.TelemetryDataset {
    dataset_id : "export-test-123",
    created_at : 1640995200000L,
    traces : [
      @azimuth.ExportableTrace {
        trace_id : "trace-123",
        spans : [
          @azimuth.ExportableSpan {
            span_id : "span-123",
            parent_span_id : None,
            operation_name : "http.get",
            start_time : 1640995200000L,
            end_time : Some(1640995200100L),
            status : @azimuth.SpanStatus::Ok,
            attributes : [
              ("http.method", @azimuth.StringValue("GET")),
              ("http.url", @azimuth.StringValue("/api/users")),
              ("http.status_code", @azimuth.IntValue(200))
            ]
          }
        ]
      }
    ],
    metrics : [
      @azimuth.ExportableMetric {
        name : "http.requests.total",
        value : 1000.0,
        timestamp : 1640995200000L,
        attributes : [
          ("service.name", @azimuth.StringValue("api-gateway")),
          ("http.method", @azimuth.StringValue("GET"))
        ]
      }
    ],
    logs : [
      @azimuth.ExportableLog {
        timestamp : 1640995200050L,
        severity : @azimuth.LogSeverity::Info,
        message : "Request processed successfully",
        attributes : [
          ("trace.id", @azimuth.StringValue("trace-123")),
          ("span.id", @azimuth.StringValue("span-123"))
        ]
      }
    ]
  }
  
  // 导出为不同格式
  let json_export = @azimuth.export_dataset(telemetry_dataset, @azimuth.ExportFormat::JSON)
  let protobuf_export = @azimuth.export_dataset(telemetry_dataset, @azimuth.ExportFormat::Protobuf)
  let avro_export = @azimuth.export_dataset(telemetry_dataset, @azimuth.ExportFormat::Avro)
  
  // 验证导出结果
  assert_true(json_export.data_size > 0)
  assert_true(protobuf_export.data_size > 0)
  assert_true(avro_export.data_size > 0)
  
  // 从不同格式导入
  let json_imported = @azimuth.import_dataset(json_export, @azimuth.ExportFormat::JSON)
  let protobuf_imported = @azimuth.import_dataset(protobuf_export, @azimuth.ExportFormat::Protobuf)
  let avro_imported = @azimuth.import_dataset(avro_export, @azimuth.ExportFormat::Avro)
  
  // 验证导入数据完整性
  assert_eq(json_imported.dataset_id, telemetry_dataset.dataset_id)
  assert_eq(protobuf_imported.dataset_id, telemetry_dataset.dataset_id)
  assert_eq(avro_imported.dataset_id, telemetry_dataset.dataset_id)
  
  assert_eq(json_imported.traces.length(), telemetry_dataset.traces.length())
  assert_eq(protobuf_imported.traces.length(), telemetry_dataset.traces.length())
  assert_eq(avro_imported.traces.length(), telemetry_dataset.traces.length())
  
  assert_eq(json_imported.metrics.length(), telemetry_dataset.metrics.length())
  assert_eq(protobuf_imported.metrics.length(), telemetry_dataset.metrics.length())
  assert_eq(avro_imported.metrics.length(), telemetry_dataset.metrics.length())
  
  assert_eq(json_imported.logs.length(), telemetry_dataset.logs.length())
  assert_eq(protobuf_imported.logs.length(), telemetry_dataset.logs.length())
  assert_eq(avro_imported.logs.length(), telemetry_dataset.logs.length())
}

test "telemetry anomaly detection and alerting" {
  // 测试遥测异常检测和告警
  let anomaly_detection_config = @azimuth.AnomalyDetectionConfig {
    algorithms : [
      @azimuth.AnomalyAlgorithm {
        name : "statistical_outlier",
        parameters : [
          ("z_score_threshold", @azimuth.FloatValue(3.0)),
          ("window_size", @azimuth.IntValue(100))
        ]
      },
      @azimuth.AnomalyAlgorithm {
        name : "seasonal_decomposition",
        parameters : [
          ("seasonality_period", @azimuth.IntValue(24)), // 24小时季节性
          ("threshold", @azimuth.FloatValue(2.0))
        ]
      }
    ],
    alerting_rules : [
      @azimuth.AlertingRule {
        rule_name : "high_error_rate",
        condition : "error_rate > 0.05",
        severity : @azimuth.AlertSeverity::Critical,
        cooldown_minutes : 15
      },
      @azimuth.AlertingRule {
        rule_name : "high_latency",
        condition : "p95_latency > 1000",
        severity : @azimuth.AlertSeverity::Warning,
        cooldown_minutes : 10
      }
    ]
  }
  
  // 模拟时间序列数据，包含异常
  let time_series_data = @azimuth.TimeSeriesData {
    metric_name : "error_rate",
    data_points : [
      (1640995200000L, 0.01), // 正常
      (1640995260000L, 0.02), // 正常
      (1640995320000L, 0.015), // 正常
      (1640995380000L, 0.08), // 异常 - 高错误率
      (1640995440000L, 0.09), // 异常 - 持续高错误率
      (1640995500000L, 0.07), // 异常 - 仍然高
      (1640995560000L, 0.02) // 恢复正常
    ]
  }
  
  // 异常检测
  let anomaly_results = @azimuth.detect_anomalies(anomaly_detection_config, time_series_data)
  
  // 验证异常检测结果
  assert_true(anomaly_results.anomalies.length() > 0)
  
  // 验证异常时间点
  let anomaly_timestamps = anomaly_results.anomalies.map(fn(a) { a.timestamp })
  assert_true(anomaly_timestamps.includes(1640995380000L)) // 第一个异常点
  assert_true(anomaly_timestamps.includes(1640995440000L)) // 第二个异常点
  assert_true(anomaly_timestamps.includes(1640995500000L)) // 第三个异常点
  
  // 生成告警
  let alerts = @azimuth.generate_alerts(anomaly_detection_config, anomaly_results)
  
  // 验证告警生成
  assert_true(alerts.length() > 0)
  
  let critical_alerts = alerts.filter(fn(a) { a.severity == @azimuth.AlertSeverity::Critical })
  assert_true(critical_alerts.length() > 0)
  
  // 验证告警内容
  for alert in critical_alerts {
    assert_eq(alert.rule_name, "high_error_rate")
    assert_true(alert.message.contains("error_rate"))
    assert_true(alert.triggered_at > 0L)
  }
}