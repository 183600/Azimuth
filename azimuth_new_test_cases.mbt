// Azimuth 新增测试用例
// 包含遥测系统的新功能测试和边界情况测试

// 测试1: 遥测数据压缩
test "遥测数据压缩和解压缩" {
  // 创建压缩器
  let compressor = TelemetryCompressor::new()
  
  // 创建测试数据
  let telemetry_data = [
    SpanData {
      trace_id: "trace-12345",
      span_id: "span-67890",
      parent_span_id: Some("span-12345"),
      name: "http.request",
      kind: SpanKind::Server,
      start_time: 1609459200000000000L,
      end_time: 1609459201000000000L,
      status: SpanStatus::Ok,
      attributes: [
        ("http.method", "GET"),
        ("http.url", "/api/users"),
        ("http.status_code", "200"),
        ("service.name", "user-service")
      ],
      events: [
        SpanEvent {
          name: "http.request.started",
          timestamp: 1609459200000000000L,
          attributes: []
        },
        SpanEvent {
          name: "http.request.completed",
          timestamp: 1609459201000000000L,
          attributes: [("duration_ms", "1000")]
        }
      ]
    },
    SpanData {
      trace_id: "trace-12345",
      span_id: "span-11111",
      parent_span_id: Some("span-67890"),
      name: "database.query",
      kind: SpanKind::Client,
      start_time: 1609459200200000000L,
      end_time: 1609459200800000000L,
      status: SpanStatus::Ok,
      attributes: [
        ("db.system", "postgresql"),
        ("db.statement", "SELECT * FROM users WHERE id = ?"),
        ("db.operation", "SELECT"),
        ("service.name", "user-service")
      ],
      events: []
    }
  ]
  
  // 测试压缩
  let compressed_data = TelemetryCompressor::compress(compressor, telemetry_data)
  assert_true(compressed_data.length() > 0)
  assert_true(compressed_data.length() < telemetry_data.length() * 100)  // 压缩后应该更小
  
  // 测试解压缩
  let decompressed_data = TelemetryCompressor::decompress(compressor, compressed_data)
  assert_eq(decompressed_data.length(), telemetry_data.length())
  
  // 验证解压缩后的数据完整性
  let original_span = telemetry_data[0]
  let decompressed_span = decompressed_data[0]
  assert_eq(original_span.trace_id, decompressed_span.trace_id)
  assert_eq(original_span.span_id, decompressed_span.span_id)
  assert_eq(original_span.name, decompressed_span.name)
  assert_eq(original_span.attributes.length(), decompressed_span.attributes.length())
}

// 测试2: 时间序列数据处理
test "时间序列数据处理和分析" {
  // 创建时间序列数据点
  let time_series_points = [
    TimeSeriesPoint {
      timestamp: 1609459200000L,  // 2021-01-01 00:00:00
      value: 42.5,
      labels: [("metric.name", "cpu.usage"), ("service.name", "api-service")]
    },
    TimeSeriesPoint {
      timestamp: 1609459260000L,  // 2021-01-01 00:01:00
      value: 45.2,
      labels: [("metric.name", "cpu.usage"), ("service.name", "api-service")]
    },
    TimeSeriesPoint {
      timestamp: 1609459320000L,  // 2021-01-01 00:02:00
      value: 38.9,
      labels: [("metric.name", "cpu.usage"), ("service.name", "api-service")]
    },
    TimeSeriesPoint {
      timestamp: 1609459380000L,  // 2021-01-01 00:03:00
      value: 51.7,
      labels: [("metric.name", "cpu.usage"), ("service.name", "api-service")]
    },
    TimeSeriesPoint {
      timestamp: 1609459440000L,  // 2021-01-01 00:04:00
      value: 47.3,
      labels: [("metric.name", "cpu.usage"), ("service.name", "api-service")]
    }
  ]
  
  // 创建时间序列处理器
  let processor = TimeSeriesProcessor::new()
  
  // 测试时间窗口聚合
  let windowed_aggregates = TimeSeriesProcessor::aggregate_by_time_window(
    processor, 
    time_series_points, 
    120000L  // 2分钟窗口
  )
  
  assert_eq(windowed_aggregates.length(), 3)  // 5个点分成3个窗口
  
  // 验证第一个窗口的聚合结果
  let first_window = windowed_aggregates[0]
  assert_eq(first_window.point_count, 2)
  assert_eq(first_window.avg, (42.5 + 45.2) / 2.0)
  assert_eq(first_window.min, 42.5)
  assert_eq(first_window.max, 45.2)
  
  // 测试趋势分析
  let trend = TimeSeriesProcessor::analyze_trend(processor, time_series_points)
  assert_eq(trend.direction, TrendDirection::Increasing)  // 整体趋势上升
  assert_true(trend.slope > 0.0)
  
  // 测试异常检测
  let anomalies = TimeSeriesProcessor::detect_anomalies(processor, time_series_points, 2.0)
  assert_eq(anomalies.length(), 1)  // 51.7可能是异常值
  assert_eq(anomalies[0].value, 51.7)
  
  // 测试预测
  let predictions = TimeSeriesProcessor::predict_next_values(processor, time_series_points, 2)
  assert_eq(predictions.length(), 2)
  assert_true(predictions[0] > 40.0)  // 预测值应该在合理范围内
}

// 测试3: 分布式追踪链路测试
test "分布式追踪链路完整性" {
  // 创建追踪链路
  let trace_id = TraceId::generate()
  
  // 创建根span
  let root_span = SpanBuilder::new("api.gateway")
    .with_trace_id(trace_id)
    .with_span_kind(SpanKind::Server)
    .with_attributes([
      ("http.method", "POST"),
      ("http.url", "/api/orders"),
      ("service.name", "gateway-service")
    ])
    .start()
  
  // 创建子span1: 认证服务
  let auth_span = SpanBuilder::new("auth.service")
    .with_trace_id(trace_id)
    .with_parent_span_id(Span::span_context(root_span).span_id)
    .with_span_kind(SpanKind::Client)
    .with_attributes([
      ("service.name", "auth-service"),
      ("operation.type", "validate_token")
    ])
    .start()
  
  // 创建子span2: 订单服务
  let order_span = SpanBuilder::new("order.service")
    .with_trace_id(trace_id)
    .with_parent_span_id(Span::span_context(root_span).span_id)
    .with_span_kind(SpanKind::Client)
    .with_attributes([
      ("service.name", "order-service"),
      ("operation.type", "create_order")
    ])
    .start()
  
  // 创建子span3: 库存服务（订单服务的子span）
  let inventory_span = SpanBuilder::new("inventory.service")
    .with_trace_id(trace_id)
    .with_parent_span_id(Span::span_context(order_span).span_id)
    .with_span_kind(SpanKind::Client)
    .with_attributes([
      ("service.name", "inventory-service"),
      ("operation.type", "check_availability")
    ])
    .start()
  
  // 添加事件和属性
  Span::add_event(auth_span, "token.validated", [("user.id", "user-123")])
  Span::set_attribute(auth_span, "auth.result", "success")
  Span::end(auth_span)
  
  Span::add_event(order_span, "order.created", [("order.id", "order-456")])
  Span::set_attribute(order_span, "order.total", "99.99")
  Span::end(order_span)
  
  Span::add_event(inventory_span, "inventory.checked", [("product.id", "prod-789")])
  Span::set_attribute(inventory_span, "stock.available", "10")
  Span::end(inventory_span)
  
  // 结束根span
  Span::add_event(root_span, "request.completed", [("duration_ms", "250")])
  Span::end(root_span)
  
  // 验证追踪链路完整性
  let trace_collector = TraceCollector::new()
  let collected_spans = TraceCollector::get_spans_by_trace_id(trace_collector, trace_id)
  
  assert_eq(collected_spans.length(), 4)
  
  // 验证父子关系
  let root_span_collected = collected_spans.find(fn(s) { s.name == "api.gateway" }).unwrap()
  let auth_span_collected = collected_spans.find(fn(s) { s.name == "auth.service" }).unwrap()
  let order_span_collected = collected_spans.find(fn(s) { s.name == "order.service" }).unwrap()
  let inventory_span_collected = collected_spans.find(fn(s) { s.name == "inventory.service" }).unwrap()
  
  // 验证所有span都有相同的trace_id
  assert_true(collected_spans.all(fn(s) { s.trace_id == trace_id }))
  
  // 验证父子关系
  assert_eq(auth_span_collected.parent_span_id, Some(root_span_collected.span_id))
  assert_eq(order_span_collected.parent_span_id, Some(root_span_collected.span_id))
  assert_eq(inventory_span_collected.parent_span_id, Some(order_span_collected.span_id))
  
  // 验证时间顺序
  assert_true(root_span_collected.start_time <= auth_span_collected.start_time)
  assert_true(root_span_collected.start_time <= order_span_collected.start_time)
  assert_true(order_span_collected.start_time <= inventory_span_collected.start_time)
}

// 测试4: 自定义度量聚合测试
test "自定义度量聚合和计算" {
  // 创建度量注册器
  let meter_registry = MeterRegistry::new()
  
  // 创建自定义计数器
  let request_counter = MeterRegistry::create_counter(
    meter_registry,
    "custom.requests.total",
    Some("Total custom requests"),
    Some("count")
  )
  
  // 创建自定义直方图
  let response_time_histogram = MeterRegistry::create_histogram(
    meter_registry,
    "custom.response.time",
    Some("Custom response time"),
    Some("ms")
  )
  
  // 创建自定义仪表
  let active_connections_gauge = MeterRegistry::create_gauge(
    meter_registry,
    "custom.active.connections",
    Some("Active connections"),
    Some("connections")
  )
  
  // 记录度量数据
  for i in 1..=100 {
    Counter::add_with_attributes(request_counter, 1.0, [
      ("endpoint", if i % 3 == 0 { "/api/users" } else if i % 3 == 1 { "/api/orders" } else { "/api/products" }),
      ("method", if i % 2 == 0 { "GET" } else { "POST" }),
      ("status", if i % 10 == 0 { "500" } else { "200" })
    ])
    
    let response_time = if i % 5 == 0 { 500.0 + (i as Float) * 2.0 } else { 50.0 + (i as Float) * 0.5 }
    Histogram::record_with_attributes(response_time_histogram, response_time, [
      ("endpoint", if i % 3 == 0 { "/api/users" } else if i % 3 == 1 { "/api/orders" } else { "/api/products" })
    ])
  }
  
  // 设置仪表值
  Gauge::set(active_connections_gauge, 25.0)
  
  // 创建自定义聚合器
  let aggregator = CustomMetricAggregator::new(meter_registry)
  
  // 测试按属性分组聚合
  let requests_by_endpoint = CustomMetricAggregator::group_by_attributes(
    aggregator,
    "custom.requests.total",
    ["endpoint"]
  )
  
  assert_eq(requests_by_endpoint.length(), 3)
  assert_true(requests_by_endpoint.has_key("/api/users"))
  assert_true(requests_by_endpoint.has_key("/api/orders"))
  assert_true(requests_by_endpoint.has_key("/api/products"))
  
  // 验证每个端点的请求数
  let users_requests = requests_by_endpoint.get("/api/users").unwrap()
  let orders_requests = requests_by_endpoint.get("/api/orders").unwrap()
  let products_requests = requests_by_endpoint.get("/api/products").unwrap()
  
  assert_eq(users_requests, 34.0)  // 100/3向上取整
  assert_eq(orders_requests, 33.0)
  assert_eq(products_requests, 33.0)
  
  // 测试百分位数计算
  let response_time_percentiles = CustomMetricAggregator::calculate_percentiles(
    aggregator,
    "custom.response.time",
    [50.0, 90.0, 95.0, 99.0]
  )
  
  assert_true(response_time_percentiles.has_key("50.0"))
  assert_true(response_time_percentiles.has_key("90.0"))
  assert_true(response_time_percentiles.has_key("95.0"))
  assert_true(response_time_percentiles.has_key("99.0"))
  
  let p50 = response_time_percentiles.get("50.0").unwrap()
  let p95 = response_time_percentiles.get("95.0").unwrap()
  
  assert_true(p50 < p95)  // 95百分位数应该大于50百分位数
  
  // 测试时间窗口聚合
  let time_window_aggregates = CustomMetricAggregator::aggregate_by_time_window(
    aggregator,
    "custom.requests.total",
    60000L  // 1分钟窗口
  )
  
  assert_true(time_window_aggregates.length() > 0)
  
  // 测试仪表值
  let gauge_value = CustomMetricAggregator::get_gauge_value(aggregator, "custom.active.connections")
  assert_eq(gauge_value, 25.0)
}

// 测试5: 遥测配置动态更新测试
test "遥测配置动态更新" {
  // 创建初始配置
  let initial_config = TelemetryConfig {
    sampling_probability: 0.1,
    batch_size: 100,
    export_interval_ms: 5000,
    max_attributes_per_span: 10,
    max_events_per_span: 5,
    max_links_per_span: 5,
    max_attributes_per_metric: 10,
    max_attributes_per_log: 10,
    otlp_endpoint: "http://localhost:4317",
    service_name: "test-service",
    service_version: "1.0.0",
    environment: "development"
  }
  
  // 创建配置管理器
  let config_manager = TelemetryConfigManager::new(initial_config)
  
  // 验证初始配置
  let current_config = TelemetryConfigManager::get_config(config_manager)
  assert_eq(current_config.sampling_probability, 0.1)
  assert_eq(current_config.batch_size, 100)
  assert_eq(current_config.export_interval_ms, 5000)
  
  // 测试动态更新采样概率
  TelemetryConfigManager::update_sampling_probability(config_manager, 0.5)
  let updated_config = TelemetryConfigManager::get_config(config_manager)
  assert_eq(updated_config.sampling_probability, 0.5)
  assert_eq(updated_config.batch_size, 100)  // 其他配置保持不变
  
  // 测试动态更新批处理大小
  TelemetryConfigManager::update_batch_size(config_manager, 200)
  let batch_updated_config = TelemetryConfigManager::get_config(config_manager)
  assert_eq(batch_updated_config.sampling_probability, 0.5)
  assert_eq(batch_updated_config.batch_size, 200)
  
  // 测试批量更新配置
  let config_updates = [
    ("export_interval_ms", "10000"),
    ("max_attributes_per_span", "20"),
    ("service_version", "1.1.0"),
    ("environment", "staging")
  ]
  
  TelemetryConfigManager::batch_update(config_manager, config_updates)
  let batch_updated_final_config = TelemetryConfigManager::get_config(config_manager)
  
  assert_eq(batch_updated_final_config.export_interval_ms, 10000)
  assert_eq(batch_updated_final_config.max_attributes_per_span, 20)
  assert_eq(batch_updated_final_config.service_version, "1.1.0")
  assert_eq(batch_updated_final_config.environment, "staging")
  
  // 测试配置验证
  let invalid_updates = [
    ("sampling_probability", "1.5"),  // 超出范围
    ("batch_size", "-1"),  // 负数
    ("export_interval_ms", "0")  // 零值
  ]
  
  let validation_results = TelemetryConfigManager::validate_updates(config_manager, invalid_updates)
  assert_eq(validation_results.length(), 3)
  assert_false(validation_results[0].is_valid)
  assert_false(validation_results[1].is_valid)
  assert_false(validation_results[2].is_valid)
  
  // 测试配置回滚
  TelemetryConfigManager::save_config_snapshot(config_manager, "before_major_changes")
  
  let major_updates = [
    ("sampling_probability", "0.8"),
    ("batch_size", "500"),
    ("export_interval_ms", "15000")
  ]
  
  TelemetryConfigManager::batch_update(config_manager, major_updates)
  let major_updated_config = TelemetryConfigManager::get_config(config_manager)
  assert_eq(major_updated_config.sampling_probability, 0.8)
  
  // 回滚到之前的快照
  TelemetryConfigManager::rollback_to_snapshot(config_manager, "before_major_changes")
  let rolled_back_config = TelemetryConfigManager::get_config(config_manager)
  assert_eq(rolled_back_config.sampling_probability, 0.5)
  assert_eq(rolled_back_config.batch_size, 200)
  assert_eq(rolled_back_config.export_interval_ms, 10000)
}

// 测试6: 跨服务上下文传播测试
test "跨服务上下文传播和 baggage 传播" {
  // 创建上下文传播器
  let propagator = TextMapPropagator::new()
  
  // 创建初始上下文
  let initial_context = Context::root()
  
  // 添加上下文值
  let correlation_id_key = ContextKey::new("correlation.id")
  let user_id_key = ContextKey::new("user.id")
  let request_id_key = ContextKey::new("request.id")
  
  let context_with_values = initial_context
    |> Context::with_value(correlation_id_key, "corr-abc123")
    |> Context::with_value(user_id_key, "user-456")
    |> Context::with_value(request_id_key, "req-789")
  
  // 创建 span 上下文
  let span_context = SpanContext {
    trace_id: "trace-12345",
    span_id: "span-67890",
    trace_flags: TraceFlags::SAMPLED,
    trace_state: TraceState::default(),
    is_remote: false
  }
  
  let context_with_span = Context::with_span_context(context_with_values, span_context)
  
  // 添加 baggage
  let baggage = Baggage::builder()
    .put("service.version", "1.2.3")
    .put("deployment.environment", "production")
    .put("region", "us-west-2")
    .build()
  
  let context_with_baggage = Context::with_baggage(context_with_span, baggage)
  
  // 测试注入
  let carrier = TextMapCarrier::new()
  TextMapPropagator::inject(propagator, context_with_baggage, carrier)
  
  // 验证注入的头部
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let tracestate = TextMapCarrier::get(carrier, "tracestate")
  let baggage_header = TextMapCarrier::get(carrier, "baggage")
  
  assert_true(traceparent.is_some())
  assert_true(baggage_header.is_some())
  
  let traceparent_value = traceparent.unwrap()
  assert_true(traceparent_value.contains("trace-12345"))
  assert_true(traceparent_value.contains("span-67890"))
  
  let baggage_value = baggage_header.unwrap()
  assert_true(baggage_value.contains("service.version=1.2.3"))
  assert_true(baggage_value.contains("deployment.environment=production"))
  assert_true(baggage_value.contains("region=us-west-2"))
  
  // 测试提取
  let extract_carrier = TextMapCarrier::new()
  TextMapCarrier::put(extract_carrier, "traceparent", traceparent_value)
  TextMapCarrier::put(extract_carrier, "baggage", baggage_value)
  
  let extracted_context = TextMapPropagator::extract(propagator, extract_carrier)
  
  // 验证提取的 span 上下文
  let extracted_span_context = Context::span_context(extracted_context)
  assert_true(SpanContext::is_valid(extracted_span_context))
  assert_eq(SpanContext::trace_id(extracted_span_context), "trace-12345")
  assert_eq(SpanContext::span_id(extracted_span_context), "span-67890")
  
  // 验证提取的 baggage
  let extracted_baggage = Context::baggage(extracted_context)
  assert_true(Baggage::get(extracted_baggage, "service.version").is_some())
  assert_true(Baggage::get(extracted_baggage, "deployment.environment").is_some())
  assert_true(Baggage::get(extracted_baggage, "region").is_some())
  
  let service_version = Baggage::get(extracted_baggage, "service.version").unwrap()
  assert_eq(service_version.value, "1.2.3")
  
  // 测试跨服务传播模拟
  let service_a_carrier = TextMapCarrier::new()
  TextMapPropagator::inject(propagator, context_with_baggage, service_a_carrier)
  
  // 模拟服务A到服务B的传播
  let service_b_carrier = TextMapCarrier::new()
  for (key, value) in service_a_carrier.entries() {
    TextMapCarrier::put(service_b_carrier, key, value)
  }
  
  // 服务B添加自己的 baggage
  let service_b_context = TextMapPropagator::extract(propagator, service_b_carrier)
  let service_b_baggage = Context::baggage(service_b_context)
  let updated_baggage = Baggage::builder()
    .from_baggage(service_b_baggage)
    .put("service.name", "service-b")
    .put("service.instance.id", "instance-123")
    .build()
  
  let service_b_context_with_baggage = Context::with_baggage(service_b_context, updated_baggage)
  
  // 模拟服务B到服务C的传播
  let service_c_carrier = TextMapCarrier::new()
  TextMapPropagator::inject(propagator, service_b_context_with_baggage, service_c_carrier)
  
  // 验证传播到服务C的上下文
  let service_c_context = TextMapPropagator::extract(propagator, service_c_carrier)
  let service_c_baggage = Context::baggage(service_c_context)
  
  // 验证原始 baggage 仍然存在
  assert_true(Baggage::get(service_c_baggage, "service.version").is_some())
  assert_true(Baggage::get(service_c_baggage, "deployment.environment").is_some())
  assert_true(Baggage::get(service_c_baggage, "region").is_some())
  
  // 验证服务B添加的 baggage 也存在
  assert_true(Baggage::get(service_c_baggage, "service.name").is_some())
  assert_true(Baggage::get(service_c_baggage, "service.instance.id").is_some())
  
  let service_name = Baggage::get(service_c_baggage, "service.name").unwrap()
  assert_eq(service_name.value, "service-b")
}

// 测试7: 遥测数据批处理测试
test "遥测数据批处理和优化" {
  // 创建批处理器
  let batch_processor = BatchProcessor::new(BatchConfig {
    max_batch_size: 10,
    max_export_timeout_ms: 5000,
    max_export_batch_size: 5,
    scheduled_delay_ms: 1000
  })
  
  // 创建测试数据
  let test_spans = []
  for i in 1..=15 {
    let span = SpanData {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      parent_span_id: if i > 1 { Some("span-" + (i - 1).to_string()) } else { None },
      name: "test.operation." + i.to_string(),
      kind: SpanKind::Internal,
      start_time: 1609459200000000000L + (i as Int64) * 1000000L,
      end_time: 1609459200000000000L + (i as Int64) * 2000000L,
      status: SpanStatus::Ok,
      attributes: [
        ("operation.index", i.to_string()),
        ("operation.type", "test")
      ],
      events: []
    }
    test_spans = test_spans.push(span)
  }
  
  // 测试批处理
  let batches = BatchProcessor::process(batch_processor, test_spans)
  
  // 验证批处理结果
  assert_eq(batches.length(), 3)  // 15个span分成3批，每批5个
  assert_eq(batches[0].length(), 5)
  assert_eq(batches[1].length(), 5)
  assert_eq(batches[2].length(), 5)
  
  // 验证每批的内容
  for batch in batches {
    assert_true(batch.length() <= 5)  // 每批不超过5个
    for span in batch {
      assert_true(span.name.contains("test.operation."))
    }
  }
  
  // 测试基于时间的批处理
  let time_based_spans = []
  for i in 1..=5 {
    let span = SpanData {
      trace_id: "time-trace-" + i.to_string(),
      span_id: "time-span-" + i.to_string(),
      parent_span_id: None,
      name: "time.test.operation." + i.to_string(),
      kind: SpanKind::Internal,
      start_time: 1609459200000000000L + (i as Int64) * 1000000000L,  // 每个span间隔1秒
      end_time: 1609459200000000000L + (i as Int64) * 1000000000L + 1000000L,
      status: SpanStatus::Ok,
      attributes: [("time.index", i.to_string())],
      events: []
    }
    time_based_spans = time_based_spans.push(span)
  }
  
  // 设置时间窗口为2秒
  let time_batch_processor = BatchProcessor::new(BatchConfig {
    max_batch_size: 10,
    max_export_timeout_ms: 5000,
    max_export_batch_size: 3,
    scheduled_delay_ms: 2000  // 2秒
  })
  
  let time_batches = BatchProcessor::process_by_time_window(time_batch_processor, time_based_spans, 2000L)
  
  // 验证时间批处理结果
  assert_eq(time_batches.length(), 3)  // 5个span按2秒窗口分成3批
  assert_eq(time_batches[0].length(), 2)  // 前2秒内的span
  assert_eq(time_batches[1].length(), 2)  // 接下来2秒内的span
  assert_eq(time_batches[2].length(), 1)  // 最后1秒内的span
  
  // 测试批处理优化
  let optimization_config = OptimizationConfig {
    deduplicate_attributes: true,
    compress_common_attributes: true,
    merge_similar_spans: true,
    similarity_threshold: 0.8
  }
  
  let optimized_batches = BatchProcessor::optimize_batches(batch_processor, batches, optimization_config)
  
  // 验证优化结果
  assert_eq(optimized_batches.length(), batches.length())  // 批数量不变
  
  // 测试批处理指标
  let metrics = BatchProcessor::get_metrics(batch_processor)
  assert_eq(metrics.total_spans_processed, 15)
  assert_eq(metrics.total_batches_created, 3)
  assert_eq(metrics.average_batch_size, 5.0)
  assert_eq(metrics.max_batch_size, 5)
  assert_eq(metrics.min_batch_size, 5)
}

// 测试8: 异常恢复和容错测试
test "异常恢复和容错机制" {
  // 创建容错配置
  let fault_tolerance_config = FaultToleranceConfig {
    max_retries: 3,
    initial_retry_delay_ms: 1000,
    max_retry_delay_ms: 10000,
    retry_multiplier: 2.0,
    circuit_breaker_failure_threshold: 5,
    circuit_breaker_recovery_timeout_ms: 30000,
    timeout_ms: 5000
  }
  
  // 创建容错处理器
  let fault_handler = FaultToleranceHandler::new(fault_tolerance_config)
  
  // 测试重试机制
  let mut retry_attempts = 0
  let retry_result = FaultToleranceHandler::execute_with_retry(fault_handler, fn() {
    retry_attempts = retry_attempts + 1
    if retry_attempts < 3 {
      Err(TelemetryError::NetworkError("Connection timeout"))
    } else {
      Ok("Success after retries")
    }
  })
  
  assert_eq(retry_attempts, 3)
  match retry_result {
    Ok(value) => assert_eq(value, "Success after retries"),
    Err(_) => assert_true(false)
  }
  
  // 测试断路器
  let mut circuit_breaker_failures = 0
  let circuit_breaker_result = FaultToleranceHandler::execute_with_circuit_breaker(fault_handler, "test.service", fn() {
    circuit_breaker_failures = circuit_breaker_failures + 1
    if circuit_breaker_failures <= 6 {  // 超过失败阈值5
      Err(TelemetryError::ServiceUnavailable("Service down"))
    } else {
      Ok("Service recovered")
    }
  })
  
  // 前5次应该失败，第6次应该触发断路器
  assert_eq(circuit_breaker_failures, 6)
  match circuit_breaker_result {
    Err(TelemetryError::CircuitBreakerOpen(_)) => assert_true(true),
    _ => assert_true(false)
  }
  
  // 测试超时机制
  let timeout_result = FaultToleranceHandler::execute_with_timeout(fault_handler, fn() {
    // 模拟长时间运行的操作
    let mut counter = 0
    while counter < 1000000 {
      counter = counter + 1
    }
    "Operation completed"
  })
  
  match timeout_result {
    Err(TelemetryError::Timeout(_)) => assert_true(true),
    _ => assert_true(false)
  }
  
  // 测试舱壁隔离模式
  let bulkhead_config = BulkheadConfig {
    max_concurrent_calls: 3,
    max_wait_duration_ms: 1000
  }
  
  let bulkhead_handler = FaultToleranceHandler::with_bulkhead(fault_handler, bulkhead_config)
  
  // 模拟并发调用
  let mut concurrent_calls = 0
  let mut rejected_calls = 0
  
  for i in 1..=5 {
    let result = FaultToleranceHandler::execute_with_bulkhead(bulkhead_handler, fn() {
      concurrent_calls = concurrent_calls + 1
      // 模拟短时间操作
      let mut counter = 0
      while counter < 100000 {
        counter = counter + 1
      }
      concurrent_calls = concurrent_calls - 1
      "Operation completed"
    })
    
    match result {
      Ok(_) => assert_true(true),
      Err(TelemetryError::BulkheadRejected(_)) => rejected_calls = rejected_calls + 1,
      _ => assert_true(false)
    }
  }
  
  // 由于最大并发数为3，5个调用中应该有一些被拒绝
  assert_true(rejected_calls >= 1)
  assert_true(concurrent_calls <= 3)
  
  // 测试降级机制
  let fallback_result = FaultToleranceHandler::execute_with_fallback(fault_handler, fn() {
    Err(TelemetryError::ServiceUnavailable("Primary service down"))
  }, fn() {
    "Fallback response"
  })
  
  match fallback_result {
    Ok(value) => assert_eq(value, "Fallback response"),
    _ => assert_true(false)
  }
  
  // 测试组合容错策略
  let combined_result = FaultToleranceHandler::execute_with_combined_strategies(
    fault_handler,
    "combined.test.service",
    fn() {
      Err(TelemetryError::NetworkError("Network failure"))
    },
    fn() {
      "Combined fallback result"
    }
  )
  
  match combined_result {
    Ok(value) => assert_eq(value, "Combined fallback result"),
    _ => assert_true(false)
  }
  
  // 测试容错指标
  let metrics = FaultToleranceHandler::get_metrics(fault_handler)
  assert_true(metrics.total_calls > 0)
  assert_true(metrics.successful_calls >= 0)
  assert_true(metrics.failed_calls > 0)
  assert_true(metrics.retry_attempts > 0)
  assert_true(metrics.circuit_breaker_opens >= 0)
  assert_true(metrics.timeouts >= 0)
}