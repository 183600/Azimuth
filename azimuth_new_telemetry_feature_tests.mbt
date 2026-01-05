// Azimuth New Telemetry Feature Tests
// 新的遥测特性测试用例，专注于增强功能和边缘场景

// 测试1: 遥测数据持久化
test "遥测数据持久化测试" {
  // 创建持久化管理器
  let persistence_manager = TelemetryPersistenceManager::new({
    storage_path: "/tmp/azimuth_telemetry",
    max_file_size: 10485760,  // 10MB
    rotation_policy: "daily",
    compression_enabled: true
  })
  
  // 创建测试遥测数据
  let test_spans = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "http.get",
      start_time: 1640995200000,
      end_time: 1640995200250,
      status: "ok",
      attributes: [
        ("http.method", "GET"),
        ("http.url", "/api/users"),
        ("http.status_code", "200")
      ]
    },
    {
      trace_id: "trace-002",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "database.query",
      start_time: 1640995200100,
      end_time: 1640995200180,
      status: "ok",
      attributes: [
        ("db.type", "postgresql"),
        ("db.statement", "SELECT * FROM users"),
        ("db.rows", "15")
      ]
    },
    {
      trace_id: "trace-003",
      span_id: "span-003",
      parent_span_id: None,
      operation_name: "cache.get",
      start_time: 1640995200200,
      end_time: 1640995200220,
      status: "error",
      attributes: [
        ("cache.key", "user:123"),
        ("cache.hit", "false"),
        ("error.type", "timeout")
      ]
    }
  ]
  
  // 持久化测试数据
  for span in test_spans {
    let result = TelemetryPersistenceManager::persist(persistence_manager, span)
    assert_true(result.success)
  }
  
  // 验证数据持久化
  let persisted_spans = TelemetryPersistenceManager::query(persistence_manager, {
    start_time: 1640995000000,
    end_time: 1640995300000,
    filters: []
  })
  
  assert_eq(persisted_spans.length(), 3)
  
  // 验证特定span的持久化
  let http_span = persisted_spans.find(fn(s) { s.operation_name == "http.get" })
  match http_span {
    Some(span) => {
      assert_eq(span.trace_id, "trace-001")
      assert_eq(span.status, "ok")
      assert_eq(span.attributes.find(fn(a) { a.0 == "http.status_code" }), Some(("http.status_code", "200")))
    }
    None => assert_true(false)
  }
  
  // 验证错误span的持久化
  let error_span = persisted_spans.find(fn(s) { s.status == "error" })
  match error_span {
    Some(span) => {
      assert_eq(span.operation_name, "cache.get")
      assert_eq(span.trace_id, "trace-003")
    }
    None => assert_true(false)
  }
  
  // 测试按条件查询
  let filtered_spans = TelemetryPersistenceManager::query(persistence_manager, {
    start_time: 1640995000000,
    end_time: 1640995300000,
    filters: [
      ("status", "ok")
    ]
  })
  
  assert_eq(filtered_spans.length(), 2)
  
  // 测试时间范围查询
  let time_filtered_spans = TelemetryPersistenceManager::query(persistence_manager, {
    start_time: 1640995200150,
    end_time: 1640995200250,
    filters: []
  })
  
  assert_eq(time_filtered_spans.length(), 2)
}

// 测试2: 遥测数据聚合和分析
test "遥测数据聚合和分析测试" {
  // 创建数据分析器
  let data_analyzer = TelemetryDataAnalyzer::new({
    aggregation_window: 60000,  // 1分钟
    percentiles: [50, 90, 95, 99],
    group_by_attributes: ["service.name", "operation.name"]
  })
  
  // 创建不同服务的测试数据
  let test_data = []
  
  // 服务A的数据
  for i in 0..=49 {
    let span = {
      trace_id: "trace-a-" + i.to_string(),
      span_id: "span-a-" + i.to_string(),
      service_name: "service-a",
      operation_name: if i % 2 == 0 { "operation.x" } else { "operation.y" },
      start_time: 1640995200000 + i * 100,
      duration: 100 + (i % 50) * 10,
      status: if i % 20 == 0 { "error" } else { "ok" }
    }
    test_data = test_data.push(span)
  }
  
  // 服务B的数据
  for i in 0..=49 {
    let span = {
      trace_id: "trace-b-" + i.to_string(),
      span_id: "span-b-" + i.to_string(),
      service_name: "service-b",
      operation_name: if i % 3 == 0 { "operation.p" } else { "operation.q" },
      start_time: 1640995200000 + i * 120,
      duration: 150 + (i % 40) * 15,
      status: if i % 25 == 0 { "error" } else { "ok" }
    }
    test_data = test_data.push(span)
  }
  
  // 分析数据
  let analysis_result = TelemetryDataAnalyzer::analyze(data_analyzer, test_data)
  
  // 验证总体统计
  assert_eq(analysis_result.total_spans, 100)
  assert_eq(analysis_result.error_count, 9)  // 2 from service-a + 2 from service-b
  assert_eq(analysis_result.success_count, 91)
  assert_eq(analysis_result.error_rate, 0.09)
  
  // 验证服务级别统计
  let service_stats = analysis_result.service_statistics
  assert_eq(service_stats.length(), 2)
  
  let service_a = service_stats.find(fn(s) { s.service_name == "service-a" })
  match service_a {
    Some(stats) => {
      assert_eq(stats.span_count, 50)
      assert_eq(stats.error_count, 2)
      assert_eq(stats.error_rate, 0.04)
      assert_true(stats.average_duration > 100)
      assert_true(stats.average_duration < 600)
    }
    None => assert_true(false)
  }
  
  let service_b = service_stats.find(fn(s) { s.service_name == "service-b" })
  match service_b {
    Some(stats) => {
      assert_eq(stats.span_count, 50)
      assert_eq(stats.error_count, 2)
      assert_eq(stats.error_rate, 0.04)
      assert_true(stats.average_duration > 150)
      assert_true(stats.average_duration < 750)
    }
    None => assert_true(false)
  }
  
  // 验证操作级别统计
  let operation_stats = analysis_result.operation_statistics
  assert_true(operation_stats.length() >= 4)  // operation.x, operation.y, operation.p, operation.q
  
  // 验证百分位数计算
  let duration_percentiles = analysis_result.duration_percentiles
  assert_true(duration_percentiles.contains_key("p50"))
  assert_true(duration_percentiles.contains_key("p90"))
  assert_true(duration_percentiles.contains_key("p95"))
  assert_true(duration_percentiles.contains_key("p99"))
  
  // 验证百分位数顺序
  let p50 = duration_percentiles["p50"]
  let p90 = duration_percentiles["p90"]
  let p95 = duration_percentiles["p95"]
  let p99 = duration_percentiles["p99"]
  
  assert_true(p50 <= p90)
  assert_true(p90 <= p95)
  assert_true(p95 <= p99)
}

// 测试3: 遥测数据实时监控和告警
test "遥测数据实时监控和告警测试" {
  // 创建监控管理器
  let monitoring_manager = TelemetryMonitoringManager::new({
    alert_rules: [
      {
        name: "high_error_rate",
        condition: "error_rate > 0.1",
        window: 300000,  // 5分钟
        severity: "warning",
        cooldown: 600000  // 10分钟冷却
      },
      {
        name: "high_latency",
        condition: "p95_latency > 1000",
        window: 300000,
        severity: "critical",
        cooldown: 300000  // 5分钟冷却
      },
      {
        name: "low_throughput",
        condition: "throughput < 100",
        window: 600000,  // 10分钟
        severity: "info",
        cooldown: 900000  // 15分钟冷却
      }
    ],
    notification_channels: ["email", "slack"]
  })
  
  // 启动监控
  TelemetryMonitoringManager::start(monitoring_manager)
  
  // 创建测试数据流
  let data_stream = TelemetryDataStream::new()
  
  // 场景1: 正常情况
  for i in 0..=99 {
    let span = {
      trace_id: "trace-normal-" + i.to_string(),
      span_id: "span-normal-" + i.to_string(),
      service_name: "api-service",
      operation_name: "process_request",
      start_time: 1640995200000 + i * 100,
      duration: 200 + (i % 100) * 5,
      status: "ok"
    }
    TelemetryDataStream::add_span(data_stream, span)
  }
  
  // 处理数据流
  TelemetryMonitoringManager::process_stream(monitoring_manager, data_stream)
  
  // 检查告警状态 - 不应该有告警
  let alerts = TelemetryMonitoringManager::get_active_alerts(monitoring_manager)
  assert_eq(alerts.length(), 0)
  
  // 场景2: 高错误率情况
  let error_data_stream = TelemetryDataStream::new()
  for i in 0..=99 {
    let span = {
      trace_id: "trace-error-" + i.to_string(),
      span_id: "span-error-" + i.to_string(),
      service_name: "api-service",
      operation_name: "process_request",
      start_time: 1640995300000 + i * 100,
      duration: 200 + (i % 100) * 5,
      status: if i < 15 { "error" } else { "ok" }  // 15%错误率
    }
    TelemetryDataStream::add_span(error_data_stream, span)
  }
  
  // 处理错误数据流
  TelemetryMonitoringManager::process_stream(monitoring_manager, error_data_stream)
  
  // 检查告警状态 - 应该有高错误率告警
  let error_alerts = TelemetryMonitoringManager::get_active_alerts(monitoring_manager)
  assert_eq(error_alerts.length(), 1)
  assert_eq(error_alerts[0].rule_name, "high_error_rate")
  assert_eq(error_alerts[0].severity, "warning")
  
  // 场景3: 高延迟情况
  let latency_data_stream = TelemetryDataStream::new()
  for i in 0..=99 {
    let span = {
      trace_id: "trace-latency-" + i.to_string(),
      span_id: "span-latency-" + i.to_string(),
      service_name: "api-service",
      operation_name: "slow_operation",
      start_time: 1640995400000 + i * 100,
      duration: if i >= 95 { 1500 } else { 300 + i * 10 },  // 最后几个span延迟很高
      status: "ok"
    }
    TelemetryDataStream::add_span(latency_data_stream, span)
  }
  
  // 处理高延迟数据流
  TelemetryMonitoringManager::process_stream(monitoring_manager, latency_data_stream)
  
  // 检查告警状态 - 应该有高延迟告警
  let latency_alerts = TelemetryMonitoringManager::get_active_alerts(monitoring_manager)
  assert_true(latency_alerts.length() >= 1)
  
  let high_latency_alert = latency_alerts.find(fn(a) { a.rule_name == "high_latency" })
  match high_latency_alert {
    Some(alert) => {
      assert_eq(alert.severity, "critical")
    }
    None => assert_true(false)
  }
  
  // 场景4: 低吞吐量情况
  let low_throughput_stream = TelemetryDataStream::new()
  for i in 0..=9 {
    let span = {
      trace_id: "trace-low-" + i.to_string(),
      span_id: "span-low-" + i.to_string(),
      service_name: "api-service",
      operation_name: "rare_operation",
      start_time: 1640995500000 + i * 60000,  // 每分钟一个请求
      duration: 200,
      status: "ok"
    }
    TelemetryDataStream::add_span(low_throughput_stream, span)
  }
  
  // 处理低吞吐量数据流
  TelemetryMonitoringManager::process_stream(monitoring_manager, low_throughput_stream)
  
  // 检查告警状态 - 应该有低吞吐量告警
  let throughput_alerts = TelemetryMonitoringManager::get_active_alerts(monitoring_manager)
  assert_true(throughput_alerts.length() >= 1)
  
  let low_throughput_alert = throughput_alerts.find(fn(a) { a.rule_name == "low_throughput" })
  match low_throughput_alert {
    Some(alert) => {
      assert_eq(alert.severity, "info")
    }
    None => assert_true(false)
  }
  
  // 测试告警冷却期
  let cooldown_data_stream = TelemetryDataStream::new()
  for i in 0..=19 {
    let span = {
      trace_id: "trace-cooldown-" + i.to_string(),
      span_id: "span-cooldown-" + i.to_string(),
      service_name: "api-service",
      operation_name: "process_request",
      start_time: 1640995600000 + i * 100,
      duration: 200,
      status: "error"  // 100%错误率，但应该在冷却期内
    }
    TelemetryDataStream::add_span(cooldown_data_stream, span)
  }
  
  // 处理冷却期数据流
  TelemetryMonitoringManager::process_stream(monitoring_manager, cooldown_data_stream)
  
  // 检查告警状态 - 不应该有新的高错误率告警（在冷却期内）
  let cooldown_alerts = TelemetryMonitoringManager::get_active_alerts(monitoring_manager)
  let new_error_alerts = cooldown_alerts.filter(fn(a) { 
    a.rule_name == "high_error_rate" and 
    a.triggered_at > 1640995600000 
  })
  assert_eq(new_error_alerts.length(), 0)
  
  // 停止监控
  TelemetryMonitoringManager::stop(monitoring_manager)
}

// 测试4: 遥测数据分布式追踪
test "遥测数据分布式追踪测试" {
  // 创建分布式追踪管理器
  let distributed_tracer = DistributedTracer::new({
    service_name: "order-service",
    sampling_rate: 1.0,  // 100%采样
    propagators: ["trace-context", "baggage"],
    max_links: 10
  })
  
  // 创建根span
  let root_span = DistributedTracer::start_span(distributed_tracer, {
    operation_name: "process_order",
    kind: "server",
    attributes: [
      ("order.id", "order-12345"),
      ("user.id", "user-67890"),
      ("order.amount", "99.99")
    ]
  })
  
  // 验证根span属性
  assert_eq(root_span.operation_name, "process_order")
  assert_eq(root_span.kind, "server")
  assert_eq(root_span.parent_span_id, None)
  assert_true(root_span.trace_id.length() > 0)
  assert_true(root_span.span_id.length() > 0)
  assert_eq(root_span.attributes.find(fn(a) { a.0 == "order.id" }), Some(("order.id", "order-12345")))
  
  // 创建子span1: 库存检查
  let inventory_span = DistributedTracer::start_span(distributed_tracer, {
    operation_name: "check_inventory",
    kind: "client",
    parent: root_span,
    attributes: [
      ("product.id", "prod-111"),
      ("quantity", "2")
    ]
  })
  
  // 验证子span1属性
  assert_eq(inventory_span.operation_name, "check_inventory")
  assert_eq(inventory_span.kind, "client")
  assert_eq(inventory_span.parent_span_id, Some(root_span.span_id))
  assert_eq(inventory_span.trace_id, root_span.trace_id)
  
  // 结束子span1
  DistributedTracer::end_span(distributed_tracer, inventory_span, {
    status: "ok",
    attributes: [
      ("inventory.available", "true"),
      ("inventory.count", "50")
    ]
  })
  
  // 创建子span2: 支付处理
  let payment_span = DistributedTracer::start_span(distributed_tracer, {
    operation_name: "process_payment",
    kind: "client",
    parent: root_span,
    attributes: [
      ("payment.method", "credit_card"),
      ("payment.amount", "99.99")
    ]
  })
  
  // 验证子span2属性
  assert_eq(payment_span.operation_name, "process_payment")
  assert_eq(payment_span.kind, "client")
  assert_eq(payment_span.parent_span_id, Some(root_span.span_id))
  assert_eq(payment_span.trace_id, root_span.trace_id)
  
  // 创建子span2.1: 支付网关调用
  let gateway_span = DistributedTracer::start_span(distributed_tracer, {
    operation_name: "gateway_request",
    kind: "client",
    parent: payment_span,
    attributes: [
      ("gateway.provider", "stripe"),
      ("gateway.endpoint", "/api/v1/charges")
    ]
  })
  
  // 验证子span2.1属性
  assert_eq(gateway_span.operation_name, "gateway_request")
  assert_eq(gateway_span.kind, "client")
  assert_eq(gateway_span.parent_span_id, Some(payment_span.span_id))
  assert_eq(gateway_span.trace_id, root_span.trace_id)
  
  // 结束子span2.1
  DistributedTracer::end_span(distributed_tracer, gateway_span, {
    status: "ok",
    attributes: [
      ("http.status_code", "200"),
      ("payment.transaction_id", "txn-abc123")
    ]
  })
  
  // 结束子span2
  DistributedTracer::end_span(distributed_tracer, payment_span, {
    status: "ok",
    attributes: [
      ("payment.confirmed", "true"),
      ("payment.id", "pay-def456")
    ]
  })
  
  // 创建子span3: 订单确认
  let confirmation_span = DistributedTracer::start_span(distributed_tracer, {
    operation_name: "send_confirmation",
    kind: "client",
    parent: root_span,
    attributes: [
      ("notification.type", "email"),
      ("recipient", "user@example.com")
    ]
  })
  
  // 结束子span3
  DistributedTracer::end_span(distributed_tracer, confirmation_span, {
    status: "ok",
    attributes: [
      ("notification.sent", "true"),
      ("notification.id", "notif-ghi789")
    ]
  })
  
  // 结束根span
  DistributedTracer::end_span(distributed_tracer, root_span, {
    status: "ok",
    attributes: [
      ("order.status", "completed"),
      ("order.total_amount", "99.99")
    ]
  })
  
  // 获取追踪树
  let trace_tree = DistributedTracer::get_trace_tree(distributed_tracer, root_span.trace_id)
  
  // 验证追踪树结构
  assert_eq(trace_tree.trace_id, root_span.trace_id)
  assert_eq(trace_tree.root_span.operation_name, "process_order")
  assert_eq(trace_tree.spans.length(), 5)  // root + 4子span
  
  // 验证父子关系
  let root_tree_span = trace_tree.spans.find(fn(s) { s.span_id == root_span.span_id })
  match root_tree_span {
    Some(span) => {
      assert_eq(span.parent_span_id, None)
      assert_eq(span.children.length(), 3)  // inventory, payment, confirmation
    }
    None => assert_true(false)
  }
  
  let payment_tree_span = trace_tree.spans.find(fn(s) { s.span_id == payment_span.span_id })
  match payment_tree_span {
    Some(span) => {
      assert_eq(span.parent_span_id, Some(root_span.span_id))
      assert_eq(span.children.length(), 1)  // gateway
    }
    None => assert_true(false)
  }
  
  // 验证追踪上下文传播
  let trace_context = DistributedTracer::extract_context(distributed_tracer, {
    headers: [
      ("traceparent", "00-" + root_span.trace_id + "-" + root_span.span_id + "-01"),
      ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
    ]
  })
  
  assert_eq(trace_context.trace_id, root_span.trace_id)
  assert_eq(trace_context.span_id, root_span.span_id)
  
  // 验证baggage传播
  let baggage_items = DistributedTracer::get_baggage(distributed_tracer, trace_context)
  assert_true(baggage_items.contains_key("rojo"))
  assert_true(baggage_items.contains_key("congo"))
}

// 测试5: 遥测数据异常检测
test "遥测数据异常检测测试" {
  // 创建异常检测器
  let anomaly_detector = TelemetryAnomalyDetector::new({
    algorithms: ["statistical", "ml"],
    sensitivity: 0.8,
    training_window: 86400000,  // 24小时
    detection_window: 300000    // 5分钟
  })
  
  // 创建正常基线数据
  let baseline_data = []
  let base_timestamp = 1640995200000
  
  // 生成24小时的正常数据（每小时100个数据点）
  for hour in 0..=23 {
    for minute in 0..=99 {
      let timestamp = base_timestamp + hour * 3600000 + minute * 36000
      let latency = 200 + (minute % 20) * 10  // 200-400ms的正常延迟
      let error_rate = if minute % 50 == 0 { 0.02 } else { 0.0 }  // 2%的错误率
      
      let data_point = {
        timestamp: timestamp,
        service_name: "api-service",
        operation_name: "process_request",
        latency: latency,
        error_rate: error_rate,
        throughput: 100 + (minute % 10) * 5  // 100-145的吞吐量
      }
      
      baseline_data = baseline_data.push(data_point)
    }
  }
  
  // 训练异常检测模型
  let training_result = TelemetryAnomalyDetector::train(anomaly_detector, baseline_data)
  assert_true(training_result.success)
  assert_true(training_result.model_accuracy > 0.9)
  
  // 场景1: 正常数据 - 不应该检测到异常
  let normal_data = []
  for i in 0..=9 {
    let timestamp = base_timestamp + 86400000 + i * 60000  // 基线后的第一天
    let latency = 250 + (i % 15) * 8  // 250-370ms的正常延迟
    let error_rate = if i % 25 == 0 { 0.02 } else { 0.0 }  // 2%的错误率
    
    let data_point = {
      timestamp: timestamp,
      service_name: "api-service",
      operation_name: "process_request",
      latency: latency,
      error_rate: error_rate,
      throughput: 120 + (i % 8) * 6  // 120-168的吞吐量
    }
    
    normal_data = normal_data.push(data_point)
  }
  
  let normal_result = TelemetryAnomalyDetector::detect(anomaly_detector, normal_data)
  assert_eq(normal_result.anomalies.length(), 0)
  assert_true(normal_result.confidence > 0.8)
  
  // 场景2: 延迟异常 - 应该检测到异常
  let latency_anomaly_data = []
  for i in 0..=9 {
    let timestamp = base_timestamp + 86400000 + 600000 + i * 60000  // 10分钟后
    let latency = if i >= 5 { 2000 + i * 100 } else { 250 + (i % 15) * 8 }  // 后半部分延迟异常高
    let error_rate = 0.0
    
    let data_point = {
      timestamp: timestamp,
      service_name: "api-service",
      operation_name: "process_request",
      latency: latency,
      error_rate: error_rate,
      throughput: 120 + (i % 8) * 6
    }
    
    latency_anomaly_data = latency_anomaly_data.push(data_point)
  }
  
  let latency_anomaly_result = TelemetryAnomalyDetector::detect(anomaly_detector, latency_anomaly_data)
  assert_true(latency_anomaly_result.anomalies.length() > 0)
  
  let latency_anomaly = latency_anomaly_result.anomalies.find(fn(a) { a.type == "latency_spike" })
  match latency_anomaly {
    Some(anomaly) => {
      assert_true(anomaly.severity >= 0.7)
      assert_true(anomaly.confidence > 0.8)
      assert_true(anomaly.timestamp >= base_timestamp + 86400000 + 900000)  // 异常开始时间
    }
    None => assert_true(false)
  }
  
  // 场景3: 错误率异常 - 应该检测到异常
  let error_anomaly_data = []
  for i in 0..=9 {
    let timestamp = base_timestamp + 86400000 + 1200000 + i * 60000  // 20分钟后
    let latency = 250 + (i % 15) * 8
    let error_rate = if i >= 3 and i <= 7 { 0.25 } else { 0.02 }  // 中间5分钟错误率异常高
    
    let data_point = {
      timestamp: timestamp,
      service_name: "api-service",
      operation_name: "process_request",
      latency: latency,
      error_rate: error_rate,
      throughput: 120 + (i % 8) * 6
    }
    
    error_anomaly_data = error_anomaly_data.push(data_point)
  }
  
  let error_anomaly_result = TelemetryAnomalyDetector::detect(anomaly_detector, error_anomaly_data)
  assert_true(error_anomaly_result.anomalies.length() > 0)
  
  let error_anomaly = error_anomaly_result.anomalies.find(fn(a) { a.type == "error_rate_spike" })
  match error_anomaly {
    Some(anomaly) => {
      assert_true(anomaly.severity >= 0.8)
      assert_true(anomaly.confidence > 0.8)
      assert_true(anomaly.timestamp >= base_timestamp + 86400000 + 1380000)  // 异常开始时间
      assert_true(anomaly.timestamp <= base_timestamp + 86400000 + 1620000)  // 异常结束时间
    }
    None => assert_true(false)
  }
  
  // 场景4: 吞吐量异常 - 应该检测到异常
  let throughput_anomaly_data = []
  for i in 0..=9 {
    let timestamp = base_timestamp + 86400000 + 1800000 + i * 60000  // 30分钟后
    let latency = 250 + (i % 15) * 8
    let error_rate = 0.02
    let throughput = if i >= 6 { 20 } else { 120 + (i % 8) * 6 }  // 后半部分吞吐量异常低
    
    let data_point = {
      timestamp: timestamp,
      service_name: "api-service",
      operation_name: "process_request",
      latency: latency,
      error_rate: error_rate,
      throughput: throughput
    }
    
    throughput_anomaly_data = throughput_anomaly_data.push(data_point)
  }
  
  let throughput_anomaly_result = TelemetryAnomalyDetector::detect(anomaly_detector, throughput_anomaly_data)
  assert_true(throughput_anomaly_result.anomalies.length() > 0)
  
  let throughput_anomaly = throughput_anomaly_result.anomalies.find(fn(a) { a.type == "throughput_drop" })
  match throughput_anomaly {
    Some(anomaly) => {
      assert_true(anomaly.severity >= 0.6)
      assert_true(anomaly.confidence > 0.7)
      assert_true(anomaly.timestamp >= base_timestamp + 86400000 + 2160000)  // 异常开始时间
    }
    None => assert_true(false)
  }
  
  // 场景5: 多维度异常 - 应该检测到多个异常
  let multi_anomaly_data = []
  for i in 0..=9 {
    let timestamp = base_timestamp + 86400000 + 2400000 + i * 60000  // 40分钟后
    let latency = if i >= 5 { 1500 } else { 250 + (i % 15) * 8 }  // 后半部分延迟高
    let error_rate = if i >= 3 and i <= 7 { 0.15 } else { 0.02 }  // 中间部分错误率高
    let throughput = if i >= 7 { 30 } else { 120 + (i % 8) * 6 }  // 最后部分吞吐量低
    
    let data_point = {
      timestamp: timestamp,
      service_name: "api-service",
      operation_name: "process_request",
      latency: latency,
      error_rate: error_rate,
      throughput: throughput
    }
    
    multi_anomaly_data = multi_anomaly_data.push(data_point)
  }
  
  let multi_anomaly_result = TelemetryAnomalyDetector::detect(anomaly_detector, multi_anomaly_data)
  assert_true(multi_anomaly_result.anomalies.length() >= 2)  // 至少检测到2种异常
  
  // 验证不同类型的异常都被检测到
  let has_latency_anomaly = multi_anomaly_result.anomalies.any(fn(a) { a.type == "latency_spike" })
  let has_error_anomaly = multi_anomaly_result.anomalies.any(fn(a) { a.type == "error_rate_spike" })
  let has_throughput_anomaly = multi_anomaly_result.anomalies.any(fn(a) { a.type == "throughput_drop" })
  
  assert_true(has_latency_anomaly or has_error_anomaly or has_throughput_anomaly)
  
  // 测试异常检测模型更新
  let model_update_result = TelemetryAnomalyDetector::update_model(anomaly_detector, normal_data)
  assert_true(model_update_result.success)
  assert_true(model_update_result.model_version > training_result.model_version)
}

// 测试6: 遥测数据资源限制和节流
test "遥测数据资源限制和节流测试" {
  // 创建资源管理器
  let resource_manager = TelemetryResourceManager::new({
    max_spans_per_second: 1000,
    max_attributes_per_span: 128,
    max_events_per_span: 32,
    max_links_per_span: 32,
    max_span_length: 32768,  // 32KB
    memory_limit: 104857600,  // 100MB
    storage_limit: 1073741824  // 1GB
  })
  
  // 测试span生成速率限制
  let span_generator = TelemetrySpanGenerator::new({
    service_name: "test-service",
    operation_name: "test-operation"
  })
  
  let generated_spans = []
  let start_time = Time::now()
  
  // 尝试生成大量span（超过速率限制）
  for i in 0..=1999 {  // 尝试生成2000个span，但限制是1000/秒
    let span = TelemetrySpanGenerator::generate(span_generator, {
      span_id: "span-" + i.to_string(),
      trace_id: "trace-" + (i % 100).to_string(),
      attributes: [
        ("iteration", i.to_string())
      ]
    })
    
    let result = TelemetryResourceManager::process_span(resource_manager, span)
    if result.accepted {
      generated_spans = generated_spans.push(span)
    }
  }
  
  let processing_time = Time::now() - start_time
  
  // 验证速率限制生效
  let actual_rate = generated_spans.length().to_float() / processing_time.to_float() * 1000.0
  assert_true(actual_rate <= 1000.0 * 1.1)  // 允许10%误差
  
  // 测试属性数量限制
  let many_attributes = []
  for i in 0..=150 {  // 尝试添加150个属性，但限制是128
    many_attributes = many_attributes.push(("attr." + i.to_string(), "value." + i.to_string()))
  }
  
  let span_with_many_attrs = TelemetrySpanGenerator::generate(span_generator, {
    span_id: "span-many-attrs",
    trace_id: "trace-many-attrs",
    attributes: many_attributes
  })
  
  let attr_result = TelemetryResourceManager::process_span(resource_manager, span_with_many_attrs)
  assert_true(attr_result.accepted)  // span应该被接受，但属性应该被截断
  assert_true(attr_result.truncated_attributes)  // 属性应该被截断
  assert_true(span_with_many_attrs.attributes.length() <= 128)  // 实际属性数应该不超过限制
  
  // 测试事件数量限制
  let many_events = []
  for i in 0..=40 {  // 尝试添加40个事件，但限制是32
    many_events = many_events.push({
      name: "event." + i.to_string(),
      timestamp: Time::now(),
      attributes: [
        ("event_index", i.to_string())
      ]
    })
  }
  
  let span_with_many_events = TelemetrySpanGenerator::generate(span_generator, {
    span_id: "span-many-events",
    trace_id: "trace-many-events",
    events: many_events
  })
  
  let events_result = TelemetryResourceManager::process_span(resource_manager, span_with_many_events)
  assert_true(events_result.accepted)  // span应该被接受，但事件应该被截断
  assert_true(events_result.truncated_events)  // 事件应该被截断
  assert_true(span_with_many_events.events.length() <= 32)  // 实际事件数应该不超过限制
  
  // 测试span大小限制
  let large_attribute_value = "x".repeat(40000)  // 40KB的属性值
  let large_span = TelemetrySpanGenerator::generate(span_generator, {
    span_id: "span-large",
    trace_id: "trace-large",
    attributes: [
      ("large_attribute", large_attribute_value)
    ]
  })
  
  let large_result = TelemetryResourceManager::process_span(resource_manager, large_span)
  assert_true(large_result.accepted)  // span应该被接受，但应该被截断
  assert_true(large_result.truncated_content)  // 内容应该被截断
  
  // 计算span的序列化大小
  let serialized_size = TelemetryResourceManager::calculate_span_size(resource_manager, large_span)
  assert_true(serialized_size <= 32768)  // 应该不超过32KB
  
  // 测试内存限制
  let memory_usage_before = TelemetryResourceManager::get_memory_usage(resource_manager)
  
  // 生成大量span直到接近内存限制
  let memory_test_spans = []
  for batch in 0..=9 {  // 10个批次
    let batch_spans = []
    for i in 0..=999 {  // 每批1000个span
      let span = TelemetrySpanGenerator::generate(span_generator, {
        span_id: "memory-span-" + batch.to_string() + "-" + i.to_string(),
        trace_id: "memory-trace-" + batch.to_string(),
        attributes: [
          ("batch", batch.to_string()),
          ("index", i.to_string()),
          ("data", "x".repeat(100))  // 每个span约100B
        ]
      })
      
      let result = TelemetryResourceManager::process_span(resource_manager, span)
      if result.accepted {
        batch_spans = batch_spans.push(span)
      }
    }
    
    memory_test_spans = memory_test_spans.concat(batch_spans)
    
    // 检查内存使用
    let current_memory = TelemetryResourceManager::get_memory_usage(resource_manager)
    if current_memory.used > memory_usage_before.limit * 0.9 {  // 如果使用超过90%限制
      break
    }
  }
  
  let memory_usage_after = TelemetryResourceManager::get_memory_usage(resource_manager)
  
  // 验证内存限制被遵守
  assert_true(memory_usage_after.used <= memory_usage_after.limit)
  
  // 测试存储限制
  let storage_usage_before = TelemetryResourceManager::get_storage_usage(resource_manager)
  
  // 模拟存储大量数据
  let storage_test_spans = []
  for i in 0..=99 {  // 100个span
    let span = TelemetrySpanGenerator::generate(span_generator, {
      span_id: "storage-span-" + i.to_string(),
      trace_id: "storage-trace-" + i.to_string(),
      attributes: [
        ("index", i.to_string()),
        ("data", "y".repeat(1000))  // 每个span约1KB
      ]
    })
    
    let result = TelemetryResourceManager::process_span(resource_manager, span)
    if result.accepted {
      storage_test_spans = storage_test_spans.push(span)
    }
  }
  
  // 模拟持久化
  for span in storage_test_spans {
    TelemetryResourceManager::persist_span(resource_manager, span)
  }
  
  let storage_usage_after = TelemetryResourceManager::get_storage_usage(resource_manager)
  
  // 验证存储限制被遵守
  assert_true(storage_usage_after.used <= storage_usage_after.limit)
  
  // 测试资源清理
  let cleanup_result = TelemetryResourceManager::cleanup_resources(resource_manager, {
    older_than: Time::now() - 3600000,  // 1小时前
    keep_at_least: 1000  // 至少保留1000个span
  })
  
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.cleaned_spans > 0)
  
  let memory_usage_after_cleanup = TelemetryResourceManager::get_memory_usage(resource_manager)
  assert_true(memory_usage_after_cleanup.used < memory_usage_after.used)
}

// 测试7: 遥测数据多租户隔离
test "遥测数据多租户隔离测试" {
  // 创建多租户管理器
  let multi_tenant_manager = MultiTenantTelemetryManager::new({
    isolation_level: "strict",  // 严格隔离
    default_quota: {
      max_spans_per_day: 10000,
      max_storage_mb: 100,
      max_data_retention_days: 30
    }
  })
  
  // 创建租户
  let tenants = [
    {
      id: "tenant-a",
      name: "Customer A",
      quota: {
        max_spans_per_day: 50000,
        max_storage_mb: 500,
        max_data_retention_days: 90
      },
      isolation_config: {
        dedicated_storage: true,
        encryption_enabled: true,
        cross_tenant_access: false
      }
    },
    {
      id: "tenant-b",
      name: "Customer B",
      quota: {
        max_spans_per_day: 20000,
        max_storage_mb: 200,
        max_data_retention_days: 60
      },
      isolation_config: {
        dedicated_storage: false,
        encryption_enabled: true,
        cross_tenant_access: false
      }
    },
    {
      id: "tenant-c",
      name: "Customer C",
      quota: {
        max_spans_per_day: 15000,
        max_storage_mb: 150,
        max_data_retention_days: 45
      },
      isolation_config: {
        dedicated_storage: false,
        encryption_enabled: true,
        cross_tenant_access: false
      }
    }
  ]
  
  // 注册租户
  for tenant in tenants {
    let result = MultiTenantTelemetryManager::register_tenant(multi_tenant_manager, tenant)
    assert_true(result.success)
  }
  
  // 验证租户注册
  let registered_tenants = MultiTenantTelemetryManager::list_tenants(multi_tenant_manager)
  assert_eq(registered_tenants.length(), 3)
  
  // 为每个租户生成遥测数据
  let tenant_spans = []
  
  // 租户A的数据
  for i in 0..=99 {
    let span = {
      tenant_id: "tenant-a",
      trace_id: "trace-a-" + i.to_string(),
      span_id: "span-a-" + i.to_string(),
      service_name: "service-a",
      operation_name: "operation-a",
      timestamp: 1640995200000 + i * 1000,
      attributes: [
        ("tenant.data", "data-a-" + i.to_string()),
        ("sensitive", "confidential-a")
      ]
    }
    tenant_spans = tenant_spans.push(span)
  }
  
  // 租户B的数据
  for i in 0..=49 {
    let span = {
      tenant_id: "tenant-b",
      trace_id: "trace-b-" + i.to_string(),
      span_id: "span-b-" + i.to_string(),
      service_name: "service-b",
      operation_name: "operation-b",
      timestamp: 1640995200000 + i * 2000,
      attributes: [
        ("tenant.data", "data-b-" + i.to_string()),
        ("sensitive", "confidential-b")
      ]
    }
    tenant_spans = tenant_spans.push(span)
  }
  
  // 租户C的数据
  for i in 0..=74 {
    let span = {
      tenant_id: "tenant-c",
      trace_id: "trace-c-" + i.to_string(),
      span_id: "span-c-" + i.to_string(),
      service_name: "service-c",
      operation_name: "operation-c",
      timestamp: 1640995200000 + i * 1500,
      attributes: [
        ("tenant.data", "data-c-" + i.to_string()),
        ("sensitive", "confidential-c")
      ]
    }
    tenant_spans = tenant_spans.push(span)
  }
  
  // 处理所有span
  for span in tenant_spans {
    let result = MultiTenantTelemetryManager::process_span(multi_tenant_manager, span)
    assert_true(result.accepted)
  }
  
  // 测试租户隔离查询
  let tenant_a_query = MultiTenantTelemetryManager::query(multi_tenant_manager, {
    tenant_id: "tenant-a",
    start_time: 1640995000000,
    end_time: 1640995400000,
    filters: []
  })
  
  assert_eq(tenant_a_query.spans.length(), 100)
  assert_true(tenant_a_query.spans.all(fn(s) { s.tenant_id == "tenant-a" }))
  
  let tenant_b_query = MultiTenantTelemetryManager::query(multi_tenant_manager, {
    tenant_id: "tenant-b",
    start_time: 1640995000000,
    end_time: 1640995400000,
    filters: []
  })
  
  assert_eq(tenant_b_query.spans.length(), 50)
  assert_true(tenant_b_query.spans.all(fn(s) { s.tenant_id == "tenant-b" }))
  
  let tenant_c_query = MultiTenantTelemetryManager::query(multi_tenant_manager, {
    tenant_id: "tenant-c",
    start_time: 1640995000000,
    end_time: 1640995400000,
    filters: []
  })
  
  assert_eq(tenant_c_query.spans.length(), 75)
  assert_true(tenant_c_query.spans.all(fn(s) { s.tenant_id == "tenant-c" }))
  
  // 测试跨租户访问限制
  let cross_tenant_query = MultiTenantTelemetryManager::query(multi_tenant_manager, {
    tenant_id: "tenant-a",
    start_time: 1640995000000,
    end_time: 1640995400000,
    filters: [],
    allow_cross_tenant: true  // 尝试跨租户查询
  })
  
  assert_eq(cross_tenant_query.spans.length(), 100)  // 应该只返回租户A的数据
  assert_true(cross_tenant_query.spans.all(fn(s) { s.tenant_id == "tenant-a" }))
  
  // 测试租户配额限制
  let tenant_a_quota = MultiTenantTelemetryManager::get_quota_usage(multi_tenant_manager, "tenant-a")
  assert_eq(tenant_a_quota.spans_today, 100)
  assert_true(tenant_a_quota.spans_today <= tenant_a_quota.max_spans_per_day)
  assert_true(tenant_a_quota.storage_used_mb > 0)
  assert_true(tenant_a_quota.storage_used_mb <= tenant_a_quota.max_storage_mb)
  
  // 测试租户数据加密
  let encrypted_span = {
    tenant_id: "tenant-a",
    trace_id: "trace-encrypted",
    span_id: "span-encrypted",
    service_name: "service-a",
    operation_name: "operation-encrypted",
    timestamp: 1640995300000,
    attributes: [
      ("sensitive.data", "very_confidential_information")
    ]
  }
  
  let encrypt_result = MultiTenantTelemetryManager::process_span(multi_tenant_manager, encrypted_span)
  assert_true(encrypt_result.accepted)
  assert_true(encrypt_result.encrypted)
  
  // 验证加密数据
  let encrypted_query = MultiTenantTelemetryManager::query(multi_tenant_manager, {
    tenant_id: "tenant-a",
    start_time: 1640995200000,
    end_time: 1640995400000,
    filters: [
      ("span_id", "span-encrypted")
    ]
  })
  
  assert_eq(encrypted_query.spans.length(), 1)
  let stored_span = encrypted_query.spans[0]
  
  // 验证敏感数据已加密
  let sensitive_attr = stored_span.attributes.find(fn(a) { a.0 == "sensitive.data" })
  match sensitive_attr {
    Some((_, value)) => {
      assert_ne(value, "very_confidential_information")  // 不应该是原始值
      assert_true(value.length() > 0)  // 应该有加密后的值
    }
    None => assert_true(false)
  }
  
  // 测试租户数据隔离清理
  let cleanup_result = MultiTenantTelemetryManager::cleanup_tenant_data(multi_tenant_manager, {
    tenant_id: "tenant-b",
    older_than: 1640995200000 - 86400000,  // 1天前
    keep_at_least: 10
  })
  
  assert_true(cleanup_result.success)
  
  // 验证清理后的数据
  let tenant_b_cleanup_query = MultiTenantTelemetryManager::query(multi_tenant_manager, {
    tenant_id: "tenant-b",
    start_time: 0,
    end_time: 1840995200000,  // 很远的未来
    filters: []
  })
  
  assert_true(tenant_b_cleanup_query.spans.length() <= 10)  // 应该只保留10个或更少
}

// 测试8: 遥测数据自定义指标
test "遥测数据自定义指标测试" {
  // 创建自定义指标管理器
  let custom_metrics_manager = CustomMetricsManager::new({
    default_aggregation: "sum",
    default_resolution: 60000,  // 1分钟
    max_metrics_per_tenant: 1000
  })
  
  // 定义自定义指标
  let custom_metrics = [
    {
      name: "user.login.count",
      type: "counter",
      description: "Number of user logins",
      unit: "count",
      aggregation: "sum",
      tags: ["service", "region"]
    },
    {
      name: "order.value",
      type: "gauge",
      description: "Current order value",
      unit: "currency",
      aggregation: "last",
      tags: ["service", "currency"]
    },
    {
      name: "response.time.histogram",
      type: "histogram",
      description: "Response time distribution",
      unit: "ms",
      aggregation: "histogram",
      buckets: [10, 50, 100, 500, 1000, 5000],
      tags: ["service", "endpoint"]
    },
    {
      name: "cpu.utilization",
      type: "gauge",
      description: "CPU utilization percentage",
      unit: "percent",
      aggregation: "avg",
      tags: ["host", "region"]
    }
  ]
  
  // 注册自定义指标
  for metric in custom_metrics {
    let result = CustomMetricsManager::register_metric(custom_metrics_manager, metric)
    assert_true(result.success)
  }
  
  // 验证指标注册
  let registered_metrics = CustomMetricsManager::list_metrics(custom_metrics_manager)
  assert_eq(registered_metrics.length(), 4)
  
  // 生成指标数据
  let metric_data = []
  
  // 计数器数据
  for i in 0..=99 {
    let data_point = {
      metric_name: "user.login.count",
      timestamp: 1640995200000 + i * 60000,  // 每分钟一个数据点
      value: 1.0,
      tags: [
        ("service", "auth-service"),
        ("region", "us-west-2")
      ]
    }
    metric_data = metric_data.push(data_point)
  }
  
  // 仪表盘数据
  for i in 0..=49 {
    let data_point = {
      metric_name: "order.value",
      timestamp: 1640995200000 + i * 120000,  // 每2分钟一个数据点
      value: 50.0 + (i % 20) * 5.0,  // 50-145的订单值
      tags: [
        ("service", "order-service"),
        ("currency", "USD")
      ]
    }
    metric_data = metric_data.push(data_point)
  }
  
  // 直方图数据
  for i in 0..=199 {
    let response_time = 10 + (i % 100) * 50  // 10-5010ms的响应时间
    let data_point = {
      metric_name: "response.time.histogram",
      timestamp: 1640995200000 + i * 30000,  // 每30秒一个数据点
      value: response_time.to_float(),
      tags: [
        ("service", "api-service"),
        ("endpoint", "/api/users")
      ]
    }
    metric_data = metric_data.push(data_point)
  }
  
  // CPU利用率数据
  for i in 0..=47 {
    let data_point = {
      metric_name: "cpu.utilization",
      timestamp: 1640995200000 + i * 1800000,  // 每30分钟一个数据点
      value: 20.0 + (i % 30) * 2.0,  // 20-78%的CPU利用率
      tags: [
        ("host", "server-" + (i % 4).to_string()),
        ("region", "us-east-1")
      ]
    }
    metric_data = metric_data.push(data_point)
  }
  
  // 处理指标数据
  for data_point in metric_data {
    let result = CustomMetricsManager::record_metric(custom_metrics_manager, data_point)
    assert_true(result.success)
  }
  
  // 查询计数器指标
  let counter_query = CustomMetricsManager::query_metric(custom_metrics_manager, {
    metric_name: "user.login.count",
    start_time: 1640995000000,
    end_time: 1640995400000,
    aggregation: "sum",
    tags: [
      ("service", "auth-service"),
      ("region", "us-west-2")
    ]
  })
  
  assert_eq(counter_query.data_points.length(), 100)
  assert_eq(counter_query.aggregated_value, 100.0)  // 100次登录
  
  // 查询仪表盘指标
  let gauge_query = CustomMetricsManager::query_metric(custom_metrics_manager, {
    metric_name: "order.value",
    start_time: 1640995000000,
    end_time: 1640995400000,
    aggregation: "last",
    tags: [
      ("service", "order-service"),
      ("currency", "USD")
    ]
  })
  
  assert_eq(gauge_query.data_points.length(), 50)
  assert_true(gauge_query.aggregated_value > 0)  // 应该有最后的值
  
  // 查询直方图指标
  let histogram_query = CustomMetricsManager::query_metric(custom_metrics_manager, {
    metric_name: "response.time.histogram",
    start_time: 1640995000000,
    end_time: 1640995400000,
    aggregation: "histogram",
    tags: [
      ("service", "api-service"),
      ("endpoint", "/api/users")
    ]
  })
  
  assert_eq(histogram_query.data_points.length(), 200)
  assert_true(histogram_query.bucket_counts.length() > 0)  // 应该有桶计数
  
  // 验证直方图桶
  let buckets = histogram_query.bucket_counts
  assert_true(buckets.contains_key("10"))
  assert_true(buckets.contains_key("50"))
  assert_true(buckets.contains_key("100"))
  assert_true(buckets.contains_key("500"))
  assert_true(buckets.contains_key("1000"))
  assert_true(buckets.contains_key("5000"))
  
  // 验证总计数
  let total_count = buckets.map(fn(b) { b.1 }).reduce(fn(a, b) { a + b }, 0.0)
  assert_eq(total_count, 200.0)  // 200个数据点
  
  // 查询平均值指标
  let avg_query = CustomMetricsManager::query_metric(custom_metrics_manager, {
    metric_name: "cpu.utilization",
    start_time: 1640995000000,
    end_time: 1640995400000,
    aggregation: "avg",
    tags: [
      ("host", "server-0"),
      ("region", "us-east-1")
    ]
  })
  
  assert_eq(avg_query.data_points.length(), 12)  // 48个数据点，4个主机，每个主机12个
  assert_true(avg_query.aggregated_value >= 20.0)  // 最小值是20
  assert_true(avg_query.aggregated_value <= 78.0)  // 最大值是78
  
  // 测试多标签查询
  let multi_tag_query = CustomMetricsManager::query_metric(custom_metrics_manager, {
    metric_name: "cpu.utilization",
    start_time: 1640995000000,
    end_time: 1640995400000,
    aggregation: "avg",
    tags: [],  // 不指定标签，获取所有组合
    group_by: ["host", "region"]
  })
  
  assert_true(multi_tag_query.grouped_data.length() > 0)
  
  // 验证每个主机都有数据
  let host_groups = multi_tag_query.grouped_data.map(fn(g) { g.tags.find(fn(t) { t.0 == "host" }) })
  assert_true(host_groups.any(fn(h) { h == Some(("host", "server-0")) }))
  assert_true(host_groups.any(fn(h) { h == Some(("host", "server-1")) }))
  assert_true(host_groups.any(fn(h) { h == Some(("host", "server-2")) }))
  assert_true(host_groups.any(fn(h) { h == Some(("host", "server-3")) }))
  
  // 测试指标导出
  let export_result = CustomMetricsManager::export_metrics(custom_metrics_manager, {
    format: "prometheus",
    start_time: 1640995000000,
    end_time: 1640995400000
  })
  
  assert_true(export_result.success)
  assert_true(export_result.data.length() > 0)
  
  // 验证Prometheus格式
  let prometheus_data = export_result.data
  assert_true(prometheus_data.contains("# TYPE user_login_count counter"))
  assert_true(prometheus_data.contains("# TYPE order_value gauge"))
  assert_true(prometheus_data.contains("# TYPE response_time_histogram histogram"))
  assert_true(prometheus_data.contains("# TYPE cpu_utilization gauge"))
  
  // 测试指标删除
  let delete_result = CustomMetricsManager::delete_metric_data(custom_metrics_manager, {
    metric_name: "user.login.count",
    older_than: 1640995200000 + 30 * 60000,  // 30分钟前
    keep_at_least: 10
  })
  
  assert_true(delete_result.success)
  assert_true(delete_result.deleted_count > 0)
  
  // 验证删除后的数据
  let after_delete_query = CustomMetricsManager::query_metric(custom_metrics_manager, {
    metric_name: "user.login.count",
    start_time: 1640995000000,
    end_time: 1640995400000,
    aggregation: "sum",
    tags: [
      ("service", "auth-service"),
      ("region", "us-west-2")
    ]
  })
  
  assert_true(after_delete_query.data_points.length() <= 10)  // 应该只保留10个或更少
}

// 测试9: 遥测数据安全性和合规性
test "遥测数据安全性和合规性测试" {
  // 创建安全管理器
  let security_manager = TelemetrySecurityManager::new({
    encryption_enabled: true,
    encryption_algorithm: "AES-256-GCM",
    data_retention_days: 90,
    pii_detection_enabled: true,
    gdpr_compliance_enabled: true,
    audit_log_enabled: true
  })
  
  // 创建包含敏感信息的测试数据
  let sensitive_data = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      service_name: "user-service",
      operation_name: "register_user",
      attributes: [
        ("user.email", "john.doe@example.com"),
        ("user.phone", "+1-555-123-4567"),
        ("user.ssn", "123-45-6789"),
        ("user.name", "John Doe"),
        ("user.address", "123 Main St, Anytown, USA"),
        ("user.credit_card", "4111-1111-1111-1111")
      ]
    },
    {
      trace_id: "trace-002",
      span_id: "span-002",
      service_name: "order-service",
      operation_name: "process_payment",
      attributes: [
        ("order.id", "order-12345"),
        ("payment.amount", "99.99"),
        ("payment.currency", "USD"),
        ("payment.card_holder", "Jane Smith"),
        ("payment.billing_address", "456 Oak Ave, Somewhere, USA")
      ]
    },
    {
      trace_id: "trace-003",
      span_id: "span-003",
      service_name: "health-service",
      operation_name: "record_health_data",
      attributes: [
        ("patient.id", "patient-67890"),
        ("patient.name", "Bob Johnson"),
        ("patient.diagnosis", "Hypertension"),
        ("patient.medication", "Lisinopril 10mg"),
        ("patient.doctor", "Dr. Alice Brown")
      ]
    }
  ]
  
  // 处理敏感数据
  let processed_data = []
  for data in sensitive_data {
    let result = TelemetrySecurityManager::process_span(security_manager, data)
    assert_true(result.success)
    processed_data = processed_data.push(result.processed_span)
  }
  
  // 验证PII检测和掩码
  let user_span = processed_data.find(fn(s) { s.operation_name == "register_user" })
  match user_span {
    Some(span) => {
      // 验证电子邮件被掩码
      let email_attr = span.attributes.find(fn(a) { a.0 == "user.email" })
      match email_attr {
        Some((_, value)) => {
          assert_ne(value, "john.doe@example.com")  // 不应该是原始值
          assert_true(value.contains("****"))  // 应该包含掩码字符
        }
        None => assert_true(false)
      }
      
      // 验证电话号码被掩码
      let phone_attr = span.attributes.find(fn(a) { a.0 == "user.phone" })
      match phone_attr {
        Some((_, value)) => {
          assert_ne(value, "+1-555-123-4567")  // 不应该是原始值
          assert_true(value.contains("****"))  // 应该包含掩码字符
        }
        None => assert_true(false)
      }
      
      // 验证SSN被掩码
      let ssn_attr = span.attributes.find(fn(a) { a.0 == "user.ssn" })
      match ssn_attr {
        Some((_, value)) => {
          assert_ne(value, "123-45-6789")  // 不应该是原始值
          assert_true(value.contains("XXX"))  // 应该包含掩码字符
        }
        None => assert_true(false)
      }
      
      // 验证信用卡号被掩码
      let cc_attr = span.attributes.find(fn(a) { a.0 == "user.credit_card" })
      match cc_attr {
        Some((_, value)) => {
          assert_ne(value, "4111-1111-1111-1111")  // 不应该是原始值
          assert_true(value.contains("XXXX"))  // 应该包含掩码字符
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证HIPAA数据（健康信息）处理
  let health_span = processed_data.find(fn(s) { s.operation_name == "record_health_data" })
  match health_span {
    Some(span) => {
      // 验证患者姓名被掩码
      let name_attr = span.attributes.find(fn(a) { a.0 == "patient.name" })
      match name_attr {
        Some((_, value)) => {
          assert_ne(value, "Bob Johnson")  // 不应该是原始值
          assert_true(value.contains("****"))  // 应该包含掩码字符
        }
        None => assert_true(false)
      }
      
      // 验证诊断信息被掩码
      let diagnosis_attr = span.attributes.find(fn(a) { a.0 == "patient.diagnosis" })
      match diagnosis_attr {
        Some((_, value)) => {
          assert_ne(value, "Hypertension")  // 不应该是原始值
          assert_true(value.contains("****"))  // 应该包含掩码字符
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试数据加密
  let encryption_test_span = {
    trace_id: "trace-encrypted",
    span_id: "span-encrypted",
    service_name: "secure-service",
    operation_name: "secure_operation",
    attributes: [
      ("secret.data", "top_secret_information"),
      ("api.key", "sk-1234567890abcdef")
    ]
  }
  
  let encrypt_result = TelemetrySecurityManager::process_span(security_manager, encryption_test_span)
  assert_true(encrypt_result.success)
  assert_true(encrypt_result.encrypted)
  
  // 验证加密数据
  let encrypted_span = encrypt_result.processed_span
  let secret_attr = encrypted_span.attributes.find(fn(a) { a.0 == "secret.data" })
  match secret_attr {
    Some((_, value)) => {
      assert_ne(value, "top_secret_information")  // 不应该是原始值
      assert_true(value.length() > 0)  // 应该有加密后的值
    }
    None => assert_true(false)
  }
  
  // 测试数据解密（授权访问）
  let decrypt_result = TelemetrySecurityManager::decrypt_span(security_manager, {
    trace_id: "trace-encrypted",
    span_id: "span-encrypted",
    access_reason: "audit",
    requester: "auditor@example.com"
  })
  
  assert_true(decrypt_result.success)
  
  let decrypted_span = decrypt_result.decrypted_span
  let decrypted_secret_attr = decrypted_span.attributes.find(fn(a) { a.0 == "secret.data" })
  match decrypted_secret_attr {
    Some((_, value)) => {
      assert_eq(value, "top_secret_information")  // 应该是原始值
    }
    None => assert_true(false)
  }
  
  // 测试访问控制
  let unauthorized_decrypt_result = TelemetrySecurityManager::decrypt_span(security_manager, {
    trace_id: "trace-encrypted",
    span_id: "span-encrypted",
    access_reason: "unauthorized",
    requester: "hacker@example.com"
  })
  
  assert_false(unauthorized_decrypt_result.success)
  assert_eq(unauthorized_decrypt_result.error, "Unauthorized access")
  
  // 测试审计日志
  let audit_logs = TelemetrySecurityManager::get_audit_logs(security_manager, {
    start_time: 1640995000000,
    end_time: 1640995400000,
    filters: [
      ("action", "decrypt")
    ]
  })
  
  assert_true(audit_logs.length() >= 2)  // 至少有一次授权和一次未授权访问
  
  // 验证授权访问日志
  let authorized_log = audit_logs.find(fn(log) { 
    log.requester == "auditor@example.com" and 
    log.access_reason == "audit" and 
    log.success == true 
  })
  match authorized_log {
    Some(log) => {
      assert_eq(log.action, "decrypt")
      assert_eq(log.trace_id, "trace-encrypted")
      assert_eq(log.span_id, "span-encrypted")
    }
    None => assert_true(false)
  }
  
  // 验证未授权访问日志
  let unauthorized_log = audit_logs.find(fn(log) { 
    log.requester == "hacker@example.com" and 
    log.access_reason == "unauthorized" and 
    log.success == false 
  })
  match unauthorized_log {
    Some(log) => {
      assert_eq(log.action, "decrypt")
      assert_eq(log.trace_id, "trace-encrypted")
      assert_eq(log.span_id, "span-encrypted")
      assert_eq(log.error, "Unauthorized access")
    }
    None => assert_true(false)
  }
  
  // 测试GDPR数据删除权（被遗忘权）
  let gdpr_delete_request = {
    user_identifier: "john.doe@example.com",
    reason: "gdpr_right_to_be_forgotten",
    requester: "john.doe@example.com"
  }
  
  let gdpr_delete_result = TelemetrySecurityManager::delete_user_data(security_manager, gdpr_delete_request)
  assert_true(gdpr_delete_result.success)
  assert_true(gdpr_delete_result.deleted_records > 0)
  
  // 验证数据已被删除
  let post_delete_query = TelemetrySecurityManager::query_spans(security_manager, {
    filters: [
      ("user.email", "john.doe@example.com")
    ]
  })
  
  assert_eq(post_delete_query.spans.length(), 0)  // 不应该找到任何记录
  
  // 验证删除操作已记录在审计日志中
  let delete_audit_logs = TelemetrySecurityManager::get_audit_logs(security_manager, {
    start_time: 1640995000000,
    end_time: 1640995400000,
    filters: [
      ("action", "delete_user_data"),
      ("reason", "gdpr_right_to_be_forgotten")
    ]
  })
  
  assert_true(delete_audit_logs.length() >= 1)
  
  let delete_log = delete_audit_logs.find(fn(log) { 
    log.requester == "john.doe@example.com" and 
    log.success == true 
  })
  match delete_log {
    Some(log) => {
      assert_eq(log.action, "delete_user_data")
      assert_eq(log.reason, "gdpr_right_to_be_forgotten")
      assert_true(log.deleted_records > 0)
    }
    None => assert_true(false)
  }
  
  // 测试数据保留策略
  let old_span = {
    trace_id: "trace-old",
    span_id: "span-old",
    service_name: "old-service",
    operation_name: "old_operation",
    timestamp: 1640995200000 - (100 * 86400000),  // 100天前
    attributes: [
      ("old.data", "old_value")
    ]
  }
  
  let old_span_result = TelemetrySecurityManager::process_span(security_manager, old_span)
  assert_true(old_span_result.success)
  
  // 执行数据保留清理
  let retention_cleanup_result = TelemetrySecurityManager::enforce_retention_policy(security_manager)
  assert_true(retention_cleanup_result.success)
  assert_true(retention_cleanup_result.expired_records_deleted > 0)
  
  // 验证过期数据已被删除
  let old_data_query = TelemetrySecurityManager::query_spans(security_manager, {
    filters: [
      ("trace_id", "trace-old")
    ]
  })
  
  assert_eq(old_data_query.spans.length(), 0)  // 不应该找到过期数据
}

// 测试10: 遥测数据国际化和本地化
test "遥测数据国际化和本地化测试" {
  // 创建国际化管理器
  let i18n_manager = TelemetryI18nManager::new({
    default_locale: "en-US",
    supported_locales: ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR"],
    date_formats: {
      "en-US": "MM/DD/YYYY HH:mm:ss",
      "zh-CN": "YYYY年MM月DD日 HH:mm:ss",
      "ja-JP": "YYYY年MM月DD日 HH:mm:ss",
      "es-ES": "DD/MM/YYYY HH:mm:ss",
      "fr-FR": "DD/MM/YYYY HH:mm:ss"
    },
    number_formats: {
      "en-US": { decimal_separator: ".", thousands_separator: "," },
      "zh-CN": { decimal_separator: ".", thousands_separator: "," },
      "ja-JP": { decimal_separator: ".", thousands_separator: "," },
      "es-ES": { decimal_separator: ",", thousands_separator: "." },
      "fr-FR": { decimal_separator: ",", thousands_separator: " " }
    }
  })
  
  // 添加翻译资源
  let translations = [
    {
      locale: "en-US",
      translations: {
        "operation.user_login": "User Login",
        "operation.database_query": "Database Query",
        "operation.http_request": "HTTP Request",
        "status.ok": "OK",
        "status.error": "Error",
        "status.timeout": "Timeout",
        "attribute.http.method": "HTTP Method",
        "attribute.http.status_code": "HTTP Status Code",
        "attribute.db.statement": "Database Statement",
        "attribute.error.type": "Error Type"
      }
    },
    {
      locale: "zh-CN",
      translations: {
        "operation.user_login": "用户登录",
        "operation.database_query": "数据库查询",
        "operation.http_request": "HTTP请求",
        "status.ok": "成功",
        "status.error": "错误",
        "status.timeout": "超时",
        "attribute.http.method": "HTTP方法",
        "attribute.http.status_code": "HTTP状态码",
        "attribute.db.statement": "数据库语句",
        "attribute.error.type": "错误类型"
      }
    },
    {
      locale: "ja-JP",
      translations: {
        "operation.user_login": "ユーザーログイン",
        "operation.database_query": "データベースクエリ",
        "operation.http_request": "HTTPリクエスト",
        "status.ok": "成功",
        "status.error": "エラー",
        "status.timeout": "タイムアウト",
        "attribute.http.method": "HTTPメソッド",
        "attribute.http.status_code": "HTTPステータスコード",
        "attribute.db.statement": "データベースステートメント",
        "attribute.error.type": "エラータイプ"
      }
    },
    {
      locale: "es-ES",
      translations: {
        "operation.user_login": "Inicio de Sesión de Usuario",
        "operation.database_query": "Consulta de Base de Datos",
        "operation.http_request": "Solicitud HTTP",
        "status.ok": "Correcto",
        "status.error": "Error",
        "status.timeout": "Tiempo de Espera Agotado",
        "attribute.http.method": "Método HTTP",
        "attribute.http.status_code": "Código de Estado HTTP",
        "attribute.db.statement": "Declaración de Base de Datos",
        "attribute.error.type": "Tipo de Error"
      }
    },
    {
      locale: "fr-FR",
      translations: {
        "operation.user_login": "Connexion Utilisateur",
        "operation.database_query": "Requête de Base de Données",
        "operation.http_request": "Requête HTTP",
        "status.ok": "OK",
        "status.error": "Erreur",
        "status.timeout": "Expiration du Délai",
        "attribute.http.method": "Méthode HTTP",
        "attribute.http.status_code": "Code de Statut HTTP",
        "attribute.db.statement": "Instruction de Base de Données",
        "attribute.error.type": "Type d'Erreur"
      }
    }
  ]
  
  // 注册翻译
  for translation in translations {
    let result = TelemetryI18nManager::register_translation(i18n_manager, translation)
    assert_true(result.success)
  }
  
  // 创建多语言测试数据
  let multilingual_data = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      service_name: "auth-service",
      operation_name: "operation.user_login",
      start_time: 1640995200000,  // 2022-01-01 00:00:00 UTC
      end_time: 1640995200250,
      status: "status.ok",
      attributes: [
        ("attribute.http.method", "POST"),
        ("attribute.http.status_code", "200"),
        ("user.locale", "en-US")
      ]
    },
    {
      trace_id: "trace-002",
      span_id: "span-002",
      service_name: "db-service",
      operation_name: "operation.database_query",
      start_time: 1640995201000,
      end_time: 1640995201180,
      status: "status.error",
      attributes: [
        ("attribute.db.statement", "SELECT * FROM users WHERE id = ?"),
        ("attribute.error.type", "connection_timeout"),
        ("user.locale", "zh-CN")
      ]
    },
    {
      trace_id: "trace-003",
      span_id: "span-003",
      service_name: "api-service",
      operation_name: "operation.http_request",
      start_time: 1640995202000,
      end_time: 1640995203220,
      status: "status.timeout",
      attributes: [
        ("attribute.http.method", "GET"),
        ("attribute.http.status_code", "408"),
        ("user.locale", "ja-JP")
      ]
    },
    {
      trace_id: "trace-004",
      span_id: "span-004",
      service_name: "payment-service",
      operation_name: "operation.user_login",
      start_time: 1640995203000,
      end_time: 1640995203150,
      status: "status.ok",
      attributes: [
        ("attribute.http.method", "POST"),
        ("attribute.http.status_code", "200"),
        ("user.locale", "es-ES")
      ]
    },
    {
      trace_id: "trace-005",
      span_id: "span-005",
      service_name: "order-service",
      operation_name: "operation.database_query",
      start_time: 1640995204000,
      end_time: 1640995204180,
      status: "status.error",
      attributes: [
        ("attribute.db.statement", "INSERT INTO orders (user_id, amount) VALUES (?, ?)"),
        ("attribute.error.type", "constraint_violation"),
        ("user.locale", "fr-FR")
      ]
    }
  ]
  
  // 处理多语言数据
  for data in multilingual_data {
    let result = TelemetryI18nManager::process_span(i18n_manager, data)
    assert_true(result.success)
  }
  
  // 测试英文本地化
  let en_us_result = TelemetryI18nManager::localize_span(i18n_manager, {
    trace_id: "trace-001",
    span_id: "span-001",
    locale: "en-US"
  })
  
  assert_true(en_us_result.success)
  assert_eq(en_us_result.localized_span.operation_name, "User Login")
  assert_eq(en_us_result.localized_span.status, "OK")
  
  let en_http_method = en_us_result.localized_span.attributes.find(fn(a) { a.0 == "HTTP Method" })
  match en_http_method {
    Some((_, value)) => assert_eq(value, "POST")
    None => assert_true(false)
  }
  
  // 测试中文本地化
  let zh_cn_result = TelemetryI18nManager::localize_span(i18n_manager, {
    trace_id: "trace-002",
    span_id: "span-002",
    locale: "zh-CN"
  })
  
  assert_true(zh_cn_result.success)
  assert_eq(zh_cn_result.localized_span.operation_name, "数据库查询")
  assert_eq(zh_cn_result.localized_span.status, "错误")
  
  let zh_db_statement = zh_cn_result.localized_span.attributes.find(fn(a) { a.0 == "数据库语句" })
  match zh_db_statement {
    Some((_, value)) => assert_eq(value, "SELECT * FROM users WHERE id = ?")
    None => assert_true(false)
  }
  
  // 测试日文本地化
  let ja_jp_result = TelemetryI18nManager::localize_span(i18n_manager, {
    trace_id: "trace-003",
    span_id: "span-003",
    locale: "ja-JP"
  })
  
  assert_true(ja_jp_result.success)
  assert_eq(ja_jp_result.localized_span.operation_name, "HTTPリクエスト")
  assert_eq(ja_jp_result.localized_span.status, "タイムアウト")
  
  // 测试西班牙语本地化
  let es_es_result = TelemetryI18nManager::localize_span(i18n_manager, {
    trace_id: "trace-004",
    span_id: "span-004",
    locale: "es-ES"
  })
  
  assert_true(es_es_result.success)
  assert_eq(es_es_result.localized_span.operation_name, "Inicio de Sesión de Usuario")
  assert_eq(es_es_result.localized_span.status, "Correcto")
  
  // 测试法语本地化
  let fr_fr_result = TelemetryI18nManager::localize_span(i18n_manager, {
    trace_id: "trace-005",
    span_id: "span-005",
    locale: "fr-FR"
  })
  
  assert_true(fr_fr_result.success)
  assert_eq(fr_fr_result.localized_span.operation_name, "Requête de Base de Données")
  assert_eq(fr_fr_result.localized_span.status, "Erreur")
  
  // 测试日期格式本地化
  let date_localization_tests = [
    { locale: "en-US", expected: "01/01/2022 00:00:00" },
    { locale: "zh-CN", expected: "2022年01月01日 00:00:00" },
    { locale: "ja-JP", expected: "2022年01月01日 00:00:00" },
    { locale: "es-ES", expected: "01/01/2022 00:00:00" },
    { locale: "fr-FR", expected: "01/01/2022 00:00:00" }
  ]
  
  for test in date_localization_tests {
    let formatted_date = TelemetryI18nManager::format_date(i18n_manager, {
      timestamp: 1640995200000,  // 2022-01-01 00:00:00 UTC
      locale: test.locale
    })
    
    assert_eq(formatted_date, test.expected)
  }
  
  // 测试数字格式本地化
  let number_localization_tests = [
    { locale: "en-US", number: 1234.56, expected: "1,234.56" },
    { locale: "zh-CN", number: 1234.56, expected: "1,234.56" },
    { locale: "ja-JP", number: 1234.56, expected: "1,234.56" },
    { locale: "es-ES", number: 1234.56, expected: "1.234,56" },
    { locale: "fr-FR", number: 1234.56, expected: "1 234,56" }
  ]
  
  for test in number_localization_tests {
    let formatted_number = TelemetryI18nManager::format_number(i18n_manager, {
      number: test.number,
      locale: test.locale
    })
    
    assert_eq(formatted_number, test.expected)
  }
  
  // 测试多语言聚合报告
  let multilingual_report = TelemetryI18nManager::generate_multilingual_report(i18n_manager, {
    start_time: 1640995000000,
    end_time: 1640995300000,
    metrics: [
      "operation_count",
      "error_rate",
      "average_duration"
    ],
    locales: ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR"]
  })
  
  assert_true(multilingual_report.success)
  assert_eq(multilingual_report.localized_reports.length(), 5)
  
  // 验证英文报告
  let en_report = multilingual_report.localized_reports.find(fn(r) { r.locale == "en-US" })
  match en_report {
    Some(report) => {
      assert_true(report.title.contains("Telemetry Report"))
      assert_true(report.sections.any(fn(s) { s.contains("Operation Count") }))
      assert_true(report.sections.any(fn(s) { s.contains("Error Rate") }))
      assert_true(report.sections.any(fn(s) { s.contains("Average Duration") }))
    }
    None => assert_true(false)
  }
  
  // 验证中文报告
  let zh_report = multilingual_report.localized_reports.find(fn(r) { r.locale == "zh-CN" })
  match zh_report {
    Some(report) => {
      assert_true(report.title.contains("遥测报告"))
      assert_true(report.sections.any(fn(s) { s.contains("操作计数") }))
      assert_true(report.sections.any(fn(s) { s.contains("错误率") }))
      assert_true(report.sections.any(fn(s) { s.contains("平均持续时间") }))
    }
    None => assert_true(false)
  }
  
  // 测试回退语言（当请求的语言不支持时）
  let unsupported_locale_result = TelemetryI18nManager::localize_span(i18n_manager, {
    trace_id: "trace-001",
    span_id: "span-001",
    locale: "ko-KR"  // 不支持的语言
  })
  
  assert_true(unsupported_locale_result.success)
  assert_eq(unsupported_locale_result.localized_span.operation_name, "User Login")  // 回退到默认语言
  assert_eq(unsupported_locale_result.fallback_locale, "en-US")
  
  // 测试动态语言切换
  let dynamic_switch_result = TelemetryI18nManager::switch_locale(i18n_manager, {
    trace_id: "trace-002",
    span_id: "span-002",
    from_locale: "zh-CN",
    to_locale: "ja-JP"
  })
  
  assert_true(dynamic_switch_result.success)
  assert_eq(dynamic_switch_result.original_span.operation_name, "数据库查询")
  assert_eq(dynamic_switch_result.converted_span.operation_name, "データベースクエリ")
}