// Azimuth Premium Advanced Integration Tests
// 高级集成测试用例，测试系统各组件间的协同工作

test "distributed tracing end-to-end workflow" {
  // 创建根span
  let root_span = @azimuth.create_span("user.registration")
  assert_eq(root_span.operation_name, "user.registration")
  assert_true(root_span.trace_id.length() == 32)
  assert_true(root_span.span_id.length() == 16)
  assert_true(root_span.start_time > 0)
  
  // 添加属性
  @azimuth.set_span_attribute(root_span, "user.id", "12345")
  @azimuth.set_span_attribute(root_span, "user.email", "test@example.com")
  @azimuth.set_span_attribute(root_span, "registration.source", "web")
  
  // 创建子span - 数据库操作
  let db_span = @azimuth.create_child_span(root_span, "database.insert_user")
  @azimuth.set_span_attribute(db_span, "db.system", "postgresql")
  @azimuth.set_span_attribute(db_span, "db.statement", "INSERT INTO users (id, email) VALUES ($1, $2)")
  @azimuth.add_span_event(db_span, "query.start", @azimuth.timestamp_now())
  
  // 模拟数据库操作耗时
  @azimuth.sleep(50)
  
  @azimuth.add_span_event(db_span, "query.complete", @azimuth.timestamp_now())
  @azimuth.set_span_attribute(db_span, "db.rows_affected", 1)
  @azimuth.finish_span(db_span)
  
  // 创建子span - 缓存操作
  let cache_span = @azimuth.create_child_span(root_span, "cache.set_user")
  @azimuth.set_span_attribute(cache_span, "cache.system", "redis")
  @azimuth.set_span_attribute(cache_span, "cache.key", "user:12345")
  @azimuth.add_span_event(cache_span, "cache.write.start", @azimuth.timestamp_now())
  
  // 模拟缓存操作耗时
  @azimuth.sleep(10)
  
  @azimuth.add_span_event(cache_span, "cache.write.complete", @azimuth.timestamp_now())
  @azimuth.set_span_status(cache_span, @azimuth.SpanStatus::Ok)
  @azimuth.finish_span(cache_span)
  
  // 创建子span - 消息队列操作
  let mq_span = @azimuth.create_child_span(root_span, "message_queue.publish_welcome")
  @azimuth.set_span_attribute(mq_span, "messaging.system", "rabbitmq")
  @azimuth.set_span_attribute(mq_span, "messaging.destination", "welcome.email")
  @azimuth.add_span_event(mq_span, "message.publish.start", @azimuth.timestamp_now())
  
  // 模拟消息发布耗时
  @azimuth.sleep(20)
  
  @azimuth.add_span_event(mq_span, "message.publish.complete", @azimuth.timestamp_now())
  @azimuth.set_span_status(mq_span, @azimuth.SpanStatus::Ok)
  @azimuth.finish_span(mq_span)
  
  // 完成根span
  @azimuth.add_span_event(root_span, "registration.complete", @azimuth.timestamp_now())
  @azimuth.set_span_status(root_span, @azimuth.SpanStatus::Ok)
  @azimuth.finish_span(root_span)
  
  // 验证span关系
  assert_eq(db_span.trace_id, root_span.trace_id)
  assert_eq(cache_span.trace_id, root_span.trace_id)
  assert_eq(mq_span.trace_id, root_span.trace_id)
  
  assert_eq(db_span.parent_span_id, Some(root_span.span_id))
  assert_eq(cache_span.parent_span_id, Some(root_span.span_id))
  assert_eq(mq_span.parent_span_id, Some(root_span.span_id))
  
  // 验证span持续时间
  assert_true(db_span.duration >= 50)
  assert_true(cache_span.duration >= 10)
  assert_true(mq_span.duration >= 20)
  assert_true(root_span.duration >= 80)
}

test "metrics collection and aggregation pipeline" {
  // 创建指标收集器
  let metrics_collector = @azimuth.MetricsCollector {
    buffer_size : 1000,
    flush_interval_ms : 5000,
    aggregation_window_ms : 60000
  }
  
  // 添加计数器指标
  let request_counter = @azimuth.create_counter("http.requests.total", [
    ("method", @azimuth.StringValue("GET")),
    ("endpoint", @azimuth.StringValue("/api/users"))
  ])
  
  // 记录请求计数
  for i in 1..=100 {
    @azimuth.counter_add(request_counter, 1, [
      ("status_code", @azimuth.IntValue(if i % 10 == 0 { 500 } else { 200 }))
    ])
  }
  
  // 添加直方图指标
  let response_time_histogram = @azimuth.create_histogram("http.request.duration", [
    ("method", @azimuth.StringValue("GET")),
    ("endpoint", @azimuth.StringValue("/api/users"))
  ])
  
  // 记录响应时间
  for i in 1..=100 {
    let duration = if i % 4 == 0 { 500.0 } 
                   else if i % 3 == 0 { 200.0 } 
                   else if i % 2 == 0 { 100.0 } 
                   else { 50.0 }
    @azimuth.histogram_record(response_time_histogram, duration, [
      ("status_code", @azimuth.IntValue(if i % 10 == 0 { 500 } else { 200 }))
    ])
  }
  
  // 添加仪表指标
  let active_connections_gauge = @azimuth.create_gauge("http.connections.active", [
    ("server", @azimuth.StringValue("api-server-01"))
  ])
  
  // 记录活跃连接数变化
  @azimuth.gauge_set(active_connections_gauge, 50, [])
  @azimuth.gauge_set(active_connections_gauge, 75, [])
  @azimuth.gauge_set(active_connections_gauge, 60, [])
  @azimuth.gauge_set(active_connections_gauge, 45, [])
  
  // 获取聚合指标
  let aggregated_metrics = metrics_collector.get_aggregated_metrics()
  assert_true(aggregated_metrics.length() >= 3)
  
  // 验证计数器指标
  let counter_metric = aggregated_metrics.find(fn(m) { m.name == "http.requests.total" })
  match counter_metric {
    Some(metric) => {
      match metric.value {
        @azimuth.MetricValue::Counter(count) => assert_eq(count, 100)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证直方图指标
  let histogram_metric = aggregated_metrics.find(fn(m) { m.name == "http.request.duration" })
  match histogram_metric {
    Some(metric) => {
      match metric.value {
        @azimuth.MetricValue::Histogram(h) => {
          assert_eq(h.count, 100)
          assert_true(h.sum > 0.0)
          assert_true(h.buckets.length() > 0)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证仪表指标
  let gauge_metric = aggregated_metrics.find(fn(m) { m.name == "http.connections.active" })
  match gauge_metric {
    Some(metric) => {
      match metric.value {
        @azimuth.MetricValue::Gauge(value) => assert_eq(value, 45)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

test "log correlation with tracing context" {
  // 创建span上下文
  let span_context = @azimuth.SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1234567890abcdef",
    sampled : true,
    trace_state : "key1=value1,key2=value2"
  }
  
  // 创建日志记录器
  let logger = @azimuth.create_logger("user.service")
  @azimuth.set_logger_attribute(logger, "service.version", "1.0.0")
  @azimuth.set_logger_attribute(logger, "service.instance.id", "instance-123")
  
  // 设置日志上下文
  @azimuth.set_logger_context(logger, span_context)
  
  // 记录不同级别的日志
  @azimuth.log_debug(logger, "Starting user registration process", [
    ("user.id", "12345"),
    ("user.email", "test@example.com")
  ])
  
  @azimuth.log_info(logger, "User validation successful", [
    ("user.id", "12345"),
    ("validation.result", "success")
  ])
  
  @azimuth.log_warn(logger, "User profile incomplete", [
    ("user.id", "12345"),
    ("missing.fields", "phone,address")
  ])
  
  @azimuth.log_error(logger, "Database connection failed", [
    ("error.code", "DB_CONN_ERROR"),
    ("error.message", "Connection timeout after 30 seconds"),
    ("retry.count", "3")
  ])
  
  // 获取日志记录
  let log_records = @azimuth.get_logger_records(logger)
  assert_eq(log_records.length(), 4)
  
  // 验证日志记录与trace上下文的关联
  for record in log_records {
    assert_eq(record.trace_id, Some("1234567890abcdef1234567890abcdef"))
    assert_eq(record.span_id, Some("1234567890abcdef"))
    assert_eq(record.service_name, "user.service")
    assert_eq(record.service_version, Some("1.0.0"))
    assert_eq(record.service_instance_id, Some("instance-123"))
  }
  
  // 验证日志级别
  assert_eq(log_records[0].level, @azimuth.LogLevel::Debug)
  assert_eq(log_records[1].level, @azimuth.LogLevel::Info)
  assert_eq(log_records[2].level, @azimuth.LogLevel::Warn)
  assert_eq(log_records[3].level, @azimuth.LogLevel::Error)
  
  // 验证日志属性
  let debug_record = log_records[0]
  assert_eq(debug_record.message, "Starting user registration process")
  assert_eq(debug_record.attributes.find(fn(a) { a.0 == "user.id" }).unwrap().1, @azimuth.StringValue("12345"))
  
  let error_record = log_records[3]
  assert_eq(error_record.message, "Database connection failed")
  assert_eq(error_record.attributes.find(fn(a) { a.0 == "error.code" }).unwrap().1, @azimuth.StringValue("DB_CONN_ERROR"))
}

test "cross-service context propagation" {
  // 创建服务A的span上下文
  let service_a_context = @azimuth.SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1111111111111111",
    sampled : true,
    trace_state : "key1=value1,key2=value2"
  }
  
  // 创建服务A的span
  let service_a_span = @azimuth.create_span_with_context("service.a.operation", service_a_context)
  @azimuth.set_span_attribute(service_a_span, "service.name", "service-a")
  @azimuth.set_span_attribute(service_a_span, "service.version", "1.0.0")
  
  // 创建baggage
  let baggage = @azimuth.Baggage {
    entries : [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("region", "us-west-2"),
      ("tenant.id", "tenant-001")
    ]
  }
  
  // 创建跨服务传播器
  let propagator = @azimuth.CrossServicePropagator {
    injectors : [@azimuth.TraceContextInjector, @azimuth.BaggageInjector],
    extractors : [@azimuth.TraceContextExtractor, @azimuth.BaggageExtractor]
  }
  
  // 服务A向服务B传播上下文
  let service_b_carrier = @azimuth.TextMapCarrier { headers : [] }
  propagator.inject(service_a_span, service_b_carrier)
  propagator.inject_baggage(baggage, service_b_carrier)
  
  // 验证注入的headers
  assert_true(service_b_carrier.headers.length() >= 2)
  assert_true(service_b_carrier.headers.any(fn(h) { h.0 == "traceparent" }))
  assert_true(service_b_carrier.headers.any(fn(h) { h.0 == "baggage" }))
  
  // 服务B提取上下文
  let service_b_context = propagator.extract(service_b_carrier)
  match service_b_context {
    Some(ctx) => {
      assert_eq(ctx.trace_id, service_a_context.trace_id)
      assert_eq(ctx.sampled, true)
      assert_eq(ctx.trace_state, "key1=value1,key2=value2")
    }
    None => assert_true(false)
  }
  
  let service_b_baggage = propagator.extract_baggage(service_b_carrier)
  match service_b_baggage {
    Some(b) => {
      assert_eq(b.entries.length(), 4)
      assert_true(b.entries.any(fn(e) { e.0 == "user.id" && e.1 == "12345" }))
      assert_true(b.entries.any(fn(e) { e.0 == "request.id" && e.1 == "req-67890" }))
      assert_true(b.entries.any(fn(e) { e.0 == "region" && e.1 == "us-west-2" }))
      assert_true(b.entries.any(fn(e) { e.0 == "tenant.id" && e.1 == "tenant-001" }))
    }
    None => assert_true(false)
  }
  
  // 服务B创建span
  let service_b_span = @azimuth.create_span_with_context("service.b.operation", service_b_context.unwrap())
  @azimuth.set_span_attribute(service_b_span, "service.name", "service-b")
  @azimuth.set_span_attribute(service_b_span, "service.version", "2.0.0")
  
  // 服务B向服务C传播上下文
  let service_c_carrier = @azimuth.TextMapCarrier { headers : [] }
  propagator.inject(service_b_span, service_c_carrier)
  propagator.inject_baggage(service_b_baggage.unwrap(), service_c_carrier)
  
  // 服务C提取上下文
  let service_c_context = propagator.extract(service_c_carrier)
  match service_c_context {
    Some(ctx) => {
      assert_eq(ctx.trace_id, service_a_context.trace_id)
      assert_eq(ctx.sampled, true)
    }
    None => assert_true(false)
  }
  
  // 验证跨服务上下文传播链
  assert_eq(service_a_span.trace_id, service_b_span.trace_id)
  assert_eq(service_b_context.unwrap().trace_id, service_c_context.unwrap().trace_id)
  
  // 验证span父子关系
  assert_eq(service_b_span.parent_span_id, Some(service_a_span.span_id))
}

test "resource utilization monitoring" {
  // 创建资源监控器
  let resource_monitor = @azimuth.ResourceMonitor {
    collection_interval_ms : 1000,
    retention_period_ms : 300000, // 5分钟
    metrics : [
      @azimuth.ResourceMetric::CPU,
      @azimuth.ResourceMetric::Memory,
      @azimuth.ResourceMetric::Disk,
      @azimuth.ResourceMetric::Network
    ]
  }
  
  // 启动资源监控
  @azimuth.start_resource_monitoring(resource_monitor)
  
  // 模拟工作负载
  @azimuth.sleep(2000)
  
  // 获取CPU使用率
  let cpu_usage = @azimuth.get_cpu_usage(resource_monitor)
  assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
  
  // 获取内存使用率
  let memory_usage = @azimuth.get_memory_usage(resource_monitor)
  assert_true(memory_usage >= 0.0 && memory_usage <= 100.0)
  
  // 获取磁盘使用率
  let disk_usage = @azimuth.get_disk_usage(resource_monitor)
  assert_true(disk_usage >= 0.0 && disk_usage <= 100.0)
  
  // 获取网络使用统计
  let network_stats = @azimuth.get_network_stats(resource_monitor)
  assert_true(network_stats.bytes_sent >= 0)
  assert_true(network_stats.bytes_received >= 0)
  assert_true(network_stats.packets_sent >= 0)
  assert_true(network_stats.packets_received >= 0)
  
  // 获取历史资源使用数据
  let resource_history = @azimuth.get_resource_history(resource_monitor)
  assert_true(resource_history.length() >= 1)
  
  // 验证历史数据的时间顺序
  for i in 1..<resource_history.length() {
    assert_true(resource_history[i].timestamp >= resource_history[i-1].timestamp)
  }
  
  // 计算平均资源使用率
  let avg_cpu = @azimuth.calculate_average_cpu_usage(resource_history)
  let avg_memory = @azimuth.calculate_average_memory_usage(resource_history)
  
  assert_true(avg_cpu >= 0.0 && avg_cpu <= 100.0)
  assert_true(avg_memory >= 0.0 && avg_memory <= 100.0)
  
  // 检测资源使用异常
  let anomalies = @azimuth.detect_resource_anomalies(resource_history)
  assert_true(anomalies.length() >= 0)
  
  // 停止资源监控
  @azimuth.stop_resource_monitoring(resource_monitor)
}

test "distributed transaction tracing" {
  // 创建分布式事务追踪器
  let transaction_tracer = @azimuth.DistributedTransactionTracer {
    transaction_timeout_ms : 30000,
    participant_timeout_ms : 10000,
    retry_policy : @azimuth.RetryPolicy {
      max_retries : 3,
      backoff_strategy : @azimuth.BackoffStrategy::Exponential,
      initial_delay_ms : 1000
    }
  }
  
  // 开始分布式事务
  let transaction = transaction_tracer.begin_transaction("order.processing")
  assert_eq(transaction.transaction_id, transaction.trace_id)
  assert_eq(transaction.status, @azimuth.TransactionStatus::Active)
  assert_true(transaction.start_time > 0)
  
  // 添加参与者 - 订单服务
  let order_participant = transaction_tracer.add_participant(transaction, "order.service")
  assert_eq(order_participant.transaction_id, transaction.transaction_id)
  assert_eq(order_participant.service_name, "order.service")
  assert_eq(order_participant.status, @azimuth.ParticipantStatus::Preparing)
  
  // 添加参与者 - 库存服务
  let inventory_participant = transaction_tracer.add_participant(transaction, "inventory.service")
  assert_eq(inventory_participant.transaction_id, transaction.transaction_id)
  assert_eq(inventory_participant.service_name, "inventory.service")
  assert_eq(inventory_participant.status, @azimuth.ParticipantStatus::Preparing)
  
  // 添加参与者 - 支付服务
  let payment_participant = transaction_tracer.add_participant(transaction, "payment.service")
  assert_eq(payment_participant.transaction_id, transaction.transaction_id)
  assert_eq(payment_participant.service_name, "payment.service")
  assert_eq(payment_participant.status, @azimuth.ParticipantStatus::Preparing)
  
  // 订单服务准备阶段
  @azimuth.add_participant_event(order_participant, "prepare.start")
  @azimuth.sleep(50)
  @azimuth.add_participant_event(order_participant, "prepare.complete")
  transaction_tracer.set_participant_status(order_participant, @azimuth.ParticipantStatus::Prepared)
  
  // 库存服务准备阶段
  @azimuth.add_participant_event(inventory_participant, "prepare.start")
  @azimuth.sleep(100)
  @azimuth.add_participant_event(inventory_participant, "prepare.complete")
  transaction_tracer.set_participant_status(inventory_participant, @azimuth.ParticipantStatus::Prepared)
  
  // 支付服务准备阶段
  @azimuth.add_participant_event(payment_participant, "prepare.start")
  @azimuth.sleep(80)
  @azimuth.add_participant_event(payment_participant, "prepare.complete")
  transaction_tracer.set_participant_status(payment_participant, @azimuth.ParticipantStatus::Prepared)
  
  // 检查所有参与者是否已准备就绪
  let all_prepared = transaction_tracer.check_all_participants_prepared(transaction)
  assert_true(all_prepared)
  
  // 提交阶段
  transaction_tracer.set_transaction_status(transaction, @azimuth.TransactionStatus::Committing)
  
  // 订单服务提交
  @azimuth.add_participant_event(order_participant, "commit.start")
  @azimuth.sleep(30)
  @azimuth.add_participant_event(order_participant, "commit.complete")
  transaction_tracer.set_participant_status(order_participant, @azimuth.ParticipantStatus::Committed)
  
  // 库存服务提交
  @azimuth.add_participant_event(inventory_participant, "commit.start")
  @azimuth.sleep(40)
  @azimuth.add_participant_event(inventory_participant, "commit.complete")
  transaction_tracer.set_participant_status(inventory_participant, @azimuth.ParticipantStatus::Committed)
  
  // 支付服务提交
  @azimuth.add_participant_event(payment_participant, "commit.start")
  @azimuth.sleep(35)
  @azimuth.add_participant_event(payment_participant, "commit.complete")
  transaction_tracer.set_participant_status(payment_participant, @azimuth.ParticipantStatus::Committed)
  
  // 完成事务
  transaction_tracer.set_transaction_status(transaction, @azimuth.TransactionStatus::Committed)
  transaction_tracer.end_transaction(transaction)
  
  // 验证事务状态
  assert_eq(transaction.status, @azimuth.TransactionStatus::Committed)
  assert_true(transaction.end_time > transaction.start_time)
  assert_true(transaction.duration > 0)
  
  // 验证参与者状态
  assert_eq(order_participant.status, @azimuth.ParticipantStatus::Committed)
  assert_eq(inventory_participant.status, @azimuth.ParticipantStatus::Committed)
  assert_eq(payment_participant.status, @azimuth.ParticipantStatus::Committed)
  
  // 获取事务执行报告
  let report = transaction_tracer.generate_transaction_report(transaction)
  assert_true(report.contains("transaction_id"))
  assert_true(report.contains("status"))
  assert_true(report.contains("participants"))
  assert_true(report.contains("duration"))
}