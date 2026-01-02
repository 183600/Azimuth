// Azimuth 跨服务一致性测试用例
// 专注于分布式系统中多个服务间的遥测数据一致性和协调

// 测试1: 分布式追踪上下文传播一致性
test "分布式追踪上下文传播一致性测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.tracing")
  
  // 创建跨服务追踪管理器
  let cross_service_tracing = CrossServiceTracingManager::new(tracer)
  
  // 创建追踪一致性指标
  let trace_counter = Meter::create_counter(MeterProvider::get_meter(tracer_provider, "tracing.metrics"), "cross.service.traces")
  let consistency_gauge = Meter::create_gauge(MeterProvider::get_meter(tracer_provider, "tracing.metrics"), "trace.context.consistency")
  let propagation_gauge = Meter::create_gauge(MeterProvider::get_meter(tracer_provider, "tracing.metrics"), "context.propagation.success.rate")
  
  // 模拟跨服务调用链
  let trace_chain_span = Tracer::start_span(tracer, "cross.service.trace.chain")
  
  // 创建根追踪上下文
  let root_span = Tracer::start_span(tracer, "api.gateway.request")
  let root_context = Span::context(root_span)
  Span::set_attribute(root_span, "service.name", StringValue("api.gateway"))
  Span::set_attribute(root_span, "request.id", StringValue("req-12345"))
  
  // 服务1: 用户服务
  let user_service_span = Tracer::start_span_with_context(tracer, "user.service.call", root_context)
  CrossServiceTracingManager::propagate_context(cross_service_tracing, root_context, "user.service")
  Span::set_attribute(user_service_span, "service.name", StringValue("user.service"))
  Span::set_attribute(user_service_span, "operation", StringValue("get.user.profile"))
  Span::set_status(user_service_span, Ok)
  Span::end(user_service_span)
  
  // 服务2: 订单服务
  let order_service_span = Tracer::start_span_with_context(tracer, "order.service.call", root_context)
  CrossServiceTracingManager::propagate_context(cross_service_tracing, root_context, "order.service")
  Span::set_attribute(order_service_span, "service.name", StringValue("order.service"))
  Span::set_attribute(order_service_span, "operation", StringValue("create.order"))
  Span::set_status(order_service_span, Ok)
  Span::end(order_service_span)
  
  // 服务3: 支付服务
  let payment_service_span = Tracer::start_span_with_context(tracer, "payment.service.call", root_context)
  CrossServiceTracingManager::propagate_context(cross_service_tracing, root_context, "payment.service")
  Span::set_attribute(payment_service_span, "service.name", StringValue("payment.service"))
  Span::set_attribute(payment_service_span, "operation", StringValue("process.payment"))
  Span::set_status(payment_service_span, Ok)
  Span::end(payment_service_span)
  
  // 服务4: 库存服务
  let inventory_service_span = Tracer::start_span_with_context(tracer, "inventory.service.call", root_context)
  CrossServiceTracingManager::propagate_context(cross_service_tracing, root_context, "inventory.service")
  Span::set_attribute(inventory_service_span, "service.name", StringValue("inventory.service"))
  Span::set_attribute(inventory_service_span, "operation", StringValue("reserve.inventory"))
  Span::set_status(inventory_service_span, Ok)
  Span::end(inventory_service_span)
  
  // 完成根span
  Span::set_status(root_span, Ok)
  Span::end(root_span)
  
  // 验证追踪上下文一致性
  let trace_consistency = CrossServiceTracingManager::verify_trace_consistency(cross_service_tracing, root_context)
  let propagation_success = CrossServiceTracingManager::calculate_propagation_success(cross_service_tracing)
  
  Counter::add(trace_counter, 1.0)
  Gauge::set(consistency_gauge, trace_consistency)
  Gauge::set(propagation_gauge, propagation_success)
  
  Span::set_attribute(trace_chain_span, "trace.consistency", FloatValue(trace_consistency))
  Span::set_attribute(trace_chain_span, "propagation.success", FloatValue(propagation_success))
  Span::set_status(trace_chain_span, Ok)
  Span::end(trace_chain_span)
  
  // 验证跨服务追踪一致性
  assert_true(trace_consistency > 0.9) // 应该有95%以上的一致性
  assert_true(propagation_success > 0.9)
  assert_eq(Counter::value(trace_counter), 1.0)
}

// 测试2: 跨服务指标数据一致性
test "跨服务指标数据一致性测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross.service.metrics")
  
  // 创建跨服务指标管理器
  let cross_service_metrics = CrossServiceMetricsManager::new(meter)
  
  // 创建指标一致性指标
  let metrics_counter = Meter::create_counter(meter, "cross.service.metrics.collected")
  let consistency_gauge = Meter::create_gauge(meter, "metrics.data.consistency")
  let synchronization_gauge = Meter::create_gauge(meter, "metrics.synchronization.latency.ms")
  
  // 配置多个服务的指标收集
  let services = [
    ("api.gateway", ServiceMetricsConfig::new([
      ("metrics.port", IntValue(9090)),
      ("scrape.interval", IntValue(15)),
      ("metric.prefix", StringValue("gateway_"))
    ])),
    ("user.service", ServiceMetricsConfig::new([
      ("metrics.port", IntValue(9091)),
      ("scrape.interval", IntValue(15)),
      ("metric.prefix", StringValue("user_"))
    ])),
    ("order.service", ServiceMetricsConfig::new([
      ("metrics.port", IntValue(9092)),
      ("scrape.interval", IntValue(15)),
      ("metric.prefix", StringValue("order_"))
    ])),
    ("payment.service", ServiceMetricsConfig::new([
      ("metrics.port", IntValue(9093)),
      ("scrape.interval", IntValue(15)),
      ("metric.prefix", StringValue("payment_"))
    ]))
  ]
  
  for (service_name, config) in services {
    CrossServiceMetricsManager::register_service(cross_service_metrics, service_name, config)
  }
  
  // 模拟跨服务业务流程
  let business_flow_span = Tracer::start_span(TracerProvider::get_tracer(provider, "cross.service.metrics"), "business.flow.metrics")
  
  // 生成跨服务指标数据
  let common_trace_id = "trace-" + Time::now().to_string()
  let common_request_id = "req-" + Time::now().to_string()
  
  let service_metrics = [
    ("api.gateway", [
      ("http_requests_total", 1.0),
      ("request_duration_seconds", 0.125),
      ("response_size_bytes", 1024.0)
    ]),
    ("user.service", [
      ("user_queries_total", 1.0),
      ("database_query_duration_seconds", 0.045),
      ("cache_hits_total", 3.0)
    ]),
    ("order.service", [
      ("order_creations_total", 1.0),
      ("order_validation_duration_seconds", 0.025),
      ("inventory_checks_total", 1.0)
    ]),
    ("payment.service", [
      ("payment_attempts_total", 1.0),
      ("payment_processing_duration_seconds", 0.350),
      ("payment_amount_total", 99.99)
    ])
  ]
  
  for (service_name, metrics) in service_metrics {
    let sync_start = Time::now()
    
    // 添加通用标签
    let labeled_metrics = []
    for (metric_name, value) in metrics {
      let labeled_metric = LabeledMetric::new(metric_name, value, [
        ("trace_id", StringValue(common_trace_id)),
        ("request_id", StringValue(common_request_id)),
        ("service_name", StringValue(service_name))
      ])
      labeled_metrics = labeled_metrics.push(labeled_metric)
    }
    
    // 收集服务指标
    CrossServiceMetricsManager::collect_metrics(cross_service_metrics, service_name, labeled_metrics)
    
    let sync_end = Time::now()
    let sync_latency = Time::duration_between(sync_start, sync_end)
    Histogram::record(synchronization_gauge, sync_latency * 1000.0) // 转换为毫秒
    
    Counter::add(metrics_counter, 1.0)
  }
  
  // 验证指标数据一致性
  let metrics_consistency = CrossServiceMetricsManager::verify_metrics_consistency(cross_service_metrics, common_trace_id)
  Gauge::set(consistency_gauge, metrics_consistency)
  
  Span::set_attribute(business_flow_span, "metrics.consistency", FloatValue(metrics_consistency))
  Span::set_status(business_flow_span, Ok)
  Span::end(business_flow_span)
  
  // 验证跨服务指标一致性
  assert_true(metrics_consistency > 0.8) // 应该有80%以上的一致性
  assert_eq(Counter::value(metrics_counter), 4.0)
}

// 测试3: 跨服务日志关联一致性
test "跨服务日志关联一致性测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "cross.service.logging")
  
  // 创建跨服务日志管理器
  let cross_service_logging = CrossServiceLoggingManager::new(logger)
  
  // 创建日志一致性指标
  let log_counter = Meter::create_counter(MeterProvider::get_meter(logger_provider, "logging.metrics"), "cross.service.logs")
  let correlation_gauge = Meter::create_gauge(MeterProvider::get_meter(logger_provider, "logging.metrics"), "log.correlation.success.rate")
  let completeness_gauge = Meter::create_gauge(MeterProvider::get_meter(logger_provider, "logging.metrics"), "log.chain.completeness")
  
  // 配置跨服务日志关联
  let correlation_config = LogCorrelationConfig::new([
    ("correlation.id.field", StringValue("trace_id")),
    ("request.id.field", StringValue("request_id")),
    ("service.name.field", StringValue("service_name")),
    ("timestamp.format", StringValue("ISO8601"))
  ])
  
  CrossServiceLoggingManager::configure_correlation(cross_service_logging, correlation_config)
  
  // 模拟跨服务日志链
  let log_chain_span = Tracer::start_span(TracerProvider::get_tracer(logger_provider, "cross.service.logging"), "cross.service.log.chain")
  
  let common_correlation_id = "corr-" + Time::now().to_string()
  let common_request_id = "req-" + Time::now().to_string()
  
  // 服务日志序列
  let service_logs = [
    ("api.gateway", [
      LogEntry::new("INFO", "Request received", [
        ("trace_id", StringValue(common_correlation_id)),
        ("request_id", StringValue(common_request_id)),
        ("service_name", StringValue("api.gateway")),
        ("endpoint", StringValue("/api/v1/orders")),
        ("method", StringValue("POST"))
      ]),
      LogEntry::new("INFO", "Request authenticated", [
        ("trace_id", StringValue(common_correlation_id)),
        ("request_id", StringValue(common_request_id)),
        ("service_name", StringValue("api.gateway")),
        ("user_id", StringValue("user-12345"))
      ])
    ]),
    ("user.service", [
      LogEntry::new("INFO", "User profile fetched", [
        ("trace_id", StringValue(common_correlation_id)),
        ("request_id", StringValue(common_request_id)),
        ("service_name", StringValue("user.service")),
        ("user_id", StringValue("user-12345"))
      ]),
      LogEntry::new("DEBUG", "User permissions validated", [
        ("trace_id", StringValue(common_correlation_id)),
        ("request_id", StringValue(common_request_id)),
        ("service_name", StringValue("user.service")),
        ("permissions", StringValue("order.create,order.read"))
      ])
    ]),
    ("order.service", [
      LogEntry::new("INFO", "Order creation started", [
        ("trace_id", StringValue(common_correlation_id)),
        ("request_id", StringValue(common_request_id)),
        ("service_name", StringValue("order.service")),
        ("order_id", StringValue("order-67890"))
      ]),
      LogEntry::new("INFO", "Order validation completed", [
        ("trace_id", StringValue(common_correlation_id)),
        ("request_id", StringValue(common_request_id)),
        ("service_name", StringValue("order.service")),
        ("validation.status", StringValue("passed"))
      ])
    ]),
    ("payment.service", [
      LogEntry::new("INFO", "Payment processing initiated", [
        ("trace_id", StringValue(common_correlation_id)),
        ("request_id", StringValue(common_request_id)),
        ("service_name", StringValue("payment.service")),
        ("payment_id", StringValue("pay-11111"))
      ]),
      LogEntry::new("INFO", "Payment completed successfully", [
        ("trace_id", StringValue(common_correlation_id)),
        ("request_id", StringValue(common_request_id)),
        ("service_name", StringValue("payment.service")),
        ("amount", StringValue("99.99")),
        ("currency", StringValue("USD"))
      ])
    ])
  ]
  
  // 收集所有服务日志
  let mut all_logs = []
  for (service_name, logs) in service_logs {
    for log in logs {
      CrossServiceLoggingManager::collect_log(cross_service_logging, service_name, log)
      all_logs = all_logs.push(log)
      Counter::add(log_counter, 1.0)
    }
  }
  
  // 验证日志关联一致性
  let correlation_success = CrossServiceLoggingManager::verify_log_correlation(cross_service_logging, common_correlation_id)
  let chain_completeness = CrossServiceLoggingManager::verify_log_chain_completeness(cross_service_logging, common_correlation_id)
  
  Gauge::set(correlation_gauge, correlation_success)
  Gauge::set(completeness_gauge, chain_completeness)
  
  Span::set_attribute(log_chain_span, "correlation.success", FloatValue(correlation_success))
  Span::set_attribute(log_chain_span, "chain.completeness", FloatValue(chain_completeness))
  Span::set_status(log_chain_span, Ok)
  Span::end(log_chain_span)
  
  // 验证跨服务日志关联一致性
  assert_true(correlation_success > 0.9) // 应该有90%以上的关联成功率
  assert_true(chain_completeness > 0.8) // 应该有80%以上的完整性
  assert_eq(Counter::value(log_counter), 8.0)
}

// 测试4: 跨服务时间同步一致性
test "跨服务时间同步一致性测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross.service.time")
  
  // 创建跨服务时间同步管理器
  let time_sync = CrossServiceTimeSyncManager::new(meter)
  
  // 创建时间同步指标
  let sync_counter = Meter::create_counter(meter, "time.sync.operations")
  let drift_gauge = Meter::create_gauge(meter, "time.drift.milliseconds")
  let accuracy_gauge = Meter::create_gauge(meter, "time.sync.accuracy.percentage")
  
  // 配置时间同步参数
  let sync_config = TimeSyncConfig::new([
    ("ntp.server", StringValue("pool.ntp.org")),
    ("sync.interval", IntValue(60)), // 60秒
    ("max.drift.ms", IntValue(100)), // 最大允许100ms偏差
    ("sync.timeout", IntValue(5)) // 5秒超时
  ])
  
  CrossServiceTimeSyncManager::configure(time_sync, sync_config)
  
  // 模拟多个服务的时间状态
  let services = [
    ("api.gateway", Time::now() - (20 * 1000)), // 慢20秒
    ("user.service", Time::now() + (10 * 1000)), // 快10秒
    ("order.service", Time::now() - (5 * 1000)),  // 慢5秒
    ("payment.service", Time::now() + (15 * 1000)), // 快15秒
    ("inventory.service", Time::now() - (2 * 1000)) // 慢2秒
  ]
  
  let time_sync_span = Tracer::start_span(TracerProvider::get_tracer(provider, "cross.service.time"), "cross.service.time.synchronization")
  
  // 执行时间同步
  let mut time_drifts = []
  let reference_time = Time::now()
  
  for (service_name, service_time) in services {
    let sync_start = Time::now()
    
    // 计算时间偏差
    let time_drift = Time::duration_between(service_time, reference_time)
    time_drifts = time_drifts.push(time_drift)
    
    // 执行时间同步
    let sync_result = CrossServiceTimeSyncManager::sync_service_time(time_sync, service_name, service_time)
    assert_true(sync_result.success)
    
    let sync_end = Time::now()
    let sync_duration = Time::duration_between(sync_start, sync_end)
    
    Counter::add(sync_counter, 1.0)
    
    Span::add_event(time_sync_span, "service.time.synced", Some("service: " + service_name + ", drift: " + time_drift.to_string() + "ms"))
  }
  
  // 计算时间同步统计
  let max_drift = time_drifts.reduce(fn(acc, drift) { if drift > acc { drift } else { acc } }, 0.0)
  let avg_drift = time_drifts.reduce(fn(acc, drift) { acc + drift }, 0.0) / time_drifts.length().to_float()
  let sync_accuracy = if max_drift > 0.0 { (1.0 - (avg_drift / max_drift)) * 100.0 } else { 100.0 }
  
  Gauge::set(drift_gauge, max_drift)
  Gauge::set(accuracy_gauge, sync_accuracy)
  
  Span::set_attribute(time_sync_span, "max.time.drift", FloatValue(max_drift))
  Span::set_attribute(time_sync_span, "sync.accuracy", FloatValue(sync_accuracy))
  Span::set_status(time_sync_span, Ok)
  Span::end(time_sync_span)
  
  // 验证跨服务时间同步一致性
  assert_true(max_drift < 100.0) // 最大偏差应该小于100ms
  assert_true(sync_accuracy > 80.0) // 同步准确率应该大于80%
  assert_eq(Counter::value(sync_counter), 5.0)
}

// 测试5: 跨服务配置一致性
test "跨服务配置一致性测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross.service.config")
  
  // 创建跨服务配置管理器
  let config_manager = CrossServiceConfigManager::new(meter)
  
  // 创建配置一致性指标
  let config_counter = Meter::create_counter(meter, "config.synchronizations")
  let consistency_gauge = Meter::create_gauge(meter, "config.consistency.score")
  let propagation_gauge = Meter::create_gauge(meter, "config.propagation.latency.ms")
  
  // 定义全局配置
  let global_config = ServiceConfig::new([
    ("telemetry.enabled", BoolValue(true)),
    ("sampling.rate", FloatValue(0.1)),
    ("batch.size", IntValue(100)),
    ("exporter.endpoint", StringValue("http://otel-collector:4317")),
    ("service.version", StringValue("1.2.3")),
    ("environment", StringValue("production"))
  ])
  
  // 配置服务列表
  let services = ["api.gateway", "user.service", "order.service", "payment.service", "inventory.service"]
  
  let config_sync_span = Tracer::start_span(TracerProvider::get_tracer(provider, "cross.service.config"), "cross.service.config.synchronization")
  
  // 分发配置到所有服务
  let mut config_propagation_times = []
  
  for service_name in services {
    let propagation_start = Time::now()
    
    // 推送配置到服务
    let push_result = CrossServiceConfigManager::push_config(config_manager, service_name, global_config)
    assert_true(push_result.success)
    
    let propagation_end = Time::now()
    let propagation_time = Time::duration_between(propagation_start, propagation_end)
    config_propagation_times = config_propagation_times.push(propagation_time)
    
    Histogram::record(propagation_gauge, propagation_time * 1000.0) // 转换为毫秒
    Counter::add(config_counter, 1.0)
    
    Span::add_event(config_sync_span, "config.pushed", Some("service: " + service_name))
  }
  
  // 验证配置一致性
  let mut consistency_scores = []
  
  for service_name in services {
    let service_config = CrossServiceConfigManager::get_service_config(config_manager, service_name)
    let consistency_score = CrossServiceConfigManager::calculate_config_consistency(config_manager, global_config, service_config)
    consistency_scores = consistency_scores.push(consistency_score)
  }
  
  let avg_consistency = consistency_scores.reduce(fn(acc, score) { acc + score }, 0.0) / consistency_scores.length().to_float()
  Gauge::set(consistency_gauge, avg_consistency)
  
  // 模拟配置更新
  let updated_config = ServiceConfig::new([
    ("telemetry.enabled", BoolValue(true)),
    ("sampling.rate", FloatValue(0.2)), // 更新采样率
    ("batch.size", IntValue(200)),      // 更新批量大小
    ("exporter.endpoint", StringValue("http://otel-collector:4317")),
    ("service.version", StringValue("1.2.4")), // 更新版本
    ("environment", StringValue("production"))
  ])
  
  // 分发更新后的配置
  for service_name in services {
    let update_result = CrossServiceConfigManager::update_config(config_manager, service_name, updated_config)
    assert_true(update_result.success)
    
    Span::add_event(config_sync_span, "config.updated", Some("service: " + service_name))
  }
  
  // 验证配置更新一致性
  let mut update_consistency_scores = []
  
  for service_name in services {
    let service_config = CrossServiceConfigManager::get_service_config(config_manager, service_name)
    let consistency_score = CrossServiceConfigManager::calculate_config_consistency(config_manager, updated_config, service_config)
    update_consistency_scores = update_consistency_scores.push(consistency_score)
  }
  
  let avg_update_consistency = update_consistency_scores.reduce(fn(acc, score) { acc + score }, 0.0) / update_consistency_scores.length().to_float()
  
  Span::set_attribute(config_sync_span, "initial.consistency", FloatValue(avg_consistency))
  Span::set_attribute(config_sync_span, "update.consistency", FloatValue(avg_update_consistency))
  Span::set_status(config_sync_span, Ok)
  Span::end(config_sync_span)
  
  // 验证跨服务配置一致性
  assert_true(avg_consistency > 0.95) // 初始配置一致性应该大于95%
  assert_true(avg_update_consistency > 0.95) // 更新后配置一致性应该大于95%
  assert_eq(Counter::value(config_counter), 10.0) // 5个服务初始配置 + 5个服务更新配置
}

// 测试6: 跨服务状态一致性
test "跨服务状态一致性测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross.service.state")
  
  // 创建跨服务状态管理器
  let state_manager = CrossServiceStateManager::new(meter)
  
  // 创建状态一致性指标
  let state_counter = Meter::create_counter(meter, "state.synchronizations")
  let consistency_gauge = Meter::create_gauge(meter, "state.consistency.percentage")
  let convergence_gauge = Meter::create_gauge(meter, "state.convergence.time.seconds")
  
  // 配置状态同步参数
  let state_config = StateSyncConfig::new([
    ("sync.strategy", StringValue("eventual.consistency")),
    ("conflict.resolution", StringValue("last.write.wins")),
    ("sync.interval", IntValue(30)),
    ("max.sync.retries", IntValue(3))
  ])
  
  CrossServiceStateManager::configure(state_manager, state_config)
  
  // 定义服务状态
  let services = ["order.service", "payment.service", "inventory.service", "notification.service"]
  
  let state_sync_span = Tracer::start_span(TracerProvider::get_tracer(provider, "cross.service.state"), "cross.service.state.synchronization")
  
  // 模拟订单处理流程中的状态变化
  let order_id = "order-" + Time::now().to_string()
  
  // 初始状态：订单创建
  let initial_state = ServiceState::new([
    ("order.id", StringValue(order_id)),
    ("order.status", StringValue("created")),
    ("payment.status", StringValue("pending")),
    ("inventory.status", StringValue("reserved")),
    ("notification.status", StringValue("pending"))
  ])
  
  // 分发初始状态到所有服务
  for service_name in services {
    CrossServiceStateManager::set_service_state(state_manager, service_name, initial_state)
    Counter::add(state_counter, 1.0)
  }
  
  // 状态变化1：支付处理
  let convergence_start = Time::now()
  
  let payment_state = ServiceState::new([
    ("order.id", StringValue(order_id)),
    ("order.status", StringValue("processing")),
    ("payment.status", StringValue("processing")),
    ("inventory.status", StringValue("reserved")),
    ("notification.status", StringValue("pending"))
  ])
  
  CrossServiceStateManager::update_service_state(state_manager, "payment.service", payment_state)
  
  // 等待状态同步
  let sync_result1 = CrossServiceStateManager::wait_for_convergence(state_manager, order_id, 10.0) // 10秒超时
  assert_true(sync_result1.converged)
  
  // 状态变化2：支付完成
  let payment_completed_state = ServiceState::new([
    ("order.id", StringValue(order_id)),
    ("order.status", StringValue("paid")),
    ("payment.status", StringValue("completed")),
    ("inventory.status", StringValue("reserved")),
    ("notification.status", StringValue("pending"))
  ])
  
  CrossServiceStateManager::update_service_state(state_manager, "payment.service", payment_completed_state)
  
  // 等待状态同步
  let sync_result2 = CrossServiceStateManager::wait_for_convergence(state_manager, order_id, 10.0)
  assert_true(sync_result2.converged)
  
  // 状态变化3：库存确认
  let inventory_confirmed_state = ServiceState::new([
    ("order.id", StringValue(order_id)),
    ("order.status", StringValue("confirmed")),
    ("payment.status", StringValue("completed")),
    ("inventory.status", StringValue("committed")),
    ("notification.status", StringValue("pending"))
  ])
  
  CrossServiceStateManager::update_service_state(state_manager, "inventory.service", inventory_confirmed_state)
  
  // 等待状态同步
  let sync_result3 = CrossServiceStateManager::wait_for_convergence(state_manager, order_id, 10.0)
  assert_true(sync_result3.converged)
  
  // 状态变化4：通知发送
  let notification_sent_state = ServiceState::new([
    ("order.id", StringValue(order_id)),
    ("order.status", StringValue("completed")),
    ("payment.status", StringValue("completed")),
    ("inventory.status", StringValue("committed")),
    ("notification.status", StringValue("sent"))
  ])
  
  CrossServiceStateManager::update_service_state(state_manager, "notification.service", notification_sent_state)
  
  // 等待最终状态同步
  let final_sync_result = CrossServiceStateManager::wait_for_convergence(state_manager, order_id, 10.0)
  assert_true(final_sync_result.converged)
  
  let convergence_end = Time::now()
  let total_convergence_time = Time::duration_between(convergence_start, convergence_end)
  Gauge::set(convergence_gauge, total_convergence_time)
  
  // 验证最终状态一致性
  let final_state = CrossServiceStateManager::get_converged_state(state_manager, order_id)
  let state_consistency = CrossServiceStateManager::calculate_state_consistency(state_manager, order_id)
  Gauge::set(consistency_gauge, state_consistency)
  
  Span::set_attribute(state_sync_span, "convergence.time", FloatValue(total_convergence_time))
  Span::set_attribute(state_sync_span, "final.consistency", FloatValue(state_consistency))
  Span::set_status(state_sync_span, Ok)
  Span::end(state_sync_span)
  
  // 验证跨服务状态一致性
  assert_true(final_sync_result.converged)
  assert_true(state_consistency > 0.95) // 状态一致性应该大于95%
  assert_true(total_convergence_time < 30.0) // 收敛时间应该小于30秒
  assert_eq(Counter::value(state_counter), 8.0) // 4个服务初始状态 + 4个服务状态更新
}

// 测试7: 跨服务事务一致性
test "跨服务事务一致性测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross.service.transaction")
  
  // 创建跨服务事务管理器
  let transaction_manager = CrossServiceTransactionManager::new(meter)
  
  // 创建事务一致性指标
  let transaction_counter = Meter::create_counter(meter, "cross.service.transactions")
  let commit_gauge = Meter::create_gauge(meter, "transaction.commit.success.rate")
  let rollback_gauge = Meter::create_gauge(meter, "transaction.rollback.rate")
  
  // 配置分布式事务参数
  let transaction_config = TransactionConfig::new([
    ("isolation.level", StringValue("read.committed")),
    ("timeout.seconds", IntValue(30)),
    ("retry.attempts", IntValue(3)),
    ("compensation.strategy", StringValue("saga"))
  ])
  
  CrossServiceTransactionManager::configure(transaction_manager, transaction_config)
  
  let transaction_span = Tracer::start_span(TracerProvider::get_tracer(provider, "cross.service.transaction"), "distributed.transaction.processing")
  
  // 开始分布式事务
  let transaction_id = CrossServiceTransactionManager::begin_transaction(transaction_manager, "order.processing")
  Span::set_attribute(transaction_span, "transaction.id", StringValue(transaction_id))
  
  // 事务参与者
  let participants = [
    ("order.service", TransactionParticipant::new("order.service", [
      ("create.order", CompensationAction::new("cancel.order"))
    ])),
    ("payment.service", TransactionParticipant::new("payment.service", [
      ("process.payment", CompensationAction::new("refund.payment"))
    ])),
    ("inventory.service", TransactionParticipant::new("inventory.service", [
      ("reserve.inventory", CompensationAction::new("release.inventory"))
    ])),
    ("notification.service", TransactionParticipant::new("notification.service", [
      ("send.notification", CompensationAction::new("revoke.notification"))
    ]))
  ]
  
  // 注册事务参与者
  for (service_name, participant) in participants {
    CrossServiceTransactionManager::register_participant(transaction_manager, transaction_id, service_name, participant)
  }
  
  // 执行事务步骤
  let transaction_steps = [
    ("order.service", "create.order", fn() { true }),  // 成功
    ("payment.service", "process.payment", fn() { true }), // 成功
    ("inventory.service", "reserve.inventory", fn() { false }), // 失败
    ("notification.service", "send.notification", fn() { true }) // 不会执行
  ]
  
  let mut step_results = []
  
  for (service_name, operation, operation_fn) in transaction_steps {
    let step_result = CrossServiceTransactionManager::execute_step(transaction_manager, transaction_id, service_name, operation, operation_fn)
    step_results = step_results.push(step_result)
    
    Span::add_event(transaction_span, "transaction.step", Some("service: " + service_name + ", operation: " + operation + ", success: " + step_result.success.to_string()))
    
    Counter::add(transaction_counter, 1.0)
    
    // 如果步骤失败，停止后续步骤
    if !step_result.success {
      break
    }
  }
  
  // 检查事务结果
  let all_steps_succeeded = step_results.filter(fn(result) { result.success }).length() == step_results.length()
  
  if all_steps_succeeded {
    // 提交事务
    let commit_result = CrossServiceTransactionManager::commit_transaction(transaction_manager, transaction_id)
    assert_true(commit_result.success)
    Span::add_event(transaction_span, "transaction.committed", None)
  } else {
    // 回滚事务
    let rollback_result = CrossServiceTransactionManager::rollback_transaction(transaction_manager, transaction_id)
    assert_true(rollback_result.success)
    Span::add_event(transaction_span, "transaction.rolled.back", None)
  }
  
  // 计算事务统计
  let transaction_stats = CrossServiceTransactionManager::get_transaction_stats(transaction_manager)
  Gauge::set(commit_gauge, transaction_stats.commit_success_rate)
  Gauge::set(rollback_gauge, transaction_stats.rollback_rate)
  
  Span::set_attribute(transaction_span, "commit.success.rate", FloatValue(transaction_stats.commit_success_rate))
  Span::set_attribute(transaction_span, "rollback.rate", FloatValue(transaction_stats.rollback_rate))
  Span::set_status(transaction_span, Ok)
  Span::end(transaction_span)
  
  // 验证跨服务事务一致性
  assert_false(all_steps_succeeded) // 应该有步骤失败
  assert_true(transaction_stats.rollback_rate > 0.0) // 应该有回滚
  assert_eq(Counter::value(transaction_counter), 4.0)
}

// 测试8: 跨服务数据一致性验证
test "跨服务数据一致性验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross.service.data.consistency")
  
  // 创建跨服务数据一致性验证器
  let consistency_validator = CrossServiceConsistencyValidator::new(meter)
  
  // 创建一致性验证指标
  let validation_counter = Meter::create_counter(meter, "consistency.validations")
  let inconsistency_gauge = Meter::create_gauge(meter, "data.inconsistencies.detected")
  let resolution_gauge = Meter::create_gauge(meter, "inconsistency.resolution.success.rate")
  
  // 配置一致性验证规则
  let validation_rules = [
    ConsistencyRule::new("order.payment.match", [
      ("field1", StringValue("orders.total_amount")),
      ("field2", StringValue("payments.amount")),
      ("operator", StringValue("equals")),
      ("tolerance", FloatValue(0.01)) // 1%容差
    ]),
    ConsistencyRule::new("inventory.reservation.match", [
      ("field1", StringValue("order_items.quantity")),
      ("field2", StringValue("inventory.reserved_quantity")),
      ("operator", StringValue("equals")),
      ("tolerance", FloatValue(0.0))
    ]),
    ConsistencyRule::new("user.permission.consistency", [
      ("field1", StringValue("users.role")),
      ("field2", StringValue("permissions.required_role")),
      ("operator", StringValue("contains")),
      ("tolerance", FloatValue(0.0))
    ])
  ]
  
  for rule in validation_rules {
    ConsistencyValidator::add_rule(consistency_validator, rule)
  }
  
  let validation_span = Tracer::start_span(TracerProvider::get_tracer(provider, "cross.service.data.consistency"), "cross.service.data.consistency.validation")
  
  // 生成测试数据集
  let test_datasets = [
    ("orders", [
      ("order_id", StringValue("order-001")),
      ("user_id", StringValue("user-123")),
      ("total_amount", FloatValue(99.99)),
      ("status", StringValue("completed"))
    ]),
    ("payments", [
      ("payment_id", StringValue("pay-001")),
      ("order_id", StringValue("order-001")),
      ("amount", FloatValue(99.99)), // 匹配订单金额
      ("status", StringValue("completed"))
    ]),
    ("order_items", [
      ("item_id", StringValue("item-001")),
      ("order_id", StringValue("order-001")),
      ("product_id", StringValue("prod-123")),
      ("quantity", IntValue(2))
    ]),
    ("inventory", [
      ("product_id", StringValue("prod-123")),
      ("reserved_quantity", IntValue(2)), // 匹配订单项数量
      ("available_quantity", IntValue(98))
    ]),
    ("users", [
      ("user_id", StringValue("user-123")),
      ("username", StringValue("john.doe")),
      ("role", StringValue("premium"))
    ]),
    ("permissions", [
      ("permission_id", StringValue("perm-001")),
      ("user_id", StringValue("user-123")),
      ("required_role", StringValue("premium")) // 匹配用户角色
    ])
  ]
  
  // 添加数据集到验证器
  for (dataset_name, data) in test_datasets {
    ConsistencyValidator::add_dataset(consistency_validator, dataset_name, data)
  }
  
  // 执行一致性验证
  let validation_results = ConsistencyValidator::validate_all_rules(consistency_validator)
  
  let mut inconsistencies_detected = 0
  let mut resolutions_attempted = 0
  let mut resolutions_successful = 0
  
  for result in validation_results {
    Counter::add(validation_counter, 1.0)
    
    if !result.consistent {
      inconsistencies_detected = inconsistencies_detected + 1
      
      // 尝试解决不一致
      let resolution_result = ConsistencyValidator::resolve_inconsistency(consistency_validator, result)
      resolutions_attempted = resolutions_attempted + 1
      
      if resolution_result.success {
        resolutions_successful = resolutions_successful + 1
      }
    }
  }
  
  let resolution_success_rate = if resolutions_attempted > 0 {
    resolutions_successful.to_float() / resolutions_attempted.to_float()
  } else {
    1.0
  }
  
  Gauge::set(inconsistency_gauge, inconsistencies_detected.to_float())
  Gauge::set(resolution_gauge, resolution_success_rate)
  
  Span::set_attribute(validation_span, "inconsistencies.detected", IntValue(inconsistencies_detected))
  Span::set_attribute(validation_span, "resolution.success.rate", FloatValue(resolution_success_rate))
  Span::set_status(validation_span, Ok)
  Span::end(validation_span)
  
  // 验证跨服务数据一致性
  assert_eq(inconsistencies_detected, 0) // 应该没有不一致
  assert_eq(Counter::value(validation_counter), 3.0) // 3个验证规则
}

// 测试9: 跨服务负载均衡一致性
test "跨服务负载均衡一致性测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross.service.load.balancing")
  
  // 创建跨服务负载均衡管理器
  let load_balancer = CrossServiceLoadBalancer::new(meter)
  
  // 创建负载均衡指标
  let balance_counter = Meter::create_counter(meter, "load.balancing.operations")
  let distribution_gauge = Meter::create_gauge(meter, "load.distribution.score")
  let health_gauge = Meter::create_gauge(meter, "service.health.consistency")
  
  // 配置负载均衡策略
  let lb_config = LoadBalancingConfig::new([
    ("algorithm", StringValue("weighted.round.robin")),
    ("health.check.interval", IntValue(10)),
    ("failure.threshold", IntValue(3)),
    ("recovery.threshold", IntValue(2))
  ])
  
  CrossServiceLoadBalancer::configure(load_balancer, lb_config)
  
  let load_balance_span = Tracer::start_span(TracerProvider::get_tracer(provider, "cross.service.load.balancing"), "cross.service.load.balancing")
  
  // 定义服务实例
  let service_instances = [
    ("user.service", [
      ServiceInstance::new("user-service-1", "http://user-service-1:8080", 80),
      ServiceInstance::new("user-service-2", "http://user-service-2:8080", 60),
      ServiceInstance::new("user-service-3", "http://user-service-3:8080", 40)
    ]),
    ("order.service", [
      ServiceInstance::new("order-service-1", "http://order-service-1:8080", 70),
      ServiceInstance::new("order-service-2", "http://order-service-2:8080", 90)
    ]),
    ("payment.service", [
      ServiceInstance::new("payment-service-1", "http://payment-service-1:8080", 100)
    ])
  ]
  
  // 注册服务实例
  for (service_name, instances) in service_instances {
    for instance in instances {
      CrossServiceLoadBalancer::register_instance(load_balancer, service_name, instance)
    }
  }
  
  // 模拟负载均衡请求
  let total_requests = 1000
  let mut request_distribution = []
  
  for i in 0..=total_requests - 1 {
    // 选择服务
    let service_name = if i % 3 == 0 { "user.service" } else if i % 3 == 1 { "order.service" } else { "payment.service" }
    
    // 负载均衡选择实例
    let selected_instance = CrossServiceLoadBalancer::select_instance(load_balancer, service_name)
    assert_true(selected_instance.is_some())
    
    match selected_instance {
      Some(instance) => {
        // 记录请求分布
        let instance_id = ServiceInstance::id(instance)
        let existing = request_distribution.find(fn((id, _) { id == instance_id }))
        match existing {
          Some((_, count)) => {
            // 更新计数
          }
          None => {
            request_distribution = request_distribution.push((instance_id, 1))
          }
        }
        
        Counter::add(balance_counter, 1.0)
      }
      None => assert_true(false)
    }
  }
  
  // 计算负载分布分数
  let distribution_score = CrossServiceLoadBalancer::calculate_distribution_score(load_balancer, request_distribution)
  Gauge::set(distribution_gauge, distribution_score)
  
  // 计算服务健康一致性
  let health_consistency = CrossServiceLoadBalancer::calculate_health_consistency(load_balancer)
  Gauge::set(health_gauge, health_consistency)
  
  Span::set_attribute(load_balance_span, "distribution.score", FloatValue(distribution_score))
  Span::set_attribute(load_balance_span, "health.consistency", FloatValue(health_consistency))
  Span::set_status(load_balance_span, Ok)
  Span::end(load_balance_span)
  
  // 验证跨服务负载均衡一致性
  assert_true(distribution_score > 0.8) // 分布分数应该大于80%
  assert_true(health_consistency > 0.9) // 健康一致性应该大于90%
  assert_eq(Counter::value(balance_counter), 1000.0)
}

// 测试10: 跨服务缓存一致性
test "跨服务缓存一致性测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross.service.cache")
  
  // 创建跨服务缓存管理器
  let cache_manager = CrossServiceCacheManager::new(meter)
  
  // 创建缓存一致性指标
  let cache_counter = Meter::create_counter(meter, "cache.operations")
  let hit_gauge = Meter::create_gauge(meter, "cache.hit.rate")
  let consistency_gauge = Meter::create_gauge(meter, "cache.consistency.score")
  
  // 配置缓存一致性策略
  let cache_config = CacheConfig::new([
    ("consistency.strategy", StringValue("invalidation")),
    ("ttl.seconds", IntValue(300)), // 5分钟TTL
    ("max.size", IntValue(10000)),
    ("sync.interval", IntValue(30))
  ])
  
  CrossServiceCacheManager::configure(cache_manager, cache_config)
  
  let cache_span = Tracer::start_span(TracerProvider::get_tracer(provider, "cross.service.cache"), "cross.service.cache.consistency")
  
  // 定义服务缓存
  let services = ["user.service", "order.service", "product.service", "catalog.service"]
  
  for service_name in services {
    CrossServiceCacheManager::register_service_cache(cache_manager, service_name)
  }
  
  // 模拟缓存操作
  let cache_operations = [
    ("user.service", "user-123", UserCacheData::new("user-123", "john.doe", "john@example.com")),
    ("user.service", "user-456", UserCacheData::new("user-456", "jane.doe", "jane@example.com")),
    ("order.service", "order-789", OrderCacheData::new("order-789", "user-123", 99.99)),
    ("product.service", "prod-001", ProductCacheData::new("prod-001", "Laptop", 999.99)),
    ("catalog.service", "cat-001", CatalogCacheData::new("cat-001", ["prod-001", "prod-002"]))
  ]
  
  // 执行缓存写入操作
  for (service_name, key, data) in cache_operations {
    CrossServiceCacheManager::put(cache_manager, service_name, key, data)
    Counter::add(cache_counter, 1.0)
  }
  
  // 执行缓存读取操作
  let mut cache_hits = 0
  let mut cache_misses = 0
  
  for (service_name, key, _) in cache_operations {
    let cache_result = CrossServiceCacheManager::get(cache_manager, service_name, key)
    
    match cache_result {
      Some(_) => cache_hits = cache_hits + 1,
      None => cache_misses = cache_misses + 1
    }
    
    Counter::add(cache_counter, 1.0)
  }
  
  // 计算缓存命中率
  let cache_hit_rate = if (cache_hits + cache_misses) > 0 {
    cache_hits.to_float() / (cache_hits + cache_misses).to_float()
  } else {
    0.0
  }
  Gauge::set(hit_gauge, cache_hit_rate)
  
  // 模拟缓存更新和失效
  let updated_user_data = UserCacheData::new("user-123", "john.doe.updated", "john.updated@example.com")
  CrossServiceCacheManager::put(cache_manager, "user.service", "user-123", updated_user_data)
  CrossServiceCacheManager::invalidate_related(cache_manager, "user.service", "user-123")
  
  Counter::add(cache_counter, 2.0) // 一次put，一次invalidate
  
  // 验证缓存一致性
  let consistency_score = CrossServiceCacheManager::verify_consistency(cache_manager)
  Gauge::set(consistency_gauge, consistency_score)
  
  Span::set_attribute(cache_span, "cache.hit.rate", FloatValue(cache_hit_rate))
  Span::set_attribute(cache_span, "consistency.score", FloatValue(consistency_score))
  Span::set_status(cache_span, Ok)
  Span::end(cache_span)
  
  // 验证跨服务缓存一致性
  assert_true(cache_hit_rate > 0.8) // 缓存命中率应该大于80%
  assert_true(consistency_score > 0.9) // 一致性分数应该大于90%
  assert_eq(Counter::value(cache_counter), 12.0) // 5次put + 5次get + 2次更新操作
}

// 辅助数据结构定义
type UserCacheData {
  user_id: String
  username: String
  email: String
}

type OrderCacheData {
  order_id: String
  user_id: String
  amount: Float
}

type ProductCacheData {
  product_id: String
  name: String
  price: Float
}

type CatalogCacheData {
  catalog_id: String
  product_ids: Array[String]
}

// 辅助函数实现
fn UserCacheData::new(user_id : String, username : String, email : String) -> UserCacheData {
  { user_id: user_id, username: username, email: email }
}

fn OrderCacheData::new(order_id : String, user_id : String, amount : Float) -> OrderCacheData {
  { order_id: order_id, user_id: user_id, amount: amount }
}

fn ProductCacheData::new(product_id : String, name : String, price : Float) -> ProductCacheData {
  { product_id: product_id, name: name, price: price }
}

fn CatalogCacheData::new(catalog_id : String, product_ids : Array[String]) -> CatalogCacheData {
  { catalog_id: catalog_id, product_ids: product_ids }
}