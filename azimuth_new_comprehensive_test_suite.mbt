// Azimuth New Comprehensive Test Suite
// 新的综合测试套件，涵盖遥测系统的各个方面

// Test 1: 高性能时间序列数据处理
test "high-performance time series data processing" {
  // 创建时间序列数据处理器
  let processor = TimeSeriesProcessor::new()
  
  // 添加时间序列数据点
  let data_points = [
    TimeSeriesPoint::new(1735689600L, 45.2, [("metric.type", "response.time")]),
    TimeSeriesPoint::new(1735689660L, 52.8, [("metric.type", "response.time")]),
    TimeSeriesPoint::new(1735689720L, 38.1, [("metric.type", "response.time")]),
    TimeSeriesPoint::new(1735689780L, 67.4, [("metric.type", "response.time")]),
    TimeSeriesPoint::new(1735689840L, 41.9, [("metric.type", "response.time")])
  ]
  
  // 批量处理数据点
  for point in data_points {
    TimeSeriesProcessor::add_point(processor, point)
  }
  
  // 计算统计指标
  let stats = TimeSeriesProcessor::calculate_statistics(processor)
  assert_true(stats.average > 40.0)
  assert_true(stats.max > 60.0)
  assert_true(stats.min < 50.0)
  assert_eq(stats.count, 5)
  
  // 测试时间窗口查询
  let window_start = 1735689600L
  let window_end = 1735689720L
  let window_data = TimeSeriesProcessor::query_window(processor, window_start, window_end)
  assert_eq(window_data.length(), 3)
}

// Test 2: 动态配置热更新
test "dynamic configuration hot updates" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 设置初始配置
  let initial_config = [
    ("telemetry.sampling.rate", "0.1"),
    ("telemetry.batch.size", "100"),
    ("telemetry.export.interval", "5000"),
    ("telemetry.max.queue.size", "1000")
  ]
  
  for (key, value) in initial_config {
    ConfigurationManager::set(config_manager, key, value)
  }
  
  // 验证初始配置
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling.rate"), Some("0.1"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.batch.size"), Some("100"))
  
  // 测试配置热更新
  ConfigurationManager::update(config_manager, "telemetry.sampling.rate", "0.2")
  ConfigurationManager::update(config_manager, "telemetry.batch.size", "200")
  
  // 验证更新后的配置
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling.rate"), Some("0.2"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.batch.size"), Some("200"))
  
  // 测试配置变更通知
  let notification_received = ref false
  ConfigurationManager::register_change_handler(config_manager, "telemetry.sampling.rate", fn(old_val, new_val) {
    assert_eq(old_val, "0.2")
    assert_eq(new_val, "0.3")
    notification_received := true
  })
  
  ConfigurationManager::update(config_manager, "telemetry.sampling.rate", "0.3")
  assert_true(notification_received)
}

// Test 3: 跨服务上下文传播
test "cross-service context propagation" {
  // 创建服务A的上下文
  let service_a_context = Context::root()
  let trace_id = TraceId::generate()
  let span_a = SpanBuilder::new("service-a-operation")
    .with_trace_id(trace_id)
    .with_attributes([("service.name", "service-a"), ("service.version", "1.0.0")])
    .start()
  
  // 从服务A传播到服务B
  let propagated_context = Context::with_span(service_a_context, span_a)
  let injected_headers = Propagator::inject(propagated_context)
  
  // 验证注入的头部信息
  assert_true(injected_headers.contains("traceparent"))
  assert_true(injected_headers.contains("tracestate"))
  
  // 服务B提取上下文
  let service_b_context = Propagator::extract(injected_headers)
  let extracted_span = Context::get_span(service_b_context)
  
  // 验证上下文正确传播
  assert_eq(Span::trace_id(extracted_span), trace_id)
  assert_true(Span::is_valid(extracted_span))
  
  // 在服务B中创建子span
  let span_b = SpanBuilder::new("service-b-operation")
    .with_parent(extracted_span)
    .with_attributes([("service.name", "service-b"), ("service.version", "2.0.0")])
    .start()
  
  // 验证父子关系
  assert_eq(Span::parent_span_id(span_b), Span::span_id(extracted_span))
  assert_eq(Span::trace_id(span_b), trace_id)
  
  Span::end(span_a)
  Span::end(span_b)
}

// Test 4: 智能采样策略
test "intelligent sampling strategies" {
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new()
  
  // 配置采样策略
  AdaptiveSampler::configure(adaptive_sampler, [
    ("error.sample.rate", "1.0"),        // 错误span 100% 采样
    ("slow.sample.rate", "0.5"),         // 慢span 50% 采样
    ("normal.sample.rate", "0.1"),       // 正常span 10% 采样
    ("latency.threshold", "1000")        // 超过1秒认为是慢span
  ])
  
  // 测试错误span采样
  let error_span = TestSpan::new("error.operation")
  Span::set_status(error_span, Error, Some("Operation failed"))
  assert_true(AdaptiveSampler::should_sample(adaptive_sampler, error_span))
  
  // 测试慢span采样
  let slow_span = TestSpan::new("slow.operation")
  Span::set_attribute(slow_span, "duration", 1500)
  assert_true(AdaptiveSampler::should_sample(adaptive_sampler, slow_span))
  
  // 测试正常span采样（部分采样）
  let normal_span = TestSpan::new("normal.operation")
  Span::set_attribute(normal_span, "duration", 200)
  
  // 由于正常span只有10%采样率，多次测试确保采样策略工作
  let mut sample_count = 0
  for i in 1..=100 {
    if AdaptiveSampler::should_sample(adaptive_sampler, normal_span) {
      sample_count = sample_count + 1
    }
  }
  
  // 验证采样率在合理范围内（5-15%）
  assert_true(sample_count >= 5)
  assert_true(sample_count <= 15)
}

// Test 5: 度量数据聚合和分析
test "metrics data aggregation and analysis" {
  // 创建度量聚合器
  let aggregator = MetricsAggregator::new()
  
  // 添加不同类型的度量数据
  let counter_metrics = [
    Metric::counter("http.requests.total", 100, [("method", "GET"), ("status", "200")]),
    Metric::counter("http.requests.total", 25, [("method", "POST"), ("status", "201")]),
    Metric::counter("http.requests.total", 5, [("method", "GET"), ("status", "404")])
  ]
  
  let histogram_metrics = [
    Metric::histogram("http.request.duration", 45.2, [("endpoint", "/api/users")]),
    Metric::histogram("http.request.duration", 123.7, [("endpoint", "/api/orders")]),
    Metric::histogram("http.request.duration", 78.9, [("endpoint", "/api/users")]),
    Metric::histogram("http.request.duration", 256.1, [("endpoint", "/api/products")])
  ]
  
  // 聚合计数器度量
  for metric in counter_metrics {
    MetricsAggregator::add(aggregator, metric)
  }
  
  let counter_results = MetricsAggregator::aggregate_counters(aggregator)
  let get_requests = counter_results.get_or_default([("method", "GET")], 0)
  let post_requests = counter_results.get_or_default([("method", "POST")], 0)
  
  assert_eq(get_requests, 105)  // 100 GET 200 + 5 GET 404
  assert_eq(post_requests, 25)
  
  // 聚合直方图度量
  for metric in histogram_metrics {
    MetricsAggregator::add(aggregator, metric)
  }
  
  let histogram_results = MetricsAggregator::aggregate_histograms(aggregator)
  let users_endpoint_stats = histogram_results.get([("endpoint", "/api/users")])
  
  match users_endpoint_stats {
    Some(stats) => {
      assert_eq(stats.count, 2)
      assert_true(stats.average > 60.0)  // (45.2 + 78.9) / 2
    }
    None => assert_true(false)
  }
}

// Test 6: 异常恢复和弹性机制
test "exception recovery and resilience mechanisms" {
  // 创建弹性管理器
  let resilience_manager = ResilienceManager::new()
  
  // 配置断路器
  let circuit_breaker = CircuitBreaker::new("database.connection")
    .with_failure_threshold(5)      // 5次失败后打开断路器
    .with_recovery_timeout(10000)   // 10秒后尝试半开状态
    .with_success_threshold(3)      // 3次成功后关闭断路器
  
  ResilienceManager::register_circuit_breaker(resilience_manager, circuit_breaker)
  
  // 模拟失败操作
  let mut failure_count = 0
  for i in 1..=6 {
    let result = ResilienceManager::execute(resilience_manager, "database.connection", fn() {
      failure_count = failure_count + 1
      if failure_count <= 5 {
        Err("Connection failed")
      } else {
        Ok("Success")
      }
    })
    
    if i <= 5 {
      // 前5次应该失败
      match result {
        Err(_) => assert_true(true)
        Ok(_) => assert_true(false)
      }
    } else {
      // 第6次应该被断路器阻止
      match result {
        Err(msg) => assert_eq(msg, "Circuit breaker is open")
        Ok(_) => assert_true(false)
      }
    }
  }
  
  // 验证断路器状态
  let breaker_state = ResilienceManager::get_circuit_breaker_state(resilience_manager, "database.connection")
  assert_eq(breaker_state, "open")
  
  // 测试重试机制
  let mut attempt_count = 0
  let retry_result = ResilienceManager::execute_with_retry(resilience_manager, "api.call", fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err("Temporary failure")
    } else {
      Ok("Final success")
    }
  }, 3, 1000)  // 最多重试3次，间隔1秒
  
  match retry_result {
    Ok(value) => assert_eq(value, "Final success")
    Err(_) => assert_true(false)
  }
  
  assert_eq(attempt_count, 3)
}

// Test 7: 多维度属性查询
test "multi-dimensional attribute queries" {
  // 创建属性存储
  let attribute_store = AttributeStore::new()
  
  // 添加带有复杂属性的span
  let spans = [
    ("span-1", [
      ("service.name", "user-service"),
      ("operation.name", "get_user"),
      ("user.id", "12345"),
      ("region", "us-west-2"),
      ("http.method", "GET"),
      ("http.status_code", "200")
    ]),
    ("span-2", [
      ("service.name", "user-service"),
      ("operation.name", "create_user"),
      ("user.id", "67890"),
      ("region", "us-west-2"),
      ("http.method", "POST"),
      ("http.status_code", "201")
    ]),
    ("span-3", [
      ("service.name", "order-service"),
      ("operation.name", "get_order"),
      ("user.id", "12345"),
      ("region", "us-east-1"),
      ("http.method", "GET"),
      ("http.status_code", "200")
    ]),
    ("span-4", [
      ("service.name", "user-service"),
      ("operation.name", "get_user"),
      ("user.id", "12345"),
      ("region", "us-west-2"),
      ("http.method", "GET"),
      ("http.status_code", "404")
    ])
  ]
  
  for (span_id, attributes) in spans {
    AttributeStore::store_span_attributes(attribute_store, span_id, attributes)
  }
  
  // 测试单维度查询
  let user_service_spans = AttributeStore::query_by_attribute(attribute_store, "service.name", "user-service")
  assert_eq(user_service_spans.length(), 3)
  
  let get_operations = AttributeStore::query_by_attribute(attribute_store, "operation.name", "get_user")
  assert_eq(get_operations.length(), 2)
  
  // 测试多维度查询
  let multi_query_results = AttributeStore::query_by_attributes(attribute_store, [
    ("service.name", "user-service"),
    ("operation.name", "get_user"),
    ("user.id", "12345")
  ])
  assert_eq(multi_query_results.length(), 2)
  
  // 测试范围查询
  let status_range_results = AttributeStore::query_by_attribute_range(
    attribute_store, 
    "http.status_code", 
    "200", 
    "299"
  )
  assert_eq(status_range_results.length(), 3)
  
  // 测试模糊查询
  let user_service模糊 = AttributeStore::query_by_attribute_pattern(
    attribute_store, 
    "service.name", 
    "*-service"
  )
  assert_eq(user_service模糊.length(), 4)
}

// Test 8: 实时流处理和监控
test "real-time stream processing and monitoring" {
  // 创建流处理器
  let stream_processor = StreamProcessor::new()
  
  // 配置流处理规则
  StreamProcessor::add_rule(stream_processor, "error_rate_alert", fn(events) {
    let error_count = events.filter(fn(e) { e.severity == Error }).length()
    let total_count = events.length()
    
    if total_count > 0 {
      let error_rate = (error_count * 100) / total_count
      if error_rate > 10 {  // 错误率超过10%
        return Some(Alert::new("high_error_rate", "Error rate is " + error_rate.to_string() + "%", Warning))
      }
    }
    None
  })
  
  StreamProcessor::add_rule(stream_processor, "latency_spike_alert", fn(events) {
    let avg_latency = events
      .filter(fn(e) { e.has_attribute("duration") })
      .map(fn(e) { e.get_attribute("duration") })
      .reduce(fn(acc, val) { acc + val }, 0) / events.length()
    
    if avg_latency > 1000 {  // 平均延迟超过1秒
      return Some(Alert::new("high_latency", "Average latency is " + avg_latency.to_string() + "ms", Warning))
    }
    None
  })
  
  // 生成测试事件流
  let events = [
    Event::new("request.completed", Info, [("duration", "200"), ("status", "200")]),
    Event::new("request.completed", Info, [("duration", "300"), ("status", "200")]),
    Event::new("request.completed", Error, [("duration", "5000"), ("status", "500")]),
    Event::new("request.completed", Info, [("duration", "150"), ("status", "200")]),
    Event::new("request.completed", Error, [("duration", "3000"), ("status", "500")]),
    Event::new("request.completed", Info, [("duration", "250"), ("status", "200")]),
    Event::new("request.completed", Info, [("duration", "180"), ("status", "200")]),
    Event::new("request.completed", Error, [("duration", "2000"), ("status", "500")]),
    Event::new("request.completed", Info, [("duration", "220"), ("status", "200")]),
    Event::new("request.completed", Info, [("duration", "190"), ("status", "200")])
  ]
  
  // 处理事件流
  let alerts = []
  for event in events {
    let event_alerts = StreamProcessor::process_event(stream_processor, event)
    alerts = alerts.concat(event_alerts)
  }
  
  // 验证生成的警报
  assert_eq(alerts.length(), 2)  // 应该有两个警报：高错误率和高延迟
  
  let error_alert = alerts.find(fn(a) { a.alert_type == "high_error_rate" })
  let latency_alert = alerts.find(fn(a) { a.alert_type == "high_latency" })
  
  match error_alert {
    Some(alert) => {
      assert_eq(alert.severity, Warning)
      assert_true(alert.message.contains("30"))  // 3/10 = 30% 错误率
    }
    None => assert_true(false)
  }
  
  match latency_alert {
    Some(alert) => {
      assert_eq(alert.severity, Warning)
      assert_true(alert.message.contains("1000"))  // 平均延迟超过1000ms
    }
    None => assert_true(false)
  }
}