// Azimuth Premium Cross-Service Communication Tests
// 跨服务通信测试，确保遥测数据在分布式系统中的正确传播和处理

// Test 1: Trace Context Propagation
test "跟踪上下文传播测试" {
  // 创建服务A的根跨度
  let service_a_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let service_a_span_id = "00f067aa0ba902b7"
  let service_a_ctx = SpanContext::new(service_a_trace_id, service_a_span_id, true, "")
  let service_a_span = Span::new("service_a_operation", Server, service_a_ctx)
  
  // 模拟服务A向服务B发送请求
  let headers = []
  let carrier = TextMapCarrier::new(headers)
  
  // 注入跟踪上下文到载体
  let propagator = TraceContextPropagator::new()
  TraceContextPropagator::inject(propagator, service_a_ctx, carrier)
  
  // 验证载体包含跟踪信息
  let injected_headers = TextMapCarrier::headers(carrier)
  let mut traceparent_found = false
  let mut tracestate_found = false
  
  for (key, value) in injected_headers {
    match key {
      "traceparent" => {
        traceparent_found = true
        assert_true(value.contains(service_a_trace_id))
        assert_true(value.contains(service_a_span_id))
      }
      "tracestate" => {
        tracestate_found = true
      }
      _ => ()
    }
  }
  
  assert_true(traceparent_found)
  
  // 服务B接收请求并提取跟踪上下文
  let service_b_ctx = TraceContextPropagator::extract(propagator, carrier)
  
  // 验证提取的上下文
  assert_eq(SpanContext::trace_id(service_b_ctx), service_a_trace_id)
  assert_eq(SpanContext::span_id(service_b_ctx), service_a_span_id)
  assert_true(SpanContext::is_sampled(service_b_ctx))
  
  // 服务B创建子跨度
  let service_b_span_id = "b7ad6b7169203331"
  let service_b_child_ctx = SpanContext::new(service_a_trace_id, service_b_span_id, true, "")
  let service_b_span = Span::new("service_b_operation", Server, service_b_child_ctx)
  
  // 验证父子关系
  assert_eq(SpanContext::trace_id(Span::span_context(service_b_span)), service_a_trace_id)
  assert_neq(SpanContext::span_id(Span::span_context(service_b_span)), service_a_span_id)
}

// Test 2: Baggage Propagation
test "行李传播测试" {
  // 创建服务A的行李
  let service_a_baggage = Baggage::new()
  let service_a_baggage_with_entries = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(service_a_baggage, "user.id", "user_12345"),
      "session.id", "session_67890"
    ),
    "request.id", "req_abcde"
  )
  
  // 模拟服务A向服务B发送请求
  let headers = []
  let carrier = TextMapCarrier::new(headers)
  
  // 注入行李到载体
  let baggage_propagator = BaggagePropagator::new()
  BaggagePropagator::inject(baggage_propagator, service_a_baggage_with_entries, carrier)
  
  // 验证载体包含行李信息
  let injected_headers = TextMapCarrier::headers(carrier)
  let mut baggage_found = false
  
  for (key, value) in injected_headers {
    match key {
      "baggage" => {
        baggage_found = true
        assert_true(value.contains("user.id=user_12345"))
        assert_true(value.contains("session.id=session_67890"))
        assert_true(value.contains("request.id=req_abcde"))
      }
      _ => ()
    }
  }
  
  assert_true(baggage_found)
  
  // 服务B接收请求并提取行李
  let service_b_baggage = BaggagePropagator::extract(baggage_propagator, carrier)
  
  // 验证提取的行李
  let user_id = Baggage::get_entry(service_b_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user_12345")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(service_b_baggage, "session.id")
  match session_id {
    Some(value) => assert_eq(value, "session_67890")
    None => assert_true(false)
  }
  
  let request_id = Baggage::get_entry(service_b_baggage, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req_abcde")
    None => assert_true(false)
  }
  
  // 服务B添加新的行李条目
  let service_b_updated_baggage = Baggage::set_entry(service_b_baggage, "service.b.timestamp", "2023-01-01T12:00:00Z")
  
  // 服务B向服务C发送请求
  let headers_b_to_c = []
  let carrier_b_to_c = TextMapCarrier::new(headers_b_to_c)
  BaggagePropagator::inject(baggage_propagator, service_b_updated_baggage, carrier_b_to_c)
  
  // 服务C接收请求并提取行李
  let service_c_baggage = BaggagePropagator::extract(baggage_propagator, carrier_b_to_c)
  
  // 验证服务C能够获取所有行李条目
  let c_user_id = Baggage::get_entry(service_c_baggage, "user.id")
  match c_user_id {
    Some(value) => assert_eq(value, "user_12345")
    None => assert_true(false)
  }
  
  let c_timestamp = Baggage::get_entry(service_c_baggage, "service.b.timestamp")
  match c_timestamp {
    Some(value) => assert_eq(value, "2023-01-01T12:00:00Z")
    None => assert_true(false)
  }
}

// Test 3: Cross-Service Metrics Correlation
test "跨服务指标关联测试" {
  // 创建服务A的指标
  let provider_a = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(provider_a, "service_a_meter")
  
  let service_a_counter = Meter::create_counter(meter_a, "service_a_requests", Some("Service A requests"), Some("count"))
  let service_a_latency = Meter::create_histogram(meter_a, "service_a_latency", Some("Service A latency"), Some("ms"))
  
  // 记录服务A的指标
  Counter::add(service_a_counter, 1.0, Some(Attributes::with_data([
    ("service.name", StringValue("service_a")),
    ("operation.name", StringValue("process_request")),
    ("status", StringValue("success")),
    ("trace.id", StringValue("trace_12345"))
  ])))
  
  Histogram::record(service_a_latency, 150.0, Some(Attributes::with_data([
    ("service.name", StringValue("service_a")),
    ("operation.name", StringValue("process_request")),
    ("status", StringValue("success")),
    ("trace.id", StringValue("trace_12345"))
  ])))
  
  // 创建服务B的指标
  let provider_b = MeterProvider::default()
  let meter_b = MeterProvider::get_meter(provider_b, "service_b_meter")
  
  let service_b_counter = Meter::create_counter(meter_b, "service_b_requests", Some("Service B requests"), Some("count"))
  let service_b_latency = Meter::create_histogram(meter_b, "service_b_latency", Some("Service B latency"), Some("ms"))
  
  // 记录服务B的指标（相同的trace.id）
  Counter::add(service_b_counter, 1.0, Some(Attributes::with_data([
    ("service.name", StringValue("service_b")),
    ("operation.name", StringValue("handle_request")),
    ("status", StringValue("success")),
    ("trace.id", StringValue("trace_12345"))
  ])))
  
  Histogram::record(service_b_latency, 75.0, Some(Attributes::with_data([
    ("service.name", StringValue("service_b")),
    ("operation.name", StringValue("handle_request")),
    ("status", StringValue("success")),
    ("trace.id", StringValue("trace_12345"))
  ])))
  
  // 收集指标
  let metrics_a = Meter::collect_metrics(meter_a)
  let metrics_b = Meter::collect_metrics(meter_b)
  
  // 验证指标包含trace.id属性
  let trace_id_found_in_a = check_metrics_contain_attribute(metrics_a, "trace.id", "trace_12345")
  let trace_id_found_in_b = check_metrics_contain_attribute(metrics_b, "trace.id", "trace_12345")
  
  assert_true(trace_id_found_in_a)
  assert_true(trace_id_found_in_b)
  
  // 模拟指标聚合和分析
  let combined_metrics = combine_metrics([metrics_a, metrics_b])
  let trace_metrics = filter_metrics_by_attribute(combined_metrics, "trace.id", "trace_12345")
  
  // 验证能够按trace.id关联指标
  assert_true(trace_metrics.length() >= 4) // 至少有2个counter和2个histogram
  
  // 计算总延迟
  let total_latency = calculate_total_latency_for_trace(trace_metrics, "trace_12345")
  assert_eq(total_latency, 225.0) // 150.0 + 75.0
}

// Test 4: Cross-Service Log Correlation
test "跨服务日志关联测试" {
  // 创建服务A的日志记录器
  let provider_a = LoggerProvider::default()
  let logger_a = LoggerProvider::get_logger(provider_a, "service_a_logger")
  
  // 创建服务B的日志记录器
  let provider_b = LoggerProvider::default()
  let logger_b = LoggerProvider::get_logger(provider_b, "service_b_logger")
  
  let trace_id = "trace_67890"
  let span_a_id = "span_a_11111"
  let span_b_id = "span_b_22222"
  
  // 服务A记录日志
  let log_a_1 = LogRecord::new_with_context(
    Info,
    Some("Processing request started"),
    Some(Attributes::with_data([
      ("service.name", StringValue("service_a")),
      ("operation.name", StringValue("process_request")),
      ("request.id", StringValue("req_abc123")),
      ("trace.id", StringValue(trace_id)),
      ("span.id", StringValue(span_a_id))
    ])),
    Some(1672531200000L),
    None,
    Some(trace_id),
    Some(span_a_id),
    None
  )
  
  let log_a_2 = LogRecord::new_with_context(
    Info,
    Some("Request processing completed"),
    Some(Attributes::with_data([
      ("service.name", StringValue("service_a")),
      ("operation.name", StringValue("process_request")),
      ("request.id", StringValue("req_abc123")),
      ("trace.id", StringValue(trace_id)),
      ("span.id", StringValue(span_a_id)),
      ("processing.time", IntValue(100))
    ])),
    Some(1672531300000L),
    None,
    Some(trace_id),
    Some(span_a_id),
    None
  )
  
  Logger::emit(logger_a, log_a_1)
  Logger::emit(logger_a, log_a_2)
  
  // 服务B记录日志（相同的trace.id，不同的span.id）
  let log_b_1 = LogRecord::new_with_context(
    Info,
    Some("Handling request from service A"),
    Some(Attributes::with_data([
      ("service.name", StringValue("service_b")),
      ("operation.name", StringValue("handle_request")),
      ("request.id", StringValue("req_abc123")),
      ("trace.id", StringValue(trace_id)),
      ("span.id", StringValue(span_b_id)),
      ("parent.span.id", StringValue(span_a_id))
    ])),
    Some(1672531250000L),
    None,
    Some(trace_id),
    Some(span_b_id),
    None
  )
  
  let log_b_2 = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::with_data([
      ("service.name", StringValue("service_b")),
      ("operation.name", StringValue("handle_request")),
      ("request.id", StringValue("req_abc123")),
      ("trace.id", StringValue(trace_id)),
      ("span.id", StringValue(span_b_id)),
      ("error.code", StringValue("ECONNREFUSED")),
      ("retry.count", IntValue(3))
    ])),
    Some(1672531270000L),
    None,
    Some(trace_id),
    Some(span_b_id),
    None
  )
  
  Logger::emit(logger_b, log_b_1)
  Logger::emit(logger_b, log_b_2)
  
  // 模拟日志收集和关联
  let logs_a = collect_logs_from_provider(provider_a)
  let logs_b = collect_logs_from_provider(provider_b)
  let all_logs = combine_logs([logs_a, logs_b])
  
  // 按trace.id过滤日志
  let trace_logs = filter_logs_by_trace_id(all_logs, trace_id)
  
  // 验证能够按trace.id关联日志
  assert_eq(trace_logs.length(), 4)
  
  // 按时间戳排序日志
  let sorted_logs = sort_logs_by_timestamp(trace_logs)
  
  // 验证日志的时间顺序
  assert_eq(LogRecord::body(sorted_logs[0]), Some("Processing request started"))
  assert_eq(LogRecord::body(sorted_logs[1]), Some("Handling request from service A"))
  assert_eq(LogRecord::body(sorted_logs[2]), Some("Database connection failed"))
  assert_eq(LogRecord::body(sorted_logs[3]), Some("Request processing completed"))
  
  // 验证日志的服务分布
  let service_a_logs = filter_logs_by_service(sorted_logs, "service_a")
  let service_b_logs = filter_logs_by_service(sorted_logs, "service_b")
  
  assert_eq(service_a_logs.length(), 2)
  assert_eq(service_b_logs.length(), 2)
}

// Test 5: Service Mesh Integration
test "服务网格集成测试" {
  // 模拟服务网格中的服务通信
  let mesh_config = ServiceMeshConfig {
    enable_tracing: true,
    enable_metrics: true,
    enable_logging: true,
    sampling_rate: 1.0,
    propagation_format: "w3c"
  }
  
  // 创建服务网格上下文
  let mesh_context = ServiceMeshContext::new(mesh_config)
  
  // 服务A（入口服务）
  let ingress_service = Service::new("ingress", mesh_context)
  let ingress_span = Service::start_span(ingress_service, "ingress_request", External)
  
  // 添加服务网格特定标签
  Span::add_event(ingress_span, "mesh.request.received", Some([
    ("mesh.protocol", StringValue("http")),
    ("mesh.version", StringValue("1.0")),
    ("mesh.source.workload", StringValue("ingress-gateway")),
    ("mesh.source.namespace", StringValue("production"))
  ]))
  
  // 服务B（中间服务）
  let middle_service = Service::new("middle", mesh_context)
  let middle_span = Service::start_child_span(middle_service, "middle_processing", ingress_span)
  
  Span::add_event(middle_span, "mesh.processing.started", Some([
    ("mesh.workload", StringValue("middle-service")),
    ("mesh.namespace", StringValue("production"))
  ]))
  
  // 服务C（出口服务）
  let egress_service = Service::new("egress", mesh_context)
  let egress_span = Service::start_child_span(egress_service, "egress_request", middle_span)
  
  Span::add_event(egress_span, "mesh.request.forwarded", Some([
    ("mesh.destination.workload", StringValue("external-service")),
    ("mesh.destination.namespace", StringValue("external"))
  ]))
  
  // 结束所有跨度
  Span::end(egress_span)
  Span::end(middle_span)
  Span::end(ingress_span)
  
  // 验证服务网格跟踪信息
  let ingress_ctx = Span::span_context(ingress_span)
  let middle_ctx = Span::span_context(middle_span)
  let egress_ctx = Span::span_context(egress_span)
  
  // 验证trace.id一致性
  assert_eq(SpanContext::trace_id(ingress_ctx), SpanContext::trace_id(middle_ctx))
  assert_eq(SpanContext::trace_id(middle_ctx), SpanContext::trace_id(egress_ctx))
  
  // 验证span.id唯一性
  assert_neq(SpanContext::span_id(ingress_ctx), SpanContext::span_id(middle_ctx))
  assert_neq(SpanContext::span_id(middle_ctx), SpanContext::span_id(egress_ctx))
  
  // 验证服务网格特定属性
  let ingress_events = Span::events(ingress_span)
  let middle_events = Span::events(middle_span)
  let egress_events = Span::events(egress_span)
  
  assert_true(ingress_events.length() >= 1)
  assert_true(middle_events.length() >= 1)
  assert_true(egress_events.length() >= 1)
  
  // 验证事件包含服务网格信息
  let ingress_mesh_event = find_event_by_name(ingress_events, "mesh.request.received")
  match ingress_mesh_event {
    Some(event) => {
      let attrs = LogRecord::attributes(event)
      assert_true(check_attribute_exists(attrs, "mesh.protocol"))
      assert_true(check_attribute_exists(attrs, "mesh.version"))
    }
    None => assert_true(false)
  }
}

// Test 6: Async Communication Patterns
test "异步通信模式测试" {
  // 模拟异步消息队列通信
  let message_queue = MessageQueue::new("async_queue")
  
  // 生产者服务
  let producer_service = Service::new("producer", ServiceMeshContext::default())
  let producer_span = Service::start_span(producer_service, "produce_message", Internal)
  
  // 创建消息
  let message = Message::new("test_message", [
    ("message.id", StringValue("msg_12345")),
    ("producer.service", StringValue("producer")),
    ("trace.id", StringValue(SpanContext::trace_id(Span::span_context(producer_span)))),
    ("producer.span.id", StringValue(SpanContext::span_id(Span::span_context(producer_span))))
  ])
  
  // 发送消息
  let send_result = MessageQueue::send(message_queue, message)
  match send_result {
    Ok(message_id) => assert_eq(message_id, "msg_12345")
    Err(_) => assert_true(false)
  }
  
  Span::add_event(producer_span, "message.sent", Some([
    ("message.id", StringValue("msg_12345")),
    ("queue.name", StringValue("async_queue"))
  ]))
  
  Span::end(producer_span)
  
  // 消费者服务
  let consumer_service = Service::new("consumer", ServiceMeshContext::default())
  
  // 接收消息
  let received_message = MessageQueue::receive(message_queue, 5000) // 5秒超时
  match received_message {
    Ok(msg) => {
      // 从消息中提取跟踪上下文
      let message_trace_id = Message::get_attribute(msg, "trace.id")
      let message_span_id = Message::get_attribute(msg, "producer.span.id")
      
      match (message_trace_id, message_span_id) {
        (Some(StringValue(trace_id)), Some(StringValue(span_id))) => {
          // 创建消费者跨度，关联到生产者
          let consumer_ctx = SpanContext::new(trace_id, "consumer_span_" + generate_random_id(), true, "")
          let consumer_span = Service::start_span_with_context(consumer_service, "consume_message", Internal, consumer_ctx)
          
          // 添加父跨度引用
          Span::add_link(consumer_span, SpanContext::new(trace_id, span_id, true, ""))
          
          Span::add_event(consumer_span, "message.received", Some([
            ("message.id", StringValue("msg_12345")),
            ("producer.service", StringValue("producer")),
            ("producer.span.id", StringValue(span_id))
          ]))
          
          // 处理消息
          process_message(msg)
          
          Span::add_event(consumer_span, "message.processed", Some([
            ("message.id", StringValue("msg_12345")),
            ("processing.time", IntValue(50))
          ]))
          
          Span::end(consumer_span)
          
          // 验证异步通信的跟踪关联
          let consumer_trace_id = SpanContext::trace_id(Span::span_context(consumer_span))
          assert_eq(consumer_trace_id, trace_id)
        }
        _ => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 7: Circuit Breaker Pattern
test "断路器模式测试" {
  // 创建断路器
  let circuit_breaker = CircuitBreaker::new("test_breaker", [
    ("failure.threshold", IntValue(5)),
    ("recovery.timeout", IntValue(10000)), // 10秒
    ("success.threshold", IntValue(3))
  ])
  
  let service_a = Service::new("service_a", ServiceMeshContext::default())
  let service_b = Service::new("service_b", ServiceMeshContext::default())
  
  // 模拟服务A调用服务B
  let mut success_count = 0
  let mut failure_count = 0
  
  for i in 0..10 {
    let span_a = Service::start_span(service_a, "call_service_b", Client)
    
    let result = CircuitBreaker::execute(circuit_breaker, () => {
      let span_b = Service::start_child_span(service_b, "handle_request", span_a)
      
      // 模拟失败
      if i < 7 {
        Span::set_status(span_b, Error, Some("Simulated failure"))
        Span::end(span_b)
        Error("Service failure")
      } else {
        Span::set_status(span_b, Ok, Some("Success"))
        Span::end(span_b)
        Ok(())
      }
    })
    
    match result {
      Ok(_) => {
        success_count = success_count + 1
        Span::set_status(span_a, Ok, Some("Service call successful"))
      }
      Err(e) => {
        failure_count = failure_count + 1
        Span::set_status(span_a, Error, Some("Service call failed: " + e))
        Span::add_event(span_a, "circuit_breaker.open", Some([
          ("failure.count", IntValue(failure_count))
        ]))
      }
    }
    
    Span::end(span_a)
  }
  
  // 验证断路器行为
  assert_true(failure_count >= 5) // 应该有足够的失败触发断路器
  assert_eq(success_count, 3) // 只有最后几次可能成功（恢复后）
  
  // 验证断路器状态
  let breaker_state = CircuitBreaker::get_state(circuit_breaker)
  match breaker_state {
    Closed => assert_true(false), // 应该不是关闭状态
    Open => assert_true(true),    // 应该是打开状态
    HalfOpen => assert_true(true) // 或者半开状态
  }
}

// Test 8: Retry Pattern with Telemetry
test "重试模式与遥测测试" {
  // 创建重试策略
  let retry_policy = RetryPolicy::new([
    ("max.attempts", IntValue(3)),
    ("initial.delay", IntValue(100)), // 100ms
    ("max.delay", IntValue(1000)),    // 1s
    ("multiplier", FloatValue(2.0))
  ])
  
  let service_a = Service::new("service_a", ServiceMeshContext::default())
  let unreliable_service = Service::new("unreliable_service", ServiceMeshContext::default())
  
  // 模拟服务A调用不可靠服务
  let span_a = Service::start_span(service_a, "call_unreliable_service", Client)
  
  let mut attempt_count = 0
  let result = RetryPolicy::execute_with_retry(retry_policy, () => {
    attempt_count = attempt_count + 1
    
    let span_unreliable = Service::start_child_span(unreliable_service, "handle_request", span_a)
    Span::add_event(span_unreliable, "attempt.started", Some([
      ("attempt.number", IntValue(attempt_count))
    ]))
    
    // 前两次失败，第三次成功
    if attempt_count < 3 {
      Span::set_status(span_unreliable, Error, Some("Simulated failure"))
      Span::end(span_unreliable)
      Error("Attempt " + attempt_count.to_string() + " failed")
    } else {
      Span::set_status(span_unreliable, Ok, Some("Success"))
      Span::end(span_unreliable)
      Ok("Success after " + attempt_count.to_string() + " attempts")
    }
  })
  
  match result {
    Ok(message) => {
      assert_eq(message, "Success after 3 attempts")
      assert_eq(attempt_count, 3)
      
      Span::set_status(span_a, Ok, Some("Operation successful after retries"))
      Span::add_event(span_a, "retry.completed", Some([
        ("total.attempts", IntValue(attempt_count)),
        ("final.result", StringValue("success"))
      ]))
    }
    Err(_) => assert_true(false)
  }
  
  Span::end(span_a)
  
  // 验证重试跟踪信息
  let span_a_events = Span::events(span_a)
  let retry_completed_event = find_event_by_name(span_a_events, "retry.completed")
  match retry_completed_event {
    Some(event) => {
      let attrs = LogRecord::attributes(event)
      let total_attempts = get_attribute_value(attrs, "total.attempts")
      match total_attempts {
        Some(IntValue(attempts)) => assert_eq(attempts, 3)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 9: Distributed Transaction Tracing
test "分布式事务跟踪测试" {
  // 创建分布式事务
  let transaction_id = "txn_" + generate_random_id()
  let transaction = DistributedTransaction::new(transaction_id, [
    ("transaction.type", StringValue("order_processing")),
    ("user.id", StringValue("user_12345")),
    ("order.id", StringValue("order_67890"))
  ])
  
  // 服务A：订单服务
  let order_service = Service::new("order_service", ServiceMeshContext::default())
  let order_span = Service::start_span_with_transaction(order_service, "create_order", Server, transaction)
  
  Span::add_event(order_span, "order.created", Some([
    ("order.id", StringValue("order_67890")),
    ("order.amount", FloatValue(99.99)),
    ("transaction.id", StringValue(transaction_id))
  ]))
  
  // 服务B：库存服务
  let inventory_service = Service::new("inventory_service", ServiceMeshContext::default())
  let inventory_span = Service::start_child_span_with_transaction(inventory_service, "reserve_inventory", order_span, transaction)
  
  Span::add_event(inventory_span, "inventory.reserved", Some([
    ("product.id", StringValue("prod_11111")),
    ("quantity", IntValue(1)),
    ("transaction.id", StringValue(transaction_id))
  ]))
  
  // 服务C：支付服务
  let payment_service = Service::new("payment_service", ServiceMeshContext::default())
  let payment_span = Service::start_child_span_with_transaction(payment_service, "process_payment", inventory_span, transaction)
  
  Span::add_event(payment_span, "payment.processed", Some([
    ("payment.id", StringValue("pay_22222")),
    ("payment.amount", FloatValue(99.99)),
    ("transaction.id", StringValue(transaction_id))
  ]))
  
  // 结束所有跨度
  Span::end(payment_span)
  Span::end(inventory_span)
  Span::end(order_span)
  
  // 验证分布式事务跟踪
  let order_ctx = Span::span_context(order_span)
  let inventory_ctx = Span::span_context(inventory_span)
  let payment_ctx = Span::span_context(payment_span)
  
  // 验证所有跨度属于同一事务
  assert_eq(SpanContext::trace_id(order_ctx), SpanContext::trace_id(inventory_ctx))
  assert_eq(SpanContext::trace_id(inventory_ctx), SpanContext::trace_id(payment_ctx))
  
  // 验证事务ID在所有事件中传播
  let order_events = Span::events(order_span)
  let inventory_events = Span::events(inventory_span)
  let payment_events = Span::events(payment_span)
  
  assert_true(check_events_contain_attribute(order_events, "transaction.id", transaction_id))
  assert_true(check_events_contain_attribute(inventory_events, "transaction.id", transaction_id))
  assert_true(check_events_contain_attribute(payment_events, "transaction.id", transaction_id))
  
  // 验证事务时间线
  let all_spans = [order_span, inventory_span, payment_span]
  let transaction_timeline = build_transaction_timeline(all_spans)
  
  assert_eq(transaction_timeline.length(), 3)
  assert_eq(transaction_timeline[0].service_name, "order_service")
  assert_eq(transaction_timeline[1].service_name, "inventory_service")
  assert_eq(transaction_timeline[2].service_name, "payment_service")
}

// Test 10: Cross-Service Performance Analysis
test "跨服务性能分析测试" {
  // 创建多服务调用链
  let services = [
    ("gateway", Service::new("gateway", ServiceMeshContext::default())),
    ("auth", Service::new("auth", ServiceMeshContext::default())),
    ("user", Service::new("user", ServiceMeshContext::default())),
    ("order", Service::new("order", ServiceMeshContext::default())),
    ("inventory", Service::new("inventory", ServiceMeshContext::default())),
    ("notification", Service::new("notification", ServiceMeshContext::default()))
  ]
  
  let mut spans = []
  let mut current_span = None
  
  // 模拟服务调用链
  for i in 0..services.length() {
    let (service_name, service) = services[i]
    
    let span = match current_span {
      None => Service::start_span(service, "process_request", Server)
      Some(parent) => Service::start_child_span(service, "process_request", parent)
    }
    
    // 模拟服务处理时间
    let processing_time = match service_name {
      "gateway" => 50,
      "auth" => 100,
      "user" => 75,
      "order" => 200,
      "inventory" => 150,
      "notification" => 25
      _ => 100
    }
    
    // 添加性能指标事件
    Span::add_event(span, "processing.started", Some([
      ("service.name", StringValue(service_name)),
      ("processing.time.expected", IntValue(processing_time))
    ]))
    
    // 模拟处理时间
    simulate_processing_time(processing_time)
    
    Span::add_event(span, "processing.completed", Some([
      ("service.name", StringValue(service_name)),
      ("processing.time.actual", IntValue(processing_time))
    ]))
    
    spans = spans + [span]
    current_span = Some(span)
  }
  
  // 结束所有跨度（反向顺序）
  for i in Range::new(0, spans.length()).rev() {
    Span::end(spans[i])
  }
  
  // 分析性能
  let performance_analysis = analyze_cross_service_performance(spans)
  
  // 验证性能分析结果
  assert_eq(performance_analysis.total_services, 6)
  assert_eq(performance_analysis.total_duration, 600) // 50+100+75+200+150+25
  assert_eq(performance_analysis.slowest_service, "order")
  assert_eq(performance_analysis.fastest_service, "notification")
  
  // 验证服务时间分布
  let service_times = performance_analysis.service_times
  assert_eq(service_times.get("gateway"), Some(50))
  assert_eq(service_times.get("auth"), Some(100))
  assert_eq(service_times.get("user"), Some(75))
  assert_eq(service_times.get("order"), Some(200))
  assert_eq(service_times.get("inventory"), Some(150))
  assert_eq(service_times.get("notification"), Some(25))
  
  // 验证瓶颈识别
  let bottlenecks = identify_performance_bottlenecks(performance_analysis)
  assert_true(bottlenecks.length() >= 1)
  assert_true(bottlenecks.contains("order")) // 最慢的服务
}

// 辅助函数和类型定义
type ServiceMeshConfig {
  enable_tracing: Bool,
  enable_metrics: Bool,
  enable_logging: Bool,
  sampling_rate: Float,
  propagation_format: String
}

type ServiceMeshContext {
  config: ServiceMeshConfig
}

type Service {
  name: String,
  mesh_context: ServiceMeshContext
}

type MessageQueue {
  name: String
}

type Message {
  id: String,
  attributes: Array<(String, AttributeValue)>
}

type CircuitBreaker {
  name: String,
  config: Array<(String, AttributeValue)>,
  state: CircuitBreakerState
}

type CircuitBreakerState {
  Closed | Open | HalfOpen
}

type RetryPolicy {
  config: Array<(String, AttributeValue)>
}

type DistributedTransaction {
  id: String,
  attributes: Array<(String, AttributeValue)>
}

type PerformanceAnalysis {
  total_services: Int,
  total_duration: Int,
  slowest_service: String,
  fastest_service: String,
  service_times: Map<String, Int>
}

fn ServiceMeshContext::new(config: ServiceMeshConfig) -> ServiceMeshContext {
  ServiceMeshContext { config: config }
}

fn ServiceMeshContext::default() -> ServiceMeshContext {
  ServiceMeshContext::new(ServiceMeshConfig {
    enable_tracing: true,
    enable_metrics: true,
    enable_logging: true,
    sampling_rate: 1.0,
    propagation_format: "w3c"
  })
}

fn Service::new(name: String, mesh_context: ServiceMeshContext) -> Service {
  Service { name: name, mesh_context: mesh_context }
}

fn Service::start_span(service: Service, operation_name: String, kind: SpanKind) -> Span {
  let trace_id = generate_trace_id()
  let span_id = generate_span_id()
  let ctx = SpanContext::new(trace_id, span_id, true, "")
  Span::new(operation_name, kind, ctx)
}

fn Service::start_span_with_context(service: Service, operation_name: String, kind: SpanKind, ctx: SpanContext) -> Span {
  Span::new(operation_name, kind, ctx)
}

fn Service::start_child_span(service: Service, operation_name: String, parent_span: Span) -> Span {
  let parent_ctx = Span::span_context(parent_span)
  let child_span_id = generate_span_id()
  let child_ctx = SpanContext::new(
    SpanContext::trace_id(parent_ctx),
    child_span_id,
    true,
    SpanContext::trace_state(parent_ctx)
  )
  Span::new(operation_name, Internal, child_ctx)
}

fn Service::start_child_span_with_transaction(service: Service, operation_name: String, parent_span: Span, transaction: DistributedTransaction) -> Span {
  let child_span = Service::start_child_span(service, operation_name, parent_span)
  
  // 添加事务属性
  for (key, value) in transaction.attributes {
    match key {
      "transaction.id" => {
        Span::add_event(child_span, "transaction.context", Some([(key, value)]))
      }
      _ => ()
    }
  }
  
  child_span
}

fn MessageQueue::new(name: String) -> MessageQueue {
  MessageQueue { name: name }
}

fn Message::new(id: String, attributes: Array<(String, AttributeValue)>) -> Message {
  Message { id: id, attributes: attributes }
}

fn Message::get_attribute(message: Message, key: String) -> Option<AttributeValue> {
  for (k, v) in message.attributes {
    if k == key {
      return Some(v)
    }
  }
  None
}

fn MessageQueue::send(queue: MessageQueue, message: Message) -> Result<String, String> {
  // 模拟发送消息
  Ok(message.id)
}

fn MessageQueue::receive(queue: MessageQueue, timeout_ms: Int) -> Result<Message, String> {
  // 模拟接收消息
  let message = Message::new("msg_12345", [
    ("message.id", StringValue("msg_12345")),
    ("producer.service", StringValue("producer")),
    ("trace.id", StringValue("trace_12345")),
    ("producer.span.id", StringValue("span_12345"))
  ])
  Ok(message)
}

fn CircuitBreaker::new(name: String, config: Array<(String, AttributeValue)>) -> CircuitBreaker {
  CircuitBreaker {
    name: name,
    config: config,
    state: Closed
  }
}

fn CircuitBreaker::execute(breaker: CircuitBreaker, operation: () -> Result<Unit, String>) -> Result<Unit, String> {
  // 模拟断路器执行
  let result = operation()
  
  // 更新断路器状态
  match result {
    Ok(_) => (),
    Err(_) => {
      // 模拟断路器打开
      // breaker.state = Open
    }
  }
  
  result
}

fn CircuitBreaker::get_state(breaker: CircuitBreaker) -> CircuitBreakerState {
  breaker.state
}

fn RetryPolicy::new(config: Array<(String, AttributeValue)>) -> RetryPolicy {
  RetryPolicy { config: config }
}

fn RetryPolicy::execute_with_retry(policy: RetryPolicy, operation: () -> Result<String, String>) -> Result<String, String> {
  let mut attempts = 0
  let max_attempts = 3
  
  while attempts < max_attempts {
    let result = operation()
    match result {
      Ok(value) => return Ok(value),
      Err(_) => {
        attempts = attempts + 1
        if attempts >= max_attempts {
          return result
        }
      }
    }
  }
  
  Error("Max retry attempts exceeded")
}

fn DistributedTransaction::new(id: String, attributes: Array<(String, AttributeValue)>) -> DistributedTransaction {
  DistributedTransaction { id: id, attributes: attributes }
}

fn check_metrics_contain_attribute(metrics: Array<Metric>, key: String, value: String) -> Bool {
  for metric in metrics {
    let attributes = Metric::attributes(metric)
    for (k, v) in attributes {
      if k == key {
        match v {
          StringValue(s) => {
            if s == value {
              return true
            }
          }
          _ => ()
        }
      }
    }
  }
  false
}

fn combine_metrics(metrics_arrays: Array<Array<Metric>>) -> Array<Metric> {
  let mut combined = []
  for metrics in metrics_arrays {
    for metric in metrics {
      combined = combined + [metric]
    }
  }
  combined
}

fn filter_metrics_by_attribute(metrics: Array<Metric>, key: String, value: String) -> Array<Metric> {
  let mut filtered = []
  for metric in metrics {
    let attributes = Metric::attributes(metric)
    for (k, v) in attributes {
      if k == key {
        match v {
          StringValue(s) => {
            if s == value {
              filtered = filtered + [metric]
              break
            }
          }
          _ => ()
        }
      }
    }
  }
  filtered
}

fn calculate_total_latency_for_trace(metrics: Array<Metric>, trace_id: String) -> Float {
  let mut total_latency = 0.0
  for metric in metrics {
    let attributes = Metric::attributes(metric)
    let mut has_trace_id = false
    for (k, v) in attributes {
      if k == "trace.id" {
        match v {
          StringValue(s) => {
            if s == trace_id {
              has_trace_id = true
            }
          }
          _ => ()
        }
      }
    }
    
    if has_trace_id {
      match Metric::type(metric) {
        HistogramMetric => {
          // 简化：假设直方图有一个值
          total_latency = total_latency + 100.0 // 模拟值
        }
        _ => ()
      }
    }
  }
  total_latency
}

fn collect_logs_from_provider(provider: LoggerProvider) -> Array<LogRecord> {
  // 模拟日志收集
  []
}

fn combine_logs(log_arrays: Array<Array<LogRecord>>) -> Array<LogRecord> {
  let mut combined = []
  for logs in log_arrays {
    for log in logs {
      combined = combined + [log]
    }
  }
  combined
}

fn filter_logs_by_trace_id(logs: Array<LogRecord>, trace_id: String) -> Array<LogRecord> {
  let mut filtered = []
  for log in logs {
    if LogRecord::trace_id(log) == Some(trace_id) {
      filtered = filtered + [log]
    }
  }
  filtered
}

fn sort_logs_by_timestamp(logs: Array<LogRecord>) -> Array<LogRecord> {
  // 简化排序实现
  logs
}

fn filter_logs_by_service(logs: Array<LogRecord>, service_name: String) -> Array<LogRecord> {
  let mut filtered = []
  for log in logs {
    match LogRecord::attributes(log) {
      Some(attrs) => {
        for (key, value) in attrs {
          if key == "service.name" {
            match value {
              StringValue(name) => {
                if name == service_name {
                  filtered = filtered + [log]
                  break
                }
              }
              _ => ()
            }
          }
        }
      }
      None => ()
    }
  }
  filtered
}

fn find_event_by_name(events: Array<LogRecord>, name: String) -> Option<LogRecord> {
  for event in events {
    match LogRecord::body(event) {
      Some(body) => {
        if body == name {
          return Some(event)
        }
      }
      None => ()
    }
  }
  None
}

fn check_attribute_exists(attributes: Array<(String, AttributeValue)>, key: String) -> Bool {
  for (k, _) in attributes {
    if k == key {
      return true
    }
  }
  false
}

fn get_attribute_value(attributes: Array<(String, AttributeValue)>, key: String) -> Option<AttributeValue> {
  for (k, v) in attributes {
    if k == key {
      return Some(v)
    }
  }
  None
}

fn generate_random_id() -> String {
  "random_id_" + get_current_timestamp().to_string()
}

fn generate_trace_id() -> String {
  "trace_" + generate_random_id()
}

fn generate_span_id() -> String {
  "span_" + generate_random_id()
}

fn process_message(message: Message) -> Unit {
  // 模拟消息处理
  ()
}

fn simulate_processing_time(ms: Int) -> Unit {
  // 模拟处理时间
  ()
}

fn analyze_cross_service_performance(spans: Array<Span>) -> PerformanceAnalysis {
  let mut total_duration = 0
  let mut service_times = Map::new()
  let mut slowest_service = ""
  let mut fastest_service = ""
  let mut max_time = 0
  let mut min_time = 999999
  
  for span in spans {
    let service_name = "unknown" // 简化实现
    let duration = 100 // 模拟持续时间
    
    total_duration = total_duration + duration
    service_times.set(service_name, duration)
    
    if duration > max_time {
      max_time = duration
      slowest_service = service_name
    }
    
    if duration < min_time {
      min_time = duration
      fastest_service = service_name
    }
  }
  
  PerformanceAnalysis {
    total_services: spans.length(),
    total_duration: total_duration,
    slowest_service: slowest_service,
    fastest_service: fastest_service,
    service_times: service_times
  }
}

fn identify_performance_bottlenecks(analysis: PerformanceAnalysis) -> Array<String> {
  let mut bottlenecks = []
  let threshold = analysis.total_duration / analysis.total_services * 2
  
  for (service, time) in analysis.service_times.to_array() {
    if time > threshold {
      bottlenecks = bottlenecks + [service]
    }
  }
  
  bottlenecks
}

fn check_events_contain_attribute(events: Array<LogRecord>, key: String, value: String) -> Bool {
  for event in events {
    match LogRecord::attributes(event) {
      Some(attrs) => {
        for (k, v) in attrs {
          if k == key {
            match v {
              StringValue(s) => {
                if s == value {
                  return true
                }
              }
              _ => ()
            }
          }
        }
      }
      None => ()
    }
  }
  false
}

fn build_transaction_timeline(spans: Array<Span>) -> Array<TransactionSpan> {
  let mut timeline = []
  for span in spans {
    let span_info = TransactionSpan {
      service_name: "unknown", // 简化实现
      operation_name: Span::name(span),
      start_time: 0, // 简化实现
      end_time: 0    // 简化实现
    }
    timeline = timeline + [span_info]
  }
  timeline
}

type TransactionSpan {
  service_name: String,
  operation_name: String,
  start_time: Int,
  end_time: Int
}