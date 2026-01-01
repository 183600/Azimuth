// Cross-Service Tracing Consistency Tests
// 测试跨服务追踪的一致性和完整性

test "trace_context_propagation_across_services" {
  // 跨服务追踪上下文传播测试
  
  // 模拟服务A的入口点
  let service_a_tracer = azimuth.Tracer.new("service-a")
  let root_span = azimuth.Tracer.start_span(service_a_tracer, "api-request")
  let root_context = azimuth.Span.get_context(root_span)
  
  // 验证根Span上下文
  let root_trace_id = azimuth.SpanContext.get_trace_id(root_context)
  let root_span_id = azimuth.SpanContext.get_span_id(root_context)
  @assertion.assert_equal(@unsafe.String.length(root_trace_id), 32) // 128位trace ID
  @assertion.assert_equal(@unsafe.String.length(root_span_id), 16)  // 64位span ID
  
  // 服务A调用服务B
  let service_b_tracer = azimuth.Tracer.new("service-b")
  let service_b_span = azimuth.Tracer.start_span_with_parent(
    service_b_tracer,
    "database-query",
    root_context,
    azimuth.SpanKind.CLIENT
  )
  
  let service_b_context = azimuth.Span.get_context(service_b_span)
  let service_b_trace_id = azimuth.SpanContext.get_trace_id(service_b_context)
  let service_b_span_id = azimuth.SpanContext.get_span_id(service_b_context)
  
  // 验证Trace ID一致性
  @assertion.assert_equal(service_b_trace_id, root_trace_id)
  @assertion.assert_not_equal(service_b_span_id, root_span_id)
  
  // 服务B调用服务C
  let service_c_tracer = azimuth.Tracer.new("service-c")
  let service_c_span = azimuth.Tracer.start_span_with_parent(
    service_c_tracer,
    "cache-operation",
    service_b_context,
    azimuth.SpanKind.INTERNAL
  )
  
  let service_c_context = azimuth.Span.get_context(service_c_span)
  let service_c_trace_id = azimuth.SpanContext.get_trace_id(service_c_context)
  let service_c_span_id = azimuth.SpanContext.get_span_id(service_c_context)
  
  // 验证Trace ID在所有服务中保持一致
  @assertion.assert_equal(service_c_trace_id, root_trace_id)
  @assertion.assert_not_equal(service_c_span_id, service_b_span_id)
  @assertion.assert_not_equal(service_c_span_id, root_span_id)
  
  // 结束所有Span
  azimuth.Span.end(service_c_span)
  azimuth.Span.end(service_b_span)
  azimuth.Span.end(root_span)
}

test "w3c_trace_context_headers" {
  // W3C Trace Context头部格式测试
  
  let tracer = azimuth.Tracer.new("test-service")
  let span = azimuth.Tracer.start_span(tracer, "test-operation")
  let span_context = azimuth.Span.get_context(span)
  
  // 生成traceparent头部
  let traceparent = azimuth.Propagator.generate_traceparent(span_context)
  
  // 验证traceparent格式: version-trace_id-span_id-flags
  let parts = @unsafe.String.split(traceparent, "-")
  @assertion.assert_equal(@unsafe.Array.length(parts), 4)
  
  let version = @unsafe.Array.get(parts, 0)
  let trace_id = @unsafe.Array.get(parts, 1)
  let span_id = @unsafe.Array.get(parts, 2)
  let flags = @unsafe.Array.get(parts, 3)
  
  @assertion.assert_equal(version, "00") // 当前版本
  @assertion.assert_equal(@unsafe.String.length(trace_id), 32)
  @assertion.assert_equal(@unsafe.String.length(span_id), 16)
  @assertion.assert_equal(@unsafe.String.length(flags), 2)
  
  // 验证trace_id和span_id与原始上下文一致
  @assertion.assert_equal(trace_id, azimuth.SpanContext.get_trace_id(span_context))
  @assertion.assert_equal(span_id, azimuth.SpanContext.get_span_id(span_context))
  
  // 测试tracestate头部
  let tracestate = "rojo=00f067aa0ba902b7,congo=61fecae8d1f6e2c6"
  azimuth.SpanContext.set_trace_state(span_context, tracestate)
  
  let extracted_tracestate = azimuth.Propagator.extract_tracestate(span_context)
  @assertion.assert_equal(extracted_tracestate, tracestate)
}

test "cross_service_baggage_consistency" {
  // 跨服务Baggage一致性测试
  
  // 服务A创建初始Baggage
  let service_a_context = azimuth.Context.new()
  
  let initial_baggage = [
    ("user_id", "12345"),
    ("session_id", "sess_abcdef123456"),
    ("request_id", "req_789012345"),
    ("correlation_id", "corr_555666777"),
    ("client_version", "2.1.0"),
    ("device_type", "mobile")
  ]
  
  for (key, value) in initial_baggage {
    azimuth.Context.set_baggage(service_a_context, key, value)
  }
  
  // 服务A调用服务B，传递Baggage
  let service_b_context = azimuth.Context.copy(service_a_context)
  
  // 服务B添加新的Baggage项
  azimuth.Context.set_baggage(service_b_context, "service_b_latency", "150ms")
  azimuth.Context.set_baggage(service_b_context, "db_query_type", "select")
  
  // 服务B调用服务C，继续传递Baggage
  let service_c_context = azimuth.Context.copy(service_b_context)
  
  // 服务C添加更多Baggage项
  azimuth.Context.set_baggage(service_c_context, "cache_hit", "true")
  azimuth.Context.set_baggage(service_c_context, "processing_time", "75ms")
  
  // 验证所有服务中的Baggage一致性
  for (key, expected_value) in initial_baggage {
    // 在服务B中验证
    match azimuth.Context.get_baggage(service_b_context, key) {
      Some(actual_value) => @assertion.assert_equal(actual_value, expected_value)
      None => @assertion.assert_true(false)
    }
    
    // 在服务C中验证
    match azimuth.Context.get_baggage(service_c_context, key) {
      Some(actual_value) => @assertion.assert_equal(actual_value, expected_value)
      None => @assertion.assert_true(false)
    }
  }
  
  // 验证服务特定Baggage
  match azimuth.Context.get_baggage(service_c_context, "service_b_latency") {
    Some(v) => @assertion.assert_equal(v, "150ms")
    None => @assertion.assert_true(false)
  }
  
  match azimuth.Context.get_baggage(service_c_context, "cache_hit") {
    Some(v) => @assertion.assert_equal(v, "true")
    None => @assertion.assert_true(false)
  }
}

test "distributed_sampling_consistency" {
  // 分布式采样一致性测试
  
  // 服务A创建根Span并设置采样决策
  let service_a_tracer = azimuth.Tracer.new("service-a")
  let root_span = azimuth.Tracer.start_span(service_a_tracer, "root-operation")
  let root_context = azimuth.Span.get_context(root_span)
  
  // 强制设置采样决策
  let sampled_context = azimuth.SpanContext.with_sampling(root_context, true)
  let not_sampled_context = azimuth.SpanContext.with_sampling(root_context, false)
  
  // 测试采样传播 - 采样场景
  let service_b_tracer = azimuth.Tracer.new("service-b")
  let sampled_child_span = azimuth.Tracer.start_span_with_parent(
    service_b_tracer,
    "sampled-operation",
    sampled_context,
    azimuth.SpanKind.INTERNAL
  )
  
  let sampled_child_context = azimuth.Span.get_context(sampled_child_span)
  @assertion.assert_true(azimuth.SpanContext.is_sampled(sampled_child_context))
  
  // 测试采样传播 - 非采样场景
  let not_sampled_child_span = azimuth.Tracer.start_span_with_parent(
    service_b_tracer,
    "not-sampled-operation",
    not_sampled_context,
    azimuth.SpanKind.INTERNAL
  )
  
  let not_sampled_child_context = azimuth.Span.get_context(not_sampled_child_span)
  @assertion.assert_false(azimuth.SpanContext.is_sampled(not_sampled_child_context))
  
  // 验证采样决策在Span创建时的行为
  @assertion.assert_true(azimuth.Span.is_recording(sampled_child_span))
  @assertion.assert_false(azimuth.Span.is_recording(not_sampled_child_span))
  
  // 结束Span
  azimuth.Span.end(sampled_child_span)
  azimuth.Span.end(not_sampled_child_span)
  azimuth.Span.end(root_span)
}

test "cross_service_error_propagation" {
  // 跨服务错误传播测试
  
  let service_a_tracer = azimuth.Tracer.new("service-a")
  let root_span = azimuth.Tracer.start_span(service_a_tracer, "api-request")
  let root_context = azimuth.Span.get_context(root_span)
  
  // 服务A调用服务B
  let service_b_tracer = azimuth.Tracer.new("service-b")
  let service_b_span = azimuth.Tracer.start_span_with_parent(
    service_b_tracer,
    "database-operation",
    root_context,
    azimuth.SpanKind.CLIENT
  )
  
  // 模拟服务B中的错误
  azimuth.Span.set_status(service_b_span, azimuth.StatusCode.ERROR, "Database connection failed")
  azimuth.Span.add_event_with_attributes(service_b_span, "exception", [
    ("exception.type", azimuth.AttributeValue.StringValue("ConnectionException")),
    ("exception.message", azimuth.AttributeValue.StringValue("Unable to connect to database")),
    ("exception.stacktrace", azimuth.AttributeValue.StringValue("at service-b/db.py:45"))
  ])
  
  azimuth.Span.end(service_b_span)
  
  // 服务A处理错误并创建响应Span
  let response_span = azimuth.Tracer.start_span_with_parent(
    service_a_tracer,
    "error-response",
    root_context,
    azimuth.SpanKind.SERVER
  )
  
  // 传播错误信息
  azimuth.Span.set_status(response_span, azimuth.StatusCode.ERROR, "Request failed due to downstream error")
  azimuth.Span.add_event_with_attributes(response_span, "error_propagated", [
    ("downstream_service", azimuth.AttributeValue.StringValue("service-b")),
    ("original_error", azimuth.AttributeValue.StringValue("Database connection failed")),
    ("error_code", azimuth.AttributeValue.IntValue(500))
  ])
  
  azimuth.Span.end(response_span)
  azimuth.Span.end(root_span)
  
  // 验证错误传播的完整性
  let service_b_events = azimuth.Span.get_events(service_b_span)
  let response_events = azimuth.Span.get_events(response_span)
  
  @assertion.assert_equal(@unsafe.Array.length(service_b_events), 1)
  @assertion.assert_equal(@unsafe.Array.length(response_events), 1)
  
  // 验证错误状态
  let service_b_status = azimuth.Span.get_status(service_b_span)
  let response_status = azimuth.Span.get_status(response_span)
  
  @assertion.assert_equal(azimuth.Status.get_code(service_b_status), azimuth.StatusCode.ERROR)
  @assertion.assert_equal(azimuth.Status.get_code(response_status), azimuth.StatusCode.ERROR)
}

test "cross_service_performance_metrics" {
  // 跨服务性能指标测试
  
  let meter_provider = azimuth.MeterProvider.new()
  let meter = azimuth.MeterProvider.get_meter(meter_provider, "cross-service-metrics")
  
  // 创建跨服务性能计数器
  let request_counter = azimuth.Meter.create_counter(meter, "cross_service_requests", "Cross-service requests", "requests")
  let latency_histogram = azimuth.Meter.create_histogram(meter, "cross_service_latency", "Cross-service latency", "ms")
  
  // 模拟跨服务调用
  let service_calls = [
    ("service-a", "service-b", 120),
    ("service-a", "service-c", 85),
    ("service-b", "service-d", 200),
    ("service-c", "service-e", 150),
    ("service-d", "service-f", 95)
  ]
  
  for (from_service, to_service, latency) in service_calls {
    let labels = [
      ("from_service", from_service),
      ("to_service", to_service)
    ]
    
    // 记录请求计数
    azimuth.Counter.add(request_counter, 1, labels)
    
    // 记录延迟
    azimuth.Histogram.record(latency, labels)
  }
  
  // 验证跨服务调用指标
  for (from_service, to_service, expected_latency) in service_calls {
    let labels = [("from_service", from_service), ("to_service", to_service)]
    
    let count = azimuth.Counter.collect(request_counter, labels)
    match count {
      Some(v) => @assertion.assert_equal(v, 1)
      None => @assertion.assert_true(false)
    }
    
    let latency_stats = azimuth.Histogram.collect_stats(latency_histogram, labels)
    match latency_stats {
      Some(stats) => {
        let avg_latency = azimuth.HistogramStats.get_average(stats)
        @assertion.assert_true(avg_latency > 0.0)
      }
      None => @assertion.assert_true(false)
    }
  }
}

test "cross_service_context_isolation" {
  // 跨服务上下文隔离测试
  
  // 服务A的独立上下文
  let service_a_context = azimuth.Context.new()
  azimuth.Context.set_string(service_a_context, "service_name", "service-a")
  azimuth.Context.set_baggage(service_a_context, "internal_id", "a_12345")
  
  // 服务B的独立上下文
  let service_b_context = azimuth.Context.new()
  azimuth.Context.set_string(service_b_context, "service_name", "service-b")
  azimuth.Context.set_baggage(service_b_context, "internal_id", "b_67890")
  
  // 验证上下文隔离
  match azimuth.Context.get_string(service_a_context, "service_name") {
    Some(v) => @assertion.assert_equal(v, "service-a")
    None => @assertion.assert_true(false)
  }
  
  match azimuth.Context.get_string(service_b_context, "service_name") {
    Some(v) => @assertion.assert_equal(v, "service-b")
    None => @assertion.assert_true(false)
  }
  
  match azimuth.Context.get_baggage(service_a_context, "internal_id") {
    Some(v) => @assertion.assert_equal(v, "a_12345")
    None => @assertion.assert_true(false)
  }
  
  match azimuth.Context.get_baggage(service_b_context, "internal_id") {
    Some(v) => @assertion.assert_equal(v, "b_67890")
    None => @assertion.assert_true(false)
  }
  
  // 验证交叉访问失败
  match azimuth.Context.get_baggage(service_a_context, "non_existent_key") {
    None => @assertion.assert_true(true)
    Some(_) => @assertion.assert_true(false)
  }
  
  // 创建共享的追踪上下文
  let shared_trace_context = azimuth.Context.new()
  let trace_span_context = azimuth.SpanContext.new(
    "shared_trace_1234567890123456",
    "shared_span_1234567890123456",
    true,
    ""
  )
  
  let enriched_shared_context = azimuth.Context.set_span_context(shared_trace_context, trace_span_context)
  
  // 服务A和服务B都使用共享追踪上下文，但保持各自的业务上下文
  let service_a_with_shared = azimuth.Context.merge(service_a_context, enriched_shared_context)
  let service_b_with_shared = azimuth.Context.merge(service_b_context, enriched_shared_context)
  
  // 验证追踪上下文共享
  let service_a_span_context = azimuth.Context.get_span_context(service_a_with_shared)
  let service_b_span_context = azimuth.Context.get_span_context(service_b_with_shared)
  
  @assertion.assert_equal(
    azimuth.SpanContext.get_trace_id(service_a_span_context),
    azimuth.SpanContext.get_trace_id(service_b_span_context)
  )
  
  // 验证业务上下文隔离
  match azimuth.Context.get_string(service_a_with_shared, "service_name") {
    Some(v) => @assertion.assert_equal(v, "service-a")
    None => @assertion.assert_true(false)
  }
  
  match azimuth.Context.get_string(service_b_with_shared, "service_name") {
    Some(v) => @assertion.assert_equal(v, "service-b")
    None => @assertion.assert_true(false)
  }
}