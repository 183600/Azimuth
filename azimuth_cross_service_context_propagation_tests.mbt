// 跨服务上下文传播测试用例
// 测试Azimuth遥测系统的跨服务上下文传播功能

test "HTTP头部上下文传播" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "http.propagation.test")
  
  // 创建父span
  let parent_span = Tracer::start_span(tracer, "parent.service.operation")
  Span::set_attribute(parent_span, "service.name", "parent-service")
  Span::set_attribute(parent_span, "operation.type", "http_request")
  
  let parent_ctx = Span::span_context(parent_span)
  
  // 模拟HTTP头部传播
  let propagator = TraceContextPropagator::new()
  let headers = TextMapPropagator::inject(propagator, parent_ctx)
  
  // 验证注入的头部
  assert_true(headers.contains("traceparent"))
  assert_true(headers.contains("tracestate"))
  
  // 模拟服务间传播
  let extracted_ctx = TextMapPropagator::extract(propagator, headers)
  assert_true(SpanContext::is_valid(extracted_ctx))
  assert_eq(SpanContext::trace_id(extracted_ctx), SpanContext::trace_id(parent_ctx))
  assert_eq(SpanContext::span_id(extracted_ctx), SpanContext::span_id(parent_ctx))
  
  // 在子服务中创建子span
  let child_span = Tracer::start_span_with_context(tracer, "child.service.operation", extracted_ctx)
  Span::set_attribute(child_span, "service.name", "child-service")
  Span::set_attribute(child_span, "operation.type", "database_query")
  
  let child_ctx = Span::span_context(child_span)
  assert_eq(SpanContext::trace_id(child_ctx), SpanContext::trace_id(parent_ctx))
  assert_neq(SpanContext::span_id(child_ctx), SpanContext::span_id(parent_ctx))
  
  // 结束span
  Span::end(child_span)
  Span::end(parent_span)
  
  assert_true(true)
}

test "多级服务调用链传播" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multi.service.chain")
  
  // 服务A：入口服务
  let service_a_span = Tracer::start_span(tracer, "service.a.entry")
  Span::set_attribute(service_a_span, "service.name", "service-a")
  Span::set_attribute(service_a_span, "service.version", "1.0.0")
  
  let service_a_ctx = Span::span_context(service_a_span)
  
  // 服务A调用服务B
  let propagator = TraceContextPropagator::new()
  let headers_to_b = TextMapPropagator::inject(propagator, service_a_ctx)
  let ctx_for_b = TextMapPropagator::extract(propagator, headers_to_b)
  
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.process", ctx_for_b)
  Span::set_attribute(service_b_span, "service.name", "service-b")
  Span::set_attribute(service_b_span, "service.version", "2.1.0")
  
  // 服务B调用服务C
  let headers_to_c = TextMapPropagator::inject(propagator, Span::span_context(service_b_span))
  let ctx_for_c = TextMapPropagator::extract(propagator, headers_to_c)
  
  let service_c_span = Tracer::start_span_with_context(tracer, "service.c.database", ctx_for_c)
  Span::set_attribute(service_c_span, "service.name", "service-c")
  Span::set_attribute(service_c_span, "service.version", "1.5.2")
  
  // 验证trace ID传播
  let trace_a = SpanContext::trace_id(Span::span_context(service_a_span))
  let trace_b = SpanContext::trace_id(Span::span_context(service_b_span))
  let trace_c = SpanContext::trace_id(Span::span_context(service_c_span))
  
  assert_eq(trace_a, trace_b)
  assert_eq(trace_b, trace_c)
  
  // 验证父子关系
  let parent_b = SpanContext::parent_span_id(Span::span_context(service_b_span))
  let parent_c = SpanContext::parent_span_id(Span::span_context(service_c_span))
  
  assert_eq(parent_b, SpanContext::span_id(Span::span_context(service_a_span)))
  assert_eq(parent_c, SpanContext::span_id(Span::span_context(service_b_span)))
  
  // 结束所有span（按正确顺序）
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  assert_true(true)
}

test " baggage跨服务传播" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "baggage.propagation.test")
  
  // 创建初始span并设置baggage
  let initial_span = Tracer::start_span(tracer, "initial.operation")
  Span::set_attribute(initial_span, "service.name", "init-service")
  
  // 添加baggage项
  Baggage::set_baggage("user.id", "user-12345")
  Baggage::set_baggage("request.id", "req-67890")
  Baggage::set_baggage("tenant.id", "tenant-abc")
  Baggage::set_baggage("correlation.id", "corr-def")
  
  let initial_ctx = Span::span_context(initial_span)
  
  // 传播baggage到下一个服务
  let propagator = TraceContextPropagator::new()
  let headers = TextMapPropagator::inject(propagator, initial_ctx)
  
  // 在下一个服务中提取baggage
  let extracted_ctx = TextMapPropagator::extract(propagator, headers)
  let next_span = Tracer::start_span_with_context(tracer, "next.service.operation", extracted_ctx)
  
  // 验证baggage项
  assert_eq(Baggage::get_baggage("user.id"), "user-12345")
  assert_eq(Baggage::get_baggage("request.id"), "req-67890")
  assert_eq(Baggage::get_baggage("tenant.id"), "tenant-abc")
  assert_eq(Baggage::get_baggage("correlation.id"), "corr-def")
  
  // 在下一个服务中添加更多baggage
  Baggage::set_baggage("service.region", "us-west")
  Baggage::set_baggage("service.instance", "instance-001")
  
  // 继续传播到第三个服务
  let headers_to_third = TextMapPropagator::inject(propagator, Span::span_context(next_span))
  let ctx_for_third = TextMapPropagator::extract(propagator, headers_to_third)
  
  let third_span = Tracer::start_span_with_context(tracer, "third.service.operation", ctx_for_third)
  
  // 验证所有baggage项
  assert_eq(Baggage::get_baggage("user.id"), "user-12345")
  assert_eq(Baggage::get_baggage("request.id"), "req-67890")
  assert_eq(Baggage::get_baggage("tenant.id"), "tenant-abc")
  assert_eq(Baggage::get_baggage("correlation.id"), "corr-def")
  assert_eq(Baggage::get_baggage("service.region"), "us-west")
  assert_eq(Baggage::get_baggage("service.instance"), "instance-001")
  
  // 结束所有span
  Span::end(third_span)
  Span::end(next_span)
  Span::end(initial_span)
  
  assert_true(true)
}

test "跨服务度量上下文传播" {
  let meter_provider = MeterProvider::default()
  let tracer_provider = TracerProvider::default()
  
  let meter = MeterProvider::get_meter(meter_provider, "cross.service.metrics")
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.metrics")
  
  // 在服务A中创建span和度量
  let service_a_span = Tracer::start_span(tracer, "service.a.operation")
  let request_counter = Meter::create_counter(meter, "cross.service.requests")
  
  Counter::add_with_attributes(request_counter, 1.0, [
    ("service", "service-a"),
    ("operation", "process_data"),
    ("trace_id", SpanContext::trace_id(Span::span_context(service_a_span)))
  ])
  
  // 传播上下文到服务B
  let propagator = TraceContextPropagator::new()
  let headers = TextMapPropagator::inject(propagator, Span::span_context(service_a_span))
  let ctx_for_b = TextMapPropagator::extract(propagator, headers)
  
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.operation", ctx_for_b)
  
  // 在服务B中创建相关度量
  let processing_time = Meter::create_histogram(meter, "cross.service.processing_time")
  Histogram::record_with_attributes(processing_time, 125.5, [
    ("service", "service-b"),
    ("operation", "transform_data"),
    ("trace_id", SpanContext::trace_id(Span::span_context(service_b_span))),
    ("parent_service", "service-a")
  ])
  
  // 验证度量关联性
  let metrics = Meter::collect_metrics(meter)
  let related_metrics = metrics.filter(fn(m) {
    m.get_attribute("trace_id") == SpanContext::trace_id(Span::span_context(service_a_span))
  })
  
  assert_true(related_metrics.length() >= 2)
  
  // 结束span
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  assert_true(true)
}