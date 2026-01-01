// 跨服务追踪测试用例
// 测试 Azimuth 遥测系统中的跨服务追踪功能

test "cross_service_trace_propagation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "service-a")
  
  // 在服务A中创建span
  let span_a = Tracer::start_span(tracer, "service-a-operation")
  let ctx_a = SpanContext::from_span(span_a)
  
  // 模拟HTTP请求头传播
  let carrier = TextMapCarrier::new()
  let propagator = TraceContextPropagator::new()
  
  // 注入追踪上下文到carrier
  Propagator::inject(propagator, ctx_a, carrier)
  
  // 验证traceparent头
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent != None)
  
  // 模拟服务B接收请求
  let tracer_b = TracerProvider::get_tracer(provider, "service-b")
  let ctx_b = Propagator::extract(propagator, carrier)
  
  // 在服务B中创建子span
  let span_b = Tracer::start_span(tracer_b, "service-b-operation", Some(ctx_b))
  let ctx_b_final = SpanContext::from_span(span_b)
  
  // 验证trace ID一致性
  let trace_id_a = SpanContext::trace_id(ctx_a)
  let trace_id_b = SpanContext::trace_id(ctx_b_final)
  assert_eq(trace_id_a, trace_id_b)
  
  // 验证span关系
  let span_id_a = SpanContext::span_id(ctx_a)
  let parent_id_b = SpanContext::parent_span_id(ctx_b_final)
  assert_eq(parent_id_b, Some(span_id_a))
  
  Span::end(span_b)
  Span::end(span_a)
}

test "cross_service_baggage_propagation" {
  let carrier = TextMapCarrier::new()
  
  // 在服务A中设置baggage
  let baggage_a = Baggage::new()
  Baggage::set(baggage_a, "user.id", "12345")
  Baggage::set(baggage_a, "session.id", "abcdef")
  Baggage::set(baggage_a, "request.source", "mobile")
  
  // 注入baggage到carrier
  Baggage::inject_to_carrier(baggage_a, carrier)
  
  // 验证baggage头
  let baggage_header = TextMapCarrier::get(carrier, "baggage")
  assert_true(baggage_header != None)
  
  // 在服务B中提取baggage
  let baggage_b = Baggage::from_carrier(carrier)
  
  // 验证baggage内容
  let user_id = Baggage::get(baggage_b, "user.id")
  let session_id = Baggage::get(baggage_b, "session.id")
  let request_source = Baggage::get(baggage_b, "request.source")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(session_id, Some("abcdef"))
  assert_eq(request_source, Some("mobile"))
  
  // 在服务B中添加新的baggage项
  Baggage::set(baggage_b, "service.b.version", "2.1.0")
  Baggage::set(baggage_b, "processing.time", "150ms")
  
  // 验证新添加的baggage项
  let service_version = Baggage::get(baggage_b, "service.b.version")
  let processing_time = Baggage::get(baggage_b, "processing.time")
  
  assert_eq(service_version, Some("2.1.0"))
  assert_eq(processing_time, Some("150ms"))
}

test "cross_service_metrics_correlation" {
  let provider = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(provider, "service-a-metrics")
  let meter_b = MeterProvider::get_meter(provider, "service-b-metrics")
  
  // 在服务A中创建指标
  let counter_a = Meter::create_counter(meter_a, "requests.total")
  let histogram_a = Meter::create_histogram(meter_a, "request.duration")
  
  // 记录服务A的指标
  Counter::add(counter_a, 1.0)
  Histogram::record(histogram_a, 120.5)
  
  // 在服务B中创建相关指标
  let counter_b = Meter::create_counter(meter_b, "requests.total")
  let histogram_b = Meter::create_histogram(meter_b, "request.duration")
  
  // 记录服务B的指标
  Counter::add(counter_b, 1.0)
  Histogram::record(histogram_b, 85.3)
  
  // 验证指标名称一致性
  assert_eq(counter_a.name, "requests.total")
  assert_eq(counter_b.name, "requests.total")
  assert_eq(histogram_a.name, "request.duration")
  assert_eq(histogram_b.name, "request.duration")
  
  // 验证指标单位一致性
  assert_eq(histogram_a.unit, Some("ms"))
  assert_eq(histogram_b.unit, Some("ms"))
}

test "cross_service_error_propagation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "error-test")
  
  // 在服务A中创建span并记录错误
  let span_a = Tracer::start_span(tracer, "service-a-operation")
  let ctx_a = SpanContext::from_span(span_a)
  
  // 记录错误事件
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.type", StringValue("ValidationError"))
  Attributes::set(error_attrs, "error.message", StringValue("Invalid input parameter"))
  
  Span::add_event(span_a, "error", Some(error_attrs))
  Span::set_status(span_a, StatusCode::ERROR)
  
  // 传播错误上下文到服务B
  let carrier = TextMapCarrier::new()
  let propagator = TraceContextPropagator::new()
  Propagator::inject(propagator, ctx_a, carrier)
  
  // 在服务B中提取上下文并创建span
  let ctx_b = Propagator::extract(propagator, carrier)
  let span_b = Tracer::start_span(tracer, "service-b-error-handling", Some(ctx_b))
  
  // 在服务B中记录相关错误
  let error_attrs_b = Attributes::new()
  Attributes::set(error_attrs_b, "error.type", StringValue("DownstreamError"))
  Attributes::set(error_attrs_b, "error.source", StringValue("service-a"))
  
  Span::add_event(span_b, "downstream_error", Some(error_attrs_b))
  Span::set_status(span_b, StatusCode::ERROR)
  
  // 验证错误上下文的传播
  let trace_id_a = SpanContext::trace_id(ctx_a)
  let trace_id_b = SpanContext::trace_id(SpanContext::from_span(span_b))
  assert_eq(trace_id_a, trace_id_b)
  
  Span::end(span_b)
  Span::end(span_a)
}

test "cross_service_sampling_consistency" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "sampling-test")
  
  // 创建带采样决策的span
  let span_a = Tracer::start_span(tracer, "service-a-operation")
  let ctx_a = SpanContext::from_span(span_a)
  
  // 验证采样决策
  let is_sampled_a = SpanContext::is_sampled(ctx_a)
  
  // 传播采样决策
  let carrier = TextMapCarrier::new()
  let propagator = TraceContextPropagator::new()
  Propagator::inject(propagator, ctx_a, carrier)
  
  // 在服务B中提取采样决策
  let ctx_b = Propagator::extract(propagator, carrier)
  let span_b = Tracer::start_span(tracer, "service-b-operation", Some(ctx_b))
  let ctx_b_final = SpanContext::from_span(span_b)
  
  // 验证采样决策一致性
  let is_sampled_b = SpanContext::is_sampled(ctx_b_final)
  assert_eq(is_sampled_a, is_sampled_b)
  
  Span::end(span_b)
  Span::end(span_a)
}