// Azimuth 分布式追踪增强测试用例
// 测试分布式追踪系统的高级功能，包括跨服务传播、上下文管理和追踪关联

// 测试1: 跨服务追踪传播
test "跨服务追踪传播功能" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "distributed.test")
  
  // 在服务A中创建根span
  let root_span = Tracer::start_span(tracer, "serviceA.operation")
  Span::set_attribute(root_span, "service.name", "serviceA")
  Span::set_attribute(root_span, "operation.type", "request")
  
  // 获取span上下文
  let span_context = Span::span_context(root_span)
  let trace_id = SpanContext::trace_id(span_context)
  let span_id = SpanContext::span_id(span_context)
  
  // 创建传播载体
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-" + trace_id + "-" + span_id + "-01")
  TextMapCarrier::set(carrier, "baggage", "user.id=12345,request.source=web")
  
  // 模拟HTTP请求头传播
  let http_headers = [("traceparent", TextMapCarrier::get(carrier, "traceparent")), 
                      ("baggage", TextMapCarrier::get(carrier, "baggage"))]
  
  // 在服务B中提取上下文
  let propagator = TraceContextPropagator::new()
  let extracted_context = Propagator::extract(propagator, http_headers)
  
  // 在服务B中创建子span
  let child_span = Tracer::start_span_with_context(tracer, "serviceB.operation", extracted_context)
  Span::set_attribute(child_span, "service.name", "serviceB")
  Span::set_attribute(child_span, "operation.type", "process")
  
  // 验证父子关系
  let child_context = Span::span_context(child_span)
  let child_trace_id = SpanContext::trace_id(child_context)
  let parent_span_id = SpanContext::parent_span_id(child_context)
  
  assert_eq(child_trace_id, trace_id) // 追踪ID应该相同
  assert_eq(parent_span_id, span_id) // 父span ID应该匹配
  
  // 结束spans
  Span::end(child_span)
  Span::end(root_span)
  
  assert_true(true)
}

// 测试2: 追踪上下文管理
test "追踪上下文管理功能" {
  // 创建上下文管理器
  let context_manager = ContextManager::new()
  
  // 创建初始上下文
  let initial_context = Context::empty()
  let span_context = SpanContext::new("trace123", "span456", true, true)
  
  // 将span上下文添加到当前上下文
  let context_with_span = ContextManager::set_span(context_manager, initial_context, span_context)
  
  // 验证上下文中的span
  let retrieved_span = ContextManager::get_span(context_with_span)
  assert_eq(SpanContext::trace_id(retrieved_span), "trace123")
  assert_eq(SpanContext::span_id(retrieved_span), "span456")
  
  // 添加baggage项
  let context_with_baggage = ContextManager::set_baggage(context_with_span, "user.id", "12345")
  let context_with_more_baggage = ContextManager::set_baggage(context_with_baggage, "request.id", "req789")
  
  // 验证baggage项
  let user_id = ContextManager::get_baggage(context_with_more_baggage, "user.id")
  let request_id = ContextManager::get_baggage(context_with_more_baggage, "request.id")
  let non_existent = ContextManager::get_baggage(context_with_more_baggage, "non.existent")
  
  assert_eq(user_id, "12345")
  assert_eq(request_id, "req789")
  assert_eq(non_existent, "")
  
  // 测试上下文传播
  let propagated_context = ContextManager::propagate(context_with_more_baggage)
  let propagated_user_id = ContextManager::get_baggage(propagated_context, "user.id")
  
  assert_eq(propagated_user_id, "12345")
}

// 测试3: 追踪采样策略
test "追踪采样策略功能" {
  // 创建采样器
  let always_on_sampler = AlwaysOnSampler::new()
  let always_off_sampler = AlwaysOffSampler::new()
  let trace_id_ratio_sampler = TraceIdRatioBasedSampler::new(0.5) // 50%采样率
  
  // 测试始终开启采样
  let sampling_decision1 = Sampler::should_sample(always_on_sampler, 
    SamplingParameters::new("trace123", "span456", "test.operation", [("service", "test")]))
  assert_eq(SamplingDecision::decision(sampling_decision1), Decision::RecordAndSample)
  
  // 测试始终关闭采样
  let sampling_decision2 = Sampler::should_sample(always_off_sampler, 
    SamplingParameters::new("trace456", "span789", "test.operation", [("service", "test")]))
  assert_eq(SamplingDecision::decision(sampling_decision2), Decision::Drop)
  
  // 测试基于比例的采样（使用固定的trace_id确保可预测结果）
  let sampling_decision3 = Sampler::should_sample(trace_id_ratio_sampler, 
    SamplingParameters::new("00000000000000001111111111111111", "span123", "test.operation", [("service", "test")]))
  // 由于trace_id是固定的，采样结果应该是可预测的
  assert_true(SamplingDecision::decision(sampling_decision3) == Decision::RecordAndSample || 
              SamplingDecision::decision(sampling_decision3) == Decision::RecordOnly)
  
  // 测试基于属性的采样
  let attribute_sampler = AttributeBasedSampler::new([("service.level", "critical")])
  let sampling_decision4 = Sampler::should_sample(attribute_sampler, 
    SamplingParameters::new("trace789", "span456", "critical.operation", [("service.level", "critical")]))
  assert_eq(SamplingDecision::decision(sampling_decision4), Decision::RecordAndSample)
  
  let sampling_decision5 = Sampler::should_sample(attribute_sampler, 
    SamplingParameters::new("trace101", "span789", "normal.operation", [("service.level", "normal")]))
  assert_eq(SamplingDecision::decision(sampling_decision5), Decision::Drop)
}

// 测试4: 追踪链路完整性
test "追踪链路完整性功能" {
  // 创建追踪链路验证器
  let chain_validator = TraceChainValidator::new()
  
  // 创建一系列相关的spans
  let root_span = SpanData::new("root.operation", 1000, 1500, [])
  let child_span1 = SpanData::with_parent("child.operation1", 1100, 1200, [], "root.operation")
  let child_span2 = SpanData::with_parent("child.operation2", 1200, 1300, [], "root.operation")
  let grandchild_span = SpanData::with_parent("grandchild.operation", 1150, 1180, [], "child.operation1")
  
  // 添加spans到验证器
  TraceChainValidator::add_span(chain_validator, root_span)
  TraceChainValidator::add_span(chain_validator, child_span1)
  TraceChainValidator::add_span(chain_validator, child_span2)
  TraceChainValidator::add_span(chain_validator, grandchild_span)
  
  // 验证链路完整性
  let is_complete = TraceChainValidator::is_chain_complete(chain_validator)
  let missing_spans = TraceChainValidator::find_missing_spans(chain_validator)
  let orphan_spans = TraceChainValidator::find_orphan_spans(chain_validator)
  
  assert_true(is_complete)
  assert_eq(missing_spans.length(), 0)
  assert_eq(orphan_spans.length(), 0)
  
  // 测试不完整的链路
  let orphan_span = SpanData::with_parent("orphan.operation", 1400, 1450, [], "non.existent.parent")
  TraceChainValidator::add_span(chain_validator, orphan_span)
  
  let is_still_complete = TraceChainValidator::is_chain_complete(chain_validator)
  let new_orphan_spans = TraceChainValidator::find_orphan_spans(chain_validator)
  
  assert_false(is_still_complete)
  assert_eq(new_orphan_spans.length(), 1)
  assert_eq(new_orphan_spans[0], "orphan.operation")
}

// 测试5: 异步操作追踪
test "异步操作追踪功能" {
  // 创建异步追踪器
  let async_tracer = AsyncTracer::new("async.test")
  
  // 创建根span
  let root_span = AsyncTracer::start_span(async_tracer, "async.root.operation")
  
  // 模拟异步操作
  let async_operation1 = AsyncTracer::start_async_span(async_tracer, "async.operation1", root_span)
  let async_operation2 = AsyncTracer::start_async_span(async_tracer, "async.operation2", root_span)
  
  // 设置异步操作属性
  AsyncSpan::set_attribute(async_operation1, "operation.type", "database.query")
  AsyncSpan::set_attribute(async_operation2, "operation.type", "api.call")
  
  // 模拟异步操作完成
  AsyncSpan::add_event(async_operation1, "query.started", [])
  AsyncSpan::add_event(async_operation2, "api.request.sent", [])
  
  // 模拟一些延迟
  // 在实际环境中，这些可能是真正的异步操作
  
  AsyncSpan::add_event(async_operation1, "query.completed", [("rows", "150")])
  AsyncSpan::add_event(async_operation2, "api.response.received", [("status", "200")])
  
  // 完成异步操作
  AsyncSpan::end(async_operation1)
  AsyncSpan::end(async_operation2)
  
  // 完成根span
  AsyncTracer::end_span(async_tracer, root_span)
  
  // 验证异步追踪数据
  let async_spans = AsyncTracer::get_completed_async_spans(async_tracer)
  assert_eq(async_spans.length(), 2)
  
  let span1 = async_spans.filter(fn(s) { AsyncSpan::name(s) == "async.operation1" })[0]
  let span2 = async_spans.filter(fn(s) { AsyncSpan::name(s) == "async.operation2" })[0]
  
  assert_eq(AsyncSpan::get_attribute(span1, "operation.type"), "database.query")
  assert_eq(AsyncSpan::get_attribute(span2, "operation.type"), "api.call")
  
  // 验证父子关系
  assert_eq(AsyncSpan::parent_span_id(span1), AsyncSpan::span_id(root_span))
  assert_eq(AsyncSpan::parent_span_id(span2), AsyncSpan::span_id(root_span))
}

// 测试6: 跨协议追踪传播
test "跨协议追踪传播功能" {
  // 创建跨协议传播器
  let cross_protocol_propagator = CrossProtocolPropagator::new()
  
  // 创建初始追踪上下文
  let trace_context = TraceContext::new("trace123", "span456", true, [])
  
  // HTTP传播
  let http_carrier = HttpHeaderCarrier::new()
  CrossProtocolPropagator::inject_http(cross_protocol_propagator, trace_context, http_carrier)
  
  let http_traceparent = HttpHeaderCarrier::get(http_carrier, "traceparent")
  let http_baggage = HttpHeaderCarrier::get(http_carrier, "baggage")
  
  assert_true(http_traceparent.contains("trace123"))
  assert_true(http_traceparent.contains("span456"))
  
  // gRPC传播
  let grpc_carrier = GrpcMetadataCarrier::new()
  CrossProtocolPropagator::inject_grpc(cross_protocol_propagator, trace_context, grpc_carrier)
  
  let grpc_traceparent = GrpcMetadataCarrier::get(grpc_carrier, "grpc-trace-bin")
  assert_true(grpc_traceparent.length() > 0)
  
  // 消息队列传播
  let mq_carrier = MessagePropertiesCarrier::new()
  CrossProtocolPropagator::inject_mq(cross_protocol_propagator, trace_context, mq_carrier)
  
  let mq_trace_id = MessagePropertiesCarrier::get(mq_carrier, "trace_id")
  let mq_span_id = MessagePropertiesCarrier::get(mq_carrier, "span_id")
  
  assert_eq(mq_trace_id, "trace123")
  assert_eq(mq_span_id, "span456")
  
  // 测试跨协议提取
  let extracted_from_http = CrossProtocolPropagator::extract_http(cross_protocol_propagator, http_carrier)
  let extracted_from_grpc = CrossProtocolPropagator::extract_grpc(cross_protocol_propagator, grpc_carrier)
  let extracted_from_mq = CrossProtocolPropagator::extract_mq(cross_protocol_propagator, mq_carrier)
  
  assert_eq(TraceContext::trace_id(extracted_from_http), "trace123")
  assert_eq(TraceContext::trace_id(extracted_from_grpc), "trace123")
  assert_eq(TraceContext::trace_id(extracted_from_mq), "trace123")
}