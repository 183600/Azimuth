// 跨服务传播和上下文管理测试
// Cross-service propagation and context management tests

test "多层级上下文传播测试" {
  // 测试上下文在多层级服务调用中的传播
  
  let root_ctx = Context::root()
  let service_key = ContextKey::new("service_name")
  let trace_key = ContextKey::new("trace_id")
  
  // 模拟服务A的上下文
  let service_a_ctx = Context::with_value(root_ctx, service_key, "service_a")
  let service_a_with_trace = Context::with_value(service_a_ctx, trace_key, "trace_12345")
  
  // 模拟服务B继承服务A的上下文
  let service_b_key = ContextKey::new("service_name")
  let service_b_ctx = Context::with_value(service_a_with_trace, service_b_key, "service_b")
  
  // 验证上下文传播
  let service_a_name = Context::get(service_b_ctx, service_key)
  let service_b_name = Context::get(service_b_ctx, service_b_key)
  let trace_id = Context::get(service_b_ctx, trace_key)
  
  assert_eq(service_a_name, Some("service_a"))
  assert_eq(service_b_name, Some("service_b"))
  assert_eq(trace_id, Some("trace_12345"))
  
  // 模拟服务C继承服务B的上下文
  let service_c_key = ContextKey::new("service_name")
  let operation_key = ContextKey::new("operation")
  let service_c_ctx = Context::with_value(service_b_ctx, service_c_key, "service_c")
  let service_c_with_op = Context::with_value(service_c_ctx, operation_key, "database_query")
  
  // 验证完整上下文链
  let final_service_a = Context::get(service_c_with_op, service_key)
  let final_service_b = Context::get(service_c_with_op, ContextKey::new("service_name"))
  let final_trace = Context::get(service_c_with_op, trace_key)
  let final_operation = Context::get(service_c_with_op, operation_key)
  
  assert_eq(final_service_a, Some("service_a"))
  assert_eq(final_service_b, Some("service_c")) // 最后设置的值
  assert_eq(final_trace, Some("trace_12345"))
  assert_eq(final_operation, Some("database_query"))
}

test "W3C TraceContext传播测试" {
  // 测试W3C TraceContext标准的传播实现
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "trace_context_test")
  
  // 创建初始Span
  let parent_span = Tracer::start_span(tracer, "parent_operation")
  let parent_ctx = Span::span_context(parent_span)
  
  // 模拟HTTP请求载体
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 注入上下文到载体
  let injection_ctx = Context::with_value(Context::root(), ContextKey::new("span_ctx"), "parent_span")
  CompositePropagator::inject(propagator, injection_ctx, carrier)
  
  // 验证载体中的traceparent
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_traceparent.is_some())
  
  // 模拟下游服务提取上下文
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  let extraction_result = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extraction_result, Some("true"))
  
  // 创建子Span
  let child_span = Tracer::start_span(tracer, "child_operation")
  let child_ctx = Span::span_context(child_span)
  
  // 验证父子关系
  assert_eq(SpanContext::trace_id(parent_ctx), "test_trace_id")
  assert_eq(SpanContext::trace_id(child_ctx), "test_trace_id")
  assert_true(SpanContext::is_sampled(parent_ctx))
  assert_true(SpanContext::is_sampled(child_ctx))
  
  Span::end(parent_span)
  Span::end(child_span)
}

test "Baggage传播链测试" {
  // 测试Baggage在服务链中的传播和累积
  
  let initial_baggage = Baggage::new()
  
  // 服务A添加baggage项
  let service_a_baggage = Baggage::set_entry(initial_baggage, "user.id", "user_12345")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "service.version", "1.0.0")
  
  // 服务B继承并添加baggage项
  let service_b_baggage = Baggage::set_entry(service_a_baggage, "request.id", "req_abcdef")
  let service_b_baggage = Baggage::set_entry(service_b_baggage, "operation.type", "read")
  
  // 服务C继承并添加baggage项
  let service_c_baggage = Baggage::set_entry(service_b_baggage, "database.table", "users")
  let service_c_baggage = Baggage::set_entry(service_c_baggage, "latency.ms", "150")
  
  // 验证baggage传播
  assert_eq(Baggage::get_entry(service_c_baggage, "user.id"), Some("user_12345"))
  assert_eq(Baggage::get_entry(service_c_baggage, "service.version"), Some("1.0.0"))
  assert_eq(Baggage::get_entry(service_c_baggage, "request.id"), Some("req_abcdef"))
  assert_eq(Baggage::get_entry(service_c_baggage, "operation.type"), Some("read"))
  assert_eq(Baggage::get_entry(service_c_baggage, "database.table"), Some("users"))
  assert_eq(Baggage::get_entry(service_c_baggage, "latency.ms"), Some("150"))
  
  // 测试baggage项移除
  let reduced_baggage = Baggage::remove_entry(service_c_baggage, "latency.ms")
  assert_eq(Baggage::get_entry(reduced_baggage, "latency.ms"), None)
  assert_eq(Baggage::get_entry(reduced_baggage, "user.id"), Some("user_12345"))
}

test "异步上下文传播测试" {
  // 测试异步操作中的上下文传播
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "async_test_tracer")
  
  // 创建主操作Span
  let main_span = Tracer::start_span(tracer, "main_async_operation")
  let async_key = ContextKey::new("async_operation")
  let correlation_key = ContextKey::new("correlation_id")
  
  // 设置异步上下文
  let async_ctx = Context::with_value(Context::root(), async_key, "async_processing")
  let correlation_ctx = Context::with_value(async_ctx, correlation_key, "corr_123456")
  
  // 模拟多个并行异步操作
  let mut async_results = []
  
  // 异步操作1
  let async1_ctx = Context::with_value(correlation_ctx, ContextKey::new("async_task"), "task_1")
  let async1_result = Context::get(async1_ctx, correlation_key)
  async_results.push(async1_result)
  
  // 异步操作2
  let async2_ctx = Context::with_value(correlation_ctx, ContextKey::new("async_task"), "task_2")
  let async2_result = Context::get(async2_ctx, correlation_key)
  async_results.push(async2_result)
  
  // 异步操作3
  let async3_ctx = Context::with_value(correlation_ctx, ContextKey::new("async_task"), "task_3")
  let async3_result = Context::get(async3_ctx, correlation_key)
  async_results.push(async3_result)
  
  // 验证所有异步操作都继承了相同的correlation_id
  for result in async_results {
    assert_eq(result, Some("corr_123456"))
  }
  
  Span::end(main_span)
}

test "跨服务错误传播测试" {
  // 测试错误信息在跨服务调用中的传播
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error_propagation_tracer")
  
  // 服务A创建Span并设置错误状态
  let service_a_span = Tracer::start_span(tracer, "service_a_operation")
  Span::set_status(service_a_span, Error, Some("Database connection failed"))
  
  // 模拟错误上下文传播
  let error_key = ContextKey::new("error_code")
  let error_message_key = ContextKey::new("error_message")
  let error_ctx = Context::with_value(Context::root(), error_key, "DB_CONN_ERROR")
  let error_ctx_with_msg = Context::with_value(error_ctx, error_message_key, "Database connection failed")
  
  // 服务B接收错误上下文
  let service_b_span = Tracer::start_span(tracer, "service_b_operation")
  let propagated_error_code = Context::get(error_ctx_with_msg, error_key)
  let propagated_error_message = Context::get(error_ctx_with_msg, error_message_key)
  
  // 服务B根据传播的错误设置状态
  match propagated_error_code {
    Some(code) => {
      Span::set_status(service_b_span, Error, Some("Propagated error: " + code))
    }
    None => ()
  }
  
  assert_eq(propagated_error_code, Some("DB_CONN_ERROR"))
  assert_eq(propagated_error_message, Some("Database connection failed"))
  assert_eq(Span::status(service_b_span), Error)
  
  Span::end(service_a_span)
  Span::end(service_b_span)
}

test "混合传播协议测试" {
  // 测试多种传播协议的混合使用
  
  let carrier = TextMapCarrier::new()
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 设置上下文
  let ctx = Context::with_value(Context::root(), ContextKey::new("user_id"), "user_789")
  
  // 注入trace context
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 手动注入baggage头（模拟baggage propagator）
  TextMapCarrier::set(carrier, "baggage", "user.id=user_789,service.name=api_gateway")
  
  // 验证所有头都被正确设置
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let baggage = TextMapCarrier::get(carrier, "baggage")
  
  assert_true(traceparent.is_some())
  assert_eq(baggage, Some("user.id=user_789,service.name=api_gateway"))
  
  // 提取并验证
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let user_id = Context::get(extracted_ctx, ContextKey::new("user_id"))
  let extraction_marker = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(user_id, Some("user_789"))
  assert_eq(extraction_marker, Some("true"))
}

test "上下文边界隔离测试" {
  // 测试不同上下文之间的边界隔离
  
  // 创建独立的上下文
  let ctx1 = Context::with_value(Context::root(), ContextKey::new("service"), "service_a")
  let ctx1_with_user = Context::with_value(ctx1, ContextKey::new("user"), "user_111")
  
  let ctx2 = Context::with_value(Context::root(), ContextKey::new("service"), "service_b")
  let ctx2_with_user = Context::with_value(ctx2, ContextKey::new("user"), "user_222")
  
  // 验证上下文隔离
  let ctx1_service = Context::get(ctx1_with_user, ContextKey::new("service"))
  let ctx1_user = Context::get(ctx1_with_user, ContextKey::new("user"))
  
  let ctx2_service = Context::get(ctx2_with_user, ContextKey::new("service"))
  let ctx2_user = Context::get(ctx2_with_user, ContextKey::new("user"))
  
  assert_eq(ctx1_service, Some("service_a"))
  assert_eq(ctx1_user, Some("user_111"))
  assert_eq(ctx2_service, Some("service_b"))
  assert_eq(ctx2_user, Some("user_222"))
  
  // 验证交叉访问返回None
  let ctx1_cross_user = Context::get(ctx1_with_user, ContextKey::new("user_from_ctx2"))
  let ctx2_cross_user = Context::get(ctx2_with_user, ContextKey::new("user_from_ctx1"))
  
  assert_true(ctx1_cross_user.is_none())
  assert_true(ctx2_cross_user.is_none())
  
  // 测试上下文合并时的键冲突处理
  let shared_key = ContextKey::new("shared_key")
  let merged_ctx1 = Context::with_value(ctx1_with_user, shared_key, "value_from_ctx1")
  let merged_ctx2 = Context::with_value(ctx2_with_user, shared_key, "value_from_ctx2")
  
  let merged_value1 = Context::get(merged_ctx1, shared_key)
  let merged_value2 = Context::get(merged_ctx2, shared_key)
  
  assert_eq(merged_value1, Some("value_from_ctx1"))
  assert_eq(merged_value2, Some("value_from_ctx2"))
}

test "Span上下文生命周期测试" {
  // 测试Span上下文的完整生命周期
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle_tracer")
  
  // 创建根Span
  let root_span = Tracer::start_span(tracer, "root_operation")
  let root_ctx = Span::span_context(root_span)
  
  // 验证根Span上下文
  assert_true(SpanContext::is_valid(root_ctx))
  assert_eq(SpanContext::trace_id(root_ctx), "test_trace_id")
  assert_eq(SpanContext::span_id(root_ctx), "test_span_id")
  
  // 创建子Span
  let child_span = Tracer::start_span(tracer, "child_operation")
  let child_ctx = Span::span_context(child_span)
  
  // 验证子Span上下文
  assert_true(SpanContext::is_valid(child_ctx))
  assert_eq(SpanContext::trace_id(child_ctx), "test_trace_id") // 相同的trace_id
  
  // 创建孙Span
  let grandchild_span = Tracer::start_span(tracer, "grandchild_operation")
  let grandchild_ctx = Span::span_context(grandchild_span)
  
  // 验证孙Span上下文
  assert_true(SpanContext::is_valid(grandchild_ctx))
  assert_eq(SpanContext::trace_id(grandchild_ctx), "test_trace_id") // 相同的trace_id
  
  // 按顺序结束Span
  Span::end(grandchild_span)
  Span::end(child_span)
  Span::end(root_span)
  
  // 验证Span状态
  assert_eq(Span::status(root_span), Unset)
  assert_eq(Span::status(child_span), Unset)
  assert_eq(Span::status(grandchild_span), Unset)
}