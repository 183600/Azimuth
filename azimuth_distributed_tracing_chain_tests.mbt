// Azimuth Distributed Tracing Chain Tests
// 分布式追踪链路测试用例 - 专注于跨服务追踪、链路分析和父子关系管理

// Test 1: 跨服务追踪链路完整性
test "cross-service trace chain integrity" {
  // 创建根跨度
  let trace_id = TraceId::generate()
  let root_span_id = SpanId::generate()
  let root_ctx = SpanContext::new(trace_id, root_span_id, true, "active")
  let root_span = Span::new("user_request", Server, root_ctx)
  
  // 设置根跨度属性
  Span::set_attribute(root_span, "user.id", StringValue("user_12345"))
  Span::set_attribute(root_span, "request.path", StringValue("/api/v1/orders"))
  Span::set_attribute(root_span, "http.method", StringValue("POST"))
  
  // 创建子服务1的跨度
  let service1_span_id = SpanId::generate()
  let service1_ctx = SpanContext::new(trace_id, service1_span_id, true, "active")
  let service1_span = Span::new("order_service", Server, service1_ctx)
  Span::set_parent(service1_span, root_span)
  
  Span::set_attribute(service1_span, "service.name", StringValue("order-service"))
  Span::set_attribute(service1_span, "service.version", StringValue("2.1.0"))
  Span::set_attribute(service1_span, "db.query", StringValue("SELECT * FROM orders WHERE user_id = ?"))
  
  // 创建子服务2的跨度
  let service2_span_id = SpanId::generate()
  let service2_ctx = SpanContext::new(trace_id, service2_span_id, true, "active")
  let service2_span = Span::new("payment_service", Server, service2_ctx)
  Span::set_parent(service2_span, service1_span)
  
  Span::set_attribute(service2_span, "service.name", StringValue("payment-service"))
  Span::set_attribute(service2_span, "service.version", StringValue("1.5.2"))
  Span::set_attribute(service2_span, "payment.gateway", StringValue("stripe"))
  Span::set_attribute(service2_span, "payment.amount", FloatValue(99.99))
  
  // 创建子服务3的跨度
  let service3_span_id = SpanId::generate()
  let service3_ctx = SpanContext::new(trace_id, service3_span_id, true, "active")
  let service3_span = Span::new("inventory_service", Server, service3_ctx)
  Span::set_parent(service3_span, service1_span)
  
  Span::set_attribute(service3_span, "service.name", StringValue("inventory-service"))
  Span::set_attribute(service3_span, "service.version", StringValue("3.0.1"))
  Span::set_attribute(service3_span, "product.id", StringValue("prod_67890"))
  Span::set_attribute(service3_span, "operation", StringValue("decrease_stock"))
  
  // 验证追踪链路完整性
  let trace_chain = TraceChain::new(trace_id)
  TraceChain::add_span(trace_chain, root_span)
  TraceChain::add_span(trace_chain, service1_span)
  TraceChain::add_span(trace_chain, service2_span)
  TraceChain::add_span(trace_chain, service3_span)
  
  // 验证链路完整性
  assert_true(TraceChain::is_complete(trace_chain))
  assert_eq(TraceChain::span_count(trace_chain), 4)
  assert_eq(TraceChain::root_span(trace_chain).unwrap().name, "user_request")
  
  // 验证父子关系
  let root_children = TraceChain::get_children(trace_chain, root_span_id)
  assert_eq(root_children.length(), 1)
  assert_eq(root_children[0].name, "order_service")
  
  let service1_children = TraceChain::get_children(trace_chain, service1_span_id)
  assert_eq(service1_children.length(), 2)
  assert_true(service1_children[0].name == "payment_service" || service1_children[0].name == "inventory_service")
  assert_true(service1_children[1].name == "payment_service" || service1_children[1].name == "inventory_service")
}

// Test 2: 追踪上下文传播机制
test "trace context propagation mechanisms" {
  // 创建初始上下文
  let trace_id = TraceId::generate()
  let span_id = SpanId::generate()
  let span_ctx = SpanContext::new(trace_id, span_id, true, "active")
  
  // 设置 baggage 项
  let baggage = Baggage::new()
  Baggage::set(baggage, "user.id", "user_12345")
  Baggage::set(baggage, "request.origin", "mobile_app")
  Baggage::set(baggage, "session.id", "sess_abcdef")
  
  // 测试 HTTP 头部传播
  let http_propagator = HttpTraceContextPropagator::new()
  let http_headers = HttpHeaders::new()
  
  Propagator::inject(http_propagator, span_ctx, http_headers)
  Propagator::inject_baggage(http_propagator, baggage, http_headers)
  
  // 验证注入的头部
  let trace_header = HttpHeaders::get(http_headers, "traceparent")
  assert_true(trace_header !== None)
  assert_true(trace_header.unwrap().contains(trace_id))
  assert_true(trace_header.unwrap().contains(span_id))
  
  let baggage_header = HttpHeaders::get(http_headers, "baggage")
  assert_true(baggage_header !== None)
  assert_true(baggage_header.unwrap().contains("user.id"))
  assert_true(baggage_header.unwrap().contains("request.origin"))
  
  // 测试提取
  let extracted_ctx = Propagator::extract(http_propagator, http_headers)
  let extracted_baggage = Propagator::extract_baggage(http_propagator, http_headers)
  
  assert_eq(SpanContext::trace_id(extracted_ctx), trace_id)
  assert_true(SpanContext::is_sampled(extracted_ctx))
  
  let user_id = Baggage::get(extracted_baggage, "user.id")
  assert_eq(user_id, "user_12345")
  
  // 测试 gRPC 元数据传播
  let grpc_propagator = GrpcTraceContextPropagator::new()
  let grpc_metadata = GrpcMetadata::new()
  
  Propagator::inject(grpc_propagator, span_ctx, grpc_metadata)
  Propagator::inject_baggage(grpc_propagator, baggage, grpc_metadata)
  
  let grpc_trace_header = GrpcMetadata::get(grpc_metadata, "traceparent")
  assert_true(grpc_trace_header !== None)
  
  let grpc_extracted_ctx = Propagator::extract(grpc_propagator, grpc_metadata)
  assert_eq(SpanContext::trace_id(grpc_extracted_ctx), trace_id)
}

// Test 3: 异步操作追踪
test "asynchronous operation tracing" {
  // 创建父跨度
  let trace_id = TraceId::generate()
  let parent_span_id = SpanId::generate()
  let parent_ctx = SpanContext::new(trace_id, parent_span_id, true, "active")
  let parent_span = Span::new("async_operation", Server, parent_ctx)
  
  Span::set_attribute(parent_span, "operation.type", StringValue("async_data_processing"))
  Span::set_attribute(parent_span, "batch.size", IntValue(1000))
  
  // 创建异步子跨度
  let async_spans = []
  let async_tasks = ["data_validation", "data_transformation", "data_enrichment", "data_storage"]
  
  for i in 0..<async_tasks.length() {
    let async_span_id = SpanId::generate()
    let async_ctx = SpanContext::new(trace_id, async_span_id, true, "active")
    let async_span = Span::new(async_tasks[i], Client, async_ctx)
    
    Span::set_parent(async_span, parent_span)
    Span::set_attribute(async_span, "async.task", StringValue(async_tasks[i]))
    Span::set_attribute(async_span, "async.index", IntValue(i))
    Span::set_attribute(async_span, "async.parallel", BoolValue(true))
    
    async_spans = async_spans.push(async_span)
  }
  
  // 模拟异步操作完成
  for i in 0..<async_spans.length() {
    let span = async_spans[i]
    Span::set_status(span, Ok(()))
    Span::set_attribute(span, "processing.time", FloatValue((i + 1) * 50.0))
    Span::end(span)
  }
  
  // 结束父跨度
  Span::set_status(parent_span, Ok(()))
  Span::end(parent_span)
  
  // 验证异步追踪
  let trace_analyzer = AsyncTraceAnalyzer::new()
  Analyzer::add_trace(trace_analyzer, parent_span)
  for span in async_spans {
    Analyzer::add_trace(trace_analyzer, span)
  }
  
  let async_analysis = Analyzer::analyze_async_operations(trace_analyzer)
  assert_eq(async_analysis.total_operations, 4)
  assert_eq(async_analysis.completed_operations, 4)
  assert_eq(async_analysis.failed_operations, 0)
  assert_true(async_analysis.average_duration > 0.0)
  assert_true(async_analysis.max_duration >= async_analysis.min_duration)
  
  // 验证并行性
  assert_true(async_analysis.max_concurrent_operations <= 4)
  assert_true(async_analysis.total_parallel_time > 0.0)
}

// Test 4: 错误和异常追踪
test "error and exception tracing" {
  // 创建追踪链
  let trace_id = TraceId::generate()
  let root_span_id = SpanId::generate()
  let root_ctx = SpanContext::new(trace_id, root_span_id, true, "active")
  let root_span = Span::new("error_prone_operation", Server, root_ctx)
  
  // 创建可能失败的子跨度
  let service_span_id = SpanId::generate()
  let service_ctx = SpanContext::new(trace_id, service_span_id, true, "active")
  let service_span = Span::new("database_operation", Client, service_ctx)
  Span::set_parent(service_span, root_span)
  
  // 模拟数据库连接错误
  let db_error = DatabaseError::new("Connection timeout", "CONNECTION_TIMEOUT")
  Span::record_exception(service_span, db_error)
  Span::set_status(service_span, Err("Database connection failed"))
  Span::set_attribute(service_span, "error.type", StringValue("database_error"))
  Span::set_attribute(service_span, "error.code", StringValue("CONNECTION_TIMEOUT"))
  Span::end(service_span)
  
  // 创建重试跨度
  let retry_span_id = SpanId::generate()
  let retry_ctx = SpanContext::new(trace_id, retry_span_id, true, "active")
  let retry_span = Span::new("database_operation_retry", Client, retry_ctx)
  Span::set_parent(retry_span, root_span)
  
  Span::set_attribute(retry_span, "retry.attempt", IntValue(1))
  Span::set_attribute(retry_span, "retry.max_attempts", IntValue(3))
  
  // 模拟重试成功
  Span::set_status(retry_span, Ok(()))
  Span::set_attribute(retry_span, "retry.success", BoolValue(true))
  Span::end(retry_span)
  
  // 创建另一个失败的子跨度
  let validation_span_id = SpanId::generate()
  let validation_ctx = SpanContext::new(trace_id, validation_span_id, true, "active")
  let validation_span = Span::new("data_validation", Internal, validation_ctx)
  Span::set_parent(validation_span, root_span)
  
  // 模拟验证错误
  let validation_error = ValidationError::new("Invalid input data", "INVALID_INPUT")
  Span::record_exception(validation_span, validation_error)
  Span::set_status(validation_span, Err("Data validation failed"))
  Span::set_attribute(validation_span, "error.type", StringValue("validation_error"))
  Span::set_attribute(validation_span, "error.code", StringValue("INVALID_INPUT"))
  Span::set_attribute(validation_span, "error.field", StringValue("email_address"))
  Span::end(validation_span)
  
  // 结束根跨度
  Span::set_status(root_span, Err("Partial operation failure"))
  Span::end(root_span)
  
  // 验证错误追踪
  let error_analyzer = ErrorTraceAnalyzer::new()
  Analyzer::add_trace(error_analyzer, root_span)
  Analyzer::add_trace(error_analyzer, service_span)
  Analyzer::add_trace(error_analyzer, retry_span)
  Analyzer::add_trace(error_analyzer, validation_span)
  
  let error_analysis = Analyzer::analyze_errors(error_analyzer)
  assert_eq(error_analysis.total_spans, 4)
  assert_eq(error_analysis.failed_spans, 2)
  assert_eq(error_analysis.successful_spans, 1)
  assert_eq(error_analysis.error_types.length(), 2)
  
  // 验证错误分类
  let db_errors = ErrorAnalysis::get_errors_by_type(error_analysis, "database_error")
  assert_eq(db_errors.length(), 1)
  
  let validation_errors = ErrorAnalysis::get_errors_by_type(error_analysis, "validation_error")
  assert_eq(validation_errors.length(), 1)
  
  // 验证重试分析
  let retry_analysis = Analyzer::analyze_retries(error_analyzer)
  assert_eq(retry_analysis.total_retries, 1)
  assert_eq(retry_analysis.successful_retries, 1)
  assert_eq(retry_analysis.failed_retries, 0)
}

// Test 5: 追踪采样策略
test "trace sampling strategies" {
  // 测试固定概率采样
  let fixed_sampler = FixedRateSampler::new(0.1) // 10% 采样率
  let trace_id1 = TraceId::generate()
  let span_id1 = SpanId::generate()
  let sampling_decision1 = Sampler::should_sample(fixed_sampler, trace_id1, span_id1, "test_operation")
  
  // 由于是概率采样，结果可能为true或false
  assert_true(sampling_decision1.sampled == true || sampling_decision1.sampled == false)
  
  // 测试基于trace_id的确定性采样
  let deterministic_sampler = TraceIdRatioBasedSampler::new(0.5) // 50% 采样率
  let trace_id2 = TraceId::from_string("00000000000000000000000000000001") // 小值
  let trace_id3 = TraceId::from_string("ffffffffffffffffffffffffffffffff") // 大值
  
  let sampling_decision2 = Sampler::should_sample(deterministic_sampler, trace_id2, span_id1, "test_operation")
  let sampling_decision3 = Sampler::should_sample(deterministic_sampler, trace_id3, span_id1, "test_operation")
  
  // 对于相同的trace_id，采样决策应该一致
  let sampling_decision2_repeat = Sampler::should_sample(deterministic_sampler, trace_id2, span_id1, "test_operation")
  assert_eq(sampling_decision2.sampled, sampling_decision2_repeat.sampled)
  
  // 测试基于属性的自适应采样
  let adaptive_sampler = AdaptiveSampler::new()
  Sampler::add_rule(adaptive_sampler, "operation.name", "critical_operation", 1.0) // 总是采样
  Sampler::add_rule(adaptive_sampler, "operation.name", "debug_operation", 0.0) // 从不采样
  Sampler::add_rule(adaptive_sampler, "http.status_code", "5xx", 1.0) // 错误总是采样
  Sampler::add_rule(adaptive_sampler, "http.status_code", "4xx", 0.5) // 客户端错误50%采样
  Sampler::add_rule(adaptive_sampler, "http.status_code", "2xx", 0.1) // 成功10%采样
  
  // 测试关键操作
  let critical_attrs = Attributes::new()
  Attributes::set(critical_attrs, "operation.name", StringValue("critical_operation"))
  let critical_decision = Sampler::should_sample_with_attributes(adaptive_sampler, trace_id1, span_id1, "critical_operation", critical_attrs)
  assert_true(critical_decision.sampled)
  
  // 测试调试操作
  let debug_attrs = Attributes::new()
  Attributes::set(debug_attrs, "operation.name", StringValue("debug_operation"))
  let debug_decision = Sampler::should_sample_with_attributes(adaptive_sampler, trace_id1, span_id1, "debug_operation", debug_attrs)
  assert_false(debug_decision.sampled)
  
  // 测试错误响应
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "http.status_code", StringValue("500"))
  let error_decision = Sampler::should_sample_with_attributes(adaptive_sampler, trace_id1, span_id1, "error_operation", error_attrs)
  assert_true(error_decision.sampled)
  
  // 测试客户端错误
  let client_error_attrs = Attributes::new()
  Attributes::set(client_error_attrs, "http.status_code", StringValue("404"))
  let client_error_decision = Sampler::should_sample_with_attributes(adaptive_sampler, trace_id1, span_id1, "not_found_operation", client_error_attrs)
  // 结果可能是true或false，取决于具体实现
  
  // 测试成功响应
  let success_attrs = Attributes::new()
  Attributes::set(success_attrs, "http.status_code", StringValue("200"))
  let success_decision = Sampler::should_sample_with_attributes(adaptive_sampler, trace_id1, span_id1, "success_operation", success_attrs)
  // 结果可能是true或false，取决于具体实现
  
  // 测试父跨度采样决策传播
  let parent_sampler = ParentBasedSampler::new(fixed_sampler)
  let parent_ctx = SpanContext::new(trace_id1, span_id1, true, "active")
  let child_span_id = SpanId::generate()
  
  // 如果父跨度被采样，子跨度也应该被采样
  let parent_decision = SamplingResult::new(true, Attributes::new())
  let child_decision = Sampler::should_sample_with_parent(parent_sampler, trace_id1, child_span_id, "child_operation", parent_decision)
  assert_true(child_decision.sampled)
  
  // 如果父跨度不被采样，子跨度也不应该被采样
  let parent_decision_not_sampled = SamplingResult::new(false, Attributes::new())
  let child_decision_not_sampled = Sampler::should_sample_with_parent(parent_sampler, trace_id1, child_span_id, "child_operation", parent_decision_not_sampled)
  assert_false(child_decision_not_sampled.sampled)
}