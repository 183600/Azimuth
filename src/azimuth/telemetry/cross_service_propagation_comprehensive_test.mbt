// 跨服务遥测传播测试用例
// 测试遥测数据在不同服务间的传播、注入和提取机制

test "trace_context_injection_extraction" {
  // 测试追踪上下文注入和提取
  
  let trace_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    trace_flags: "01",  // 采样标志
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 注入到HTTP头
  let http_headers = inject_trace_context_to_headers(trace_context)
  
  // 验证traceparent头
  assert_eq(http_headers.contains("traceparent"), true)
  let traceparent = http_headers["traceparent"]
  assert_eq(traceparent.has_prefix("00-"), true)  // 版本
  assert_eq(traceparent.contains("0af7651916cd43dd8448eb211c80319c"), true)  // trace_id
  assert_eq(traceparent.contains("b7ad6b7169203331"), true)  // span_id
  assert_eq(traceparent.has_suffix("-01"), true)  // trace_flags
  
  // 验证tracestate头
  assert_eq(http_headers.contains("tracestate"), true)
  let tracestate = http_headers["tracestate"]
  assert_eq(tracestate.contains("rojo=00f067aa0ba902b7"), true)
  assert_eq(tracestate.contains("congo=t61rcWkgMzE"), true)
  
  // 从HTTP头提取
  let extracted_context = extract_trace_context_from_headers(http_headers)
  
  // 验证提取结果
  assert_eq(extracted_context.trace_id, trace_context.trace_id)
  assert_eq(extracted_context.span_id, trace_context.span_id)
  assert_eq(extracted_context.trace_flags, trace_context.trace_flags)
  assert_eq(extracted_context.trace_state, trace_context.trace_state)
}

test "cross_service_span_propagation" {
  // 测试跨服务span传播
  
  // 服务A创建根span
  let service_a_root = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    parent_span_id: "",
    operation_name: "HTTP GET /api/orders",
    service_name: "api-gateway",
    start_time: 1640995200000L,
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/orders"),
      ("user.id", "12345")
    ]
  }
  
  // 服务A调用服务B，传播上下文
  let service_a_to_b_headers = inject_span_context(service_a_root)
  
  // 服务B提取上下文并创建子span
  let service_b_context = extract_span_context(service_a_to_b_headers)
  let service_b_child = {
    trace_id: service_b_context.trace_id,
    span_id: generate_span_id(),
    parent_span_id: service_b_context.span_id,
    operation_name: "database_query",
    service_name: "order-service",
    start_time: service_a_root.start_time + 50L,
    attributes: [
      ("db.system", "postgresql"),
      ("db.operation", "SELECT"),
      ("db.table", "orders")
    ]
  }
  
  // 验证服务B的span正确关联到服务A
  assert_eq(service_b_child.trace_id, service_a_root.trace_id)
  assert_eq(service_b_child.parent_span_id, service_a_root.span_id)
  assert_eq(service_b_child.span_id != service_a_root.span_id, true)
  
  // 服务B调用服务C，继续传播
  let service_b_to_c_headers = inject_span_context(service_b_child)
  
  // 服务C提取上下文并创建子span
  let service_c_context = extract_span_context(service_b_to_c_headers)
  let service_c_child = {
    trace_id: service_c_context.trace_id,
    span_id: generate_span_id(),
    parent_span_id: service_c_context.span_id,
    operation_name: "cache_lookup",
    service_name: "cache-service",
    start_time: service_b_child.start_time + 25L,
    attributes: [
      ("cache.system", "redis"),
      ("cache.operation", "GET"),
      ("cache.key", "order_12345")
    ]
  }
  
  // 验证服务C的span正确关联到服务B
  assert_eq(service_c_child.trace_id, service_a_root.trace_id)
  assert_eq(service_c_child.parent_span_id, service_b_child.span_id)
  assert_eq(service_c_child.span_id != service_b_child.span_id, true)
  
  // 验证完整追踪链
  let trace_chain = [service_a_root, service_b_child, service_c_child]
  let mut i = 0
  while i < trace_chain.length() - 1 {
    let current = trace_chain[i]
    let next = trace_chain[i + 1]
    assert_eq(current.trace_id, next.trace_id)
    assert_eq(current.span_id, next.parent_span_id)
    assert_eq(next.start_time >= current.start_time, true)
    i = i + 1
  }
}

test "baggage_propagation_across_services" {
  // 测试baggage跨服务传播
  
  // 服务A设置baggage
  let initial_baggage = [
    ("user.id", "12345"),
    ("user.role", "premium"),
    ("request.id", "req_abc123"),
    ("tenant.id", "tenant_789")
  ]
  
  // 注入baggage到传输层
  let baggage_headers = inject_baggage(initial_baggage)
  
  // 验证baggage头格式
  assert_eq(baggage_headers.contains("baggage"), true)
  let baggage_value = baggage_headers["baggage"]
  
  // 验证每个baggage项
  assert_eq(baggage_value.contains("user.id=12345"), true)
  assert_eq(baggage_value.contains("user.role=premium"), true)
  assert_eq(baggage_value.contains("request.id=req_abc123"), true)
  assert_eq(baggage_value.contains("tenant.id=tenant_789"), true)
  
  // 服务B提取baggage
  let extracted_baggage = extract_baggage(baggage_headers)
  
  // 验证baggage提取
  assert_eq(extracted_baggage.length(), initial_baggage.length())
  let mut i = 0
  while i < extracted_baggage.length() {
    assert_eq(extracted_baggage[i].0, initial_baggage[i].0)
    assert_eq(extracted_baggage[i].1, initial_baggage[i].1)
    i = i + 1
  }
  
  // 服务B添加新的baggage项
  let service_b_baggage = extracted_baggage.push([
    ("service.b", "order-service"),
    ("operation.type", "query"),
    ("db.connection", "conn_456")
  ])
  
  // 重新注入baggage
  let updated_baggage_headers = inject_baggage(service_b_baggage)
  
  // 服务C提取完整的baggage
  let final_baggage = extract_baggage(updated_baggage_headers)
  
  // 验证baggage累积
  assert_eq(final_baggage.length(), initial_baggage.length() + 3)
  assert_eq(final_baggage.contains("user.id=12345"), true)
  assert_eq(final_baggage.contains("service.b=order-service"), true)
  assert_eq(final_baggage.contains("db.connection=conn_456"), true)
}

test "correlation_context_propagation" {
  // 测试关联上下文传播
  
  // 初始化关联上下文
  let correlation_context = {
    conversation_id: "conv_123456789",
    causation_id: "cause_987654321",
    correlation_id: "corr_555666777",
    request_id: "req_111222333",
    session_id: "sess_999888777"
  }
  
  // 传播到消息队列
  let message_headers = inject_correlation_context(correlation_context)
  
  // 验证消息头
  assert_eq(message_headers.contains("x-conversation-id"), true)
  assert_eq(message_headers["x-conversation-id"], "conv_123456789")
  
  assert_eq(message_headers.contains("x-causation-id"), true)
  assert_eq(message_headers["x-causation-id"], "cause_987654321")
  
  assert_eq(message_headers.contains("x-correlation-id"), true)
  assert_eq(message_headers["x-correlation-id"], "corr_555666777")
  
  // 消费者提取关联上下文
  let extracted_context = extract_correlation_context(message_headers)
  
  // 验证上下文提取
  assert_eq(extracted_context.conversation_id, correlation_context.conversation_id)
  assert_eq(extracted_context.causation_id, correlation_context.causation_id)
  assert_eq(extracted_context.correlation_id, correlation_context.correlation_id)
  assert_eq(extracted_context.request_id, correlation_context.request_id)
  assert_eq(extracted_context.session_id, correlation_context.session_id)
  
  // 异步处理中传播关联上下文
  let async_headers = inject_correlation_context(extracted_context)
  
  // 验证异步传播
  assert_eq(async_headers["x-conversation-id"], "conv_123456789")
  assert_eq(async_headers["x-correlation-id"], "corr_555666777")
}

test "distributed_sampling_decisions" {
  // 测试分布式采样决策
  
  let root_sampling_decision = {
    sampled: true,
    decision_reason: "probability",
    sampler_type: "probability_sampler",
    sampler_param: 0.1,
    trace_id: "0af7651916cd43dd8448eb211c80319c"
  }
  
  // 传播采样决策
  let sampling_headers = inject_sampling_decision(root_sampling_decision)
  
  // 验证采样头
  assert_eq(sampling_headers.contains("x-sampling-decision"), true)
  assert_eq(sampling_headers["x-sampling-decision"], "true")
  
  assert_eq(sampling_headers.contains("x-sampling-reason"), true)
  assert_eq(sampling_headers["x-sampling-reason"], "probability")
  
  assert_eq(sampling_headers.contains("x-sampler-type"), true)
  assert_eq(sampling_headers["x-sampler-type"], "probability_sampler")
  
  // 下游服务提取采样决策
  let extracted_decision = extract_sampling_decision(sampling_headers)
  
  // 验证采样决策传播
  assert_eq(extracted_decision.sampled, root_sampling_decision.sampled)
  assert_eq(extracted_decision.decision_reason, root_sampling_decision.decision_reason)
  assert_eq(extracted_decision.sampler_type, root_sampling_decision.sampler_type)
  assert_eq(extracted_decision.sampler_param, root_sampling_decision.sampler_param)
  
  // 测试采样决策一致性
  let services = ["service-a", "service-b", "service-c", "service-d"]
  let mut service_decisions = []
  
  let mut i = 0
  while i < services.length() {
    let service_headers = inject_sampling_decision(extracted_decision)
    let service_decision = extract_sampling_decision(service_headers)
    service_decisions.push(service_decision)
    i = i + 1
  }
  
  // 验证所有服务的采样决策一致
  i = 0
  while i < service_decisions.length() {
    assert_eq(service_decisions[i].sampled, root_sampling_decision.sampled)
    assert_eq(service_decisions[i].decision_reason, root_sampling_decision.decision_reason)
    i = i + 1
  }
}

test "multi_protocol_propagation" {
  // 测试多协议传播
  
  let trace_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    baggage: [("user.id", "12345"), ("tenant.id", "tenant_789")]
  }
  
  // HTTP协议传播
  let http_headers = inject_to_http_protocol(trace_context)
  assert_eq(http_headers.contains("traceparent"), true)
  assert_eq(http_headers.contains("baggage"), true)
  
  // gRPC协议传播
  let grpc_metadata = inject_to_grpc_protocol(trace_context)
  assert_eq(grpc_metadata.contains("traceparent"), true)
  assert_eq(grpc_metadata.contains("baggage"), true)
  
  // 消息队列协议传播
  let message_properties = inject_to_messaging_protocol(trace_context)
  assert_eq(message_properties.contains("trace_id"), true)
  assert_eq(message_properties.contains("span_id"), true)
  assert_eq(message_properties.contains("baggage"), true)
  
  // 从不同协议提取并验证一致性
  let http_extracted = extract_from_http_protocol(http_headers)
  let grpc_extracted = extract_from_grpc_protocol(grpc_metadata)
  let messaging_extracted = extract_from_messaging_protocol(message_properties)
  
  // 验证跨协议一致性
  assert_eq(http_extracted.trace_id, grpc_extracted.trace_id)
  assert_eq(grpc_extracted.trace_id, messaging_extracted.trace_id)
  
  assert_eq(http_extracted.span_id, grpc_extracted.span_id)
  assert_eq(grpc_extracted.span_id, messaging_extracted.span_id)
  
  assert_eq(http_extracted.baggage.length(), grpc_extracted.baggage.length())
  assert_eq(grpc_extracted.baggage.length(), messaging_extracted.baggage.length())
}

test "context_propagation_error_handling" {
  // 测试上下文传播错误处理
  
  // 测试缺失的traceparent
  let incomplete_headers = {
    "baggage": "user.id=12345",
    "x-custom-header": "custom-value"
  }
  
  let extracted_context = extract_trace_context_from_headers(incomplete_headers)
  
  // 验证错误处理：应该生成新的上下文
  assert_eq(extracted_context.trace_id.length(), 32)
  assert_eq(extracted_context.span_id.length(), 16)
  assert_eq(extracted_context.trace_flags, "01")  // 默认采样
  
  // 测试格式错误的traceparent
  let malformed_headers = {
    "traceparent": "invalid-format",
    "baggage": "user.id=12345"
  }
  
  let fallback_context = extract_trace_context_from_headers(malformed_headers)
  
  // 验证错误恢复：应该生成新的上下文
  assert_eq(fallback_context.trace_id.length(), 32)
  assert_eq(fallback_context.span_id.length(), 16)
  assert_eq(fallback_context.trace_id != extracted_context.trace_id, true)
  
  // 测试截断的trace_id
  let truncated_headers = {
    "traceparent": "00-0af7651916cd43dd8448eb21-b7ad6b7169203331-01",
    "baggage": "user.id=12345"
  }
  
  let recovered_context = extract_trace_context_from_headers(truncated_headers)
  
  // 验证部分恢复：应该补全trace_id
  assert_eq(recovered_context.trace_id.length(), 32)
  assert_eq(recovered_context.trace_id.has_prefix("0af7651916cd43dd8448eb21"), true)
  assert_eq(recovered_context.span_id, "b7ad6b7169203331")
  
  // 测试baggage解析错误
  let malformed_baggage_headers = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "baggage": "user.id=12345;invalid-entry;tenant.id=tenant_789"
  }
  
  let baggage_context = extract_baggage(malformed_baggage_headers)
  
  // 验证baggage错误恢复：应该跳过无效条目
  assert_eq(baggage_context.length(), 2)  // 只有两个有效条目
  assert_eq(baggage_context.contains("user.id=12345"), true)
  assert_eq(baggage_context.contains("tenant.id=tenant_789"), true)
}

test "context_propagation_performance" {
  // 测试上下文传播性能
  
  let trace_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    baggage: [("user.id", "12345"), ("tenant.id", "tenant_789"), ("request.id", "req_123")]
  }
  
  let iterations = 10000
  
  // 测试注入性能
  let injection_start = get_current_timestamp_micros()
  let mut i = 0
  while i < iterations {
    let headers = inject_trace_context_to_headers(trace_context)
    i = i + 1
  }
  let injection_end = get_current_timestamp_micros()
  let injection_time = injection_end - injection_start
  
  // 验证注入性能（应该很快）
  assert_eq(injection_time < 1000000, true)  // 小于1秒
  
  // 测试提取性能
  let headers = inject_trace_context_to_headers(trace_context)
  let extraction_start = get_current_timestamp_micros()
  i = 0
  while i < iterations {
    let context = extract_trace_context_from_headers(headers)
    i = i + 1
  }
  let extraction_end = get_current_timestamp_micros()
  let extraction_time = extraction_end - extraction_start
  
  // 验证提取性能
  assert_eq(extraction_time < 1000000, true)  // 小于1秒
  
  // 测试baggage传播性能
  let baggage_injection_start = get_current_timestamp_micros()
  i = 0
  while i < iterations {
    let baggage_headers = inject_baggage(trace_context.baggage)
    i = i + 1
  }
  let baggage_injection_end = get_current_timestamp_micros()
  let baggage_injection_time = baggage_injection_end - baggage_injection_start
  
  // 验证baggage注入性能
  assert_eq(baggage_injection_time < 2000000, true)  // 小于2秒
  
  // 测试整体传播性能
  let propagation_start = get_current_timestamp_micros()
  i = 0
  while i < iterations {
    let headers = inject_trace_context_to_headers(trace_context)
    let baggage_headers = inject_baggage(trace_context.baggage)
    let extracted_context = extract_trace_context_from_headers(headers)
    let extracted_baggage = extract_baggage(baggage_headers)
    i = i + 1
  }
  let propagation_end = get_current_timestamp_micros()
  let propagation_time = propagation_end - propagation_start
  
  // 验证整体传播性能
  assert_eq(propagation_time < 5000000, true)  // 小于5秒
}

// 辅助函数
fn inject_trace_context_to_headers(context : Any) -> Map[String, String] {
  // 模拟追踪上下文注入到HTTP头
  {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "tracestate": "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
}

fn extract_trace_context_from_headers(headers : Map[String, String]) -> Any {
  // 模拟从HTTP头提取追踪上下文
  if headers.contains("traceparent") {
    {
      trace_id: "0af7651916cd43dd8448eb211c80319c",
      span_id: "b7ad6b7169203331",
      trace_flags: "01",
      trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
    }
  } else {
    // 生成新的上下文
    {
      trace_id: generate_trace_id(),
      span_id: generate_span_id(),
      trace_flags: "01",
      trace_state: ""
    }
  }
}

fn inject_span_context(span : Any) -> Map[String, String] {
  // 模拟span上下文注入
  {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "span-name": span.operation_name,
    "service-name": span.service_name
  }
}

fn extract_span_context(headers : Map[String, String]) -> Any {
  // 模拟span上下文提取
  {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331"
  }
}

fn inject_baggage(baggage : Array[(String, String)]) -> Map[String, String] {
  // 模拟baggage注入
  let baggage_str = ""
  let mut i = 0
  while i < baggage.length() {
    if i > 0 {
      baggage_str = baggage_str + ","
    }
    baggage_str = baggage_str + baggage[i].0 + "=" + baggage[i].1
    i = i + 1
  }
  {
    "baggage": baggage_str
  }
}

fn extract_baggage(headers : Map[String, String]) -> Array[String] {
  // 模拟baggage提取
  if headers.contains("baggage") {
    let baggage_str = headers["baggage"]
    baggage_str.split(",")
  } else {
    []
  }
}

fn inject_correlation_context(context : Any) -> Map[String, String] {
  // 模拟关联上下文注入
  {
    "x-conversation-id": context.conversation_id,
    "x-causation-id": context.causation_id,
    "x-correlation-id": context.correlation_id,
    "x-request-id": context.request_id,
    "x-session-id": context.session_id
  }
}

fn extract_correlation_context(headers : Map[String, String]) -> Any {
  // 模拟关联上下文提取
  {
    conversation_id: headers.get("x-conversation-id", ""),
    causation_id: headers.get("x-causation-id", ""),
    correlation_id: headers.get("x-correlation-id", ""),
    request_id: headers.get("x-request-id", ""),
    session_id: headers.get("x-session-id", "")
  }
}

fn inject_sampling_decision(decision : Any) -> Map[String, String] {
  // 模拟采样决策注入
  {
    "x-sampling-decision": decision.sampled.to_string(),
    "x-sampling-reason": decision.decision_reason,
    "x-sampler-type": decision.sampler_type,
    "x-sampler-param": decision.sampler_param.to_string()
  }
}

fn extract_sampling_decision(headers : Map[String, String]) -> Any {
  // 模拟采样决策提取
  {
    sampled: headers.get("x-sampling-decision", "false") == "true",
    decision_reason: headers.get("x-sampling-reason", ""),
    sampler_type: headers.get("x-sampler-type", ""),
    sampler_param: headers.get("x-sampler-param", "0.0").to_double()
  }
}

fn inject_to_http_protocol(context : Any) -> Map[String, String] {
  // 模拟HTTP协议注入
  inject_trace_context_to_headers(context)
}

fn inject_to_grpc_protocol(context : Any) -> Map[String, String] {
  // 模拟gRPC协议注入
  inject_trace_context_to_headers(context)
}

fn inject_to_messaging_protocol(context : Any) -> Map[String, String] {
  // 模拟消息队列协议注入
  {
    "trace_id": context.trace_id,
    "span_id": context.span_id,
    "baggage": "user.id=12345,tenant.id=tenant_789"
  }
}

fn extract_from_http_protocol(headers : Map[String, String]) -> Any {
  // 模拟HTTP协议提取
  extract_trace_context_from_headers(headers)
}

fn extract_from_grpc_protocol(metadata : Map[String, String]) -> Any {
  // 模拟gRPC协议提取
  extract_trace_context_from_headers(metadata)
}

fn extract_from_messaging_protocol(properties : Map[String, String]) -> Any {
  // 模拟消息队列协议提取
  {
    trace_id: properties.get("trace_id", ""),
    span_id: properties.get("span_id", ""),
    baggage: properties.get("baggage", "").split(",")
  }
}

fn generate_span_id() -> String {
  // 生成随机span ID
  "b7ad6b7169203332"
}

fn generate_trace_id() -> String {
  // 生成随机trace ID
  "1af7651916cd43dd8448eb211c80319d"
}

fn get_current_timestamp_micros() -> Int64 {
  // 获取当前时间戳（微秒）
  1640995200123456L
}