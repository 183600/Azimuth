// 跨服务上下文传播测试用例

test "cross_service_context_propagation_with_trace_context" {
  // 测试跨服务的链路追踪上下文传播
  
  // 创建原始Trace上下文
  let original_trace_id = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10]
  let original_span_id = [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]
  
  let original_context = azimuth::telemetry::api::trace::SpanContext::{
    trace_id: original_trace_id,
    span_id: original_span_id,
    trace_flags: 0x01,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 模拟HTTP头部传播
  let traceparent_header = "00-" + 
    original_trace_id.map(fn(b) { b.to_hex_string() }).join("") + "-" +
    original_span_id.map(fn(b) { b.to_hex_string() }).join("") + "-01"
  
  let tracestate_header = "key1=value1,key2=value2"
  
  // 验证头部格式
  assert_eq(traceparent_header.has_prefix("00-"), true)
  assert_eq(traceparent_header.has_suffix("-01"), true)
  assert_eq(tracestate_header, "key1=value1,key2=value2")
  
  // 模拟服务B从头部提取上下文
  let extracted_trace_id = traceparent_header.split("-")[1].to_byte_array()
  let extracted_span_id = traceparent_header.split("-")[2].to_byte_array()
  let extracted_trace_flags = traceparent_header.split("-")[3].to_byte()
  
  let extracted_context = azimuth::telemetry::api::trace::SpanContext::{
    trace_id: extracted_trace_id,
    span_id: extracted_span_id,
    trace_flags: extracted_trace_flags,
    trace_state: tracestate_header
  }
  
  // 验证提取的上下文与原始上下文匹配
  assert_eq(extracted_context.trace_id, original_context.trace_id)
  assert_eq(extracted_context.span_id, original_context.span_id)
  assert_eq(extracted_context.trace_flags, original_context.trace_flags)
  assert_eq(extracted_context.trace_state, original_context.trace_state)
}

test "cross_service_context_propagation_with_baggage_items" {
  // 测试跨服务的行李项（baggage）传播
  
  // 创建原始行李项
  let original_baggage = [
    ("user.id", "user123"),
    ("request.id", "req-456"),
    ("service.version", "1.2.3"),
    ("region", "us-west-2")
  ]
  
  // 将行李项转换为HTTP头部格式
  let baggage_header = original_baggage.map(fn(item) { 
    item.0 + "=" + item.1 
  }).join(",")
  
  // 验证行李头部格式
  assert_eq(baggage_header.contains("user.id=user123"), true)
  assert_eq(baggage_header.contains("request.id=req-456"), true)
  assert_eq(baggage_header.contains("service.version=1.2.3"), true)
  assert_eq(baggage_header.contains("region=us-west-2"), true)
  
  // 模拟服务B从头部提取行李项
  let extracted_baggage = baggage_header.split(",").map(fn(item) {
    let parts = item.split("=")
    (parts[0], parts[1])
  })
  
  // 验证提取的行李项
  assert_eq(extracted_baggage.length(), 4)
  
  let baggage_map = extracted_baggage.to_map()
  assert_eq(baggage_map.get("user.id"), Some("user123"))
  assert_eq(baggage_map.get("request.id"), Some("req-456"))
  assert_eq(baggage_map.get("service.version"), Some("1.2.3"))
  assert_eq(baggage_map.get("region"), Some("us-west-2"))
}

test "cross_service_context_propagation_with_correlation_ids" {
  // 测试跨服务的关联ID传播
  
  // 创建关联ID
  let correlation_id = "corr-789-abc-def"
  let conversation_id = "conv-123-456-789"
  let causal_chain_id = "chain-aaa-bbb-ccc"
  
  // 将关联ID添加到上下文属性
  let correlation_attributes = [
    ("correlation.id", azimuth::telemetry::api::common::AttributeValue::string(correlation_id)),
    ("conversation.id", azimuth::telemetry::api::common::AttributeValue::string(conversation_id)),
    ("causal.chain.id", azimuth::telemetry::api::common::AttributeValue::string(causal_chain_id))
  ]
  
  // 模拟服务A创建Span并添加关联属性
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xAA })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xBB })
  
  let span_context = azimuth::telemetry::api::trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0x01,
    trace_state: ""
  }
  
  let span = azimuth::telemetry::api::trace::Span::{
    name: "service-a-operation",
    context: span_context,
    kind: azimuth::telemetry::api::trace::SpanKind::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: azimuth::telemetry::api::trace::StatusCode::Ok,
    status_description: None,
    attributes: correlation_attributes,
    events: [],
    links: []
  }
  
  // 验证Span包含关联属性
  assert_eq(span.attributes.length(), 3)
  
  let attr_map = span.attributes.to_map()
  match attr_map.get("correlation.id") {
    Some(azimuth::telemetry::api::common::AttributeValue::StringValue(v)) => assert_eq(v, correlation_id)
    _ => assert_eq(false, true, "Expected correlation.id string value")
  }
  
  match attr_map.get("conversation.id") {
    Some(azimuth::telemetry::api::common::AttributeValue::StringValue(v)) => assert_eq(v, conversation_id)
    _ => assert_eq(false, true, "Expected conversation.id string value")
  }
  
  match attr_map.get("causal.chain.id") {
    Some(azimuth::telemetry::api::common::AttributeValue::StringValue(v)) => assert_eq(v, causal_chain_id)
    _ => assert_eq(false, true, "Expected causal.chain.id string value")
  }
}

test "cross_service_context_propagation_with_sampling_decisions" {
  // 测试跨服务的采样决策传播
  
  // 创建带有采样决策的上下文
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(i) { (i + 1).to_byte() })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(i) { (i + 1).to_byte() })
  
  // 测试不同的采样决策
  let sampling_decisions = [
    (0x00, "not-sampled"),    // 不采样
    (0x01, "sampled"),        // 采样
    (0x02, "debug-sampled"),  // 调试采样
    (0x04, "extended-sampled") // 扩展采样
  ]
  
  for decision in sampling_decisions {
    let context = azimuth::telemetry::api::trace::SpanContext::{
      trace_id: trace_id,
      span_id: span_id,
      trace_flags: decision.0,
      trace_state: "sampling.decision=" + decision.1
    }
    
    // 模拟传播采样决策
    let sampling_header = decision.1
    let trace_flags_header = decision.0.to_hex_string()
    
    // 验证采样决策传播
    assert_eq(sampling_header, decision.1)
    assert_eq(trace_flags_header.length(), 2)  // 单字节十六进制表示
    
    // 验证从头部恢复采样决策
    let restored_trace_flags = trace_flags_header.from_hex_string().to_byte()
    let restored_sampling_decision = "sampling.decision=" + decision.1
    
    assert_eq(restored_trace_flags, decision.0)
    assert_eq(restored_sampling_decision, context.trace_state)
  }
}

test "cross_service_context_propagation_with_service_mesh_integration" {
  // 测试服务网格集成的上下文传播
  
  // 创建服务网格特定的上下文信息
  let mesh_headers = [
    ("x-request-id", "req-123-456-789"),
    ("x-b3-traceid", "80f198ee56343ba864fe8b2a57d3eff7"),
    ("x-b3-spanid", "e457b5a2e4d86bd1"),
    ("x-b3-parentspanid", "05e3ac9a4f6e3b90"),
    ("x-b3-sampled", "1"),
    ("x-ot-span-context", "80f198ee56343ba864fe8b2a57d3eff7;e457b5a2e4d86bd1;1")
  ]
  
  // 模拟服务网格环境下的上下文传播
  let mesh_context = mesh_headers.to_map()
  
  // 验证服务网格头部
  assert_eq(mesh_context.get("x-request-id"), Some("req-123-456-789"))
  assert_eq(mesh_context.get("x-b3-traceid"), Some("80f198ee56343ba864fe8b2a57d3eff7"))
  assert_eq(mesh_context.get("x-b3-spanid"), Some("e457b5a2e4d86bd1"))
  assert_eq(mesh_context.get("x-b3-parentspanid"), Some("05e3ac9a4f6e3b90"))
  assert_eq(mesh_context.get("x-b3-sampled"), Some("1"))
  
  // 从B3头部创建Span上下文
  let b3_trace_id = mesh_context.get("x-b3-traceid").unwrap().from_hex_string()
  let b3_span_id = mesh_context.get("x-b3-spanid").unwrap().from_hex_string()
  let b3_sampled = mesh_context.get("x-b3-sampled").unwrap()
  
  // 验证B3追踪ID格式（16字节）
  assert_eq(b3_trace_id.length(), 16)
  
  // 验证B3 Span ID格式（8字节）
  assert_eq(b3_span_id.length(), 8)
  
  // 验证采样决策
  assert_eq(b3_sampled, "1")
  
  // 创建服务网格感知的Span上下文
  let mesh_aware_context = azimuth::telemetry::api::trace::SpanContext::{
    trace_id: b3_trace_id,
    span_id: b3_span_id,
    trace_flags: if b3_sampled == "1" { 0x01 } else { 0x00 },
    trace_state: "mesh.origin=istio,mesh.version=1.12.0"
  }
  
  // 验证服务网格感知上下文
  assert_eq(mesh_aware_context.trace_id, b3_trace_id)
  assert_eq(mesh_aware_context.span_id, b3_span_id)
  assert_eq(mesh_aware_context.trace_flags, 0x01)
  assert_eq(mesh_aware_context.trace_state, "mesh.origin=istio,mesh.version=1.12.0")
}