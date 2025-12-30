// 遥测跨服务传播测试用例

test "telemetry_context_propagation" {
  // 测试遥测上下文传播
  
  let parent_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let child_span_id = "00f067aa0ba902b7"
  
  // 验证父上下文
  assert_eq(parent_trace_id.length(), 32)
  assert_eq(parent_span_id.length(), 16)
  assert_eq(child_span_id.length(), 16)
  
  // 创建传播头
  let traceparent_header = "00-" + parent_trace_id + "-" + parent_span_id + "-01"
  
  // 验证traceparent格式
  assert_eq(traceparent_header.has_prefix("00-"), true)
  assert_eq(traceparent_header.contains(parent_trace_id), true)
  assert_eq(traceparent_header.contains(parent_span_id), true)
  assert_eq(traceparent_header.has_suffix("-01"), true)
  assert_eq(traceparent_header.length(), 55) // 2 + 1 + 32 + 1 + 16 + 1 + 2 = 55
  
  // 创建子上下文
  let child_context = parent_trace_id + ":" + child_span_id + ":" + parent_span_id
  
  // 验证子上下文
  assert_eq(child_context.contains(parent_trace_id), true)
  assert_eq(child_context.contains(child_span_id), true)
  assert_eq(child_context.contains(parent_span_id), true)
  assert_eq(child_context.length(), 65) // 32 + 1 + 16 + 1 + 16 = 65
}

test "telemetry_baggage_propagation" {
  // 测试遥测baggage传播
  
  let baggage_entries = [
    ("user.id", "12345"),
    ("tenant.id", "company-abc"),
    ("request.id", "req-67890"),
    ("session.id", "sess-11111")
  ]
  
  // 验证baggage条目
  assert_eq(baggage_entries.length(), 4)
  assert_eq(baggage_entries[0].0, "user.id")
  assert_eq(baggage_entries[0].1, "12345")
  assert_eq(baggage_entries[3].0, "session.id")
  assert_eq(baggage_entries[3].1, "sess-11111")
  
  // 创建baggage头
  let mut baggage_header = ""
  let mut i = 0
  while i < baggage_entries.length() {
    baggage_header = baggage_header + baggage_entries[i].0 + "=" + baggage_entries[i].1
    if i < baggage_entries.length() - 1 {
      baggage_header = baggage_header + ","
    }
    i = i + 1
  }
  
  // 验证baggage头格式
  assert_eq(baggage_header.contains("user.id=12345"), true)
  assert_eq(baggage_header.contains("tenant.id=company-abc"), true)
  assert_eq(baggage_header.contains("request.id=req-67890"), true)
  assert_eq(baggage_header.contains("session.id=sess-11111"), true)
  assert_eq(baggage_header.contains(","), true) // 应该包含逗号分隔符
  
  // 模拟baggage解析
  let parsed_entries = []
  let header_parts = baggage_header.split(",")
  
  i = 0
  while i < header_parts.length() {
    let part = header_parts[i]
    let key_value = part.split("=")
    if key_value.length() == 2 {
      parsed_entries.push((key_value[0], key_value[1]))
    }
    i = i + 1
  }
  
  // 验证解析结果
  assert_eq(parsed_entries.length(), 4)
  assert_eq(parsed_entries[0].0, "user.id")
  assert_eq(parsed_entries[0].1, "12345")
}

test "telemetry_cross_service_injection" {
  // 测试跨服务注入
  
  let source_service = "auth-service"
  let target_service = "user-service"
  let operation_name = "validate_user"
  
  // 创建服务间调用上下文
  let trace_id = "a1b2c3d4e5f6789012345678901234ab"
  let source_span_id = "1111111111111111"
  let target_span_id = "2222222222222222"
  
  // 验证服务信息
  assert_eq(source_service.has_suffix("-service"), true)
  assert_eq(target_service.has_suffix("-service"), true)
  assert_eq(operation_name.has_prefix("validate"), true)
  
  // 创建注入头
  let injection_headers = [
    ("traceparent", "00-" + trace_id + "-" + source_span_id + "-01"),
    ("x-service-name", source_service),
    ("x-operation-name", operation_name),
    ("x-target-service", target_service)
  ]
  
  // 验证注入头
  assert_eq(injection_headers.length(), 4)
  assert_eq(injection_headers[0].0, "traceparent")
  assert_eq(injection_headers[0].1.contains(trace_id), true)
  assert_eq(injection_headers[1].0, "x-service-name")
  assert_eq(injection_headers[1].1, source_service)
  
  // 创建跨服务调用记录
  let call_record = source_service + "->" + target_service + ":" + operation_name + 
                   "[" + trace_id + ":" + source_span_id + "->" + target_span_id + "]"
  
  // 验证调用记录
  assert_eq(call_record.has_prefix("auth-service->user-service"), true)
  assert_eq(call_record.contains(":validate_user"), true)
  assert_eq(call_record.contains(trace_id), true)
  assert_eq(call_record.contains("1111111111111111"), true)
  assert_eq(call_record.contains("2222222222222222"), true)
}