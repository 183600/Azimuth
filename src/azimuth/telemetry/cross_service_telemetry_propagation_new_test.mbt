// 跨服务遥测传播测试用例
// 测试遥测上下文在不同服务间的传播机制

test "trace_context_http_headers_propagation" {
  // 测试通过HTTP头传播追踪上下文
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let sampled_flag = "01"
  
  // 创建W3C traceparent头
  let traceparent_header = "00-" + trace_id + "-" + span_id + "-" + sampled_flag
  
  // 验证traceparent格式
  assert_eq(traceparent_header.has_prefix("00-"), true)
  assert_eq(traceparent_header.contains("-" + trace_id + "-"), true)
  assert_eq(traceparent_header.contains("-" + span_id + "-"), true)
  assert_eq(traceparent_header.has_suffix("-" + sampled_flag), true)
  
  // 解析traceparent头
  let parts = traceparent_header.split("-")
  assert_eq(parts.length(), 4)
  assert_eq(parts[0], "00")  // version
  assert_eq(parts[1], trace_id)
  assert_eq(parts[2], span_id)
  assert_eq(parts[3], sampled_flag)
  
  // 创建tracestate头
  let tracestate_header = "vendor1=value1,vendor2=value2"
  
  // 验证tracestate格式
  assert_eq(tracestate_header.contains("vendor1=value1"), true)
  assert_eq(tracestate_header.contains("vendor2=value2"), true)
  assert_eq(tracestate_header.contains(","), true)
}

test "trace_context_bagger_format_propagation" {
  // 测试Bagger格式传播
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let baggage_entries = [
    ("user-id", "12345"),
    ("request-id", "req-67890"),
    ("region", "us-west-2")
  ]
  
  // 创建baggage头
  let mut baggage_header = ""
  let mut i = 0
  while i < baggage_entries.length() {
    let (key, value) = baggage_entries[i]
    if i > 0 {
      baggage_header = baggage_header + ","
    }
    baggage_header = baggage_header + key + "=" + value
    i = i + 1
  }
  
  // 验证baggage头
  assert_eq(baggage_header.contains("user-id=12345"), true)
  assert_eq(baggage_header.contains("request-id=req-67890"), true)
  assert_eq(baggage_header.contains("region=us-west-2"), true)
  
  // 解析baggage头
  let entries = baggage_header.split(",")
  assert_eq(entries.length(), baggage_entries.length())
  
  // 验证每个条目
  i = 0
  while i < entries.length() {
    let entry = entries[i]
    let kv_pair = entry.split("=")
    assert_eq(kv_pair.length(), 2)
    
    let (expected_key, expected_value) = baggage_entries[i]
    assert_eq(kv_pair[0], expected_key)
    assert_eq(kv_pair[1], expected_value)
    
    i = i + 1
  }
}

test "cross_service_context_injection" {
  // 测试跨服务上下文注入
  
  let source_service = "auth-service"
  let target_service = "payment-service"
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  
  // 源服务创建上下文
  let source_context = {
    "trace_id": trace_id,
    "span_id": parent_span_id,
    "service_name": source_service,
    "operation": "authenticate"
  }
  
  // 注入传播头
  let propagation_headers = {
    "traceparent": "00-" + trace_id + "-" + parent_span_id + "-01",
    "x-service-name": source_service,
    "x-operation-name": "authenticate"
  }
  
  // 验证传播头
  assert_eq(propagation_headers["traceparent"].contains(trace_id), true)
  assert_eq(propagation_headers["traceparent"].contains(parent_span_id), true)
  assert_eq(propagation_headers["x-service-name"], source_service)
  assert_eq(propagation_headers["x-operation-name"], "authenticate")
  
  // 目标服务提取上下文
  let extracted_trace_id = propagation_headers["traceparent"].split("-")[1]
  let extracted_parent_span_id = propagation_headers["traceparent"].split("-")[2]
  let extracted_service_name = propagation_headers["x-service-name"]
  
  // 验证提取的上下文
  assert_eq(extracted_trace_id, trace_id)
  assert_eq(extracted_parent_span_id, parent_span_id)
  assert_eq(extracted_service_name, source_service)
  
  // 目标服务创建子span
  let child_span_id = "c7ad6b7169203331"
  let target_context = {
    "trace_id": extracted_trace_id,
    "span_id": child_span_id,
    "parent_span_id": extracted_parent_span_id,
    "service_name": target_service,
    "operation": "process_payment"
  }
  
  // 验证子span上下文
  assert_eq(target_context["trace_id"], trace_id)
  assert_eq(target_context["parent_span_id"], parent_span_id)
  assert_eq(target_context["service_name"], target_service)
  assert_eq(target_context["operation"], "process_payment")
}

test "cross_service_sampling_decision_propagation" {
  // 测试跨服务采样决策传播
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_sampled = true
  let sampling_decision = "01"  // 01表示已采样
  
  // 创建包含采样决策的traceparent
  let traceparent_sampled = "00-" + trace_id + "-b7ad6b7169203331-" + sampling_decision
  let traceparent_not_sampled = "00-" + trace_id + "-b7ad6b7169203331-00"
  
  // 解析采样决策
  let sampled_parts = traceparent_sampled.split("-")
  let not_sampled_parts = traceparent_not_sampled.split("-")
  
  let extracted_sampled_decision = sampled_parts[3]
  let extracted_not_sampled_decision = not_sampled_parts[3]
  
  // 验证采样决策传播
  assert_eq(extracted_sampled_decision, "01")
  assert_eq(extracted_not_sampled_decision, "00")
  
  // 子服务应该遵循父服务的采样决策
  let child_should_sample = extracted_sampled_decision == "01"
  let child_should_not_sample = extracted_not_sampled_decision == "01"
  
  assert_eq(child_should_sample, true)
  assert_eq(child_should_not_sample, false)
}

test "cross_service_error_propagation" {
  // 测试跨服务错误传播
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let error_code = "PAYMENT_DECLINED"
  let error_message = "Insufficient funds"
  let error_stack = "PaymentService.processPayment:45"
  
  // 创建错误上下文
  let error_context = {
    "trace_id": trace_id,
    "error_code": error_code,
    "error_message": error_message,
    "error_stack": error_stack,
    "service": "payment-service"
  }
  
  // 创建错误传播头
  let error_headers = {
    "x-error-code": error_code,
    "x-error-message": error_message,
    "x-error-stack": error_stack,
    "x-trace-id": trace_id
  }
  
  // 验证错误传播头
  assert_eq(error_headers["x-error-code"], error_code)
  assert_eq(error_headers["x-error-message"], error_message)
  assert_eq(error_headers["x-error-stack"], error_stack)
  assert_eq(error_headers["x-trace-id"], trace_id)
  
  // 下游服务提取错误信息
  let propagated_error_code = error_headers["x-error-code"]
  let propagated_error_message = error_headers["x-error-message"]
  let propagated_trace_id = error_headers["x-trace-id"]
  
  // 验证错误信息传播
  assert_eq(propagated_error_code, error_code)
  assert_eq(propagated_error_message, error_message)
  assert_eq(propagated_trace_id, trace_id)
  
  // 下游服务可以基于传播的错误信息进行相应处理
  let is_payment_error = propagated_error_code.has_prefix("PAYMENT_")
  let should_retry = propagated_error_code != "ACCOUNT_LOCKED"
  
  assert_eq(is_payment_error, true)
  assert_eq(should_retry, true)
}