// 上下文传播测试用例
// 测试Azimuth Telemetry上下文传播功能

test "trace_context_injection_and_extraction" {
  // 测试追踪上下文注入和提取
  
  let original_context = {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    trace_flags: "01",
    trace_state: "vendor1=opaqueValue1,vendor2=opaqueValue2",
    baggage: [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("session.id", "sess-abc123")
    ]
  }
  
  // 注入traceparent header
  let traceparent = "00-" + original_context.trace_id + "-" + original_context.span_id + "-" + original_context.trace_flags
  assert_eq(traceparent.length(), 55)
  assert_eq(traceparent.has_prefix("00-"), true)
  assert_eq(traceparent.contains(original_context.trace_id), true)
  assert_eq(traceparent.contains(original_context.span_id), true)
  assert_eq(traceparent.has_suffix("-" + original_context.trace_flags), true)
  
  // 注入tracestate header
  let tracestate = original_context.trace_state
  assert_eq(tracestate.contains("vendor1"), true)
  assert_eq(tracestate.contains("vendor2"), true)
  
  // 注入baggage header
  let baggage_header = ""
  let mut i = 0
  while i < original_context.baggage.length() {
    let (key, value) = original_context.baggage[i]
    if i > 0 {
      baggage_header = baggage_header + ","
    }
    baggage_header = baggage_header + key + "=" + value
    i = i + 1
  }
  
  assert_eq(baggage_header.contains("user.id=12345"), true)
  assert_eq(baggage_header.contains("request.id=req-67890"), true)
  assert_eq(baggage_header.contains("session.id=sess-abc123"), true)
  
  // 提取上下文
  let extracted_context = {
    trace_id: traceparent.split("-")[1],
    span_id: traceparent.split("-")[2],
    trace_flags: traceparent.split("-")[3],
    trace_state: tracestate,
    baggage: baggage_header.split(",")
  }
  
  // 验证提取的上下文
  assert_eq(extracted_context.trace_id, original_context.trace_id)
  assert_eq(extracted_context.span_id, original_context.span_id)
  assert_eq(extracted_context.trace_flags, original_context.trace_flags)
  assert_eq(extracted_context.trace_state, original_context.trace_state)
  assert_eq(extracted_context.baggage.length(), original_context.baggage.length())
}

test "cross_service_context_propagation" {
  // 测试跨服务上下文传播
  
  // 服务A创建初始上下文
  let service_a_context = {
    trace_id: "a1b2c3d4e5f678901234567890123456",
    span_id: "1111111111111111",
    service_name: "auth-service",
    operation_name: "authenticate_user"
  }
  
  // 服务A传播到服务B
  let service_a_to_b_headers = {
    traceparent: "00-" + service_a_context.trace_id + "-" + service_a_context.span_id + "-01",
    tracestate: "auth.service=auth-service,v1=true",
    baggage: "user.id=12345,auth.token=abc123,request.id=req-001"
  }
  
  // 服务B接收并创建子span
  let service_b_context = {
    trace_id: service_a_to_b_headers.traceparent.split("-")[1],
    parent_span_id: service_a_to_b_headers.traceparent.split("-")[2],
    span_id: "2222222222222222",
    service_name: "user-service",
    operation_name: "get_user_profile"
  }
  
  // 验证服务B上下文
  assert_eq(service_b_context.trace_id, service_a_context.trace_id)
  assert_eq(service_b_context.parent_span_id, service_a_context.span_id)
  assert_eq(service_b_context.span_id != service_a_context.span_id, true)
  
  // 服务B传播到服务C
  let service_b_to_c_headers = {
    traceparent: "00-" + service_b_context.trace_id + "-" + service_b_context.span_id + "-01",
    tracestate: service_a_to_b_headers.tracestate + ",user.service=user-service",
    baggage: service_a_to_b_headers.baggage + ",profile.cache=miss"
  }
  
  // 服务C接收并创建子span
  let service_c_context = {
    trace_id: service_b_to_c_headers.traceparent.split("-")[1],
    parent_span_id: service_b_to_c_headers.traceparent.split("-")[2],
    span_id: "3333333333333333",
    service_name: "profile-service",
    operation_name: "enrich_profile_data"
  }
  
  // 验证服务C上下文
  assert_eq(service_c_context.trace_id, service_a_context.trace_id)
  assert_eq(service_c_context.parent_span_id, service_b_context.span_id)
  assert_eq(service_c_context.span_id != service_b_context.span_id, true)
  
  // 验证tracestate传播
  assert_eq(service_b_to_c_headers.tracestate.contains("auth.service"), true)
  assert_eq(service_b_to_c_headers.tracestate.contains("user.service"), true)
  
  // 验证baggage传播
  assert_eq(service_b_to_c_headers.baggage.contains("user.id"), true)
  assert_eq(service_b_to_c_headers.baggage.contains("profile.cache"), true)
}

test "async_context_propagation" {
  // 测试异步上下文传播
  
  let main_thread_context = {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "aaaaaaaaaaaaaaaa",
    correlation_id: "corr-async-001",
    user_id: "user-async-123",
    request_id: "req-async-456"
  }
  
  // 异步任务1
  let async_task_1_context = {
    trace_id: main_thread_context.trace_id,
    parent_span_id: main_thread_context.span_id,
    span_id: "bbbbbbbbbbbbbbbb",
    task_name: "process_payment",
    async_id: "async-task-001"
  }
  
  // 异步任务2
  let async_task_2_context = {
    trace_id: main_thread_context.trace_id,
    parent_span_id: main_thread_context.span_id,
    span_id: "cccccccccccccccc",
    task_name: "send_notification",
    async_id: "async-task-002"
  }
  
  // 异步任务3（嵌套）
  let async_task_3_context = {
    trace_id: main_thread_context.trace_id,
    parent_span_id: async_task_1_context.span_id,
    span_id: "dddddddddddddddd",
    task_name: "update_inventory",
    async_id: "async-task-003"
  }
  
  // 验证异步任务上下文
  assert_eq(async_task_1_context.trace_id, main_thread_context.trace_id)
  assert_eq(async_task_1_context.parent_span_id, main_thread_context.span_id)
  assert_eq(async_task_2_context.trace_id, main_thread_context.trace_id)
  assert_eq(async_task_2_context.parent_span_id, main_thread_context.span_id)
  assert_eq(async_task_3_context.trace_id, main_thread_context.trace_id)
  assert_eq(async_task_3_context.parent_span_id, async_task_1_context.span_id)
  
  // 验证span ID唯一性
  let span_ids = [
    main_thread_context.span_id,
    async_task_1_context.span_id,
    async_task_2_context.span_id,
    async_task_3_context.span_id
  ]
  
  let mut i = 0
  while i < span_ids.length() {
    let mut j = i + 1
    while j < span_ids.length() {
      assert_eq(span_ids[i] != span_ids[j], true)
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证异步任务ID唯一性
  let async_ids = [
    async_task_1_context.async_id,
    async_task_2_context.async_id,
    async_task_3_context.async_id
  ]
  
  i = 0
  while i < async_ids.length() {
    let mut j = i + 1
    while j < async_ids.length() {
      assert_eq(async_ids[i] != async_ids[j], true)
      j = j + 1
    }
    i = i + 1
  }
}

test "context_isolation_and_boundaries" {
  // 测试上下文隔离和边界
  
  // 请求1的上下文
  let request_1_context = {
    request_id: "req-001",
    trace_id: "trace-11111111111111111111111111111111",
    span_id: "span-1111111111111111",
    user_id: "user-001",
    session_id: "sess-001",
    tenant_id: "tenant-001"
  }
  
  // 请求2的上下文（并发）
  let request_2_context = {
    request_id: "req-002",
    trace_id: "trace-22222222222222222222222222222222",
    span_id: "span-2222222222222222",
    user_id: "user-002",
    session_id: "sess-002",
    tenant_id: "tenant-001"
  }
  
  // 请求3的上下文（并发）
  let request_3_context = {
    request_id: "req-003",
    trace_id: "trace-33333333333333333333333333333333",
    span_id: "span-3333333333333333",
    user_id: "user-003",
    session_id: "sess-003",
    tenant_id: "tenant-002"
  }
  
  // 验证上下文隔离
  assert_eq(request_1_context.trace_id != request_2_context.trace_id, true)
  assert_eq(request_1_context.trace_id != request_3_context.trace_id, true)
  assert_eq(request_2_context.trace_id != request_3_context.trace_id, true)
  
  assert_eq(request_1_context.span_id != request_2_context.span_id, true)
  assert_eq(request_1_context.span_id != request_3_context.span_id, true)
  assert_eq(request_2_context.span_id != request_3_context.span_id, true)
  
  assert_eq(request_1_context.user_id != request_2_context.user_id, true)
  assert_eq(request_1_context.user_id != request_3_context.user_id, true)
  assert_eq(request_2_context.user_id != request_3_context.user_id, true)
  
  // 验证租户边界
  assert_eq(request_1_context.tenant_id, request_2_context.tenant_id) // 同一租户
  assert_eq(request_1_context.tenant_id != request_3_context.tenant_id, true) // 不同租户
  
  // 创建上下文映射
  let context_map = {
    "req-001": request_1_context,
    "req-002": request_2_context,
    "req-003": request_3_context
  }
  
  // 验证上下文检索
  let retrieved_context_1 = context_map["req-001"]
  let retrieved_context_2 = context_map["req-002"]
  let retrieved_context_3 = context_map["req-003"]
  
  assert_eq(retrieved_context_1.request_id, "req-001")
  assert_eq(retrieved_context_2.request_id, "req-002")
  assert_eq(retrieved_context_3.request_id, "req-003")
  
  assert_eq(retrieved_context_1.trace_id, request_1_context.trace_id)
  assert_eq(retrieved_context_2.trace_id, request_2_context.trace_id)
  assert_eq(retrieved_context_3.trace_id, request_3_context.trace_id)
}

test "context_serialization_formats" {
  // 测试上下文序列化格式
  
  let context = {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    trace_flags: "01",
    trace_state: "vendor1=value1,vendor2=value2",
    baggage: [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("session.id", "sess-abc123")
    ]
  }
  
  // W3C Trace Context格式
  let w3c_traceparent = "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags
  let w3c_tracestate = context.trace_state
  
  assert_eq(w3c_traceparent.length(), 55)
  assert_eq(w3c_traceparent.has_prefix("00-"), true)
  assert_eq(w3c_tracestate.contains("vendor1"), true)
  
  // B3格式（单header）
  let b3_single = context.trace_id + "-" + context.span_id + "-" + "1" + "-" + context.trace_state
  assert_eq(b3_single.contains(context.trace_id), true)
  assert_eq(b3_single.contains(context.span_id), true)
  assert_eq(b3_single.contains(context.trace_state), true)
  
  // B3格式（多header）
  let b3_trace_id = context.trace_id
  let b3_span_id = context.span_id
  let b3_parent_span_id = "00f067aa0ba902b6"
  let b3_sampled = "1"
  let b3_flags = "1"
  
  assert_eq(b3_trace_id.length(), 32)
  assert_eq(b3_span_id.length(), 16)
  assert_eq(b3_sampled, "1")
  assert_eq(b3_flags, "1")
  
  // Baggage格式
  let baggage_header = ""
  let mut i = 0
  while i < context.baggage.length() {
    let (key, value) = context.baggage[i]
    if i > 0 {
      baggage_header = baggage_header + ","
    }
    baggage_header = baggage_header + key + "=" + value
    i = i + 1
  }
  
  // 验证baggage格式
  assert_eq(baggage_header.contains("user.id=12345"), true)
  assert_eq(baggage_header.contains("request.id=req-67890"), true)
  assert_eq(baggage_header.contains("session.id=sess-abc123"), true)
  
  // 验证URL编码（特殊字符）
  let special_baggage = "user.name=John%20Doe,search.query=foo%20bar%2Bbaz"
  assert_eq(special_baggage.contains("John%20Doe"), true)
  assert_eq(special_baggage.contains("foo%20bar%2Bbaz"), true)
  
  // 验证propagator组合
  let composite_headers = {
    "traceparent": w3c_traceparent,
    "tracestate": w3c_tracestate,
    "baggage": baggage_header,
    "x-b3-traceid": b3_trace_id,
    "x-b3-spanid": b3_span_id,
    "x-b3-parentspanid": b3_parent_span_id,
    "x-b3-sampled": b3_sampled,
    "x-b3-flags": b3_flags
  }
  
  assert_eq(composite_headers.length(), 8)
  assert_eq(composite_headers["traceparent"].length(), 55)
  assert_eq(composite_headers["x-b3-traceid"], context.trace_id)
  assert_eq(composite_headers["x-b3-spanid"], context.span_id)
}

test "context_propagation_error_handling" {
  // 测试上下文传播错误处理
  
  // 测试无效traceparent格式
  let invalid_trace_parents = [
    "invalid-format",
    "00-invalidtraceid-1234567890abcdef-01",
    "00-4bf92f3577b34da6a3ce929d0e0e4736-invalidspanid-01",
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-invalidflags",
    "01-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01", // 无效版本
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-03"  // 无效标志
  ]
  
  let mut i = 0
  while i < invalid_trace_parents.length() {
    let invalid = invalid_trace_parents[i]
    
    // 验证无效格式检测
    if invalid.contains("invalid-format") {
      assert_eq(invalid.has_prefix("00-"), false)
    } else if invalid.contains("invalidtraceid") {
      let parts = invalid.split("-")
      assert_eq(parts[1].length(), 32, false) // 应该失败
    } else if invalid.contains("invalidspanid") {
      let parts = invalid.split("-")
      assert_eq(parts[2].length(), 16, false) // 应该失败
    } else if invalid.contains("invalidflags") {
      let parts = invalid.split("-")
      assert_eq(parts[3].length(), 2, false) // 应该失败
    }
    
    i = i + 1
  }
  
  // 测试无效baggage格式
  let invalid_baggages = [
    "key_without_value",
    "=value_without_key",
    "key=value=with=extra=equals",
    "key=value,invalid_entry",
    "key=value,=no_key",
    "key=value,key_without_value"
  ]
  
  i = 0
  while i < invalid_baggages.length() {
    let invalid = invalid_baggages[i]
    
    if invalid.contains("key_without_value") {
      assert_eq(invalid.contains("="), false)
    } else if invalid.contains("=value_without_key") {
      assert_eq(invalid.has_prefix("="), true)
    } else if invalid.contains("key=value=with=extra=equals") {
      let parts = invalid.split("=")
      assert_eq(parts.length(), 5) // key, value, with, extra, equals
    }
    
    i = i + 1
  }
  
  // 测试截断处理
  let long_trace_id = "1234567890abcdef1234567890abcdef" // 正确长度
  let truncated_trace_id = "1234567890abcdef1234567890abcde" // 截断的
  let long_span_id = "1234567890abcdef" // 正确长度
  let truncated_span_id = "1234567890abcd" // 截断的
  
  assert_eq(long_trace_id.length(), 32)
  assert_eq(truncated_trace_id.length(), 31)
  assert_eq(long_span_id.length(), 16)
  assert_eq(truncated_span_id.length(), 15)
  
  // 测试上下文恢复
  let fallback_context = {
    trace_id: "00000000000000000000000000000000",
    span_id: "0000000000000000",
    trace_flags: "00",
    trace_state: "",
    baggage: []
  }
  
  assert_eq(fallback_context.trace_id, "00000000000000000000000000000000")
  assert_eq(fallback_context.span_id, "0000000000000000")
  assert_eq(fallback_context.trace_flags, "00")
  assert_eq(fallback_context.trace_state.length(), 0)
  assert_eq(fallback_context.baggage.length(), 0)
}