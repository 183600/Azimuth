// 简化的跨服务遥测测试用例

@testable
test "cross_service_trace_propagation" {
  // 测试跨服务追踪传播
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let service_a_name = "user-service"
  let service_b_name = "order-service"
  let service_c_name = "payment-service"
  
  // 服务A创建初始span
  let service_a_span = {
    "trace_id": trace_id,
    "span_id": "a1b2c3d4e5f67788",
    "parent_span_id": "",
    "service_name": service_a_name,
    "operation_name": "get_user_profile",
    "start_time": "1640995200",
    "attributes": [
      ("http.method", "GET"),
      ("http.url", "/api/users/123")
    ]
  }
  
  // 服务A调用服务B，传播trace context
  let service_b_span = {
    "trace_id": trace_id,
    "span_id": "b1c2d3e4f5a67788",
    "parent_span_id": service_a_span["span_id"],
    "service_name": service_b_name,
    "operation_name": "create_order",
    "start_time": "1640995201",
    "attributes": [
      ("http.method", "POST"),
      ("http.url", "/api/orders")
    ]
  }
  
  // 服务B调用服务C，继续传播trace context
  let service_c_span = {
    "trace_id": trace_id,
    "span_id": "c1d2e3f4a5b67788",
    "parent_span_id": service_b_span["span_id"],
    "service_name": service_c_name,
    "operation_name": "process_payment",
    "start_time": "1640995202",
    "attributes": [
      ("http.method", "POST"),
      ("http.url", "/api/payments")
    ]
  }
  
  // 验证trace context传播
  assert_eq(service_a_span["trace_id"], trace_id)
  assert_eq(service_b_span["trace_id"], trace_id)
  assert_eq(service_c_span["trace_id"], trace_id)
  
  // 验证父子关系
  assert_eq(service_b_span["parent_span_id"], service_a_span["span_id"])
  assert_eq(service_c_span["parent_span_id"], service_b_span["span_id"])
  
  // 验证服务名称
  assert_eq(service_a_span["service_name"], service_a_name)
  assert_eq(service_b_span["service_name"], service_b_name)
  assert_eq(service_c_span["service_name"], service_c_name)
  
  // 创建完整的追踪链
  let trace_chain = [service_a_span, service_b_span, service_c_span]
  
  // 验证追踪链
  assert_eq(trace_chain.length(), 3)
  assert_eq(trace_chain[0]["service_name"], "user-service")
  assert_eq(trace_chain[1]["service_name"], "order-service")
  assert_eq(trace_chain[2]["service_name"], "payment-service")
}

@testable
test "cross_service_baggage_propagation" {
  // 测试跨服务baggage传播
  
  let initial_baggage = [
    ("user.id", "12345"),
    ("request.id", "req-abc123"),
    ("session.id", "sess-def456"),
    ("tenant.id", "tenant-789")
  ]
  
  // 服务A添加baggage
  let service_a_baggage = initial_baggage
  service_a_baggage.push(("service.a.timestamp", "1640995200"))
  
  // 服务B添加baggage
  let service_b_baggage = service_a_baggage
  service_b_baggage.push(("service.b.operation", "process_order"))
  
  // 服务C添加baggage
  let service_c_baggage = service_b_baggage
  service_c_baggage.push(("service.c.result", "success"))
  
  // 验证baggage传播
  assert_eq(service_a_baggage.length(), 5)
  assert_eq(service_b_baggage.length(), 6)
  assert_eq(service_c_baggage.length(), 7)
  
  // 验证初始baggage保留
  let mut found_user_id = false
  let mut found_request_id = false
  let mut i = 0
  while i < service_c_baggage.length() {
    if service_c_baggage[i].0 == "user.id" && service_c_baggage[i].1 == "12345" {
      found_user_id = true
    }
    if service_c_baggage[i].0 == "request.id" && service_c_baggage[i].1 == "req-abc123" {
      found_request_id = true
    }
    i = i + 1
  }
  
  assert_eq(found_user_id, true)
  assert_eq(found_request_id, true)
  
  // 验证各服务添加的baggage
  let mut found_service_a = false
  let mut found_service_b = false
  let mut found_service_c = false
  
  i = 0
  while i < service_c_baggage.length() {
    match service_c_baggage[i].0 {
      "service.a.timestamp" => {
        assert_eq(service_c_baggage[i].1, "1640995200")
        found_service_a = true
      }
      "service.b.operation" => {
        assert_eq(service_c_baggage[i].1, "process_order")
        found_service_b = true
      }
      "service.c.result" => {
        assert_eq(service_c_baggage[i].1, "success")
        found_service_c = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_service_a, true)
  assert_eq(found_service_b, true)
  assert_eq(found_service_c, true)
}

@testable
test "cross_service_metrics_correlation" {
  // 测试跨服务指标关联
  
  let services = ["gateway", "auth", "user", "order", "payment"]
  let request_id = "req-xyz789"
  
  // 各服务记录的指标
  let service_metrics = [
    ("gateway", [
      ("http.requests.total", "1"),
      ("http.request.duration_ms", "45"),
      ("upstream.requests", "3")
    ]),
    ("auth", [
      ("auth.requests.total", "1"),
      ("auth.duration_ms", "25"),
      ("auth.success", "1")
    ]),
    ("user", [
      ("user.queries.total", "1"),
      ("user.query.duration_ms", "120"),
      ("user.cache.hit", "1")
    ]),
    ("order", [
      ("order.creations.total", "1"),
      ("order.creation.duration_ms", "200"),
      ("order.validation.success", "1")
    ]),
    ("payment", [
      ("payment.attempts.total", "1"),
      ("payment.duration_ms", "350"),
      ("payment.success", "1")
    ])
  ]
  
  // 验证各服务指标数量
  let mut i = 0
  while i < service_metrics.length() {
    assert_eq(service_metrics[i].1.length(), 3)
    i = i + 1
  }
  
  // 计算总体指标
  let mut total_requests = 0
  let mut total_duration = 0
  let mut success_count = 0
  
  i = 0
  while i < service_metrics.length() {
    let (service_name, metrics) = service_metrics[i]
    let mut j = 0
    while j < metrics.length() {
      let (metric_name, metric_value) = metrics[j]
      
      if metric_name.has_suffix(".total") {
        total_requests = total_requests + metric_value.to_int()
      }
      if metric_name.has_suffix("duration_ms") {
        total_duration = total_duration + metric_value.to_int()
      }
      if metric_name.has_suffix("success") {
        success_count = success_count + metric_value.to_int()
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证总体指标
  assert_eq(total_requests, 5) // 每个服务1个请求
  assert_eq(total_duration, 740) // 45 + 25 + 120 + 200 + 350
  assert_eq(success_count, 4) // auth, user, order, payment success
  
  // 创建关联指标
  let correlated_metrics = [
    ("request.id", request_id),
    ("service.count", services.length().to_string()),
    ("total.duration.ms", total_duration.to_string()),
    ("success.services", success_count.to_string()),
    ("end.to.end.duration", total_duration.to_string())
  ]
  
  // 验证关联指标
  assert_eq(correlated_metrics.length(), 5)
  assert_eq(correlated_metrics[0].1, "req-xyz789")
  assert_eq(correlated_metrics[1].1, "5")
  assert_eq(correlated_metrics[2].1, "740")
  assert_eq(correlated_metrics[3].1, "4")
}

@testable
test "cross_service_context_serialization" {
  // 测试跨服务上下文序列化
  
  let trace_context = {
    "trace_id": "trace-ctx-abc123",
    "span_id": "span-ctx-def456",
    "baggage": [
      ("user.id", "98765"),
      ("session.id", "session-xyz"),
      ("tenant.id", "tenant-123")
    ],
    "trace_flags": "01",
    "trace_state": "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 序列化为HTTP头格式
  let traceparent_header = "00-" + trace_context["trace_id"] + "-" + trace_context["span_id"] + "-" + trace_context["trace_flags"]
  
  // 验证traceparent格式
  assert_eq(traceparent_header.has_prefix("00-"), true)
  assert_eq(traceparent_header.contains(trace_context["trace_id"]), true)
  assert_eq(traceparent_header.contains(trace_context["span_id"]), true)
  assert_eq(traceparent_header.has_suffix("-" + trace_context["trace_flags"]), true)
  
  // 序列化baggage头
  let mut baggage_header = ""
  let mut i = 0
  while i < trace_context["baggage"].length() {
    let (key, value) = trace_context["baggage"][i]
    baggage_header = baggage_header + key + "=" + value
    if i < trace_context["baggage"].length() - 1 {
      baggage_header = baggage_header + ","
    }
    i = i + 1
  }
  
  // 验证baggage格式
  assert_eq(baggage_header.contains("user.id=98765"), true)
  assert_eq(baggage_header.contains("session.id=session-xyz"), true)
  assert_eq(baggage_header.contains("tenant.id=tenant-123"), true)
  
  // 创建完整的上下文字符串
  let context_string = "{"
  context_string = context_string + "\"traceparent\":\"" + traceparent_header + "\"," 
  context_string = context_string + "\"baggage\":\"" + baggage_header + "\"," 
  context_string = context_string + "\"tracestate\":\"" + trace_context["trace_state"] + "\"" 
  context_string = context_string + "}"
  
  // 验证上下文字符串
  assert_eq(context_string.has_prefix("{"), true)
  assert_eq(context_string.has_suffix("}"), true)
  assert_eq(context_string.contains("\"traceparent\":\"00-"), true)
  assert_eq(context_string.contains("\"baggage\":\"user.id=98765"), true)
  assert_eq(context_string.contains("\"tracestate\":\"rojo=00f067aa0ba902b7"), true)
}