// 跨服务遥测传播测试用例
// 测试遥测系统在不同服务之间的传播功能

test "追踪上下文传播" {
  // 测试追踪ID生成
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  assert_eq(trace_id.length(), 32)
  
  // 测试父跨度ID
  let parent_span_id = "00f067aa0ba902b7"
  assert_eq(parent_span_id.length(), 16)
  
  // 测试子跨度生成
  let child_span_id = "b7902baa0af76519"
  assert_eq(child_span_id.length(), 16)
  assert_eq(child_span_id != parent_span_id, true)
  
  // 测试追踪上下文提取
  let trace_headers = {
    "X-Trace-Id": trace_id,
    "X-Parent-Span-Id": parent_span_id,
    "X-Span-Id": child_span_id
  }
  
  assert_eq(trace_headers["X-Trace-Id"], trace_id)
  assert_eq(trace_headers["X-Parent-Span-Id"], parent_span_id)
  assert_eq(trace_headers["X-Span-Id"], child_span_id)
}

test " baggage传播" {
  // 测试baggage项设置
  let baggage_items = {
    "user_id": "12345",
    "session_id": "abcdef123456",
    "request_id": "req-789",
    "tenant_id": "tenant-001"
  }
  
  // 测试baggage序列化为HTTP头
  let baggage_header = "user_id=12345,session_id=abcdef123456,request_id=req-789,tenant_id=tenant-001"
  
  // 验证baggage头包含所有项
  assert_eq(baggage_header.contains("user_id=12345"), true)
  assert_eq(baggage_header.contains("session_id=abcdef123456"), true)
  assert_eq(baggage_header.contains("request_id=req-789"), true)
  assert_eq(baggage_header.contains("tenant_id=tenant-001"), true)
  
  // 测试baggage解析
  let header_parts = baggage_header.split(",")
  assert_eq(header_parts.length(), 4)
  
  for part in header_parts {
    let key_value = part.split("=")
    assert_eq(key_value.length(), 2)
    let key = key_value[0]
    let value = key_value[1]
    assert_eq(baggage_items.contains(key), true)
    assert_eq(baggage_items[key], value)
  }
}

test "跨服务HTTP传播" {
  // 测试HTTP请求头注入
  let outgoing_headers = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-00f067aa0ba902b7-01",
    "tracestate": "congo=t61rcWkgMzE",
    "baggage": "userId=12345,sessionId=abcdef123456",
    "X-Request-Id": "req-789"
  }
  
  // 验证traceparent格式
  let traceparent = outgoing_headers["traceparent"]
  let traceparent_parts = traceparent.split("-")
  assert_eq(traceparent_parts.length(), 4)
  assert_eq(traceparent_parts[0], "00") // 版本
  assert_eq(traceparent_parts[1], "0af7651916cd43dd8448eb211c80319c") // trace-id
  assert_eq(traceparent_parts[2], "00f067aa0ba902b7") // parent-span-id
  assert_eq(traceparent_parts[3], "01") // flags
  
  // 测试HTTP响应头传播
  let incoming_headers = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-b7902baa0af76519-01",
    "tracestate": "congo=t61rcWkgMzE,rojo=00f067aa0ba902b7",
    "X-Response-Time": "150ms"
  }
  
  // 验证响应头包含新的span-id
  let response_traceparent = incoming_headers["traceparent"]
  let response_parts = response_traceparent.split("-")
  assert_eq(response_parts[1], "0af7651916cd43dd8448eb211c80319c") // 相同的trace-id
  assert_eq(response_parts[2], "b7902baa0af76519") // 新的span-id
}

test "消息队列传播" {
  // 测试消息属性传播
  let message_properties = {
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "00f067aa0ba902b7",
    "parent_span_id": "b7902baa0af76519",
    "baggage": "userId=12345,sessionId=abcdef123456",
    "correlation_id": "corr-123"
  }
  
  // 验证消息属性
  assert_eq(message_properties["trace_id"], "0af7651916cd43dd8448eb211c80319c")
  assert_eq(message_properties["span_id"], "00f067aa0ba902b7")
  assert_eq(message_properties["correlation_id"], "corr-123")
  
  // 测试消息体中的追踪信息
  let message_body = {
    "event": "user_action",
    "data": {
      "user_id": "12345",
      "action": "login",
      "timestamp": "1640995200"
    },
    "trace_context": {
      "trace_id": "0af7651916cd43dd8448eb211c80319c",
      "span_id": "00f067aa0ba902b7"
    }
  }
  
  // 验证消息体包含追踪上下文
  assert_eq(message_body.trace_context.trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(message_body.trace_context.span_id, "00f067aa0ba902b7")
}

test "服务网格传播" {
  // 测试Istio/Envoy传播
  let envoy_headers = {
    "x-request-id": "req-12345",
    "x-b3-traceid": "0af7651916cd43dd8448eb211c80319c",
    "x-b3-spanid": "00f067aa0ba902b7",
    "x-b3-parentspanid": "b7902baa0af76519",
    "x-b3-sampled": "1",
    "x-ot-span-context": "0af7651916cd43dd8448eb211c80319c;00f067aa0ba902b7;1"
  }
  
  // 验证B3传播格式
  assert_eq(envoy_headers["x-b3-traceid"], "0af7651916cd43dd8448eb211c80319c")
  assert_eq(envoy_headers["x-b3-spanid"], "00f067aa0ba902b7")
  assert_eq(envoy_headers["x-b3-sampled"], "1")
  
  // 测试OpenTracing传播
  let ot_headers = {
    "uber-trace-id": "0af7651916cd43dd8448eb211c80319c:00f067aa0ba902b7:0:1",
    "uberctx-user_id": "12345",
    "uberctx-session_id": "abcdef123456"
  }
  
  // 验证Uber追踪格式
  let uber_trace = ot_headers["uber-trace-id"]
  let uber_parts = uber_trace.split(":")
  assert_eq(uber_parts.length(), 4)
  assert_eq(uber_parts[0], "0af7651916cd43dd8448eb211c80319c") // trace-id
  assert_eq(uber_parts[1], "00f067aa0ba902b7") // span-id
  assert_eq(uber_parts[2], "0") // parent-span-id
  assert_eq(uber_parts[3], "1") // flags
}

test "跨语言传播" {
  // 测试Java服务传播
  let java_context = {
    "traceId": "0af7651916cd43dd8448eb211c80319c",
    "spanId": "00f067aa0ba902b7",
    "parentSpanId": "b7902baa0af76519",
    "flags": "1",
    "baggage": "userId=12345,sessionId=abcdef123456"
  }
  
  // 测试Python服务传播
  let python_context = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-00f067aa0ba902b7-01",
    "tracestate": "congo=t61rcWkgMzE",
    "baggage": "userId=12345,sessionId=abcdef123456"
  }
  
  // 测试Node.js服务传播
  let nodejs_context = {
    "traceId": "0af7651916cd43dd8448eb211c80319c",
    "spanId": "00f067aa0ba902b7",
    "traceFlags": "01",
    "baggageItems": {
      "userId": "12345",
      "sessionId": "abcdef123456"
    }
  }
  
  // 验证所有服务使用相同的trace-id
  assert_eq(java_context["traceId"], python_context["traceparent"].split("-")[1])
  assert_eq(java_context["traceId"], nodejs_context["traceId"])
  
  // 验证baggage在所有服务中一致
  assert_eq(java_context["baggage"].contains("userId=12345"), true)
  assert_eq(python_context["baggage"].contains("userId=12345"), true)
  assert_eq(nodejs_context["baggageItems"]["userId"], "12345")
}

test "传播采样决策" {
  // 测试采样决策传播
  let sampled_trace = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-00f067aa0ba902b7-01",
    "sampled": "1"
  }
  
  let not_sampled_trace = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-00f067aa0ba902b7-00",
    "sampled": "0"
  }
  
  // 验证采样决策
  let sampled_traceparent = sampled_trace["traceparent"]
  let sampled_parts = sampled_traceparent.split("-")
  assert_eq(sampled_parts[3], "01") // 采样标志
  
  let not_sampled_traceparent = not_sampled_trace["traceparent"]
  let not_sampled_parts = not_sampled_traceparent.split("-")
  assert_eq(not_sampled_parts[3], "00") // 不采样标志
  
  // 测试采样决策一致性
  let is_sampled = sampled_trace["sampled"] == "1"
  let is_not_sampled = not_sampled_trace["sampled"] == "0"
  
  assert_eq(is_sampled, true)
  assert_eq(is_not_sampled, true)
}