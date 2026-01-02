// Azimuth Cross-Service Tracing Tests
// 测试跨服务追踪功能

test "distributed trace creation and propagation" {
  // 创建根span上下文
  let root_span_context = @azimuth.SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1111111111111111",
    sampled : true,
    trace_state : "service=api-gateway,operation=handle-request"
  }
  
  // 验证根span上下文
  assert_eq(root_span_context.trace_id.length(), 32)
  assert_eq(root_span_context.span_id.length(), 16)
  assert_true(root_span_context.sampled)
  
  // 服务A创建子span
  let service_a_span = @azimuth.SpanContext {
    trace_id : root_span_context.trace_id,
    span_id : "2222222222222222",
    sampled : root_span_context.sampled,
    trace_state : root_span_context.trace_state + ",service=service-a,operation=process-data"
  }
  
  // 验证服务A span
  assert_eq(service_a_span.trace_id, root_span_context.trace_id)
  assert_not_eq(service_a_span.span_id, root_span_context.span_id)
  assert_eq(service_a_span.sampled, root_span_context.sampled)
  
  // 服务B创建子span
  let service_b_span = @azimuth.SpanContext {
    trace_id : root_span_context.trace_id,
    span_id : "3333333333333333",
    sampled : root_span_context.sampled,
    trace_state : root_span_context.trace_state + ",service=service-b,operation=validate-request"
  }
  
  // 验证服务B span
  assert_eq(service_b_span.trace_id, root_span_context.trace_id)
  assert_not_eq(service_b_span.span_id, root_span_context.span_id)
  assert_not_eq(service_b_span.span_id, service_a_span.span_id)
  assert_eq(service_b_span.sampled, root_span_context.sampled)
}

test "cross-service baggage propagation" {
  // API网关创建初始baggage
  let gateway_baggage = @azimuth.Baggage {
    entries : [
      ("user.id", "user123"),
      ("request.id", "req-abc123"),
      ("client.ip", "192.168.1.100"),
      ("user.agent", "Mozilla/5.0...")
    ]
  }
  
  // 认证服务添加信息
  let auth_baggage = @azimuth.Baggage {
    entries : gateway_baggage.entries + [
      ("auth.method", "oauth2"),
      ("auth.scope", "read:write"),
      ("auth.token.id", "token-456")
    ]
  }
  
  // 业务服务添加信息
  let business_baggage = @azimuth.Baggage {
    entries : auth_baggage.entries + [
      ("business.operation", "create-order"),
      ("business.priority", "high"),
      ("business.tenant", "tenant-789")
    ]
  }
  
  // 验证baggage传播链
  assert_eq(gateway_baggage.entries.length(), 4)
  assert_eq(auth_baggage.entries.length(), 7)
  assert_eq(business_baggage.entries.length(), 10)
  
  // 验证原始信息保持不变
  assert_eq(business_baggage.entries[0], ("user.id", "user123"))
  assert_eq(business_baggage.entries[1], ("request.id", "req-abc123"))
  assert_eq(business_baggage.entries[2], ("client.ip", "192.168.1.100"))
  assert_eq(business_baggage.entries[3], ("user.agent", "Mozilla/5.0..."))
  
  // 验证认证服务添加的信息
  assert_eq(business_baggage.entries[4], ("auth.method", "oauth2"))
  assert_eq(business_baggage.entries[5], ("auth.scope", "read:write"))
  assert_eq(business_baggage.entries[6], ("auth.token.id", "token-456"))
  
  // 验证业务服务添加的信息
  assert_eq(business_baggage.entries[7], ("business.operation", "create-order"))
  assert_eq(business_baggage.entries[8], ("business.priority", "high"))
  assert_eq(business_baggage.entries[9], ("business.tenant", "tenant-789"))
}

test "http header propagation for distributed tracing" {
  // 创建包含追踪信息的HTTP headers
  let inbound_headers = @azimuth.TextMapCarrier {
    headers : [
      ("traceparent", "00-1234567890abcdef1234567890abcdef-1111111111111111-01"),
      ("tracestate", "service=api-gateway,operation=handle-request"),
      ("baggage", "user.id=user123,request.id=req-abc123"),
      ("x-request-id", "req-xyz789"),
      ("x-correlation-id", "corr-abc456")
    ]
  }
  
  // 验证入站headers
  assert_eq(inbound_headers.headers.length(), 5)
  
  // 提取traceparent信息
  let traceparent = inbound_headers.headers[0]
  assert_eq(traceparent.0, "traceparent")
  assert_eq(traceparent.1, "00-1234567890abcdef1234567890abcdef-1111111111111111-01")
  
  // 服务处理请求后添加新信息
  let outbound_headers = @azimuth.TextMapCarrier {
    headers : inbound_headers.headers + [
      ("x-service-name", "order-service"),
      ("x-service-version", "1.2.3"),
      ("x-processing-time", "150"),
      ("x-cache-status", "hit")
    ]
  }
  
  // 验证出站headers
  assert_eq(outbound_headers.headers.length(), 9)
  
  // 验证原始headers保持不变
  assert_eq(outbound_headers.headers[0], inbound_headers.headers[0])
  assert_eq(outbound_headers.headers[1], inbound_headers.headers[1])
  assert_eq(outbound_headers.headers[2], inbound_headers.headers[2])
  
  // 验证新增headers
  assert_eq(outbound_headers.headers[5], ("x-service-name", "order-service"))
  assert_eq(outbound_headers.headers[6], ("x-service-version", "1.2.3"))
  assert_eq(outbound_headers.headers[7], ("x-processing-time", "150"))
  assert_eq(outbound_headers.headers[8], ("x-cache-status", "hit"))
}

test "trace context consistency across microservices" {
  // 微服务A的上下文
  let service_a_context = @azimuth.SpanContext {
    trace_id : "abcdef1234567890abcdef1234567890",
    span_id : "aaaaaaaaaaaaaaaa",
    sampled : true,
    trace_state : "service.a=processing,operation.a=validate"
  }
  
  // 微服务B的上下文（继承trace_id）
  let service_b_context = @azimuth.SpanContext {
    trace_id : service_a_context.trace_id,
    span_id : "bbbbbbbbbbbbbbbb",
    sampled : service_a_context.sampled,
    trace_state : service_a_context.trace_state + ",service.b=processing,operation.b=transform"
  }
  
  // 微服务C的上下文（继承trace_id）
  let service_c_context = @azimuth.SpanContext {
    trace_id : service_a_context.trace_id,
    span_id : "cccccccccccccccc",
    sampled : service_a_context.sampled,
    trace_state : service_a_context.trace_state + ",service.c=processing,operation.c=persist"
  }
  
  // 验证trace_id一致性
  assert_eq(service_a_context.trace_id, service_b_context.trace_id)
  assert_eq(service_b_context.trace_id, service_c_context.trace_id)
  assert_eq(service_a_context.trace_id, service_c_context.trace_id)
  
  // 验证span_id唯一性
  assert_not_eq(service_a_context.span_id, service_b_context.span_id)
  assert_not_eq(service_b_context.span_id, service_c_context.span_id)
  assert_not_eq(service_a_context.span_id, service_c_context.span_id)
  
  // 验证采样标志一致性
  assert_eq(service_a_context.sampled, service_b_context.sampled)
  assert_eq(service_b_context.sampled, service_c_context.sampled)
  
  // 验证trace_state累积
  assert_eq(service_a_context.trace_state, "service.a=processing,operation.a=validate")
  assert_eq(service_b_context.trace_state, "service.a=processing,operation.a=validate,service.b=processing,operation.b=transform")
  assert_eq(service_c_context.trace_state, "service.a=processing,operation.a=validate,service.c=processing,operation.c=persist")
}

test "async service communication tracing" {
  // 主服务发起异步调用
  let main_service_context = @azimuth.SpanContext {
    trace_id : "11112222333344445555666677778888",
    span_id : "1111111111111111",
    sampled : true,
    trace_state : "service=main,operation=async-process"
  }
  
  // 异步服务1的上下文
  let async_service_1_context = @azimuth.SpanContext {
    trace_id : main_service_context.trace_id,
    span_id : "2222222222222222",
    sampled : main_service_context.sampled,
    trace_state : main_service_context.trace_state + ",service=async-1,operation=process-data"
  }
  
  // 异步服务2的上下文
  let async_service_2_context = @azimuth.SpanContext {
    trace_id : main_service_context.trace_id,
    span_id : "3333333333333333",
    sampled : main_service_context.sampled,
    trace_state : main_service_context.trace_state + ",service=async-2,operation=process-notification"
  }
  
  // 异步服务3的上下文
  let async_service_3_context = @azimuth.SpanContext {
    trace_id : main_service_context.trace_id,
    span_id : "4444444444444444",
    sampled : main_service_context.sampled,
    trace_state : main_service_context.trace_state + ",service=async-3,operation=process-audit"
  }
  
  // 验证所有异步服务共享相同的trace_id
  assert_eq(async_service_1_context.trace_id, main_service_context.trace_id)
  assert_eq(async_service_2_context.trace_id, main_service_context.trace_id)
  assert_eq(async_service_3_context.trace_id, main_service_context.trace_id)
  
  // 验证每个异步服务有唯一的span_id
  assert_not_eq(async_service_1_context.span_id, async_service_2_context.span_id)
  assert_not_eq(async_service_2_context.span_id, async_service_3_context.span_id)
  assert_not_eq(async_service_1_context.span_id, async_service_3_context.span_id)
  
  // 验证trace_state包含各自的service信息
  assert_true(async_service_1_context.trace_state.contains("service=async-1"))
  assert_true(async_service_2_context.trace_state.contains("service=async-2"))
  assert_true(async_service_3_context.trace_state.contains("service=async-3"))
}

test "cross-service error propagation" {
  // 服务A发起请求
  let service_a_context = @azimuth.SpanContext {
    trace_id : "deadbeefdeadbeefdeadbeefdeadbeef",
    span_id : "aaaaaaaaaaaaaaaa",
    sampled : true,
    trace_state : "service=service-a,operation=process-request"
  }
  
  // 服务B处理时发生错误
  let service_b_context = @azimuth.SpanContext {
    trace_id : service_a_context.trace_id,
    span_id : "bbbbbbbbbbbbbbbb",
    sampled : service_a_context.sampled,
    trace_state : service_a_context.trace_state + ",service=service-b,operation=handle-error,error=true"
  }
  
  // 服务C处理错误
  let service_c_context = @azimuth.SpanContext {
    trace_id : service_a_context.trace_id,
    span_id : "cccccccccccccccc",
    sampled : service_a_context.sampled,
    trace_state : service_a_context.trace_state + ",service=service-c,operation=error-recovery,error=true"
  }
  
  // 验证错误传播
  assert_true(service_b_context.trace_state.contains("error=true"))
  assert_true(service_c_context.trace_state.contains("error=true"))
  
  // 验证trace_id在错误传播中保持一致
  assert_eq(service_a_context.trace_id, service_b_context.trace_id)
  assert_eq(service_b_context.trace_id, service_c_context.trace_id)
}

test "service mesh tracing" {
  // 服务网格中的sidecar代理
  let ingress_proxy_context = @azimuth.SpanContext {
    trace_id : "11111111111111111111111111111111",
    span_id : "1111111111111111",
    sampled : true,
    trace_state : "component=ingress-proxy,operation=route-request"
  }
  
  // 应用服务
  let app_service_context = @azimuth.SpanContext {
    trace_id : ingress_proxy_context.trace_id,
    span_id : "2222222222222222",
    sampled : ingress_proxy_context.sampled,
    trace_state : ingress_proxy_context.trace_state + ",component=app-service,operation=process-business-logic"
  }
  
  // 数据库服务
  let db_service_context = @azimuth.SpanContext {
    trace_id : ingress_proxy_context.trace_id,
    span_id : "3333333333333333",
    sampled : ingress_proxy_context.sampled,
    trace_state : ingress_proxy_context.trace_state + ",component=db-service,operation=query-database"
  }
  
  // 出口代理
  let egress_proxy_context = @azimuth.SpanContext {
    trace_id : ingress_proxy_context.trace_id,
    span_id : "4444444444444444",
    sampled : ingress_proxy_context.sampled,
    trace_state : ingress_proxy_context.trace_state + ",component=egress-proxy,operation=forward-response"
  }
  
  // 验证服务网格中的追踪一致性
  assert_eq(ingress_proxy_context.trace_id, app_service_context.trace_id)
  assert_eq(app_service_context.trace_id, db_service_context.trace_id)
  assert_eq(db_service_context.trace_id, egress_proxy_context.trace_id)
  
  // 验证每个组件有唯一的span_id
  let span_ids = [
    ingress_proxy_context.span_id,
    app_service_context.span_id,
    db_service_context.span_id,
    egress_proxy_context.span_id
  ]
  
  // 简单验证所有span_id都不同
  assert_not_eq(span_ids[0], span_ids[1])
  assert_not_eq(span_ids[1], span_ids[2])
  assert_not_eq(span_ids[2], span_ids[3])
  
  // 验证组件信息在trace_state中
  assert_true(ingress_proxy_context.trace_state.contains("component=ingress-proxy"))
  assert_true(app_service_context.trace_state.contains("component=app-service"))
  assert_true(db_service_context.trace_state.contains("component=db-service"))
  assert_true(egress_proxy_context.trace_state.contains("component=egress-proxy"))
}