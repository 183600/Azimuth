// Azimuth 跨服务传播测试用例
// 专注于测试跨微服务架构中的上下文传播和数据一致性

// 测试1: 追踪上下文跨服务传播
test "追踪上下文跨服务传播测试" {
  // 创建初始追踪上下文
  let initial_context = TraceContext::new({
    trace_id: "trace-123456789",
    span_id: "root-span-001",
    baggage: {
      "user.id": "user-12345",
      "request.id": "req-abcdef",
      "session.id": "sess-xyz789",
      "tenant.id": "tenant-001"
    }
  })
  
  // 模拟服务链：Gateway -> Auth -> User -> Database
  let gateway_service = Service::new("gateway", initial_context)
  
  // Gateway处理请求，创建子span
  let gateway_span = gateway_service.create_span("http.request", {
    operation_name: "process.user.request",
    tags: {
      "http.method": "GET",
      "http.url": "/api/users/12345",
      "user.agent": "Mozilla/5.0"
    }
  })
  
  // Gateway调用Auth服务
  let auth_context = gateway_service.propagate_context_to("auth-service", gateway_span)
  let auth_service = Service::new("auth-service", auth_context)
  
  let auth_span = auth_service.create_span("authenticate", {
    operation_name: "verify.user.token",
    tags: {
      "auth.method": "jwt",
      "token.version": "v2"
    }
  })
  
  // Auth服务调用User服务
  let user_context = auth_service.propagate_context_to("user-service", auth_span)
  let user_service = Service::new("user-service", user_context)
  
  let user_span = user_service.create_span("get.user.profile", {
    operation_name: "fetch.user.data",
    tags: {
      "user.id": "12345",
      "data.fields": "profile,preferences"
    }
  })
  
  // User服务调用Database
  let db_context = user_service.propagate_context_to("database", user_span)
  let db_service = Service::new("database", db_context)
  
  let db_span = db_service.create_span("sql.query", {
    operation_name: "select.user.profile",
    tags: {
      "db.statement": "SELECT * FROM users WHERE id = ?",
      "db.type": "postgresql"
    }
  })
  
  // 验证追踪ID一致性
  assert_eq(gateway_service.get_trace_id(), "trace-123456789")
  assert_eq(auth_service.get_trace_id(), "trace-123456789")
  assert_eq(user_service.get_trace_id(), "trace-123456789")
  assert_eq(db_service.get_trace_id(), "trace-123456789")
  
  // 验证父子关系
  assert_eq(auth_span.parent_span_id, Some(gateway_span.span_id))
  assert_eq(user_span.parent_span_id, Some(auth_span.span_id))
  assert_eq(db_span.parent_span_id, Some(user_span.span_id))
  
  // 验证baggage传播
  assert_true(db_service.has_baggage("user.id"))
  assert_true(db_service.has_baggage("request.id"))
  assert_true(db_service.has_baggage("session.id"))
  assert_true(db_service.has_baggage("tenant.id"))
  
  assert_eq(db_service.get_baggage("user.id"), Some("user-12345"))
  assert_eq(db_service.get_baggage("request.id"), Some("req-abcdef"))
  assert_eq(db_service.get_baggage("session.id"), Some("sess-xyz789"))
  assert_eq(db_service.get_baggage("tenant.id"), Some("tenant-001"))
}

// 测试2: 错误上下文跨服务传播
test "错误上下文跨服务传播测试" {
  // 创建初始上下文
  let initial_context = TraceContext::new({
    trace_id: "trace-error-001",
    span_id: "root-error-span",
    baggage: {
      "request.id": "req-error-001",
      "user.id": "user-99999"
    }
  })
  
  let order_service = Service::new("order-service", initial_context)
  
  // Order服务尝试创建订单
  let order_span = order_service.create_span("create.order", {
    operation_name: "process.new.order",
    tags: {
      "order.amount": "99.99",
      "product.id": "prod-12345"
    }
  })
  
  // Order服务调用Payment服务
  let payment_context = order_service.propagate_context_to("payment-service", order_span)
  let payment_service = Service::new("payment-service", payment_context)
  
  let payment_span = payment_service.create_span("process.payment", {
    operation_name: "charge.credit.card",
    tags: {
      "payment.method": "credit_card",
      "card.type": "visa"
    }
  })
  
  // Payment服务调用第三方支付网关（模拟失败）
  let gateway_context = payment_service.propagate_context_to("payment-gateway", payment_span)
  let gateway_service = Service::new("payment-gateway", gateway_context)
  
  let gateway_span = gateway_service.create_span("gateway.request", {
    operation_name: "call.external.payment",
    tags: {
      "gateway.provider": "stripe",
      "api.version": "v3"
    }
  })
  
  // 模拟支付网关错误
  let payment_error = ServiceError::new({
    error_code: "PAYMENT_DECLINED",
    error_message: "Insufficient funds",
    error_details: {
      "decline_reason": "insufficient_funds",
      "card_last4": "1234",
      "available_balance": "25.50"
    }
  })
  
  gateway_span.record_error(payment_error)
  
  // 错误传播回Payment服务
  let propagated_error = payment_service.handle_error_from("payment-gateway", payment_error)
  payment_span.record_error(propagated_error)
  
  // 错误传播回Order服务
  let final_error = order_service.handle_error_from("payment-service", propagated_error)
  order_span.record_error(final_error)
  
  // 验证错误上下文传播
  assert_true(gateway_span.has_error())
  assert_true(payment_span.has_error())
  assert_true(order_span.has_error())
  
  // 验证错误信息完整性
  let gateway_error = gateway_span.get_error()
  match gateway_error {
    Some(error) => {
      assert_eq(error.error_code, "PAYMENT_DECLINED")
      assert_eq(error.error_message, "Insufficient funds")
    }
    None => assert_true(false)
  }
  
  // 验证错误链路
  let order_error = order_span.get_error()
  match order_error {
    Some(error) => {
      assert_eq(error.error_code, "PAYMENT_DECLINED")
      assert_true(error.error_details.contains_key("source_service"))
      assert_eq(error.error_details.get("source_service"), Some("payment-service"))
    }
    None => assert_true(false)
  }
}

// 测试3: 跨服务采样决策传播
test "跨服务采样决策传播测试" {
  // 创建采样策略
  let sampling_strategy = SamplingStrategy::new({
    default_rate: 0.1,  // 10%默认采样率
    service_specific: {
      "critical-service": 1.0,  // 100%采样
      "debug-service": 0.5      // 50%采样
    }
  })
  
  // 测试场景1：未采样的请求
  let unsampled_trace_id = "trace-unsampled-001"
  let unsampled_context = TraceContext::new({
    trace_id: unsampled_trace_id,
    span_id: "root-unsampled",
    baggage: {},
    sampled: false
  })
  
  let web_service = Service::new("web-service", unsampled_context)
  
  // 验证未采样决策传播
  let api_context = web_service.propagate_context_to("api-service", web_service.current_span())
  assert_false(api_context.sampled)
  
  let api_service = Service::new("api-service", api_context)
  let db_context = api_service.propagate_context_to("database", api_service.current_span())
  assert_false(db_context.sampled)
  
  // 测试场景2：已采样的请求
  let sampled_trace_id = "trace-sampled-001"
  let sampled_context = TraceContext::new({
    trace_id: sampled_trace_id,
    span_id: "root-sampled",
    baggage: {},
    sampled: true
  })
  
  let web_service2 = Service::new("web-service", sampled_context)
  
  // 验证采样决策传播
  let api_context2 = web_service2.propagate_context_to("api-service", web_service2.current_span())
  assert_true(api_context2.sampled)
  
  let api_service2 = Service::new("api-service", api_context2)
  let db_context2 = api_service2.propagate_context_to("database", api_service2.current_span())
  assert_true(db_context2.sampled)
  
  // 测试场景3：服务特定采样率
  let critical_trace_id = "trace-critical-001"
  let critical_context = TraceContext::new({
    trace_id: critical_trace_id,
    span_id: "root-critical",
    baggage: {},
    sampled: None  // 未明确指定采样决策
  })
  
  let critical_service = Service::new("critical-service", critical_context)
  
  // 应用服务特定采样策略
  let sampling_decision = sampling_strategy.should_sample("critical-service", critical_trace_id)
  assert_true(sampling_decision)
  
  critical_service.set_sampling_decision(sampling_decision)
  
  // 验证采样决策传播
  let downstream_context = critical_service.propagate_context_to("downstream-service", critical_service.current_span())
  assert_true(downstream_context.sampled)
}

// 测试4: 跨服务元数据传播
test "跨服务元数据传播测试" {
  // 创建初始元数据
  let initial_metadata = ServiceMetadata::new({
    service_name: "gateway",
    version: "1.2.3",
    environment: "production",
    region: "us-east-1",
    deployment: {
      pod_name: "gateway-7f8d9c2b",
      namespace: "frontend",
      cluster: "prod-cluster-01"
    },
    business_context: {
      tenant_id: "tenant-123",
      organization_id: "org-456",
      product_line: "ecommerce"
    }
  })
  
  let gateway_service = Service::with_metadata("gateway", initial_metadata)
  
  // 添加运行时元数据
  gateway_service.add_runtime_metadata({
    "request.start_time": "2023-01-01T12:00:00Z",
    "client.ip": "192.168.1.100",
    "client.geo": "US-NY"
  })
  
  // 传播到下游服务
  let auth_metadata = gateway_service.propagate_metadata_to("auth-service")
  let auth_service = Service::with_metadata("auth-service", auth_metadata)
  
  // Auth服务添加自己的元数据
  auth_service.add_runtime_metadata({
    "auth.method": "jwt",
    "token.issuer": "auth-service",
    "token.expiry": "2023-01-01T12:30:00Z"
  })
  
  // 继续传播
  let user_metadata = auth_service.propagate_metadata_to("user-service")
  let user_service = Service::with_metadata("user-service", user_metadata)
  
  user_service.add_runtime_metadata({
    "user.id": "user-789",
    "user.tier": "premium",
    "feature.flags": "new_dashboard,advanced_analytics"
  })
  
  // 验证元数据传播
  // 验证初始元数据保留
  assert_eq(user_service.get_metadata("environment"), Some("production"))
  assert_eq(user_service.get_metadata("region"), Some("us-east-1"))
  assert_eq(user_service.get_metadata("tenant_id"), Some("tenant-123"))
  assert_eq(user_service.get_metadata("organization_id"), Some("org-456"))
  
  // 验证上游服务元数据
  assert_eq(user_service.get_metadata("auth.method"), Some("jwt"))
  assert_eq(user_service.get_metadata("token.issuer"), Some("auth-service"))
  
  // 验证客户端上下文
  assert_eq(user_service.get_metadata("client.ip"), Some("192.168.1.100"))
  assert_eq(user_service.get_metadata("client.geo"), Some("US-NY"))
  
  // 验证当前服务元数据
  assert_eq(user_service.get_metadata("user.id"), Some("user-789"))
  assert_eq(user_service.get_metadata("user.tier"), Some("premium"))
}

// 测试5: 跨服务链路追踪完整性
test "跨服务链路追踪完整性测试" {
  // 创建复杂的微服务调用链
  let trace_collector = TraceCollector::new()
  
  // 初始请求
  let initial_context = TraceContext::new({
    trace_id: "trace-complex-001",
    span_id: "entry-point",
    baggage: {
      "request.id": "req-complex-001",
      "user.id": "user-complex",
      "session.id": "sess-complex"
    }
  })
  
  let entry_service = Service::new("entry-service", initial_context)
  let entry_span = entry_service.create_span("http.request", {
    operation_name: "process.complex.request",
    tags: {
      "http.method": "POST",
      "http.url": "/api/complex/workflow"
    }
  })
  
  trace_collector.add_span(entry_span)
  
  // 并行调用多个服务
  let parallel_calls = [
    ("auth-service", "authenticate"),
    ("config-service", "get.configuration"),
    ("validation-service", "validate.request")
  ]
  
  let parallel_spans = []
  for (service_name, operation) in parallel_calls {
    let context = entry_service.propagate_context_to(service_name, entry_span)
    let service = Service::new(service_name, context)
    let span = service.create_span(operation, {
      operation_name: operation,
      tags: {
        "service.name": service_name,
        "operation.type": "parallel"
      }
    })
    
    trace_collector.add_span(span)
    parallel_spans.push(span)
  }
  
  // 串行调用
  let sequential_calls = [
    ("business-service", "process.business.logic"),
    ("data-service", "fetch.required.data"),
    ("notification-service", "send.notifications")
  ]
  
  let last_span = entry_span
  for (service_name, operation) in sequential_calls {
    let context = entry_service.propagate_context_to(service_name, last_span)
    let service = Service::new(service_name, context)
    let span = service.create_span(operation, {
      operation_name: operation,
      tags: {
        "service.name": service_name,
        "operation.type": "sequential"
      }
    })
    
    trace_collector.add_span(span)
    last_span = span
  }
  
  // 验证追踪完整性
  let trace = trace_collector.get_trace("trace-complex-001")
  match trace {
    Some(collected_trace) => {
      // 验证span数量
      assert_eq(collected_trace.spans.length(), 7)  // 1 entry + 3 parallel + 3 sequential
      
      // 验证根span
      let root_span = collected_trace.spans.find(|span| span.span_id == "entry-point")
      match root_span {
        Some(span) => {
          assert_eq(span.operation_name, "process.complex.request")
          assert_eq(span.parent_span_id, None)
        }
        None => assert_true(false)
      }
      
      // 验证并行span的父级都是entry span
      let parallel_auth_span = collected_trace.spans.find(|span| 
        span.service_name == "auth-service" && span.operation_name == "authenticate"
      )
      match parallel_auth_span {
        Some(span) => assert_eq(span.parent_span_id, Some("entry-point"))
        None => assert_true(false)
      }
      
      // 验证串行调用的链式关系
      let business_span = collected_trace.spans.find(|span| 
        span.service_name == "business-service"
      )
      let data_span = collected_trace.spans.find(|span| 
        span.service_name == "data-service"
      )
      
      match (business_span, data_span) {
        (Some(business), Some(data)) => {
          assert_eq(business.parent_span_id, Some("entry-point"))
          assert_eq(data.parent_span_id, Some(business.span_id))
        }
        _ => assert_true(false)
      }
      
      // 验证所有span的trace_id一致
      for span in collected_trace.spans {
        assert_eq(span.trace_id, "trace-complex-001")
      }
      
      // 验证baggage在所有span中传播
      for span in collected_trace.spans {
        assert_true(span.baggage.contains_key("request.id"))
        assert_true(span.baggage.contains_key("user.id"))
        assert_true(span.baggage.contains_key("session.id"))
      }
    }
    None => assert_true(false)
  }
}