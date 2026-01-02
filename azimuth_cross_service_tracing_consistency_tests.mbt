// Azimuth Cross-Service Tracing Consistency Tests
// 跨服务追踪一致性测试用例

test "cross-service trace context propagation" {
  // 测试跨服务追踪上下文传播
  let initial_context = @azimuth.TraceContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "root1234567890",
    sampled : true,
    trace_state : "sampling.decision=true;foo=bar",
    baggage : [
      ("user.id", "user123"),
      ("request.id", "req-456"),
      ("client.version", "1.2.3")
    ]
  }
  
  // 模拟服务A处理请求
  let service_a_context = @azimuth.create_child_context(initial_context, "service-a")
  let service_a_span = @azimuth.Span {
    context : service_a_context,
    parent_span_id : Some(initial_context.span_id),
    operation_name : "service-a.process",
    start_time : 1640995200000L,
    end_time : Some(1640995200100L),
    status : @azimuth.SpanStatus::Ok,
    attributes : [
      ("service.name", @azimuth.StringValue("service-a")),
      ("service.version", @azimuth.StringValue("2.1.0"))
    ],
    events : []
  }
  
  // 模拟服务B处理请求
  let service_b_context = @azimuth.create_child_context(service_a_context, "service-b")
  let service_b_span = @azimuth.Span {
    context : service_b_context,
    parent_span_id : Some(service_a_context.span_id),
    operation_name : "service-b.process",
    start_time : 1640995200050L,
    end_time : Some(1640995200150L),
    status : @azimuth.SpanStatus::Ok,
    attributes : [
      ("service.name", @azimuth.StringValue("service-b")),
      ("service.version", @azimuth.StringValue("1.5.2"))
    ],
    events : []
  }
  
  // 验证追踪上下文一致性
  assert_eq(initial_context.trace_id, service_a_context.trace_id)
  assert_eq(service_a_context.trace_id, service_b_context.trace_id)
  
  // 验证span ID唯一性
  assert_not_eq(initial_context.span_id, service_a_context.span_id)
  assert_not_eq(service_a_context.span_id, service_b_context.span_id)
  assert_not_eq(initial_context.span_id, service_b_context.span_id)
  
  // 验证采样决策传播
  assert_true(initial_context.sampled)
  assert_true(service_a_context.sampled)
  assert_true(service_b_context.sampled)
  
  // 验证baggage传播
  assert_eq(initial_context.baggage.length(), service_a_context.baggage.length())
  assert_eq(service_a_context.baggage.length(), service_b_context.baggage.length())
  
  // 验证时间顺序
  assert_true(service_a_span.start_time <= service_b_span.start_time)
  assert_true(service_a_context.span_id == service_b_span.parent_span_id.unwrap())
}

test "distributed trace timeline consistency" {
  // 测试分布式追踪时间线一致性
  let trace_timeline = @azimuth.TraceTimeline {
    trace_id : "fedcba0987654321fedcba0987654321",
    spans : [
      @azimuth.Span {
        context : @azimuth.SpanContext {
          trace_id : "fedcba0987654321fedcba0987654321",
          span_id : "gateway12345678",
          sampled : true,
          trace_state : ""
        },
        parent_span_id : None,
        operation_name : "api-gateway.request",
        start_time : 1640995200000L,
        end_time : Some(1640995200050L),
        status : @azimuth.SpanStatus::Ok,
        attributes : [
          ("service.name", @azimuth.StringValue("api-gateway")),
          ("http.method", @azimuth.StringValue("POST")),
          ("http.path", @azimuth.StringValue("/api/orders"))
        ],
        events : []
      },
      @azimuth.Span {
        context : @azimuth.SpanContext {
          trace_id : "fedcba0987654321fedcba0987654321",
          span_id : "auth1234567890",
          sampled : true,
          trace_state : ""
        },
        parent_span_id : Some("gateway12345678"),
        operation_name : "auth-service.validate",
        start_time : 1640995200010L,
        end_time : Some(1640995200040L),
        status : @azimuth.SpanStatus::Ok,
        attributes : [
          ("service.name", @azimuth.StringValue("auth-service")),
          ("auth.method", @azimuth.StringValue("jwt"))
        ],
        events : []
      },
      @azimuth.Span {
        context : @azimuth.SpanContext {
          trace_id : "fedcba0987654321fedcba0987654321",
          span_id : "order1234567890",
          sampled : true,
          trace_state : ""
        },
        parent_span_id : Some("gateway12345678"),
        operation_name : "order-service.create",
        start_time : 1640995200055L,
        end_time : Some(1640995200120L),
        status : @azimuth.SpanStatus::Ok,
        attributes : [
          ("service.name", @azimuth.StringValue("order-service")),
          ("order.id", @azimuth.StringValue("order-12345"))
        ],
        events : []
      },
      @azimuth.Span {
        context : @azimuth.SpanContext {
          trace_id : "fedcba0987654321fedcba0987654321",
          span_id : "payment12345678",
          sampled : true,
          trace_state : ""
        },
        parent_span_id : Some("order1234567890"),
        operation_name : "payment-service.process",
        start_time : 1640995200080L,
        end_time : Some(1640995200110L),
        status : @azimuth.SpanStatus::Ok,
        attributes : [
          ("service.name", @azimuth.StringValue("payment-service")),
          ("payment.amount", @azimuth.FloatValue(99.99)),
          ("payment.currency", @azimuth.StringValue("USD"))
        ],
        events : []
      }
    ]
  }
  
  // 验证时间线一致性
  let sorted_spans = trace_timeline.spans.sort_by(fn(a, b) { a.start_time - b.start_time })
  
  // 验证时间顺序
  for i in 1..sorted_spans.length() {
    assert_true(sorted_spans[i-1].start_time <= sorted_spans[i].start_time)
  }
  
  // 验证父子关系
  let gateway_span = sorted_spans.filter(fn(s) { s.context.span_id == "gateway12345678" })[0]
  let auth_span = sorted_spans.filter(fn(s) { s.context.span_id == "auth1234567890" })[0]
  let order_span = sorted_spans.filter(fn(s) { s.context.span_id == "order1234567890" })[0]
  let payment_span = sorted_spans.filter(fn(s) { s.context.span_id == "payment12345678" })[0]
  
  // 验证auth和order是gateway的子span
  match auth_span.parent_span_id {
    Some(id) => assert_eq(id, gateway_span.context.span_id)
    None => assert_true(false)
  }
  
  match order_span.parent_span_id {
    Some(id) => assert_eq(id, gateway_span.context.span_id)
    None => assert_true(false)
  }
  
  // 验证payment是order的子span
  match payment_span.parent_span_id {
    Some(id) => assert_eq(id, order_span.context.span_id)
    None => assert_true(false)
  }
  
  // 验证时间重叠关系
  assert_true(auth_span.start_time >= gateway_span.start_time)
  assert_true(auth_span.end_time.unwrap() <= gateway_span.end_time.unwrap())
  
  assert_true(order_span.start_time >= gateway_span.start_time)
  assert_true(order_span.end_time.unwrap() <= gateway_span.end_time.unwrap())
  
  assert_true(payment_span.start_time >= order_span.start_time)
  assert_true(payment_span.end_time.unwrap() <= order_span.end_time.unwrap())
}

test "cross-service error propagation and correlation" {
  // 测试跨服务错误传播和关联
  let error_trace = @azimuth.TraceWithErrors {
    trace_id : "deadbeefdeadbeefdeadbeefdeadbeef",
    spans : [
      @azimuth.Span {
        context : @azimuth.SpanContext {
          trace_id : "deadbeefdeadbeefdeadbeefdeadbeef",
          span_id : "frontend12345678",
          sampled : true,
          trace_state : ""
        },
        parent_span_id : None,
        operation_name : "frontend.submit_order",
        start_time : 1640995200000L,
        end_time : Some(1640995200300L),
        status : @azimuth.SpanStatus::Error,
        attributes : [
          ("service.name", @azimuth.StringValue("frontend")),
          ("error.code", @azimuth.StringValue("ORDER_SUBMISSION_FAILED"))
        ],
        events : [
          @azimuth.SpanEvent {
            name : "exception",
            timestamp : 1640995200250L,
            attributes : [
              ("exception.type", @azimuth.StringValue("OrderSubmissionException")),
              ("exception.message", @azimuth.StringValue("Failed to submit order: Payment declined"))
            ]
          }
        ]
      },
      @azimuth.Span {
        context : @azimuth.SpanContext {
          trace_id : "deadbeefdeadbeefdeadbeefdeadbeef",
          span_id : "api1234567890",
          sampled : true,
          trace_state : ""
        },
        parent_span_id : Some("frontend12345678"),
        operation_name : "api-service.process_order",
        start_time : 1640995200050L,
        end_time : Some(1640995200280L),
        status : @azimuth.SpanStatus::Error,
        attributes : [
          ("service.name", @azimuth.StringValue("api-service")),
          ("error.code", @azimuth.StringValue("PAYMENT_PROCESSING_ERROR"))
        ],
        events : [
          @azimuth.SpanEvent {
            name : "exception",
            timestamp : 1640995200230L,
            attributes : [
              ("exception.type", @azimuth.StringValue("PaymentProcessingException")),
              ("exception.message", @azimuth.StringValue("Payment gateway returned error: Insufficient funds"))
            ]
          }
        ]
      },
      @azimuth.Span {
        context : @azimuth.SpanContext {
          trace_id : "deadbeefdeadbeefdeadbeefdeadbeef",
          span_id : "payment12345678",
          sampled : true,
          trace_state : ""
        },
        parent_span_id : Some("api1234567890"),
        operation_name : "payment-service.charge",
        start_time : 1640995200100L,
        end_time : Some(1640995200220L),
        status : @azimuth.SpanStatus::Error,
        attributes : [
          ("service.name", @azimuth.StringValue("payment-service")),
          ("error.code", @azimuth.StringValue("INSUFFICIENT_FUNDS")),
          ("payment.gateway", @azimuth.StringValue("stripe")),
          ("gateway.error.code", @azimuth.StringValue("card_declined"))
        ],
        events : [
          @azimuth.SpanEvent {
            name : "gateway.response",
            timestamp : 1640995200200L,
            attributes : [
              ("response.code", @azimuth.StringValue("card_declined")),
              ("response.message", @azimuth.StringValue("Your card has insufficient funds."))
            ]
          }
        ]
      }
    ]
  }
  
  // 验证错误传播链
  let frontend_span = error_trace.spans.filter(fn(s) { s.context.span_id == "frontend12345678" })[0]
  let api_span = error_trace.spans.filter(fn(s) { s.context.span_id == "api1234567890" })[0]
  let payment_span = error_trace.spans.filter(fn(s) { s.context.span_id == "payment12345678" })[0]
  
  // 验证所有span都有错误状态
  match frontend_span.status {
    @azimuth.SpanStatus::Error => assert_true(true)
    _ => assert_true(false)
  }
  
  match api_span.status {
    @azimuth.SpanStatus::Error => assert_true(true)
    _ => assert_true(false)
  }
  
  match payment_span.status {
    @azimuth.SpanStatus::Error => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证错误代码传播
  let frontend_error_code = frontend_span.attributes.filter(fn(a) { a.0 == "error.code" })
  let api_error_code = api_span.attributes.filter(fn(a) { a.0 == "error.code" })
  let payment_error_code = payment_span.attributes.filter(fn(a) { a.0 == "error.code" })
  
  match frontend_error_code[0].1 {
    @azimuth.StringValue(code) => assert_eq(code, "ORDER_SUBMISSION_FAILED")
    _ => assert_true(false)
  }
  
  match api_error_code[0].1 {
    @azimuth.StringValue(code) => assert_eq(code, "PAYMENT_PROCESSING_ERROR")
    _ => assert_true(false)
  }
  
  match payment_error_code[0].1 {
    @azimuth.StringValue(code) => assert_eq(code, "INSUFFICIENT_FUNDS")
    _ => assert_true(false)
  }
  
  // 验证错误事件时间顺序
  let payment_error_time = payment_span.events[0].timestamp
  let api_error_time = api_span.events[0].timestamp
  let frontend_error_time = frontend_span.events[0].timestamp
  
  assert_true(payment_error_time <= api_error_time)
  assert_true(api_error_time <= frontend_error_time)
  
  // 验证根本原因在payment服务
  match payment_span.events[0].attributes.filter(fn(a) { a.0 == "response.code" })[0].1 {
    @azimuth.StringValue(code) => assert_eq(code, "card_declined")
    _ => assert_true(false)
  }
}

test "cross-service baggage consistency" {
  // 测试跨服务baggage一致性
  let initial_baggage = [
    ("user.id", "user123"),
    ("request.id", "req-456"),
    ("client.version", "1.2.3"),
    ("trace.sampled", "true"),
    ("debug.enabled", "false")
  ]
  
  // 模拟多个服务处理请求
  let service_a_baggage = @azimuth.propagate_baggage(initial_baggage, "service-a")
  let service_b_baggage = @azimuth.propagate_baggage(service_a_baggage, "service-b")
  let service_c_baggage = @azimuth.propagate_baggage(service_b_baggage, "service-c")
  
  // 验证初始baggage项在所有服务中保持一致
  for baggage_item in initial_baggage {
    let key = baggage_item.0
    let value = baggage_item.1
    
    // 验证service-a中的baggage
    let service_a_item = service_a_baggage.filter(fn(item) { item.0 == key })
    assert_eq(service_a_item.length(), 1)
    assert_eq(service_a_item[0].1, value)
    
    // 验证service-b中的baggage
    let service_b_item = service_b_baggage.filter(fn(item) { item.0 == key })
    assert_eq(service_b_item.length(), 1)
    assert_eq(service_b_item[0].1, value)
    
    // 验证service-c中的baggage
    let service_c_item = service_c_baggage.filter(fn(item) { item.0 == key })
    assert_eq(service_c_item.length(), 1)
    assert_eq(service_c_item[0].1, value)
  }
  
  // 验证服务特定的baggage项添加
  let service_a_specific = service_a_baggage.filter(fn(item) { 
    item.0 == "service.a.processing.time" 
  })
  assert_eq(service_a_specific.length(), 1)
  assert_eq(service_a_specific[0].0, "service.a.processing.time")
  
  let service_b_specific = service_b_baggage.filter(fn(item) { 
    item.0 == "service.b.cache.hit" 
  })
  assert_eq(service_b_specific.length(), 1)
  assert_eq(service_b_specific[0].0, "service.b.cache.hit")
  
  let service_c_specific = service_c_baggage.filter(fn(item) { 
    item.0 == "service.c.db.connection" 
  })
  assert_eq(service_c_specific.length(), 1)
  assert_eq(service_c_specific[0].0, "service.c.db.connection")
  
  // 验证baggage不会无限增长
  assert_true(service_c_baggage.length() <= initial_baggage.length() + 3) // 每个服务添加一项
  
  // 验证baggage合并策略
  let merged_baggage = @azimuth.merge_baggage(service_a_baggage, service_c_baggage)
  assert_true(merged_baggage.length() >= service_a_baggage.length())
  assert_true(merged_baggage.length() >= service_c_baggage.length())
  
  // 验证合并后原始值保持不变
  for baggage_item in initial_baggage {
    let key = baggage_item.0
    let value = baggage_item.1
    
    let merged_item = merged_baggage.filter(fn(item) { item.0 == key })
    assert_eq(merged_item.length(), 1)
    assert_eq(merged_item[0].1, value)
  }
}

test "cross-service sampling decision consistency" {
  // 测试跨服务采样决策一致性
  let trace_id = "1234567890abcdef1234567890abcdef"
  
  // 测试基于概率的采样决策
  let sampling_config = @azimuth.SamplingConfig {
    sampling_type : @azimuth.SamplingType::Probability,
    probability : 0.1, // 10%采样率
    rate_limit : Some(1000)
  }
  
  let initial_decision = @azimuth.make_sampling_decision(trace_id, sampling_config)
  let service_a_decision = @azimuth.make_sampling_decision(trace_id, sampling_config)
  let service_b_decision = @azimuth.make_sampling_decision(trace_id, sampling_config)
  let service_c_decision = @azimuth.make_sampling_decision(trace_id, sampling_config)
  
  // 验证相同trace_id的采样决策一致
  assert_eq(initial_decision, service_a_decision)
  assert_eq(service_a_decision, service_b_decision)
  assert_eq(service_b_decision, service_c_decision)
  
  // 测试基于速率限制的采样决策
  let rate_limit_config = @azimuth.SamplingConfig {
    sampling_type : @azimuth.SamplingType::RateLimit,
    probability : 1.0,
    rate_limit : Some(100) // 每秒100个采样
  }
  
  let rate_limit_decision_1 = @azimuth.make_sampling_decision(trace_id, rate_limit_config)
  let rate_limit_decision_2 = @azimuth.make_sampling_decision(trace_id, rate_limit_config)
  
  // 验证速率限制采样决策在短时间内一致
  assert_eq(rate_limit_decision_1, rate_limit_decision_2)
  
  // 测试基于父span的采样决策
  let parent_sampled = true
  let child_decision_1 = @azimuth.make_child_sampling_decision(parent_sampled)
  let child_decision_2 = @azimuth.make_child_sampling_decision(parent_sampled)
  let child_decision_3 = @azimuth.make_child_sampling_decision(false)
  
  // 验证子span继承父span的采样决策
  assert_true(child_decision_1)
  assert_true(child_decision_2)
  assert_false(child_decision_3)
  
  // 测试采样决策传播
  let sampling_context = @azimuth.SamplingContext {
    trace_id : "abcdef1234567890abcdef1234567890",
    parent_sampled : true,
    sampling_config : sampling_config,
    service_name : "order-service"
  }
  
  let propagated_context = @azimuth.propagate_sampling_context(sampling_context, "payment-service")
  
  // 验证采样决策传播
  assert_eq(propagated_context.trace_id, sampling_context.trace_id)
  assert_eq(propagated_context.parent_sampled, sampling_context.parent_sampled)
  assert_eq(propagated_context.sampling_config, sampling_context.sampling_config)
  assert_not_eq(propagated_context.service_name, sampling_context.service_name)
  assert_eq(propagated_context.service_name, "payment-service")
}