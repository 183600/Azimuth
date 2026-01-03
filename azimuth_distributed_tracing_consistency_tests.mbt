// Azimuth Distributed Tracing Consistency Tests
// 专注于分布式追踪系统中数据一致性和完整性的高级测试用例

test "跨服务追踪ID一致性验证测试" {
  // 模拟分布式追踪场景
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  
  // 服务A的根span
  let root_span = azimuth::Span {
    span_context: azimuth::SpanContext {
      trace_id: trace_id,
      span_id: "b7ad6b7169203331",
      parent_span_id: None,
      sampled: true,
      trace_state: ""
    },
    operation_name: "user.request",
    start_time: 1634567890123,
    end_time: 1634567890456,
    status: azimuth::SpanStatus::Ok,
    attributes: []
  }
  
  // 服务B的子span
  let service_b_span = azimuth::Span {
    span_context: azimuth::SpanContext {
      trace_id: trace_id,  // 必须与根span相同
      span_id: "00f067aa0ba902b7",
      parent_span_id: Some("b7ad6b7169203331"),  // 指向父span
      sampled: true,
      trace_state: ""
    },
    operation_name: "database.query",
    start_time: 1634567890150,
    end_time: 1634567890200,
    status: azimuth::SpanStatus::Ok,
    attributes: []
  }
  
  // 服务C的子span
  let service_c_span = azimuth::Span {
    span_context: azimuth::SpanContext {
      trace_id: trace_id,  // 必须与根span相同
      span_id: "b2c5d6e7f8a9b0c1",
      parent_span_id: Some("b7ad6b7169203331"),  // 指向父span
      sampled: true,
      trace_state: ""
    },
    operation_name: "cache.get",
    start_time: 1634567890180,
    end_time: 1634567890190,
    status: azimuth::SpanStatus::Ok,
    attributes: []
  }
  
  // 验证追踪ID一致性
  assert_eq(root_span.span_context.trace_id, trace_id)
  assert_eq(service_b_span.span_context.trace_id, trace_id)
  assert_eq(service_c_span.span_context.trace_id, trace_id)
  
  // 验证父子关系
  match service_b_span.span_context.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, root_span.span_context.span_id)
    None => assert_true(false)
  }
  
  match service_c_span.span_context.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, root_span.span_context.span_id)
    None => assert_true(false)
  }
  
  // 验证时间顺序
  assert_true(root_span.start_time <= service_b_span.start_time)
  assert_true(root_span.start_time <= service_c_span.start_time)
  assert_true(service_b_span.end_time <= root_span.end_time)
  assert_true(service_c_span.end_time <= root_span.end_time)
}

test "分布式追踪时间戳一致性验证测试" {
  // 创建分布式追踪链
  let trace_start_time = 1634567890000
  
  let spans = [
    azimuth::Span {
      span_context: azimuth::SpanContext {
        trace_id: "trace123",
        span_id: "span1",
        parent_span_id: None,
        sampled: true,
        trace_state: ""
      },
      operation_name: "service.a.entry",
      start_time: trace_start_time,
      end_time: trace_start_time + 100,
      status: azimuth::SpanStatus::Ok,
      attributes: []
    },
    azimuth::Span {
      span_context: azimuth::SpanContext {
        trace_id: "trace123",
        span_id: "span2",
        parent_span_id: Some("span1"),
        sampled: true,
        trace_state: ""
      },
      operation_name: "service.b.process",
      start_time: trace_start_time + 20,
      end_time: trace_start_time + 80,
      status: azimuth::SpanStatus::Ok,
      attributes: []
    },
    azimuth::Span {
      span_context: azimuth::SpanContext {
        trace_id: "trace123",
        span_id: "span3",
        parent_span_id: Some("span2"),
        sampled: true,
        trace_state: ""
      },
      operation_name: "service.c.database",
      start_time: trace_start_time + 30,
      end_time: trace_start_time + 70,
      status: azimuth::SpanStatus::Ok,
      attributes: []
    }
  ]
  
  // 验证时间戳一致性
  let root_span = spans[0]
  let child_span1 = spans[1]
  let child_span2 = spans[2]
  
  // 子span必须在父span的时间范围内
  assert_true(root_span.start_time <= child_span1.start_time)
  assert_true(child_span1.end_time <= root_span.end_time)
  
  assert_true(child_span1.start_time <= child_span2.start_time)
  assert_true(child_span2.end_time <= child_span1.end_time)
  
  // 验证持续时间合理性
  let root_duration = root_span.end_time - root_span.start_time
  let child1_duration = child_span1.end_time - child_span1.start_time
  let child2_duration = child_span2.end_time - child_span2.start_time
  
  assert_eq(root_duration, 100)
  assert_eq(child1_duration, 60)
  assert_eq(child2_duration, 40)
  
  // 子span持续时间不应超过父span
  assert_true(child1_duration <= root_duration)
  assert_true(child2_duration <= child1_duration)
}

test "分布式追踪上下文传播一致性测试" {
  // 模拟跨服务上下文传播
  let original_context = azimuth::Context {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "b7ad6b7169203331",
    baggage: [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("session.id", "sess-abcde")
    ],
    trace_flags: "01"
  }
  
  // 服务A注入上下文到HTTP头
  let injected_headers = azimuth::inject_context_to_headers(original_context)
  
  // 验证注入的头部
  assert_true(injected_headers.contains_key("traceparent"))
  assert_true(injected_headers.contains_key("tracestate"))
  assert_true(injected_headers.contains_key("baggage"))
  
  // 服务B从HTTP头提取上下文
  let extracted_context = azimuth::extract_context_from_headers(injected_headers)
  
  // 验证上下文一致性
  assert_eq(extracted_context.trace_id, original_context.trace_id)
  assert_eq(extracted_context.span_id, original_context.span_id)
  assert_eq(extracted_context.trace_flags, original_context.trace_flags)
  
  // 验证baggage传播
  assert_eq(extracted_context.baggage.length(), original_context.baggage.length())
  
  for baggage_item in original_context.baggage {
    let (key, value) = baggage_item
    let extracted_value = extracted_context.baggage.find(fn(item) { item.0 == key })
    match extracted_value {
      Some((_, v)) => assert_eq(v, value)
      None => assert_true(false)
    }
  }
}

test "分布式追踪采样一致性验证测试" {
  // 测试采样决策在分布式系统中的传播
  let trace_id = "trace123456"
  
  // 根决策：采样
  let sampled_context = azimuth::SamplingDecision {
    trace_id: trace_id,
    sampled: true,
    decision_reason: "probability_based",
    sample_rate: 0.1
  }
  
  // 验证子服务遵循采样决策
  let service_a_decision = azimuth::make_sampling_decision(trace_id, sampled_context)
  assert_true(service_a_decision.sampled)
  assert_eq(service_a_decision.decision_reason, "parent_decision")
  
  let service_b_decision = azimuth::make_sampling_decision(trace_id, service_a_decision)
  assert_true(service_b_decision.sampled)
  assert_eq(service_b_decision.decision_reason, "parent_decision")
  
  // 根决策：不采样
  let not_sampled_context = azimuth::SamplingDecision {
    trace_id: trace_id + "nosample",
    sampled: false,
    decision_reason: "probability_based",
    sample_rate: 0.1
  }
  
  let service_c_decision = azimuth::make_sampling_decision(trace_id + "nosample", not_sampled_context)
  assert_false(service_c_decision.sampled)
  assert_eq(service_c_decision.decision_reason, "parent_decision")
  
  // 验证采样一致性
  let all_spans = [
    azimuth::create_span("root", sampled_context),
    azimuth::create_span("service.a", service_a_decision),
    azimuth::create_span("service.b", service_b_decision)
  ]
  
  for span in all_spans {
    assert_eq(span.span_context.sampled, sampled_context.sampled)
  }
}

test "分布式追踪属性传播一致性测试" {
  // 测试关键属性在分布式追踪中的传播
  let root_attributes = [
    ("service.name", "user-service"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "production"),
    ("host.name", "host-01"),
    ("user.id", "12345"),
    ("request.method", "GET"),
    ("request.path", "/api/users/12345")
  ]
  
  // 创建根span
  let root_span = azimuth::Span {
    span_context: azimuth::SpanContext {
      trace_id: "trace123",
      span_id: "span1",
      parent_span_id: None,
      sampled: true,
      trace_state: ""
    },
    operation_name: "user.get",
    start_time: 1634567890000,
    end_time: 1634567890100,
    status: azimuth::SpanStatus::Ok,
    attributes: root_attributes
  }
  
  // 创建子span，继承关键属性
  let child_span = azimuth::create_child_span(root_span, "database.query")
  
  // 验证关键属性传播
  let inherited_keys = ["service.name", "service.version", "deployment.environment", "user.id"]
  
  for key in inherited_keys {
    let root_value = root_span.attributes.find(fn(attr) { attr.0 == key })
    let child_value = child_span.attributes.find(fn(attr) { attr.0 == key })
    
    match (root_value, child_value) {
      (Some((_, root_val)), Some((_, child_val))) => assert_eq(root_val, child_val)
      _ => assert_true(false)
    }
  }
  
  // 验证子span的独有属性
  let child_specific = child_span.attributes.find(fn(attr) { attr.0 == "db.operation" })
  match child_specific {
    Some((_, value)) => assert_eq(value, "SELECT")
    None => assert_true(false)
  }
}

test "分布式追踪错误传播一致性测试" {
  // 测试错误信息在分布式追踪中的传播
  let error_span = azimuth::Span {
    span_context: azimuth::SpanContext {
      trace_id: "error-trace",
      span_id: "error-span",
      parent_span_id: None,
      sampled: true,
      trace_state: ""
    },
    operation_name: "failing.operation",
    start_time: 1634567890000,
    end_time: 1634567890050,
    status: azimuth::SpanStatus::Error,
    attributes: [
      ("error.type", "DatabaseConnectionError"),
      ("error.message", "Connection timeout after 30 seconds"),
      ("error.stack", "db.connect() -> timeout"),
      ("error.code", "DB_CONN_TIMEOUT")
    ]
  }
  
  // 创建子span，继承错误信息
  let child_error_span = azimuth::create_child_span(error_span, "error.recovery")
  
  // 验证错误状态传播
  match child_error_span.status {
    azimuth::SpanStatus::Error => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证错误属性传播
  let error_type = child_error_span.attributes.find(fn(attr) { attr.0 == "error.type" })
  match error_type {
    Some((_, value)) => assert_eq(value, "DatabaseConnectionError")
    None => assert_true(false)
  }
  
  // 验证错误链完整性
  let error_chain = azimuth::build_error_chain([error_span, child_error_span])
  assert_eq(error_chain.length(), 2)
  assert_eq(error_chain[0].operation_name, "failing.operation")
  assert_eq(error_chain[1].operation_name, "error.recovery")
}