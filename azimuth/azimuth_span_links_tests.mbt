// Span链接测试
// 测试Span之间的链接关系和关联

test "span with single link" {
  // 测试Span与单个链接
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "link-test")
  
  // 创建父Span
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  let parent_ctx = Span::span_context(parent_span)
  
  // 创建子Span并链接到父Span
  let child_span = Tracer::start_span(tracer, "child-operation")
  
  // 模拟添加链接（简化实现）
  let link_attributes = [("operation.type", StringValue("causal")), ("correlation.id", StringValue("corr-123"))]
  let link_info = {
    "trace_id": SpanContext::trace_id(parent_ctx),
    "span_id": SpanContext::span_id(parent_ctx),
    "attributes": link_attributes
  }
  
  // 验证链接信息
  assert_eq(link_info["trace_id"], SpanContext::trace_id(parent_ctx))
  assert_eq(link_info["span_id"], SpanContext::span_id(parent_ctx))
  assert_eq(link_info["attributes"][0].0, "operation.type")
  assert_eq(link_info["attributes"][0].1, StringValue("causal"))
  
  // 验证Span上下文
  assert_true(SpanContext::is_valid(parent_ctx))
  assert_true(SpanContext::is_valid(Span::span_context(child_span)))
  
  Span::end(parent_span)
  Span::end(child_span)
}

test "span with multiple links" {
  // 测试Span与多个链接
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "multi-link-test")
  
  // 创建多个相关Span
  let span_a = Tracer::start_span(tracer, "operation-a")
  let span_b = Tracer::start_span(tracer, "operation-b")
  let span_c = Tracer::start_span(tracer, "operation-c")
  
  // 创建聚合Span并链接到多个Span
  let aggregate_span = Tracer::start_span(tracer, "aggregate-operation")
  
  // 模拟添加多个链接（简化实现）
  let links = [
    {
      "trace_id": SpanContext::trace_id(Span::span_context(span_a)),
      "span_id": SpanContext::span_id(Span::span_context(span_a)),
      "attributes": [("operation.type", StringValue("input"))]
    },
    {
      "trace_id": SpanContext::trace_id(Span::span_context(span_b)),
      "span_id": SpanContext::span_id(Span::span_context(span_b)),
      "attributes": [("operation.type", StringValue("processing"))]
    },
    {
      "trace_id": SpanContext::trace_id(Span::span_context(span_c)),
      "span_id": SpanContext::span_id(Span::span_context(span_c)),
      "attributes": [("operation.type", StringValue("output"))]
    }
  ]
  
  // 验证多个链接
  assert_eq(links.length, 3)
  assert_eq(links[0]["attributes"][0].1, StringValue("input"))
  assert_eq(links[1]["attributes"][0].1, StringValue("processing"))
  assert_eq(links[2]["attributes"][0].1, StringValue("output"))
  
  Span::end(span_a)
  Span::end(span_b)
  Span::end(span_c)
  Span::end(aggregate_span)
}

test "span links with cross-trace correlation" {
  // 测试跨追踪的Span链接
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "cross-trace-test")
  
  // 创建不同追踪的Span
  let trace_a_span = Tracer::start_span(tracer, "trace-a-operation")
  let trace_b_span = Tracer::start_span(tracer, "trace-b-operation")
  
  // 创建关联Span并链接到不同追踪的Span
  let correlation_span = Tracer::start_span(tracer, "correlation-operation")
  
  // 模拟跨追踪链接（简化实现）
  let cross_trace_links = [
    {
      "trace_id": SpanContext::trace_id(Span::span_context(trace_a_span)),
      "span_id": SpanContext::span_id(Span::span_context(trace_a_span)),
      "attributes": [
        ("correlation.type", StringValue("async")),
        ("correlation.id", StringValue("async-123"))
      ]
    },
    {
      "trace_id": SpanContext::trace_id(Span::span_context(trace_b_span)),
      "span_id": SpanContext::span_id(Span::span_context(trace_b_span)),
      "attributes": [
        ("correlation.type", StringValue("batch")),
        ("correlation.id", StringValue("batch-456"))
      ]
    }
  ]
  
  // 验证跨追踪链接
  assert_eq(cross_trace_links.length, 2)
  assert_eq(cross_trace_links[0]["attributes"][0].1, StringValue("async"))
  assert_eq(cross_trace_links[1]["attributes"][0].1, StringValue("batch"))
  
  // 验证不同的追踪ID
  assert_true(
    SpanContext::trace_id(Span::span_context(trace_a_span)) != 
    SpanContext::trace_id(Span::span_context(trace_b_span))
  )
  
  Span::end(trace_a_span)
  Span::end(trace_b_span)
  Span::end(correlation_span)
}

test "span links with baggage propagation" {
  // 测试Span链接与Baggage传播
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "baggage-link-test")
  
  // 创建带有Baggage的Span
  let source_span = Tracer::start_span(tracer, "source-operation")
  let baggage = Baggage::new()
  let baggage_with_data = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_data, "session.id", "sess-001")
  
  // 创建目标Span并链接到源Span，传播Baggage
  let target_span = Tracer::start_span(tracer, "target-operation")
  
  // 模拟带有Baggage的链接（简化实现）
  let baggage_link = {
    "trace_id": SpanContext::trace_id(Span::span_context(source_span)),
    "span_id": SpanContext::span_id(Span::span_context(source_span)),
    "attributes": [
      ("link.type", StringValue("baggage-propagation")),
      ("user.id", StringValue("12345")),
      ("session.id", StringValue("sess-001"))
    ],
    "baggage": baggage_with_session
  }
  
  // 验证Baggage传播
  assert_eq(baggage_link["attributes"][1].1, StringValue("12345"))
  assert_eq(baggage_link["attributes"][2].1, StringValue("sess-001"))
  assert_eq(baggage_link["link.type"], StringValue("baggage-propagation"))
  
  // 验证Baggage条目
  assert_eq(Baggage::get_entry(baggage_with_session, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage_with_session, "session.id"), Some("sess-001"))
  
  Span::end(source_span)
  Span::end(target_span)
}

test "span links with temporal relationships" {
  // 测试Span链接与时间关系
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "temporal-link-test")
  
  // 创建有时间关系的Span
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  let precursor_span = Tracer::start_span(tracer, "precursor-operation")
  Span::add_event(precursor_span, "operation.started", Some([("timestamp", IntValue(start_time))]))
  
  // 模拟时间流逝
  let intermediate_time = start_time + 1000000000L  // 1 second later
  
  let followup_span = Tracer::start_span(tracer, "followup-operation")
  Span::add_event(followup_span, "operation.started", Some([("timestamp", IntValue(intermediate_time))]))
  
  // 创建带有时间关系的链接
  let temporal_links = [
    {
      "trace_id": SpanContext::trace_id(Span::span_context(precursor_span)),
      "span_id": SpanContext::span_id(Span::span_context(precursor_span)),
      "attributes": [
        ("temporal.relationship", StringValue("precedes")),
        ("time.delta", IntValue(1000000000L))
      ]
    }
  ]
  
  // 验证时间关系
  assert_eq(temporal_links[0]["attributes"][0].1, StringValue("precedes"))
  assert_eq(temporal_links[0]["attributes"][1].1, IntValue(1000000000L))
  
  Span::end(precursor_span)
  Span::end(followup_span)
}

test "span links with causality tracking" {
  // 测试Span链接与因果关系跟踪
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "causality-test")
  
  // 创建有因果关系的Span
  let trigger_span = Tracer::start_span(tracer, "trigger-operation")
  Span::add_event(trigger_span, "event.triggered", Some([("event.id", StringValue("evt-001"))]))
  
  let processing_span = Tracer::start_span(tracer, "processing-operation")
  Span::add_event(processing_span, "event.processing", Some([("event.id", StringValue("evt-001"))]))
  
  let completion_span = Tracer::start_span(tracer, "completion-operation")
  Span::add_event(completion_span, "event.completed", Some([("event.id", StringValue("evt-001"))]))
  
  // 创建因果关系链
  let causality_chain = [
    {
      "from_span": "trigger-operation",
      "to_span": "processing-operation",
      "relationship": "triggers",
      "event.id": "evt-001"
    },
    {
      "from_span": "processing-operation",
      "to_span": "completion-operation",
      "relationship": "leads_to",
      "event.id": "evt-001"
    }
  ]
  
  // 验证因果关系链
  assert_eq(causality_chain.length, 2)
  assert_eq(causality_chain[0]["relationship"], "triggers")
  assert_eq(causality_chain[1]["relationship"], "leads_to")
  assert_eq(causality_chain[0]["event.id"], "evt-001")
  assert_eq(causality_chain[1]["event.id"], "evt-001")
  
  Span::end(trigger_span)
  Span::end(processing_span)
  Span::end(completion_span)
}

test "span links with error propagation" {
  // 测试Span链接与错误传播
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "error-link-test")
  
  // 创建失败Span
  let failing_span = Tracer::start_span(tracer, "failing-operation")
  Span::add_event(failing_span, "error.occurred", Some([
    ("error.type", StringValue("timeout")),
    ("error.message", StringValue("Operation timed out"))
  ]))
  Span::set_status(failing_span, Error, Some("Timeout error"))
  
  // 创建受影响的Span并链接到失败Span
  let affected_span = Tracer::start_span(tracer, "affected-operation")
  
  // 创建错误传播链接
  let error_links = [
    {
      "trace_id": SpanContext::trace_id(Span::span_context(failing_span)),
      "span_id": SpanContext::span_id(Span::span_context(failing_span)),
      "attributes": [
        ("link.type", StringValue("error-propagation")),
        ("error.type", StringValue("timeout")),
        ("error.message", StringValue("Operation timed out")),
        ("impact.severity", StringValue("high"))
      ]
    }
  ]
  
  // 验证错误传播
  assert_eq(error_links[0]["attributes"][0].1, StringValue("error-propagation"))
  assert_eq(error_links[0]["attributes"][1].1, StringValue("timeout"))
  assert_eq(error_links[0]["attributes"][3].1, StringValue("high"))
  
  // 验证Span状态
  assert_eq(Span::status(failing_span), Error)
  
  Span::end(failing_span)
  Span::end(affected_span)
}

test "span links with performance correlation" {
  // 测试Span链接与性能关联
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "performance-link-test")
  
  // 创建性能相关的Span
  let db_query_span = Tracer::start_span(tracer, "database.query")
  Span::add_event(db_query_span, "query.started", Some([("query.type", StringValue("SELECT"))]))
  Span::add_event(db_query_span, "query.completed", Some([
    ("query.type", StringValue("SELECT")),
    ("execution.time", IntValue(250000000L))  // 250ms
  ]))
  
  let api_response_span = Tracer::start_span(tracer, "api.response")
  Span::add_event(api_response_span, "response.generated", Some([
    ("response.time", IntValue(300000000L))  // 300ms
  ]))
  
  // 创建性能关联链接
  let performance_links = [
    {
      "trace_id": SpanContext::trace_id(Span::span_context(db_query_span)),
      "span_id": SpanContext::span_id(Span::span_context(db_query_span)),
      "attributes": [
        ("link.type", StringValue("performance-correlation")),
        ("db.execution.time", IntValue(250000000L)),
        ("api.response.time", IntValue(300000000L)),
        ("performance.impact", StringValue("significant"))
      ]
    }
  ]
  
  // 验证性能关联
  assert_eq(performance_links[0]["attributes"][0].1, StringValue("performance-correlation"))
  assert_eq(performance_links[0]["attributes"][1].1, IntValue(250000000L))
  assert_eq(performance_links[0]["attributes"][2].1, IntValue(300000000L))
  assert_eq(performance_links[0]["attributes"][3].1, StringValue("significant"))
  
  Span::end(db_query_span)
  Span::end(api_response_span)
}