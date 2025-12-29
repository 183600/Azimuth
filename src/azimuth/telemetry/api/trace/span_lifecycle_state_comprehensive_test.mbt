test "Span lifecycle and state management test" {
  // 测试SpanContext创建
  let trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 
                  9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  let trace_flags = 1_byte
  let trace_state = "rojo=00f067aa0ba902b7,congo=61afbaf20a2c42b0"
  
  let span_context = SpanContext{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: trace_flags,
    trace_state: trace_state
  }
  
  assert_eq(span_context.trace_id, trace_id)
  assert_eq(span_context.span_id, span_id)
  assert_eq(span_context.trace_flags, trace_flags)
  assert_eq(span_context.trace_state, trace_state)
  
  // 测试所有SpanKind
  let internal_span = Span{
    name: "internal-operation",
    context: span_context,
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(internal_span.kind, Internal)
  
  let server_span = Span{
    name: "server-operation",
    context: span_context,
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(server_span.kind, Server)
  
  let client_span = Span{
    name: "client-operation",
    context: span_context,
    kind: Client,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(client_span.kind, Client)
  
  let producer_span = Span{
    name: "producer-operation",
    context: span_context,
    kind: Producer,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(producer_span.kind, Producer)
  
  let consumer_span = Span{
    name: "consumer-operation",
    context: span_context,
    kind: Consumer,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(consumer_span.kind, Consumer)
  
  // 测试Span状态转换
  let mut mutable_span = Span{
    name: "mutable-span",
    context: span_context,
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  // 初始状态
  assert_eq(mutable_span.status, Unset)
  assert_eq(mutable_span.end_time_unix_nanos, None)
  
  // 设置为Ok状态
  mutable_span.status = Ok
  mutable_span.status_description = Some("Operation completed successfully")
  assert_eq(mutable_span.status, Ok)
  match mutable_span.status_description {
    Some(desc) => assert_eq(desc, "Operation completed successfully")
    None => @test.fail("Expected status_description to be Some")
  }
  
  // 设置为Error状态
  mutable_span.status = Error
  mutable_span.status_description = Some("Operation failed")
  assert_eq(mutable_span.status, Error)
  match mutable_span.status_description {
    Some(desc) => assert_eq(desc, "Operation failed")
    None => @test.fail("Expected status_description to be Some")
  }
  
  // 完成Span
  mutable_span.end_time_unix_nanos = Some(2000000L)
  match mutable_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 2000000L)
    None => @test.fail("Expected end_time_unix_nanos to be Some")
  }
  
  // 测试带父Span的Span
  let parent_span_id = [9_byte, 9_byte, 9_byte, 9_byte, 9_byte, 9_byte, 9_byte, 9_byte]
  let child_span = trace::Span{
    name: "child-operation",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: parent_span_id,
    start_time_unix_nanos: 1500000L,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  match child_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, parent_span_id)
    None => @test.fail("Expected parent_span_id to be Some")
  }
  
  // 测试带有属性的Span
  let span_with_attrs = trace::Span{
    name: "span-with-attributes",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [
      ("user_id", common::AttributeValue::int(12345L)),
      ("operation", common::AttributeValue::string("process_data")),
      ("duration_ms", common::AttributeValue::float(150.5)),
      ("success", common::AttributeValue::bool(true))
    ],
    events: [],
    links: []
  }
  
  assert_eq(span_with_attrs.attributes.length(), 4)
  
  // 验证属性
  let user_id_attr = span_with_attrs.attributes[0]
  assert_eq(user_id_attr.0, "user_id")
  match user_id_attr.1 {
    common::IntValue(id) => assert_eq(id, 12345L)
    _ => @test.fail("Expected IntValue for user_id")
  }
  
  let operation_attr = span_with_attrs.attributes[1]
  assert_eq(operation_attr.0, "operation")
  match operation_attr.1 {
    common::StringValue(op) => assert_eq(op, "process_data")
    _ => @test.fail("Expected StringValue for operation")
  }
  
  let duration_attr = span_with_attrs.attributes[2]
  assert_eq(duration_attr.0, "duration_ms")
  match duration_attr.1 {
    common::FloatValue(d) => assert_eq(d, 150.5)
    _ => @test.fail("Expected FloatValue for duration_ms")
  }
  
  let success_attr = span_with_attrs.attributes[3]
  assert_eq(success_attr.0, "success")
  match success_attr.1 {
    common::BoolValue(s) => assert_eq(s, true)
    _ => @test.fail("Expected BoolValue for success")
  }
  
  // 测试带有事件的Span
  let span_event = trace::SpanEvent{
    name: "database.query",
    timestamp_unix_nanos: 1100000L,
    attributes: [
      ("db.statement", common::AttributeValue::string("SELECT * FROM users")),
      ("db.type", common::AttributeValue::string("postgresql")),
      ("db.duration_ms", common::AttributeValue::float(25.3))
    ]
  }
  
  let span_with_events = trace::Span{
    name: "span-with-events",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [span_event],
    links: []
  }
  
  assert_eq(span_with_events.events.length(), 1)
  let event = span_with_events.events[0]
  assert_eq(event.name, "database.query")
  assert_eq(event.timestamp_unix_nanos, 1100000L)
  assert_eq(event.attributes.length(), 3)
  
  // 测试带有链接的Span
  let linked_context = trace::SpanContext{
    trace_id: [2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte,
                2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte],
    span_id: [2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte],
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let span_link = trace::SpanLink{
    context: linked_context,
    attributes: [
      ("link.type", common::AttributeValue::string("parent")),
      ("relationship", common::AttributeValue::string("follows_from"))
    ]
  }
  
  let span_with_links = trace::Span{
    name: "span-with-links",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: [span_link]
  }
  
  assert_eq(span_with_links.links.length(), 1)
  let link = span_with_links.links[0]
  assert_eq(link.context.trace_id, linked_context.trace_id)
  assert_eq(link.context.span_id, linked_context.span_id)
  assert_eq(link.attributes.length(), 2)
  
  // 测试NoopTracer
  let noop_tracer = trace::NoopTracer::{}
  let dummy_context = context::Context::{}
  
  let (ctx, span) = noop_tracer.start_span(
    dummy_context,
    "test-span",
    Some(trace::Internal),
    [("test_attr", common::AttributeValue::string("test_value"))],
    Some(1000000L)
  )
  
  assert_eq(span.name, "test-span")
  assert_eq(span.kind, trace::Internal)
  assert_eq(span.start_time_unix_nanos, 1000000L)
  assert_eq(span.attributes.length(), 1)
  assert_eq(span.attributes[0].0, "test_attr")
  match span.attributes[0].1 {
    common::StringValue(val) => assert_eq(val, "test_value")
    _ => @test.fail("Expected StringValue for test_attr")
  }
  
  // 测试NoopTracerProvider
  let noop_provider = trace::NoopTracerProvider::{}
  let tracer = noop_provider.get_tracer("test-tracer", Some("1.0.0"))
  
  // 验证返回的tracer可以正常工作
  let (ctx2, span2) = tracer.start_span(
    dummy_context,
    "another-test-span",
    Some(trace::Server),
    [],
    None
  )
  
  assert_eq(span2.name, "another-test-span")
  assert_eq(span2.kind, trace::Server)
  assert_eq(span2.start_time_unix_nanos, 0L)  // 默认值
  
  // 测试边界条件
  let empty_name_span = trace::Span{
    name: "",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(empty_name_span.name, "")
  
  // 测试最大时间戳
  let max_time_span = trace::Span{
    name: "max-time-span",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 9223372036854775807L,  // Int64最大值
    end_time_unix_nanos: Some(9223372036854775807L),
    status: trace::Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(max_time_span.start_time_unix_nanos, 9223372036854775807L)
  match max_time_span.end_time_unix_nanos {
    Some(time) => assert_eq(time, 9223372036854775807L)
    None => @test.fail("Expected end_time_unix_nanos to be Some")
  }
  
  // 测试长Span名称
  let long_name = "this-is-a-very-long-span-name-that-tests-boundary-conditions-and-ensures-that-the-system-can-handle-span-names-of-significant-length"
  let long_name_span = trace::Span{
    name: long_name,
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(long_name_span.name, long_name)
  assert_eq(long_name_span.name.length(), 143)
}