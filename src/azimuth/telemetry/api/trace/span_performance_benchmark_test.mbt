// Span性能基准测试
// 测试Span的性能，包括创建、操作和销毁的性能基准

test "span_creation_performance_benchmark" {
  // 测试Span创建的性能基准
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 基准测试：创建大量简单的span
  let start_time = 1000L
  let mut i = 0
  while i < 1000 {
    let (_, span) = tracer.start_span(ctx, "perf_span_" + i.to_string(), Some(Server), None, Some(start_time + i))
    
    // 验证span正确创建
    assert_eq(span.name, "perf_span_" + i.to_string())
    assert_eq(span.kind, Server)
    assert_eq(span.start_time_unix_nanos, start_time + i)
    
    i = i + 1
  }
  
  // 基准测试：创建带属性的span
  let attributes = [
    ("service.name", AttributeValue::string("performance-test")),
    ("operation.type", AttributeValue::string("benchmark")),
    ("iteration", AttributeValue::int(0L))
  ]
  
  i = 0
  while i < 500 {
    let iter_attributes = [
      ("service.name", AttributeValue::string("performance-test")),
      ("operation.type", AttributeValue::string("benchmark")),
      ("iteration", AttributeValue::int(i.to_int64()))
    ]
    
    let (_, span) = tracer.start_span(ctx, "attr_span_" + i.to_string(), Some(Client), Some(iter_attributes))
    
    // 验证span正确创建
    assert_eq(span.name, "attr_span_" + i.to_string())
    assert_eq(span.kind, Client)
    assert_eq(span.attributes.length(), 3)
    
    i = i + 1
  }
}

test "span_attribute_operations_performance" {
  // 测试Span属性操作的性能
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建包含大量属性的span
  let mut large_attributes = []
  let mut i = 0
  while i < 100 {
    let key = "attr.key_" + i.to_string()
    let value = AttributeValue::string("attr.value_" + i.to_string())
    large_attributes.push((key, value))
    i = i + 1
  }
  
  let (_, large_span) = tracer.start_span(ctx, "large_attributes_span", Some(Server), Some(large_attributes))
  
  // 验证大量属性的正确性
  assert_eq(large_span.name, "large_attributes_span")
  assert_eq(large_span.attributes.length(), 100)
  
  // 测试复杂属性类型的性能
  let complex_attributes = [
    ("string.basic", AttributeValue::string("basic_value")),
    ("int.basic", AttributeValue::int(42L)),
    ("float.basic", AttributeValue::float(3.14159)),
    ("bool.basic", AttributeValue::bool(true)),
    ("string.array", AttributeValue::array_string(["item1", "item2", "item3"])),
    ("int.array", AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])),
    ("float.array", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array", AttributeValue::array_bool([true, false, true])),
    ("unicode.value", AttributeValue::string("Hello 世界 🌍")),
    ("json.value", AttributeValue::string("{\"key\":\"value\",\"number\":42}"))
  ]
  
  let (_, complex_span) = tracer.start_span(ctx, "complex_attributes_span", Some(Internal), Some(complex_attributes))
  
  // 验证复杂属性的正确性
  assert_eq(complex_span.name, "complex_attributes_span")
  assert_eq(complex_span.attributes.length(), 10)
}

test "span_events_operations_performance" {
  // 测试Span事件操作的性能
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建包含大量事件的span
  let mut large_events = []
  let mut i = 0
  while i < 50 {
    let event = SpanEvent::{
      name: "event_" + i.to_string(),
      timestamp_unix_nanos: 1000L + i,
      attributes: [
        ("event.index", AttributeValue::int(i.to_int64())),
        ("event.type", AttributeValue::string("benchmark_event"))
      ]
    }
    large_events.push(event)
    i = i + 1
  }
  
  let event_span = Span::{
    name: "large_events_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: large_events,
    links: []
  }
  
  // 验证大量事件的正确性
  assert_eq(event_span.name, "large_events_span")
  assert_eq(event_span.events.length(), 50)
  
  // 验证事件的时间顺序
  i = 0
  while i < event_span.events.length() - 1 {
    assert_eq(event_span.events[i].timestamp_unix_nanos < event_span.events[i + 1].timestamp_unix_nanos, true)
    i = i + 1
  }
  
  // 测试复杂事件的性能
  let complex_events = [
    SpanEvent::{
      name: "error_event",
      timestamp_unix_nanos: 1000L,
      attributes: [
        ("error.type", AttributeValue::string("timeout")),
        ("error.message", AttributeValue::string("Operation timed out after 30 seconds")),
        ("error.code", AttributeValue::int(503L)),
        ("error.retries", AttributeValue::int(3L)),
        ("error.stack_trace", AttributeValue::string("at Service.process (service.js:123)\nat Controller.handle (controller.js:456)"))
      ]
    },
    SpanEvent::{
      name: "metric_event",
      timestamp_unix_nanos: 1100L,
      attributes: [
        ("metric.name", AttributeValue::string("response.time")),
        ("metric.value", AttributeValue::float(250.5)),
        ("metric.unit", AttributeValue::string("milliseconds")),
        ("metric.tags", AttributeValue::array_string(["api", "users", "get"])),
        ("metric.threshold", AttributeValue::float(1000.0))
      ]
    },
    SpanEvent::{
      name: "business_event",
      timestamp_unix_nanos: 1200L,
      attributes: [
        ("business.process", AttributeValue::string("user_registration")),
        ("business.step", AttributeValue::string("email_verification")),
        ("business.result", AttributeValue::string("success")),
        ("business.user_id", AttributeValue::string("user12345")),
        ("business.timestamp", AttributeValue::int(1640995200000L))
      ]
    }
  ]
  
  let complex_event_span = Span::{
    name: "complex_events_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x02_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Client,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: complex_events,
    links: []
  }
  
  // 验证复杂事件的正确性
  assert_eq(complex_event_span.name, "complex_events_span")
  assert_eq(complex_event_span.events.length(), 3)
  assert_eq(complex_event_span.events[0].attributes.length(), 5)
  assert_eq(complex_event_span.events[1].attributes.length(), 5)
  assert_eq(complex_event_span.events[2].attributes.length(), 5)
}

test "span_links_operations_performance" {
  // 测试Span链接操作的性能
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建包含大量链接的span
  let mut large_links = []
  let mut i = 0
  while i < 20 {
    let linked_context = SpanContext::{
      trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { (i + j).to_byte() }),
      span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { (i + j).to_byte() }),
      trace_flags: 1_byte,
      trace_state: ""
    }
    
    let link = SpanLink::{
      context: linked_context,
      attributes: [
        ("link.index", AttributeValue::int(i.to_int64())),
        ("link.type", AttributeValue::string("causality"))
      ]
    }
    large_links.push(link)
    i = i + 1
  }
  
  let link_span = Span::{
    name: "large_links_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Producer,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: large_links
  }
  
  // 验证大量链接的正确性
  assert_eq(link_span.name, "large_links_span")
  assert_eq(link_span.links.length(), 20)
  
  // 测试复杂链接的性能
  let complex_linked_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte }),
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  let complex_links = [
    SpanLink::{
      context: complex_linked_context,
      attributes: [
        ("link.type", AttributeValue::string("correlation")),
        ("related.service", AttributeValue::string("auth-service")),
        ("related.operation", AttributeValue::string("user_authentication")),
        ("correlation.id", AttributeValue::string("corr-12345")),
        ("causality.type", AttributeValue::string("parent_child"))
      ]
    }
  ]
  
  let complex_link_span = Span::{
    name: "complex_links_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x02_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Consumer,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: complex_links
  }
  
  // 验证复杂链接的正确性
  assert_eq(complex_link_span.name, "complex_links_span")
  assert_eq(complex_link_span.links.length(), 1)
  assert_eq(complex_link_span.links[0].attributes.length(), 5)
}

test "span_nested_creation_performance" {
  // 测试嵌套Span创建的性能
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建深层嵌套的span层次结构
  let mut current_ctx = ctx
  let mut i = 0
  while i < 10 {
    let (new_ctx, span) = tracer.start_span(current_ctx, "nested_span_" + i.to_string(), Some(Server), None)
    
    // 验证span正确创建
    assert_eq(span.name, "nested_span_" + i.to_string())
    assert_eq(span.kind, Server)
    
    current_ctx = new_ctx
    i = i + 1
  }
  
  // 创建宽层嵌套的span结构（一个父span有多个子span）
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_span", Some(Server), None)
  
  let mut j = 0
  while j < 20 {
    let (_, child_span) = tracer.start_span(parent_ctx, "child_span_" + j.to_string(), Some(Client), None)
    
    // 验证子span正确创建
    assert_eq(child_span.name, "child_span_" + j.to_string())
    assert_eq(child_span.kind, Client)
    
    j = j + 1
  }
  
  // 验证父span正确创建
  assert_eq(parent_span.name, "parent_span")
  assert_eq(parent_span.kind, Server)
}

test "span_memory_usage_performance" {
  // 测试Span内存使用的性能
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建大量span来测试内存使用模式
  let mut spans = []
  let mut i = 0
  while i < 100 {
    let attributes = [
      ("span.index", AttributeValue::int(i.to_int64())),
      ("span.data", AttributeValue::string("data_" + i.to_string())),
      ("span.timestamp", AttributeValue::int((1000 + i).to_int64()))
    ]
    
    let (_, span) = tracer.start_span(ctx, "memory_test_span_" + i.to_string(), Some(Internal), Some(attributes))
    spans.push(span)
    
    i = i + 1
  }
  
  // 验证所有span都正确创建
  assert_eq(spans.length(), 100)
  
  // 验证span数据的完整性
  i = 0
  while i < spans.length() {
    let span = spans[i]
    assert_eq(span.name, "memory_test_span_" + i.to_string())
    assert_eq(span.kind, Internal)
    assert_eq(span.attributes.length(), 3)
    i = i + 1
  }
  
  // 测试大span的内存使用
  let large_attributes = []
  let mut k = 0
  while k < 50 {
    let key = "large.attr.key_" + k.to_string()
    let value = AttributeValue::string("large.attr.value." + k.to_string() + ".with.additional.data.to.increase.memory.usage")
    large_attributes.push((key, value))
    k = k + 1
  }
  
  let (_, large_span) = tracer.start_span(ctx, "large_memory_span", Some(Server), Some(large_attributes))
  
  // 验证大span正确创建
  assert_eq(large_span.name, "large_memory_span")
  assert_eq(large_span.attributes.length(), 50)
}

test "span_concurrent_operations_performance" {
  // 测试Span并发操作的性能
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 模拟并发创建span
  let mut concurrent_spans = []
  let mut i = 0
  while i < 50 {
    let (new_ctx, span) = tracer.start_span(ctx, "concurrent_span_" + i.to_string(), Some(Server), None)
    
    // 为每个span创建子span
    let (_, child_span) = tracer.start_span(new_ctx, "child_concurrent_span_" + i.to_string(), Some(Client), None)
    
    concurrent_spans.push((span, child_span))
    i = i + 1
  }
  
  // 验证所有并发span都正确创建
  assert_eq(concurrent_spans.length(), 50)
  
  // 验证父子span关系
  i = 0
  while i < concurrent_spans.length() {
    let (parent_span, child_span) = concurrent_spans[i]
    
    assert_eq(parent_span.name, "concurrent_span_" + i.to_string())
    assert_eq(parent_span.kind, Server)
    
    assert_eq(child_span.name, "child_concurrent_span_" + i.to_string())
    assert_eq(child_span.kind, Client)
    
    i = i + 1
  }
}

test "span_status_transitions_performance" {
  // 测试Span状态转换的性能
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 测试大量状态转换
  let mut i = 0
  while i < 100 {
    let (_, initial_span) = tracer.start_span(ctx, "status_test_span_" + i.to_string(), Some(Server), None)
    
    // 转换到Ok状态
    let ok_span = Span::{
      name: initial_span.name,
      context: initial_span.context,
      kind: initial_span.kind,
      parent_span_id: initial_span.parent_span_id,
      start_time_unix_nanos: initial_span.start_time_unix_nanos,
      end_time_unix_nanos: Some((1000 + i).to_int64()),
      status: Ok,
      status_description: Some("Operation completed successfully"),
      attributes: initial_span.attributes,
      events: initial_span.events,
      links: initial_span.links
    }
    
    // 转换到Error状态
    let error_span = Span::{
      name: ok_span.name,
      context: ok_span.context,
      kind: ok_span.kind,
      parent_span_id: ok_span.parent_span_id,
      start_time_unix_nanos: ok_span.start_time_unix_nanos,
      end_time_unix_nanos: Some((1100 + i).to_int64()),
      status: Error,
      status_description: Some("Operation failed"),
      attributes: [
        ("error.code", AttributeValue::string("ERR_" + i.to_string())),
        ("error.retries", AttributeValue::int(i.to_int64()))
      ],
      events: ok_span.events,
      links: ok_span.links
    }
    
    // 验证状态转换
    assert_eq(initial_span.status, Unset)
    assert_eq(ok_span.status, Ok)
    assert_eq(error_span.status, Error)
    
    match ok_span.status_description {
      Some(description) => assert_eq(description, "Operation completed successfully")
      None => @test.fail("Test failed")
    }
    
    match error_span.status_description {
      Some(description) => assert_eq(description, "Operation failed")
      None => @test.fail("Test failed")
    }
    
    i = i + 1
  }
}

test "span_comprehensive_performance_benchmark" {
  // 综合性能基准测试
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建复杂的span来测试综合性能
  let mut i = 0
  while i < 20 {
    // 复杂属性
    let complex_attributes = [
      ("service.name", AttributeValue::string("performance-service")),
      ("operation.name", AttributeValue::string("complex_operation_" + i.to_string())),
      ("iteration", AttributeValue::int(i.to_int64())),
      ("timestamp", AttributeValue::int((1000 + i).to_int64())),
      ("data.size", AttributeValue::int((1024 * i).to_int64())),
      ("processing.time", AttributeValue::float((100.5 + i.to_float()).to_float())),
      ("success.rate", AttributeValue::float((0.95 + i.to_float() * 0.001).to_float())),
      ("is.cached", AttributeValue::bool(i % 2 == 0)),
      ("tags", AttributeValue::array_string(["perf", "benchmark", "test"])),
      ("metrics", AttributeValue::array_float([1.0, 2.0, 3.0, 4.0, 5.0]))
    ]
    
    // 复杂事件
    let complex_events = [
      SpanEvent::{
        name: "operation.start",
        timestamp_unix_nanos: (1000 + i).to_int64(),
        attributes: [
          ("event.type", AttributeValue::string("lifecycle")),
          ("phase", AttributeValue::string("beginning"))
        ]
      },
      SpanEvent::{
        name: "operation.progress",
        timestamp_unix_nanos: (1050 + i).to_int64(),
        attributes: [
          ("event.type", AttributeValue::string("progress")),
          ("completion", AttributeValue::float(0.5)),
          ("processed.items", AttributeValue::int((i * 10).to_int64()))
        ]
      },
      SpanEvent::{
        name: "operation.complete",
        timestamp_unix_nanos: (1100 + i).to_int64(),
        attributes: [
          ("event.type", AttributeValue::string("lifecycle")),
          ("phase", AttributeValue::string("end")),
          ("total.items", AttributeValue::int((i * 20).to_int64())),
          ("duration.ms", AttributeValue::int(100L))
        ]
      }
    ]
    
    // 复杂链接
    let linked_context = SpanContext::{
      trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { (i + j).to_byte() }),
      span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { (i + j).to_byte() }),
      trace_flags: 1_byte,
      trace_state: "key=value"
    }
    
    let complex_links = [
      SpanLink::{
        context: linked_context,
        attributes: [
          ("link.type", AttributeValue::string("correlation")),
          ("related.service", AttributeValue::string("upstream-service")),
          ("trace.id", AttributeValue::string("trace_" + i.to_string()))
        ]
      }
    ]
    
    // 创建复杂span
    let complex_span = Span::{
      name: "comprehensive_performance_span_" + i.to_string(),
      context: SpanContext::{
        trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { (i * 2 + j).to_byte() }),
        span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { (i * 2 + j).to_byte() }),
        trace_flags: 1_byte,
        trace_state: ""
      },
      kind: Server,
      parent_span_id: None,
      start_time_unix_nanos: (1000 + i).to_int64(),
      end_time_unix_nanos: Some((1200 + i).to_int64()),
      status: Ok,
      status_description: Some("Comprehensive operation completed successfully"),
      attributes: complex_attributes,
      events: complex_events,
      links: complex_links
    }
    
    // 验证复杂span的正确性
    assert_eq(complex_span.name, "comprehensive_performance_span_" + i.to_string())
    assert_eq(complex_span.kind, Server)
    assert_eq(complex_span.attributes.length(), 10)
    assert_eq(complex_span.events.length(), 3)
    assert_eq(complex_span.links.length(), 1)
    assert_eq(complex_span.status, Ok)
    
    i = i + 1
  }
}