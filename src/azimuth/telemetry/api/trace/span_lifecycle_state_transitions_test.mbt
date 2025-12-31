// Span生命周期状态转换测试
// 测试Span在不同状态之间的转换和生命周期管理

test "span_initial_state_validation" {
  // 测试Span的初始状态
  let ctx = Context::empty()
  let (_, span) = NoopTracer::start_span(ctx, "initial_state_test")
  
  // 验证初始状态
  assert_eq(span.name, "initial_state_test")
  assert_eq(span.status, Unset)
  assert_eq(span.kind, Internal)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  assert_eq(span.attributes.length(), 0)
  
  // 验证时间戳
  assert_eq(span.start_time_unix_nanos, 0L)
  match span.end_time_unix_nanos {
    None => assert_eq(true, true)  // 初始状态下end_time应该为None
    Some(_) => @test.fail("Test failed")
  }
  
  // 验证父span关系
  match span.parent_span_id {
    None => assert_eq(true, true)  // 初始状态下没有父span
    Some(_) => @test.fail("Test failed")
  }
  
  // 验证状态描述
  match span.status_description {
    None => assert_eq(true, true)  // 初始状态下没有状态描述
    Some(_) => @test.fail("Test failed")
  }
}

test "span_unset_to_ok_transition" {
  // 测试从Unset状态到Ok状态的转换
  let ctx = Context::empty()
  let (_, initial_span) = NoopTracer::start_span(ctx, "unset_to_ok_test")
  
  // 验证初始状态
  assert_eq(initial_span.status, Unset)
  
  // 创建转换到Ok状态的span
  let ok_span = Span::{
    name: initial_span.name,
    context: initial_span.context,
    kind: initial_span.kind,
    parent_span_id: initial_span.parent_span_id,
    start_time_unix_nanos: initial_span.start_time_unix_nanos,
    end_time_unix_nanos: Some(1000L),  // 设置结束时间
    status: Ok,  // 状态转换为Ok
    status_description: Some("Operation completed successfully"),
    attributes: initial_span.attributes,
    events: initial_span.events,
    links: initial_span.links
  }
  
  // 验证状态转换
  assert_eq(ok_span.status, Ok)
  match ok_span.status_description {
    Some(description) => assert_eq(description, "Operation completed successfully")
    None => @test.fail("Test failed")
  }
  match ok_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 1000L)
    None => @test.fail("Test failed")
  }
}

test "span_unset_to_error_transition" {
  // 测试从Unset状态到Error状态的转换
  let ctx = Context::empty()
  let (_, initial_span) = NoopTracer::start_span(ctx, "unset_to_error_test")
  
  // 验证初始状态
  assert_eq(initial_span.status, Unset)
  
  // 添加错误事件
  let error_events = [
    SpanEvent::{
      name: "error_occurred",
      timestamp_unix_nanos: 500L,
      attributes: [
        ("error.type", AttributeValue::string("timeout")),
        ("error.message", AttributeValue::string("Operation timed out"))
      ]
    }
  ]
  
  // 创建转换到Error状态的span
  let error_span = Span::{
    name: initial_span.name,
    context: initial_span.context,
    kind: initial_span.kind,
    parent_span_id: initial_span.parent_span_id,
    start_time_unix_nanos: initial_span.start_time_unix_nanos,
    end_time_unix_nanos: Some(800L),  // 设置结束时间
    status: Error,  // 状态转换为Error
    status_description: Some("Operation failed due to timeout"),
    attributes: [
      ("error.code", AttributeValue::string("TIMEOUT")),
      ("error.retries", AttributeValue::int(3L))
    ],
    events: error_events,
    links: initial_span.links
  }
  
  // 验证状态转换
  assert_eq(error_span.status, Error)
  match error_span.status_description {
    Some(description) => assert_eq(description, "Operation failed due to timeout")
    None => @test.fail("Test failed")
  }
  match error_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 800L)
    None => @test.fail("Test failed")
  }
  assert_eq(error_span.attributes.length(), 2)
  assert_eq(error_span.events.length(), 1)
}

test "span_status_sequence_validation" {
  // 测试Span状态序列的合理性
  
  // 场景1: 正常流程 Unset -> Ok
  let normal_flow_span = Span::{
    name: "normal_flow",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(2000L),
    status: Ok,
    status_description: Some("Request processed successfully"),
    attributes: [
      ("http.method", AttributeValue::string("GET")),
      ("http.status_code", AttributeValue::int(200L))
    ],
    events: [],
    links: []
  }
  
  assert_eq(normal_flow_span.start_time_unix_nanos < normal_flow_span.end_time_unix_nanos.unwrap(), true)
  assert_eq(normal_flow_span.status, Ok)
  
  // 场景2: 异常流程 Unset -> Error
  let error_flow_span = Span::{
    name: "error_flow",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x02_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(1500L),
    status: Error,
    status_description: Some("Database connection failed"),
    attributes: [
      ("error.code", AttributeValue::string("DB_ERROR")),
      ("error.retries", AttributeValue::int(3L))
    ],
    events: [
      SpanEvent::{
        name: "connection_attempt",
        timestamp_unix_nanos: 1100L,
        attributes: [("attempt", AttributeValue::int(1L))]
      },
      SpanEvent::{
        name: "connection_failed",
        timestamp_unix_nanos: 1200L,
        attributes: [("error", AttributeValue::string("timeout"))]
      },
      SpanEvent::{
        name: "retry_attempt",
        timestamp_unix_nanos: 1300L,
        attributes: [("attempt", AttributeValue::int(2L))]
      },
      SpanEvent::{
        name: "final_failure",
        timestamp_unix_nanos: 1400L,
        attributes: [("error", AttributeValue::string("connection_refused"))]
      }
    ],
    links: []
  }
  
  assert_eq(error_flow_span.start_time_unix_nanos < error_flow_span.end_time_unix_nanos.unwrap(), true)
  assert_eq(error_flow_span.status, Error)
  assert_eq(error_flow_span.events.length(), 4)
  
  // 验证事件时间顺序
  assert_eq(error_flow_span.events[0].timestamp_unix_nanos < error_flow_span.events[1].timestamp_unix_nanos, true)
  assert_eq(error_flow_span.events[1].timestamp_unix_nanos < error_flow_span.events[2].timestamp_unix_nanos, true)
  assert_eq(error_flow_span.events[2].timestamp_unix_nanos < error_flow_span.events[3].timestamp_unix_nanos, true)
}

test "span_lifecycle_with_parent_child_relationship" {
  // 测试父子Span关系中的生命周期管理
  
  // 创建父span
  let parent_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x10_byte })
  let parent_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })
  
  let parent_span = Span::{
    name: "parent_operation",
    context: SpanContext::{
      trace_id: parent_trace_id,
      span_id: parent_span_id,
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(5000L),
    status: Ok,
    status_description: Some("Parent operation completed"),
    attributes: [
      ("operation.type", AttributeValue::string("batch_processing")),
      ("batch.size", AttributeValue::int(100L))
    ],
    events: [],
    links: []
  }
  
  // 创建子span
  let child_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte })
  
  let child_span = Span::{
    name: "child_operation",
    context: SpanContext::{
      trace_id: parent_trace_id,  // 相同的trace_id
      span_id: child_span_id,     // 不同的span_id
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: Some(parent_span_id),  // 设置父span_id
    start_time_unix_nanos: 1500L,  // 在父span的时间范围内
    end_time_unix_nanos: Some(3000L),
    status: Ok,
    status_description: Some("Child operation completed"),
    attributes: [
      ("operation.type", AttributeValue::string("data_validation")),
      ("records.processed", AttributeValue::int(25L))
    ],
    events: [],
    links: []
  }
  
  // 验证父子关系
  assert_eq(parent_span.context.trace_id, child_span.context.trace_id)
  assert_eq(parent_span.context.span_id, child_span.parent_span_id.unwrap())
  
  // 验证时间范围关系
  assert_eq(parent_span.start_time_unix_nanos <= child_span.start_time_unix_nanos, true)
  assert_eq(child_span.end_time_unix_nanos.unwrap() <= parent_span.end_time_unix_nanos.unwrap(), true)
  
  // 验证状态
  assert_eq(parent_span.status, Ok)
  assert_eq(child_span.status, Ok)
}

test "span_lifecycle_with_multiple_status_changes" {
  // 测试Span在生命周期中的多次状态变化
  
  let ctx = Context::empty()
  let (_, initial_span) = NoopTracer::start_span(ctx, "multi_status_test")
  
  // 阶段1: 初始状态
  assert_eq(initial_span.status, Unset)
  
  // 阶段2: 添加警告事件但状态仍为Unset
  let warning_span = Span::{
    name: initial_span.name,
    context: initial_span.context,
    kind: initial_span.kind,
    parent_span_id: initial_span.parent_span_id,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: Unset,  // 状态仍为Unset
    status_description: None,
    attributes: initial_span.attributes,
    events: [
      SpanEvent::{
        name: "warning",
        timestamp_unix_nanos: 1500L,
        attributes: [
          ("warning.type", AttributeValue::string("slow_response")),
          ("response_time_ms", AttributeValue::int(5000L))
        ]
      }
    ],
    links: initial_span.links
  }
  
  assert_eq(warning_span.status, Unset)
  assert_eq(warning_span.events.length(), 1)
  
  // 阶段3: 状态变为Error
  let error_span = Span::{
    name: warning_span.name,
    context: warning_span.context,
    kind: warning_span.kind,
    parent_span_id: warning_span.parent_span_id,
    start_time_unix_nanos: warning_span.start_time_unix_nanos,
    end_time_unix_nanos: Some(3000L),
    status: Error,
    status_description: Some("Operation failed after retries"),
    attributes: [
      ("error.code", AttributeValue::string("OPERATION_FAILED")),
      ("total.retries", AttributeValue::int(3L))
    ],
    events: [
      // 保留之前的警告事件
      warning_span.events[0],
      // 添加新的错误事件
      SpanEvent::{
        name: "error",
        timestamp_unix_nanos: 2500L,
        attributes: [
          ("error.type", AttributeValue::string("connection_failed")),
          ("final_attempt", AttributeValue::bool(true))
        ]
      }
    ],
    links: warning_span.links
  }
  
  assert_eq(error_span.status, Error)
  match error_span.status_description {
    Some(description) => assert_eq(description, "Operation failed after retries")
    None => @test.fail("Test failed")
  }
  assert_eq(error_span.events.length(), 2)
  assert_eq(error_span.events[0].name, "warning")
  assert_eq(error_span.events[1].name, "error")
}

test "span_lifecycle_edge_cases" {
  // 测试Span生命周期的边界情况
  
  // 边界情况1: 零时间间隔的span
  let zero_duration_span = Span::{
    name: "zero_duration",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xaa_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xbb_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(1000L),  // 相同的开始和结束时间
    status: Ok,
    status_description: Some("Instant operation"),
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(zero_duration_span.start_time_unix_nanos, zero_duration_span.end_time_unix_nanos.unwrap())
  assert_eq(zero_duration_span.status, Ok)
  
  // 边界情况2: 负数时间戳（理论上不应该出现，但测试健壮性）
  let negative_time_span = Span::{
    name: "negative_time",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xcc_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xdd_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: -1000L,
    end_time_unix_nanos: Some(-500L),
    status: Ok,
    status_description: Some("Operation with negative timestamps"),
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(negative_time_span.start_time_unix_nanos, -1000L)
  assert_eq(negative_time_span.end_time_unix_nanos.unwrap(), -500L)
  assert_eq(negative_time_span.status, Ok)
  
  // 边界情况3: 极大时间戳
  let max_time_span = Span::{
    name: "max_time",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xee_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 9223372036854775807L,  // Int64最大值
    end_time_unix_nanos: Some(9223372036854775807L),
    status: Ok,
    status_description: Some("Operation at max timestamp"),
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(max_time_span.start_time_unix_nanos, 9223372036854775807L)
  assert_eq(max_time_span.end_time_unix_nanos.unwrap(), 9223372036854775807L)
  assert_eq(max_time_span.status, Ok)
}