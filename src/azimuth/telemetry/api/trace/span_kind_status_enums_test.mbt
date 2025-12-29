// SpanKind和StatusCode枚举的测试用例
// 测试链路追踪中的枚举类型

test "span_kind_all_values" {
  // 测试所有SpanKind枚举值
  let internal_span = Span::{
    name: "internal-operation",
    context: SpanContext::{
      trace_id: Array::make(16, 0_byte),
      span_id: Array::make(8, 1_byte),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1234567890L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let server_span = Span::{ ..internal_span, kind: Server }
  let client_span = Span::{ ..internal_span, kind: Client }
  let producer_span = Span::{ ..internal_span, kind: Producer }
  let consumer_span = Span::{ ..internal_span, kind: Consumer }
  
  // 验证每种SpanKind
  match internal_span.kind {
    Internal => assert_eq(true, true)
    _ => @test.fail("Expected Internal")
  }
  
  match server_span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server")
  }
  
  match client_span.kind {
    Client => assert_eq(true, true)
    _ => @test.fail("Expected Client")
  }
  
  match producer_span.kind {
    Producer => assert_eq(true, true)
    _ => @test.fail("Expected Producer")
  }
  
  match consumer_span.kind {
    Consumer => assert_eq(true, true)
    _ => @test.fail("Expected Consumer")
  }
}

test "status_code_all_values" {
  // 测试所有StatusCode枚举值
  let unset_span = Span::{
    name: "unset-span",
    context: SpanContext::{
      trace_id: Array::make(16, 0_byte),
      span_id: Array::make(8, 1_byte),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1234567890L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let ok_span = Span::{ ..unset_span, status: Ok }
  let error_span = Span::{ ..unset_span, status: Error, status_description: Some("Something went wrong") }
  
  // 验证每种StatusCode
  match unset_span.status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Expected Unset")
  }
  
  match ok_span.status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Expected Ok")
  }
  
  match error_span.status {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error")
  }
  
  // 验证错误状态描述
  match error_span.status_description {
    Some(desc) => assert_eq(desc, "Something went wrong")
    None => @test.fail("Expected Some(description)")
  }
}

test "span_with_all_enum_combinations" {
  // 测试SpanKind和StatusCode的组合
  let base_context = SpanContext::{
    trace_id: Array::make(16, 0_byte),
    span_id: Array::make(8, 1_byte),
    trace_flags: 1_byte,
    trace_state: "key1=value1"
  }
  
  let attributes = [
    ("http.method", AttributeValue::string("GET")),
    ("http.status_code", AttributeValue::int(200L))
  ]
  
  // 测试不同组合
  let combinations = [
    (Server, Ok),
    (Client, Ok),
    (Producer, Ok),
    (Consumer, Ok),
    (Internal, Unset),
    (Server, Error),
    (Client, Error),
    (Producer, Error),
    (Consumer, Error)
  ]
  
  let mut i = 0
  while i < combinations.length() {
    let (kind, status) = combinations[i]
    
    let span = Span::{
      name: "test-span-" + i.to_string(),
      context: base_context,
      kind: kind,
      parent_span_id: Some(Array::make(8, (i + 1).to_byte())),
      start_time_unix_nanos: 1234567890L + i.to_int64(),
      end_time_unix_nanos: Some(1234567890L + i.to_int64() + 1000L),
      status: status,
      status_description: if status == Error { Some("Error occurred") } else { None },
      attributes: attributes,
      events: [],
      links: []
    }
    
    // 验证span属性
    assert_eq(span.context.trace_flags, 1_byte)
    assert_eq(span.context.trace_state, "key1=value1")
    assert_eq(span.attributes.length(), 2)
    
    i = i + 1
  }
}

test "span_context_byte_arrays" {
  // 测试SpanContext中的字节数组
  let trace_id = [
    0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte,
    8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte
  ]
  
  let span_id = [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte]
  
  let span_context = SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: "key=value"
  }
  
  // 验证trace_id字节
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.trace_id[0], 0_byte)
  assert_eq(span_context.trace_id[7], 7_byte)
  assert_eq(span_context.trace_id[15], 15_byte)
  
  // 验证span_id字节
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.span_id[0], 0_byte)
  assert_eq(span_context.span_id[7], 7_byte)
  
  // 验证其他字段
  assert_eq(span_context.trace_flags, 1_byte)
  assert_eq(span_context.trace_state, "key=value")
}