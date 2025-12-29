// Trace模块综合测试用例
test "span_kinds" {
  let internal_span = Internal
  let server_span = Server
  let client_span = Client
  let producer_span = Producer
  let consumer_span = Consumer
  
  match internal_span {
    Internal => assert_eq(true, true)
    _ => @test.fail("Expected Internal span kind")
  }
  
  match server_span {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  
  match client_span {
    Client => assert_eq(true, true)
    _ => @test.fail("Expected Client span kind")
  }
  
  match producer_span {
    Producer => assert_eq(true, true)
    _ => @test.fail("Expected Producer span kind")
  }
  
  match consumer_span {
    Consumer => assert_eq(true, true)
    _ => @test.fail("Expected Consumer span kind")
  }
}

test "status_codes" {
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  match unset_status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Expected Unset status")
  }
  
  match ok_status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Expected Ok status")
  }
  
  match error_status {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error status")
  }
}

test "span_context_creation" {
  let span_context = SpanContext::{
    trace_id: Array::make(16, 1_byte),
    span_id: Array::make(8, 2_byte),
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 1_byte)
  assert_eq(span_context.trace_state, "key1=value1,key2=value2")
  
  // 验证trace_id的内容
  assert_eq(span_context.trace_id[0], 1_byte)
  assert_eq(span_context.trace_id[15], 1_byte)
  
  // 验证span_id的内容
  assert_eq(span_context.span_id[0], 2_byte)
  assert_eq(span_context.span_id[7], 2_byte)
}
