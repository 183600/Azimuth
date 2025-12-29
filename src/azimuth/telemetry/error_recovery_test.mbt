// 错误恢复测试用例
// 测试telemetry系统在错误条件下的恢复能力和容错性

test "span_error_recovery" {
  // 测试Span创建和操作中的错误恢复
  
  // 测试无效的trace_id和span_id的处理
  let invalid_trace_id = Array::make(16, 0x00_byte) // 全零，无效
  let invalid_span_id = Array::make(8, 0x00_byte)   // 全零，无效
  let valid_trace_id = [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                       0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte,
                       0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                       0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]
  let valid_span_id = [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                      0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]
  
  // 创建具有无效ID的Span（应该能够处理）
  let span_with_invalid_ids = Span::{
    name: "error-recovery-span",
    context: SpanContext::{
      trace_id: invalid_trace_id,
      span_id: invalid_span_id,
      trace_flags: 0x01_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200001000000L),
    status: Error,
    status_description: Some("Created with invalid IDs for testing"),
    attributes: [
      ("error.test", AttributeValue::bool(true)),
      ("invalid.ids", AttributeValue::bool(true))
    ],
    events: [],
    links: []
  }
  
  // 验证即使使用无效ID，Span仍然可以创建和操作
  assert_eq(span_with_invalid_ids.name, "error-recovery-span")
  assert_eq(span_with_invalid_ids.context.trace_id.length(), 16)
  assert_eq(span_with_invalid_ids.context.span_id.length(), 8)
  
  match span_with_invalid_ids.status {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error status")
  }
  
  match span_with_invalid_ids.status_description {
    Some(description) => assert_eq(description, "Created with invalid IDs for testing")
    None => @test.fail("Expected status description")
  }
  
  // 测试从错误状态恢复
  let recovered_span = Span::{
    name: "recovered-span",
    context: SpanContext::{
      trace_id: valid_trace_id,
      span_id: valid_span_id,
      trace_flags: 0x01_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: Some(invalid_span_id), // 即使parent_span_id无效，也应该能处理
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200001000000L),
    status: Ok, // 从Error恢复到Ok
    status_description: Some("Successfully recovered from error"),
    attributes: [
      ("recovery.test", AttributeValue::bool(true)),
      ("previous.error", AttributeValue::string("invalid_ids"))
    ],
    events: [
      SpanEvent::{
        name: "error_occurred",
        timestamp_unix_nanos: 1640995200000000500L,
        attributes: [("error.type", AttributeValue::string("invalid_ids"))]
      },
      SpanEvent::{
        name: "error_resolved",
        timestamp_unix_nanos: 1640995200000000800L,
        attributes: [("resolution.method", AttributeValue::string("id_regeneration"))]
      }
    ],
    links: []
  }
  
  // 验证恢复的Span
  assert_eq(recovered_span.name, "recovered-span")
  match recovered_span.status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Expected Ok status after recovery")
  }
  
  match recovered_span.parent_span_id {
    Some(parent_id) => {
      // 即使parent_span_id无效，也应该能够存储和访问
      assert_eq(parent_id.length(), 8)
    }
    None => @test.fail("Expected parent_span_id")
  }
  
  assert_eq(recovered_span.events.length(), 2)
  match recovered_span.events[0].name {
    name => assert_eq(name, "error_occurred")
  }
  match recovered_span.events[1].name {
    name => assert_eq(name, "error_resolved")
  }
}

test "context_error_recovery" {
  // 测试Context操作的错误恢复
  
  // 测试使用空键的Context操作
  let empty_key = create_key("")
  let normal_key = create_key("normal.key")
  let base_context = Context::empty()
  
  // 即使使用空键，也应该能够处理
  let context_with_empty_key = base_context.with_value(empty_key, "empty.key.value")
  let context_with_normal_key = context_with_empty_key.with_value(normal_key, "normal.key.value")
  
  // 验证空键值的存储和检索
  match context_with_normal_key.get(empty_key) {
    Some(value) => assert_eq(value, "empty.key.value")
    None => @test.fail("Expected value for empty key")
  }
  
  match context_with_normal_key.get(normal_key) {
    Some(value) => assert_eq(value, "normal.key.value")
    None => @test.fail("Expected value for normal key")
  }
  
  // 测试从损坏的Context中恢复（模拟）
  let recovered_context = Context::empty()
  let recovery_key = create_key("recovery.key")
  let final_context = recovered_context.with_value(recovery_key, "recovery.value")
  
  match final_context.get(recovery_key) {
    Some(value) => assert_eq(value, "recovery.value")
    None => @test.fail("Expected recovery value")
  }
  
  // 验证恢复的Context不包含之前的错误数据
  match final_context.get(empty_key) {
    Some(_) => @test.fail("Recovered context should not contain previous error data")
    None => assert_eq(true, true) // 预期的结果
  }
}

test "baggage_error_recovery" {
  // 测试Baggage操作的错误恢复
  
  // 测试使用空键和空值的Baggage操作
  let base_baggage = Baggage::empty()
  
  // 即使使用空键，也应该能够处理
  let baggage_with_empty_key = base_baggage.with_entry("", "empty.key.value")
  let baggage_with_empty_value = baggage_with_empty_key.with_entry("empty.value.key", "")
  let baggage_with_both_empty = baggage_with_empty_value.with_entry("", "")
  
  // 验证空键和空值的处理
  match baggage_with_both_empty.get("") {
    Some(value) => assert_eq(value, "") // 最后设置的值
    None => @test.fail("Expected value for empty key")
  }
  
  match baggage_with_both_empty.get("empty.value.key") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty value")
  }
  
  // 测试从损坏的Baggage中恢复
  let recovered_baggage = Baggage::empty()
  let recovery_entries = [
    ("recovery.key1", "recovery.value1"),
    ("recovery.key2", "recovery.value2"),
    ("recovery.key3", "recovery.value3")
  ]
  
  let mut final_baggage = recovered_baggage
  let mut i = 0
  while i < recovery_entries.length() {
    let (key, value) = recovery_entries[i]
    final_baggage = final_baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 验证恢复的Baggage
  match final_baggage.get("recovery.key1") {
    Some(value) => assert_eq(value, "recovery.value1")
    None => @test.fail("Expected recovery value 1")
  }
  
  match final_baggage.get("recovery.key2") {
    Some(value) => assert_eq(value, "recovery.value2")
    None => @test.fail("Expected recovery value 2")
  }
  
  match final_baggage.get("recovery.key3") {
    Some(value) => assert_eq(value, "recovery.value3")
    None => @test.fail("Expected recovery value 3")
  }
  
  // 验证恢复的Baggage不包含之前的错误数据
  match final_baggage.get("") {
    Some(_) => @test.fail("Recovered baggage should not contain previous error data")
    None => assert_eq(true, true) // 预期的结果
  }
}

test "log_record_error_recovery" {
  // 测试LogRecord的错误恢复
  
  // 测试创建具有无效数据的LogRecord
  let invalid_log_record = LogRecord::{
    timestamp_unix_nanos: -1L, // 无效的时间戳
    observed_timestamp_unix_nanos: Some(-1L), // 无效的观察时间戳
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some(""), // 空日志体
    attributes: [
      ("error.test", AttributeValue::bool(true)),
      ("invalid.timestamp", AttributeValue::int(-1L))
    ],
    trace_id: Some(Array::make(16, 0x00_byte)), // 无效的trace_id
    span_id: Some(Array::make(8, 0x00_byte)),   // 无效的span_id
    trace_flags: Some(0xFF_byte), // 无效的trace_flags
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证即使数据无效，LogRecord仍然可以创建
  assert_eq(invalid_log_record.timestamp_unix_nanos, -1L)
  match invalid_log_record.observed_timestamp_unix_nanos {
    Some(ts) => assert_eq(ts, -1L)
    None => @test.fail("Expected observed timestamp")
  }
  
  match invalid_log_record.body {
    Some(body) => assert_eq(body, "")
    None => @test.fail("Expected empty body")
  }
  
  match invalid_log_record.trace_id {
    Some(trace_id) => assert_eq(trace_id.length(), 16)
    None => @test.fail("Expected trace ID")
  }
  
  // 测试从错误状态恢复
  let recovered_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L, // 有效的时间戳
    observed_timestamp_unix_nanos: Some(1640995200000000000L), // 有效的观察时间戳
    severity_number: Info, // 从Error恢复到Info
    severity_text: Some("INFO"),
    body: Some("Recovered from error state"),
    attributes: [
      ("recovery.test", AttributeValue::bool(true)),
      ("previous.error", AttributeValue::string("invalid_data")),
      ("recovery.timestamp", AttributeValue::string("1640995200000000000"))
    ],
    trace_id: Some([0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                    0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte,
                    0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                    0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]), // 有效的trace_id
    span_id: Some([0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                   0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]), // 有效的span_id
    trace_flags: Some(0x01_byte), // 有效的trace_flags
    resource: Some(Resource::default("recovery-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "recovery-logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证恢复的LogRecord
  assert_eq(recovered_log_record.timestamp_unix_nanos, 1640995200000000000L)
  match recovered_log_record.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity after recovery")
  }
  
  match recovered_log_record.body {
    Some(body) => assert_eq(body, "Recovered from error state")
    None => @test.fail("Expected recovery body")
  }
  
  match recovered_log_record.resource {
    Some(resource) => assert_eq(resource.service_name, "recovery-service")
    None => @test.fail("Expected resource in recovered log")
  }
}

test "attribute_error_recovery" {
  // 测试属性的错误恢复
  
  // 测试创建具有无效数据的属性
  let invalid_attributes = [
    ("", AttributeValue::string("empty.key")), // 空键
    ("empty.value", AttributeValue::string("")), // 空值
    ("very.long.key." + "a".repeat(1000), AttributeValue::string("long.key")), // 过长的键
    ("special.chars.key", AttributeValue::string("value with \n newlines and \t tabs")), // 包含特殊字符的值
    ("numeric.key", AttributeValue::int(-9223372036854775808L)), // 最小int64值
    ("float.key", AttributeValue::float(0.0/0.0)) // NaN
  ]
  
  // 创建具有无效属性的Span
  let span_with_invalid_attributes = Span::{
    name: "invalid-attributes-span",
    context: SpanContext::{
      trace_id: [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                 0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte,
                 0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                 0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte],
      span_id: [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte],
      trace_flags: 0x01_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200001000000L),
    status: Error,
    status_description: Some("Created with invalid attributes"),
    attributes: invalid_attributes,
    events: [],
    links: []
  }
  
  // 验证即使属性无效，Span仍然可以创建和操作
  assert_eq(span_with_invalid_attributes.name, "invalid-attributes-span")
  assert_eq(span_with_invalid_attributes.attributes.length(), 6)
  
  // 验证无效属性的存在
  let mut found_empty_key = false
  let mut found_empty_value = false
  let mut found_long_key = false
  let mut found_special_chars = false
  let mut found_min_int = false
  let mut found_nan_float = false
  
  let mut i = 0
  while i < span_with_invalid_attributes.attributes.length() {
    match span_with_invalid_attributes.attributes[i] {
      ("", AttributeValue::string(_)) => found_empty_key = true
      ("empty.value", AttributeValue::string(value)) => {
        assert_eq(value, "")
        found_empty_value = true
      }
      ("very.long.key." + _, AttributeValue::string(_)) => found_long_key = true
      ("special.chars.key", AttributeValue::string(value)) => {
        assert_eq(value.contains("\n"), true)
        assert_eq(value.contains("\t"), true)
        found_special_chars = true
      }
      ("numeric.key", AttributeValue::int(value)) => {
        assert_eq(value, -9223372036854775808L)
        found_min_int = true
      }
      ("float.key", AttributeValue::float(_)) => found_nan_float = true
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(found_empty_key, true)
  assert_eq(found_empty_value, true)
  assert_eq(found_long_key, true)
  assert_eq(found_special_chars, true)
  assert_eq(found_min_int, true)
  assert_eq(found_nan_float, true)
  
  // 测试从无效属性恢复
  let recovered_attributes = [
    ("recovery.status", AttributeValue::string("success")),
    ("error.count", AttributeValue::int(6L)),
    ("recovery.timestamp", AttributeValue::string("1640995200000000000"))
  ]
  
  let recovered_span = Span::{
    name: "recovered-attributes-span",
    context: SpanContext::{
      trace_id: [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                 0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte,
                 0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                 0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte],
      span_id: [0x02_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte],
      trace_flags: 0x01_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200001000000L),
    status: Ok,
    status_description: Some("Successfully recovered from invalid attributes"),
    attributes: recovered_attributes,
    events: [],
    links: []
  }
  
  // 验证恢复的属性
  assert_eq(recovered_span.name, "recovered-attributes-span")
  assert_eq(recovered_span.attributes.length(), 3)
  
  match recovered_span.status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Expected Ok status after recovery")
  }
}

test "propagation_error_recovery" {
  // 测试传播的错误恢复
  
  // 测试无效的传播数据
  let invalid_carrier_data = [
    ("traceparent", "invalid-traceparent-format"),
    ("tracestate", ""),
    ("baggage", "invalid=baggage=format=with=extra=equals"),
    ("malformed-header", "\n\r\t")
  ]
  
  let invalid_carrier = MapCarrier::from_map(invalid_carrier_data)
  
  // 测试W3C TraceContext Propagator的错误处理
  let w3c_propagator = W3CTraceContextPropagator::{}
  let base_context = Context::empty()
  
  // 即使carrier数据无效，extract操作也应该能够处理
  let extracted_context = w3c_propagator.extract(base_context, invalid_carrier)
  
  // 验证extract操作不会崩溃
  assert_eq(extracted_context.values.length(), 0) // 原始context应该保持不变
  
  // 测试inject操作的错误处理
  let recovery_carrier_data = [
    ("valid.traceparent", "00-0123456789abcdef0123456789abcdef-0123456789abcdef-01"),
    ("valid.baggage", "key1=value1,key2=value2")
  ]
  let recovery_carrier = MapCarrier::from_map(recovery_carrier_data)
  
  // inject操作应该能够处理
  w3c_propagator.inject(extracted_context, recovery_carrier)
  
  // 验证inject操作不会崩溃
  let keys = recovery_carrier.keys()
  assert_eq(keys.length(), 2)
  
  // 测试W3C Baggage Propagator的错误处理
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 即使carrier数据无效，extract操作也应该能够处理
  let baggage_extracted_context = baggage_propagator.extract(base_context, invalid_carrier)
  
  // 验证extract操作不会崩溃
  assert_eq(baggage_extracted_context.values.length(), 0)
  
  // 测试inject操作的错误处理
  baggage_propagator.inject(baggage_extracted_context, recovery_carrier)
  
  // 验证inject操作不会崩溃
  let baggage_keys = recovery_carrier.keys()
  assert_eq(baggage_keys.length(), 2)
  
  // 测试Composite Propagator的错误恢复
  let composite_propagator = CompositePropagator::new([w3c_propagator, baggage_propagator])
  
  // 即使carrier数据无效，composite propagator也应该能够处理
  let composite_extracted_context = composite_propagator.extract(base_context, invalid_carrier)
  
  // 验证extract操作不会崩溃
  assert_eq(composite_extracted_context.values.length(), 0)
  
  // 测试inject操作的错误处理
  composite_propagator.inject(composite_extracted_context, recovery_carrier)
  
  // 验证inject操作不会崩溃
  let composite_keys = recovery_carrier.keys()
  assert_eq(composite_keys.length(), 2)
}