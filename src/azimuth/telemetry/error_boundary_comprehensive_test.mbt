// 错误边界测试 - 测试各种边界情况和错误处理

test "attribute_value_boundary_conditions" {
  // 测试AttributeValue的各种边界条件和异常情况
  
  // 1. 字符串边界测试
  let empty_string = AttributeValue::string("")
  let max_length_string = AttributeValue::string("a" * 10000)  // 假设最大长度限制
  let unicode_string = AttributeValue::string("测试中文字符串🚀emoji和特殊字符!@#$%^&*()")
  let null_bytes_string = AttributeValue::string("prefix\x00\x01\x02suffix")
  
  // 验证字符串边界值
  match empty_string {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Test failed")
  }
  
  match max_length_string {
    StringValue(s) => assert_eq(s.length(), 10000)
    _ => @test.fail("Test failed")
  }
  
  match unicode_string {
    StringValue(s) => {
      assert_eq(s.contains("测试"), true)
      assert_eq(s.contains("🚀"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 2. 整数边界测试
  let zero_int = AttributeValue::int(0L)
  let max_int64 = AttributeValue::int(9223372036854775807L)
  let min_int64 = AttributeValue::int(-9223372036854775808L)
  let negative_one = AttributeValue::int(-1L)
  
  match zero_int {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Test failed")
  }
  
  match max_int64 {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match min_int64 {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  // 3. 浮点数边界测试
  let zero_float = AttributeValue::float(0.0)
  let positive_infinity = AttributeValue::float(1.0/0.0)
  let negative_infinity = AttributeValue::float(-1.0/0.0)
  let nan_value = AttributeValue::float(0.0/0.0)
  let max_double = AttributeValue::float(1.7976931348623157e308)
  let min_positive_double = AttributeValue::float(2.2250738585072014e-308)
  let max_negative_double = AttributeValue::float(-2.2250738585072014e-308)
  
  match zero_float {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => @test.fail("Test failed")
  }
  
  match positive_infinity {
    FloatValue(f) => assert_eq(f > 1.0e308, true)
    _ => @test.fail("Test failed")
  }
  
  match negative_infinity {
    FloatValue(f) => assert_eq(f < -1.0e308, true)
    _ => @test.fail("Test failed")
  }
  
  // 4. 布尔值边界测试
  let true_value = AttributeValue::bool(true)
  let false_value = AttributeValue::bool(false)
  
  match true_value {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Test failed")
  }
  
  match false_value {
    BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Test failed")
  }
  
  // 5. 数组边界测试
  let empty_string_array = AttributeValue::array_string([])
  let large_string_array = AttributeValue::array_string([for i = 0; i < 1000; i = i + 1].map(fn(i) { "item_" + i.to_string() }))
  let empty_int_array = AttributeValue::array_int([])
  let large_int_array = AttributeValue::array_int([for i = 0; i < 1000; i = i + 1].map(fn(i) { i }))
  let mixed_string_array = AttributeValue::array_string(["", "normal", "special!@#$%", "unicode测试", "   spaces   "])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match large_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], "item_0")
      assert_eq(arr[999], "item_999")
    }
    _ => @test.fail("Test failed")
  }
  
  match mixed_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "")
      assert_eq(arr[1], "normal")
      assert_eq(arr[2], "special!@#$%")
      assert_eq(arr[3], "unicode测试")
      assert_eq(arr[4], "   spaces   ")
    }
    _ => @test.fail("Test failed")
  }
}

test "context_edge_cases_and_limits" {
  // 测试Context的边界情况和限制
  
  // 1. 空Context操作
  let empty_ctx = Context::empty()
  let non_existent_key = create_key("non.existent.key")
  
  match empty_ctx.get(non_existent_key) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 2. 特殊键名测试
  let empty_key = create_key("")
  let space_key = create_key(" ")
  let special_char_key = create_key("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  let unicode_key = create_key("键名测试🚀")
  let very_long_key = create_key("a" * 1000)
  let dot_key = create_key("key.with.dots")
  let null_key = create_key("key\x00with\x00nulls")
  
  let ctx_with_special_keys = empty_ctx
    .with_value(empty_key, "empty_key_value")
    .with_value(space_key, "space_key_value")
    .with_value(special_char_key, "special_char_value")
    .with_value(unicode_key, "unicode_value")
    .with_value(very_long_key, "long_key_value")
    .with_value(dot_key, "dot_key_value")
    .with_value(null_key, "null_key_value")
  
  // 验证特殊键名的值
  match ctx_with_special_keys.get(empty_key) {
    Some(value) => assert_eq(value, "empty_key_value")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_special_keys.get(unicode_key) {
    Some(value) => assert_eq(value, "unicode_value")
    None => @test.fail("Test failed")
  }
  
  // 3. 特殊值测试
  let empty_value = create_key("empty_value")
  let very_long_value = create_key("very_long_value")
  let special_char_value = create_key("special_char_value")
  let unicode_value = create_key("unicode_value")
  let null_value = create_key("null_value")
  
  let ctx_with_special_values = empty_ctx
    .with_value(empty_value, "")
    .with_value(very_long_value, "v" * 10000)
    .with_value(special_char_value, "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./\x00\x01\x02")
    .with_value(unicode_value, "值测试🚀测试值")
    .with_value(null_value, "prefix\x00suffix")
  
  match ctx_with_special_values.get(empty_value) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_special_values.get(very_long_value) {
    Some(value) => assert_eq(value.length(), 10000)
    None => @test.fail("Test failed")
  }
  
  // 4. 大量键值对测试
  let mut ctx_with_many_values = empty_ctx
  let mut i = 0
  while i < 1000 {
    ctx_with_many_values = ctx_with_many_values.with_value(
      create_key("key_" + i.to_string()),
      "value_" + i.to_string()
    )
    i = i + 1
  }
  
  // 验证大量键值对中的特定值
  match ctx_with_many_values.get(create_key("key_0")) {
    Some(value) => assert_eq(value, "value_0")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_many_values.get(create_key("key_999")) {
    Some(value) => assert_eq(value, "value_999")
    None => @test.fail("Test failed")
  }
  
  // 5. 键值覆盖测试
  let overwrite_key = create_key("overwrite_test")
  let ctx1 = empty_ctx.with_value(overwrite_key, "original_value")
  let ctx2 = ctx1.with_value(overwrite_key, "overwritten_value")
  let ctx3 = ctx2.with_value(overwrite_key, "final_value")
  
  match ctx3.get(overwrite_key) {
    Some(value) => assert_eq(value, "final_value")
    None => @test.fail("Test failed")
  }
}

test "span_lifecycle_boundary_conditions" {
  // 测试Span生命周期的边界条件
  
  // 1. 极端时间戳测试
  let min_timestamp = -9223372036854775808L
  let max_timestamp = 9223372036854775807L
  let zero_timestamp = 0L
  
  let span_with_min_time = Span::{
    name: "min_time_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: min_timestamp,
    end_time_unix_nanos: Some(max_timestamp),
    status: Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let span_with_zero_time = Span::{
    name: "zero_time_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: zero_timestamp,
    end_time_unix_nanos: Some(zero_timestamp),
    status: Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(span_with_min_time.start_time_unix_nanos, min_timestamp)
  match span_with_min_time.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, max_timestamp)
    None => @test.fail("Test failed")
  }
  
  assert_eq(span_with_zero_time.start_time_unix_nanos, zero_timestamp)
  match span_with_zero_time.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, zero_timestamp)
    None => @test.fail("Test failed")
  }
  
  // 2. 无效时间顺序测试
  let invalid_time_span = Span::{
    name: "invalid_time_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(500L),  // 结束时间早于开始时间
    status: Error,
    status_description: Some("Invalid time sequence"),
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(invalid_time_span.start_time_unix_nanos, 1000L)
  match invalid_time_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 500L)
    None => @test.fail("Test failed")
  }
  assert_eq(invalid_time_span.status, Error)
  
  // 3. 极长Span名称测试
  let very_long_name = "a" * 10000
  let long_name_span = Span::{
    name: very_long_name,
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(long_name_span.name.length(), 10000)
  
  // 4. 特殊字符Span名称测试
  let special_name_span = Span::{
    name: "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./测试🚀\x00\x01\x02",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(special_name_span.name.contains("测试"), true)
  assert_eq(special_name_span.name.contains("🚀"), true)
  
  // 5. 大量属性测试
  let mut many_attributes = []
  let mut i = 0
  while i < 1000 {
    many_attributes.push(("attr_" + i.to_string(), AttributeValue::string("value_" + i.to_string())))
    i = i + 1
  }
  
  let many_attributes_span = Span::{
    name: "many_attributes_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: many_attributes,
    events: [],
    links: []
  }
  
  assert_eq(many_attributes_span.attributes.length(), 1000)
  
  // 6. 大量事件测试
  let mut many_events = []
  let mut i = 0
  while i < 100 {
    many_events.push(SpanEvent::{
      name: "event_" + i.to_string(),
      timestamp_unix_nanos: i.to_int64(),
      attributes: [("event_index", AttributeValue::int(i.to_int64()))]
    })
    i = i + 1
  }
  
  let many_events_span = Span::{
    name: "many_events_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: many_events,
    links: []
  }
  
  assert_eq(many_events_span.events.length(), 100)
}

test "baggage_extreme_conditions" {
  // 测试Baggage的极端条件
  
  // 1. 空键和空值测试
  let empty_baggage = Baggage::empty()
  let baggage_with_empty_key = empty_baggage.with_entry("", "value_for_empty_key")
  let baggage_with_empty_value = baggage_with_empty_key.with_entry("key_for_empty_value", "")
  let baggage_with_both_empty = baggage_with_empty_value.with_entry("", "")
  
  match baggage_with_empty_key.get("") {
    Some(value) => assert_eq(value, "value_for_empty_key")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_empty_value.get("key_for_empty_value") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_both_empty.get("") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  // 2. 特殊字符键值测试
  let special_baggage = empty_baggage
    .with_entry("!@#$%^&*()", "!@#$%^&*()")
    .with_entry("键测试🚀", "值测试🚀")
    .with_entry("key\x00with\x00nulls", "value\x00with\x00nulls")
    .with_entry("   spaces   ", "   spaces   ")
    .with_entry("dots.and.underscores", "dots.and.underscores")
  
  match special_baggage.get("!@#$%^&*()") {
    Some(value) => assert_eq(value, "!@#$%^&*()")
    None => @test.fail("Test failed")
  }
  
  match special_baggage.get("键测试🚀") {
    Some(value) => assert_eq(value, "值测试🚀")
    None => @test.fail("Test failed")
  }
  
  // 3. 长键和长值测试
  let long_key = "k" * 1000
  let long_value = "v" * 10000
  let long_baggage = empty_baggage
    .with_entry(long_key, long_value)
    .with_entry("normal_key", long_value)
    .with_entry(long_key, "normal_value")
  
  match long_baggage.get(long_key) {
    Some(value) => assert_eq(value, "normal_value")  // 应该是覆盖后的值
    None => @test.fail("Test failed")
  }
  
  match long_baggage.get("normal_key") {
    Some(value) => assert_eq(value.length(), 10000)
    None => @test.fail("Test failed")
  }
  
  // 4. 大量条目测试
  let mut many_entries_baggage = empty_baggage
  let mut i = 0
  while i < 1000 {
    many_entries_baggage = many_entries_baggage.with_entry(
      "entry_" + i.to_string(),
      "value_" + i.to_string()
    )
    i = i + 1
  }
  
  match many_entries_baggage.get("entry_0") {
    Some(value) => assert_eq(value, "value_0")
    None => @test.fail("Test failed")
  }
  
  match many_entries_baggage.get("entry_999") {
    Some(value) => assert_eq(value, "value_999")
    None => @test.fail("Test failed")
  }
  
  // 5. 重复键覆盖测试
  let override_baggage = empty_baggage
    .with_entry("test_key", "original_value")
    .with_entry("test_key", "first_override")
    .with_entry("test_key", "second_override")
    .with_entry("test_key", "final_value")
  
  match override_baggage.get("test_key") {
    Some(value) => assert_eq(value, "final_value")
    None => @test.fail("Test failed")
  }
  
  // 6. 复合键测试（类似URL参数）
  let complex_baggage = empty_baggage
    .with_entry("user.id", "user123")
    .with_entry("user.name", "John Doe")
    .with_entry("user.email", "john@example.com")
    .with_entry("service.name", "payment-service")
    .with_entry("service.version", "2.1.0")
    .with_entry("request.id", "req-12345")
    .with_entry("trace.id", "trace-67890")
    .with_entry("session.id", "sess-abcde")
    .with_entry("correlation.id", "corr-fghij")
    .with_entry("deployment.env", "production")
    .with_entry("region", "us-west-2")
    .with_entry("availability.zone", "us-west-2a")
    .with_entry("instance.id", "i-1234567890abcdef0")
  
  match complex_baggage.get("user.id") {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Test failed")
  }
  
  match complex_baggage.get("instance.id") {
    Some(value) => assert_eq(value, "i-1234567890abcdef0")
    None => @test.fail("Test failed")
  }
}

test "log_record_boundary_conditions" {
  // 测试LogRecord的边界条件
  
  // 1. 极端时间戳测试
  let min_time_log = LogRecord::{
    timestamp_unix_nanos: -9223372036854775808L,
    observed_timestamp_unix_nanos: Some(-9223372036854775808L),
    severity_number: Error,
    severity_text: Some("MIN_TIME_ERROR"),
    body: Some("Log with minimum timestamp"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let max_time_log = LogRecord::{
    timestamp_unix_nanos: 9223372036854775807L,
    observed_timestamp_unix_nanos: Some(9223372036854775807L),
    severity_number: Fatal,
    severity_text: Some("MAX_TIME_FATAL"),
    body: Some("Log with maximum timestamp"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(min_time_log.timestamp_unix_nanos, -9223372036854775808L)
  match min_time_log.observed_timestamp_unix_nanos {
    Some(timestamp) => assert_eq(timestamp, -9223372036854775808L)
    None => @test.fail("Test failed")
  }
  
  assert_eq(max_time_log.timestamp_unix_nanos, 9223372036854775807L)
  match max_time_log.observed_timestamp_unix_nanos {
    Some(timestamp) => assert_eq(timestamp, 9223372036854775807L)
    None => @test.fail("Test failed")
  }
  
  // 2. 极长日志消息测试
  let very_long_message = "a" * 100000
  let long_message_log = LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: None,
    body: Some(very_long_message),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match long_message_log.body {
    Some(message) => assert_eq(message.length(), 100000)
    None => @test.fail("Test failed")
  }
  
  // 3. 特殊字符日志消息测试
  let special_message = "Special chars: !@#$%^&*()_+-={}[]|\\:;\"'<>?,./\x00\x01\x02 Unicode: 测试🚀 Newlines:\n\tTabs"
  let special_message_log = LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: Warn,
    severity_text: Some("SPECIAL_CHARS_WARNING"),
    body: Some(special_message),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match special_message_log.body {
    Some(message) => {
      assert_eq(message.contains("测试"), true)
      assert_eq(message.contains("🚀"), true)
      assert_eq(message.contains("\n"), true)
      assert_eq(message.contains("\t"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 4. 大量属性测试
  let mut many_attributes = []
  let mut i = 0
  while i < 1000 {
    many_attributes.push(("log_attr_" + i.to_string(), AttributeValue::string("log_value_" + i.to_string())))
    i = i + 1
  }
  
  let many_attributes_log = LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("MANY_ATTRIBUTES_INFO"),
    body: Some("Log with many attributes"),
    attributes: many_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(many_attributes_log.attributes.length(), 1000)
  
  // 5. 复杂Trace和Span ID测试
  let zero_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let max_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte })
  let zero_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  let max_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte })
  
  let zero_ids_log = LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: Debug,
    severity_text: Some("ZERO_IDS_DEBUG"),
    body: Some("Log with zero trace and span IDs"),
    attributes: [],
    trace_id: Some(zero_trace_id),
    span_id: Some(zero_span_id),
    trace_flags: Some(0_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  let max_ids_log = LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: Debug,
    severity_text: Some("MAX_IDS_DEBUG"),
    body: Some("Log with max trace and span IDs"),
    attributes: [],
    trace_id: Some(max_trace_id),
    span_id: Some(max_span_id),
    trace_flags: Some(0xff_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  match zero_ids_log.trace_id {
    Some(id) => {
      assert_eq(id.length(), 16)
      let mut i = 0
      while i < 16 {
        assert_eq(id[i], 0_byte)
        i = i + 1
      }
    }
    None => @test.fail("Test failed")
  }
  
  match max_ids_log.trace_id {
    Some(id) => {
      assert_eq(id.length(), 16)
      let mut i = 0
      while i < 16 {
        assert_eq(id[i], 0xff_byte)
        i = i + 1
      }
    }
    None => @test.fail("Test failed")
  }
}