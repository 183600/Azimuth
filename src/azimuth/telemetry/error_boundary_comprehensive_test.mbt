// 错误边界综合测试用例
// 测试错误场景和边界情况的处理

test "attribute_value_error_handling" {
  // 测试AttributeValue的错误处理和边界情况
  
  // 测试极值字符串
  let very_long_string = "a" * 10000  // 假设支持字符串重复
  let long_string_attr = AttributeValue::string(very_long_string)
  
  match long_string_attr {
    StringValue(s) => assert_eq(s.length() > 0, true)
    _ => @test.fail("Test failed")
  }
  
  // 测试特殊字符字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~\n\r\t\\"
  let special_chars_attr = AttributeValue::string(special_chars)
  
  match special_chars_attr {
    StringValue(s) => assert_eq(s, special_chars)
    _ => @test.fail("Test failed")
  }
  
  // 测试Unicode字符串
  let unicode_string = "Unicode测试🚀🌟💫✨🎯🎉🎊🎈🎁🎂🎄🎃🎆🎇🎈🎉"
  let unicode_attr = AttributeValue::string(unicode_string)
  
  match unicode_attr {
    StringValue(s) => assert_eq(s, unicode_string)
    _ => @test.fail("Test failed")
  }
  
  // 测试极值整数
  let max_int = 9223372036854775807L
  let min_int = -9223372036854775808L
  let max_int_attr = AttributeValue::int(max_int)
  let min_int_attr = AttributeValue::int(min_int)
  
  match max_int_attr {
    IntValue(i) => assert_eq(i, max_int)
    _ => @test.fail("Test failed")
  }
  
  match min_int_attr {
    IntValue(i) => assert_eq(i, min_int)
    _ => @test.fail("Test failed")
  }
  
  // 测试特殊浮点数值
  let infinity = 1.0/0.0
  let neg_infinity = -1.0/0.0
  let nan = 0.0/0.0
  let very_small = 1.0e-308
  let very_large = 1.0e+308
  
  let infinity_attr = AttributeValue::float(infinity)
  let neg_infinity_attr = AttributeValue::float(neg_infinity)
  let nan_attr = AttributeValue::float(nan)
  let very_small_attr = AttributeValue::float(very_small)
  let very_large_attr = AttributeValue::float(very_large)
  
  match infinity_attr {
    FloatValue(f) => assert_eq(f > 0.0 && f > 1.0e+308, true)
    _ => @test.fail("Test failed")
  }
  
  match neg_infinity_attr {
    FloatValue(f) => assert_eq(f < 0.0 && f < -1.0e+308, true)
    _ => @test.fail("Test failed")
  }
  
  match nan_attr {
    FloatValue(f) => assert_eq(f != f, true)  // NaN != NaN
    _ => @test.fail("Test failed")
  }
  
  // 测试空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  // 测试包含特殊值的数组
  let special_int_array = AttributeValue::array_int([0L, -1L, 1L, max_int, min_int])
  let special_float_array = AttributeValue::array_float([0.0, -0.0, infinity, neg_infinity, nan])
  let special_string_array = AttributeValue::array_string(["", " ", "\n", "\t", special_chars])
  
  match special_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 0L)
      assert_eq(arr[1], -1L)
      assert_eq(arr[2], 1L)
      assert_eq(arr[3], max_int)
      assert_eq(arr[4], min_int)
    }
    _ => @test.fail("Test failed")
  }
  
  match special_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 0.0)
      assert_eq(arr[1], -0.0)
      assert_eq(arr[2] > 1.0e+308, true)  // infinity
      assert_eq(arr[3] < -1.0e+308, true)  // neg_infinity
      assert_eq(arr[4] != arr[4], true)    // nan
    }
    _ => @test.fail("Test failed")
  }
}

test "context_error_boundary" {
  // 测试Context的错误边界情况
  
  // 测试空上下文
  let empty_ctx = Context::empty()
  assert_eq(empty_ctx.values.length(), 0)
  
  // 测试获取不存在的键
  let nonexistent_key = create_key("nonexistent.key")
  match empty_ctx.get(nonexistent_key) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试设置空键名
  let empty_key = create_key("")
  let ctx_with_empty_key = empty_ctx.with_value(empty_key, "value")
  match ctx_with_empty_key.get(empty_key) {
    Some(value) => assert_eq(value, "value")
    None => @test.fail("Test failed")
  }
  
  // 测试设置空值
  let normal_key = create_key("normal.key")
  let ctx_with_empty_value = empty_ctx.with_value(normal_key, "")
  match ctx_with_empty_value.get(normal_key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  // 测试设置特殊字符键和值
  let special_key = create_key("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  let special_value = "!@#$%^&*()_+-=[]{}|;':\",./<>?\n\r\t\\"
  let ctx_with_special = empty_ctx.with_value(special_key, special_value)
  match ctx_with_special.get(special_key) {
    Some(value) => assert_eq(value, special_value)
    None => @test.fail("Test failed")
  }
  
  // 测试设置Unicode键和值
  let unicode_key = create_key("Unicode键🚀")
  let unicode_value = "Unicode值🌟"
  let ctx_with_unicode = empty_ctx.with_value(unicode_key, unicode_value)
  match ctx_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, unicode_value)
    None => @test.fail("Test failed")
  }
  
  // 测试长键和值
  let long_key_name = "a" * 1000
  let long_key = create_key(long_key_name)
  let long_value = "b" * 1000
  let ctx_with_long = empty_ctx.with_value(long_key, long_value)
  match ctx_with_long.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Test failed")
  }
  
  // 测试覆盖相同键的值
  let ctx_with_override = empty_ctx
    .with_value(normal_key, "first_value")
    .with_value(normal_key, "second_value")
    .with_value(normal_key, "final_value")
  
  match ctx_with_override.get(normal_key) {
    Some(value) => assert_eq(value, "final_value")
    None => @test.fail("Test failed")
  }
  
  // 测试大量键值对
  let mut ctx_with_many = empty_ctx
  let mut i = 0
  while i < 100 {
    let key = create_key("key." + i.to_string())
    ctx_with_many = ctx_with_many.with_value(key, "value." + i.to_string())
    i = i + 1
  }
  
  // 验证第一个和最后一个键
  let first_key = create_key("key.0")
  let last_key = create_key("key.99")
  
  match ctx_with_many.get(first_key) {
    Some(value) => assert_eq(value, "value.0")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_many.get(last_key) {
    Some(value) => assert_eq(value, "value.99")
    None => @test.fail("Test failed")
  }
}

test "baggage_error_boundary" {
  // 测试Baggage的错误边界情况
  
  // 测试空baggage
  let empty_baggage = Baggage::empty()
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 测试获取不存在的条目
  match empty_baggage.get("nonexistent") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试设置空键
  let baggage_with_empty_key = empty_baggage.with_entry("", "value")
  match baggage_with_empty_key.get("") {
    Some(value) => assert_eq(value, "value")
    None => @test.fail("Test failed")
  }
  
  // 测试设置空值
  let baggage_with_empty_value = empty_baggage.with_entry("key", "")
  match baggage_with_empty_value.get("key") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  // 测试设置特殊字符键和值
  let baggage_with_special = empty_baggage
    .with_entry("!@#$%^&*()", "!@#$%^&*()")
    .with_entry("key with spaces", "value with spaces")
    .with_entry("key\nwith\nnewlines", "value\nwith\nnewlines")
    .with_entry("key\twith\ttabs", "value\twith\ttabs")
  
  match baggage_with_special.get("!@#$%^&*()") {
    Some(value) => assert_eq(value, "!@#$%^&*()")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_special.get("key with spaces") {
    Some(value) => assert_eq(value, "value with spaces")
    None => @test.fail("Test failed")
  }
  
  // 测试Unicode键和值
  let baggage_with_unicode = empty_baggage
    .with_entry("Unicode键🚀", "Unicode值🌟")
    .with_entry("ключ", "значение")
    .with_entry("キー", "値")
  
  match baggage_with_unicode.get("Unicode键🚀") {
    Some(value) => assert_eq(value, "Unicode值🌟")
    None => @test.fail("Test failed")
  }
  
  // 测试长键和值
  let long_key = "a" * 1000
  let long_value = "b" * 1000
  let baggage_with_long = empty_baggage.with_entry(long_key, long_value)
  match baggage_with_long.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Test failed")
  }
  
  // 测试覆盖相同键的值
  let baggage_with_override = empty_baggage
    .with_entry("duplicate.key", "first_value")
    .with_entry("duplicate.key", "second_value")
    .with_entry("duplicate.key", "final_value")
  
  match baggage_with_override.get("duplicate.key") {
    Some(value) => assert_eq(value, "final_value")
    None => @test.fail("Test failed")
  }
  
  // 测试大量条目
  let mut baggage_with_many = empty_baggage
  let mut i = 0
  while i < 100 {
    baggage_with_many = baggage_with_many.with_entry("key." + i.to_string(), "value." + i.to_string())
    i = i + 1
  }
  
  // 验证第一个和最后一个条目
  match baggage_with_many.get("key.0") {
    Some(value) => assert_eq(value, "value.0")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_many.get("key.99") {
    Some(value) => assert_eq(value, "value.99")
    None => @test.fail("Test failed")
  }
}

test "log_record_error_boundary" {
  // 测试LogRecord的错误边界情况
  
  // 测试极值时间戳
  let max_timestamp = 9223372036854775807L
  let min_timestamp = -9223372036854775808L
  let zero_timestamp = 0L
  
  let max_timestamp_log = LogRecord::{
    timestamp_unix_nanos: max_timestamp,
    observed_timestamp_unix_nanos: Some(max_timestamp),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Max timestamp log"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let min_timestamp_log = LogRecord::{
    timestamp_unix_nanos: min_timestamp,
    observed_timestamp_unix_nanos: Some(min_timestamp),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Min timestamp log"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let zero_timestamp_log = LogRecord::{
    timestamp_unix_nanos: zero_timestamp,
    observed_timestamp_unix_nanos: Some(zero_timestamp),
    severity_number: Debug,
    severity_text: Some("DEBUG"),
    body: Some("Zero timestamp log"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(max_timestamp_log.timestamp_unix_nanos, max_timestamp)
  assert_eq(min_timestamp_log.timestamp_unix_nanos, min_timestamp)
  assert_eq(zero_timestamp_log.timestamp_unix_nanos, zero_timestamp)
  
  // 测试空和特殊字符串
  let empty_body_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some(""),
    body: Some(""),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let special_chars_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Warn,
    severity_text: Some("!@#$%^&*()"),
    body: Some("!@#$%^&*()\n\r\t\\"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let unicode_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Fatal,
    severity_text: Some("Unicode🚀"),
    body: Some("Unicode消息🌟💫✨"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match empty_body_log.severity_text {
    Some(text) => assert_eq(text, "")
    None => @test.fail("Test failed")
  }
  
  match empty_body_log.body {
    Some(body) => assert_eq(body, "")
    None => @test.fail("Test failed")
  }
  
  match special_chars_log.severity_text {
    Some(text) => assert_eq(text, "!@#$%^&*()")
    None => @test.fail("Test failed")
  }
  
  match unicode_log.severity_text {
    Some(text) => assert_eq(text, "Unicode🚀")
    None => @test.fail("Test failed")
  }
  
  // 测试极长字符串
  let very_long_body = "a" * 10000
  let very_long_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("VERY_LONG"),
    body: Some(very_long_body),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match very_long_log.body {
    Some(body) => assert_eq(body.length() > 1000, true)
    None => @test.fail("Test failed")
  }
  
  // 测试复杂的属性集合
  let complex_attributes = [
    ("empty.string", AttributeValue::string("")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("unicode", AttributeValue::string("Unicode🚀")),
    ("very.long", AttributeValue::string("a" * 1000)),
    ("zero.int", AttributeValue::int(0L)),
    ("neg.int", AttributeValue::int(-1L)),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("zero.float", AttributeValue::float(0.0)),
    ("neg.float", AttributeValue::float(-1.0)),
    ("infinity", AttributeValue::float(1.0/0.0)),
    ("neg.infinity", AttributeValue::float(-1.0/0.0)),
    ("nan", AttributeValue::float(0.0/0.0)),
    ("true.bool", AttributeValue::bool(true)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    ("string.array", AttributeValue::array_string(["", " ", "!@#", "Unicode🚀"])),
    ("int.array", AttributeValue::array_int([0L, -1L, 9223372036854775807L])),
    ("float.array", AttributeValue::array_float([0.0, -1.0, 1.0/0.0, 0.0/0.0])),
    ("bool.array", AttributeValue::array_bool([true, false, true]))
  ]
  
  let complex_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("COMPLEX"),
    body: Some("Complex attributes log"),
    attributes: complex_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(complex_log.attributes.length(), 20)
  
  // 验证各种属性类型
  match complex_log.attributes[0].1 {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Test failed")
  }
  
  match complex_log.attributes[6].1 {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match complex_log.attributes[11].1 {
    FloatValue(f) => assert_eq(f != f, true)  // NaN
    _ => @test.fail("Test failed")
  }
  
  match complex_log.attributes[17].1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], "")
      assert_eq(arr[1], " ")
      assert_eq(arr[2], "!@#")
      assert_eq(arr[3], "Unicode🚀")
    }
    _ => @test.fail("Test failed")
  }
}

test "span_error_boundary" {
  // 测试Span的错误边界情况
  
  // 测试极值时间戳
  let max_timestamp = 9223372036854775807L
  let min_timestamp = -9223372036854775808L
  
  let max_timestamp_span = Span::{
    name: "max_timestamp_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: max_timestamp,
    end_time_unix_nanos: Some(max_timestamp),
    status: Ok,
    status_description: Some("Max timestamp span"),
    attributes: [],
    events: [],
    links: []
  }
  
  let min_timestamp_span = Span::{
    name: "min_timestamp_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: min_timestamp,
    end_time_unix_nanos: Some(min_timestamp),
    status: Error,
    status_description: Some("Min timestamp span"),
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(max_timestamp_span.start_time_unix_nanos, max_timestamp)
  assert_eq(min_timestamp_span.start_time_unix_nanos, min_timestamp)
  
  // 测试空和特殊字符串
  let empty_name_span = Span::{
    name: "",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let special_chars_span = Span::{
    name: "!@#$%^&*()_+-=[]{}|;':\",./<>?",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: Some("!@#$%^&*()"),
    attributes: [],
    events: [],
    links: []
  }
  
  let unicode_span = Span::{
    name: "Unicode测试🚀",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: Some("Unicode描述🌟"),
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(empty_name_span.name, "")
  assert_eq(special_chars_span.name, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
  assert_eq(unicode_span.name, "Unicode测试🚀")
  
  // 测试极长名称
  let very_long_name = "a" * 1000
  let very_long_span = Span::{
    name: very_long_name,
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  assert_eq(very_long_span.name.length() > 100, true)
  
  // 测试复杂的属性集合
  let complex_attributes = [
    ("empty.string", AttributeValue::string("")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("unicode", AttributeValue::string("Unicode🚀")),
    ("zero.int", AttributeValue::int(0L)),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("infinity", AttributeValue::float(1.0/0.0)),
    ("nan", AttributeValue::float(0.0/0.0)),
    ("true.bool", AttributeValue::bool(true)),
    ("empty.array", AttributeValue::array_string([])),
    ("mixed.array", AttributeValue::array_string(["", "!@", "Unicode🚀"]))
  ]
  
  let complex_span = Span::{
    name: "complex_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: complex_attributes,
    events: [],
    links: []
  }
  
  assert_eq(complex_span.attributes.length(), 10)
  
  // 测试复杂的事件集合
  let complex_events = [
    SpanEvent::{
      name: "",
      timestamp_unix_nanos: 1640995200000000000L,
      attributes: []
    },
    SpanEvent::{
      name: "!@#$%^&*()",
      timestamp_unix_nanos: 1640995200000000100L,
      attributes: [("special.chars", AttributeValue::string("!@#$%^&*()"))]
    },
    SpanEvent::{
      name: "Unicode事件🚀",
      timestamp_unix_nanos: 1640995200000000200L,
      attributes: [("unicode", AttributeValue::string("Unicode🌟"))]
    },
    SpanEvent::{
      name: "complex_event",
      timestamp_unix_nanos: 1640995200000000300L,
      attributes: [
        ("empty.string", AttributeValue::string("")),
        ("max.int", AttributeValue::int(9223372036854775807L)),
        ("infinity", AttributeValue::float(1.0/0.0)),
        ("true.bool", AttributeValue::bool(true)),
        ("mixed.array", AttributeValue::array_string(["", "!@", "Unicode🚀"]))
      ]
    }
  ]
  
  let events_span = Span::{
    name: "events_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: complex_events,
    links: []
  }
  
  assert_eq(events_span.events.length(), 4)
  assert_eq(events_span.events[0].name, "")
  assert_eq(events_span.events[1].name, "!@#$%^&*()")
  assert_eq(events_span.events[2].name, "Unicode事件🚀")
  assert_eq(events_span.events[3].attributes.length(), 5)
  
  // 测试复杂的链接集合
  let complex_links = [
    SpanLink::{
      context: SpanContext::{
        trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
        span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
        trace_flags: 0_byte,
        trace_state: ""
      },
      attributes: []
    },
    SpanLink::{
      context: SpanContext::{
        trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte }),
        span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte }),
        trace_flags: 0xff_byte,
        trace_state: "!@#$%^&*()"
      },
      attributes: [("special.state", AttributeValue::string("!@#$%^&*()"))]
    },
    SpanLink::{
      context: SpanContext::{
        trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x12_byte }),
        span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x34_byte }),
        trace_flags: 1_byte,
        trace_state: "Unicode状态🚀"
      },
      attributes: [
        ("unicode", AttributeValue::string("Unicode🌟")),
        ("complex", AttributeValue::array_string(["", "!@", "Unicode🚀"]))
      ]
    }
  ]
  
  let links_span = Span::{
    name: "links_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: complex_links
  }
  
  assert_eq(links_span.links.length(), 3)
  assert_eq(links_span.links[0].attributes.length(), 0)
  assert_eq(links_span.links[1].context.trace_flags, 0xff_byte)
  assert_eq(links_span.links[2].context.trace_state, "Unicode状态🚀")
}

test "span_context_error_boundary" {
  // 测试SpanContext的错误边界情况
  
  // 测试全零的trace_id和span_id
  let zero_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let zero_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  
  let zero_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  // 测试全1的trace_id和span_id
  let max_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte })
  let max_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte })
  
  let max_context = SpanContext::{
    trace_id: max_trace_id,
    span_id: max_span_id,
    trace_flags: 0xff_byte,
    trace_state: ""
  }
  
  // 测试交替模式的trace_id和span_id
  let alt_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(i) { 
    if i % 2 == 0 { 0xaa_byte } else { 0x55_byte }
  }]
  let alt_span_id = [for i = 0; i < 8; i = i + 1].map(fn(i) { 
    if i % 2 == 0 { 0xaa_byte } else { 0x55_byte }
  }]
  
  let alt_context = SpanContext::{
    trace_id: alt_trace_id,
    span_id: alt_span_id,
    trace_flags: 0xa5_byte,
    trace_state: ""
  }
  
  // 验证各种上下文
  assert_eq(zero_context.trace_id.length(), 16)
  assert_eq(zero_context.span_id.length(), 8)
  assert_eq(zero_context.trace_flags, 0_byte)
  
  assert_eq(max_context.trace_id.length(), 16)
  assert_eq(max_context.span_id.length(), 8)
  assert_eq(max_context.trace_flags, 0xff_byte)
  
  assert_eq(alt_context.trace_id.length(), 16)
  assert_eq(alt_context.span_id.length(), 8)
  assert_eq(alt_context.trace_flags, 0xa5_byte)
  
  // 测试极值trace_flags
  let min_flags_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let max_flags_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0xff_byte,
    trace_state: ""
  }
  
  assert_eq(min_flags_context.trace_flags, 0_byte)
  assert_eq(max_flags_context.trace_flags, 0xff_byte)
  
  // 测试空和特殊trace_state
  let empty_state_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let space_state_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: " "
  }
  
  let special_state_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  }
  
  let unicode_state_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: "Unicode状态🚀"
  }
  
  let very_long_state_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: "a" * 1000
  }
  
  assert_eq(empty_state_context.trace_state, "")
  assert_eq(space_state_context.trace_state, " ")
  assert_eq(special_state_context.trace_state, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
  assert_eq(unicode_state_context.trace_state, "Unicode状态🚀")
  assert_eq(very_long_state_context.trace_state.length() > 100, true)
  
  // 测试复杂的trace_state格式
  let complex_state_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: "key1=value1,key2=value2;prop1=val1,key3=value3;prop1=val1;prop2=val2"
  }
  
  let empty_key_state_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: "=value1,key2=,key3=value3"
  }
  
  let special_key_value_state_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: "!@#$%^&*()=!@#$%^&*(),Unicode键=Unicode值🚀"
  }
  
  assert_eq(complex_state_context.trace_state, "key1=value1,key2=value2;prop1=val1,key3=value3;prop1=val1;prop2=val2")
  assert_eq(empty_key_state_context.trace_state, "=value1,key2=,key3=value3")
  assert_eq(special_key_value_state_context.trace_state, "!@#$%^&*()=!@#$%^&*(),Unicode键=Unicode值🚀")
}

test "propagation_error_boundary" {
  // 测试Propagation的错误边界情况
  
  // 测试空carrier
  let empty_carrier = MapCarrier::new()
  let ctx = Context::empty()
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 从空carrier提取
  let extracted_from_empty = trace_propagator.extract(ctx, empty_carrier)
  let baggage_from_empty = baggage_propagator.extract(ctx, empty_carrier)
  
  // 测试注入到空carrier
  trace_propagator.inject(ctx, empty_carrier)
  baggage_propagator.inject(ctx, empty_carrier)
  
  // 测试带有特殊字符的carrier
  let special_data = [
    ("", "empty_key"),
    (" ", "space_key"),
    ("!@#$%^&*()", "!@#$%^&*()"),
    ("Unicode键🚀", "Unicode值🌟"),
    ("very_long_key", "a" * 1000),
    ("normal_key", "very_long_value_" + ("b" * 1000))
  ]
  
  let special_carrier = MapCarrier::from_map(special_data)
  
  // 从特殊carrier提取
  let extracted_from_special = trace_propagator.extract(ctx, special_carrier)
  let baggage_from_special = baggage_propagator.extract(ctx, special_carrier)
  
  // 测试注入到特殊carrier
  trace_propagator.inject(ctx, special_carrier)
  baggage_propagator.inject(ctx, special_carrier)
  
  // 验证carrier操作不会崩溃
  assert_eq(true, true)
  
  // 测试复合propagator的错误处理
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 使用复合propagator进行注入和提取
  composite_propagator.inject(ctx, empty_carrier)
  let extracted_composite = composite_propagator.extract(ctx, empty_carrier)
  
  composite_propagator.inject(ctx, special_carrier)
  let extracted_special_composite = composite_propagator.extract(ctx, special_carrier)
  
  // 验证复合propagator操作不会崩溃
  assert_eq(true, true)
  
  // 测试空propagator数组
  let empty_composite_propagator = CompositePropagator::new([])
  
  empty_composite_propagator.inject(ctx, empty_carrier)
  let extracted_empty_composite = empty_composite_propagator.extract(ctx, empty_carrier)
  
  // 验证空propagator数组操作不会崩溃
  assert_eq(true, true)
}