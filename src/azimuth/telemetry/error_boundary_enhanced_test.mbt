test "Error handling and boundary conditions comprehensive test" {
  // 测试Common模块的错误处理和边界条件
  
  // 测试空字符串和特殊字符
  let empty_string_attr = common::AttributeValue::string("")
  match empty_string_attr {
    common::StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected empty string value")
  }
  
  let special_chars_attr = common::AttributeValue::string("Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  match special_chars_attr {
    common::StringValue(s) => assert_eq(s, "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
    _ => @test.fail("Expected special chars string value")
  }
  
  let unicode_attr = common::AttributeValue::string("Unicode: 中文 🚀 ñáéíóú")
  match unicode_attr {
    common::StringValue(s) => assert_eq(s, "Unicode: 中文 🚀 ñáéíóú")
    _ => @test.fail("Expected unicode string value")
  }
  
  // 测试极值
  let max_int_attr = common::AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int_attr = common::AttributeValue::int(-9223372036854775808L) // Int64最小值
  let zero_int_attr = common::AttributeValue::int(0L)
  
  match max_int_attr {
    common::IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected max int value")
  }
  
  match min_int_attr {
    common::IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Expected min int value")
  }
  
  match zero_int_attr {
    common::IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Expected zero int value")
  }
  
  // 测试浮点数边界
  let max_float_attr = common::AttributeValue::float(1.7976931348623157e+308)  // Double最大值
  let min_float_attr = common::AttributeValue::float(-1.7976931348623157e+308) // Double最小值
  let inf_float_attr = common::AttributeValue::float(1.0/0.0)  // 无穷大
  let neg_inf_float_attr = common::AttributeValue::float(-1.0/0.0)  // 负无穷大
  let nan_float_attr = common::AttributeValue::float(0.0/0.0)  // NaN
  let zero_float_attr = common::AttributeValue::float(0.0)
  
  match max_float_attr {
    common::FloatValue(f) => assert_eq(f, 1.7976931348623157e+308)
    _ => @test.fail("Expected max float value")
  }
  
  match min_float_attr {
    common::FloatValue(f) => assert_eq(f, -1.7976931348623157e+308)
    _ => @test.fail("Expected min float value")
  }
  
  match zero_float_attr {
    common::FloatValue(f) => assert_eq(f, 0.0)
    _ => @test.fail("Expected zero float value")
  }
  
  // 测试布尔值
  let true_attr = common::AttributeValue::bool(true)
  let false_attr = common::AttributeValue::bool(false)
  
  match true_attr {
    common::BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected true value")
  }
  
  match false_attr {
    common::BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected false value")
  }
  
  // 测试数组边界条件
  let empty_string_array = common::AttributeValue::array_string([])
  let empty_int_array = common::AttributeValue::array_int([])
  let empty_float_array = common::AttributeValue::array_float([])
  let empty_bool_array = common::AttributeValue::array_bool([])
  
  match empty_string_array {
    common::ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty string array")
  }
  
  match empty_int_array {
    common::ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty int array")
  }
  
  match empty_float_array {
    common::ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty float array")
  }
  
  match empty_bool_array {
    common::ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty bool array")
  }
  
  // 测试单元素数组
  let single_string_array = common::AttributeValue::array_string(["single"])
  let single_int_array = common::AttributeValue::array_int([42L])
  let single_float_array = common::AttributeValue::array_float([3.14])
  let single_bool_array = common::AttributeValue::array_bool([true])
  
  match single_string_array {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => @test.fail("Expected single string array")
  }
  
  // 测试大数组
  let mut large_string_list = []
  for i = 0; i < 1000; i = i + 1 {
    large_string_list = large_string_list + ["item_" + i.to_string()]
  }
  let large_string_array = common::AttributeValue::array_string(large_string_list)
  
  match large_string_array {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], "item_0")
      assert_eq(arr[999], "item_999")
    }
    _ => @test.fail("Expected large string array")
  }
  
  // 测试Resource边界条件
  let empty_resource = common::Resource::default("")
  assert_eq(empty_resource.service_name, "")
  assert_eq(empty_resource.attributes.length(), 0)
  
  let very_long_name_resource = common::Resource::default(
    "this-is-a-very-long-service-name-that-exceeds-normal-lengths-and-tests-boundary-conditions-for-resource-creation-with-extremely-long-names"
  )
  assert_eq(very_long_name_resource.service_name.length(), 147)
  
  // 测试包含特殊字符的Resource
  let mut special_chars_resource = common::Resource::default("service-with-special-chars")
  special_chars_resource.attributes = [
    ("unicode", common::AttributeValue::string("测试服务")),
    ("emoji", common::AttributeValue::string("🚀🔥💯")),
    ("quotes", common::AttributeValue::string("String with 'single' and \"double\" quotes")),
    ("newlines", common::AttributeValue::string("Line 1\nLine 2\nLine 3")),
    ("tabs", common::AttributeValue::string("Col1\tCol2\tCol3"))
  ]
  
  // 测试InstrumentationScope边界条件
  let empty_scope = common::InstrumentationScope{
    name: "",
    version: None,
    schema_url: None
  }
  assert_eq(empty_scope.name, "")
  
  let long_scope = common::InstrumentationScope{
    name: "this-is-a-very-long-instrumentation-scope-name-that-tests-boundary-conditions",
    version: Some("version-with-many-characters-and-special-symbols-123!@#$%^&*()"),
    schema_url: Some("https://very-long-url-with-many-segments.example.com/very/deep/nested/path/to/schema/with/version/1.0.0")
  }
  assert_eq(long_scope.name.length(), 81)
  
  // 测试Logs模块的错误处理和边界条件
  
  // 测试所有严重级别
  let trace_log = logs::LogRecord::builder()
    .severity(logs::Trace)
    .body("Trace message")
    .build()
  assert_eq(trace_log.severity_number, logs::Trace)
  
  let debug_log = logs::LogRecord::builder()
    .severity(logs::Debug)
    .body("Debug message")
    .build()
  assert_eq(debug_log.severity_number, logs::Debug)
  
  let info_log = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Info message")
    .build()
  assert_eq(info_log.severity_number, logs::Info)
  
  let warn_log = logs::LogRecord::builder()
    .severity(logs::Warn)
    .body("Warning message")
    .build()
  assert_eq(warn_log.severity_number, logs::Warn)
  
  let error_log = logs::LogRecord::builder()
    .severity(logs::Error)
    .body("Error message")
    .build()
  assert_eq(error_log.severity_number, logs::Error)
  
  let fatal_log = logs::LogRecord::builder()
    .severity(logs::Fatal)
    .body("Fatal message")
    .build()
  assert_eq(fatal_log.severity_number, logs::Fatal)
  
  // 测试空和极值日志
  let empty_log = logs::LogRecord::builder().build()
  assert_eq(empty_log.timestamp_unix_nanos, 0L)
  assert_eq(empty_log.severity_number, logs::Info)  // 默认值
  assert_eq(empty_log.body, None)
  assert_eq(empty_log.attributes.length(), 0)
  
  let max_time_log = logs::LogRecord::builder()
    .timestamp(9223372036854775807L)  // Int64最大值
    .build()
  assert_eq(max_time_log.timestamp_unix_nanos, 9223372036854775807L)
  
  let min_time_log = logs::LogRecord::builder()
    .timestamp(-9223372036854775808L)  // Int64最小值
    .build()
  assert_eq(min_time_log.timestamp_unix_nanos, -9223372036854775808L)
  
  // 测试极长日志消息
  let very_long_message = "This is an extremely long log message that tests the boundary conditions of the logging system. ".repeat(100)
  let long_log = logs::LogRecord::builder()
    .body(very_long_message)
    .build()
  
  match long_log.body {
    Some(body) => assert_eq(body.length(), very_long_message.length())
    None => @test.fail("Expected body to be Some for long message")
  }
  
  // 测试包含特殊字符的日志消息
  let special_chars_log = logs::LogRecord::builder()
    .body("Special chars: \n\t\r\"'\\!@#$%^&*(){}[]|;:<>?,./ 中文 🚀")
    .build()
  
  match special_chars_log.body {
    Some(body) => assert_eq(body, "Special chars: \n\t\r\"'\\!@#$%^&*(){}[]|;:<>?,./ 中文 🚀")
    None => @test.fail("Expected body to be Some for special chars")
  }
  
  // 测试大量属性的日志
  let mut log_builder = logs::LogRecord::builder()
  for i = 0; i < 100; i = i + 1 {
    log_builder = log_builder.with_attribute("attr_" + i.to_string(), common::AttributeValue::int(i.to_int64()))
  }
  let many_attrs_log = log_builder.build()
  assert_eq(many_attrs_log.attributes.length(), 100)
  
  // 测试Trace模块的错误处理和边界条件
  
  // 测试所有SpanKind
  let kinds = [trace::Internal, trace::Server, trace::Client, trace::Producer, trace::Consumer]
  for i = 0; i < kinds.length(); i = i + 1 {
    let span = trace::Span{
      name: "test-span",
      context: trace::SpanContext{
        trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { i.to_byte() }),
        span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { i.to_byte() }),
        trace_flags: 1_byte,
        trace_state: ""
      },
      kind: kinds[i],
      parent_span_id: None,
      start_time_unix_nanos: 1000000L,
      end_time_unix_nanos: None,
      status: trace::Unset,
      status_description: None,
      attributes: [],
      events: [],
      links: []
    }
    assert_eq(span.kind, kinds[i])
  }
  
  // 测试所有StatusCode
  let statuses = [trace::Unset, trace::Ok, trace::Error]
  for i = 0; i < statuses.length(); i = i + 1 {
    let span = trace::Span{
      name: "test-span",
      context: trace::SpanContext{
        trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { 0_byte }),
        span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { 0_byte }),
        trace_flags: 0_byte,
        trace_state: ""
      },
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: 1000000L,
      end_time_unix_nanos: Some(2000000L),
      status: statuses[i],
      status_description: Some("Test status"),
      attributes: [],
      events: [],
      links: []
    }
    assert_eq(span.status, statuses[i])
  }
  
  // 测试空和极值Span
  let empty_span = trace::Span{
    name: "",
    context: trace::SpanContext{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: Some(0L),
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(empty_span.name, "")
  
  let extreme_time_span = trace::Span{
    name: "extreme-time-span",
    context: trace::SpanContext{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 255_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 255_byte }),
      trace_flags: 255_byte,
      trace_state: "very-long-trace-state-that-tests-boundary-conditions"
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 9223372036854775807L,  // Int64最大值
    end_time_unix_nanos: Some(9223372036854775807L),
    status: trace::Error,
    status_description: Some("Very long status description that tests boundary conditions and ensures the system can handle lengthy descriptions without any issues"),
    attributes: [],
    events: [],
    links: []
  }
  assert_eq(extreme_time_span.start_time_unix_nanos, 9223372036854775807L)
  
  // 测试Metrics模块的错误处理和边界条件
  
  // 测试极值指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("boundary-test-meter", None, None)
  
  let counter = meter.create_counter("", None, None)  // 空名称
  counter.add(9223372036854775807L, [])  // 最大值
  counter.add(-9223372036854775808L, []) // 最小值
  counter.add(0L, [])                    // 零值
  
  let histogram = meter.create_histogram("histogram", None, None)
  histogram.record(1.7976931348623157e+308, [])  // 最大值
  histogram.record(-1.7976931348623157e+308, []) // 最小值
  histogram.record(0.0, [])                       // 零值
  histogram.record(1.0/0.0, [])                    // 无穷大
  histogram.record(-1.0/0.0, [])                   // 负无穷大
  histogram.record(0.0/0.0, [])                    // NaN
  
  let up_down_counter = meter.create_up_down_counter("up_down", None, None)
  up_down_counter.add(9223372036854775807L, [])
  up_down_counter.add(-9223372036854775808L, [])
  
  let gauge = meter.create_gauge("gauge", None, None)
  gauge.record(1.7976931348623157e+308, [])
  gauge.record(-1.7976931348623157e+308, [])
  gauge.record(0.0, [])
  
  // 测试带有大量属性的指标
  let mut many_attributes = []
  for i = 0; i < 200; i = i + 1 {
    many_attributes = many_attributes + [("attr_" + i.to_string(), common::AttributeValue::int(i.to_int64()))]
  }
  
  let many_attrs_counter = meter.create_counter("many_attrs_counter", None, None)
  many_attrs_counter.add(1L, many_attributes)
  
  // 测试特殊字符的仪器名称
  let special_chars_counter = meter.create_counter(
    "counter.with-special-chars_and.numbers123",
    Some("special-unit!@#$%"),
    Some("Description with special chars: 中文 🚀 and symbols !@#$%^&*()")
  )
  special_chars_counter.add(1L, [])
  
  // 测试跨模块的错误处理
  
  // 测试无效的trace context
  let invalid_trace_context = trace::SpanContext{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  // 使用无效context创建span
  let invalid_span = trace::Span{
    name: "invalid-context-span",
    context: invalid_trace_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: trace::Error,
    status_description: Some("Created with invalid context"),
    attributes: [],
    events: [],
    links: []
  }
  
  // 测试带有无效属性的日志
  let invalid_attrs_log = logs::LogRecord::builder()
    .with_attribute("", common::AttributeValue::string(""))  // 空键和值
    .with_attribute("valid_key", common::AttributeValue::string(""))
    .build()
  
  // 测试Noop实现的错误处理
  let noop_logger = logs::NoopLogger::{}
  let noop_tracer = trace::NoopTracer::{}
  let noop_meter = metrics::NoopMeter::{}
  
  // 这些调用不应该崩溃
  noop_logger.emit(invalid_attrs_log)
  noop_logger.debug("", [])  // 空消息
  noop_logger.info("\0\0\0", [])  // 空字符
  
  let dummy_context = context::Context::{}
  let (_, invalid_span_result) = noop_tracer.start_span(
    dummy_context,
    "",  // 空span名称
    Some(trace::Internal),
    [("", common::AttributeValue::string(""))],  // 空属性
    Some(-9223372036854775808L)  // 最小时间戳
  )
  
  noop_meter.create_counter("", None, None).add(0L, [("", common::AttributeValue::string(""))])
  noop_meter.create_histogram("", None, None).record(0.0/0.0, [("", common::AttributeValue::string(""))])
  
  // 测试内存和性能边界
  let start_time = 1640995200000000000L
  
  // 创建大量对象来测试内存使用
  let mut many_logs = []
  for i = 0; i < 100; i = i + 1 {
    let log = logs::LogRecord::builder()
      .timestamp(start_time + i.to_int64())
      .severity(logs::Info)
      .body("Log message " + i.to_string())
      .with_attribute("index", common::AttributeValue::int(i.to_int64()))
      .build()
    many_logs = many_logs + [log]
  }
  assert_eq(many_logs.length(), 100)
  
  let mut many_spans = []
  for i = 0; i < 50; i = i + 1 {
    let span = trace::Span{
      name: "span_" + i.to_string(),
      context: trace::SpanContext{
        trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { i.to_byte() }),
        span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { i.to_byte() }),
        trace_flags: 1_byte,
        trace_state: ""
      },
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: start_time + i.to_int64(),
      end_time_unix_nanos: Some(start_time + i.to_int64() + 1000L),
      status: trace::Ok,
      status_description: None,
      attributes: [("index", common::AttributeValue::int(i.to_int64()))],
      events: [],
      links: []
    }
    many_spans = many_spans + [span]
  }
  assert_eq(many_spans.length(), 50)
}