// 错误处理和边界条件综合测试用例
// 测试整个Telemetry套件的错误处理和边界条件

test "common_module_error_handling" {
  // 测试common模块的错误处理和边界条件
  
  // 测试AttributeValue的边界条件
  let max_int_value = AttributeValue::int(9223372036854775807L)
  let min_int_value = AttributeValue::int(-9223372036854775808L)
  let zero_int_value = AttributeValue::int(0L)
  
  // 验极值
  match max_int_value {
    IntValue(value) => assert_eq(value, 9223372036854775807L)
    _ => assert_eq(false, true)
  }
  
  match min_int_value {
    IntValue(value) => assert_eq(value, -9223372036854775808L)
    _ => assert_eq(false, true)
  }
  
  match zero_int_value {
    IntValue(value) => assert_eq(value, 0L)
    _ => assert_eq(false, true)
  }
  
  // 测试浮点边界值
  let max_float_value = AttributeValue::float(1.7976931348623157e+308)
  let min_float_value = AttributeValue::float(2.2250738585072014e-308)
  let negative_inf_value = AttributeValue::float(-1.0 / 0.0)  // 负无穷
  let positive_inf_value = AttributeValue::float(1.0 / 0.0)   // 正无穷
  let nan_value = AttributeValue::float(0.0 / 0.0)            // NaN
  
  match max_float_value {
    FloatValue(value) => assert_eq(value > 1.0e+308, true)
    _ => assert_eq(false, true)
  }
  
  match min_float_value {
    FloatValue(value) => assert_eq(value > 0.0 && value < 1.0e-307, true)
    _ => assert_eq(false, true)
  }
  
  // 测试空数组和单元素数组
  let empty_string_array = AttributeValue::array_string([])
  let single_string_array = AttributeValue::array_string(["only"])
  let empty_int_array = AttributeValue::array_int([])
  let single_int_array = AttributeValue::array_int([42L])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_eq(false, true)
  }
  
  match single_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "only")
    }
    _ => assert_eq(false, true)
  }
  
  // 测试Resource的边界条件
  let empty_name_resource = common::Resource::default("")
  let very_long_name_resource = common::Resource::default("a".repeat(10000))
  let unicode_name_resource = common::Resource::default("服务名称.🌟.测试")
  
  assert_eq(empty_name_resource.service_name.length(), 0)
  assert_eq(very_long_name_resource.service_name.length(), 10000)
  assert_eq(unicode_name_resource.service_name.contains("测试"), true)
  
  // 测试InstrumentationScope的边界条件
  let empty_scope = common::InstrumentationScope::{
    name: "",
    version: Some(""),
    schema_url: Some("")
  }
  
  assert_eq(empty_scope.name.length(), 0)
  match empty_scope.version {
    Some(version) => assert_eq(version.length(), 0)
    None => assert_eq(false, true)
  }
  match empty_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url.length(), 0)
    None => assert_eq(false, true)
  }
}

test "trace_module_error_handling" {
  // 测试trace模块的错误处理和边界条件
  
  let initial_context = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer")
  
  // 测试空名称的Span
  let (empty_name_ctx, empty_name_span) = tracer.start_span(initial_context, "")
  assert_eq(empty_name_span.name.length(), 0)
  
  // 测试非常长名称的Span
  let long_name = "a".repeat(10000)
  let (long_name_ctx, long_name_span) = tracer.start_span(initial_context, long_name)
  assert_eq(long_name_span.name.length(), 10000)
  
  // 测试包含特殊字符的Span名称
  let special_name = "span-with_special.chars!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let (special_name_ctx, special_name_span) = tracer.start_span(initial_context, special_name)
  assert_eq(special_name_span.name, special_name)
  
  // 测试Unicode Span名称
  let unicode_name = "span名称.🌟.测试"
  let (unicode_name_ctx, unicode_name_span) = tracer.start_span(initial_context, unicode_name)
  assert_eq(unicode_name_span.name, unicode_name)
  
  // 测试边界时间戳
  let zero_time_span = tracer.start_span(initial_context, "zero-time", trace::Internal, [], Some(0L))
  assert_eq(zero_time_span.1.start_time_unix_nanos, 0L)
  
  let max_time_span = tracer.start_span(initial_context, "max-time", trace::Internal, [], Some(9223372036854775807L))
  assert_eq(max_time_span.1.start_time_unix_nanos, 9223372036854775807L)
  
  let negative_time_span = tracer.start_span(initial_context, "negative-time", trace::Internal, [], Some(-1L))
  assert_eq(negative_time_span.1.start_time_unix_nanos, -1L)
  
  // 测试复杂的Span属性
  let complex_attributes = [
    ("empty.string", AttributeValue::string("")),
    ("long.string", AttributeValue::string("a".repeat(1000))),
    ("unicode.string", AttributeValue::string("unicode.测试.🌟")),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("max.float", AttributeValue::float(1.7976931348623157e+308)),
    ("min.float", AttributeValue::float(2.2250738585072014e-308)),
    ("empty.array", AttributeValue::array_string([])),
    ("large.array", AttributeValue::array_int([1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L]))
  ]
  
  let (complex_ctx, complex_span) = tracer.start_span(
    initial_context,
    "complex-span",
    trace::Internal,
    complex_attributes
  )
  
  assert_eq(complex_span.attributes.length(), 9)
  
  // 测试Span事件和链接的边界条件
  let empty_event = trace::SpanEvent::{
    name: "",
    timestamp_unix_nanos: 0L,
    attributes: []
  }
  
  let max_event = trace::SpanEvent::{
    name: "a".repeat(1000),
    timestamp_unix_nanos: 9223372036854775807L,
    attributes: [
      ("max.attribute", AttributeValue::string("a".repeat(1000)))
    ]
  }
  
  let events_span = trace::Span::{
    ..complex_span,
    events: [empty_event, max_event]
  }
  
  assert_eq(events_span.events.length(), 2)
  assert_eq(events_span.events[0].name.length(), 0)
  assert_eq(events_span.events[1].name.length(), 1000)
  
  // 测试SpanContext的边界条件
  let zero_trace_id = Array::make(16, 0_byte)
  let max_trace_id = Array::make(16, 255_byte)
  let zero_span_id = Array::make(8, 0_byte)
  let max_span_id = Array::make(8, 255_byte)
  
  let zero_context = trace::SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let max_context = trace::SpanContext::{
    trace_id: max_trace_id,
    span_id: max_span_id,
    trace_flags: 255_byte,
    trace_state: "a".repeat(1000)
  }
  
  assert_eq(zero_context.trace_id[0], 0_byte)
  assert_eq(max_context.trace_id[0], 255_byte)
  assert_eq(zero_context.trace_flags, 0_byte)
  assert_eq(max_context.trace_flags, 255_byte)
  assert_eq(zero_context.trace_state.length(), 0)
  assert_eq(max_context.trace_state.length(), 1000)
}

test "logs_module_error_handling" {
  // 测试logs模块的错误处理和边界条件
  
  let builder = logs::LogRecord::builder()
  
  // 测试边界时间戳
  let zero_timestamp_log = builder
    .timestamp(0L)
    .severity(logs::Info)
    .body("zero timestamp")
    .build()
  
  assert_eq(zero_timestamp_log.timestamp_unix_nanos, 0L)
  
  let max_timestamp_log = builder
    .timestamp(9223372036854775807L)
    .severity(logs::Info)
    .body("max timestamp")
    .build()
  
  assert_eq(max_timestamp_log.timestamp_unix_nanos, 9223372036854775807L)
  
  let negative_timestamp_log = builder
    .timestamp(-1L)
    .severity(logs::Info)
    .body("negative timestamp")
    .build()
  
  assert_eq(negative_timestamp_log.timestamp_unix_nanos, -1L)
  
  // 测试边界观察时间戳
  let log_with_observed = logs::LogRecord::{
    ..zero_timestamp_log,
    observed_timestamp_unix_nanos: Some(9223372036854775807L)
  }
  
  match log_with_observed.observed_timestamp_unix_nanos {
    Some(observed) => assert_eq(observed, 9223372036854775807L)
    None => assert_eq(false, true)
  }
  
  // 测试所有严重性级别
  let trace_log = builder.severity(logs::Trace).body("trace").build()
  let debug_log = builder.severity(logs::Debug).body("debug").build()
  let info_log = builder.severity(logs::Info).body("info").build()
  let warn_log = builder.severity(logs::Warn).body("warn").build()
  let error_log = builder.severity(logs::Error).body("error").build()
  let fatal_log = builder.severity(logs::Fatal).body("fatal").build()
  
  match trace_log.severity_number { logs::Trace => assert_eq(true, true) _ => assert_eq(false, true) }
  match debug_log.severity_number { logs::Debug => assert_eq(true, true) _ => assert_eq(false, true) }
  match info_log.severity_number { logs::Info => assert_eq(true, true) _ => assert_eq(false, true) }
  match warn_log.severity_number { logs::Warn => assert_eq(true, true) _ => assert_eq(false, true) }
  match error_log.severity_number { logs::Error => assert_eq(true, true) _ => assert_eq(false, true) }
  match fatal_log.severity_number { logs::Fatal => assert_eq(true, true) _ => assert_eq(false, true) }
  
  // 测试边界严重性文本
  let log_with_empty_severity_text = logs::LogRecord::{
    ..info_log,
    severity_text: Some("")
  }
  
  let log_with_long_severity_text = logs::LogRecord::{
    ..info_log,
    severity_text: Some("a".repeat(1000))
  }
  
  let log_with_unicode_severity_text = logs::LogRecord::{
    ..info_log,
    severity_text: Some("严重性文本.🌟.测试")
  }
  
  match log_with_empty_severity_text.severity_text {
    Some(text) => assert_eq(text.length(), 0)
    None => assert_eq(false, true)
  }
  
  match log_with_long_severity_text.severity_text {
    Some(text) => assert_eq(text.length(), 1000)
    None => assert_eq(false, true)
  }
  
  match log_with_unicode_severity_text.severity_text {
    Some(text) => assert_eq(text.contains("测试"), true)
    None => assert_eq(false, true)
  }
  
  // 测试边界日志内容
  let empty_body_log = builder.body("").build()
  let long_body_log = builder.body("a".repeat(10000)).build()
  let unicode_body_log = builder.body("日志内容.🌟.测试").build()
  let special_body_log = builder.body("special.chars!@#$%^&*()_+-=[]{}|;':\",./<>?").build()
  
  match empty_body_log.body {
    Some(body) => assert_eq(body.length(), 0)
    None => assert_eq(false, true)
  }
  
  match long_body_log.body {
    Some(body) => assert_eq(body.length(), 10000)
    None => assert_eq(false, true)
  }
  
  match unicode_body_log.body {
    Some(body) => assert_eq(body.contains("测试"), true)
    None => assert_eq(false, true)
  }
  
  match special_body_log.body {
    Some(body) => assert_eq(body.contains("!@#$%^&*()"), true)
    None => assert_eq(false, true)
  }
  
  // 测试复杂属性组合
  let complex_log = builder
    .body("complex log")
    .with_attribute("empty.value", AttributeValue::string(""))
    .with_attribute("long.value", AttributeValue::string("a".repeat(1000)))
    .with_attribute("unicode.value", AttributeValue::string("unicode.测试.🌟"))
    .with_attribute("max.int", AttributeValue::int(9223372036854775807L))
    .with_attribute("min.int", AttributeValue::int(-9223372036854775808L))
    .with_attribute("max.float", AttributeValue::float(1.7976931348623157e+308))
    .with_attribute("empty.array", AttributeValue::array_string([]))
    .with_attribute("large.array", AttributeValue::array_int([1L, 2L, 3L, 4L, 5L]))
    .build()
  
  assert_eq(complex_log.attributes.length(), 8)
  
  // 测试追踪上下文的边界条件
  let zero_trace_id = Array::make(16, 0_byte)
  let max_trace_id = Array::make(16, 255_byte)
  let zero_span_id = Array::make(8, 0_byte)
  let max_span_id = Array::make(8, 255_byte)
  
  let log_with_zero_trace = logs::LogRecord::{
    ..complex_log,
    trace_id: Some(zero_trace_id),
    span_id: Some(zero_span_id),
    trace_flags: Some(0_byte)
  }
  
  let log_with_max_trace = logs::LogRecord::{
    ..complex_log,
    trace_id: Some(max_trace_id),
    span_id: Some(max_span_id),
    trace_flags: Some(255_byte)
  }
  
  match log_with_zero_trace.trace_id {
    Some(trace_id) => assert_eq(trace_id[0], 0_byte)
    None => assert_eq(false, true)
  }
  
  match log_with_max_trace.trace_id {
    Some(trace_id) => assert_eq(trace_id[0], 255_byte)
    None => assert_eq(false, true)
  }
}

test "metrics_module_error_handling" {
  // 测试metrics模块的错误处理和边界条件
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter")
  
  // 测试边界instrument名称
  let empty_name_counter = meter.create_counter("", Some("count"), Some("empty name counter"))
  let long_name_counter = meter.create_counter("a".repeat(1000), Some("count"), Some("long name counter"))
  let special_name_counter = meter.create_counter("special!@#$%^&*()", Some("count"), Some("special name counter"))
  let unicode_name_counter = meter.create_counter("计数器.🌟.测试", Some("count"), Some("unicode name counter"))
  
  // 测试边界单位
  let empty_unit_histogram = meter.create_histogram("test-histogram", Some(""), Some("empty unit histogram"))
  let long_unit_histogram = meter.create_histogram("test-histogram", Some("a".repeat(100)), Some("long unit histogram"))
  let special_unit_histogram = meter.create_histogram("test-histogram", Some("!@#$%^&*()"), Some("special unit histogram"))
  let unicode_unit_histogram = meter.create_histogram("test-histogram", Some("单位.🌟.测试"), Some("unicode unit histogram"))
  
  // 测试边界描述
  let empty_desc_gauge = meter.create_gauge("test-gauge", Some("count"), Some(""))
  let long_desc_gauge = meter.create_gauge("test-gauge", Some("count"), Some("a".repeat(1000)))
  let special_desc_gauge = meter.create_gauge("test-gauge", Some("count"), Some("special.desc!@#$%^&*()"))
  let unicode_desc_gauge = meter.create_gauge("test-gauge", Some("count"), Some("描述.🌟.测试"))
  
  // 测试边界Counter操作
  empty_name_counter.add(0L)
  empty_name_counter.add(9223372036854775807L)  // 最大Int64
  empty_name_counter.add(-9223372036854775808L) // 最小Int64
  empty_name_counter.add(-1L)                    // 负值
  
  // 测试边界Counter属性
  empty_name_counter.add(1L, [
    ("empty.value", AttributeValue::string("")),
    ("long.value", AttributeValue::string("a".repeat(1000))),
    ("unicode.value", AttributeValue::string("unicode.测试.🌟")),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("empty.array", AttributeValue::array_string([])),
    ("large.array", AttributeValue::array_int([1L, 2L, 3L, 4L, 5L]))
  ])
  
  // 测试边界Histogram操作
  empty_unit_histogram.record(0.0)
  empty_unit_histogram.record(1.7976931348623157e+308)  // 最大Double
  empty_unit_histogram.record(2.2250738585072014e-308)  // 最小正Double
  empty_unit_histogram.record(-1.7976931348623157e+308) // 负最大值
  empty_unit_histogram.record(1.0 / 0.0)                 // 正无穷
  empty_unit_histogram.record(-1.0 / 0.0)                // 负无穷
  empty_unit_histogram.record(0.0 / 0.0)                  // NaN
  
  // 测试边界Histogram属性
  empty_unit_histogram.record(1.0, [
    ("max.float", AttributeValue::float(1.7976931348623157e+308)),
    ("min.float", AttributeValue::float(2.2250738585072014e-308)),
    ("inf.float", AttributeValue::float(1.0 / 0.0)),
    ("neg.inf.float", AttributeValue::float(-1.0 / 0.0)),
    ("nan.float", AttributeValue::float(0.0 / 0.0))
  ])
  
  // 测试边界UpDownCounter操作
  let up_down_counter = meter.create_up_down_counter("test-up-down")
  up_down_counter.add(0L)
  up_down_counter.add(9223372036854775807L)
  up_down_counter.add(-9223372036854775808L)
  up_down_counter.add(1L)
  up_down_counter.add(-1L)
  
  // 测试边界Gauge操作
  let gauge = meter.create_gauge("test-gauge")
  gauge.record(0.0)
  gauge.record(1.7976931348623157e+308)
  gauge.record(-1.7976931348623157e+308)
  gauge.record(2.2250738585072014e-308)
  gauge.record(1.0 / 0.0)
  gauge.record(-1.0 / 0.0)
  gauge.record(0.0 / 0.0)
  
  // 测试Measurement边界值
  let zero_measurement = metrics::Measurement::{
    value: 0.0,
    attributes: []
  }
  
  let max_measurement = metrics::Measurement::{
    value: 1.7976931348623157e+308,
    attributes: [("max.value", AttributeValue::float(1.7976931348623157e+308))]
  }
  
  let min_measurement = metrics::Measurement::{
    value: 2.2250738585072014e-308,
    attributes: [("min.value", AttributeValue::float(2.2250738585072014e-308))]
  }
  
  let negative_measurement = metrics::Measurement::{
    value: -1.7976931348623157e+308,
    attributes: [("negative.value", AttributeValue::float(-1.7976931348623157e+308))]
  }
  
  let inf_measurement = metrics::Measurement::{
    value: 1.0 / 0.0,
    attributes: [("inf.value", AttributeValue::float(1.0 / 0.0))]
  }
  
  let nan_measurement = metrics::Measurement::{
    value: 0.0 / 0.0,
    attributes: [("nan.value", AttributeValue::float(0.0 / 0.0))]
  }
  
  assert_eq(zero_measurement.value, 0.0)
  assert_eq(max_measurement.value > 1.0e+308, true)
  assert_eq(min_measurement.value > 0.0 && min_measurement.value < 1.0e-307, true)
  assert_eq(negative_measurement.value < -1.0e+308, true)
  assert_eq(inf_measurement.value > 1.0e+308, true)  // 无穷大检查
  
  // 验证所有measurement都能创建
  assert_eq(zero_measurement.attributes.length(), 0)
  assert_eq(max_measurement.attributes.length(), 1)
  assert_eq(min_measurement.attributes.length(), 1)
  assert_eq(negative_measurement.attributes.length(), 1)
  assert_eq(inf_measurement.attributes.length(), 1)
  assert_eq(nan_measurement.attributes.length(), 1)
}

test "propagation_module_error_handling" {
  // 测试propagation模块的错误处理和边界条件
  
  let initial_context = context::Context::empty()
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  // 测试边界MapCarrier
  let empty_carrier = propagation::MapCarrier::new()
  let large_carrier_data = [
    ("key1", "value1"),
    ("key2", "value2"),
    ("key3", "value3"),
    ("key4", "value4"),
    ("key5", "value5"),
    ("key6", "value6"),
    ("key7", "value7"),
    ("key8", "value8"),
    ("key9", "value9"),
    ("key10", "value10")
  ]
  let large_carrier = propagation::MapCarrier::from_map(large_carrier_data)
  
  // 测试边界键值
  let boundary_carrier_data = [
    ("", "empty key"),
    ("empty value", ""),
    ("long key", "a".repeat(1000)),
    ("long value", "value: " + "a".repeat(1000)),
    ("unicode key", "键.🌟"),
    ("unicode value", "值.🌟"),
    ("special chars", "!@#$%^&*()_+-=[]{}|;':\",./<>?"),
    ("mixed", "mix of 中文 and english and 🌟 symbols")
  ]
  let boundary_carrier = propagation::MapCarrier::from_map(boundary_carrier_data)
  
  // 测试inject操作
  trace_propagator.inject(initial_context, empty_carrier)
  trace_propagator.inject(initial_context, large_carrier)
  trace_propagator.inject(initial_context, boundary_carrier)
  
  baggage_propagator.inject(initial_context, empty_carrier)
  baggage_propagator.inject(initial_context, large_carrier)
  baggage_propagator.inject(initial_context, boundary_carrier)
  
  // 测试extract操作
  let empty_extract_context = trace_propagator.extract(initial_context, empty_carrier)
  let large_extract_context = trace_propagator.extract(initial_context, large_carrier)
  let boundary_extract_context = trace_propagator.extract(initial_context, boundary_carrier)
  
  let empty_baggage_context = baggage_propagator.extract(initial_context, empty_carrier)
  let large_baggage_context = baggage_propagator.extract(initial_context, large_carrier)
  let boundary_baggage_context = baggage_propagator.extract(initial_context, boundary_carrier)
  
  // 验证extract不会崩溃
  assert_eq(empty_extract_context.values.length(), initial_context.values.length())
  assert_eq(large_extract_context.values.length(), initial_context.values.length())
  assert_eq(boundary_extract_context.values.length(), initial_context.values.length())
  
  assert_eq(empty_baggage_context.values.length(), initial_context.values.length())
  assert_eq(large_baggage_context.values.length(), initial_context.values.length())
  assert_eq(boundary_baggage_context.values.length(), initial_context.values.length())
  
  // 测试边界traceparent格式
  let boundary_trace_parents = [
    "",  // 空字符串
    "invalid-format",  // 无效格式
    "00-0af7651916cd43dd8448eb211c80319c",  // 不完整
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331",  // 缺少flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra",  // 额外部分
    "ff-ffffffffffffffffffffffffffffffff-ffffffffffffffff-ff",  // 最大值
    "00-00000000000000000000000000000000-0000000000000000-00",  // 最小值
    "01-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",  // 不同版本
    "99-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"   // 高版本号
  ]
  
  let mut index = 0
  while index < boundary_trace_parents.length() {
    let trace_parent = boundary_trace_parents[index]
    let test_carrier = propagation::MapCarrier::from_map([
      (propagation::TRACE_PARENT_HEADER, trace_parent)
    ])
    
    // 测试extract不会崩溃
    let _extract_context = trace_propagator.extract(initial_context, test_carrier)
    assert_eq(true, true)  // 如果能执行到这里说明没有崩溃
    
    index = index + 1
  }
  
  // 测试边界baggage格式
  let boundary_baggages = [
    "",  // 空字符串
    "invalid",  // 无效格式（没有=）
    "key=",  // 空值
    "=value",  // 空键
    "key=value,",  // 结尾逗号
    ",key=value",  // 开头逗号
    "key=value,,key2=value2",  // 双逗号
    "key=value;prop1=val1,key2=value2;prop2=val2",  // 复杂属性
    "a".repeat(1000) + "=" + "b".repeat(1000),  // 长键值
    "unicode=测试",  // Unicode
    "special=!@#$%^&*()_+-=[]{}|;':\",./<>?"  // 特殊字符
  ]
  
  let mut index2 = 0
  while index2 < boundary_baggages.length() {
    let baggage = boundary_baggages[index2]
    let test_carrier = propagation::MapCarrier::from_map([
      (propagation::BAGGAGE_HEADER, baggage)
    ])
    
    // 测试extract不会崩溃
    let _extract_context = baggage_propagator.extract(initial_context, test_carrier)
    assert_eq(true, true)  // 如果能执行到这里说明没有崩溃
    
    index2 = index2 + 1
  }
  
  // 测试CompositePropagator的边界条件
  let empty_propagators = []
  let empty_composite = propagation::CompositePropagator::new(empty_propagators)
  
  let single_propagator = [trace_propagator]
  let single_composite = propagation::CompositePropagator::new(single_propagator)
  
  let many_propagators = [
    trace_propagator,
    baggage_propagator,
    trace_propagator,  // 重复
    baggage_propagator  // 重复
  ]
  let many_composite = propagation::CompositePropagator::new(many_propagators)
  
  // 测试所有composite propagator都能正常工作
  empty_composite.inject(initial_context, empty_carrier)
  single_composite.inject(initial_context, empty_carrier)
  many_composite.inject(initial_context, empty_carrier)
  
  let _empty_extract = empty_composite.extract(initial_context, empty_carrier)
  let _single_extract = single_composite.extract(initial_context, empty_carrier)
  let _many_extract = many_composite.extract(initial_context, empty_carrier)
  
  assert_eq(true, true)  // 如果能执行到这里说明没有崩溃
}

test "context_module_error_handling" {
  // 测试context模块的错误处理和边界条件
  
  // 测试边界ContextKey
  let empty_key = context::create_key("")
  let long_key = context::create_key("a".repeat(1000))
  let unicode_key = context::create_key("键.🌟.测试")
  let special_key = context::create_key("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  
  // 测试边界Context值
  let empty_context = context::Context::empty()
  
  let empty_value_context = empty_context.with_value(empty_key, "")
  let long_value_context = empty_context.with_value(long_key, "a".repeat(1000))
  let unicode_value_context = empty_context.with_value(unicode_key, "值.🌟.测试")
  let special_value_context = empty_context.with_value(special_key, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
  
  // 验证边界值都能正确设置和获取
  match empty_value_context.get(empty_key) {
    Some(value) => assert_eq(value.length(), 0)
    None => assert_eq(false, true)
  }
  
  match long_value_context.get(long_key) {
    Some(value) => assert_eq(value.length(), 1000)
    None => assert_eq(false, true)
  }
  
  match unicode_value_context.get(unicode_key) {
    Some(value) => assert_eq(value.contains("测试"), true)
    None => assert_eq(false, true)
  }
  
  match special_value_context.get(special_key) {
    Some(value) => assert_eq(value.contains("!@#$%^&*()"), true)
    None => assert_eq(false, true)
  }
  
  // 测试大规模Context操作
  let mut large_context = empty_context
  let mut index = 0
  while index < 100 {
    let key = context::create_key("large.key." + index.to_string())
    let value = "large.value." + index.to_string() + "a".repeat(100)
    large_context = large_context.with_value(key, value)
    index = index + 1
  }
  
  // 验证大规模Context
  assert_eq(large_context.values.length(), 100)
  
  // 随机访问一些值
  match large_context.get(context::create_key("large.key.0")) {
    Some(value) => assert_eq(value.contains("large.value.0"), true)
    None => assert_eq(false, true)
  }
  
  match large_context.get(context::create_key("large.key.99")) {
    Some(value) => assert_eq(value.contains("large.value.99"), true)
    None => assert_eq(false, true)
  }
  
  // 测试Baggage的边界条件
  let empty_baggage = context::Baggage::empty()
  
  let empty_key_baggage = empty_baggage.with_entry("", "empty key value")
  let empty_value_baggage = empty_baggage.with_entry("empty value", "")
  let long_key_baggage = empty_baggage.with_entry("a".repeat(1000), "long key value")
  let long_value_baggage = empty_baggage.with_entry("long value", "a".repeat(1000))
  let unicode_baggage = empty_baggage.with_entry("unicode.键", "unicode.值.🌟")
  let special_baggage = empty_baggage.with_entry("!@#$%^&*()", "!@#$%^&*()")
  
  // 验证边界Baggage条目
  match empty_key_baggage.get("") {
    Some(value) => assert_eq(value, "empty key value")
    None => assert_eq(false, true)
  }
  
  match empty_value_baggage.get("empty value") {
    Some(value) => assert_eq(value.length(), 0)
    None => assert_eq(false, true)
  }
  
  match long_key_baggage.get("a".repeat(1000)) {
    Some(value) => assert_eq(value, "long key value")
    None => assert_eq(false, true)
  }
  
  match long_value_baggage.get("long value") {
    Some(value) => assert_eq(value.length(), 1000)
    None => assert_eq(false, true)
  }
  
  match unicode_baggage.get("unicode.键") {
    Some(value) => assert_eq(value.contains("测试"), true)
    None => assert_eq(false, true)
  }
  
  match special_baggage.get("!@#$%^&*()") {
    Some(value) => assert_eq(value, "!@#$%^&*()")
    None => assert_eq(false, true)
  }
  
  // 测试大规模Baggage操作
  let mut large_baggage = empty_baggage
  let mut index2 = 0
  while index2 < 50 {
    let key = "large.baggage.key." + index2.to_string()
    let value = "large.baggage.value." + index2.to_string() + "b".repeat(50)
    large_baggage = large_baggage.with_entry(key, value)
    index2 = index2 + 1
  }
  
  // 验证大规模Baggage
  assert_eq(large_baggage.entries.length(), 50)
  
  // 随机访问一些条目
  match large_baggage.get("large.baggage.key.0") {
    Some(value) => assert_eq(value.contains("large.baggage.value.0"), true)
    None => assert_eq(false, true)
  }
  
  match large_baggage.get("large.baggage.key.49") {
    Some(value) => assert_eq(value.contains("large.baggage.value.49"), true)
    None => assert_eq(false, true)
  }
}