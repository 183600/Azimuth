// 错误恢复容错测试
test "attribute_value_error_recovery" {
  // 测试AttributeValue的错误恢复和容错
  
  // 测试极值和异常值
  let max_int = AttributeValue::int(9223372036854775807L)
  let min_int = AttributeValue::int(-9223372036854775808L)
  let inf_float = AttributeValue::float(1.0/0.0)
  let neg_inf_float = AttributeValue::float(-1.0/0.0)
  let nan_float = AttributeValue::float(0.0/0.0)
  
  // 验证极值不会导致崩溃
  match max_int {
    IntValue(i) => {
      assert_eq(i, 9223372036854775807L)
      // 测试算术运算的错误恢复
      let overflow_add = i + 1L  // 在实际系统中可能溢出，但这里应该优雅处理
      assert_eq(overflow_add, -9223372036854775808L)  // 溢出回绕
    }
    _ => @test.fail("Expected max int")
  }
  
  match min_int {
    IntValue(i) => {
      assert_eq(i, -9223372036854775808L)
      // 测试算术运算的错误恢复
      let underflow_add = i - 1L  // 在实际系统中可能下溢，但这里应该优雅处理
      assert_eq(underflow_add, 9223372036854775807L)  // 下溢回绕
    }
    _ => @test.fail("Expected min int")
  }
  
  // 验证特殊浮点值
  match inf_float {
    FloatValue(f) => {
      assert_eq(f > 0.0, true)
      // 测试无穷大运算的错误恢复
      let inf_mult = f * 2.0
      assert_eq(inf_mult > 0.0, true)  // 无穷大乘以正数仍是无穷大
      
      let inf_div = f / f
      assert_eq(inf_div != inf_div, true)  // 无穷大除以无穷大是NaN
    }
    _ => @test.fail("Expected infinity float")
  }
  
  match nan_float {
    FloatValue(f) => {
      assert_eq(f != f, true)  // NaN不等于自身
      // 测试NaN运算的错误恢复
      let nan_add = f + 1.0
      assert_eq(nan_add != nan_add, true)  // NaN加任何数仍是NaN
      
      let nan_mult = f * 0.0
      assert_eq(nan_mult != nan_mult, true)  // NaN乘以任何数仍是NaN
    }
    _ => @test.fail("Expected NaN float")
  }
  
  // 测试空数组和大型数组
  let empty_array = AttributeValue::array_string([])
  let large_array = AttributeValue::array_string(Array::make(10000, "item"))
  
  match empty_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 0)
      // 测试空数组访问的错误恢复
      if arr.length() > 0 {
        let first_item = arr[0]  // 这里不应该执行
        assert_eq(false, true)  // 不应该到达这里
      }
    }
    _ => @test.fail("Expected empty array")
  }
  
  match large_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 10000)
      // 测试大型数组访问的错误恢复
      if arr.length() > 9999 {
        let last_item = arr[9999]
        assert_eq(last_item, "item")
      }
      
      // 测试边界访问
      if arr.length() > 0 {
        let first_item = arr[0]
        assert_eq(first_item, "item")
      }
    }
    _ => @test.fail("Expected large array")
  }
}

test "context_error_recovery" {
  // 测试Context的错误恢复和容错
  
  let ctx = Context::empty()
  
  // 测试空Context操作
  let empty_key = create_key("")
  let normal_key = create_key("normal.key")
  
  // 向空Context添加值
  let ctx_with_empty_key = ctx.with_value(empty_key, "empty.key.value")
  let ctx_with_normal_key = ctx.with_value(normal_key, "normal.key.value")
  
  // 验证空键值对
  match ctx_with_empty_key.get(empty_key) {
    Some(value) => assert_eq(value, "empty.key.value")
    None => @test.fail("Expected value for empty key")
  }
  
  match ctx_with_normal_key.get(normal_key) {
    Some(value) => assert_eq(value, "normal.key.value")
    None => @test.fail("Expected value for normal key")
  }
  
  // 测试不存在的键访问
  let nonexistent_key = create_key("nonexistent.key")
  match ctx_with_normal_key.get(nonexistent_key) {
    Some(_) => @test.fail("Expected None for nonexistent key")
    None => assert_eq(true, true)  // 正确返回None
  }
  
  // 测试特殊字符键
  let special_keys = [
    create_key("special!@#$%^&*()"),
    create_key("测试中文"),
    create_key("key with spaces"),
    create_key("key.with.dots"),
    create_key("key_with_underscores"),
    create_key("key-with-hyphens"),
    create_key("UPPERCASE.KEY"),
    create_key("123numeric.start"),
    create_key("very.long.key.name.with.many.parts.that.might.cause.issues.in.some.systems.but.should.work.fine.in.our.implementation")
  ]
  
  let mut i = 0
  while i < special_keys.length() {
    let special_value = "value.for." + i.to_string()
    let ctx_with_special = ctx.with_value(special_keys[i], special_value)
    
    match ctx_with_special.get(special_keys[i]) {
      Some(value) => assert_eq(value, special_value)
      None => @test.fail("Expected value for special key: " + i.to_string())
    }
    
    i = i + 1
  }
  
  // 测试链式操作的错误恢复
  let mut chain_ctx = ctx
  let mut j = 0
  while j < 1000 {
    chain_ctx = chain_ctx.with_value(create_key("chain.key." + j.to_string()), "chain.value." + j.to_string())
    j = j + 1
  }
  
  // 验证链式操作的结果
  match chain_ctx.get(create_key("chain.key.0")) {
    Some(value) => assert_eq(value, "chain.value.0")
    None => @test.fail("Expected chain value 0")
  }
  
  match chain_ctx.get(create_key("chain.key.999")) {
    Some(value) => assert_eq(value, "chain.value.999")
    None => @test.fail("Expected chain value 999")
  }
  
  match chain_ctx.get(create_key("chain.key.500")) {
    Some(value) => assert_eq(value, "chain.value.500")
    None => @test.fail("Expected chain value 500")
  }
}

test "baggage_error_recovery" {
  // 测试Baggage的错误恢复和容错
  
  let baggage = Baggage::empty()
  
  // 测试空Baggage操作
  let empty_baggage = baggage.with_entry("", "")
  let normal_baggage = baggage.with_entry("normal.key", "normal.value")
  
  // 验证空键值对
  match empty_baggage.get("") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty value for empty key")
  }
  
  match normal_baggage.get("normal.key") {
    Some(value) => assert_eq(value, "normal.value")
    None => @test.fail("Expected normal value")
  }
  
  // 测试不存在的键访问
  match normal_baggage.get("nonexistent.key") {
    Some(_) => @test.fail("Expected None for nonexistent key")
    None => assert_eq(true, true)  // 正确返回None
  }
  
  // 测试特殊字符键和值
  let special_entries = [
    ("special!@#$%^&*()", "special!@#$%^&*()"),
    ("测试中文", "测试中文值"),
    ("key with spaces", "value with spaces"),
    ("", "empty.key.value"),
    ("empty.value", ""),
    ("unicode.key", "Unicode值🚀"),
    ("very.long.key.name.that.exceeds.normal.limits", "very.long.value.name.that.exceeds.normal.limits")
  ]
  
  let mut special_baggage = baggage
  let mut i = 0
  while i < special_entries.length() {
    let (key, value) = special_entries[i]
    special_baggage = special_baggage.with_entry(key, value)
    
    match special_baggage.get(key) {
      Some(retrieved_value) => assert_eq(retrieved_value, value)
      None => @test.fail("Expected value for special key: " + key)
    }
    
    i = i + 1
  }
  
  // 测试重复键的处理
  let duplicate_baggage = baggage
    .with_entry("duplicate.key", "first.value")
    .with_entry("duplicate.key", "second.value")
    .with_entry("duplicate.key", "third.value")
  
  // 验证重复键的处理（应该保留最后一个值）
  match duplicate_baggage.get("duplicate.key") {
    Some(value) => assert_eq(value, "third.value")
    None => @test.fail("Expected third value for duplicate key")
  }
  
  // 测试大量条目
  let mut large_baggage = baggage
  let mut k = 0
  while k < 1000 {
    large_baggage = large_baggage.with_entry("large.key." + k.to_string(), "large.value." + k.to_string())
    k = k + 1
  }
  
  // 验证大量条目的访问
  match large_baggage.get("large.key.0") {
    Some(value) => assert_eq(value, "large.value.0")
    None => @test.fail("Expected large value 0")
  }
  
  match large_baggage.get("large.key.999") {
    Some(value) => assert_eq(value, "large.value.999")
    None => @test.fail("Expected large value 999")
  }
}

test "span_error_recovery" {
  // 测试Span的错误恢复和容错
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 测试边界时间值
  let zero_time = 0L
  let max_time = 9223372036854775807L
  let negative_time = -1L
  
  let (_, zero_span) = tracer.start_span(ctx, "zero-time-span", Internal, None, Some(zero_time))
  let (_, max_span) = tracer.start_span(ctx, "max-time-span", Internal, None, Some(max_time))
  let (_, negative_span) = tracer.start_span(ctx, "negative-time-span", Internal, None, Some(negative_time))
  
  // 验证边界时间值
  assert_eq(zero_span.start_time_unix_nanos, zero_time)
  assert_eq(max_span.start_time_unix_nanos, max_time)
  assert_eq(negative_span.start_time_unix_nanos, negative_time)
  
  // 测试特殊字符的Span名称
  let special_span_names = [
    "",
    " ",
    "special!@#$%^&*()",
    "测试中文🚀",
    "span with spaces",
    "span.with.dots",
    "span_with_underscores",
    "span-with-hyphens",
    "UPPERCASE_SPAN",
    "123numeric.start",
    "a".repeat(1000)  // 非常长的Span名称
  ]
  
  let mut i = 0
  while i < special_span_names.length() {
    let (_, special_span) = tracer.start_span(
      ctx,
      special_span_names[i],
      Internal,
      Some([("span.type", AttributeValue::string("special"))]),
      Some(1234567890L + i.to_int64())
    )
    
    assert_eq(special_span.name, special_span_names[i])
    assert_eq(special_span.kind, Internal)
    assert_eq(special_span.attributes.length(), 1)
    
    i = i + 1
  }
  
  // 测试大量属性
  let mut large_attributes = []
  let mut j = 0
  while j < 1000 {
    large_attributes.push(("large.attr." + j.to_string(), AttributeValue::string("large.value." + j.to_string())))
    j = j + 1
  }
  
  let (_, large_attr_span) = tracer.start_span(
    ctx,
    "large-attributes-span",
    Server,
    Some(large_attributes),
    Some(1234567890L)
  )
  
  // 验证大量属性
  assert_eq(large_attr_span.attributes.length(), 1000)
  
  // 测试特殊属性值
  let special_attributes = [
    ("empty.string", AttributeValue::string("")),
    ("zero.int", AttributeValue::int(0L)),
    ("negative.int", AttributeValue::int(-1L)),
    ("zero.float", AttributeValue::float(0.0)),
    ("negative.float", AttributeValue::float(-1.0)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    ("inf.float", AttributeValue::float(1.0/0.0)),
    ("neg.inf.float", AttributeValue::float(-1.0/0.0)),
    ("nan.float", AttributeValue::float(0.0/0.0)),
    ("unicode.string", AttributeValue::string("测试中文🚀")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("very.long.string", AttributeValue::string("a".repeat(1000)))
  ]
  
  let (_, special_attr_span) = tracer.start_span(
    ctx,
    "special-attributes-span",
    Client,
    Some(special_attributes),
    Some(1234567891L)
  )
  
  // 验证特殊属性
  assert_eq(special_attr_span.attributes.length(), 13)
}

test "log_record_error_recovery" {
  // 测试LogRecord的错误恢复和容错
  
  // 测试边界时间值
  let zero_time_log = LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: Some(0L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Zero time log"),
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
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Max time log"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证边界时间值
  assert_eq(zero_time_log.timestamp_unix_nanos, 0L)
  assert_eq(max_time_log.timestamp_unix_nanos, 9223372036854775807L)
  
  // 测试特殊严重程度
  let severity_levels = [Trace, Debug, Info, Warn, Error, Fatal]
  let mut i = 0
  while i < severity_levels.length() {
    let severity_log = LogRecord::{
      timestamp_unix_nanos: 1234567890L + i.to_int64(),
      observed_timestamp_unix_nanos: Some(1234567891L + i.to_int64()),
      severity_number: severity_levels[i],
      severity_text: Some(match severity_levels[i] {
        Trace => "TRACE"
        Debug => "DEBUG"
        Info => "INFO"
        Warn => "WARN"
        Error => "ERROR"
        Fatal => "FATAL"
      }),
      body: Some("Severity test log"),
      attributes: [("severity.level", AttributeValue::int(i.to_int64()))],
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: None,
      instrumentation_scope: None
    }
    
    assert_eq(severity_log.severity_number, severity_levels[i])
    i = i + 1
  }
  
  // 测试特殊body内容
  let special_bodies = [
    "",
    " ",
    "special!@#$%^&*()",
    "测试中文🚀",
    "body with\nnewlines\rand\ttabs",
    "a".repeat(10000),  // 非常长的body
    "body with \"quotes\" and 'apostrophes'",
    "body with \\backslashes and /forward/slashes",
    "body with <html> &entities; </html>",
    "body with {json: \"format\"} and [arrays]"
  ]
  
  let mut j = 0
  while j < special_bodies.length() {
    let special_body_log = LogRecord::{
      timestamp_unix_nanos: 1234567890L + j.to_int64(),
      observed_timestamp_unix_nanos: None,
      severity_number: Info,
      severity_text: None,
      body: Some(special_bodies[j]),
      attributes: [("body.index", AttributeValue::int(j.to_int64()))],
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: None,
      instrumentation_scope: None
    }
    
    match special_body_log.body {
      Some(body) => assert_eq(body, special_bodies[j])
      None => @test.fail("Expected body: " + j.to_string())
    }
    
    j = j + 1
  }
  
  // 测试特殊属性
  let special_log_attributes = [
    ("empty.string", AttributeValue::string("")),
    ("zero.int", AttributeValue::int(0L)),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("zero.float", AttributeValue::float(0.0)),
    ("inf.float", AttributeValue::float(1.0/0.0)),
    ("neg.inf.float", AttributeValue::float(-1.0/0.0)),
    ("nan.float", AttributeValue::float(0.0/0.0)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    ("large.array", AttributeValue::array_string(Array::make(1000, "item")))
  ]
  
  let special_attr_log = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: Warn,
    severity_text: Some("WARN"),
    body: Some("Special attributes log"),
    attributes: special_log_attributes,
    trace_id: Some(Array::make(16, 0xFF_byte)),
    span_id: Some(Array::make(8, 0xEE_byte)),
    trace_flags: Some(0x01_byte),
    resource: Some(Resource::default("test-service")),
    instrumentation_scope: None
  }
  
  // 验证特殊属性
  assert_eq(special_attr_log.attributes.length(), 11)
}

test "metrics_error_recovery" {
  // 测试Metrics的错误恢复和容错
  
  let meter = NoopMeter::{}
  
  // 测试边界值度量
  let counter = meter.create_counter("error.recovery.counter", "operations", "Error recovery counter")
  let histogram = meter.create_histogram("error.recovery.histogram", "seconds", "Error recovery histogram")
  let up_down_counter = meter.create_up_down_counter("error.recovery.up_down_counter", "operations", "Error recovery up-down counter")
  let gauge = meter.create_gauge("error.recovery.gauge", "bytes", "Error recovery gauge")
  
  // 测试极值操作
  let extreme_values = [
    0L,  // 零值
    1L,  // 最小正值
    -1L,  // 负值
    9223372036854775807L,  // 最大值
    -9223372036854775808L,  // 最小值
    1000000L,  // 大值
    -1000000L  // 大负值
  ]
  
  let mut i = 0
  while i < extreme_values.length() {
    // Counter操作
    counter.add(extreme_values[i], Some([
      ("test.type", AttributeValue::string("extreme")),
      ("test.value", AttributeValue::int(extreme_values[i]))
    ]))
    
    // UpDownCounter操作
    up_down_counter.add(extreme_values[i], Some([
      ("test.type", AttributeValue::string("extreme")),
      ("test.value", AttributeValue::int(extreme_values[i]))
    ]))
    
    i = i + 1
  }
  
  // 测试极值浮点数操作
  let extreme_float_values = [
    0.0,  // 零值
    1.0e-10,  // 极小正值
    -1.0e-10,  // 极小负值
    1.7976931348623157e+308,  // 最大值
    -1.7976931348623157e+308,  // 最小值
    1.0/0.0,  // 正无穷大
    -1.0/0.0,  // 负无穷大
    0.0/0.0  // NaN
  ]
  
  let mut j = 0
  while j < extreme_float_values.length() {
    // Histogram操作
    histogram.record(extreme_float_values[j], Some([
      ("test.type", AttributeValue::string("extreme.float")),
      ("test.value", AttributeValue::float(extreme_float_values[j]))
    ]))
    
    // Gauge操作
    gauge.record(extreme_float_values[j], Some([
      ("test.type", AttributeValue::string("extreme.float")),
      ("test.value", AttributeValue::float(extreme_float_values[j]))
    ]))
    
    j = j + 1
  }
  
  // 测试特殊属性
  let special_attributes = [
    ("empty.string", AttributeValue::string("")),
    ("zero.int", AttributeValue::int(0L)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    ("unicode.string", AttributeValue::string("测试中文🚀")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("very.long.string", AttributeValue::string("a".repeat(1000)))
  ]
  
  // 使用特殊属性进行度量操作
  counter.add(1L, Some(special_attributes))
  histogram.record(1.0, Some(special_attributes))
  up_down_counter.add(1L, Some(special_attributes))
  gauge.record(1.0, Some(special_attributes))
  
  // 测试空属性
  counter.add(1L, None)
  histogram.record(1.0, None)
  up_down_counter.add(1L, None)
  gauge.record(1.0, None)
  
  counter.add(1L, Some([]))
  histogram.record(1.0, Some([]))
  up_down_counter.add(1L, Some([]))
  gauge.record(1.0, Some([]))
  
  // 测试大量属性
  let mut large_attributes = []
  let mut k = 0
  while k < 1000 {
    large_attributes.push(("large.attr." + k.to_string(), AttributeValue::int(k.to_int64())))
    k = k + 1
  }
  
  counter.add(1L, Some(large_attributes))
  histogram.record(1.0, Some(large_attributes))
  up_down_counter.add(1L, Some(large_attributes))
  gauge.record(1.0, Some(large_attributes))
}

test "propagation_error_recovery" {
  // 测试传播的错误恢复和容错
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  
  // 测试无效的traceparent格式
  let invalid_traceparents = [
    "",
    "invalid",
    "00-",
    "00-0af7651916cd43dd8448eb211c80319c",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-xyz",
    "gg-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  ]
  
  let mut i = 0
  while i < invalid_traceparents.length() {
    let carrier = MapCarrier::from_map([
      ("traceparent", invalid_traceparents[i])
    ])
    
    // 即使traceparent无效，提取也不应该崩溃
    let extracted_ctx = composite.extract(ctx, carrier)
    assert_eq(true, true)
    
    i = i + 1
  }
  
  // 测试无效的baggage格式
  let invalid_baggages = [
    "",
    "invalid",
    "key=",
    "=value",
    "key=value,invalid",
    "key=value,,",
    ",key=value",
    "key=value,"
  ]
  
  let mut j = 0
  while j < invalid_baggages.length() {
    let carrier = MapCarrier::from_map([
      ("baggage", invalid_baggages[j])
    ])
    
    // 即使baggage无效，提取也不应该崩溃
    let extracted_ctx = composite.extract(ctx, carrier)
    assert_eq(true, true)
    
    j = j + 1
  }
  
  // 测试特殊字符的header
  let special_carriers = [
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("baggage", "测试中文=value"),
      ("unicode.header", "测试中文值")
    ]),
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("baggage", "special!@#$%^&*()=value"),
      ("special.header", "special!@#$%^&*()")
    ]),
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("baggage", ""),
      ("empty.header", "")
    ])
  ]
  
  let mut k = 0
  while k < special_carriers.length() {
    // 即使header包含特殊字符，提取也不应该崩溃
    let extracted_ctx = composite.extract(ctx, special_carriers[k])
    assert_eq(true, true)
    
    k = k + 1
  }
  
  // 测试极长的header值
  let very_long_traceparent = "00-" + "a".repeat(32) + "-" + "b".repeat(16) + "-01"
  let very_long_baggage = "key=" + "value".repeat(1000)
  
  let long_carrier = MapCarrier::from_map([
    ("traceparent", very_long_traceparent),
    ("baggage", very_long_baggage)
  ])
  
  // 即使header值极长，提取也不应该崩溃
  let extracted_long_ctx = composite.extract(ctx, long_carrier)
  assert_eq(true, true)
}

test "comprehensive_error_recovery" {
  // 测试综合错误恢复和容错
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  let meter = NoopMeter::{}
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("error.recovery.logger")
  
  // 创建包含各种边界情况的复杂遥测场景
  let complex_ctx = ctx
    .with_value(create_key(""), "")  // 空键值对
    .with_value(create_key("special!@#$%^&*()"), "special!@#$%^&*()")  // 特殊字符
    .with_value(create_key("测试中文"), "测试中文值")  // Unicode
    .with_value(create_key("very.long.key.name.that.exceeds.normal.limits"), "very.long.value.name.that.exceeds.normal.limits")
  
  // 创建包含边界值的Span
  let (_, complex_span) = tracer.start_span(
    complex_ctx,
    "complex!@#$%^&*()span测试中文",  // 特殊字符Span名称
    Server,
    Some([
      ("empty.string", AttributeValue::string("")),
      ("zero.int", AttributeValue::int(0L)),
      ("max.int", AttributeValue::int(9223372036854775807L)),
      ("inf.float", AttributeValue::float(1.0/0.0)),
      ("nan.float", AttributeValue::float(0.0/0.0)),
      ("false.bool", AttributeValue::bool(false)),
      ("empty.array", AttributeValue::array_string([])),
      ("unicode.string", AttributeValue::string("测试中文🚀"))
    ]),
    Some(9223372036854775807L)  // 最大时间值
  )
  
  // 创建包含边界值的LogRecord
  let complex_log_record = LogRecord::{
    timestamp_unix_nanos: 0L,  // 零时间
    observed_timestamp_unix_nanos: Some(9223372036854775807L),  // 最大时间
    severity_number: Fatal,
    severity_text: Some("FATAL"),
    body: Some("Complex!@#$%^&*()log测试中文"),  // 特殊字符body
    attributes: [
      ("empty.string", AttributeValue::string("")),
      ("zero.int", AttributeValue::int(0L)),
      ("max.int", AttributeValue::int(9223372036854775807L)),
      ("inf.float", AttributeValue::float(1.0/0.0)),
      ("nan.float", AttributeValue::float(0.0/0.0)),
      ("false.bool", AttributeValue::bool(false)),
      ("empty.array", AttributeValue::array_string([])),
      ("unicode.string", AttributeValue::string("测试中文🚀"))
    ],
    trace_id: Some(Array::make(16, 0xFF_byte)),  // 最大trace_id
    span_id: Some(Array::make(8, 0xFF_byte)),  // 最大span_id
    trace_flags: Some(0xFF_byte),  // 最大trace_flags
    resource: Some(Resource::default("complex!@#$%^&*()service测试中文")),  // 特殊字符服务名
    instrumentation_scope: None
  }
  
  // 创建包含边界值的度量
  let counter = meter.create_counter("complex!@#$%^&*()counter测试中文", "operations", "Complex counter")
  let histogram = meter.create_histogram("complex!@#$%^&*()histogram测试中文", "seconds", "Complex histogram")
  
  // 执行边界值度量操作
  counter.add(9223372036854775807L, Some([
    ("empty.string", AttributeValue::string("")),
    ("zero.int", AttributeValue::int(0L)),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("inf.float", AttributeValue::float(1.0/0.0)),
    ("nan.float", AttributeValue::float(0.0/0.0)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    ("unicode.string", AttributeValue::string("测试中文🚀"))
  ]))
  
  histogram.record(1.7976931348623157e+308, Some([
    ("empty.string", AttributeValue::string("")),
    ("zero.int", AttributeValue::int(0L)),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("inf.float", AttributeValue::float(1.0/0.0)),
    ("nan.float", AttributeValue::float(0.0/0.0)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    ("unicode.string", AttributeValue::string("测试中文🚀"))
  ]))
  
  // 发送复杂日志记录
  logger.emit(complex_log_record)
  
  // 验证所有操作都成功完成
  assert_eq(complex_span.name, "complex!@#$%^&*()span测试中文")
  assert_eq(complex_span.start_time_unix_nanos, 9223372036854775807L)
  assert_eq(complex_span.attributes.length(), 8)
  
  assert_eq(complex_log_record.timestamp_unix_nanos, 0L)
  assert_eq(complex_log_record.severity_number, Fatal)
  match complex_log_record.body {
    Some(body) => assert_eq(body, "Complex!@#$%^&*()log测试中文")
    None => @test.fail("Expected complex body")
  }
  assert_eq(complex_log_record.attributes.length(), 8)
  
  // 验证复杂Context仍然可以正常工作
  match complex_ctx.get(create_key("")) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty key value")
  }
  
  match complex_ctx.get(create_key("测试中文")) {
    Some(value) => assert_eq(value, "测试中文值")
    None => @test.fail("Expected Chinese key value")
  }
  
  // 验证错误恢复成功
  assert_eq(true, true)  // 如果没有崩溃，说明错误恢复成功
}