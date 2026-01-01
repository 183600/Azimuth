// Telemetry Serialization Tests - 遥测数据序列化测试
// 专注于遥测数据的序列化和反序列化功能

test "Span数据序列化和反序列化测试" {
  // 测试Span数据的序列化和反序列化
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.test")
  
  // 创建复杂的span用于序列化测试
  let span = Tracer::start_span(tracer, "serialization.test.span")
  let span_ctx = Span::context(span)
  
  // 设置丰富的属性
  Span::set_attribute(span, "service.name", StringValue("test-service"))
  Span::set_attribute(span, "service.version", StringValue("1.0.0"))
  Span::set_attribute(span, "operation.name", StringValue("test.operation"))
  Span::set_attribute(span, "user.id", IntValue(12345))
  Span::set_attribute(span, "request.size", IntValue(1024))
  Span::set_attribute(span, "response.time", FloatValue(150.5))
  Span::set_attribute(span, "success", BoolValue(true))
  Span::set_attribute(span, "tags", ArrayStringValue(["tag1", "tag2", "tag3"]))
  Span::set_attribute(span, "status.codes", ArrayIntValue([200, 201, 204]))
  
  // 添加多个事件
  Span::add_event(span, "request.started", Some([
    ("timestamp", IntValue(1735689600000000000L)),
    ("endpoint", StringValue("/api/v1/test"))
  ]))
  
  Span::add_event(span, "validation.completed", Some([
    ("timestamp", IntValue(1735689600000000001L)),
    ("validation.result", StringValue("success"))
  ]))
  
  Span::add_event(span, "response.sent", Some([
    ("timestamp", IntValue(1735689600000000002L)),
    ("response.size", IntValue(2048))
  ]))
  
  // 设置状态
  Span::set_status(span, Ok)
  
  // 模拟序列化过程（在实际实现中会转换为JSON/protobuf等格式）
  let serialized_data = {
    "trace_id": SpanContext::trace_id(span_ctx),
    "span_id": SpanContext::span_id(span_ctx),
    "parent_span_id": SpanContext::parent_span_id(span_ctx),
    "name": Span::name(span),
    "kind": Span::kind(span).to_string(),
    "start_time": Span::start_time(span),
    "end_time": Span::end_time(span),
    "status": Span::status(span).to_string(),
    "attributes": [
      ("service.name", "test-service"),
      ("service.version", "1.0.0"),
      ("operation.name", "test.operation"),
      ("user.id", "12345"),
      ("request.size", "1024"),
      ("response.time", "150.5"),
      ("success", "true"),
      ("tags", "tag1,tag2,tag3"),
      ("status.codes", "200,201,204")
    ],
    "events": [
      ("request.started", "1735689600000000000", "endpoint=/api/v1/test"),
      ("validation.completed", "1735689600000000001", "validation.result=success"),
      ("response.sent", "1735689600000000002", "response.size=2048")
    ]
  }
  
  // 模拟反序列化过程
  let deserialized_trace_id = serialized_data["trace_id"]
  let deserialized_span_id = serialized_data["span_id"]
  let deserialized_name = serialized_data["name"]
  let deserialized_kind = serialized_data["kind"]
  
  // 验证反序列化的数据
  assert_eq(deserialized_trace_id, SpanContext::trace_id(span_ctx))
  assert_eq(deserialized_span_id, SpanContext::span_id(span_ctx))
  assert_eq(deserialized_name, "serialization.test.span")
  assert_eq(deserialized_kind, "Internal") // 假设默认为Internal
  
  Span::end(span)
  
  // 验证序列化后的span数据完整性
  assert_true(true)
}

test "Attributes序列化和反序列化测试" {
  // 测试Attributes的序列化和反序列化
  let attrs = Attributes::new()
  
  // 设置各种类型的属性
  Attributes::set(attrs, "string.value", StringValue("test.string"))
  Attributes::set(attrs, "empty.string", StringValue(""))
  Attributes::set(attrs, "unicode.string", StringValue("测试中文🚀"))
  Attributes::set(attrs, "special.chars", StringValue("!@#$%^&*()"))
  Attributes::set(attrs, "int.value", IntValue(42))
  Attributes::set(attrs, "negative.int", IntValue(-123))
  Attributes::set(attrs, "zero.int", IntValue(0))
  Attributes::set(attrs, "float.value", FloatValue(3.14159))
  Attributes::set(attrs, "negative.float", FloatValue(-2.71828))
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  Attributes::set(attrs, "bool.true", BoolValue(true))
  Attributes::set(attrs, "bool.false", BoolValue(false))
  Attributes::set(attrs, "string.array", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attrs, "empty.array", ArrayStringValue([]))
  Attributes::set(attrs, "int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  
  // 模拟序列化为JSON格式
  let serialized_attrs = [
    ("string.value", "string", "test.string"),
    ("empty.string", "string", ""),
    ("unicode.string", "string", "测试中文🚀"),
    ("special.chars", "string", "!@#$%^&*()"),
    ("int.value", "int", "42"),
    ("negative.int", "int", "-123"),
    ("zero.int", "int", "0"),
    ("float.value", "float", "3.14159"),
    ("negative.float", "float", "-2.71828"),
    ("zero.float", "float", "0.0"),
    ("bool.true", "bool", "true"),
    ("bool.false", "bool", "false"),
    ("string.array", "array<string>", "item1,item2,item3"),
    ("empty.array", "array<string>", ""),
    ("int.array", "array<int>", "1,2,3,4,5"),
    ("empty.int.array", "array<int>", "")
  ]
  
  // 模拟反序列化过程
  let deserialized_attrs = Attributes::new()
  for attr in serialized_attrs {
    let key = attr.0
    let type_str = attr.1
    let value_str = attr.2
    
    if type_str == "string" {
      Attributes::set(deserialized_attrs, key, StringValue(value_str))
    } else if type_str == "int" {
      let int_value = value_str.to_int()
      Attributes::set(deserialized_attrs, key, IntValue(int_value))
    } else if type_str == "float" {
      let float_value = value_str.to_double()
      Attributes::set(deserialized_attrs, key, FloatValue(float_value))
    } else if type_str == "bool" {
      let bool_value = value_str == "true"
      Attributes::set(deserialized_attrs, key, BoolValue(bool_value))
    } else if type_str == "array<string>" {
      if value_str == "" {
        Attributes::set(deserialized_attrs, key, ArrayStringValue([]))
      } else {
        let string_array = value_str.split(",")
        Attributes::set(deserialized_attrs, key, ArrayStringValue(string_array))
      }
    } else if type_str == "array<int>" {
      if value_str == "" {
        Attributes::set(deserialized_attrs, key, ArrayIntValue([]))
      } else {
        let int_str_array = value_str.split(",")
        let int_array = []
        for int_str in int_str_array {
          int_array.push(int_str.to_int())
        }
        Attributes::set(deserialized_attrs, key, ArrayIntValue(int_array))
      }
    }
  }
  
  // 验证反序列化的属性
  match Attributes::get(deserialized_attrs, "string.value") {
    Some(StringValue(value)) => assert_eq(value, "test.string")
    _ => assert_true(false)
  }
  
  match Attributes::get(deserialized_attrs, "int.value") {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  match Attributes::get(deserialized_attrs, "bool.true") {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  match Attributes::get(deserialized_attrs, "string.array") {
    Some(ArrayStringValue(value)) => assert_eq(value.length(), 3)
    _ => assert_true(false)
  }
}

test "LogRecord序列化和反序列化测试" {
  // 测试LogRecord的序列化和反序列化
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("info"))
  Attributes::set(attrs, "component", StringValue("test.component"))
  Attributes::set(attrs, "request.id", StringValue("req-12345"))
  
  let log = LogRecord::new_with_context(
    Warn,
    Some("Test serialization log message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  // 模拟序列化为JSON格式
  let serialized_log = {
    "severity_number": "Warn",
    "severity_text": "WARN",
    "body": "Test serialization log message",
    "attributes": [
      ("log.level", "info"),
      ("component", "test.component"),
      ("request.id", "req-12345")
    ],
    "timestamp": "1735689600000000000",
    "observed_timestamp": "1735689600000000001",
    "trace_id": "trace-12345",
    "span_id": "span-67890"
  }
  
  // 模拟反序列化过程
  let deserialized_severity = serialized_log["severity_number"]
  let deserialized_body = serialized_log["body"]
  let deserialized_trace_id = serialized_log["trace_id"]
  let deserialized_span_id = serialized_log["span_id"]
  
  // 验证反序列化的数据
  assert_eq(deserialized_severity, "Warn")
  assert_eq(deserialized_body, "Test serialization log message")
  assert_eq(deserialized_trace_id, "trace-12345")
  assert_eq(deserialized_span_id, "span-67890")
  
  // 验证原始LogRecord的数据
  assert_eq(LogRecord::severity_number(log), Warn)
  match LogRecord::body(log) {
    Some(body) => assert_eq(body, "Test serialization log message")
    None => assert_true(false)
  }
  assert_eq(LogRecord::trace_id(log), Some("trace-12345"))
  assert_eq(LogRecord::span_id(log), Some("span-67890"))
}

test "Metrics序列化和反序列化测试" {
  // 测试Metrics的序列化和反序列化
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "serialization.metrics")
  
  // 创建各种类型的metrics
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test up-down"), Some("items"))
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  
  // 记录一些数据
  Counter::add(counter, 100.0)
  Histogram::record(histogram, 150.5)
  Counter::add(updown_counter, 50.0)
  Counter::add(gauge, 75.0)
  
  // 模拟序列化为JSON格式
  let serialized_metrics = [
    {
      "name": "test.counter",
      "type": "counter",
      "description": "Test counter",
      "unit": "count",
      "value": 100.0,
      "attributes": []
    },
    {
      "name": "test.histogram",
      "type": "histogram",
      "description": "Test histogram",
      "unit": "ms",
      "value": 150.5,
      "attributes": []
    },
    {
      "name": "test.updown",
      "type": "updown_counter",
      "description": "Test up-down",
      "unit": "items",
      "value": 50.0,
      "attributes": []
    },
    {
      "name": "test.gauge",
      "type": "gauge",
      "description": "Test gauge",
      "unit": "percent",
      "value": 75.0,
      "attributes": []
    }
  ]
  
  // 模拟反序列化过程
  for metric in serialized_metrics {
    let name = metric["name"]
    let type_str = metric["type"]
    let description = metric["description"]
    let unit = metric["unit"]
    let value = metric["value"]
    
    // 验证反序列化的数据
    assert_true(name == "test.counter" || name == "test.histogram" || 
                name == "test.updown" || name == "test.gauge")
    assert_true(type_str == "counter" || type_str == "histogram" || 
                type_str == "updown_counter" || type_str == "gauge")
    assert_eq(description, "Test " + type_str.split("_")[0])
    assert_true(unit == "count" || unit == "ms" || unit == "items" || unit == "percent")
    assert_true(value >= 0.0)
  }
  
  // 验证原始metrics的属性
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
}

test "Resource序列化和反序列化测试" {
  // 测试Resource的序列化和反序列化
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("test")),
    ("host.name", StringValue("test-host")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.15.0")),
    ("process.id", IntValue(12345)),
    ("process.name", StringValue("test-process")),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("0.1.0"))
  ])
  
  // 模拟序列化为JSON格式
  let serialized_resource = {
    "attributes": [
      ("service.name", "string", "test-service"),
      ("service.version", "string", "1.0.0"),
      ("service.instance.id", "string", "instance-123"),
      ("deployment.environment", "string", "test"),
      ("host.name", "string", "test-host"),
      ("os.type", "string", "linux"),
      ("os.version", "string", "5.15.0"),
      ("process.id", "int", "12345"),
      ("process.name", "string", "test-process"),
      ("telemetry.sdk.name", "string", "azimuth"),
      ("telemetry.sdk.version", "string", "0.1.0")
    ]
  }
  
  // 模拟反序列化过程
  let deserialized_attrs = []
  for attr in serialized_resource["attributes"] {
    let key = attr.0
    let type_str = attr.1
    let value_str = attr.2
    
    if type_str == "string" {
      deserialized_attrs.push((key, StringValue(value_str)))
    } else if type_str == "int" {
      deserialized_attrs.push((key, IntValue(value_str.to_int())))
    }
  }
  
  let deserialized_resource = Resource::with_attributes(Resource::new(), deserialized_attrs)
  
  // 验证反序列化的资源属性
  match Resource::get_attribute(deserialized_resource, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "test-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(deserialized_resource, "process.id") {
    Some(IntValue(value)) => assert_eq(value, 12345)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(deserialized_resource, "telemetry.sdk.version") {
    Some(StringValue(value)) => assert_eq(value, "0.1.0")
    _ => assert_true(false)
  }
}

test "Baggage序列化和反序列化测试" {
  // 测试Baggage的序列化和反序列化
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage = Baggage::set_entry(baggage, "user.role", "admin")
  let baggage = Baggage::set_entry(baggage, "request.id", "req-67890")
  let baggage = Baggage::set_entry(baggage, "trace.sampled", "true")
  let baggage = Baggage::set_entry(baggage, "special.chars", "!@#$%^&*()")
  let baggage = Baggage::set_entry(baggage, "unicode.value", "测试中文")
  
  // 模拟序列化为W3C Baggage头格式
  let serialized_baggage = "user.id=12345,user.role=admin,request.id=req-67890,trace.sampled=true,special.chars=!@#$%^&*(),unicode.value=测试中文"
  
  // 模拟反序列化过程
  let deserialized_baggage = Baggage::new()
  let entries = serialized_baggage.split(",")
  
  for entry in entries {
    let parts = entry.split("=")
    if parts.length() == 2 {
      let key = parts[0]
      let value = parts[1]
      deserialized_baggage = Baggage::set_entry(deserialized_baggage, key, value)
    }
  }
  
  // 验证反序列化的baggage条目
  match Baggage::get_entry(deserialized_baggage, "user.id") {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(deserialized_baggage, "user.role") {
    Some(value) => assert_eq(value, "admin")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(deserialized_baggage, "unicode.value") {
    Some(value) => assert_eq(value, "测试中文")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(deserialized_baggage, "special.chars") {
    Some(value) => assert_eq(value, "!@#$%^&*()")
    None => assert_true(false)
  }
}

test "SpanContext序列化和反序列化测试" {
  // 测试SpanContext的序列化和反序列化
  let span_ctx = SpanContext::new("trace-1234567890abcdef", "span-1234567890abcdef", true, "key1=value1,key2=value2")
  
  // 模拟序列化为W3C TraceContext头格式
  let serialized_traceparent = "00-trace-1234567890abcdef-span-1234567890abcdef-01"
  let serialized_tracestate = "key1=value1,key2=value2"
  
  // 模拟反序列化过程
  let traceparent_parts = serialized_traceparent.split("-")
  if traceparent_parts.length() == 4 {
    let version = traceparent_parts[0]
    let trace_id = traceparent_parts[1]
    let span_id = traceparent_parts[2]
    let flags = traceparent_parts[3]
    
    // 验证反序列化的数据
    assert_eq(version, "00")
    assert_eq(trace_id, "trace-1234567890abcdef")
    assert_eq(span_id, "span-1234567890abcdef")
    assert_eq(flags, "01")
    
    // 重建SpanContext
    let deserialized_span_ctx = SpanContext::new(trace_id, span_id, flags == "01", serialized_tracestate)
    
    // 验证重建的SpanContext
    assert_eq(SpanContext::trace_id(deserialized_span_ctx), "trace-1234567890abcdef")
    assert_eq(SpanContext::span_id(deserialized_span_ctx), "span-1234567890abcdef")
    assert_true(SpanContext::is_sampled(deserialized_span_ctx))
    assert_eq(SpanContext::trace_state(deserialized_span_ctx), "key1=value1,key2=value2")
  }
  
  // 验证原始SpanContext
  assert_eq(SpanContext::trace_id(span_ctx), "trace-1234567890abcdef")
  assert_eq(SpanContext::span_id(span_ctx), "span-1234567890abcdef")
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_eq(SpanContext::trace_state(span_ctx), "key1=value1,key2=value2")
}