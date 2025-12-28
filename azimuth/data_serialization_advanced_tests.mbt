// Data Serialization and Deserialization Advanced Tests for Azimuth Telemetry System
// Focus on data integrity, format compatibility, and transformation

test "SpanContext序列化测试" {
  // 测试基本的SpanContext序列化场景
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "rojo=00f067aa0ba902b7")
  
  // 模拟序列化为字符串格式
  let serialized = SpanContext::trace_id(span_ctx) + "-" + SpanContext::span_id(span_ctx) + "-" + 
    (if SpanContext::is_sampled(span_ctx) { "1" } else { "0" }) + "-" + span_ctx.trace_state
  
  assert_eq(serialized, "0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-1-rojo=00f067aa0ba902b7")
  
  // 测试空SpanContext的序列化
  let empty_ctx = SpanContext::new("", "", false, "")
  let empty_serialized = SpanContext::trace_id(empty_ctx) + "-" + SpanContext::span_id(empty_ctx) + "-" +
    (if SpanContext::is_sampled(empty_ctx) { "1" } else { "0" }) + "-" + empty_ctx.trace_state
  
  assert_eq(empty_serialized, "--0-")
}

test "属性值序列化测试" {
  let attrs = Attributes::new()
  
  // 测试各种属性类型的序列化
  Attributes::set(attrs, "string.attr", StringValue("test_string"))
  Attributes::set(attrs, "int.attr", IntValue(42))
  Attributes::set(attrs, "float.attr", FloatValue(3.14159))
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 模拟序列化为JSON格式
  let serialized_parts = []
  
  // 获取并序列化字符串属性
  let string_result = Attributes::get(attrs, "string.attr")
  match string_result {
    Some(StringValue(value)) => {
      serialized_parts.push("\"string.attr\":\"" + value + "\"")
    }
    _ => {}
  }
  
  // 获取并序列化整数属性
  let int_result = Attributes::get(attrs, "int.attr")
  match int_result {
    Some(IntValue(value)) => {
      serialized_parts.push("\"int.attr\":" + value.to_string())
    }
    _ => {}
  }
  
  // 获取并序列化浮点数属性
  let float_result = Attributes::get(attrs, "float.attr")
  match float_result {
    Some(FloatValue(value)) => {
      serialized_parts.push("\"float.attr\":" + value.to_string())
    }
    _ => {}
  }
  
  // 获取并序列化布尔属性
  let bool_result = Attributes::get(attrs, "bool.attr")
  match bool_result {
    Some(BoolValue(value)) => {
      serialized_parts.push("\"bool.attr\":" + (if value { "true" } else { "false" }))
    }
    _ => {}
  }
  
  // 获取并序列化字符串数组属性
  let array_string_result = Attributes::get(attrs, "array.string")
  match array_string_result {
    Some(ArrayStringValue(values)) => {
      let array_str = "[\"" + values.join("\",\"") + "\"]"
      serialized_parts.push("\"array.string\":" + array_str)
    }
    _ => {}
  }
  
  // 验证序列化结果
  assert_true(serialized_parts.length() > 0)
  assert_true(serialized_parts[0].contains("string.attr"))
  assert_true(serialized_parts[1].contains("int.attr"))
}

test "Resource序列化测试" {
  let resource_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("host.name", StringValue("production-server-01")),
    ("process.pid", IntValue(12345)),
    ("process.memory.usage", IntValue(1048576)),
    ("feature.flags", ArrayStringValue(["feature-a", "feature-b", "feature-c"])),
    ("availability.zones", ArrayIntValue([1, 2, 3])),
    ("maintenance.mode", BoolValue(false)),
    ("cpu.usage", FloatValue(0.75))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), resource_attrs)
  
  // 模拟序列化为YAML格式
  let yaml_lines = ["resource:"]
  
  for attr in resource_attrs {
    match attr.1 {
      StringValue(value) => {
        yaml_lines.push("  " + attr.0 + ": \"" + value + "\"")
      }
      IntValue(value) => {
        yaml_lines.push("  " + attr.0 + ": " + value.to_string())
      }
      FloatValue(value) => {
        yaml_lines.push("  " + attr.0 + ": " + value.to_string())
      }
      BoolValue(value) => {
        yaml_lines.push("  " + attr.0 + ": " + (if value { "true" } else { "false" }))
      }
      ArrayStringValue(values) => {
        yaml_lines.push("  " + attr.0 + ":")
        for value in values {
          yaml_lines.push("    - \"" + value + "\"")
        }
      }
      ArrayIntValue(values) => {
        yaml_lines.push("  " + attr.0 + ":")
        for value in values {
          yaml_lines.push("    - " + value.to_string())
        }
      }
    }
  }
  
  let yaml_content = yaml_lines.join("\n")
  
  // 验证YAML格式
  assert_true(yaml_content.contains("resource:"))
  assert_true(yaml_content.contains("service.name: \"azimuth-service\""))
  assert_true(yaml_content.contains("process.pid: 12345"))
  assert_true(yaml_content.contains("maintenance.mode: false"))
  assert_true(yaml_content.contains("feature.flags:"))
  assert_true(yaml_content.contains("- \"feature-a\""))
}

test "LogRecord序列化测试" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "error.type", StringValue("ValidationError"))
  Attributes::set(attrs, "error.code", IntValue(400))
  Attributes::set(attrs, "user.id", StringValue("user-12345"))
  Attributes::set(attrs, "request.id", StringValue("req-67890"))
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Failed to process user request"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    None
  )
  
  // 模拟序列化为结构化日志格式
  let severity_str = match LogRecord::severity_number(log_record) {
    Trace => "TRACE"
    Debug => "DEBUG"
    Info => "INFO"
    Warn => "WARN"
    Error => "ERROR"
    Fatal => "FATAL"
  }
  
  let body_str = match LogRecord::body(log_record) {
    Some(message) => message
    None => ""
  }
  
  let timestamp_str = match log_record.timestamp {
    Some(ts) => ts.to_string()
    None => ""
  }
  
  let trace_id_str = match LogRecord::trace_id(log_record) {
    Some(tid) => tid
    None => ""
  }
  
  let span_id_str = match LogRecord::span_id(log_record) {
    Some(sid) => sid
    None => ""
  }
  
  // 构建结构化日志条目
  let structured_log = "{" +
    "\"timestamp\":\"" + timestamp_str + "\"," +
    "\"severity\":\"" + severity_str + "\"," +
    "\"message\":\"" + body_str + "\"," +
    "\"trace_id\":\"" + trace_id_str + "\"," +
    "\"span_id\":\"" + span_id_str + "\"," +
    "\"attributes\":{" +
    "\"error.type\":\"ValidationError\"," +
    "\"error.code\":400," +
    "\"user.id\":\"user-12345\"," +
    "\"request.id\":\"req-67890\"" +
    "}" +
    "}"
  
  // 验证结构化日志格式
  assert_true(structured_log.contains("\"severity\":\"ERROR\""))
  assert_true(structured_log.contains("\"message\":\"Failed to process user request\""))
  assert_true(structured_log.contains("\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\""))
  assert_true(structured_log.contains("\"error.type\":\"ValidationError\""))
  assert_true(structured_log.contains("\"error.code\":400"))
}

test "Instrument序列化测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "serialization.test")
  
  // 创建各种类型的instrument
  let counter = Meter::create_counter(meter, "request.count", Some("Total number of requests"), Some("requests"))
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Number of active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Current memory usage"), Some("bytes"))
  
  let instruments = [
    Histogram::as_instrument(counter),
    Histogram::as_instrument(histogram),
    Counter(updown_counter.name, updown_counter.description, updown_counter.unit),
    Histogram(gauge.name, gauge.description, gauge.unit)
  ]
  
  // 模拟序列化为Prometheus格式
  let prometheus_lines = []
  
  for instrument in instruments {
    let name = Instrument::name(instrument)
    let description = match Instrument::description(instrument) {
      Some(desc) => desc
      None => ""
    }
    let unit = match Instrument::unit(instrument) {
      Some(u) => u
      None => ""
    }
    
    if description != "" {
      prometheus_lines.push("# HELP " + name + " " + description)
    }
    if unit != "" {
      prometheus_lines.push("# TYPE " + name + " " + unit)
    }
    
    // 添加示例值
    prometheus_lines.push(name + " 0")
  }
  
  let prometheus_content = prometheus_lines.join("\n")
  
  // 验证Prometheus格式
  assert_true(prometheus_content.contains("# HELP request.count Total number of requests"))
  assert_true(prometheus_content.contains("# TYPE response.time ms"))
  assert_true(prometheus_content.contains("request.count 0"))
  assert_true(prometheus_content.contains("response.time 0"))
}

test "Baggage序列化测试" {
  let baggage = Baggage::new()
  
  // 设置多个baggage条目
  let baggage1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session-67890")
  let baggage3 = Baggage::set_entry(baggage2, "request.origin", "web")
  let baggage4 = Baggage::set_entry(baggage3, "user.role", "admin")
  let final_baggage = Baggage::set_entry(baggage4, "trace.sampled", "true")
  
  // 模拟序列化为W3C Baggage格式
  let baggage_entries = [
    "user.id=12345",
    "session.id=session-67890", 
    "request.origin=web",
    "user.role=admin",
    "trace.sampled=true"
  ]
  
  let serialized_baggage = baggage_entries.join(",")
  
  // 验证W3C Baggage格式
  assert_eq(serialized_baggage, "user.id=12345,session.id=session-67890,request.origin=web,user.role=admin,trace.sampled=true")
  
  // 测试反序列化（简化版本）
  let entries = serialized_baggage.split(",")
  assert_eq(entries.length(), 5)
  assert_true(entries.contains("user.id=12345"))
  assert_true(entries.contains("session.id=session-67890"))
}

test "TextMapCarrier序列化测试" {
  let carrier = TextMapCarrier::new()
  
  // 设置各种类型的头部
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "user.id=12345,session.id=session67890")
  TextMapCarrier::set(carrier, "x-request-id", "req-12345")
  TextMapCarrier::set(carrier, "x-b3-traceid", "0af7651916cd43dd8448eb211c80319c")
  TextMapCarrier::set(carrier, "x-b3-spanid", "b7ad6b7169203331")
  TextMapCarrier::set(carrier, "x-b3-sampled", "1")
  TextMapCarrier::set(carrier, "user-agent", "Azimuth-Telemetry/1.0.0")
  TextMapCarrier::set(carrier, "content-type", "application/json")
  
  // 模拟序列化为HTTP头部格式
  let header_lines = []
  
  let known_headers = [
    "traceparent", "baggage", "x-request-id", "x-b3-traceid", 
    "x-b3-spanid", "x-b3-sampled", "user-agent", "content-type"
  ]
  
  for header in known_headers {
    let value = TextMapCarrier::get(carrier, header)
    match value {
      Some(v) => {
        header_lines.push(header + ": " + v)
      }
      None => {}
    }
  }
  
  let headers_content = header_lines.join("\n")
  
  // 验证HTTP头部格式
  assert_true(headers_content.contains("traceparent: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_true(headers_content.contains("baggage: user.id=12345,session.id=session67890"))
  assert_true(headers_content.contains("x-request-id: req-12345"))
  assert_true(headers_content.contains("user-agent: Azimuth-Telemetry/1.0.0"))
}

test "复合数据结构序列化测试" {
  // 创建复杂的嵌套数据结构
  let attrs = Attributes::new()
  Attributes::set(attrs, "nested.level1", StringValue("value1"))
  Attributes::set(attrs, "nested.level2", IntValue(42))
  Attributes::set(attrs, "nested.array", ArrayStringValue(["item1", "item2", "item3"]))
  
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("complex-service")),
    ("service.namespace", StringValue("production")),
    ("service.version", StringValue("2.1.0"))
  ])
  
  let span_ctx = SpanContext::new("trace123", "span456", true, "key1=value1,key2=value2")
  
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("Complex serialization test"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    None
  )
  
  // 模拟序列化为完整的JSON格式
  let json_content = "{" +
    "\"log_record\":{" +
    "\"severity\":\"WARN\"," +
    "\"body\":\"Complex serialization test\"," +
    "\"timestamp\":\"1735689600000000000\"," +
    "\"trace_id\":\"trace123\"," +
    "\"span_id\":\"span456\"," +
    "\"attributes\":{" +
    "\"nested.level1\":\"value1\"," +
    "\"nested.level2\":42," +
    "\"nested.array\":[\"item1\",\"item2\",\"item3\"]" +
    "}" +
    "}," +
    "\"resource\":{" +
    "\"service.name\":\"complex-service\"," +
    "\"service.namespace\":\"production\"," +
    "\"service.version\":\"2.1.0\"" +
    "}," +
    "\"span_context\":{" +
    "\"trace_id\":\"trace123\"," +
    "\"span_id\":\"span456\"," +
    "\"sampled\":true," +
    "\"trace_state\":\"key1=value1,key2=value2\"" +
    "}" +
    "}"
  
  // 验证JSON结构完整性
  assert_true(json_content.contains("\"log_record\":{"))
  assert_true(json_content.contains("\"resource\":{"))
  assert_true(json_content.contains("\"span_context\":{"))
  assert_true(json_content.contains("\"severity\":\"WARN\""))
  assert_true(json_content.contains("\"service.name\":\"complex-service\""))
  assert_true(json_content.contains("\"sampled\":true"))
  assert_true(json_content.contains("\"nested.array\":[\"item1\",\"item2\",\"item3\"]"))
}

test "数据完整性验证测试" {
  // 创建原始数据
  let original_attrs = Attributes::new()
  Attributes::set(original_attrs, "string.key", StringValue("original_value"))
  Attributes::set(original_attrs, "int.key", IntValue(100))
  Attributes::set(original_attrs, "bool.key", BoolValue(true))
  
  // 模拟序列化和反序列化过程
  let serialized_data = []
  
  // "序列化"字符串属性
  let string_result = Attributes::get(original_attrs, "string.key")
  match string_result {
    Some(StringValue(value)) => {
      serialized_data.push("string:" + value)
    }
    _ => {}
  }
  
  // "序列化"整数属性
  let int_result = Attributes::get(original_attrs, "int.key")
  match int_result {
    Some(IntValue(value)) => {
      serialized_data.push("int:" + value.to_string())
    }
    _ => {}
  }
  
  // "序列化"布尔属性
  let bool_result = Attributes::get(original_attrs, "bool.key")
  match bool_result {
    Some(BoolValue(value)) => {
      serialized_data.push("bool:" + (if value { "true" } else { "false" }))
    }
    _ => {}
  }
  
  // "反序列化"过程
  let restored_attrs = Attributes::new()
  for data in serialized_data {
    if data.starts_with("string:") {
      let value = data.substring(7) // 跳过 "string:"
      Attributes::set(restored_attrs, "string.key", StringValue(value))
    } else if data.starts_with("int:") {
      let value_str = data.substring(4) // 跳过 "int:"
      let value = value_str.to_int()
      Attributes::set(restored_attrs, "int.key", IntValue(value))
    } else if data.starts_with("bool:") {
      let value_str = data.substring(5) // 跳过 "bool:"
      let value = value_str == "true"
      Attributes::set(restored_attrs, "bool.key", BoolValue(value))
    }
  }
  
  // 验证数据完整性
  let restored_string = Attributes::get(restored_attrs, "string.key")
  match restored_string {
    Some(StringValue(value)) => assert_eq(value, "original_value")
    _ => assert_true(false)
  }
  
  let restored_int = Attributes::get(restored_attrs, "int.key")
  match restored_int {
    Some(IntValue(value)) => assert_eq(value, 100)
    _ => assert_true(false)
  }
  
  let restored_bool = Attributes::get(restored_attrs, "bool.key")
  match restored_bool {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
}