// Azimuth Telemetry System - Serialization and Deserialization Tests
// 测试序列化和反序列化功能

test "属性值序列化格式测试" {
  // 测试字符串属性值的序列化表示
  let string_attr = StringValue("test value")
  let string_array_attr = ArrayStringValue(["item1", "item2", "item3"])
  
  // 验证字符串属性值的结构
  match string_attr {
    StringValue(value) => assert_eq(value, "test value")
    _ => assert_true(false, "Expected StringValue")
  }
  
  match string_array_attr {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => assert_true(false, "Expected ArrayStringValue")
  }
  
  // 测试整数属性值的序列化表示
  let int_attr = IntValue(42)
  let int_array_attr = ArrayIntValue([1, 2, 3, 4, 5])
  
  match int_attr {
    IntValue(value) => assert_eq(value, 42)
    _ => assert_true(false, "Expected IntValue")
  }
  
  match int_array_attr {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1)
      assert_eq(values[1], 2)
      assert_eq(values[2], 3)
      assert_eq(values[3], 4)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false, "Expected ArrayIntValue")
  }
  
  // 测试浮点数属性值的序列化表示
  let float_attr = FloatValue(3.14159)
  match float_attr {
    FloatValue(value) => assert_eq(value, 3.14159)
    _ => assert_true(false, "Expected FloatValue")
  }
  
  // 测试布尔属性值的序列化表示
  let bool_attr_true = BoolValue(true)
  let bool_attr_false = BoolValue(false)
  
  match bool_attr_true {
    BoolValue(value) => assert_true(value)
    _ => assert_true(false, "Expected BoolValue")
  }
  
  match bool_attr_false {
    BoolValue(value) => assert_false(value)
    _ => assert_true(false, "Expected BoolValue")
  }
}

test "Span上下文序列化测试" {
  // 测试SpanContext的序列化格式
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "key1=value1,key2=value2")
  
  // 验证SpanContext各个字段的值
  assert_eq(SpanContext::trace_id(span_ctx), "trace-123")
  assert_eq(SpanContext::span_id(span_ctx), "span-456")
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  // 测试不同类型的SpanContext
  let unsampled_ctx = SpanContext::new("trace-789", "span-101112", false, "")
  assert_false(SpanContext::is_sampled(unsampled_ctx))
  assert_true(SpanContext::is_valid(unsampled_ctx))
  
  let invalid_ctx = SpanContext::new("", "span-131415", true, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
  
  let empty_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_ctx))
  assert_false(SpanContext::is_sampled(empty_ctx))
  
  // 测试复杂trace_state
  let complex_ctx = SpanContext::new("trace-complex", "span-complex", true, "vendor1=value1,vendor2=value2,custom=custom_value")
  assert_eq(SpanContext::trace_id(complex_ctx), "trace-complex")
  assert_eq(SpanContext::span_id(complex_ctx), "span-complex")
  assert_true(SpanContext::is_sampled(complex_ctx))
  assert_true(SpanContext::is_valid(complex_ctx))
}

test "资源属性序列化测试" {
  // 测试Resource的序列化
  let resource = Resource::new()
  assert_eq(resource.attributes.length(), 0)
  
  // 测试带属性的Resource
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("localhost")),
    ("process.pid", IntValue(12345)),
    ("process.memory.usage", FloatValue(1024.5)),
    ("process.debug", BoolValue(false))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  assert_eq(resource_with_attrs.attributes.length(), 7)
  
  // 验证能够正确获取属性
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let process_pid = Resource::get_attribute(resource_with_attrs, "process.pid")
  let process_memory = Resource::get_attribute(resource_with_attrs, "process.memory.usage")
  let process_debug = Resource::get_attribute(resource_with_attrs, "process.debug")
  let nonexistent = Resource::get_attribute(resource_with_attrs, "nonexistent.key")
  
  assert_eq(service_name, Some(StringValue("test-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(process_pid, Some(IntValue(12345)))
  assert_eq(process_memory, Some(FloatValue(1024.5)))
  assert_eq(process_debug, Some(BoolValue(false)))
  assert_eq(nonexistent, None)
  
  // 测试Resource合并
  let base_attrs = [("base.key", StringValue("base-value")), ("common.key", StringValue("base-common"))]
  let override_attrs = [("override.key", StringValue("override-value")), ("common.key", StringValue("override-common"))]
  
  let base_resource = Resource::with_attributes(resource, base_attrs)
  let override_resource = Resource::with_attributes(resource, override_attrs)
  let merged = Resource::merge(base_resource, override_resource)
  
  // 在简化实现中，merge返回override资源
  let merged_override = Resource::get_attribute(merged, "override.key")
  let merged_common = Resource::get_attribute(merged, "common.key")
  let merged_base = Resource::get_attribute(merged, "base.key")
  
  assert_eq(merged_override, Some(StringValue("override-value")))
  assert_eq(merged_common, Some(StringValue("override-common")))
  assert_eq(merged_base, None) // 简化实现中base属性被覆盖
}

test "HTTP请求响应序列化测试" {
  // 测试HttpRequest的序列化结构
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-456"),
    ("User-Agent", "Azimuth-Client/1.0")
  ]
  
  let request = HttpRequest::new("POST", "https://api.example.com/v1/data", headers, Some("{\"key\":\"value\"}"))
  
  // 验证请求的各个字段
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/v1/data")
  assert_eq(HttpRequest::body(request), Some("{\"key\":\"value\"}"))
  
  // 测试不同HTTP方法的请求
  let get_request = HttpRequest::new("GET", "https://api.example.com/resource", [], None)
  let put_request = HttpRequest::new("PUT", "https://api.example.com/resource", [], Some("{\"updated\":true}"))
  let delete_request = HttpRequest::new("DELETE", "https://api.example.com/resource/123", [], None)
  let patch_request = HttpRequest::new("PATCH", "https://api.example.com/resource/123", [], Some("{\"field\":\"new_value\"}"))
  
  assert_eq(HttpRequest::http_method(get_request), "GET")
  assert_eq(HttpRequest::http_method(put_request), "PUT")
  assert_eq(HttpRequest::http_method(delete_request), "DELETE")
  assert_eq(HttpRequest::http_method(patch_request), "PATCH")
  
  // 测试HttpResponse的序列化结构
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-ID", "resp-789"),
    ("Cache-Control", "no-cache"),
    ("Server", "Azimuth-Server/1.0")
  ]
  
  let response = HttpResponse::new(200, response_headers, Some("{\"result\":\"success\",\"data\":[1,2,3]}"))
  
  // 验证响应的各个字段
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"result\":\"success\",\"data\":[1,2,3]}"))
  
  // 测试不同状态码的响应
  let success_response = HttpResponse::new(200, [], Some("OK"))
  let created_response = HttpResponse::new(201, [], Some("Created"))
  let no_content_response = HttpResponse::new(204, [], None)
  let bad_request_response = HttpResponse::new(400, [], Some("Bad Request"))
  let unauthorized_response = HttpResponse::new(401, [], Some("Unauthorized"))
  let forbidden_response = HttpResponse::new(403, [], Some("Forbidden"))
  let not_found_response = HttpResponse::new(404, [], Some("Not Found"))
  let internal_error_response = HttpResponse::new(500, [], Some("Internal Server Error"))
  
  assert_eq(HttpResponse::status_code(success_response), 200)
  assert_eq(HttpResponse::status_code(created_response), 201)
  assert_eq(HttpResponse::status_code(no_content_response), 204)
  assert_eq(HttpResponse::status_code(bad_request_response), 400)
  assert_eq(HttpResponse::status_code(unauthorized_response), 401)
  assert_eq(HttpResponse::status_code(forbidden_response), 403)
  assert_eq(HttpResponse::status_code(not_found_response), 404)
  assert_eq(HttpResponse::status_code(internal_error_response), 500)
  
  // 测试空体的请求和响应
  let empty_request = HttpRequest::new("HEAD", "https://api.example.com/health", [], None)
  let empty_response = HttpResponse::new(204, [], None)
  
  assert_eq(HttpRequest::body(empty_request), None)
  assert_eq(HttpResponse::body(empty_response), None)
}

test "日志记录序列化测试" {
  // 测试LogRecord的序列化结构
  let simple_log = LogRecord::new(Info, "Simple log message")
  
  assert_eq(LogRecord::severity_number(simple_log), Info)
  assert_eq(LogRecord::body(simple_log), Some("Simple log message"))
  assert_eq(LogRecord::trace_id(simple_log), None)
  assert_eq(LogRecord::span_id(simple_log), None)
  
  // 测试完整的LogRecord
  let attrs = Attributes::new()
  let full_log = LogRecord::new_with_context(
    Error,
    Some("Detailed error message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(full_log), Error)
  assert_eq(LogRecord::body(full_log), Some("Detailed error message"))
  assert_eq(LogRecord::trace_id(full_log), Some("trace-123"))
  assert_eq(LogRecord::span_id(full_log), Some("span-456"))
  
  // 测试不同严重程度的日志
  let trace_log = LogRecord::new(Trace, "Trace level message")
  let debug_log = LogRecord::new(Debug, "Debug level message")
  let info_log = LogRecord::new(Info, "Info level message")
  let warn_log = LogRecord::new(Warn, "Warning level message")
  let error_log = LogRecord::new(Error, "Error level message")
  let fatal_log = LogRecord::new(Fatal, "Fatal level message")
  
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // 测试带有特殊字符的日志消息
  let unicode_log = LogRecord::new(Info, "测试中文日志消息")
  let special_chars_log = LogRecord::new(Warn, "Message with special chars: !@#$%^&*()")
  let json_log = LogRecord::new(Error, "{\"error\":\"validation_failed\",\"field\":\"email\"}")
  let multiline_log = LogRecord::new(Info, "Line 1\nLine 2\nLine 3")
  let empty_log = LogRecord::new(Debug, "")
  
  assert_eq(LogRecord::body(unicode_log), Some("测试中文日志消息"))
  assert_eq(LogRecord::body(special_chars_log), Some("Message with special chars: !@#$%^&*()"))
  assert_eq(LogRecord::body(json_log), Some("{\"error\":\"validation_failed\",\"field\":\"email\"}"))
  assert_eq(LogRecord::body(multiline_log), Some("Line 1\nLine 2\nLine 3"))
  assert_eq(LogRecord::body(empty_log), Some(""))
  
  // 测试带有空body的日志
  let no_body_log = LogRecord::new_with_context(
    Info,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::body(no_body_log), None)
}

test "指标数据序列化测试" {
  // 测试Instrument类型的序列化
  let counter_instrument = Counter("request_count", Some("Total number of requests"), Some("count"))
  let histogram_instrument = Histogram("request_duration", Some("Request duration histogram"), Some("ms"))
  let updown_counter_instrument = UpDownCounter("active_connections", Some("Current active connections"), Some("connections"))
  let gauge_instrument = Gauge("cpu_usage", Some("Current CPU usage"), Some("percent"))
  
  // 验证Instrument的名称
  assert_eq(Instrument::name(counter_instrument), "request_count")
  assert_eq(Instrument::name(histogram_instrument), "request_duration")
  assert_eq(Instrument::name(updown_counter_instrument), "active_connections")
  assert_eq(Instrument::name(gauge_instrument), "cpu_usage")
  
  // 验证Instrument的描述
  assert_eq(Instrument::description(counter_instrument), Some("Total number of requests"))
  assert_eq(Instrument::description(histogram_instrument), Some("Request duration histogram"))
  assert_eq(Instrument::description(updown_counter_instrument), Some("Current active connections"))
  assert_eq(Instrument::description(gauge_instrument), Some("Current CPU usage"))
  
  // 验证Instrument的单位
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  assert_eq(Instrument::unit(updown_counter_instrument), Some("connections"))
  assert_eq(Instrument::unit(gauge_instrument), Some("percent"))
  
  // 测试Meter和MeterProvider的序列化结构
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  assert_eq(meter.scope.name, "test-meter")
  assert_eq(meter.scope.version, None)
  assert_eq(meter.scope.schema_url, None)
  
  // 测试带有版本的Meter
  let meter_with_version = MeterProvider::get_meter(meter_provider, "versioned-meter", Some("2.1.0"))
  assert_eq(meter_with_version.scope.name, "versioned-meter")
  assert_eq(meter_with_version.scope.version, Some("2.1.0"))
  
  // 测试Counter的序列化
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("unit"))
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("unit"))
  
  // 测试Histogram的序列化
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  assert_eq(histogram.name, "test_histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // 测试Histogram转换为Instrument
  let histogram_as_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_as_instrument), "test_histogram")
  assert_eq(Instrument::description(histogram_as_instrument), Some("Test histogram"))
  assert_eq(Instrument::unit(histogram_as_instrument), Some("ms"))
}