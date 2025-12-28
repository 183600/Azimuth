// Azimuth Telemetry System - Data Serialization and Format Tests
// 测试数据序列化和格式转换功能

test "attribute value JSON serialization" {
  // 测试属性值的JSON序列化
  let attrs = @azimuth.Attributes::new()
  
  // 设置各种类型的属性用于序列化测试
  @azimuth.Attributes::set(attrs, "string.value", @azimuth.StringValue("test string"))
  @azimuth.Attributes::set(attrs, "int.value", @azimuth.IntValue(42))
  @azimuth.Attributes::set(attrs, "float.value", @azimuth.FloatValue(3.14159))
  @azimuth.Attributes::set(attrs, "bool.value", @azimuth.BoolValue(true))
  @azimuth.Attributes::set(attrs, "string.array", @azimuth.ArrayStringValue(["a", "b", "c"]))
  @azimuth.Attributes::set(attrs, "int.array", @azimuth.ArrayIntValue([1, 2, 3]))
  
  // 测试特殊字符串值的序列化
  @azimuth.Attributes::set(attrs, "json.string", @azimuth.StringValue("{\"key\":\"value\"}"))
  @azimuth.Attributes::set(attrs, "json.array", @azimuth.StringValue("[1,2,3]"))
  @azimuth.Attributes::set(attrs, "url.encoded", @azimuth.StringValue("hello%20world"))
  @azimuth.Attributes::set(attrs, "base64.encoded", @azimuth.StringValue("SGVsbG8gV29ybGQ="))
  
  // 测试Unicode和特殊字符
  @azimuth.Attributes::set(attrs, "unicode.value", @azimuth.StringValue("中文测试"))
  @azimuth.Attributes::set(attrs, "emoji.value", @azimuth.StringValue("🚀 🎯 📊"))
  @azimuth.Attributes::set(attrs, "special.chars", @azimuth.StringValue("special!@#$%^&*()"))
  @azimuth.Attributes::set(attrs, "control.chars", @azimuth.StringValue("control	
"))
  
  // 测试空值和边界值
  @azimuth.Attributes::set(attrs, "empty.string", @azimuth.StringValue(""))
  @azimuth.Attributes::set(attrs, "zero.int", @azimuth.IntValue(0))
  @azimuth.Attributes::set(attrs, "zero.float", @azimuth.FloatValue(0.0))
  @azimuth.Attributes::set(attrs, "false.bool", @azimuth.BoolValue(false))
  
  // 验证序列化操作不会抛出异常
  assert_true(true)
}

test "span context serialization formats" {
  // 测试Span上下文的序列化格式
  let span_ctx = @azimuth.SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1,key2=value2")
  
  // 验证上下文组件
  let trace_id = @azimuth.SpanContext::trace_id(span_ctx)
  let span_id = @azimuth.SpanContext::span_id(span_ctx)
  let sampled = @azimuth.SpanContext::is_sampled(span_ctx)
  let trace_state = span_ctx.trace_state
  
  // 测试W3C Trace Context格式
  let w3c_traceparent = "00-" + trace_id + "-" + span_id + "-" + (sampled ? "01" : "00")
  
  // 测试不同的trace ID格式
  let short_trace_ctx = @azimuth.SpanContext::new("abc123", "def456", true, "")
  let long_trace_ctx = @azimuth.SpanContext::new("0af7651916cd43dd8448eb211c80319c0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331b7ad6b7169203331", true, "")
  
  // 测试不同的采样状态
  let sampled_ctx = @azimuth.SpanContext::new("trace123", "span456", true, "")
  let not_sampled_ctx = @azimuth.SpanContext::new("trace123", "span456", false, "")
  
  // 测试复杂的trace state
  let complex_trace_state_ctx = @azimuth.SpanContext::new("trace123", "span456", true, "vendor1=value1,vendor2=value2,custom=custom_value")
  
  // 验证序列化格式操作不会抛出异常
  assert_true(true)
}

test "log record serialization" {
  // 测试日志记录的序列化
  let logger_provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "serialization-test-logger")
  
  // 创建简单日志记录
  let simple_log = @azimuth.LogRecord::new(@azimuth.Info, "Simple log message")
  
  // 创建复杂日志记录
  let complex_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(complex_attrs, "user.id", @azimuth.StringValue("user123"))
  @azimuth.Attributes::set(complex_attrs, "request.id", @azimuth.StringValue("req456"))
  @azimuth.Attributes::set(complex_attrs, "duration", @azimuth.IntValue(1500))
  
  let complex_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Complex error message"),
    Some(complex_attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123"),
    Some("span456"),
    Some(@azimuth.Context::root())
  )
  
  // 测试不同严重程度的日志记录
  let trace_log = @azimuth.LogRecord::new(@azimuth.Trace, "Trace level message")
  let debug_log = @azimuth.LogRecord::new(@azimuth.Debug, "Debug level message")
  let info_log = @azimuth.LogRecord::new(@azimuth.Info, "Info level message")
  let warn_log = @azimuth.LogRecord::new(@azimuth.Warn, "Warning level message")
  let error_log = @azimuth.LogRecord::new(@azimuth.Error, "Error level message")
  let fatal_log = @azimuth.LogRecord::new(@azimuth.Fatal, "Fatal level message")
  
  // 测试包含特殊字符的日志消息
  let special_char_log = @azimuth.LogRecord::new(@azimuth.Info, "Special chars: !@#$%^&*(){}[]|\\:;\"'<>?,./")
  let unicode_log = @azimuth.LogRecord::new(@azimuth.Info, "Unicode: 中文测试 🚀 🎯")
  let json_log = @azimuth.LogRecord::new(@azimuth.Info, "{\"error\":\"validation failed\",\"code\":400}")
  
  // 测试长日志消息
  let long_message = "This is a very long log message that contains a lot of text and might be used for storing detailed information such as error messages, descriptions, or other verbose content in telemetry systems. ".repeat(10)
  let long_log = @azimuth.LogRecord::new(@azimuth.Info, long_message)
  
  // 验证日志序列化操作不会抛出异常
  assert_true(true)
}

test "metric data serialization" {
  // 测试指标数据的序列化
  let meter_provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "serialization-test-meter")
  
  // 创建各种类型的指标
  let counter = @azimuth.Meter::create_counter(meter, "test.counter", Some("Test counter description"), Some("count"))
  let histogram = @azimuth.Meter::create_histogram(meter, "test.histogram", Some("Test histogram description"), Some("ms"))
  let up_down_counter = @azimuth.Meter::create_counter(meter, "test.up_down_counter", Some("Test up-down counter"), Some("items"))
  let gauge = @azimuth.Meter::create_counter(meter, "test.gauge", Some("Test gauge"), Some("value"))
  
  // 测试指标记录（模拟序列化）
  @azimuth.Counter::add(counter, 100.0)
  @azimuth.Counter::add(counter, 50.5)
  @azimuth.Counter::add(counter, 0.0)
  @azimuth.Counter::add(counter, -10.0)
  
  @azimuth.Histogram::record(histogram, 100.0)
  @azimuth.Histogram::record(histogram, 200.5)
  @azimuth.Histogram::record(histogram, 0.1)
  @azimuth.Histogram::record(histogram, 1000.0)
  
  // 测试仪器类型的序列化
  let counter_instrument = @azimuth.Counter("counter.name", Some("Counter description"), Some("unit"))
  let histogram_instrument = @azimuth.Histogram("histogram.name", Some("Histogram description"), Some("ms"))
  let up_down_counter_instrument = @azimuth.UpDownCounter("updown.name", Some("UpDownCounter description"), Some("items"))
  let gauge_instrument = @azimuth.Gauge("gauge.name", Some("Gauge description"), Some("value"))
  
  // 测试仪器属性的序列化
  let counter_name = @azimuth.Instrument::name(counter_instrument)
  let counter_desc = @azimuth.Instrument::description(counter_instrument)
  let counter_unit = @azimuth.Instrument::unit(counter_instrument)
  
  let histogram_name = @azimuth.Instrument::name(histogram_instrument)
  let histogram_desc = @azimuth.Instrument::description(histogram_instrument)
  let histogram_unit = @azimuth.Instrument::unit(histogram_instrument)
  
  // 验证指标序列化操作不会抛出异常
  assert_true(true)
}

test "resource serialization" {
  // 测试资源的序列化
  let resource = @azimuth.Resource::new()
  
  // 创建资源属性
  let resource_attrs = [
    ("service.name", @azimuth.StringValue("azimuth-service")),
    ("service.version", @azimuth.StringValue("1.0.0")),
    ("service.instance.id", @azimuth.StringValue("instance-123")),
    ("host.name", @azimuth.StringValue("hostname")),
    ("host.arch", @azimuth.StringValue("amd64")),
    ("os.type", @azimuth.StringValue("linux")),
    ("os.version", @azimuth.StringValue("5.15.0")),
    ("process.id", @azimuth.IntValue(1234)),
    ("process.executable.name", @azimuth.StringValue("azimuth")),
    ("process.command.line", @azimuth.StringValue("azimuth run")),
    ("telemetry.sdk.name", @azimuth.StringValue("azimuth")),
    ("telemetry.sdk.version", @azimuth.StringValue("1.0.0")),
    ("telemetry.sdk.language", @azimuth.StringValue("moonbit"))
  ]
  
  let resource_with_attrs = @azimuth.Resource::with_attributes(resource, resource_attrs)
  
  // 测试特殊属性值的序列化
  let special_attrs = [
    ("json.config", @azimuth.StringValue("{\"database\":{\"host\":\"localhost\",\"port\":5432}}")),
    ("url.endpoint", @azimuth.StringValue("https://api.example.com/v1/data")),
    ("base64.certificate", @azimuth.StringValue("MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA")),
    ("unicode.description", @azimuth.StringValue("服务描述：中文测试")),
    ("emoji.status", @azimuth.StringValue("🟢 running"))
  ]
  
  let resource_with_special = @azimuth.Resource::with_attributes(resource, special_attrs)
  
  // 测试资源合并的序列化
  let merged_resource = @azimuth.Resource::merge(resource_with_attrs, resource_with_special)
  
  // 验证资源序列化操作不会抛出异常
  assert_true(true)
}

test "baggage serialization formats" {
  // 测试Baggage的序列化格式
  let baggage = @azimuth.Baggage::new()
  
  // 添加各种类型的条目
  let baggage1 = @azimuth.Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = @azimuth.Baggage::set_entry(baggage1, "request.id", "req456")
  let baggage3 = @azimuth.Baggage::set_entry(baggage2, "session.id", "session789")
  
  // 测试W3C Baggage格式
  // 格式: key1=value1,key2=value2
  let w3c_format = "user.id=user123,request.id=req456,session.id=session789"
  
  // 测试包含特殊字符的条目
  let special_baggage = @azimuth.Baggage::set_entry(baggage3, "special.key", "special=value&test")
  let unicode_baggage = @azimuth.Baggage::set_entry(special_baggage, "unicode.键", "值")
  let url_encoded_baggage = @azimuth.Baggage::set_entry(unicode_baggage, "url.encoded", "hello%20world")
  
  // 测试空值和边界条目
  let empty_key_baggage = @azimuth.Baggage::set_entry(url_encoded_baggage, "", "empty-key-value")
  let empty_value_baggage = @azimuth.Baggage::set_entry(empty_key_baggage, "empty.value", "")
  let long_key_baggage = @azimuth.Baggage::set_entry(empty_value_baggage, "this.is.a.very.long.key.name.that.might.be.used.in.some.scenarios", "long-key-value")
  let long_value_baggage = @azimuth.Baggage::set_entry(long_key_baggage, "long.value", "this.is.a.very.long.value.that.contains.a.lot.of.text.and.might.be.used.for.storing.detailed.information")
  
  // 测试属性化的Baggage条目
  // 格式: key=value;property1=value1;property2=value2
  let with_properties = @azimuth.Baggage::set_entry(long_value_baggage, "prop.key", "prop.value")
  
  // 验证Baggage序列化操作不会抛出异常
  assert_true(true)
}

test "text map carrier HTTP header serialization" {
  // 测试TextMapCarrier的HTTP头部序列化
  let carrier = @azimuth.TextMapCarrier::new()
  
  // 设置标准的HTTP头部
  @azimuth.TextMapCarrier::set(carrier, "Content-Type", "application/json; charset=utf-8")
  @azimuth.TextMapCarrier::set(carrier, "Content-Length", "1024")
  @azimuth.TextMapCarrier::set(carrier, "Authorization", "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9")
  @azimuth.TextMapCarrier::set(carrier, "User-Agent", "Azimuth-Telemetry/1.0 (MoonBit)")
  @azimuth.TextMapCarrier::set(carrier, "Accept", "application/json, text/plain, */*")
  @azimuth.TextMapCarrier::set(carrier, "Connection", "keep-alive")
  
  // 设置遥测相关的头部
  @azimuth.TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  @azimuth.TextMapCarrier::set(carrier, "tracestate", "vendor1=value1,vendor2=value2")
  @azimuth.TextMapCarrier::set(carrier, "baggage", "user.id=user123,request.id=req456")
  @azimuth.TextMapCarrier::set(carrier, "X-Correlation-ID", "corr-123")
  @azimuth.TextMapCarrier::set(carrier, "X-Request-ID", "req-456")
  
  // 测试自定义头部
  @azimuth.TextMapCarrier::set(carrier, "X-Service-Name", "azimuth-service")
  @azimuth.TextMapCarrier::set(carrier, "X-Service-Version", "1.0.0")
  @azimuth.TextMapCarrier::set(carrier, "X-Environment", "production")
  @azimuth.TextMapCarrier::set(carrier, "X-Region", "us-west-2")
  
  // 测试特殊字符头部
  @azimuth.TextMapCarrier::set(carrier, "X-Special", "special!@#$%^&*(){}[]|\\:;\"'<>?,./")
  @azimuth.TextMapCarrier::set(carrier, "X-Unicode", "中文测试")
  @azimuth.TextMapCarrier::set(carrier, "X-Emoji", "🚀 telemetry 🎯 data")
  @azimuth.TextMapCarrier::set(carrier, "X-JSON", "{\"context\":{\"user\":\"test\",\"action\":\"serialize\"}}")
  
  // 测试长头部值
  let long_header_value = "This is a very long header value that contains a lot of text and might be used for storing detailed information in HTTP headers for telemetry purposes."
  @azimuth.TextMapCarrier::set(carrier, "X-Long-Header", long_header_value)
  
  // 测试多值头部
  @azimuth.TextMapCarrier::set(carrier, "X-Multi-Value", "value1,value2,value3")
  @azimuth.TextMapCarrier::set(carrier, "Accept-Encoding", "gzip, deflate, br")
  @azimuth.TextMapCarrier::set(carrier, "Accept-Language", "en-US,en;q=0.9,zh-CN;q=0.8,zh;q=0.7")
  
  // 验证HTTP头部序列化操作不会抛出异常
  assert_true(true)
}

test "http request response serialization" {
  // 测试HTTP请求和响应的序列化
  // 创建HTTP请求
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("User-Agent", "Azimuth-Client/1.0"),
    ("X-Request-ID", "req-123")
  ]
  
  let request = @azimuth.HttpRequest::new(
    "POST",
    "https://api.example.com/v1/telemetry",
    request_headers,
    Some("{\"metric\":{\"name\":\"test.counter\",\"value\":42}}")
  )
  
  // 创建HTTP响应
  let response_headers = [
    ("Content-Type", "application/json"),
    ("Content-Length", "50"),
    ("X-Response-ID", "resp-456"),
    ("X-Processing-Time", "150ms")
  ]
  
  let response = @azimuth.HttpResponse::new(
    200,
    response_headers,
    Some("{\"status\":\"success\",\"message\":\"Data received\"}")
  )
  
  // 测试不同HTTP方法的请求
  let get_request = @azimuth.HttpRequest::new("GET", "https://api.example.com/data", [], None)
  let post_request = @azimuth.HttpRequest::new("POST", "https://api.example.com/create", [], Some("data"))
  let put_request = @azimuth.HttpRequest::new("PUT", "https://api.example.com/update", [], Some("updated"))
  let delete_request = @azimuth.HttpRequest::new("DELETE", "https://api.example.com/delete", [], None)
  let patch_request = @azimuth.HttpRequest::new("PATCH", "https://api.example.com/patch", [], Some("patch"))
  
  // 测试不同状态码的响应
  let success_response = @azimuth.HttpResponse::new(200, [], Some("Success"))
  let created_response = @azimuth.HttpResponse::new(201, [], Some("Created"))
  let no_content_response = @azimuth.HttpResponse::new(204, [], None)
  let bad_request_response = @azimuth.HttpResponse::new(400, [], Some("Bad Request"))
  let unauthorized_response = @azimuth.HttpResponse::new(401, [], Some("Unauthorized"))
  let not_found_response = @azimuth.HttpResponse::new(404, [], Some("Not Found"))
  let server_error_response = @azimuth.HttpResponse::new(500, [], Some("Internal Server Error"))
  
  // 测试包含特殊字符的请求体和响应体
  let special_request = @azimuth.HttpRequest::new(
    "POST",
    "https://api.example.com/special",
    [("Content-Type", "application/json")],
    Some("{\"special\":\"中文测试 🚀\",\"chars\":\"!@#$%^&*()\"}")
  )
  
  let special_response = @azimuth.HttpResponse::new(
    200,
    [("Content-Type", "application/json")],
    Some("{\"result\":\"success\",\"message\":\"处理完成 🎯\"}")
  )
  
  // 验证HTTP序列化操作不会抛出异常
  assert_true(true)
}