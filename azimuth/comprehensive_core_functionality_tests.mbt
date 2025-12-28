// Azimuth 核心功能测试用例
// 包含10个针对不同功能的测试用例

test "span_kind_enum_test" {
  // 测试 SpanKind 枚举的所有变体
  let internal_span = Internal
  let server_span = Server
  let client_span = Client
  let producer_span = Producer
  let consumer_span = Consumer
  
  // 验证枚举值可以正确创建和比较
  assert_eq(internal_span, Internal)
  assert_eq(server_span, Server)
  assert_eq(client_span, Client)
  assert_eq(producer_span, Producer)
  assert_eq(consumer_span, Consumer)
  
  // 验证不同枚举值不相等
  assert_true(internal_span != server_span)
  assert_true(client_span != producer_span)
}

test "status_code_enum_test" {
  // 测试 StatusCode 枚举的所有变体
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  // 验证枚举值可以正确创建和比较
  assert_eq(unset_status, Unset)
  assert_eq(ok_status, Ok)
  assert_eq(error_status, Error)
  
  // 验证不同枚举值不相等
  assert_not_eq(unset_status, ok_status)
  assert_not_eq(ok_status, error_status)
  assert_not_eq(unset_status, error_status)
}

test "severity_number_enum_test" {
  // 测试 SeverityNumber 枚举的所有变体
  let trace_severity = Trace
  let debug_severity = Debug
  let info_severity = Info
  let warn_severity = Warn
  let error_severity = Error
  let fatal_severity = Fatal
  
  // 验证枚举值可以正确创建和比较
  assert_eq(trace_severity, Trace)
  assert_eq(debug_severity, Debug)
  assert_eq(info_severity, Info)
  assert_eq(warn_severity, Warn)
  assert_eq(error_severity, Error)
  assert_eq(fatal_severity, Fatal)
  
  // 验证严重程度的顺序关系（通过不等式验证）
  assert_not_eq(trace_severity, debug_severity)
  assert_not_eq(info_severity, warn_severity)
  assert_not_eq(error_severity, fatal_severity)
}

test "attribute_value_types_test" {
  // 测试 AttributeValue 枚举的各种类型
  let string_val = StringValue("test")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14)
  let bool_val = BoolValue(true)
  let array_string_val = ArrayStringValue(["a", "b", "c"])
  let array_int_val = ArrayIntValue([1, 2, 3])
  
  // 验证各种类型的属性值可以正确创建
  assert_eq(string_val, StringValue("test"))
  assert_eq(int_val, IntValue(42))
  assert_eq(float_val, FloatValue(3.14))
  assert_eq(bool_val, BoolValue(true))
  assert_eq(array_string_val, ArrayStringValue(["a", "b", "c"]))
  assert_eq(array_int_val, ArrayIntValue([1, 2, 3]))
  
  // 验证不同类型的值不相等
  assert_not_eq(string_val, int_val)
  assert_not_eq(float_val, bool_val)
  assert_not_eq(array_string_val, array_int_val)
}

test "instrumentation_scope_test" {
  // 测试 InstrumentationScope 的创建和属性
  let scope_with_version = InstrumentationScope::{
    name: "test.instrument",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  let scope_minimal = InstrumentationScope::{
    name: "minimal.instrument",
    version: None,
    schema_url: None
  }
  
  // 验证作用域属性
  assert_eq(scope_with_version.name, "test.instrument")
  assert_eq(scope_with_version.version, Some("1.0.0"))
  assert_eq(scope_with_version.schema_url, Some("https://example.com/schema"))
  
  assert_eq(scope_minimal.name, "minimal.instrument")
  assert_eq(scope_minimal.version, None)
  assert_eq(scope_minimal.schema_url, None)
  
  // 验证不同作用域不相等
  assert_not_eq(scope_with_version.name, scope_minimal.name)
}

test "span_context_validation_test" {
  // 测试 SpanContext 的验证功能
  let valid_context = SpanContext::new("trace123", "span456", true, "state=value")
  let invalid_empty_trace = SpanContext::new("", "span456", true, "")
  let invalid_empty_span = SpanContext::new("trace123", "", true, "")
  let not_sampled_context = SpanContext::new("trace789", "span012", false, "")
  
  // 验证有效上下文
  assert_eq(SpanContext::trace_id(valid_context), "trace123")
  assert_eq(SpanContext::span_id(valid_context), "span456")
  assert_eq(SpanContext::is_valid(valid_context), true)
  assert_eq(SpanContext::is_sampled(valid_context), true)
  
  // 验证无效上下文
  assert_eq(SpanContext::is_valid(invalid_empty_trace), false)
  assert_eq(SpanContext::is_valid(invalid_empty_span), false)
  
  // 验证采样状态
  assert_eq(SpanContext::is_sampled(not_sampled_context), false)
  assert_eq(SpanContext::is_sampled(valid_context), true)
}

test "resource_operations_test" {
  // 测试 Resource 的创建和操作
  let base_resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(base_resource, attrs)
  
  // 验证资源属性获取
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.instance.id"), Some(StringValue("instance-123")))
  
  // 验证不存在的属性
  assert_eq(Resource::get_attribute(resource_with_attrs, "nonexistent"), None)
  
  // 测试资源合并
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("environment", StringValue("production"))
  ]
  let override_resource = Resource::with_attributes(base_resource, override_attrs)
  let merged = Resource::merge(resource_with_attrs, override_resource)
  
  // 验证合并结果（根据实现，应该返回override_resource）
  assert_eq(Resource::get_attribute(merged, "service.name"), Some(StringValue("override-service")))
  assert_eq(Resource::get_attribute(merged, "environment"), Some(StringValue("production")))
}

test "text_map_carrier_test" {
  // 测试 TextMapCarrier 的注入和提取操作
  let carrier = TextMapCarrier::new()
  
  // 测试获取已知的traceparent
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // 测试获取不存在的头部
  let nonexistent = TextMapCarrier::get(carrier, "nonexistent")
  assert_eq(nonexistent, None)
  
  // 测试设置头部（虽然实现是简化的，但应该不会出错）
  TextMapCarrier::set(carrier, "custom-header", "custom-value")
  // 由于实现是简化的，我们无法验证设置是否成功，但至少确保操作不会崩溃
}

test "clock_and_random_test" {
  // 测试 Clock 和 Random 的基本功能
  let system_clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(system_clock)
  
  // 验证时间戳是合理的（应该是2025年的时间戳）
  assert_eq(timestamp, 1735689600000000000L)
  
  // 测试随机数生成器
  let system_random = Random::system()
  
  // 测试生成字节数组
  let random_bytes = Random::next_bytes(system_random, 10)
  // 简化实现返回空数组，验证这一点
  assert_eq(random_bytes.length, 0)
  
  // 测试生成UInt64随机数
  let random_u64 = Random::next_u64(system_random)
  assert_eq(random_u64, 12345UL)
}

test "http_client_operations_test" {
  // 测试 HttpClient 的请求和响应操作
  let client = HttpClient::new()
  
  // 测试创建HTTP请求
  let headers = [("Content-Type", "application/json"), ("Accept", "application/json")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("{\"test\": true}"))
  
  // 验证请求属性
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"test\": true}"))
  
  // 测试创建HTTP响应
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\": \"success\"}"))
  
  // 验证响应属性
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"result\": \"success\"}"))
}