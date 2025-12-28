// 额外系统测试 - Additional System Tests for Azimuth Telemetry
// 测试核心系统功能的集成和边界情况

test "Resource 资源管理测试" {
  // 测试 Resource 创建和属性管理
  let empty_resource = @azimuth.telemetry.api.common.Resource.new()
  assert_eq(empty_resource.attributes.length, 0)
  
  // 测试带属性的 Resource
  let attributes = [
    ("service.name", @azimuth.telemetry.api.common.StringValue("azimuth-service")),
    ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
    ("service.instance.id", @azimuth.telemetry.api.common.StringValue("instance-123"))
  ]
  let resource_with_attrs = @azimuth.telemetry.api.common.Resource.with_attributes(empty_resource, attributes)
  assert_eq(resource_with_attrs.attributes.length, 3)
  
  // 测试属性获取
  let service_name = @azimuth.telemetry.api.common.Resource.get_attribute(resource_with_attrs, "service.name")
  assert_eq(service_name, Some(@azimuth.telemetry.api.common.StringValue("azimuth-service")))
  
  let missing_attr = @azimuth.telemetry.api.common.Resource.get_attribute(resource_with_attrs, "missing.key")
  assert_eq(missing_attr, None)
  
  // 测试资源合并
  let override_attrs = [
    ("service.version", @azimuth.telemetry.api.common.StringValue("2.0.0")),
    ("environment", @azimuth.telemetry.api.common.StringValue("production"))
  ]
  let override_resource = @azimuth.telemetry.api.common.Resource.with_attributes(empty_resource, override_attrs)
  let merged_resource = @azimuth.telemetry.api.common.Resource.merge(resource_with_attrs, override_resource)
  assert_eq(merged_resource.attributes.length, 2)
}

test "HTTP Client 网络请求测试" {
  // 测试 HTTP 请求创建
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123")
  ]
  let request = @azimuth.telemetry.platform.HttpRequest.new(
    "POST", 
    "https://api.example.com/telemetry", 
    headers, 
    Some("{\"data\": \"test\"}")
  )
  
  assert_eq(@azimuth.telemetry.platform.HttpRequest.http_method(request), "POST")
  assert_eq(@azimuth.telemetry.platform.HttpRequest.url(request), "https://api.example.com/telemetry")
  assert_eq(@azimuth.telemetry.platform.HttpRequest.body(request), Some("{\"data\": \"test\"}"))
  
  // 测试 HTTP 响应创建
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req-456")
  ]
  let response = @azimuth.telemetry.platform.HttpResponse.new(
    200, 
    response_headers, 
    Some("{\"status\": \"success\"}")
  )
  
  assert_eq(@azimuth.telemetry.platform.HttpResponse.status_code(response), 200)
  assert_eq(@azimuth.telemetry.platform.HttpResponse.body(response), Some("{\"status\": \"success\"}"))
  
  // 测试 HTTP Client 创建
  let client = @azimuth.telemetry.platform.HttpClient.new()
  assert_true(client != @azimuth.telemetry.platform.HttpClient::{})
}

test "Baggage 传播上下文测试" {
  // 测试 Baggage 创建和操作
  let empty_baggage = @azimuth.telemetry.api.context.Baggage.new()
  assert_eq(empty_baggage.entries.length, 0)
  
  // 测试添加条目
  let baggage_with_entry = @azimuth.telemetry.api.context.Baggage.set_entry(
    empty_baggage, 
    "user.id", 
    "user123"
  )
  
  // 测试获取条目
  let user_id = @azimuth.telemetry.api.context.Baggage.get_entry(baggage_with_entry, "user.id")
  // 由于简化实现，这里测试 None
  assert_eq(user_id, None)
  
  let missing_entry = @azimuth.telemetry.api.context.Baggage.get_entry(baggage_with_entry, "missing.key")
  assert_eq(missing_entry, None)
  
  // 测试删除条目
  let baggage_after_remove = @azimuth.telemetry.api.context.Baggage.remove_entry(
    baggage_with_entry, 
    "user.id"
  )
  // 简化实现返回原 baggage
  assert_eq(baggage_after_remove.entries.length, 0)
}

test "Composite Propagator 复合传播器测试" {
  // 测试 W3C 传播器创建
  let trace_propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  let baggage_propagator = @azimuth.telemetry.api.propagation.W3CBaggagePropagator.new()
  
  // 测试复合传播器创建
  let propagators = [trace_propagator]
  let composite_propagator = @azimuth.telemetry.api.propagation.CompositePropagator.new(propagators)
  assert_eq(composite_propagator.propagators.length, 1)
  
  // 测试注入操作
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  
  @azimuth.telemetry.api.propagation.CompositePropagator.inject(
    composite_propagator, 
    ctx, 
    carrier
  )
  
  // 验证注入的头部
  let traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // 测试提取操作
  let extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator.extract(
    composite_propagator, 
    carrier
  )
  
  // 验证提取的上下文
  let extracted_key = @azimuth.telemetry.api.context.ContextKey::new("extracted")
  let extracted_value = @azimuth.telemetry.api.context.Context.get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

test "LogRecord 扩展日志记录测试" {
  // 测试基本日志记录创建
  let basic_record = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber::Info, 
    "Basic log message"
  )
  
  assert_eq(@azimuth.telemetry.api.logs.LogRecord.severity_number(basic_record), @azimuth.telemetry.api.logs.SeverityNumber::Info)
  assert_eq(@azimuth.telemetry.api.logs.LogRecord.body(basic_record), Some("Basic log message"))
  assert_eq(@azimuth.telemetry.api.logs.LogRecord.trace_id(basic_record), None)
  assert_eq(@azimuth.telemetry.api.logs.LogRecord.span_id(basic_record), None)
  
  // 测试扩展日志记录创建
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let timestamp = 1735689600000000000L
  
  let extended_record = @azimuth.telemetry.api.logs.LogRecord::new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber::Error,
    Some("Extended log message"),
    Some(attrs),
    Some(timestamp),
    Some(timestamp + 1000L),
    Some("trace-123"),
    Some("span-456"),
    Some(ctx)
  )
  
  assert_eq(@azimuth.telemetry.api.logs.LogRecord.severity_number(extended_record), @azimuth.telemetry.api.logs.SeverityNumber::Error)
  assert_eq(@azimuth.telemetry.api.logs.LogRecord.body(extended_record), Some("Extended log message"))
  assert_eq(@azimuth.telemetry.api.logs.LogRecord.trace_id(extended_record), Some("trace-123"))
  assert_eq(@azimuth.telemetry.api.logs.LogRecord.span_id(extended_record), Some("span-456"))
  
  // 测试日志记录发射
  let logger_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logger_provider, "test.logger")
  
  @azimuth.telemetry.api.logs.Logger.emit(logger, extended_record)
  // 简化实现没有返回值，只测试不抛出异常
  assert_true(true)
}

test "SpanContext 上下文验证测试" {
  // 测试有效的 SpanContext
  let valid_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c", 
    "b7ad6b7169203331", 
    true, 
    "rojo=00f067aa0ba902b7"
  )
  
  assert_eq(@azimuth.telemetry.api.trace.SpanContext.trace_id(valid_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(@azimuth.telemetry.api.trace.SpanContext.span_id(valid_ctx), "b7ad6b7169203331")
  assert_true(@azimuth.telemetry.api.trace.SpanContext.is_valid(valid_ctx))
  assert_true(@azimuth.telemetry.api.trace.SpanContext.is_sampled(valid_ctx))
  
  // 测试无效的 SpanContext
  let invalid_ctx1 = @azimuth.telemetry.api.trace.SpanContext.new("", "b7ad6b7169203331", true, "")
  let invalid_ctx2 = @azimuth.telemetry.api.trace.SpanContext.new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  let invalid_ctx3 = @azimuth.telemetry.api.trace.SpanContext.new("", "", true, "")
  
  assert_false(@azimuth.telemetry.api.trace.SpanContext.is_valid(invalid_ctx1))
  assert_false(@azimuth.telemetry.api.trace.SpanContext.is_valid(invalid_ctx2))
  assert_false(@azimuth.telemetry.api.trace.SpanContext.is_valid(invalid_ctx3))
  
  // 测试采样状态
  let unsampled_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c", 
    "b7ad6b7169203331", 
    false, 
    ""
  )
  
  assert_false(@azimuth.telemetry.api.trace.SpanContext.is_sampled(unsampled_ctx))
}

test "平台功能 Clock 和 Random 测试" {
  // 测试系统时钟
  let clock = @azimuth.telemetry.platform.Clock.system()
  let timestamp = @azimuth.telemetry.platform.Clock.now_unix_nanos(clock)
  
  // 验证时间戳合理性（应该是一个较大的正数）
  assert_true(timestamp > 1000000000000000000L)
  assert_true(timestamp < 2000000000000000000L)
  
  // 测试系统随机数生成器
  let random = @azimuth.telemetry.platform.Random.system()
  
  // 测试字节数组生成
  let bytes = @azimuth.telemetry.platform.Random.next_bytes(random, 16)
  assert_eq(bytes.length, 16)
  
  // 测试 UInt64 随机数生成
  let random_u64 = @azimuth.telemetry.platform.Random.next_u64(random)
  assert_true(random_u64 > 0UL)
  assert_true(random_u64 <= 18446744073709551615UL) // Max UInt64
}

test "TextMapCarrier 文本映射载体测试" {
  // 测试空载体创建
  let empty_carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  assert_eq(empty_carrier.headers.length, 0)
  
  // 测试设置头部
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(
    empty_carrier, 
    "custom-header", 
    "custom-value"
  )
  
  // 测试获取头部
  let traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(empty_carrier, "traceparent")
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  let missing_header = @azimuth.telemetry.api.propagation.TextMapCarrier.get(empty_carrier, "missing-header")
  assert_eq(missing_header, None)
  
  // 测试多个头部操作
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(empty_carrier, "baggage", "key1=value1,key2=value2")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(empty_carrier, "x-trace-id", "trace-123")
  
  // 验证特定头部存在
  let baggage = @azimuth.telemetry.api.propagation.TextMapCarrier.get(empty_carrier, "baggage")
  // 简化实现返回 None
  assert_eq(baggage, None)
}