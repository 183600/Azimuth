// 错误处理测试用例

test "span_error_recording" {
  // 测试Span错误记录
  
  let tracer = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("error-test-tracer")
  let (ctx, span) = tracer.start_span(
    azimuth::telemetry::api::context::Context::empty(),
    "error.prone.operation",
    azimuth::telemetry::api::trace::SpanKind::Server
  )
  
  // 记录异常
  span.record_exception(
    exception_type="DatabaseConnectionError",
    exception_message="Failed to connect to database server",
    exception_stacktrace="at DatabaseService.connect(DatabaseService.java:45)\n at OrderService.process(OrderService.java:123)",
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    attributes=azimuth::telemetry::api::common::Attributes::empty()
      .with("database.host", azimuth::telemetry::api::common::AttributeValue::String("db.example.com"))
      .with("database.port", azimuth::telemetry::api::common::AttributeValue::Int64(5432))
      .with("retry.count", azimuth::telemetry::api::common::AttributeValue::Int64(3))
  )
  
  // 设置错误状态
  span.set_status(
    azimuth::telemetry::api::trace::StatusCode::Error,
    "Database operation failed after 3 retries"
  )
  
  // 添加错误事件
  span.add_event(
    "connection.attempt.failed",
    azimuth::telemetry::api::common::Attributes::empty()
      .with("attempt", azimuth::telemetry::api::common::AttributeValue::Int64(1))
      .with("error.code", azimuth::telemetry::api::common::AttributeValue::String("ECONNREFUSED"))
  )
  
  span.add_event(
    "connection.attempt.failed",
    azimuth::telemetry::api::common::Attributes::empty()
      .with("attempt", azimuth::telemetry::api::common::AttributeValue::Int64(2))
      .with("error.code", azimuth::telemetry::api::common::AttributeValue::String("ETIMEDOUT"))
  )
  
  span.add_event(
    "connection.attempt.failed",
    azimuth::telemetry::api::common::Attributes::empty()
      .with("attempt", azimuth::telemetry::api::common::AttributeValue::Int64(3))
      .with("error.code", azimuth::telemetry::api::common::AttributeValue::String("ECONNRESET"))
  )
  
  // 结束Span
  span.end()
  
  // 验证Span状态
  let span_context = span.span_context()
  assert_eq(span_context.has_error(), true)
  
  let readable_span = span.to_readable()
  assert_eq(readable_span.status.code, azimuth::telemetry::api::trace::StatusCode::Error)
  assert_eq(readable_span.status.description.unwrap(), "Database operation failed after 3 retries")
  assert_eq(readable_span.events.length(), 3)
  assert_eq(readable_span.exceptions.length(), 1)
  assert_eq(readable_span.exceptions[0].type, "DatabaseConnectionError")
}

test "metric_error_handling" {
  // 测试指标错误处理
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("error-test-meter")
  
  // 创建错误计数器
  let error_counter = meter.create_counter("errors.total", "Total number of errors", "errors")
  
  // 记录不同类型的错误
  let db_error_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("error.type", azimuth::telemetry::api::common::AttributeValue::String("database"))
    .with("error.code", azimuth::telemetry::api::common::AttributeValue::String("CONNECTION_FAILED"))
    .with("service", azimuth::telemetry::api::common::AttributeValue::String("order-service"))
  
  error_counter.add(1, db_error_attrs)
  
  let network_error_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("error.type", azimuth::telemetry::api::common::AttributeValue::String("network"))
    .with("error.code", azimuth::telemetry::api::common::AttributeValue::String("TIMEOUT"))
    .with("service", azimuth::telemetry::api::common::AttributeValue::String("user-service"))
  
  error_counter.add(1, network_error_attrs)
  
  let validation_error_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("error.type", azimuth::telemetry::api::common::AttributeValue::String("validation"))
    .with("error.code", azimuth::telemetry::api::common::AttributeValue::String("INVALID_INPUT"))
    .with("service", azimuth::telemetry::api::common::AttributeValue::String("api-gateway"))
  
  error_counter.add(3, validation_error_attrs)
  
  // 收集错误指标
  let db_measurement = error_counter.collect(db_error_attrs)
  let network_measurement = error_counter.collect(network_error_attrs)
  let validation_measurement = error_counter.collect(validation_error_attrs)
  
  // 验证错误计数
  assert_eq(db_measurement.value, 1)
  assert_eq(network_measurement.value, 1)
  assert_eq(validation_measurement.value, 3)
  
  // 测试错误率直方图
  let error_rate_histogram = meter.create_histogram("error.rate", "Error rate percentage", "%")
  
  let service_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("payment-service"))
  
  error_rate_histogram.record(5.2, service_attrs)  // 5.2% error rate
  error_rate_histogram.record(12.8, service_attrs) // 12.8% error rate
  error_rate_histogram.record(3.1, service_attrs)  // 3.1% error rate
  
  let rate_measurement = error_rate_histogram.collect(service_attrs)
  assert_eq(rate_measurement.count, 3)
  assert_eq(rate_measurement.mean, 7.033333333333333)
}

test "log_error_handling" {
  // 测试日志错误处理
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("error-test-logger")
  
  // 创建错误日志记录
  let critical_error_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("error.severity", azimuth::telemetry::api::common::AttributeValue::String("critical"))
    .with("error.category", azimuth::telemetry::api::common::AttributeValue::String("system"))
    .with("error.code", azimuth::telemetry::api::common::AttributeValue::String("OUT_OF_MEMORY"))
    .with("process.id", azimuth::telemetry::api::common::AttributeValue::Int64(12345))
    .with("node.name", azimuth::telemetry::api::common::AttributeValue::String("worker-node-03"))
    .with("memory.usage", azimuth::telemetry::api::common::AttributeValue::Int64(8589934592))  // 8GB
  
  let critical_log = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Fatal,
    severity_text="FATAL",
    body="System out of memory, process will terminate",
    attributes=critical_error_attrs,
    trace_id=None,
    span_id=None,
    trace_flags=None
  )
  
  logger.emit(critical_log)
  
  // 验证错误日志
  assert_eq(critical_log.severity_number, azimuth::telemetry::api::logs::SeverityNumber::Fatal)
  assert_eq(critical_log.severity_text, "FATAL")
  assert_eq(critical_log.attributes.get("error.severity").unwrap().to_string(), "critical")
  assert_eq(critical_log.attributes.get("error.code").unwrap().to_string(), "OUT_OF_MEMORY")
  
  // 创建带异常堆栈的日志
  let exception_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("exception.type", azimuth::telemetry::api::common::AttributeValue::String("NullPointerException"))
    .with("exception.message", azimuth::telemetry::api::common::AttributeValue::String("Cannot invoke method on null object"))
    .with("exception.stacktrace", azimuth::telemetry::api::common::AttributeValue::String(
      "java.lang.NullPointerException: Cannot invoke method on null object\n" +
      "at com.example.UserService.getUser(UserService.java:67)\n" +
      "at com.example.UserController.handleRequest(UserController.java:34)\n" +
      "at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)"
    ))
    .with("class.name", azimuth::telemetry::api::common::AttributeValue::String("UserService"))
    .with("method.name", azimuth::telemetry::api::common::AttributeValue::String("getUser"))
    .with("line.number", azimuth::telemetry::api::common::AttributeValue::Int64(67))
  
  let exception_log = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Error,
    severity_text="ERROR",
    body="Unhandled exception in user service",
    attributes=exception_attrs,
    trace_id=Some("4bf92f3577b34da6a3ce929d0e0e4736"),
    span_id=Some("00f067aa0ba902b7"),
    trace_flags=Some(1)
  )
  
  logger.emit(exception_log)
  
  // 验证异常日志
  assert_eq(exception_log.severity_number, azimuth::telemetry::api::logs::SeverityNumber::Error)
  assert_eq(exception_log.trace_id.unwrap(), "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(exception_log.span_id.unwrap(), "00f067aa0ba902b7")
  assert_eq(exception_log.attributes.get("exception.type").unwrap().to_string(), "NullPointerException")
}

test "context_propagation_error_handling" {
  // 测试上下文传播错误处理
  
  // 创建损坏的traceparent头部
  let corrupted_carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  corrupted_carrier.set("traceparent", "invalid-traceparent-format")
  
  let propagator = azimuth::telemetry::api::propagation::W3CTraceContextPropagator::new()
  
  // 尝试提取损坏的上下文
  let ctx = propagator.extract(corrupted_carrier)
  
  // 验证错误处理
  let span_context = ctx.get(azimuth::telemetry::api::context::create_key("span_context"))
  assert_eq(span_context.is_none(), true)
  
  // 测试部分缺失的traceparent
  let partial_carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  partial_carrier.set("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736")  // 缺少span_id和flags
  
  let partial_ctx = propagator.extract(partial_carrier)
  let partial_span_context = partial_ctx.get(azimuth::telemetry::api::context::create_key("span_context"))
  assert_eq(partial_span_context.is_none(), true)
  
  // 测试不支持的版本
  let unsupported_carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  unsupported_carrier.set("traceparent", "ff-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
  
  let unsupported_ctx = propagator.extract(unsupported_carrier)
  let unsupported_span_context = unsupported_ctx.get(azimuth::telemetry::api::context::create_key("span_context"))
  assert_eq(unsupported_span_context.is_none(), true)
}

test "resource_error_handling" {
  // 测试资源错误处理
  
  // 测试空资源名称
  let empty_resource = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String(""))
    .build()
  
  // 验证空资源名称的处理
  assert_eq(empty_resource.attributes.get("service.name").unwrap().to_string(), "")
  
  // 测试无效属性值
  let invalid_resource = azimuth::telemetry::api::common::Resource::builder()
    .with("invalid.attribute", azimuth::telemetry::api::common::AttributeValue::String(""))
    .with("null.attribute", azimuth::telemetry::api::common::AttributeValue::String("null"))
    .build()
  
  // 验证无效属性的处理
  assert_eq(invalid_resource.attributes.get("invalid.attribute").unwrap().to_string(), "")
  assert_eq(invalid_resource.attributes.get("null.attribute").unwrap().to_string(), "null")
  
  // 测试资源合并时的错误处理
  let resource1 = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("service-a"))
    .build()
  
  let resource2 = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String(""))  // 空值覆盖
    .build()
  
  let merged_resource = resource1.merge(resource2)
  
  // 验证合并后的空值处理
  assert_eq(merged_resource.attributes.get("service.name").unwrap().to_string(), "")
}

test "exporter_error_handling" {
  // 测试导出器错误处理
  
  // 创建模拟的失败导出器
  let failing_exporter = azimuth::telemetry::sdk::trace::FailingSpanExporter::new(
    failure_rate=1.0,  // 100%失败率
    error_type="ConnectionTimeout"
  )
  
  let tracer = azimuth::telemetry::sdk::trace::SdkTracerProvider::builder()
    .with_span_processor(azimuth::telemetry::sdk::trace::BatchSpanProcessor::new(
      exporter=failing_exporter,
      max_queue_size=100,
      scheduled_delay_millis=1000,
      max_export_batch_size=10
    ))
    .build()
    .get_tracer("error-test-tracer")
  
  // 创建多个Span
  for i = 0; i < 5; i = i + 1 {
    let (_, span) = tracer.start_span(
      azimuth::telemetry::api::context::Context::empty(),
      "failing.span." + i.to_string(),
      azimuth::telemetry::api::trace::SpanKind::Server
    )
    span.end()
  }
  
  // 等待导出尝试
  azimuth::telemetry::sdk::platform::time::sleep(2000)  // 2秒
  
  // 验证导出错误处理
  let export_stats = failing_exporter.get_export_stats()
  assert_eq(export_stats.total_attempts, 5)
  assert_eq(export_stats.failed_attempts, 5)
  assert_eq(export_stats.successful_attempts, 0)
  
  // 测试重试机制
  let retrying_exporter = azimuth::telemetry::sdk::trace::RetryingSpanExporter::new(
    base_exporter=failing_exporter,
    max_retries=3,
    base_delay_ms=100
  )
  
  let retry_stats = retrying_exporter.get_retry_stats()
  assert_eq(retry_stats.total_retries >= 5, true)  // 每个失败至少重试一次
}

test "attribute_validation_error_handling" {
  // 测试属性验证错误处理
  
  // 测试过长的属性键
  let very_long_key = "this.is.a.very.long.key.name.that.exceeds.the.normal.limits.and.might.cause.issues.in.some.systems.that.have.strict.validation.rules.for.attribute.keys"
  let long_key_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with(very_long_key, azimuth::telemetry::api::common::AttributeValue::String("value"))
  
  // 验证长键的处理
  assert_eq(long_key_attrs.size(), 1)
  assert_eq(long_key_attrs.get(very_long_key).unwrap().to_string(), "value")
  
  // 测试过长的属性值
  let very_long_value = "this.is.a.very.long.value.that.contains.a.lot.of.information.and.might.exceed.the.normal.limits.for.attribute.values.in.some.systems.that.have.strict.validation.rules".repeat(10)
  let long_value_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("long.value", azimuth::telemetry::api::common::AttributeValue::String(very_long_value))
  
  // 验证长值的处理
  assert_eq(long_value_attrs.size(), 1)
  assert_eq(long_value_attrs.get("long.value").unwrap().to_string(), very_long_value)
  
  // 测试特殊字符属性键
  let special_chars_key = "key.with.special@chars#and$symbols%and&chars*and+chars"
  let special_chars_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with(special_chars_key, azimuth::telemetry::api::common::AttributeValue::String("special_value"))
  
  // 验证特殊字符键的处理
  assert_eq(special_chars_attrs.size(), 1)
  assert_eq(special_chars_attrs.get(special_chars_key).unwrap().to_string(), "special_value")
  
  // 测试Unicode属性键和值
  let unicode_key = "键名.包含.中文.和.🚀.emoji"
  let unicode_value = "值.包含.中文.和.🎉.emoji.以及.其他.特殊.字符"
  let unicode_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with(unicode_key, azimuth::telemetry::api::common::AttributeValue::String(unicode_value))
  
  // 验证Unicode的处理
  assert_eq(unicode_attrs.size(), 1)
  assert_eq(unicode_attrs.get(unicode_key).unwrap().to_string(), unicode_value)
}

test "configuration_error_handling" {
  // 测试配置错误处理
  
  // 测试无效的采样率配置
  let invalid_sampler_configs = [
    (-0.1, "Negative sampling rate"),
    (1.5, "Sampling rate greater than 1.0"),
    (0.0, "Zero sampling rate"),
    (1.0, "Maximum valid sampling rate")
  ]
  
  for (rate, description) in invalid_sampler_configs {
    if rate >= 0.0 and rate <= 1.0 {
      // 有效的采样率
      let sampler = azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(rate)
      assert_eq(sampler.get_sampling_rate(), rate)
    } else {
      // 无效的采样率应该被处理
      let sampler = azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(rate)
      // 验证错误处理（可能调整为有效值或使用默认值）
      assert_eq(sampler.get_sampling_rate() >= 0.0 and sampler.get_sampling_rate() <= 1.0, true)
    }
  }
  
  // 测试无效的导出器配置
  let invalid_exporter_configs = [
    ("", "Empty endpoint"),
    ("invalid-url", "Invalid URL format"),
    ("ftp://example.com", "Unsupported protocol"),
    ("http://", "Missing host")
  ]
  
  for (endpoint, description) in invalid_exporter_configs {
    let exporter = azimuth::telemetry::sdk::exporter::OtlpHttpExporter::builder()
      .with_endpoint(endpoint)
      .build()
    
    // 验证无效配置的处理
    assert_eq(exporter.is_valid(), false)
  }
  
  // 测试有效的导出器配置
  let valid_exporter = azimuth::telemetry::sdk::exporter::OtlpHttpExporter::builder()
    .with_endpoint("https://otel-collector.example.com:4318")
    .with_headers([("Authorization", "Bearer token123")])
    .with_timeout(5000)
    .build()
  
  assert_eq(valid_exporter.is_valid(), true)
}

test "memory_and_resource_error_handling" {
  // 测试内存和资源错误处理
  
  // 测试大量属性创建的内存处理
  let mut large_attrs = azimuth::telemetry::api::common::Attributes::empty()
  
  // 创建大量属性
  for i = 0; i < 1000; i = i + 1 {
    large_attrs = large_attrs.with(
      "attr." + i.to_string(),
      azimuth::telemetry::api::common::AttributeValue::String("value." + i.to_string())
    )
  }
  
  // 验证大量属性的处理
  assert_eq(large_attrs.size(), 1000)
  
  // 测试大量Span创建的内存处理
  let tracer = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("memory-test-tracer")
  let spans = []
  
  // 创建大量Span
  for i = 0; i < 100; i = i + 1 {
    let (_, span) = tracer.start_span(
      azimuth::telemetry::api::context::Context::empty(),
      "memory.test.span." + i.to_string(),
      azimuth::telemetry::api::trace::SpanKind::Internal
    )
    spans.push(span)
  }
  
  // 结束所有Span
  for span in spans {
    span.end()
  }
  
  // 验证大量Span的处理
  assert_eq(spans.length(), 100)
  
  // 测试资源清理
  tracer.force_flush()
  tracer.shutdown()
  
  // 验证资源清理
  assert_eq(tracer.is_shutdown(), true)
}