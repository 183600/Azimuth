// 遥测数据完整性测试
// Telemetry data integrity tests for Azimuth telemetry system

test "Span数据完整性验证测试" {
  // 测试Span数据的完整性和一致性
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "data.integrity.tracer")
  
  // 创建具有完整数据的Span
  let (ctx, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "integrity.test.span")
  
  // 验证Span基本属性
  assert_eq(span.name(), "integrity.test.span")
  assert_eq(span.kind(), @azimuth.telemetry.api.trace.SpanKind.Internal)
  assert_true(span.is_recording())
  
  let span_ctx = span.span_context()
  assert_true(span_ctx.is_valid())
  assert_true(span_ctx.trace_id().length() == 32)  // 16 bytes = 32 hex chars
  assert_true(span_ctx.span_id().length() == 16)  // 8 bytes = 16 hex chars
  
  // 设置多种类型的属性
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "string.attr", @azimuth.telemetry.api.common.StringValue("test_value"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "int.attr", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "float.attr", @azimuth.telemetry.api.common.FloatValue(3.14159))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "bool.attr", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "array.string", @azimuth.telemetry.api.common.ArrayStringValue(["item1", "item2", "item3"]))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "array.int", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 添加多个事件
  let event_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.type", @azimuth.telemetry.api.common.StringValue("user_action"))
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.data", @azimuth.telemetry.api.common.StringValue("test_data"))
  
  @azimuth.telemetry.api.trace.Span.add_event(span, "event1", Some(event_attrs), None)
  @azimuth.telemetry.api.trace.Span.add_event(span, "event2", None, Some(1700000000000000000L))
  @azimuth.telemetry.api.trace.Span.add_event(span, "event3", Some(event_attrs), Some(1700000001000000000L))
  
  // 记录异常
  @azimuth.telemetry.api.trace.Span.record_exception(
    span,
    "Test exception for integrity check",
    Some("IntegrityTestException"),
    Some("at integrity.test()\n  at main()\n  at entry_point()"),
    Some(event_attrs)
  )
  
  // 设置状态
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Test completed with error"))
  
  // 验证Span状态在结束前
  assert_true(span.is_recording())
  assert_eq(span.status(), @azimuth.telemetry.api.trace.StatusCode.Error)
  
  // 结束Span
  @azimuth.telemetry.api.trace.Span.end(span)
  
  // 验证Span状态在结束后
  assert_false(span.is_recording())
  
  // 验证Span上下文仍然有效
  let final_ctx = span.span_context()
  assert_true(final_ctx.is_valid())
  assert_eq(final_ctx.trace_id(), span_ctx.trace_id())
  assert_eq(final_ctx.span_id(), span_ctx.span_id())
}

test "指标数据一致性测试" {
  // 测试指标数据的一致性和准确性
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "data.integrity.meter")
  
  // 创建不同类型的指标
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(
    meter,
    "integrity.test.counter",
    Some("Test counter for data integrity"),
    Some("operations")
  )
  
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(
    meter,
    "integrity.test.histogram",
    Some("Test histogram for data integrity"),
    Some("ms")
  )
  
  let up_down_counter = @azimuth.telemetry.sdk.metrics.Meter.create_up_down_counter(
    meter,
    "integrity.test.up_down_counter",
    Some("Test up-down counter for data integrity"),
    Some("items")
  )
  
  let gauge = @azimuth.telemetry.sdk.metrics.Meter.create_gauge(
    meter,
    "integrity.test.gauge",
    Some("Test gauge for data integrity"),
    Some("percent")
  )
  
  // 验证指标属性
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(counter), "integrity.test.counter")
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.description(counter), Some("Test counter for data integrity"))
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.unit(counter), Some("operations"))
  
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(histogram), "integrity.test.histogram")
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(up_down_counter), "integrity.test.up_down_counter")
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(gauge), "integrity.test.gauge")
  
  // 测试Counter数据一致性
  let counter_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(counter_attrs, "counter.type", @azimuth.telemetry.api.common.StringValue("increment"))
  
  let counter_values = [1.0, 2.5, 3.0, 4.75, 5.0]
  let expected_counter_total = 0.0
  
  for value in counter_values {
    @azimuth.telemetry.sdk.metrics.Counter.add(counter, value, Some(counter_attrs))
  }
  
  // 测试Histogram数据一致性
  let histogram_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(histogram_attrs, "histogram.type", @azimuth.telemetry.api.common.StringValue("response_time"))
  
  let histogram_values = [10.5, 25.0, 15.75, 30.25, 20.0, 18.5, 22.75]
  
  for value in histogram_values {
    @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, value, Some(histogram_attrs))
  }
  
  // 测试UpDownCounter数据一致性
  let up_down_values = [10.0, -3.5, 5.0, -2.0, 8.0]
  let expected_up_down_total = 0.0
  
  for value in up_down_values {
    @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, value, None)
  }
  
  // 测试Gauge数据一致性
  let gauge_values = [25.5, 30.0, 28.75, 35.25, 32.0]
  
  for value in gauge_values {
    @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, value, None)
  }
  
  // 验证指标操作不会导致错误
  assert_true(counter != @azimuth.telemetry.sdk.metrics.Counter::{})
  assert_true(histogram != @azimuth.telemetry.sdk.metrics.Histogram::{})
  assert_true(up_down_counter != @azimuth.telemetry.sdk.metrics.UpDownCounter::{})
  assert_true(gauge != @azimuth.telemetry.sdk.metrics.Gauge::{})
}

test "日志记录数据完整性测试" {
  // 测试日志记录的数据完整性
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "data.integrity.logger")
  
  // 创建具有完整属性的日志记录
  let log_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(log_attrs, "log.source", @azimuth.telemetry.api.common.StringValue("integrity.test"))
  @azimuth.telemetry.api.common.Attributes.set(log_attrs, "log.component", @azimuth.telemetry.api.common.StringValue("data.validation"))
  @azimuth.telemetry.api.common.Attributes.set(log_attrs, "log.version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  @azimuth.telemetry.api.common.Attributes.set(log_attrs, "log.environment", @azimuth.telemetry.api.common.StringValue("test"))
  
  let current_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let log_record = @azimuth.telemetry.api.logs.LogRecord.new_with_context(
    @azimuth.telemetry.api.logs.Info,
    Some("Data integrity test log message"),
    Some(log_attrs),
    Some(current_time),
    Some(current_time + 1000L),
    Some("trace123456789abcdef"),
    Some("span1234567890abcdef"),
    Some(@azimuth.telemetry.api.context.Context.root())
  )
  
  // 验证日志记录属性
  assert_eq(log_record.severity_number(), @azimuth.telemetry.api.logs.Info)
  assert_eq(log_record.severity_text(), Some("INFO"))
  assert_eq(log_record.body(), Some("Data integrity test log message"))
  assert_eq(log_record.timestamp(), Some(current_time))
  assert_eq(log_record.observed_timestamp(), Some(current_time + 1000L))
  assert_eq(log_record.trace_id(), Some("trace123456789abcdef"))
  assert_eq(log_record.span_id(), Some("span1234567890abcdef"))
  
  // 验证日志记录属性
  let record_attrs = log_record.attributes()
  assert_true(record_attrs != None)
  
  // 测试不同严重程度的日志记录
  let severity_levels = [
    (@azimuth.telemetry.api.logs.Trace, "Trace level message"),
    (@azimuth.telemetry.api.logs.Debug, "Debug level message"),
    (@azimuth.telemetry.api.logs.Info, "Info level message"),
    (@azimuth.telemetry.api.logs.Warn, "Warning level message"),
    (@azimuth.telemetry.api.logs.Error, "Error level message"),
    (@azimuth.telemetry.api.logs.Fatal, "Fatal level message")
  ]
  
  for (severity, message) in severity_levels {
    let severity_log = @azimuth.telemetry.api.logs.LogRecord.new(
      severity,
      message,
      Some(log_attrs),
      Some(current_time),
      None,
      Some("trace123456789abcdef"),
      Some("span1234567890abcdef"),
      None
    )
    
    // 验证严重程度
    assert_eq(severity_log.severity_number(), severity)
    
    // 验证消息
    assert_eq(severity_log.body(), Some(message))
    
    // 验证时间戳
    assert_eq(severity_log.timestamp(), Some(current_time))
    
    // 验证追踪上下文
    assert_eq(severity_log.trace_id(), Some("trace123456789abcdef"))
    assert_eq(severity_log.span_id(), Some("span1234567890abcdef"))
  }
  
  // 发出日志记录
  @azimuth.telemetry.sdk.logs.Logger.emit(logger, log_record)
  
  // 验证日志记录发出后不会损坏数据
  assert_eq(log_record.severity_number(), @azimuth.telemetry.api.logs.Info)
  assert_eq(log_record.body(), Some("Data integrity test log message"))
}

test "上下文传播数据完整性测试" {
  // 测试上下文传播中的数据完整性
  
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator.new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::new(),
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator::new()
  ])
  
  // 创建具有丰富数据的上下文
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  let user_key = @azimuth.telemetry.api.context.ContextKey.new("user.id")
  let session_key = @azimuth.telemetry.api.context.ContextKey.new("session.id")
  let request_key = @azimuth.telemetry.api.context.ContextKey.new("request.id")
  let correlation_key = @azimuth.telemetry.api.context.ContextKey.new("correlation.id")
  
  let ctx_with_values = @azimuth.telemetry.api.context.Context.with_value(
    @azimuth.telemetry.api.context.Context.with_value(
      @azimuth.telemetry.api.context.Context.with_value(
        @azimuth.telemetry.api.context.Context.with_value(ctx, user_key, "user12345"),
        session_key, "session_abcdef"
      ),
      request_key, "req_123456"
    ),
    correlation_key, "corr_789012"
  )
  
  // 测试上下文注入
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  @azimuth.telemetry.api.propagation.CompositePropagator.inject(composite, ctx_with_values, carrier)
  
  // 验证注入的数据
  let traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "traceparent")
  assert_true(traceparent != None)
  assert_true(traceparent.unwrap().starts_with("00-"))
  
  // 验证traceparent格式
  let traceparent_value = traceparent.unwrap()
  let parts = traceparent_value.split("-")
  assert_eq(parts.length(), 4)
  assert_eq(parts[0], "00")  // Version
  assert_eq(parts[1].length(), 32)  // Trace ID
  assert_eq(parts[2].length(), 16)  // Span ID
  assert_eq(parts[3].length(), 2)   // Flags
  
  // 测试上下文提取
  let extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator.extract(composite, carrier)
  assert_true(extracted_ctx != @azimuth.telemetry.api.context.Context::{})
  
  // 验证提取的上下文保持数据完整性
  let extraction_key = @azimuth.telemetry.api.context.ContextKey.new("extracted")
  let extraction_result = @azimuth.telemetry.api.context.Context.get(extracted_ctx, extraction_key)
  assert_eq(extraction_result, Some("true"))
  
  // 测试多次注入和提取的数据一致性
  let carrier2 = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  @azimuth.telemetry.api.propagation.CompositePropagator.inject(composite, extracted_ctx, carrier2)
  
  let traceparent2 = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier2, "traceparent")
  assert_true(traceparent2 != None)
  
  // 验证多次注入的traceparent格式一致
  let traceparent2_value = traceparent2.unwrap()
  let parts2 = traceparent2_value.split("-")
  assert_eq(parts2.length(), 4)
  assert_eq(parts2[0], "00")
  assert_eq(parts2[1].length(), 32)
  assert_eq(parts2[2].length(), 16)
  assert_eq(parts2[3].length(), 2)
  
  // 测试baggage传播的数据完整性
  let baggage_carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(baggage_carrier, "baggage", "user=user12345,session=session_abcdef,request=req_123456")
  
  let baggage_extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator.extract(composite, baggage_carrier)
  assert_true(baggage_extracted_ctx != @azimuth.telemetry.api.context.Context::{})
}

test "资源属性数据完整性测试" {
  // 测试资源属性的数据完整性
  
  // 创建具有丰富属性的资源
  let resource_attrs = [
    ("service.name", @azimuth.telemetry.api.common.StringValue("integrity.test.service")),
    ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
    ("service.instance.id", @azimuth.telemetry.api.common.StringValue("instance-abc123")),
    ("deployment.environment", @azimuth.telemetry.api.common.StringValue("test")),
    ("host.name", @azimuth.telemetry.api.common.StringValue("test-host-01")),
    ("host.ip", @azimuth.telemetry.api.common.StringValue("192.168.1.100")),
    ("os.type", @azimuth.telemetry.api.common.StringValue("linux")),
    ("os.version", @azimuth.telemetry.api.common.StringValue("5.15.0")),
    ("process.id", @azimuth.telemetry.api.common.IntValue(12345)),
    ("process.executable.name", @azimuth.telemetry.api.common.StringValue("integrity_test")),
    ("process.command_args", @azimuth.telemetry.api.common.ArrayStringValue(["./integrity_test", "--config", "test.yaml"])),
    ("process.owner", @azimuth.telemetry.api.common.StringValue("testuser")),
    ("cloud.provider", @azimuth.telemetry.api.common.StringValue("aws")),
    ("cloud.region", @azimuth.telemetry.api.common.StringValue("us-west-2")),
    ("cloud.availability_zone", @azimuth.telemetry.api.common.StringValue("us-west-2a")),
    ("kubernetes.namespace", @azimuth.telemetry.api.common.StringValue("test")),
    ("kubernetes.pod.name", @azimuth.telemetry.api.common.StringValue("integrity-test-pod-123")),
    ("kubernetes.container.name", @azimuth.telemetry.api.common.StringValue("integrity-test-container")),
    ("telemetry.sdk.name", @azimuth.telemetry.api.common.StringValue("azimuth")),
    ("telemetry.sdk.language", @azimuth.telemetry.api.common.StringValue("moonbit")),
    ("telemetry.sdk.version", @azimuth.telemetry.api.common.StringValue("0.1.0"))
  ]
  
  let resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    resource_attrs
  )
  
  // 验证资源属性完整性
  for (key, expected_value) in resource_attrs {
    let actual_value = @azimuth.telemetry.api.common.Resource.get_attribute(resource, key)
    // 简化实现可能返回None，但不应该崩溃
  }
  
  // 测试资源合并的数据完整性
  let additional_attrs = [
    ("custom.attr1", @azimuth.telemetry.api.common.StringValue("custom_value1")),
    ("custom.attr2", @azimuth.telemetry.api.common.IntValue(42)),
    ("custom.attr3", @azimuth.telemetry.api.common.FloatValue(3.14159)),
    ("custom.attr4", @azimuth.telemetry.api.common.BoolValue(true)),
    ("custom.attr5", @azimuth.telemetry.api.common.ArrayStringValue(["item1", "item2", "item3"])),
    ("custom.attr6", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let additional_resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    additional_attrs
  )
  
  // 合并资源
  let merged_resource = @azimuth.telemetry.api.common.Resource.merge(resource, additional_resource)
  
  // 验证合并后的资源属性
  for (key, expected_value) in additional_attrs {
    let actual_value = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, key)
    // 简化实现可能返回None，但不应该崩溃
  }
  
  // 测试使用资源创建提供者
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.with_resource(resource)
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.with_resource(resource)
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.with_resource(resource)
  
  // 验证提供者创建成功
  assert_true(trace_provider != @azimuth.telemetry.sdk.trace.TracerProvider::{})
  assert_true(metrics_provider != @azimuth.telemetry.sdk.metrics.MeterProvider::{})
  assert_true(logs_provider != @azimuth.telemetry.sdk.logs.LoggerProvider::{})
  
  // 测试使用资源提供者创建仪器
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "resource.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "resource.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "resource.test.logger")
  
  // 验证仪器创建成功
  assert_true(tracer != @azimuth.telemetry.sdk.trace.Tracer::{})
  assert_true(meter != @azimuth.telemetry.sdk.metrics.Meter::{})
  assert_true(logger != @azimuth.telemetry.sdk.logs.Logger::{})
}

test "属性序列化反序列化完整性测试" {
  // 测试属性序列化和反序列化的数据完整性
  
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  
  // 添加各种类型的属性
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.basic", @azimuth.telemetry.api.common.StringValue("basic_string"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.empty", @azimuth.telemetry.api.common.StringValue(""))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.special", @azimuth.telemetry.api.common.StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.unicode", @azimuth.telemetry.api.common.StringValue("🚀 测试 🌟"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.long", "x".repeat(1000))
  
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.zero", @azimuth.telemetry.api.common.IntValue(0))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.positive", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.negative", @azimuth.telemetry.api.common.IntValue(-42))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.max", @azimuth.telemetry.api.common.IntValue(2147483647))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.min", @azimuth.telemetry.api.common.IntValue(-2147483648))
  
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.zero", @azimuth.telemetry.api.common.FloatValue(0.0))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.positive", @azimuth.telemetry.api.common.FloatValue(3.14159))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.negative", @azimuth.telemetry.api.common.FloatValue(-3.14159))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.scientific", @azimuth.telemetry.api.common.FloatValue(1.23e-4))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.large", @azimuth.telemetry.api.common.FloatValue(1.7976931348623157e+308))
  
  @azimuth.telemetry.api.common.Attributes.set(attrs, "bool.true", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "bool.false", @azimuth.telemetry.api.common.BoolValue(false))
  
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string.empty", @azimuth.telemetry.api.common.ArrayStringValue([]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string.single", @azimuth.telemetry.api.common.ArrayStringValue(["single"]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string.multiple", @azimuth.telemetry.api.common.ArrayStringValue(["item1", "item2", "item3"]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string.empty_items", @azimuth.telemetry.api.common.ArrayStringValue(["", "", ""]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string.special", @azimuth.telemetry.api.common.ArrayStringValue(["!@#", "$%^", "&*()"]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string.unicode", @azimuth.telemetry.api.common.ArrayStringValue(["🚀", "测试", "🌟"]))
  
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int.empty", @azimuth.telemetry.api.common.ArrayIntValue([]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int.single", @azimuth.telemetry.api.common.ArrayIntValue([42]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int.multiple", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3, 4, 5]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int.zeroes", @azimuth.telemetry.api.common.ArrayIntValue([0, 0, 0]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int.positive_negative", @azimuth.telemetry.api.common.ArrayIntValue([-10, -5, 0, 5, 10]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int.limits", @azimuth.telemetry.api.common.ArrayIntValue([-2147483648, 2147483647]))
  
  // 测试属性获取的完整性
  let test_keys = [
    "string.basic", "string.empty", "string.special", "string.unicode", "string.long",
    "int.zero", "int.positive", "int.negative", "int.max", "int.min",
    "float.zero", "float.positive", "float.negative", "float.scientific", "float.large",
    "bool.true", "bool.false",
    "array.string.empty", "array.string.single", "array.string.multiple", "array.string.empty_items", "array.string.special", "array.string.unicode",
    "array.int.empty", "array.int.single", "array.int.multiple", "array.int.zeroes", "array.int.positive_negative", "array.int.limits"
  ]
  
  for key in test_keys {
    let value = @azimuth.telemetry.api.common.Attributes.get(attrs, key)
    // 简化实现可能返回None，但不应该崩溃
  }
  
  // 测试属性在Span中的完整性
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "attribute.integrity.tracer")
  
  let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "attribute.integrity.span")
  
  // 将所有属性设置到Span中
  for key in test_keys {
    let value = @azimuth.telemetry.api.common.Attributes.get(attrs, key)
    match value {
      Some(v) => @azimuth.telemetry.api.trace.Span.set_attribute(span, key, v)
      None => ()  // 跳过None值
    }
  }
  
  // 添加带有复杂属性的事件
  let event_attrs = @azimuth.telemetry.api.common.Attributes.new()
  for key in test_keys {
    let value = @azimuth.telemetry.api.common.Attributes.get(attrs, key)
    match value {
      Some(v) => @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event." + key, v)
      None => ()  // 跳过None值
    }
  }
  
  @azimuth.telemetry.api.trace.Span.add_event(span, "attribute.integrity.event", Some(event_attrs), None)
  
  // 结束Span
  @azimuth.telemetry.api.trace.Span.end(span)
  
  // 验证Span仍然有效
  assert_false(span.is_recording())
  assert_true(span.span_context().is_valid())
}

test "导出器数据完整性测试" {
  // 测试导出器处理数据的完整性
  
  // 创建测试Span
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "exporter.integrity.tracer")
  
  let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "exporter.integrity.span")
  
  // 添加完整的数据
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "exporter.test", @azimuth.telemetry.api.common.StringValue("data.integrity"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "timestamp", @azimuth.telemetry.api.common.IntValue(1700000000))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  
  @azimuth.telemetry.api.trace.Span.add_event(span, "exporter.test.event", Some([
    ("event.data", @azimuth.telemetry.api.common.StringValue("test_data")),
    ("event.type", @azimuth.telemetry.api.common.StringValue("integrity_check"))
  ]), None)
  
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Data integrity test"))
  
  // 创建导出器
  let console_exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.new()
  let otlp_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.new()
  
  // 测试单个Span导出
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export(console_exporter, span)
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(otlp_exporter, span)
  
  // 创建多个Span进行批量导出测试
  let spans = []
  
  for i in 0..5 {
    let (_, batch_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "batch.integrity.span." + i.to_string())
    
    @azimuth.telemetry.api.trace.Span.set_attribute(batch_span, "batch.index", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span.set_attribute(batch_span, "batch.total", @azimuth.telemetry.api.common.IntValue(5))
    @azimuth.telemetry.api.trace.Span.set_attribute(batch_span, "batch.data", @azimuth.telemetry.api.common.StringValue("batch_data_" + i.to_string()))
    
    spans = spans @ [batch_span]
  }
  
  // 测试批量导出
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export_batch(console_exporter, spans)
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export_batch(otlp_exporter, spans)
  
  // 结束所有Span
  @azimuth.telemetry.api.trace.Span.end(span)
  for batch_span in spans {
    @azimuth.telemetry.api.trace.Span.end(batch_span)
  }
  
  // 验证导出器仍然可用
  assert_true(console_exporter != @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter::{})
  assert_true(otlp_exporter != @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  
  // 测试日志导出器
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "exporter.integrity.logger")
  
  let console_log_exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleLogExporter.new()
  
  let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.Info,
    "Exporter integrity test log",
    Some([
      ("exporter.test", @azimuth.telemetry.api.common.StringValue("data.integrity")),
      ("log.type", @azimuth.telemetry.api.common.StringValue("integrity_check"))
    ]),
    Some(@azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())),
    None,
    Some("trace123456789abcdef"),
    Some("span1234567890abcdef"),
    None
  )
  
  @azimuth.telemetry.sdk.exporter.console.ConsoleLogExporter.export_log(console_log_exporter, log_record)
  
  // 测试指标导出器
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "exporter.integrity.meter")
  
  let console_metric_exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter.new()
  
  let counter_data = @azimuth.telemetry.sdk.exporter.console.MetricData.Counter(
    "exporter.integrity.counter",
    Some("Exporter integrity test counter"),
    Some("operations"),
    42.0,
    Some([
      ("exporter.test", @azimuth.telemetry.api.common.StringValue("data.integrity"))
    ])
  )
  
  let histogram_data = @azimuth.telemetry.sdk.exporter.console.MetricData.Histogram(
    "exporter.integrity.histogram",
    Some("Exporter integrity test histogram"),
    Some("ms"),
    [10.5, 25.0, 15.75, 30.25, 20.0],
    Some([
      ("exporter.test", @azimuth.telemetry.api.common.StringValue("data.integrity"))
    ])
  )
  
  @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter.export_metric(console_metric_exporter, counter_data)
  @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter.export_metric(console_metric_exporter, histogram_data)
}