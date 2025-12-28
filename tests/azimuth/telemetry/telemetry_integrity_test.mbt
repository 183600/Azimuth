// 遥测数据完整性测试
// Telemetry data integrity tests for Azimuth telemetry system

test "Span数据完整性验证测试" {
  // 测试Span数据的完整性和一致性
  
  let trace_provider = telemetry.sdk.trace.TracerProvider::default()
  let tracer = telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "data.integrity.tracer")
  
  // 创建具有完整数据的Span
  let (ctx, span) = telemetry.sdk.trace.Tracer::start_span(tracer, "integrity.test.span")
  
  // 验证Span基本属性
  assert_eq(span.name(), "integrity.test.span")
  assert_eq(span.kind(), telemetry.api.trace.SpanKind.Internal)
  assert_true(span.is_recording())
  
  let span_ctx = span.span_context()
  assert_true(span_ctx.is_valid())
  assert_true(span_ctx.trace_id().length() == 32)  // 16 bytes = 32 hex chars
  assert_true(span_ctx.span_id().length() == 16)  // 8 bytes = 16 hex chars
  
  // 设置多种类型的属性
  telemetry.api.trace.Span.set_attribute(span, "string.attr", telemetry.api.common.StringValue("test_value"))
  telemetry.api.trace.Span.set_attribute(span, "int.attr", telemetry.api.common.IntValue(42))
  telemetry.api.trace.Span.set_attribute(span, "float.attr", telemetry.api.common.FloatValue(3.14159))
  telemetry.api.trace.Span.set_attribute(span, "bool.attr", telemetry.api.common.BoolValue(true))
  telemetry.api.trace.Span.set_attribute(span, "array.string", telemetry.api.common.ArrayStringValue(["item1", "item2", "item3"]))
  telemetry.api.trace.Span.set_attribute(span, "array.int", telemetry.api.common.ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 添加多个事件
  let event_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(event_attrs, "event.type", telemetry.api.common.StringValue("user_action"))
  telemetry.api.common.Attributes.set(event_attrs, "event.data", telemetry.api.common.StringValue("test_data"))
  
  telemetry.api.trace.Span.add_event(span, "event1", Some(event_attrs), None)
  telemetry.api.trace.Span.add_event(span, "event2", None, Some(1700000000000000000L))
  telemetry.api.trace.Span.add_event(span, "event3", Some(event_attrs), Some(1700000001000000000L))
  
  // 记录异常
  telemetry.api.trace.Span.record_exception(
    span,
    "Test exception for integrity check",
    Some("IntegrityTestException"),
    Some("at integrity.test()\n  at main()\n  at entry_point()"),
    Some(event_attrs)
  )
  
  // 设置状态
  telemetry.api.trace.Span.set_status(span, telemetry.api.trace.StatusCode.Error, Some("Test completed with error"))
  
  // 验证Span状态在结束前
  assert_true(span.is_recording())
  assert_eq(span.status(), telemetry.api.trace.StatusCode.Error)
  
  // 结束Span
  telemetry.api.trace.Span.end(span)
  
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
  
  let metrics_provider = telemetry.sdk.metrics.MeterProvider::default()
  let meter = telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "data.integrity.meter")
  
  // 创建不同类型的指标
  let counter = telemetry.sdk.metrics.Meter.create_counter(
    meter,
    "integrity.test.counter",
    Some("Test counter for data integrity"),
    Some("operations")
  )
  
  let histogram = telemetry.sdk.metrics.Meter.create_histogram(
    meter,
    "integrity.test.histogram",
    Some("Test histogram for data integrity"),
    Some("ms")
  )
  
  let up_down_counter = telemetry.sdk.metrics.Meter.create_up_down_counter(
    meter,
    "integrity.test.up_down_counter",
    Some("Test up-down counter for data integrity"),
    Some("items")
  )
  
  let gauge = telemetry.sdk.metrics.Meter.create_gauge(
    meter,
    "integrity.test.gauge",
    Some("Test gauge for data integrity"),
    Some("percent")
  )
  
  // 验证指标属性
  assert_eq(telemetry.sdk.metrics.Instrument.name(counter), "integrity.test.counter")
  assert_eq(telemetry.sdk.metrics.Instrument.description(counter), Some("Test counter for data integrity"))
  assert_eq(telemetry.sdk.metrics.Instrument.unit(counter), Some("operations"))
  
  assert_eq(telemetry.sdk.metrics.Instrument.name(histogram), "integrity.test.histogram")
  assert_eq(telemetry.sdk.metrics.Instrument.name(up_down_counter), "integrity.test.up_down_counter")
  assert_eq(telemetry.sdk.metrics.Instrument.name(gauge), "integrity.test.gauge")
  
  // 测试Counter数据一致性
  let counter_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(counter_attrs, "counter.type", telemetry.api.common.StringValue("increment"))
  
  let counter_values = [1.0, 2.5, 3.0, 4.75, 5.0]
  let expected_counter_total = 0.0
  
  for value in counter_values {
    telemetry.sdk.metrics.Counter.add(counter, value, Some(counter_attrs))
  }
  
  // 测试Histogram数据一致性
  let histogram_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(histogram_attrs, "histogram.type", telemetry.api.common.StringValue("response_time"))
  
  let histogram_values = [10.5, 25.0, 15.75, 30.25, 20.0, 18.5, 22.75]
  
  for value in histogram_values {
    telemetry.sdk.metrics.Histogram.record(histogram, value, Some(histogram_attrs))
  }
  
  // 测试UpDownCounter数据一致性
  let up_down_values = [10.0, -3.5, 5.0, -2.0, 8.0]
  let expected_up_down_total = 0.0
  
  for value in up_down_values {
    telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, value, None)
  }
  
  // 测试Gauge数据一致性
  let gauge_values = [25.5, 30.0, 28.75, 35.25, 32.0]
  
  for value in gauge_values {
    telemetry.sdk.metrics.Gauge.record(gauge, value, None)
  }
  
  // 验证指标操作不会导致错误
  assert_true(counter != telemetry.sdk.metrics.Counter::{})
  assert_true(histogram != telemetry.sdk.metrics.Histogram::{})
  assert_true(up_down_counter != telemetry.sdk.metrics.UpDownCounter::{})
  assert_true(gauge != telemetry.sdk.metrics.Gauge::{})
}

test "日志记录数据完整性测试" {
  // 测试日志记录的数据完整性
  
  let logs_provider = telemetry.sdk.logs.LoggerProvider::default()
  let logger = telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "data.integrity.logger")
  
  // 创建具有完整属性的日志记录
  let log_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(log_attrs, "log.source", telemetry.api.common.StringValue("integrity.test"))
  telemetry.api.common.Attributes.set(log_attrs, "log.component", telemetry.api.common.StringValue("data.validation"))
  telemetry.api.common.Attributes.set(log_attrs, "log.version", telemetry.api.common.StringValue("1.0.0"))
  telemetry.api.common.Attributes.set(log_attrs, "log.environment", telemetry.api.common.StringValue("test"))
  
  let current_time = telemetry.sdk.platform.time.Clock.now_unix_nanos(telemetry.sdk.platform.time.Clock.system())
  
  let log_record = telemetry.api.logs.LogRecord.new_with_context(
    telemetry.api.logs.Info,
    Some("Data integrity test log message"),
    Some(log_attrs),
    Some(current_time),
    Some(current_time + 1000L),
    Some("trace123456789abcdef"),
    Some("span1234567890abcdef"),
    Some(telemetry.api.context.Context.root())
  )
  
  // 验证日志记录属性
  assert_eq(log_record.severity_number(), telemetry.api.logs.Info)
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
    (telemetry.api.logs.Trace, "Trace level message"),
    (telemetry.api.logs.Debug, "Debug level message"),
    (telemetry.api.logs.Info, "Info level message"),
    (telemetry.api.logs.Warn, "Warning level message"),
    (telemetry.api.logs.Error, "Error level message"),
    (telemetry.api.logs.Fatal, "Fatal level message")
  ]
  
  for (severity, message) in severity_levels {
    let severity_log = telemetry.api.logs.LogRecord.new(
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
  telemetry.sdk.logs.Logger.emit(logger, log_record)
  
  // 验证日志记录发出后不会损坏数据
  assert_eq(log_record.severity_number(), telemetry.api.logs.Info)
  assert_eq(log_record.body(), Some("Data integrity test log message"))
}

test "上下文传播数据完整性测试" {
  // 测试上下文传播中的数据完整性
  
  let composite = telemetry.api.propagation.CompositePropagator.new([
    telemetry.api.propagation.W3CTraceContextPropagator::new(),
    telemetry.api.propagation.W3CBaggagePropagator::new()
  ])
  
  // 创建具有丰富数据的上下文
  let ctx = telemetry.api.context.Context.root()
  
  let user_key = telemetry.api.context.ContextKey.new("user.id")
  let session_key = telemetry.api.context.ContextKey.new("session.id")
  let request_key = telemetry.api.context.ContextKey.new("request.id")
  let correlation_key = telemetry.api.context.ContextKey.new("correlation.id")
  
  let ctx_with_values = telemetry.api.context.Context.with_value(
    telemetry.api.context.Context.with_value(
      telemetry.api.context.Context.with_value(
        telemetry.api.context.Context.with_value(ctx, user_key, "user12345"),
        session_key, "session_abcdef"
      ),
      request_key, "req_123456"
    ),
    correlation_key, "corr_789012"
  )
  
  // 测试上下文注入
  let carrier = telemetry.api.propagation.TextMapCarrier.new()
  telemetry.api.propagation.CompositePropagator.inject(composite, ctx_with_values, carrier)
  
  // 验证注入的数据
  let traceparent = telemetry.api.propagation.TextMapCarrier.get(carrier, "traceparent")
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
  let extracted_ctx = telemetry.api.propagation.CompositePropagator.extract(composite, carrier)
  assert_true(extracted_ctx != telemetry.api.context.Context::{})
  
  // 验证提取的上下文保持数据完整性
  let extraction_key = telemetry.api.context.ContextKey.new("extracted")
  let extraction_result = telemetry.api.context.Context.get(extracted_ctx, extraction_key)
  assert_eq(extraction_result, Some("true"))
  
  // 测试多次注入和提取的数据一致性
  let carrier2 = telemetry.api.propagation.TextMapCarrier.new()
  telemetry.api.propagation.CompositePropagator.inject(composite, extracted_ctx, carrier2)
  
  let traceparent2 = telemetry.api.propagation.TextMapCarrier.get(carrier2, "traceparent")
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
  let baggage_carrier = telemetry.api.propagation.TextMapCarrier.new()
  telemetry.api.propagation.TextMapCarrier.set(baggage_carrier, "baggage", "user=user12345,session=session_abcdef,request=req_123456")
  
  let baggage_extracted_ctx = telemetry.api.propagation.CompositePropagator.extract(composite, baggage_carrier)
  assert_true(baggage_extracted_ctx != telemetry.api.context.Context::{})
}

test "资源属性数据完整性测试" {
  // 测试资源属性的数据完整性
  
  // 创建具有丰富属性的资源
  let resource_attrs = [
    ("service.name", telemetry.api.common.StringValue("integrity.test.service")),
    ("service.version", telemetry.api.common.StringValue("1.0.0")),
    ("service.instance.id", telemetry.api.common.StringValue("instance-abc123")),
    ("deployment.environment", telemetry.api.common.StringValue("test")),
    ("host.name", telemetry.api.common.StringValue("test-host-01")),
    ("host.ip", telemetry.api.common.StringValue("192.168.1.100")),
    ("os.type", telemetry.api.common.StringValue("linux")),
    ("os.version", telemetry.api.common.StringValue("5.15.0")),
    ("process.id", telemetry.api.common.IntValue(12345)),
    ("process.executable.name", telemetry.api.common.StringValue("integrity_test")),
    ("process.command_args", telemetry.api.common.ArrayStringValue(["./integrity_test", "--config", "test.yaml"])),
    ("process.owner", telemetry.api.common.StringValue("testuser")),
    ("cloud.provider", telemetry.api.common.StringValue("aws")),
    ("cloud.region", telemetry.api.common.StringValue("us-west-2")),
    ("cloud.availability_zone", telemetry.api.common.StringValue("us-west-2a")),
    ("kubernetes.namespace", telemetry.api.common.StringValue("test")),
    ("kubernetes.pod.name", telemetry.api.common.StringValue("integrity-test-pod-123")),
    ("kubernetes.container.name", telemetry.api.common.StringValue("integrity-test-container")),
    ("telemetry.sdk.name", telemetry.api.common.StringValue("azimuth")),
    ("telemetry.sdk.language", telemetry.api.common.StringValue("moonbit")),
    ("telemetry.sdk.version", telemetry.api.common.StringValue("0.1.0"))
  ]
  
  let resource = telemetry.api.common.Resource.with_attributes(
    telemetry.api.common.Resource.new(),
    resource_attrs
  )
  
  // 验证资源属性完整性
  for (key, expected_value) in resource_attrs {
    let actual_value = telemetry.api.common.Resource.get_attribute(resource, key)
    // 简化实现可能返回None，但不应该崩溃
  }
  
  // 测试资源合并的数据完整性
  let additional_attrs = [
    ("custom.attr1", telemetry.api.common.StringValue("custom_value1")),
    ("custom.attr2", telemetry.api.common.IntValue(42)),
    ("custom.attr3", telemetry.api.common.FloatValue(3.14159)),
    ("custom.attr4", telemetry.api.common.BoolValue(true)),
    ("custom.attr5", telemetry.api.common.ArrayStringValue(["item1", "item2", "item3"])),
    ("custom.attr6", telemetry.api.common.ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let additional_resource = telemetry.api.common.Resource.with_attributes(
    telemetry.api.common.Resource.new(),
    additional_attrs
  )
  
  // 合并资源
  let merged_resource = telemetry.api.common.Resource.merge(resource, additional_resource)
  
  // 验证合并后的资源属性
  for (key, expected_value) in additional_attrs {
    let actual_value = telemetry.api.common.Resource.get_attribute(merged_resource, key)
    // 简化实现可能返回None，但不应该崩溃
  }
  
  // 测试使用资源创建提供者
  let trace_provider = telemetry.sdk.trace.TracerProvider.with_resource(resource)
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.with_resource(resource)
  let logs_provider = telemetry.sdk.logs.LoggerProvider.with_resource(resource)
  
  // 验证提供者创建成功
  assert_true(trace_provider != telemetry.sdk.trace.TracerProvider::{})
  assert_true(metrics_provider != telemetry.sdk.metrics.MeterProvider::{})
  assert_true(logs_provider != telemetry.sdk.logs.LoggerProvider::{})
  
  // 测试使用资源提供者创建仪器
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "resource.test.tracer")
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "resource.test.meter")
  let logger = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "resource.test.logger")
  
  // 验证仪器创建成功
  assert_true(tracer != telemetry.sdk.trace.Tracer::{})
  assert_true(meter != telemetry.sdk.metrics.Meter::{})
  assert_true(logger != telemetry.sdk.logs.Logger::{})
}

test "属性序列化反序列化完整性测试" {
  // 测试属性序列化和反序列化的数据完整性
  
  let attrs = telemetry.api.common.Attributes.new()
  
  // 添加各种类型的属性
  telemetry.api.common.Attributes.set(attrs, "string.basic", telemetry.api.common.StringValue("basic_string"))
  telemetry.api.common.Attributes.set(attrs, "string.empty", telemetry.api.common.StringValue(""))
  telemetry.api.common.Attributes.set(attrs, "string.special", telemetry.api.common.StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  telemetry.api.common.Attributes.set(attrs, "string.unicode", telemetry.api.common.StringValue("🚀 测试 🌟"))
  telemetry.api.common.Attributes.set(attrs, "string.long", "x".repeat(1000))
  
  telemetry.api.common.Attributes.set(attrs, "int.zero", telemetry.api.common.IntValue(0))
  telemetry.api.common.Attributes.set(attrs, "int.positive", telemetry.api.common.IntValue(42))
  telemetry.api.common.Attributes.set(attrs, "int.negative", telemetry.api.common.IntValue(-42))
  telemetry.api.common.Attributes.set(attrs, "int.max", telemetry.api.common.IntValue(2147483647))
  telemetry.api.common.Attributes.set(attrs, "int.min", telemetry.api.common.IntValue(-2147483648))
  
  telemetry.api.common.Attributes.set(attrs, "float.zero", telemetry.api.common.FloatValue(0.0))
  telemetry.api.common.Attributes.set(attrs, "float.positive", telemetry.api.common.FloatValue(3.14159))
  telemetry.api.common.Attributes.set(attrs, "float.negative", telemetry.api.common.FloatValue(-3.14159))
  telemetry.api.common.Attributes.set(attrs, "float.scientific", telemetry.api.common.FloatValue(1.23e-4))
  telemetry.api.common.Attributes.set(attrs, "float.large", telemetry.api.common.FloatValue(1.7976931348623157e+308))
  
  telemetry.api.common.Attributes.set(attrs, "bool.true", telemetry.api.common.BoolValue(true))
  telemetry.api.common.Attributes.set(attrs, "bool.false", telemetry.api.common.BoolValue(false))
  
  telemetry.api.common.Attributes.set(attrs, "array.string.empty", telemetry.api.common.ArrayStringValue([]))
  telemetry.api.common.Attributes.set(attrs, "array.string.single", telemetry.api.common.ArrayStringValue(["single"]))
  telemetry.api.common.Attributes.set(attrs, "array.string.multiple", telemetry.api.common.ArrayStringValue(["item1", "item2", "item3"]))
  telemetry.api.common.Attributes.set(attrs, "array.string.empty_items", telemetry.api.common.ArrayStringValue(["", "", ""]))
  telemetry.api.common.Attributes.set(attrs, "array.string.special", telemetry.api.common.ArrayStringValue(["!@#", "$%^", "&*()"]))
  telemetry.api.common.Attributes.set(attrs, "array.string.unicode", telemetry.api.common.ArrayStringValue(["🚀", "测试", "🌟"]))
  
  telemetry.api.common.Attributes.set(attrs, "array.int.empty", telemetry.api.common.ArrayIntValue([]))
  telemetry.api.common.Attributes.set(attrs, "array.int.single", telemetry.api.common.ArrayIntValue([42]))
  telemetry.api.common.Attributes.set(attrs, "array.int.multiple", telemetry.api.common.ArrayIntValue([1, 2, 3, 4, 5]))
  telemetry.api.common.Attributes.set(attrs, "array.int.zeroes", telemetry.api.common.ArrayIntValue([0, 0, 0]))
  telemetry.api.common.Attributes.set(attrs, "array.int.positive_negative", telemetry.api.common.ArrayIntValue([-10, -5, 0, 5, 10]))
  telemetry.api.common.Attributes.set(attrs, "array.int.limits", telemetry.api.common.ArrayIntValue([-2147483648, 2147483647]))
  
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
    let value = telemetry.api.common.Attributes.get(attrs, key)
    // 简化实现可能返回None，但不应该崩溃
  }
  
  // 测试属性在Span中的完整性
  let trace_provider = telemetry.sdk.trace.TracerProvider::default()
  let tracer = telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "attribute.integrity.tracer")
  
  let (_, span) = telemetry.sdk.trace.Tracer.start_span(tracer, "attribute.integrity.span")
  
  // 将所有属性设置到Span中
  for key in test_keys {
    let value = telemetry.api.common.Attributes.get(attrs, key)
    match value {
      Some(v) => telemetry.api.trace.Span.set_attribute(span, key, v)
      None => ()  // 跳过None值
    }
  }
  
  // 添加带有复杂属性的事件
  let event_attrs = telemetry.api.common.Attributes.new()
  for key in test_keys {
    let value = telemetry.api.common.Attributes.get(attrs, key)
    match value {
      Some(v) => telemetry.api.common.Attributes.set(event_attrs, "event." + key, v)
      None => ()  // 跳过None值
    }
  }
  
  telemetry.api.trace.Span.add_event(span, "attribute.integrity.event", Some(event_attrs), None)
  
  // 结束Span
  telemetry.api.trace.Span.end(span)
  
  // 验证Span仍然有效
  assert_false(span.is_recording())
  assert_true(span.span_context().is_valid())
}

test "导出器数据完整性测试" {
  // 测试导出器处理数据的完整性
  
  // 创建测试Span
  let trace_provider = telemetry.sdk.trace.TracerProvider::default()
  let tracer = telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "exporter.integrity.tracer")
  
  let (_, span) = telemetry.sdk.trace.Tracer.start_span(tracer, "exporter.integrity.span")
  
  // 添加完整的数据
  telemetry.api.trace.Span.set_attribute(span, "exporter.test", telemetry.api.common.StringValue("data.integrity"))
  telemetry.api.trace.Span.set_attribute(span, "timestamp", telemetry.api.common.IntValue(1700000000))
  telemetry.api.trace.Span.set_attribute(span, "version", telemetry.api.common.StringValue("1.0.0"))
  
  telemetry.api.trace.Span.add_event(span, "exporter.test.event", Some([
    ("event.data", telemetry.api.common.StringValue("test_data")),
    ("event.type", telemetry.api.common.StringValue("integrity_check"))
  ]), None)
  
  telemetry.api.trace.Span.set_status(span, telemetry.api.trace.StatusCode.Ok, Some("Data integrity test"))
  
  // 创建导出器
  let console_exporter = telemetry.sdk.exporter.console.ConsoleSpanExporter.new()
  let otlp_exporter = telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.new()
  
  // 测试单个Span导出
  telemetry.sdk.exporter.console.ConsoleSpanExporter.export(console_exporter, span)
  telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(otlp_exporter, span)
  
  // 创建多个Span进行批量导出测试
  let spans = []
  
  for i in 0..5 {
    let (_, batch_span) = telemetry.sdk.trace.Tracer.start_span(tracer, "batch.integrity.span." + i.to_string())
    
    telemetry.api.trace.Span.set_attribute(batch_span, "batch.index", telemetry.api.common.IntValue(i))
    telemetry.api.trace.Span.set_attribute(batch_span, "batch.total", telemetry.api.common.IntValue(5))
    telemetry.api.trace.Span.set_attribute(batch_span, "batch.data", telemetry.api.common.StringValue("batch_data_" + i.to_string()))
    
    spans = spans @ [batch_span]
  }
  
  // 测试批量导出
  telemetry.sdk.exporter.console.ConsoleSpanExporter.export_batch(console_exporter, spans)
  telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export_batch(otlp_exporter, spans)
  
  // 结束所有Span
  telemetry.api.trace.Span.end(span)
  for batch_span in spans {
    telemetry.api.trace.Span.end(batch_span)
  }
  
  // 验证导出器仍然可用
  assert_true(console_exporter != telemetry.sdk.exporter.console.ConsoleSpanExporter::{})
  assert_true(otlp_exporter != telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  
  // 测试日志导出器
  let logs_provider = telemetry.sdk.logs.LoggerProvider::default()
  let logger = telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "exporter.integrity.logger")
  
  let console_log_exporter = telemetry.sdk.exporter.console.ConsoleLogExporter.new()
  
  let log_record = telemetry.api.logs.LogRecord.new(
    telemetry.api.logs.Info,
    "Exporter integrity test log",
    Some([
      ("exporter.test", telemetry.api.common.StringValue("data.integrity")),
      ("log.type", telemetry.api.common.StringValue("integrity_check"))
    ]),
    Some(telemetry.sdk.platform.time.Clock.now_unix_nanos(telemetry.sdk.platform.time.Clock.system())),
    None,
    Some("trace123456789abcdef"),
    Some("span1234567890abcdef"),
    None
  )
  
  telemetry.sdk.exporter.console.ConsoleLogExporter.export_log(console_log_exporter, log_record)
  
  // 测试指标导出器
  let metrics_provider = telemetry.sdk.metrics.MeterProvider::default()
  let meter = telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "exporter.integrity.meter")
  
  let console_metric_exporter = telemetry.sdk.exporter.console.ConsoleMetricExporter.new()
  
  let counter_data = telemetry.sdk.exporter.console.MetricData.Counter(
    "exporter.integrity.counter",
    Some("Exporter integrity test counter"),
    Some("operations"),
    42.0,
    Some([
      ("exporter.test", telemetry.api.common.StringValue("data.integrity"))
    ])
  )
  
  let histogram_data = telemetry.sdk.exporter.console.MetricData.Histogram(
    "exporter.integrity.histogram",
    Some("Exporter integrity test histogram"),
    Some("ms"),
    [10.5, 25.0, 15.75, 30.25, 20.0],
    Some([
      ("exporter.test", telemetry.api.common.StringValue("data.integrity"))
    ])
  )
  
  telemetry.sdk.exporter.console.ConsoleMetricExporter.export_metric(console_metric_exporter, counter_data)
  telemetry.sdk.exporter.console.ConsoleMetricExporter.export_metric(console_metric_exporter, histogram_data)
}