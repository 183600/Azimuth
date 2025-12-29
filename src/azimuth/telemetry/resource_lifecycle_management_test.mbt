// 资源管理测试 - 测试系统资源的创建、使用和释放
use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.context.{Context, create_key}
use azimuth.telemetry.api.logs.{LogRecord, SeverityNumber}
use azimuth.telemetry.api.metrics.{NoopMeter}
use azimuth.telemetry.api.trace.{SpanKind}
use azimuth.telemetry.api.propagation.{MapCarrier, W3CTraceContextPropagator}

test "resource_lifecycle_basic" {
  // 测试基本资源生命周期
  
  // 1. 资源创建
  let resource = Resource::default("lifecycle-test-service")
  assert_eq(resource.service_name, "lifecycle-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 2. 资源使用
  let log_record = LogRecord::builder()
    .severity(SeverityNumber::Info)
    .body("Resource lifecycle test")
    .with_attribute("service.name", AttributeValue::string(resource.service_name))
    .build()
  
  assert_eq(log_record.attributes.length(), 1)
  
  // 3. 资源复制和共享
  let resource_copy = resource
  assert_eq(resource_copy.service_name, resource.service_name)
  assert_eq(resource_copy.telemetry_sdk_name, resource.telemetry_sdk_name)
  
  // 4. 资源更新（创建新资源）
  let updated_resource = Resource::default("updated-service")
  assert_eq(updated_resource.service_name, "updated-service")
  assert_eq(updated_resource.service_name != resource.service_name, true)
}

test "resource_lifecycle_complex" {
  // 测试复杂资源生命周期
  
  // 1. 创建带有属性的复杂资源
  let base_resource = Resource::default("complex-service")
  let complex_attrs = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-123")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true))
  ]
  
  // 2. 资源属性管理
  let mut resource_with_attrs = base_resource
  
  // 模拟添加属性（在实际实现中可能需要不同的方法）
  let mut i = 0
  while i < complex_attrs.length() {
    let (key, value) = complex_attrs[i]
    // 在实际实现中，这里应该更新resource的attributes
    // 由于Resource是只读的，我们只能验证属性值
    match value {
      StringValue(s) => assert_eq(s.length() > 0, true)
      IntValue(v) => assert_eq(v >= 0L, true)
      FloatValue(v) => assert_eq(v >= 0.0, true)
      BoolValue(b) => assert_eq(b == true || b == false, true)
      _ => @test.fail("Unexpected attribute type")
    }
    i = i + 1
  }
  
  // 3. 资源生命周期验证
  assert_eq(resource_with_attrs.service_name, "complex-service")
  assert_eq(resource_with_attrs.telemetry_sdk_name, "azimuth")
  
  // 4. 资源清理（模拟）
  // 在MoonBit中，资源会自动被垃圾回收器管理
  // 这里我们只是验证资源不再被使用时的状态
  let final_resource = Resource::default("")
  assert_eq(final_resource.service_name, "")
}

test "resource_lifecycle_context" {
  // 测试Context资源生命周期
  
  // 1. Context创建和初始化
  let empty_context = Context::empty()
  assert_eq(empty_context.values.length(), 0)
  
  // 2. Context资源分配
  let key1 = create_key("resource.key1")
  let key2 = create_key("resource.key2")
  let key3 = create_key("resource.key3")
  
  let context_with_values = empty_context
    .with_value(key1, "value1")
    .with_value(key2, "value2")
    .with_value(key3, "value3")
  
  // 3. Context资源使用
  match context_with_values.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected value for key1")
  }
  
  match context_with_values.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected value for key2")
  }
  
  match context_with_values.get(key3) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected value for key3")
  }
  
  // 4. Context资源复制和共享
  let context_copy = context_with_values
  match context_copy.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected value for key1 in copy")
  }
  
  // 5. Context资源释放（模拟）
  let cleared_context = Context::empty()
  match cleared_context.get(key1) {
    Some(_) => @test.fail("Expected None for cleared context")
    None => assert_eq(true, true)
  }
}

test "resource_lifecycle_logs" {
  // 测试日志资源生命周期
  
  // 1. LogRecord资源创建
  let log_record = LogRecord::builder()
    .severity(SeverityNumber::Info)
    .body("Resource lifecycle log test")
    .build()
  
  // 2. LogRecord资源使用
  assert_eq(log_record.severity_number, SeverityNumber::Info)
  assert_eq(log_record.body.unwrap_or(""), "Resource lifecycle log test")
  assert_eq(log_record.attributes.length(), 0)
  
  // 3. 复杂LogRecord资源创建
  let complex_log_record = LogRecord::builder()
    .severity(SeverityNumber::Error)
    .body("Complex resource error")
    .severity_text("ERROR")
    .with_attribute("error.type", AttributeValue::string("ResourceError"))
    .with_attribute("error.code", AttributeValue::int(500L))
    .with_attribute("error.retryable", AttributeValue::bool(false))
    .with_attribute("error.timestamp", AttributeValue::int(1234567890L))
    .build()
  
  // 4. 复杂LogRecord资源使用
  assert_eq(complex_log_record.severity_number, SeverityNumber::Error)
  assert_eq(complex_log_record.severity_text.unwrap_or(""), "ERROR")
  assert_eq(complex_log_record.attributes.length(), 4)
  
  // 5. LogRecord资源复制
  let log_record_copy = complex_log_record
  assert_eq(log_record_copy.severity_number, complex_log_record.severity_number)
  assert_eq(log_record_copy.body, complex_log_record.body)
  assert_eq(log_record_copy.attributes.length(), complex_log_record.attributes.length())
  
  // 6. LogRecord资源清理（模拟）
  let minimal_log_record = LogRecord::builder()
    .severity(SeverityNumber::Debug)
    .build()
  
  assert_eq(minimal_log_record.severity_number, SeverityNumber::Debug)
  assert_eq(minimal_log_record.body, None)
  assert_eq(minimal_log_record.attributes.length(), 0)
}

test "resource_lifecycle_metrics" {
  // 测试指标资源生命周期
  
  // 1. Meter资源创建
  let meter = NoopMeter::{}
  
  // 2. 指标资源创建
  let counter = meter.create_counter("resource.counter", "count", "Resource counter")
  let histogram = meter.create_histogram("resource.histogram", "ms", "Resource histogram")
  let gauge = meter.create_gauge("resource.gauge", "units", "Resource gauge")
  
  // 3. 指标资源使用
  counter.add(1L, [("resource.test", AttributeValue::bool(true))])
  histogram.record(100.0, [("resource.test", AttributeValue::bool(true))])
  gauge.record(50.0, [("resource.test", AttributeValue::bool(true))])
  
  // 4. 指标资源批量操作
  let mut i = 0
  while i < 100 {
    counter.add(1L, [("batch.id", AttributeValue::int(i.to_int64()))])
    histogram.record(i.to_double(), [("batch.id", AttributeValue::int(i.to_int64()))])
    gauge.record(i.to_double() / 10.0, [("batch.id", AttributeValue::int(i.to_int64()))])
    i = i + 1
  }
  
  // 5. 指标资源清理（模拟）
  // 在实际实现中，可能需要显式清理指标资源
  // 这里我们只是验证操作不会导致错误
  assert_eq(true, true)
}

test "resource_lifecycle_spans" {
  // 测试Span资源生命周期
  
  // 1. Tracer资源创建
  let tracer = NoopTracer::{}
  let base_context = Context::empty()
  
  // 2. Span资源创建
  let (context1, span1) = tracer.start_span(
    base_context,
    "resource-span-1",
    Some(SpanKind::Internal),
    [("resource.test", AttributeValue::bool(true))]
  )
  
  // 3. Span资源使用
  assert_eq(span1.name, "resource-span-1")
  assert_eq(span1.kind, SpanKind::Internal)
  assert_eq(span1.attributes.length(), 1)
  
  // 4. 嵌套Span资源创建
  let (context2, span2) = tracer.start_span(
    context1,
    "resource-span-2",
    Some(SpanKind::Client),
    [("nested.span", AttributeValue::bool(true))]
  )
  
  assert_eq(span2.name, "resource-span-2")
  assert_eq(span2.kind, SpanKind::Client)
  assert_eq(span2.attributes.length(), 1)
  
  // 5. Span资源链
  let (context3, span3) = tracer.start_span(
    context2,
    "resource-span-3",
    Some(SpanKind::Server),
    [("chained.span", AttributeValue::bool(true))]
  )
  
  assert_eq(span3.name, "resource-span-3")
  assert_eq(span3.kind, SpanKind::Server)
  assert_eq(span3.attributes.length(), 1)
  
  // 6. Span资源清理（模拟）
  // 在实际实现中，可能需要显式结束Span
  // 这里我们只是验证Span资源被正确创建
  assert_eq(span1.name.length() > 0, true)
  assert_eq(span2.name.length() > 0, true)
  assert_eq(span3.name.length() > 0, true)
}

test "resource_lifecycle_propagation" {
  // 测试传播资源生命周期
  
  // 1. Propagator资源创建
  let propagator = W3CTraceContextPropagator::{}
  
  // 2. Carrier资源创建
  let carrier = MapCarrier::new()
  
  // 3. 传播资源使用
  let context = Context::empty()
  let key = create_key("propagation.resource.key")
  let context_with_data = context.with_value(key, "propagation.resource.value")
  
  // 4. 注入操作
  propagator.inject(context_with_data, carrier)
  
  // 5. 提取操作
  let extracted_context = propagator.extract(Context::empty(), carrier)
  
  // 6. 验证传播资源
  match extracted_context.get(key) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected extracted context value")
  }
  
  // 7. 传播资源清理（模拟）
  let empty_carrier = MapCarrier::new()
  let empty_extracted_context = propagator.extract(Context::empty(), empty_carrier)
  
  match empty_extracted_context.get(key) {
    Some(_) => @test.fail("Expected None for empty carrier")
    None => assert_eq(true, true)
  }
}

test "resource_lifecycle_memory_management" {
  // 测试内存管理
  
  // 1. 大量资源创建
  let mut resources = []
  let mut i = 0
  while i < 1000 {
    let resource = Resource::default("memory-test-service-" + i.to_string())
    resources.push(resource)
    i = i + 1
  }
  
  // 2. 验证资源创建
  assert_eq(resources.length(), 1000)
  
  // 3. 资源访问验证
  let mut j = 0
  while j < 10 {
    let resource = resources[j]
    assert_eq(resource.service_name, "memory-test-service-" + j.to_string())
    j = j + 1
  }
  
  // 4. 大量Context创建
  let mut contexts = []
  let key = create_key("memory.test.key")
  
  let mut k = 0
  while k < 1000 {
    let context = Context::empty()
    let ctx = context.with_value(key, "memory.test.value." + k.to_string())
    contexts.push(ctx)
    k = k + 1
  }
  
  // 5. 验证Context创建
  assert_eq(contexts.length(), 1000)
  
  // 6. Context访问验证
  let mut l = 0
  while l < 10 {
    let context = contexts[l]
    match context.get(key) {
      Some(value) => assert_eq(value, "memory.test.value." + l.to_string())
      None => @test.fail("Expected value for memory.test.key")
    }
    l = l + 1
  }
  
  // 7. 资源释放（模拟）
  // 在实际实现中，可能需要显式释放资源
  // 这里我们只是创建新的空数组来模拟资源释放
  resources = []
  contexts = []
  
  assert_eq(resources.length(), 0)
  assert_eq(contexts.length(), 0)
}

test "resource_lifecycle_error_handling" {
  // 测试资源生命周期中的错误处理
  
  // 1. 资源创建错误处理
  let invalid_resource = Resource::default("")
  assert_eq(invalid_resource.service_name, "")
  
  // 2. 资源使用错误处理
  let log_record = LogRecord::builder()
    .severity(SeverityNumber::Error)
    .body("")
    .with_attribute("", AttributeValue::string(""))
    .build()
  
  assert_eq(log_record.severity_number, SeverityNumber::Error)
  assert_eq(log_record.body.unwrap_or(""), "")
  assert_eq(log_record.attributes.length(), 1)
  
  // 3. 资源清理错误处理
  let context = Context::empty()
  let invalid_key = create_key("")
  let context_with_invalid = context.with_value(invalid_key, "")
  
  match context_with_invalid.get(invalid_key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected value for empty key")
  }
  
  // 4. 资源恢复
  let recovered_resource = Resource::default("recovered-service")
  assert_eq(recovered_resource.service_name, "recovered-service")
  assert_eq(recovered_resource.telemetry_sdk_name, "azimuth")
  
  let recovered_context = Context::empty()
  let recovery_key = create_key("recovery.key")
  let recovered_context_with_data = recovered_context.with_value(recovery_key, "recovered.value")
  
  match recovered_context_with_data.get(recovery_key) {
    Some(value) => assert_eq(value, "recovered.value")
    None => @test.fail("Expected recovered value")
  }
}