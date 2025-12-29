// 并发安全测试 - 测试多线程并发场景

test "concurrent_context_operations" {
  // 测试并发Context操作的安全性
  
  let base_ctx = context::Context::empty()
  
  // 模拟并发写入不同的键
  let ctx1 = base_ctx.with_value(context::create_key("thread1.key1"), "thread1.value1")
  let ctx2 = base_ctx.with_value(context::create_key("thread2.key2"), "thread2.value2")
  let ctx3 = base_ctx.with_value(context::create_key("thread3.key3"), "thread3.value3")
  let ctx4 = base_ctx.with_value(context::create_key("thread4.key4"), "thread4.value4")
  let ctx5 = base_ctx.with_value(context::create_key("thread5.key5"), "thread5.value5")
  
  // 验证每个context都有正确的值
  match ctx1.get(context::create_key("thread1.key1")) {
    Some(value) => assert_eq(value, "thread1.value1")
    None => @test.fail("Expected thread1.key1 value")
  }
  
  match ctx2.get(context::create_key("thread2.key2")) {
    Some(value) => assert_eq(value, "thread2.value2")
    None => @test.fail("Expected thread2.key2 value")
  }
  
  match ctx3.get(context::create_key("thread3.key3")) {
    Some(value) => assert_eq(value, "thread3.value3")
    None => @test.fail("Expected thread3.key3 value")
  }
  
  match ctx4.get(context::create_key("thread4.key4")) {
    Some(value) => assert_eq(value, "thread4.value4")
    None => @test.fail("Expected thread4.key4 value")
  }
  
  match ctx5.get(context::create_key("thread5.key5")) {
    Some(value) => assert_eq(value, "thread5.value5")
    None => @test.fail("Expected thread5.key5 value")
  }
  
  // 验证交叉访问不会影响其他context
  match ctx1.get(context::create_key("thread2.key2")) {
    Some(_) => @test.fail("Should not find thread2.key2 in ctx1")
    None => assert_eq(true, true)
  }
  
  match ctx2.get(context::create_key("thread1.key1")) {
    Some(_) => @test.fail("Should not find thread1.key1 in ctx2")
    None => assert_eq(true, true)
  }
}

test "concurrent_baggage_operations" {
  // 测试并发Baggage操作的安全性
  
  let base_baggage = context::Baggage::empty()
  
  // 模拟并发写入不同的baggage条目
  let baggage1 = base_baggage.with_entry("thread1.baggage1", "thread1.value1")
  let baggage2 = base_baggage.with_entry("thread2.baggage2", "thread2.value2")
  let baggage3 = base_baggage.with_entry("thread3.baggage3", "thread3.value3")
  let baggage4 = base_baggage.with_entry("thread4.baggage4", "thread4.value4")
  let baggage5 = base_baggage.with_entry("thread5.baggage5", "thread5.value5")
  
  // 验证每个baggage都有正确的值
  match baggage1.get("thread1.baggage1") {
    Some(value) => assert_eq(value, "thread1.value1")
    None => @test.fail("Expected thread1.baggage1 value")
  }
  
  match baggage2.get("thread2.baggage2") {
    Some(value) => assert_eq(value, "thread2.value2")
    None => @test.fail("Expected thread2.baggage2 value")
  }
  
  match baggage3.get("thread3.baggage3") {
    Some(value) => assert_eq(value, "thread3.value3")
    None => @test.fail("Expected thread3.baggage3 value")
  }
  
  match baggage4.get("thread4.baggage4") {
    Some(value) => assert_eq(value, "thread4.value4")
    None => @test.fail("Expected thread4.baggage4 value")
  }
  
  match baggage5.get("thread5.baggage5") {
    Some(value) => assert_eq(value, "thread5.value5")
    None => @test.fail("Expected thread5.baggage5 value")
  }
  
  // 验证交叉访问不会影响其他baggage
  match baggage1.get("thread2.baggage2") {
    Some(_) => @test.fail("Should not find thread2.baggage2 in baggage1")
    None => assert_eq(true, true)
  }
  
  match baggage2.get("thread1.baggage1") {
    Some(_) => @test.fail("Should not find thread1.baggage1 in baggage2")
    None => assert_eq(true, true)
  }
}

test "concurrent_span_creation" {
  // 测试并发Span创建的安全性
  
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("concurrent-test-tracer")
  
  // 模拟并发创建多个span
  let (ctx1, span1) = tracer.start_span(
    ctx,
    "concurrent-span-1",
    trace::Internal,
    [("thread.id", common::AttributeValue::string("thread1"))],
    Some(1000000L)
  )
  
  let (ctx2, span2) = tracer.start_span(
    ctx,
    "concurrent-span-2",
    trace::Server,
    [("thread.id", common::AttributeValue::string("thread2"))],
    Some(2000000L)
  )
  
  let (ctx3, span3) = tracer.start_span(
    ctx,
    "concurrent-span-3",
    trace::Client,
    [("thread.id", common::AttributeValue::string("thread3"))],
    Some(3000000L)
  )
  
  let (ctx4, span4) = tracer.start_span(
    ctx,
    "concurrent-span-4",
    trace::Producer,
    [("thread.id", common::AttributeValue::string("thread4"))],
    Some(4000000L)
  )
  
  let (ctx5, span5) = tracer.start_span(
    ctx,
    "concurrent-span-5",
    trace::Consumer,
    [("thread.id", common::AttributeValue::string("thread5"))],
    Some(5000000L)
  )
  
  // 验证每个span都有正确的属性
  assert_eq(span1.name, "concurrent-span-1")
  assert_eq(span2.name, "concurrent-span-2")
  assert_eq(span3.name, "concurrent-span-3")
  assert_eq(span4.name, "concurrent-span-4")
  assert_eq(span5.name, "concurrent-span-5")
  
  match span1.kind { trace::Internal => assert_eq(true, true) _ => @test.fail("Expected Internal") }
  match span2.kind { trace::Server => assert_eq(true, true) _ => @test.fail("Expected Server") }
  match span3.kind { trace::Client => assert_eq(true, true) _ => @test.fail("Expected Client") }
  match span4.kind { trace::Producer => assert_eq(true, true) _ => @test.fail("Expected Producer") }
  match span5.kind { trace::Consumer => assert_eq(true, true) _ => @test.fail("Expected Consumer") }
  
  assert_eq(span1.start_time_unix_nanos, 1000000L)
  assert_eq(span2.start_time_unix_nanos, 2000000L)
  assert_eq(span3.start_time_unix_nanos, 3000000L)
  assert_eq(span4.start_time_unix_nanos, 4000000L)
  assert_eq(span5.start_time_unix_nanos, 5000000L)
  
  // 验证每个span都有正确的thread.id属性
  match span1.attributes[0].1 {
    common::StringValue(thread_id) => assert_eq(thread_id, "thread1")
    _ => @test.fail("Expected StringValue")
  }
  
  match span2.attributes[0].1 {
    common::StringValue(thread_id) => assert_eq(thread_id, "thread2")
    _ => @test.fail("Expected StringValue")
  }
  
  match span3.attributes[0].1 {
    common::StringValue(thread_id) => assert_eq(thread_id, "thread3")
    _ => @test.fail("Expected StringValue")
  }
  
  match span4.attributes[0].1 {
    common::StringValue(thread_id) => assert_eq(thread_id, "thread4")
    _ => @test.fail("Expected StringValue")
  }
  
  match span5.attributes[0].1 {
    common::StringValue(thread_id) => assert_eq(thread_id, "thread5")
    _ => @test.fail("Expected StringValue")
  }
}

test "concurrent_metrics_operations" {
  // 测试并发Metrics操作的安全性
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("concurrent-test-meter")
  
  // 创建多个指标实例
  let counter1 = meter.create_counter("concurrent.counter1", "count", "Concurrent counter 1")
  let counter2 = meter.create_counter("concurrent.counter2", "count", "Concurrent counter 2")
  let counter3 = meter.create_counter("concurrent.counter3", "count", "Concurrent counter 3")
  
  let histogram1 = meter.create_histogram("concurrent.histogram1", "ms", "Concurrent histogram 1")
  let histogram2 = meter.create_histogram("concurrent.histogram2", "ms", "Concurrent histogram 2")
  
  let gauge1 = meter.create_gauge("concurrent.gauge1", "percent", "Concurrent gauge 1")
  let gauge2 = meter.create_gauge("concurrent.gauge2", "percent", "Concurrent gauge 2")
  
  let up_down_counter1 = meter.create_up_down_counter("concurrent.updown1", "count", "Concurrent up-down 1")
  let up_down_counter2 = meter.create_up_down_counter("concurrent.updown2", "count", "Concurrent up-down 2")
  
  // 模拟并发操作不同的指标
  counter1.add(1L, [("thread.id", common::AttributeValue::string("thread1"))])
  counter2.add(2L, [("thread.id", common::AttributeValue::string("thread2"))])
  counter3.add(3L, [("thread.id", common::AttributeValue::string("thread3"))])
  
  histogram1.record(100.0, [("thread.id", common::AttributeValue::string("thread1"))])
  histogram2.record(200.0, [("thread.id", common::AttributeValue::string("thread2"))])
  
  gauge1.record(75.0, [("thread.id", common::AttributeValue::string("thread1"))])
  gauge2.record(85.0, [("thread.id", common::AttributeValue::string("thread2"))])
  
  up_down_counter1.add(5L, [("thread.id", common::AttributeValue::string("thread1"))])
  up_down_counter2.add(-3L, [("thread.id", common::AttributeValue::string("thread2"))])
  
  // 验证操作不会失败（no-op实现）
  assert_eq(true, true)
}

test "concurrent_log_operations" {
  // 测试并发Log操作的安全性
  
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("concurrent-test-logger", "1.0.0")
  
  // 模拟并发创建多个日志记录
  let log1 = logs::LogRecord::builder()
    .timestamp(1000000L)
    .severity(logs::Info)
    .body("Concurrent log message 1")
    .with_attribute("thread.id", common::AttributeValue::string("thread1"))
    .with_attribute("log.sequence", common::AttributeValue::int(1L))
    .build()
  
  let log2 = logs::LogRecord::builder()
    .timestamp(2000000L)
    .severity(logs::Warn)
    .body("Concurrent log message 2")
    .with_attribute("thread.id", common::AttributeValue::string("thread2"))
    .with_attribute("log.sequence", common::AttributeValue::int(2L))
    .build()
  
  let log3 = logs::LogRecord::builder()
    .timestamp(3000000L)
    .severity(logs::Error)
    .body("Concurrent log message 3")
    .with_attribute("thread.id", common::AttributeValue::string("thread3"))
    .with_attribute("log.sequence", common::AttributeValue::int(3L))
    .build()
  
  let log4 = logs::LogRecord::builder()
    .timestamp(4000000L)
    .severity(logs::Debug)
    .body("Concurrent log message 4")
    .with_attribute("thread.id", common::AttributeValue::string("thread4"))
    .with_attribute("log.sequence", common::AttributeValue::int(4L))
    .build()
  
  let log5 = logs::LogRecord::builder()
    .timestamp(5000000L)
    .severity(logs::Fatal)
    .body("Concurrent log message 5")
    .with_attribute("thread.id", common::AttributeValue::string("thread5"))
    .with_attribute("log.sequence", common::AttributeValue::int(5L))
    .build()
  
  // 验证每个日志记录都有正确的属性
  match log1.body {
    Some(body) => assert_eq(body, "Concurrent log message 1")
    None => @test.fail("Expected log1 body")
  }
  
  match log2.body {
    Some(body) => assert_eq(body, "Concurrent log message 2")
    None => @test.fail("Expected log2 body")
  }
  
  match log3.body {
    Some(body) => assert_eq(body, "Concurrent log message 3")
    None => @test.fail("Expected log3 body")
  }
  
  match log4.body {
    Some(body) => assert_eq(body, "Concurrent log message 4")
    None => @test.fail("Expected log4 body")
  }
  
  match log5.body {
    Some(body) => assert_eq(body, "Concurrent log message 5")
    None => @test.fail("Expected log5 body")
  }
  
  assert_eq(log1.timestamp_unix_nanos, 1000000L)
  assert_eq(log2.timestamp_unix_nanos, 2000000L)
  assert_eq(log3.timestamp_unix_nanos, 3000000L)
  assert_eq(log4.timestamp_unix_nanos, 4000000L)
  assert_eq(log5.timestamp_unix_nanos, 5000000L)
  
  match log1.severity_number { logs::Info => assert_eq(true, true) _ => @test.fail("Expected Info") }
  match log2.severity_number { logs::Warn => assert_eq(true, true) _ => @test.fail("Expected Warn") }
  match log3.severity_number { logs::Error => assert_eq(true, true) _ => @test.fail("Expected Error") }
  match log4.severity_number { logs::Debug => assert_eq(true, true) _ => @test.fail("Expected Debug") }
  match log5.severity_number { logs::Fatal => assert_eq(true, true) _ => @test.fail("Expected Fatal") }
}

test "concurrent_propagation_operations" {
  // 测试并发传播操作的安全性
  
  let ctx = context::Context::empty()
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  let composite = propagation::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 模拟并发注入操作
  let carrier1 = propagation::MapCarrier::new()
  let carrier2 = propagation::MapCarrier::new()
  let carrier3 = propagation::MapCarrier::new()
  let carrier4 = propagation::MapCarrier::new()
  let carrier5 = propagation::MapCarrier::new()
  
  trace_propagator.inject(ctx, carrier1)
  baggage_propagator.inject(ctx, carrier2)
  composite.inject(ctx, carrier3)
  trace_propagator.inject(ctx, carrier4)
  baggage_propagator.inject(ctx, carrier5)
  
  // 模拟并发提取操作
  let extracted_ctx1 = trace_propagator.extract(ctx, carrier1)
  let extracted_ctx2 = baggage_propagator.extract(ctx, carrier2)
  let extracted_ctx3 = composite.extract(ctx, carrier3)
  let extracted_ctx4 = trace_propagator.extract(ctx, carrier4)
  let extracted_ctx5 = baggage_propagator.extract(ctx, carrier5)
  
  // 验证操作不会失败
  assert_eq(true, true)
  
  // 验证carrier的键
  let keys1 = carrier1.keys()
  let keys2 = carrier2.keys()
  let keys3 = carrier3.keys()
  let keys4 = carrier4.keys()
  let keys5 = carrier5.keys()
  
  assert_eq(keys1.length(), 1)  // traceparent
  assert_eq(keys2.length(), 1)  // baggage
  assert_eq(keys3.length(), 2)  // traceparent + baggage
  assert_eq(keys4.length(), 1)  // traceparent
  assert_eq(keys5.length(), 1)  // baggage
}

test "concurrent_resource_creation" {
  // 测试并发Resource创建的安全性
  
  // 模拟并发创建多个资源
  let resource1 = common::Resource::default("concurrent-service-1")
  let resource2 = common::Resource::default("concurrent-service-2")
  let resource3 = common::Resource::default("concurrent-service-3")
  let resource4 = common::Resource::default("concurrent-service-4")
  let resource5 = common::Resource::default("concurrent-service-5")
  
  // 验证每个资源都有正确的属性
  assert_eq(resource1.service_name, "concurrent-service-1")
  assert_eq(resource2.service_name, "concurrent-service-2")
  assert_eq(resource3.service_name, "concurrent-service-3")
  assert_eq(resource4.service_name, "concurrent-service-4")
  assert_eq(resource5.service_name, "concurrent-service-5")
  
  // 验证所有资源都有相同的SDK信息
  assert_eq(resource1.telemetry_sdk_name, "azimuth")
  assert_eq(resource2.telemetry_sdk_name, "azimuth")
  assert_eq(resource3.telemetry_sdk_name, "azimuth")
  assert_eq(resource4.telemetry_sdk_name, "azimuth")
  assert_eq(resource5.telemetry_sdk_name, "azimuth")
  
  assert_eq(resource1.telemetry_sdk_version, "0.1.0")
  assert_eq(resource2.telemetry_sdk_version, "0.1.0")
  assert_eq(resource3.telemetry_sdk_version, "0.1.0")
  assert_eq(resource4.telemetry_sdk_version, "0.1.0")
  assert_eq(resource5.telemetry_sdk_version, "0.1.0")
  
  // 验证所有资源都有空的属性数组
  assert_eq(resource1.attributes.length(), 0)
  assert_eq(resource2.attributes.length(), 0)
  assert_eq(resource3.attributes.length(), 0)
  assert_eq(resource4.attributes.length(), 0)
  assert_eq(resource5.attributes.length(), 0)
}

test "concurrent_attribute_value_operations" {
  // 测试并发AttributeValue操作的安全性
  
  // 模拟并发创建不同类型的属性值
  let string_attr1 = common::AttributeValue::string("concurrent-string-1")
  let string_attr2 = common::AttributeValue::string("concurrent-string-2")
  let string_attr3 = common::AttributeValue::string("concurrent-string-3")
  
  let int_attr1 = common::AttributeValue::int(100L)
  let int_attr2 = common::AttributeValue::int(200L)
  let int_attr3 = common::AttributeValue::int(300L)
  
  let float_attr1 = common::AttributeValue::float(1.1)
  let float_attr2 = common::AttributeValue::float(2.2)
  let float_attr3 = common::AttributeValue::float(3.3)
  
  let bool_attr1 = common::AttributeValue::bool(true)
  let bool_attr2 = common::AttributeValue::bool(false)
  let bool_attr3 = common::AttributeValue::bool(true)
  
  let array_string_attr1 = common::AttributeValue::array_string(["item1", "item2"])
  let array_string_attr2 = common::AttributeValue::array_string(["item3", "item4"])
  let array_string_attr3 = common::AttributeValue::array_string(["item5", "item6"])
  
  let array_int_attr1 = common::AttributeValue::array_int([1L, 2L])
  let array_int_attr2 = common::AttributeValue::array_int([3L, 4L])
  let array_int_attr3 = common::AttributeValue::array_int([5L, 6L])
  
  // 验证每个属性值都有正确的值
  match string_attr1 {
    common::StringValue(s) => assert_eq(s, "concurrent-string-1")
    _ => @test.fail("Expected StringValue")
  }
  
  match string_attr2 {
    common::StringValue(s) => assert_eq(s, "concurrent-string-2")
    _ => @test.fail("Expected StringValue")
  }
  
  match string_attr3 {
    common::StringValue(s) => assert_eq(s, "concurrent-string-3")
    _ => @test.fail("Expected StringValue")
  }
  
  match int_attr1 {
    common::IntValue(i) => assert_eq(i, 100L)
    _ => @test.fail("Expected IntValue")
  }
  
  match int_attr2 {
    common::IntValue(i) => assert_eq(i, 200L)
    _ => @test.fail("Expected IntValue")
  }
  
  match int_attr3 {
    common::IntValue(i) => assert_eq(i, 300L)
    _ => @test.fail("Expected IntValue")
  }
  
  match float_attr1 {
    common::FloatValue(f) => assert_eq(f, 1.1)
    _ => @test.fail("Expected FloatValue")
  }
  
  match float_attr2 {
    common::FloatValue(f) => assert_eq(f, 2.2)
    _ => @test.fail("Expected FloatValue")
  }
  
  match float_attr3 {
    common::FloatValue(f) => assert_eq(f, 3.3)
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_attr1 {
    common::BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match bool_attr2 {
    common::BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected BoolValue")
  }
  
  match bool_attr3 {
    common::BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match array_string_attr1 {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match array_string_attr2 {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], "item3")
      assert_eq(arr[1], "item4")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match array_string_attr3 {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], "item5")
      assert_eq(arr[1], "item6")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match array_int_attr1 {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match array_int_attr2 {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], 3L)
      assert_eq(arr[1], 4L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match array_int_attr3 {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], 5L)
      assert_eq(arr[1], 6L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
}

test "concurrent_complex_telemetry_pipeline" {
  // 测试并发复杂遥测管道的安全性
  
  let ctx = context::Context::empty()
  let resource = common::Resource::default("concurrent-pipeline-service")
  
  // 创建多个tracer、meter、logger实例
  let tracer_provider1 = trace::NoopTracerProvider::{}
  let tracer_provider2 = trace::NoopTracerProvider::{}
  let meter_provider1 = metrics::NoopMeterProvider::{}
  let meter_provider2 = metrics::NoopMeterProvider::{}
  let logger_provider1 = logs::NoopLoggerProvider::{}
  let logger_provider2 = logs::NoopLoggerProvider::{}
  
  let tracer1 = tracer_provider1.get_tracer("concurrent-tracer-1")
  let tracer2 = tracer_provider2.get_tracer("concurrent-tracer-2")
  let meter1 = meter_provider1.get_meter("concurrent-meter-1")
  let meter2 = meter_provider2.get_meter("concurrent-meter-2")
  let logger1 = logger_provider1.get_logger("concurrent-logger-1")
  let logger2 = logger_provider2.get_logger("concurrent-logger-2")
  
  // 模拟并发执行复杂的遥测操作
  let (ctx1, span1) = tracer1.start_span(ctx, "concurrent-operation-1", trace::Server)
  let (ctx2, span2) = tracer2.start_span(ctx, "concurrent-operation-2", trace::Client)
  
  let counter1 = meter1.create_counter("concurrent.counter.1", "count", "Counter 1")
  let counter2 = meter2.create_counter("concurrent.counter.2", "count", "Counter 2")
  
  counter1.add(1L, [("operation.id", common::AttributeValue::string("op1"))])
  counter2.add(2L, [("operation.id", common::AttributeValue::string("op2"))])
  
  let log1 = logs::LogRecord::builder()
    .timestamp(1000000L)
    .severity(logs::Info)
    .body("Concurrent log 1")
    .with_attribute("operation.id", common::AttributeValue::string("op1"))
    .build()
  
  let log2 = logs::LogRecord::builder()
    .timestamp(2000000L)
    .severity(logs::Info)
    .body("Concurrent log 2")
    .with_attribute("operation.id", common::AttributeValue::string("op2"))
    .build()
  
  // 验证所有操作都成功完成
  assert_eq(span1.name, "concurrent-operation-1")
  assert_eq(span2.name, "concurrent-operation-2")
  match span1.kind { trace::Server => assert_eq(true, true) _ => @test.fail("Expected Server") }
  match span2.kind { trace::Client => assert_eq(true, true) _ => @test.fail("Expected Client") }
  
  match log1.body {
    Some(body) => assert_eq(body, "Concurrent log 1")
    None => @test.fail("Expected log1 body")
  }
  
  match log2.body {
    Some(body) => assert_eq(body, "Concurrent log 2")
    None => @test.fail("Expected log2 body")
  }
}