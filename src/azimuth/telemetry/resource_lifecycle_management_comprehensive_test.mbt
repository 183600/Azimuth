// Azimuth Telemetry - Resource Management and Lifecycle Test
// 资源管理和生命周期测试

test "resource_lifecycle_management" {
  // 资源生命周期管理测试
  
  // 测试Resource创建
  let basic_resource = common::Resource::default("test-service")
  assert_eq(basic_resource.service_name, "test-service")
  assert_eq(basic_resource.service_version, None)
  assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(basic_resource.attributes.length(), 0)
  
  // 测试带版本的Resource
  let versioned_resource = common::Resource::default("versioned-service")
  // 在实际实现中，这里应该能设置版本
  assert_eq(versioned_resource.service_name, "versioned-service")
  
  // 测试Resource属性管理
  let resource_with_attrs = common::Resource::default("attr-service")
  // 在实际实现中，这里应该能添加属性
  assert_eq(resource_with_attrs.service_name, "attr-service")
  
  // 测试Resource复制和共享
  let shared_resource1 = basic_resource
  let shared_resource2 = basic_resource
  assert_eq(shared_resource1.service_name, shared_resource2.service_name)
  assert_eq(shared_resource1.telemetry_sdk_name, shared_resource2.telemetry_sdk_name)
  
  // 测试Resource生命周期
  let mut i = 0
  while i < 10 {
    let temp_resource = common::Resource::default("temp-service-" + i.to_string())
    assert_eq(temp_resource.service_name, "temp-service-" + i.to_string())
    // temp_resource在循环结束时应该被自动清理
    i = i + 1
  }
  
  // 验证原始资源仍然有效
  assert_eq(basic_resource.service_name, "test-service")
}

test "instrumentation_scope_lifecycle" {
  // InstrumentationScope生命周期测试
  
  // 测试基本InstrumentationScope
  let basic_scope = common::InstrumentationScope::{
    name: "basic-scope",
    version: None,
    schema_url: None
  }
  assert_eq(basic_scope.name, "basic-scope")
  assert_eq(basic_scope.version, None)
  assert_eq(basic_scope.schema_url, None)
  
  // 测试完整InstrumentationScope
  let full_scope = common::InstrumentationScope::{
    name: "full-scope",
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/schema")
  }
  assert_eq(full_scope.name, "full-scope")
  assert_eq(full_scope.version, Some("1.0.0"))
  assert_eq(full_scope.schema_url, Some("http://example.com/schema"))
  
  // 测试InstrumentationScope共享
  let shared_scope1 = full_scope
  let shared_scope2 = full_scope
  assert_eq(shared_scope1.name, shared_scope2.name)
  assert_eq(shared_scope1.version, shared_scope2.version)
  assert_eq(shared_scope1.schema_url, shared_scope2.schema_url)
  
  // 测试InstrumentationScope生命周期
  let mut i = 0
  while i < 10 {
    let temp_scope = common::InstrumentationScope::{
      name: "temp-scope-" + i.to_string(),
      version: Some("1." + i.to_string() + ".0"),
      schema_url: Some("http://temp.example.com/" + i.to_string())
    }
    assert_eq(temp_scope.name, "temp-scope-" + i.to_string())
    assert_eq(temp_scope.version, Some("1." + i.to_string() + ".0"))
    // temp_scope在循环结束时应该被自动清理
    i = i + 1
  }
  
  // 验证原始scope仍然有效
  assert_eq(basic_scope.name, "basic-scope")
  assert_eq(full_scope.name, "full-scope")
}

test "provider_lifecycle_management" {
  // Provider生命周期管理测试
  
  // 测试TracerProvider创建和销毁
  let trace_provider = trace::NoopTracerProvider::{}
  let tracer1 = trace_provider.get_tracer("service-A")
  let tracer2 = trace_provider.get_tracer("service-B", Some("1.0.0"))
  
  assert_type(tracer1, trace::NoopTracer)
  assert_type(tracer2, trace::NoopTracer)
  
  // 测试MeterProvider创建和销毁
  let metrics_provider = metrics::NoopMeterProvider::{}
  let meter1 = metrics_provider.get_meter("service-A")
  let meter2 = metrics_provider.get_meter("service-B", Some("1.0.0"), Some("http://example.com/schema"))
  
  assert_type(meter1, metrics::NoopMeter)
  assert_type(meter2, metrics::NoopMeter)
  
  // 测试LoggerProvider创建和销毁
  let logs_provider = logs::NoopLoggerProvider::{}
  let logger1 = logs_provider.get_logger("service-A")
  let logger2 = logs_provider.get_logger("service-B", Some("1.0.0"), Some("http://example.com/schema"))
  
  assert_type(logger1, logs::NoopLogger)
  assert_type(logger2, logs::NoopLogger)
  
  // 测试Provider复用
  let reused_tracer = trace_provider.get_tracer("service-A")
  let reused_meter = metrics_provider.get_meter("service-A")
  let reused_logger = logs_provider.get_logger("service-A")
  
  assert_type(reused_tracer, trace::NoopTracer)
  assert_type(reused_meter, metrics::NoopMeter)
  assert_type(reused_logger, logs::NoopLogger)
  
  // 测试Provider生命周期
  let mut i = 0
  while i < 5 {
    let temp_trace_provider = trace::NoopTracerProvider::{}
    let temp_metrics_provider = metrics::NoopMeterProvider::{}
    let temp_logs_provider = logs::NoopLoggerProvider::{}
    
    let temp_tracer = temp_trace_provider.get_tracer("temp-service-" + i.to_string())
    let temp_meter = temp_metrics_provider.get_meter("temp-service-" + i.to_string())
    let temp_logger = temp_logs_provider.get_logger("temp-service-" + i.to_string())
    
    assert_type(temp_tracer, trace::NoopTracer)
    assert_type(temp_meter, metrics::NoopMeter)
    assert_type(temp_logger, logs::NoopLogger)
    
    // temp_providers在循环结束时应该被自动清理
    i = i + 1
  }
  
  // 验证原始providers仍然有效
  let final_tracer = trace_provider.get_tracer("final-test")
  let final_meter = metrics_provider.get_meter("final-test")
  let final_logger = logs_provider.get_logger("final-test")
  
  assert_type(final_tracer, trace::NoopTracer)
  assert_type(final_meter, metrics::NoopMeter)
  assert_type(final_logger, logs::NoopLogger)
}

test "instrument_lifecycle_management" {
  // Instrument生命周期管理测试
  
  let trace_provider = trace::NoopTracerProvider::{}
  let metrics_provider = metrics::NoopMeterProvider::{}
  let logs_provider = logs::NoopLoggerProvider::{}
  
  let tracer = trace_provider.get_tracer("instrument-test")
  let meter = metrics_provider.get_meter("instrument-test")
  let logger = logs_provider.get_logger("instrument-test")
  
  // 测试Trace Instruments生命周期
  let ctx = context::Context::empty()
  let mut spans = []
  
  // 创建多个span
  let mut i = 0
  while i < 10 {
    let (_, span) = tracer.start_span(ctx, "lifecycle-span-" + i.to_string())
    spans.push(span)
    i = i + 1
  }
  
  // 验证所有span都存在
  assert_eq(spans.length(), 10)
  let mut j = 0
  while j < spans.length() {
    assert_eq(spans[j].name, "lifecycle-span-" + j.to_string())
    j = j + 1
  }
  
  // 测试Metrics Instruments生命周期
  let counters = []
  let histograms = []
  let gauges = []
  let updown_counters = []
  
  // 创建多个指标
  let mut k = 0
  while k < 5 {
    counters.push(meter.create_counter("lifecycle-counter-" + k.to_string()))
    histograms.push(meter.create_histogram("lifecycle-histogram-" + k.to_string()))
    gauges.push(meter.create_gauge("lifecycle-gauge-" + k.to_string()))
    updown_counters.push(meter.create_up_down_counter("lifecycle-updown-" + k.to_string()))
    k = k + 1
  }
  
  // 验证所有指标都存在
  assert_eq(counters.length(), 5)
  assert_eq(histograms.length(), 5)
  assert_eq(gauges.length(), 5)
  assert_eq(updown_counters.length(), 5)
  
  // 使用指标
  let mut l = 0
  while l < counters.length() {
    counters[l].add(1L)
    histograms[l].record(1.0)
    gauges[l].record(1.0)
    updown_counters[l].add(1L)
    l = l + 1
  }
  
  // 测试LogRecords生命周期
  let log_records = []
  
  // 创建多个LogRecord
  let mut m = 0
  while m < 5 {
    let log_record = logs::LogRecord::builder()
      .severity(logs::Info)
      .body("Lifecycle log " + m.to_string())
      .with_attribute("iteration", common::AttributeValue::int(m.to_int64()))
      .build()
    log_records.push(log_record)
    m = m + 1
  }
  
  // 验证所有LogRecord都存在
  assert_eq(log_records.length(), 5)
  let mut n = 0
  while n < log_records.length() {
    match log_records[n].body {
      Some(body) => assert_eq(body, "Lifecycle log " + n.to_string())
      None => assert(false, "Expected log body")
    }
    n = n + 1
  }
  
  // 发送所有日志
  let mut o = 0
  while o < log_records.length() {
    logger.emit(log_records[o])
    o = o + 1
  }
}

test "context_lifecycle_management" {
  // Context生命周期管理测试
  
  // 测试Context创建和销毁
  let base_ctx = context::Context::empty()
  assert_eq(base_ctx.values.length(), 0)
  
  // 测试Context链式创建
  let ctx1 = base_ctx.with_value(context::create_key("key1"), "value1")
  let ctx2 = ctx1.with_value(context::create_key("key2"), "value2")
  let ctx3 = ctx2.with_value(context::create_key("key3"), "value3")
  
  // 验证Context链
  assert_eq(ctx3.get(context::create_key("key1")), Some("value1"))
  assert_eq(ctx3.get(context::create_key("key2")), Some("value2"))
  assert_eq(ctx3.get(context::create_key("key3")), Some("value3"))
  
  // 验证原始Context未改变
  assert_eq(base_ctx.get(context::create_key("key1")), None)
  assert_eq(ctx1.get(context::create_key("key2")), None)
  assert_eq(ctx2.get(context::create_key("key3")), None)
  
  // 测试Baggage生命周期
  let base_baggage = context::Baggage::empty()
  assert_eq(base_baggage.entries.length(), 0)
  
  let baggage1 = base_baggage.with_entry("baggage1", "value1")
  let baggage2 = baggage1.with_entry("baggage2", "value2")
  let baggage3 = baggage2.with_entry("baggage3", "value3")
  
  // 验证Baggage链
  assert_eq(baggage3.get("baggage1"), Some("value1"))
  assert_eq(baggage3.get("baggage2"), Some("value2"))
  assert_eq(baggage3.get("baggage3"), Some("value3"))
  
  // 验证原始Baggage未改变
  assert_eq(base_baggage.get("baggage1"), None)
  assert_eq(baggage1.get("baggage2"), None)
  assert_eq(baggage2.get("baggage3"), None)
  
  // 测试大量Context创建和销毁
  let mut i = 0
  while i < 100 {
    let temp_ctx = base_ctx.with_value(
      context::create_key("temp-key-" + i.to_string()),
      "temp-value-" + i.to_string()
    )
    // 验证temp_ctx
    assert_eq(temp_ctx.get(context::create_key("temp-key-" + i.to_string())), Some("temp-value-" + i.to_string()))
    // temp_ctx在循环结束时应该被自动清理
    i = i + 1
  }
  
  // 验证原始Context仍然有效
  assert_eq(ctx1.get(context::create_key("key1")), Some("value1"))
  assert_eq(baggage1.get("baggage1"), Some("value1"))
}

test "attribute_value_lifecycle" {
  // AttributeValue生命周期测试
  
  // 测试基本AttributeValue
  let string_value = common::AttributeValue::string("test-string")
  let int_value = common::AttributeValue::int(42L)
  let float_value = common::AttributeValue::float(3.14)
  let bool_value = common::AttributeValue::bool(true)
  
  // 验证基本值
  match string_value {
    common::StringValue(s) => assert_eq(s, "test-string")
    _ => assert(false, "Expected string value")
  }
  
  match int_value {
    common::IntValue(i) => assert_eq(i, 42L)
    _ => assert(false, "Expected int value")
  }
  
  match float_value {
    common::FloatValue(f) => assert_eq(f, 3.14)
    _ => assert(false, "Expected float value")
  }
  
  match bool_value {
    common::BoolValue(b) => assert_eq(b, true)
    _ => assert(false, "Expected bool value")
  }
  
  // 测试数组AttributeValue
  let string_array = common::AttributeValue::array_string(["a", "b", "c"])
  let int_array = common::AttributeValue::array_int([1L, 2L, 3L])
  let float_array = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = common::AttributeValue::array_bool([true, false, true])
  
  // 验证数组值
  match string_array {
    common::ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => assert(false, "Expected string array value")
  }
  
  match int_array {
    common::ArrayIntValue(arr) => assert_eq(arr.length(), 3)
    _ => assert(false, "Expected int array value")
  }
  
  match float_array {
    common::ArrayFloatValue(arr) => assert_eq(arr.length(), 3)
    _ => assert(false, "Expected float array value")
  }
  
  match bool_array {
    common::ArrayBoolValue(arr) => assert_eq(arr.length(), 3)
    _ => assert(false, "Expected bool array value")
  }
  
  // 测试大量AttributeValue创建和销毁
  let mut i = 0
  while i < 1000 {
    let temp_string = common::AttributeValue::string("temp-string-" + i.to_string())
    let temp_int = common::AttributeValue::int(i.to_int64())
    let temp_float = common::AttributeValue::float(i.to_double())
    let temp_bool = common::AttributeValue::bool(i % 2 == 0)
    
    // 验证临时值
    match temp_string {
      common::StringValue(s) => assert_eq(s, "temp-string-" + i.to_string())
      _ => assert(false, "Expected string value")
    }
    
    match temp_int {
      common::IntValue(int_val) => assert_eq(int_val, i.to_int64())
      _ => assert(false, "Expected int value")
    }
    
    match temp_float {
      common::FloatValue(float_val) => assert_eq(float_val, i.to_double())
      _ => assert(false, "Expected float value")
    }
    
    match temp_bool {
      common::BoolValue(bool_val) => assert_eq(bool_val, i % 2 == 0)
      _ => assert(false, "Expected bool value")
    }
    
    // temp_values在循环结束时应该被自动清理
    i = i + 1
  }
  
  // 验证原始值仍然有效
  match string_value {
    common::StringValue(s) => assert_eq(s, "test-string")
    _ => assert(false, "Expected string value")
  }
}

test "memory_management_simulation" {
  // 内存管理模拟测试
  
  // 模拟大量对象创建和销毁
  let mut i = 0
  while i < 100 {
    // 创建临时providers
    let temp_trace_provider = trace::NoopTracerProvider::{}
    let temp_metrics_provider = metrics::NoopMeterProvider::{}
    let temp_logs_provider = logs::NoopLoggerProvider::{}
    
    // 创建临时instruments
    let temp_tracer = temp_trace_provider.get_tracer("temp-tracer-" + i.to_string())
    let temp_meter = temp_metrics_provider.get_meter("temp-meter-" + i.to_string())
    let temp_logger = temp_logs_provider.get_logger("temp-logger-" + i.to_string())
    
    // 创建临时资源
    let temp_resource = common::Resource::default("temp-resource-" + i.to_string())
    let temp_scope = common::InstrumentationScope::{
      name: "temp-scope-" + i.to_string(),
      version: Some("1.0.0"),
      schema_url: None
    }
    
    // 创建临时context
    let temp_ctx = context::Context::empty()
    let temp_key = context::create_key("temp-key")
    let temp_enriched_ctx = temp_ctx.with_value(temp_key, "temp-value-" + i.to_string())
    
    // 创建临时spans
    let (_, temp_span) = temp_tracer.start_span(temp_enriched_ctx, "temp-span")
    
    // 创建临时metrics
    let temp_counter = temp_meter.create_counter("temp-counter")
    let temp_histogram = temp_meter.create_histogram("temp-histogram")
    temp_counter.add(1L)
    temp_histogram.record(1.0)
    
    // 创建临时logs
    temp_logger.info("Temp log " + i.to_string())
    
    // 创建临时attributes
    let temp_string_value = common::AttributeValue::string("temp-string-" + i.to_string())
    let temp_int_value = common::AttributeValue::int(i.to_int64())
    let temp_array_value = common::AttributeValue::array_string(["a", "b", "c"])
    
    // 验证临时对象
    assert_eq(temp_span.name, "temp-span")
    assert_eq(temp_resource.service_name, "temp-resource-" + i.to_string())
    assert_eq(temp_scope.name, "temp-scope-" + i.to_string())
    assert_eq(temp_enriched_ctx.get(temp_key), Some("temp-value-" + i.to_string()))
    
    match temp_string_value {
      common::StringValue(s) => assert_eq(s, "temp-string-" + i.to_string())
      _ => assert(false, "Expected string value")
    }
    
    // 所有临时对象在循环结束时应该被自动清理
    i = i + 1
  }
  
  // 验证内存清理后仍能正常创建对象
  let final_trace_provider = trace::NoopTracerProvider::{}
  let final_tracer = final_trace_provider.get_tracer("final-tracer")
  let final_ctx = context::Context::empty()
  let (_, final_span) = final_tracer.start_span(final_ctx, "final-span")
  
  assert_eq(final_span.name, "final-span")
}

test "resource_cleanup_verification" {
  // 资源清理验证测试
  
  // 创建大量资源
  let providers = []
  let tracers = []
  let meters = []
  let loggers = []
  let resources = []
  let scopes = []
  let contexts = []
  let attributes = []
  
  let mut i = 0
  while i < 50 {
    // 创建providers
    providers.push(trace::NoopTracerProvider::{})
    providers.push(metrics::NoopMeterProvider::{})
    providers.push(logs::NoopLoggerProvider::{})
    
    // 创建instruments
    let trace_provider = trace::NoopTracerProvider::{}
    let metrics_provider = metrics::NoopMeterProvider::{}
    let logs_provider = logs::NoopLoggerProvider::{}
    
    tracers.push(trace_provider.get_tracer("tracer-" + i.to_string()))
    meters.push(metrics_provider.get_meter("meter-" + i.to_string()))
    loggers.push(logs_provider.get_logger("logger-" + i.to_string()))
    
    // 创建资源
    resources.push(common::Resource::default("resource-" + i.to_string()))
    scopes.push(common::InstrumentationScope::{
      name: "scope-" + i.to_string(),
      version: Some("1.0.0"),
      schema_url: None
    })
    
    // 创建context
    let ctx = context::Context::empty()
    let key = context::create_key("key-" + i.to_string())
    contexts.push(ctx.with_value(key, "value-" + i.to_string()))
    
    // 创建attributes
    attributes.push(common::AttributeValue::string("attr-" + i.to_string()))
    attributes.push(common::AttributeValue::int(i.to_int64()))
    attributes.push(common::AttributeValue::array_string(["a", "b", "c"]))
    
    i = i + 1
  }
  
  // 验证资源创建成功
  assert_eq(providers.length(), 150)  // 50 * 3 providers
  assert_eq(tracers.length(), 50)
  assert_eq(meters.length(), 50)
  assert_eq(loggers.length(), 50)
  assert_eq(resources.length(), 50)
  assert_eq(scopes.length(), 50)
  assert_eq(contexts.length(), 50)
  assert_eq(attributes.length(), 150)  // 50 * 3 attributes
  
  // 使用部分资源
  let mut j = 0
  while j < 10 {
    let ctx = context::Context::empty()
    let (_, span) = tracers[j].start_span(ctx, "verification-span")
    meters[j].create_counter("verification-counter").add(1L)
    loggers[j].info("Verification log")
    j = j + 1
  }
  
  // 清理引用（在实际实现中，这里应该显式释放资源）
  // 由于MoonBit的垃圾回收机制，我们无法显式释放资源
  // 但我们可以将引用设为None或重新赋值
  
  // 验证部分资源仍然可用
  let final_provider = trace::NoopTracerProvider::{}
  let final_tracer = final_provider.get_tracer("verification-final")
  let final_ctx = context::Context::empty()
  let (_, final_span) = final_tracer.start_span(final_ctx, "final-verification")
  
  assert_eq(final_span.name, "final-verification")
}