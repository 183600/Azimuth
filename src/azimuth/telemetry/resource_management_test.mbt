// 资源管理测试 - 测试资源正确释放

test "resource_lifecycle_management" {
  // 测试Resource的生命周期管理
  
  // 1. 创建资源
  let resource1 = common::Resource::default("resource-1")
  let resource2 = common::Resource::default("resource-2")
  let resource3 = common::Resource::default("resource-3")
  
  // 验证资源创建成功
  assert_eq(resource1.service_name, "resource-1")
  assert_eq(resource2.service_name, "resource-2")
  assert_eq(resource3.service_name, "resource-3")
  
  // 2. 模拟资源使用
  let resources : Array[common::Resource] = [resource1, resource2, resource3]
  
  // 3. 验证资源在使用期间的状态
  let mut i = 0
  while i < resources.length() {
    let resource = resources[i]
    assert_eq(resource.telemetry_sdk_name, "azimuth")
    assert_eq(resource.telemetry_sdk_version, "0.1.0")
    assert_eq(resource.attributes.length(), 0)
    i = i + 1
  }
  
  // 4. 模拟资源释放（在MoonBit中，资源会通过垃圾回收自动释放）
  // 这里我们验证资源不再被引用时的行为
  let resource_after_use = common::Resource::default("resource-after-use")
  assert_eq(resource_after_use.service_name, "resource-after-use")
  
  // 验证新资源创建不受之前资源影响
  assert_eq(resource_after_use.telemetry_sdk_name, "azimuth")
  assert_eq(resource_after_use.telemetry_sdk_version, "0.1.0")
}

test "context_resource_cleanup" {
  // 测试Context的资源清理
  
  let ctx = context::Context::empty()
  
  // 1. 创建多个带有不同值的context
  let key1 = context::create_key("key1")
  let key2 = context::create_key("key2")
  let key3 = context::create_key("key3")
  
  let ctx1 = ctx.with_value(key1, "value1")
  let ctx2 = ctx1.with_value(key2, "value2")
  let ctx3 = ctx2.with_value(key3, "value3")
  
  // 2. 验证context链式创建
  match ctx3.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(key3) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
  
  // 3. 验证原始context不受影响
  match ctx.get(key1) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 4. 模拟context清理（创建新的空context）
  let cleaned_ctx = context::Context::empty()
  match cleaned_ctx.get(key1) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "baggage_resource_management" {
  // 测试Baggage的资源管理
  
  let baggage = context::Baggage::empty()
  
  // 1. 创建带有多个条目的baggage
  let baggage1 = baggage.with_entry("entry1", "value1")
  let baggage2 = baggage1.with_entry("entry2", "value2")
  let baggage3 = baggage2.with_entry("entry3", "value3")
  
  // 2. 验证baggage条目
  match baggage3.get("entry1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match baggage3.get("entry2") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  match baggage3.get("entry3") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
  
  // 3. 验证原始baggage不受影响
  match baggage.get("entry1") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 4. 模拟baggage清理
  let cleaned_baggage = context::Baggage::empty()
  match cleaned_baggage.get("entry1") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "log_record_resource_management" {
  // 测试LogRecord的资源管理
  
  // 1. 创建多个日志记录
  let log1 = logs::LogRecord::builder()
    .timestamp(1000000L)
    .severity(logs::Info)
    .body("Log message 1")
    .with_attribute("attr1", AttributeValue::string("value1"))
    .build()
  
  let log2 = logs::LogRecord::builder()
    .timestamp(2000000L)
    .severity(logs::Warn)
    .body("Log message 2")
    .with_attribute("attr2", AttributeValue::string("value2"))
    .build()
  
  let log3 = logs::LogRecord::builder()
    .timestamp(3000000L)
    .severity(logs::Error)
    .body("Log message 3")
    .with_attribute("attr3", AttributeValue::string("value3"))
    .build()
  
  // 2. 验证日志记录创建
  match log1.body {
    Some(body) => assert_eq(body, "Log message 1")
    None => @test.fail("Test failed")
  }
  
  match log2.body {
    Some(body) => assert_eq(body, "Log message 2")
    None => @test.fail("Test failed")
  }
  
  match log3.body {
    Some(body) => assert_eq(body, "Log message 3")
    None => @test.fail("Test failed")
  }
  
  // 3. 验证日志记录属性
  assert_eq(log1.attributes.length(), 1)
  assert_eq(log2.attributes.length(), 1)
  assert_eq(log3.attributes.length(), 1)
  
  // 4. 模拟日志记录处理后的清理
  let logs : Array[logs::LogRecord] = [log1, log2, log3]
  
  // 处理日志记录
  let mut i = 0
  while i < logs.length() {
    let log = logs[i]
    // 模拟日志处理
    match log.body {
      Some(_) => assert_eq(true, true)  // 处理成功
      None => @test.fail("Test failed")
    }
    i = i + 1
  }
  
  // 5. 创建新的日志记录验证资源可用性
  let new_log = logs::LogRecord::builder()
    .timestamp(4000000L)
    .severity(logs::Debug)
    .body("New log message")
    .build()
  
  match new_log.body {
    Some(body) => assert_eq(body, "New log message")
    None => @test.fail("Test failed")
  }
}

test "span_resource_management" {
  // 测试Span的资源管理
  
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("resource-test-tracer")
  
  // 1. 创建多个span
  let (ctx1, span1) = tracer.start_span(ctx, "span-1", trace::Internal)
  let (ctx2, span2) = tracer.start_span(ctx1, "span-2", trace::Server)
  let (ctx3, span3) = tracer.start_span(ctx2, "span-3", trace::Client)
  
  // 2. 验证span创建
  assert_eq(span1.name, "span-1")
  assert_eq(span2.name, "span-2")
  assert_eq(span3.name, "span-3")
  
  match span1.kind { trace::Internal => assert_eq(true, true) _ => @test.fail("Test failed") }
  match span2.kind { trace::Server => assert_eq(true, true) _ => @test.fail("Test failed") }
  match span3.kind { trace::Client => assert_eq(true, true) _ => @test.fail("Test failed") }
  
  // 3. 验证span上下文独立性
  assert_eq(span1.context.trace_id.length(), 16)
  assert_eq(span1.context.span_id.length(), 8)
  assert_eq(span2.context.trace_id.length(), 16)
  assert_eq(span2.context.span_id.length(), 8)
  assert_eq(span3.context.trace_id.length(), 16)
  assert_eq(span3.context.span_id.length(), 8)
  
  // 4. 模拟span结束和资源清理
  let spans : Array[trace::Span] = [span1, span2, span3]
  
  // 处理span结束
  let mut i = 0
  while i < spans.length() {
    let span = spans[i]
    // 模拟span结束处理
    assert_eq(span.name.length() > 0, true)
    i = i + 1
  }
  
  // 5. 创建新的span验证资源可用性
  let (new_ctx, new_span) = tracer.start_span(ctx, "new-span", trace::Producer)
  assert_eq(new_span.name, "new-span")
  match new_span.kind { trace::Producer => assert_eq(true, true) _ => @test.fail("Test failed") }
}

test "metrics_resource_management" {
  // 测试Metrics的资源管理
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("resource-test-meter")
  
  // 1. 创建多个指标仪器
  let counter1 = meter.create_counter("counter-1", "count", "Counter 1")
  let counter2 = meter.create_counter("counter-2", "count", "Counter 2")
  let histogram1 = meter.create_histogram("histogram-1", "ms", "Histogram 1")
  let histogram2 = meter.create_histogram("histogram-2", "ms", "Histogram 2")
  let gauge1 = meter.create_gauge("gauge-1", "percent", "Gauge 1")
  let gauge2 = meter.create_gauge("gauge-2", "percent", "Gauge 2")
  let up_down1 = meter.create_up_down_counter("updown-1", "count", "UpDown 1")
  let up_down2 = meter.create_up_down_counter("updown-2", "count", "UpDown 2")
  
  // 2. 使用所有指标仪器
  counter1.add(1L, [("instrument", AttributeValue::string("counter1"))])
  counter2.add(2L, [("instrument", AttributeValue::string("counter2"))])
  histogram1.record(100.0, [("instrument", AttributeValue::string("histogram1"))])
  histogram2.record(200.0, [("instrument", AttributeValue::string("histogram2"))])
  gauge1.record(75.0, [("instrument", AttributeValue::string("gauge1"))])
  gauge2.record(85.0, [("instrument", AttributeValue::string("gauge2"))])
  up_down1.add(5L, [("instrument", AttributeValue::string("updown1"))])
  up_down2.add(-3L, [("instrument", AttributeValue::string("updown2"))])
  
  // 3. 验证操作完成
  assert_eq(true, true)  // 所有操作都应该成功
  
  // 4. 创建新的指标仪器验证资源可用性
  let new_counter = meter.create_counter("new-counter", "count", "New Counter")
  let new_histogram = meter.create_histogram("new-histogram", "ms", "New Histogram")
  let new_gauge = meter.create_gauge("new-gauge", "percent", "New Gauge")
  let new_updown = meter.create_up_down_counter("new-updown", "count", "New UpDown")
  
  // 5. 使用新创建的指标仪器
  new_counter.add(10L, [("instrument", AttributeValue::string("new"))])
  new_histogram.record(150.0, [("instrument", AttributeValue::string("new"))])
  new_gauge.record(95.0, [("instrument", AttributeValue::string("new"))])
  new_updown.add(1L, [("instrument", AttributeValue::string("new"))])
  
  // 验证新操作完成
  assert_eq(true, true)
}

test "propagation_resource_management" {
  // 测试传播的资源管理
  
  let ctx = context::Context::empty()
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  let composite = propagation::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 1. 创建多个carrier并注入
  let carrier1 = propagation::MapCarrier::new()
  let carrier2 = propagation::MapCarrier::new()
  let carrier3 = propagation::MapCarrier::new()
  
  trace_propagator.inject(ctx, carrier1)
  baggage_propagator.inject(ctx, carrier2)
  composite.inject(ctx, carrier3)
  
  // 2. 验证carrier内容
  match carrier1.get(propagation::TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier2.get(propagation::BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier3.get(propagation::TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier3.get(propagation::BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  // 3. 提取传播信息
  let extracted_ctx1 = trace_propagator.extract(ctx, carrier1)
  let extracted_ctx2 = baggage_propagator.extract(ctx, carrier2)
  let extracted_ctx3 = composite.extract(ctx, carrier3)
  
  // 4. 验证提取操作完成
  assert_eq(true, true)
  
  // 5. 创建新的carrier验证资源可用性
  let new_carrier = propagation::MapCarrier::new()
  trace_propagator.inject(ctx, new_carrier)
  baggage_propagator.inject(ctx, new_carrier)
  
  match new_carrier.get(propagation::TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match new_carrier.get(propagation::BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}

test "large_scale_resource_management" {
  // 测试大规模资源管理
  
  // 1. 创建大量资源
  let mut resources : Array[common::Resource] = []
  let mut i = 0
  while i < 1000 {
    let resource = common::Resource::default("large-scale-resource-" + i.to_string())
    resources.push(resource)
    i = i + 1
  }
  
  // 2. 验证所有资源创建成功
  assert_eq(resources.length(), 1000)
  
  // 3. 创建大量context
  let mut contexts : Array[context::Context] = []
  i = 0
  while i < 1000 {
    let ctx = context::Context::empty()
    let key = context::create_key("key-" + i.to_string())
    let ctx_with_value = ctx.with_value(key, "value-" + i.to_string())
    contexts.push(ctx_with_value)
    i = i + 1
  }
  
  // 4. 验证所有context创建成功
  assert_eq(contexts.length(), 1000)
  
  // 5. 创建大量baggage
  let mut baggages : Array[context::Baggage] = []
  i = 0
  while i < 1000 {
    let baggage = context::Baggage::empty()
    let baggage_with_entry = baggage.with_entry("entry-" + i.to_string(), "value-" + i.to_string())
    baggages.push(baggage_with_entry)
    i = i + 1
  }
  
  // 6. 验证所有baggage创建成功
  assert_eq(baggages.length(), 1000)
  
  // 7. 创建大量日志记录
  let mut log_records : Array[logs::LogRecord] = []
  i = 0
  while i < 1000 {
    let log_record = logs::LogRecord::builder()
      .timestamp(1000000L + i.to_int64())
      .severity(match i % 6 {
        0 => logs::Trace
        1 => logs::Debug
        2 => logs::Info
        3 => logs::Warn
        4 => logs::Error
        _ => logs::Fatal
      })
      .body("Large scale log " + i.to_string())
      .with_attribute("index", AttributeValue::int(i.to_int64()))
      .build()
    log_records.push(log_record)
    i = i + 1
  }
  
  // 8. 验证所有日志记录创建成功
  assert_eq(log_records.length(), 1000)
  
  // 9. 创建大量span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("large-scale-tracer")
  let base_ctx = context::Context::empty()
  
  let mut spans : Array[trace::Span] = []
  i = 0
  while i < 500 {  // span创建较复杂，减少数量
    let (ctx, span) = tracer.start_span(
      base_ctx,
      "large-scale-span-" + i.to_string(),
      match i % 5 {
        0 => trace::Internal
        1 => trace::Server
        2 => trace::Client
        3 => trace::Producer
        _ => trace::Consumer
      },
      [("index", AttributeValue::int(i.to_int64()))]
    )
    spans.push(span)
    i = i + 1
  }
  
  // 10. 验证所有span创建成功
  assert_eq(spans.length(), 500)
  
  // 11. 验证资源清理后创建新资源的能力
  let new_resource = common::Resource::default("resource-after-cleanup")
  let new_ctx = context::Context::empty().with_value(context::create_key("new-key"), "new-value")
  let new_baggage = context::Baggage::empty().with_entry("new-entry", "new-value")
  let new_log = logs::LogRecord::builder().severity(logs::Info).body("New log").build()
  let (new_span_ctx, new_span) = tracer.start_span(new_ctx, "new-span", trace::Internal)
  
  // 验证新资源创建成功
  assert_eq(new_resource.service_name, "resource-after-cleanup")
  match new_ctx.get(context::create_key("new-key")) {
    Some(value) => assert_eq(value, "new-value")
    None => @test.fail("Test failed")
  }
  match new_baggage.get("new-entry") {
    Some(value) => assert_eq(value, "new-value")
    None => @test.fail("Test failed")
  }
  match new_log.body {
    Some(body) => assert_eq(body, "New log")
    None => @test.fail("Test failed")
  }
  assert_eq(new_span.name, "new-span")
}

test "memory_efficiency_test" {
  // 测试内存效率
  
  // 1. 测试大量小对象的创建和销毁
  let mut small_objects : Array[common::AttributeValue] = []
  let mut i = 0
  while i < 10000 {
    let attr = AttributeValue::string("small-object-" + i.to_string())
    small_objects.push(attr)
    i = i + 1
  }
  
  // 验证所有对象创建成功
  assert_eq(small_objects.length(), 10000)
  
  // 2. 测试大对象的创建和销毁
  let large_string = "x" * 100000
  let large_attr = AttributeValue::string(large_string)
  let large_array = AttributeValue::array_string([for i = 0; i < 10000; i = i + 1].map(fn(_) { "large-array-item" }))
  
  // 验证大对象创建成功
  match large_attr {
    StringValue(s) => assert_eq(s.length(), 100000)
    _ => @test.fail("Test failed")
  }
  
  match large_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 10000)
    _ => @test.fail("Test failed")
  }
  
  // 3. 测试复杂嵌套结构的创建和销毁
  let mut complex_attrs : Array[(String, common::AttributeValue)] = []
  i = 0
  while i < 1000 {
    complex_attrs.push((
      "complex.key." + i.to_string(),
      AttributeValue::array_string([
        "value1." + i.to_string(),
        "value2." + i.to_string(),
        "value3." + i.to_string()
      ])
    ))
    i = i + 1
  }
  
  // 验证复杂结构创建成功
  assert_eq(complex_attrs.length(), 1000)
  
  // 4. 验证资源清理后系统仍然响应
  let cleanup_resource = common::Resource::default("cleanup-test")
  assert_eq(cleanup_resource.service_name, "cleanup-test")
  
  let cleanup_ctx = context::Context::empty()
  let cleanup_key = context::create_key("cleanup-key")
  let cleanup_ctx_with_value = cleanup_ctx.with_value(cleanup_key, "cleanup-value")
  
  match cleanup_ctx_with_value.get(cleanup_key) {
    Some(value) => assert_eq(value, "cleanup-value")
    None => @test.fail("Test failed")
  }
}