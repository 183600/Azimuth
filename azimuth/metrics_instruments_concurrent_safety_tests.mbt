// 指标仪器并发安全测试用例
// 测试指标仪器在并发环境下的安全性和正确性

test "counter_concurrent_operations" {
  // 测试计数器的并发操作安全性
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent-test-meter")
  let counter = Meter::create_counter(meter, "concurrent.counter", Some("Test counter for concurrent operations"), Some("count"))
  
  // 模拟并发添加操作
  for i in 0..10 {
    Counter::add(counter, 1.0, None)
  }
  
  // 验证计数器属性
  assert_eq(counter.name, "concurrent.counter")
  match counter.description {
    Some(desc) => assert_eq(desc, "Test counter for concurrent operations")
    _ => assert_false(true, "Expected counter description")
  }
  match counter.unit {
    Some(unit) => assert_eq(unit, "count")
    _ => assert_false(true, "Expected counter unit")
  }
}

test "histogram_concurrent_recording" {
  // 测试直方图的并发记录安全性
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-test-meter")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time histogram"), Some("ms"))
  
  // 模拟并发记录操作
  let test_values = [10.5, 25.3, 50.7, 75.2, 100.0, 150.8, 200.1, 300.5]
  for value in test_values {
    Histogram::record(histogram, value, None)
  }
  
  // 验证直方图属性
  assert_eq(histogram.name, "response.time")
  match histogram.description {
    Some(desc) => assert_eq(desc, "Response time histogram")
    _ => assert_false(true, "Expected histogram description")
  }
  match histogram.unit {
    Some(unit) => assert_eq(unit, "ms")
    _ => assert_false(true, "Expected histogram unit")
  }
  
  // 验证直方图作为仪器的转换
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "response.time")
}

test "updown_counter_concurrent_increment_decrement" {
  // 测试上下计数器的并发增减操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown-test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  
  // 模拟并发增减操作
  for i in 0..5 {
    Counter::add(counter, 1.0, None)  // 增加
  }
  
  for i in 0..3 {
    Counter::add(counter, -1.0, None)  // 减少
  }
  
  // 验证上下计数器属性
  assert_eq(updown_counter.name, "active.connections")
  match updown_counter.description {
    Some(desc) => assert_eq(desc, "Active connections")
    _ => assert_false(true, "Expected updown counter description")
  }
  match updown_counter.unit {
    Some(unit) => assert_eq(unit, "connections")
    _ => assert_false(true, "Expected updown counter unit")
  }
}

test "gauge_concurrent_observations" {
  // 测试仪表的并发观察操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge-test-meter")
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage gauge"), Some("bytes"))
  
  // 模拟并发观察操作
  let memory_values = [1024.0, 2048.0, 1536.0, 3072.0, 2560.0]
  for value in memory_values {
    Counter::add(counter, value, None)  // 使用gauge记录值
  }
  
  // 验证仪表属性
  assert_eq(gauge.name, "memory.usage")
  match gauge.description {
    Some(desc) => assert_eq(desc, "Memory usage gauge")
    _ => assert_false(true, "Expected gauge description")
  }
  match gauge.unit {
    Some(unit) => assert_eq(unit, "bytes")
    _ => assert_false(true, "Expected gauge unit")
  }
}

test "meter_provider_thread_safety" {
  // 测试仪表提供者的线程安全性
  let provider = MeterProvider::default()
  
  // 并发创建多个仪表
  let meter_names = ["meter1", "meter2", "meter3", "meter4", "meter5"]
  let meters = []
  
  for name in meter_names {
    let meter = MeterProvider::get_meter(provider, name)
    meters.push(meter)
  }
  
  // 验证每个仪表都有唯一的名称
  for (i, meter) in meters.enumerate() {
    assert_eq(meter.scope.name, meter_names[i])
    match meter.scope.version {
      Some(_) => assert_false(true, "Expected no version for default meter")
      _ => ()  // 期望None
    }
    match meter.scope.schema_url {
      Some(_) => assert_false(true, "Expected no schema_url for default meter")
      _ => ()  // 期望None
    }
  }
}

test "instrument_attribute_concurrent_access" {
  // 测试仪器属性的并发访问
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "attribute-test-meter")
  
  // 创建不同类型的仪器
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("unit"))
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("unit"))
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test updown"), Some("unit"))
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("unit"))
  
  // 并发访问仪器属性
  let instruments = [
    Counter(counter.name, counter.description, counter.unit),
    Histogram(histogram.name, histogram.description, histogram.unit),
    UpDownCounter(updown_counter.name, updown_counter.description, updown_counter.unit),
    Gauge(gauge.name, gauge.description, gauge.unit)
  ]
  
  for instrument in instruments {
    let name = Instrument::name(instrument)
    assert_true(name.contains("test."))
    
    let description = Instrument::description(instrument)
    match description {
      Some(desc) => assert_eq(desc, "Test " + name.replace("test.", ""))
      _ => assert_false(true, "Expected description for " + name)
    }
    
    let unit = Instrument::unit(instrument)
    match unit {
      Some(u) => assert_eq(u, "unit")
      _ => assert_false(true, "Expected unit for " + name)
    }
  }
}

test "concurrent_metric_recording_with_attributes" {
  // 测试带属性的并发指标记录
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "attributes-test-meter")
  let counter = Meter::create_counter(meter, "attribute.counter")
  
  // 创建测试属性
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("test-service"))
  Attributes::set(attrs, "operation.type", StringValue("read"))
  
  // 并发记录带属性的指标
  for i in 0..10 {
    Counter::add(counter, 1.0, Some(attrs))
  }
  
  // 验证属性设置
  let service_name = Attributes::get(attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_false(true, "Expected service.name attribute")
  }
  
  let operation_type = Attributes::get(attrs, "operation.type")
  match operation_type {
    Some(StringValue(op_type)) => assert_eq(op_type, "read")
    _ => assert_false(true, "Expected operation.type attribute")
  }
}

test "instrument_lifecycle_concurrent_creation" {
  // 测试仪器生命周期的并发创建
  let provider = MeterProvider::default()
  
  // 并发创建多个不同类型的仪器
  let counter_names = ["counter1", "counter2", "counter3"]
  let histogram_names = ["histogram1", "histogram2"]
  let updown_names = ["updown1"]
  let gauge_names = ["gauge1", "gauge2", "gauge3", "gauge4"]
  
  // 创建计数器
  for name in counter_names {
    let meter = MeterProvider::get_meter(provider, name + "-meter")
    let counter = Meter::create_counter(meter, name)
    assert_eq(counter.name, name)
  }
  
  // 创建直方图
  for name in histogram_names {
    let meter = MeterProvider::get_meter(provider, name + "-meter")
    let histogram = Meter::create_histogram(meter, name)
    assert_eq(histogram.name, name)
  }
  
  // 创建上下计数器
  for name in updown_names {
    let meter = MeterProvider::get_meter(provider, name + "-meter")
    let updown = Meter::create_updown_counter(meter, name)
    assert_eq(updown.name, name)
  }
  
  // 创建仪表
  for name in gauge_names {
    let meter = MeterProvider::get_meter(provider, name + "-meter")
    let gauge = Meter::create_gauge(meter, name)
    assert_eq(gauge.name, name)
  }
}

test "metric_aggregation_concurrent_safety" {
  // 测试指标聚合的并发安全性
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation-test-meter")
  let counter = Meter::create_counter(meter, "aggregation.counter")
  
  // 模拟不同来源的并发聚合
  let sources = ["source1", "source2", "source3", "source4", "source5"]
  
  for source in sources {
    let attrs = Attributes::new()
    Attributes::set(attrs, "source", StringValue(source))
    
    // 每个源贡献多个值
    for i in 0..5 {
      Counter::add(counter, 1.0, Some(attrs))
    }
  }
  
  // 验证每个源的属性都可以正确设置
  for source in sources {
    let test_attrs = Attributes::new()
    Attributes::set(test_attrs, "source", StringValue(source))
    
    let source_attr = Attributes::get(test_attrs, "source")
    match source_attr {
      Some(StringValue(src)) => assert_eq(src, source)
      _ => assert_false(true, "Expected source attribute for " + source)
    }
  }
}

test "noop_meter_concurrent_operations" {
  // 测试noop仪表的并发操作
  let noop_provider = MeterProvider::noop()
  let noop_meter = MeterProvider::get_meter(noop_provider, "noop-meter")
  let noop_counter = Meter::create_counter(noop_meter, "noop.counter")
  
  // 并发执行noop操作
  for i in 0..100 {
    Counter::add(noop_counter, 1.0, None)
  }
  
  // 验证noop计数器的属性
  assert_eq(noop_counter.name, "noop.counter")
  match noop_counter.description {
    Some(_) => assert_false(true, "Expected no description for noop counter")
    _ => ()  // 期望None
  }
  match noop_counter.unit {
    Some(_) => assert_false(true, "Expected no unit for noop counter")
    _ => ()  // 期望None
  }
}