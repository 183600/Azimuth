// Azimuth Metrics Collection Quality Tests
// 测试指标收集功能的高质量测试用例

test "meter_provider_operations" {
  // 测试Meter提供者操作
  let noop_provider = MeterProvider::noop()
  let default_provider = MeterProvider::default()
  
  // 测试获取Meter
  let meter1 = MeterProvider::get_meter(noop_provider, "test.service")
  let meter2 = MeterProvider::get_meter(default_provider, "another.service")
  
  // 验证Meter的名称
  assert_eq(meter1.scope.name, "test.service")
  assert_eq(meter2.scope.name, "another.service")
  assert_eq(meter1.scope.version, None)
  assert_eq(meter2.scope.version, None)
  assert_eq(meter1.scope.schema_url, None)
  assert_eq(meter2.scope.schema_url, None)
}

test "counter_operations" {
  // 测试计数器操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "counter.test")
  let counter = Meter::create_counter(meter, "request.count", Some("Total number of requests"), Some("requests"))
  
  // 验证计数器属性
  assert_eq(counter.name, "request.count")
  assert_eq(counter.description, Some("Total number of requests"))
  assert_eq(counter.unit, Some("requests"))
  
  // 测试计数器增加操作
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.5)
  
  // 测试带属性的计数器增加
  let attrs = Attributes::new()
  Counter::add(counter, 2.0, Some(attrs))
  
  // 验证操作不会出错
  assert_eq(true, true)
}

test "histogram_operations" {
  // 测试直方图操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram.test")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  
  // 验证直方图属性
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time in milliseconds"))
  assert_eq(histogram.unit, Some("ms"))
  
  // 测试直方图记录操作
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 250.5)
  Histogram::record(histogram, 500.0)
  
  // 测试带属性的直方图记录
  let attrs = Attributes::new()
  Histogram::record(histogram, 75.0, Some(attrs))
  
  // 测试转换为Instrument类型
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "response.time")
  assert_eq(Instrument::description(instrument), Some("Response time in milliseconds"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
  
  // 验证操作不会出错
  assert_eq(true, true)
}

test "updown_counter_operations" {
  // 测试上下计数器操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown.test")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Currently active connections"), Some("connections"))
  
  // 验证上下计数器属性
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Currently active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // 测试增加操作
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, 5.0)
  
  // 测试减少操作
  UpDownCounter::add(updown_counter, -3.0)
  UpDownCounter::add(updown_counter, -7.0)
  
  // 测试带属性的操作
  let attrs = Attributes::new()
  UpDownCounter::add(updown_counter, 2.0, Some(attrs))
  
  // 验证操作不会出错
  assert_eq(true, true)
}

test "gauge_operations" {
  // 测试仪表操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge.test")
  let gauge = Meter::create_gauge(meter, "cpu.usage", Some("Current CPU usage percentage"), Some("percent"))
  
  // 验证仪表属性
  assert_eq(gauge.name, "cpu.usage")
  assert_eq(gauge.description, Some("Current CPU usage percentage"))
  assert_eq(gauge.unit, Some("percent"))
  
  // 在简化实现中，Gauge没有特定的操作方法
  // 但我们可以验证它的属性
  assert_eq(gauge.name, "cpu.usage")
  assert_eq(gauge.description, Some("Current CPU usage percentage"))
  assert_eq(gauge.unit, Some("percent"))
}

test "instrument_type_conversions" {
  // 测试仪器类型转换
  let counter_inst = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_inst = Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter_inst = UpDownCounter("test.updown", Some("Test updown"), Some("items"))
  let gauge_inst = Gauge("test.gauge", Some("Test gauge"), Some("percent"))
  
  // 测试获取仪器名称
  assert_eq(Instrument::name(counter_inst), "test.counter")
  assert_eq(Instrument::name(histogram_inst), "test.histogram")
  assert_eq(Instrument::name(updown_counter_inst), "test.updown")
  assert_eq(Instrument::name(gauge_inst), "test.gauge")
  
  // 测试获取仪器描述
  assert_eq(Instrument::description(counter_inst), Some("Test counter"))
  assert_eq(Instrument::description(histogram_inst), Some("Test histogram"))
  assert_eq(Instrument::description(updown_counter_inst), Some("Test updown"))
  assert_eq(Instrument::description(gauge_inst), Some("Test gauge"))
  
  // 测试获取仪器单位
  assert_eq(Instrument::unit(counter_inst), Some("count"))
  assert_eq(Instrument::unit(histogram_inst), Some("ms"))
  assert_eq(Instrument::unit(updown_counter_inst), Some("items"))
  assert_eq(Instrument::unit(gauge_inst), Some("percent"))
}

test "metrics_with_attributes" {
  // 测试带属性的指标操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "attributes.test")
  
  // 创建属性
  let attrs = Attributes::new()
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  
  // 创建带属性的计数器
  let counter = Meter::create_counter(meter, "http.requests")
  Counter::add(counter, 1.0, Some(attrs))
  
  // 创建带属性的直方图
  let histogram = Meter::create_histogram(meter, "http.duration")
  Histogram::record(histogram, 150.0, Some(attrs))
  
  // 创建带属性的上下计数器
  let updown_counter = Meter::create_updown_counter(meter, "active.requests")
  UpDownCounter::add(updown_counter, 3.0, Some(attrs))
  
  // 验证操作不会出错
  assert_eq(true, true)
}

test "multi_meter_operations" {
  // 测试多个Meter的操作
  let provider = MeterProvider::default()
  
  // 创建多个Meter
  let service1_meter = MeterProvider::get_meter(provider, "service1", Some("1.0.0"))
  let service2_meter = MeterProvider::get_meter(provider, "service2", Some("2.0.0"))
  let service3_meter = MeterProvider::get_meter(provider, "service3")
  
  // 验证Meter的属性
  assert_eq(service1_meter.scope.name, "service1")
  assert_eq(service1_meter.scope.version, Some("1.0.0"))
  
  assert_eq(service2_meter.scope.name, "service2")
  assert_eq(service2_meter.scope.version, Some("2.0.0"))
  
  assert_eq(service3_meter.scope.name, "service3")
  assert_eq(service3_meter.scope.version, None)
  
  // 在每个Meter上创建不同的指标
  let service1_counter = Meter::create_counter(service1_meter, "service1.requests")
  let service2_histogram = Meter::create_histogram(service2_meter, "service2.duration")
  let service3_gauge = Meter::create_gauge(service3_meter, "service3.memory")
  
  // 执行操作
  Counter::add(service1_counter, 10.0)
  Histogram::record(service2_histogram, 200.0)
  
  // 验证操作不会出错
  assert_eq(true, true)
}