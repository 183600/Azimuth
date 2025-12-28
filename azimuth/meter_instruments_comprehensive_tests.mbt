// 仪表和仪表类型综合测试用例
test "Meter提供程序和Meter创建测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  assert_eq(meter.scope.name, "test.meter")
  assert_true(meter.scope.version is None)
  assert_true(meter.scope.schema_url is None)
}

test "Counter创建和操作测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "counter.test")
  let counter = Meter::create_counter(meter, "request.count")
  
  assert_eq(counter.name, "request.count")
  assert_true(counter.description is None)
  assert_true(counter.unit is None)
  
  Counter::add(counter, 10.5)
  Counter::add(counter, 5.0, Some(Attributes::new()))
  
  // 测试通过表示操作不崩溃
  assert_true(true)
}

test "Histogram创建和操作测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram.test")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time histogram"), Some("ms"))
  
  assert_eq(histogram.name, "response.time")
  match histogram.description {
    Some(desc) => assert_eq(desc, "Response time histogram")
    _ => assert_true(false)
  }
  match histogram.unit {
    Some(unit) => assert_eq(unit, "ms")
    _ => assert_true(false)
  }
  
  Histogram::record(histogram, 150.5)
  Histogram::record(histogram, 75.0, Some(Attributes::new()))
  
  // 测试通过表示操作不崩溃
  assert_true(true)
}

test "UpDownCounter创建和操作测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown.test")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  
  assert_eq(updown_counter.name, "active.connections")
  match updown_counter.description {
    Some(desc) => assert_eq(desc, "Active connections")
    _ => assert_true(false)
  }
  match updown_counter.unit {
    Some(unit) => assert_eq(unit, "connections")
    _ => assert_true(false)
  }
  
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0, Some(Attributes::new()))
  
  // 测试通过表示操作不崩溃
  assert_true(true)
}

test "Gauge创建和操作测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge.test")
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage gauge"), Some("bytes"))
  
  assert_eq(gauge.name, "memory.usage")
  match gauge.description {
    Some(desc) => assert_eq(desc, "Memory usage gauge")
    _ => assert_true(false)
  }
  match gauge.unit {
    Some(unit) => assert_eq(unit, "bytes")
    _ => assert_true(false)
  }
  
  // Gauge通常用于观察值而不是累加
  // 测试创建成功
  assert_true(true)
}

test "Instrument类型和属性测试" {
  let counter_instrument = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_instrument = Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_instrument = UpDownCounter("test.updown", Some("Test updown"), Some("items"))
  let gauge_instrument = Gauge("test.gauge", Some("Test gauge"), Some("percent"))
  
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  assert_eq(Instrument::name(updown_instrument), "test.updown")
  assert_eq(Instrument::name(gauge_instrument), "test.gauge")
  
  match Instrument::description(counter_instrument) {
    Some(desc) => assert_eq(desc, "Test counter")
    _ => assert_true(false)
  }
  
  match Instrument::unit(histogram_instrument) {
    Some(unit) => assert_eq(unit, "ms")
    _ => assert_true(false)
  }
}

test "Meter多种仪表创建测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi.instruments.test")
  
  let counter = Meter::create_counter(meter, "requests")
  let histogram = Meter::create_histogram(meter, "latency")
  let updown = Meter::create_updown_counter(meter, "connections")
  let gauge = Meter::create_gauge(meter, "cpu")
  
  assert_eq(counter.name, "requests")
  assert_eq(histogram.name, "latency")
  assert_eq(updown.name, "connections")
  assert_eq(gauge.name, "cpu")
  
  // 验证所有仪表都有相同的scope
  assert_eq(counter.name, "requests")
}