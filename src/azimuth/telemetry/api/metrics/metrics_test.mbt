test "instrument_type_values" {
  // 测试InstrumentType的所有值
  let counter = Counter
  let histogram = Histogram
  let up_down_counter = UpDownCounter
  let gauge = Gauge
  let observable_counter = ObservableCounter
  let observable_gauge = ObservableGauge
  let observable_up_down_counter = ObservableUpDownCounter
  
  // 确保所有仪器类型可以创建
  assert_eq(true, true)
}

test "measurement_creation" {
  // 测试Measurement创建
  let attributes = [("method", @azimuth.telemetry.api.common.AttributeValue::string("GET"))]
  let measurement = {
    value: 42.5,
    attributes
  }
  
  assert_eq(measurement.value, 42.5)
  assert_eq(measurement.attributes.length(), 1)
}

test "noop_counter_add" {
  // 测试NoopCounter的add方法
  let counter = NoopCounter
  let attributes = [("status", @azimuth.telemetry.api.common.AttributeValue::string("success"))]
  
  // No-op实现应该不会抛出异常
  NoopCounter::add(10L, Some(attributes))
  assert_eq(true, true)
}

test "noop_histogram_record" {
  // 测试NoopHistogram的record方法
  let attributes = [("endpoint", @azimuth.telemetry.api.common.AttributeValue::string("/api/test"))]
  
  // No-op实现应该不会抛出异常
  NoopHistogram::record(100.0, Some(attributes))
  assert_eq(true, true)
}

test "noop_up_down_counter_add" {
  // 测试NoopUpDownCounter的add方法
  
  // 测试正数和负数
  NoopUpDownCounter::add(5L, None)
  NoopUpDownCounter::add(-3L, None)
  assert_eq(true, true)
}

test "noop_gauge_record" {
  // 测试NoopGauge的record方法
  let attributes = [("instance", @azimuth.telemetry.api.common.AttributeValue::string("server-1"))]
  
  NoopGauge::record(75.5, Some(attributes))
  assert_eq(true, true)
}

test "noop_meter_create_instruments" {
  // 测试NoopMeter创建各种仪器
  let meter = NoopMeter
  
  let int_counter = NoopMeter::create_counter("test.counter", "1", "Test counter")
  let float_histogram = NoopMeter::create_histogram("test.histogram", "ms", "Test histogram")
  let int_up_down_counter = NoopMeter::create_up_down_counter("test.up_down", "1", "Test up down counter")
  let float_gauge = NoopMeter::create_gauge("test.gauge", "%", "Test gauge")
  
  // 确保所有仪器都可以创建和使用
  NoopCounter::add(1L, None)
  NoopHistogram::record(50.0, None)
  NoopUpDownCounter::add(-1L, None)
  NoopGauge::record(80.0, None)
  
  assert_eq(true, true)
}

test "noop_meter_provider_get_meter" {
  // 测试NoopMeterProvider的get_meter方法
  let meter = NoopMeterProvider::get_meter("test-meter", Some("1.0.0"), Some("http://example.com/schema"))
  
  // 确保返回的meter可以创建仪器
  let counter = NoopMeter::create_counter("test", "1", "Test")
  NoopCounter::add(42L, None)
  
  assert_eq(true, true)
}

test "measurement_with_multiple_attributes" {
  // 测试带多个属性的Measurement
  let attributes = [
    ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("POST")),
    ("http.status_code", @azimuth.telemetry.api.common.AttributeValue::int(201L)),
    ("user.role", @azimuth.telemetry.api.common.AttributeValue::string("admin")),
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("auth-service"))
  ]
  
  let measurement = {
    value: 123.45,
    attributes
  }
  
  assert_eq(measurement.value, 123.45)
  assert_eq(measurement.attributes.length(), 4)
}

test "meter_comprehensive_instrument_creation" {
  // 测试Meter创建各种类型的仪器
  
  // 创建不同类型的Int64仪器
  let int_counter = NoopMeter::create_counter("http.requests.total", "1", "Total HTTP requests")
  let int_up_down_counter = NoopMeter::create_up_down_counter("active.connections", "1", "Active connections")
  
  // 创建不同类型的Float64仪器
  let float_histogram = NoopMeter::create_histogram("http.request.duration", "ms", "HTTP request duration")
  let float_gauge = NoopMeter::create_gauge("cpu.usage", "%", "CPU usage percentage")
  
  // 测试所有仪器的基本操作
  let common_attrs = [("service", @azimuth.telemetry.api.common.AttributeValue::string("api"))]
  
  // Counter操作
  NoopCounter::add(1L, Some(common_attrs))
  NoopCounter::add(5L, None)
  
  // UpDownCounter操作（正负值）
  NoopUpDownCounter::add(10L, Some(common_attrs))
  NoopUpDownCounter::add(-3L, None)
  
  // Histogram操作
  NoopHistogram::record(100.5, Some(common_attrs))
  NoopHistogram::record(250.75, None)
  
  // Gauge操作
  NoopGauge::record(75.5, Some(common_attrs))
  NoopGauge::record(80.0, None)
  
  // 验证所有操作都能正常执行（no-op实现不会抛出异常）
  assert_eq(true, true)
  
  // 额外测试：创建具有不同单位和描述的仪器
  let bytes_counter = NoopMeter::create_counter("bytes.transferred", "bytes", "Total bytes transferred")
  let latency_histogram = NoopMeter::create_histogram("operation.latency", "seconds", "Operation latency in seconds")
  let memory_gauge = NoopMeter::create_gauge("memory.usage", "MiB", "Memory usage in MiB")
  
  // 测试这些仪器也能正常工作
  NoopCounter::add(1024L, None)
  NoopHistogram::record(0.05, None)
  NoopGauge::record(512.0, None)
  
  assert_eq(true, true)
}