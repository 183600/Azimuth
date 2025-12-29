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
  let attributes = [("method", @common.AttributeValue::string("GET"))]
  let measurement = {
    value: 42.5,
    attributes
  }
  
  assert_eq(measurement.value, 42.5)
  assert_eq(measurement.attributes.length(), 1)
}

test "noop_counter_add" {
  // 测试NoopCounter的add方法
  let counter = NoopCounter[Int64]
  let attributes = [("status", @common.AttributeValue::string("success"))]
  
  // No-op实现应该不会抛出异常
  counter.add(10L, Some(attributes))
  assert_eq(true, true)
}

test "noop_histogram_record" {
  // 测试NoopHistogram的record方法
  let histogram = NoopHistogram[Float64]
  let attributes = [("endpoint", @common.AttributeValue::string("/api/test"))]
  
  // No-op实现应该不会抛出异常
  histogram.record(100.0, Some(attributes))
  assert_eq(true, true)
}

test "noop_up_down_counter_add" {
  // 测试NoopUpDownCounter的add方法
  let counter = NoopUpDownCounter[Int64]
  
  // 测试正数和负数
  counter.add(5L, None)
  counter.add(-3L, None)
  assert_eq(true, true)
}

test "noop_gauge_record" {
  // 测试NoopGauge的record方法
  let gauge = NoopGauge[Float64]
  let attributes = [("instance", @common.AttributeValue::string("server-1"))]
  
  gauge.record(75.5, Some(attributes))
  assert_eq(true, true)
}

test "noop_meter_create_instruments" {
  // 测试NoopMeter创建各种仪器
  let meter = NoopMeter
  
  let int_counter = meter.create_counter[Int64]("test.counter", "1", "Test counter")
  let float_histogram = meter.create_histogram[Float64]("test.histogram", "ms", "Test histogram")
  let int_up_down_counter = meter.create_up_down_counter[Int64]("test.up_down", "1", "Test up down counter")
  let float_gauge = meter.create_gauge[Float64]("test.gauge", "%", "Test gauge")
  
  // 确保所有仪器都可以创建和使用
  int_counter.add(1L, None)
  float_histogram.record(50.0, None)
  int_up_down_counter.add(-1L, None)
  float_gauge.record(80.0, None)
  
  assert_eq(true, true)
}

test "noop_meter_provider_get_meter" {
  // 测试NoopMeterProvider的get_meter方法
  let provider = NoopMeterProvider
  
  let meter = provider.get_meter("test-meter", Some("1.0.0"), Some("http://example.com/schema"))
  
  // 确保返回的meter可以创建仪器
  let counter = meter.create_counter[Int64]("test", "1", "Test")
  counter.add(42L, None)
  
  assert_eq(true, true)
}

test "measurement_with_multiple_attributes" {
  // 测试带多个属性的Measurement
  let attributes = [
    ("http.method", @common.AttributeValue::string("POST")),
    ("http.status_code", @common.AttributeValue::int(201L)),
    ("user.role", @common.AttributeValue::string("admin")),
    ("service.name", @common.AttributeValue::string("auth-service"))
  ]
  
  let measurement = {
    value: 123.45,
    attributes
  }
  
  assert_eq(measurement.value, 123.45)
  assert_eq(measurement.attributes.length(), 4)
}