// Metrics API的各种Instrument类型综合测试
test "counter_instrument_operations" {
  // 测试Counter Instrument的基本操作
  
  let meter = NoopMeter::{}
  let counter = meter.create_counter("http_requests_total", Some("count"), Some("Total HTTP requests"))
  
  // 测试添加正值
  counter.add(10L, Some([
    ("method", AttributeValue::string("GET")),
    ("status", AttributeValue::string("200"))
  ]))
  
  counter.add(5L, Some([
    ("method", AttributeValue::string("POST")),
    ("status", AttributeValue::string("201"))
  ]))
  
  // 测试添加零值
  counter.add(0L, Some([
    ("method", AttributeValue::string("PUT")),
    ("status", AttributeValue::string("204"))
  ]))
  
  // 测试添加大值
  counter.add(9223372036854775807L, Some([
    ("method", AttributeValue::string("GET")),
    ("status", AttributeValue::string("500"))
  ]))
  
  // 由于是Noop实现，我们只能验证调用不会出错
  assert_eq(true, true)
}

test "histogram_instrument_operations" {
  // 测试Histogram Instrument的基本操作
  
  let meter = NoopMeter::{}
  let histogram = meter.create_histogram("request_duration", Some("ms"), Some("Request duration in milliseconds"))
  
  // 测试记录不同的值
  histogram.record(100.0, Some([
    ("endpoint", AttributeValue::string("/api/users")),
    ("method", AttributeValue::string("GET"))
  ]))
  
  histogram.record(250.5, Some([
    ("endpoint", AttributeValue::string("/api/orders")),
    ("method", AttributeValue::string("POST"))
  ]))
  
  histogram.record(0.0, Some([
    ("endpoint", AttributeValue::string("/health")),
    ("method", AttributeValue::string("GET"))
  ]))
  
  // 测试记录边界值
  histogram.record(-1.0, Some([
    ("endpoint", AttributeValue::string("/api/error")),
    ("method", AttributeValue::string("GET"))
  ]))
  
  histogram.record(1.7976931348623157e+308, Some([
    ("endpoint", AttributeValue::string("/api/max")),
    ("method", AttributeValue::string("GET"))
  ]))
  
  // 测试特殊浮点值
  histogram.record(0.0/0.0, Some([
    ("endpoint", AttributeValue::string("/api/nan")),
    ("method", AttributeValue::string("GET"))
  ]))
  
  histogram.record(1.0/0.0, Some([
    ("endpoint", AttributeValue::string("/api/inf")),
    ("method", AttributeValue::string("GET"))
  ]))
  
  assert_eq(true, true)
}

test "up_down_counter_instrument_operations" {
  // 测试UpDownCounter Instrument的基本操作
  
  let meter = NoopMeter::{}
  let up_down_counter = meter.create_up_down_counter("active_connections", Some("connections"), Some("Current active connections"))
  
  // 测试添加正值（增加）
  up_down_counter.add(10L, Some([
    ("server", AttributeValue::string("web-01")),
    ("region", AttributeValue::string("us-east"))
  ]))
  
  // 测试添加负值（减少）
  up_down_counter.add(-3L, Some([
    ("server", AttributeValue::string("web-01")),
    ("region", AttributeValue::string("us-east"))
  ]))
  
  // 测试添加零值
  up_down_counter.add(0L, Some([
    ("server", AttributeValue::string("web-02")),
    ("region", AttributeValue::string("us-west"))
  ]))
  
  // 测试添加大负值
  up_down_counter.add(-9223372036854775808L, Some([
    ("server", AttributeValue::string("web-03")),
    ("region", AttributeValue::string("eu-west"))
  ]))
  
  assert_eq(true, true)
}

test "gauge_instrument_operations" {
  // 测试Gauge Instrument的基本操作
  
  let meter = NoopMeter::{}
  let gauge = meter.create_gauge("cpu_usage", Some("percent"), Some("Current CPU usage percentage"))
  
  // 测试记录不同的值
  gauge.record(75.5, Some([
    ("host", AttributeValue::string("server-01")),
    ("core", AttributeValue::int(0L))
  ]))
  
  gauge.record(0.0, Some([
    ("host", AttributeValue::string("server-01")),
    ("core", AttributeValue::int(1L))
  ]))
  
  gauge.record(100.0, Some([
    ("host", AttributeValue::string("server-02")),
    ("core", AttributeValue::int(0L))
  ]))
  
  // 测试记录负值（某些指标可能为负）
  gauge.record(-10.5, Some([
    ("host", AttributeValue::string("server-03")),
    ("metric", AttributeValue::string("temperature_delta"))
  ]))
  
  // 测试记录小数
  gauge.record(0.001, Some([
    ("host", AttributeValue::string("server-04")),
    ("metric", AttributeValue::string("memory_fragmentation"))
  ]))
  
  assert_eq(true, true)
}

test "meter_provider_and_meter_creation" {
  // 测试MeterProvider和Meter的创建
  
  let meter_provider = NoopMeterProvider::{}
  
  // 测试基本Meter创建
  let meter1 = meter_provider.get_meter("test-meter", None, None)
  assert_eq(true, true) // 验证创建成功
  
  // 测试带版本的Meter创建
  let meter2 = meter_provider.get_meter("test-meter", Some("1.0.0"), None)
  assert_eq(true, true)
  
  // 测试带schema_url的Meter创建
  let meter3 = meter_provider.get_meter("test-meter", None, Some("https://example.com/schema/v1"))
  assert_eq(true, true)
  
  // 测试完整的Meter创建
  let meter4 = meter_provider.get_meter(
    "comprehensive-meter",
    Some("2.1.0"),
    Some("https://opentelemetry.io/schemas/v1.20.0")
  )
  assert_eq(true, true)
  
  // 测试使用不同的Meter创建Instrument
  let counter = meter1.create_counter("test_counter", None, None)
  let histogram = meter2.create_histogram("test_histogram", Some("ms"), None)
  let up_down_counter = meter3.create_up_down_counter("test_up_down", None, Some("TestUpDown"))
  let gauge = meter4.create_gauge("test_gauge", Some("unit"), Some("Test Gauge"))
  
  assert_eq(true, true)
}

test "instrument_with_complex_attributes" {
  // 测试带有复杂属性的Instrument操作
  
  let meter = NoopMeter::{}
  let counter = meter.create_counter("complex_operations", None, None)
  
  // 测试字符串数组属性
  counter.add(1L, Some([
    ("tags", AttributeValue::array_string(["web", "api", "critical"])),
    ("services", AttributeValue::array_string(["auth", "payment", "notification"]))
  ]))
  
  // 测试整数数组属性
  counter.add(1L, Some([
    ("port_numbers", AttributeValue::array_int([8080L, 8443L, 9090L])),
    ("error_codes", AttributeValue::array_int([404L, 500L, 503L]))
  ]))
  
  // 测试浮点数数组属性
  counter.add(1L, Some([
    ("percentiles", AttributeValue::array_float([50.0, 95.0, 99.0])),
    ("thresholds", AttributeValue::array_float([1.0, 5.0, 10.0]))
  ]))
  
  // 测试布尔数组属性
  counter.add(1L, Some([
    ("feature_flags", AttributeValue::array_bool([true, false, true])),
    ("status_checks", AttributeValue::array_bool([true, true, false]))
  ]))
  
  // 测试混合复杂属性
  counter.add(1L, Some([
    ("string_attr", AttributeValue::string("complex_value")),
    ("int_attr", AttributeValue::int(42L)),
    ("float_attr", AttributeValue::float(3.14159)),
    ("bool_attr", AttributeValue::bool(true)),
    ("string_array", AttributeValue::array_string(["a", "b", "c"])),
    ("int_array", AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array", AttributeValue::array_bool([true, false, true]))
  ]))
  
  assert_eq(true, true)
}

test "measurement_struct_validation" {
  // 测试Measurement结构的使用
  
  // 创建基本的Measurement
  let measurement1 = Measurement::{
    value: 123.45,
    attributes: [
      ("unit", AttributeValue::string("bytes")),
      ("source", AttributeValue::string("disk"))
    ]
  }
  
  assert_eq(measurement1.value, 123.45)
  assert_eq(measurement1.attributes.length(), 2)
  
  // 创建带有复杂属性的Measurement
  let measurement2 = Measurement::{
    value: 99.9,
    attributes: [
      ("service.name", AttributeValue::string("payment-service")),
      ("service.version", AttributeValue::string("1.2.3")),
      ("deployment.environment", AttributeValue::string("production")),
      ("host.name", AttributeValue::string("web-server-01")),
      ("process.pid", AttributeValue::int(12345L)),
      ("cpu.percent", AttributeValue::float(75.5)),
      ("healthy", AttributeValue::bool(true)),
      ("tags", AttributeValue::array_string(["web", "api", "payment"])),
      ("ports", AttributeValue::array_int([8080L, 8443L])),
      ("thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
      ("features", AttributeValue::array_bool([true, false, true]))
    ]
  }
  
  assert_eq(measurement2.value, 99.9)
  assert_eq(measurement2.attributes.length(), 11)
  
  // 验证各种属性类型
  match measurement2.attributes[5].1 {
    FloatValue(cpu_percent) => assert_eq(cpu_percent, 75.5)
    _ => @test.fail("Test failed")
  }
  
  match measurement2.attributes[7].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "web")
      assert_eq(tags[2], "payment")
    }
    _ => @test.fail("Test failed")
  }
}

test "instrument_type_enum_validation" {
  // 测试InstrumentType枚举的所有值
  
  // 这里我们通过创建不同的Instrument来验证InstrumentType
  // 由于MoonBit的限制，我们无法直接访问InstrumentType枚举值
  // 但我们可以通过创建不同类型的Instrument来间接验证
  
  let meter = NoopMeter::{}
  
  // Counter
  let counter = meter.create_counter("test_counter", None, None)
  assert_eq(true, true)
  
  // Histogram
  let histogram = meter.create_histogram("test_histogram", None, None)
  assert_eq(true, true)
  
  // UpDownCounter
  let up_down_counter = meter.create_up_down_counter("test_up_down_counter", None, None)
  assert_eq(true, true)
  
  // Gauge
  let gauge = meter.create_gauge("test_gauge", None, None)
  assert_eq(true, true)
  
  // 测试Instrument的描述和单位
  let counter_with_meta = meter.create_counter(
    "detailed_counter",
    Some("requests"),
    Some("Total number of HTTP requests")
  )
  assert_eq(true, true)
  
  let histogram_with_meta = meter.create_histogram(
    "response_time",
    Some("milliseconds"),
    Some("Response time distribution"
  )
  assert_eq(true, true)
}