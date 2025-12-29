// Metrics模块高级测试用例

test "measurement_complex_attributes" {
  // 测试带有复杂属性的Measurement
  let complex_attrs : Attributes = [
    ("service.name", AttributeValue::string("payment-service")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("http.method", AttributeValue::string("POST")),
    ("http.status_code", AttributeValue::int(200L)),
    ("response.size", AttributeValue::float(1024.5)),
    ("cache.hit", AttributeValue::bool(true)),
    ("endpoints", AttributeValue::array_string(["/api/pay", "/api/verify", "/api/status"])),
    ("status_codes", AttributeValue::array_int([200L, 201L, 400L, 500L])),
    ("response_times", AttributeValue::array_float([10.5, 25.3, 100.7, 250.2])),
    ("feature_flags", AttributeValue::array_bool([true, false, true, true]))
  ]
  
  let measurement = Measurement::{
    value: 42.5,
    attributes: complex_attrs
  }
  
  assert_eq(measurement.value, 42.5)
  assert_eq(measurement.attributes.length(), 10)
  
  // 验证字符串属性
  match measurement.attributes[0].1 {
    StringValue(name) => assert_eq(name, "payment-service")
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证整数属性
  match measurement.attributes[3].1 {
    IntValue(code) => assert_eq(code, 200L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 验证浮点数属性
  match measurement.attributes[4].1 {
    FloatValue(size) => assert_eq(size, 1024.5)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 验证布尔属性
  match measurement.attributes[5].1 {
    BoolValue(hit) => assert_eq(hit, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  // 验证字符串数组属性
  match measurement.attributes[6].1 {
    ArrayStringValue(endpoints) => {
      assert_eq(endpoints.length(), 3)
      assert_eq(endpoints[0], "/api/pay")
      assert_eq(endpoints[1], "/api/verify")
      assert_eq(endpoints[2], "/api/status")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "instrument_type_coverage" {
  // 测试所有仪器类型的创建和使用
  
  // 创建MeterProvider和Meter
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 测试Counter
  let counter = meter.create_counter("request_count", Some("requests"), Some("Total number of requests"))
  counter.add(1L, [("http.method", AttributeValue::string("GET"))])
  counter.add(5L, [("http.method", AttributeValue::string("POST"))])
  counter.add(0L, [("http.method", AttributeValue::string("PUT"))])
  counter.add(-1L, [("http.method", AttributeValue::string("DELETE"))])  // 测试负值
  
  // 测试Histogram
  let histogram = meter.create_histogram("response_time", Some("ms"), Some("Response time in milliseconds"))
  histogram.record(100.0, [("endpoint", AttributeValue::string("/api/users"))])
  histogram.record(0.0, [("endpoint", AttributeValue::string("/api/products"))])  // 测试零值
  histogram.record(9999.99, [("endpoint", AttributeValue::string("/api/orders"))])  // 测试大值
  
  // 测试UpDownCounter
  let up_down_counter = meter.create_up_down_counter("active_connections", Some("connections"), Some("Current active connections"))
  up_down_counter.add(10L, [("server", AttributeValue::string("web-01"))])
  up_down_counter.add(-5L, [("server", AttributeValue::string("web-01"))])  // 测试减少
  up_down_counter.add(0L, [("server", AttributeValue::string("web-02"))])   // 测试零值
  
  // 测试Gauge
  let gauge = meter.create_gauge("cpu_usage", Some("percent"), Some("CPU usage percentage"))
  gauge.record(75.5, [("instance", AttributeValue::string("server-01"))])
  gauge.record(0.0, [("instance", AttributeValue::string("server-02"))])    // 测试零值
  gauge.record(100.0, [("instance", AttributeValue::string("server-03"))])  // 测试最大值
  gauge.record(-1.0, [("instance", AttributeValue::string("server-04"))])   // 测试负值
  
  // 测试带有复杂属性的仪器
  let complex_attrs : Attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("process.pid", AttributeValue::int(12345L)),
    ("tags", AttributeValue::array_string(["web", "api", "critical"]))
  ]
  
  let complex_counter = meter.create_counter("complex_requests", Some("requests"), Some("Complex request counter"))
  complex_counter.add(1L, complex_attrs)
  
  // 测试空属性
  let empty_counter = meter.create_counter("empty_attrs", Some("count"), Some("Counter with empty attributes"))
  empty_counter.add(1L, [])
  
  // 测试None属性
  let none_counter = meter.create_counter("none_attrs", Some("count"), Some("Counter with None attributes"))
  none_counter.add(1L, None)
}