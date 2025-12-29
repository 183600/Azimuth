test "meter_and_instruments" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("test-meter", "1.0.0", "http://example.com/schema")
  
  // 测试创建各种指标
  let counter = meter.create_counter("test-counter", "count", "A test counter")
  let histogram = meter.create_histogram("test-histogram", "ms", "A test histogram")
  let up_down_counter = meter.create_up_down_counter("test-up-down", "bytes", "A test up-down counter")
  let gauge = meter.create_gauge("test-gauge", "percent", "A test gauge")
  
  // 测试调用指标方法（确保不会崩溃）
  counter.add(10L, [("tag", AttributeValue::string("value"))])
  histogram.record(100.5, [("tag", AttributeValue::string("value"))])
  up_down_counter.add(-5L, [("tag", AttributeValue::string("value"))])
  gauge.record(75.0, [("tag", AttributeValue::string("value"))])
  
  assert_eq(true, true) // 如果没有崩溃，测试通过
}

test "measurement_with_various_attributes" {
  let attributes = [
    ("method", AttributeValue::string("GET")),
    ("status", AttributeValue::int(200L)),
    ("latency", AttributeValue::float(123.4)),
    ("cache.hit", AttributeValue::bool(true))
  ]
  
  let measurement = Measurement::{
    value: 42.0,
    attributes
  }
  
  assert_eq(measurement.value, 42.0)
  assert_eq(measurement.attributes.length(), 4)
  
  // 验证各种属性类型
  match measurement.attributes[0].1 {
    AttributeValue::StringValue(s) => assert_eq(s, "GET")
    _ => @test.fail("Expected StringValue")
  }
  
  match measurement.attributes[1].1 {
    AttributeValue::IntValue(i) => assert_eq(i, 200L)
    _ => @test.fail("Expected IntValue")
  }
  
  match measurement.attributes[2].1 {
    AttributeValue::FloatValue(f) => assert_eq(f, 123.4)
    _ => @test.fail("Expected FloatValue")
  }
  
  match measurement.attributes[3].1 {
    AttributeValue::BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
}