// Metrics模块指标仪器测试
// 测试各种指标仪器的创建和使用

test "counter_creation_and_usage" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("test_meter", Some("1.0.0"))
  let counter = meter.create_counter("http_requests", Some("count"), Some("Total HTTP requests"))
  
  // 测试计数器操作（Noop实现不会实际计数）
  counter.add(1L)
  counter.add(5L, Some([("method", AttributeValue::string("GET"))]))
  
  @test.succeed()  // 如果没有异常就算成功
}

test "histogram_creation_and_usage" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("test_meter")
  let histogram = meter.create_histogram("response_time", Some("ms"), Some("Response time in milliseconds"))
  
  // 测试直方图操作
  histogram.record(100.0)
  histogram.record(250.5, Some([("endpoint", AttributeValue::string("/api/users"))]))
  
  @test.succeed()
}

test "up_down_counter_creation_and_usage" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("test_meter")
  let up_down_counter = meter.create_up_down_counter("active_connections", Some("connections"), Some("Current active connections"))
  
  // 测试上下计数器操作
  up_down_counter.add(10L)
  up_down_counter.add(-3L, Some([("server", AttributeValue::string("web-01"))]))
  
  @test.succeed()
}

test "gauge_creation_and_usage" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("test_meter")
  let gauge = meter.create_gauge("memory_usage", Some("bytes"), Some("Current memory usage"))
  
  // 测试仪表操作
  gauge.record(1024.0 * 1024.0 * 512.0)  // 512MB
  gauge.record(2.147483648e9, Some([("type", AttributeValue::string("heap"))]))  // ~2GB
  
  @test.succeed()
}

test "meter_multiple_instruments" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("multi_instrument_meter", Some("2.0.0"), Some("http://example.com/schema"))
  
  let counter = meter.create_counter("operations")
  let histogram = meter.create_histogram("duration", Some("s"))
  let up_down_counter = meter.create_up_down_counter("queue_size")
  let gauge = meter.create_gauge("cpu_usage", Some("%"))
  
  // 使用所有仪器
  counter.add(100L)
  histogram.record(0.5)
  up_down_counter.add(25L)
  gauge.record(75.5)
  
  @test.succeed()
}

test "instrument_with_various_attributes" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("attribute_test_meter")
  let counter = meter.create_counter("api_calls")
  
  let complex_attributes = [
    ("service", AttributeValue::string("user-service")),
    ("version", AttributeValue::string("v1.2.3")),
    ("status", AttributeValue::int(200L)),
    ("cache_hit", AttributeValue::bool(true)),
    ("latency_ms", AttributeValue::float(45.7)),
    ("tags", AttributeValue::array_string(["api", "public", "authenticated"])),
    ("retry_count", AttributeValue::array_int([1L, 2L, 3L])),
    ("percentiles", AttributeValue::array_float([50.0, 90.0, 99.0])),
    ("flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  counter.add(1L, Some(complex_attributes))
  
  @test.succeed()
}

test "measurement_structure_validation" {
  // 测试Measurement结构体的使用
  let measurement = Measurement::{
    value: 42.5,
    attributes: [
      ("unit", AttributeValue::string("seconds")),
      ("category", AttributeValue::string("performance"))
    ]
  }
  
  assert_eq(measurement.value, 42.5)
  assert_eq(measurement.attributes.length(), 2)
  match measurement.attributes[0] {
    (key, StringValue(value)) => {
      assert_eq(key, "unit")
      assert_eq(value, "seconds")
    }
    _ => @test.fail("Expected string attribute")
  }
}

test "instrument_type_coverage" {
  // 测试所有仪器类型
  let instrument_types = [
    Counter,
    Histogram,
    UpDownCounter,
    Gauge,
    ObservableCounter,
    ObservableGauge,
    ObservableUpDownCounter
  ]
  
  let mut index = 0
  while index < instrument_types.length() {
    let instrument_type = instrument_types[index]
    // 验证仪器类型可以被正确识别
    match instrument_type {
      Counter => @test.succeed()
      Histogram => @test.succeed()
      UpDownCounter => @test.succeed()
      Gauge => @test.succeed()
      ObservableCounter => @test.succeed()
      ObservableGauge => @test.succeed()
      ObservableUpDownCounter => @test.succeed()
    }
    index = index + 1
  }
}