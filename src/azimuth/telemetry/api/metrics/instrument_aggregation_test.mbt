// API Metrics模块的指标聚合测试

test "instrument_types_creation" {
  // 测试各种仪器类型的创建
  
  // 测试Counter
  let counter = metrics::NoopCounter::{}
  counter.add(10L, [])
  counter.add(5L, [("operation.type", common::AttributeValue::string("read"))])
  
  // 测试Histogram
  let histogram = metrics::NoopHistogram::{}
  histogram.record(1.5, [])
  histogram.record(2.7, [("http.method", common::AttributeValue::string("GET"))])
  
  // 测试UpDownCounter
  let up_down_counter = metrics::NoopUpDownCounter::{}
  up_down_counter.add(3L, [])
  up_down_counter.add(-2L, [("queue.type", common::AttributeValue::string("priority"))])
  
  // 测试Gauge
  let gauge = metrics::NoopGauge::{}
  gauge.record(42.0, [])
  gauge.record(15.7, [("metric.type", common::AttributeValue::string("temperature"))])
}

test "meter_instrument_creation" {
  // 测试Meter创建各种仪器
  
  let meter = metrics::NoopMeter::{}
  
  // 创建Counter
  let counter1 = meter.create_counter("http_requests_total", None, None)
  let counter2 = meter.create_counter("http_requests_total", Some("count"), None)
  let counter3 = meter.create_counter("http_requests_total", None, Some("Total HTTP requests"))
  let counter4 = meter.create_counter("http_requests_total", Some("count"), Some("Total HTTP requests"))
  
  // 创建Histogram
  let histogram1 = meter.create_histogram("http_request_duration", None, None)
  let histogram2 = meter.create_histogram("http_request_duration", Some("seconds"), None)
  let histogram3 = meter.create_histogram("http_request_duration", None, Some("HTTP request duration"))
  let histogram4 = meter.create_histogram("http_request_duration", Some("seconds"), Some("HTTP request duration"))
  
  // 创建UpDownCounter
  let up_down_counter1 = meter.create_up_down_counter("active_connections", None, None)
  let up_down_counter2 = meter.create_up_down_counter("active_connections", Some("connections"), None)
  let up_down_counter3 = meter.create_up_down_counter("active_connections", None, Some("Active network connections"))
  let up_down_counter4 = meter.create_up_down_counter("active_connections", Some("connections"), Some("Active network connections"))
  
  // 创建Gauge
  let gauge1 = meter.create_gauge("cpu_usage", None, None)
  let gauge2 = meter.create_gauge("cpu_usage", Some("percent"), None)
  let gauge3 = meter.create_gauge("cpu_usage", None, Some("Current CPU usage percentage"))
  let gauge4 = meter.create_gauge("cpu_usage", Some("percent"), Some("Current CPU usage percentage"))
  
  // 验证所有仪器都可以调用方法而不崩溃
  counter1.add(1L, [])
  counter2.add(2L, [("method", common::AttributeValue::string("GET"))])
  
  histogram1.record(1.0, [])
  histogram2.record(2.5, [("status", common::AttributeValue::int(200L))])
  
  up_down_counter1.add(1L, [])
  up_down_counter2.add(-1L, [("connection.type", common::AttributeValue::string("tcp"))])
  
  gauge1.record(50.0, [])
  gauge2.record(75.5, [("core", common::AttributeValue::int(0L))])
}

test "meter_provider_functionality" {
  // 测试MeterProvider的功能
  
  let provider = metrics::NoopMeterProvider::{}
  
  // 创建不同配置的Meter
  let meter1 = provider.get_meter("test.meter", None, None)
  let meter2 = provider.get_meter("test.meter", Some("1.0.0"), None)
  let meter3 = provider.get_meter("test.meter", None, Some("https://example.com/schema"))
  let meter4 = provider.get_meter("test.meter", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 验证所有Meter都可以创建仪器
  let counter1 = meter1.create_counter("test.counter", None, None)
  let histogram2 = meter2.create_histogram("test.histogram", None, None)
  let up_down_counter3 = meter3.create_up_down_counter("test.up_down_counter", None, None)
  let gauge4 = meter4.create_gauge("test.gauge", None, None)
  
  // 验证所有仪器都可以调用方法而不崩溃
  counter1.add(10L, [])
  histogram2.record(1.5, [])
  up_down_counter3.add(5L, [])
  gauge4.record(25.0, [])
}

test "measurement_creation_and_validation" {
  // 测试Measurement的创建和验证
  
  // 创建基本Measurement
  let measurement1 = metrics::Measurement::{
    value: 42.0,
    attributes: []
  }
  
  assert_eq(measurement1.value, 42.0)
  assert_eq(measurement1.attributes.length(), 0)
  
  // 创建带属性的Measurement
  let measurement2 = metrics::Measurement::{
    value: 123.45,
    attributes: [
      ("attribute1", common::AttributeValue::string("value1")),
      ("attribute2", common::AttributeValue::int(42L)),
      ("attribute3", common::AttributeValue::bool(true)),
      ("attribute4", common::AttributeValue::float(3.14))
    ]
  }
  
  assert_eq(measurement2.value, 123.45)
  assert_eq(measurement2.attributes.length(), 4)
  
  // 验证属性
  assert_eq(measurement2.attributes[0].0, "attribute1")
  match measurement2.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "value1")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(measurement2.attributes[1].0, "attribute2")
  match measurement2.attributes[1].1 {
    common::IntValue(i) => assert_eq(i, 42L)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(measurement2.attributes[2].0, "attribute3")
  match measurement2.attributes[2].1 {
    common::BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(measurement2.attributes[3].0, "attribute4")
  match measurement2.attributes[3].1 {
    common::FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "measurement_with_array_attributes" {
  // 测试带有数组属性的Measurement
  
  // 创建带有数组属性的Measurement
  let measurement = metrics::Measurement::{
    value: 100.0,
    attributes: [
      ("string.array", common::AttributeValue::array_string(["GET", "POST", "PUT"])),
      ("int.array", common::AttributeValue::array_int([200L, 404L, 500L])),
      ("float.array", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
      ("bool.array", common::AttributeValue::array_bool([true, false, true]))
    ]
  }
  
  assert_eq(measurement.value, 100.0)
  assert_eq(measurement.attributes.length(), 4)
  
  // 验证字符串数组属性
  assert_eq(measurement.attributes[0].0, "string.array")
  match measurement.attributes[0].1 {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "GET")
      assert_eq(arr[1], "POST")
      assert_eq(arr[2], "PUT")
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证整数数组属性
  assert_eq(measurement.attributes[1].0, "int.array")
  match measurement.attributes[1].1 {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 200L)
      assert_eq(arr[1], 404L)
      assert_eq(arr[2], 500L)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证浮点数数组属性
  assert_eq(measurement.attributes[2].0, "float.array")
  match measurement.attributes[2].1 {
    common::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证布尔数组属性
  assert_eq(measurement.attributes[3].0, "bool.array")
  match measurement.attributes[3].1 {
    common::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "counter_with_various_attributes" {
  // 测试带有各种属性的Counter
  
  let counter = metrics::NoopCounter::{}
  
  // 基本属性
  counter.add(1L, [
    ("http.method", common::AttributeValue::string("GET"))
  ])
  
  // 多个属性
  counter.add(2L, [
    ("http.method", common::AttributeValue::string("POST")),
    ("http.status_code", common::AttributeValue::int(201L)),
    ("http.success", common::AttributeValue::bool(true))
  ])
  
  // 数组属性
  counter.add(3L, [
    ("http.methods", common::AttributeValue::array_string(["GET", "POST"])),
    ("status.codes", common::AttributeValue::array_int([200L, 201L]))
  ])
  
  // 复杂属性组合
  counter.add(4L, [
    ("service.name", common::AttributeValue::string("api-service")),
    ("service.version", common::AttributeValue::string("1.2.3")),
    ("service.instance.id", common::AttributeValue::string("instance-123")),
    ("deployment.environment", common::AttributeValue::string("production")),
    ("feature.flags", common::AttributeValue::array_bool([true, false])),
    ("response.times", common::AttributeValue::array_float([1.1, 2.2, 3.3]))
  ])
}

test "histogram_with_various_values" {
  // 测试带有各种值的Histogram
  
  let histogram = metrics::NoopHistogram::{}
  
  // 整数值
  histogram.record(1.0, [])
  histogram.record(2.0, [])
  histogram.record(3.0, [])
  
  // 浮点值
  histogram.record(1.5, [])
  histogram.record(2.7, [])
  histogram.record(3.14, [])
  
  // 带属性的值
  histogram.record(1.1, [
    ("http.method", common::AttributeValue::string("GET"))
  ])
  
  histogram.record(2.2, [
    ("http.method", common::AttributeValue::string("POST")),
    ("http.status_code", common::AttributeValue::int(200L))
  ])
  
  // 极值
  histogram.record(0.001, [])
  histogram.record(999999.999, [])
  
  // 负值（如果允许）
  histogram.record(-1.0, [
    ("test.negative", common::AttributeValue::bool(true))
  ])
}

test "up_down_counter_with_positive_and_negative" {
  // 测试UpDownCounter的正负值
  
  let up_down_counter = metrics::NoopUpDownCounter::{}
  
  // 正值
  up_down_counter.add(10L, [])
  up_down_counter.add(5L, [("operation", common::AttributeValue::string("increment"))])
  
  // 负值
  up_down_counter.add(-3L, [])
  up_down_counter.add(-7L, [("operation", common::AttributeValue::string("decrement"))])
  
  // 零值
  up_down_counter.add(0L, [])
  
  // 交替增减
  up_down_counter.add(15L, [("type", common::AttributeValue::string("batch"))])
  up_down_counter.add(-5L, [("type", common::AttributeValue::string("batch"))])
  up_down_counter.add(3L, [("type", common::AttributeValue::string("batch"))])
  
  // 带复杂属性的值
  up_down_counter.add(1L, [
    ("queue.name", common::AttributeValue::string("priority-queue")),
    ("queue.size", common::AttributeValue::int(100L)),
    ("queue.capacity", common::AttributeValue::int(1000L)),
    ("queue.full", common::AttributeValue::bool(false))
  ])
}

test "gauge_with_fluctuating_values" {
  // 测试Gauge的波动值
  
  let gauge = metrics::NoopGauge::{}
  
  // 递增序列
  gauge.record(10.0, [])
  gauge.record(15.0, [])
  gauge.record(20.0, [])
  
  // 递减序列
  gauge.record(18.0, [])
  gauge.record(12.0, [])
  gauge.record(5.0, [])
  
  // 波动序列
  gauge.record(25.0, [])
  gauge.record(8.0, [])
  gauge.record(30.0, [])
  gauge.record(2.0, [])
  
  // 带属性的值
  gauge.record(50.0, [
    ("metric.type", common::AttributeValue::string("cpu")),
    ("metric.unit", common::AttributeValue::string("percent"))
  ])
  
  gauge.record(75.5, [
    ("metric.type", common::AttributeValue::string("memory")),
    ("metric.unit", common::AttributeValue::string("percent"))
  ])
  
  gauge.record(1024.0, [
    ("metric.type", common::AttributeValue::string("disk")),
    ("metric.unit", common::AttributeValue::string("megabytes"))
  ])
  
  // 带数组属性的值
  gauge.record(60.0, [
    ("server.names", common::AttributeValue::array_string(["server1", "server2"])),
    ("core.ids", common::AttributeValue::array_int([0L, 1L, 2L, 3L]))
  ])
}

test "instrument_type_enum_validation" {
  // 测试InstrumentType枚举的验证
  
  // 验证所有仪器类型
  let counter_type = metrics::Counter
  let histogram_type = metrics::Histogram
  let up_down_counter_type = metrics::UpDownCounter
  let gauge_type = metrics::Gauge
  let observable_counter_type = metrics::ObservableCounter
  let observable_gauge_type = metrics::ObservableGauge
  let observable_up_down_counter_type = metrics::ObservableUpDownCounter
  
  // 由于MoonBit的枚举特性，我们可以通过模式匹配来验证类型
  let validate_type = fn(t : metrics::InstrumentType) -> String {
    match t {
      Counter => "Counter"
      Histogram => "Histogram"
      UpDownCounter => "UpDownCounter"
      Gauge => "Gauge"
      ObservableCounter => "ObservableCounter"
      ObservableGauge => "ObservableGauge"
      ObservableUpDownCounter => "ObservableUpDownCounter"
    }
  }
  
  assert_eq(validate_type(counter_type), "Counter")
  assert_eq(validate_type(histogram_type), "Histogram")
  assert_eq(validate_type(up_down_counter_type), "UpDownCounter")
  assert_eq(validate_type(gauge_type), "Gauge")
  assert_eq(validate_type(observable_counter_type), "ObservableCounter")
  assert_eq(validate_type(observable_gauge_type), "ObservableGauge")
  assert_eq(validate_type(observable_up_down_counter_type), "ObservableUpDownCounter")
}