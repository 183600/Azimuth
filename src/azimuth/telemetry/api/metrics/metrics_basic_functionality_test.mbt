// Metrics API基础功能测试
test "InstrumentType enum values" {
  let counter_type = Counter
  let histogram_type = Histogram
  let up_down_counter_type = UpDownCounter
  let gauge_type = Gauge
  let observable_counter_type = ObservableCounter
  let observable_gauge_type = ObservableGauge
  let observable_up_down_counter_type = ObservableUpDownCounter
  
  assert match (counter_type) {
    Counter => true
    _ => false
  }
  
  assert match (histogram_type) {
    Histogram => true
    _ => false
  }
  
  assert match (up_down_counter_type) {
    UpDownCounter => true
    _ => false
  }
  
  assert match (gauge_type) {
    Gauge => true
    _ => false
  }
  
  assert match (observable_counter_type) {
    ObservableCounter => true
    _ => false
  }
  
  assert match (observable_gauge_type) {
    ObservableGauge => true
    _ => false
  }
  
  assert match (observable_up_down_counter_type) {
    ObservableUpDownCounter => true
    _ => false
  }
}

test "Measurement creation and validation" {
  let measurement = Measurement::{
    value: 42.5,
    attributes: [
      ("method", AttributeValue::string("GET")),
      ("status", AttributeValue::int(200L))
    ]
  }
  
  assert measurement.value == 42.5
  assert measurement.attributes.length == 2
  
  // 验证属性值
  assert match (measurement.attributes[0].1) {
    StringValue(s) => s == "GET"
    _ => false
  }
  
  assert match (measurement.attributes[1].1) {
    IntValue(i) => i == 200L
    _ => false
  }
}

test "Measurement with no attributes" {
  let measurement = Measurement::{
    value: 100.0,
    attributes: []
  }
  
  assert measurement.value == 100.0
  assert measurement.attributes.length == 0
}

test "NoopCounter functionality" {
  let counter = NoopCounter::{}
  
  // 测试add方法不会抛出异常
  counter.add(10L)
  counter.add(5L, attributes: [("test", AttributeValue::string("value"))])
  
  // No-op实现，无法验证状态，但可以确保方法调用不失败
  assert true
}

test "NoopHistogram functionality" {
  let histogram = NoopHistogram::{}
  
  // 测试record方法不会抛出异常
  histogram.record(25.5)
  histogram.record(10.0, attributes: [("bucket", AttributeValue::string("large"))])
  
  assert true
}

test "NoopUpDownCounter functionality" {
  let up_down_counter = NoopUpDownCounter::{}
  
  // 测试add方法不会抛出异常（包括负数）
  up_down_counter.add(15L)
  up_down_counter.add(-5L, attributes: [("direction", AttributeValue::string("down"))])
  
  assert true
}

test "NoopGauge functionality" {
  let gauge = NoopGauge::{}
  
  // 测试record方法不会抛出异常
  gauge.record(75.3)
  gauge.record(50.0, attributes: [("sensor", AttributeValue::string("temperature"))])
  
  assert true
}

test "NoopMeter instrument creation" {
  let meter = NoopMeter::{}
  
  let counter = meter.create_counter("test_counter", unit: "count", description: "Test counter")
  let histogram = meter.create_histogram("test_histogram", unit: "ms", description: "Test histogram")
  let up_down_counter = meter.create_up_down_counter("test_up_down", unit: "count", description: "Test up-down counter")
  let gauge = meter.create_gauge("test_gauge", unit: "percent", description: "Test gauge")
  
  // 验证所有instrument都可以创建
  assert match (counter) {
    NoopCounter => true
    _ => false
  }
  
  assert match (histogram) {
    NoopHistogram => true
    _ => false
  }
  
  assert match (up_down_counter) {
    NoopUpDownCounter => true
    _ => false
  }
  
  assert match (gauge) {
    NoopGauge => true
    _ => false
  }
}

test "NoopMeterProvider functionality" {
  let provider = NoopMeterProvider::{}
  
  let meter1 = provider.get_meter("test_meter")
  let meter2 = provider.get_meter("another_meter", version: Some("1.0.0"))
  let meter3 = provider.get_meter("schema_meter", schema_url: Some("https://example.com/schema"))
  
  // 验证返回的都是NoopMeter实例
  let counter1 = meter1.create_counter("test")
  let counter2 = meter2.create_counter("test")
  let counter3 = meter3.create_counter("test")
  
  assert match (counter1) {
    NoopCounter => true
    _ => false
  }
  
  assert match (counter2) {
    NoopCounter => true
    _ => false
  }
  
  assert match (counter3) {
    NoopCounter => true
    _ => false
  }
}

test "Meter instrument creation with minimal parameters" {
  let meter = NoopMeter::{}
  
  // 测试只用必需参数创建instrument
  let counter = meter.create_counter("minimal_counter")
  let histogram = meter.create_histogram("minimal_histogram")
  let up_down_counter = meter.create_up_down_counter("minimal_up_down")
  let gauge = meter.create_gauge("minimal_gauge")
  
  // 验证创建成功
  assert match (counter) {
    NoopCounter => true
    _ => false
  }
  
  assert match (histogram) {
    NoopHistogram => true
    _ => false
  }
  
  assert match (up_down_counter) {
    NoopUpDownCounter => true
    _ => false
  }
  
  assert match (gauge) {
    NoopGauge => true
    _ => false
  }
}