// InstrumentType枚举的测试用例
// 测试指标仪器类型

test "instrument_type_all_values" {
  // 测试所有InstrumentType枚举值
  let counter_type = Counter
  let histogram_type = Histogram
  let up_down_counter_type = UpDownCounter
  let gauge_type = Gauge
  let observable_counter_type = ObservableCounter
  let observable_gauge_type = ObservableGauge
  let observable_up_down_counter_type = ObservableUpDownCounter
  
  // 虽然我们无法直接比较枚举值，但可以通过使用它们来验证
  // 在实际应用中，这些类型会用于创建不同的指标仪器
  
  // 创建测量值来测试不同类型的度量
  let counter_measurement = Measurement::{
    value: 10.0,
    attributes: [("instrument.type", AttributeValue::string("counter"))]
  }
  
  let histogram_measurement = Measurement::{
    value: 100.5,
    attributes: [("instrument.type", AttributeValue::string("histogram"))]
  }
  
  let gauge_measurement = Measurement::{
    value: 75.0,
    attributes: [("instrument.type", AttributeValue::string("gauge"))]
  }
  
  assert_eq(counter_measurement.value, 10.0)
  assert_eq(histogram_measurement.value, 100.5)
  assert_eq(gauge_measurement.value, 75.0)
  
  // 验证属性
  match counter_measurement.attributes[0].1 {
    AttributeValue::StringValue(s) => assert_eq(s, "counter")
    _ => @test.fail("Expected StringValue")
  }
}

test "meter_creates_all_instrument_types" {
  // 测试Meter创建所有类型的仪器
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("test-meter", "1.0.0")
  
  // 创建各种类型的仪器
  let counter = meter.create_counter("test.counter", "count", "A test counter")
  let histogram = meter.create_histogram("test.histogram", "ms", "A test histogram")
  let up_down_counter = meter.create_up_down_counter("test.up_down_counter", "bytes", "A test up-down counter")
  let gauge = meter.create_gauge("test.gauge", "percent", "A test gauge")
  
  // 验证仪器可以正常使用（不会崩溃）
  counter.add(10L)
  counter.add(5L, [("tag", AttributeValue::string("value"))])
  
  histogram.record(100.5)
  histogram.record(200.3, [("operation", AttributeValue::string("read"))])
  
  up_down_counter.add(-3L)
  up_down_counter.add(7L, [("direction", AttributeValue::string("up"))])
  
  gauge.record(75.0)
  gauge.record(25.5, [("status", AttributeValue::string("active"))])
  
  assert_eq(true, true) // 如果没有崩溃，测试通过
}

test "measurement_with_various_attribute_types" {
  // 测试包含各种属性类型的测量值
  let attributes = [
    ("string.attr", AttributeValue::string("test")),
    ("int.attr", AttributeValue::int(42L)),
    ("float.attr", AttributeValue::float(3.14)),
    ("bool.attr", AttributeValue::bool(true)),
    ("string.array", AttributeValue::array_string(["a", "b", "c"])),
    ("int.array", AttributeValue::array_int([1L, 2L, 3L])),
    ("float.array", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array", AttributeValue::array_bool([true, false, true]))
  ]
  
  let measurement = Measurement::{
    value: 123.45,
    attributes: attributes
  }
  
  assert_eq(measurement.value, 123.45)
  assert_eq(measurement.attributes.length(), 8)
  
  // 验证各种属性类型
  match measurement.attributes[0].1 {
    AttributeValue::StringValue(s) => assert_eq(s, "test")
    _ => @test.fail("Expected StringValue")
  }
  
  match measurement.attributes[1].1 {
    AttributeValue::IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match measurement.attributes[2].1 {
    AttributeValue::FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match measurement.attributes[3].1 {
    AttributeValue::BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match measurement.attributes[4].1 {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "instrument_edge_case_measurements" {
  // 测试边界情况的测量值
  
  // 零值和负值
  let zero_measurement = Measurement::{
    value: 0.0,
    attributes: [("type", AttributeValue::string("zero"))]
  }
  
  let negative_measurement = Measurement::{
    value: -42.5,
    attributes: [("type", AttributeValue::string("negative"))]
  }
  
  // 极大值和极小值
  let max_measurement = Measurement::{
    value: 1.7976931348623157e+308,  // Double最大值
    attributes: [("type", AttributeValue::string("max"))]
  }
  
  let min_measurement = Measurement::{
    value: -1.7976931348623157e+308, // Double最小值
    attributes: [("type", AttributeValue::string("min"))]
  }
  
  // 特殊浮点值
  let infinity_measurement = Measurement::{
    value: 1.0/0.0,  // 正无穷
    attributes: [("type", AttributeValue::string("infinity"))]
  }
  
  let neg_infinity_measurement = Measurement::{
    value: -1.0/0.0, // 负无穷
    attributes: [("type", AttributeValue::string("neg_infinity"))]
  }
  
  let nan_measurement = Measurement::{
    value: 0.0/0.0,  // NaN
    attributes: [("type", AttributeValue::string("nan"))]
  }
  
  // 验证值
  assert_eq(zero_measurement.value, 0.0)
  assert_eq(negative_measurement.value, -42.5)
  assert_eq(max_measurement.value, 1.7976931348623157e+308)
  assert_eq(min_measurement.value, -1.7976931348623157e+308)
  assert_eq(infinity_measurement.value, 1.0/0.0)
  assert_eq(neg_infinity_measurement.value, -1.0/0.0)
  assert_eq(nan_measurement.value, 0.0/0.0)
}

test "meter_provider_with_different_parameters" {
  // 测试MeterProvider使用不同参数创建Meter
  let provider = NoopMeterProvider::{}
  
  // 只提供名称
  let meter1 = provider.get_meter("meter1")
  
  // 提供名称和版本
  let meter2 = provider.get_meter("meter2", "2.0.0")
  
  // 提供所有参数
  let meter3 = provider.get_meter("meter3", "3.0.0", "http://example.com/schema")
  
  // 创建仪器并使用它们
  let counter1 = meter1.create_counter("counter1")
  let counter2 = meter2.create_counter("counter2", "ops")
  let counter3 = meter3.create_counter("counter3", "ops", "Counter with all parameters")
  
  // 使用仪器
  counter1.add(1L)
  counter2.add(2L, [("version", AttributeValue::string("2.0.0"))])
  counter3.add(3L, [("schema", AttributeValue::string("full"))])
  
  assert_eq(true, true) // 如果没有崩溃，测试通过
}