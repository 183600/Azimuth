test "Metrics instrument types comprehensive test" {
  // 测试所有仪器类型
  assert_eq(metrics::Counter, metrics::Counter)
  assert_eq(metrics::Histogram, metrics::Histogram)
  assert_eq(metrics::UpDownCounter, metrics::UpDownCounter)
  assert_eq(metrics::Gauge, metrics::Gauge)
  assert_eq(metrics::ObservableCounter, metrics::ObservableCounter)
  assert_eq(metrics::ObservableGauge, metrics::ObservableGauge)
  assert_eq(metrics::ObservableUpDownCounter, metrics::ObservableUpDownCounter)
  
  // 测试Measurement结构
  let measurement = metrics::Measurement{
    value: 42.5,
    attributes: [
      ("unit", common::AttributeValue::string("ms")),
      ("status", common::AttributeValue::string("success"))
    ]
  }
  
  assert_eq(measurement.value, 42.5)
  assert_eq(measurement.attributes.length(), 2)
  assert_eq(measurement.attributes[0].0, "unit")
  assert_eq(measurement.attributes[1].0, "status")
  
  match measurement.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "ms")
    _ => @test.fail("Expected StringValue for unit attribute")
  }
  
  match measurement.attributes[1].1 {
    common::StringValue(s) => assert_eq(s, "success")
    _ => @test.fail("Expected StringValue for status attribute")
  }
  
  // 测试NoopCounter
  let noop_counter = metrics::NoopCounter::{}
  
  // 这些调用不应该崩溃，但也不应该产生任何效果
  noop_counter.add(1L, [])
  noop_counter.add(10L, [("method", common::AttributeValue::string("GET"))])
  noop_counter.add(-5L, [("status", common::AttributeValue::string("error"))])
  noop_counter.add(0L, [])
  noop_counter.add(9223372036854775807L, [("max", common::AttributeValue::bool(true))])  // Int64最大值
  noop_counter.add(-9223372036854775808L, [("min", common::AttributeValue::bool(true))]) // Int64最小值
  
  // 测试NoopHistogram
  let noop_histogram = metrics::NoopHistogram::{}
  
  noop_histogram.record(1.0, [])
  noop_histogram.record(3.14, [("operation", common::AttributeValue::string("calculate"))])
  noop_histogram.record(0.0, [])
  noop_histogram.record(-1.5, [("negative", common::AttributeValue::bool(true))])
  noop_histogram.record(1.7976931348623157e+308, [("max_float", common::AttributeValue::bool(true))])  // Double最大值
  noop_histogram.record(-1.7976931348623157e+308, [("min_float", common::AttributeValue::bool(true))]) // Double最小值
  
  // 测试NoopUpDownCounter
  let noop_up_down_counter = metrics::NoopUpDownCounter::{}
  
  noop_up_down_counter.add(5L, [])
  noop_up_down_counter.add(-3L, [("adjustment", common::AttributeValue::string("decrease"))])
  noop_up_down_counter.add(0L, [])
  noop_up_down_counter.add(100L, [("batch_size", common::AttributeValue::int(100L))])
  
  // 测试NoopGauge
  let noop_gauge = metrics::NoopGauge::{}
  
  noop_gauge.record(25.0, [])
  noop_gauge.record(0.0, [("empty", common::AttributeValue::bool(true))])
  noop_gauge.record(-10.5, [("negative", common::AttributeValue::bool(true))])
  noop_gauge.record(100.0, [("percentage", common::AttributeValue::string("cpu"))])
  
  // 测试NoopMeter
  let noop_meter = metrics::NoopMeter::{}
  
  // 创建各种仪器
  let counter1 = noop_meter.create_counter("request_count", Some("requests"), Some("Total number of requests"))
  let counter2 = noop_meter.create_counter("error_count", None, None)
  
  let histogram1 = noop_meter.create_histogram("response_time", Some("ms"), Some("Response time in milliseconds"))
  let histogram2 = noop_meter.create_histogram("request_size", None, None)
  
  let up_down_counter1 = noop_meter.create_up_down_counter("queue_size", Some("items"), Some("Current queue size"))
  let up_down_counter2 = noop_meter.create_up_down_counter("active_connections", None, None)
  
  let gauge1 = noop_meter.create_gauge("cpu_usage", Some("%"), Some("CPU usage percentage"))
  let gauge2 = noop_meter.create_gauge("memory_usage", None, None)
  
  // 使用创建的仪器
  counter1.add(1L, [("method", common::AttributeValue::string("GET"))])
  counter2.add(1L, [])
  
  histogram1.record(150.0, [("endpoint", common::AttributeValue::string("/api/data"))])
  histogram2.record(1024.0, [])
  
  up_down_counter1.add(10L, [("queue", common::AttributeValue::string("priority"))])
  up_down_counter2.add(-1L, [])
  
  gauge1.record(75.5, [("host", common::AttributeValue::string("server-1"))])
  gauge2.record(512.0, [])
  
  // 测试NoopMeterProvider
  let noop_meter_provider = metrics::NoopMeterProvider::{}
  
  let meter1 = noop_meter_provider.get_meter("test-meter", Some("1.0.0"), Some("https://example.com/schema"))
  let meter2 = noop_meter_provider.get_meter("another-meter", None, None)
  
  // 使用获取的meter创建仪器
  let meter1_counter = meter1.create_counter("meter1_counter", None, None)
  let meter2_histogram = meter2.create_histogram("meter2_histogram", Some("bytes"), Some("Size in bytes"))
  
  meter1_counter.add(100L, [])
  meter2_histogram.record(2048.0, [])
  
  // 测试边界条件
  let empty_name_counter = noop_meter.create_counter("", None, None)
  empty_name_counter.add(1L, [])
  
  let long_name_histogram = noop_meter.create_histogram(
    "this-is-a-very-long-instrument-name-that-tests-boundary-conditions-and-ensures-the-system-can-handle-names-of-significant-length",
    Some("microseconds"),
    Some("Very detailed description for a long-named histogram instrument that measures duration in microseconds")
  )
  long_name_histogram.record(123.456, [])
  
  let special_chars_gauge = noop_meter.create_gauge("cpu.usage_2024@server", Some("%"), Some("CPU usage with special chars"))
  special_chars_gauge.record(85.5, [])
  
  // 测试复杂属性
  let complex_counter = noop_meter.create_counter("complex_counter", None, None)
  complex_counter.add(1L, [
    ("string_attr", common::AttributeValue::string("test")),
    ("int_attr", common::AttributeValue::int(42L)),
    ("float_attr", common::AttributeValue::float(3.14)),
    ("bool_attr", common::AttributeValue::bool(true)),
    ("string_array_attr", common::AttributeValue::array_string(["a", "b", "c"])),
    ("int_array_attr", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array_attr", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array_attr", common::AttributeValue::array_bool([true, false, true]))
  ])
  
  // 测试大量属性
  let many_attrs_histogram = noop_meter.create_histogram("many_attrs_histogram", None, None)
  let mut many_attributes = []
  
  // 添加50个属性
  for i = 0; i < 50; i = i + 1 {
    many_attributes = many_attributes + [("attr_" + i.to_string(), common::AttributeValue::int(i.to_int64()))]
  }
  
  many_attrs_histogram.record(100.0, many_attributes)
  
  // 测试极值
  let extreme_values_up_down_counter = noop_meter.create_up_down_counter("extreme_values", None, None)
  extreme_values_up_down_counter.add(9223372036854775807L, [("max_int", common::AttributeValue::bool(true))])  // Int64最大值
  extreme_values_up_down_counter.add(-9223372036854775808L, [("min_int", common::AttributeValue::bool(true))]) // Int64最小值
  
  let extreme_values_gauge = noop_meter.create_gauge("extreme_gauge", None, None)
  extreme_values_gauge.record(1.7976931348623157e+308, [("max_double", common::AttributeValue::bool(true))])  // Double最大值
  extreme_values_gauge.record(-1.7976931348623157e+308, [("min_double", common::AttributeValue::bool(true))]) // Double最小值
  extreme_values_gauge.record(0.0, [("zero", common::AttributeValue::bool(true))])
  
  // 测试空属性数组
  let empty_attrs_counter = noop_meter.create_counter("empty_attrs_counter", None, None)
  empty_attrs_counter.add(1L, [])
  
  // 测试特殊浮点值
  let special_floats_histogram = noop_meter.create_histogram("special_floats", None, None)
  special_floats_histogram.record(1.0, [])
  special_floats_histogram.record(-1.0, [])
  special_floats_histogram.record(0.5, [])
  special_floats_histogram.record(-0.5, [])
  special_floats_histogram.record(1000000.0, [])
  special_floats_histogram.record(0.000001, [])
  
  // 测试InstrumentType枚举值
  let instrument_types = [
    metrics::Counter,
    metrics::Histogram,
    metrics::UpDownCounter,
    metrics::Gauge,
    metrics::ObservableCounter,
    metrics::ObservableGauge,
    metrics::ObservableUpDownCounter
  ]
  
  assert_eq(instrument_types.length(), 7)
  assert_eq(instrument_types[0], metrics::Counter)
  assert_eq(instrument_types[1], metrics::Histogram)
  assert_eq(instrument_types[2], metrics::UpDownCounter)
  assert_eq(instrument_types[3], metrics::Gauge)
  assert_eq(instrument_types[4], metrics::ObservableCounter)
  assert_eq(instrument_types[5], metrics::ObservableGauge)
  assert_eq(instrument_types[6], metrics::ObservableUpDownCounter)
  
  // 测试Measurement的边界条件
  let zero_measurement = metrics::Measurement{ value: 0.0, attributes: [] }
  assert_eq(zero_measurement.value, 0.0)
  assert_eq(zero_measurement.attributes.length(), 0)
  
  let negative_measurement = metrics::Measurement{
    value: -100.0,
    attributes: [("negative", common::AttributeValue::bool(true))]
  }
  assert_eq(negative_measurement.value, -100.0)
  assert_eq(negative_measurement.attributes.length(), 1)
  
  let max_measurement = metrics::Measurement{
    value: 1.7976931348623157e+308,  // Double最大值
    attributes: [("max", common::AttributeValue::bool(true))]
  }
  assert_eq(max_measurement.value, 1.7976931348623157e+308)
  
  let min_measurement = metrics::Measurement{
    value: -1.7976931348623157e+308,  // Double最小值
    attributes: [("min", common::AttributeValue::bool(true))]
  }
  assert_eq(min_measurement.value, -1.7976931348623157e+308)
  
  // 测试带有大量属性的Measurement
  let mut many_attrs = []
  for i = 0; i < 100; i = i + 1 {
    many_attrs = many_attrs + [("measurement_attr_" + i.to_string(), common::AttributeValue::int(i.to_int64()))]
  }
  
  let complex_measurement = metrics::Measurement{
    value: 500.0,
    attributes: many_attrs
  }
  assert_eq(complex_measurement.value, 500.0)
  assert_eq(complex_measurement.attributes.length(), 100)
}