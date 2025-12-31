// Azimuth Telemetry - Metrics API Integration Test
// 遥测指标API集成测试

test "metrics_api_basic_functionality" {
  // 测试MeterProvider基本功能
  let provider = metrics::NoopMeterProvider::{}
  let meter = provider.get_meter("test-meter", Some("1.0.0"), Some("http://example.com/schema"))
  
  // 测试创建不同类型的指标
  let counter = meter.create_counter("test-counter", Some("count"), Some("Test counter metric"))
  let histogram = meter.create_histogram("test-histogram", Some("ms"), Some("Test histogram metric"))
  let up_down_counter = meter.create_up_down_counter("test-up-down", Some("items"), Some("Test up-down counter"))
  let gauge = meter.create_gauge("test-gauge", Some("percent"), Some("Test gauge metric"))
  
  // 验证指标创建成功（Noop实现返回对应的Noop类型）
  assert_type(counter, metrics::NoopCounter)
  assert_type(histogram, metrics::NoopHistogram)
  assert_type(up_down_counter, metrics::NoopUpDownCounter)
  assert_type(gauge, metrics::NoopGauge)
}

test "metrics_api_counter_operations" {
  // 测试Counter指标操作
  let provider = metrics::NoopMeterProvider::{}
  let meter = provider.get_meter("test-meter")
  let counter = meter.create_counter("request-count", Some("requests"), Some("Total number of requests"))
  
  // 测试计数器增加操作
  counter.add(1L, Some([("method", common::AttributeValue::string("GET"))]))
  counter.add(5L, Some([("method", common::AttributeValue::string("POST"))]))
  counter.add(10L, Some([("method", common::AttributeValue::string("GET")), ("status", common::AttributeValue::int(200L))]))
  
  // 测试无属性操作
  counter.add(3L, None)
  
  // 测试空属性数组
  counter.add(2L, Some([]))
  
  // 测试负值（Counter通常不允许负值，但Noop实现不限制）
  counter.add(-1L, Some([("test", common::AttributeValue::bool(true))]))
}

test "metrics_api_histogram_operations" {
  // 测试Histogram指标操作
  let provider = metrics::NoopMeterProvider::{}
  let meter = provider.get_meter("test-meter")
  let histogram = meter.create_histogram("response-time", Some("ms"), Some("Response time distribution"))
  
  // 测试记录不同值
  histogram.record(100.0, Some([("endpoint", common::AttributeValue::string("/api/users"))]))
  histogram.record(250.5, Some([("endpoint", common::AttributeValue::string("/api/products"))]))
  histogram.record(0.1, Some([("endpoint", common::AttributeValue::string("/api/health"))]))
  
  // 测试边界值
  histogram.record(0.0, Some([("test", common::AttributeValue::string("zero"))]))
  histogram.record(-1.0, Some([("test", common::AttributeValue::string("negative"))]))
  histogram.record(999999.999, Some([("test", common::AttributeValue::string("large"))]))
  
  // 测试无属性操作
  histogram.record(50.0, None)
  
  // 测试空属性数组
  histogram.record(75.0, Some([]))
}

test "metrics_api_up_down_counter_operations" {
  // 测试UpDownCounter指标操作
  let provider = metrics::NoopMeterProvider::{}
  let meter = provider.get_meter("test-meter")
  let up_down_counter = meter.create_up_down_counter("queue-size", Some("items"), Some("Current queue size"))
  
  // 测试增加操作
  up_down_counter.add(10L, Some([("queue", common::AttributeValue::string("high-priority"))]))
  up_down_counter.add(5L, Some([("queue", common::AttributeValue::string("low-priority"))]))
  
  // 测试减少操作
  up_down_counter.add(-3L, Some([("queue", common::AttributeValue::string("high-priority"))]))
  up_down_counter.add(-2L, Some([("queue", common::AttributeValue::string("low-priority"))]))
  
  // 测试零值操作
  up_down_counter.add(0L, Some([("test", common::AttributeValue::string("zero"))]))
  
  // 测试无属性操作
  up_down_counter.add(1L, None)
}

test "metrics_api_gauge_operations" {
  // 测试Gauge指标操作
  let provider = metrics::NoopMeterProvider::{}
  let meter = provider.get_meter("test-meter")
  let gauge = meter.create_gauge("cpu-usage", Some("percent"), Some("CPU usage percentage"))
  
  // 测试记录不同值
  gauge.record(25.5, Some([("instance", common::AttributeValue::string("server-1"))]))
  gauge.record(75.0, Some([("instance", common::AttributeValue::string("server-2"))]))
  gauge.record(100.0, Some([("instance", common::AttributeValue::string("server-3"))]))
  
  // 测试边界值
  gauge.record(0.0, Some([("test", common::AttributeValue::string("minimum"))]))
  gauge.record(-10.0, Some([("test", common::AttributeValue::string("negative"))]))
  gauge.record(150.0, Some([("test", common::AttributeValue::string("overflow"))]))
  
  // 测试无属性操作
  gauge.record(50.0, None)
}

test "metrics_api_complex_attributes" {
  // 测试复杂属性类型
  let provider = metrics::NoopMeterProvider::{}
  let meter = provider.get_meter("test-meter")
  let counter = meter.create_counter("complex-attr-test")
  
  // 测试数组属性
  let array_string_attr = common::AttributeValue::array_string(["tag1", "tag2", "tag3"])
  let array_int_attr = common::AttributeValue::array_int([1L, 2L, 3L, 4L])
  let array_float_attr = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let array_bool_attr = common::AttributeValue::array_bool([true, false, true, false])
  
  counter.add(1L, Some([
    ("tags", array_string_attr),
    ("numbers", array_int_attr),
    ("measurements", array_float_attr),
    ("flags", array_bool_attr)
  ]))
  
  // 测试混合属性类型
  counter.add(1L, Some([
    ("string", common::AttributeValue::string("test")),
    ("integer", common::AttributeValue::int(42L)),
    ("float", common::AttributeValue::float(3.14159)),
    ("boolean", common::AttributeValue::bool(false)),
    ("string-array", common::AttributeValue::array_string(["a", "b"])),
    ("int-array", common::AttributeValue::array_int([1L, 2L]))
  ]))
}

test "metrics_api_multiple_meters" {
  // 测试多个Meter实例
  let provider = metrics::NoopMeterProvider::{}
  
  // 创建多个Meter实例
  let meter1 = provider.get_meter("service-A", Some("1.0.0"))
  let meter2 = provider.get_meter("service-B", Some("2.0.0"), Some("http://example.com/schema-B"))
  let meter3 = provider.get_meter("service-C")
  
  // 在不同Meter上创建指标
  let counter1 = meter1.create_counter("requests-A")
  let counter2 = meter2.create_counter("requests-B")
  let histogram1 = meter1.create_histogram("latency-A")
  let gauge2 = meter2.create_gauge("memory-B")
  let up_down_counter3 = meter3.create_up_down_counter("connections-C")
  
  // 操作不同Meter的指标
  counter1.add(10L, Some([("service", common::AttributeValue::string("A"))]))
  counter2.add(20L, Some([("service", common::AttributeValue::string("B"))]))
  histogram1.record(100.0, Some([("service", common::AttributeValue::string("A"))]))
  gauge2.record(75.0, Some([("service", common::AttributeValue::string("B"))]))
  up_down_counter3.add(5L, Some([("service", common::AttributeValue::string("C"))]))
}

test "metrics_api_edge_cases" {
  // 测试边界情况和错误条件
  let provider = metrics::NoopMeterProvider::{}
  let meter = provider.get_meter("edge-case-test")
  
  // 测试空名称
  let empty_name_counter = meter.create_counter("")
  empty_name_counter.add(1L)
  
  // 测试长名称
  let long_name = "metric-name-".repeat(100)
  let long_name_histogram = meter.create_histogram(long_name)
  long_name_histogram.record(1.0)
  
  // 测试特殊字符名称
  let special_chars_counter = meter.create_counter("metric.name_with-special@chars#123")
  special_chars_counter.add(1L)
  
  // 测试极值
  let counter = meter.create_counter("extreme-values")
  counter.add(Int64::max_value(), Some([("test", common::AttributeValue::string("max"))]))
  counter.add(Int64::min_value(), Some([("test", common::AttributeValue::string("min"))]))
  
  let histogram = meter.create_histogram("extreme-floats")
  histogram.record(Double::max_value(), Some([("test", common::AttributeValue::string("float-max"))]))
  histogram.record(Double::min_value(), Some([("test", common::AttributeValue::string("float-min"))]))
  histogram.record(Double::infinity, Some([("test", common::AttributeValue::string("infinity"))]))
  histogram.record(Double::neg_infinity, Some([("test", common::AttributeValue::string("neg-infinity"))]))
}

test "metrics_api_attribute_variations" {
  // 测试属性变化
  let provider = metrics::NoopMeterProvider::{}
  let meter = provider.get_meter("attribute-test")
  let counter = meter.create_counter("attribute-variations")
  
  // 测试相同属性不同值的多次操作
  counter.add(1L, Some([("method", common::AttributeValue::string("GET"))]))
  counter.add(2L, Some([("method", common::AttributeValue::string("GET"))]))
  counter.add(3L, Some([("method", common::AttributeValue::string("POST"))]))
  counter.add(4L, Some([("method", common::AttributeValue::string("PUT"))]))
  
  // 测试不同属性组合
  counter.add(1L, Some([
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::int(200L))
  ]))
  counter.add(1L, Some([
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::int(404L))
  ]))
  counter.add(1L, Some([
    ("method", common::AttributeValue::string("POST")),
    ("status", common::AttributeValue::int(201L))
  ]))
  
  // 测试大量属性
  let many_attrs = []
  let mut i = 0
  while i < 50 {
    many_attrs.push(("attr-" + i.to_string(), common::AttributeValue::int(i.to_int64())))
    i = i + 1
  }
  counter.add(1L, Some(many_attrs))
}