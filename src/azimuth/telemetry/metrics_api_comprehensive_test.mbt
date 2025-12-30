// 指标API综合测试用例
// 测试Azimuth Telemetry Metrics API的完整功能

test "counter_instrument_creation_and_usage" {
  // 测试计数器仪表的创建和使用
  
  // 创建No-op计数器
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", Some("1.0.0"))
  let counter = meter.create_counter("http_requests_total", Some("count"), Some("Total HTTP requests"))
  
  // 测试计数器添加操作
  let attributes = [
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::string("200"))
  ]
  
  // 这些操作不应该抛出异常
  counter.add(1L, Some(attributes))
  counter.add(5L, Some(attributes))
  counter.add(10L, None)  // 测试无属性的情况
  
  // 测试负值计数器（虽然通常不建议，但API应该支持）
  counter.add(-3L, Some(attributes))
  
  // 验证不同类型的属性
  let complex_attributes = [
    ("method", common::AttributeValue::string("POST")),
    ("status_code", common::AttributeValue::int(201L)),
    ("success", common::AttributeValue::bool(true)),
    ("duration", common::AttributeValue::float(123.4))
  ]
  
  counter.add(2L, Some(complex_attributes))
}

test "histogram_instrument_creation_and_usage" {
  // 测试直方图仪表的创建和使用
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("performance-meter")
  let histogram = meter.create_histogram("request_duration_ms", Some("ms"), Some("Request duration in milliseconds"))
  
  // 测试直方图记录操作
  let attributes = [
    ("endpoint", common::AttributeValue::string("/api/users")),
    ("method", common::AttributeValue::string("GET"))
  ]
  
  // 记录不同的值
  histogram.record(10.5, Some(attributes))
  histogram.record(25.3, Some(attributes))
  histogram.record(100.0, Some(attributes))
  histogram.record(0.1, Some(attributes))  // 测试小值
  histogram.record(1000.0, Some(attributes))  // 测试大值
  
  // 测试无属性记录
  histogram.record(50.0, None)
  
  // 测试负值（某些场景下可能有意义）
  histogram.record(-5.0, Some(attributes))
  
  // 测试零值
  histogram.record(0.0, Some(attributes))
}

test "up_down_counter_instrument_operations" {
  // 测试上下计数器的操作
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("resource-meter")
  let up_down_counter = meter.create_up_down_counter("active_connections", Some("connections"), Some("Current active connections"))
  
  let attributes = [
    ("service", common::AttributeValue::string("database")),
    ("instance", common::AttributeValue::string("primary"))
  ]
  
  // 测试增加操作
  up_down_counter.add(10L, Some(attributes))
  up_down_counter.add(5L, Some(attributes))
  
  // 测试减少操作
  up_down_counter.add(-3L, Some(attributes))
  up_down_counter.add(-7L, Some(attributes))
  
  // 测试归零
  up_down_counter.add(-5L, Some(attributes))
  
  // 测试无属性操作
  up_down_counter.add(100L, None)
  up_down_counter.add(-50L, None)
}

test "gauge_instrument_operations" {
  // 测试仪表仪表的操作
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("system-meter")
  let gauge = meter.create_gauge("cpu_usage_percent", Some("%"), Some("Current CPU usage percentage"))
  
  let attributes = [
    ("host", common::AttributeValue::string("web-server-01")),
    ("core", common::AttributeValue::string("0"))
  ]
  
  // 测试仪表值记录
  gauge.record(25.5, Some(attributes))
  gauge.record(75.2, Some(attributes))
  gauge.record(100.0, Some(attributes))  // 测试最大值
  gauge.record(0.0, Some(attributes))   // 测试最小值
  
  // 测试无属性记录
  gauge.record(50.0, None)
  
  // 测试边界值
  gauge.record(99.9, Some(attributes))
  gauge.record(0.1, Some(attributes))
}

test "meter_provider_and_meter_hierarchy" {
  // 测试仪表提供者和仪表层次结构
  
  // 测试多个仪表创建
  let meter_provider = metrics::NoopMeterProvider::{}
  
  let http_meter = meter_provider.get_meter("http-instrumentation", Some("1.0.0"))
  let db_meter = meter_provider.get_meter("database-instrumentation", Some("2.1.0"))
  let cache_meter = meter_provider.get_meter("cache-instrumentation", None)
  
  // 验证不同仪表创建的仪表
  let http_counter = http_meter.create_counter("http_requests", Some("count"), None)
  let db_histogram = db_meter.create_histogram("db_query_time", Some("ms"), Some("Database query time"))
  let cache_gauge = cache_meter.create_gauge("cache_hit_ratio", Some("%"), Some("Cache hit ratio"))
  
  // 测试不同仪表的仪表操作
  http_counter.add(100L, Some([("method", common::AttributeValue::string("GET"))]))
  
  db_histogram.record(50.0, Some([("table", common::AttributeValue::string("users"))]))
  
  cache_gauge.record(85.5, Some([("cache_type", common::AttributeValue::string("redis"))]))
  
  // 测试相同名称的仪表创建（应该返回独立的实例）
  let another_http_meter = meter_provider.get_meter("http-instrumentation", Some("1.0.0"))
  let another_counter = another_http_meter.create_counter("http_requests", Some("count"), None)
  another_counter.add(200L, None)
}

test "instrument_type_enumeration" {
  // 测试仪表类型枚举
  
  // 验证所有仪表类型
  let counter_type = metrics::Counter
  let histogram_type = metrics::Histogram
  let up_down_counter_type = metrics::UpDownCounter
  let gauge_type = metrics::Gauge
  let observable_counter_type = metrics::ObservableCounter
  let observable_gauge_type = metrics::ObservableGauge
  let observable_up_down_counter_type = metrics::ObservableUpDownCounter
  
  // 在实际实现中，这些类型可以用于模式匹配和类型检查
  // 这里我们验证类型的存在性
  let all_types = [
    counter_type, histogram_type, up_down_counter_type, gauge_type,
    observable_counter_type, observable_gauge_type, observable_up_down_counter_type
  ]
  
  assert_eq(all_types.length(), 7)
}

test "measurement_structure_validation" {
  // 测试测量结构验证
  
  // 创建测量值
  let simple_measurement = metrics::Measurement::{
    value: 42.5,
    attributes: [
      ("unit", common::AttributeValue::string("ms")),
      ("type", common::AttributeValue::string("latency"))
    ]
  }
  
  assert_eq(simple_measurement.value, 42.5)
  assert_eq(simple_measurement.attributes.length(), 2)
  
  // 创建复杂测量值
  let complex_measurement = metrics::Measurement::{
    value: 1000.0,
    attributes: [
      ("service", common::AttributeValue::string("api-gateway")),
      ("version", common::AttributeValue::string("v2.1.0")),
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-east-1")),
      ("availability_zone", common::AttributeValue::string("us-east-1a")),
      ("instance_type", common::AttributeValue::string("m5.large")),
      ("critical", common::AttributeValue::bool(true)),
      ("tags", common::AttributeValue::array_string(["web", "public", "ssl"]))
    ]
  }
  
  assert_eq(complex_measurement.value, 1000.0)
  assert_eq(complex_measurement.attributes.length(), 8)
  
  // 验证数组属性
  let (_, tags_value) = complex_measurement.attributes[7]
  match tags_value {
    common::ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags.contains("web"), true)
      assert_eq(tags.contains("public"), true)
      assert_eq(tags.contains("ssl"), true)
    }
    _ => assert_eq(false, true, "Expected ArrayStringValue for tags")
  }
}

test "metric_instrument_edge_cases" {
  // 测试指标仪表边界情况
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("edge-case-meter")
  
  // 测试空字符串名称
  let empty_name_counter = meter.create_counter("", Some("count"), Some("Empty name counter"))
  empty_name_counter.add(1L, None)
  
  // 测试长名称
  let long_name = "this_is_a_very_long_metric_name_that_tests_the_systems_ability_to_handle_extended_metric_names_without_issues"
  let long_name_histogram = meter.create_histogram(long_name, Some("ms"), None)
  long_name_histogram.record(100.0, None)
  
  // 测试特殊字符名称
  let special_chars_gauge = meter.create_gauge("metric.with-special_chars_and_numbers_123", Some("%"), None)
  special_chars_gauge.record(75.0, None)
  
  // 测试极值
  let counter = meter.create_counter("extreme_values", None, None)
  counter.add(9223372036854775807L, None)  // 最大Int64值
  counter.add(-9223372036854775808L, None) // 最小Int64值
  
  let histogram = meter.create_histogram("extreme_floats", None, None)
  histogram.record(1.7976931348623157e+308, None)  // 接近Double最大值
  histogram.record(-1.7976931348623157e+308, None) // 接近Double最小值
  histogram.record(4.9406564584124654e-324, None)  // 接近Double最小正值
  
  // 测试空属性数组
  let empty_attr_counter = meter.create_counter("empty_attrs", None, None)
  empty_attr_counter.add(5L, Some([]))
  
  // 测试大量属性
  let many_attrs = [
    ("attr1", common::AttributeValue::string("value1")),
    ("attr2", common::AttributeValue::string("value2")),
    ("attr3", common::AttributeValue::string("value3")),
    ("attr4", common::AttributeValue::string("value4")),
    ("attr5", common::AttributeValue::string("value5"))
  ]
  let many_attrs_counter = meter.create_counter("many_attrs", None, None)
  many_attrs_counter.add(10L, Some(many_attrs))
}