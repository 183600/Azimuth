test "instrument_types_individual" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("test-meter", "1.0.0", "http://example.com/schema")
  
  // 测试Counter的单独使用
  let counter = meter.create_counter("test-counter", "count", "A test counter")
  counter.add(1L, [])
  counter.add(10L, [("type", AttributeValue::string("api"))])
  counter.add(100L, [
    ("method", AttributeValue::string("GET")),
    ("status", AttributeValue::int(200L)),
    ("latency", AttributeValue::float(123.4))
  ])
  counter.add(0L) // 测试零值
  counter.add(-5L) // 测试负值
  
  // 测试Histogram的单独使用
  let histogram = meter.create_histogram("test-histogram", "ms", "A test histogram")
  histogram.record(1.0, [])
  histogram.record(100.5, [("type", AttributeValue::string("db"))])
  histogram.record(0.0) // 测试零值
  histogram.record(-1.0) // 测试负值
  histogram.record(999999.999) // 测试大值
  histogram.record(1.0/0.0) // 测试无穷大
  histogram.record(0.0/0.0) // 测试NaN
  
  // 测试UpDownCounter的单独使用
  let up_down_counter = meter.create_up_down_counter("test-up-down", "bytes", "A test up-down counter")
  up_down_counter.add(10L, [])
  up_down_counter.add(-5L, [("direction", AttributeValue::string("up"))])
  up_down_counter.add(0L) // 测试零值
  up_down_counter.add(-100L) // 测试大的负值
  up_down_counter.add(1000L, [
    ("resource", AttributeValue::string("memory")),
    ("process", AttributeValue::string("worker"))
  ])
  
  // 测试Gauge的单独使用
  let gauge = meter.create_gauge("test-gauge", "percent", "A test gauge")
  gauge.record(0.0, [])
  gauge.record(50.5, [("metric", AttributeValue::string("cpu"))])
  gauge.record(100.0) // 测试最大值
  gauge.record(-10.0) // 测试负值
  gauge.record(1.0/0.0) // 测试无穷大
  gauge.record(0.0/0.0) // 测试NaN
  gauge.record(99.99, [
    ("host", AttributeValue::string("server-01")),
    ("region", AttributeValue::string("us-west"))
  ])
  
  // 测试带特殊字符的instrument名称
  let special_counter = meter.create_counter("special-counter!@#$%", "count", "Special counter")
  special_counter.add(1L, [("special", AttributeValue::string("!@#$%"))])
  
  // 测试Unicode instrument名称
  let unicode_histogram = meter.create_histogram("测试直方图🚀", "ms", "Unicode histogram")
  unicode_histogram.record(42.0, [("unicode", AttributeValue::string("测试值🎉"))])
  
  assert_eq(true, true) // 如果没有崩溃，测试通过
}