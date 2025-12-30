// 跨仪器类型测试
// 测试不同类型指标仪器之间的协作

test "cross_instrument_attribute_consistency" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("cross_instrument_test")
  
  let counter = meter.create_counter("http_requests")
  let histogram = meter.create_histogram("request_duration")
  let up_down_counter = meter.create_up_down_counter("active_connections")
  let gauge = meter.create_gauge("memory_usage")
  
  let base_attributes = [
    ("service", @azimuth.telemetry.api.common.AttributeValue::string("api-gateway")),
    ("version", @azimuth.telemetry.api.common.AttributeValue::string("v1.2.3"))
  ]
  
  counter.add(1L, Some(base_attributes))
  histogram.record(125.5, Some(base_attributes))
  up_down_counter.add(1L, Some(base_attributes))
  gauge.record(1024.0 * 1024.0 * 256.0, Some(base_attributes))
  
  @test.succeed()
}

test "instrument_type_specific_operations" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("type_specific_test")
  
  let counter = meter.create_counter("operations_total")
  let up_down_counter = meter.create_up_down_counter("queue_size")
  let histogram = meter.create_histogram("response_time_ms")
  let gauge = meter.create_gauge("cpu_percent")
  
  counter.add(1L)
  counter.add(5L, Some([("operation", @azimuth.telemetry.api.common.AttributeValue::string("create"))]))
  
  up_down_counter.add(100L)
  up_down_counter.add(-25L, Some([("server", @azimuth.telemetry.api.common.AttributeValue::string("web-01"))]))
  
  histogram.record(10.0)
  histogram.record(25.0, Some([("endpoint", @azimuth.telemetry.api.common.AttributeValue::string("/api/users"))]))
  
  gauge.record(1024.0 * 1024.0 * 512.0)
  gauge.record(2.147483648e9, Some([("type", @azimuth.telemetry.api.common.AttributeValue::string("heap"))]))
  
  @test.succeed()
}

test "meter_factory_pattern_consistency" {
  let provider = NoopMeterProvider::{}
  
  let meter1 = provider.get_meter("consistent_meter", Some("1.0.0"))
  let meter2 = provider.get_meter("consistent_meter", Some("1.0.0"))
  let meter3 = provider.get_meter("consistent_meter", Some("2.0.0"))
  
  let counter1 = meter1.create_counter("shared_counter")
  let counter2 = meter2.create_counter("shared_counter")
  let histogram1 = meter1.create_histogram("shared_histogram")
  let histogram2 = meter3.create_histogram("shared_histogram")
  
  counter1.add(10L)
  counter2.add(20L)
  histogram1.record(100.0)
  histogram2.record(200.0)
  
  @test.succeed()
}