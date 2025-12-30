// Metrics API 跨仪器类型集成测试
// 测试不同类型仪器之间的协作和数据一致性

test "cross_instrument_attribute_consistency" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("cross_instrument_test")
  
  // 创建不同类型的仪器
  let counter = meter.create_counter("http_requests")
  let histogram = meter.create_histogram("request_duration")
  let up_down_counter = meter.create_up_down_counter("active_connections")
  let gauge = meter.create_gauge("memory_usage")
  
  // 定义一致的属性集合
  let base_attributes = [
    ("service", @azimuth.telemetry.api.common.AttributeValue::string("api-gateway")),
    ("version", @azimuth.telemetry.api.common.AttributeValue::string("v1.2.3")),
    ("environment", @azimuth.telemetry.api.common.AttributeValue::string("production"))
  ]
  
  // 使用相同的属性集合操作不同仪器
  counter.add(1L, Some(base_attributes))
  histogram.record(125.5, Some(base_attributes))
  up_down_counter.add(1L, Some(base_attributes))
  gauge.record(1024.0 * 1024.0 * 256.0, Some(base_attributes))  // 256MB
  
  // 验证属性在不同仪器间的一致性
  let extended_attributes = [
    ("service", @azimuth.telemetry.api.common.AttributeValue::string("api-gateway")),
    ("version", @azimuth.telemetry.api.common.AttributeValue::string("v1.2.3")),
    ("environment", @azimuth.telemetry.api.common.AttributeValue::string("production")),
    ("endpoint", @azimuth.telemetry.api.common.AttributeValue::string("/api/users"))
  ]
  
  counter.add(1L, Some(extended_attributes))
  histogram.record(89.2, Some(extended_attributes))
  up_down_counter.add(-1L, Some(extended_attributes))
  
  @test.succeed()
}

test "instrument_type_specific_operations" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("type_specific_test")
  
  // Counter 特定操作：只能增加
  let counter = meter.create_counter("operations_total")
  counter.add(1L)
  counter.add(5L)
  counter.add(10L, Some([("operation", @azimuth.telemetry.api.common.AttributeValue::string("create"))]))
  
  // UpDownCounter 特定操作：可以增加和减少
  let up_down_counter = meter.create_up_down_counter("queue_size")
  up_down_counter.add(100L)  // 初始队列大小
  up_down_counter.add(-25L)  // 处理了25个
  up_down_counter.add(50L)   // 新增50个
  
  // Histogram 特定操作：记录分布
  let histogram = meter.create_histogram("response_time_ms")
  let response_times = [10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0]
  let mut i = 0
  while i < response_times.length() {
    histogram.record(response_times[i], Some([
      ("percentile", @azimuth.telemetry.api.common.AttributeValue::float(i.to_float() * 100.0 / response_times.length().to_float()))
    ]))
    i = i + 1
  }
  
  // Gauge 特定操作：设置当前值
  let gauge = meter.create_gauge("cpu_percent")
  let cpu_readings = [15.5, 25.8, 45.2, 78.9, 92.1, 65.4, 38.7]
  i = 0
  while i < cpu_readings.length() {
    gauge.record(cpu_readings[i], Some([
      ("core", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64())),
      ("timestamp", @azimuth.telemetry.api.common.AttributeValue::int((1640995200L + i.to_int64())))
    ]))
    i = i + 1
  }
  
  @test.succeed()
}

test "meter_factory_pattern_consistency" {
  let provider = NoopMeterProvider::{}
  
  // 测试相同名称的meter返回相同的实例（在Noop实现中）
  let meter1 = provider.get_meter("consistent_meter", Some("1.0.0"))
  let meter2 = provider.get_meter("consistent_meter", Some("1.0.0"))
  let meter3 = provider.get_meter("consistent_meter", Some("2.0.0"))  // 不同版本
  
  // 创建相同名称的仪器
  let counter1 = meter1.create_counter("shared_counter")
  let counter2 = meter2.create_counter("shared_counter")
  let histogram1 = meter1.create_histogram("shared_histogram")
  let histogram2 = meter3.create_histogram("shared_histogram")
  
  // 使用所有仪器
  counter1.add(10L)
  counter2.add(20L)
  histogram1.record(100.0)
  histogram2.record(200.0)
  
  @test.succeed()
}

test "complex_attribute_scenarios" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("complex_attributes_test")
  
  // 嵌套属性场景
  let complex_attributes = [
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("user-service")),
    ("service.version", @azimuth.telemetry.api.common.AttributeValue::string("v2.1.0")),
    ("service.namespace", @azimuth.telemetry.api.common.AttributeValue::string("backend")),
    
    ("deployment.region", @azimuth.telemetry.api.common.AttributeValue::string("us-west-2")),
    ("deployment.zone", @azimuth.telemetry.api.common.AttributeValue::string("us-west-2a")),
    ("deployment.cluster", @azimuth.telemetry.api.common.AttributeValue::string("prod-cluster-1")),
    
    ("request.method", @azimuth.telemetry.api.common.AttributeValue::string("POST")),
    ("request.scheme", @azimuth.telemetry.api.common.AttributeValue::string("https")),
    ("request.host", @azimuth.telemetry.api.common.AttributeValue::string("api.example.com")),
    
    ("response.status_code", @azimuth.telemetry.api.common.AttributeValue::int(201L)),
    ("response.size_bytes", @azimuth.telemetry.api.common.AttributeValue::int(1024L)),
    ("response.duration_ms", @azimuth.telemetry.api.common.AttributeValue::float(145.7)),
    
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("none")),
    ("error.retry_count", @azimuth.telemetry.api.common.AttributeValue::int(0L)),
    
    ("tags", @azimuth.telemetry.api.common.AttributeValue::array_string(["api", "rest", "authenticated"])),
    ("feature_flags", @azimuth.telemetry.api.common.AttributeValue::array_bool([true, false, true])),
    ("retry_delays", @azimuth.telemetry.api.common.AttributeValue::array_float([100.0, 200.0, 400.0])),
    ("status_codes", @azimuth.telemetry.api.common.AttributeValue::array_int([200L, 201L, 400L, 500L]))
  ]
  
  let counter = meter.create_counter("http_requests_total")
  let histogram = meter.create_histogram("http_request_duration_ms")
  let gauge = meter.create_gauge("active_connections")
  
  // 使用复杂属性操作所有仪器
  counter.add(1L, Some(complex_attributes))
  histogram.record(145.7, Some(complex_attributes))
  gauge.record(42L.to_float(), Some(complex_attributes))
  
  @test.succeed()
}

test "instrument_lifecycle_integration" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("lifecycle_test")
  
  // 模拟应用程序生命周期中的仪器使用
  let startup_counter = meter.create_counter("application_startups")
  let shutdown_counter = meter.create_counter("application_shutdowns")
  let active_sessions = meter.create_up_down_counter("active_sessions")
  let memory_usage = meter.create_gauge("memory_usage_bytes")
  let request_duration = meter.create_histogram("request_duration_ms")
  
  // 启动阶段
  startup_counter.add(1L)
  memory_usage.record(64.0 * 1024.0 * 1024.0)  // 64MB
  
  // 运行阶段
  active_sessions.add(1L)  // 新用户会话
  request_duration.record(50.0, Some([("endpoint", @azimuth.telemetry.api.common.AttributeValue::string("/api/login"))]))
  
  active_sessions.add(1L)  // 又一个用户会话
  request_duration.record(120.0, Some([("endpoint", @azimuth.telemetry.api.common.AttributeValue::string("/api/data"))]))
  
  memory_usage.record(128.0 * 1024.0 * 1024.0)  // 内存增长到128MB
  
  // 用户离开
  active_sessions.add(-1L)
  
  // 关闭阶段
  active_sessions.add(-1L)  // 最后一个用户离开
  shutdown_counter.add(1L)
  memory_usage.record(32.0 * 1024.0 * 1024.0)  // 内存清理到32MB
  
  @test.succeed()
}

test "measurement_data_integrity" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("data_integrity_test")
  
  // 测试测量的数据完整性
  let measurements = [
    Measurement::{ value: 1.0, attributes: [("type", @azimuth.telemetry.api.common.AttributeValue::string("counter"))] },
    Measurement::{ value: 100.5, attributes: [("type", @azimuth.telemetry.api.common.AttributeValue::string("histogram"))] },
    Measurement::{ value: -10.0, attributes: [("type", @azimuth.telemetry.api.common.AttributeValue::string("up_down_counter"))] },
    Measurement::{ value: 75.25, attributes: [("type", @azimuth.telemetry.api.common.AttributeValue::string("gauge"))] }
  ]
  
  let counter = meter.create_counter("test_counter")
  let histogram = meter.create_histogram("test_histogram")
  let up_down_counter = meter.create_up_down_counter("test_up_down_counter")
  let gauge = meter.create_gauge("test_gauge")
  
  // 根据测量值操作相应仪器
  counter.add(measurements[0].value.to_int64(), Some(measurements[0].attributes))
  histogram.record(measurements[1].value, Some(measurements[1].attributes))
  up_down_counter.add(measurements[2].value.to_int64(), Some(measurements[2].attributes))
  gauge.record(measurements[3].value, Some(measurements[3].attributes))
  
  // 验证测量值结构
  let mut i = 0
  while i < measurements.length() {
    let measurement = measurements[i]
    assert_eq(measurement.attributes.length(), 1)
    match measurement.attributes[0] {
      (key, @azimuth.telemetry.api.common.StringValue(value)) => {
        assert_eq(key, "type")
        match i {
          0 => assert_eq(value, "counter")
          1 => assert_eq(value, "histogram")
          2 => assert_eq(value, "up_down_counter")
          3 => assert_eq(value, "gauge")
          _ => @test.fail("Invalid index")
        }
      }
      _ => @test.fail("Expected string attribute value")
    }
    i = i + 1
  }
}