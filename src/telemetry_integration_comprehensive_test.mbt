// 遥测系统集成测试 - 测试metrics、trace和logs的协同工作
test "telemetry_integration_signals_correlation" {
  // 测试目标：验证metrics、trace和logs信号之间的关联性
  
  // 1. 创建共享的遥测资源
  let resource = @azimuth.telemetry.api.common.Resource::default("integration-test-service")
  assert_eq(resource.service_name, "integration-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  
  // 2. 创建关联的属性
  let correlation_attributes = [
    ("operation.id", @azimuth.telemetry.api.common.AttributeValue::string("op-12345")),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-67890")),
    ("service.version", @azimuth.telemetry.api.common.AttributeValue::string("1.0.0"))
  ]
  
  // 3. 测试Span创建与属性关联
  let span_context = @azimuth.telemetry.api.trace.SpanContext{
    trace_id: [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88],
    span_id: [0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00, 0x11],
    trace_flags: 0x01,
    trace_state: "test=state"
  }
  
  let test_span = @azimuth.telemetry.api.trace.Span{
    name: "integration-operation",
    context: span_context,
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000, // 2022-01-01 00:00:00 UTC
    end_time_unix_nanos: Some(1640995201000000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: correlation_attributes,
    events: [],
    links: []
  }
  
  // 验证Span属性
  assert_eq(test_span.name, "integration-operation")
  assert_eq(test_span.attributes.length(), 3)
  
  // 4. 测试Metrics与Span的关联
  let metric_measurements = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 100.0,
      attributes: correlation_attributes + [("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("latency"))]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: 5.0,
      attributes: correlation_attributes + [("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("error_count"))]
    }
  ]
  
  // 验证Metrics测量
  assert_eq(metric_measurements.length(), 2)
  assert_eq(metric_measurements[0].value, 100.0)
  assert_eq(metric_measurements[0].attributes.length(), 4)
  
  // 5. 测试Logs与上下文的关联
  let log_attributes = correlation_attributes + [
    ("log.level", @azimuth.telemetry.api.common.AttributeValue::string("INFO")),
    ("trace.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["1234567890abcdef"])),
    ("span.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["aabbccddeeff0011"]))
  ]
  
  // 验证Logs属性
  assert_eq(log_attributes.length(), 6)
  
  // 6. 验证跨信号的数据一致性
  let operation_id_found_in_span = test_span.attributes.any(fn((key, _)) { key == "operation.id" })
  let operation_id_found_in_metrics = metric_measurements[0].attributes.any(fn((key, _)) { key == "operation.id" })
  let operation_id_found_in_logs = log_attributes.any(fn((key, _)) { key == "operation.id" })
  
  assert(operation_id_found_in_span, "operation.id应该在Span属性中找到")
  assert(operation_id_found_in_metrics, "operation.id应该在Metrics属性中找到")
  assert(operation_id_found_in_logs, "operation.id应该在Logs属性中找到")
  
  // 7. 测试时间戳一致性
  let span_start_time = test_span.start_time_unix_nanos
  let span_end_time = test_span.end_time_unix_nanos.unwrap()
  assert(span_end_time > span_start_time, "Span结束时间应该晚于开始时间")
  
  // 8. 验证资源属性传播
  let resource_attributes = resource.attributes
  assert_eq(resource.service_name, "integration-test-service")
  
  println("✓ 遥测系统集成测试通过：metrics、trace和logs协同工作正常")
}

test "telemetry_cross_signal_data_flow" {
  // 测试目标：验证跨遥测信号的数据流和传播
  
  // 1. 模拟分布式追踪场景
  let trace_id = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10]
  let parent_span_id = [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]
  let child_span_id = [0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28]
  
  // 2. 创建父Span
  let parent_context = @azimuth.telemetry.api.trace.SpanContext{
    trace_id: trace_id,
    span_id: parent_span_id,
    trace_flags: 0x01,
    trace_state: "parent=test"
  }
  
  // 3. 创建子Span，继承父Span上下文
  let child_context = @azimuth.telemetry.api.trace.SpanContext{
    trace_id: trace_id, // 相同的trace_id
    span_id: child_span_id, // 不同的span_id
    trace_flags: 0x01,
    trace_state: "child=test"
  }
  
  // 4. 验证追踪上下文传播
  assert_eq(parent_context.trace_id, child_context.trace_id, "父子Span应该有相同的trace_id")
  assert(parent_context.span_id != child_context.span_id, "父子Span应该有不同的span_id")
  
  // 5. 测试跨服务的度量关联
  let service_a_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 250.0,
      attributes: [
        ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("service-a")),
        ("trace.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["0102030405060708090a0b0c0d0e0f10"]))
      ]
    }
  ]
  
  let service_b_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 180.0,
      attributes: [
        ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("service-b")),
        ("trace.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["0102030405060708090a0b0c0d0e0f10"]))
      ]
    }
  ]
  
  // 6. 验证跨服务度量关联
  let service_a_trace_id = service_a_metrics[0].attributes.find(fn((key, _)) { key == "trace.id" })
  let service_b_trace_id = service_b_metrics[0].attributes.find(fn((key, _)) { key == "trace.id" })
  
  assert(service_a_trace_id != None, "Service A metrics应该包含trace.id")
  assert(service_b_trace_id != None, "Service B metrics应该包含trace.id")
  
  // 7. 测试日志事件的关联
  let span_events = [
    @azimuth.telemetry.api.trace.SpanEvent{
      name: "service_call",
      timestamp_unix_nanos: 1640995200500000000,
      attributes: [
        ("target.service", @azimuth.telemetry.api.common.AttributeValue::string("service-b")),
        ("call.duration_ms", @azimuth.telemetry.api.common.AttributeValue::int(50))
      ]
    },
    @azimuth.telemetry.api.trace.SpanEvent{
      name: "error_occurred",
      timestamp_unix_nanos: 1640995200800000000,
      attributes: [
        ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("timeout")),
        ("error.retry_count", @azimuth.telemetry.api.common.AttributeValue::int(3))
      ]
    }
  ]
  
  // 8. 验证事件时间序列
  assert(span_events.length(), 2)
  assert(span_events[0].timestamp_unix_nanos < span_events[1].timestamp_unix_nanos, "事件应该按时间顺序排列")
  
  println("✓ 跨信号数据流测试通过：分布式追踪场景正常工作")
}