// 跨模块数据流测试用例
// 测试不同API模块之间的数据一致性和流转

test "trace_to_metrics_data_flow" {
  // 测试从追踪数据到指标数据的转换流程
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let operation_name = "http_request"
  let duration_ms = 250L
  
  // 模拟追踪数据
  let trace_data = (
    trace_id,
    span_id,
    operation_name,
    duration_ms
  )
  
  // 验证追踪数据完整性
  assert_eq(trace_data.0.length(), 32)
  assert_eq(trace_data.1.length(), 16)
  assert_eq(trace_data.2, "http_request")
  assert_eq(trace_data.3 > 0L, true)
  
  // 转换为指标数据
  let metric_name = "span_duration_ms"
  let metric_value = trace_data.3.to_double()
  let metric_labels = [
    ("operation", trace_data.2),
    ("trace_id", trace_data.0)
  ]
  
  // 验证指标数据
  assert_eq(metric_name, "span_duration_ms")
  assert_eq(metric_value, 250.0)
  assert_eq(metric_labels.length(), 2)
  assert_eq(metric_labels[0].0, "operation")
  assert_eq(metric_labels[0].1, "http_request")
  assert_eq(metric_labels[1].0, "trace_id")
  assert_eq(metric_labels[1].1, trace_id)
}

test "logs_to_trace_correlation" {
  // 测试日志与追踪的关联性
  
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "fedcba0987654321"
  let log_message = "Database connection established"
  let log_severity = "INFO"
  
  // 创建日志记录
  let log_record = (
    trace_id,
    span_id,
    log_message,
    log_severity
  )
  
  // 验证日志记录
  assert_eq(log_record.0.length(), 32)
  assert_eq(log_record.1.length(), 16)
  assert_eq(log_record.2, "Database connection established")
  assert_eq(log_record.3, "INFO")
  
  // 创建关联的追踪上下文
  let trace_context = (
    log_record.0,
    log_record.1,
    "database_span"
  )
  
  // 验证追踪上下文与日志的关联
  assert_eq(trace_context.0, log_record.0)
  assert_eq(trace_context.1, log_record.1)
  assert_eq(trace_context.2, "database_span")
  
  // 验证关联数据完整性
  let correlation_data = (
    trace_context,
    log_record
  )
  
  assert_eq(correlation_data.0.0, correlation_data.1.0)
  assert_eq(correlation_data.0.1, correlation_data.1.1)
}

test "metrics_to_attributes_conversion" {
  // 测试指标数据到属性的转换
  
  let metric_name = "http_requests_total"
  let metric_value = 1000L
  let metric_unit = "count"
  let metric_labels = [
    ("method", "GET"),
    ("status", "200"),
    ("endpoint", "/api/users")
  ]
  
  // 验证原始指标数据
  assert_eq(metric_name, "http_requests_total")
  assert_eq(metric_value, 1000L)
  assert_eq(metric_unit, "count")
  assert_eq(metric_labels.length(), 3)
  
  // 转换为属性数组
  let attributes = [
    ("metric.name", AttributeValue::string(metric_name)),
    ("metric.value", AttributeValue::int(metric_value)),
    ("metric.unit", AttributeValue::string(metric_unit)),
    ("label.method", AttributeValue::string(metric_labels[0].1)),
    ("label.status", AttributeValue::string(metric_labels[1].1)),
    ("label.endpoint", AttributeValue::string(metric_labels[2].1))
  ]
  
  // 验证转换后的属性
  assert_eq(attributes.length(), 6)
  
  match attributes[0].1 {
    StringValue(s) => assert_eq(s, "http_requests_total")
    _ => @test.fail("Test failed")
  }
  
  match attributes[1].1 {
    IntValue(i) => assert_eq(i, 1000L)
    _ => @test.fail("Test failed")
  }
  
  match attributes[2].1 {
    StringValue(s) => assert_eq(s, "count")
    _ => @test.fail("Test failed")
  }
}

test "context_propagation_across_modules" {
  // 测试跨模块的上下文传播
  
  // 初始上下文数据
  let initial_context = [
    ("trace_id", "abcdef1234567890abcdef1234567890"),
    ("span_id", "1234567890abcdef"),
    ("baggage.user_id", "user123"),
    ("baggage.request_id", "req456")
  ]
  
  // 验证初始上下文
  assert_eq(initial_context.length(), 4)
  assert_eq(initial_context[0].0, "trace_id")
  assert_eq(initial_context[1].0, "span_id")
  assert_eq(initial_context[2].0, "baggage.user_id")
  assert_eq(initial_context[3].0, "baggage.request_id")
  
  // 模拟传播到日志模块
  let log_context = initial_context.map(fn(pair) {
    if pair.0.has_prefix("baggage") {
      ("log." + pair.0, pair.1)
    } else {
      pair
    }
  })
  
  // 验证日志上下文
  assert_eq(log_context.length(), 4)
  assert_eq(log_context[0].0, "trace_id")  // 未改变
  assert_eq(log_context[1].0, "span_id")   // 未改变
  assert_eq(log_context[2].0, "log.baggage.user_id")  // 已改变
  assert_eq(log_context[3].0, "log.baggage.request_id") // 已改变
  
  // 模拟传播到指标模块
  let metrics_context = log_context.filter(fn(pair) {
    pair.0 != "log.baggage.request_id"  // 过滤掉不需要的
  })
  
  // 验证指标上下文
  assert_eq(metrics_context.length(), 3)
  assert_eq(metrics_context[0].0, "trace_id")
  assert_eq(metrics_context[1].0, "span_id")
  assert_eq(metrics_context[2].0, "log.baggage.user_id")
}

test "resource_metadata_consistency" {
  // 测试资源元数据在不同模块间的一致性
  
  // 定义资源元数据
  let resource_metadata = [
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("service.namespace", "finance"),
    ("deployment.environment", "production"),
    ("host.name", "web-01"),
    ("process.pid", "12345")
  ]
  
  // 验证资源元数据
  assert_eq(resource_metadata.length(), 6)
  assert_eq(resource_metadata[0].1, "payment-service")
  assert_eq(resource_metadata[1].1, "1.2.3")
  
  // 在追踪模块中使用
  let trace_resource = resource_metadata.filter(fn(pair) {
    pair.0.has_prefix("service") or pair.0.has_prefix("deployment")
  })
  
  // 验证追踪资源
  assert_eq(trace_resource.length(), 4)
  assert_eq(trace_resource[0].0, "service.name")
  assert_eq(trace_resource[1].0, "service.version")
  assert_eq(trace_resource[2].0, "service.namespace")
  assert_eq(trace_resource[3].0, "deployment.environment")
  
  // 在指标模块中使用
  let metrics_resource = resource_metadata.filter(fn(pair) {
    pair.0.has_prefix("service") or pair.0.has_prefix("host")
  })
  
  // 验证指标资源
  assert_eq(metrics_resource.length(), 4)
  assert_eq(metrics_resource[0].0, "service.name")
  assert_eq(metrics_resource[1].0, "service.version")
  assert_eq(metrics_resource[2].0, "service.namespace")
  assert_eq(metrics_resource[3].0, "host.name")
  
  // 验证核心服务信息在所有模块中一致
  let core_service_info = [
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("service.namespace", "finance")
  ]
  
  for core_info in core_service_info {
    let trace_found = trace_resource.any(fn(pair) { pair.0 == core_info.0 and pair.1 == core_info.1 })
    let metrics_found = metrics_resource.any(fn(pair) { pair.0 == core_info.0 and pair.1 == core_info.1 })
    
    assert_eq(trace_found, true)
    assert_eq(metrics_found, true)
  }
}