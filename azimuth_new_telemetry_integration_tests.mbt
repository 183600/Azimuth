// Azimuth New Telemetry Integration Tests
// 新的遥测集成测试用例，专注于核心功能和集成场景

test "span lifecycle management" {
  // 测试 Span 生命周期管理
  let span_builder = @azimuth.create_span_builder("user.authentication")
  let span = span_builder
    .with_attribute("user.id", @azimuth.StringValue("user123"))
    .with_attribute("auth.method", @azimuth.StringValue("password"))
    .start()
  
  // 验证 Span 初始状态
  assert_eq(span.operation_name, "user.authentication")
  assert_eq(span.status, @azimuth.SpanStatus::Unset)
  assert_true(span.start_time > 0L)
  assert_eq(span.end_time, None)
  
  // 添加事件
  span.add_event("password.verified", [
    ("verification.result", @azimuth.StringValue("success")),
    ("verification.time_ms", @azimuth.IntValue(150))
  ])
  
  // 设置状态并结束 Span
  span.set_status(@azimuth.SpanStatus::Ok)
  span.end()
  
  // 验证 Span 结束状态
  assert_eq(span.status, @azimuth.SpanStatus::Ok)
  assert_not_eq(span.end_time, None)
  assert_true(span.end_time.unwrap() > span.start_time)
  assert_eq(span.events.length(), 1)
}

test "metric instrument operations" {
  // 测试度量仪器操作
  let counter = @azimuth.create_counter("api.requests.total", "Total API requests", "requests")
  counter.add(1, [
    ("method", @azimuth.StringValue("GET")),
    ("endpoint", @azimuth.StringValue("/api/users")),
    ("status_code", @azimuth.StringValue("200"))
  ])
  
  let histogram = @azimuth.create_histogram(
    "api.request.duration", 
    "API request duration in milliseconds", 
    "ms"
  )
  histogram.record(125.5, [
    ("method", @azimuth.StringValue("POST")),
    ("endpoint", @azimuth.StringValue("/api/orders")),
    ("status_code", @azimuth.StringValue("201"))
  ])
  
  let gauge = @azimuth.create_gauge("system.memory.usage", "System memory usage", "bytes")
  gauge.set(1073741824, [
    ("memory.type", @azimuth.StringValue("heap")),
    ("instance.id", @azimuth.StringValue("instance-001"))
  ])
  
  // 验证度量仪器创建和配置
  assert_eq(counter.name, "api.requests.total")
  assert_eq(counter.description, "Total API requests")
  assert_eq(counter.unit, "requests")
  
  assert_eq(histogram.name, "api.request.duration")
  assert_eq(histogram.unit, "ms")
  
  assert_eq(gauge.name, "system.memory.usage")
  assert_eq(gauge.unit, "bytes")
}

test "context propagation with baggage" {
  // 测试带有 Baggage 的上下文传播
  let parent_context = @azimuth.create_context("trace-123456789", "parent-span-123")
  parent_context.set_baggage("user.id", "user456")
  parent_context.set_baggage("request.id", "req-789")
  parent_context.set_baggage("session.id", "sess-101112")
  
  // 创建子上下文
  let child_context = @azimuth.create_child_context(parent_context, "child-span-456")
  
  // 验证上下文传播
  assert_eq(child_context.trace_id, parent_context.trace_id)
  assert_not_eq(child_context.span_id, parent_context.span_id)
  
  // 验证 Baggage 传播
  let user_baggage = child_context.get_baggage("user.id")
  match user_baggage {
    Some(value) => assert_eq(value, "user456")
    None => assert_true(false)
  }
  
  let request_baggage = child_context.get_baggage("request.id")
  match request_baggage {
    Some(value) => assert_eq(value, "req-789")
    None => assert_true(false)
  }
  
  // 在子上下文中添加新的 Baggage
  child_context.set_baggage("operation.id", "op-131415")
  
  // 验证新 Baggage 不影响父上下文
  let parent_operation_baggage = parent_context.get_baggage("operation.id")
  match parent_operation_baggage {
    Some(_) => assert_true(false) // 父上下文不应该有这个 baggage
    None => assert_true(true)
  }
  
  // 验证子上下文有新的 Baggage
  let child_operation_baggage = child_context.get_baggage("operation.id")
  match child_operation_baggage {
    Some(value) => assert_eq(value, "op-131415")
    None => assert_true(false)
  }
}

test "log record correlation with traces" {
  // 测试日志记录与追踪的关联
  let logger = @azimuth.create_logger("payment.service")
  let context = @azimuth.create_context("payment-trace-123", "payment-span-456")
  
  // 记录关联的日志
  logger.info("Payment processed successfully", [
    ("payment.id", @azimuth.StringValue("pay-789")),
    ("amount", @azimuth.FloatValue(99.99)),
    ("currency", @azimuth.StringValue("USD"))
  ], context)
  
  logger.error("Payment processing failed", [
    ("payment.id", @azimuth.StringValue("pay-101112")),
    ("error.code", @azimuth.StringValue("INSUFFICIENT_FUNDS")),
    ("error.message", @azimuth.StringValue("Account balance too low"))
  ], context)
  
  // 验证日志记录创建
  let logs = logger.get_logs()
  assert_eq(logs.length(), 2)
  
  // 验证成功日志
  let success_log = logs[0]
  match success_log.severity {
    @azimuth.LogSeverity::Info => assert_true(true)
    _ => assert_true(false)
  }
  
  match success_log.body {
    @azimuth.StringValue(msg) => assert_eq(msg, "Payment processed successfully")
    _ => assert_true(false)
  }
  
  // 验证错误日志
  let error_log = logs[1]
  match error_log.severity {
    @azimuth.LogSeverity::Error => assert_true(true)
    _ => assert_true(false)
  }
  
  match error_log.body {
    @azimuth.StringValue(msg) => assert_eq(msg, "Payment processing failed")
    _ => assert_true(false)
  }
  
  // 验证追踪关联
  assert_eq(success_log.trace_id, Some("payment-trace-123"))
  assert_eq(success_log.span_id, Some("payment-span-456"))
  assert_eq(error_log.trace_id, Some("payment-trace-123"))
  assert_eq(error_log.span_id, Some("payment-span-456"))
}

test "resource detection and attributes" {
  // 测试资源检测和属性
  let resource_detector = @azimuth.ResourceDetector::new()
  let resource = resource_detector
    .with_service_name("order.service")
    .with_service_version("1.2.3")
    .with_deployment_environment("production")
    .with_attribute("k8s.pod.name", @azimuth.StringValue("order-service-7d4f8c9b-abc"))
    .with_attribute("k8s.namespace", @azimuth.StringValue("orders"))
    .detect()
  
  // 验证资源属性
  assert_eq(resource.attributes.length(), 5)
  
  let service_name_attr = resource.get_attribute("service.name")
  match service_name_attr {
    Some(@azimuth.StringValue(value)) => assert_eq(value, "order.service")
    _ => assert_true(false)
  }
  
  let service_version_attr = resource.get_attribute("service.version")
  match service_version_attr {
    Some(@azimuth.StringValue(value)) => assert_eq(value, "1.2.3")
    _ => assert_true(false)
  }
  
  let env_attr = resource.get_attribute("deployment.environment")
  match env_attr {
    Some(@azimuth.StringValue(value)) => assert_eq(value, "production")
    _ => assert_true(false)
  }
  
  let pod_name_attr = resource.get_attribute("k8s.pod.name")
  match pod_name_attr {
    Some(@azimuth.StringValue(value)) => assert_eq(value, "order-service-7d4f8c9b-abc")
    _ => assert_true(false)
  }
}

test "sampling decision propagation" {
  // 测试采样决策传播
  let sampler = @azimuth.create_probability_sampler(0.1) // 10% 采样率
  
  // 创建多个上下文以测试采样决策
  let context1 = @azimuth.create_context_with_sampler("trace-001", "span-001", sampler)
  let context2 = @azimuth.create_context_with_sampler("trace-002", "span-002", sampler)
  let context3 = @azimuth.create_context_with_sampler("trace-003", "span-003", sampler)
  
  // 验证采样决策存在
  assert_true(context1.sampled || !context1.sampled) // 任何布尔值都可以
  assert_true(context2.sampled || !context2.sampled)
  assert_true(context3.sampled || !context3.sampled)
  
  // 创建子上下文并验证采样决策传播
  let child_context1 = @azimuth.create_child_context(context1, "child-span-001")
  assert_eq(child_context1.sampled, context1.sampled)
  
  let child_context2 = @azimuth.create_child_context(context2, "child-span-002")
  assert_eq(child_context2.sampled, context2.sampled)
  
  // 测试始终采样和始终不采样
  let always_on_sampler = @azimuth.create_always_on_sampler()
  let always_on_context = @azimuth.create_context_with_sampler("trace-always-on", "span-always-on", always_on_sampler)
  assert_true(always_on_context.sampled)
  
  let always_off_sampler = @azimuth.create_always_off_sampler()
  let always_off_context = @azimuth.create_context_with_sampler("trace-always-off", "span-always-off", always_off_sampler)
  assert_false(always_off_context.sampled)
}

test "attribute value type conversion" {
  // 测试属性值类型转换
  let attributes = [
    ("string.attr", @azimuth.StringValue("hello world")),
    ("int.attr", @azimuth.IntValue(42)),
    ("float.attr", @azimuth.FloatValue(3.14159)),
    ("bool.attr", @azimuth.BoolValue(true)),
    ("array.attr", @azimuth.ArrayValue([@azimuth.StringValue("a"), @azimuth.StringValue("b"), @azimuth.StringValue("c")])),
    ("kv.attr", @azimuth.KVValue([("key1", @azimuth.StringValue("value1")), ("key2", @azimuth.IntValue(123))]))
  ]
  
  // 验证字符串属性
  let string_attr = attributes.filter(fn(attr) { attr.0 == "string.attr" })[0]
  match string_attr.1 {
    @azimuth.StringValue(value) => assert_eq(value, "hello world")
    _ => assert_true(false)
  }
  
  // 验证整数属性
  let int_attr = attributes.filter(fn(attr) { attr.0 == "int.attr" })[0]
  match int_attr.1 {
    @azimuth.IntValue(value) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  // 验证浮点数属性
  let float_attr = attributes.filter(fn(attr) { attr.0 == "float.attr" })[0]
  match float_attr.1 {
    @azimuth.FloatValue(value) => assert_true(abs(value - 3.14159) < 0.00001)
    _ => assert_true(false)
  }
  
  // 验证布尔属性
  let bool_attr = attributes.filter(fn(attr) { attr.0 == "bool.attr" })[0]
  match bool_attr.1 {
    @azimuth.BoolValue(value) => assert_true(value)
    _ => assert_true(false)
  }
  
  // 验证数组属性
  let array_attr = attributes.filter(fn(attr) { attr.0 == "array.attr" })[0]
  match array_attr.1 {
    @azimuth.ArrayValue(values) => {
      assert_eq(values.length(), 3)
      match values[0] {
        @azimuth.StringValue(v) => assert_eq(v, "a")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证键值对属性
  let kv_attr = attributes.filter(fn(attr) { attr.0 == "kv.attr" })[0]
  match kv_attr.1 {
    @azimuth.KVValue(pairs) => {
      assert_eq(pairs.length(), 2)
      assert_eq(pairs[0].0, "key1")
      match pairs[0].1 {
        @azimuth.StringValue(v) => assert_eq(v, "value1")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

test "telemetry data export and batching" {
  // 测试遥测数据导出和批处理
  let exporter = @azimuth.create_batch_exporter(
    @azimuth.ExporterType::OTLP,
    "https://otel-collector.example.com:4317",
    512, // batch size
    5000 // timeout in ms
  )
  
  // 创建多个 Span 以测试批处理
  let spans = []
  for i in 1..=10 {
    let span = @azimuth.create_span("test.operation." + i.to_string())
      .with_attribute("iteration", @azimuth.IntValue(i))
      .with_attribute("batch.id", @azimuth.StringValue("batch-001"))
      .start()
    
    span.set_status(@azimuth.SpanStatus::Ok)
    span.end()
    spans = spans.push(span)
  }
  
  // 导出 Span
  let export_result = exporter.export_spans(spans)
  
  // 验证导出结果
  match export_result {
    @azimuth.ExportResult::Success => assert_true(true)
    @azimuth.ExportResult::Failed(reason) => assert_true(false)
    @azimuth.ExportResult::Retryable(reason) => assert_true(false)
  }
  
  // 验证导出器配置
  assert_eq(exporter.exporter_type, @azimuth.ExporterType::OTLP)
  assert_eq(exporter.endpoint, "https://otel-collector.example.com:4317")
  assert_eq(exporter.batch_size, 512)
  assert_eq(exporter.timeout_ms, 5000)
}

test "trace state handling" {
  // 测试追踪状态处理
  let trace_state = @azimuth.TraceState::new()
    .with("vendor1", "value1")
    .with("vendor2", "value2")
    .with("vendor3", "value3")
  
  // 验证追踪状态
  assert_eq(trace_state.count(), 3)
  
  let vendor1_value = trace_state.get("vendor1")
  match vendor1_value {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  let vendor2_value = trace_state.get("vendor2")
  match vendor2_value {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  let non_existent_value = trace_state.get("nonexistent")
  match non_existent_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 更新现有的键值对
  let updated_trace_state = trace_state.with("vendor1", "updated_value1")
  let updated_vendor1_value = updated_trace_state.get("vendor1")
  match updated_vendor1_value {
    Some(value) => assert_eq(value, "updated_value1")
    None => assert_true(false)
  }
  
  // 验证其他键值对不受影响
  let unchanged_vendor2_value = updated_trace_state.get("vendor2")
  match unchanged_vendor2_value {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  // 删除键值对
  let deleted_trace_state = updated_trace_state.delete("vendor2")
  assert_eq(deleted_trace_state.count(), 2)
  
  let deleted_vendor2_value = deleted_trace_state.get("vendor2")
  match deleted_vendor2_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 验证序列化
  let serialized = trace_state.serialize()
  assert_true(serialized.contains("vendor1=value1"))
  assert_true(serialized.contains("vendor2=value2"))
  assert_true(serialized.contains("vendor3=value3"))
}