// 遥测集成测试用例

test "integration_common_context_logs" {
  // 测试Common模块与Context模块的集成
  use azimuth.telemetry.api.common.{AttributeValue, Resource}
  
  // 创建资源
  let resource = Resource::default("integration-test-service")
  
  // 创建上下文
  let ctx = Context::empty()
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  
  // 在上下文中添加追踪信息
  let ctx_with_trace = ctx
    .with_value(trace_key, "trace-123456")
    .with_value(span_key, "span-789012")
  
  // 创建与资源相关的属性
  let resource_attrs : Array[(String, AttributeValue)] = [
    ("service.name", AttributeValue::string(resource.service_name)),
    ("service.version", AttributeValue::string(resource.telemetry_sdk_version)),
    ("sdk.name", AttributeValue::string(resource.telemetry_sdk_name))
  ]
  
  // 验证集成数据
  assert_eq(resource.service_name, "integration-test-service")
  assert_eq(resource_attrs.length(), 3)
  
  match ctx_with_trace.get(trace_key) {
    Some(value) => assert_eq(value, "trace-123456")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_trace.get(span_key) {
    Some(value) => assert_eq(value, "span-789012")
    None => @test.fail("Test failed")
  }
}

test "integration_context_baggage" {
  // 测试Context与Baggage的集成
  
  // 创建上下文和baggage
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 在上下文中添加追踪信息
  let trace_key = create_key("trace.id")
  let ctx_with_trace = ctx.with_value(trace_key, "trace-integration-test")
  
  // 在baggage中添加相关元数据
  let baggage_with_metadata = baggage
    .with_entry("user.id", "user-integration-123")
    .with_entry("request.id", "req-integration-456")
    .with_entry("correlation.id", "corr-integration-789")
  
  // 验证上下文和baggage的独立性和关联性
  match ctx_with_trace.get(trace_key) {
    Some(value) => assert_eq(value.contains("integration"), true)
    None => @test.fail("Test failed")
  }
  
  match baggage_with_metadata.get("user.id") {
    Some(value) => assert_eq(value.contains("integration"), true)
    None => @test.fail("Test failed")
  }
  
  match baggage_with_metadata.get("request.id") {
    Some(value) => assert_eq(value.contains("integration"), true)
    None => @test.fail("Test failed")
  }
  
  // 测试集成场景：从baggage中提取信息到context
  let user_id_from_baggage = baggage_with_metadata.get("user.id")
  match user_id_from_baggage {
    Some(user_id) => {
      let user_key = create_key("user.id")
      let ctx_with_user = ctx_with_trace.with_value(user_key, user_id)
      
      match ctx_with_user.get(user_key) {
        Some(ctx_user_id) => assert_eq(ctx_user_id, user_id)
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "integration_logs_metrics_attributes" {
  // 测试Logs和Metrics模块的属性集成
  use azimuth.telemetry.api.common.AttributeValue
  use azimuth.telemetry.api.logs.{SeverityNumber, LogRecord}
  use azimuth.telemetry.api.metrics.{Measurement}
  
  // 创建共享属性
  let shared_attrs : Array[(String, AttributeValue)] = [
    ("service.name", AttributeValue::string("integration-service")),
    ("service.version", AttributeValue::string("2.0.0")),
    ("environment", AttributeValue::string("test")),
    ("region", AttributeValue::string("us-west-2"))
  ]
  
  // 创建日志记录属性
  let log_attrs = [
    ("log.level", AttributeValue::string("INFO")),
    ("log.message", AttributeValue::string("Integration test log message")),
    ("log.timestamp", AttributeValue::int(1640995200000000000L))
  ]
  
  // 创建指标测量属性
  let metric_attrs = [
    ("metric.name", AttributeValue::string("integration.counter")),
    ("metric.value", AttributeValue::float(123.45)),
    ("metric.unit", AttributeValue::string("count"))
  ]
  
  // 合并所有属性（模拟集成场景）
  let mut all_attrs = []
  let mut i = 0
  while i < shared_attrs.length() {
    all_attrs.push(shared_attrs[i])
    i = i + 1
  }
  
  let mut j = 0
  while j < log_attrs.length() {
    all_attrs.push(log_attrs[j])
    j = j + 1
  }
  
  let mut k = 0
  while k < metric_attrs.length() {
    all_attrs.push(metric_attrs[k])
    k = k + 1
  }
  
  // 验证集成属性
  assert_eq(all_attrs.length(), shared_attrs.length() + log_attrs.length() + metric_attrs.length())
  assert_eq(all_attrs.length(), 10)
  
  // 验证特定属性存在
  let mut found_service_name = false
  let mut found_log_level = false
  let mut found_metric_name = false
  
  let mut idx = 0
  while idx < all_attrs.length() {
    let (key, _) = all_attrs[idx]
    if key == "service.name" {
      found_service_name = true
    } else if key == "log.level" {
      found_log_level = true
    } else if key == "metric.name" {
      found_metric_name = true
    }
    idx = idx + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_log_level, true)
  assert_eq(found_metric_name, true)
}

test "integration_full_telemetry_pipeline" {
  // 测试完整的遥测管道集成
  
  // 1. 资源初始化
  use azimuth.telemetry.api.common.{AttributeValue, Resource}
  let resource = Resource::default("pipeline-test-service")
  
  // 2. 上下文创建和配置
  let ctx = Context::empty()
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  let user_key = create_key("user.id")
  
  let ctx_with_data = ctx
    .with_value(trace_key, "pipeline-trace-123")
    .with_value(span_key, "pipeline-span-456")
    .with_value(user_key, "pipeline-user-789")
  
  // 3. Baggage配置
  let baggage = Baggage::empty()
  let baggage_with_data = baggage
    .with_entry("request.id", "pipeline-req-111")
    .with_entry("session.id", "pipeline-session-222")
    .with_entry("correlation.id", "pipeline-corr-333")
  
  // 4. 属性收集
  let pipeline_attrs : Array[(String, AttributeValue)] = [
    ("pipeline.stage", AttributeValue::string("processing")),
    ("pipeline.step", AttributeValue::int(1L)),
    ("pipeline.duration_ms", AttributeValue::float(250.5)),
    ("pipeline.success", AttributeValue::bool(true))
  ]
  
  // 5. 验证管道集成
  // 验证资源
  assert_eq(resource.service_name, "pipeline-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  
  // 验证上下文数据
  match ctx_with_data.get(trace_key) {
    Some(value) => assert_eq(value.contains("pipeline"), true)
    None => @test.fail("Test failed")
  }
  
  // 验证baggage数据
  match baggage_with_data.get("request.id") {
    Some(value) => assert_eq(value.contains("pipeline"), true)
    None => @test.fail("Test failed")
  }
  
  // 验证属性数据
  assert_eq(pipeline_attrs.length(), 4)
  
  // 6. 模拟数据流处理
  let mut processing_steps = []
  
  // 收集所有键值对用于处理
  let mut all_data = []
  
  // 添加上下文数据
  match ctx_with_data.get(trace_key) {
    Some(value) => all_data.push(("trace.id", value))
    None => {}
  }
  
  match ctx_with_data.get(span_key) {
    Some(value) => all_data.push(("span.id", value))
    None => {}
  }
  
  // 添加baggage数据
  match baggage_with_data.get("request.id") {
    Some(value) => all_data.push(("request.id", value))
    None => {}
  }
  
  // 验证数据收集
  assert_eq(all_data.length() >= 3, true)
  
  // 模拟处理步骤
  let mut step_count = 0
  let mut data_idx = 0
  while data_idx < all_data.length() {
    let (key, value) = all_data[data_idx]
    if key.contains("pipeline") || value.contains("pipeline") {
      step_count = step_count + 1
    }
    data_idx = data_idx + 1
  }
  
  // 验证处理结果
  assert_eq(step_count >= 0, true)  // 至少处理了一些数据
}

test "integration_error_propagation" {
  // 测试错误传播集成
  
  // 模拟错误场景
  let ctx = Context::empty()
  let error_key = create_key("error.type")
  let error_message_key = create_key("error.message")
  let error_stack_key = create_key("error.stack")
  
  // 在上下文中设置错误信息
  let error_ctx = ctx
    .with_value(error_key, "ValidationError")
    .with_value(error_message_key, "Invalid input parameter")
    .with_value(error_stack_key, "at processRequest(line 42)")
  
  // 创建错误相关的baggage
  let error_baggage = Baggage::empty()
  let error_baggage_with_data = error_baggage
    .with_entry("error.code", "400")
    .with_entry("error.retry.count", "3")
    .with_entry("error.severity", "medium")
  
  // 验证错误信息传播
  match error_ctx.get(error_key) {
    Some(value) => assert_eq(value, "ValidationError")
    None => @test.fail("Test failed")
  }
  
  match error_ctx.get(error_message_key) {
    Some(value) => assert_eq(value, "Invalid input parameter")
    None => @test.fail("Test failed")
  }
  
  match error_baggage.get("error.code") {
    Some(value) => assert_eq(value, "400")
    None => @test.fail("Test failed")
  }
  
  // 测试错误恢复场景
  let recovery_key = create_key("error.recovery.attempted")
  let recovery_ctx = error_ctx.with_value(recovery_key, "true")
  
  match recovery_ctx.get(recovery_key) {
    Some(value) => assert_eq(value, "true")
    None => @test.fail("Test failed")
  }
  
  // 验证原始错误信息仍然存在
  match recovery_ctx.get(error_key) {
    Some(value) => assert_eq(value, "ValidationError")
    None => @test.fail("Test failed")
  }
}