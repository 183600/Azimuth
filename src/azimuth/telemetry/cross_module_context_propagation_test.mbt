// 跨模块Context传播测试
// 测试Context在不同模块之间的传播，确保数据的完整性和一致性

test "context_propagation_between_trace_and_context_modules" {
  // 测试Context在trace和context模块之间的传播
  
  // 在context模块中创建context
  let base_ctx = Context::empty()
  let user_key = create_key("user_id")
  let request_key = create_key("request_id")
  
  let ctx_with_data = base_ctx
    .with_value(user_key, "user123")
    .with_value(request_key, "req456")
  
  // 在trace模块中使用context
  let tracer = NoopTracer::{}
  let (trace_ctx, span) = tracer.start_span(ctx_with_data, "cross_module_operation", Some(Server), None)
  
  // 验证context数据在trace模块中仍然可用
  match trace_ctx.get(user_key) {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Test failed")
  }
  
  match trace_ctx.get(request_key) {
    Some(value) => assert_eq(value, "req456")
    None => @test.fail("Test failed")
  }
  
  // 验证span正确创建
  assert_eq(span.name, "cross_module_operation")
  assert_eq(span.kind, Server)
}

test "context_propagation_with_baggage_integration" {
  // 测试Context与Baggage的集成传播
  
  // 创建带有baggage的context
  let base_ctx = Context::empty()
  let baggage = Baggage::empty()
    .with_entry("correlation_id", "corr789")
    .with_entry("session_id", "sess123")
    .with_entry("tenant_id", "tenant456")
  
  // 将baggage信息添加到context（模拟传播）
  let correlation_key = create_key("correlation_id")
  let session_key = create_key("session_id")
  let tenant_key = create_key("tenant_id")
  
  let ctx_with_baggage = base_ctx
    .with_value(correlation_key, "corr789")
    .with_value(session_key, "sess123")
    .with_value(tenant_key, "tenant456")
  
  // 在trace模块中使用带有baggage信息的context
  let tracer = NoopTracer::{}
  let (trace_ctx, span) = tracer.start_span(ctx_with_baggage, "baggage_operation", Some(Client), None)
  
  // 验证baggage信息在trace模块中可用
  match trace_ctx.get(correlation_key) {
    Some(value) => assert_eq(value, "corr789")
    None => @test.fail("Test failed")
  }
  
  match trace_ctx.get(session_key) {
    Some(value) => assert_eq(value, "sess123")
    None => @test.fail("Test failed")
  }
  
  match trace_ctx.get(tenant_key) {
    Some(value) => assert_eq(value, "tenant456")
    None => @test.fail("Test failed")
  }
  
  // 验证span属性
  assert_eq(span.name, "baggage_operation")
  assert_eq(span.kind, Client)
}

test "context_propagation_with_attribute_types" {
  // 测试Context传播时包含不同类型的属性
  
  // 创建包含各种类型属性的context
  let base_ctx = Context::empty()
  
  // 模拟将AttributeValue类型的信息存储在context中
  let string_key = create_key("string_data")
  let int_key = create_key("int_data")
  let float_key = create_key("float_data")
  let bool_key = create_key("bool_data")
  let array_key = create_key("array_data")
  
  let ctx_with_types = base_ctx
    .with_value(string_key, "string_value")
    .with_value(int_key, "42")
    .with_value(float_key, "3.14159")
    .with_value(bool_key, "true")
    .with_value(array_key, "[\"item1\",\"item2\",\"item3\"]")
  
  // 在trace模块中使用包含类型数据的context
  let tracer = NoopTracer::{}
  
  // 将context中的字符串数据转换为span属性
  let mut attributes = []
  
  match ctx_with_types.get(string_key) {
    Some(value) => {
      attributes.push(("string_data", AttributeValue::string(value)))
    }
    None => ()
  }
  
  match ctx_with_types.get(int_key) {
    Some(value) => {
      // 尝试将字符串转换为整数
      attributes.push(("int_data", AttributeValue::string(value)))
    }
    None => ()
  }
  
  match ctx_with_types.get(float_key) {
    Some(value) => {
      attributes.push(("float_data", AttributeValue::string(value)))
    }
    None => ()
  }
  
  match ctx_with_types.get(bool_key) {
    Some(value) => {
      attributes.push(("bool_data", AttributeValue::string(value)))
    }
    None => ()
  }
  
  match ctx_with_types.get(array_key) {
    Some(value) => {
      attributes.push(("array_data", AttributeValue::string(value)))
    }
    None => ()
  }
  
  let (_, span) = tracer.start_span(ctx_with_types, "typed_operation", Some(Internal), Some(attributes))
  
  // 验证span属性正确设置
  assert_eq(span.name, "typed_operation")
  assert_eq(span.kind, Internal)
  assert_eq(span.attributes.length(), 5)
  
  // 验证context数据仍然完整
  match ctx_with_types.get(string_key) {
    Some(value) => assert_eq(value, "string_value")
    None => @test.fail("Test failed")
  }
}

test "context_propagation_across_multiple_operations" {
  // 测试Context在多个操作间的传播
  
  // 初始化context
  let base_ctx = Context::empty()
  let operation_key = create_key("operation_id")
  let user_key = create_key("user_id")
  let trace_key = create_key("trace_id")
  
  let initial_ctx = base_ctx
    .with_value(operation_key, "op123")
    .with_value(user_key, "user456")
    .with_value(trace_key, "trace789")
  
  let tracer = NoopTracer::{}
  
  // 操作1：创建第一个span
  let (ctx1, span1) = tracer.start_span(initial_ctx, "operation_1", Some(Server), None)
  
  // 在操作1中添加新的context数据
  let step1_key = create_key("step1_result")
  let ctx1_enhanced = ctx1.with_value(step1_key, "success")
  
  // 操作2：基于增强的context创建第二个span
  let (ctx2, span2) = tracer.start_span(ctx1_enhanced, "operation_2", Some(Client), None)
  
  // 在操作2中添加更多数据
  let step2_key = create_key("step2_result")
  let ctx2_enhanced = ctx2.with_value(step2_key, "completed")
  
  // 操作3：基于进一步增强的context创建第三个span
  let (ctx3, span3) = tracer.start_span(ctx2_enhanced, "operation_3", Some(Internal), None)
  
  // 验证初始数据在所有操作中都保持不变
  match ctx3.get(operation_key) {
    Some(value) => assert_eq(value, "op123")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(user_key) {
    Some(value) => assert_eq(value, "user456")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(trace_key) {
    Some(value) => assert_eq(value, "trace789")
    None => @test.fail("Test failed")
  }
  
  // 验证每个操作添加的数据都存在
  match ctx3.get(step1_key) {
    Some(value) => assert_eq(value, "success")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(step2_key) {
    Some(value) => assert_eq(value, "completed")
    None => @test.fail("Test failed")
  }
  
  // 验证所有span都正确创建
  assert_eq(span1.name, "operation_1")
  assert_eq(span1.kind, Server)
  
  assert_eq(span2.name, "operation_2")
  assert_eq(span2.kind, Client)
  
  assert_eq(span3.name, "operation_3")
  assert_eq(span3.kind, Internal)
}

test "context_propagation_with_error_handling" {
  // 测试Context传播时的错误处理
  
  // 创建包含错误信息的context
  let base_ctx = Context::empty()
  let error_key = create_key("error_info")
  let retry_key = create_key("retry_count")
  
  let ctx_with_error = base_ctx
    .with_value(error_key, "connection_timeout")
    .with_value(retry_key, "3")
  
  let tracer = NoopTracer::{}
  
  // 创建表示错误的span
  let error_attributes = [
    ("error.type", AttributeValue::string("timeout")),
    ("error.retries", AttributeValue::int(3L)),
    ("error.severity", AttributeValue::string("high"))
  ]
  
  let (error_ctx, error_span) = tracer.start_span(ctx_with_error, "error_operation", Some(Client), Some(error_attributes))
  
  // 验证错误信息在context中保持
  match error_ctx.get(error_key) {
    Some(value) => assert_eq(value, "connection_timeout")
    None => @test.fail("Test failed")
  }
  
  match error_ctx.get(retry_key) {
    Some(value) => assert_eq(value, "3")
    None => @test.fail("Test failed")
  }
  
  // 验证错误span正确创建
  assert_eq(error_span.name, "error_operation")
  assert_eq(error_span.kind, Client)
  assert_eq(error_span.attributes.length(), 3)
  
  // 创建恢复操作的span
  let recovery_key = create_key("recovery_strategy")
  let recovery_ctx = error_ctx.with_value(recovery_key, "circuit_breaker")
  
  let (final_ctx, recovery_span) = tracer.start_span(recovery_ctx, "recovery_operation", Some(Server), None)
  
  // 验证所有信息都传播到最终context
  match final_ctx.get(error_key) {
    Some(value) => assert_eq(value, "connection_timeout")
    None => @test.fail("Test failed")
  }
  
  match final_ctx.get(retry_key) {
    Some(value) => assert_eq(value, "3")
    None => @test.fail("Test failed")
  }
  
  match final_ctx.get(recovery_key) {
    Some(value) => assert_eq(value, "circuit_breaker")
    None => @test.fail("Test failed")
  }
  
  // 验证恢复span正确创建
  assert_eq(recovery_span.name, "recovery_operation")
  assert_eq(recovery_span.kind, Server)
}

test "context_propagation_with_complex_scenarios" {
  // 测试Context传播的复杂场景
  
  // 创建复杂的初始context
  let base_ctx = Context::empty()
  
  // 用户相关数据
  let user_id_key = create_key("user_id")
  let user_role_key = create_key("user_role")
  let tenant_id_key = create_key("tenant_id")
  
  // 请求相关数据
  let request_id_key = create_key("request_id")
  let correlation_id_key = create_key("correlation_id")
  let session_id_key = create_key("session_id")
  
  // 系统相关数据
  let service_name_key = create_key("service_name")
  let version_key = create_key("version")
  let environment_key = create_key("environment")
  
  let complex_ctx = base_ctx
    .with_value(user_id_key, "user123")
    .with_value(user_role_key, "admin")
    .with_value(tenant_id_key, "tenant456")
    .with_value(request_id_key, "req789")
    .with_value(correlation_id_key, "corr012")
    .with_value(session_id_key, "sess345")
    .with_value(service_name_key, "api-service")
    .with_value(version_key, "2.1.0")
    .with_value(environment_key, "production")
  
  let tracer = NoopTracer::{}
  
  // 第一阶段：认证操作
  let auth_attributes = [
    ("auth.method", AttributeValue::string("jwt")),
    ("auth.success", AttributeValue::bool(true))
  ]
  
  let (auth_ctx, auth_span) = tracer.start_span(complex_ctx, "authentication", Some(Server), Some(auth_attributes))
  
  // 在认证后添加用户权限信息
  let permissions_key = create_key("user_permissions")
  let auth_ctx_enhanced = auth_ctx.with_value(permissions_key, "read,write,admin")
  
  // 第二阶段：业务逻辑操作
  let business_attributes = [
    ("operation.type", AttributeValue::string("data_processing")),
    ("business.rules.applied", AttributeValue::int(5L))
  ]
  
  let (business_ctx, business_span) = tracer.start_span(auth_ctx_enhanced, "business_logic", Some(Internal), Some(business_attributes))
  
  // 在业务逻辑后添加处理结果
  let result_key = create_key("processing_result")
  let business_ctx_enhanced = business_ctx.with_value(result_key, "success")
  
  // 第三阶段：数据访问操作
  let data_attributes = [
    ("db.system", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT")),
    ("db.rows_affected", AttributeValue::int(10L))
  ]
  
  let (final_ctx, data_span) = tracer.start_span(business_ctx_enhanced, "data_access", Some(Client), Some(data_attributes))
  
  // 验证所有初始数据在最终context中都保持
  match final_ctx.get(user_id_key) {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Test failed")
  }
  
  match final_ctx.get(user_role_key) {
    Some(value) => assert_eq(value, "admin")
    None => @test.fail("Test failed")
  }
  
  match final_ctx.get(tenant_id_key) {
    Some(value) => assert_eq(value, "tenant456")
    None => @test.fail("Test failed")
  }
  
  match final_ctx.get(service_name_key) {
    Some(value) => assert_eq(value, "api-service")
    None => @test.fail("Test failed")
  }
  
  // 验证每个阶段添加的数据都存在
  match final_ctx.get(permissions_key) {
    Some(value) => assert_eq(value, "read,write,admin")
    None => @test.fail("Test failed")
  }
  
  match final_ctx.get(result_key) {
    Some(value) => assert_eq(value, "success")
    None => @test.fail("Test failed")
  }
  
  // 验证所有span都正确创建
  assert_eq(auth_span.name, "authentication")
  assert_eq(auth_span.kind, Server)
  assert_eq(auth_span.attributes.length(), 2)
  
  assert_eq(business_span.name, "business_logic")
  assert_eq(business_span.kind, Internal)
  assert_eq(business_span.attributes.length(), 2)
  
  assert_eq(data_span.name, "data_access")
  assert_eq(data_span.kind, Client)
  assert_eq(data_span.attributes.length(), 3)
}

test "context_propagation_performance_considerations" {
  // 测试Context传播的性能考虑
  
  let base_ctx = Context::empty()
  
  // 创建包含大量数据的context
  let mut performance_ctx = base_ctx
  let mut i = 0
  while i < 100 {
    let key = create_key("perf_key_" + i.to_string())
    let value = "perf_value_" + i.to_string()
    performance_ctx = performance_ctx.with_value(key, value)
    i = i + 1
  }
  
  let tracer = NoopTracer::{}
  
  // 测试在大量数据context下的span创建性能
  let start_time = 1000L
  
  let mut ctx = performance_ctx
  let mut j = 0
  while j < 10 {
    let (new_ctx, span) = tracer.start_span(ctx, "performance_test_" + j.to_string(), Some(Server), None)
    ctx = new_ctx
    
    // 验证span正确创建
    assert_eq(span.name, "performance_test_" + j.to_string())
    assert_eq(span.kind, Server)
    
    // 验证context数据完整性
    let test_key = create_key("perf_key_" + (j * 10).to_string())
    match ctx.get(test_key) {
      Some(value) => assert_eq(value, "perf_value_" + (j * 10).to_string())
      None => @test.fail("Test failed at iteration " + j.to_string())
    }
    
    j = j + 1
  }
  
  // 验证最终context仍然包含所有数据
  i = 0
  while i < 100 {
    let key = create_key("perf_key_" + i.to_string())
    let expected_value = "perf_value_" + i.to_string()
    match ctx.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Test failed for key perf_key_" + i.to_string())
    }
    i = i + 1
  }
}