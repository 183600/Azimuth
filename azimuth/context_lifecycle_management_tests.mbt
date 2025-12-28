// 上下文生命周期管理测试 - 测试上下文的创建、传递、修改和销毁
// Context Lifecycle Management Test - Testing context creation, propagation, modification, and destruction

test "基础上下文创建和销毁测试" {
  // 创建根上下文
  let root_ctx = Context::root()
  
  // 验证根上下文的基本属性
  let root_value = Context::get(root_ctx, ContextKey::new("any.key"))
  assert_eq(root_value, None) // 根上下文应该没有值
  
  // 创建带值的上下文
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(root_ctx, key, "test.value")
  
  // 验证值设置成功
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test.value"))
  
  // 验证原始上下文不受影响
  let root_value_after = Context::get(root_ctx, key)
  assert_eq(root_value_after, None)
  
  // 创建新的上下文链
  let key2 = ContextKey::new("second.key")
  let ctx_with_two_values = Context::with_value(ctx_with_value, key2, "second.value")
  
  // 验证两个值都存在
  let first_value = Context::get(ctx_with_two_values, key)
  let second_value = Context::get(ctx_with_two_values, key2)
  assert_eq(first_value, Some("test.value"))
  assert_eq(second_value, Some("second.value"))
}

test "上下文链式传播测试" {
  // 创建上下文链
  let ctx = Context::root()
  
  // 第一层：用户信息
  let user_key = ContextKey::new("user.id")
  ctx = Context::with_value(ctx, user_key, "user123")
  
  // 第二层：请求信息
  let request_key = ContextKey::new("request.id")
  ctx = Context::with_value(ctx, request_key, "req456")
  
  // 第三层：追踪信息
  let trace_key = ContextKey::new("trace.id")
  ctx = Context::with_value(ctx, trace_key, "trace789")
  
  // 第四层：业务信息
  let business_key = ContextKey::new("business.unit")
  ctx = Context::with_value(ctx, business_key, "payments")
  
  // 验证所有层级的值都可访问
  let user_id = Context::get(ctx, user_key)
  let request_id = Context::get(ctx, request_key)
  let trace_id = Context::get(ctx, trace_key)
  let business_unit = Context::get(ctx, business_key)
  
  assert_eq(user_id, Some("user123"))
  assert_eq(request_id, Some("req456"))
  assert_eq(trace_id, Some("trace789"))
  assert_eq(business_unit, Some("payments"))
}

test "上下文值覆盖和继承测试" {
  // 创建基础上下文
  let base_ctx = Context::root()
  let shared_key = ContextKey::new("shared.key")
  
  // 设置初始值
  let ctx_v1 = Context::with_value(base_ctx, shared_key, "value.v1")
  let v1_result = Context::get(ctx_v1, shared_key)
  assert_eq(v1_result, Some("value.v1"))
  
  // 覆盖值
  let ctx_v2 = Context::with_value(ctx_v1, shared_key, "value.v2")
  let v2_result = Context::get(ctx_v2, shared_key)
  assert_eq(v2_result, Some("value.v2"))
  
  // 验证原始上下文不受影响
  let v1_still_valid = Context::get(ctx_v1, shared_key)
  assert_eq(v1_still_valid, Some("value.v1"))
  
  // 添加新键
  let new_key = ContextKey::new("new.key")
  let ctx_with_new = Context::with_value(ctx_v2, new_key, "new.value")
  
  // 验证新值和覆盖值都存在
  let shared_value = Context::get(ctx_with_new, shared_key)
  let new_value = Context::get(ctx_with_new, new_key)
  
  assert_eq(shared_value, Some("value.v2"))
  assert_eq(new_value, Some("new.value"))
}

test "复杂上下文嵌套测试" {
  // 创建多层嵌套的上下文结构
  let ctx = Context::root()
  
  // 服务层上下文
  let service_key = ContextKey::new("service.name")
  ctx = Context::with_value(ctx, service_key, "api-gateway")
  
  let version_key = ContextKey::new("service.version")
  ctx = Context::with_value(ctx, version_key, "1.2.3")
  
  // 请求层上下文
  let method_key = ContextKey::new("http.method")
  ctx = Context::with_value(ctx, method_key, "POST")
  
  let path_key = ContextKey::new("http.path")
  ctx = Context::with_value(ctx, path_key, "/api/payments")
  
  // 用户层上下文
  let user_key = ContextKey::new("user.id")
  ctx = Context::with_value(ctx, user_key, "user456")
  
  let tenant_key = ContextKey::new("tenant.id")
  ctx = Context::with_value(ctx, tenant_key, "tenant789")
  
  // 追踪层上下文
  let trace_key = ContextKey::new("trace.id")
  ctx = Context::with_value(ctx, trace_key, "trace123456")
  
  let span_key = ContextKey::new("span.id")
  ctx = Context::with_value(ctx, span_key, "span789012")
  
  // 验证所有嵌套值
  assert_eq(Context::get(ctx, service_key), Some("api-gateway"))
  assert_eq(Context::get(ctx, version_key), Some("1.2.3"))
  assert_eq(Context::get(ctx, method_key), Some("POST"))
  assert_eq(Context::get(ctx, path_key), Some("/api/payments"))
  assert_eq(Context::get(ctx, user_key), Some("user456"))
  assert_eq(Context::get(ctx, tenant_key), Some("tenant789"))
  assert_eq(Context::get(ctx, trace_key), Some("trace123456"))
  assert_eq(Context::get(ctx, span_key), Some("span789012"))
}

test "上下文与Span集成测试" {
  // 创建Span上下文
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // 创建应用上下文
  let app_ctx = Context::root()
  let trace_key = ContextKey::new("trace.context")
  let app_ctx_with_span = Context::with_value(app_ctx, trace_key, trace_id + ":" + span_id)
  
  // 添加业务上下文
  let business_key = ContextKey::new("business.context")
  let full_ctx = Context::with_value(app_ctx_with_span, business_key, "payment.processing")
  
  // 验证Span和业务上下文集成
  let trace_context = Context::get(full_ctx, trace_key)
  let business_context = Context::get(full_ctx, business_key)
  
  assert_eq(trace_context, Some(trace_id + ":" + span_id))
  assert_eq(business_context, Some("payment.processing"))
  
  // 验证Span上下文独立工作
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_valid(span_ctx))
}

test "上下文与传播器集成测试" {
  // 创建传播器
  let propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([propagator])
  
  // 创建包含丰富信息的上下文
  let ctx = Context::root()
  let trace_key = ContextKey::new("trace.id")
  ctx = Context::with_value(ctx, trace_key, "1234567890abcdef")
  
  let baggage_key = ContextKey::new("baggage")
  ctx = Context::with_value(ctx, baggage_key, "user.id=123,tenant.id=456")
  
  let correlation_key = ContextKey::new("correlation.id")
  ctx = Context::with_value(ctx, correlation_key, "corr-789012")
  
  // 创建载体
  let carrier = TextMapCarrier::new()
  
  // 注入上下文
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 验证注入结果
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // 提取上下文
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "上下文与日志集成测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "context-logger")
  
  // 创建包含丰富上下文的信息
  let ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  ctx = Context::with_value(ctx, user_key, "user123")
  
  let request_key = ContextKey::new("request.id")
  ctx = Context::with_value(ctx, request_key, "req456")
  
  let operation_key = ContextKey::new("operation.name")
  ctx = Context::with_value(ctx, operation_key, "process.payment")
  
  // 创建带有上下文的日志记录
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Operation completed successfully"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000100L),
    Some("trace123456"),
    Some("span789012"),
    Some(ctx)
  )
  
  // 发送日志
  Logger::emit(logger, log_record)
  
  // 验证日志和上下文集成
  assert_eq(LogRecord::body(log_record), Some("Operation completed successfully"))
  assert_eq(LogRecord::context(log_record), Some(ctx))
  assert_eq(LogRecord::trace_id(log_record), Some("trace123456"))
  assert_eq(LogRecord::span_id(log_record), Some("span789012"))
  
  // 验证上下文值仍然可访问
  assert_eq(Context::get(ctx, user_key), Some("user123"))
  assert_eq(Context::get(ctx, request_key), Some("req456"))
  assert_eq(Context::get(ctx, operation_key), Some("process.payment"))
}

test "上下文生命周期边界测试" {
  // 测试空键
  let empty_key = ContextKey::new("")
  let ctx = Context::root()
  let ctx_with_empty_key = Context::with_value(ctx, empty_key, "empty.key.value")
  
  let empty_key_value = Context::get(ctx_with_empty_key, empty_key)
  assert_eq(empty_key_value, Some("empty.key.value"))
  
  // 测试空值
  let normal_key = ContextKey::new("normal.key")
  let ctx_with_empty_value = Context::with_value(ctx, normal_key, "")
  
  let empty_value = Context::get(ctx_with_empty_value, normal_key)
  assert_eq(empty_value, Some(""))
  
  // 测试长键名
  let long_key_name = "this.is.a.very.long.key.name.that.tests.system.robustness"
  let long_key = ContextKey::new(long_key_name)
  let ctx_with_long_key = Context::with_value(ctx, long_key, "long.key.value")
  
  let long_key_value = Context::get(ctx_with_long_key, long_key)
  assert_eq(long_key_value, Some("long.key.value"))
  
  // 测试特殊字符键
  let special_key_name = "key.with.special-chars_and_underscores"
  let special_key = ContextKey::new(special_key_name)
  let ctx_with_special_key = Context::with_value(ctx, special_key, "special.value")
  
  let special_key_value = Context::get(ctx_with_special_key, special_key)
  assert_eq(special_key_value, Some("special.value"))
}

test "上下文性能和内存测试" {
  // 创建深层嵌套的上下文链
  let ctx = Context::root()
  
  // 创建100层嵌套
  for i in 0..100 {
    let key = ContextKey::new("level." + i.to_string())
    ctx = Context::with_value(ctx, key, "value.at.level." + i.to_string())
  }
  
  // 验证深层访问
  let level_0 = Context::get(ctx, ContextKey::new("level.0"))
  let level_50 = Context::get(ctx, ContextKey::new("level.50"))
  let level_99 = Context::get(ctx, ContextKey::new("level.99"))
  
  assert_eq(level_0, Some("value.at.level.0"))
  assert_eq(level_50, Some("value.at.level.50"))
  assert_eq(level_99, Some("value.at.level.99"))
  
  // 创建大量上下文实例
  let contexts = []
  for j in 0..50 {
    let temp_ctx = Context::root()
    let key = ContextKey::new("batch.key." + j.to_string())
    let batch_ctx = Context::with_value(temp_ctx, key, "batch.value." + j.to_string())
    contexts.push(batch_ctx)
  }
  
  // 验证批量上下文
  for k in 0..contexts.length() {
    let key = ContextKey::new("batch.key." + k.to_string())
    let value = Context::get(contexts[k], key)
    assert_eq(value, Some("batch.value." + k.to_string()))
  }
}

test "上下文并发生命周期测试" {
  // 创建共享基础上下文
  let base_ctx = Context::root()
  let shared_key = ContextKey::new("shared.base.key")
  let base_with_shared = Context::with_value(base_ctx, shared_key, "shared.base.value")
  
  // 创建多个派生上下文
  let derived_contexts = []
  for i in 0..10 {
    let ctx = base_with_shared
    let unique_key = ContextKey::new("unique.key." + i.to_string())
    ctx = Context::with_value(ctx, unique_key, "unique.value." + i.to_string())
    derived_contexts.push(ctx)
  }
  
  // 验证所有派生上下文都有共享值
  for i in 0..derived_contexts.length() {
    let shared_value = Context::get(derived_contexts[i], shared_key)
    assert_eq(shared_value, Some("shared.base.value"))
    
    let unique_key = ContextKey::new("unique.key." + i.to_string())
    let unique_value = Context::get(derived_contexts[i], unique_key)
    assert_eq(unique_value, Some("unique.value." + i.to_string()))
  }
  
  // 验证基础上下文未受影响
  let base_shared_value = Context::get(base_with_shared, shared_key)
  assert_eq(base_shared_value, Some("shared.base.value"))
  
  let missing_unique = Context::get(base_with_shared, ContextKey::new("unique.key.0"))
  assert_eq(missing_unique, None)
}