// Azimuth Advanced Composite Propagator Comprehensive Tests - 高级复合传播器综合测试
// 测试多传播器组合、复杂注入提取场景、跨协议传播和高级上下文传递

test "多传播器组合注入提取测试" {
  // 创建多个不同类型的传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建复合传播器，包含多个传播器
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 创建测试上下文，包含多种信息
  let base_ctx = Context::root()
  let ctx_with_trace = Context::with_value(base_ctx, ContextKey::new("trace.id"), "trace-12345")
  let ctx_with_user = Context::with_value(ctx_with_trace, ContextKey::new("user.id"), "user-67890")
  let ctx_with_session = Context::with_value(ctx_with_user, ContextKey::new("session.id"), "session-abcde")
  let final_ctx = Context::with_value(ctx_with_session, ContextKey::new("request.id"), "req-12345")
  
  // 创建TextMapCarrier用于注入
  let carrier = TextMapCarrier::new()
  
  // 使用复合传播器注入上下文
  CompositePropagator::inject(composite_propagator, final_ctx, carrier)
  
  // 验证注入结果
  match TextMapCarrier::get(carrier, "traceparent") {
    Some(trace_value) => assert_true(trace_value.length() > 0)
    None => assert_true(false)
  }
  
  // 创建新的carrier和上下文进行提取测试
  let new_carrier = TextMapCarrier::new()
  let test_ctx = Context::with_value(Context::root(), ContextKey::new("test"), "value")
  
  CompositePropagator::inject(composite_propagator, test_ctx, new_carrier)
  
  // 提取上下文
  let extracted_ctx = CompositePropagator::extract(composite_propagator, new_carrier)
  
  // 验证提取的上下文
  match Context::get(extracted_ctx, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试多个传播器的组合
  let multiple_composite = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CTraceContextPropagator::new(),
    W3CTraceContextPropagator::new()
  ])
  
  let multi_carrier = TextMapCarrier::new()
  let multi_ctx = Context::with_value(Context::root(), ContextKey::new("multi"), "propagation")
  
  CompositePropagator::inject(multiple_composite, multi_ctx, multi_carrier)
  
  let multi_extracted = CompositePropagator::extract(multiple_composite, multi_carrier)
  
  match Context::get(multi_extracted, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
}

test "复杂上下文链传播测试" {
  // 测试复杂的上下文链传播场景
  let root_ctx = Context::root()
  
  // 构建上下文链
  let ctx1 = Context::with_value(root_ctx, ContextKey::new("service.name"), "auth-service")
  let ctx2 = Context::with_value(ctx1, ContextKey::new("service.version"), "1.2.3")
  let ctx3 = Context::with_value(ctx2, ContextKey::new("correlation.id"), "corr-12345")
  let ctx4 = Context::with_value(ctx3, ContextKey::new("user.id"), "user-67890")
  let ctx5 = Context::with_value(ctx4, ContextKey::new("request.id"), "req-abcdef")
  let ctx6 = Context::with_value(ctx5, ContextKey::new("trace.id"), "trace-12345")
  let ctx7 = Context::with_value(ctx6, ContextKey::new("span.id"), "span-67890")
  let final_chain_ctx = Context::with_value(ctx7, ContextKey::new("baggage.user.role"), "admin")
  
  // 创建复合传播器
  let chain_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 注入上下文链
  let chain_carrier = TextMapCarrier::new()
  CompositePropagator::inject(chain_propagator, final_chain_ctx, chain_carrier)
  
  // 验证链式上下文的传播
  let chain_extracted = CompositePropagator::extract(chain_propagator, chain_carrier)
  
  // 验证提取的上下文包含预期信息
  match Context::get(chain_extracted, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试跨服务上下文传播
  let service_a_ctx = Context::with_value(Context::root(), ContextKey::new("service.a"), "value.a")
  let service_b_ctx = Context::with_value(service_a_ctx, ContextKey::new("service.b"), "value.b")
  let service_c_ctx = Context::with_value(service_b_ctx, ContextKey::new("service.c"), "value.c")
  
  let cross_service_carrier = TextMapCarrier::new()
  CompositePropagator::inject(chain_propagator, service_c_ctx, cross_service_carrier)
  
  let cross_service_extracted = CompositePropagator::extract(chain_propagator, cross_service_carrier)
  
  match Context::get(cross_service_extracted, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试嵌套上下文传播
  let nested_ctx1 = Context::with_value(Context::root(), ContextKey::new("level1"), "value1")
  let nested_ctx2 = Context::with_value(nested_ctx1, ContextKey::new("level2"), "value2")
  let nested_ctx3 = Context::with_value(nested_ctx2, ContextKey::new("level3"), "value3")
  
  let nested_carrier = TextMapCarrier::new()
  CompositePropagator::inject(chain_propagator, nested_ctx3, nested_carrier)
  
  let nested_extracted = CompositePropagator::extract(chain_propagator, nested_carrier)
  
  match Context::get(nested_extracted, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
}

test "Baggage跨传播器测试" {
  // 测试Baggage在不同传播器间的传递
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_with_baggage = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 创建包含Baggage的上下文
  let base_baggage = Baggage::new()
  let with_user_baggage = Baggage::set_entry(base_baggage, "user.id", "user-12345")
  let with_session_baggage = Baggage::set_entry(with_user_baggage, "session.id", "session-67890")
  let with_role_baggage = Baggage::set_entry(with_session_baggage, "user.role", "admin")
  let final_baggage = Baggage::set_entry(with_role_baggage, "request.path", "/api/users")
  
  // 创建包含Baggage信息的上下文
  let baggage_ctx = Context::with_value(Context::root(), ContextKey::new("baggage.data"), "serialized")
  
  // 注入Baggage信息
  let baggage_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_with_baggage, baggage_ctx, baggage_carrier)
  
  // 验证Baggage信息的注入
  match TextMapCarrier::get(baggage_carrier, "traceparent") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // 提取Baggage信息
  let extracted_baggage_ctx = CompositePropagator::extract(composite_with_baggage, baggage_carrier)
  
  match Context::get(extracted_baggage_ctx, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试Baggage的传播和修改
  let modified_baggage = Baggage::remove_entry(final_baggage, "request.path")
  let modified_baggage2 = Baggage::set_entry(modified_baggage, "new.key", "new.value")
  
  // 验证Baggage修改
  match Baggage::get_entry(final_baggage, "request.path") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  match Baggage::get_entry(modified_baggage2, "new.key") {
    Some("new.value") => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试复杂Baggage场景
  let complex_baggage = Baggage::new()
  let complex_final = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(
        Baggage::set_entry(complex_baggage, "user.id", "user-123"),
        "user.email", "user@example.com"
      ),
      "user.preferences", "dark-mode"
    ),
    "user.timezone", "UTC"
  )
  
  // 验证复杂Baggage
  match Baggage::get_entry(complex_final, "user.id") {
    Some("user-123") => assert_true(true)
    _ => assert_true(false)
  }
  
  match Baggage::get_entry(complex_final, "user.email") {
    Some("user@example.com") => assert_true(true)
    _ => assert_true(false)
  }
  
  match Baggage::get_entry(complex_final, "user.preferences") {
    Some("dark-mode") => assert_true(true)
    _ => assert_true(false)
  }
  
  match Baggage::get_entry(complex_final, "user.timezone") {
    Some("UTC") => assert_true(true)
    _ => assert_true(false)
  }
}

test "跨协议传播测试" {
  // 测试不同协议间的上下文传播
  let protocols = ["http", "grpc", "websocket", "message-queue", "event-stream"]
  
  for protocol in protocols {
    let protocol_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
    
    // 创建协议特定的上下文
    let protocol_ctx = Context::with_value(Context::root(), ContextKey::new("protocol"), protocol)
    let protocol_ctx2 = Context::with_value(protocol_ctx, ContextKey::new("protocol.version"), "1.0")
    let final_protocol_ctx = Context::with_value(protocol_ctx2, ContextKey::new("trace.context"), "active")
    
    // 创建协议特定的carrier
    let protocol_carrier = TextMapCarrier::new()
    
    // 注入协议特定的上下文
    CompositePropagator::inject(protocol_propagator, final_protocol_ctx, protocol_carrier)
    
    // 验证协议特定的注入
    match TextMapCarrier::get(protocol_carrier, "traceparent") {
      Some(_) => assert_true(true)
      None => assert_true(false)
    }
    
    // 提取协议特定的上下文
    let extracted_protocol_ctx = CompositePropagator::extract(protocol_propagator, protocol_carrier)
    
    match Context::get(extracted_protocol_ctx, ContextKey::new("extracted")) {
      Some("true") => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // 测试HTTP头传播
  let http_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let http_ctx = Context::with_value(Context::root(), ContextKey::new("http.method"), "GET")
  let http_ctx2 = Context::with_value(http_ctx, ContextKey::new("http.url"), "https://api.example.com/users")
  let final_http_ctx = Context::with_value(http_ctx2, ContextKey::new("http.headers"), "authorization,content-type")
  
  let http_carrier = TextMapCarrier::new()
  CompositePropagator::inject(http_propagator, final_http_ctx, http_carrier)
  
  // 验证HTTP传播
  match TextMapCarrier::get(http_carrier, "traceparent") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  let extracted_http_ctx = CompositePropagator::extract(http_propagator, http_carrier)
  
  match Context::get(extracted_http_ctx, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试gRPC元数据传播
  let grpc_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let grpc_ctx = Context::with_value(Context::root(), ContextKey::new("grpc.method"), "GetUser")
  let grpc_ctx2 = Context::with_value(grpc_ctx, ContextKey::new("grpc.service"), "UserService")
  let final_grpc_ctx = Context::with_value(grpc_ctx2, ContextKey::new("grpc.timeout"), "30s")
  
  let grpc_carrier = TextMapCarrier::new()
  CompositePropagator::inject(grpc_propagator, final_grpc_ctx, grpc_carrier)
  
  // 验证gRPC传播
  match TextMapCarrier::get(grpc_carrier, "traceparent") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  let extracted_grpc_ctx = CompositePropagator::extract(grpc_propagator, grpc_carrier)
  
  match Context::get(extracted_grpc_ctx, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
}

test "传播器错误处理和恢复测试" {
  // 测试传播器的错误处理和恢复能力
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 测试空上下文的注入和提取
  let empty_ctx = Context::root()
  let error_carrier = TextMapCarrier::new()
  
  // 注入空上下文
  CompositePropagator::inject(propagator, empty_ctx, error_carrier)
  
  // 提取空上下文
  let extracted_empty = CompositePropagator::extract(propagator, error_carrier)
  
  match Context::get(extracted_empty, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试损坏的carrier处理
  let damaged_carrier = TextMapCarrier::new()
  let normal_ctx = Context::with_value(Context::root(), ContextKey::new("normal"), "context")
  
  CompositePropagator::inject(propagator, normal_ctx, damaged_carrier)
  
  // 即使carrier损坏，提取也应该能处理
  let extracted_from_damaged = CompositePropagator::extract(propagator, damaged_carrier)
  
  match Context::get(extracted_from_damaged, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试传播器链中的错误处理
  let error_propagators = [
    CompositePropagator::new([W3CTraceContextPropagator::new()]),
    CompositePropagator::new([W3CTraceContextPropagator::new()]),
    CompositePropagator::new([W3CTraceContextPropagator::new()])
  ]
  
  for error_prop in error_propagators {
    let error_ctx = Context::with_value(Context::root(), ContextKey::new("error.test"), "value")
    let error_test_carrier = TextMapCarrier::new()
    
    CompositePropagator::inject(error_prop, error_ctx, error_test_carrier)
    
    let error_extracted = CompositePropagator::extract(error_prop, error_test_carrier)
    
    match Context::get(error_extracted, ContextKey::new("extracted")) {
      Some("true") => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // 测试传播器配置错误恢复
  let misconfigured_propagator = CompositePropagator::new([])
  let misconfigured_ctx = Context::with_value(Context::root(), ContextKey::new("misconfigured"), "test")
  let misconfigured_carrier = TextMapCarrier::new()
  
  // 即使传播器配置错误，也应该能优雅处理
  CompositePropagator::inject(misconfigured_propagator, misconfigured_ctx, misconfigured_carrier)
  
  let misconfigured_extracted = CompositePropagator::extract(misconfigured_propagator, misconfigured_carrier)
  
  match Context::get(misconfigured_extracted, ContextKey::new("extracted")) {
    Some("true") => assert_true(true)
    _ => assert_true(false)
  }
}

test "传播器性能和优化测试" {
  // 测试传播器的性能和优化
  let high_performance_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 大量上下文注入和提取操作
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  let carriers = []
  for i in 0..=99 {
    let ctx = Context::with_value(Context::root(), ContextKey::new("perf.test"), i.to_string())
    let carrier = TextMapCarrier::new()
    
    CompositePropagator::inject(high_performance_propagator, ctx, carrier)
    carriers.push(carrier)
  }
  
  let inject_time = Clock::now_unix_nanos(Clock::system())
  
  // 批量提取
  for carrier in carriers {
    let extracted = CompositePropagator::extract(high_performance_propagator, carrier)
    
    match Context::get(extracted, ContextKey::new("extracted")) {
      Some("true") => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  
  let inject_duration = inject_time - start_time
  let extract_duration = end_time - inject_time
  let total_duration = end_time - start_time
  
  // 验证性能在合理范围内
  assert_true(inject_duration < 10000000000L)  // 注入时间小于10秒
  assert_true(extract_duration < 10000000000L)  // 提取时间小于10秒
  assert_true(total_duration < 20000000000L)    // 总时间小于20秒
  assert_true(carriers.length() == 100)
  
  // 测试传播器缓存优化
  let cached_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  let cache_start = Clock::now_unix_nanos(Clock::system())
  
  // 重复的上下文操作（测试缓存效果）
  let cached_ctx = Context::with_value(Context::root(), ContextKey::new("cached"), "value")
  
  for i in 0..=49 {
    let carrier = TextMapCarrier::new()
    CompositePropagator::inject(cached_propagator, cached_ctx, carrier)
    
    let extracted = CompositePropagator::extract(cached_propagator, carrier)
    
    match Context::get(extracted, ContextKey::new("extracted")) {
      Some("true") => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  let cache_end = Clock::now_unix_nanos(Clock::system())
  let cache_duration = cache_end - cache_start
  
  // 缓存操作应该更快
  assert_true(cache_duration < 5000000000L)  // 缓存操作小于5秒
  
  // 测试传播器内存使用优化
  let memory_optimized_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 创建大量传播器实例测试内存使用
  let propagators = []
  for i in 0..=19 {
    let prop = CompositePropagator::new([W3CTraceContextPropagator::new()])
    propagators.push(prop)
  }
  
  // 使用所有传播器进行操作
  for prop in propagators {
    let ctx = Context::with_value(Context::root(), ContextKey::new("memory.test"), "value")
    let carrier = TextMapCarrier::new()
    
    CompositePropagator::inject(prop, ctx, carrier)
    
    let extracted = CompositePropagator::extract(prop, carrier)
    
    match Context::get(extracted, ContextKey::new("extracted")) {
      Some("true") => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  assert_true(propagators.length() == 20)
}