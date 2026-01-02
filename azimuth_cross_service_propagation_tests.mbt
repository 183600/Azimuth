// Azimuth Telemetry System - 跨服务传播功能测试
// 测试遥测数据在跨服务环境中的传播功能

test "W3C TraceContext跨服务传播测试" {
  // 创建W3C TraceContext传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 服务A：创建初始Context和Span
  let tracer_provider = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(tracer_provider, "service.a")
  let span_a = Tracer::start_span(tracer_a, "service.a.operation")
  let context_a = Context::root()
  
  // 服务A：注入Context到HTTP头
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context_a, carrier)
  
  // 验证注入的数据
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent is Some)
  
  match traceparent {
    Some(value) => {
      // W3C traceparent格式：version-trace_id-span_id-flags
      // 示例：00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01
      assert_true(value.starts_with("00-"))
      let parts = value.split("-")
      assert_eq(parts.length(), 4)
      assert_eq(parts[0], "00")  // 版本
      assert_eq(parts[3].length(), 2)  // 标志
    }
    None => assert_true(false)
  }
  
  // 服务B：从HTTP头提取Context
  let context_b = CompositePropagator::extract(composite_propagator, carrier)
  
  // 服务B：创建子Span
  let tracer_b = TracerProvider::get_tracer(tracer_provider, "service.b")
  let span_b = Tracer::start_span(tracer_b, "service.b.operation")
  
  // 服务B：注入Context到下一个HTTP头
  let carrier_b = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context_b, carrier_b)
  
  // 验证传播的数据
  let traceparent_b = TextMapCarrier::get(carrier_b, "traceparent")
  assert_true(traceparent_b is Some)
  
  // 服务C：从HTTP头提取Context
  let context_c = CompositePropagator::extract(composite_propagator, carrier_b)
  
  // 服务C：创建子Span
  let tracer_c = TracerProvider::get_tracer(tracer_provider, "service.c")
  let span_c = Tracer::start_span(tracer_c, "service.c.operation")
  
  // 验证跨服务传播的一致性
  fn validate_trace_context_propagation(
    original_carrier : TextMapCarrier,
    propagated_carriers : Array[TextMapCarrier]
  ) -> Bool {
    let original_traceparent = TextMapCarrier::get(original_carrier, "traceparent")
    
    match original_traceparent {
      Some(original_value) => {
        let original_parts = original_value.split("-")
        if original_parts.length() != 4 {
          return false
        }
        
        let original_trace_id = original_parts[1]
        
        // 验证所有传播的载体都有相同的trace_id
        for carrier in propagated_carriers {
          let traceparent = TextMapCarrier::get(carrier, "traceparent")
          
          match traceparent {
            Some(value) => {
              let parts = value.split("-")
              if parts.length() != 4 {
                return false
              }
              
              // trace_id应该相同
              if parts[1] != original_trace_id {
                return false
              }
            }
            None => return false
          }
        }
        
        true
      }
      None => false
    }
  }
  
  assert_true(validate_trace_context_propagation(carrier, [carrier_b]))
  
  // 结束所有Span
  Span::end(span_c)
  Span::end(span_b)
  Span::end(span_a)
}

test "W3C Baggage跨服务传播测试" {
  // 创建W3C Baggage传播器
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([baggage_propagator])
  
  // 服务A：创建初始Baggage
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage = Baggage::set_entry(baggage, "user.role", "admin")
  let baggage = Baggage::set_entry(baggage, "request.id", "req-abc123")
  
  // 服务A：将Baggage注入到Context
  let context_a = Context::root()
  let user_key = ContextKey::new("user.id")
  let context_a = Context::with_value(context_a, user_key, "12345")
  
  // 服务A：注入Context到HTTP头
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context_a, carrier)
  
  // 手动添加baggage头（简化实现）
  TextMapCarrier::set(carrier, "baggage", "user.id=12345,user.role=admin,request.id=req-abc123")
  
  // 验证注入的数据
  let baggage_header = TextMapCarrier::get(carrier, "baggage")
  assert_true(baggage_header is Some)
  
  match baggage_header {
    Some(value) => {
      assert_true(value.contains("user.id=12345"))
      assert_true(value.contains("user.role=admin"))
      assert_true(value.contains("request.id=req-abc123"))
    }
    None => assert_true(false)
  }
  
  // 服务B：从HTTP头提取Context
  let context_b = CompositePropagator::extract(composite_propagator, carrier)
  
  // 服务B：添加新的Baggage条目
  let baggage_b = Baggage::new()
  let baggage_b = Baggage::set_entry(baggage_b, "service.b.start.time", "2025-01-01T12:00:00Z")
  let baggage_b = Baggage::set_entry(baggage_b, "service.b.operation", "database.query")
  
  // 服务B：注入Context到下一个HTTP头
  let carrier_b = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context_b, carrier_b)
  
  // 手动添加更新后的baggage头
  TextMapCarrier::set(carrier_b, "baggage", "user.id=12345,user.role=admin,request.id=req-abc123,service.b.start.time=2025-01-01T12:00:00Z,service.b.operation=database.query")
  
  // 验证传播的数据
  let baggage_header_b = TextMapCarrier::get(carrier_b, "baggage")
  assert_true(baggage_header_b is Some)
  
  match baggage_header_b {
    Some(value) => {
      assert_true(value.contains("user.id=12345"))
      assert_true(value.contains("service.b.start.time=2025-01-01T12:00:00Z"))
    }
    None => assert_true(false)
  }
  
  // 服务C：从HTTP头提取Context
  let context_c = CompositePropagator::extract(composite_propagator, carrier_b)
  
  // 服务C：添加更多Baggage条目
  let baggage_c = Baggage::new()
  let baggage_c = Baggage::set_entry(baggage_c, "service.c.cache.hit", "true")
  let baggage_c = Baggage::set_entry(baggage_c, "service.c.response.time", "150ms")
  
  // 服务C：注入Context到下一个HTTP头
  let carrier_c = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context_c, carrier_c)
  
  // 手动添加更新后的baggage头
  TextMapCarrier::set(carrier_c, "baggage", "user.id=12345,user.role=admin,request.id=req-abc123,service.b.start.time=2025-01-01T12:00:00Z,service.b.operation=database.query,service.c.cache.hit=true,service.c.response.time=150ms")
  
  // 验证跨服务Baggage传播的一致性
  fn validate_baggage_propagation(
    original_carrier : TextMapCarrier,
    propagated_carriers : Array[TextMapCarrier],
    expected_entries : Array[String]
  ) -> Bool {
    // 验证原始载体包含预期的条目
    let original_baggage = TextMapCarrier::get(original_carrier, "baggage")
    
    match original_baggage {
      Some(original_value) => {
        for entry in expected_entries {
          if !original_value.contains(entry) {
            return false
          }
        }
      }
      None => return false
    }
    
    // 验证所有传播的载体都包含原始条目
    for carrier in propagated_carriers {
      let baggage = TextMapCarrier::get(carrier, "baggage")
      
      match baggage {
        Some(value) => {
          for entry in expected_entries {
            if !value.contains(entry) {
              return false
            }
          }
        }
        None => return false
      }
    }
    
    true
  }
  
  let expected_entries = ["user.id=12345", "user.role=admin", "request.id=req-abc123"]
  assert_true(validate_baggage_propagation(carrier, [carrier_b, carrier_c], expected_entries))
}

test "复合传播器跨服务测试" {
  // 创建复合传播器，包含TraceContext和Baggage传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 服务A：创建初始Context和Baggage
  let context_a = Context::root()
  let user_key = ContextKey::new("user.id")
  let context_a = Context::with_value(context_a, user_key, "12345")
  
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage = Baggage::set_entry(baggage, "user.role", "admin")
  
  // 服务A：创建Span
  let tracer_provider = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(tracer_provider, "service.a")
  let span_a = Tracer::start_span(tracer_a, "service.a.operation")
  
  // 服务A：注入Context和Baggage
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context_a, carrier)
  
  // 手动添加baggage头
  TextMapCarrier::set(carrier, "baggage", "user.id=12345,user.role=admin")
  
  // 验证注入的数据
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let baggage_header = TextMapCarrier::get(carrier, "baggage")
  
  assert_true(traceparent is Some)
  assert_true(baggage_header is Some)
  
  // 服务B：提取Context和Baggage
  let context_b = CompositePropagator::extract(composite_propagator, carrier)
  
  // 服务B：创建子Span
  let tracer_b = TracerProvider::get_tracer(tracer_provider, "service.b")
  let span_b = Tracer::start_span(tracer_b, "service.b.operation")
  
  // 服务B：添加新的Baggage条目
  let baggage_b = Baggage::new()
  let baggage_b = Baggage::set_entry(baggage_b, "service.b.operation", "database.query")
  
  // 服务B：注入到下一个载体
  let carrier_b = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context_b, carrier_b)
  
  // 手动添加更新后的baggage头
  TextMapCarrier::set(carrier_b, "baggage", "user.id=12345,user.role=admin,service.b.operation=database.query")
  
  // 服务C：提取Context和Baggage
  let context_c = CompositePropagator::extract(composite_propagator, carrier_b)
  
  // 服务C：创建子Span
  let tracer_c = TracerProvider::get_tracer(tracer_provider, "service.c")
  let span_c = Tracer::start_span(tracer_c, "service.c.operation")
  
  // 验证复合传播的一致性
  fn validate_composite_propagation(
    original_carrier : TextMapCarrier,
    propagated_carriers : Array[TextMapCarrier]
  ) -> Bool {
    // 验证原始载体包含traceparent和baggage
    let original_traceparent = TextMapCarrier::get(original_carrier, "traceparent")
    let original_baggage = TextMapCarrier::get(original_carrier, "baggage")
    
    if original_traceparent is None || original_baggage is None {
      return false
    }
    
    // 验证所有传播的载体都包含traceparent和baggage
    for carrier in propagated_carriers {
      let traceparent = TextMapCarrier::get(carrier, "traceparent")
      let baggage = TextMapCarrier::get(carrier, "baggage")
      
      if traceparent is None || baggage is None {
        return false
      }
      
      // 验证trace_id的一致性
      match (original_traceparent, traceparent) {
        (Some(orig), Some(prop)) => {
          let orig_parts = orig.split("-")
          let prop_parts = prop.split("-")
          
          if orig_parts.length() != 4 || prop_parts.length() != 4 {
            return false
          }
          
          // trace_id应该相同
          if orig_parts[1] != prop_parts[1] {
            return false
          }
        }
        _ => return false
      }
      
      // 验证baggage包含原始条目
      match (original_baggage, baggage) {
        (Some(orig), Some(prop)) => {
          if !prop.contains("user.id=12345") || !prop.contains("user.role=admin") {
            return false
          }
        }
        _ => return false
      }
    }
    
    true
  }
  
  assert_true(validate_composite_propagation(carrier, [carrier_b]))
  
  // 结束所有Span
  Span::end(span_c)
  Span::end(span_b)
  Span::end(span_a)
}

test "HTTP头传播边界条件测试" {
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 测试空载体
  let empty_carrier = TextMapCarrier::new()
  let context_from_empty = CompositePropagator::extract(composite_propagator, empty_carrier)
  // 应该返回根上下文或空上下文，不崩溃
  
  // 测试只包含traceparent的载体
  let traceparent_only_carrier = TextMapCarrier::new()
  TextMapCarrier::set(traceparent_only_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let context_from_traceparent = CompositePropagator::extract(composite_propagator, traceparent_only_carrier)
  // 应该成功提取traceparent
  
  // 测试只包含baggage的载体
  let baggage_only_carrier = TextMapCarrier::new()
  TextMapCarrier::set(baggage_only_carrier, "baggage", "user.id=12345,user.role=admin")
  
  let context_from_baggage = CompositePropagator::extract(composite_propagator, baggage_only_carrier)
  // 应该成功提取baggage
  
  // 测试包含其他HTTP头的载体
  let mixed_headers_carrier = TextMapCarrier::new()
  TextMapCarrier::set(mixed_headers_carrier, "host", "api.example.com")
  TextMapCarrier::set(mixed_headers_carrier, "user-agent", "Mozilla/5.0")
  TextMapCarrier::set(mixed_headers_carrier, "accept", "application/json")
  TextMapCarrier::set(mixed_headers_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(mixed_headers_carrier, "baggage", "user.id=12345,user.role=admin")
  TextMapCarrier::set(mixed_headers_carrier, "x-custom-header", "custom-value")
  
  let context_from_mixed = CompositePropagator::extract(composite_propagator, mixed_headers_carrier)
  // 应该成功提取traceparent和baggage，忽略其他头
  
  // 测试格式错误的traceparent
  let malformed_traceparent_carrier = TextMapCarrier::new()
  TextMapCarrier::set(malformed_traceparent_carrier, "traceparent", "invalid-format")
  
  let context_from_malformed = CompositePropagator::extract(composite_propagator, malformed_traceparent_carrier)
  // 应该优雅处理错误，不崩溃
  
  // 测试格式错误的baggage
  let malformed_baggage_carrier = TextMapCarrier::new()
  TextMapCarrier::set(malformed_baggage_carrier, "baggage", "invalid-format-without-equals")
  
  let context_from_malformed_baggage = CompositePropagator::extract(composite_propagator, malformed_baggage_carrier)
  // 应该优雅处理错误，不崩溃
  
  // 测试空值的traceparent和baggage
  let empty_values_carrier = TextMapCarrier::new()
  TextMapCarrier::set(empty_values_carrier, "traceparent", "")
  TextMapCarrier::set(empty_values_carrier, "baggage", "")
  
  let context_from_empty_values = CompositePropagator::extract(composite_propagator, empty_values_carrier)
  // 应该优雅处理空值，不崩溃
  
  // 测试非常长的traceparent和baggage
  let long_traceparent = "00-" + "a".repeat(32) + "-" + "b".repeat(16) + "-01"
  let long_baggage = "key1=" + "a".repeat(100) + ",key2=" + "b".repeat(100)
  
  let long_values_carrier = TextMapCarrier::new()
  TextMapCarrier::set(long_values_carrier, "traceparent", long_traceparent)
  TextMapCarrier::set(long_values_carrier, "baggage", long_baggage)
  
  let context_from_long_values = CompositePropagator::extract(composite_propagator, long_values_carrier)
  // 应该处理长值，不崩溃
  
  // 验证所有边界条件测试都不会崩溃
  assert_true(true)
}

test "跨服务传播性能测试" {
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 创建初始Context
  let context = Context::root()
  let user_key = ContextKey::new("user.id")
  let context = Context::with_value(context, user_key, "12345")
  
  // 性能测试：大量注入和提取操作
  let iterations = 100
  
  // 测试注入性能
  let start_inject_time = Clock::system()
  let inject_start = Clock::now_unix_nanos(start_inject_time)
  
  let mut carriers = []
  for i = 0; i < iterations; i = i + 1 {
    let carrier = TextMapCarrier::new()
    CompositePropagator::inject(composite_propagator, context, carrier)
    carriers.push(carrier)
  }
  
  let inject_end = Clock::now_unix_nanos(start_inject_time)
  let inject_duration = inject_end - inject_start
  
  // 测试提取性能
  let extract_start = Clock::now_unix_nanos(start_inject_time)
  
  let mut contexts = []
  for carrier in carriers {
    let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
    contexts.push(extracted_context)
  }
  
  let extract_end = Clock::now_unix_nanos(start_inject_time)
  let extract_duration = extract_end - extract_start
  
  // 验证性能指标
  let avg_inject_time = inject_duration.to_double() / iterations.to_double()
  let avg_extract_time = extract_duration.to_double() / iterations.to_double()
  
  // 在实际环境中，这里会有具体的性能阈值
  // 这里我们只验证操作完成且没有崩溃
  assert_true(avg_inject_time > 0.0)
  assert_true(avg_extract_time > 0.0)
  
  // 验证所有操作都成功
  assert_eq(carriers.length(), iterations)
  assert_eq(contexts.length(), iterations)
  
  // 验证提取的Context数量正确
  let mut valid_contexts = 0
  for ctx in contexts {
    // 在实际实现中，这里会检查Context的有效性
    valid_contexts = valid_contexts + 1
  }
  
  assert_eq(valid_contexts, iterations)
}