// 阿兹米克跨服务传播测试用例
// 专注于分布式系统中遥测数据的跨服务传播和一致性

// 测试1: 基本跨服务跟踪传播
test "基本跨服务跟踪传播测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.test")
  
  // 服务A创建根Span
  let service_a_span = Tracer::start_span(tracer, "service.a.operation")
  let service_a_context = Span::context(service_a_span)
  
  // 设置服务A的属性
  Span::set_attribute(service_a_span, "service.name", StringValue("service-a"))
  Span::set_attribute(service_a_span, "service.version", StringValue("1.0.0"))
  Span::set_attribute(service_a_span, "operation.name", StringValue("process.data"))
  
  // 模拟服务A向服务B传播上下文
  let propagator = TraceContextPropagator::new()
  let headers = []
  let injected_headers = TraceContextPropagator::inject(propagator, service_a_context, headers)
  
  // 验证注入的头部
  let traceparent_found = injected_headers.any(fn(header) {
    let (key, _) = header
    key == "traceparent"
  })
  assert_true(traceparent_found)
  
  // 服务B接收并提取上下文
  let service_b_context = TraceContextPropagator::extract(propagator, injected_headers)
  assert_true(SpanContext::is_valid(service_b_context))
  
  // 验证跟踪ID一致性
  assert_eq(SpanContext::trace_id(service_a_context), SpanContext::trace_id(service_b_context))
  
  // 服务B创建子Span
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.operation", service_b_context)
  let service_b_context_child = Span::context(service_b_span)
  
  // 设置服务B的属性
  Span::set_attribute(service_b_span, "service.name", StringValue("service-b"))
  Span::set_attribute(service_b_span, "service.version", StringValue("1.2.0"))
  Span::set_attribute(service_b_span, "operation.name", StringValue("transform.data"))
  
  // 验证父子关系
  assert_eq(SpanContext::trace_id(service_a_context), SpanContext::trace_id(service_b_context_child))
  assert_neq(SpanContext::span_id(service_a_context), SpanContext::span_id(service_b_context_child))
  
  // 结束Span
  Span::end(service_b_span)
  Span::end(service_a_span)
}

// 测试2: 跨服务行李传播
test "跨服务行李传播测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "baggage.propagation.test")
  
  // 服务A创建根Span和行李
  let service_a_span = Tracer::start_span(tracer, "service.a.with.baggage")
  let service_a_context = Span::context(service_a_span)
  
  // 设置行李项
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-67890")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-abc")
  
  // 将行李添加到上下文
  let context_with_baggage = Context::with_value(service_a_context, ContextKey::new("baggage"), baggage_with_tenant)
  
  // 模拟跨服务传播
  let propagator = TextMapPropagator::new()
  let headers = []
  let injected_headers = TextMapPropagator::inject(propagator, context_with_baggage, headers)
  
  // 验证行李头部
  let baggage_header_found = injected_headers.any(fn(header) {
    let (key, _) = header
    key == "baggage"
  })
  assert_true(baggage_header_found)
  
  // 服务B提取上下文和行李
  let service_b_context = TextMapPropagator::extract(propagator, injected_headers)
  let extracted_baggage = Context::get(service_b_context, ContextKey::new("baggage"))
  
  match extracted_baggage {
    Some(baggage) => {
      // 验证提取的行李项
      let user_id = Baggage::get_entry(baggage, "user.id")
      match user_id {
        Some(id) => assert_eq(id, "user-12345")
        None => assert_true(false)
      }
      
      let session_id = Baggage::get_entry(baggage, "session.id")
      match session_id {
        Some(id) => assert_eq(id, "session-67890")
        None => assert_true(false)
      }
      
      let tenant_id = Baggage::get_entry(baggage, "tenant.id")
      match tenant_id {
        Some(id) => assert_eq(id, "tenant-abc")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 结束Span
  Span::end(service_a_span)
}

// 测试3: 多级服务调用链传播
test "多级服务调用链传播测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multi.level.chain.test")
  
  // 服务A -> 服务B -> 服务C -> 服务D 的调用链
  
  // 服务A：入口点
  let service_a_span = Tracer::start_span(tracer, "service.a.entry")
  let service_a_context = Span::context(service_a_span)
  Span::set_attribute(service_a_span, "service.name", StringValue("service-a"))
  Span::set_attribute(service_a_span, "operation.type", StringValue("entry"))
  
  // 传播到服务B
  let propagator = TraceContextPropagator::new()
  let headers_a_to_b = []
  let headers_b = TraceContextPropagator::inject(propagator, service_a_context, headers_a_to_b)
  
  // 服务B：中间处理
  let service_b_context = TraceContextPropagator::extract(propagator, headers_b)
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.process", service_b_context)
  let service_b_context_child = Span::context(service_b_span)
  Span::set_attribute(service_b_span, "service.name", StringValue("service-b"))
  Span::set_attribute(service_b_span, "operation.type", StringValue("process"))
  
  // 传播到服务C
  let headers_b_to_c = []
  let headers_c = TraceContextPropagator::inject(propagator, service_b_context_child, headers_b_to_c)
  
  // 服务C：业务逻辑
  let service_c_context = TraceContextPropagator::extract(propagator, headers_c)
  let service_c_span = Tracer::start_span_with_context(tracer, "service.c.business", service_c_context)
  let service_c_context_child = Span::context(service_c_span)
  Span::set_attribute(service_c_span, "service.name", StringValue("service-c"))
  Span::set_attribute(service_c_span, "operation.type", StringValue("business"))
  
  // 传播到服务D
  let headers_c_to_d = []
  let headers_d = TraceContextPropagator::inject(propagator, service_c_context_child, headers_c_to_d)
  
  // 服务D：数据存储
  let service_d_context = TraceContextPropagator::extract(propagator, headers_d)
  let service_d_span = Tracer::start_span_with_context(tracer, "service.d.storage", service_d_context)
  Span::set_attribute(service_d_span, "service.name", StringValue("service-d"))
  Span::set_attribute(service_d_span, "operation.type", StringValue("storage"))
  
  // 验证跟踪ID在整个调用链中保持一致
  let service_a_trace_id = SpanContext::trace_id(service_a_context)
  let service_b_trace_id = SpanContext::trace_id(service_b_context)
  let service_c_trace_id = SpanContext::trace_id(service_c_context)
  let service_d_trace_id = SpanContext::trace_id(service_d_context)
  
  assert_eq(service_a_trace_id, service_b_trace_id)
  assert_eq(service_b_trace_id, service_c_trace_id)
  assert_eq(service_c_trace_id, service_d_trace_id)
  
  // 验证每个服务都有不同的Span ID
  let service_a_span_id = SpanContext::span_id(service_a_context)
  let service_b_span_id = SpanContext::span_id(service_b_context_child)
  let service_c_span_id = SpanContext::span_id(service_c_context_child)
  let service_d_span_id = SpanContext::span_id(Span::context(service_d_span))
  
  assert_neq(service_a_span_id, service_b_span_id)
  assert_neq(service_b_span_id, service_c_span_id)
  assert_neq(service_c_span_id, service_d_span_id)
  
  // 结束所有Span（按调用顺序的逆序）
  Span::end(service_d_span)
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
}

// 测试4: 跨服务上下文一致性验证
test "跨服务上下文一致性验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "context.consistency.test")
  
  // 创建复合传播器（同时传播跟踪上下文和行李）
  let composite_propagator = CompositePropagator::new([
    TraceContextPropagator::new(),
    TextMapPropagator::new()
  ])
  
  // 服务A创建丰富上下文
  let service_a_span = Tracer::start_span(tracer, "service.a.rich.context")
  let service_a_context = Span::context(service_a_span)
  
  // 添加跟踪属性
  Span::set_attribute(service_a_span, "user.id", StringValue("user-123"))
  Span::set_attribute(service_a_span, "request.id", StringValue("req-456"))
  Span::set_attribute(service_a_span, "operation.type", StringValue("critical"))
  
  // 添加行李
  let baggage = Baggage::new()
  let baggage_with_auth = Baggage::set_entry(baggage, "auth.token", "token-abc")
  let baggage_with_locale = Baggage::set_entry(baggage_with_auth, "user.locale", "zh-CN")
  let rich_context = Context::with_value(service_a_context, ContextKey::new("baggage"), baggage_with_locale)
  
  // 传播丰富上下文
  let headers = []
  let injected_headers = CompositePropagator::inject(composite_propagator, rich_context, headers)
  
  // 验证注入了多种头部
  let traceparent_found = injected_headers.any(fn(header) {
    let (key, _) = header
    key == "traceparent"
  })
  let baggage_found = injected_headers.any(fn(header) {
    let (key, _) = header
    key == "baggage"
  })
  assert_true(traceparent_found)
  assert_true(baggage_found)
  
  // 服务B提取丰富上下文
  let service_b_context = CompositePropagator::extract(composite_propagator, injected_headers)
  
  // 验证跟踪上下文
  assert_true(SpanContext::is_valid(service_b_context))
  assert_eq(SpanContext::trace_id(service_a_context), SpanContext::trace_id(service_b_context))
  
  // 验证行李
  let extracted_baggage = Context::get(service_b_context, ContextKey::new("baggage"))
  match extracted_baggage {
    Some(baggage) => {
      let auth_token = Baggage::get_entry(baggage, "auth.token")
      match auth_token {
        Some(token) => assert_eq(token, "token-abc")
        None => assert_true(false)
      }
      
      let user_locale = Baggage::get_entry(baggage, "user.locale")
      match user_locale {
        Some(locale) => assert_eq(locale, "zh-CN")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 服务B创建子Span并继承上下文
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.inherit.context", service_b_context)
  
  // 验证子Span可以访问父Span的属性（通过上下文）
  // 注意：实际实现可能需要通过上下文传递属性，而不是直接继承
  
  Span::end(service_b_span)
  Span::end(service_a_span)
}

// 测试5: 跨服务传播的采样决策
test "跨服务传播的采样决策测试" {
  let tracer_provider = TracerProvider::new_with_sampler(Sampler::probability(0.5)) // 50%采样率
  let tracer = TracerProvider::get_tracer(tracer_provider, "sampling.decision.test")
  
  // 测试多个跟踪，验证采样决策的一致性传播
  let sampled_spans = []
  let un_sampled_spans = []
  
  for i in 1..=10 {
    // 服务A创建根Span
    let service_a_span = Tracer::start_span(tracer, "service.a.sampling.test." + i.to_string())
    let service_a_context = Span::context(service_a_span)
    
    // 检查采样决策
    if SpanContext::is_sampled(service_a_context) {
      sampled_spans = sampled_spans.push(service_a_context)
    } else {
      un_sampled_spans = un_sampled_spans.push(service_a_context)
    }
    
    // 传播到服务B
    let propagator = TraceContextPropagator::new()
    let headers = []
    let injected_headers = TraceContextPropagator::inject(propagator, service_a_context, headers)
    
    // 服务B提取上下文
    let service_b_context = TraceContextPropagator::extract(propagator, injected_headers)
    
    // 验证采样决策在传播过程中保持一致
    assert_eq(SpanContext::is_sampled(service_a_context), SpanContext::is_sampled(service_b_context))
    
    // 服务B创建子Span
    let service_b_span = Tracer::start_span_with_context(tracer, "service.b.sampling.test." + i.to_string(), service_b_context)
    let service_b_context_child = Span::context(service_b_span)
    
    // 验证子Span继承父Span的采样决策
    assert_eq(SpanContext::is_sampled(service_b_context), SpanContext::is_sampled(service_b_context_child))
    
    Span::end(service_b_span)
    Span::end(service_a_span)
  }
  
  // 验证采样率大致符合预期（允许一定误差）
  let total_spans = sampled_spans.length() + un_sampled_spans.length()
  let sampled_ratio = sampled_spans.length().to_float() / total_spans.to_float()
  
  // 50%采样率，允许20%的误差范围
  assert_true(sampled_ratio >= 0.3 && sampled_ratio <= 0.7)
  assert_true(total_spans == 10)
}

// 测试6: 跨服务传播的故障恢复
test "跨服务传播的故障恢复测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "propagation.fault.recovery.test")
  
  // 服务A创建根Span
  let service_a_span = Tracer::start_span(tracer, "service.a.fault.test")
  let service_a_context = Span::context(service_a_span)
  
  // 正常传播
  let propagator = TraceContextPropagator::new()
  let normal_headers = []
  let normal_injected = TraceContextPropagator::inject(propagator, service_a_context, normal_headers)
  
  // 服务B正常提取
  let normal_extracted = TraceContextPropagator::extract(propagator, normal_injected)
  assert_true(SpanContext::is_valid(normal_extracted))
  
  // 模拟传播故障：头部损坏
  let corrupted_headers = [("traceparent", "invalid-trace-context")]
  let corrupted_extracted = TraceContextPropagator::extract(propagator, corrupted_headers)
  assert_false(SpanContext::is_valid(corrupted_extracted))
  
  // 模拟传播故障：头部缺失
  let missing_headers = [("other-header", "some-value")]
  let missing_extracted = TraceContextPropagator::extract(propagator, missing_headers)
  assert_false(SpanContext::is_valid(missing_extracted))
  
  // 故障恢复：创建新的根上下文
  let recovery_context = SpanContext::new_random(true)
  assert_true(SpanContext::is_valid(recovery_context))
  
  // 服务B使用恢复上下文继续处理
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.recovery", recovery_context)
  Span::set_attribute(service_b_span, "recovery.mode", StringValue("propagation.failure"))
  
  // 验证恢复上下文的有效性
  let service_b_context = Span::context(service_b_span)
  assert_true(SpanContext::is_valid(service_b_context))
  assert_true(SpanContext::is_sampled(service_b_context))
  
  Span::end(service_b_span)
  Span::end(service_a_span)
}

// 测试7: 跨服务传播的性能优化
test "跨服务传播的性能优化测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "propagation.performance.test")
  
  // 创建大量并发传播测试
  let concurrent_operations = 100
  let start_time = Timestamp::now()
  
  for i in 1..=concurrent_operations {
    // 服务A创建根Span
    let service_a_span = Tracer::start_span(tracer, "service.a.concurrent." + i.to_string())
    let service_a_context = Span::context(service_a_span)
    
    // 传播到服务B
    let propagator = TraceContextPropagator::new()
    let headers = []
    let injected_headers = TraceContextPropagator::inject(propagator, service_a_context, headers)
    
    // 服务B提取上下文
    let service_b_context = TraceContextPropagator::extract(propagator, injected_headers)
    
    // 服务B创建子Span
    let service_b_span = Tracer::start_span_with_context(tracer, "service.b.concurrent." + i.to_string(), service_b_context)
    
    Span::end(service_b_span)
    Span::end(service_a_span)
  }
  
  let end_time = Timestamp::now()
  let total_duration = end_time - start_time
  let avg_duration_per_operation = total_duration.to_float() / concurrent_operations.to_float()
  
  // 验证性能：每个传播操作应该在合理时间内完成
  // 这里设置一个宽松的限制，实际值取决于系统性能
  assert_true(avg_duration_per_operation < 1000000.0) // 1秒以内
  
  // 测试压缩传播
  let compression_propagator = CompressionPropagator::new()
  let large_context_span = Tracer::start_span(tracer, "service.large.context")
  let large_context = Span::context(large_context_span)
  
  // 添加大量属性
  Span::set_attribute(large_context_span, "large.data", StringValue("x".repeat(10000)))
  
  let headers_before = []
  let injected_before = TraceContextPropagator::inject(propagator, large_context, headers_before)
  let compressed_headers = []
  let injected_compressed = CompressionPropagator::inject(compression_propagator, large_context, compressed_headers)
  
  // 验证压缩效果
  let total_size_before = injected_before.reduce(fn(acc, header) {
    let (_, value) = header
    acc + value.length()
  }, 0)
  
  let total_size_compressed = injected_compressed.reduce(fn(acc, header) {
    let (_, value) = header
    acc + value.length()
  }, 0)
  
  assert_true(total_size_compressed < total_size_before)
  
  Span::end(large_context_span)
}