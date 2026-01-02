// Azimuth Distributed Tracing Context Tests
// 测试分布式追踪上下文传播功能

test "trace context creation and serialization" {
  // 创建追踪上下文
  let trace_id = @azimuth.TraceId::generate()
  let span_id = @azimuth.SpanId::generate()
  let trace_flags = @azimuth.TraceFlags::SAMPLED
  
  let context = @azimuth.TraceContext::new(trace_id, span_id, trace_flags)
  
  // 验证上下文属性
  assert_eq(context.trace_id, trace_id)
  assert_eq(context.span_id, span_id)
  assert_eq(context.trace_flags, trace_flags)
  
  // 序列化上下文
  let serialized = context.to_string()
  assert_true(serialized.contains(trace_id.to_string()))
  assert_true(serialized.contains(span_id.to_string()))
}

test "trace context injection into headers" {
  // 创建追踪上下文
  let trace_id = @azimuth.TraceId::from_string("4bf92f3577b34da6a3ce929d0e0e4736")
  let span_id = @azimuth.SpanId::from_string("00f067aa0ba902b7")
  let context = @azimuth.TraceContext::new(trace_id, span_id, @azimuth.TraceFlags::SAMPLED)
  
  // 创建注入器
  let injector = @azimuth.TextMapInjector::new()
  let headers = @azimuth.Headers::new()
  
  // 注入上下文到头部
  injector.inject(context, headers)
  
  // 验证头部包含追踪信息
  let traceparent = headers.get("traceparent")
  assert_true(traceparent.is_some())
  
  match traceparent {
    Some(value) => {
      assert_true(value.contains("00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))
    }
    None => assert_true(false)
  }
}

test "trace context extraction from headers" {
  // 创建带有追踪信息的头部
  let headers = @azimuth.Headers::new()
  headers.set("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
  
  // 创建提取器
  let extractor = @azimuth.TextMapExtractor::new()
  
  // 从头部提取上下文
  let extracted_context = extractor.extract(headers)
  
  // 验证提取的上下文
  assert_true(extracted_context.is_some())
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id.to_string(), "4bf92f3577b34da6a3ce929d0e0e4736")
      assert_eq(context.span_id.to_string(), "00f067aa0ba902b7")
      assert_eq(context.trace_flags, @azimuth.TraceFlags::SAMPLED)
    }
    None => assert_true(false)
  }
}

test "trace context propagation across service boundaries" {
  // 服务A创建追踪上下文
  let service_a_trace_id = @azimuth.TraceId::generate()
  let service_a_span_id = @azimuth.SpanId::generate()
  let context = @azimuth.TraceContext::new(
    service_a_trace_id, 
    service_a_span_id, 
    @azimuth.TraceFlags::SAMPLED
  )
  
  // 服务A注入上下文到HTTP头部
  let injector = @azimuth.TextMapInjector::new()
  let request_headers = @azimuth.Headers::new()
  injector.inject(context, request_headers)
  
  // 服务B提取上下文
  let extractor = @azimuth.TextMapExtractor::new()
  let extracted_context = extractor.extract(request_headers)
  
  assert_true(extracted_context.is_some())
  
  // 服务B创建子span
  match extracted_context {
    Some(parent_context) => {
      let service_b_span_id = @azimuth.SpanId::generate()
      let child_context = @azimuth.TraceContext::new_child(
        parent_context.trace_id,
        service_b_span_id,
        parent_context.trace_flags
      )
      
      // 验证子上下文继承父上下文的trace_id
      assert_eq(child_context.trace_id, parent_context.trace_id)
      assert_ne(child_context.span_id, parent_context.span_id)
      assert_eq(child_context.trace_flags, parent_context.trace_flags)
    }
    None => assert_true(false)
  }
}

test "baggage items propagation with trace context" {
  // 创建追踪上下文
  let trace_id = @azimuth.TraceId::generate()
  let span_id = @azimuth.SpanId::generate()
  let context = @azimuth.TraceContext::new(trace_id, span_id, @azimuth.TraceFlags::SAMPLED)
  
  // 添加baggage项
  let baggage = @azimuth.Baggage::new()
  baggage.set("user.id", "12345")
  baggage.set("request.source", "mobile")
  
  // 创建带baggage的上下文
  let context_with_baggage = @azimuth.TraceContext::with_baggage(context, baggage)
  
  // 序列化和反序列化
  let serialized = context_with_baggage.to_string()
  let deserialized = @azimuth.TraceContext::from_string(serialized)
  
  assert_true(deserialized.is_some())
  match deserialized {
    Some(parsed_context) => {
      assert_eq(parsed_context.trace_id, trace_id)
      assert_eq(parsed_context.span_id, span_id)
      
      // 验证baggage项
      let parsed_baggage = parsed_context.baggage
      assert_true(parsed_baggage.is_some())
      match parsed_baggage {
        Some(b) => {
          assert_eq(b.get("user.id"), Some("12345"))
          assert_eq(b.get("request.source"), Some("mobile"))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

test "trace context with sampling decisions" {
  // 创建未采样的追踪上下文
  let trace_id = @azimuth.TraceId::generate()
  let span_id = @azimuth.SpanId::generate()
  let unsampled_context = @azimuth.TraceContext::new(
    trace_id, 
    span_id, 
    @azimuth.TraceFlags::NOT_SAMPLED
  )
  
  // 验证采样决策
  assert_false(unsampled_context.is_sampled())
  
  // 创建采样的追踪上下文
  let sampled_context = @azimuth.TraceContext::new(
    trace_id, 
    span_id, 
    @azimuth.TraceFlags::SAMPLED
  )
  
  // 验证采样决策
  assert_true(sampled_context.is_sampled())
  
  // 测试采样决策的传播
  let injector = @azimuth.TextMapInjector::new()
  let headers = @azimuth.Headers::new()
  injector.inject(sampled_context, headers)
  
  let extractor = @azimuth.TextMapExtractor::new()
  let extracted = extractor.extract(headers)
  
  assert_true(extracted.is_some())
  match extracted {
    Some(context) => {
      assert_true(context.is_sampled())
    }
    None => assert_true(false)
  }
}