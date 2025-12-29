// 跨模块数据一致性测试
test "trace_context_consistency" {
  // 测试Trace和Context模块之间的数据一致性
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 在Context中设置追踪相关信息
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let operation_key = create_key("operation.name")
  
  let enriched_ctx = ctx
    .with_value(trace_key, "trace-12345678901234567890123456789012")
    .with_value(span_key, "span-1234567890123456")
    .with_value(user_key, "user-123456789")
    .with_value(session_key, "session-abcdef123456")
    .with_value(operation_key, "user.profile.operation")
  
  // 使用enriched Context创建Span
  let (span_ctx, span) = tracer.start_span(
    enriched_ctx,
    "user-profile-span",
    Server,
    Some([
      ("service.name", AttributeValue::string("user-service")),
      ("operation.name", AttributeValue::string("getUserProfile")),
      ("user.id", AttributeValue::string("user-123456789")),
      ("session.id", AttributeValue::string("session-abcdef123456")),
      ("trace.id", AttributeValue::string("trace-12345678901234567890123456789012")),
      ("span.id", AttributeValue::string("span-1234567890123456"))
    ]),
    Some(1234567890L)
  )
  
  // 验证Context中的值在Span创建后仍然保持一致
  match span_ctx.get(trace_key) {
    Some(value) => assert_eq(value, "trace-12345678901234567890123456789012")
    None => @test.fail("Expected trace.id in span context")
  }
  
  match span_ctx.get(span_key) {
    Some(value) => assert_eq(value, "span-1234567890123456")
    None => @test.fail("Expected span.id in span context")
  }
  
  match span_ctx.get(user_key) {
    Some(value) => assert_eq(value, "user-123456789")
    None => @test.fail("Expected user.id in span context")
  }
  
  match span_ctx.get(session_key) {
    Some(value) => assert_eq(value, "session-abcdef123456")
    None => @test.fail("Expected session.id in span context")
  }
  
  match span_ctx.get(operation_key) {
    Some(value) => assert_eq(value, "user.profile.operation")
    None => @test.fail("Expected operation.name in span context")
  }
  
  // 验证Span属性与Context值的一致性
  let mut found_trace_id = false
  let mut found_span_id = false
  let mut found_user_id = false
  let mut found_session_id = false
  
  let mut i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    
    match key {
      "trace.id" => {
        match value {
          StringValue(trace_id) => {
            assert_eq(trace_id, "trace-12345678901234567890123456789012")
            found_trace_id = true
          }
          _ => @test.fail("Expected StringValue for trace.id")
        }
      }
      "span.id" => {
        match value {
          StringValue(span_id) => {
            assert_eq(span_id, "span-1234567890123456")
            found_span_id = true
          }
          _ => @test.fail("Expected StringValue for span.id")
        }
      }
      "user.id" => {
        match value {
          StringValue(user_id) => {
            assert_eq(user_id, "user-123456789")
            found_user_id = true
          }
          _ => @test.fail("Expected StringValue for user.id")
        }
      }
      "session.id" => {
        match value {
          StringValue(session_id) => {
            assert_eq(session_id, "session-abcdef123456")
            found_session_id = true
          }
          _ => @test.fail("Expected StringValue for session.id")
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_trace_id, true)
  assert_eq(found_span_id, true)
  assert_eq(found_user_id, true)
  assert_eq(found_session_id, true)
}

test "context_baggage_consistency" {
  // 测试Context和Baggage模块之间的数据一致性
  
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 在Context中设置追踪信息
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  let correlation_key = create_key("correlation.id")
  
  let enriched_ctx = ctx
    .with_value(trace_key, "trace-12345678901234567890123456789012")
    .with_value(span_key, "span-1234567890123456")
    .with_value(correlation_key, "corr-abcdef1234567890")
  
  // 在Baggage中设置相关元数据
  let enriched_baggage = baggage
    .with_entry("user.id", "user-123456789")
    .with_entry("session.id", "session-abcdef123456")
    .with_entry("request.id", "req-1234567890abcdef")
    .with_entry("correlation.id", "corr-abcdef1234567890")  // 与Context中的correlation.id一致
  
  // 验证Context和Baggage中correlation.id的一致性
  match enriched_ctx.get(correlation_key) {
    Some(ctx_correlation_id) => {
      match enriched_baggage.get("correlation.id") {
        Some(baggage_correlation_id) => {
          assert_eq(ctx_correlation_id, baggage_correlation_id)
          assert_eq(ctx_correlation_id, "corr-abcdef1234567890")
        }
        None => @test.fail("Expected correlation.id in baggage")
      }
    }
    None => @test.fail("Expected correlation.id in context")
  }
  
  // 验证Context和Baggage的数据独立性
  let updated_ctx = enriched_ctx.with_value(create_key("context.only"), "context.only.value")
  let updated_baggage = enriched_baggage.with_entry("baggage.only", "baggage.only.value")
  
  // Context的更新不应该影响Baggage
  match updated_baggage.get("context.only") {
    Some(_) => @test.fail("Context update should not affect baggage")
    None => assert_eq(true, true)
  }
  
  // Baggage的更新不应该影响Context
  match updated_ctx.get(create_key("baggage.only")) {
    Some(_) => @test.fail("Baggage update should not affect context")
    None => assert_eq(true, true)
  }
  
  // 验证原始数据仍然保持一致
  match updated_ctx.get(correlation_key) {
    Some(value) => assert_eq(value, "corr-abcdef1234567890")
    None => @test.fail("Expected correlation.id in updated context")
  }
  
  match updated_baggage.get("correlation.id") {
    Some(value) => assert_eq(value, "corr-abcdef1234567890")
    None => @test.fail("Expected correlation.id in updated baggage")
  }
}

test "logs_trace_consistency" {
  // 测试Logs和Trace模块之间的数据一致性
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("consistency.logger")
  
  // 创建Span
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "consistency-test-operation",
    Server,
    Some([
      ("service.name", AttributeValue::string("consistency-service")),
      ("operation.name", AttributeValue::string("testOperation")),
      ("user.id", AttributeValue::string("user-123456789")),
      ("session.id", AttributeValue::string("session-abcdef123456"))
    ]),
    Some(1234567890L)
  )
  
  // 在Span上下文中创建LogRecord
  let trace_context = SpanContext::{
    trace_id: Array::make(16, 0x12_byte),
    span_id: Array::make(8, 0x34_byte),
    trace_flags: 0x01_byte,
    trace_state: "test-trace-state"
  }
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567891L,
    observed_timestamp_unix_nanos: Some(1234567892L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Consistency test log message"),
    attributes: [
      ("service.name", AttributeValue::string("consistency-service")),  // 与Span一致
      ("operation.name", AttributeValue::string("testOperation")),  // 与Span一致
      ("user.id", AttributeValue::string("user-123456789")),  // 与Span一致
      ("session.id", AttributeValue::string("session-abcdef123456")),  // 与Span一致
      ("span.name", AttributeValue::string(span.name)),  // 引用Span名称
      ("log.type", AttributeValue::string("consistency.test"))
    ],
    trace_id: Some(trace_context.trace_id),
    span_id: Some(trace_context.span_id),
    trace_flags: Some(trace_context.trace_flags),
    resource: Some(Resource::default("consistency-service")),
    instrumentation_scope: None
  }
  
  // 验证LogRecord与Span的一致性
  let mut found_service_name = false
  let mut found_operation_name = false
  let mut found_user_id = false
  let mut found_session_id = false
  let mut found_span_name = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "service.name" => {
        match value {
          StringValue(service_name) => {
            assert_eq(service_name, "consistency-service")
            found_service_name = true
            
            // 验证与Span属性一致
            let mut span_service_found = false
            let mut j = 0
            while j < span.attributes.length() {
              let (span_key, span_value) = span.attributes[j]
              if span_key == "service.name" {
                match span_value {
                  StringValue(span_service_name) => {
                    assert_eq(span_service_name, service_name)
                    span_service_found = true
                  }
                  _ => @test.fail("Expected StringValue for span service.name")
                }
              }
              j = j + 1
            }
            assert_eq(span_service_found, true)
          }
          _ => @test.fail("Expected StringValue for service.name")
        }
      }
      "operation.name" => {
        match value {
          StringValue(operation_name) => {
            assert_eq(operation_name, "testOperation")
            found_operation_name = true
            
            // 验证与Span属性一致
            let mut span_operation_found = false
            let mut j = 0
            while j < span.attributes.length() {
              let (span_key, span_value) = span.attributes[j]
              if span_key == "operation.name" {
                match span_value {
                  StringValue(span_operation_name) => {
                    assert_eq(span_operation_name, operation_name)
                    span_operation_found = true
                  }
                  _ => @test.fail("Expected StringValue for span operation.name")
                }
              }
              j = j + 1
            }
            assert_eq(span_operation_found, true)
          }
          _ => @test.fail("Expected StringValue for operation.name")
        }
      }
      "user.id" => {
        match value {
          StringValue(user_id) => {
            assert_eq(user_id, "user-123456789")
            found_user_id = true
            
            // 验证与Span属性一致
            let mut span_user_found = false
            let mut j = 0
            while j < span.attributes.length() {
              let (span_key, span_value) = span.attributes[j]
              if span_key == "user.id" {
                match span_value {
                  StringValue(span_user_id) => {
                    assert_eq(span_user_id, user_id)
                    span_user_found = true
                  }
                  _ => @test.fail("Expected StringValue for span user.id")
                }
              }
              j = j + 1
            }
            assert_eq(span_user_found, true)
          }
          _ => @test.fail("Expected StringValue for user.id")
        }
      }
      "session.id" => {
        match value {
          StringValue(session_id) => {
            assert_eq(session_id, "session-abcdef123456")
            found_session_id = true
            
            // 验证与Span属性一致
            let mut span_session_found = false
            let mut j = 0
            while j < span.attributes.length() {
              let (span_key, span_value) = span.attributes[j]
              if span_key == "session.id" {
                match span_value {
                  StringValue(span_session_id) => {
                    assert_eq(span_session_id, session_id)
                    span_session_found = true
                  }
                  _ => @test.fail("Expected StringValue for span session.id")
                }
              }
              j = j + 1
            }
            assert_eq(span_session_found, true)
          }
          _ => @test.fail("Expected StringValue for session.id")
        }
      }
      "span.name" => {
        match value {
          StringValue(span_name) => {
            assert_eq(span_name, span.name)
            found_span_name = true
          }
          _ => @test.fail("Expected StringValue for span.name")
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_operation_name, true)
  assert_eq(found_user_id, true)
  assert_eq(found_session_id, true)
  assert_eq(found_span_name, true)
  
  // 验证追踪上下文一致性
  match log_record.trace_id {
    Some(log_trace_id) => {
      assert_eq(log_trace_id.length(), trace_context.trace_id.length())
      let mut i = 0
      while i < log_trace_id.length() {
        assert_eq(log_trace_id[i], trace_context.trace_id[i])
        i = i + 1
      }
    }
    None => @test.fail("Expected trace_id in log record")
  }
  
  match log_record.span_id {
    Some(log_span_id) => {
      assert_eq(log_span_id.length(), trace_context.span_id.length())
      let mut i = 0
      while i < log_span_id.length() {
        assert_eq(log_span_id[i], trace_context.span_id[i])
        i = i + 1
      }
    }
    None => @test.fail("Expected span_id in log record")
  }
  
  match log_record.trace_flags {
    Some(log_trace_flags) => assert_eq(log_trace_flags, trace_context.trace_flags)
    None => @test.fail("Expected trace_flags in log record")
  }
}

test "metrics_context_consistency" {
  // 测试Metrics和Context模块之间的数据一致性
  
  let ctx = Context::empty()
  let meter = NoopMeter::{}
  
  // 在Context中设置相关信息
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let request_key = create_key("request.id")
  let operation_key = create_key("operation.name")
  let service_key = create_key("service.name")
  
  let enriched_ctx = ctx
    .with_value(user_key, "user-123456789")
    .with_value(session_key, "session-abcdef123456")
    .with_value(request_key, "req-1234567890abcdef")
    .with_value(operation_key, "processPayment")
    .with_value(service_key, "payment-service")
  
  // 基于Context信息创建度量
  let counter = meter.create_counter("payment.requests", "requests", "Payment request counter")
  let histogram = meter.create_histogram("payment.duration", "seconds", "Payment duration histogram")
  let gauge = meter.create_gauge("payment.queue.size", "items", "Payment queue size gauge")
  
  // 使用Context信息记录度量数据
  match enriched_ctx.get(user_key) {
    Some(user_id) => {
      // Counter与Context一致
      counter.add(1L, Some([
        ("user.id", AttributeValue::string(user_id)),
        ("service.name", AttributeValue::string("payment-service")),  // 与Context一致
        ("operation.name", AttributeValue::string("processPayment")),  // 与Context一致
        ("metric.type", AttributeValue::string("counter"))
      ]))
      
      // Histogram与Context一致
      histogram.record(2.5, Some([
        ("user.id", AttributeValue::string(user_id)),
        ("service.name", AttributeValue::string("payment-service")),  // 与Context一致
        ("operation.name", AttributeValue::string("processPayment")),  // 与Context一致
        ("metric.type", AttributeValue::string("histogram"))
      ]))
      
      // Gauge与Context一致
      gauge.record(100.0, Some([
        ("user.id", AttributeValue::string(user_id)),
        ("service.name", AttributeValue::string("payment-service")),  // 与Context一致
        ("operation.name", AttributeValue::string("processPayment")),  // 与Context一致
        ("metric.type", AttributeValue::string("gauge"))
      ]))
    }
    None => @test.fail("Expected user.id in context")
  }
  
  // 验证Context中的其他值
  match enriched_ctx.get(session_key) {
    Some(session_id) => assert_eq(session_id, "session-abcdef123456")
    None => @test.fail("Expected session.id in context")
  }
  
  match enriched_ctx.get(request_key) {
    Some(request_id) => assert_eq(request_id, "req-1234567890abcdef")
    None => @test.fail("Expected request.id in context")
  }
  
  match enriched_ctx.get(operation_key) {
    Some(operation_name) => assert_eq(operation_name, "processPayment")
    None => @test.fail("Expected operation.name in context")
  }
  
  match enriched_ctx.get(service_key) {
    Some(service_name) => assert_eq(service_name, "payment-service")
    None => @test.fail("Expected service.name in context")
  }
}

test "propagation_context_baggage_consistency" {
  // 测试Propagation、Context和Baggage模块之间的数据一致性
  
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 在Context中设置追踪信息
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  let user_key = create_key("user.id")
  
  let enriched_ctx = ctx
    .with_value(trace_key, "trace-12345678901234567890123456789012")
    .with_value(span_key, "span-1234567890123456")
    .with_value(user_key, "user-123456789")
  
  // 在Baggage中设置相关元数据
  let enriched_baggage = baggage
    .with_entry("user.id", "user-123456789")  // 与Context中的user.id一致
    .with_entry("session.id", "session-abcdef123456")
    .with_entry("request.id", "req-1234567890abcdef")
    .with_entry("service.name", "test-service")
    .with_entry("deployment.env", "test")
  
  // 注入上下文和baggage
  let carrier = MapCarrier::new()
  composite.inject(enriched_ctx, carrier)
  
  // 验证注入的数据
  match carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get("baggage") {
    Some(baggage_value) => {
      // 验证baggage包含预期的键值对
      assert_eq(baggage_value.contains("user.id=user-123456789"), true)
      assert_eq(baggage_value.contains("session.id=session-abcdef123456"), true)
      assert_eq(baggage_value.contains("request.id=req-1234567890abcdef"), true)
      assert_eq(baggage_value.contains("service.name=test-service"), true)
      assert_eq(baggage_value.contains("deployment.env=test"), true)
    }
    None => @test.fail("Expected baggage header")
  }
  
  // 提取上下文
  let extraction_ctx = Context::empty()
  let extracted_ctx = composite.extract(extraction_ctx, carrier)
  
  // 验证提取的上下文可以用于创建新的Span
  let tracer = NoopTracer::{}
  let (_, new_span) = tracer.start_span(
    extracted_ctx,
    "propagated-operation",
    Server,
    Some([
      ("service.name", AttributeValue::string("test-service")),
      ("operation.type", AttributeValue::string("propagated")),
      ("user.id", AttributeValue::string("user-123456789"))  // 与原始Context一致
    ]),
    Some(1234567891L)
  )
  
  // 验证新Span的属性
  assert_eq(new_span.name, "propagated-operation")
  assert_eq(new_span.kind, Server)
  assert_eq(new_span.attributes.length(), 3)
  
  // 验证原始Context中的数据仍然保持一致
  match enriched_ctx.get(user_key) {
    Some(user_id) => {
      assert_eq(user_id, "user-123456789")
      
      // 验证与Baggage中user.id的一致性
      match enriched_baggage.get("user.id") {
        Some(baggage_user_id) => {
          assert_eq(user_id, baggage_user_id)
        }
        None => @test.fail("Expected user.id in baggage")
      }
    }
    None => @test.fail("Expected user.id in enriched context")
  }
}

test "resource_all_modules_consistency" {
  // 测试Resource在所有模块中的一致性
  
  let resource = Resource::default("consistency-test-service")
  
  // 在Trace中使用Resource
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  let (_, span) = tracer.start_span(
    ctx,
    "resource-consistency-span",
    Server,
    Some([
      ("service.name", AttributeValue::string(resource.service_name)),  // 与Resource一致
      ("service.version", AttributeValue::string("1.0.0")),
      ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),  // 与Resource一致
      ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version))  // 与Resource一致
    ]),
    Some(1234567890L)
  )
  
  // 在Logs中使用Resource
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567891L,
    observed_timestamp_unix_nanos: Some(1234567892L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Resource consistency test"),
    attributes: [
      ("service.name", AttributeValue::string(resource.service_name)),  // 与Resource一致
      ("service.version", AttributeValue::string("1.0.0")),
      ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),  // 与Resource一致
      ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version)),  // 与Resource一致
      ("log.type", AttributeValue::string("resource.consistency"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource),  // 直接使用Resource
    instrumentation_scope: None
  }
  
  // 在Metrics中使用Resource
  let meter = NoopMeter::{}
  let counter = meter.create_counter("resource.consistency.counter", "operations", "Resource consistency counter")
  
  counter.add(1L, Some([
    ("service.name", AttributeValue::string(resource.service_name)),  // 与Resource一致
    ("service.version", AttributeValue::string("1.0.0")),
    ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),  // 与Resource一致
    ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version)),  // 与Resource一致
    ("metric.type", AttributeValue::string("counter"))
  ]))
  
  // 验证Resource的一致性
  assert_eq(resource.service_name, "consistency-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 验证Span中的Resource一致性
  let mut span_service_found = false
  let mut span_sdk_name_found = false
  let mut span_sdk_version_found = false
  
  let mut i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    
    match key {
      "service.name" => {
        match value {
          StringValue(service_name) => {
            assert_eq(service_name, resource.service_name)
            span_service_found = true
          }
          _ => @test.fail("Expected StringValue for service.name")
        }
      }
      "telemetry.sdk.name" => {
        match value {
          StringValue(sdk_name) => {
            assert_eq(sdk_name, resource.telemetry_sdk_name)
            span_sdk_name_found = true
          }
          _ => @test.fail("Expected StringValue for telemetry.sdk.name")
        }
      }
      "telemetry.sdk.version" => {
        match value {
          StringValue(sdk_version) => {
            assert_eq(sdk_version, resource.telemetry_sdk_version)
            span_sdk_version_found = true
          }
          _ => @test.fail("Expected StringValue for telemetry.sdk.version")
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(span_service_found, true)
  assert_eq(span_sdk_name_found, true)
  assert_eq(span_sdk_version_found, true)
  
  // 验证LogRecord中的Resource一致性
  match log_record.resource {
    Some(log_resource) => {
      assert_eq(log_resource.service_name, resource.service_name)
      assert_eq(log_resource.telemetry_sdk_name, resource.telemetry_sdk_name)
      assert_eq(log_resource.telemetry_sdk_version, resource.telemetry_sdk_version)
    }
    None => @test.fail("Expected resource in log record")
  }
  
  // 验证LogRecord属性中的Resource一致性
  let mut log_service_found = false
  let mut log_sdk_name_found = false
  let mut log_sdk_version_found = false
  
  i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "service.name" => {
        match value {
          StringValue(service_name) => {
            assert_eq(service_name, resource.service_name)
            log_service_found = true
          }
          _ => @test.fail("Expected StringValue for service.name")
        }
      }
      "telemetry.sdk.name" => {
        match value {
          StringValue(sdk_name) => {
            assert_eq(sdk_name, resource.telemetry_sdk_name)
            log_sdk_name_found = true
          }
          _ => @test.fail("Expected StringValue for telemetry.sdk.name")
        }
      }
      "telemetry.sdk.version" => {
        match value {
          StringValue(sdk_version) => {
            assert_eq(sdk_version, resource.telemetry_sdk_version)
            log_sdk_version_found = true
          }
          _ => @test.fail("Expected StringValue for telemetry.sdk.version")
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(log_service_found, true)
  assert_eq(log_sdk_name_found, true)
  assert_eq(log_sdk_version_found, true)
}

test "cross_module_data_integrity" {
  // 测试跨模块数据完整性
  
  let resource = Resource::default("integrity-test-service")
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  let tracer = NoopTracer::{}
  let meter = NoopMeter::{}
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integrity.logger")
  
  // 创建完整的数据链
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let request_key = create_key("request.id")
  let correlation_key = create_key("correlation.id")
  
  // 1. 在Context中设置基础数据
  let base_ctx = ctx
    .with_value(trace_key, "trace-12345678901234567890123456789012")
    .with_value(span_key, "span-1234567890123456")
    .with_value(user_key, "user-123456789")
    .with_value(session_key, "session-abcdef123456")
    .with_value(request_key, "req-1234567890abcdef")
    .with_value(correlation_key, "corr-abcdef1234567890")
  
  // 2. 在Baggage中设置相关数据
  let enriched_baggage = baggage
    .with_entry("user.id", "user-123456789")  // 与Context一致
    .with_entry("session.id", "session-abcdef123456")  // 与Context一致
    .with_entry("request.id", "req-1234567890abcdef")  // 与Context一致
    .with_entry("correlation.id", "corr-abcdef1234567890")  // 与Context一致
    .with_entry("service.name", "integrity-test-service")  // 与Resource一致
    .with_entry("deployment.env", "production")
    .with_entry("operation.type", "integrity.test")
  
  // 3. 创建Span并确保数据一致性
  let (span_ctx, span) = tracer.start_span(
    base_ctx,
    "integrity-test-operation",
    Server,
    Some([
      ("service.name", AttributeValue::string(resource.service_name)),  // 与Resource一致
      ("operation.name", AttributeValue::string("integrityTestOperation")),
      ("user.id", AttributeValue::string("user-123456789")),  // 与Context一致
      ("session.id", AttributeValue::string("session-abcdef123456")),  // 与Context一致
      ("request.id", AttributeValue::string("req-1234567890abcdef")),  // 与Context一致
      ("correlation.id", AttributeValue::string("corr-abcdef1234567890")),  // 与Context一致
      ("trace.id", AttributeValue::string("trace-12345678901234567890123456789012")),  // 与Context一致
      ("span.id", AttributeValue::string("span-1234567890123456")),  // 与Context一致
      ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),  // 与Resource一致
      ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version))  // 与Resource一致
    ]),
    Some(1234567890L)
  )
  
  // 4. 创建LogRecord并确保数据一致性
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567891L,
    observed_timestamp_unix_nanos: Some(1234567892L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Integrity test operation completed"),
    attributes: [
      ("service.name", AttributeValue::string(resource.service_name)),  // 与Resource一致
      ("operation.name", AttributeValue::string("integrityTestOperation")),  // 与Span一致
      ("user.id", AttributeValue::string("user-123456789")),  // 与Context/Baggage一致
      ("session.id", AttributeValue::string("session-abcdef123456")),  // 与Context/Baggage一致
      ("request.id", AttributeValue::string("req-1234567890abcdef")),  // 与Context/Baggage一致
      ("correlation.id", AttributeValue::string("corr-abcdef1234567890")),  // 与Context/Baggage一致
      ("trace.id", AttributeValue::string("trace-12345678901234567890123456789012")),  // 与Context一致
      ("span.id", AttributeValue::string("span-1234567890123456")),  // 与Context一致
      ("span.name", AttributeValue::string(span.name)),  // 引用Span
      ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),  // 与Resource一致
      ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version)),  // 与Resource一致
      ("log.type", AttributeValue::string("integrity.test"))
    ],
    trace_id: Some(Array::make(16, 0x12_byte)),
    span_id: Some(Array::make(8, 0x34_byte)),
    trace_flags: Some(0x01_byte),
    resource: Some(resource),
    instrumentation_scope: None
  }
  
  // 5. 创建Metrics并确保数据一致性
  let counter = meter.create_counter("integrity.operations", "operations", "Integrity operation counter")
  let histogram = meter.create_histogram("integrity.duration", "seconds", "Integrity duration histogram")
  
  counter.add(1L, Some([
    ("service.name", AttributeValue::string(resource.service_name)),  // 与Resource一致
    ("operation.name", AttributeValue::string("integrityTestOperation")),  // 与Span一致
    ("user.id", AttributeValue::string("user-123456789")),  // 与Context/Baggage一致
    ("session.id", AttributeValue::string("session-abcdef123456")),  // 与Context/Baggage一致
    ("request.id", AttributeValue::string("req-1234567890abcdef")),  // 与Context/Baggage一致
    ("correlation.id", AttributeValue::string("corr-abcdef1234567890")),  // 与Context/Baggage一致
    ("trace.id", AttributeValue::string("trace-12345678901234567890123456789012")),  // 与Context一致
    ("span.id", AttributeValue::string("span-1234567890123456")),  // 与Context一致
    ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),  // 与Resource一致
    ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version)),  // 与Resource一致
    ("metric.type", AttributeValue::string("counter"))
  ]))
  
  histogram.record(2.5, Some([
    ("service.name", AttributeValue::string(resource.service_name)),  // 与Resource一致
    ("operation.name", AttributeValue::string("integrityTestOperation")),  // 与Span一致
    ("user.id", AttributeValue::string("user-123456789")),  // 与Context/Baggage一致
    ("session.id", AttributeValue::string("session-abcdef123456")),  // 与Context/Baggage一致
    ("request.id", AttributeValue::string("req-1234567890abcdef")),  // 与Context/Baggage一致
    ("correlation.id", AttributeValue::string("corr-abcdef1234567890")),  // 与Context/Baggage一致
    ("trace.id", AttributeValue::string("trace-12345678901234567890123456789012")),  // 与Context一致
    ("span.id", AttributeValue::string("span-1234567890123456")),  // 与Context一致
    ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),  // 与Resource一致
    ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version)),  // 与Resource一致
    ("metric.type", AttributeValue::string("histogram"))
  ]))
  
  // 6. 验证跨模块数据完整性
  
  // 验证所有模块中的service.name一致
  assert_eq(resource.service_name, "integrity-test-service")
  
  // 验证所有模块中的user.id一致
  match base_ctx.get(user_key) {
    Some(ctx_user_id) => {
      match enriched_baggage.get("user.id") {
        Some(baggage_user_id) => {
          assert_eq(ctx_user_id, baggage_user_id)
          assert_eq(ctx_user_id, "user-123456789")
          
          // 验证Span中的user.id
          let mut span_user_found = false
          let mut i = 0
          while i < span.attributes.length() {
            let (key, value) = span.attributes[i]
            if key == "user.id" {
              match value {
                StringValue(span_user_id) => {
                  assert_eq(span_user_id, ctx_user_id)
                  span_user_found = true
                }
                _ => @test.fail("Expected StringValue for user.id")
              }
            }
            i = i + 1
          }
          assert_eq(span_user_found, true)
          
          // 验证LogRecord中的user.id
          let mut log_user_found = false
          i = 0
          while i < log_record.attributes.length() {
            let (key, value) = log_record.attributes[i]
            if key == "user.id" {
              match value {
                StringValue(log_user_id) => {
                  assert_eq(log_user_id, ctx_user_id)
                  log_user_found = true
                }
                _ => @test.fail("Expected StringValue for user.id")
              }
            }
            i = i + 1
          }
          assert_eq(log_user_found, true)
        }
        None => @test.fail("Expected user.id in baggage")
      }
    }
    None => @test.fail("Expected user.id in context")
  }
  
  // 验证所有模块中的trace.id一致
  match base_ctx.get(trace_key) {
    Some(ctx_trace_id) => {
      // 验证Span中的trace.id
      let mut span_trace_found = false
      let mut i = 0
      while i < span.attributes.length() {
        let (key, value) = span.attributes[i]
        if key == "trace.id" {
          match value {
            StringValue(span_trace_id) => {
              assert_eq(span_trace_id, ctx_trace_id)
              span_trace_found = true
            }
            _ => @test.fail("Expected StringValue for trace.id")
          }
        }
        i = i + 1
      }
      assert_eq(span_trace_found, true)
      
      // 验证LogRecord中的trace.id
      let mut log_trace_found = false
      i = 0
      while i < log_record.attributes.length() {
        let (key, value) = log_record.attributes[i]
        if key == "trace.id" {
          match value {
            StringValue(log_trace_id) => {
              assert_eq(log_trace_id, ctx_trace_id)
              log_trace_found = true
            }
            _ => @test.fail("Expected StringValue for trace.id")
          }
        }
        i = i + 1
      }
      assert_eq(log_trace_found, true)
    }
    None => @test.fail("Expected trace.id in context")
  }
  
  // 验证数据完整性测试成功
  assert_eq(true, true)
}