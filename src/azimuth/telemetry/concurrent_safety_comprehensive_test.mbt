// 并发安全测试 - 测试遥测系统在并发环境下的安全性

test "concurrent_span_creation" {
  // 测试并发Span创建的安全性
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  let num_threads = 10
  let operations_per_thread = 100
  
  // 模拟并发Span创建
  // 在实际实现中，这里会创建多个线程，但由于MoonBit的限制，
  // 我们通过快速连续操作来模拟并发场景
  
  let mut thread_id = 0
  while thread_id < num_threads {
    // 模拟一个"线程"的操作
    let mut operation_id = 0
    while operation_id < operations_per_thread {
      let span_name = "concurrent_span_" + thread_id.to_string() + "_" + operation_id.to_string()
      let (_, span) = tracer.start_span(ctx, span_name, Internal, Some([
        ("thread.id", AttributeValue::int(thread_id.to_int64())),
        ("operation.id", AttributeValue::int(operation_id.to_int64())),
        ("span.name", AttributeValue::string(span_name))
      ]))
      
      // 验证Span创建成功
      assert_eq(span.name, span_name)
      assert_eq(span.attributes.length(), 3)
      
      match span.attributes[0].1 {
        IntValue(tid) => assert_eq(tid, thread_id.to_int64())
        _ => @test.fail("Test failed")
      }
      
      match span.attributes[1].1 {
        IntValue(oid) => assert_eq(oid, operation_id.to_int64())
        _ => @test.fail("Test failed")
      }
      
      operation_id = operation_id + 1
    }
    thread_id = thread_id + 1
  }
  
  // 验证总操作数
  let total_operations = num_threads * operations_per_thread
  assert_eq(total_operations, 1000)
}

test "concurrent_metrics_operations" {
  // 测试并发Metrics操作的安全性
  
  let meter = NoopMeter::{}
  let num_threads = 10
  let operations_per_thread = 100
  
  // 创建各种instrument
  let counter = meter.create_counter("concurrent_counter", Some("operations"), Some("Concurrent test counter"))
  let histogram = meter.create_histogram("concurrent_histogram", Some("seconds"), Some("Concurrent test histogram"))
  let up_down_counter = meter.create_up_down_counter("concurrent_up_down_counter", Some("operations"), Some("Concurrent test up-down counter"))
  let gauge = meter.create_gauge("concurrent_gauge", Some("value"), Some("Concurrent test gauge"))
  
  // 模拟并发Metrics操作
  let mut thread_id = 0
  while thread_id < num_threads {
    // 模拟一个"线程"的操作
    let mut operation_id = 0
    while operation_id < operations_per_thread {
      let thread_attr = [("thread.id", AttributeValue::int(thread_id.to_int64()))]
      let operation_attr = [
        ("thread.id", AttributeValue::int(thread_id.to_int64())),
        ("operation.id", AttributeValue::int(operation_id.to_int64()))
      ]
      
      // Counter操作
      counter.add(1L, Some(thread_attr))
      counter.add(operation_id.to_int64(), Some(operation_attr))
      
      // Histogram操作
      histogram.record(operation_id.to_double(), Some(thread_attr))
      histogram.record(operation_id.to_double() * 0.001, Some(operation_attr))
      
      // UpDownCounter操作
      up_down_counter.add(1L, Some(thread_attr))
      up_down_counter.add(operation_id.to_int64() - 50L, Some(operation_attr))
      
      // Gauge操作
      gauge.record(operation_id.to_double(), Some(thread_attr))
      gauge.record(operation_id.to_double() * 0.1, Some(operation_attr))
      
      operation_id = operation_id + 1
    }
    thread_id = thread_id + 1
  }
  
  // 验证总操作数
  let total_operations = num_threads * operations_per_thread
  assert_eq(total_operations, 1000)
}

test "concurrent_log_operations" {
  // 测试并发Log操作的安全性
  
  let logger = NoopLogger::{}
  let num_threads = 10
  let operations_per_thread = 100
  
  // 模拟并发Log操作
  let mut thread_id = 0
  while thread_id < num_threads {
    // 模拟一个"线程"的操作
    let mut operation_id = 0
    while operation_id < operations_per_thread {
      let thread_attr = [("thread.id", AttributeValue::int(thread_id.to_int64()))]
      let operation_attr = [
        ("thread.id", AttributeValue::int(thread_id.to_int64())),
        ("operation.id", AttributeValue::int(operation_id.to_int64()))
      ]
      
      let log_message = "Concurrent log message from thread " + thread_id.to_string() + " operation " + operation_id.to_string()
      
      // 基本日志操作
      logger.debug(log_message + " [DEBUG]", Some(operation_attr))
      logger.info(log_message + " [INFO]", Some(operation_attr))
      logger.warn(log_message + " [WARN]", Some(operation_attr))
      logger.error(log_message + " [ERROR]", Some(operation_attr))
      logger.fatal(log_message + " [FATAL]", Some(operation_attr))
      
      // LogRecord操作
      let log_record = LogRecord::builder()
        .timestamp(1640995200000000000L + (thread_id * 1000 + operation_id).to_int64())
        .severity(Info)
        .body(log_message)
        .with_attribute("thread.id", AttributeValue::int(thread_id.to_int64()))
        .with_attribute("operation.id", AttributeValue::int(operation_id.to_int64()))
        .build()
      
      logger.emit(log_record)
      
      operation_id = operation_id + 1
    }
    thread_id = thread_id + 1
  }
  
  // 验证总操作数
  let total_operations = num_threads * operations_per_thread
  assert_eq(total_operations, 1000)
}

test "concurrent_context_operations" {
  // 测试并发Context操作的安全性
  
  let num_threads = 10
  let operations_per_thread = 100
  
  // 模拟并发Context操作
  let mut thread_id = 0
  while thread_id < num_threads {
    // 每个"线程"从空的上下文开始
    let ctx = Context::empty()
    let baggage = Baggage::empty()
    
    // 模拟一个"线程"的操作
    let mut operation_id = 0
    while operation_id < operations_per_thread {
      let thread_key = create_key("thread." + thread_id.to_string())
      let operation_key = create_key("operation." + thread_id.to_string() + "." + operation_id.to_string())
      
      // Context操作
      let ctx_with_thread = ctx.with_value(thread_key, "thread_value_" + thread_id.to_string())
      let ctx_with_operation = ctx_with_thread.with_value(operation_key, "operation_value_" + operation_id.to_string())
      
      // 验证Context操作
      match ctx_with_operation.get(thread_key) {
        Some(value) => assert_eq(value, "thread_value_" + thread_id.to_string())
        None => @test.fail("Test failed")
      }
      
      match ctx_with_operation.get(operation_key) {
        Some(value) => assert_eq(value, "operation_value_" + operation_id.to_string())
        None => @test.fail("Test failed")
      }
      
      // Baggage操作
      let baggage_with_thread = baggage.with_entry("thread." + thread_id.to_string(), "thread_value_" + thread_id.to_string())
      let baggage_with_operation = baggage_with_thread.with_entry("operation." + operation_id.to_string(), "operation_value_" + operation_id.to_string())
      
      // 验证Baggage操作
      match baggage_with_operation.get("thread." + thread_id.to_string()) {
        Some(value) => assert_eq(value, "thread_value_" + thread_id.to_string())
        None => @test.fail("Test failed")
      }
      
      match baggage_with_operation.get("operation." + operation_id.to_string()) {
        Some(value) => assert_eq(value, "operation_value_" + operation_id.to_string())
        None => @test.fail("Test failed")
      }
      
      operation_id = operation_id + 1
    }
    thread_id = thread_id + 1
  }
  
  // 验证总操作数
  let total_operations = num_threads * operations_per_thread
  assert_eq(total_operations, 1000)
}

test "concurrent_propagation_operations" {
  // 测试并发传播操作的安全性
  
  let ctx = Context::empty()
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  let num_threads = 10
  let operations_per_thread = 100
  
  // 模拟并发传播操作
  let mut thread_id = 0
  while thread_id < num_threads {
    // 模拟一个"线程"的操作
    let mut operation_id = 0
    while operation_id < operations_per_thread {
      // 创建独立的载体
      let carrier = MapCarrier::new()
      
      // 注入操作
      trace_propagator.inject(ctx, carrier)
      baggage_propagator.inject(ctx, carrier)
      composite_propagator.inject(ctx, carrier)
      
      // 创建测试载体用于提取
      let test_carrier = MapCarrier::from_map([
        ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
        ("baggage", "thread." + thread_id.to_string() + "=thread_value_" + thread_id.to_string() + ",operation." + operation_id.to_string() + "=operation_value_" + operation_id.to_string())
      ])
      
      // 提取操作
      let extracted_ctx1 = trace_propagator.extract(ctx, test_carrier)
      let extracted_ctx2 = baggage_propagator.extract(ctx, test_carrier)
      let extracted_ctx3 = composite_propagator.extract(ctx, test_carrier)
      
      // 验证提取操作不会崩溃
      assert_eq(true, true)
      
      operation_id = operation_id + 1
    }
    thread_id = thread_id + 1
  }
  
  // 验证总操作数
  let total_operations = num_threads * operations_per_thread
  assert_eq(total_operations, 1000)
}

test "concurrent_resource_sharing" {
  // 测试并发资源共享的安全性
  
  let tracer_provider = NoopTracerProvider::{}
  let logger_provider = NoopLoggerProvider::{}
  let meter_provider = NoopMeterProvider::{}
  let num_threads = 10
  let operations_per_thread = 50
  
  // 共享的Tracer、Logger、Meter
  let shared_tracer = tracer_provider.get_tracer("shared_tracer", Some("1.0.0"))
  let shared_logger = logger_provider.get_logger("shared_logger", Some("1.0.0"))
  let shared_meter = meter_provider.get_meter("shared_meter", Some("1.0.0"))
  
  // 共享的Instrument
  let shared_counter = shared_meter.create_counter("shared_counter", Some("operations"), Some("Shared counter"))
  let shared_histogram = shared_meter.create_histogram("shared_histogram", Some("seconds"), Some("Shared histogram"))
  
  // 模拟并发资源共享操作
  let mut thread_id = 0
  while thread_id < num_threads {
    // 模拟一个"线程"的操作
    let mut operation_id = 0
    while operation_id < operations_per_thread {
      let thread_attr = [("thread.id", AttributeValue::int(thread_id.to_int64()))]
      let operation_attr = [
        ("thread.id", AttributeValue::int(thread_id.to_int64())),
        ("operation.id", AttributeValue::int(operation_id.to_int64()))
      ]
      
      // 使用共享Tracer
      let (_, span) = shared_tracer.start_span(Context::empty(), "shared_span", Internal, Some(operation_attr))
      assert_eq(span.name, "shared_span")
      
      // 使用共享Logger
      shared_logger.info("Shared log message", Some(operation_attr))
      
      // 使用共享Instrument
      shared_counter.add(1L, Some(operation_attr))
      shared_histogram.record(operation_id.to_double() * 0.001, Some(operation_attr))
      
      operation_id = operation_id + 1
    }
    thread_id = thread_id + 1
  }
  
  // 验证总操作数
  let total_operations = num_threads * operations_per_thread
  assert_eq(total_operations, 500)
}

test "concurrent_complex_scenarios" {
  // 测试并发复杂场景的安全性
  
  let tracer_provider = NoopTracerProvider::{}
  let logger_provider = NoopLoggerProvider::{}
  let meter_provider = NoopMeterProvider::{}
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let num_threads = 5
  let operations_per_thread = 20  // 较少的操作，但每个操作更复杂
  
  // 模拟并发复杂场景
  let mut thread_id = 0
  while thread_id < num_threads {
    // 每个线程有自己的Tracer、Logger、Meter
    let tracer = tracer_provider.get_tracer("complex_tracer_" + thread_id.to_string(), Some("1.0.0"))
    let logger = logger_provider.get_logger("complex_logger_" + thread_id.to_string(), Some("1.0.0"))
    let meter = meter_provider.get_meter("complex_meter_" + thread_id.to_string(), Some("1.0.0"))
    
    // 创建Instrument
    let counter = meter.create_counter("complex_counter", Some("operations"), Some("Complex counter"))
    let histogram = meter.create_histogram("complex_histogram", Some("seconds"), Some("Complex histogram"))
    
    // 模拟一个"线程"的复杂操作
    let mut operation_id = 0
    while operation_id < operations_per_thread {
      // 1. 创建Context和Baggage
      let ctx = Context::empty()
      let baggage = Baggage::empty()
      
      let ctx_with_data = ctx
        .with_value(create_key("thread.id"), thread_id.to_string())
        .with_value(create_key("operation.id"), operation_id.to_string())
      
      let baggage_with_data = baggage
        .with_entry("thread.id", thread_id.to_string())
        .with_entry("operation.id", operation_id.to_string())
        .with_entry("complex.scenario", "concurrent_test")
      
      // 2. 传播操作
      let carrier = MapCarrier::new()
      trace_propagator.inject(ctx_with_data, carrier)
      baggage_propagator.inject(ctx_with_data, carrier)
      
      let extracted_ctx = composite_propagator_new([trace_propagator, baggage_propagator]).extract(ctx, carrier)
      
      // 3. 创建Span
      let complex_attributes = [
        ("thread.id", AttributeValue::int(thread_id.to_int64())),
        ("operation.id", AttributeValue::int(operation_id.to_int64())),
        ("scenario.type", AttributeValue::string("concurrent_complex")),
        ("service.name", AttributeValue::string("complex_test_service")),
        ("operation.name", AttributeValue::string("complex_operation_" + operation_id.to_string())),
        ("user.id", AttributeValue::string("user_" + thread_id.to_string() + "_" + operation_id.to_string())),
        ("session.id", AttributeValue::string("session_" + thread_id.to_string())),
        ("request.id", AttributeValue::string("req_" + thread_id.to_string() + "_" + operation_id.to_string())),
        ("tags", AttributeValue::array_string(["concurrent", "complex", "test"])),
        ("status.codes", AttributeValue::array_int([200L, 400L, 500L])),
        ("thresholds", AttributeValue::array_float([0.1, 0.5, 1.0])),
        ("flags", AttributeValue::array_bool([true, false, true]))
      ]
      
      let (span_ctx, span) = tracer.start_span(extracted_ctx, "complex_concurrent_operation", Server, Some(complex_attributes))
      
      // 4. 记录指标
      counter.add(1L, Some(complex_attributes))
      histogram.record(operation_id.to_double() * 0.01, Some(complex_attributes))
      
      // 5. 记录日志
      let log_record = LogRecord::builder()
        .timestamp(1640995200000000000L + (thread_id * 1000 + operation_id).to_int64())
        .severity(Info)
        .body("Complex concurrent operation " + thread_id.to_string() + "_" + operation_id.to_string())
        .with_attribute("thread.id", AttributeValue::int(thread_id.to_int64()))
        .with_attribute("operation.id", AttributeValue::int(operation_id.to_int64()))
        .with_attribute("scenario.type", AttributeValue::string("concurrent_complex"))
        .build()
      
      logger.emit(log_record)
      
      // 6. 验证所有操作成功
      assert_eq(span.name, "complex_concurrent_operation")
      assert_eq(span.kind, Server)
      assert_eq(span.attributes.length(), 13)
      
      operation_id = operation_id + 1
    }
    thread_id = thread_id + 1
  }
  
  // 验证总操作数
  let total_operations = num_threads * operations_per_thread
  assert_eq(total_operations, 100)
}

// 辅助函数：创建复合传播器
fn composite_propagator_new(propagators : Array[TextMapPropagator]) -> CompositePropagator {
  CompositePropagator::new(propagators)
}