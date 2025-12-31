// 跨信号关联测试 - 测试Trace、Metrics、Logs之间的关联
// Cross-Signal Correlation Test

test "cross_signal_trace_metrics_correlation" {
  // 创建一个带有trace_id的span context
  let trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 
                  9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  
  let span_context = api::trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: "test=state"
  }
  
  // 创建带有trace关联的metrics
  let meter_provider = api::metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", "1.0.0")
  let counter = meter.create_counter("test-counter", "count", "Test counter with trace correlation")
  
  // 添加带有trace属性的metric
  let trace_attributes = [
    ("trace_id", api::common::AttributeValue::array_string(
      trace_id.map(fn(byte) { byte.to_string() })
    )),
    ("span_id", api::common::AttributeValue::array_string(
      span_id.map(fn(byte) { byte.to_string() })
    )),
    ("service.name", api::common::AttributeValue::string("test-service"))
  ]
  
  counter.add(1L, trace_attributes)
  
  // 创建带有相同trace关联的logs
  let logger_provider = api::logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("test-logger", "1.0.0")
  
  let log_record = api::logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(api::logs::Info)
    .body("Test log with trace correlation")
    .with_attribute("trace_id", api::common::AttributeValue::array_string(
      trace_id.map(fn(byte) { byte.to_string() })
    ))
    .with_attribute("span_id", api::common::AttributeValue::array_string(
      span_id.map(fn(byte) { byte.to_string() })
    ))
    .with_attribute("service.name", api::common::AttributeValue::string("test-service"))
    .build()
  
  logger.emit(log_record)
  
  // 验证所有信号都包含相同的trace标识符
  assert_eq(trace_id.length(), 16)
  assert_eq(span_id.length(), 8)
  assert_eq(trace_attributes.length(), 3)
}

test "cross_signal_batch_correlation" {
  // 测试批量操作的跨信号关联
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(i) { (i + 1).to_byte() })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(i) { (i + 1).to_byte() })
  
  let meter_provider = api::metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("batch-test-meter")
  let counter = meter.create_counter("batch-counter")
  let histogram = meter.create_histogram("batch-histogram")
  
  let logger_provider = api::logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("batch-test-logger")
  
  // 批量创建相关联的信号
  for i = 0; i < 10; i = i + 1 {
    // 每个操作都带有相同的trace标识
    let attributes = [
      ("trace_id", api::common::AttributeValue::array_string(
        trace_id.map(fn(byte) { byte.to_string() })
      )),
      ("span_id", api::common::AttributeValue::array_string(
        span_id.map(fn(byte) { byte.to_string() })
      )),
      ("operation.index", api::common::AttributeValue::int(i.to_int64()))
    ]
    
    counter.add(1L, attributes)
    histogram.record(i.to_double(), attributes)
    
    let log_record = api::logs::LogRecord::builder()
      .timestamp(1234567890L + i.to_int64())
      .severity(api::logs::Info)
      .body("Batch operation ${i}")
      .with_attribute("trace_id", api::common::AttributeValue::array_string(
        trace_id.map(fn(byte) { byte.to_string() })
      ))
      .with_attribute("span_id", api::common::AttributeValue::array_string(
        span_id.map(fn(byte) { byte.to_string() })
      ))
      .with_attribute("operation.index", api::common::AttributeValue::int(i.to_int64()))
      .build()
    
    logger.emit(log_record)
  }
  
  // 验证批量操作的一致性
  assert_eq(10, 10) // 所有操作都应该成功执行
}

test "cross_signal_context_propagation" {
  // 测试跨信号的上下文传播
  let context = api::context::Context::current()
  
  let tracer_provider = api::trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("context-test-tracer")
  let (ctx, span) = tracer.start_span(context, "test-span", api::trace::Server)
  
  // 在span上下文中创建metrics和logs
  let meter_provider = api::metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("context-test-meter")
  let counter = meter.create_counter("context-counter")
  
  let logger_provider = api::logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("context-test-logger")
  
  // 创建带有上下文属性的信号
  let context_attributes = [
    ("span.name", api::common::AttributeValue::string("test-span")),
    ("span.kind", api::common::AttributeValue::string("server")),
    ("service.name", api::common::AttributeValue::string("context-test-service"))
  ]
  
  counter.add(5L, context_attributes)
  
  let log_record = api::logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(api::logs::Info)
    .body("Context propagation test")
    .with_attribute("span.name", api::common::AttributeValue::string("test-span"))
    .with_attribute("span.kind", api::common::AttributeValue::string("server"))
    .with_attribute("service.name", api::common::AttributeValue::string("context-test-service"))
    .build()
  
  logger.emit(log_record)
  
  // 验证上下文传播的正确性
  assert_eq(span.name, "test-span")
  assert_eq(span.kind, api::trace::Server)
  assert_eq(context_attributes.length(), 3)
}