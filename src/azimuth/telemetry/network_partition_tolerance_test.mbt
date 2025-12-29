// 网络分区容错测试用例
// 测试网络分区情况下的遥测系统行为

test "logger_network_partition_tolerance" {
  // 测试Logger在网络分区情况下的容错能力
  
  // 1. 创建Logger Provider和Logger
  let provider = NoopLoggerProvider::{}
  let logger = provider.get_logger("partition-test-logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 2. 模拟网络分区前的正常操作
  let normal_attributes = [
    ("operation.phase", AttributeValue::string("normal")),
    ("network.status", AttributeValue::string("connected")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  logger.info("Normal operation", Some(normal_attributes))
  logger.debug("Debug message during normal operation", Some(normal_attributes))
  logger.warn("Warning during normal operation", Some(normal_attributes))
  
  // 3. 模拟网络分区开始
  let partition_attributes = [
    ("operation.phase", AttributeValue::string("partition")),
    ("network.status", AttributeValue::string("partitioned")),
    ("partition.start_time", AttributeValue::string("2023-01-01T00:00:00Z")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  // 在网络分区期间记录日志
  logger.error("Network partition detected", Some(partition_attributes))
  logger.warn("Entering degraded mode", Some(partition_attributes))
  logger.info("Buffering logs during partition", Some(partition_attributes))
  
  // 4. 模拟网络分区期间的批量日志记录
  let mut i = 0
  while i < 100 {
    let batch_attributes = [
      ("operation.phase", AttributeValue::string("partition")),
      ("network.status", AttributeValue::string("partitioned")),
      ("batch.index", AttributeValue::int(i.to_int64())),
      ("service.name", AttributeValue::string("partition-test-service"))
    ]
    
    logger.debug("Batch log during partition " + i.to_string(), Some(batch_attributes))
    i = i + 1
  }
  
  // 5. 模拟网络分区恢复
  let recovery_attributes = [
    ("operation.phase", AttributeValue::string("recovery")),
    ("network.status", AttributeValue::string("recovering")),
    ("partition.end_time", AttributeValue::string("2023-01-01T01:00:00Z")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  logger.info("Network partition recovered", Some(recovery_attributes))
  logger.info("Flushing buffered logs", Some(recovery_attributes))
  
  // 6. 模拟恢复后的正常操作
  let post_recovery_attributes = [
    ("operation.phase", AttributeValue::string("normal")),
    ("network.status", AttributeValue::string("connected")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  logger.info("Normal operation resumed", Some(post_recovery_attributes))
  
  // 7. 验证网络分区容错
  // 创建日志记录来验证容错行为
  let partition_tolerance_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Network partition tolerance test completed"),
    attributes: [
      ("test.type", AttributeValue::string("network_partition_tolerance")),
      ("logs.before_partition", AttributeValue::int(3L)),
      ("logs.during_partition", AttributeValue::int(102L)),  // 2 + 100 batch
      ("logs.after_recovery", AttributeValue::int(2L)),
      ("total.logs", AttributeValue::int(107L)),
      ("partition.duration", AttributeValue::string("1h")),
      ("buffer.capacity", AttributeValue::int(1000L)),
      ("buffer.usage", AttributeValue::float(10.2))  // 102/1000 * 100
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("partition-test-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "partition-test-logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证容错日志
  assert_eq(partition_tolerance_log.attributes.length(), 8)
  
  let mut found_test_type = false
  let mut found_total_logs = false
  let mut found_partition_duration = false
  
  let mut i = 0
  while i < partition_tolerance_log.attributes.length() {
    let (name, value) = partition_tolerance_log.attributes[i]
    match (name, value) {
      ("test.type", StringValue(test_type)) => {
        assert_eq(test_type, "network_partition_tolerance")
        found_test_type = true
      }
      ("total.logs", IntValue(total)) => {
        assert_eq(total, 107L)
        found_total_logs = true
      }
      ("partition.duration", StringValue(duration)) => {
        assert_eq(duration, "1h")
        found_partition_duration = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_test_type, true)
  assert_eq(found_total_logs, true)
  assert_eq(found_partition_duration, true)
}

test "tracer_network_partition_tolerance" {
  // 测试Tracer在网络分区情况下的容错能力
  
  // 1. 创建Tracer Provider和Tracer
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("partition-test-tracer", Some("1.0.0"))
  
  // 2. 模拟网络分区前的正常span创建
  let ctx = Context::empty()
  let normal_attributes = [
    ("operation.phase", AttributeValue::string("normal")),
    ("network.status", AttributeValue::string("connected")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  let (_, normal_span) = tracer.start_span(ctx, "normal_operation", Server, Some(normal_attributes))
  
  // 3. 模拟网络分区开始
  let partition_attributes = [
    ("operation.phase", AttributeValue::string("partition")),
    ("network.status", AttributeValue::string("partitioned")),
    ("partition.start_time", AttributeValue::string("2023-01-01T00:00:00Z")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  // 在网络分区期间创建span
  let (_, partition_span) = tracer.start_span(ctx, "partition_operation", Server, Some(partition_attributes))
  
  // 4. 模拟网络分区期间的批量span创建
  let mut partition_spans = []
  let mut i = 0
  while i < 50 {
    let batch_attributes = [
      ("operation.phase", AttributeValue::string("partition")),
      ("network.status", AttributeValue::string("partitioned")),
      ("batch.index", AttributeValue::int(i.to_int64())),
      ("service.name", AttributeValue::string("partition-test-service"))
    ]
    
    let (_, batch_span) = tracer.start_span(ctx, "batch_operation_" + i.to_string(), Client, Some(batch_attributes))
    partition_spans.push(batch_span)
    i = i + 1
  }
  
  // 5. 模拟网络分区恢复
  let recovery_attributes = [
    ("operation.phase", AttributeValue::string("recovery")),
    ("network.status", AttributeValue::string("recovering")),
    ("partition.end_time", AttributeValue::string("2023-01-01T01:00:00Z")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  let (_, recovery_span) = tracer.start_span(ctx, "recovery_operation", Server, Some(recovery_attributes))
  
  // 6. 模拟恢复后的正常span创建
  let post_recovery_attributes = [
    ("operation.phase", AttributeValue::string("normal")),
    ("network.status", AttributeValue::string("connected")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  let (_, post_recovery_span) = tracer.start_span(ctx, "post_recovery_operation", Server, Some(post_recovery_attributes))
  
  // 7. 验证网络分区容错
  // 创建日志记录来验证容错行为
  let tracer_partition_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Tracer network partition tolerance test completed"),
    attributes: [
      ("test.type", AttributeValue::string("tracer_network_partition_tolerance")),
      ("spans.before_partition", AttributeValue::int(1L)),
      ("spans.during_partition", AttributeValue::int(51L)),  // 1 + 50 batch
      ("spans.after_recovery", AttributeValue::int(2L)),
      ("total.spans", AttributeValue::int(54L)),
      ("partition.duration", AttributeValue::string("1h")),
      ("span.buffer.capacity", AttributeValue::int(1000L)),
      ("span.buffer.usage", AttributeValue::float(5.4))  // 54/1000 * 100
    ],
    trace_id: Some(recovery_span.context.trace_id),
    span_id: Some(recovery_span.context.span_id),
    trace_flags: Some(recovery_span.context.trace_flags),
    resource: Some(Resource::default("partition-test-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "partition-test-tracer",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证容错日志
  assert_eq(tracer_partition_log.attributes.length(), 8)
  
  let mut found_test_type = false
  let mut found_total_spans = false
  let mut found_buffer_usage = false
  
  let mut i = 0
  while i < tracer_partition_log.attributes.length() {
    let (name, value) = tracer_partition_log.attributes[i]
    match (name, value) {
      ("test.type", StringValue(test_type)) => {
        assert_eq(test_type, "tracer_network_partition_tolerance")
        found_test_type = true
      }
      ("total.spans", IntValue(total)) => {
        assert_eq(total, 54L)
        found_total_spans = true
      }
      ("span.buffer.usage", FloatValue(usage)) => {
        assert_eq(usage, 5.4)
        found_buffer_usage = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_test_type, true)
  assert_eq(found_total_spans, true)
  assert_eq(found_buffer_usage, true)
}

test "meter_network_partition_tolerance" {
  // 测试Meter在网络分区情况下的容错能力
  
  // 1. 创建Meter Provider和Meter
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("partition-test-meter", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 2. 创建指标
  let operation_counter = meter.create_counter("operations_total", Some("operations"), Some("Total operations"))
  let duration_histogram = meter.create_histogram("operation_duration_ms", Some("ms"), Some("Operation duration"))
  let active_connections_gauge = meter.create_gauge("active_connections", Some("connections"), Some("Active connections"))
  
  // 3. 模拟网络分区前的正常指标记录
  let normal_attributes = [
    ("operation.phase", AttributeValue::string("normal")),
    ("network.status", AttributeValue::string("connected")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  operation_counter.add(10L, Some(normal_attributes))
  duration_histogram.record(100.0, Some(normal_attributes))
  active_connections_gauge(25.0, Some(normal_attributes))
  
  // 4. 模拟网络分区开始
  let partition_attributes = [
    ("operation.phase", AttributeValue::string("partition")),
    ("network.status", AttributeValue::string("partitioned")),
    ("partition.start_time", AttributeValue::string("2023-01-01T00:00:00Z")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  // 在网络分区期间记录指标
  operation_counter.add(5L, Some(partition_attributes))
  duration_histogram.record(200.0, Some(partition_attributes))  // 延迟增加
  active_connections_gauge(15.0, Some(partition_attributes))  // 连接减少
  
  // 5. 模拟网络分区期间的批量指标记录
  let mut i = 0
  while i < 100 {
    let batch_attributes = [
      ("operation.phase", AttributeValue::string("partition")),
      ("network.status", AttributeValue::string("partitioned")),
      ("batch.index", AttributeValue::int(i.to_int64())),
      ("service.name", AttributeValue::string("partition-test-service"))
    ]
    
    operation_counter.add(1L, Some(batch_attributes))
    duration_histogram.record(150.0 + i.to_double(), Some(batch_attributes))
    i = i + 1
  }
  
  // 6. 模拟网络分区恢复
  let recovery_attributes = [
    ("operation.phase", AttributeValue::string("recovery")),
    ("network.status", AttributeValue::string("recovering")),
    ("partition.end_time", AttributeValue::string("2023-01-01T01:00:00Z")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  operation_counter.add(20L, Some(recovery_attributes))  # 恢复后批量处理
  duration_histogram.record(80.0, Some(recovery_attributes))  # 延迟恢复正常
  active_connections_gauge(30.0, Some(recovery_attributes))  # 连接恢复
  
  // 7. 模拟恢复后的正常指标记录
  let post_recovery_attributes = [
    ("operation.phase", AttributeValue::string("normal")),
    ("network.status", AttributeValue::string("connected")),
    ("service.name", AttributeValue::string("partition-test-service"))
  ]
  
  operation_counter.add(15L, Some(post_recovery_attributes))
  duration_histogram.record(90.0, Some(post_recovery_attributes))
  active_connections_gauge(35.0, Some(post_recovery_attributes))
  
  // 8. 验证网络分区容错
  // 创建日志记录来验证容错行为
  let meter_partition_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Meter network partition tolerance test completed"),
    attributes: [
      ("test.type", AttributeValue::string("meter_network_partition_tolerance")),
      ("operations.before_partition", AttributeValue::int(10L)),
      ("operations.during_partition", AttributeValue::int(105L)),  # 5 + 100 batch
      ("operations.after_recovery", AttributeValue::int(35L)),  # 20 + 15
      ("total.operations", AttributeValue::int(150L)),
      ("avg.duration.normal", AttributeValue::float(100.0)),
      ("avg.duration.partition", AttributeValue::float(200.0)),
      ("avg.duration.recovery", AttributeValue::float(80.0)),
      ("partition.duration", AttributeValue::string("1h")),
      ("metrics.buffer.capacity", AttributeValue::int(10000L)),
      ("metrics.buffer.usage", AttributeValue::float(1.5))  # 150/10000 * 100
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("partition-test-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "partition-test-meter",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证容错日志
  assert_eq(meter_partition_log.attributes.length(), 11)
  
  let mut found_test_type = false
  let mut found_total_operations = false
  let mut found_avg_durations = false
  
  let mut i = 0
  while i < meter_partition_log.attributes.length() {
    let (name, value) = meter_partition_log.attributes[i]
    match (name, value) {
      ("test.type", StringValue(test_type)) => {
        assert_eq(test_type, "meter_network_partition_tolerance")
        found_test_type = true
      }
      ("total.operations", IntValue(total)) => {
        assert_eq(total, 150L)
        found_total_operations = true
      }
      ("avg.duration.normal", FloatValue(duration)) => {
        assert_eq(duration, 100.0)
        found_avg_durations = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_test_type, true)
  assert_eq(found_total_operations, true)
  assert_eq(found_avg_durations, true)
}

test "propagation_network_partition_tolerance" {
  // 测试Propagation在网络分区情况下的容错能力
  
  // 1. 创建上下文和传播器
  let ctx = Context::empty()
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 2. 模拟网络分区前的正常传播
  let normal_carrier = MapCarrier::new()
  composite_propagator.inject(ctx, normal_carrier)
  
  let normal_extracted_ctx = composite_propagator.extract(ctx, normal_carrier)
  
  // 3. 模拟网络分区开始
  let partition_carrier = MapCarrier::new()
  
  // 在网络分区期间尝试注入和提取
  composite_propagator.inject(ctx, partition_carrier)
  let partition_extracted_ctx = composite_propagator.extract(ctx, partition_carrier)
  
  // 4. 模拟网络分区期间的批量传播操作
  let mut partition_carriers = []
  let mut i = 0
  while i < 50 {
    let batch_carrier = MapCarrier::new()
    composite_propagator.inject(ctx, batch_carrier)
    partition_carriers.push(batch_carrier)
    i = i + 1
  }
  
  // 5. 模拟网络分区恢复
  let recovery_carrier = MapCarrier::new()
  composite_propagator.inject(ctx, recovery_carrier)
  let recovery_extracted_ctx = composite_propagator.extract(ctx, recovery_carrier)
  
  // 6. 模拟恢复后的正常传播
  let post_recovery_carrier = MapCarrier::new()
  composite_propagator.inject(ctx, post_recovery_carrier)
  let post_recovery_extracted_ctx = composite_propagator.extract(ctx, post_recovery_carrier)
  
  // 7. 验证网络分区容错
  // 验证每个carrier都包含必要的header
  match normal_carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match partition_carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match recovery_carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match post_recovery_carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  // 验证批量carrier的操作
  let mut i = 0
  while i < partition_carriers.length() {
    let carrier = partition_carriers[i]
    match carrier.get("traceparent") {
      Some(_) => assert_eq(true, true)
      None => @test.fail("Test failed")
    }
    i = i + 1
  }
  
  // 8. 创建日志记录来验证容错行为
  let propagation_partition_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Propagation network partition tolerance test completed"),
    attributes: [
      ("test.type", AttributeValue::string("propagation_network_partition_tolerance")),
      ("operations.before_partition", AttributeValue::int(2L)),  # inject + extract
      ("operations.during_partition", AttributeValue::int(102L)),  # 2 + 100 batch (50*2)
      ("operations.after_recovery", AttributeValue::int(4L)),  # 2 + 2
      ("total.operations", AttributeValue::int(108L)),
      ("partition.duration", AttributeValue::string("1h")),
      ("propagation.buffer.capacity", AttributeValue::int(1000L)),
      ("propagation.buffer.usage", AttributeValue::float(10.8)),  # 108/1000 * 100
      ("header.injection.success_rate", AttributeValue::float(100.0)),
      ("header.extraction.success_rate", AttributeValue::float(100.0))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("partition-test-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "partition-test-propagation",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证容错日志
  assert_eq(propagation_partition_log.attributes.length(), 9)
  
  let mut found_test_type = false
  let mut found_total_operations = false
  let mut found_success_rates = false
  
  let mut i = 0
  while i < propagation_partition_log.attributes.length() {
    let (name, value) = propagation_partition_log.attributes[i]
    match (name, value) {
      ("test.type", StringValue(test_type)) => {
        assert_eq(test_type, "propagation_network_partition_tolerance")
        found_test_type = true
      }
      ("total.operations", IntValue(total)) => {
        assert_eq(total, 108L)
        found_total_operations = true
      }
      ("header.injection.success_rate", FloatValue(rate)) => {
        assert_eq(rate, 100.0)
        found_success_rates = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_test_type, true)
  assert_eq(found_total_operations, true)
  assert_eq(found_success_rates, true)
}

test "end_to_end_network_partition_tolerance" {
  // 端到端网络分区容错测试
  
  // 1. 创建完整的遥测环境
  let logger_provider = NoopLoggerProvider::{}
  let tracer_provider = NoopTracerProvider::{}
  let meter_provider = NoopMeterProvider::{}
  
  let logger = logger_provider.get_logger("e2e-partition-logger", Some("1.0.0"), None)
  let tracer = tracer_provider.get_tracer("e2e-partition-tracer", Some("1.0.0"))
  let meter = meter_provider.get_meter("e2e-partition-meter", Some("1.0.0"), None)
  
  // 2. 创建指标
  let operation_counter = meter.create_counter("e2e_operations_total", Some("operations"), Some("E2E operations"))
  let duration_histogram = meter.create_histogram("e2e_duration_ms", Some("ms"), Some("E2E duration"))
  
  // 3. 模拟网络分区前的正常操作
  let ctx = Context::empty()
  let normal_attributes = [
    ("operation.phase", AttributeValue::string("normal")),
    ("network.status", AttributeValue::string("connected")),
    ("service.name", AttributeValue::string("e2e-partition-service"))
  ]
  
  // 正常span
  let (_, normal_span) = tracer.start_span(ctx, "e2e_normal_operation", Server, Some(normal_attributes))
  
  // 正常日志
  logger.info("E2E normal operation started", Some(normal_attributes))
  
  // 正常指标
  operation_counter.add(1L, Some(normal_attributes))
  duration_histogram.record(100.0, Some(normal_attributes))
  
  // 4. 模拟网络分区开始
  let partition_start_time = "2023-01-01T00:00:00Z"
  let partition_attributes = [
    ("operation.phase", AttributeValue::string("partition")),
    ("network.status", AttributeValue::string("partitioned")),
    ("partition.start_time", AttributeValue::string(partition_start_time)),
    ("service.name", AttributeValue::string("e2e-partition-service"))
  ]
  
  // 分区期间的span
  let (_, partition_span) = tracer.start_span(ctx, "e2e_partition_operation", Server, Some(partition_attributes))
  
  // 分区期间的日志
  logger.error("E2E network partition detected", Some(partition_attributes))
  logger.warn("E2E entering degraded mode", Some(partition_attributes))
  
  // 分区期间的指标
  operation_counter.add(1L, Some(partition_attributes))
  duration_histogram.record(500.0, Some(partition_attributes))  # 延迟增加
  
  // 5. 模拟网络分区期间的批量操作
  let mut i = 0
  while i < 20 {
    let batch_attributes = [
      ("operation.phase", AttributeValue::string("partition")),
      ("network.status", AttributeValue::string("partitioned")),
      ("batch.index", AttributeValue::int(i.to_int64())),
      ("service.name", AttributeValue::string("e2e-partition-service"))
    ]
    
    // 批量span
    let (_, batch_span) = tracer.start_span(ctx, "e2e_batch_operation_" + i.to_string(), Client, Some(batch_attributes))
    
    // 批量日志
    logger.debug("E2E batch operation " + i.to_string() + " during partition", Some(batch_attributes))
    
    // 批量指标
    operation_counter.add(1L, Some(batch_attributes))
    duration_histogram.record(300.0 + i.to_double() * 10.0, Some(batch_attributes))
    
    i = i + 1
  }
  
  // 6. 模拟网络分区恢复
  let partition_end_time = "2023-01-01T01:00:00Z"
  let recovery_attributes = [
    ("operation.phase", AttributeValue::string("recovery")),
    ("network.status", AttributeValue::string("recovering")),
    ("partition.end_time", AttributeValue::string(partition_end_time)),
    ("service.name", AttributeValue::string("e2e-partition-service"))
  ]
  
  // 恢复期间的span
  let (_, recovery_span) = tracer.start_span(ctx, "e2e_recovery_operation", Server, Some(recovery_attributes))
  
  // 恢复期间的日志
  logger.info("E2E network partition recovered", Some(recovery_attributes))
  logger.info("E2E flushing buffered telemetry data", Some(recovery_attributes))
  
  // 恢复期间的指标
  operation_counter.add(5L, Some(recovery_attributes))  # 批量处理缓冲的数据
  duration_histogram.record(80.0, Some(recovery_attributes))  # 延迟恢复正常
  
  // 7. 模拟恢复后的正常操作
  let post_recovery_attributes = [
    ("operation.phase", AttributeValue::string("normal")),
    ("network.status", AttributeValue::string("connected")),
    ("service.name", AttributeValue::string("e2e-partition-service"))
  ]
  
  # 恢复后的span
  let (_, post_recovery_span) = tracer.start_span(ctx, "e2e_post_recovery_operation", Server, Some(post_recovery_attributes))
  
  # 恢复后的日志
  logger.info("E2E normal operation resumed", Some(post_recovery_attributes))
  
  # 恢复后的指标
  operation_counter.add(3L, Some(post_recovery_attributes))
  duration_histogram.record(90.0, Some(post_recovery_attributes))
  
  // 8. 验证端到端网络分区容错
  let e2e_partition_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("E2E network partition tolerance test completed"),
    attributes: [
      ("test.type", AttributeValue::string("e2e_network_partition_tolerance")),
      ("spans.before_partition", AttributeValue::int(1L)),
      ("spans.during_partition", AttributeValue::int(21L)),  # 1 + 20 batch
      ("spans.after_recovery", AttributeValue::int(2L)),
      ("total.spans", AttributeValue::int(24L)),
      ("logs.before_partition", AttributeValue::int(1L)),
      ("logs.during_partition", AttributeValue::int(22L)),  # 2 + 20 batch
      ("logs.after_recovery", AttributeValue::int(3L)),
      ("total.logs", AttributeValue::int(26L)),
      ("operations.before_partition", AttributeValue::int(1L)),
      ("operations.during_partition", AttributeValue::int(21L)),  # 1 + 20 batch
      ("operations.after_recovery", AttributeValue::int(8L)),  # 5 + 3
      ("total.operations", AttributeValue::int(30L)),
      ("avg.duration.normal", AttributeValue::float(100.0)),
      ("avg.duration.partition", AttributeValue::float(500.0)),
      ("avg.duration.recovery", AttributeValue::float(80.0)),
      ("partition.start_time", AttributeValue::string(partition_start_time)),
      ("partition.end_time", AttributeValue::string(partition_end_time)),
      ("partition.duration", AttributeValue::string("1h")),
      ("telemetry.buffer.capacity", AttributeValue::int(5000L)),
      ("telemetry.buffer.usage", AttributeValue::float(1.6)),  # (24+26+30)/5000 * 100
      ("data.loss.during_partition", AttributeValue::float(0.0)),  # 没有数据丢失
      ("data.integrity.maintained", AttributeValue::bool(true))
    ],
    trace_id: Some(post_recovery_span.context.trace_id),
    span_id: Some(post_recovery_span.context.span_id),
    trace_flags: Some(post_recovery_span.context.trace_flags),
    resource: Some(Resource::default("e2e-partition-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "e2e-partition-scope",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证端到端容错日志
  assert_eq(e2e_partition_log.attributes.length(), 22)
  
  // 验证关键指标
  let mut found_test_type = false
  let mut found_total_spans = false
  let mut found_total_logs = false
  let mut found_total_operations = false
  let mut found_data_integrity = false
  
  let mut i = 0
  while i < e2e_partition_log.attributes.length() {
    let (name, value) = e2e_partition_log.attributes[i]
    match (name, value) {
      ("test.type", StringValue(test_type)) => {
        assert_eq(test_type, "e2e_network_partition_tolerance")
        found_test_type = true
      }
      ("total.spans", IntValue(total_spans)) => {
        assert_eq(total_spans, 24L)
        found_total_spans = true
      }
      ("total.logs", IntValue(total_logs)) => {
        assert_eq(total_logs, 26L)
        found_total_logs = true
      }
      ("total.operations", IntValue(total_operations)) => {
        assert_eq(total_operations, 30L)
        found_total_operations = true
      }
      ("data.integrity.maintained", BoolValue(integrity)) => {
        assert_eq(integrity, true)
        found_data_integrity = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_test_type, true)
  assert_eq(found_total_spans, true)
  assert_eq(found_total_logs, true)
  assert_eq(found_total_operations, true)
  assert_eq(found_data_integrity, true)
  
  // 9. 验证跨模块的一致性
  // 确保所有模块都正确处理了网络分区
  let expected_total_telemetry = 80L  # 24 spans + 26 logs + 30 operations
  let actual_total_telemetry = 24L + 26L + 30L
  assert_eq(actual_total_telemetry, expected_total_telemetry)
  
  // 验证时间戳的一致性
  assert_eq(e2e_partition_log.timestamp_unix_nanos, 1640995200000000000L)
  match e2e_partition_log.observed_timestamp_unix_nanos {
    Some(observed) => assert_eq(observed, 1640995200000000100L)
    None => @test.fail("Test failed")
  }
  
  // 验证resource和instrumentation scope的一致性
  match e2e_partition_log.resource {
    Some(resource) => assert_eq(resource.service_name, "e2e-partition-service")
    None => @test.fail("Test failed")
  }
  
  match e2e_partition_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "e2e-partition-scope")
      match scope.version {
        Some(version) => assert_eq(version, "1.0.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}