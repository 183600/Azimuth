// Azimuth Telemetry - Cross Signal Correlation Test
// 测试跨遥测信号（Trace、Metrics、Logs）的关联性

test "cross_signal_trace_metrics_correlation" {
  // 测试Trace和Metrics信号的关联性
  let trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 
                  9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  
  // 创建Span上下文
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 创建Span
  let span = trace::Span::{
    name: "test_operation",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: Some(2000000L),
    status: trace::Ok,
    status_description: None,
    attributes: [("operation.type", common::AttributeValue::string("test"))],
    events: [],
    links: []
  }
  
  // 创建关联的Metrics
  let metric_attributes = [
    ("trace.id", common::AttributeValue::array_string(["1234567890123456"])),
    ("span.id", common::AttributeValue::array_string(["12345678"])),
    ("operation.name", common::AttributeValue::string("test_operation"))
  ]
  
  // 验证关联性
  assert_eq(span.name, "test_operation")
  assert_eq(metric_attributes.length(), 3)
  
  // 验证Trace ID在Metrics中的正确表示
  let trace_id_attr = metric_attributes.find(fn((key, _)) { key == "trace.id" })
  assert_some(trace_id_attr)
  let (_, trace_id_value) = trace_id_attr.unwrap()
  match trace_id_value {
    common::ArrayStringValue(values) => assert_eq(values.length(), 1)
    _ => assert_false(true, "Expected ArrayStringValue")
  }
}

test "cross_signal_trace_logs_correlation" {
  // 测试Trace和Logs信号的关联性
  let trace_id = [10_byte, 20_byte, 30_byte, 40_byte, 50_byte, 60_byte, 70_byte, 80_byte,
                  90_byte, 100_byte, 110_byte, 120_byte, 130_byte, 140_byte, 150_byte, 160_byte]
  let span_id = [10_byte, 20_byte, 30_byte, 40_byte, 50_byte, 60_byte, 70_byte, 80_byte]
  
  // 创建Span事件
  let span_event = trace::SpanEvent::{
    name: "log_event",
    timestamp_unix_nanos: 1500000L,
    attributes: [
      ("log.level", common::AttributeValue::string("INFO")),
      ("log.message", common::AttributeValue::string("Operation completed successfully"))
    ]
  }
  
  // 创建包含事件信息的Span
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let span = trace::Span::{
    name: "operation_with_logs",
    context: span_context,
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000000L,
    end_time_unix_nanos: Some(3000000L),
    status: trace::Ok,
    status_description: None,
    attributes: [],
    events: [span_event],
    links: []
  }
  
  // 验证Span事件与日志的关联性
  assert_eq(span.events.length(), 1)
  let event = span.events[0]
  assert_eq(event.name, "log_event")
  assert_eq(event.timestamp_unix_nanos, 1500000L)
  assert_eq(event.attributes.length(), 2)
  
  // 验证日志属性
  let log_level_attr = event.attributes.find(fn((key, _)) { key == "log.level" })
  assert_some(log_level_attr)
  let (_, log_level_value) = log_level_attr.unwrap()
  match log_level_value {
    common::StringValue(level) => assert_eq(level, "INFO")
    _ => assert_false(true, "Expected StringValue")
  }
}

test "cross_signal_metrics_logs_correlation" {
  // 测试Metrics和Logs信号的关联性
  let metric_attributes = [
    ("service.name", common::AttributeValue::string("test_service")),
    ("operation.type", common::AttributeValue::string("batch_processing")),
    ("batch.size", common::AttributeValue::int(100L)),
    ("log.correlation.id", common::AttributeValue::string("log-12345"))
  ]
  
  // 模拟日志记录
  let log_attributes = [
    ("service.name", common::AttributeValue::string("test_service")),
    ("operation.type", common::AttributeValue::string("batch_processing")),
    ("batch.size", common::AttributeValue::int(100L)),
    ("log.correlation.id", common::AttributeValue::string("log-12345")),
    ("log.level", common::AttributeValue::string("WARN")),
    ("log.message", common::AttributeValue::string("Batch processing approaching limits"))
  ]
  
  // 验证Metrics和Logs的关联性
  let common_attrs = metric_attributes.filter(fn((key, _)) {
    log_attributes.any(fn((log_key, _)) { log_key == key })
  })
  
  assert_eq(common_attrs.length(), 4) // service.name, operation.type, batch.size, log.correlation.id
  
  // 验证关联ID的一致性
  let correlation_id_metric = metric_attributes.find(fn((key, _)) { key == "log.correlation.id" })
  let correlation_id_log = log_attributes.find(fn((key, _)) { key == "log.correlation.id" })
  
  assert_some(correlation_id_metric)
  assert_some(correlation_id_log)
  
  let (_, metric_id_value) = correlation_id_metric.unwrap()
  let (_, log_id_value) = correlation_id_log.unwrap()
  
  match (metric_id_value, log_id_value) {
    (common::StringValue(metric_id), common::StringValue(log_id)) => {
      assert_eq(metric_id, log_id)
      assert_eq(metric_id, "log-12345")
    }
    _ => assert_false(true, "Expected both StringValue")
  }
}

test "cross_signal_comprehensive_correlation" {
  // 测试三个信号的综合关联性
  let trace_id = [255_byte, 254_byte, 253_byte, 252_byte, 251_byte, 250_byte, 249_byte, 248_byte,
                  247_byte, 246_byte, 245_byte, 244_byte, 243_byte, 242_byte, 241_byte, 240_byte]
  let span_id = [255_byte, 254_byte, 253_byte, 252_byte, 251_byte, 250_byte, 249_byte, 248_byte]
  
  // 共同的属性键
  let common_attributes = [
    ("service.name", common::AttributeValue::string("comprehensive_test")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("environment", common::AttributeValue::string("testing"))
  ]
  
  // Trace信号
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: "test=true"
  }
  
  let span = trace::Span::{
    name: "comprehensive_operation",
    context: span_context,
    kind: trace::Client,
    parent_span_id: None,
    start_time_unix_nanos: 5000000L,
    end_time_unix_nanos: Some(8000000L),
    status: trace::Ok,
    status_description: None,
    attributes: common_attributes,
    events: [
      trace::SpanEvent::{
        name: "operation_start",
        timestamp_unix_nanos: 5000000L,
        attributes: [
          ("event.type", common::AttributeValue::string("start")),
          ("log.level", common::AttributeValue::string("INFO"))
        ]
      },
      trace::SpanEvent::{
        name: "operation_end", 
        timestamp_unix_nanos: 8000000L,
        attributes: [
          ("event.type", common::AttributeValue::string("end")),
          ("log.level", common::AttributeValue::string("INFO")),
          ("duration.ms", common::AttributeValue::int(3L))
        ]
      }
    ],
    links: []
  }
  
  // Metrics信号
  let metric_attributes = common_attributes + [
    ("trace.id", common::AttributeValue::array_string(["fffefdfcfbfaf9f8f7f6f5f4f3f2f1f0"])),
    ("span.id", common::AttributeValue::array_string(["fffefdfcfbfaf9f8"])),
    ("operation.duration", common::AttributeValue::float(3.0))
  ]
  
  // Logs信号（通过Span事件体现）
  let log_events = span.events.map(fn(event) {
    (event.name, event.attributes)
  })
  
  // 验证综合关联性
  assert_eq(span.attributes.length(), 3) // 三个共同属性
  assert_eq(metric_attributes.length(), 6) // 三个共同属性 + 三个关联属性
  assert_eq(log_events.length(), 2) // 两个日志事件
  
  // 验证所有信号都包含共同的服务标识
  let service_name_in_span = span.attributes.any(fn((key, value)) {
    key == "service.name" && match value { common::StringValue(name) => name == "comprehensive_test", _ => false }
  })
  let service_name_in_metrics = metric_attributes.any(fn((key, value)) {
    key == "service.name" && match value { common::StringValue(name) => name == "comprehensive_test", _ => false }
  })
  
  assert_true(service_name_in_span)
  assert_true(service_name_in_metrics)
  
  // 验证时间序列的一致性
  assert_eq(span.start_time_unix_nanos, 5000000L)
  assert_eq(span.end_time_unix_nanos, Some(8000000L))
  assert_eq(span.events[0].timestamp_unix_nanos, 5000000L)
  assert_eq(span.events[1].timestamp_unix_nanos, 8000000L)
}