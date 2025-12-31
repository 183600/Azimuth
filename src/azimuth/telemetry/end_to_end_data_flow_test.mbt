// 端到端遥测数据流测试
// 测试从数据产生到导出的完整流程

test "end_to_end_telemetry_data_flow" {
  // 模拟完整的遥测数据流：产生 -> 处理 -> 导出
  
  // 1. 数据产生阶段
  let service_name = "order-service"
  let operation_name = "process_order"
  let user_id = "user-789"
  let order_id = "order-12345"
  
  // 创建Span
  let trace_id_bytes = [10_byte, 15_byte, 20_byte, 25_byte, 30_byte, 35_byte, 40_byte, 45_byte, 
                       50_byte, 55_byte, 60_byte, 65_byte, 70_byte, 75_byte, 80_byte, 85_byte]
  let span_id_bytes = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  
  let span = trace::Span::{
    name: operation_name,
    context: trace::SpanContext::{
      trace_id: trace_id_bytes,
      span_id: span_id_bytes,
      trace_flags: 1_byte,
      trace_state: "sampled=1"
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995203000000000L),
    status: trace::Ok,
    status_description: Some("Order processed successfully"),
    attributes: [
      ("service.name", common::AttributeValue::string(service_name)),
      ("user.id", common::AttributeValue::string(user_id)),
      ("order.id", common::AttributeValue::string(order_id)),
      ("operation.duration_ms", common::AttributeValue::int(300L))
    ],
    events: [
      trace::SpanEvent::{
        name: "validation_started",
        timestamp_unix_nanos: 1640995200050000000L,
        attributes: [("validation.type", common::AttributeValue::string("order_schema"))]
      },
      trace::SpanEvent::{
        name: "database_query",
        timestamp_unix_nanos: 1640995200100000000L,
        attributes: [
          ("db.statement", common::AttributeValue::string("SELECT * FROM orders WHERE id = ?")),
          ("db.duration_ms", common::AttributeValue::int(50L))
        ]
      }
    ],
    links: []
  }
  
  // 2. 创建关联的日志记录
  let log_records = [
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995200050000000L,
      observed_timestamp_unix_nanos: Some(1640995200050000000L),
      severity_number: logs::Info,
      severity_text: Some("INFO"),
      body: Some("Starting order validation"),
      attributes: [
        ("order.id", common::AttributeValue::string(order_id)),
        ("validation.step", common::AttributeValue::string("schema_check"))
      ],
      trace_id: Some(trace_id_bytes),
      span_id: Some(span_id_bytes),
      trace_flags: Some(1_byte),
      resource: Some(common::Resource::default(service_name)),
      instrumentation_scope: Some(common::InstrumentationScope::{
        name: "order-validator",
        version: Some("1.2.0"),
        schema_url: None
      })
    },
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995200150000000L,
      observed_timestamp_unix_nanos: Some(1640995200150000000L),
      severity_number: logs::Info,
      severity_text: Some("INFO"),
      body: Some("Order validation completed"),
      attributes: [
        ("order.id", common::AttributeValue::string(order_id)),
        ("validation.result", common::AttributeValue::string("passed")),
        ("validation.duration_ms", common::AttributeValue::int(10L))
      ],
      trace_id: Some(trace_id_bytes),
      span_id: Some(span_id_bytes),
      trace_flags: Some(1_byte),
      resource: Some(common::Resource::default(service_name)),
      instrumentation_scope: Some(common::InstrumentationScope::{
        name: "order-validator",
        version: Some("1.2.0"),
        schema_url: None
      })
    }
  ]
  
  // 3. 创建指标数据
  let metrics_data = [
    ("orders_processed_total", common::AttributeValue::int(1L), "count"),
    ("order_processing_duration_ms", common::AttributeValue::float(300.0), "milliseconds"),
    ("database_query_duration_ms", common::AttributeValue::float(50.0), "milliseconds"),
    ("validation_success_rate", common::AttributeValue::float(100.0), "percent")
  ]
  
  // 4. 验证数据流的完整性
  assert_eq(span.name, operation_name)
  assert_eq(span.attributes.length(), 4)
  assert_eq(span.events.length(), 2)
  assert_eq(log_records.length(), 2)
  assert_eq(metrics_data.length(), 4)
  
  // 验证时间顺序
  assert_eq(span.start_time_unix_nanos < span.events[0].timestamp_unix_nanos, true)
  assert_eq(span.events[0].timestamp_unix_nanos < span.events[1].timestamp_unix_nanos, true)
  assert_eq(span.events[1].timestamp_unix_nanos < log_records[1].timestamp_unix_nanos, true)
  assert_eq(log_records[1].timestamp_unix_nanos < span.end_time_unix_nanos?, true)
  
  // 验证关联性
  assert_eq(log_records[0].trace_id?, trace_id_bytes)
  assert_eq(log_records[1].trace_id?, trace_id_bytes)
  assert_eq(log_records[0].span_id?, span_id_bytes)
  assert_eq(log_records[1].span_id?, span_id_bytes)
  
  // 验证业务逻辑一致性
  match span.attributes[0].1 {
    common::StringValue(svc_name) => assert_eq(svc_name, service_name)
    _ => @test.fail("Service name should be string")
  }
  
  match span.attributes[2].1 {
    common::StringValue(ord_id) => assert_eq(ord_id, order_id)
    _ => @test.fail("Order ID should be string")
  }
  
  // 验证日志内容
  match log_records[0].body {
    Some(log_body) => assert_eq(log_body.has_prefix("Starting order"), true)
    None => @test.fail("Log should have body")
  }
  
  match log_records[1].body {
    Some(log_body) => assert_eq(log_body.has_prefix("Order validation"), true)
    None => @test.fail("Log should have body")
  }
  
  // 验证指标数据类型
  let mut index = 0
  while index < metrics_data.length() {
    let (_, value, unit) = metrics_data[index]
    match value {
      common::IntValue(_) | common::FloatValue(_) => assert_eq(true, true)
      _ => @test.fail("Metric value should be numeric")
    }
    assert_eq(unit.length() > 0, true)
    index = index + 1
  }
}

test "end_to_end_error_flow_tracking" {
  // 测试错误场景的端到端数据流
  
  let error_operation = "payment_processing"
  let error_message = "Payment gateway timeout"
  let error_code = 5002L
  
  // 创建失败的Span
  let failed_span = trace::Span::{
    name: error_operation,
    context: trace::SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 200_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 100_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: trace::Client,
    parent_span_id: None,
    start_time_unix_nanos: 1640995210000000000L,
    end_time_unix_nanos: Some(1640995215000000000L),
    status: trace::Error,
    status_description: Some(error_message),
    attributes: [
      ("error.code", common::AttributeValue::int(error_code)),
      ("payment.gateway", common::AttributeValue::string("stripe")),
      ("retry.count", common::AttributeValue::int(3L))
    ],
    events: [
      trace::SpanEvent::{
        name: "payment_attempt",
        timestamp_unix_nanos: 1640995210100000000L,
        attributes: [("attempt.number", common::AttributeValue::int(1L))]
      },
      trace::SpanEvent::{
        name: "payment_retry",
        timestamp_unix_nanos: 1640995210200000000L,
        attributes: [("retry.number", common::AttributeValue::int(1L))]
      },
      trace::SpanEvent::{
        name: "error_occurred",
        timestamp_unix_nanos: 1640995210300000000L,
        attributes: [
          ("error.type", common::AttributeValue::string("timeout")),
          ("error.details", common::AttributeValue::string("Gateway response timeout"))
        ]
      }
    ],
    links: []
  }
  
  // 创建错误日志
  let error_logs = [
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995210150000000L,
      observed_timestamp_unix_nanos: Some(1640995210150000000L),
      severity_number: logs::Warn,
      severity_text: Some("WARN"),
      body: Some("Payment attempt failed, retrying"),
      attributes: [
        ("attempt.number", common::AttributeValue::int(1L)),
        ("reason", common::AttributeValue::string("network_error"))
      ],
      trace_id: Some(failed_span.context.trace_id),
      span_id: Some(failed_span.context.span_id),
      trace_flags: Some(0_byte),
      resource: None,
      instrumentation_scope: None
    },
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995210350000000L,
      observed_timestamp_unix_nanos: Some(1640995210350000000L),
      severity_number: logs::Error,
      severity_text: Some("ERROR"),
      body: Some("Payment processing failed after retries"),
      attributes: [
        ("final.error.code", common::AttributeValue::int(error_code)),
        ("total.attempts", common::AttributeValue::int(3L)),
        ("error.message", common::AttributeValue::string(error_message))
      ],
      trace_id: Some(failed_span.context.trace_id),
      span_id: Some(failed_span.context.span_id),
      trace_flags: Some(0_byte),
      resource: None,
      instrumentation_scope: None
    }
  ]
  
  // 创建错误指标
  let error_metrics = [
    ("payment_errors_total", common::AttributeValue::int(1L)),
    ("payment_retry_count", common::AttributeValue::int(3L)),
    ("payment_failure_rate", common::AttributeValue::float(100.0))
  ]
  
  // 验证错误流的一致性
  assert_eq(failed_span.status, trace::Error)
  assert_eq(failed_span.status_description?, error_message)
  assert_eq(error_logs[1].severity_number, logs::Error)
  assert_eq(error_logs[1].severity_text?, "ERROR")
  
  // 验证错误事件序列
  assert_eq(failed_span.events.length(), 3)
  assert_eq(failed_span.events[0].name, "payment_attempt")
  assert_eq(failed_span.events[2].name, "error_occurred")
  
  // 验证错误传播
  match failed_span.attributes[0].1 {
    common::IntValue(code) => assert_eq(code, error_code)
    _ => @test.fail("Error code should be int")
  }
  
  match error_logs[1].attributes[0].1 {
    common::IntValue(final_code) => assert_eq(final_code, error_code)
    _ => @test.fail("Final error code should be int")
  }
  
  // 验证时间线逻辑
  assert_eq(failed_span.start_time_unix_nanos < failed_span.events[0].timestamp_unix_nanos, true)
  assert_eq(failed_span.events[2].timestamp_unix_nanos < error_logs[1].timestamp_unix_nanos, true)
  assert_eq(error_logs[1].timestamp_unix_nanos < failed_span.end_time_unix_nanos?, true)
}

test "end_to_end_high_volume_scenario" {
  // 测试高并发场景下的数据处理
  
  let base_timestamp = 1640995200000000000L
  let span_count = 10
  let logs_per_span = 3
  let metrics_per_operation = 5
  
  // 创建多个Span
  let spans = []
  let mut i = 0
  while i < span_count {
    let span_start = base_timestamp + (i * 1000000000L)  // 每个span间隔1秒
    let span = trace::Span::{
      name: "batch_operation_" + i.to_string(),
      context: trace::SpanContext::{
        trace_id: [for j = 0; j < 16; j = j + 1].map(fn(j) { ((i + j) % 256) |> byte }),
        span_id: [for j = 0; j < 8; j = j + 1].map(fn(j) { ((i * 2 + j) % 256) |> byte }),
        trace_flags: 1_byte,
        trace_state: ""
      },
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: span_start,
      end_time_unix_nanos: Some(span_start + 500000000L),  // 500ms持续时间
      status: trace::Ok,
      status_description: None,
      attributes: [
        ("batch.index", common::AttributeValue::int(i.to_int64())),
        ("operation.type", common::AttributeValue::string("batch_process"))
      ],
      events: [],
      links: []
    }
    spans.push(span)
    i = i + 1
  }
  
  // 验证批量数据
  assert_eq(spans.length(), span_count)
  
  // 验证时间顺序
  let mut j = 0
  while j < spans.length() - 1 {
    assert_eq(spans[j].start_time_unix_nanos < spans[j + 1].start_time_unix_nanos, true)
    j = j + 1
  }
  
  // 验证每个Span的属性
  let mut k = 0
  while k < spans.length() {
    let current_span = spans[k]
    match current_span.attributes[0].1 {
      common::IntValue(batch_index) => assert_eq(batch_index, k.to_int64())
      _ => @test.fail("Batch index should be int")
    }
    
    match current_span.attributes[1].1 {
      common::StringValue(op_type) => assert_eq(op_type, "batch_process")
      _ => @test.fail("Operation type should be string")
    }
    k = k + 1
  }
  
  // 验证Span持续时间
  let mut m = 0
  while m < spans.length() {
    let span = spans[m]
    let duration = span.end_time_unix_nanos? - span.start_time_unix_nanos
    assert_eq(duration, 500000000L)  // 500ms
    m = m + 1
  }
}