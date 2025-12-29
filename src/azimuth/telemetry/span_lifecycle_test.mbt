// Azimuth Telemetry - Span Lifecycle Tests
// Span生命周期测试

test "span_lifecycle_complete_workflow" {
  // 测试Span的完整生命周期工作流
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  let base_time = 1640995200000000000L
  
  // 1. 创建Span（开始）
  let (span_context, span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "lifecycle-test-span",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("test-service")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("lifecycle")),
      ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-123"))
    ]),
    Some(base_time)
  )
  
  // 验证Span初始状态
  assert_eq(span.name, "lifecycle-test-span")
  assert_eq(span.kind, @azimuth.telemetry.api.trace.Server)
  assert_eq(span.start_time_unix_nanos, base_time)
  assert_eq(span.end_time_unix_nanos, None) // 还未结束
  assert_eq(span.status, @azimuth.telemetry.api.trace.Unset)
  assert_eq(span.events.length(), 0)
  
  // 2. 添加事件（运行时）
  let start_event = @azimuth.telemetry.api.trace.SpanEvent::{
    name: "operation.started",
    timestamp_unix_nanos: base_time + 1000000L,
    attributes: [
      ("phase", @azimuth.telemetry.api.common.AttributeValue::string("initialization")),
      ("component", @azimuth.telemetry.api.common.AttributeValue::string("database"))
    ]
  }
  
  let progress_event = @azimuth.telemetry.api.trace.SpanEvent::{
    name: "operation.in_progress",
    timestamp_unix_nanos: base_time + 5000000L,
    attributes: [
      ("phase", @azimuth.telemetry.api.common.AttributeValue::string("processing")),
      ("records.processed", @azimuth.telemetry.api.common.AttributeValue::int(50L)),
      ("records.total", @azimuth.telemetry.api.common.AttributeValue::int(100L))
    ]
  }
  
  let events = [start_event, progress_event]
  
  // 验证事件
  assert_eq(events.length(), 2)
  assert_eq(events[0].name, "operation.started")
  assert_eq(events[1].name, "operation.in_progress")
  assert_eq(events[0].timestamp_unix_nanos, base_time + 1000000L)
  assert_eq(events[1].timestamp_unix_nanos, base_time + 5000000L)
  
  // 3. 添加链接（关联）
  let linked_trace_id = Array::make(16, 1_byte)
  let linked_span_id = Array::make(8, 2_byte)
  
  let linked_context = @azimuth.telemetry.api.trace.SpanContext::{
    trace_id: linked_trace_id,
    span_id: linked_span_id,
    trace_flags: 1_byte,
    trace_state: "parent.operation=request-123"
  }
  
  let causal_link = @azimuth.telemetry.api.trace.SpanLink::{
    context: linked_context,
    attributes: [
      ("link.type", @azimuth.telemetry.api.common.AttributeValue::string("causal")),
      ("parent.operation", @azimuth.telemetry.api.common.AttributeValue::string("user-request"))
    ]
  }
  
  let links = [causal_link]
  
  // 验证链接
  assert_eq(links.length(), 1)
  assert_eq(links[0].context.trace_flags, 1_byte)
  assert_eq(links[0].context.trace_state, "parent.operation=request-123")
  
  // 4. 模拟Span结束（在实际实现中会设置end_time）
  let end_time = base_time + 10000000L // 10秒后结束
  
  // 创建最终状态的日志记录
  let completion_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(end_time)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Span lifecycle test completed successfully")
    .with_attribute("span.name", @azimuth.telemetry.api.common.AttributeValue::string("lifecycle-test-span"))
    .with_attribute("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(10000L))
    .with_attribute("events.count", @azimuth.telemetry.api.common.AttributeValue::int(2L))
    .with_attribute("links.count", @azimuth.telemetry.api.common.AttributeValue::int(1L))
    .build()
  
  // 5. 创建Span生命周期的指标
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  let span_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("spans.created.total", "spans", "Total spans created")
  let span_duration = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("span.duration", "ms", "Span duration")
  
  let span_attrs = [
    ("span.name", @azimuth.telemetry.api.common.AttributeValue::string("lifecycle-test-span")),
    ("span.kind", @azimuth.telemetry.api.common.AttributeValue::string("server")),
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("test-service"))
  ]
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(span_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(10000.0, Some(span_attrs))
  
  // 验证Span生命周期完成
  assert_eq(span.name, "lifecycle-test-span")
  assert_eq(span.start_time_unix_nanos, base_time)
  assert_eq(events.length(), 2)
  assert_eq(links.length(), 1)
  assert_eq(completion_log.body, Some("Span lifecycle test completed successfully"))
  assert_eq(completion_log.attributes.length(), 4)
}