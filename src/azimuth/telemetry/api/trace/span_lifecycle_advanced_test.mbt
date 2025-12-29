// Span高级生命周期管理测试用例
// 测试复杂的Span生命周期场景和状态转换

test "span_hierarchical_lifecycle" {
  // 测试Span的层次化生命周期
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("hierarchical-test", "1.0.0")
  let ctx = Context::empty()
  
  // 创建根span
  let (root_ctx, root_span) = tracer.start_span(
    ctx,
    "root-operation",
    Server,
    [
      ("service.name", AttributeValue::string("api-gateway")),
      ("operation.type", AttributeValue::string("request")),
      ("request.id", AttributeValue::string("req-123456"))
    ],
    1640995200000000000L  // 2022-01-01 00:00:00 UTC
  )
  
  // 验证根span
  assert_eq(root_span.name, "root-operation")
  match root_span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  assert_eq(root_span.attributes.length(), 3)
  
  // 创建第一级子span
  let (auth_ctx, auth_span) = tracer.start_span(
    root_ctx,
    "authenticate-user",
    Internal,
    [
      ("sub.operation", AttributeValue::string("auth")),
      ("user.id", AttributeValue::int(12345L)),
      ("auth.method", AttributeValue::string("jwt"))
    ],
    1640995200000000100L  // 100ns later
  )
  
  // 验证第一级子span
  assert_eq(auth_span.name, "authenticate-user")
  match auth_span.kind {
    Internal => assert_eq(true, true)
    _ => @test.fail("Expected Internal span kind")
  }
  assert_eq(auth_span.attributes.length(), 3)
  
  // 创建第二级子span
  let (db_ctx, db_span) = tracer.start_span(
    auth_ctx,
    "database-query",
    Client,
    [
      ("sub.operation", AttributeValue::string("db")),
      ("db.type", AttributeValue::string("postgresql")),
      ("db.query", AttributeValue::string("SELECT * FROM users WHERE id = $1")),
      ("db.connection.pool", AttributeValue::string("primary"))
    ],
    1640995200000000200L  // 200ns later
  )
  
  // 验证第二级子span
  assert_eq(db_span.name, "database-query")
  match db_span.kind {
    Client => assert_eq(true, true)
    _ => @test.fail("Expected Client span kind")
  }
  assert_eq(db_span.attributes.length(), 4)
  
  // 创建并行的子span
  let (cache_ctx, cache_span) = tracer.start_span(
    auth_ctx,
    "cache-lookup",
    Internal,
    [
      ("sub.operation", AttributeValue::string("cache")),
      ("cache.type", AttributeValue::string("redis")),
      ("cache.key", AttributeValue::string("user:12345"))
    ],
    1640995200000000250L  // 250ns later
  )
  
  // 验证并行子span
  assert_eq(cache_span.name, "cache-lookup")
  match cache_span.kind {
    Internal => assert_eq(true, true)
    _ => @test.fail("Expected Internal span kind")
  }
  assert_eq(cache_span.attributes.length(), 3)
  
  // 创建另一个根span（不同的trace）
  let (another_root_ctx, another_root_span) = tracer.start_span(
    ctx,
    "background-job",
    Producer,
    [
      ("job.type", AttributeValue::string("cleanup")),
      ("job.schedule", AttributeValue::string("daily")),
      ("job.instance", AttributeValue::string("worker-01"))
    ],
    1640995200000001000L  // 1μs later
  )
  
  // 验证另一个根span
  assert_eq(another_root_span.name, "background-job")
  match another_root_span.kind {
    Producer => assert_eq(true, true)
    _ => @test.fail("Expected Producer span kind")
  }
  assert_eq(another_root_span.attributes.length(), 3)
  
  // 验证所有span的时间戳递增
  assert_eq(root_span.start_time_unix_nanos < auth_span.start_time_unix_nanos, true)
  assert_eq(auth_span.start_time_unix_nanos < db_span.start_time_unix_nanos, true)
  assert_eq(auth_span.start_time_unix_nanos < cache_span.start_time_unix_nanos, true)
  assert_eq(root_span.start_time_unix_nanos < another_root_span.start_time_unix_nanos, true)
}

test "span_status_transitions" {
  // 测试Span状态转换
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("status-test", "1.0.0")
  let ctx = Context::empty()
  
  // 创建span并测试初始状态
  let (ctx1, span1) = tracer.start_span(ctx, "initial-status", Internal, [], 1000L)
  match span1.status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Expected Unset status")
  }
  match span1.status_description {
    Some(_) => @test.fail("Expected None for status_description")
    None => assert_eq(true, true)
  }
  
  // 模拟span状态转换过程
  // 注意：由于NoopTracer不支持状态更新，这里我们创建不同状态的span来模拟
  
  // 创建成功状态的span
  let (ctx2, span2) = tracer.start_span(ctx, "success-operation", Internal, [], 2000L)
  // 在实际实现中，这里会调用span.set_status(Ok, "Operation completed successfully")
  // 但由于NoopTracer的限制，我们只能创建新的span来模拟
  
  // 创建错误状态的span
  let (ctx3, span3) = tracer.start_span(ctx, "error-operation", Internal, [], 3000L)
  // 在实际实现中，这里会调用span.set_status(Error, "Database connection failed")
  
  // 测试不同类型的错误状态
  let (ctx4, span4) = tracer.start_span(
    ctx,
    "timeout-error",
    Internal,
    [
      ("error.type", AttributeValue::string("timeout")),
      ("error.message", AttributeValue::string("Operation timed out after 30 seconds")),
      ("error.code", AttributeValue::int(408L)),
      ("error.retryable", AttributeValue::bool(true))
    ],
    4000L
  )
  
  let (ctx5, span5) = tracer.start_span(
    ctx,
    "validation-error",
    Internal,
    [
      ("error.type", AttributeValue::string("validation")),
      ("error.message", AttributeValue::string("Invalid input parameter")),
      ("error.field", AttributeValue::string("user.email")),
      ("error.code", AttributeValue::int(400L)),
      ("error.retryable", AttributeValue::bool(false))
    ],
    5000L
  )
  
  let (ctx6, span6) = tracer.start_span(
    ctx,
    "system-error",
    Internal,
    [
      ("error.type", AttributeValue::string("system")),
      ("error.message", AttributeValue::string("Out of memory")),
      ("error.code", AttributeValue::int(500L)),
      ("error.severity", AttributeValue::string("critical")),
      ("error.component", AttributeValue::string("memory.manager"))
    ],
    6000L
  )
  
  // 验证错误span的属性
  assert_eq(span4.attributes.length(), 4)
  assert_eq(span5.attributes.length(), 5)
  assert_eq(span6.attributes.length(), 5)
  
  // 验证错误类型属性
  let mut found_timeout_error = false
  let mut found_validation_error = false
  let mut found_system_error = false
  
  let mut i = 0
  while i < span4.attributes.length() {
    match span4.attributes[i].0 {
      "error.type" => {
        match span4.attributes[i].1 {
          AttributeValue::StringValue(error_type) => {
            assert_eq(error_type, "timeout")
            found_timeout_error = true
          }
          _ => @test.fail("Expected StringValue")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  i = 0
  while i < span5.attributes.length() {
    match span5.attributes[i].0 {
      "error.type" => {
        match span5.attributes[i].1 {
          AttributeValue::StringValue(error_type) => {
            assert_eq(error_type, "validation")
            found_validation_error = true
          }
          _ => @test.fail("Expected StringValue")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  i = 0
  while i < span6.attributes.length() {
    match span6.attributes[i].0 {
      "error.type" => {
        match span6.attributes[i].1 {
          AttributeValue::StringValue(error_type) => {
            assert_eq(error_type, "system")
            found_system_error = true
          }
          _ => @test.fail("Expected StringValue")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_timeout_error, true)
  assert_eq(found_validation_error, true)
  assert_eq(found_system_error, true)
}

test "span_event_lifecycle" {
  // 测试Span事件的生命周期
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("event-test", "1.0.0")
  let ctx = Context::empty()
  
  // 创建一个span来添加事件
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "operation-with-events",
    Internal,
    [
      ("operation.name", AttributeValue::string("data.processing")),
      ("batch.id", AttributeValue::string("batch-12345"))
    ],
    1000L
  )
  
  // 创建各种类型的事件
  let start_event = SpanEvent::{
    name: "operation.started",
    timestamp_unix_nanos: 1000L,
    attributes: [
      ("event.phase", AttributeValue::string("start")),
      ("initiator", AttributeValue::string("scheduler"))
    ]
  }
  
  let progress_event = SpanEvent::{
    name: "operation.progress",
    timestamp_unix_nanos: 2000L,
    attributes: [
      ("event.phase", AttributeValue::string("progress")),
      ("progress.percentage", AttributeValue::float(25.0)),
      ("items.processed", AttributeValue::int(2500L)),
      ("items.total", AttributeValue::int(10000L))
    ]
  }
  
  let warning_event = SpanEvent::{
    name: "operation.warning",
    timestamp_unix_nanos: 3000L,
    attributes: [
      ("event.phase", AttributeValue::string("warning")),
      ("warning.type", AttributeValue::string("slow.processing")),
      ("warning.message", AttributeValue::string("Processing slower than expected")),
      ("threshold.exceeded", AttributeValue::string("processing.time"))
    ]
  }
  
  let error_event = SpanEvent::{
    name: "operation.error",
    timestamp_unix_nanos: 4000L,
    attributes: [
      ("event.phase", AttributeValue::string("error")),
      ("error.type", AttributeValue::string("data.corruption")),
      ("error.message", AttributeValue::string("Invalid data format detected")),
      ("error.severity", AttributeValue::string("medium")),
      ("error.recoverable", AttributeValue::bool(true)),
      ("error.retry.count", AttributeValue::int(3L))
    ]
  }
  
  let recovery_event = SpanEvent::{
    name: "operation.recovery",
    timestamp_unix_nanos: 5000L,
    attributes: [
      ("event.phase", AttributeValue::string("recovery")),
      ("recovery.action", AttributeValue::string("data.cleanup")),
      ("recovery.success", AttributeValue::bool(true)),
      ("data.cleaned", AttributeValue::int(100L))
    ]
  }
  
  let completion_event = SpanEvent::{
    name: "operation.completed",
    timestamp_unix_nanos: 6000L,
    attributes: [
      ("event.phase", AttributeValue::string("completion")),
      ("final.status", AttributeValue::string("success")),
      ("total.duration.ms", AttributeValue::float(5.0)),
      ("items.processed", AttributeValue::int(10000L)),
      ("items.failed", AttributeValue::int(100L)),
      ("items.retried", AttributeValue::int(50L))
    ]
  }
  
  // 验证事件结构
  let events = [start_event, progress_event, warning_event, error_event, recovery_event, completion_event]
  let mut i = 0
  while i < events.length() {
    let event = events[i]
    
    // 验证事件名称不为空
    assert_eq(event.name.length() > 0, true)
    
    // 验证时间戳递增
    if i > 0 {
      assert_eq(events[i-1].timestamp_unix_nanos < event.timestamp_unix_nanos, true)
    }
    
    // 验证事件属性
    assert_eq(event.attributes.length() > 0, true)
    
    i = i + 1
  }
  
  // 验证特定事件的属性
  assert_eq(start_event.attributes.length(), 2)
  assert_eq(progress_event.attributes.length(), 4)
  assert_eq(warning_event.attributes.length(), 3)
  assert_eq(error_event.attributes.length(), 6)
  assert_eq(recovery_event.attributes.length(), 4)
  assert_eq(completion_event.attributes.length(), 6)
  
  // 验证错误事件的复杂属性
  let mut found_error_type = false
  let mut found_error_severity = false
  let mut found_error_recoverable = false
  
  let mut j = 0
  while j < error_event.attributes.length() {
    match error_event.attributes[j].0 {
      "error.type" => found_error_type = true,
      "error.severity" => found_error_severity = true,
      "error.recoverable" => {
        match error_event.attributes[j].1 {
          AttributeValue::BoolValue(recoverable) => {
            assert_eq(recoverable, true)
            found_error_recoverable = true
          }
          _ => @test.fail("Expected BoolValue")
        }
      }
      _ => ()
    }
    j = j + 1
  }
  
  assert_eq(found_error_type, true)
  assert_eq(found_error_severity, true)
  assert_eq(found_error_recoverable, true)
}

test "span_link_relationships" {
  // 测试Span链接关系
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("link-test", "1.0.0")
  let ctx = Context::empty()
  
  // 创建多个相关的span context用于链接
  let parent_context = SpanContext::{
    trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 
               9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
    span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
    trace_flags: 1_byte,
    trace_state: "parent=value1,child=value2"
  }
  
  let related_context1 = SpanContext::{
    trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 
               9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],  // 相同trace
    span_id: [9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],  // 不同span
    trace_flags: 1_byte,
    trace_state: "related=value3"
  }
  
  let related_context2 = SpanContext::{
    trace_id: [17_byte, 18_byte, 19_byte, 20_byte, 21_byte, 22_byte, 23_byte, 24_byte, 
               25_byte, 26_byte, 27_byte, 28_byte, 29_byte, 30_byte, 31_byte, 32_byte],  // 不同trace
    span_id: [17_byte, 18_byte, 19_byte, 20_byte, 21_byte, 22_byte, 23_byte, 24_byte],
    trace_flags: 0_byte,
    trace_state: "external=value4"
  }
  
  let batch_context = SpanContext::{
    trace_id: [33_byte, 34_byte, 35_byte, 36_byte, 37_byte, 38_byte, 39_byte, 40_byte, 
               41_byte, 42_byte, 43_byte, 44_byte, 45_byte, 46_byte, 47_byte, 48_byte],
    span_id: [25_byte, 26_byte, 27_byte, 28_byte, 29_byte, 30_byte, 31_byte, 32_byte],
    trace_flags: 1_byte,
    trace_state: "batch=value5,job=processing"
  }
  
  // 创建带有链接的span
  let (linked_ctx, linked_span) = tracer.start_span(
    ctx,
    "operation-with-links",
    Consumer,
    [
      ("operation.type", AttributeValue::string("aggregation")),
      ("link.count", AttributeValue::int(4L))
    ],
    1000L
  )
  
  // 创建各种类型的链接
  let parent_link = SpanLink::{
    context: parent_context,
    attributes: [
      ("link.type", AttributeValue::string("parent")),
      ("relationship", AttributeValue::string("direct"))
    ]
  }
  
  let same_trace_link = SpanLink::{
    context: related_context1,
    attributes: [
      ("link.type", AttributeValue::string("same_trace")),
      ("relationship", AttributeValue::string("causal"))
    ]
  }
  
  let different_trace_link = SpanLink::{
    context: related_context2,
    attributes: [
      ("link.type", AttributeValue::string("different_trace")),
      ("relationship", AttributeValue::string("correlation")),
      ("external.system", AttributeValue::string("payment.service"))
    ]
  }
  
  let batch_link = SpanLink::{
    context: batch_context,
    attributes: [
      ("link.type", AttributeValue::string("batch")),
      ("relationship", AttributeValue::string("batch_member")),
      ("batch.id", AttributeValue::string("batch-2023-001")),
      ("batch.size", AttributeValue::int(100L)),
      ("batch.index", AttributeValue::int(42L))
    ]
  }
  
  // 验证链接结构
  let links = [parent_link, same_trace_link, different_trace_link, batch_link]
  let mut i = 0
  while i < links.length() {
    let link = links[i]
    
    // 验证链接的context
    assert_eq(link.context.trace_id.length(), 16)
    assert_eq(link.context.span_id.length(), 8)
    
    // 验证链接属性
    assert_eq(link.attributes.length() > 0, true)
    
    i = i + 1
  }
  
  // 验证特定链接的属性
  assert_eq(parent_link.attributes.length(), 2)
  assert_eq(same_trace_link.attributes.length(), 2)
  assert_eq(different_trace_link.attributes.length(), 3)
  assert_eq(batch_link.attributes.length(), 5)
  
  // 验证相同trace的链接
  assert_eq(parent_context.trace_id, related_context1.trace_id)
  assert_eq(parent_context.trace_id != related_context2.trace_id, true)
  assert_eq(parent_context.trace_id != batch_context.trace_id, true)
  
  // 验证不同span ID
  assert_eq(parent_context.span_id != related_context1.span_id, true)
  
  // 验证trace flags
  assert_eq(parent_context.trace_flags, 1_byte)
  assert_eq(related_context1.trace_flags, 1_byte)
  assert_eq(related_context2.trace_flags, 0_byte)
  assert_eq(batch_context.trace_flags, 1_byte)
  
  // 验证trace state
  assert_eq(parent_context.trace_state.contains("parent=value1"), true)
  assert_eq(related_context1.trace_state.contains("related=value3"), true)
  assert_eq(related_context2.trace_state.contains("external=value4"), true)
  assert_eq(batch_context.trace_state.contains("batch=value5"), true)
}

test "span_timing_and_duration" {
  // 测试Span时间和持续时间计算
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("timing-test", "1.0.0")
  let ctx = Context::empty()
  
  // 创建不同时间跨度的span
  let base_time = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  
  // 微秒级操作
  let (micro_ctx, micro_span) = tracer.start_span(
    ctx,
    "micro-operation",
    Internal,
    [("operation.scale", AttributeValue::string("micro"))],
    base_time
  )
  
  // 毫秒级操作
  let (milli_ctx, milli_span) = tracer.start_span(
    ctx,
    "milli-operation",
    Internal,
    [("operation.scale", AttributeValue::string("milli"))],
    base_time + 1000000L  // 1ms later
  )
  
  // 秒级操作
  let (second_ctx, second_span) = tracer.start_span(
    ctx,
    "second-operation",
    Internal,
    [("operation.scale", AttributeValue::string("second"))],
    base_time + 1000000000L  // 1s later
  )
  
  // 分钟级操作
  let (minute_ctx, minute_span) = tracer.start_span(
    ctx,
    "minute-operation",
    Internal,
    [("operation.scale", AttributeValue::string("minute"))],
    base_time + 60000000000L  // 60s later
  )
  
  // 验证时间戳递增
  assert_eq(micro_span.start_time_unix_nanos < milli_span.start_time_unix_nanos, true)
  assert_eq(milli_span.start_time_unix_nanos < second_span.start_time_unix_nanos, true)
  assert_eq(second_span.start_time_unix_nanos < minute_span.start_time_unix_nanos, true)
  
  // 测试时间戳精度
  let high_precision_time = 1640995200123456789L
  let (precision_ctx, precision_span) = tracer.start_span(
    ctx,
    "high-precision-operation",
    Internal,
    [
      ("timestamp.precision", AttributeValue::string("nanosecond")),
      ("timestamp.value", AttributeValue::string("1640995200123456789"))
    ],
    high_precision_time
  )
  
  assert_eq(precision_span.start_time_unix_nanos, high_precision_time)
  
  // 测试极值时间戳
  let min_time = -9223372036854775808L  // Int64最小值
  let max_time = 9223372036854775807L   // Int64最大值
  
  let (min_ctx, min_span) = tracer.start_span(
    ctx,
    "minimum-time-operation",
    Internal,
    [("time.boundary", AttributeValue::string("minimum"))],
    min_time
  )
  
  let (max_ctx, max_span) = tracer.start_span(
    ctx,
    "maximum-time-operation",
    Internal,
    [("time.boundary", AttributeValue::string("maximum"))],
    max_time
  )
  
  assert_eq(min_span.start_time_unix_nanos, min_time)
  assert_eq(max_span.start_time_unix_nanos, max_time)
  
  // 测试相对时间计算
  let start_time = 1000000L
  let end_time = 5000000L
  let duration = end_time - start_time
  
  let (relative_ctx, relative_span) = tracer.start_span(
    ctx,
    "relative-timing-operation",
    Internal,
    [
      ("start.time", AttributeValue::int(start_time)),
      ("end.time", AttributeValue::int(end_time)),
      ("duration", AttributeValue::int(duration)),
      ("duration.ms", AttributeValue::float((duration as Double) / 1000000.0))
    ],
    start_time
  )
  
  assert_eq(relative_span.start_time_unix_nanos, start_time)
  assert_eq(duration, 4000000L)
  
  // 验证duration属性
  let mut found_duration = false
  let mut found_duration_ms = false
  
  let mut i = 0
  while i < relative_span.attributes.length() {
    match relative_span.attributes[i].0 {
      "duration" => {
        match relative_span.attributes[i].1 {
          AttributeValue::IntValue(d) => {
            assert_eq(d, duration)
            found_duration = true
          }
          _ => @test.fail("Expected IntValue")
        }
      }
      "duration.ms" => {
        match relative_span.attributes[i].1 {
          AttributeValue::FloatValue(d_ms) => {
            assert_eq(d_ms, 4.0)
            found_duration_ms = true
          }
          _ => @test.fail("Expected FloatValue")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_duration, true)
  assert_eq(found_duration_ms, true)
}