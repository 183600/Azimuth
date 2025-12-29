// Azimuth Telemetry - Span Lifecycle Tests
// 测试Span的完整生命周期管理

test "span_complete_lifecycle" {
  // 测试Span的完整生命周期
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let start_time = 1640995200000000000L // 2022-01-01 00:00:00 UTC
  let end_time = start_time + 10000000L // 10ms later
  
  // 1. 创建span
  let (ctx1, span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx,
    "operation-lifecycle",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("api-service")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("business-logic"))
    ]),
    Some(start_time)
  )
  
  // 验证span初始状态
  assert_eq(span.name, "operation-lifecycle")
  assert_eq(span.kind, @azimuth.telemetry.api.trace.Server)
  assert_eq(span.start_time_unix_nanos, start_time)
  assert_eq(span.end_time_unix_nanos, None) // 还未结束
  assert_eq(span.status, @azimuth.telemetry.api.trace.Unset)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  
  // 2. 模拟span生命周期中的事件
  let start_event = @azimuth.telemetry.api.trace.SpanEvent::{
    name: "operation.started",
    timestamp_unix_nanos: start_time + 1000000L,
    attributes: [
      ("phase", @azimuth.telemetry.api.common.AttributeValue::string("initialization")),
      ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(1))
    ]
  }
  
  let processing_event = @azimuth.telemetry.api.trace.SpanEvent::{
    name: "operation.processing",
    timestamp_unix_nanos: start_time + 5000000L,
    attributes: [
      ("phase", @azimuth.telemetry.api.common.AttributeValue::string("main-logic")),
      ("records.processed", @azimuth.telemetry.api.common.AttributeValue::int(150)),
      ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(4))
    ]
  }
  
  let completion_event = @azimuth.telemetry.api.trace.SpanEvent::{
    name: "operation.completed",
    timestamp_unix_nanos: end_time,
    attributes: [
      ("phase", @azimuth.telemetry.api.common.AttributeValue::string("cleanup")),
      ("total.records", @azimuth.telemetry.api.common.AttributeValue::int(150)),
      ("success.rate", @azimuth.telemetry.api.common.AttributeValue::float(0.98)),
      ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(5))
    ]
  }
  
  // 3. 验证事件时间顺序和内容
  assert_eq(start_event.timestamp_unix_nanos, start_time + 1000000L)
  assert_eq(processing_event.timestamp_unix_nanos, start_time + 5000000L)
  assert_eq(completion_event.timestamp_unix_nanos, end_time)
  
  // 验证事件属性
  match processing_event.attributes[0] {
    ("phase", @azimuth.telemetry.api.common.AttributeValue::StringValue(phase)) => {
      assert_eq(phase, "main-logic")
    }
    _ => @test.fail("Expected phase attribute")
  }
  
  match completion_event.attributes[1] {
    ("total.records", @azimuth.telemetry.api.common.AttributeValue::IntValue(total)) => {
      assert_eq(total, 150)
    }
    _ => @test.fail("Expected total.records attribute")
  }
}

test "span_status_transitions" {
  // 测试Span状态转换
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建span并测试不同状态
  let (_, success_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx,
    "successful-operation",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("query")),
      ("result.count", @azimuth.telemetry.api.common.AttributeValue::int(42))
    ])
  )
  
  let (_, error_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx,
    "failed-operation",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("mutation")),
      ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(500))
    ])
  )
  
  // 验证初始状态
  assert_eq(success_span.status, @azimuth.telemetry.api.trace.Unset)
  assert_eq(success_span.status_description, None)
  assert_eq(error_span.status, @azimuth.telemetry.api.trace.Unset)
  assert_eq(error_span.status_description, None)
  
  // 在真实实现中，这里会测试状态转换
  // success_span.set_status(@azimuth.telemetry.api.trace.Ok, "Operation completed successfully")
  // error_span.set_status(@azimuth.telemetry.api.trace.Error, "Database connection failed")
}

test "span_parent_child_relationships" {
  // 测试Span父子关系
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let base_time = 1640995200000000000L
  
  // 创建根span
  let (ctx1, root_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx,
    "root-operation",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("gateway")),
      ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-001"))
    ]),
    Some(base_time)
  )
  
  // 创建子span
  let (ctx2, child_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx1,
    "child-operation",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("downstream")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("database"))
    ]),
    Some(base_time + 1000000L)
  )
  
  // 创建孙子span
  let (ctx3, grandchild_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx2,
    "grandchild-operation",
    Some(@azimuth.telemetry.api.trace.Internal),
    Some([
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("cache")),
      ("cache.key", @azimuth.telemetry.api.common.AttributeValue::string("user:123"))
    ]),
    Some(base_time + 2000000L)
  )
  
  // 验证span层次结构
  assert_eq(root_span.name, "root-operation")
  assert_eq(root_span.kind, @azimuth.telemetry.api.trace.Server)
  assert_eq(root_span.parent_span_id, None)
  assert_eq(root_span.start_time_unix_nanos, base_time)
  
  assert_eq(child_span.name, "child-operation")
  assert_eq(child_span.kind, @azimuth.telemetry.api.trace.Client)
  assert_eq(child_span.start_time_unix_nanos, base_time + 1000000L)
  
  assert_eq(grandchild_span.name, "grandchild-operation")
  assert_eq(grandchild_span.kind, @azimuth.telemetry.api.trace.Internal)
  assert_eq(grandchild_span.start_time_unix_nanos, base_time + 2000000L)
  
  // 验证时间顺序
  assert_eq(root_span.start_time_unix_nanos < child_span.start_time_unix_nanos, true)
  assert_eq(child_span.start_time_unix_nanos < grandchild_span.start_time_unix_nanos, true)
}

test "span_with_complex_events" {
  // 测试包含复杂事件的Span
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let base_time = 1640995200000000000L
  
  let (_, span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx,
    "complex-event-span",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("operation.id", @azimuth.telemetry.api.common.AttributeValue::string("op-12345")),
      ("batch.size", @azimuth.telemetry.api.common.AttributeValue::int(1000))
    ]),
    Some(base_time)
  )
  
  // 创建复杂的span事件
  let validation_event = @azimuth.telemetry.api.trace.SpanEvent::{
    name: "validation.completed",
    timestamp_unix_nanos: base_time + 1000000L,
    attributes: [
      ("validation.type", @azimuth.telemetry.api.common.AttributeValue::string("schema")),
      ("records.valid", @azimuth.telemetry.api.common.AttributeValue::int(950)),
      ("records.invalid", @azimuth.telemetry.api.common.AttributeValue::int(50)),
      ("validation.duration.ms", @azimuth.telemetry.api.common.AttributeValue::float(15.5)),
      ("validation.rules", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue([
        "required_fields", "data_types", "value_ranges", "format_checks"
      ]))
    ]
  }
  
  let transformation_event = @azimuth.telemetry.api.trace.SpanEvent::{
    name: "transformation.started",
    timestamp_unix_nanos: base_time + 2000000L,
    attributes: [
      ("transformation.type", @azimuth.telemetry.api.common.AttributeValue::string("enrichment")),
      ("input.format", @azimuth.telemetry.api.common.AttributeValue::string("json")),
      ("output.format", @azimuth.telemetry.api.common.AttributeValue::string("avro")),
      ("parallel.workers", @azimuth.telemetry.api.common.AttributeValue::int(4)),
      ("memory.usage.mb", @azimuth.telemetry.api.common.AttributeValue::float(256.5)),
      ("cpu.usage.percent", @azimuth.telemetry.api.common.AttributeValue::float(75.2))
    ]
  }
  
  let error_event = @azimuth.telemetry.api.trace.SpanEvent::{
    name: "error.occurred",
    timestamp_unix_nanos: base_time + 8000000L,
    attributes: [
      ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("TransformationError")),
      ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(5001)),
      ("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Field mapping failed")),
      ("failed.records", @azimuth.telemetry.api.common.AttributeValue::int(25)),
      ("retry.count", @azimuth.telemetry.api.common.AttributeValue::int(3)),
      ("recovery.possible", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
      ("error.stack", @azimuth.telemetry.api.common.AttributeValue::string("com.example.Transformer.mapField(Transformer.java:123)")),
      ("affected.fields", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue([
        "user.address", "order.total", "payment.method"
      ]))
    ]
  }
  
  // 验证复杂事件属性
  match validation_event.attributes[4] {
    ("validation.rules", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(rules)) => {
      assert_eq(rules.length(), 4)
      assert_eq(rules[0], "required_fields")
      assert_eq(rules[1], "data_types")
      assert_eq(rules[2], "value_ranges")
      assert_eq(rules[3], "format_checks")
    }
    _ => @test.fail("Expected validation.rules attribute")
  }
  
  match error_event.attributes[0] {
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::StringValue(error_type)) => {
      assert_eq(error_type, "TransformationError")
    }
    _ => @test.fail("Expected error.type attribute")
  }
  
  match error_event.attributes[5] {
    ("recovery.possible", @azimuth.telemetry.api.common.AttributeValue::BoolValue(recovery)) => {
      assert_eq(recovery, true)
    }
    _ => @test.fail("Expected recovery.possible attribute")
  }
  
  match error_event.attributes[8] {
    ("affected.fields", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(fields)) => {
      assert_eq(fields.length(), 3)
      assert_eq(fields[0], "user.address")
      assert_eq(fields[1], "order.total")
      assert_eq(fields[2], "payment.method")
    }
    _ => @test.fail("Expected affected.fields attribute")
  }
}

test "span_with_complex_links" {
  // 测试包含复杂链接的Span
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  let (_, span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx,
    "complex-links-span",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("operation.id", @azimuth.telemetry.api.common.AttributeValue::string("op-67890")),
      ("workflow.type", @azimuth.telemetry.api.common.AttributeValue::string("data-pipeline"))
    ])
  )
  
  // 创建复杂的span链接
  let causal_trace_id = Array::make(16, 1_byte)
  let causal_span_id = Array::make(8, 1_byte)
  causal_trace_id[0] = 0x12_byte
  causal_trace_id[1] = 0x34_byte
  causal_span_id[0] = 0x56_byte
  causal_span_id[1] = 0x78_byte
  
  let causal_context = @azimuth.telemetry.api.trace.SpanContext::{
    trace_id: causal_trace_id,
    span_id: causal_span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  let causal_link = @azimuth.telemetry.api.trace.SpanLink::{
    context: causal_context,
    attributes: [
      ("link.type", @azimuth.telemetry.api.common.AttributeValue::string("causal")),
      ("relationship", @azimuth.telemetry.api.common.AttributeValue::string("parent-operation")),
      ("correlation.id", @azimuth.telemetry.api.common.AttributeValue::string("corr-67890")),
      ("causality.strength", @azimuth.telemetry.api.common.AttributeValue::float(0.95)),
      ("time.delta.ms", @azimuth.telemetry.api.common.AttributeValue::int(5000))
    ]
  }
  
  let context_trace_id = Array::make(16, 2_byte)
  let context_span_id = Array::make(8, 2_byte)
  context_trace_id[0] = 0xab_byte
  context_trace_id[1] = 0xcd_byte
  context_span_id[0] = 0xef_byte
  context_span_id[1] = 0x01_byte
  
  let context_link_context = @azimuth.telemetry.api.trace.SpanContext::{
    trace_id: context_trace_id,
    span_id: context_span_id,
    trace_flags: 1_byte,
    trace_state: "sample.decision=true"
  }
  
  let context_link = @azimuth.telemetry.api.trace.SpanLink::{
    context: context_link_context,
    attributes: [
      ("link.type", @azimuth.telemetry.api.common.AttributeValue::string("context")),
      ("shared.data", @azimuth.telemetry.api.common.AttributeValue::string("user-session")),
      ("data.source", @azimuth.telemetry.api.common.AttributeValue::string("auth-service")),
      ("session.age.minutes", @azimuth.telemetry.api.common.AttributeValue::int(45)),
      ("user.privilege.level", @azimuth.telemetry.api.common.AttributeValue::string("premium")),
      ("shared.resources", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue([
        "cache", "connection-pool", "thread-local"
      ]))
    ]
  }
  
  // 验证复杂链接属性
  assert_eq(causal_link.context.trace_id.length(), 16)
  assert_eq(causal_link.context.span_id.length(), 8)
  assert_eq(causal_link.context.trace_flags, 1_byte)
  assert_eq(causal_link.context.trace_state, "key1=value1,key2=value2")
  
  match causal_link.attributes[0] {
    ("link.type", @azimuth.telemetry.api.common.AttributeValue::StringValue(link_type)) => {
      assert_eq(link_type, "causal")
    }
    _ => @test.fail("Expected link.type attribute")
  }
  
  match causal_link.attributes[3] {
    ("causality.strength", @azimuth.telemetry.api.common.AttributeValue::FloatValue(strength)) => {
      assert_eq(strength, 0.95)
    }
    _ => @test.fail("Expected causality.strength attribute")
  }
  
  match context_link.attributes[5] {
    ("shared.resources", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(resources)) => {
      assert_eq(resources.length(), 3)
      assert_eq(resources[0], "cache")
      assert_eq(resources[1], "connection-pool")
      assert_eq(resources[2], "thread-local")
    }
    _ => @test.fail("Expected shared.resources attribute")
  }
}

test "span_context_and_trace_propagation" {
  // 测试Span上下文和追踪传播的详细场景
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建根span
  let (ctx1, root_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx,
    "root-operation",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("gateway")),
      ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-001"))
    ])
  )
  
  // 验证根span的上下文
  assert_eq(root_span.context.trace_id.length(), 16)
  assert_eq(root_span.context.span_id.length(), 8)
  assert_eq(root_span.context.trace_flags, 0_byte)
  assert_eq(root_span.context.trace_state, "")
  
  // 创建子span（应该继承trace_id）
  let (ctx2, child_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    ctx1,
    "child-operation",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("downstream")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("database"))
    ])
  )
  
  // 验证子span的上下文
  assert_eq(child_span.context.trace_id.length(), 16)
  assert_eq(child_span.context.span_id.length(), 8)
  
  // 创建具有不同trace_flags的span
  let sampled_trace_id = Array::make(16, 3_byte)
  let sampled_span_id = Array::make(8, 3_byte)
  sampled_trace_id[0] = 0xaa_byte
  sampled_trace_id[1] = 0xbb_byte
  sampled_span_id[0] = 0xcc_byte
  sampled_span_id[1] = 0xdd_byte
  
  let sampled_context = @azimuth.telemetry.api.trace.SpanContext::{
    trace_id: sampled_trace_id,
    span_id: sampled_span_id,
    trace_flags: 1_byte, // 采样标志
    trace_state: "sample.decision=true"
  }
  
  // 创建带链接的span来模拟传播
  let propagation_link = @azimuth.telemetry.api.trace.SpanLink::{
    context: sampled_context,
    attributes: [
      ("propagation.type", @azimuth.telemetry.api.common.AttributeValue::string("trace-context")),
      ("sampling.decision", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
      ("propagation.source", @azimuth.telemetry.api.common.AttributeValue::string("external-service")),
      ("propagation.protocol", @azimuth.telemetry.api.common.AttributeValue::string("http")),
      ("propagation.version", @azimuth.telemetry.api.common.AttributeValue::string("1.0"))
    ]
  }
  
  // 验证传播的上下文信息
  assert_eq(propagation_link.context.trace_flags, 1_byte)
  assert_eq(propagation_link.context.trace_state, "sample.decision=true")
  
  match propagation_link.attributes[1] {
    ("sampling.decision", @azimuth.telemetry.api.common.AttributeValue::BoolValue(sampled)) => {
      assert_eq(sampled, true)
    }
    _ => @test.fail("Expected sampling.decision attribute")
  }
  
  match propagation_link.attributes[2] {
    ("propagation.source", @azimuth.telemetry.api.common.AttributeValue::StringValue(source)) => {
      assert_eq(source, "external-service")
    }
    _ => @test.fail("Expected propagation.source attribute")
  }
}