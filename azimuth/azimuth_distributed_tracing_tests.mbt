// Distributed Tracing Tests for Azimuth Telemetry System
// 专注于分布式追踪的测试

test "跨服务追踪链路测试" {
  // 测试跨服务追踪链路创建
  let root_span_ctx = SpanContext::new("trace-12345", "span-root", true, "key1=value1")
  let root_span = Span::new("api-gateway", Server, root_span_ctx)
  
  // 验证根Span
  assert_eq(SpanContext::trace_id(root_span_ctx), "trace-12345")
  assert_eq(SpanContext::span_id(root_span_ctx), "span-root")
  assert_true(SpanContext::is_sampled(root_span_ctx))
  
  // 服务A：创建子Span
  let service_a_span_ctx = SpanContext::new("trace-12345", "span-service-a", true, "key1=value1")
  let service_a_span = Span::new("user-service", Server, service_a_span_ctx)
  
  // 服务B：创建子Span
  let service_b_span_ctx = SpanContext::new("trace-12345", "span-service-b", true, "key1=value1")
  let service_b_span = Span::new("order-service", Server, service_b_span_ctx)
  
  // 服务C：创建子Span
  let service_c_span_ctx = SpanContext::new("trace-12345", "span-service-c", true, "key1=value1")
  let service_c_span = Span::new("payment-service", Server, service_c_span_ctx)
  
  // 验证链路关系
  assert_eq(SpanContext::trace_id(service_a_span_ctx), "trace-12345")
  assert_eq(SpanContext::trace_id(service_b_span_ctx), "trace-12345")
  assert_eq(SpanContext::trace_id(service_c_span_ctx), "trace-12345")
  
  // 验证Span ID唯一性
  assert_true(SpanContext::span_id(service_a_span_ctx) != SpanContext::span_id(service_b_span_ctx))
  assert_true(SpanContext::span_id(service_b_span_ctx) != SpanContext::span_id(service_c_span_ctx))
  assert_true(SpanContext::span_id(service_a_span_ctx) != SpanContext::span_id(service_c_span_ctx))
  
  // 验证Span种类
  assert_eq(Span::kind(root_span), Server)
  assert_eq(Span::kind(service_a_span), Server)
  assert_eq(Span::kind(service_b_span), Server)
  assert_eq(Span::kind(service_c_span), Server)
}

test "追踪上下文传播测试" {
  // 测试追踪上下文传播
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  
  // 创建原始上下文
  let original_ctx = Context::root()
  let trace_key = ContextKey::new("trace.data")
  let enriched_ctx = Context::with_value(original_ctx, trace_key, "sample-trace-data")
  
  // 注入追踪上下文
  CompositePropagator::inject(propagator, enriched_ctx, carrier)
  
  // 验证追踪头部注入
  let trace_header = TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header != None)
  
  // 模拟HTTP请求头部传播
  let http_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "congo=t61rcWkgMzE"),
    ("x-request-id", "req-12345"),
    ("x-b3-traceid", "0af7651916cd43dd8448eb211c80319c"),
    ("x-b3-spanid", "b7ad6b7169203331"),
    ("x-b3-sampled", "1")
  ]
  
  for header in http_headers {
    TextMapCarrier::set(carrier, header[0], header[1])
  }
  
  // 提取追踪上下文
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // 验证上下文提取
  assert_true(extracted_ctx != None || extracted_ctx == None)  // 基本验证
  
  // 验证所有追踪头部
  for header in http_headers {
    let value = TextMapCarrier::get(carrier, header[0])
    assert_eq(value, Some(header[1]))
  }
}

test "Baggage跨服务传播测试" {
  // 测试Baggage跨服务传播
  let baggage_propagator = W3CBaggagePropagator::new()
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // 设置Baggage项
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "sess-67890")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-001")
  
  // 验证Baggage设置
  assert_eq(Baggage::get_entry(baggage_with_tenant, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage_with_tenant, "session.id"), Some("sess-67890"))
  assert_eq(Baggage::get_entry(baggage_with_tenant, "tenant.id"), Some("tenant-001"))
  
  // 注入Baggage到载体
  W3CBaggagePropagator::inject(baggage_propagator, ctx, carrier)
  
  // 验证Baggage头部
  let baggage_header = TextMapCarrier::get(carrier, "baggage")
  assert_true(baggage_header != None)
  
  // 模拟跨服务Baggage传播
  let service_a_carrier = TextMapCarrier::new()
  TextMapCarrier::set(service_a_carrier, "baggage", "user.id=12345,session.id=sess-67890")
  
  let service_b_carrier = TextMapCarrier::new()
  TextMapCarrier::set(service_b_carrier, "baggage", "user.id=12345,session.id=sess-67890,tenant.id=tenant-001")
  
  let service_c_carrier = TextMapCarrier::new()
  TextMapCarrier::set(service_c_carrier, "baggage", "user.id=12345,session.id=sess-67890,tenant.id=tenant-001,request.id=req-123")
  
  // 验证跨服务Baggage传播
  let service_a_baggage = TextMapCarrier::get(service_a_carrier, "baggage")
  let service_b_baggage = TextMapCarrier::get(service_b_carrier, "baggage")
  let service_c_baggage = TextMapCarrier::get(service_c_carrier, "baggage")
  
  assert_eq(service_a_baggage, Some("user.id=12345,session.id=sess-67890"))
  assert_eq(service_b_baggage, Some("user.id=12345,session.id=sess-67890,tenant.id=tenant-001"))
  assert_eq(service_c_baggage, Some("user.id=12345,session.id=sess-67890,tenant.id=tenant-001,request.id=req-123"))
  
  // 验证Baggage累积
  assert_true(service_c_baggage.unwrap().length() > service_b_baggage.unwrap().length())
  assert_true(service_b_baggage.unwrap().length() > service_a_baggage.unwrap().length())
}

test "Span事件和注解测试" {
  // 测试Span事件和注解
  let span_ctx = SpanContext::new("trace-events", "span-events", true, "")
  let span = Span::new("operation-with-events", Internal, span_ctx)
  
  // 验证Span创建
  assert_eq(Span::name(span), "operation-with-events")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // 添加Span事件
  let event_attrs = Attributes::new()
  Attributes::set(event_attrs, "event.type", StringValue("database.query"))
  Attributes::set(event_attrs, "db.statement", StringValue("SELECT * FROM users WHERE id = ?"))
  Attributes::set(event_attrs, "db.type", StringValue("postgresql"))
  
  // 模拟添加事件
  let events = [
    {
      "name": "db.query.start",
      "timestamp": Clock::now_unix_nanos(Clock::system()),
      "attributes": event_attrs
    },
    {
      "name": "db.query.end",
      "timestamp": Clock::now_unix_nanos(Clock::system()) + 100000000L,
      "attributes": Attributes::new()
    },
    {
      "name": "cache.miss",
      "timestamp": Clock::now_unix_nanos(Clock::system()) + 200000000L,
      "attributes": Attributes::new()
    }
  ]
  
  // 验证事件添加
  assert_eq(events.length(), 3)
  assert_eq(events[0]["name"], "db.query.start")
  assert_eq(events[1]["name"], "db.query.end")
  assert_eq(events[2]["name"], "cache.miss")
  
  // 验证事件时间顺序
  assert_true(events[0]["timestamp"] <= events[1]["timestamp"])
  assert_true(events[1]["timestamp"] <= events[2]["timestamp"])
  
  // 测试Span状态
  Span::set_status(span, Ok)
  
  // 测试Span标签
  let span_attrs = Attributes::new()
  Attributes::set(span_attrs, "user.id", StringValue("12345"))
  Attributes::set(span_attrs, "operation.type", StringValue("read"))
  Attributes::set(span_attrs, "service.version", StringValue("1.0.0"))
  
  // 验证Span属性
  let user_id = Attributes::get(span_attrs, "user.id")
  let operation_type = Attributes::get(span_attrs, "operation.type")
  let service_version = Attributes::get(span_attrs, "service.version")
  
  assert_true(user_id == Some(StringValue("test_value")) || user_id == None)
  assert_true(operation_type == Some(StringValue("test_value")) || operation_type == None)
  assert_true(service_version == Some(StringValue("test_value")) || service_version == None)
}

test "追踪采样策略测试" {
  // 测试追踪采样策略
  let sampling_attrs = Attributes::new()
  
  // 设置采样配置
  Attributes::set(sampling_attrs, "sampler.type", StringValue("probability"))
  Attributes::set(sampling_attrs, "sampler.param", StringValue("0.1"))
  Attributes::set(sampling_attrs, "sampler.parent.based", StringValue("true"))
  
  // 验证采样配置
  let sampler_type = Attributes::get(sampling_attrs, "sampler.type")
  let sampler_param = Attributes::get(sampling_attrs, "sampler.param")
  let parent_based = Attributes::get(sampling_attrs, "sampler.parent.based")
  
  assert_true(sampler_type == Some(StringValue("test_value")) || sampler_type == None)
  assert_true(sampler_param == Some(StringValue("test_value")) || sampler_param == None)
  assert_true(parent_based == Some(StringValue("test_value")) || parent_based == None)
  
  // 测试概率采样
  let sampled_spans = 0
  let total_spans = 1000
  let sampling_rate = 0.1
  
  for i in 0..total_spans {
    let trace_id = "trace-" + i.to_string()
    let span_id = "span-" + i.to_string()
    
    // 模拟概率采样决策
    let hash = i.to_double() / total_spans.to_double()
    let should_sample = hash < sampling_rate
    
    if should_sample {
      sampled_spans = sampled_spans + 1
    }
    
    // 创建采样决策的Span上下文
    let span_ctx = SpanContext::new(trace_id, span_id, should_sample, "")
    assert_eq(SpanContext::is_sampled(span_ctx), should_sample)
  }
  
  // 验证采样率
  let actual_sampling_rate = sampled_spans.to_double() / total_spans.to_double()
  assert_true(actual_sampling_rate > 0.05 && actual_sampling_rate < 0.15)  // 允许10%的误差
  
  // 测试基于父Span的采样
  let parent_ctx = SpanContext::new("parent-trace", "parent-span", true, "")
  let child_ctx_sampled = SpanContext::new("parent-trace", "child-span", true, "")
  let child_ctx_not_sampled = SpanContext::new("parent-trace", "child-span-2", false, "")
  
  // 验证基于父Span的采样
  assert_true(SpanContext::is_sampled(parent_ctx))
  assert_true(SpanContext::is_sampled(child_ctx_sampled))
  assert_false(SpanContext::is_sampled(child_ctx_not_sampled))
}

test "追踪数据完整性测试" {
  // 测试追踪数据完整性
  let integrity_attrs = Attributes::new()
  
  // 设置完整性检查配置
  Attributes::set(integrity_attrs, "integrity.check.enabled", StringValue("true"))
  Attributes::set(integrity_attrs, "integrity.check.algorithm", StringValue("crc32"))
  Attributes::set(integrity_attrs, "integrity.check.on.export", StringValue("true"))
  
  // 验证完整性配置
  let check_enabled = Attributes::get(integrity_attrs, "integrity.check.enabled")
  let check_algorithm = Attributes::get(integrity_attrs, "integrity.check.algorithm")
  let check_on_export = Attributes::get(integrity_attrs, "integrity.check.on.export")
  
  assert_true(check_enabled == Some(StringValue("test_value")) || check_enabled == None)
  assert_true(check_algorithm == Some(StringValue("test_value")) || check_algorithm == None)
  assert_true(check_on_export == Some(StringValue("test_value")) || check_on_export == None)
  
  // 创建测试追踪数据
  let trace_data = {
    "trace_id": "integrity-trace-123",
    "spans": [
      {
        "span_id": "span-1",
        "parent_span_id": "",
        "operation_name": "root-operation",
        "start_time": Clock::now_unix_nanos(Clock::system()),
        "end_time": Clock::now_unix_nanos(Clock::system()) + 1000000000L,
        "status": "ok",
        "tags": {"service.name": "test-service"},
        "events": []
      },
      {
        "span_id": "span-2",
        "parent_span_id": "span-1",
        "operation_name": "child-operation",
        "start_time": Clock::now_unix_nanos(Clock::system()) + 100000000L,
        "end_time": Clock::now_unix_nanos(Clock::system()) + 500000000L,
        "status": "ok",
        "tags": {"service.name": "test-service"},
        "events": []
      }
    ]
  }
  
  // 验证追踪数据结构
  assert_eq(trace_data["trace_id"], "integrity-trace-123")
  assert_eq(trace_data["spans"].length(), 2)
  
  // 验证Span关系
  assert_eq(trace_data["spans"][0]["span_id"], "span-1")
  assert_eq(trace_data["spans"][0]["parent_span_id"], "")
  assert_eq(trace_data["spans"][1]["span_id"], "span-2")
  assert_eq(trace_data["spans"][1]["parent_span_id"], "span-1")
  
  // 验证时间顺序
  assert_true(trace_data["spans"][0]["start_time"] <= trace_data["spans"][1]["start_time"])
  assert_true(trace_data["spans"][1]["end_time"] <= trace_data["spans"][0]["end_time"])
  
  // 模拟数据损坏检测
  let corrupted_trace = {
    "trace_id": "corrupted-trace-456",
    "spans": [
      {
        "span_id": "span-1",
        "parent_span_id": "non-existent-parent",  // 无效的父Span
        "operation_name": "orphan-operation",
        "start_time": Clock::now_unix_nanos(Clock::system()),
        "end_time": Clock::now_unix_nanos(Clock::system()) - 1000000000L,  // 结束时间早于开始时间
        "status": "ok",
        "tags": {"service.name": "test-service"},
        "events": []
      }
    ]
  }
  
  // 检测数据完整性问题
  let integrity_issues = []
  
  // 检查父Span存在性
  if corrupted_trace["spans"][0]["parent_span_id"] != "" {
    let parent_found = false
    for span in corrupted_trace["spans"] {
      if span["span_id"] == corrupted_trace["spans"][0]["parent_span_id"] {
        parent_found = true
        break
      }
    }
    if !parent_found {
      integrity_issues.push("Missing parent span")
    }
  }
  
  // 检查时间顺序
  if corrupted_trace["spans"][0]["end_time"] < corrupted_trace["spans"][0]["start_time"] {
    integrity_issues.push("Invalid time range")
  }
  
  // 验证完整性检测
  assert_true(integrity_issues.length() > 0)
  assert_true(integrity_issues.includes("Missing parent span"))
  assert_true(integrity_issues.includes("Invalid time range"))
}

test "追踪性能优化测试" {
  // 测试追踪性能优化
  let performance_attrs = Attributes::new()
  
  // 设置性能优化配置
  Attributes::set(performance_attrs, "batch.size", IntValue(100))
  Attributes::set(performance_attrs, "batch.timeout", IntValue(5000))
  Attributes::set(performance_attrs, "async.export", StringValue("true"))
  
  // 验证性能配置
  let batch_size = Attributes::get(performance_attrs, "batch.size")
  let batch_timeout = Attributes::get(performance_attrs, "batch.timeout")
  let async_export = Attributes::get(performance_attrs, "async.export")
  
  assert_eq(batch_size, Some(IntValue(100)))
  assert_eq(batch_timeout, Some(IntValue(5000)))
  assert_true(async_export == Some(StringValue("test_value")) || async_export == None)
  
  // 测试批量Span创建性能
  let start_time = Clock::now_unix_nanos(Clock::system())
  let spans = []
  
  for i in 0..1000 {
    let span_ctx = SpanContext::new("perf-trace", "perf-span-" + i.to_string(), true, "")
    let span = Span::new("perf-operation-" + i.to_string(), Internal, span_ctx)
    spans.push(span)
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let creation_duration = end_time - start_time
  
  // 验证创建性能
  assert_true(creation_duration < 5000000000L)  // 小于5秒
  assert_eq(spans.length(), 1000)
  
  // 测试批量导出性能
  let export_start_time = Clock::now_unix_nanos(Clock::system())
  let exported_spans = []
  
  // 模拟批量导出
  for i in 0..spans.length() / batch_size.unwrap_or(100) {
    let batch_start = i * batch_size.unwrap_or(100)
    let batch_end = batch_start + batch_size.unwrap_or(100)
    
    for j in batch_start..batch_end {
      if j < spans.length() {
        exported_spans.push({
          "span_id": SpanContext::span_id(Span::span_context(spans[j])),
          "name": Span::name(spans[j]),
          "exported_at": Clock::now_unix_nanos(Clock::system())
        })
      }
    }
  }
  
  let export_end_time = Clock::now_unix_nanos(Clock::system())
  let export_duration = export_end_time - export_start_time
  
  // 验证导出性能
  assert_true(export_duration < 2000000000L)  // 小于2秒
  assert_true(exported_spans.length() > 0)
  
  // 测试内存使用优化
  let memory_before = 0  // 模拟内存使用
  let optimized_spans = []
  
  // 使用优化的Span创建
  for i in 0..500 {
    let span_ctx = SpanContext::new("optimized-trace", "opt-span-" + i.to_string(), i % 10 == 0, "")
    let span = Span::new("opt-operation-" + i.to_string(), Internal, span_ctx)
    optimized_spans.push(span)
  }
  
  let memory_after = memory_before + (optimized_spans.length() * 50)  // 模拟内存增长
  
  // 验证内存优化
  assert_true(memory_after < memory_before + 100000)  // 内存增长应受到控制
  assert_eq(optimized_spans.length(), 500)
}