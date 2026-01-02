// Azimuth 新型综合测试套件
// 包含多种功能领域的测试用例

// 测试1: 基本属性和资源管理
test "basic attributes and resource management" {
  // 创建属性集合
  let attributes = @azimuth.Attributes {
    values : [
      ("service.name", @azimuth.StringValue("test-service")),
      ("service.version", @azimuth.StringValue("1.0.0")),
      ("environment", @azimuth.StringValue("test")),
      ("request.count", @azimuth.IntValue(100)),
      ("cpu.usage", @azimuth.FloatValue(0.75)),
      ("enabled", @azimuth.BoolValue(true)),
      ("tags", @azimuth.ArrayStringValue(["tag1", "tag2", "tag3"])),
      ("ports", @azimuth.ArrayIntValue([8080, 9090]))
    ]
  }
  
  // 验证属性数量
  assert_eq(attributes.values.length(), 8)
  
  // 验证特定属性值
  let service_name = attributes.values.filter(fn(attr) { attr.0 == "service.name" })
  assert_eq(service_name.length(), 1)
  match service_name[0].1 {
    @azimuth.StringValue(name) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  // 验证整数属性
  let request_count = attributes.values.filter(fn(attr) { attr.0 == "request.count" })
  assert_eq(request_count.length(), 1)
  match request_count[0].1 {
    @azimuth.IntValue(count) => assert_eq(count, 100)
    _ => assert_true(false)
  }
  
  // 验证浮点属性
  let cpu_usage = attributes.values.filter(fn(attr) { attr.0 == "cpu.usage" })
  assert_eq(cpu_usage.length(), 1)
  match cpu_usage[0].1 {
    @azimuth.FloatValue(usage) => assert_eq(usage, 0.75)
    _ => assert_true(false)
  }
  
  // 验证布尔属性
  let enabled = attributes.values.filter(fn(attr) { attr.0 == "enabled" })
  assert_eq(enabled.length(), 1)
  match enabled[0].1 {
    @azimuth.BoolValue(is_enabled) => assert_eq(is_enabled, true)
    _ => assert_true(false)
  }
  
  // 验证数组属性
  let tags = attributes.values.filter(fn(attr) { attr.0 == "tags" })
  assert_eq(tags.length(), 1)
  match tags[0].1 {
    @azimuth.ArrayStringValue(tag_array) => {
      assert_eq(tag_array.length(), 3)
      assert_eq(tag_array[0], "tag1")
      assert_eq(tag_array[2], "tag3")
    }
    _ => assert_true(false)
  }
  
  // 创建资源
  let resource = @azimuth.Resource {
    attributes : [
      ("service.name", @azimuth.StringValue("api-service")),
      ("service.instance.id", @azimuth.StringValue("instance-123")),
      ("deployment.environment", @azimuth.StringValue("production")),
      ("host.name", @azimuth.StringValue("prod-server-01")),
      ("os.type", @azimuth.StringValue("linux")),
      ("os.version", @azimuth.StringValue("5.4.0")),
      ("memory.total", @azimuth.IntValue(8589934592)), // 8GB
      ("cpu.count", @azimuth.IntValue(4))
    ]
  }
  
  // 验证资源属性
  assert_eq(resource.attributes.length(), 8)
  
  let service_name_attr = resource.attributes.filter(fn(attr) { attr.0 == "service.name" })
  assert_eq(service_name_attr.length(), 1)
  match service_name_attr[0].1 {
    @azimuth.StringValue(name) => assert_eq(name, "api-service")
    _ => assert_true(false)
  }
  
  let memory_total = resource.attributes.filter(fn(attr) { attr.0 == "memory.total" })
  assert_eq(memory_total.length(), 1)
  match memory_total[0].1 {
    @azimuth.IntValue(memory) => assert_eq(memory, 8589934592)
    _ => assert_true(false)
  }
}

// 测试2: 度量仪器和操作
test "metric instruments and operations" {
  // 创建计数器
  let request_counter = @azimuth.Counter {
    name : "http.requests.total",
    description : Some("Total HTTP requests"),
    unit : Some("requests")
  }
  
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  // 创建直方图
  let response_histogram = @azimuth.Histogram {
    name : "http.request.duration",
    description : Some("HTTP request duration"),
    unit : Some("ms")
  }
  
  assert_eq(response_histogram.name, "http.request.duration")
  assert_eq(response_histogram.description, Some("HTTP request duration"))
  assert_eq(response_histogram.unit, Some("ms"))
  
  // 创建仪表
  let memory_gauge = @azimuth.Gauge {
    name : "memory.usage",
    description : Some("Memory usage"),
    unit : Some("bytes")
  }
  
  assert_eq(memory_gauge.name, "memory.usage")
  assert_eq(memory_gauge.description, Some("Memory usage"))
  assert_eq(memory_gauge.unit, Some("bytes"))
  
  // 创建上下开关计数器
  let active_connections = @azimuth.UpDownCounter {
    name : "active.connections",
    description : Some("Active connections"),
    unit : Some("connections")
  }
  
  assert_eq(active_connections.name, "active.connections")
  assert_eq(active_connections.description, Some("Active connections"))
  assert_eq(active_connections.unit, Some("connections"))
  
  // 测试仪器类型枚举
  let counter_instrument = @azimuth.Instrument::Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_instrument = @azimuth.Instrument::Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let gauge_instrument = @azimuth.Instrument::Gauge("test.gauge", Some("Test gauge"), Some("value"))
  let updown_counter_instrument = @azimuth.Instrument::UpDownCounter("test.updown", Some("Test updown"), Some("items"))
  
  // 验证仪器类型
  match counter_instrument {
    @azimuth.Instrument::Counter(name, desc, unit) => {
      assert_eq(name, "test.counter")
      assert_eq(desc, Some("Test counter"))
      assert_eq(unit, Some("count"))
    }
    _ => assert_true(false)
  }
  
  match histogram_instrument {
    @azimuth.Instrument::Histogram(name, desc, unit) => {
      assert_eq(name, "test.histogram")
      assert_eq(desc, Some("Test histogram"))
      assert_eq(unit, Some("ms"))
    }
    _ => assert_true(false)
  }
  
  match gauge_instrument {
    @azimuth.Instrument::Gauge(name, desc, unit) => {
      assert_eq(name, "test.gauge")
      assert_eq(desc, Some("Test gauge"))
      assert_eq(unit, Some("value"))
    }
    _ => assert_true(false)
  }
  
  match updown_counter_instrument {
    @azimuth.Instrument::UpDownCounter(name, desc, unit) => {
      assert_eq(name, "test.updown")
      assert_eq(desc, Some("Test updown"))
      assert_eq(unit, Some("items"))
    }
    _ => assert_true(false)
  }
}

// 测试3: 上下文和baggage管理
test "context and baggage management" {
  // 创建基本上下文
  let context = @azimuth.Context {
    data : Some(("user.id", "user-12345"))
  }
  
  // 验证上下文数据
  match context.data {
    Some((key, value)) => {
      assert_eq(key, "user.id")
      assert_eq(value, "user-12345")
    }
    None => assert_true(false)
  }
  
  // 创建上下文键
  let user_key = @azimuth.ContextKey[String] {
    key : "user.id"
  }
  
  let session_key = @azimuth.ContextKey[String] {
    key : "session.id"
  }
  
  assert_eq(user_key.key, "user.id")
  assert_eq(session_key.key, "session.id")
  
  // 创建baggage
  let baggage = @azimuth.Baggage {
    entries : [
      ("user.id", "user-12345"),
      ("request.id", "req-abcdef"),
      ("session.id", "sess-123456"),
      ("trace.sampled", "true"),
      ("client.version", "2.1.0")
    ]
  }
  
  // 验证baggage条目
  assert_eq(baggage.entries.length(), 5)
  
  // 验证特定baggage条目
  let user_entry = baggage.entries.filter(fn(entry) { entry.0 == "user.id" })
  assert_eq(user_entry.length(), 1)
  assert_eq(user_entry[0].1, "user-12345")
  
  let session_entry = baggage.entries.filter(fn(entry) { entry.0 == "session.id" })
  assert_eq(session_entry.length(), 1)
  assert_eq(session_entry[0].1, "sess-123456")
  
  // 创建span上下文
  let span_context = @azimuth.SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1234567890abcdef",
    sampled : true,
    trace_state : "sampling.decision=true"
  }
  
  // 验证span上下文
  assert_eq(span_context.trace_id, "1234567890abcdef1234567890abcdef")
  assert_eq(span_context.span_id, "1234567890abcdef")
  assert_eq(span_context.sampled, true)
  assert_eq(span_context.trace_state, "sampling.decision=true")
  
  // 创建文本映射载体
  let carrier = @azimuth.TextMapCarrier {
    headers : [
      ("traceparent", "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01"),
      ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
      ("baggage", "userId=user123,sessionId=abc123")
    ]
  }
  
  // 验证载体头部
  assert_eq(carrier.headers.length(), 3)
  
  let traceparent = carrier.headers.filter(fn(header) { header.0 == "traceparent" })
  assert_eq(traceparent.length(), 1)
  assert_eq(traceparent[0].1, "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01")
  
  let baggage_header = carrier.headers.filter(fn(header) { header.0 == "baggage" })
  assert_eq(baggage_header.length(), 1)
  assert_eq(baggage_header[0].1, "userId=user123,sessionId=abc123")
}

// 测试4: Span生命周期和状态管理
test "span lifecycle and status management" {
  // 创建span上下文
  let span_context = @azimuth.SpanContext {
    trace_id : "abcdef1234567890abcdef1234567890",
    span_id : "1234567890abcdef",
    sampled : true,
    trace_state : ""
  }
  
  // 创建span
  let span = @azimuth.Span {
    name : "http.request",
    kind : @azimuth.SpanKind::Server,
    recording : true,
    span_context : span_context
  }
  
  // 验证span属性
  assert_eq(span.name, "http.request")
  assert_eq(span.recording, true)
  
  // 验证span种类
  match span.kind {
    @azimuth.SpanKind::Server => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证span上下文
  assert_eq(span.span_context.trace_id, "abcdef1234567890abcdef1234567890")
  assert_eq(span.span_context.span_id, "1234567890abcdef")
  assert_eq(span.span_context.sampled, true)
  
  // 创建不同种类的span
  let internal_span = @azimuth.Span {
    name : "internal.processing",
    kind : @azimuth.SpanKind::Internal,
    recording : true,
    span_context : @azimuth.SpanContext {
      trace_id : "abcdef1234567890abcdef1234567890",
      span_id : "fedcba0987654321",
      sampled : true,
      trace_state : ""
    }
  }
  
  let client_span = @azimuth.Span {
    name : "external.api.call",
    kind : @azimuth.SpanKind::Client,
    recording : true,
    span_context : @azimuth.SpanContext {
      trace_id : "abcdef1234567890abcdef1234567890",
      span_id : "0123456789abcdef",
      sampled : true,
      trace_state : ""
    }
  }
  
  // 验证span种类
  match internal_span.kind {
    @azimuth.SpanKind::Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  match client_span.kind {
    @azimuth.SpanKind::Client => assert_true(true)
    _ => assert_true(false)
  }
  
  // 创建instrumentation scope
  let scope = @azimuth.InstrumentationScope {
    name : "my-service",
    version : Some("1.0.0"),
    schema_url : Some("https://example.com/schema/v1")
  }
  
  // 验证scope属性
  assert_eq(scope.name, "my-service")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, Some("https://example.com/schema/v1"))
  
  // 创建tracer
  let tracer = @azimuth.Tracer {
    scope : scope
  }
  
  // 验证tracer的scope
  assert_eq(tracer.scope.name, "my-service")
  assert_eq(tracer.scope.version, Some("1.0.0"))
  
  // 创建tracer provider
  let tracer_provider = @azimuth.TracerProvider {}
  
  // 验证tracer provider创建成功
  assert_true(true) // 简单验证结构体创建成功
}

// 测试5: 传播器和上下文注入
test "propagators and context injection" {
  // 创建W3C追踪上下文传播器
  let trace_propagator = @azimuth.W3CTraceContextPropagator {}
  
  // 创建W3C baggage传播器
  let baggage_propagator = @azimuth.W3CBaggagePropagator {}
  
  // 创建复合传播器
  let composite_propagator = @azimuth.CompositePropagator {
    propagators : [trace_propagator, baggage_propagator]
  }
  
  // 验证复合传播器
  assert_eq(composite_propagator.propagators.length(), 2)
  
  // 创建上下文
  let context = @azimuth.Context {
    data : Some(("correlation.id", "corr-12345"))
  }
  
  // 创建文本映射载体
  let carrier = @azimuth.TextMapCarrier {
    headers : []
  }
  
  // 验证初始载体为空
  assert_eq(carrier.headers.length(), 0)
  
  // 创建span上下文用于传播
  let span_context = @azimuth.SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1234567890abcdef",
    sampled : true,
    trace_state : "rojo=00f067aa0ba902b7"
  }
  
  // 创建baggage用于传播
  let baggage = @azimuth.Baggage {
    entries : [
      ("user.id", "user123"),
      ("session.id", "session456"),
      ("request.id", "req789")
    ]
  }
  
  // 验证baggage条目
  assert_eq(baggage.entries.length(), 3)
  
  // 验证特定baggage条目
  let user_entry = baggage.entries.filter(fn(entry) { entry.0 == "user.id" })
  assert_eq(user_entry.length(), 1)
  assert_eq(user_entry[0].1, "user123")
  
  // 创建带有追踪头部的载体
  let trace_carrier = @azimuth.TextMapCarrier {
    headers : [
      ("traceparent", "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01"),
      ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
      ("baggage", "userId=user123,sessionId=session456")
    ]
  }
  
  // 验证追踪头部
  assert_eq(trace_carrier.headers.length(), 3)
  
  // 验证traceparent格式
  let traceparent_header = trace_carrier.headers.filter(fn(header) { header.0 == "traceparent" })
  assert_eq(traceparent_header.length(), 1)
  let traceparent_value = traceparent_header[0].1
  assert_true(traceparent_value.length() > 0)
  assert_eq(traceparent_value.substr(0, 2), "00") // 版本
  assert_eq(traceparent_value.substr(3, 1), "-") // 分隔符
  
  // 验证tracestate
  let tracestate_header = trace_carrier.headers.filter(fn(header) { header.0 == "tracestate" })
  assert_eq(tracestate_header.length(), 1)
  let tracestate_value = tracestate_header[0].1
  assert_true(tracestate_value.contains("rojo="))
  assert_true(tracestate_value.contains("congo="))
  
  // 验证baggage头部
  let baggage_header = trace_carrier.headers.filter(fn(header) { header.0 == "baggage" })
  assert_eq(baggage_header.length(), 1)
  let baggage_value = baggage_header[0].1
  assert_true(baggage_value.contains("userId=user123"))
  assert_true(baggage_value.contains("sessionId=session456"))
}