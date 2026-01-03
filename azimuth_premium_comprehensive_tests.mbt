// Azimuth Premium Comprehensive Test Suite
// 高质量综合测试用例，覆盖遥测系统的核心功能和高级特性

// 测试1: 遥测数据完整性和一致性
test "telemetry data integrity and consistency validation" {
  // 创建Span上下文
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "1234567890abcdef"
  let span_ctx = SpanContext({
    trace_id: trace_id,
    span_id: span_id,
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  // 创建Span并验证属性
  let span = Span({
    name: "http.request",
    kind: Server,
    recording: true,
    span_context: span_ctx,
    parent_span_id: Some("parent1234567890"),
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("https://api.example.com/users")),
      ("http.status_code", IntValue(200))
    ],
    events: [],
    links: [],
    status: Unset,
    start_time: 1704067200000000000L,
    end_time: None,
    duration: None
  })
  
  // 验证Span数据完整性
  assert_eq(span.name, "http.request")
  assert_eq(span.span_context.trace_id, trace_id)
  assert_eq(span.span_context.span_id, span_id)
  assert_true(span.span_context.sampled)
  
  // 验证Span属性
  let mut found_method = false
  let mut found_url = false
  let mut found_status = false
  
  for (key, value) in span.attributes {
    match key {
      "http.method" => {
        match value {
          StringValue(method) => {
            assert_eq(method, "GET")
            found_method = true
          }
          _ => assert_true(false)
        }
      }
      "http.url" => {
        match value {
          StringValue(url) => {
            assert_eq(url, "https://api.example.com/users")
            found_url = true
          }
          _ => assert_true(false)
        }
      }
      "http.status_code" => {
        match value {
          IntValue(status) => {
            assert_eq(status, 200)
            found_status = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false) // 意外的键
    }
  }
  
  assert_true(found_method)
  assert_true(found_url)
  assert_true(found_status)
}

// 测试2: 资源属性管理和合并
test "resource attributes management and merging strategies" {
  // 创建基础资源
  let base_resource = Resource({
    attributes: [
      ("service.name", StringValue("azimuth-service")),
      ("service.version", StringValue("1.2.3")),
      ("deployment.environment", StringValue("staging")),
      ("host.name", StringValue("server-01"))
    ],
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  // 创建覆盖资源
  let override_resource = Resource({
    attributes: [
      ("service.name", StringValue("azimuth-service-prod")),
      ("host.name", StringValue("prod-server-01")),
      ("deployment.environment", StringValue("production")),
      ("service.instance.id", StringValue("instance-12345"))
    ],
    schema_url: Some("https://opentelemetry.io/schemas/1.21.0")
  })
  
  // 合并资源
  let merged_resource = Resource({
    attributes: [
      // 来自override_resource的属性（优先级更高）
      ("service.name", StringValue("azimuth-service-prod")),
      ("deployment.environment", StringValue("production")),
      ("host.name", StringValue("prod-server-01")),
      ("service.instance.id", StringValue("instance-12345")),
      // 来自base_resource的独有属性
      ("service.version", StringValue("1.2.3"))
    ],
    schema_url: Some("https://opentelemetry.io/schemas/1.21.0") // 使用更新的schema
  })
  
  // 验证合并结果
  let mut found_service_name = false
  let mut found_service_version = false
  let mut found_deployment_env = false
  let mut found_host_name = false
  let mut found_instance_id = false
  
  for (key, value) in merged_resource.attributes {
    match key {
      "service.name" => {
        match value {
          StringValue(name) => {
            assert_eq(name, "azimuth-service-prod") // 应该是覆盖的值
            found_service_name = true
          }
          _ => assert_true(false)
        }
      }
      "service.version" => {
        match value {
          StringValue(version) => {
            assert_eq(version, "1.2.3") // 来自基础资源
            found_service_version = true
          }
          _ => assert_true(false)
        }
      }
      "deployment.environment" => {
        match value {
          StringValue(env) => {
            assert_eq(env, "production") // 应该是覆盖的值
            found_deployment_env = true
          }
          _ => assert_true(false)
        }
      }
      "host.name" => {
        match value {
          StringValue(host) => {
            assert_eq(host, "prod-server-01") // 应该是覆盖的值
            found_host_name = true
          }
          _ => assert_true(false)
        }
      }
      "service.instance.id" => {
        match value {
          StringValue(instance) => {
            assert_eq(instance, "instance-12345") // 来自覆盖资源
            found_instance_id = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false) // 意外的键
    }
  }
  
  assert_true(found_service_name)
  assert_true(found_service_version)
  assert_true(found_deployment_env)
  assert_true(found_host_name)
  assert_true(found_instance_id)
  
  // 验证schema URL
  match merged_resource.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.21.0")
    None => assert_true(false)
  }
}

// 测试3: 度量指标聚合和分析
test "metrics aggregation and analysis operations" {
  // 创建不同类型的度量指标
  let counter_metric = Counter({
    name: "http.requests.total",
    description: Some("Total number of HTTP requests"),
    unit: Some("requests"),
    data: [
      ("GET", 100),
      ("POST", 50),
      ("PUT", 25),
      ("DELETE", 10)
    ]
  })
  
  let histogram_metric = Histogram({
    name: "http.request.duration",
    description: Some("HTTP request duration distribution"),
    unit: Some("ms"),
    buckets: [
      (0.0, 10.0, 50),    // 0-10ms: 50 requests
      (10.0, 50.0, 100),  // 10-50ms: 100 requests
      (50.0, 100.0, 75),  // 50-100ms: 75 requests
      (100.0, 500.0, 25), // 100-500ms: 25 requests
      (500.0, Infinity, 5) // >500ms: 5 requests
    ],
    sum: 12500.0, // 总持续时间
    count: 255    // 总请求数
  })
  
  let gauge_metric = Gauge({
    name: "system.memory.usage",
    description: Some("Current memory usage in bytes"),
    unit: Some("bytes"),
    value: 1073741824 // 1GB
  })
  
  // 验证Counter指标聚合
  let mut total_requests = 0
  for (method, count) in counter_metric.data {
    total_requests = total_requests + count
  }
  assert_eq(total_requests, 185)
  
  // 验证Histogram指标统计
  assert_eq(histogram_metric.count, 255)
  assert_eq(histogram_metric.sum, 12500.0)
  
  // 计算平均请求持续时间
  let avg_duration = histogram_metric.sum / histogram_metric.count.to_double()
  assert_eq(avg_duration, 49.019607843137254)
  
  // 验证Gauge指标
  assert_eq(gauge_metric.value, 1073741824)
  
  // 创建复合度量指标集合
  let metrics_collection = MetricsCollection({
    metrics: [counter_metric, histogram_metric, gauge_metric],
    resource: Resource({
      attributes: [
        ("service.name", StringValue("azimuth-api")),
        ("service.version", StringValue("2.0.0"))
      ],
      schema_url: None
    }),
    scope: InstrumentationScope({
      name: "azimuth.instrumentation",
      version: Some("1.0.0"),
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    })
  })
  
  // 验证度量指标集合
  assert_eq(metrics_collection.metrics.length(), 3)
  assert_eq(metrics_collection.resource.attributes.length(), 2)
  assert_eq(metrics_collection.scope.name, "azimuth.instrumentation")
}

// 测试4: 日志记录与Span关联
test "log record correlation with span context" {
  // 创建Span上下文
  let span_ctx = SpanContext({
    trace_id: "abcdef1234567890abcdef1234567890",
    span_id: "abcdef1234567890",
    sampled: true,
    trace_state: ""
  })
  
  // 创建LogRecord
  let log_record = LogRecord({
    timestamp: 1704067200000000000L,
    observed_timestamp: Some(1704067200000000001L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("HTTP request processed successfully"),
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("/api/users")),
      ("http.status_code", IntValue(200)),
      ("user.id", StringValue("12345"))
    ],
    trace_id: Some(span_ctx.trace_id),
    span_id: Some(span_ctx.span_id),
    trace_flags: Some(1),
    resource: None,
    instrumentation_scope: None
  })
  
  // 验证LogRecord与Span的关联
  match log_record.trace_id {
    Some(trace_id) => assert_eq(trace_id, span_ctx.trace_id)
    None => assert_true(false)
  }
  
  match log_record.span_id {
    Some(span_id) => assert_eq(span_id, span_ctx.span_id)
    None => assert_true(false)
  }
  
  match log_record.severity_text {
    Some(severity) => assert_eq(severity, "INFO")
    None => assert_true(false)
  }
  
  match log_record.body {
    Some(body) => assert_eq(body, "HTTP request processed successfully")
    None => assert_true(false)
  }
  
  // 验证LogRecord属性
  let mut found_method = false
  let mut found_url = false
  let mut found_status = false
  let mut found_user_id = false
  
  for (key, value) in log_record.attributes {
    match key {
      "http.method" => {
        match value {
          StringValue(method) => {
            assert_eq(method, "GET")
            found_method = true
          }
          _ => assert_true(false)
        }
      }
      "http.url" => {
        match value {
          StringValue(url) => {
            assert_eq(url, "/api/users")
            found_url = true
          }
          _ => assert_true(false)
        }
      }
      "http.status_code" => {
        match value {
          IntValue(status) => {
            assert_eq(status, 200)
            found_status = true
          }
          _ => assert_true(false)
        }
      }
      "user.id" => {
        match value {
          StringValue(user_id) => {
            assert_eq(user_id, "12345")
            found_user_id = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false) // 意外的键
    }
  }
  
  assert_true(found_method)
  assert_true(found_url)
  assert_true(found_status)
  assert_true(found_user_id)
}

// 测试5: 上下文传播和Baggage管理
test "context propagation and baggage management" {
  // 创建Baggage
  let baggage = Baggage({
    entries: [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("session.id", "sess-abcdef"),
      ("trace.origin", "frontend")
    ]
  })
  
  // 创建上下文
  let context = Context({
    values: [
      ("correlation.id", "corr-12345"),
      ("baggage", baggage),
      ("span_context", SpanContext({
        trace_id: "1234567890abcdef1234567890abcdef",
        span_id: "1234567890abcdef",
        sampled: true,
        trace_state: "key1=value1,key2=value2"
      }))
    ]
  })
  
  // 验证Baggage条目
  assert_eq(baggage.entries.length(), 4)
  
  let mut found_user_id = false
  let mut found_request_id = false
  let mut found_session_id = false
  let mut found_trace_origin = false
  
  for (key, value) in baggage.entries {
    match key {
      "user.id" => {
        assert_eq(value, "12345")
        found_user_id = true
      }
      "request.id" => {
        assert_eq(value, "req-67890")
        found_request_id = true
      }
      "session.id" => {
        assert_eq(value, "sess-abcdef")
        found_session_id = true
      }
      "trace.origin" => {
        assert_eq(value, "frontend")
        found_trace_origin = true
      }
      _ => assert_true(false) // 意外的键
    }
  }
  
  assert_true(found_user_id)
  assert_true(found_request_id)
  assert_true(found_session_id)
  assert_true(found_trace_origin)
  
  // 验证上下文值
  assert_eq(context.values.length(), 3)
  
  // 创建TextMapCarrier用于传播
  let carrier = TextMapCarrier({
    headers: [
      ("traceparent", "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01"),
      ("tracestate", "key1=value1,key2=value2"),
      ("baggage", "user.id=12345,request.id=req-67890,session.id=sess-abcdef,trace.origin=frontend"),
      ("x-correlation-id", "corr-12345")
    ]
  })
  
  // 验证传播载体
  assert_eq(carrier.headers.length(), 4)
  
  let mut found_traceparent = false
  let mut found_tracestate = false
  let mut found_baggage_header = false
  let mut found_correlation_id = false
  
  for (key, value) in carrier.headers {
    match key {
      "traceparent" => {
        assert_eq(value, "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01")
        found_traceparent = true
      }
      "tracestate" => {
        assert_eq(value, "key1=value1,key2=value2")
        found_tracestate = true
      }
      "baggage" => {
        assert_eq(value, "user.id=12345,request.id=req-67890,session.id=sess-abcdef,trace.origin=frontend")
        found_baggage_header = true
      }
      "x-correlation-id" => {
        assert_eq(value, "corr-12345")
        found_correlation_id = true
      }
      _ => assert_true(false) // 意外的键
    }
  }
  
  assert_true(found_traceparent)
  assert_true(found_tracestate)
  assert_true(found_baggage_header)
  assert_true(found_correlation_id)
}

// 测试6: 仪器化范围和版本管理
test "instrumentation scope and version management" {
  // 创建完整的仪器化范围
  let full_scope = InstrumentationScope({
    name: "azimuth.http.instrumentation",
    version: Some("1.2.3"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0"),
    attributes: [
      ("language", StringValue("moonbit")),
      ("framework", StringValue("azimuth")),
      ("runtime", StringValue("wasm-gc"))
    ]
  })
  
  // 创建最小仪器化范围
  let minimal_scope = InstrumentationScope({
    name: "minimal.instrumentation",
    version: None,
    schema_url: None,
    attributes: []
  })
  
  // 验证完整范围
  assert_eq(full_scope.name, "azimuth.http.instrumentation")
  match full_scope.version {
    Some(version) => assert_eq(version, "1.2.3")
    None => assert_true(false)
  }
  match full_scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    None => assert_true(false)
  }
  assert_eq(full_scope.attributes.length(), 3)
  
  // 验证最小范围
  assert_eq(minimal_scope.name, "minimal.instrumentation")
  match minimal_scope.version {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  match minimal_scope.schema_url {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  assert_eq(minimal_scope.attributes.length(), 0)
  
  // 验证完整范围的属性
  let mut found_language = false
  let mut found_framework = false
  let mut found_runtime = false
  
  for (key, value) in full_scope.attributes {
    match key {
      "language" => {
        match value {
          StringValue(lang) => {
            assert_eq(lang, "moonbit")
            found_language = true
          }
          _ => assert_true(false)
        }
      }
      "framework" => {
        match value {
          StringValue(fw) => {
            assert_eq(fw, "azimuth")
            found_framework = true
          }
          _ => assert_true(false)
        }
      }
      "runtime" => {
        match value {
          StringValue(rt) => {
            assert_eq(rt, "wasm-gc")
            found_runtime = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false) // 意外的键
    }
  }
  
  assert_true(found_language)
  assert_true(found_framework)
  assert_true(found_runtime)
}

// 测试7: Span链接和事件管理
test "span links and events management" {
  // 创建Span上下文
  let parent_ctx = SpanContext({
    trace_id: "parent1234567890abcdef1234567890",
    span_id: "parent1234567890",
    sampled: true,
    trace_state: ""
  })
  
  let linked_ctx = SpanContext({
    trace_id: "linked1234567890abcdef1234567890",
    span_id: "linked1234567890",
    sampled: false,
    trace_state: ""
  })
  
  // 创建Span事件
  let events = [
    Event({
      name: "http.request.started",
      timestamp: 1704067200000000000L,
      attributes: [
        ("http.method", StringValue("GET")),
        ("http.url", StringValue("/api/users"))
      ]
    }),
    Event({
      name: "http.request.completed",
      timestamp: 1704067200000000001L,
      attributes: [
        ("http.status_code", IntValue(200)),
        ("response.size", IntValue(1024))
      ]
    })
  ]
  
  // 创建Span链接
  let links = [
    Link({
      linked_span_context: linked_ctx,
      attributes: [
        ("link.type", StringValue("follows_from")),
        ("relationship", StringValue("causal"))
      ]
    })
  ]
  
  // 创建Span
  let span = Span({
    name: "http.request.processing",
    kind: Server,
    recording: true,
    span_context: parent_ctx,
    parent_span_id: Some("root1234567890"),
    attributes: [
      ("service.name", StringValue("azimuth-api")),
      ("operation.name", StringValue("process_request"))
    ],
    events: events,
    links: links,
    status: Ok,
    start_time: 1704067200000000000L,
    end_time: Some(1704067200000000002L),
    duration: Some(2000000L) // 2ms
  })
  
  // 验证Span事件
  assert_eq(span.events.length(), 2)
  
  let first_event = span.events[0]
  assert_eq(first_event.name, "http.request.started")
  assert_eq(first_event.timestamp, 1704067200000000000L)
  assert_eq(first_event.attributes.length(), 2)
  
  let second_event = span.events[1]
  assert_eq(second_event.name, "http.request.completed")
  assert_eq(second_event.timestamp, 1704067200000000001L)
  assert_eq(second_event.attributes.length(), 2)
  
  // 验证Span链接
  assert_eq(span.links.length(), 1)
  
  let link = span.links[0]
  assert_eq(link.linked_span_context.trace_id, linked_ctx.trace_id)
  assert_eq(link.linked_span_context.span_id, linked_ctx.span_id)
  assert_eq(link.attributes.length(), 2)
  
  // 验证Span持续时间
  match span.duration {
    Some(duration) => assert_eq(duration, 2000000L)
    None => assert_true(false)
  }
  
  // 验证Span状态
  match span.status {
    Ok => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试8: 异常处理和错误状态管理
test "exception handling and error status management" {
  // 创建错误Span上下文
  let error_ctx = SpanContext({
    trace_id: "error1234567890abcdef1234567890",
    span_id: "error1234567890",
    sampled: true,
    trace_state: ""
  })
  
  // 创建异常事件
  let exception_event = Event({
    name: "exception",
    timestamp: 1704067200000000000L,
    attributes: [
      ("exception.type", StringValue("TimeoutException")),
      ("exception.message", StringValue("Request timed out after 30 seconds")),
      ("exception.stacktrace", StringValue("at process_request (line 42)\n  at handle_request (line 128)")),
      ("exception.escaped", BoolValue(true))
    ]
  })
  
  // 创建错误Span
  let error_span = Span({
    name: "http.request.error",
    kind: Server,
    recording: true,
    span_context: error_ctx,
    parent_span_id: Some("parent1234567890"),
    attributes: [
      ("service.name", StringValue("azimuth-api")),
      ("error.type", StringValue("timeout")),
      ("error.retry_count", IntValue(3))
    ],
    events: [exception_event],
    links: [],
    status: Error,
    start_time: 1704067200000000000L,
    end_time: Some(1704067200000000000L),
    duration: Some(0L)
  })
  
  // 验证错误Span
  assert_eq(error_span.name, "http.request.error")
  assert_eq(error_span.events.length(), 1)
  
  // 验证异常事件
  let exception = error_span.events[0]
  assert_eq(exception.name, "exception")
  assert_eq(exception.attributes.length(), 4)
  
  // 验证异常属性
  let mut found_exception_type = false
  let mut found_exception_message = false
  let mut found_stacktrace = false
  let mut found_escaped = false
  
  for (key, value) in exception.attributes {
    match key {
      "exception.type" => {
        match value {
          StringValue(exc_type) => {
            assert_eq(exc_type, "TimeoutException")
            found_exception_type = true
          }
          _ => assert_true(false)
        }
      }
      "exception.message" => {
        match value {
          StringValue(message) => {
            assert_eq(message, "Request timed out after 30 seconds")
            found_exception_message = true
          }
          _ => assert_true(false)
        }
      }
      "exception.stacktrace" => {
        match value {
          StringValue(stacktrace) => {
            assert_true(stacktrace.contains("process_request"))
            assert_true(stacktrace.contains("handle_request"))
            found_stacktrace = true
          }
          _ => assert_true(false)
        }
      }
      "exception.escaped" => {
        match value {
          BoolValue(escaped) => {
            assert_eq(escaped, true)
            found_escaped = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false) // 意外的键
    }
  }
  
  assert_true(found_exception_type)
  assert_true(found_exception_message)
  assert_true(found_stacktrace)
  assert_true(found_escaped)
  
  // 验证错误状态
  match error_span.status {
    Error => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证错误属性
  let mut found_error_type = false
  let mut found_retry_count = false
  
  for (key, value) in error_span.attributes {
    match key {
      "error.type" => {
        match value {
          StringValue(err_type) => {
            assert_eq(err_type, "timeout")
            found_error_type = true
          }
          _ => assert_true(false)
        }
      }
      "error.retry_count" => {
        match value {
          IntValue(retry_count) => {
            assert_eq(retry_count, 3)
            found_retry_count = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false) // 意外的键
    }
  }
  
  assert_true(found_error_type)
  assert_true(found_retry_count)
}