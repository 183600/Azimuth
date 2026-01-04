// Azimuth Fundamental Telemetry Data Types Tests
// 基础遥测数据类型测试，验证核心数据结构的正确性和可靠性

// 测试1: 遥测度量类型验证
test "telemetry metric types validation" {
  // 测试计数器度量类型
  let counter_metric = azimuth::telemetry::Metric::Counter {
    name: "request_count",
    description: "Total number of requests",
    unit: "count",
    value: 1250.0,
    attributes: [
      ("service", azimuth::telemetry::AttributeValue::StringValue("api-service")),
      ("endpoint", azimuth::telemetry::AttributeValue::StringValue("/users"))
    ]
  }
  
  match counter_metric {
    azimuth::telemetry::Metric::Counter { name, value, attributes, ... } => {
      assert_eq(name, "request_count")
      assert_eq(value, 1250.0)
      assert_eq(attributes.length(), 2)
    }
    _ => assert_true(false)
  }
  
  // 测试仪表度量类型
  let gauge_metric = azimuth::telemetry::Metric::Gauge {
    name: "memory_usage",
    description: "Current memory usage",
    unit: "bytes",
    value: 5368709120.0, // 5GB
    attributes: [
      ("instance", azimuth::telemetry::AttributeValue::StringValue("server-1"))
    ]
  }
  
  match gauge_metric {
    azimuth::telemetry::Metric::Gauge { name, value, ... } => {
      assert_eq(name, "memory_usage")
      assert_eq(value, 5368709120.0)
    }
    _ => assert_true(false)
  }
  
  // 测试直方图度量类型
  let histogram_metric = azimuth::telemetry::Metric::Histogram {
    name: "response_time",
    description: "Response time distribution",
    unit: "ms",
    buckets: [
      (0.0, 10.0, 120.0),
      (10.0, 50.0, 450.0),
      (50.0, 100.0, 280.0),
      (100.0, 500.0, 130.0),
      (500.0, Infinity, 20.0)
    ],
    count: 1000.0,
    sum: 23500.0
  }
  
  match histogram_metric {
    azimuth::telemetry::Metric::Histogram { name, buckets, count, sum, ... } => {
      assert_eq(name, "response_time")
      assert_eq(buckets.length(), 5)
      assert_eq(count, 1000.0)
      assert_eq(sum, 23500.0)
    }
    _ => assert_true(false)
  }
}

// 测试2: 遥测日志记录类型验证
test "telemetry log record types validation" {
  // 测试信息级别日志记录
  let info_log = azimuth::telemetry::LogRecord {
    timestamp: 1609459200000L, // 2021-01-01 00:00:00 UTC
    severity: azimuth::telemetry::Severity::Info,
    severity_text: "INFO",
    body: Some("User login successful"),
    attributes: [
      ("user.id", azimuth::telemetry::AttributeValue::StringValue("user-12345")),
      ("ip.address", azimuth::telemetry::AttributeValue::StringValue("192.168.1.100")),
      ("success", azimuth::telemetry::AttributeValue::BoolValue(true))
    ],
    trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
    span_id: Some("00f067aa0ba902b7")
  }
  
  assert_eq(info_log.severity, azimuth::telemetry::Severity::Info)
  assert_eq(info_log.severity_text, "INFO")
  match info_log.body {
    Some(body) => assert_eq(body, "User login successful")
    None => assert_true(false)
  }
  assert_eq(info_log.attributes.length(), 3)
  
  // 测试错误级别日志记录
  let error_log = azimuth::telemetry::LogRecord {
    timestamp: 1609459260000L, // 2021-01-01 00:01:00 UTC
    severity: azimuth::telemetry::Severity::Error,
    severity_text: "ERROR",
    body: Some("Database connection failed"),
    attributes: [
      ("error.code", azimuth::telemetry::AttributeValue::IntValue(5003)),
      ("retry.count", azimuth::telemetry::AttributeValue::IntValue(3)),
      ("database.name", azimuth::telemetry::AttributeValue::StringValue("production_db"))
    ],
    trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
    span_id: Some("00f067aa0ba902b7")
  }
  
  assert_eq(error_log.severity, azimuth::telemetry::Severity::Error)
  assert_eq(error_log.severity_text, "ERROR")
  match error_log.body {
    Some(body) => assert_eq(body, "Database connection failed")
    None => assert_true(false)
  }
  
  // 验证错误日志属性
  let mut error_code_found = false
  let mut retry_count_found = false
  for (key, value) in error_log.attributes {
    match key {
      "error.code" => {
        match value {
          azimuth::telemetry::AttributeValue::IntValue(code) => {
            assert_eq(code, 5003)
            error_code_found = true
          }
          _ => assert_true(false)
        }
      }
      "retry.count" => {
        match value {
          azimuth::telemetry::AttributeValue::IntValue(count) => {
            assert_eq(count, 3)
            retry_count_found = true
          }
          _ => assert_true(false)
        }
      }
      _ => () // 其他键
    }
  }
  assert_true(error_code_found)
  assert_true(retry_count_found)
}

// 测试3: 遥测跨度类型验证
test "telemetry span types validation" {
  // 测试服务器跨度
  let server_span = azimuth::telemetry::Span {
    name: "HTTP GET /api/users",
    kind: azimuth::telemetry::SpanKind::Server,
    status: azimuth::telemetry::SpanStatus::Ok,
    start_time: 1609459200000L,
    end_time: 1609459200150L,
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("b7ad6b7169203331"),
    attributes: [
      ("http.method", azimuth::telemetry::AttributeValue::StringValue("GET")),
      ("http.url", azimuth::telemetry::AttributeValue::StringValue("/api/users")),
      ("http.status_code", azimuth::telemetry::AttributeValue::IntValue(200)),
      ("user.agent", azimuth::telemetry::AttributeValue::StringValue("Mozilla/5.0"))
    ],
    events: [
      {
        timestamp: 1609459200050L,
        name: "database.query",
        attributes: [
          ("db.statement", azimuth::telemetry::AttributeValue::StringValue("SELECT * FROM users")),
          ("db.duration", azimuth::telemetry::AttributeValue::FloatValue(25.5))
        ]
      }
    ],
    links: []
  }
  
  assert_eq(server_span.kind, azimuth::telemetry::SpanKind::Server)
  assert_eq(server_span.status, azimuth::telemetry::SpanStatus::Ok)
  assert_eq(server_span.end_time - server_span.start_time, 150L)
  assert_eq(server_span.attributes.length(), 4)
  assert_eq(server_span.events.length(), 1)
  
  // 测试客户端跨度
  let client_span = azimuth::telemetry::Span {
    name: "HTTP POST /external/api",
    kind: azimuth::telemetry::SpanKind::Client,
    status: azimuth::telemetry::SpanStatus::Error,
    start_time: 1609459201000L,
    end_time: 1609459203000L,
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "b7ad6b7169203331",
    parent_span_id: None,
    attributes: [
      ("http.method", azimuth::telemetry::AttributeValue::StringValue("POST")),
      ("http.url", azimuth::telemetry::AttributeValue::StringValue("https://external.api.com/data")),
      ("http.status_code", azimuth::telemetry::AttributeValue::IntValue(500)),
      ("error.message", azimuth::telemetry::AttributeValue::StringValue("Internal server error"))
    ],
    events: [
      {
        timestamp: 1609459201500L,
        name: "connection.timeout",
        attributes: [
          ("timeout.duration", azimuth::telemetry::AttributeValue::IntValue(5000))
        ]
      },
      {
        timestamp: 1609459202500L,
        name: "retry.attempt",
        attributes: [
          ("retry.count", azimuth::telemetry::AttributeValue::IntValue(2))
        ]
      }
    ],
    links: []
  }
  
  assert_eq(client_span.kind, azimuth::telemetry::SpanKind::Client)
  assert_eq(client_span.status, azimuth::telemetry::SpanStatus::Error)
  assert_eq(client_span.end_time - client_span.start_time, 2000L)
  assert_eq(client_span.attributes.length(), 4)
  assert_eq(client_span.events.length(), 2)
}

// 测试4: 遥测资源类型验证
test "telemetry resource types validation" {
  // 测试服务资源
  let service_resource = azimuth::telemetry::Resource {
    attributes: [
      ("service.name", azimuth::telemetry::AttributeValue::StringValue("payment-service")),
      ("service.version", azimuth::telemetry::AttributeValue::StringValue("2.3.1")),
      ("service.instance.id", azimuth::telemetry::AttributeValue::StringValue("instance-abc123")),
      ("service.namespace", azimuth::telemetry::AttributeValue::StringValue("production")),
      ("deployment.environment", azimuth::telemetry::AttributeValue::StringValue("prod"))
    ]
  }
  
  // 验证服务资源属性
  let mut service_name_found = false
  let mut service_version_found = false
  let mut service_instance_found = false
  
  for (key, value) in service_resource.attributes {
    match key {
      "service.name" => {
        match value {
          azimuth::telemetry::AttributeValue::StringValue(name) => {
            assert_eq(name, "payment-service")
            service_name_found = true
          }
          _ => assert_true(false)
        }
      }
      "service.version" => {
        match value {
          azimuth::telemetry::AttributeValue::StringValue(version) => {
            assert_eq(version, "2.3.1")
            service_version_found = true
          }
          _ => assert_true(false)
        }
      }
      "service.instance.id" => {
        match value {
          azimuth::telemetry::AttributeValue::StringValue(instance) => {
            assert_eq(instance, "instance-abc123")
            service_instance_found = true
          }
          _ => assert_true(false)
        }
      }
      _ => () // 其他键
    }
  }
  
  assert_true(service_name_found)
  assert_true(service_version_found)
  assert_true(service_instance_found)
  
  // 测试主机资源
  let host_resource = azimuth::telemetry::Resource {
    attributes: [
      ("host.name", azimuth::telemetry::AttributeValue::StringValue("web-server-01")),
      ("host.type", azimuth::telemetry::AttributeValue::StringValue("virtual")),
      ("host.arch", azimuth::telemetry::AttributeValue::StringValue("amd64")),
      ("host.os.type", azimuth::telemetry::AttributeValue::StringValue("linux")),
      ("host.os.version", azimuth::telemetry::AttributeValue::StringValue("5.4.0-74-generic")),
      ("host.cpu.count", azimuth::telemetry::AttributeValue::IntValue(8)),
      ("host.memory.available", azimuth::telemetry::AttributeValue::IntValue(16777216000)) // 16GB
    ]
  }
  
  // 验证主机资源属性
  let mut host_name_found = false
  let mut cpu_count_found = false
  let mut memory_available_found = false
  
  for (key, value) in host_resource.attributes {
    match key {
      "host.name" => {
        match value {
          azimuth::telemetry::AttributeValue::StringValue(name) => {
            assert_eq(name, "web-server-01")
            host_name_found = true
          }
          _ => assert_true(false)
        }
      }
      "host.cpu.count" => {
        match value {
          azimuth::telemetry::AttributeValue::IntValue(count) => {
            assert_eq(count, 8)
            cpu_count_found = true
          }
          _ => assert_true(false)
        }
      }
      "host.memory.available" => {
        match value {
          azimuth::telemetry::AttributeValue::IntValue(memory) => {
            assert_eq(memory, 16777216000)
            memory_available_found = true
          }
          _ => assert_true(false)
        }
      }
      _ => () // 其他键
    }
  }
  
  assert_true(host_name_found)
  assert_true(cpu_count_found)
  assert_true(memory_available_found)
}

// 测试5: 遥测仪器类型验证
test "telemetry instrument types validation" {
  // 测试计数器仪器
  let counter_instrument = azimuth::telemetry::Instrument::Counter {
    name: "http_requests_total",
    description: "Total number of HTTP requests",
    unit: "requests",
    meter_name: "http-server",
    attributes: [
      ("method", azimuth::telemetry::AttributeValue::StringValue("GET")),
      ("status", azimuth::telemetry::AttributeValue::StringValue("200"))
    ]
  }
  
  match counter_instrument {
    azimuth::telemetry::Instrument::Counter { name, description, unit, meter_name, attributes } => {
      assert_eq(name, "http_requests_total")
      assert_eq(description, "Total number of HTTP requests")
      assert_eq(unit, "requests")
      assert_eq(meter_name, "http-server")
      assert_eq(attributes.length(), 2)
    }
    _ => assert_true(false)
  }
  
  // 测试直方图仪器
  let histogram_instrument = azimuth::telemetry::Instrument::Histogram {
    name: "http_request_duration_seconds",
    description: "HTTP request duration in seconds",
    unit: "seconds",
    meter_name: "http-server",
    boundaries: [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0],
    attributes: [
      ("method", azimuth::telemetry::AttributeValue::StringValue("POST")),
      ("endpoint", azimuth::telemetry::AttributeValue::StringValue("/api/orders"))
    ]
  }
  
  match histogram_instrument {
    azimuth::telemetry::Instrument::Histogram { name, boundaries, attributes, ... } => {
      assert_eq(name, "http_request_duration_seconds")
      assert_eq(boundaries.length(), 11)
      assert_eq(attributes.length(), 2)
    }
    _ => assert_true(false)
  }
  
  // 测试仪表仪器
  let gauge_instrument = azimuth::telemetry::Instrument::Gauge {
    name: "system_memory_usage_bytes",
    description: "Current system memory usage in bytes",
    unit: "bytes",
    meter_name: "system-monitor",
    attributes: [
      ("node", azimuth::telemetry::AttributeValue::StringValue("worker-1")),
      ("region", azimuth::telemetry::AttributeValue::StringValue("us-west-2"))
    ]
  }
  
  match gauge_instrument {
    azimuth::telemetry::Instrument::Gauge { name, description, unit, meter_name, attributes } => {
      assert_eq(name, "system_memory_usage_bytes")
      assert_eq(description, "Current system memory usage in bytes")
      assert_eq(unit, "bytes")
      assert_eq(meter_name, "system-monitor")
      assert_eq(attributes.length(), 2)
    }
    _ => assert_true(false)
  }
}

// 测试6: 遥测上下文类型验证
test "telemetry context types validation" {
  // 测试追踪上下文
  let trace_context = azimuth::telemetry::TraceContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    trace_flags: 1, // 采样标志
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",
    is_remote: false
  }
  
  assert_eq(trace_context.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(trace_context.span_id, "00f067aa0ba902b7")
  assert_eq(trace_context.trace_flags, 1)
  assert_eq(trace_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  assert_false(trace_context.is_remote)
  
  // 测试 baggage 上下文
  let baggage_context = azimuth::telemetry::BaggageContext {
    entries: [
      {
        key: "user.id",
        value: "user-12345",
        metadata: Some({
          properties: [
            ("propagate", "true"),
            ("ttl", "3600")
          ]
        })
      },
      {
        key: "request.id",
        value: "req-abcde",
        metadata: None
      },
      {
        key: "session.id",
        value: "sess-xyz",
        metadata: Some({
          properties: [
            ("propagate", "true"),
            ("secure", "true")
          ]
        })
      }
    ]
  }
  
  assert_eq(baggage_context.entries.length(), 3)
  
  // 验证 baggage 条目
  let mut user_id_found = false
  let mut request_id_found = false
  let mut session_id_found = false
  
  for entry in baggage_context.entries {
    match entry.key {
      "user.id" => {
        assert_eq(entry.value, "user-12345")
        match entry.metadata {
          Some(metadata) => {
            assert_eq(metadata.properties.length(), 2)
          }
          None => assert_true(false)
        }
        user_id_found = true
      }
      "request.id" => {
        assert_eq(entry.value, "req-abcde")
        assert_eq(entry.metadata, None)
        request_id_found = true
      }
      "session.id" => {
        assert_eq(entry.value, "sess-xyz")
        match entry.metadata {
          Some(metadata) => {
            assert_eq(metadata.properties.length(), 2)
          }
          None => assert_true(false)
        }
        session_id_found = true
      }
      _ => () // 其他键
    }
  }
  
  assert_true(user_id_found)
  assert_true(request_id_found)
  assert_true(session_id_found)
}