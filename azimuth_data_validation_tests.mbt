// Azimuth Data Validation Test Suite
// 数据验证和完整性检查测试用例

test "输入数据类型验证" {
  // 测试输入数据类型验证
  let valid_string = "azimuth-telemetry"
  let invalid_string = ""
  let valid_number = 42
  let invalid_number = -1
  let valid_float = 3.14
  let invalid_float = -0.1
  
  // 字符串验证
  assert_true(valid_string.length() > 0)
  assert_true(valid_string.contains("telemetry"))
  assert_false(invalid_string.length() > 0)
  
  // 整数验证
  assert_true(valid_number >= 0)
  assert_false(invalid_number >= 0)
  
  // 浮点数验证
  assert_true(valid_float > 0.0)
  assert_false(invalid_float > 0.0)
}

test "遥测数据结构完整性验证" {
  // 测试遥测数据结构完整性
  let telemetry_data = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "abcdef1234567890abcdef1234567890",
    span_id : "1234567890abcdef",
    parent_span_id : Some("abcdef1234567890"),
    operation_name : "http.request",
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 150L,
    attributes : [
      ("http.method", @azimuth.StringValue("GET")),
      ("http.url", @azimuth.StringValue("/api/users")),
      ("http.status_code", @azimuth.IntValue(200))
    ],
    events : []
  }
  
  // 验证必需字段
  assert_true(telemetry_data.timestamp > 0L)
  assert_true(telemetry_data.trace_id.length() == 32)
  assert_true(telemetry_data.span_id.length() == 16)
  assert_true(telemetry_data.operation_name.length() > 0)
  assert_true(telemetry_data.duration_ms >= 0L)
  
  // 验证可选字段
  match telemetry_data.parent_span_id {
    Some(parent_id) => assert_true(parent_id.length() == 16)
    None => assert_true(true)
  }
  
  // 验证属性结构
  assert_true(telemetry_data.attributes.length() >= 3)
  for attr in telemetry_data.attributes {
    assert_true(attr.0.length() > 0)
    match attr.1 {
      @azimuth.StringValue(v) => assert_true(v.length() > 0)
      @azimuth.IntValue(v) => assert_true(v >= 0)
      @azimuth.FloatValue(v) => assert_true(v >= 0.0)
      @azimuth.BoolValue(v) => assert_true(true)
    }
  }
}

test "配置参数验证" {
  // 测试配置参数验证
  let valid_config = @azimuth.TelemetryConfig {
    service_name : "payment-service",
    service_version : "2.1.0",
    sampling_config : @azimuth.SamplingConfig {
      sampling_type : @azimuth.SamplingType::Probability,
      probability : 0.1,
      rate_limit : Some(1000)
    },
    exporter_config : @azimuth.ExporterConfig {
      exporter_type : @azimuth.ExporterType::OTLP,
      endpoint : "https://otel-collector.example.com:4317",
      headers : [("Authorization", "Bearer token123")],
      timeout_ms : 5000,
      batch_size : 512,
      max_export_batch_size : 512
    },
    resource_attributes : [
      ("deployment.environment", @azimuth.StringValue("production"))
    ]
  }
  
  // 验证服务配置
  assert_true(valid_config.service_name.length() > 0)
  assert_true(valid_config.service_version.length() > 0)
  
  // 验证采样配置
  match valid_config.sampling_config.sampling_type {
    @azimuth.SamplingType::Probability => {
      assert_true(valid_config.sampling_config.probability >= 0.0)
      assert_true(valid_config.sampling_config.probability <= 1.0)
    }
    @azimuth.SamplingType::RateLimit => {
      match valid_config.sampling_config.rate_limit {
        Some(limit) => assert_true(limit > 0)
        None => assert_true(false)
      }
    }
    @azimuth.SamplingType::Always => assert_true(true)
    @azimuth.SamplingType::Never => assert_true(true)
  }
  
  // 验证导出器配置
  match valid_config.exporter_config.exporter_type {
    @azimuth.ExporterType::OTLP => assert_true(true)
    @azimuth.ExporterType::Jaeger => assert_true(true)
    @azimuth.ExporterType::Zipkin => assert_true(true)
    @azimuth.ExporterType::Prometheus => assert_true(true)
  }
  
  assert_true(valid_config.exporter_config.endpoint.length() > 0)
  assert_true(valid_config.exporter_config.timeout_ms > 0)
  assert_true(valid_config.exporter_config.batch_size > 0)
  assert_true(valid_config.exporter_config.max_export_batch_size > 0)
}

test "度量数据范围验证" {
  // 测试度量数据范围验证
  let counter_metric = @azimuth.CounterMetric {
    name : "http.requests.total",
    description : "Total number of HTTP requests",
    unit : "requests",
    value : 1000L,
    attributes : [
      ("method", @azimuth.StringValue("GET")),
      ("status", @azimuth.StringValue("200"))
    ]
  }
  
  let histogram_metric = @azimuth.HistogramMetric {
    name : "http.request.duration",
    description : "HTTP request duration in milliseconds",
    unit : "ms",
    buckets : [
      (0.0, 10L),
      (10.0, 50L),
      (50.0, 100L),
      (100.0, 500L),
      (500.0, 100L)
    ],
    sum : 12500.0,
    count : 100L,
    attributes : [
      ("endpoint", @azimuth.StringValue("/api/users"))
    ]
  }
  
  // 验证计数器指标
  assert_true(counter_metric.name.length() > 0)
  assert_true(counter_metric.value >= 0L)
  assert_true(counter_metric.attributes.length() >= 2)
  
  // 验证直方图指标
  assert_true(histogram_metric.name.length() > 0)
  assert_true(histogram_metric.count >= 0L)
  assert_true(histogram_metric.sum >= 0.0)
  assert_true(histogram_metric.buckets.length() >= 2)
  
  // 验证直方图桶边界
  for i in 0..<(histogram_metric.buckets.length() - 1) {
    let current_bucket = histogram_metric.buckets[i]
    let next_bucket = histogram_metric.buckets[i + 1]
    assert_true(current_bucket.0 < next_bucket.0)
    assert_true(current_bucket.1 >= 0L)
  }
  
  // 验证最后一个桶
  let last_bucket = histogram_metric.buckets[histogram_metric.buckets.length() - 1]
  assert_true(last_bucket.1 >= 0L)
}

test "日志记录格式验证" {
  // 测试日志记录格式验证
  let log_record = @azimuth.LogRecord {
    timestamp : 1640995200300L,
    severity : @azimuth.LogSeverity::Info,
    body : @azimuth.StringValue("User authentication successful"),
    attributes : [
      ("user.id", @azimuth.StringValue("user123")),
      ("auth.method", @azimuth.StringValue("oauth2")),
      ("client.ip", @azimuth.StringValue("192.168.1.100"))
    ],
    trace_id : Some("1234567890abcdef1234567890abcdef"),
    span_id : Some("auth1234567890")
  }
  
  // 验证时间戳
  assert_true(log_record.timestamp > 0L)
  
  // 验证严重性级别
  match log_record.severity {
    @azimuth.LogSeverity::Trace => assert_true(true)
    @azimuth.LogSeverity::Debug => assert_true(true)
    @azimuth.LogSeverity::Info => assert_true(true)
    @azimuth.LogSeverity::Warn => assert_true(true)
    @azimuth.LogSeverity::Error => assert_true(true)
    @azimuth.LogSeverity::Fatal => assert_true(true)
  }
  
  // 验证日志主体
  match log_record.body {
    @azimuth.StringValue(msg) => assert_true(msg.length() > 0)
    _ => assert_true(false)
  }
  
  // 验证属性
  assert_true(log_record.attributes.length() >= 3)
  for attr in log_record.attributes {
    assert_true(attr.0.length() > 0)
    match attr.1 {
      @azimuth.StringValue(v) => assert_true(v.length() > 0)
      @azimuth.IntValue(v) => assert_true(true)
      @azimuth.FloatValue(v) => assert_true(true)
      @azimuth.BoolValue(v) => assert_true(true)
    }
  }
  
  // 验证追踪关联
  match log_record.trace_id {
    Some(trace_id) => assert_true(trace_id.length() == 32)
    None => assert_true(true)
  }
  
  match log_record.span_id {
    Some(span_id) => assert_true(span_id.length() == 16)
    None => assert_true(true)
  }
}

test "上下文传播数据验证" {
  // 测试上下文传播数据验证
  let context = @azimuth.Context {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "abcdef1234567890",
    baggage : [
      ("user.id", "user123"),
      ("request.id", "req-456"),
      ("trace.sampled", "true")
    ],
    entries : [
      ("correlation.id", "corr-789"),
      ("session.id", "sess-101112")
    ]
  }
  
  // 验证追踪ID
  assert_true(context.trace_id.length() == 32)
  
  // 验证跨度ID
  assert_true(context.span_id.length() == 16)
  
  // 验证baggage项
  assert_true(context.baggage.length() >= 3)
  for baggage_item in context.baggage {
    assert_true(baggage_item.0.length() > 0)
    assert_true(baggage_item.1.length() > 0)
  }
  
  // 验证上下文条目
  assert_true(context.entries.length() >= 2)
  for entry in context.entries {
    assert_true(entry.0.length() > 0)
    assert_true(entry.1.length() > 0)
  }
}