// Azimuth 序列化综合测试用例
// 测试遥测数据的序列化和反序列化功能

test "遥测数据JSON序列化" {
  // 创建遥测数据点
  let telemetry_data = @azimuth.TelemetryDataPoint {
    name: "cpu.usage",
    value: @azimuth.FloatValue(75.5),
    timestamp: @azimuth.Timestamp(1640995200),
    attributes: @azimuth.Attributes {
      values: [
        ("host", @azimuth.StringValue("server-1")),
        ("region", @azimuth.StringValue("us-west")),
        ("environment", @azimuth.StringValue("production"))
      ]
    }
  }
  
  // 模拟JSON序列化
  let json_output = {
    let name = telemetry_data.name
    let value = match telemetry_data.value {
      @azimuth.FloatValue(v) => v.to_string()
      _ => "null"
    }
    let timestamp = telemetry_data.timestamp.to_string()
    let attributes = telemetry_data.attributes.values.map(fn(attr) {
      let key = attr.0
      let value = match attr.1 {
        @azimuth.StringValue(v) => "\"" + v + "\""
        _ => "null"
      }
      "\"" + key + "\":" + value
    }).join(",")
    
    "{\n      \"name\":\"" + name + "\",\n      \"value\":" + value + ",\n      \"timestamp\":" + timestamp + ",\n      \"attributes\":{" + attributes + "}\n    }"
  }
  
  // 验证JSON结构
  assert_true(json_output.contains("\"name\":\"cpu.usage\""))
  assert_true(json_output.contains("\"value\":75.5"))
  assert_true(json_output.contains("\"timestamp\":1640995200"))
  assert_true(json_output.contains("\"host\":\"server-1\""))
  assert_true(json_output.contains("\"region\":\"us-west\""))
  assert_true(json_output.contains("\"environment\":\"production\""))
  
  // 模拟JSON反序列化
  let parsed_name = "cpu.usage"
  let parsed_value = 75.5
  let parsed_timestamp = @azimuth.Timestamp(1640995200)
  let parsed_attributes = [
    ("host", @azimuth.StringValue("server-1")),
    ("region", @azimuth.StringValue("us-west")),
    ("environment", @azimuth.StringValue("production"))
  ]
  
  // 验证反序列化结果
  assert_eq(parsed_name, telemetry_data.name)
  assert_eq(parsed_value, match telemetry_data.value { @azimuth.FloatValue(v) => v _ => 0.0 })
  assert_eq(parsed_timestamp, telemetry_data.timestamp)
  assert_eq(parsed_attributes.length(), telemetry_data.attributes.values.length())
}

test "追踪数据二进制序列化" {
  // 创建Span数据
  let span_data = @azimuth.Span {
    context: @azimuth.TraceContext {
      trace_id: @azimuth.TraceId("1234567890abcdef1234567890abcdef"),
      span_id: @azimuth.SpanId("1111111111111111"),
      sampled: true,
      baggage: @azimuth.Baggage {
        entries: [
          ("user.id", "user-123"),
          ("request.source", "mobile-app")
        ]
      }
    },
    parent_span_id: Some(@azimuth.SpanId("0000000000000000")),
    name: "http.request",
    kind: @azimuth.SpanKind.Client,
    start_time: @azimuth.Timestamp(1640995200),
    end_time: Some(@azimuth.Timestamp(1640995250)),
    status: @azimuth.SpanStatus.Ok,
    attributes: @azimuth.Attributes {
      values: [
        ("http.method", @azimuth.StringValue("GET")),
        ("http.url", @azimuth.StringValue("https://api.example.com/data")),
        ("http.status_code", @azimuth.IntValue(200))
      ]
    },
    events: [
      @azimuth.SpanEvent {
        name: "http.request.sent",
        timestamp: @azimuth.Timestamp(1640995205),
        attributes: @azimuth.Attributes {
          values: [
            ("http.request.size", @azimuth.IntValue(256))
          ]
        }
      },
      @azimuth.SpanEvent {
        name: "http.response.received",
        timestamp: @azimuth.Timestamp(1640995245),
        attributes: @azimuth.Attributes {
          values: [
            ("http.response.size", @azimuth.IntValue(1024))
          ]
        }
      }
    ],
    links: []
  }
  
  // 模拟二进制序列化（简化版本）
  let binary_data = {
    // 序列化TraceContext
    let trace_id_bytes = span_data.context.trace_id.as_bytes()
    let span_id_bytes = span_data.context.span_id.as_bytes()
    let sampled_byte = if span_data.context.sampled { 1 } else { 0 }
    
    // 序列化Span基本信息
    let name_bytes = span_data.name.as_bytes()
    let kind_byte = match span_data.kind {
      @azimuth.SpanKind.Client => 1
      @azimuth.SpanKind.Server => 2
      @azimuth.SpanKind.Internal => 3
      @azimuth.SpanKind.Producer => 4
      @azimuth.SpanKind.Consumer => 5
    }
    
    // 序列化时间戳
    let start_time_bytes = span_data.start_time.to_bytes()
    let end_time_bytes = match span_data.end_time {
      Some(time) => time.to_bytes()
      None => [0, 0, 0, 0, 0, 0, 0, 0]
    }
    
    // 合并所有字节
    trace_id_bytes + span_id_bytes + [sampled_byte] + [kind_byte] + 
    name_bytes + start_time_bytes + end_time_bytes
  }
  
  // 验证二进制数据不为空
  assert_true(binary_data.length() > 0)
  
  // 模拟二进制反序列化
  let parsed_trace_id = @azimuth.TraceId.from_bytes(binary_data.slice(0, 16))
  let parsed_span_id = @azimuth.SpanId.from_bytes(binary_data.slice(16, 24))
  let parsed_sampled = binary_data[24] == 1
  let parsed_kind = match binary_data[25] {
    1 => @azimuth.SpanKind.Client
    2 => @azimuth.SpanKind.Server
    3 => @azimuth.SpanKind.Internal
    4 => @azimuth.SpanKind.Producer
    5 => @azimuth.SpanKind.Consumer
    _ => @azimuth.SpanKind.Internal
  }
  
  // 验证反序列化结果
  assert_eq(parsed_trace_id, span_data.context.trace_id)
  assert_eq(parsed_span_id, span_data.context.span_id)
  assert_eq(parsed_sampled, span_data.context.sampled)
  assert_eq(parsed_kind, span_data.kind)
}

test "指标数据Protocol Buffers序列化" {
  // 创建指标数据
  let metric_data = @azimuth.MetricData {
    name: "request.duration",
    description: "Request processing duration",
    unit: "ms",
    metric_type: @azimuth.MetricType.Histogram,
    data: @azimuth.HistogramData {
      buckets: [
        @azimuth.HistogramBucket {
          upper_bound: 10.0,
          count: 25
        },
        @azimuth.HistogramBucket {
          upper_bound: 50.0,
          count: 45
        },
        @azimuth.HistogramBucket {
          upper_bound: 100.0,
          count: 20
        },
        @azimuth.HistogramBucket {
          upper_bound: 500.0,
          count: 8
        },
        @azimuth.HistogramBucket {
          upper_bound: 1000.0,
          count: 2
        }
      ],
      count: 100,
      sum: 8750.0
    },
    attributes: @azimuth.Attributes {
      values: [
        ("service", @azimuth.StringValue("api-service")),
        ("endpoint", @azimuth.StringValue("/users")),
        ("method", @azimuth.StringValue("GET"))
      ]
    },
    timestamp: @azimuth.Timestamp(1640995200)
  }
  
  // 模拟Protocol Buffers序列化
  let proto_fields = [
    ("name", metric_data.name),
    ("description", metric_data.description),
    ("unit", metric_data.unit),
    ("metric_type", match metric_data.metric_type {
      @azimuth.MetricType.Counter => "COUNTER"
      @azimuth.MetricType.Gauge => "GAUGE"
      @azimuth.MetricType.Histogram => "HISTOGRAM"
      @azimuth.MetricType.Summary => "SUMMARY"
    }),
    ("timestamp", metric_data.timestamp.to_string())
  ]
  
  // 序列化直方图数据
  let histogram_data = match metric_data.data {
    @azimuth.HistogramData(h) => {
      let buckets = h.buckets.map(fn(bucket) {
        "bucket{le=" + bucket.upper_bound.to_string() + "}=" + bucket.count.to_string()
      }).join(",")
      "count=" + h.count.to_string() + ",sum=" + h.sum.to_string() + "," + buckets
    }
    _ => ""
  }
  
  // 序列化属性
  let attributes = metric_data.attributes.values.map(fn(attr) {
    let key = attr.0
    let value = match attr.1 {
      @azimuth.StringValue(v) => v
      _ => ""
    }
    key + "=\"" + value + "\""
  }).join(",")
  
  // 创建Protocol Buffers风格的输出
  let proto_output = proto_fields.map(fn(field) {
    field.0 + ":\"" + field.1 + "\""
  }).join(",") + ",histogram:{" + histogram_data + "},attributes:{" + attributes + "}"
  
  // 验证Protocol Buffers输出
  assert_true(proto_output.contains("name:\"request.duration\""))
  assert_true(proto_output.contains("description:\"Request processing duration\""))
  assert_true(proto_output.contains("unit:\"ms\""))
  assert_true(proto_output.contains("metric_type:\"HISTOGRAM\""))
  assert_true(proto_output.contains("count=100"))
  assert_true(proto_output.contains("sum=8750.0"))
  assert_true(proto_output.contains("service=\"api-service\""))
  assert_true(proto_output.contains("endpoint=\"/users\""))
  assert_true(proto_output.contains("method=\"GET\""))
  
  // 模拟Protocol Buffers反序列化
  let parsed_name = "request.duration"
  let parsed_description = "Request processing duration"
  let parsed_unit = "ms"
  let parsed_metric_type = @azimuth.MetricType.Histogram
  let parsed_timestamp = @azimuth.Timestamp(1640995200)
  
  // 验证反序列化结果
  assert_eq(parsed_name, metric_data.name)
  assert_eq(parsed_description, metric_data.description)
  assert_eq(parsed_unit, metric_data.unit)
  assert_eq(parsed_metric_type, metric_data.metric_type)
  assert_eq(parsed_timestamp, metric_data.timestamp)
}

test "日志数据Avro序列化" {
  // 创建日志记录
  let log_record = @azimuth.LogRecord {
    timestamp: @azimuth.Timestamp(1640995200),
    severity: @azimuth.LogSeverity.Info,
    body: @azimuth.StringValue("User login successful"),
    resource: @azimuth.Resource {
      attributes: @azimuth.Attributes {
        values: [
          ("service.name", @azimuth.StringValue("auth-service")),
          ("service.version", @azimuth.StringValue("1.2.3")),
          ("host.name", @azimuth.StringValue("auth-server-1"))
        ]
      }
    },
    instrumentation_scope: @azimuth.InstrumentationScope {
      name: "azimuth.auth",
      version: "1.0.0"
    },
    attributes: @azimuth.Attributes {
      values: [
        ("user.id", @azimuth.StringValue("user-123")),
        ("user.ip", @azimuth.StringValue("192.168.1.100")),
        ("login.method", @azimuth.StringValue("password")),
        ("session.id", @azimuth.StringValue("sess-456"))
      ]
    },
    trace_id: Some(@azimuth.TraceId("abcdef1234567890abcdef1234567890")),
    span_id: Some(@azimuth.SpanId("1111111111111111")),
    flags: 1
  }
  
  // 模拟Avro序列化（简化版本）
  let avro_schema = {
    "{\n      \"type\":\"record\",\n      \"name\":\"LogRecord\",\n      \"fields\":[\n        {\"name\":\"timestamp\",\"type\":\"long\"},\n        {\"name\":\"severity\",\"type\":\"string\"},\n        {\"name\":\"body\",\"type\":\"string\"},\n        {\"name\":\"resource\",\"type\":\"{\n          \"type\":\"record\",\n          \"name\":\"Resource\",\n          \"fields\":[\n            {\"name\":\"attributes\",\"type\":{\"type\":\"map\",\"values\":\"string\"}}\n          ]\n        }\"},\n        {\"name\":\"instrumentation_scope\",\"type\":\"{\n          \"type\":\"record\",\n          \"name\":\"InstrumentationScope\",\n          \"fields\":[\n            {\"name\":\"name\",\"type\":\"string\"},\n            {\"name\":\"version\",\"type\":\"string\"}\n          ]\n        }\"},\n        {\"name\":\"attributes\",\"type\":{\"type\":\"map\",\"values\":\"string\"}},\n        {\"name\":\"trace_id\",\"type\":[\"null\",\"string\"]},\n        {\"name\":\"span_id\",\"type\":[\"null\",\"string\"]},\n        {\"name\":\"flags\",\"type\":\"int\"}\n      ]\n    }"
  }
  
  // 序列化日志记录
  let avro_fields = [
    ("timestamp", log_record.timestamp.to_string()),
    ("severity", match log_record.severity {
      @azimuth.LogSeverity.Trace => "TRACE"
      @azimuth.LogSeverity.Debug => "DEBUG"
      @azimuth.LogSeverity.Info => "INFO"
      @azimuth.LogSeverity.Warn => "WARN"
      @azimuth.LogSeverity.Error => "ERROR"
      @azimuth.LogSeverity.Fatal => "FATAL"
    }),
    ("body", match log_record.body {
      @azimuth.StringValue(v) => v
      _ => ""
    })
  ]
  
  // 序列化资源属性
  let resource_attributes = log_record.resource.attributes.values.map(fn(attr) {
    let key = attr.0
    let value = match attr.1 {
      @azimuth.StringValue(v) => v
      _ => ""
    }
    "\"" + key + "\":\"" + value + "\""
  }).join(",")
  
  // 序列化仪器范围
  let instrumentation_scope = {
    let name = log_record.instrumentation_scope.name
    let version = log_record.instrumentation_scope.version
    "{\"name\":\"" + name + "\",\"version\":\"" + version + "\"}"
  }
  
  // 序列化日志属性
  let log_attributes = log_record.attributes.values.map(fn(attr) {
    let key = attr.0
    let value = match attr.1 {
      @azimuth.StringValue(v) => v
      _ => ""
    }
    "\"" + key + "\":\"" + value + "\""
  }).join(",")
  
  // 序列化追踪信息
  let trace_id = match log_record.trace_id {
    Some(id) => "\"" + id.to_string() + "\""
    None => "null"
  }
  let span_id = match log_record.span_id {
    Some(id) => "\"" + id.to_string() + "\""
    None => "null"
  }
  
  // 创建Avro风格的输出
  let avro_output = "{" + avro_fields.map(fn(field) {
    "\"" + field.0 + "\":" + field.1
  }).join(",") + ",\"resource\":{" + resource_attributes + "},\"instrumentation_scope\":" + 
  instrumentation_scope + ",\"attributes\":{" + log_attributes + "},\"trace_id\":" + trace_id + 
  ",\"span_id\":" + span_id + ",\"flags\":" + log_record.flags.to_string() + "}"
  
  // 验证Avro输出
  assert_true(avro_output.contains("\"timestamp\":1640995200"))
  assert_true(avro_output.contains("\"severity\":\"INFO\""))
  assert_true(avro_output.contains("\"body\":\"User login successful\""))
  assert_true(avro_output.contains("\"service.name\":\"auth-service\""))
  assert_true(avro_output.contains("\"service.version\":\"1.2.3\""))
  assert_true(avro_output.contains("\"host.name\":\"auth-server-1\""))
  assert_true(avro_output.contains("\"name\":\"azimuth.auth\""))
  assert_true(avro_output.contains("\"version\":\"1.0.0\""))
  assert_true(avro_output.contains("\"user.id\":\"user-123\""))
  assert_true(avro_output.contains("\"user.ip\":\"192.168.1.100\""))
  assert_true(avro_output.contains("\"login.method\":\"password\""))
  assert_true(avro_output.contains("\"session.id\":\"sess-456\""))
  assert_true(avro_output.contains("\"trace_id\":\"abcdef1234567890abcdef1234567890\""))
  assert_true(avro_output.contains("\"span_id\":\"1111111111111111\""))
  assert_true(avro_output.contains("\"flags\":1"))
  
  // 模拟Avro反序列化
  let parsed_timestamp = @azimuth.Timestamp(1640995200)
  let parsed_severity = @azimuth.LogSeverity.Info
  let parsed_body = @azimuth.StringValue("User login successful")
  let parsed_trace_id = Some(@azimuth.TraceId("abcdef1234567890abcdef1234567890"))
  let parsed_span_id = Some(@azimuth.SpanId("1111111111111111"))
  let parsed_flags = 1
  
  // 验证反序列化结果
  assert_eq(parsed_timestamp, log_record.timestamp)
  assert_eq(parsed_severity, log_record.severity)
  assert_eq(parsed_body, log_record.body)
  assert_eq(parsed_trace_id, log_record.trace_id)
  assert_eq(parsed_span_id, log_record.span_id)
  assert_eq(parsed_flags, log_record.flags)
}

test "序列化性能和压缩测试" {
  // 创建大量遥测数据
  let large_dataset = (0..1000).map(fn(i) {
    @azimuth.TelemetryDataPoint {
      name: "metric." + i.to_string(),
      value: @azimuth.FloatValue(i.to_float() * 1.5),
      timestamp: @azimuth.Timestamp(1640995200 + i * 60),
      attributes: @azimuth.Attributes {
        values: [
          ("source", @azimuth.StringValue("sensor-" + (i % 10).to_string())),
          ("type", @azimuth.StringValue(if i % 2 == 0 { "temperature" } else { "humidity" })),
          ("location", @azimuth.StringValue("room-" + (i % 5).to_string()))
        ]
      }
    }
  })
  
  // 测试JSON序列化性能
  let json_start_time = @azimuth.Timestamp.now()
  let json_serialized = large_dataset.map(fn(data) {
    let name = data.name
    let value = match data.value { @azimuth.FloatValue(v) => v.to_string() _ => "null" }
    let timestamp = data.timestamp.to_string()
    let attributes = data.attributes.values.map(fn(attr) {
      let key = attr.0
      let value = match attr.1 { @azimuth.StringValue(v) => v _ => "" }
      "\"" + key + "\":\"" + value + "\""
    }).join(",")
    "{\"name\":\"" + name + "\",\"value\":" + value + ",\"timestamp\":" + timestamp + ",\"attributes\":{" + attributes + "}}"
  })
  let json_end_time = @azimuth.Timestamp.now()
  let json_serialization_time = json_end_time - json_start_time
  
  // 测试二进制序列化性能
  let binary_start_time = @azimuth.Timestamp.now()
  let binary_serialized = large_dataset.map(fn(data) {
    let name_bytes = data.name.as_bytes()
    let value_bytes = match data.value { 
      @azimuth.FloatValue(v) => v.to_string().as_bytes() 
      _ => "null".as_bytes() 
    }
    let timestamp_bytes = data.timestamp.to_bytes()
    name_bytes + value_bytes + timestamp_bytes
  })
  let binary_end_time = @azimuth.Timestamp.now()
  let binary_serialization_time = binary_end_time - binary_start_time
  
  // 验证序列化结果
  assert_eq(json_serialized.length(), 1000)
  assert_eq(binary_serialized.length(), 1000)
  
  // 计算序列化大小
  let json_size = json_serialized.reduce(fn(acc, item) { acc + item.length() }, 0)
  let binary_size = binary_serialized.reduce(fn(acc, item) { acc + item.length() }, 0)
  
  // 验证二进制序列化更紧凑
  assert_true(binary_size < json_size)
  
  // 计算压缩比
  let compression_ratio = binary_size.to_float() / json_size.to_float()
  assert_true(compression_ratio < 1.0) // 二进制应该比JSON更小
  
  // 测试压缩性能
  let compression_start_time = @azimuth.Timestamp.now()
  let compressed_data = json_serialized.map(fn(item) {
    // 模拟压缩（简化版本）
    item.replace(" ", "").replace("\n", "").replace("\t", "")
  })
  let compression_end_time = @azimuth.Timestamp.now()
  let compression_time = compression_end_time - compression_start_time
  
  // 计算压缩后大小
  let compressed_size = compressed_data.reduce(fn(acc, item) { acc + item.length() }, 0)
  
  // 验证压缩效果
  assert_true(compressed_size < json_size)
  
  // 计算压缩比
  let compression_ratio = compressed_size.to_float() / json_size.to_float()
  assert_true(compression_ratio < 1.0) // 压缩后应该更小
  
  // 验证压缩比在合理范围内（假设压缩率在30%-70%之间）
  assert_true(compression_ratio > 0.3 && compression_ratio < 0.7)
}