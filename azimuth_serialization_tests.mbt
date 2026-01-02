// Azimuth Serialization Test Suite
// 测试遥测数据的序列化和反序列化功能

test "span数据序列化和反序列化" {
  // 创建span数据
  let span = Span {
    name: "test.operation",
    span_context: SpanContext {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      sampled: true,
      trace_state: ""
    },
    parent_span_id: Some("abcdef1234567890"),
    kind: SpanKind::INTERNAL,
    start_time: "2025-01-02T10:00:00Z",
    end_time: Some("2025-01-02T10:00:05Z"),
    status: SpanStatus {
      code: StatusCode::OK,
      message: "Operation completed successfully"
    },
    attributes: [
      ("operation.type", AttributeValue::StringValue("test")),
      ("operation.duration", AttributeValue::IntValue(5000)),
      ("operation.success", AttributeValue::BoolValue(true))
    ],
    events: [
      SpanEvent {
        name: "operation.started",
        timestamp: "2025-01-02T10:00:00Z",
        attributes: [("step", "start")]
      },
      SpanEvent {
        name: "operation.completed",
        timestamp: "2025-01-02T10:00:05Z",
        attributes: [("result", "success")]
      }
    ],
    links: [],
    resource: Resource {
      attributes: [
        ("service.name", AttributeValue::StringValue("test.service")),
        ("service.version", AttributeValue::StringValue("1.0.0"))
      ]
    }
  }
  
  // 序列化span到JSON
  let json_data = SpanSerializer::to_json(span)
  
  // 验证JSON不为空
  assert_true(json_data.length() > 0)
  
  // 反序列化JSON到span
  let deserialized_span = SpanSerializer::from_json(json_data)
  
  // 验证反序列化的数据
  assert_eq(Span::name(deserialized_span), Span::name(span))
  assert_eq(SpanContext::trace_id(Span::span_context(deserialized_span)), 
            SpanContext::trace_id(Span::span_context(span)))
  assert_eq(SpanContext::span_id(Span::span_context(deserialized_span)), 
            SpanContext::span_id(Span::span_context(span)))
  
  assert_true(true)
}

test "度量数据序列化和反序列化" {
  // 创建度量数据
  let metric = Metric {
    name: "request.count",
    description: Some("Total number of requests"),
    unit: Some("count"),
    data: MetricData::Counter(CounterData {
      value: 100.0,
      attributes: [
        ("method", AttributeValue::StringValue("GET")),
        ("status", AttributeValue::StringValue("200"))
      ]
    }),
    resource: Resource {
      attributes: [
        ("service.name", AttributeValue::StringValue("test.service")),
        ("service.version", AttributeValue::StringValue("1.0.0"))
      ]
    }
  }
  
  // 序列化度量到JSON
  let json_data = MetricSerializer::to_json(metric)
  
  // 验证JSON不为空
  assert_true(json_data.length() > 0)
  
  // 反序列化JSON到度量
  let deserialized_metric = MetricSerializer::from_json(json_data)
  
  // 验证反序列化的数据
  assert_eq(Metric::name(deserialized_metric), Metric::name(metric))
  assert_eq(Metric::description(deserialized_metric), Metric::description(metric))
  assert_eq(Metric::unit(deserialized_metric), Metric::unit(metric))
  
  assert_true(true)
}

test "日志数据序列化和反序列化" {
  // 创建日志数据
  let log = LogRecord {
    timestamp: "2025-01-02T10:00:00Z",
    observed_timestamp: Some("2025-01-02T10:00:00.100Z"),
    severity_number: Some(SeverityNumber::INFO),
    severity_text: Some("INFO"),
    body: Some("Operation completed successfully"),
    attributes: [
      ("operation.id", AttributeValue::StringValue("op-123")),
      ("operation.duration", AttributeValue::IntValue(500)),
      ("user.id", AttributeValue::StringValue("user-456"))
    ],
    trace_id: Some("1234567890abcdef1234567890abcdef"),
    span_id: Some("1234567890abcdef"),
    trace_flags: Some(1),
    resource: Resource {
      attributes: [
        ("service.name", AttributeValue::StringValue("test.service")),
        ("service.version", AttributeValue::StringValue("1.0.0"))
      ]
    },
    instrumentation_scope: Some(InstrumentationScope {
      name: "test.logger",
      version: Some("1.0.0"),
      schema_url: Some("https://example.com/schema")
    })
  }
  
  // 序列化日志到JSON
  let json_data = LogSerializer::to_json(log)
  
  // 验证JSON不为空
  assert_true(json_data.length() > 0)
  
  // 反序列化JSON到日志
  let deserialized_log = LogSerializer::from_json(json_data)
  
  // 验证反序列化的数据
  assert_eq(LogRecord::timestamp(deserialized_log), LogRecord::timestamp(log))
  assert_eq(LogRecord::severity_text(deserialized_log), LogRecord::severity_text(log))
  assert_eq(LogRecord::body(deserialized_log), LogRecord::body(log))
  
  assert_true(true)
}

test "资源数据序列化和反序列化" {
  // 创建资源数据
  let resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("test.service")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("service.instance.id", AttributeValue::StringValue("instance-123")),
      ("host.name", AttributeValue::StringValue("test-host")),
      ("host.arch", AttributeValue::StringValue("amd64")),
      ("os.type", AttributeValue::StringValue("linux")),
      ("os.version", AttributeValue::StringValue("5.15.0")),
      ("process.pid", AttributeValue::IntValue(12345)),
      ("process.executable.name", AttributeValue::StringValue("test-app")),
      ("process.command_args", AttributeValue::ArrayStringValue(["./test-app", "--config", "test.yaml"]))
    ]
  }
  
  // 序列化资源到JSON
  let json_data = ResourceSerializer::to_json(resource)
  
  // 验证JSON不为空
  assert_true(json_data.length() > 0)
  
  // 反序列化JSON到资源
  let deserialized_resource = ResourceSerializer::from_json(json_data)
  
  // 验证反序列化的数据
  let service_name = Resource::get_attribute(deserialized_resource, "service.name")
  let service_version = Resource::get_attribute(deserialized_resource, "service.version")
  let instance_id = Resource::get_attribute(deserialized_resource, "service.instance.id")
  
  match service_name {
    Some(AttributeValue::StringValue(name)) => assert_eq(name, "test.service")
    _ => assert_true(false)
  }
  
  match service_version {
    Some(AttributeValue::StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  match instance_id {
    Some(AttributeValue::StringValue(id)) => assert_eq(id, "instance-123")
    _ => assert_true(false)
  }
  
  assert_true(true)
}

test "批量数据序列化和反序列化" {
  // 创建批量遥测数据
  let batch_data = BatchTelemetryData {
    resource: Resource {
      attributes: [
        ("service.name", AttributeValue::StringValue("test.service"))
      ]
    },
    scope_metrics: [
      ScopeMetrics {
        scope: InstrumentationScope {
          name: "test.instrumentation",
          version: Some("1.0.0")
        },
        metrics: [
          Metric {
            name: "request.count",
            data: MetricData::Counter(CounterData { value: 100.0, attributes: [] }),
            resource: Resource { attributes: [] },
            description: None,
            unit: None
          },
          Metric {
            name: "request.duration",
            data: MetricData::Histogram(HistogramData {
              data_points: [
                HistogramDataPoint {
                  attributes: [],
                  start_time: "2025-01-02T10:00:00Z",
                  time: "2025-01-02T10:01:00Z",
                  count: 100,
                  sum: 50000.0,
                  bucket_counts: [10, 20, 30, 40],
                  explicit_bounds: [10.0, 50.0, 100.0]
                }
              ],
              aggregation_temporality: AggregationTemporality::DELTA
            }),
            resource: Resource { attributes: [] },
            description: None,
            unit: None
          }
        ]
      }
    ],
    spans: [
      Span {
        name: "span1",
        span_context: SpanContext {
          trace_id: "1234567890abcdef1234567890abcdef",
          span_id: "1234567890abcdef",
          sampled: true,
          trace_state: ""
        },
        parent_span_id: None,
        kind: SpanKind::INTERNAL,
        start_time: "2025-01-02T10:00:00Z",
        end_time: Some("2025-01-02T10:00:05Z"),
        status: SpanStatus { code: StatusCode::OK, message: "" },
        attributes: [],
        events: [],
        links: [],
        resource: Resource { attributes: [] }
      }
    ],
    logs: [
      LogRecord {
        timestamp: "2025-01-02T10:00:00Z",
        observed_timestamp: None,
        severity_number: Some(SeverityNumber::INFO),
        severity_text: Some("INFO"),
        body: Some("Test log message"),
        attributes: [],
        trace_id: None,
        span_id: None,
        trace_flags: None,
        resource: Resource { attributes: [] },
        instrumentation_scope: None
      }
    ]
  }
  
  // 序列化批量数据到JSON
  let json_data = BatchSerializer::to_json(batch_data)
  
  // 验证JSON不为空
  assert_true(json_data.length() > 0)
  
  // 反序列化JSON到批量数据
  let deserialized_batch = BatchSerializer::from_json(json_data)
  
  // 验证反序列化的数据
  assert_eq(BatchTelemetryData::spans(deserialized_batch).length(), 
            BatchTelemetryData::spans(batch_data).length())
  assert_eq(BatchTelemetryData::logs(deserialized_batch).length(), 
            BatchTelemetryData::logs(batch_data).length())
  assert_eq(BatchTelemetryData::scope_metrics(deserialized_batch).length(), 
            BatchTelemetryData::scope_metrics(batch_data).length())
  
  assert_true(true)
}

test "属性值类型序列化" {
  // 测试不同属性值类型的序列化
  let string_attr = AttributeValue::StringValue("test string")
  let int_attr = AttributeValue::IntValue(42)
  let float_attr = AttributeValue::FloatValue(3.14)
  let bool_attr = AttributeValue::BoolValue(true)
  let array_string_attr = AttributeValue::ArrayStringValue(["a", "b", "c"])
  let array_int_attr = AttributeValue::ArrayIntValue([1, 2, 3])
  
  // 序列化各种类型的属性值
  let string_json = AttributeValueSerializer::to_json(string_attr)
  let int_json = AttributeValueSerializer::to_json(int_attr)
  let float_json = AttributeValueSerializer::to_json(float_attr)
  let bool_json = AttributeValueSerializer::to_json(bool_attr)
  let array_string_json = AttributeValueSerializer::to_json(array_string_attr)
  let array_int_json = AttributeValueSerializer::to_json(array_int_attr)
  
  // 验证JSON不为空
  assert_true(string_json.length() > 0)
  assert_true(int_json.length() > 0)
  assert_true(float_json.length() > 0)
  assert_true(bool_json.length() > 0)
  assert_true(array_string_json.length() > 0)
  assert_true(array_int_json.length() > 0)
  
  // 反序列化属性值
  let deserialized_string = AttributeValueSerializer::from_json(string_json)
  let deserialized_int = AttributeValueSerializer::from_json(int_json)
  let deserialized_float = AttributeValueSerializer::from_json(float_json)
  let deserialized_bool = AttributeValueSerializer::from_json(bool_json)
  let deserialized_array_string = AttributeValueSerializer::from_json(array_string_json)
  let deserialized_array_int = AttributeValueSerializer::from_json(array_int_json)
  
  // 验证反序列化的值
  match (string_attr, deserialized_string) {
    (AttributeValue::StringValue(original), AttributeValue::StringValue(deserialized)) => 
      assert_eq(original, deserialized)
    _ => assert_true(false)
  }
  
  match (int_attr, deserialized_int) {
    (AttributeValue::IntValue(original), AttributeValue::IntValue(deserialized)) => 
      assert_eq(original, deserialized)
    _ => assert_true(false)
  }
  
  match (float_attr, deserialized_float) {
    (AttributeValue::FloatValue(original), AttributeValue::FloatValue(deserialized)) => 
      assert_eq(original, deserialized)
    _ => assert_true(false)
  }
  
  match (bool_attr, deserialized_bool) {
    (AttributeValue::BoolValue(original), AttributeValue::BoolValue(deserialized)) => 
      assert_eq(original, deserialized)
    _ => assert_true(false)
  }
  
  assert_true(true)
}