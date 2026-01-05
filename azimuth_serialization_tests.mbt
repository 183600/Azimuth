// Azimuth 数据序列化和反序列化测试
// 专注于遥测数据的序列化、反序列化和数据格式转换功能

// 测试1: JSON序列化和反序列化
test "JSON序列化和反序列化基础功能" {
  // 创建JSON序列化器
  let json_serializer = JsonSerializer::new()
  
  // 配置序列化选项
  JsonSerializer::configure(json_serializer, {
    pretty_print: false,
    include_null_values: false,
    date_format: "iso8601",
    max_depth: 32,
    field_naming_policy: "snake_case"
  })
  
  // 创建测试遥测数据
  let telemetry_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database.query",
    start_time: 1640995200000,  // 毫秒时间戳
    end_time: 1640995200250,
    status: "ok",
    service_name: "payment-service",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users WHERE id = $1"),
      ("db.instance", "payment-db-primary"),
      ("http.method", "POST"),
      ("http.url", "/api/payments"),
      ("http.status_code", 200),
      ("user.id", "user-12345"),
      ("payment.amount", 99.99),
      ("payment.currency", "USD"),
      ("retry.count", 0)
    ],
    events: [
      {
        name: "db.query.start",
        timestamp: 1640995200000,
        attributes: [
          ("db.statement", "SELECT * FROM users WHERE id = $1")
        ]
      },
      {
        name: "db.query.complete",
        timestamp: 1640995200200,
        attributes: [
          ("db.rows_affected", 1),
          ("db.execution_time", 200)
        ]
      }
    ],
    links: [
      {
        trace_id: "trace-54321",
        span_id: "span-98765",
        type: "follows_from"
      }
    ]
  }
  
  // 序列化为JSON
  let json_result = JsonSerializer::serialize(json_serializer, telemetry_span)
  assert_true(json_result.success)
  assert_true(json_result.data.length() > 0)
  
  // 验证JSON内容
  let json_data = json_result.data
  assert_true(json_data.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json_data.contains("\"span_id\":\"span-67890\""))
  assert_true(json_data.contains("\"operation_name\":\"database.query\""))
  assert_true(json_data.contains("\"service_name\":\"payment-service\""))
  assert_true(json_data.contains("\"status\":\"ok\""))
  
  // 验证属性序列化
  assert_true(json_data.contains("\"db.type\":\"postgresql\""))
  assert_true(json_data.contains("\"payment.amount\":99.99"))
  assert_true(json_data.contains("\"http.status_code\":200"))
  
  // 验证事件序列化
  assert_true(json_data.contains("\"name\":\"db.query.start\""))
  assert_true(json_data.contains("\"name\":\"db.query.complete\""))
  
  // 反序列化JSON
  let deserialize_result = JsonSerializer::deserialize(json_serializer, json_data, "TelemetrySpan")
  assert_true(deserialize_result.success)
  
  let deserialized_span = deserialize_result.data
  assert_eq(deserialized_span.trace_id, "trace-12345")
  assert_eq(deserialized_span.span_id, "span-67890")
  assert_eq(deserialized_span.operation_name, "database.query")
  assert_eq(deserialized_span.service_name, "payment-service")
  assert_eq(deserialized_span.status, "ok")
  
  // 验证属性反序列化
  let db_type = deserialized_span.attributes.find(fn(a) { a.0 == "db.type" })
  assert_true(db_type != None)
  match db_type {
    Some((key, StringValue(value))) => {
      assert_eq(key, "db.type")
      assert_eq(value, "postgresql")
    }
    _ => assert_true(false)
  }
  
  let payment_amount = deserialized_span.attributes.find(fn(a) { a.0 == "payment.amount" })
  assert_true(payment_amount != None)
  match payment_amount {
    Some((key, FloatValue(value))) => {
      assert_eq(key, "payment.amount")
      assert_eq(value, 99.99)
    }
    _ => assert_true(false)
  }
  
  // 验证事件反序列化
  assert_eq(deserialized_span.events.length(), 2)
  assert_eq(deserialized_span.events[0].name, "db.query.start")
  assert_eq(deserialized_span.events[1].name, "db.query.complete")
  
  // 验证链接反序列化
  assert_eq(deserialized_span.links.length(), 1)
  assert_eq(deserialized_span.links[0].trace_id, "trace-54321")
  assert_eq(deserialized_span.links[0].span_id, "span-98765")
  
  // 测试复杂嵌套结构序列化
  let complex_data = {
    spans: [telemetry_span],
    metrics: [
      {
        name: "http.requests.total",
        value: 1000.0,
        unit: "count",
        timestamp: 1640995200000,
        attributes: [
          ("service.name", "payment-service"),
          ("http.method", "POST"),
          ("http.status_code", 200)
        ]
      },
      {
        name: "http.request.duration",
        value: 250.5,
        unit: "milliseconds",
        timestamp: 1640995200000,
        attributes: [
          ("service.name", "payment-service"),
          ("http.method", "POST"),
          ("endpoint", "/api/payments")
        ]
      }
    ],
    logs: [
      {
        timestamp: 1640995200000,
        severity: "info",
        message: "Payment processing started",
        attributes: [
          ("service.name", "payment-service"),
          ("trace_id", "trace-12345"),
          ("user.id", "user-12345")
        ]
      },
      {
        timestamp: 1640995200250,
        severity: "info",
        message: "Payment processing completed",
        attributes: [
          ("service.name", "payment-service"),
          ("trace_id", "trace-12345"),
          ("payment.id", "pay-98765"),
          ("payment.amount", 99.99)
        ]
      }
    ],
    resource: {
      attributes: [
        ("service.name", "payment-service"),
        ("service.version", "1.2.3"),
        ("deployment.environment", "production"),
        ("host.name", "payment-server-01"),
        ("host.ip", "10.0.1.100"),
        ("os.type", "linux"),
        ("os.version", "5.4.0-74-generic"),
        ("process.pid", 12345),
        ("process.executable.name", "payment-service"),
        ("process.command_args", ["--config", "/etc/payment-service/config.yaml"])
      ]
    }
  }
  
  // 序列化复杂数据
  let complex_json_result = JsonSerializer::serialize(json_serializer, complex_data)
  assert_true(complex_json_result.success)
  assert_true(complex_json_result.data.length() > json_result.data.length())
  
  // 反序列化复杂数据
  let complex_deserialize_result = JsonSerializer::deserialize(json_serializer, complex_json_result.data, "TelemetryData")
  assert_true(complex_deserialize_result.success)
  
  let complex_deserialized = complex_deserialize_result.data
  assert_eq(complex_deserialized.spans.length(), 1)
  assert_eq(complex_deserialized.metrics.length(), 2)
  assert_eq(complex_deserialized.logs.length(), 2)
  assert_true(complex_deserialized.resource != None)
  
  // 测试序列化性能
  let performance_span = telemetry_span
  let iterations = 1000
  
  let serialize_start_time = Time::now()
  for i in 0..=iterations {
    JsonSerializer::serialize(json_serializer, performance_span)
  }
  let serialize_duration = Time::now() - serialize_start_time
  let serialize_avg = serialize_duration / iterations
  
  assert_true(serialize_avg < 10.0)  // 平均序列化时间应该<10ms
  
  // 测试反序列化性能
  let test_json = json_result.data
  let deserialize_start_time = Time::now()
  for i in 0..=iterations {
    JsonSerializer::deserialize(json_serializer, test_json, "TelemetrySpan")
  }
  let deserialize_duration = Time::now() - deserialize_start_time
  let deserialize_avg = deserialize_duration / iterations
  
  assert_true(deserialize_avg < 15.0)  // 平均反序列化时间应该<15ms
}

// 测试2: Protocol Buffers序列化
test "Protocol Buffers序列化功能" {
  // 创建Protobuf序列化器
  let protobuf_serializer = ProtobufSerializer::new()
  
  // 配置序列化选项
  ProtobufSerializer::configure(protobuf_serializer, {
    use_default_values: false,
    validate_required_fields: true,
    compression_enabled: false,
    max_message_size: 1048576  // 1MB
  })
  
  // 创建测试数据
  let telemetry_data = {
    resource: {
      attributes: [
        ("service.name", "order-service"),
        ("service.version", "2.1.0"),
        ("deployment.environment", "production"),
        ("host.name", "order-server-03")
      ]
    },
    instrumentation_scope: {
      name: "order-service-instrumentation",
      version: "1.0.0"
    },
    spans: [
      {
        trace_id: "trace-order-123",
        span_id: "span-order-456",
        parent_span_id: Some("span-api-789"),
        name: "order.process",
        kind: "server",
        start_time_unix_nano: 1640995200000000000,
        end_time_unix_nano: 1640995200350000000,
        status: {
          code: "ok",
          message: ""
        },
        attributes: [
          ("order.id", "order-12345"),
          ("user.id", "user-67890"),
          ("order.amount", 149.99),
          ("order.currency", "EUR"),
          ("payment.method", "credit_card")
        ],
        events: [
          {
            time_unix_nano: 1640995200100000000,
            name: "validation.start",
            attributes: [
              ("validation.type", "order_data")
            ]
          },
          {
            time_unix_nano: 1640995200200000000,
            name: "validation.complete",
            attributes: [
              ("validation.result", "success"),
              ("validation.duration_ms", 100
            ]
          },
          {
            time_unix_nano: 1640995200250000000,
            name: "inventory.check",
            attributes: [
              ("product.id", "prod-111"),
              ("quantity", 2),
              ("available", true)
            ]
          },
          {
            time_unix_nano: 1640995200300000000,
            name: "payment.process",
            attributes: [
              ("payment.gateway", "stripe"),
              ("payment.transaction_id", "txn-99999")
            ]
          }
        ],
        links: [
          {
            trace_id: "trace-inventory-111",
            span_id: "span-inventory-222",
            attributes: [
              ("link.type", "inventory_reservation")
            ]
          }
        ]
      }
    ],
    metrics: [
      {
        name: "order.processing.duration",
        unit: "ms",
        description: "Order processing duration in milliseconds",
        data: {
          histogram: {
            data_points: [
              {
                start_time_unix_nano: 1640995200000000000,
                time_unix_nano: 1640995200600000000,
                count: 100,
                sum: 25000.0,
                bucket_counts: [10, 25, 40, 20, 5],
                explicit_bounds: [50.0, 100.0, 200.0, 500.0]
              }
            ]
          }
        }
      },
      {
        name: "orders.total",
        unit: "count",
        description: "Total number of orders processed",
        data: {
          sum: {
            data_points: [
              {
                start_time_unix_nano: 1640995200000000000,
                time_unix_nano: 1640995200600000000,
                value: 100.0,
                attributes: [
                  ("order.status", "completed"),
                  ("payment.method", "credit_card")
                ]
              },
              {
                start_time_unix_nano: 1640995200000000000,
                time_unix_nano: 1640995200600000000,
                value: 25.0,
                attributes: [
                  ("order.status", "completed"),
                  ("payment.method", "paypal")
                ]
              }
            ]
          }
        }
      }
    ],
    logs: [
      {
        time_unix_nano: 1640995200050000000,
        severity_number: "info",
        severity_text: "INFO",
        body: {
          string_value: "Order processing started"
        },
        attributes: [
          ("order.id", "order-12345"),
          ("user.id", "user-67890")
        ]
      },
      {
        time_unix_nano: 1640995200150000000,
        severity_number: "warn",
        severity_text: "WARN",
        body: {
          string_value: "Inventory level low for product"
        },
        attributes: [
          ("product.id", "prod-111"),
          ("current_stock", 5),
          ("reorder_threshold", 10)
        ]
      },
      {
        time_unix_nano: 1640995200350000000,
        severity_number: "info",
        severity_text: "INFO",
        body: {
          string_value: "Order processing completed successfully"
        },
        attributes: [
          ("order.id", "order-12345"),
          ("payment.transaction_id", "txn-99999")
        ]
      }
    ]
  }
  
  // 序列化为Protobuf
  let protobuf_result = ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
  assert_true(protobuf_result.success)
  assert_true(protobuf_result.data.length() > 0)
  
  // 验证Protobuf二进制数据
  let protobuf_data = protobuf_result.data
  assert_true(protobuf_data.length() > 0)
  
  // 反序列化Protobuf
  let deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, protobuf_data, "TelemetryData")
  assert_true(deserialize_result.success)
  
  let deserialized_data = deserialize_result.data
  assert_eq(deserialized_data.resource.attributes.length(), 4)
  assert_eq(deserialized_data.spans.length(), 1)
  assert_eq(deserialized_data.metrics.length(), 2)
  assert_eq(deserialized_data.logs.length(), 3)
  
  // 验证Span反序列化
  let span = deserialized_data.spans[0]
  assert_eq(span.trace_id, "trace-order-123")
  assert_eq(span.span_id, "span-order-456")
  assert_eq(span.name, "order.process")
  assert_eq(span.kind, "server")
  assert_eq(span.status.code, "ok")
  
  // 验证Span属性
  let order_id = span.attributes.find(fn(a) { a.key == "order.id" })
  assert_true(order_id != None)
  match order_id {
    Some(attr) => {
      assert_eq(attr.key, "order.id")
      assert_eq(attr.value.string_value, "order-12345")
    }
    None => assert_true(false)
  }
  
  let order_amount = span.attributes.find(fn(a) { a.key == "order.amount" })
  assert_true(order_amount != None)
  match order_amount {
    Some(attr) => {
      assert_eq(attr.key, "order.amount")
      assert_eq(attr.value.double_value, 149.99)
    }
    None => assert_true(false)
  }
  
  // 验证Span事件
  assert_eq(span.events.length(), 4)
  assert_eq(span.events[0].name, "validation.start")
  assert_eq(span.events[1].name, "validation.complete")
  assert_eq(span.events[2].name, "inventory.check")
  assert_eq(span.events[3].name, "payment.process")
  
  // 验证Histogram指标
  let histogram_metric = deserialized_data.metrics.find(fn(m) { m.name == "order.processing.duration" })
  assert_true(histogram_metric != None)
  
  match histogram_metric {
    Some(metric) => {
      assert_eq(metric.unit, "ms")
      match metric.data {
        HistogramData(histogram) => {
          assert_eq(histogram.data_points.length(), 1)
          let data_point = histogram.data_points[0]
          assert_eq(data_point.count, 100)
          assert_eq(data_point.sum, 25000.0)
          assert_eq(data_point.bucket_counts.length(), 5)
          assert_eq(data_point.explicit_bounds.length(), 4)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证Sum指标
  let sum_metric = deserialized_data.metrics.find(fn(m) { m.name == "orders.total" })
  assert_true(sum_metric != None)
  
  match sum_metric {
    Some(metric) => {
      match metric.data {
        SumData(sum) => {
          assert_eq(sum.data_points.length(), 2)
          let credit_card_point = sum.data_points.find(fn(dp) { 
            dp.attributes.any(fn(a) { a.key == "payment.method" and a.value.string_value == "credit_card" })
          })
          assert_true(credit_card_point != None)
          match credit_card_point {
            Some(point) => assert_eq(point.value, 100.0)
            None => assert_true(false)
          }
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证日志
  let warn_log = deserialized_data.logs.find(fn(l) { l.severity_text == "WARN" })
  assert_true(warn_log != None)
  
  match warn_log {
    Some(log) => {
      assert_eq(log.body.string_value, "Inventory level low for product")
      let product_id = log.attributes.find(fn(a) { a.key == "product.id" })
      assert_true(product_id != None)
      match product_id {
        Some(attr) => assert_eq(attr.value.string_value, "prod-111")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试压缩序列化
  ProtobufSerializer::enable_compression(protobuf_serializer, true)
  
  let compressed_result = ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
  assert_true(compressed_result.success)
  assert_true(compressed_result.data.length() < protobuf_result.data.length())  // 压缩后应该更小
  
  // 验证压缩数据可以正确反序列化
  let compressed_deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, compressed_result.data, "TelemetryData")
  assert_true(compressed_deserialize_result.success)
  
  // 测试序列化大小比较
  let json_serializer_for_comparison = JsonSerializer::new()
  let json_result_for_comparison = JsonSerializer::serialize(json_serializer_for_comparison, telemetry_data)
  
  let size_ratio = protobuf_result.data.length().to_float() / json_result_for_comparison.data.length().to_float()
  assert_true(size_ratio < 0.8)  // Protobuf应该比JSON小至少20%
  
  // 测试批量序列化性能
  let batch_data = []
  for i in 0..=100 {
    batch_data.push(telemetry_data)
  }
  
  let batch_serialize_start = Time::now()
  let batch_results = ProtobufSerializer::serialize_batch(protobuf_serializer, batch_data)
  let batch_serialize_duration = Time::now() - batch_serialize_start
  
  assert_eq(batch_results.length(), 101)
  assert_true(batch_results.all(fn(r) { r.success }))
  assert_true(batch_serialize_duration < 1000.0)  // 批量序列化应该在1秒内完成
}

// 测试3: Avro序列化
test "Avro序列化功能" {
  // 创建Avro序列化器
  let avro_serializer = AvroSerializer::new()
  
  // 定义Avro Schema
  let telemetry_schema = {
    "type": "record",
    "name": "TelemetryEvent",
    "namespace": "com.azimuth.telemetry",
    "fields": [
      {
        "name": "event_id",
        "type": "string"
      },
      {
        "name": "timestamp",
        "type": "long"
      },
      {
        "name": "event_type",
        "type": {
          "type": "enum",
          "name": "EventType",
          "symbols": ["span", "metric", "log"]
        }
      },
      {
        "name": "service_name",
        "type": "string"
      },
      {
        "name": "trace_id",
        "type": ["null", "string"],
        "default": null
      },
      {
        "name": "attributes",
        "type": {
          "type": "map",
          "values": "string"
        }
      },
      {
        "name": "payload",
        "type": ["null", {
          "type": "record",
          "name": "SpanPayload",
          "fields": [
            {
              "name": "span_id",
              "type": "string"
            },
            {
              "name": "parent_span_id",
              "type": ["null", "string"],
              "default": null
            },
            {
              "name": "operation_name",
              "type": "string"
            },
            {
              "name": "duration_ms",
              "type": "int"
            },
            {
              "name": "status",
              "type": {
                "type": "enum",
                "name": "SpanStatus",
                "symbols": ["ok", "error", "timeout"]
              }
            }
          ]
        }, {
          "type": "record",
          "name": "MetricPayload",
          "fields": [
            {
              "name": "metric_name",
              "type": "string"
            },
            {
              "name": "value",
              "type": "double"
            },
            {
              "name": "unit",
              "type": "string"
            }
          ]
        }, {
          "type": "record",
          "name": "LogPayload",
          "fields": [
            {
              "name": "severity",
              "type": {
                "type": "enum",
                "name": "LogSeverity",
                "symbols": ["trace", "debug", "info", "warn", "error", "fatal"]
              }
            },
            {
              "name": "message",
              "type": "string"
            }
          ]
        }],
        "default": null
      }
    ]
  }
  
  // 注册Schema
  AvroSerializer::register_schema(avro_serializer, "TelemetryEvent", telemetry_schema)
  
  // 创建测试数据
  let span_event = {
    event_id: "event-12345",
    timestamp: 1640995200000,
    event_type: "span",
    service_name: "user-service",
    trace_id: Some("trace-54321"),
    attributes: {
      "user.id": "user-98765",
      "http.method": "GET",
      "http.url": "/api/users/98765",
      "http.status_code": "200"
    },
    payload: Some({
      span_id: "span-11111",
      parent_span_id: Some("span-22222"),
      operation_name: "user.get_profile",
      duration_ms: 150,
      status: "ok"
    })
  }
  
  let metric_event = {
    event_id: "event-67890",
    timestamp: 1640995200000,
    event_type: "metric",
    service_name: "user-service",
    trace_id: None,
    attributes: {
      "metric.type": "counter",
      "component": "database"
    },
    payload: Some({
      metric_name: "database.queries.total",
      value: 1250.0,
      unit: "count"
    })
  }
  
  let log_event = {
    event_id: "event-11111",
    timestamp: 1640995200000,
    event_type: "log",
    service_name: "user-service",
    trace_id: Some("trace-54321"),
    attributes: {
      "logger": "com.example.UserService",
      "thread": "worker-thread-3"
    },
    payload: Some({
      severity: "info",
      message: "User profile retrieved successfully"
    })
  }
  
  // 序列化Span事件
  let span_serialize_result = AvroSerializer::serialize(avro_serializer, span_event, "TelemetryEvent")
  assert_true(span_serialize_result.success)
  assert_true(span_serialize_result.data.length() > 0)
  
  // 序列化Metric事件
  let metric_serialize_result = AvroSerializer::serialize(avro_serializer, metric_event, "TelemetryEvent")
  assert_true(metric_serialize_result.success)
  assert_true(metric_serialize_result.data.length() > 0)
  
  // 序列化Log事件
  let log_serialize_result = AvroSerializer::serialize(avro_serializer, log_event, "TelemetryEvent")
  assert_true(log_serialize_result.success)
  assert_true(log_serialize_result.data.length() > 0)
  
  // 反序列化Span事件
  let span_deserialize_result = AvroSerializer::deserialize(avro_serializer, span_serialize_result.data, "TelemetryEvent")
  assert_true(span_deserialize_result.success)
  
  let deserialized_span = span_deserialize_result.data
  assert_eq(deserialized_span.event_id, "event-12345")
  assert_eq(deserialized_span.event_type, "span")
  assert_eq(deserialized_span.service_name, "user-service")
  assert_eq(deserialized_span.trace_id, Some("trace-54321"))
  
  // 验证属性
  assert_eq(deserialized_span.attributes.get("user.id"), Some("user-98765"))
  assert_eq(deserialized_span.attributes.get("http.method"), Some("GET"))
  
  // 验证Span payload
  assert_true(deserialized_span.payload != None)
  match deserialized_span.payload {
    Some(SpanPayload(span_payload)) => {
      assert_eq(span_payload.span_id, "span-11111")
      assert_eq(span_payload.parent_span_id, Some("span-22222"))
      assert_eq(span_payload.operation_name, "user.get_profile")
      assert_eq(span_payload.duration_ms, 150)
      assert_eq(span_payload.status, "ok")
    }
    _ => assert_true(false)
  }
  
  // 反序列化Metric事件
  let metric_deserialize_result = AvroSerializer::deserialize(avro_serializer, metric_serialize_result.data, "TelemetryEvent")
  assert_true(metric_deserialize_result.success)
  
  let deserialized_metric = metric_deserialize_result.data
  assert_eq(deserialized_metric.event_type, "metric")
  assert_eq(deserialized_metric.trace_id, None)
  
  // 验证Metric payload
  assert_true(deserialized_metric.payload != None)
  match deserialized_metric.payload {
    Some(MetricPayload(metric_payload)) => {
      assert_eq(metric_payload.metric_name, "database.queries.total")
      assert_eq(metric_payload.value, 1250.0)
      assert_eq(metric_payload.unit, "count")
    }
    _ => assert_true(false)
  }
  
  // 反序列化Log事件
  let log_deserialize_result = AvroSerializer::deserialize(avro_serializer, log_serialize_result.data, "TelemetryEvent")
  assert_true(log_deserialize_result.success)
  
  let deserialized_log = log_deserialize_result.data
  assert_eq(deserialized_log.event_type, "log")
  
  // 验证Log payload
  assert_true(deserialized_log.payload != None)
  match deserialized_log.payload {
    Some(LogPayload(log_payload)) => {
      assert_eq(log_payload.severity, "info")
      assert_eq(log_payload.message, "User profile retrieved successfully")
    }
    _ => assert_true(false)
  }
  
  // 测试Schema演进
  let evolved_schema = {
    "type": "record",
    "name": "TelemetryEvent",
    "namespace": "com.azimuth.telemetry",
    "fields": [
      {
        "name": "event_id",
        "type": "string"
      },
      {
        "name": "timestamp",
        "type": "long"
      },
      {
        "name": "event_type",
        "type": {
          "type": "enum",
          "name": "EventType",
          "symbols": ["span", "metric", "log", "trace"]
        }
      },
      {
        "name": "service_name",
        "type": "string"
      },
      {
        "name": "trace_id",
        "type": ["null", "string"],
        "default": null
      },
      {
        "name": "attributes",
        "type": {
          "type": "map",
          "values": "string"
        }
      },
      {
        "name": "version",
        "type": "int",
        "default": 1
      },
      {
        "name": "payload",
        "type": ["null", {
          "type": "record",
          "name": "SpanPayload",
          "fields": [
            {
              "name": "span_id",
              "type": "string"
            },
            {
              "name": "parent_span_id",
              "type": ["null", "string"],
              "default": null
            },
            {
              "name": "operation_name",
              "type": "string"
            },
            {
              "name": "duration_ms",
              "type": "int"
            },
            {
              "name": "status",
              "type": {
                "type": "enum",
                "name": "SpanStatus",
                "symbols": ["ok", "error", "timeout"]
              }
            },
            {
              "name": "service_version",
              "type": ["null", "string"],
              "default": null
            }
          ]
        }, {
          "type": "record",
          "name": "MetricPayload",
          "fields": [
            {
              "name": "metric_name",
              "type": "string"
            },
            {
              "name": "value",
              "type": "double"
            },
            {
              "name": "unit",
              "type": "string"
            }
          ]
        }, {
          "type": "record",
          "name": "LogPayload",
          "fields": [
            {
              "name": "severity",
              "type": {
                "type": "enum",
                "name": "LogSeverity",
                "symbols": ["trace", "debug", "info", "warn", "error", "fatal"]
              }
            },
            {
              "name": "message",
              "type": "string"
            }
          ]
        }],
        "default": null
      }
    ]
  }
  
  // 注册新Schema
  AvroSerializer::register_schema(avro_serializer, "TelemetryEventV2", evolved_schema)
  
  // 使用旧Schema序列化的数据应该能用新Schema反序列化
  let schema_evolution_result = AvroSerializer::deserialize_with_schema(avro_serializer, span_serialize_result.data, "TelemetryEventV2")
  assert_true(schema_evolution_result.success)
  
  let evolved_event = schema_evolution_result.data
  assert_eq(evolved_event.event_id, "event-12345")
  assert_eq(evolved_event.version, 1)  // 应该使用默认值
  
  // 测试批量序列化
  let events = [span_event, metric_event, log_event]
  let batch_serialize_result = AvroSerializer::serialize_batch(avro_serializer, events, "TelemetryEvent")
  assert_true(batch_serialize_result.success)
  assert_true(batch_serialize_result.data.length() > 0)
  
  // 测试批量反序列化
  let batch_deserialize_result = AvroSerializer::deserialize_batch(avro_serializer, batch_serialize_result.data, "TelemetryEvent")
  assert_true(batch_deserialize_result.success)
  assert_eq(batch_deserialize_result.data.length(), 3)
  
  // 验证批量反序列化结果
  let batch_events = batch_deserialize_result.data
  assert_eq(batch_events[0].event_type, "span")
  assert_eq(batch_events[1].event_type, "metric")
  assert_eq(batch_events[2].event_type, "log")
}

// 测试4: MessagePack序列化
test "MessagePack序列化功能" {
  // 创建MessagePack序列化器
  let msgpack_serializer = MessagePackSerializer::new()
  
  // 配置序列化选项
  MessagePackSerializer::configure(msgpack_serializer, {
    use_str8_format: true,
    use_bin_format: true,
    compatibility_mode: "none",
    timestamp_format: "unix_timestamp"
  })
  
  // 创建测试数据
  let telemetry_batch = {
    batch_id: "batch-12345",
    timestamp: 1640995200,
    source: "telemetry-collector-01",
    version: "1.0.0",
    spans: [
      {
        trace_id: "trace-abc",
        span_id: "span-def",
        parent_span_id: None,
        operation: "http.request",
        start_time: 1640995200000,
        duration: 125000000,  // 纳秒
        tags: {
          "http.method": "GET",
          "http.url": "/api/users",
          "http.status_code": 200,
          "service.name": "user-service",
          "component": "http-server"
        },
        logs: [
          {
            timestamp: 1640995200100000000,
            level: "info",
            message: "Request started"
          },
          {
            timestamp: 1640995200125000000,
            level: "info",
            message: "Request completed"
          }
        ]
      },
      {
        trace_id: "trace-ghi",
        span_id: "span-jkl",
        parent_span_id: Some("span-mno"),
        operation: "database.query",
        start_time: 1640995201000,
        duration: 45000000,  // 纳秒
        tags: {
          "db.type": "postgresql",
          "db.statement": "SELECT * FROM orders WHERE user_id = $1",
          "db.instance": "orders-db",
          "service.name": "order-service"
        },
        logs: [
          {
            timestamp: 1640995201020000000,
            level: "debug",
            message: "Executing query"
          },
          {
            timestamp: 1640995201065000000,
            level: "info",
            message: "Query completed",
            fields: {
              "rows": 5,
              "duration_ms": 45
            }
          }
        ]
      }
    ],
    metrics: [
      {
        name: "http.requests.total",
        value: 1250,
        timestamp: 1640995200,
        tags: {
          "service": "user-service",
          "method": "GET",
          "status": "200"
        },
        type: "counter"
      },
      {
        name: "http.request.duration",
        value: 125.5,
        timestamp: 1640995200,
        tags: {
          "service": "user-service",
          "method": "GET",
          "endpoint": "/api/users"
        },
        type: "gauge"
      },
      {
        name: "database.connections.active",
        value: 15,
        timestamp: 1640995200,
        tags: {
          "service": "order-service",
          "db.instance": "orders-db"
        },
        type: "gauge"
      }
    ]
  }
  
  // 序列化为MessagePack
  let msgpack_result = MessagePackSerializer::serialize(msgpack_serializer, telemetry_batch)
  assert_true(msgpack_result.success)
  assert_true(msgpack_result.data.length() > 0)
  
  // 验证MessagePack二进制数据
  let msgpack_data = msgpack_result.data
  assert_true(msgpack_data.length() > 0)
  
  // 反序列化MessagePack
  let deserialize_result = MessagePackSerializer::deserialize(msgpack_serializer, msgpack_data, "TelemetryBatch")
  assert_true(deserialize_result.success)
  
  let deserialized_batch = deserialize_result.data
  assert_eq(deserialized_batch.batch_id, "batch-12345")
  assert_eq(deserialized_batch.source, "telemetry-collector-01")
  assert_eq(deserialized_batch.version, "1.0.0")
  assert_eq(deserialized_batch.spans.length(), 2)
  assert_eq(deserialized_batch.metrics.length(), 3)
  
  // 验证Span反序列化
  let first_span = deserialized_batch.spans[0]
  assert_eq(first_span.trace_id, "trace-abc")
  assert_eq(first_span.span_id, "span-def")
  assert_eq(first_span.operation, "http.request")
  assert_eq(first_span.duration, 125000000)
  
  // 验证Span标签
  assert_eq(first_span.tags.get("http.method"), Some("GET"))
  assert_eq(first_span.tags.get("http.status_code"), Some("200"))
  assert_eq(first_span.tags.get("service.name"), Some("user-service"))
  
  // 验证Span日志
  assert_eq(first_span.logs.length(), 2)
  assert_eq(first_span.logs[0].level, "info")
  assert_eq(first_span.logs[0].message, "Request started")
  
  // 验证Metric反序列化
  let counter_metric = deserialized_batch.metrics.find(fn(m) { m.name == "http.requests.total" })
  assert_true(counter_metric != None)
  
  match counter_metric {
    Some(metric) => {
      assert_eq(metric.value, 1250)
      assert_eq(metric.type, "counter")
      assert_eq(metric.tags.get("service"), Some("user-service"))
    }
    None => assert_true(false)
  }
  
  let gauge_metric = deserialized_batch.metrics.find(fn(m) { m.name == "http.request.duration" })
  assert_true(gauge_metric != None)
  
  match gauge_metric {
    Some(metric) => {
      assert_eq(metric.value, 125.5)
      assert_eq(metric.type, "gauge")
    }
    None => assert_true(false)
  }
  
  // 测试流式序列化
  let stream_serializer = MessagePackSerializer::create_stream_serializer(msgpack_serializer)
  
  // 写入数组头部
  MessagePackSerializer::stream_write_array_header(stream_serializer, 3)
  
  // 写入元素
  MessagePackSerializer::stream_write_string(stream_serializer, "item1")
  MessagePackSerializer::stream_write_integer(stream_serializer, 42)
  MessagePackSerializer::stream_write_boolean(stream_serializer, true)
  
  // 获取流式序列化结果
  let stream_result = MessagePackSerializer::stream_finish(stream_serializer)
  assert_true(stream_result.success)
  
  // 测试流式反序列化
  let stream_deserializer = MessagePackSerializer::create_stream_deserializer(msgpack_serializer, stream_result.data)
  
  // 读取数组
  let array_result = MessagePackSerializer::stream_read_array(stream_deserializer)
  assert_true(array_result.success)
  assert_eq(array_result.value, 3)
  
  // 读取元素
  let string_result = MessagePackSerializer::stream_read_string(stream_deserializer)
  assert_true(string_result.success)
  assert_eq(string_result.value, "item1")
  
  let int_result = MessagePackSerializer::stream_read_integer(stream_deserializer)
  assert_true(int_result.success)
  assert_eq(int_result.value, 42)
  
  let bool_result = MessagePackSerializer::stream_read_boolean(stream_deserializer)
  assert_true(bool_result.success)
  assert_eq(bool_result.value, true)
  
  // 测试序列化大小比较
  let json_serializer_for_comparison = JsonSerializer::new()
  let json_result_for_comparison = JsonSerializer::serialize(json_serializer_for_comparison, telemetry_batch)
  
  let size_ratio = msgpack_result.data.length().to_float() / json_result_for_comparison.data.length().to_float()
  assert_true(size_ratio < 0.7)  // MessagePack应该比JSON小至少30%
  
  // 测试序列化性能
  let iterations = 1000
  let performance_data = telemetry_batch
  
  let msgpack_start_time = Time::now()
  for i in 0..=iterations {
    MessagePackSerializer::serialize(msgpack_serializer, performance_data)
  }
  let msgpack_duration = Time::now() - msgpack_start_time
  let msgpack_avg = msgpack_duration / iterations
  
  let json_start_time = Time::now()
  for i in 0..=iterations {
    JsonSerializer::serialize(json_serializer_for_comparison, performance_data)
  }
  let json_duration = Time::now() - json_start_time
  let json_avg = json_duration / iterations
  
  assert_true(msgpack_avg < json_avg)  // MessagePack应该比JSON快
}

// 测试5: 自定义序列化格式
test "自定义序列化格式功能" {
  // 创建自定义序列化器
  let custom_serializer = CustomSerializer::new()
  
  // 注册自定义格式
  CustomSerializer::register_format(custom_serializer, "telemetry_binary", {
    header_size: 16,
    version: 1,
    compression: "lz4",
    checksum: "crc32",
    field_encoding: "varint"
  })
  
  // 定义字段映射
  CustomSerializer::define_field_mapping(custom_serializer, "telemetry_binary", {
    "trace_id": {
      field_id: 1,
      data_type: "string",
      required: true
    },
    "span_id": {
      field_id: 2,
      data_type: "string",
      required: true
    },
    "parent_span_id": {
      field_id: 3,
      data_type: "optional_string",
      required: false
    },
    "operation_name": {
      field_id: 4,
      data_type: "string",
      required: true
    },
    "start_time": {
      field_id: 5,
      data_type: "timestamp",
      required: true
    },
    "duration": {
      field_id: 6,
      data_type: "duration",
      required: true
    },
    "status": {
      field_id: 7,
      data_type: "enum",
      required: true,
      enum_values: ["ok", "error", "timeout"]
    },
    "service_name": {
      field_id: 8,
      data_type: "string",
      required: true
    },
    "attributes": {
      field_id: 9,
      data_type: "key_value_map",
      required: false,
      key_type: "string",
      value_types: ["string", "int", "float", "bool"]
    }
  })
  
  // 创建测试数据
  let telemetry_span = {
    trace_id: "trace-custom-123",
    span_id: "span-custom-456",
    parent_span_id: Some("span-custom-789"),
    operation_name: "custom.operation",
    start_time: 1640995200000,
    duration: 250000000,  // 纳秒
    status: "ok",
    service_name: "custom-service",
    attributes: {
      "custom.field1": "value1",
      "custom.field2": 42,
      "custom.field3": 3.14,
      "custom.field4": true
    }
  }
  
  // 序列化为自定义格式
  let custom_serialize_result = CustomSerializer::serialize(custom_serializer, telemetry_span, "telemetry_binary")
  assert_true(custom_serialize_result.success)
  assert_true(custom_serialize_result.data.length() > 0)
  
  // 验证二进制头部
  let binary_data = custom_serialize_result.data
  assert_true(binary_data.length() >= 16)  // 至少包含头部
  
  // 验证版本标识
  let version = binary_data[0]
  assert_eq(version, 1)
  
  // 验证格式标识
  let format_id = binary_data.slice(1, 4)
  assert_true(format_id.length() == 3)
  
  // 反序列化自定义格式
  let custom_deserialize_result = CustomSerializer::deserialize(custom_serializer, binary_data, "telemetry_binary")
  assert_true(custom_deserialize_result.success)
  
  let deserialized_span = custom_deserialize_result.data
  assert_eq(deserialized_span.trace_id, "trace-custom-123")
  assert_eq(deserialized_span.span_id, "span-custom-456")
  assert_eq(deserialized_span.operation_name, "custom.operation")
  assert_eq(deserialized_span.status, "ok")
  assert_eq(deserialized_span.service_name, "custom-service")
  
  // 验证属性
  assert_eq(deserialized_span.attributes.get("custom.field1"), Some("value1"))
  assert_eq(deserialized_span.attributes.get("custom.field2"), Some(42))
  assert_eq(deserialized_span.attributes.get("custom.field3"), Some(3.14))
  assert_eq(deserialized_span.attributes.get("custom.field4"), Some(true))
  
  // 测试自定义序列化器的高级功能
  CustomSerializer::enable_field_compression(custom_serializer, "telemetry_binary", ["attributes"])
  CustomSerializer::enable_field_encryption(custom_serializer, "telemetry_binary", ["trace_id", "span_id"], {
    algorithm: "aes256",
    key_id: "encryption-key-123"
  })
  
  // 使用高级功能序列化
  let advanced_serialize_result = CustomSerializer::serialize(custom_serializer, telemetry_span, "telemetry_binary")
  assert_true(advanced_serialize_result.success)
  
  // 验证压缩和加密
  let advanced_data = advanced_serialize_result.data
  assert_true(advanced_data.length() > custom_serialize_result.data.length())  // 加密可能增加大小
  
  // 使用高级功能反序列化
  let advanced_deserialize_result = CustomSerializer::deserialize(custom_serializer, advanced_data, "telemetry_binary")
  assert_true(advanced_deserialize_result.success)
  
  let advanced_deserialized_span = advanced_deserialize_result.data
  assert_eq(advanced_deserialized_span.trace_id, "trace-custom-123")
  assert_eq(advanced_deserialized_span.span_id, "span-custom-456")
  
  // 测试序列化兼容性检查
  let compatibility_result = CustomSerializer::check_compatibility(custom_serializer, {
    source_format: "telemetry_binary",
    target_format: "telemetry_binary",
    source_version: 1,
    target_version: 2
  })
  
  assert_true(compatibility_result.compatible)
  assert_true(compatibility_result.warnings.length() >= 0)
  
  // 测试格式迁移
  CustomSerializer::register_format(custom_serializer, "telemetry_binary_v2", {
    header_size: 20,
    version: 2,
    compression: "zstd",
    checksum: "sha256",
    field_encoding: "fixed_length"
  })
  
  let migration_result = CustomSerializer::migrate_data(custom_serializer, {
    data: custom_serialize_result.data,
    source_format: "telemetry_binary",
    target_format: "telemetry_binary_v2"
  })
  
  assert_true(migration_result.success)
  assert_true(migration_result.migrated_data.length() > 0)
  
  // 验证迁移后的数据可以正确反序列化
  let migrated_deserialize_result = CustomSerializer::deserialize(custom_serializer, migration_result.migrated_data, "telemetry_binary_v2")
  assert_true(migrated_deserialize_result.success)
  
  let migrated_span = migrated_deserialize_result.data
  assert_eq(migrated_span.trace_id, "trace-custom-123")
  assert_eq(migrated_span.operation_name, "custom.operation")
  
  // 测试序列化统计
  let serialization_stats = CustomSerializer::get_serialization_stats(custom_serializer)
  assert_true(serialization_stats.total_serializations > 0)
  assert_true(serialization_stats.total_deserializations > 0)
  assert_true(serialization_stats.format_stats.contains("telemetry_binary"))
  
  let format_stats = serialization_stats.format_stats.get("telemetry_binary")
  assert_true(format_stats != None)
  
  match format_stats {
    Some(stats) => {
      assert_true(stats.total_operations > 0)
      assert_true(stats.average_size > 0)
      assert_true(stats.average_duration > 0.0)
    }
    None => assert_true(false)
  }
}

// 测试6: 序列化格式转换
test "序列化格式转换功能" {
  // 创建格式转换器
  let format_converter = FormatConverter::new()
  
  // 注册支持的格式
  FormatConverter::register_format(format_converter, "json", JsonSerializer::new())
  FormatConverter::register_format(format_converter, "protobuf", ProtobufSerializer::new())
  FormatConverter::register_format(format_converter, "avro", AvroSerializer::new())
  FormatConverter::register_format(format_converter, "msgpack", MessagePackSerializer::new())
  
  // 创建测试数据
  let test_data = {
    spans: [
      {
        trace_id: "trace-conversion-123",
        span_id: "span-conversion-456",
        operation_name: "conversion.test",
        start_time: 1640995200000,
        duration: 100000000,
        status: "ok",
        service_name: "conversion-service",
        attributes: {
          "test.field1": "value1",
          "test.field2": 123,
          "test.field3": 45.67
        }
      }
    ],
    metrics: [
      {
        name: "conversion.test.metric",
        value: 42.0,
        unit: "count",
        timestamp: 1640995200000,
        attributes: {
          "test.attribute": "attribute_value"
        }
      }
    ]
  }
  
  // JSON到Protobuf转换
  let json_serializer = JsonSerializer::new()
  let json_data = JsonSerializer::serialize(json_serializer, test_data)
  
  let json_to_protobuf_result = FormatConverter::convert(format_converter, {
    data: json_data.data,
    source_format: "json",
    target_format: "protobuf",
    source_type: "TelemetryData",
    target_type: "TelemetryData"
  })
  
  assert_true(json_to_protobuf_result.success)
  assert_true(json_to_protobuf_result.data.length() > 0)
  
  // 验证转换后的数据可以反序列化
  let protobuf_deserializer = ProtobufSerializer::new()
  let protobuf_deserialize_result = ProtobufSerializer::deserialize(protobuf_deserializer, json_to_protobuf_result.data, "TelemetryData")
  assert_true(protobuf_deserialize_result.success)
  
  let converted_from_protobuf = protobuf_deserialize_result.data
  assert_eq(converted_from_protobuf.spans.length(), 1)
  assert_eq(converted_from_protobuf.metrics.length(), 1)
  assert_eq(converted_from_protobuf.spans[0].trace_id, "trace-conversion-123")
  
  // JSON到Avro转换
  let avro_schema = {
    "type": "record",
    "name": "TelemetryData",
    "fields": [
      {
        "name": "spans",
        "type": {
          "type": "array",
          "items": {
            "type": "record",
            "name": "Span",
            "fields": [
              {"name": "trace_id", "type": "string"},
              {"name": "span_id", "type": "string"},
              {"name": "operation_name", "type": "string"},
              {"name": "start_time", "type": "long"},
              {"name": "duration", "type": "long"},
              {"name": "status", "type": "string"},
              {"name": "service_name", "type": "string"},
              {"name": "attributes", "type": {"type": "map", "values": "string"}}
            ]
          }
        }
      },
      {
        "name": "metrics",
        "type": {
          "type": "array",
          "items": {
            "type": "record",
            "name": "Metric",
            "fields": [
              {"name": "name", "type": "string"},
              {"name": "value", "type": "double"},
              {"name": "unit", "type": "string"},
              {"name": "timestamp", "type": "long"},
              {"name": "attributes", "type": {"type": "map", "values": "string"}}
            ]
          }
        }
      }
    ]
  }
  
  let avro_serializer = AvroSerializer::new()
  AvroSerializer::register_schema(avro_serializer, "TelemetryData", avro_schema)
  
  let json_to_avro_result = FormatConverter::convert(format_converter, {
    data: json_data.data,
    source_format: "json",
    target_format: "avro",
    source_type: "TelemetryData",
    target_type: "TelemetryData",
    target_schema: avro_schema
  })
  
  assert_true(json_to_avro_result.success)
  assert_true(json_to_avro_result.data.length() > 0)
  
  // 验证Avro转换后的数据
  let avro_deserialize_result = AvroSerializer::deserialize(avro_serializer, json_to_avro_result.data, "TelemetryData")
  assert_true(avro_deserialize_result.success)
  
  let converted_from_avro = avro_deserialize_result.data
  assert_eq(converted_from_avro.spans.length(), 1)
  assert_eq(converted_from_avro.metrics.length(), 1)
  
  // Protobuf到MessagePack转换
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_data = ProtobufSerializer::serialize(protobuf_serializer, test_data)
  
  let protobuf_to_msgpack_result = FormatConverter::convert(format_converter, {
    data: protobuf_data.data,
    source_format: "protobuf",
    target_format: "msgpack",
    source_type: "TelemetryData",
    target_type: "TelemetryData"
  })
  
  assert_true(protobuf_to_msgpack_result.success)
  assert_true(protobuf_to_msgpack_result.data.length() > 0)
  
  // 验证MessagePack转换后的数据
  let msgpack_deserializer = MessagePackSerializer::new()
  let msgpack_deserialize_result = MessagePackSerializer::deserialize(msgpack_deserializer, protobuf_to_msgpack_result.data, "TelemetryData")
  assert_true(msgpack_deserialize_result.success)
  
  let converted_from_msgpack = msgpack_deserialize_result.data
  assert_eq(converted_from_msgpack.spans.length(), 1)
  assert_eq(converted_from_msgpack.metrics.length(), 1)
  
  // 测试批量格式转换
  let batch_data = []
  for i in 0..=10 {
    batch_data.push({
      spans: [
        {
          trace_id: "trace-batch-" + i.to_string(),
          span_id: "span-batch-" + i.to_string(),
          operation_name: "batch.operation",
          start_time: 1640995200000 + i * 1000,
          duration: 100000000,
          status: "ok",
          service_name: "batch-service",
          attributes: {
            "batch.index": i
          }
        }
      ],
      metrics: [
        {
          name: "batch.metric",
          value: i.to_float(),
          unit: "count",
          timestamp: 1640995200000 + i * 1000,
          attributes: {
            "batch.index": i
          }
        }
      ]
    })
  }
  
  let batch_json_data = batch_data.map(fn(data) { JsonSerializer::serialize(json_serializer, data).data })
  
  let batch_conversion_result = FormatConverter::convert_batch(format_converter, {
    data_list: batch_json_data,
    source_format: "json",
    target_format: "protobuf",
    source_type: "TelemetryData",
    target_type: "TelemetryData"
  })
  
  assert_true(batch_conversion_result.success)
  assert_eq(batch_conversion_result.converted_data.length(), 11)
  
  // 测试转换性能
  let iterations = 100
  let conversion_start_time = Time::now()
  
  for i in 0..=iterations {
    FormatConverter::convert(format_converter, {
      data: json_data.data,
      source_format: "json",
      target_format: "protobuf",
      source_type: "TelemetryData",
      target_type: "TelemetryData"
    })
  }
  
  let conversion_duration = Time::now() - conversion_start_time
  let conversion_avg = conversion_duration / iterations
  
  assert_true(conversion_avg < 50.0)  // 平均转换时间应该<50ms
  
  // 测试转换统计
  let conversion_stats = FormatConverter::get_conversion_stats(format_converter)
  assert_true(conversion_stats.total_conversions > 0)
  assert_true(conversion_stats.conversion_by_format.contains("json_to_protobuf"))
  assert_true(conversion_stats.conversion_by_format.contains("json_to_avro"))
  assert_true(conversion_stats.conversion_by_format.contains("protobuf_to_msgpack"))
  
  // 验证转换错误处理
  let invalid_conversion_result = FormatConverter::convert(format_converter, {
    data: "invalid json data",
    source_format: "json",
    target_format: "protobuf",
    source_type: "TelemetryData",
    target_type: "TelemetryData"
  })
  
  assert_false(invalid_conversion_result.success)
  assert_true(invalid_conversion_result.error_message.length() > 0)
  
  // 测试不支持的格式转换
  let unsupported_conversion_result = FormatConverter::convert(format_converter, {
    data: json_data.data,
    source_format: "json",
    target_format: "unsupported_format",
    source_type: "TelemetryData",
    target_type: "TelemetryData"
  })
  
  assert_false(unsupported_conversion_result.success)
  assert_true(unsupported_conversion_result.error_message.contains("unsupported"))
}

// 测试7: 序列化性能优化
test "序列化性能优化功能" {
  // 创建性能优化序列化器
  let optimized_serializer = OptimizedSerializer::new()
  
  // 配置优化选项
  OptimizedSerializer::configure(optimized_serializer, {
    enable_compression: true,
    compression_algorithm: "lz4",
    compression_level: 6,
    enable_batching: true,
    batch_size: 100,
    enable_caching: true,
    cache_size: 1000,
    enable_pooling: true,
    pool_size: 50
  })
  
  // 创建大型测试数据集
  let large_dataset = []
  for i in 0..=1000 {
    large_dataset.push({
      trace_id: "trace-perf-" + i.to_string(),
      span_id: "span-perf-" + i.to_string(),
      operation_name: "performance.test.operation." + (i % 10).to_string(),
      start_time: 1640995200000 + i * 1000,
      duration: 100000000 + (i % 500) * 1000000,
      status: if i % 20 == 0 { "error" } else { "ok" },
      service_name: "performance.service." + (i % 5).to_string(),
      attributes: {
        "performance.index": i,
        "performance.batch": i / 100,
        "performance.mod10": i % 10,
        "performance.mod100": i % 100,
        "performance.random": i * 12345 % 1000,
        "performance.text": "performance test data " + i.to_string(),
        "performance.float": (i * 3.14159).mod(100.0),
        "performance.bool": i % 2 == 0
      },
      events: [
        {
          name: "operation.start",
          timestamp: 1640995200000 + i * 1000,
          attributes: {
            "event.type": "start",
            "event.index": i
          }
        },
        {
          name: "operation.complete",
          timestamp: 1640995200000 + i * 1000 + 100000000,
          attributes: {
            "event.type": "complete",
            "event.index": i,
            "event.duration": 100000000 + (i % 500) * 1000000
          }
        }
      ]
    })
  }
  
  // 测试基础序列化性能
  let base_serializer = JsonSerializer::new()
  let base_serialize_start = Time::now()
  let base_result = JsonSerializer::serialize(base_serializer, large_dataset)
  let base_serialize_duration = Time::now() - base_serialize_start
  
  assert_true(base_result.success)
  assert_true(base_result.data.length() > 0)
  
  // 测试优化序列化性能
  let optimized_serialize_start = Time::now()
  let optimized_result = OptimizedSerializer::serialize(optimized_serializer, large_dataset)
  let optimized_serialize_duration = Time::now() - optimized_serialize_start
  
  assert_true(optimized_result.success)
  assert_true(optimized_result.data.length() > 0)
  
  // 验证优化效果
  assert_true(optimized_serialize_duration < base_serialize_duration)  // 优化后应该更快
  assert_true(optimized_result.data.length() < base_result.data.length())  // 优化后应该更小
  
  let speed_improvement = (base_serialize_duration - optimized_serialize_duration) / base_serialize_duration
  let size_reduction = (base_result.data.length() - optimized_result.data.length()).to_float() / base_result.data.length().to_float()
  
  assert_true(speed_improvement > 0.1)  // 至少10%速度提升
  assert_true(size_reduction > 0.2)     // 至少20%大小减少
  
  // 测试反序列化性能
  let base_deserialize_start = Time::now()
  let base_deserialize_result = JsonSerializer::deserialize(base_serializer, base_result.data, "TelemetryData")
  let base_deserialize_duration = Time::now() - base_deserialize_start
  
  let optimized_deserialize_start = Time::now()
  let optimized_deserialize_result = OptimizedSerializer::deserialize(optimized_serializer, optimized_result.data, "TelemetryData")
  let optimized_deserialize_duration = Time::now() - optimized_deserialize_start
  
  assert_true(base_deserialize_result.success)
  assert_true(optimized_deserialize_result.success)
  
  // 验证反序列化优化效果
  assert_true(optimized_deserialize_duration < base_deserialize_duration)
  
  let deserialize_speed_improvement = (base_deserialize_duration - optimized_deserialize_duration) / base_deserialize_duration
  assert_true(deserialize_speed_improvement > 0.05)  // 至少5%反序列化速度提升
  
  // 测试批量序列化优化
  let batch_data = large_dataset.chunk(100)  // 分成10个批次
  
  let batch_serialize_start = Time::now()
  let batch_results = OptimizedSerializer::serialize_batch(optimized_serializer, batch_data)
  let batch_serialize_duration = Time::now() - batch_serialize_start
  
  assert_eq(batch_results.length(), 10)
  assert_true(batch_results.all(fn(r) { r.success }))
  
  // 测试内存使用优化
  let memory_stats_before = OptimizedSerializer::get_memory_stats(optimized_serializer)
  
  // 执行多次序列化操作
  for i in 0..=50 {
    OptimizedSerializer::serialize(optimized_serializer, large_dataset.slice(0, 100))
  }
  
  let memory_stats_after = OptimizedSerializer::get_memory_stats(optimized_serializer)
  
  // 验证内存池效果
  assert_true(memory_stats_after.pool_hits > memory_stats_before.pool_hits)
  assert_true(memory_stats_after.memory_reuse_rate > 0.5)  // 至少50%内存重用率
  
  // 测试缓存效果
  let cache_stats_before = OptimizedSerializer::get_cache_stats(optimized_serializer)
  
  // 重复序列化相似数据
  for i in 0..=50 {
    let similar_data = large_dataset.slice(0, 10).map(fn(item) {
      {
        trace_id: item.trace_id,
        span_id: item.span_id,
        operation_name: "cached.operation",  // 相同的操作名
        start_time: item.start_time,
        duration: item.duration,
        status: item.status,
        service_name: item.service_name,
        attributes: item.attributes,
        events: item.events
      }
    })
    OptimizedSerializer::serialize(optimized_serializer, similar_data)
  }
  
  let cache_stats_after = OptimizedSerializer::get_cache_stats(optimized_serializer)
  
  // 验证缓存效果
  assert_true(cache_stats_after.cache_hits > cache_stats_before.cache_hits)
  assert_true(cache_stats_after.hit_rate > 0.3)  // 至少30%缓存命中率
  
  // 测试压缩算法比较
  let compression_algorithms = ["lz4", "zstd", "gzip", "snappy"]
  let compression_results = []
  
  for algorithm in compression_algorithms {
    OptimizedSerializer::set_compression_algorithm(optimized_serializer, algorithm)
    
    let compress_start = Time::now()
    let compress_result = OptimizedSerializer::serialize(optimized_serializer, large_dataset.slice(0, 100))
    let compress_duration = Time::now() - compress_start
    
    compression_results.push({
      algorithm: algorithm,
      size: compress_result.data.length(),
      duration: compress_duration,
      compression_ratio: compress_result.data.length().to_float() / base_result.data.length().to_float()
    })
  }
  
  // 验证压缩算法效果
  let lz4_result = compression_results.find(fn(r) { r.algorithm == "lz4" })
  let zstd_result = compression_results.find(fn(r) { r.algorithm == "zstd" })
  let gzip_result = compression_results.find(fn(r) { r.algorithm == "gzip" })
  
  assert_true(lz4_result != None)
  assert_true(zstd_result != None)
  assert_true(gzip_result != None)
  
  // 验证不同算法的权衡
  match lz4_result {
    Some(result) => {
      assert_true(result.compression_ratio < 0.8)  // LZ4应该有较好的压缩率
      assert_true(result.duration < 1000.0)       // LZ4应该有较快的速度
    }
    None => assert_true(false)
  }
  
  match zstd_result {
    Some(result) => {
      assert_true(result.compression_ratio < 0.6)  // ZSTD应该有更好的压缩率
    }
    None => assert_true(false)
  }
  
  // 测试序列化优化报告
  let optimization_report = OptimizedSerializer::generate_optimization_report(optimized_serializer)
  assert_true(optimization_report.summary.length() > 0)
  assert_true(optimization_report.performance_improvements.length() > 0)
  assert_true(optimization_report.memory_optimizations.length() > 0)
  assert_true(optimization_report.recommendations.length() > 0)
  
  // 验证性能改进指标
  let performance_improvements = optimization_report.performance_improvements
  let serialization_improvement = performance_improvements.find(fn(p) { p.metric == "serialization_speed" })
  assert_true(serialization_improvement != None)
  
  match serialization_improvement {
    Some(improvement) => {
      assert_true(improvement.improvement_percentage > 0.0)
      assert_true(improvement.before_time > improvement.after_time)
    }
    None => assert_true(false)
  }
  
  // 验证内存优化指标
  let memory_optimizations = optimization_report.memory_optimizations
  let memory_pool_optimization = memory_optimizations.find_fn(o) { o.optimization == "memory_pooling" })
  assert_true(memory_pool_optimization != None)
  
  match memory_pool_optimization {
    Some(optimization) => {
      assert_true(optimization.memory_reduction_percentage > 0.0)
      assert_true(optimization.allocation_reduction > 0.0)
    }
    None => assert_true(false)
  }
}

// 测试8: 序列化安全性
test "序列化安全性功能" {
  // 创建安全序列化器
  let secure_serializer = SecureSerializer::new()
  
  // 配置安全选项
  SecureSerializer::configure(secure_serializer, {
    enable_encryption: true,
    encryption_algorithm: "aes256_gcm",
    key_derivation: "pbkdf2",
    enable_signing: true,
    signature_algorithm: "hmac_sha256",
    enable_compression: true,
    compression_algorithm: "zstd",
    max_data_size: 104857600,  // 100MB
    max_depth: 32,
    validate_input: true
  })
  
  // 设置加密密钥
  SecureSerializer::set_encryption_key(secure_serializer, {
    key_id: "encryption-key-2023",
    key_data: "32-byte-encryption-key-123456789012",
    algorithm: "aes256",
    created_at: 1640995200,
    expires_at: 1672531200  // 1年后过期
  })
  
  // 设置签名密钥
  SecureSerializer::set_signing_key(secure_serializer, {
    key_id: "signing-key-2023",
    key_data: "32-byte-signing-key-1234567890123456",
    algorithm: "hmac_sha256",
    created_at: 1640995200,
    expires_at: 1672531200  // 1年后过期
  })
  
  // 创建敏感测试数据
  let sensitive_data = {
    user_data: {
      user_id: "user-sensitive-123",
      username: "sensitive_user",
      email: "sensitive@example.com",
      phone: "+1-555-123-4567",
      ssn: "123-45-6789",
      credit_card: "4532-1234-5678-9012",
      address: {
        street: "123 Sensitive St",
        city: "Sensitive City",
        state: "SC",
        zip: "12345",
        country: "US"
      },
      preferences: {
        marketing_emails: true,
        notifications: false,
        privacy_level: "high"
      }
    },
    session_data: {
      session_id: "session-sensitive-456",
      token: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.sensitive.token",
      ip_address: "192.168.1.100",
      user_agent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      login_time: 1640995200,
      last_activity: 1640995800,
      permissions: ["read", "write", "admin"]
    },
    application_data: {
      api_key: "api-key-sensitive-789",
      database_connection: "postgresql://user:password@db.example.com:5432/sensitive_db",
      secrets: {
        jwt_secret: "jwt-secret-sensitive",
        encryption_key: "encryption-key-sensitive",
        webhook_secret: "webhook-secret-sensitive"
      }
    }
  }
  
  // 序列化敏感数据
  let secure_serialize_result = SecureSerializer::serialize(secure_serializer, sensitive_data)
  assert_true(secure_serialize_result.success)
  assert_true(secure_serialize_result.data.length() > 0)
  
  // 验证加密数据
  let encrypted_data = secure_serialize_result.data
  assert_true(encrypted_data.length() > 0)
  
  // 验证数据不包含明文敏感信息
  let data_as_string = String::from_utf8_lossy(encrypted_data)
  assert_false(data_as_string.contains("user-sensitive-123"))
  assert_false(data_as_string.contains("sensitive@example.com"))
  assert_false(data_as_string.contains("123-45-6789"))
  assert_false(data_as_string.contains("4532-1234-5678-9012"))
  assert_false(data_as_string.contains("api-key-sensitive-789"))
  
  // 反序列化敏感数据
  let secure_deserialize_result = SecureSerializer::deserialize(secure_serializer, encrypted_data, "SensitiveData")
  assert_true(secure_deserialize_result.success)
  
  let deserialized_data = secure_deserialize_result.data
  assert_eq(deserialized_data.user_data.user_id, "user-sensitive-123")
  assert_eq(deserialized_data.user_data.email, "sensitive@example.com")
  assert_eq(deserialized_data.user_data.ssn, "123-45-6789")
  assert_eq(deserialized_data.session_data.session_id, "session-sensitive-456")
  assert_eq(deserialized_data.application_data.api_key, "api-key-sensitive-789")
  
  // 测试数据篡改检测
  let tampered_data = encrypted_data.slice(0, encrypted_data.length() - 1) + [0]  // 修改最后一个字节
  
  let tampered_deserialize_result = SecureSerializer::deserialize(secure_serializer, tampered_data, "SensitiveData")
  assert_false(tampered_deserialize_result.success)
  assert_true(tampered_deserialize_result.error_message.contains("signature") or tampered_deserialize_result.error_message.contains("tampered"))
  
  // 测试过期密钥处理
  let expired_key = {
    key_id: "expired-key-2022",
    key_data: "32-byte-expired-key-1234567890123456",
    algorithm: "aes256",
    created_at: 1640995200 - 365 * 24 * 3600,  // 1年前创建
    expires_at: 1640995200 - 1 * 24 * 3600     // 1天前过期
  }
  
  SecureSerializer::set_encryption_key(secure_serializer, expired_key)
  
  let expired_key_serialize_result = SecureSerializer::serialize(secure_serializer, sensitive_data)
  assert_false(expired_key_serialize_result.success)
  assert_true(expired_key_serialize_result.error_message.contains("expired"))
  
  // 恢复有效密钥
  SecureSerializer::set_encryption_key(secure_serializer, {
    key_id: "encryption-key-2023",
    key_data: "32-byte-encryption-key-123456789012",
    algorithm: "aes256",
    created_at: 1640995200,
    expires_at: 1672531200
  })
  
  // 测试输入验证
  let malicious_data = {
    user_data: {
      user_id: "user".repeat(10000),  // 过长的用户ID
      username: "normal_user",
      email: "valid@example.com",
      phone: "+1-555-123-4567",
      ssn: "123-45-6789",
      credit_card: "4532-1234-5678-9012"
    },
    nested_objects: (0..=100).map(fn(i) {  // 深度嵌套
      {
        level: i,
        data: "level " + i.to_string(),
        nested: if i < 99 { Some({ next: "nested " + (i + 1).to_string() }) } else { None }
      }
    })
  }
  
  let validation_result = SecureSerializer::validate_input(secure_serializer, malicious_data)
  assert_false(validation_result.valid)
  assert_true(validation_result.errors.length() > 0)
  
  // 验证具体错误
  let has_length_error = validation_result.errors.any(fn(e) { e.contains("length") or e.contains("too long") })
  let has_depth_error = validation_result.errors.any(fn(e) { e.contains("depth") or e.contains("nested") })
  
  assert_true(has_length_error or has_depth_error)
  
  // 测试安全序列化性能
  let iterations = 100
  let secure_serialize_start = Time::now()
  
  for i in 0..=iterations {
    SecureSerializer::serialize(secure_serializer, sensitive_data)
  }
  
  let secure_serialize_duration = Time::now() - secure_serialize_start
  let secure_serialize_avg = secure_serialize_duration / iterations
  
  let secure_deserialize_start = Time::now()
  
  for i in 0..=iterations {
    SecureSerializer::deserialize(secure_serializer, encrypted_data, "SensitiveData")
  }
  
  let secure_deserialize_duration = Time::now() - secure_deserialize_start
  let secure_deserialize_avg = secure_deserialize_duration / iterations
  
  // 验证安全序列化性能在可接受范围内
  assert_true(secure_serialize_avg < 100.0)   // 平均序列化时间应该<100ms
  assert_true(secure_deserialize_avg < 100.0)  // 平均反序列化时间应该<100ms
  
  // 测试密钥轮换
  let new_key = {
    key_id: "encryption-key-2024",
    key_data: "32-byte-new-key-1234567890123456",
    algorithm: "aes256",
    created_at: 1640995200,
    expires_at: 1672531200
  }
  
  SecureSerializer::add_encryption_key(secure_serializer, new_key)
  SecureSerializer::set_primary_key(secure_serializer, "encryption-key-2024")
  
  let key_rotation_serialize_result = SecureSerializer::serialize(secure_serializer, sensitive_data)
  assert_true(key_rotation_serialize_result.success)
  
  // 验证新密钥加密的数据可以正确反序列化
  let key_rotation_deserialize_result = SecureSerializer::deserialize(secure_serializer, key_rotation_serialize_result.data, "SensitiveData")
  assert_true(key_rotation_deserialize_result.success)
  
  // 验证旧密钥加密的数据仍然可以反序列化（密钥轮换兼容性）
  let backward_compatibility_result = SecureSerializer::deserialize(secure_serializer, encrypted_data, "SensitiveData")
  assert_true(backward_compatibility_result.success)
  
  // 测试安全统计
  let security_stats = SecureSerializer::get_security_stats(secure_serializer)
  assert_true(security_stats.total_encryptions > 0)
  assert_true(security_stats.total_decryptions > 0)
  assert_true(security_stats.signature_verifications > 0)
  assert_true(security_stats.tampering_attempts >= 0)
  assert_true(security_stats.key_rotations >= 1)
  
  // 测试安全审计日志
  let audit_log = SecureSerializer::get_audit_log(secure_serializer)
  assert_true(audit_log.length() > 0)
  
  // 验证审计日志包含关键操作
  let has_encryption_log = audit_log.any(fn(entry) { entry.operation == "encrypt" })
  let has_decryption_log = audit_log.any(fn(entry) { entry.operation == "decrypt" })
  let has_key_rotation_log = audit_log.any(fn(entry) { entry.operation == "key_rotation" })
  
  assert_true(has_encryption_log)
  assert_true(has_decryption_log)
  assert_true(has_key_rotation_log)
}
