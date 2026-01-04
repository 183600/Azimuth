// Azimuth 数据序列化和反序列化测试用例
// 专注于测试多种数据格式的序列化、反序列化、版本兼容性和数据完整性

// 测试1: JSON序列化和反序列化
test "JSON序列化和反序列化" {
  // 1. 创建JSON序列化器
  let json_serializer = JsonSerializer::new()
  
  // 2. 创建遥测数据对象
  let telemetry_data = TelemetryData({
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    parent_span_id: Some("span-111111111"),
    operation_name: "database_query",
    start_time: 1735689600000000000L,
    end_time: 1735689600500000000L,
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users WHERE id = ?"),
      ("db.instance", "production-db"),
      ("service.name", "user-service"),
      ("service.version", "1.2.3")
    ],
    events: [
      SpanEvent({
        name: "query.start",
        timestamp: 1735689600000000000L,
        attributes: [("query.type", "select")]
      }),
      SpanEvent({
        name: "query.end",
        timestamp: 1735689600500000000L,
        attributes: [("rows.affected", "1")]
      })
    ],
    links: [
      SpanLink({
        trace_id: "trace-222222222",
        span_id: "span-333333333",
        attributes: [("link.type", "causality")]
      })
    ]
  })
  
  // 3. 测试JSON序列化
  let json_result = JsonSerializer::serialize(json_serializer, telemetry_data)
  assert_true(json_result.success)
  assert_not_nil(json_result.data)
  
  let json_string = json_result.data!
  assert_true(json_string.contains("\"trace_id\":\"trace-123456789\""))
  assert_true(json_string.contains("\"span_id\":\"span-987654321\""))
  assert_true(json_string.contains("\"operation_name\":\"database_query\""))
  assert_true(json_string.contains("\"status\":\"ok\""))
  
  // 4. 测试JSON反序列化
  let deserialize_result = JsonSerializer::deserialize(json_serializer, json_string, TelemetryData)
  assert_true(deserialize_result.success)
  assert_not_nil(deserialize_result.data)
  
  let deserialized_data = deserialize_result.data!
  assert_eq(deserialized_data.trace_id, telemetry_data.trace_id)
  assert_eq(deserialized_data.span_id, telemetry_data.span_id)
  assert_eq(deserialized_data.operation_name, telemetry_data.operation_name)
  assert_eq(deserialized_data.status, telemetry_data.status)
  assert_eq(deserialized_data.attributes.length(), telemetry_data.attributes.length())
  
  // 5. 测试嵌套对象反序列化
  match deserialized_data.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "span-111111111")
    None => assert_true(false)
  }
  
  assert_eq(deserialized_data.events.length(), 2)
  assert_eq(deserialized_data.events[0].name, "query.start")
  assert_eq(deserialized_data.events[1].name, "query.end")
  
  assert_eq(deserialized_data.links.length(), 1)
  assert_eq(deserialized_data.links[0].trace_id, "trace-222222222")
  
  // 6. 测试JSON格式验证
  let invalid_json = "{ invalid json structure"
  let invalid_result = JsonSerializer::deserialize(json_serializer, invalid_json, TelemetryData)
  assert_false(invalid_result.success)
  assert_eq(invalid_result.error.type, "parse_error")
  
  // 7. 测试部分数据反序列化
  let partial_json = "{\"trace_id\":\"trace-123\",\"span_id\":\"span-456\"}"
  let partial_result = JsonSerializer::deserialize(json_serializer, partial_json, TelemetryData)
  assert_true(partial_result.success)
  
  let partial_data = partial_result.data!
  assert_eq(partial_data.trace_id, "trace-123")
  assert_eq(partial_data.span_id, "span-456")
  // 其他字段应该是默认值
}

// 测试2: Protocol Buffers序列化和反序列化
test "Protocol Buffers序列化和反序列化" {
  // 1. 创建Protobuf序列化器
  let protobuf_serializer = ProtobufSerializer::new()
  
  // 2. 定义protobuf模式（简化版）
  let schema = ProtobufSchema::new("
    message TelemetrySpan {
      string trace_id = 1;
      string span_id = 2;
      optional string parent_span_id = 3;
      string operation_name = 4;
      int64 start_time = 5;
      int64 end_time = 6;
      string status = 7;
      repeated Attribute attributes = 8;
      repeated SpanEvent events = 9;
    }
    
    message Attribute {
      string key = 1;
      string value = 2;
    }
    
    message SpanEvent {
      string name = 1;
      int64 timestamp = 2;
      repeated Attribute attributes = 3;
    }
  ")
  
  ProtobufSerializer::set_schema(protobuf_serializer, schema)
  
  // 3. 创建遥测跨度数据
  let span_data = TelemetrySpan({
    trace_id: "protobuf-trace-123",
    span_id: "protobuf-span-456",
    parent_span_id: Some("protobuf-parent-789"),
    operation_name: "http_request",
    start_time: 1735689600000000000L,
    end_time: 1735689602000000000L,
    status: "ok",
    attributes: [
      Attribute({ key: "http.method", value: "GET" }),
      Attribute({ key: "http.url", value: "https://api.example.com/users" }),
      Attribute({ key: "http.status_code", value: "200" }),
      Attribute({ key: "service.name", value: "api-gateway" })
    ],
    events: [
      SpanEvent({
        name: "request.start",
        timestamp: 1735689600000000000L,
        attributes: [Attribute({ key: "event.type", value: "request_received" })]
      }),
      SpanEvent({
        name: "request.end",
        timestamp: 1735689602000000000L,
        attributes: [Attribute({ key: "event.type", value: "response_sent" })]
      })
    ]
  })
  
  // 4. 测试Protobuf序列化
  let serialize_result = ProtobufSerializer::serialize(protobuf_serializer, span_data)
  assert_true(serialize_result.success)
  assert_not_nil(serialize_result.data)
  
  let protobuf_bytes = serialize_result.data!
  assert_true(protobuf_bytes.length() > 0)
  
  // 5. 测试Protobuf反序列化
  let deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, protobuf_bytes, TelemetrySpan)
  assert_true(deserialize_result.success)
  assert_not_nil(deserialize_result.data)
  
  let deserialized_span = deserialize_result.data!
  assert_eq(deserialized_span.trace_id, span_data.trace_id)
  assert_eq(deserialized_span.span_id, span_data.span_id)
  assert_eq(deserialized_span.operation_name, span_data.operation_name)
  assert_eq(deserialized_span.status, span_data.status)
  
  // 6. 验证二进制数据效率
  let json_serializer = JsonSerializer::new()
  let json_result = JsonSerializer::serialize(json_serializer, span_data)
  let json_size = json_result.data!.length()
  let protobuf_size = protobuf_bytes.length()
  
  // Protobuf应该比JSON更紧凑
  assert_true(protobuf_size < json_size)
  
  // 7. 测试版本兼容性
  // 模拟旧版本的protobuf数据（缺少某些字段）
  let old_version_bytes = ProtobufSerializer::serialize_old_version(protobuf_serializer, span_data)
  let compat_result = ProtobufSerializer::deserialize_with_compatibility(protobuf_serializer, old_version_bytes, TelemetrySpan)
  assert_true(compat_result.success)
  
  // 8. 测试二进制数据损坏检测
  let corrupted_bytes = protobuf_bytes.slice(0, protobuf_bytes.length() - 1) + [0xFF]
  let corrupted_result = ProtobufSerializer::deserialize(protobuf_serializer, corrupted_bytes, TelemetrySpan)
  assert_false(corrupted_result.success)
  assert_eq(corrupted_result.error.type, "data_corruption")
}

// 测试3: Avro序列化和反序列化
test "Avro序列化和反序列化" {
  // 1. 创建Avro序列化器
  let avro_serializer = AvroSerializer::new()
  
  // 2. 定义Avro模式
  let avro_schema = AvroSchema::new({
    "type": "record",
    "name": "TelemetryMetric",
    "fields": [
      {"name": "metric_name", "type": "string"},
      {"name": "metric_value", "type": "double"},
      {"name": "metric_unit", "type": ["null", "string"], "default": null},
      {"name": "timestamp", "type": "long"},
      {"name": "tags", "type": {"type": "map", "values": "string"}},
      {"name": "resource", "type": ["null", {
        "type": "record",
        "name": "Resource",
        "fields": [
          {"name": "service_name", "type": "string"},
          {"name": "service_version", "type": "string"},
          {"name": "host_name", "type": "string"}
        ]
      }], "default": null}
    ]
  })
  
  AvroSerializer::set_schema(avro_serializer, avro_schema)
  
  // 3. 创建遥测指标数据
  let metric_data = TelemetryMetric({
    metric_name: "cpu_usage",
    metric_value: 75.5,
    metric_unit: Some("percent"),
    timestamp: 1735689600000000000L,
    tags: {
      "host": "web-server-01",
      "region": "us-west-2",
      "environment": "production"
    },
    resource: Some(Resource({
      service_name: "web-service",
      service_version: "2.1.0",
      host_name: "web-server-01.example.com"
    }))
  })
  
  // 4. 测试Avro序列化
  let serialize_result = AvroSerializer::serialize(avro_serializer, metric_data)
  assert_true(serialize_result.success)
  assert_not_nil(serialize_result.data)
  
  let avro_bytes = serialize_result.data!
  assert_true(avro_bytes.length() > 0)
  
  // 5. 测试Avro反序列化
  let deserialize_result = AvroSerializer::deserialize(avro_serializer, avro_bytes, TelemetryMetric)
  assert_true(deserialize_result.success)
  assert_not_nil(deserialize_result.data)
  
  let deserialized_metric = deserialize_result.data!
  assert_eq(deserialized_metric.metric_name, metric_data.metric_name)
  assert_eq(deserialized_metric.metric_value, metric_data.metric_value)
  assert_eq(deserialized_metric.timestamp, metric_data.timestamp)
  
  match deserialized_metric.metric_unit {
    Some(unit) => assert_eq(unit, "percent")
    None => assert_true(false)
  }
  
  match deserialized_metric.resource {
    Some(resource) => {
      assert_eq(resource.service_name, "web-service")
      assert_eq(resource.service_version, "2.1.0")
    }
    None => assert_true(false)
  }
  
  // 6. 测试模式演进兼容性
  // 创建新版本的模式（添加新字段）
  let new_avro_schema = AvroSchema::new({
    "type": "record",
    "name": "TelemetryMetric",
    "fields": [
      {"name": "metric_name", "type": "string"},
      {"name": "metric_value", "type": "double"},
      {"name": "metric_unit", "type": ["null", "string"], "default": null},
      {"name": "timestamp", "type": "long"},
      {"name": "tags", "type": {"type": "map", "values": "string"}},
      {"name": "resource", "type": ["null", {
        "type": "record",
        "name": "Resource",
        "fields": [
          {"name": "service_name", "type": "string"},
          {"name": "service_version", "type": "string"},
          {"name": "host_name", "type": "string"}
        ]
      }], "default": null},
      {"name": "metric_type", "type": ["null", "string"], "default": null} // 新字段
    ]
  })
  
  // 使用新模式反序列化旧数据
  let forward_compat_result = AvroSerializer::deserialize_with_schema(avro_serializer, avro_bytes, new_avro_schema, TelemetryMetric)
  assert_true(forward_compat_result.success)
  
  // 7. 测试Avro文件格式
  let avro_file_result = AvroSerializer::serialize_to_file(avro_serializer, [metric_data], "/tmp/telemetry_metrics.avro")
  assert_true(avro_file_result.success)
  
  let file_deserialize_result = AvroSerializer::deserialize_from_file(avro_serializer, "/tmp/telemetry_metrics.avro", TelemetryMetric)
  assert_true(file_deserialize_result.success)
  assert_eq(file_deserialize_result.data.length(), 1)
  
  // 8. 测试压缩Avro数据
  let compressed_result = AvroSerializer::serialize_compressed(avro_serializer, metric_data)
  assert_true(compressed_result.success)
  
  let compressed_deserialize_result = AvroSerializer::deserialize_compressed(avro_serializer, compressed_result.data!, TelemetryMetric)
  assert_true(compressed_deserialize_result.success)
  
  // 验证压缩效果
  assert_true(compressed_result.data!.length() < avro_bytes.length())
}

// 测试4: MessagePack序列化和反序列化
test "MessagePack序列化和反序列化" {
  // 1. 创建MessagePack序列化器
  let msgpack_serializer = MessagePackSerializer::new()
  
  // 2. 创建日志数据
  let log_data = LogRecord({
    timestamp: 1735689600000000000L,
    severity: "INFO",
    body: "User login successful",
    resource: {
      service_name: "auth-service",
      service_version: "1.5.2",
      host_name: "auth-server-01"
    },
    attributes: [
      ("user.id", "12345"),
      ("user.email", "user@example.com"),
      ("ip.address", "192.168.1.100"),
      ("session.id", "sess-abcdef123456")
    ],
    trace_id: Some("trace-789012345"),
    span_id: Some("span-678901234")
  })
  
  // 3. 测试MessagePack序列化
  let serialize_result = MessagePackSerializer::serialize(msgpack_serializer, log_data)
  assert_true(serialize_result.success)
  assert_not_nil(serialize_result.data)
  
  let msgpack_bytes = serialize_result.data!
  assert_true(msgpack_bytes.length() > 0)
  
  // 4. 测试MessagePack反序列化
  let deserialize_result = MessagePackSerializer::deserialize(msgpack_serializer, msgpack_bytes, LogRecord)
  assert_true(deserialize_result.success)
  assert_not_nil(deserialize_result.data)
  
  let deserialized_log = deserialize_result.data!
  assert_eq(deserialized_log.timestamp, log_data.timestamp)
  assert_eq(deserialized_log.severity, log_data.severity)
  assert_eq(deserialized_log.body, log_data.body)
  assert_eq(deserialized_log.resource.service_name, log_data.resource.service_name)
  
  match deserialized_log.trace_id {
    Some(trace_id) => assert_eq(trace_id, "trace-789012345")
    None => assert_true(false)
  }
  
  // 5. 测试MessagePack与JSON的效率比较
  let json_serializer = JsonSerializer::new()
  let json_result = JsonSerializer::serialize(json_serializer, log_data)
  
  let msgpack_size = msgpack_bytes.length()
  let json_size = json_result.data!.length()
  
  // MessagePack应该比JSON更紧凑
  assert_true(msgpack_size < json_size)
  
  // 6. 测试流式MessagePack序列化
  let log_records = []
  for i in 0..=100 {
    let record = LogRecord({
      timestamp: 1735689600000000000L + (i as Int64 * 1000000L),
      severity: if i % 2 == 0 { "INFO" } else { "DEBUG" },
      body: "Log message " + i.to_string(),
      resource: log_data.resource,
      attributes: [("record.id", i.to_string())],
      trace_id: Some("trace-batch-" + (i / 10).to_string()),
      span_id: Some("span-" + i.to_string())
    })
    log_records.push(record)
  }
  
  let stream_serialize_result = MessagePackSerializer::serialize_stream(msgpack_serializer, log_records)
  assert_true(stream_serialize_result.success)
  
  let stream_deserialize_result = MessagePackSerializer::deserialize_stream(msgpack_serializer, stream_serialize_result.data!, LogRecord)
  assert_true(stream_deserialize_result.success)
  assert_eq(stream_deserialize_result.data.length(), 101)
  
  // 7. 测试MessagePack模式验证
  let invalid_data = [0x92, 0x01, 0x02] // 不完整的MessagePack数据
  let invalid_result = MessagePackSerializer::deserialize(msgpack_serializer, invalid_data, LogRecord)
  assert_false(invalid_result.success)
  assert_eq(invalid_result.error.type, "format_error")
  
  // 8. 测试自定义类型序列化
  let custom_data = CustomTelemetryData({
    id: "custom-123",
    measurements: [
      ("temperature", 25.5),
      ("humidity", 60.2),
      ("pressure", 1013.25)
    ],
    location: {
      latitude: 37.7749,
      longitude: -122.4194,
      altitude: Some(100.0)
    },
    metadata: {
      "device_id": "sensor-001",
      "firmware_version": "2.0.1"
    }
  })
  
  let custom_serialize_result = MessagePackSerializer::serialize_with_custom_types(msgpack_serializer, custom_data)
  assert_true(custom_serialize_result.success)
  
  let custom_deserialize_result = MessagePackSerializer::deserialize_with_custom_types(msgpack_serializer, custom_serialize_result.data!, CustomTelemetryData)
  assert_true(custom_deserialize_result.success)
  
  let custom_deserialized = custom_deserialize_result.data!
  assert_eq(custom_deserialized.id, custom_data.id)
  assert_eq(custom_deserialized.measurements.length(), 3)
  assert_eq(custom_deserialized.location.latitude, custom_data.location.latitude)
}

// 测试5: CSV序列化和反序列化
test "CSV序列化和反序列化" {
  // 1. 创建CSV序列化器
  let csv_serializer = CsvSerializer::new()
  
  // 2. 配置CSV格式
  CsvSerializer::configure(csv_serializer, CsvConfig({
    delimiter: ",",
    quote_character: "\"",
    escape_character: "\\",
    header_row: true,
    encoding: "utf-8"
  }))
  
  // 3. 创建指标数据集合
  let metrics_data = [
    MetricRecord({
      timestamp: 1735689600000000000L,
      metric_name: "cpu_usage",
      metric_value: 75.5,
      metric_unit: "percent",
      host: "web-server-01",
      service: "web-service",
      environment: "production"
    }),
    MetricRecord({
      timestamp: 1735689600600000000L,
      metric_name: "memory_usage",
      metric_value: 60.2,
      metric_unit: "percent",
      host: "web-server-01",
      service: "web-service",
      environment: "production"
    }),
    MetricRecord({
      timestamp: 1735689601200000000L,
      metric_name: "disk_usage",
      metric_value: 45.8,
      metric_unit: "percent",
      host: "web-server-01",
      service: "web-service",
      environment: "production"
    }),
    MetricRecord({
      timestamp: 1735689601800000000L,
      metric_name: "network_throughput",
      metric_value: 1024.5,
      metric_unit: "mbps",
      host: "web-server-01",
      service: "web-service",
      environment: "production"
    })
  ]
  
  // 4. 测试CSV序列化
  let serialize_result = CsvSerializer::serialize(csv_serializer, metrics_data)
  assert_true(serialize_result.success)
  assert_not_nil(serialize_result.data)
  
  let csv_string = serialize_result.data!
  
  // 5. 验证CSV格式
  let lines = csv_string.split("\n")
  assert_eq(lines.length(), 5) // 4行数据 + 1行标题
  
  // 验证标题行
  let header = lines[0]
  assert_true(header.contains("timestamp"))
  assert_true(header.contains("metric_name"))
  assert_true(header.contains("metric_value"))
  assert_true(header.contains("metric_unit"))
  assert_true(header.contains("host"))
  assert_true(header.contains("service"))
  assert_true(header.contains("environment"))
  
  // 验证数据行
  let first_data_line = lines[1]
  assert_true(first_data_line.contains("cpu_usage"))
  assert_true(first_data_line.contains("75.5"))
  assert_true(first_data_line.contains("web-server-01"))
  
  // 6. 测试CSV反序列化
  let deserialize_result = CsvSerializer::deserialize(csv_serializer, csv_string, MetricRecord)
  assert_true(deserialize_result.success)
  assert_not_nil(deserialize_result.data)
  
  let deserialized_metrics = deserialize_result.data!
  assert_eq(deserialized_metrics.length(), 4)
  
  // 验证反序列化数据
  assert_eq(deserialized_metrics[0].metric_name, "cpu_usage")
  assert_eq(deserialized_metrics[0].metric_value, 75.5)
  assert_eq(deserialized_metrics[1].metric_name, "memory_usage")
  assert_eq(deserialized_metrics[1].metric_value, 60.2)
  
  // 7. 测试特殊字符处理
  let special_metrics = [
    MetricRecord({
      timestamp: 1735689600000000000L,
      metric_name: "special,metric",
      metric_value: 50.0,
      metric_unit: "percent",
      host: "server,with,commas",
      service: "service\"with\"quotes",
      environment: "env\nwith\nnewlines"
    })
  ]
  
  let special_serialize_result = CsvSerializer::serialize(csv_serializer, special_metrics)
  assert_true(special_serialize_result.success)
  
  let special_csv = special_serialize_result.data!
  assert_true(special_csv.contains("\"special,metric\""))
  assert_true(special_csv.contains("\"server,with,commas\""))
  assert_true(special_csv.contains("\"service\"\"with\"\"quotes\""))
  
  // 8. 测试CSV文件读写
  let file_serialize_result = CsvSerializer::serialize_to_file(csv_serializer, metrics_data, "/tmp/telemetry_metrics.csv")
  assert_true(file_serialize_result.success)
  
  let file_deserialize_result = CsvSerializer::deserialize_from_file(csv_serializer, "/tmp/telemetry_metrics.csv", MetricRecord)
  assert_true(file_deserialize_result.success)
  assert_eq(file_deserialize_result.data.length(), 4)
  
  // 9. 测试大数据量CSV处理
  let large_metrics = []
  for i in 0..=10000 {
    let metric = MetricRecord({
      timestamp: 1735689600000000000L + (i as Int64 * 1000000L),
      metric_name: "metric_" + i.to_string(),
      metric_value: (i % 100) as Float,
      metric_unit: "count",
      host: "server-" + (i % 10).to_string(),
      service: "service-" + (i % 5).to_string(),
      environment: if i % 2 == 0 { "production" } else { "staging" }
    })
    large_metrics.push(metric)
  }
  
  let large_serialize_result = CsvSerializer::serialize(csv_serializer, large_metrics)
  assert_true(large_serialize_result.success)
  
  let large_deserialize_result = CsvSerializer::deserialize(csv_serializer, large_serialize_result.data!, MetricRecord)
  assert_true(large_deserialize_result.success)
  assert_eq(large_deserialize_result.data.length(), 10001)
}

// 测试6: XML序列化和反序列化
test "XML序列化和反序列化" {
  // 1. 创建XML序列化器
  let xml_serializer = XmlSerializer::new()
  
  // 2. 配置XML格式
  XmlSerializer::configure(xml_serializer, XmlConfig({
    root_element: "TelemetryData",
    namespace: "http://azimuth.io/telemetry",
    pretty_print: true,
    encoding: "utf-8"
  }))
  
  // 3. 创建复杂的遥测数据
  let telemetry_config = TelemetryConfiguration({
    service_name: "payment-service",
    service_version: "3.2.1",
    sampling_rate: 0.1,
    exporters: [
      ExporterConfig({
        name: "jaeger",
        type: "jaeger",
        endpoint: "http://jaeger:14268/api/traces",
        settings: {
          "timeout": "5000",
          "batch_size": "100"
        }
      }),
      ExporterConfig({
        name: "prometheus",
        type: "prometheus",
        endpoint: "http://prometheus:9090/metrics",
        settings: {
          "port": "8888",
          "path": "/metrics"
        }
      })
    ],
    processors: [
      ProcessorConfig({
        name: "batch",
        type: "batch",
        settings: {
          "max_batch_size": "1000",
          "timeout": "5000"
        }
      }),
      ProcessorConfig({
        name: "memory_limiter",
        type: "memory_limiter",
        settings: {
          "limit_mib": "512"
        }
      })
    ],
    resource_attributes: [
      ("service.instance.id", "instance-12345"),
      ("service.namespace", "production"),
      ("deployment.environment", "prod"),
      ("host.name", "payment-server-01"),
      ("os.type", "linux"),
      ("os.version", "ubuntu-20.04")
    ]
  })
  
  // 4. 测试XML序列化
  let serialize_result = XmlSerializer::serialize(xml_serializer, telemetry_config)
  assert_true(serialize_result.success)
  assert_not_nil(serialize_result.data)
  
  let xml_string = serialize_result.data!
  
  // 5. 验证XML格式
  assert_true(xml_string.contains("<?xml version=\"1.0\" encoding=\"utf-8\"?>"))
  assert_true(xml_string.contains("<TelemetryData"))
  assert_true(xml_string.contains("xmlns=\"http://azimuth.io/telemetry\""))
  assert_true(xml_string.contains("<service_name>payment-service</service_name>"))
  assert_true(xml_string.contains("<service_version>3.2.1</service_version>"))
  assert_true(xml_string.contains("<sampling_rate>0.1</sampling_rate>"))
  
  // 验证嵌套结构
  assert_true(xml_string.contains("<exporters>"))
  assert_true(xml_string.contains("<exporter>"))
  assert_true(xml_string.contains("<name>jaeger</name>"))
  assert_true(xml_string.contains("<type>jaeger</type>"))
  assert_true(xml_string.contains("<endpoint>http://jaeger:14268/api/traces</endpoint>"))
  
  // 6. 测试XML反序列化
  let deserialize_result = XmlSerializer::deserialize(xml_serializer, xml_string, TelemetryConfiguration)
  assert_true(deserialize_result.success)
  assert_not_nil(deserialize_result.data)
  
  let deserialized_config = deserialize_result.data!
  assert_eq(deserialized_config.service_name, telemetry_config.service_name)
  assert_eq(deserialized_config.service_version, telemetry_config.service_version)
  assert_eq(deserialized_config.sampling_rate, telemetry_config.sampling_rate)
  assert_eq(deserialized_config.exporters.length(), 2)
  assert_eq(deserialized_config.processors.length(), 2)
  assert_eq(deserialized_config.resource_attributes.length(), 6)
  
  // 验证嵌套对象反序列化
  assert_eq(deserialized_config.exporters[0].name, "jaeger")
  assert_eq(deserialized_config.exporters[0].type, "jaeger")
  assert_eq(deserialized_config.exporters[0].endpoint, "http://jaeger:14268/api/traces")
  
  assert_eq(deserialized_config.processors[1].name, "memory_limiter")
  assert_eq(deserialized_config.processors[1].settings["limit_mib"], "512")
  
  // 7. 测试XML模式验证
  let xml_schema = XmlSchema::new("
    <xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\"
               targetNamespace=\"http://azimuth.io/telemetry\"
               xmlns:tns=\"http://azimuth.io/telemetry\">
      
      <xs:element name=\"TelemetryData\" type=\"tns:TelemetryConfigurationType\"/>
      
      <xs:complexType name=\"TelemetryConfigurationType\">
        <xs:sequence>
          <xs:element name=\"service_name\" type=\"xs:string\"/>
          <xs:element name=\"service_version\" type=\"xs:string\"/>
          <xs:element name=\"sampling_rate\" type=\"xs:float\"/>
          <xs:element name=\"exporters\" type=\"tns:ExportersType\" minOccurs=\"0\"/>
          <xs:element name=\"processors\" type=\"tns:ProcessorsType\" minOccurs=\"0\"/>
          <xs:element name=\"resource_attributes\" type=\"tns:ResourceAttributesType\" minOccurs=\"0\"/>
        </xs:sequence>
      </xs:complexType>
      
      <xs:complexType name=\"ExportersType\">
        <xs:sequence>
          <xs:element name=\"exporter\" type=\"tns:ExporterConfigType\" maxOccurs=\"unbounded\"/>
        </xs:sequence>
      </xs:complexType>
      
      <xs:complexType name=\"ExporterConfigType\">
        <xs:sequence>
          <xs:element name=\"name\" type=\"xs:string\"/>
          <xs:element name=\"type\" type=\"xs:string\"/>
          <xs:element name=\"endpoint\" type=\"xs:string\"/>
          <xs:element name=\"settings\" type=\"tns:SettingsType\" minOccurs=\"0\"/>
        </xs:sequence>
      </xs:complexType>
      
      <xs:complexType name=\"SettingsType\">
        <xs:sequence>
          <xs:element name=\"setting\" type=\"tns:SettingType\" maxOccurs=\"unbounded\"/>
        </xs:sequence>
      </xs:complexType>
      
      <xs:complexType name=\"SettingType\">
        <xs:simpleContent>
          <xs:extension base=\"xs:string\">
            <xs:attribute name=\"key\" type=\"xs:string\" use=\"required\"/>
          </xs:extension>
        </xs:simpleContent>
      </xs:complexType>
      
    </xs:schema>
  ")
  
  let validation_result = XmlSerializer::validate_with_schema(xml_serializer, xml_string, xml_schema)
  assert_true(validation_result.valid)
  
  // 8. 测试XML特殊字符处理
  let special_config = TelemetryConfiguration({
    service_name: "service&with<special>chars",
    service_version: "1.0\"with'quotes",
    sampling_rate: 0.5,
    exporters: [],
    processors: [],
    resource_attributes: [
      ("special.key", "value&with<special>characters"),
      ("unicode.key", "值包含中文")
    ]
  })
  
  let special_serialize_result = XmlSerializer::serialize(xml_serializer, special_config)
  assert_true(special_serialize_result.success)
  
  let special_xml = special_serialize_result.data!
  assert_true(special_xml.contains("service&amp;with&lt;special&gt;chars"))
  assert_true(special_xml.contains("1.0&quot;with&apos;quotes"))
  assert_true(special_xml.contains("值包含中文"))
  
  // 9. 测试XML文件读写
  let file_serialize_result = XmlSerializer::serialize_to_file(xml_serializer, telemetry_config, "/tmp/telemetry_config.xml")
  assert_true(file_serialize_result.success)
  
  let file_deserialize_result = XmlSerializer::deserialize_from_file(xml_serializer, "/tmp/telemetry_config.xml", TelemetryConfiguration)
  assert_true(file_deserialize_result.success)
  assert_eq(file_deserialize_result.data.service_name, "payment-service")
}

// 测试7: 数据序列化性能测试
test "数据序列化性能测试" {
  // 1. 创建性能测试数据
  let performance_data = []
  for i in 0..=1000 {
    let telemetry_span = TelemetrySpan({
      trace_id: "trace-perf-" + i.to_string(),
      span_id: "span-perf-" + i.to_string(),
      parent_span_id: if i > 0 { Some("span-perf-" + (i - 1).to_string()) } else { None },
      operation_name: "operation_" + (i % 10).to_string(),
      start_time: 1735689600000000000L + (i as Int64 * 1000000L),
      end_time: 1735689600000000000L + (i as Int64 * 1000000L) + 500000L,
      status: if i % 20 == 0 { "error" } else { "ok" },
      attributes: [
        ("service.name", "service-" + (i % 5).to_string()),
        ("host.name", "host-" + (i % 10).to_string()),
        ("iteration", i.to_string())
      ],
      events: [],
      links: []
    })
    performance_data.push(telemetry_span)
  }
  
  // 2. 测试JSON序列化性能
  let json_serializer = JsonSerializer::new()
  let json_start_time = Time::now()
  
  let json_serialize_results = []
  for data in performance_data {
    let result = JsonSerializer::serialize(json_serializer, data)
    json_serialize_results.push(result)
  }
  
  let json_serialize_end_time = Time::now()
  let json_serialize_duration = json_serialize_end_time - json_start_time
  
  // 3. 测试JSON反序列化性能
  let json_deserialize_start_time = Time::now()
  
  let json_deserialize_results = []
  for result in json_serialize_results {
    if result.success {
      let deserialize_result = JsonSerializer::deserialize(json_serializer, result.data!, TelemetrySpan)
      json_deserialize_results.push(deserialize_result)
    }
  }
  
  let json_deserialize_end_time = Time::now()
  let json_deserialize_duration = json_deserialize_end_time - json_deserialize_start_time
  
  // 4. 测试Protobuf序列化性能
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_start_time = Time::now()
  
  let protobuf_serialize_results = []
  for data in performance_data {
    let result = ProtobufSerializer::serialize(protobuf_serializer, data)
    protobuf_serialize_results.push(result)
  }
  
  let protobuf_serialize_end_time = Time::now()
  let protobuf_serialize_duration = protobuf_serialize_end_time - protobuf_start_time
  
  // 5. 测试Protobuf反序列化性能
  let protobuf_deserialize_start_time = Time::now()
  
  let protobuf_deserialize_results = []
  for result in protobuf_serialize_results {
    if result.success {
      let deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, result.data!, TelemetrySpan)
      protobuf_deserialize_results.push(deserialize_result)
    }
  }
  
  let protobuf_deserialize_end_time = Time::now()
  let protobuf_deserialize_duration = protobuf_deserialize_end_time - protobuf_deserialize_start_time
  
  // 6. 测试MessagePack序列化性能
  let msgpack_serializer = MessagePackSerializer::new()
  let msgpack_start_time = Time::now()
  
  let msgpack_serialize_results = []
  for data in performance_data {
    let result = MessagePackSerializer::serialize(msgpack_serializer, data)
    msgpack_serialize_results.push(result)
  }
  
  let msgpack_serialize_end_time = Time::now()
  let msgpack_serialize_duration = msgpack_serialize_end_time - msgpack_start_time
  
  // 7. 测试MessagePack反序列化性能
  let msgpack_deserialize_start_time = Time::now()
  
  let msgpack_deserialize_results = []
  for result in msgpack_serialize_results {
    if result.success {
      let deserialize_result = MessagePackSerializer::deserialize(msgpack_serializer, result.data!, TelemetrySpan)
      msgpack_deserialize_results.push(deserialize_result)
    }
  }
  
  let msgpack_deserialize_end_time = Time::now()
  let msgpack_deserialize_duration = msgpack_deserialize_end_time - msgpack_deserialize_start_time
  
  // 8. 验证性能结果
  assert_eq(json_serialize_results.length(), 1001)
  assert_eq(json_deserialize_results.length(), 1001)
  assert_eq(protobuf_serialize_results.length(), 1001)
  assert_eq(protobuf_deserialize_results.length(), 1001)
  assert_eq(msgpack_serialize_results.length(), 1001)
  assert_eq(msgpack_deserialize_results.length(), 1001)
  
  // 9. 比较序列化性能
  let json_avg_serialize_time = json_serialize_duration / 1001
  let protobuf_avg_serialize_time = protobuf_serialize_duration / 1001
  let msgpack_avg_serialize_time = msgpack_serialize_duration / 1001
  
  // Protobuf和MessagePack应该比JSON更快
  assert_true(protobuf_avg_serialize_time < json_avg_serialize_time)
  assert_true(msgpack_avg_serialize_time < json_avg_serialize_time)
  
  // 10. 比较反序列化性能
  let json_avg_deserialize_time = json_deserialize_duration / 1001
  let protobuf_avg_deserialize_time = protobuf_deserialize_duration / 1001
  let msgpack_avg_deserialize_time = msgpack_deserialize_duration / 1001
  
  // Protobuf和MessagePack应该比JSON更快
  assert_true(protobuf_avg_deserialize_time < json_avg_deserialize_time)
  assert_true(msgpack_avg_deserialize_time < json_avg_deserialize_time)
  
  // 11. 比较数据大小
  let json_total_size = json_serialize_results.reduce(fn(acc, result) {
    if result.success { acc + result.data!.length() } else { acc }
  }, 0)
  
  let protobuf_total_size = protobuf_serialize_results.reduce(fn(acc, result) {
    if result.success { acc + result.data!.length() } else { acc }
  }, 0)
  
  let msgpack_total_size = msgpack_serialize_results.reduce(fn(acc, result) {
    if result.success { acc + result.data!.length() } else { acc }
  }, 0)
  
  // Protobuf和MessagePack应该比JSON更紧凑
  assert_true(protobuf_total_size < json_total_size)
  assert_true(msgpack_total_size < json_total_size)
  
  // 12. 生成性能报告
  let performance_report = {
    json: {
      serialize_time: json_serialize_duration,
      deserialize_time: json_deserialize_duration,
      avg_serialize_time: json_avg_serialize_time,
      avg_deserialize_time: json_avg_deserialize_time,
      total_size: json_total_size
    },
    protobuf: {
      serialize_time: protobuf_serialize_duration,
      deserialize_time: protobuf_deserialize_duration,
      avg_serialize_time: protobuf_avg_serialize_time,
      avg_deserialize_time: protobuf_avg_deserialize_time,
      total_size: protobuf_total_size
    },
    msgpack: {
      serialize_time: msgpack_serialize_duration,
      deserialize_time: msgpack_deserialize_duration,
      avg_serialize_time: msgpack_avg_serialize_time,
      avg_deserialize_time: msgpack_avg_deserialize_time,
      total_size: msgpack_total_size
    }
  }
  
  // 验证性能报告完整性
  assert_true(performance_report.json.serialize_time > 0)
  assert_true(performance_report.protobuf.serialize_time > 0)
  assert_true(performance_report.msgpack.serialize_time > 0)
}

// 测试8: 序列化数据完整性验证
test "序列化数据完整性验证" {
  // 1. 创建数据完整性验证器
  let integrity_validator = DataIntegrityValidator::new()
  
  // 2. 创建复杂的遥测数据
  let complex_data = ComplexTelemetryData({
    spans: [
      TelemetrySpan({
        trace_id: "trace-integrity-123",
        span_id: "span-integrity-456",
        parent_span_id: None,
        operation_name: "root_operation",
        start_time: 1735689600000000000L,
        end_time: 1735689601000000000L,
        status: "ok",
        attributes: [
          ("service.name", "test-service"),
          ("service.version", "1.0.0"),
          ("host.name", "test-host")
        ],
        events: [],
        links: []
      }),
      TelemetrySpan({
        trace_id: "trace-integrity-123",
        span_id: "span-integrity-789",
        parent_span_id: Some("span-integrity-456"),
        operation_name: "child_operation",
        start_time: 1735689600200000000L,
        end_time: 1735689600700000000L,
        status: "ok",
        attributes: [
          ("service.name", "test-service"),
          ("service.version", "1.0.0"),
          ("component", "database")
        ],
        events: [
          SpanEvent({
            name: "query.start",
            timestamp: 1735689600200000000L,
            attributes: [("query.type", "select")]
          }),
          SpanEvent({
            name: "query.end",
            timestamp: 1735689600700000000L,
            attributes: [("rows.affected", "10")]
          })
        ],
        links: []
      })
    ],
    metrics: [
      TelemetryMetric({
        metric_name: "cpu_usage",
        metric_value: 75.5,
        metric_unit: Some("percent"),
        timestamp: 1735689600000000000L,
        tags: {
          "host": "test-host",
          "service": "test-service"
        },
        resource: Some(Resource({
          service_name: "test-service",
          service_version: "1.0.0",
          host_name: "test-host"
        }))
      }),
      TelemetryMetric({
        metric_name: "memory_usage",
        metric_value: 60.2,
        metric_unit: Some("percent"),
        timestamp: 1735689600000000000L,
        tags: {
          "host": "test-host",
          "service": "test-service"
        },
        resource: Some(Resource({
          service_name: "test-service",
          service_version: "1.0.0",
          host_name: "test-host"
        }))
      })
    ],
    logs: [
      LogRecord({
        timestamp: 1735689600300000000L,
        severity: "INFO",
        body: "Processing request",
        resource: {
          service_name: "test-service",
          service_version: "1.0.0",
          host_name: "test-host"
        },
        attributes: [
          ("request.id", "req-123"),
          ("user.id", "user-456")
        ],
        trace_id: Some("trace-integrity-123"),
        span_id: Some("span-integrity-789")
      })
    ]
  })
  
  // 3. 测试JSON序列化完整性
  let json_serializer = JsonSerializer::new()
  let json_result = JsonSerializer::serialize(json_serializer, complex_data)
  assert_true(json_result.success)
  
  // 计算原始数据的校验和
  let original_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, complex_data)
  
  // 计算序列化数据的校验和
  let json_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, json_result.data!)
  
  // 4. 测试JSON反序列化完整性
  let json_deserialize_result = JsonSerializer::deserialize(json_serializer, json_result.data!, ComplexTelemetryData)
  assert_true(json_deserialize_result.success)
  
  // 验证反序列化数据的完整性
  let deserialized_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, json_deserialize_result.data)
  assert_eq(original_checksum, deserialized_checksum)
  
  // 5. 测试Protobuf序列化完整性
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_result = ProtobufSerializer::serialize(protobuf_serializer, complex_data)
  assert_true(protobuf_result.success)
  
  let protobuf_deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, protobuf_result.data!, ComplexTelemetryData)
  assert_true(protobuf_deserialize_result.success)
  
  let protobuf_deserialized_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, protobuf_deserialize_result.data)
  assert_eq(original_checksum, protobuf_deserialized_checksum)
  
  // 6. 测试数据损坏检测
  // 模拟JSON数据损坏
  let corrupted_json = json_result.data!.replace("test-service", "corrupted-service").replace("75.5", "999.9")
  let corrupted_deserialize_result = JsonSerializer::deserialize(json_serializer, corrupted_json, ComplexTelemetryData)
  assert_true(corrupted_deserialize_result.success)
  
  let corrupted_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, corrupted_deserialize_result.data)
  assert_true(corrupted_checksum != original_checksum)
  
  // 7. 测试序列化数据签名验证
  let signature_generator = DataSignatureGenerator::new()
  
  // 生成原始数据签名
  let original_signature = DataSignatureGenerator::sign_data(signature_generator, complex_data)
  
  // 生成序列化数据签名
  let json_signature = DataSignatureGenerator::sign_data(signature_generator, json_result.data!)
  
  // 验证签名一致性（相同数据应该产生相同签名）
  assert_eq(original_signature, json_signature)
  
  // 8. 测试序列化格式转换完整性
  // JSON -> Protobuf -> JSON
  let json_to_protobuf = ProtobufSerializer::serialize(protobuf_serializer, json_deserialize_result.data)
  let protobuf_to_json = ProtobufSerializer::deserialize(protobuf_serializer, json_to_protobuf.data!, ComplexTelemetryData)
  let final_json_result = JsonSerializer::serialize(json_serializer, protobuf_to_json.data)
  
  let final_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, final_json_result.data)
  assert_eq(json_checksum, final_checksum)
  
  // 9. 测试大数据量完整性
  let large_data = []
  for i in 0..=10000 {
    let span = TelemetrySpan({
      trace_id: "trace-large-" + (i / 100).to_string(),
      span_id: "span-large-" + i.to_string(),
      parent_span_id: if i > 0 { Some("span-large-" + (i - 1).to_string()) } else { None },
      operation_name: "operation_" + (i % 50).to_string(),
      start_time: 1735689600000000000L + (i as Int64 * 1000L),
      end_time: 1735689600000000000L + (i as Int64 * 1000L) + 500L,
      status: if i % 100 == 0 { "error" } else { "ok" },
      attributes: [
        ("service.name", "service-" + (i % 10).to_string()),
        ("host.name", "host-" + (i % 20).to_string()),
        ("iteration", i.to_string()),
        ("data", "x".repeat(100)) // 每个跨度包含100字节数据
      ],
      events: [],
      links: []
    })
    large_data.push(span)
  }
  
  let large_json_result = JsonSerializer::serialize(json_serializer, large_data)
  assert_true(large_json_result.success)
  
  let large_deserialize_result = JsonSerializer::deserialize(json_serializer, large_json_result.data!, Array[TelemetrySpan])
  assert_true(large_deserialize_result.success)
  assert_eq(large_deserialize_result.data.length(), 10001)
  
  // 验证大数据量完整性
  let large_original_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, large_data)
  let large_deserialized_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, large_deserialize_result.data)
  assert_eq(large_original_checksum, large_deserialized_checksum)
  
  // 10. 测试并发序列化完整性
  let concurrent_data = []
  for i in 0..=100 {
    let data = ComplexTelemetryData({
      spans: [
        TelemetrySpan({
          trace_id: "trace-concurrent-" + i.to_string(),
          span_id: "span-concurrent-" + i.to_string(),
          parent_span_id: None,
          operation_name: "concurrent_operation",
          start_time: 1735689600000000000L + (i as Int64 * 1000000L),
          end_time: 1735689600000000000L + (i as Int64 * 1000000L) + 100000L,
          status: "ok",
          attributes: [
            ("thread.id", i.to_string()),
            ("data", "concurrent_data_" + i.to_string())
          ],
          events: [],
          links: []
        })
      ],
      metrics: [],
      logs: []
    })
    concurrent_data.push(data)
  }
  
  // 并发序列化
  let concurrent_serialize_results = []
  for data in concurrent_data {
    let result = JsonSerializer::serialize(json_serializer, data)
    concurrent_serialize_results.push(result)
  }
  
  // 并发反序列化
  let concurrent_deserialize_results = []
  for result in concurrent_serialize_results {
    if result.success {
      let deserialize_result = JsonSerializer::deserialize(json_serializer, result.data!, ComplexTelemetryData)
      concurrent_deserialize_results.push(deserialize_result)
    }
  }
  
  // 验证并发序列化完整性
  assert_eq(concurrent_serialize_results.length(), 101)
  assert_eq(concurrent_deserialize_results.length(), 101)
  
  for i in 0..concurrent_data.length() {
    let original_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, concurrent_data[i])
    let deserialized_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, concurrent_deserialize_results[i].data)
    assert_eq(original_checksum, deserialized_checksum)
  }
}