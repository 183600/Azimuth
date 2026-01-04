// Azimuth 数据序列化和反序列化增强测试
// 专注于数据序列化、反序列化、格式转换和数据完整性

// 测试1: JSON序列化和反序列化
test "JSON序列化和反序列化测试" {
  // 创建JSON序列化器
  let json_serializer = JsonSerializer::new()
  
  // 定义测试数据结构
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)],
    events: Array[SpanEvent]
  }
  
  type SpanEvent = {
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }
  
  // 创建测试数据
  let test_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("parent-11111"),
    operation_name: "database.query",
    start_time: 1640995200000,
    end_time: 1640995250000,
    status: "ok",
    attributes: [
      ("service.name", "database.service"),
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users WHERE id = ?")
    ],
    events: [
      {
        timestamp: 1640995220000,
        name: "query.start",
        attributes: [
          ("query.type", "select"),
          ("table", "users")
        ]
      },
      {
        timestamp: 1640995245000,
        name: "query.complete",
        attributes: [
          ("rows.returned", "42"),
          ("execution.time.ms", "2500")
        ]
      }
    ]
  }
  
  // 测试JSON序列化
  let json_result = JsonSerializer::serialize(json_serializer, test_span)
  assert_true(json_result.is_ok())
  
  let json_string = json_result.unwrap()
  assert_true(json_string.length() > 0)
  assert_true(json_string.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json_string.contains("\"span_id\":\"span-67890\""))
  assert_true(json_string.contains("\"operation_name\":\"database.query\""))
  assert_true(json_string.contains("\"status\":\"ok\""))
  
  // 测试JSON反序列化
  let deserialize_result = JsonSerializer::deserialize(json_serializer, json_string)
  assert_true(deserialize_result.is_ok())
  
  let deserialized_span = deserialize_result.unwrap()
  
  // 验证反序列化结果
  assert_eq(deserialized_span.trace_id, test_span.trace_id)
  assert_eq(deserialized_span.span_id, test_span.span_id)
  assert_eq(deserialized_span.parent_span_id, test_span.parent_span_id)
  assert_eq(deserialized_span.operation_name, test_span.operation_name)
  assert_eq(deserialized_span.start_time, test_span.start_time)
  assert_eq(deserialized_span.end_time, test_span.end_time)
  assert_eq(deserialized_span.status, test_span.status)
  
  // 验证属性
  assert_eq(deserialized_span.attributes.length(), test_span.attributes.length())
  for (key, value) in deserialized_span.attributes {
    let original_value = test_span.attributes.find(fn((k, _)) { k == key })
    assert_true(original_value != None)
    match original_value {
      Some((_, v)) => assert_eq(value, v)
      None => assert_true(false)
    }
  }
  
  // 验证事件
  assert_eq(deserialized_span.events.length(), test_span.events.length())
  for i in 0..=deserialized_span.events.length() - 1 {
    let event = deserialized_span.events[i]
    let original_event = test_span.events[i]
    
    assert_eq(event.timestamp, original_event.timestamp)
    assert_eq(event.name, original_event.name)
    assert_eq(event.attributes.length(), original_event.attributes.length())
  }
  
  // 测试复杂数据结构的序列化
  let telemetry_data = {
    spans: [test_span, test_span],
    metrics: [
      {
        name: "http.requests.total",
        value: 1250.5,
        timestamp: 1640995200000,
        attributes: [
          ("service.name", "api.gateway"),
          ("http.method", "GET")
        ]
      }
    ],
    logs: [
      {
        timestamp: 1640995230000,
        level: "info",
        message: "Request processed successfully",
        attributes: [
          ("service.name", "api.gateway"),
          ("trace.id", "trace-12345")
        ]
      }
    ]
  }
  
  // 序列化复杂数据结构
  let complex_json_result = JsonSerializer::serialize(json_serializer, telemetry_data)
  assert_true(complex_json_result.is_ok())
  
  let complex_json_string = complex_json_result.unwrap()
  assert_true(complex_json_string.contains("\"spans\""))
  assert_true(complex_json_string.contains("\"metrics\""))
  assert_true(complex_json_string.contains("\"logs\""))
  
  // 反序列化复杂数据结构
  let complex_deserialize_result = JsonSerializer::deserialize(json_serializer, complex_json_string)
  assert_true(complex_deserialize_result.is_ok())
  
  let deserialized_data = complex_deserialize_result.unwrap()
  
  // 验证复杂数据结构
  assert_eq(deserialized_data.spans.length(), telemetry_data.spans.length())
  assert_eq(deserialized_data.metrics.length(), telemetry_data.metrics.length())
  assert_eq(deserialized_data.logs.length(), telemetry_data.logs.length())
  
  // 测试错误处理
  let invalid_json = "{ invalid json }"
  let error_result = JsonSerializer::deserialize(json_serializer, invalid_json)
  assert_true(error_result.is_err())
  
  // 测试大型数据序列化
  let large_data = []
  for i in 0..=1000 {
    large_data.push({
      id: i,
      name: "item_" + i.to_string(),
      description: "This is item number " + i.to_string(),
      metadata: {
        created_at: 1640995200000 + i * 1000,
        updated_at: 1640995200000 + i * 2000,
        tags: ["tag1", "tag2", "tag3"],
        properties: {
          prop1: "value1_" + i.to_string(),
          prop2: "value2_" + i.to_string(),
          prop3: i * 10
        }
      }
    })
  }
  
  let large_data_result = JsonSerializer::serialize(json_serializer, large_data)
  assert_true(large_data_result.is_ok())
  
  let large_json_string = large_data_result.unwrap()
  assert_true(large_json_string.length() > 100000)  // 应该是一个很大的字符串
  
  // 验证大型数据反序列化
  let large_deserialize_result = JsonSerializer::deserialize(json_serializer, large_json_string)
  assert_true(large_deserialize_result.is_ok())
  
  let large_deserialized_data = large_deserialize_result.unwrap()
  assert_eq(large_deserialized_data.length(), large_data.length())
}

// 测试2: 二进制序列化和反序列化
test "二进制序列化和反序列化测试" {
  // 创建二进制序列化器
  let binary_serializer = BinarySerializer::new()
  
  // 配置序列化选项
  BinarySerializer::configure(binary_serializer, {
    endian: "little",
    compression: "gzip",
    checksum: true,
    version: 1
  })
  
  // 定义测试数据结构
  type MetricData = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[String],
    metadata: Map[String, String]
  }
  
  // 创建测试数据
  let test_metric = {
    name: "cpu.usage",
    value: 75.5,
    timestamp: 1640995200000,
    tags: ["host:server1", "region:us-west", "env:production"],
    metadata: Map::from_array([
      ("unit", "percent"),
      ("description", "CPU usage percentage"),
      ("collector", "system.metrics")
    ])
  }
  
  // 测试二进制序列化
  let binary_result = BinarySerializer::serialize(binary_serializer, test_metric)
  assert_true(binary_result.is_ok())
  
  let binary_data = binary_result.unwrap()
  assert_true(binary_data.length() > 0)
  
  // 测试二进制反序列化
  let deserialize_result = BinarySerializer::deserialize(binary_serializer, binary_data)
  assert_true(deserialize_result.is_ok())
  
  let deserialized_metric = deserialize_result.unwrap()
  
  // 验证反序列化结果
  assert_eq(deserialized_metric.name, test_metric.name)
  assert_eq(deserialized_metric.value, test_metric.value)
  assert_eq(deserialized_metric.timestamp, test_metric.timestamp)
  assert_eq(deserialized_metric.tags.length(), test_metric.tags.length())
  
  for tag in deserialized_metric.tags {
    assert_true(test_metric.tags.contains(tag))
  }
  
  // 验证元数据
  assert_eq(deserialized_metric.metadata.size(), test_metric.metadata.size())
  for (key, value) in deserialized_metric.metadata.entries() {
    let original_value = test_metric.metadata.get(key)
    assert_true(original_value != None)
    assert_eq(value, original_value.unwrap())
  }
  
  // 测试不同数据类型的序列化
  let mixed_data = {
    int_value: 42,
    float_value: 3.14159,
    bool_value: true,
    string_value: "test_string",
    array_value: [1, 2, 3, 4, 5],
    map_value: Map::from_array([
      ("key1", "value1"),
      ("key2", "value2")
    ]),
    optional_value: Some("optional_string"),
    null_value: None
  }
  
  // 序列化混合数据类型
  let mixed_result = BinarySerializer::serialize(binary_serializer, mixed_data)
  assert_true(mixed_result.is_ok())
  
  let mixed_binary_data = mixed_result.unwrap()
  
  // 反序列化混合数据类型
  let mixed_deserialize_result = BinarySerializer::deserialize(binary_serializer, mixed_binary_data)
  assert_true(mixed_deserialize_result.is_ok())
  
  let deserialized_mixed = mixed_deserialize_result.unwrap()
  
  // 验证混合数据类型
  assert_eq(deserialized_mixed.int_value, mixed_data.int_value)
  assert_eq(deserialized_mixed.float_value, mixed_data.float_value)
  assert_eq(deserialized_mixed.bool_value, mixed_data.bool_value)
  assert_eq(deserialized_mixed.string_value, mixed_data.string_value)
  assert_eq(deserialized_mixed.array_value, mixed_data.array_value)
  assert_eq(deserialized_mixed.map_value.size(), mixed_data.map_value.size())
  assert_eq(deserialized_mixed.optional_value, mixed_data.optional_value)
  assert_eq(deserialized_mixed.null_value, mixed_data.null_value)
  
  // 测试大型二进制数据序列化
  let large_binary_data = []
  for i in 0..=10000 {
    large_binary_data.push({
      id: i,
      data: "binary_data_" + i.to_string(),
      timestamp: 1640995200000 + i * 100,
      metrics: [
        i * 1.5,
        i * 2.5,
        i * 3.5
      ]
    })
  }
  
  // 序列化大型二进制数据
  let large_binary_result = BinarySerializer::serialize(binary_serializer, large_binary_data)
  assert_true(large_binary_result.is_ok())
  
  let large_binary_bytes = large_binary_result.unwrap()
  
  // 验证压缩效果
  let json_serializer = JsonSerializer::new()
  let json_result = JsonSerializer::serialize(json_serializer, large_binary_data)
  assert_true(json_result.is_ok())
  
  let json_size = json_result.unwrap().length()
  let binary_size = large_binary_bytes.length()
  
  // 二进制序列化应该产生更小的数据（特别是启用压缩后）
  assert_true(binary_size < json_size)
  
  // 验证大型二进制数据反序列化
  let large_binary_deserialize_result = BinarySerializer::deserialize(binary_serializer, large_binary_bytes)
  assert_true(large_binary_deserialize_result.is_ok())
  
  let large_deserialized_binary = large_binary_deserialize_result.unwrap()
  assert_eq(large_deserialized_binary.length(), large_binary_data.length())
  
  // 测试二进制数据损坏检测
  let corrupted_data = binary_data.slice(0, binary_data.length() - 5) + [1, 2, 3, 4, 5]
  let corrupt_result = BinarySerializer::deserialize(binary_serializer, corrupted_data)
  assert_true(corrupt_result.is_err())
  
  // 获取序列化统计
  let serialization_stats = BinarySerializer::get_statistics(binary_serializer)
  assert_true(serialization_stats.total_serializations > 0)
  assert_true(serialization_stats.total_deserializations > 0)
  assert_true(serialization_stats.avg_serialization_time_ms > 0)
  assert_true(serialization_stats.avg_deserialization_time_ms > 0)
  assert_true(serialization_stats.compression_ratio > 0)
}

// 测试3: 协议缓冲区序列化
test "协议缓冲区序列化测试" {
  // 创建协议缓冲区序列化器
  let protobuf_serializer = ProtobufSerializer::new()
  
  // 定义协议缓冲区模式
  let telemetry_schema = "
  syntax = \"proto3\";
  
  package telemetry;
  
  message Span {
    string trace_id = 1;
    string span_id = 2;
    optional string parent_span_id = 3;
    string operation_name = 4;
    int64 start_time = 5;
    int64 end_time = 6;
    string status = 7;
    repeated Attribute attributes = 8;
    repeated Event events = 9;
  }
  
  message Attribute {
    string key = 1;
    string value = 2;
  }
  
  message Event {
    int64 timestamp = 1;
    string name = 2;
    repeated Attribute attributes = 3;
  }
  
  message Metric {
    string name = 1;
    double value = 2;
    int64 timestamp = 3;
    repeated string tags = 4;
  }
  
  message TelemetryData {
    repeated Span spans = 1;
    repeated Metric metrics = 2;
  }
  "
  
  // 注册模式
  ProtobufSerializer::register_schema(protobuf_serializer, "telemetry", telemetry_schema)
  
  // 创建测试数据
  let test_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("parent-11111"),
    operation_name: "http.request",
    start_time: 1640995200000,
    end_time: 1640995250000,
    status: "ok",
    attributes: [
      { key: "service.name", value: "api.gateway" },
      { key: "http.method", value: "GET" },
      { key: "http.url", value: "/api/users" }
    ],
    events: [
      {
        timestamp: 1640995220000,
        name: "request.start",
        attributes: [
          { key: "user.id", value: "12345" }
        ]
      },
      {
        timestamp: 1640995245000,
        name: "request.complete",
        attributes: [
          { key: "status.code", value: "200" }
        ]
      }
    ]
  }
  
  let test_metric = {
    name: "http.request.duration",
    value: 250.5,
    timestamp: 1640995250000,
    tags: ["service:api.gateway", "method:GET", "status:200"]
  }
  
  let telemetry_data = {
    spans: [test_span],
    metrics: [test_metric]
  }
  
  // 测试协议缓冲区序列化
  let protobuf_result = ProtobufSerializer::serialize(protobuf_serializer, "telemetry", telemetry_data)
  assert_true(protobuf_result.is_ok())
  
  let protobuf_data = protobuf_result.unwrap()
  assert_true(protobuf_data.length() > 0)
  
  // 测试协议缓冲区反序列化
  let deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, "telemetry", protobuf_data)
  assert_true(deserialize_result.is_ok())
  
  let deserialized_data = deserialize_result.unwrap()
  
  // 验证反序列化结果
  assert_eq(deserialized_data.spans.length(), telemetry_data.spans.length())
  assert_eq(deserialized_data.metrics.length(), telemetry_data.metrics.length())
  
  let deserialized_span = deserialized_data.spans[0]
  assert_eq(deserialized_span.trace_id, test_span.trace_id)
  assert_eq(deserialized_span.span_id, test_span.span_id)
  assert_eq(deserialized_span.parent_span_id, test_span.parent_span_id)
  assert_eq(deserialized_span.operation_name, test_span.operation_name)
  assert_eq(deserialized_span.start_time, test_span.start_time)
  assert_eq(deserialized_span.end_time, test_span.end_time)
  assert_eq(deserialized_span.status, test_span.status)
  
  // 验证属性
  assert_eq(deserialized_span.attributes.length(), test_span.attributes.length())
  for attr in deserialized_span.attributes {
    let original_attr = test_span.attributes.find(fn(a) { a.key == attr.key })
    assert_true(original_attr != None)
    match original_attr {
      Some(a) => assert_eq(attr.value, a.value)
      None => assert_true(false)
    }
  }
  
  // 验证事件
  assert_eq(deserialized_span.events.length(), test_span.events.length())
  for i in 0..=deserialized_span.events.length() - 1 {
    let event = deserialized_span.events[i]
    let original_event = test_span.events[i]
    
    assert_eq(event.timestamp, original_event.timestamp)
    assert_eq(event.name, original_event.name)
    assert_eq(event.attributes.length(), original_event.attributes.length())
  }
  
  // 验证指标
  let deserialized_metric = deserialized_data.metrics[0]
  assert_eq(deserialized_metric.name, test_metric.name)
  assert_eq(deserialized_metric.value, test_metric.value)
  assert_eq(deserialized_metric.timestamp, test_metric.timestamp)
  assert_eq(deserialized_metric.tags.length(), test_metric.tags.length())
  
  // 测试协议缓冲区版本兼容性
  let v2_schema = "
  syntax = \"proto3\";
  
  package telemetry;
  
  message Span {
    string trace_id = 1;
    string span_id = 2;
    optional string parent_span_id = 3;
    string operation_name = 4;
    int64 start_time = 5;
    int64 end_time = 6;
    string status = 7;
    repeated Attribute attributes = 8;
    repeated Event events = 9;
    string service_name = 10;  // 新增字段
  }
  
  message Attribute {
    string key = 1;
    string value = 2;
  }
  
  message Event {
    int64 timestamp = 1;
    string name = 2;
    repeated Attribute attributes = 3;
  }
  "
  
  // 注册V2模式
  ProtobufSerializer::register_schema(protobuf_serializer, "telemetry_v2", v2_schema)
  
  // 使用V1模式序列化，V2模式反序列化（向前兼容）
  let v2_deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, "telemetry_v2", protobuf_data)
  assert_true(v2_deserialize_result.is_ok())
  
  let v2_deserialized_data = v2_deserialize_result.unwrap()
  assert_eq(v2_deserialized_data.spans.length(), telemetry_data.spans.length())
  
  // 新字段应该有默认值
  let v2_span = v2_deserialized_data.spans[0]
  assert_eq(v2_span.service_name, "")  // 默认空字符串
  
  // 测试大型协议缓冲区数据
  let large_telemetry_data = {
    spans: [],
    metrics: []
  }
  
  // 生成大量span数据
  for i in 0..=1000 {
    large_telemetry_data.spans.push({
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      parent_span_id: if i > 0 { Some("span-" + (i - 1).to_string()) } else { None },
      operation_name: "operation_" + (i % 10).to_string(),
      start_time: 1640995200000 + i * 1000,
      end_time: 1640995250000 + i * 1000,
      status: if i % 50 == 0 { "error" } else { "ok" },
      attributes: [
        { key: "service.name", value: "service-" + (i % 5).to_string() },
        { key: "instance.id", value: "instance-" + (i % 10).to_string() }
      ],
      events: []
    })
  }
  
  // 生成大量指标数据
  for i in 0..=500 {
    large_telemetry_data.metrics.push({
      name: "metric_" + (i % 20).to_string(),
      value: i * 1.5,
      timestamp: 1640995200000 + i * 2000,
      tags: ["tag1:value" + (i % 5).to_string(), "tag2:value" + (i % 3).to_string()]
    })
  }
  
  // 序列化大型协议缓冲区数据
  let large_protobuf_result = ProtobufSerializer::serialize(protobuf_serializer, "telemetry", large_telemetry_data)
  assert_true(large_protobuf_result.is_ok())
  
  let large_protobuf_data = large_protobuf_result.unwrap()
  
  // 验证协议缓冲区大小效率
  let json_serializer = JsonSerializer::new()
  let json_result = JsonSerializer::serialize(json_serializer, large_telemetry_data)
  assert_true(json_result.is_ok())
  
  let json_size = json_result.unwrap().length()
  let protobuf_size = large_protobuf_data.length()
  
  // 协议缓冲区应该比JSON更紧凑
  assert_true(protobuf_size < json_size)
  
  // 验证大型协议缓冲区数据反序列化
  let large_protobuf_deserialize_result = ProtobufSerializer::deserialize(protobuf_serializer, "telemetry", large_protobuf_data)
  assert_true(large_protobuf_deserialize_result.is_ok())
  
  let large_deserialized_protobuf = large_protobuf_deserialize_result.unwrap()
  assert_eq(large_deserialized_protobuf.spans.length(), large_telemetry_data.spans.length())
  assert_eq(large_deserialized_protobuf.metrics.length(), large_telemetry_data.metrics.length())
  
  // 获取协议缓冲区统计
  let protobuf_stats = ProtobufSerializer::get_statistics(protobuf_serializer)
  assert_true(protobuf_stats.total_serializations > 0)
  assert_true(protobuf_stats.total_deserializations > 0)
  assert_true(protobuf_stats.avg_serialization_time_ms > 0)
  assert_true(protobuf_stats.avg_deserialization_time_ms > 0)
  assert_true(protobuf_stats.compression_ratio > 0)
}

// 测试4: Avro序列化
test "Avro序列化测试" {
  // 创建Avro序列化器
  let avro_serializer = AvroSerializer::new()
  
  // 定义Avro模式
  let telemetry_avro_schema = {
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
          "symbols": ["metric", "log", "span", "trace"]
        }
      },
      {
        "name": "service_name",
        "type": "string"
      },
      {
        "name": "data",
        "type": [
          "null",
          {
            "type": "record",
            "name": "MetricData",
            "fields": [
              {
                "name": "name",
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
          },
          {
            "type": "record",
            "name": "LogData",
            "fields": [
              {
                "name": "level",
                "type": {
                  "type": "enum",
                  "name": "LogLevel",
                  "symbols": ["debug", "info", "warn", "error", "fatal"]
                }
              },
              {
                "name": "message",
                "type": "string"
              }
            ]
          },
          {
            "type": "record",
            "name": "SpanData",
            "fields": [
              {
                "name": "trace_id",
                "type": "string"
              },
              {
                "name": "span_id",
                "type": "string"
              },
              {
                "name": "operation_name",
                "type": "string"
              },
              {
                "name": "duration_ms",
                "type": "long"
              }
            ]
          }
        ]
      },
      {
        "name": "tags",
        "type": {
          "type": "map",
          "values": "string"
        }
      }
    ]
  }
  
  // 注册Avro模式
  AvroSerializer::register_schema(avro_serializer, "telemetry_event", telemetry_avro_schema)
  
  // 创建测试数据
  let metric_event = {
    event_id: "event-12345",
    timestamp: 1640995200000,
    event_type: "metric",
    service_name: "api.service",
    data: {
      name: "request.duration",
      value: 250.5,
      unit: "milliseconds"
    },
    tags: {
      "endpoint": "/api/users",
      "method": "GET",
      "status": "200"
    }
  }
  
  let log_event = {
    event_id: "event-67890",
    timestamp: 1640995250000,
    event_type: "log",
    service_name: "auth.service",
    data: {
      level: "info",
      message: "User authentication successful"
    },
    tags: {
      "user.id": "12345",
      "trace.id": "trace-12345"
    }
  }
  
  let span_event = {
    event_id: "event-11111",
    timestamp: 1640995300000,
    event_type: "span",
    service_name: "database.service",
    data: {
      trace_id: "trace-12345",
      span_id: "span-67890",
      operation_name: "execute_query",
      duration_ms: 150
    },
    tags: {
      "db.type": "postgresql",
      "table": "users"
    }
  }
  
  // 测试Avro序列化
  let metric_avro_result = AvroSerializer::serialize(avro_serializer, "telemetry_event", metric_event)
  assert_true(metric_avro_result.is_ok())
  
  let metric_avro_data = metric_avro_result.unwrap()
  assert_true(metric_avro_data.length() > 0)
  
  // 测试Avro反序列化
  let metric_deserialize_result = AvroSerializer::deserialize(avro_serializer, "telemetry_event", metric_avro_data)
  assert_true(metric_deserialize_result.is_ok())
  
  let deserialized_metric = metric_deserialize_result.unwrap()
  
  // 验证反序列化结果
  assert_eq(deserialized_metric.event_id, metric_event.event_id)
  assert_eq(deserialized_metric.timestamp, metric_event.timestamp)
  assert_eq(deserialized_metric.event_type, metric_event.event_type)
  assert_eq(deserialized_metric.service_name, metric_event.service_name)
  
  // 验证联合类型数据
  match deserialized_metric.data {
    Some(metric_data) => {
      assert_eq(metric_data.name, "request.duration")
      assert_eq(metric_data.value, 250.5)
      assert_eq(metric_data.unit, "milliseconds")
    }
    None => assert_true(false)
  }
  
  // 验证标签
  assert_eq(deserialized_metric.tags.size(), metric_event.tags.size())
  for (key, value) in deserialized_metric.tags.entries() {
    let original_value = metric_event.tags.get(key)
    assert_true(original_value != None)
    assert_eq(value, original_value.unwrap())
  }
  
  // 测试不同事件类型的序列化
  let log_avro_result = AvroSerializer::serialize(avro_serializer, "telemetry_event", log_event)
  assert_true(log_avro_result.is_ok())
  
  let log_avro_data = log_avro_result.unwrap()
  
  let log_deserialize_result = AvroSerializer::deserialize(avro_serializer, "telemetry_event", log_avro_data)
  assert_true(log_deserialize_result.is_ok())
  
  let deserialized_log = log_deserialize_result.unwrap()
  
  // 验证日志事件
  assert_eq(deserialized_log.event_type, "log")
  match deserialized_log.data {
    Some(log_data) => {
      assert_eq(log_data.level, "info")
      assert_eq(log_data.message, "User authentication successful")
    }
    None => assert_true(false)
  }
  
  // 测试Span事件
  let span_avro_result = AvroSerializer::serialize(avro_serializer, "telemetry_event", span_event)
  assert_true(span_avro_result.is_ok())
  
  let span_avro_data = span_avro_result.unwrap()
  
  let span_deserialize_result = AvroSerializer::deserialize(avro_serializer, "telemetry_event", span_avro_data)
  assert_true(span_deserialize_result.is_ok())
  
  let deserialized_span = span_deserialize_result.unwrap()
  
  // 验证Span事件
  assert_eq(deserialized_span.event_type, "span")
  match deserialized_span.data {
    Some(span_data) => {
      assert_eq(span_data.trace_id, "trace-12345")
      assert_eq(span_data.span_id, "span-67890")
      assert_eq(span_data.operation_name, "execute_query")
      assert_eq(span_data.duration_ms, 150)
    }
    None => assert_true(false)
  }
  
  // 测试Avro模式演进
  let v2_avro_schema = {
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
          "symbols": ["metric", "log", "span", "trace", "alert"]  // 新增"alert"类型
        }
      },
      {
        "name": "service_name",
        "type": "string"
      },
      {
        "name": "data",
        "type": [
          "null",
          {
            "type": "record",
            "name": "MetricData",
            "fields": [
              {
                "name": "name",
                "type": "string"
              },
              {
                "name": "value",
                "type": "double"
              },
              {
                "name": "unit",
                "type": "string"
              },
              {
                "name": "description",  // 新增字段
                "type": ["null", "string"],
                "default": null
              }
            ]
          },
          {
            "type": "record",
            "name": "LogData",
            "fields": [
              {
                "name": "level",
                "type": {
                  "type": "enum",
                  "name": "LogLevel",
                  "symbols": ["debug", "info", "warn", "error", "fatal"]
                }
              },
              {
                "name": "message",
                "type": "string"
              }
            ]
          },
          {
            "type": "record",
            "name": "SpanData",
            "fields": [
              {
                "name": "trace_id",
                "type": "string"
              },
              {
                "name": "span_id",
                "type": "string"
              },
              {
                "name": "operation_name",
                "type": "string"
              },
              {
                "name": "duration_ms",
                "type": "long"
              }
            ]
          }
        ]
      },
      {
        "name": "tags",
        "type": {
          "type": "map",
          "values": "string"
        }
      },
      {
        "name": "priority",  // 新增字段
        "type": ["null", "int"],
        "default": null
      }
    ]
  }
  
  // 注册V2模式
  AvroSerializer::register_schema(avro_serializer, "telemetry_event_v2", v2_avro_schema)
  
  // 使用V1模式序列化，V2模式反序列化（向前兼容）
  let v2_deserialize_result = AvroSerializer::deserialize(avro_serializer, "telemetry_event_v2", metric_avro_data)
  assert_true(v2_deserialize_result.is_ok())
  
  let v2_deserialized_metric = v2_deserialize_result.unwrap()
  
  // 验证新字段有默认值
  assert_eq(v2_deserialized_metric.priority, None)
  
  match v2_deserialized_metric.data {
    Some(metric_data) => {
      assert_eq(metric_data.description, None)  // 新字段有默认值
    }
    None => assert_true(false)
  }
  
  // 测试大型Avro数据
  let large_avro_events = []
  
  for i in 0..=2000 {
    large_avro_events.push({
      event_id: "event-" + i.to_string(),
      timestamp: 1640995200000 + i * 1000,
      event_type: if i % 3 == 0 { "metric" } else if i % 3 == 1 { "log" } else { "span" },
      service_name: "service-" + (i % 10).to_string(),
      data: if i % 3 == 0 {
        Some({
          name: "metric-" + (i % 20).to_string(),
          value: i * 1.5,
          unit: "unit-" + (i % 5).to_string()
        })
      } else if i % 3 == 1 {
        Some({
          level: ["debug", "info", "warn", "error"][i % 4],
          message: "Log message " + i.to_string()
        })
      } else {
        Some({
          trace_id: "trace-" + (i / 10).to_string(),
          span_id: "span-" + i.to_string(),
          operation_name: "operation-" + (i % 15).to_string(),
          duration_ms: i * 10
        })
      },
      tags: {
        "tag1": "value1-" + (i % 5).to_string(),
        "tag2": "value2-" + (i % 3).to_string()
      }
    })
  }
  
  // 序列化大型Avro数据
  let large_avro_results = []
  for event in large_avro_events {
    let result = AvroSerializer::serialize(avro_serializer, "telemetry_event", event)
    large_avro_results.push(result)
  }
  
  // 验证所有序列化成功
  for result in large_avro_results {
    assert_true(result.is_ok())
  }
  
  // 获取Avro统计
  let avro_stats = AvroSerializer::get_statistics(avro_serializer)
  assert_true(avro_stats.total_serializations > 0)
  assert_true(avro_stats.total_deserializations > 0)
  assert_true(avro_stats.avg_serialization_time_ms > 0)
  assert_true(avro_stats.avg_deserialization_time_ms > 0)
}

// 测试5: 跨格式数据转换
test "跨格式数据转换测试" {
  // 创建数据转换管理器
  let data_converter = DataConverter::new()
  
  // 注册所有序列化器
  let json_serializer = JsonSerializer::new()
  let binary_serializer = BinarySerializer::new()
  let protobuf_serializer = ProtobufSerializer::new()
  let avro_serializer = AvroSerializer::new()
  
  DataConverter::register_serializer(data_converter, "json", json_serializer)
  DataConverter::register_serializer(data_converter, "binary", binary_serializer)
  DataConverter::register_serializer(data_converter, "protobuf", protobuf_serializer)
  DataConverter::register_serializer(data_converter, "avro", avro_serializer)
  
  // 定义测试数据
  let test_data = {
    id: "test-12345",
    name: "test_data",
    timestamp: 1640995200000,
    metrics: [
      {
        name: "cpu.usage",
        value: 75.5,
        tags: ["host:server1", "region:us-west"]
      },
      {
        name: "memory.usage",
        value: 60.2,
        tags: ["host:server1", "region:us-west"]
      }
    ],
    metadata: {
      version: "1.0.0",
      source: "system.monitor",
      environment: "production"
    }
  }
  
  // 测试JSON到二进制转换
  let json_to_binary_result = DataConverter::convert(data_converter, test_data, "json", "binary")
  assert_true(json_to_binary_result.is_ok())
  
  let binary_data = json_to_binary_result.unwrap()
  assert_true(binary_data.length() > 0)
  
  // 验证转换后的数据可以反序列化
  let binary_deserialize_result = BinarySerializer::deserialize(binary_serializer, binary_data)
  assert_true(binary_deserialize_result.is_ok())
  
  let converted_from_binary = binary_deserialize_result.unwrap()
  assert_eq(converted_from_binary.id, test_data.id)
  assert_eq(converted_from_binary.name, test_data.name)
  
  // 测试二进制到JSON转换
  let binary_to_json_result = DataConverter::convert(data_converter, test_data, "binary", "json")
  assert_true(binary_to_json_result.is_ok())
  
  let json_string = binary_to_json_result.unwrap()
  assert_true(json_string.length() > 0)
  assert_true(json_string.contains("\"id\":\"test-12345\""))
  assert_true(json_string.contains("\"name\":\"test_data\""))
  
  // 验证转换后的JSON可以反序列化
  let json_deserialize_result = JsonSerializer::deserialize(json_serializer, json_string)
  assert_true(json_deserialize_result.is_ok())
  
  let converted_from_json = json_deserialize_result.unwrap()
  assert_eq(converted_from_json.id, test_data.id)
  assert_eq(converted_from_json.name, test_data.name)
  
  // 测试JSON到协议缓冲区转换
  let json_to_protobuf_result = DataConverter::convert(data_converter, test_data, "json", "protobuf")
  assert_true(json_to_protobuf_result.is_ok())
  
  let protobuf_data = json_to_protobuf_result.unwrap()
  assert_true(protobuf_data.length() > 0)
  
  // 测试协议缓冲区到JSON转换
  let protobuf_to_json_result = DataConverter::convert(data_converter, test_data, "protobuf", "json")
  assert_true(protobuf_to_json_result.is_ok())
  
  let protobuf_json_string = protobuf_to_json_result.unwrap()
  assert_true(protobuf_json_string.length() > 0)
  assert_true(protobuf_json_string.contains("\"id\":\"test-12345\""))
  
  // 测试JSON到Avro转换
  let json_to_avro_result = DataConverter::convert(data_converter, test_data, "json", "avro")
  assert_true(json_to_avro_result.is_ok())
  
  let avro_data = json_to_avro_result.unwrap()
  assert_true(avro_data.length() > 0)
  
  // 测试Avro到JSON转换
  let avro_to_json_result = DataConverter::convert(data_converter, test_data, "avro", "json")
  assert_true(avro_to_json_result.is_ok())
  
  let avro_json_string = avro_to_json_result.unwrap()
  assert_true(avro_json_string.length() > 0)
  assert_true(avro_json_string.contains("\"id\":\"test-12345\""))
  
  // 测试链式转换
  let chain_conversion_result = DataConverter::chain_convert(
    data_converter,
    test_data,
    ["json", "binary", "protobuf", "avro", "json"]
  )
  assert_true(chain_conversion_result.is_ok())
  
  let final_json_string = chain_conversion_result.unwrap()
  assert_true(final_json_string.length() > 0)
  assert_true(final_json_string.contains("\"id\":\"test-12345\""))
  
  // 验证链式转换后的数据完整性
  let final_deserialize_result = JsonSerializer::deserialize(json_serializer, final_json_string)
  assert_true(final_deserialize_result.is_ok())
  
  let final_data = final_deserialize_result.unwrap()
  assert_eq(final_data.id, test_data.id)
  assert_eq(final_data.name, test_data.name)
  assert_eq(final_data.timestamp, test_data.timestamp)
  assert_eq(final_data.metrics.length(), test_data.metrics.length())
  
  // 测试大型数据的格式转换
  let large_data = []
  for i in 0..=1000 {
    large_data.push({
      id: "item-" + i.to_string(),
      name: "item_name_" + i.to_string(),
      timestamp: 1640995200000 + i * 1000,
      value: i * 1.5,
      tags: ["tag1", "tag2", "tag3"],
      metadata: {
        source: "test",
        version: "1.0.0"
      }
    })
  }
  
  // 测试大型数据JSON到二进制转换
  let large_json_to_binary_result = DataConverter::convert(data_converter, large_data, "json", "binary")
  assert_true(large_json_to_binary_result.is_ok())
  
  let large_binary_data = large_json_to_binary_result.unwrap()
  
  // 测试大型数据二进制到JSON转换
  let large_binary_to_json_result = DataConverter::convert(data_converter, large_data, "binary", "json")
  assert_true(large_binary_to_json_result.is_ok())
  
  let large_json_string = large_binary_to_json_result.unwrap()
  
  // 验证大型数据转换后的完整性
  let large_deserialize_result = JsonSerializer::deserialize(json_serializer, large_json_string)
  assert_true(large_deserialize_result.is_ok())
  
  let large_converted_data = large_deserialize_result.unwrap()
  assert_eq(large_converted_data.length(), large_data.length())
  
  // 测试转换性能
  let conversion_performance_start = Time::now()
  
  for i in 0..=100 {
    let conversion_result = DataConverter::convert(data_converter, test_data, "json", "binary")
    assert_true(conversion_result.is_ok())
  }
  
  let conversion_performance_end = Time::now()
  let conversion_time = conversion_performance_end - conversion_performance_start
  
  // 验证转换性能
  assert_true(conversion_time < 5000)  // 100次转换应在5秒内完成
  
  // 测试不支持的格式转换
  let unsupported_result = DataConverter::convert(data_converter, test_data, "json", "xml")
  assert_true(unsupported_result.is_err())
  
  // 测试无效数据转换
  let invalid_data = { invalid_field: "invalid_value" }
  let invalid_conversion_result = DataConverter::convert(data_converter, invalid_data, "json", "binary")
  assert_true(invalid_conversion_result.is_err())
  
  // 获取数据转换统计
  let conversion_stats = DataConverter::get_statistics(data_converter)
  assert_true(conversion_stats.total_conversions > 0)
  assert_true(conversion_stats.successful_conversions > 0)
  assert_true(conversion_stats.failed_conversions >= 0)
  assert_true(conversion_stats.avg_conversion_time_ms > 0)
  
  // 验证格式使用统计
  assert_true(conversion_stats.format_usage.contains("json"))
  assert_true(conversion_stats.format_usage.contains("binary"))
  assert_true(conversion_stats.format_usage.contains("protobuf"))
  assert_true(conversion_stats.format_usage.contains("avro"))
}