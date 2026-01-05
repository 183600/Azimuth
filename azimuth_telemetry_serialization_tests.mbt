// Azimuth 遥测数据序列化和反序列化测试
// 测试遥测数据的序列化和反序列化功能

// 测试1: JSON序列化和反序列化
test "JSON序列化和反序列化测试" {
  // 创建测试遥测数据
  let telemetry_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    service_name: "payment-service",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM orders"),
      ("http.method", "GET"),
      ("http.status_code", "200"),
      ("user.id", "user-12345")
    ],
    events: [
      {
        name: "db.query.start",
        timestamp: 1640995200,
        attributes: [("db.query", "BEGIN TRANSACTION")]
      },
      {
        name: "db.query.complete",
        timestamp: 1640995250,
        attributes: [("db.rows_affected", "10")]
      }
    ]
  }
  
  // 序列化为JSON
  let json_data = TelemetrySerializer::serialize_to_json(telemetry_span)
  
  // 验证JSON格式
  assert_true(json_data.length() > 0)
  assert_true(json_data.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json_data.contains("\"span_id\":\"span-67890\""))
  assert_true(json_data.contains("\"operation_name\":\"database.query\""))
  assert_true(json_data.contains("\"service_name\":\"payment-service\""))
  
  // 反序列化JSON
  let deserialized_span = TelemetrySerializer::deserialize_from_json(json_data)
  
  // 验证反序列化结果
  assert_eq(deserialized_span.trace_id, telemetry_span.trace_id)
  assert_eq(deserialized_span.span_id, telemetry_span.span_id)
  assert_eq(deserialized_span.parent_span_id, telemetry_span.parent_span_id)
  assert_eq(deserialized_span.operation_name, telemetry_span.operation_name)
  assert_eq(deserialized_span.start_time, telemetry_span.start_time)
  assert_eq(deserialized_span.end_time, telemetry_span.end_time)
  assert_eq(deserialized_span.status, telemetry_span.status)
  assert_eq(deserialized_span.service_name, telemetry_span.service_name)
  assert_eq(deserialized_span.attributes.length(), telemetry_span.attributes.length())
  assert_eq(deserialized_span.events.length(), telemetry_span.events.length())
  
  // 验证属性
  for attr in telemetry_span.attributes {
    let found = deserialized_span.attributes.any(fn(a) { 
      a[0] == attr[0] and a[1] == attr[1] 
    })
    assert_true(found)
  }
  
  // 验证事件
  for event in telemetry_span.events {
    let found_event = deserialized_span.events.find(fn(e) { 
      e.name == event.name and e.timestamp == event.timestamp 
    })
    assert_true(found_event != None)
    match found_event {
      Some(e) => {
        assert_eq(e.attributes.length(), event.attributes.length())
        for attr in event.attributes {
          let found_attr = e.attributes.any(fn(a) { 
            a[0] == attr[0] and a[1] == attr[1] 
          })
          assert_true(found_attr)
        }
      }
      None => assert_true(false)
    }
  }
}

// 测试2: Protocol Buffers序列化和反序列化
test "Protocol Buffers序列化和反序列化测试" {
  // 创建测试遥测数据
  let telemetry_metric = {
    name: "http.request.duration",
    description: "HTTP请求持续时间",
    unit: "ms",
    value: 125.5,
    timestamp: 1640995200,
    tags: [
      ("service.name", "api.gateway"),
      ("http.method", "POST"),
      ("http.status_code", "200"),
      ("endpoint", "/api/orders")
    ],
    metric_type: "histogram",
    histogram_data: {
      buckets: [10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0],
      counts: [5, 15, 45, 120, 85, 25, 5],
      sum: 125.5,
      count: 300
    }
  }
  
  // 序列化为Protocol Buffers
  let proto_data = TelemetrySerializer::serialize_to_proto(telemetry_metric)
  
  // 验证Protocol Buffers格式
  assert_true(proto_data.length() > 0)
  
  // 反序列化Protocol Buffers
  let deserialized_metric = TelemetrySerializer::deserialize_from_proto(proto_data)
  
  // 验证反序列化结果
  assert_eq(deserialized_metric.name, telemetry_metric.name)
  assert_eq(deserialized_metric.description, telemetry_metric.description)
  assert_eq(deserialized_metric.unit, telemetry_metric.unit)
  assert_eq(deserialized_metric.value, telemetry_metric.value)
  assert_eq(deserialized_metric.timestamp, telemetry_metric.timestamp)
  assert_eq(deserialized_metric.metric_type, telemetry_metric.metric_type)
  
  // 验证标签
  for tag in telemetry_metric.tags {
    let found = deserialized_metric.tags.any(fn(t) { 
      t[0] == tag[0] and t[1] == tag[1] 
    })
    assert_true(found)
  }
  
  // 验证直方图数据
  assert_eq(deserialized_metric.histogram_data.buckets.length(), telemetry_metric.histogram_data.buckets.length())
  assert_eq(deserialized_metric.histogram_data.counts.length(), telemetry_metric.histogram_data.counts.length())
  assert_eq(deserialized_metric.histogram_data.sum, telemetry_metric.histogram_data.sum)
  assert_eq(deserialized_metric.histogram_data.count, telemetry_metric.histogram_data.count)
  
  for i in 0..telemetry_metric.histogram_data.buckets.length() {
    assert_eq(deserialized_metric.histogram_data.buckets[i], telemetry_metric.histogram_data.buckets[i])
    assert_eq(deserialized_metric.histogram_data.counts[i], telemetry_metric.histogram_data.counts[i])
  }
}

// 测试3: 批量序列化和反序列化
test "批量序列化和反序列化测试" {
  // 创建多个遥测数据项
  let telemetry_data = []
  
  // 添加span数据
  for i in 0..10 {
    telemetry_data = telemetry_data.push({
      type: "span",
      data: {
        trace_id: "trace-" + i.to_string(),
        span_id: "span-" + i.to_string(),
        operation_name: "operation." + i.to_string(),
        start_time: 1640995200 + i,
        end_time: 1640995250 + i,
        status: if i % 5 == 0 { "error" } else { "ok" },
        service_name: "service-" + (i % 3).to_string()
      }
    })
  }
  
  // 添加metric数据
  for i in 0..10 {
    telemetry_data = telemetry_data.push({
      type: "metric",
      data: {
        name: "metric." + i.to_string(),
        value: 100.0 + i.to_float(),
        timestamp: 1640995200 + i,
        tags: [
          ("service.name", "service-" + (i % 3).to_string()),
          ("environment", "production")
        ]
      }
    })
  }
  
  // 批量序列化为JSON
  let batch_json = TelemetrySerializer::serialize_batch_to_json(telemetry_data)
  
  // 验证批量JSON格式
  assert_true(batch_json.length() > 0)
  assert_true(batch_json.contains("\"type\":\"span\""))
  assert_true(batch_json.contains("\"type\":\"metric\""))
  
  // 批量反序列化JSON
  let deserialized_batch = TelemetrySerializer::deserialize_batch_from_json(batch_json)
  
  // 验证批量反序列化结果
  assert_eq(deserialized_batch.length(), telemetry_data.length())
  
  for i in 0..telemetry_data.length() {
    let original = telemetry_data[i]
    let deserialized = deserialized_batch[i]
    
    assert_eq(original.type, deserialized.type)
    
    match original.type {
      "span" => {
        assert_eq(original.data.trace_id, deserialized.data.trace_id)
        assert_eq(original.data.span_id, deserialized.data.span_id)
        assert_eq(original.data.operation_name, deserialized.data.operation_name)
      }
      "metric" => {
        assert_eq(original.data.name, deserialized.data.name)
        assert_eq(original.data.value, deserialized.data.value)
        assert_eq(original.data.timestamp, deserialized.data.timestamp)
      }
      _ => assert_true(false)
    }
  }
}

// 测试4: 序列化压缩测试
test "序列化压缩测试" {
  // 创建大型遥测数据集
  let large_telemetry_data = []
  
  for i in 0..1000 {
    large_telemetry_data = large_telemetry_data.push({
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      parent_span_id: if i > 0 { Some("span-" + (i - 1).to_string()) } else { None },
      operation_name: "operation." + (i % 50).to_string(),
      start_time: 1640995200 + i,
      end_time: 1640995250 + i,
      status: if i % 20 == 0 { "error" } else { "ok" },
      service_name: "service-" + (i % 10).to_string(),
      attributes: [
        ("http.method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("http.status_code", (200 + (i % 5) * 100).to_string()),
        ("user.id", "user-" + (i % 100).to_string()),
        ("request.id", "req-" + i.to_string())
      ]
    })
  }
  
  // 序列化未压缩数据
  let uncompressed_json = TelemetrySerializer::serialize_batch_to_json(large_telemetry_data)
  let uncompressed_size = uncompressed_json.length()
  
  // 序列化压缩数据（gzip）
  let compressed_json = TelemetrySerializer::serialize_batch_to_json_compressed(large_telemetry_data, "gzip")
  let compressed_size = compressed_json.length()
  
  // 验证压缩效果
  assert_true(compressed_size < uncompressed_size)
  let compression_ratio = 1.0 - (compressed_size.to_float() / uncompressed_size.to_float())
  assert_true(compression_ratio > 0.6)  // 至少60%压缩率
  
  // 解压并反序列化
  let decompressed_data = TelemetrySerializer::deserialize_batch_from_json_compressed(compressed_json, "gzip")
  
  // 验证解压后数据完整性
  assert_eq(decompressed_data.length(), large_telemetry_data.length())
  
  for i in 0..large_telemetry_data.length() {
    let original = large_telemetry_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.trace_id, decompressed.trace_id)
    assert_eq(original.span_id, decompressed.span_id)
    assert_eq(original.operation_name, decompressed.operation_name)
    assert_eq(original.status, decompressed.status)
    assert_eq(original.service_name, decompressed.service_name)
  }
  
  // 测试不同压缩算法
  let lz4_compressed = TelemetrySerializer::serialize_batch_to_json_compressed(large_telemetry_data, "lz4")
  let lz4_size = lz4_compressed.length()
  
  // 验证LZ4压缩效果
  assert_true(lz4_size < uncompressed_size)
  let lz4_compression_ratio = 1.0 - (lz4_size.to_float() / uncompressed_size.to_float())
  assert_true(lz4_compression_ratio > 0.4)  // 至少40%压缩率
  
  // 验证LZ4解压数据完整性
  let lz4_decompressed = TelemetrySerializer::deserialize_batch_from_json_compressed(lz4_compressed, "lz4")
  assert_eq(lz4_decompressed.length(), large_telemetry_data.length())
}

// 测试5: 序列化格式转换测试
test "序列化格式转换测试" {
  // 创建测试遥测数据
  let telemetry_log = {
    timestamp: 1640995200,
    level: "error",
    message: "Database connection failed",
    service_name: "payment-service",
    trace_id: "trace-12345",
    span_id: "span-67890",
    fields: [
      ("error.type", "connection_timeout"),
      ("error.message", "Connection timeout after 30 seconds"),
      ("db.host", "db-primary.example.com"),
      ("db.port", "5432"),
      ("retry.count", "3"),
      ("user.id", "user-98765")
    ]
  }
  
  // 序列化为JSON
  let json_format = TelemetrySerializer::serialize_to_json(telemetry_log)
  
  // 从JSON转换为Protocol Buffers
  let proto_format = TelemetrySerializer::convert_format(json_format, "json", "protobuf")
  
  // 从Protocol Buffers转换为Avro
  let avro_format = TelemetrySerializer::convert_format(proto_format, "protobuf", "avro")
  
  // 从Avro转换为JSON
  let final_json = TelemetrySerializer::convert_format(avro_format, "avro", "json")
  
  // 验证格式转换后的数据完整性
  let final_data = TelemetrySerializer::deserialize_from_json(final_json)
  
  assert_eq(final_data.timestamp, telemetry_log.timestamp)
  assert_eq(final_data.level, telemetry_log.level)
  assert_eq(final_data.message, telemetry_log.message)
  assert_eq(final_data.service_name, telemetry_log.service_name)
  assert_eq(final_data.trace_id, telemetry_log.trace_id)
  assert_eq(final_data.span_id, telemetry_log.span_id)
  assert_eq(final_data.fields.length(), telemetry_log.fields.length())
  
  // 验证字段
  for field in telemetry_log.fields {
    let found = final_data.fields.any(fn(f) { 
      f[0] == field[0] and f[1] == field[1] 
    })
    assert_true(found)
  }
  
  // 验证不同格式的大小特征
  assert_true(json_format.length() > 0)
  assert_true(proto_format.length() > 0)
  assert_true(avro_format.length() > 0)
  
  // Protocol Buffers通常比JSON更紧凑
  assert_true(proto_format.length() < json_format.length())
}

// 测试6: 序列化错误处理测试
test "序列化错误处理测试" {
  // 测试空数据处理
  let empty_data = []
  let empty_json = TelemetrySerializer::serialize_batch_to_json(empty_data)
  assert_eq(empty_json, "[]")
  
  let empty_deserialized = TelemetrySerializer::deserialize_batch_from_json(empty_json)
  assert_eq(empty_deserialized.length(), 0)
  
  // 测试损坏的JSON数据处理
  let corrupted_json = "{\"trace_id\":\"trace-123\",\"span_id\":\"span-456\""
  let corrupted_result = TelemetrySerializer::deserialize_from_json_safe(corrupted_json)
  
  match corrupted_result {
    Ok(_) => assert_true(false),  // 应该失败
    Err(error) => {
      assert_true(error.contains("invalid") or error.contains("parse"))
    }
  }
  
  // 测试缺少必要字段的数据处理
  let incomplete_json = "{\"span_id\":\"span-456\",\"operation_name\":\"test\"}"
  let incomplete_result = TelemetrySerializer::deserialize_from_json_safe(incomplete_json)
  
  match incomplete_result {
    Ok(_) => assert_true(false),  // 应该失败
    Err(error) => {
      assert_true(error.contains("missing") or error.contains("required"))
    }
  }
  
  // 测试类型不匹配数据处理
  let type_mismatch_json = "{\"trace_id\":123,\"span_id\":\"span-456\"}"
  let type_mismatch_result = TelemetrySerializer::deserialize_from_json_safe(type_mismatch_json)
  
  match type_mismatch_result {
    Ok(_) => assert_true(false),  // 应该失败
    Err(error) => {
      assert_true(error.contains("type") or error.contains("mismatch"))
    }
  }
  
  // 测试过大的数据处理
  let large_string = "a".repeat(10000000)  // 10MB字符串
  let oversized_data = {
    trace_id: large_string,
    span_id: "span-456",
    operation_name: "test"
  }
  
  let oversized_result = TelemetrySerializer::serialize_to_json_safe(oversized_data)
  
  match oversized_result {
    Ok(_) => assert_true(false),  // 应该失败
    Err(error) => {
      assert_true(error.contains("size") or error.contains("limit"))
    }
  }
}

// 测试7: 序列化性能测试
test "序列化性能测试" {
  // 创建测试数据集
  let performance_data = []
  
  for i in 0..10000 {
    performance_data = performance_data.push({
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + (i % 100).to_string(),
      start_time: 1640995200 + i,
      end_time: 1640995250 + i,
      status: if i % 10 == 0 { "error" } else { "ok" },
      service_name: "service-" + (i % 20).to_string(),
      attributes: [
        ("http.method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("http.status_code", (200 + (i % 3) * 100).to_string()),
        ("user.id", "user-" + (i % 1000).to_string())
      ]
    })
  }
  
  // 测试JSON序列化性能
  let json_start_time = Time::now()
  let json_serialized = TelemetrySerializer::serialize_batch_to_json(performance_data)
  let json_serialization_time = Time::now() - json_start_time
  
  // 测试JSON反序列化性能
  let json_deserialize_start = Time::now()
  let json_deserialized = TelemetrySerializer::deserialize_batch_from_json(json_serialized)
  let json_deserialization_time = Time::now() - json_deserialize_start
  
  // 测试Protocol Buffers序列化性能
  let proto_start_time = Time::now()
  let proto_serialized = TelemetrySerializer::serialize_batch_to_proto(performance_data)
  let proto_serialization_time = Time::now() - proto_start_time
  
  // 测试Protocol Buffers反序列化性能
  let proto_deserialize_start = Time::now()
  let proto_deserialized = TelemetrySerializer::deserialize_batch_from_proto(proto_serialized)
  let proto_deserialization_time = Time::now() - proto_deserialize_start
  
  // 验证性能基准
  assert_true(json_serialization_time < 5000)  // 5秒内完成序列化
  assert_true(json_deserialization_time < 3000)  // 3秒内完成反序列化
  assert_true(proto_serialization_time < 3000)  // 3秒内完成Protocol Buffers序列化
  assert_true(proto_deserialization_time < 2000)  // 2秒内完成Protocol Buffers反序列化
  
  // 验证数据完整性
  assert_eq(json_deserialized.length(), performance_data.length())
  assert_eq(proto_deserialized.length(), performance_data.length())
  
  // Protocol Buffers通常比JSON更快
  assert_true(proto_serialization_time < json_serialization_time)
  assert_true(proto_deserialization_time < json_deserialization_time)
  
  // Protocol Buffers通常比JSON更紧凑
  assert_true(proto_serialized.length() < json_serialized.length())
  
  // 计算性能指标
  let json_serialization_throughput = performance_data.length().to_float() / json_serialization_time.to_float() * 1000.0
  let json_deserialization_throughput = performance_data.length().to_float() / json_deserialization_time.to_float() * 1000.0
  let proto_serialization_throughput = performance_data.length().to_float() / proto_serialization_time.to_float() * 1000.0
  let proto_deserialization_throughput = performance_data.length().to_float() / proto_deserialization_time.to_float() * 1000.0
  
  // 验证吞吐量
  assert_true(json_serialization_throughput > 2000)  // 至少2000项/秒
  assert_true(json_deserialization_throughput > 3000)  // 至少3000项/秒
  assert_true(proto_serialization_throughput > 3000)  // 至少3000项/秒
  assert_true(proto_deserialization_throughput > 5000)  // 至少5000项/秒
}