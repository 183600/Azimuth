// 高级遥测数据序列化测试
// 测试复杂数据结构的序列化和反序列化功能

test "complex_telemetry_data_serialization" {
  // 测试复杂遥测数据的序列化
  
  // 创建复杂的属性数据
  let complex_attributes = [
    ("user.id", common::AttributeValue::string("user-12345")),
    ("request.duration_ms", common::AttributeValue::int(250L)),
    ("memory.usage_mb", common::AttributeValue::float(128.5)),
    ("cache.hit", common::AttributeValue::bool(true)),
    ("tags", common::AttributeValue::array_string(["web", "api", "v1"])),
    ("response_codes", common::AttributeValue::array_int([200L, 201L, 204L])),
    ("latency_samples", common::AttributeValue::array_float([10.5, 15.2, 8.7, 12.3])),
    ("feature_flags", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  // 验证属性数量
  assert_eq(complex_attributes.length(), 8)
  
  // 测试序列化字符串格式
  let serialized = serialize_attributes(complex_attributes)
  assert_eq(serialized.length() > 0, true)
  assert_eq(serialized.contains("user-12345"), true)
  assert_eq(serialized.contains("250"), true)
  assert_eq(serialized.contains("128.5"), true)
  
  // 测试反序列化
  let deserialized = deserialize_attributes(serialized)
  assert_eq(deserialized.length(), complex_attributes.length())
  
  // 验证特定属性
  let found_user_id = deserialized.find(fn(attr) { attr.0 == "user.id" })
  match found_user_id {
    Some((_, value)) => {
      match value {
        common::StringValue(s) => assert_eq(s, "user-12345")
        _ => @test.fail("User ID should be a string value")
      }
    }
    None => @test.fail("User ID attribute not found")
  }
}

test "nested_telemetry_structures" {
  // 测试嵌套遥测结构的处理
  
  // 创建嵌套结构
  let outer_span_data = {
    "trace_id": "abc123def456",
    "span_id": "span789",
    "parent_span": {
      "span_id": "parent456",
      "trace_id": "abc123def456"
    },
    "events": [
      {
        "name": "db.query.start",
        "timestamp": 1234567890L,
        "attributes": [("query", "SELECT * FROM users")]
      },
      {
        "name": "db.query.end", 
        "timestamp": 1234567895L,
        "attributes": [("duration_ms", "15")]
      }
    ]
  }
  
  // 验证嵌套结构完整性
  assert_eq(outer_span_data["trace_id"], "abc123def456")
  assert_eq(outer_span_data["span_id"], "span789")
  assert_eq(outer_span_data["parent_span"]["span_id"], "parent456")
  assert_eq(outer_span_data["events"].length(), 2)
  assert_eq(outer_span_data["events"][0]["name"], "db.query.start")
  assert_eq(outer_span_data["events"][1]["attributes"][0].1, "15")
}

test "telemetry_data_compression" {
  // 测试遥测数据的压缩功能
  
  // 创建大量重复数据
  let large_dataset = []
  for i = 0; i < 1000; i = i + 1 {
    large_dataset.push((
      "metric.name", 
      common::AttributeValue::string("cpu.usage"),
      "timestamp",
      common::AttributeValue::int(1609459200L + i.to_int64()),
      "value", 
      common::AttributeValue::float(45.5 + (i.to_int64() % 100).to_float())
    ))
  }
  
  // 原始数据大小
  let original_size = calculate_data_size(large_dataset)
  assert_eq(original_size > 10000, true) // 确保数据足够大
  
  // 压缩数据
  let compressed = compress_telemetry_data(large_dataset)
  let compressed_size = compressed.length()
  
  // 验证压缩效果
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  assert_eq(compression_ratio < 0.8, true) // 至少20%的压缩率
  
  // 解压缩并验证数据完整性
  let decompressed = decompress_telemetry_data(compressed)
  assert_eq(decompressed.length(), large_dataset.length())
  
  // 验证首尾数据点
  assert_eq(decompressed[0], large_dataset[0])
  assert_eq(decompressed[999], large_dataset[999])
}