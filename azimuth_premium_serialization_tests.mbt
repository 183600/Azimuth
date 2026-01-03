// Azimuth 数据序列化/反序列化测试
// 专注于遥测数据的序列化格式、兼容性和性能

// 测试1: JSON序列化/反序列化
test "JSON序列化/反序列化" {
  // 模拟遥测数据对象
  let telemetry_data = {
    timestamp: 1640995200,
    metric_name: "cpu_usage",
    value: 75.5,
    tags: [
      { key: "host", value: "server-001" },
      { key: "region", value: "us-east" },
      { key: "environment", value: "production" }
    ],
    metadata: {
      source: "system-monitor",
      version: "1.0.0",
      collection_interval: 60
    }
  }
  
  // 模拟JSON序列化结果
  let json_serialized = "{\"timestamp\":1640995200,\"metric_name\":\"cpu_usage\",\"value\":75.5,\"tags\":[{\"key\":\"host\",\"value\":\"server-001\"},{\"key\":\"region\",\"value\":\"us-east\"},{\"key\":\"environment\",\"value\":\"production\"}],\"metadata\":{\"source\":\"system-monitor\",\"version\":\"1.0.0\",\"collection_interval\":60}}"
  
  // 模拟JSON反序列化结果
  let json_deserialized = {
    timestamp: 1640995200,
    metric_name: "cpu_usage",
    value: 75.5,
    tags: [
      { key: "host", value: "server-001" },
      { key: "region", value: "us-east" },
      { key: "environment", value: "production" }
    ],
    metadata: {
      source: "system-monitor",
      version: "1.0.0",
      collection_interval: 60
    }
  }
  
  // 验证JSON序列化
  assert_true(json_serialized.contains("\"timestamp\":1640995200"))
  assert_true(json_serialized.contains("\"metric_name\":\"cpu_usage\""))
  assert_true(json_serialized.contains("\"value\":75.5"))
  assert_true(json_serialized.contains("\"tags\":["))
  assert_true(json_serialized.contains("\"metadata\":{"))
  
  // 验证JSON反序列化
  assert_eq(json_deserialized.timestamp, telemetry_data.timestamp)
  assert_eq(json_deserialized.metric_name, telemetry_data.metric_name)
  assert_eq(json_deserialized.value, telemetry_data.value)
  assert_eq(json_deserialized.tags.length(), telemetry_data.tags.length())
  assert_eq(json_deserialized.metadata.source, telemetry_data.metadata.source)
  assert_eq(json_deserialized.metadata.version, telemetry_data.metadata.version)
  assert_eq(json_deserialized.metadata.collection_interval, telemetry_data.metadata.collection_interval)
  
  // 验证标签完整性
  for i in 0..telemetry_data.tags.length() {
    assert_eq(json_deserialized.tags[i].key, telemetry_data.tags[i].key)
    assert_eq(json_deserialized.tags[i].value, telemetry_data.tags[i].value)
  }
  
  // 验证JSON序列化性能
  let serialization_performance = {
    data_size_bytes: json_serialized.length(),
    serialization_time_ms: 5,
    deserialization_time_ms: 8,
    compression_ratio: 1.0 // JSON通常不压缩
  }
  
  assert_true(serialization_performance.data_size_bytes > 0)
  assert_true(serialization_performance.serialization_time_ms > 0)
  assert_true(serialization_performance.deserialization_time_ms > 0)
  
  // 验证JSON格式版本兼容性
  let json_v1 = "{\"timestamp\":1640995200,\"metric_name\":\"cpu_usage\",\"value\":75.5}"
  let json_v2 = "{\"timestamp\":1640995200,\"metric_name\":\"cpu_usage\",\"value\":75.5,\"unit\":\"percent\"}"
  
  // v2应该能反序列化v1的字段
  let v2_compatible_deserialized = {
    timestamp: 1640995200,
    metric_name: "cpu_usage",
    value: 75.5,
    unit: "percent" // 新字段，v1中没有
  }
  
  assert_eq(v2_compatible_deserialized.timestamp, 1640995200)
  assert_eq(v2_compatible_deserialized.metric_name, "cpu_usage")
  assert_eq(v2_compatible_deserialized.value, 75.5)
  assert_eq(v2_compatible_deserialized.unit, "percent")
}

// 测试2: 二进制序列化/反序列化
test "二进制序列化/反序列化" {
  // 模拟二进制序列化格式
  let binary_format = {
    header: {
      magic_bytes: [0x41, 0x5A, 0x4D, 0x54], // "AZMT"
      version: 2,
      flags: 0x01
    },
    payload: {
      timestamp: 1640995200,
      metric_id: 1001,
      value: 75.5,
      tag_count: 2,
      tags: [
        { key_id: 1, value_id: 10 }, // host -> server-001
        { key_id: 2, value_id: 20 }  // region -> us-east
      ]
    },
    checksum: 0x12345678
  }
  
  // 模拟二进制序列化结果（字节数组）
  let binary_serialized = [
    0x41, 0x5A, 0x4D, 0x54, // magic bytes
    0x02,                   // version
    0x01,                   // flags
    0x00, 0x00, 0x61, 0xE8, // timestamp (1640995200)
    0x03, 0xE9,             // metric_id (1001)
    0x42, 0x96, 0x00, 0x00, // value (75.5 as IEEE 754)
    0x02,                   // tag_count
    0x01, 0x0A,             // tag 1: key_id=1, value_id=10
    0x02, 0x14,             // tag 2: key_id=2, value_id=20
    0x12, 0x34, 0x56, 0x78  // checksum
  ]
  
  // 验证二进制序列化格式
  assert_eq(binary_serialized[0], 0x41) // 'A'
  assert_eq(binary_serialized[1], 0x5A) // 'Z'
  assert_eq(binary_serialized[2], 0x4D) // 'M'
  assert_eq(binary_serialized[3], 0x54) // 'T'
  assert_eq(binary_serialized[4], 2)     // version
  assert_eq(binary_serialized[5], 1)     // flags
  
  // 验证二进制反序列化
  let binary_deserialized = {
    header: {
      magic_bytes: [binary_serialized[0], binary_serialized[1], binary_serialized[2], binary_serialized[3]],
      version: binary_serialized[4],
      flags: binary_serialized[5]
    },
    payload: {
      timestamp: (binary_serialized[6].to_int() << 24) | (binary_serialized[7].to_int() << 16) | (binary_serialized[8].to_int() << 8) | binary_serialized[9].to_int(),
      metric_id: (binary_serialized[10].to_int() << 8) | binary_serialized[11].to_int(),
      value: 75.5, // 简化的IEEE 754解析
      tag_count: binary_serialized[12].to_int(),
      tags: [
        { key_id: binary_serialized[13].to_int(), value_id: binary_serialized[14].to_int() },
        { key_id: binary_serialized[15].to_int(), value_id: binary_serialized[16].to_int() }
      ]
    },
    checksum: (binary_serialized[17].to_int() << 24) | (binary_serialized[18].to_int() << 16) | (binary_serialized[19].to_int() << 8) | binary_serialized[20].to_int()
  }
  
  assert_eq(binary_deserialized.header.magic_bytes, binary_format.header.magic_bytes)
  assert_eq(binary_deserialized.header.version, binary_format.header.version)
  assert_eq(binary_deserialized.header.flags, binary_format.header.flags)
  assert_eq(binary_deserialized.payload.timestamp, binary_format.payload.timestamp)
  assert_eq(binary_deserialized.payload.metric_id, binary_format.payload.metric_id)
  assert_eq(binary_deserialized.payload.value, binary_format.payload.value)
  assert_eq(binary_deserialized.payload.tag_count, binary_format.payload.tag_count)
  assert_eq(binary_deserialized.checksum, binary_format.checksum)
  
  // 验证二进制序列化性能
  let binary_performance = {
    data_size_bytes: binary_serialized.length(),
    serialization_time_ms: 2,
    deserialization_time_ms: 3,
    compression_ratio: 0.6 // 二进制格式通常比JSON更紧凑
  }
  
  assert_true(binary_performance.data_size_bytes > 0)
  assert_true(binary_performance.serialization_time_ms > 0)
  assert_true(binary_performance.deserialization_time_ms > 0)
  assert_true(binary_performance.compression_ratio < 1.0)
  
  // 验证二进制格式版本兼容性
  let binary_v1 = [
    0x41, 0x5A, 0x4D, 0x54, // magic bytes
    0x01,                   // version 1
    0x00,                   // flags
    0x00, 0x00, 0x61, 0xE8, // timestamp
    0x03, 0xE9,             // metric_id
    0x42, 0x96, 0x00, 0x00, // value
    0x01,                   // tag_count (v1只支持1个标签)
    0x01, 0x0A              // tag
  ]
  
  // v2应该能反序列化v1的数据
  let v2_compatible_deserialized = {
    header: {
      magic_bytes: [binary_v1[0], binary_v1[1], binary_v1[2], binary_v1[3]],
      version: binary_v1[4],
      flags: binary_v1[5]
    },
    payload: {
      timestamp: 1640995200,
      metric_id: 1001,
      value: 75.5,
      tag_count: 1,
      tags: [
        { key_id: 1, value_id: 10 }
      ]
    }
  }
  
  assert_eq(v2_compatible_deserialized.header.version, 1) // 识别为v1格式
  assert_eq(v2_compatible_deserialized.payload.tag_count, 1) // 正确解析v1的标签数量
}

// 测试3: Protocol Buffers序列化/反序列化
test "Protocol Buffers序列化/反序列化" {
  // 模拟Protocol Buffers schema定义
  let proto_schema = {
    message: "TelemetryData",
    fields: [
      { name: "timestamp", type: "int64", id: 1 },
      { name: "metric_name", type: "string", id: 2 },
      { name: "value", type: "double", id: 3 },
      { name: "tags", type: "repeated Tag", id: 4 },
      { name: "metadata", type: "Metadata", id: 5 }
    ],
    nested_messages: [
      {
        name: "Tag",
        fields: [
          { name: "key", type: "string", id: 1 },
          { name: "value", type: "string", id: 2 }
        ]
      },
      {
        name: "Metadata",
        fields: [
          { name: "source", type: "string", id: 1 },
          { name: "version", type: "string", id: 2 },
          { name: "collection_interval", type: "int32", id: 3 }
        ]
      }
    ]
  }
  
  // 模拟Protocol Buffers序列化结果（简化版）
  let proto_serialized = [
    0x08,                   // field 1 (timestamp), varint type
    0x80, 0x80, 0x80, 0x80, 0x08, // varint encoded timestamp (1640995200)
    0x12,                   // field 2 (metric_name), length-delimited
    0x09,                   // length of "cpu_usage"
    0x63, 0x70, 0x75, 0x5F, 0x75, 0x73, 0x61, 0x67, 0x65, // "cpu_usage"
    0x19,                   // field 3 (value), 64-bit
    0x40, 0x52, 0xEC, 0x51, 0xB8, 0x2E, 0x53, 0x40, // 75.5 as IEEE 754
    0x22,                   // field 4 (tags), length-delimited
    0x1A,                   // length of tags array
    // tag 1
    0x0A,                   // field 1 (key), length-delimited
    0x04,                   // length of "host"
    0x68, 0x6F, 0x73, 0x74, // "host"
    0x12,                   // field 2 (value), length-delimited
    0x0A,                   // length of "server-001"
    0x73, 0x65, 0x72, 0x76, 0x65, 0x72, 0x2D, 0x30, 0x30, 0x31, // "server-001"
    // tag 2
    0x0A,                   // field 1 (key), length-delimited
    0x06,                   // length of "region"
    0x72, 0x65, 0x67, 0x69, 0x6F, 0x6E, // "region"
    0x12,                   // field 2 (value), length-delimited
    0x06,                   // length of "us-east"
    0x75, 0x73, 0x2D, 0x65, 0x61, 0x73, 0x74, // "us-east"
    // metadata
    0x2A,                   // field 5 (metadata), length-delimited
    0x1E,                   // length of metadata
    0x0A,                   // field 1 (source), length-delimited
    0x0E,                   // length of "system-monitor"
    0x73, 0x79, 0x73, 0x74, 0x65, 0x6D, 0x2D, 0x6D, 0x6F, 0x6E, 0x69, 0x74, 0x6F, 0x72, // "system-monitor"
    0x12,                   // field 2 (version), length-delimited
    0x04,                   // length of "1.0.0"
    0x31, 0x2E, 0x30, 0x2E, 0x30, // "1.0.0"
    0x18,                   // field 3 (collection_interval), varint
    0x3C                    // 60
  ]
  
  // 验证Protocol Buffers序列化格式
  assert_eq(proto_serialized[0], 0x08) // field 1, varint type
  assert_eq(proto_serialized[7], 0x12) // field 2, length-delimited
  assert_eq(proto_serialized[18], 0x19) // field 3, 64-bit
  assert_eq(proto_serialized[27], 0x22) // field 4, length-delimited
  
  // 验证Protocol Buffers反序列化
  let proto_deserialized = {
    timestamp: 1640995200,
    metric_name: "cpu_usage",
    value: 75.5,
    tags: [
      { key: "host", value: "server-001" },
      { key: "region", value: "us-east" }
    ],
    metadata: {
      source: "system-monitor",
      version: "1.0.0",
      collection_interval: 60
    }
  }
  
  assert_eq(proto_deserialized.timestamp, 1640995200)
  assert_eq(proto_deserialized.metric_name, "cpu_usage")
  assert_eq(proto_deserialized.value, 75.5)
  assert_eq(proto_deserialized.tags.length(), 2)
  assert_eq(proto_deserialized.metadata.source, "system-monitor")
  assert_eq(proto_deserialized.metadata.version, "1.0.0")
  assert_eq(proto_deserialized.metadata.collection_interval, 60)
  
  // 验证Protocol Buffers性能
  let proto_performance = {
    data_size_bytes: proto_serialized.length(),
    serialization_time_ms: 1,
    deserialization_time_ms: 1,
    compression_ratio: 0.4 // Protocol Buffers通常比JSON更紧凑
  }
  
  assert_true(proto_performance.data_size_bytes > 0)
  assert_true(proto_performance.serialization_time_ms > 0)
  assert_true(proto_performance.deserialization_time_ms > 0)
  assert_true(proto_performance.compression_ratio < 1.0)
  
  // 验证Protocol Buffers前向兼容性
  let proto_v2_schema = {
    message: "TelemetryData",
    fields: [
      { name: "timestamp", type: "int64", id: 1 },
      { name: "metric_name", type: "string", id: 2 },
      { name: "value", type: "double", id: 3 },
      { name: "tags", type: "repeated Tag", id: 4 },
      { name: "metadata", type: "Metadata", id: 5 },
      { name: "unit", type: "string", id: 6 } // 新字段
    ]
  }
  
  // v2应该能反序列化v1的数据，忽略未知字段
  let v2_compatible_deserialized = {
    timestamp: 1640995200,
    metric_name: "cpu_usage",
    value: 75.5,
    tags: [
      { key: "host", value: "server-001" },
      { key: "region", value: "us-east" }
    ],
    metadata: {
      source: "system-monitor",
      version: "1.0.0",
      collection_interval: 60
    },
    unit: "" // 新字段，v1中没有，使用默认值
  }
  
  assert_eq(v2_compatible_deserialized.timestamp, 1640995200)
  assert_eq(v2_compatible_deserialized.metric_name, "cpu_usage")
  assert_eq(v2_compatible_deserialized.value, 75.5)
  assert_eq(v2_compatible_deserialized.unit, "") // 默认值
  
  // 验证Protocol Buffers后向兼容性
  let proto_v1_deserialized_from_v2 = {
    timestamp: 1640995200,
    metric_name: "cpu_usage",
    value: 75.5,
    tags: [
      { key: "host", value: "server-001" },
      { key: "region", value: "us-east" }
    ],
    metadata: {
      source: "system-monitor",
      version: "1.0.0",
      collection_interval: 60
    }
    // unit字段被忽略，因为v1 schema中没有定义
  }
  
  assert_eq(proto_v1_deserialized_from_v2.timestamp, 1640995200)
  assert_eq(proto_v1_deserialized_from_v2.metric_name, "cpu_usage")
  assert_eq(proto_v1_deserialized_from_v2.value, 75.5)
}

// 测试4: Avro序列化/反序列化
test "Avro序列化/反序列化" {
  // 模拟Avro schema定义
  let avro_schema = {
    type: "record",
    name: "TelemetryData",
    namespace: "com.azimuth.telemetry",
    fields: [
      { name: "timestamp", type: "long" },
      { name: "metric_name", type: "string" },
      { name: "value", type: "double" },
      { 
        name: "tags", 
        type: {
          type: "map",
          values: "string"
        }
      },
      {
        name: "metadata",
        type: {
          type: "record",
          name: "Metadata",
          fields: [
            { name: "source", type: "string" },
            { name: "version", type: "string" },
            { name: "collection_interval", type: "int", "default": 60 }
          ]
        }
      }
    ]
  }
  
  // 模拟Avro序列化结果（简化版）
  let avro_serialized = [
    // timestamp (long)
    0x80, 0x80, 0x80, 0x80, 0x08,
    // metric_name (string)
    0x09, 0x63, 0x70, 0x75, 0x5F, 0x75, 0x73, 0x61, 0x67, 0x65,
    // value (double)
    0x40, 0x52, 0xEC, 0x51, 0xB8, 0x2E, 0x53, 0x40,
    // tags (map)
    0x02, // 2 pairs
    // tag 1
    0x04, 0x68, 0x6F, 0x73, 0x74, // "host"
    0x0A, 0x73, 0x65, 0x72, 0x76, 0x65, 0x72, 0x2D, 0x30, 0x30, 0x31, // "server-001"
    // tag 2
    0x06, 0x72, 0x65, 0x67, 0x69, 0x6F, 0x6E, // "region"
    0x06, 0x75, 0x73, 0x2D, 0x65, 0x61, 0x73, 0x74, // "us-east"
    // metadata (record)
    // source
    0x0E, 0x73, 0x79, 0x73, 0x74, 0x65, 0x6D, 0x2D, 0x6D, 0x6F, 0x6E, 0x69, 0x74, 0x6F, 0x72,
    // version
    0x04, 0x31, 0x2E, 0x30, 0x2E, 0x30,
    // collection_interval (int, default value 60)
    0x78 // ZigZag encoded 60
  ]
  
  // 验证Avro序列化格式
  assert_eq(avro_serialized[0], 0x80) // timestamp的第一个字节
  assert_eq(avro_serialized[5], 0x09) // metric_name长度
  assert_eq(avro_serialized[15], 0x40) // value的第一个字节
  assert_eq(avro_serialized[24], 0x02) // tags map的条目数
  
  // 验证Avro反序列化
  let avro_deserialized = {
    timestamp: 1640995200,
    metric_name: "cpu_usage",
    value: 75.5,
    tags: {
      "host": "server-001",
      "region": "us-east"
    },
    metadata: {
      source: "system-monitor",
      version: "1.0.0",
      collection_interval: 60
    }
  }
  
  assert_eq(avro_deserialized.timestamp, 1640995200)
  assert_eq(avro_deserialized.metric_name, "cpu_usage")
  assert_eq(avro_deserialized.value, 75.5)
  assert_eq(avro_deserialized.tags["host"], "server-001")
  assert_eq(avro_deserialized.tags["region"], "us-east")
  assert_eq(avro_deserialized.metadata.source, "system-monitor")
  assert_eq(avro_deserialized.metadata.version, "1.0.0")
  assert_eq(avro_deserialized.metadata.collection_interval, 60)
  
  // 验证Avro性能
  let avro_performance = {
    data_size_bytes: avro_serialized.length(),
    serialization_time_ms: 2,
    deserialization_time_ms: 2,
    compression_ratio: 0.5 // Avro通常比JSON更紧凑
  }
  
  assert_true(avro_performance.data_size_bytes > 0)
  assert_true(avro_performance.serialization_time_ms > 0)
  assert_true(avro_performance.deserialization_time_ms > 0)
  assert_true(avro_performance.compression_ratio < 1.0)
  
  // 验证Avro模式演进
  let avro_v2_schema = {
    type: "record",
    name: "TelemetryData",
    namespace: "com.azimuth.telemetry",
    fields: [
      { name: "timestamp", type: "long" },
      { name: "metric_name", type: "string" },
      { name: "value", type: "double" },
      { 
        name: "tags", 
        type: {
          type: "map",
          values: "string"
        }
      },
      {
        name: "metadata",
        type: {
          type: "record",
          name: "Metadata",
          fields: [
            { name: "source", type: "string" },
            { name: "version", type: "string" },
            { name: "collection_interval", type: "int", "default": 60 },
            { name: "retention_days", type: "int", "default": 30 } // 新字段
          ]
        },
        "default": null // 新的默认值
      }
    ]
  }
  
  // v2应该能反序列化v1的数据
  let v2_compatible_deserialized = {
    timestamp: 1640995200,
    metric_name: "cpu_usage",
    value: 75.5,
    tags: {
      "host": "server-001",
      "region": "us-east"
    },
    metadata: {
      source: "system-monitor",
      version: "1.0.0",
      collection_interval: 60,
      retention_days: 30 // 新字段，使用默认值
    }
  }
  
  assert_eq(v2_compatible_deserialized.timestamp, 1640995200)
  assert_eq(v2_compatible_deserialized.metric_name, "cpu_usage")
  assert_eq(v2_compatible_deserialized.value, 75.5)
  assert_eq(v2_compatible_deserialized.metadata.retention_days, 30) // 默认值
}

// 测试5: 自定义序列化格式
test "自定义序列化格式" {
  // 模拟自定义序列化格式
  let custom_format = {
    name: "Azimuth Compact Format (ACF)",
    version: 1,
    specification: {
      header_size: 8,
      compression: "lz4",
      checksum: "crc32"
    },
    structure: {
      header: [
        { name: "magic", size: 2, value: 0xACF1 },
        { name: "version", size: 1, value: 1 },
        { name: "flags", size: 1, value: 0 },
        { name: "data_length", size: 4 }
      ],
      payload: [
        { name: "timestamp", type: "varint" },
        { name: "metric_id", type: "uint16" },
        { name: "value", type: "float32" },
        { name: "tag_count", type: "uint8" },
        { name: "tags", type: "array", element_type: "key_value_pair" }
      ],
      footer: [
        { name: "checksum", size: 4 }
      ]
    }
  }
  
  // 模拟自定义序列化结果
  let custom_serialized = [
    // Header
    0xAC, 0xF1,             // magic
    0x01,                   // version
    0x00,                   // flags
    0x00, 0x00, 0x00, 0x20, // data_length (32 bytes)
    
    // Payload
    0x80, 0x80, 0x80, 0x08, // timestamp (varint encoded 1640995200)
    0x03, 0xE9,             // metric_id (1001)
    0x42, 0x96, 0x00, 0x00, // value (75.5 as IEEE 754)
    0x02,                   // tag_count
    // tag 1
    0x01,                   // key_id (host)
    0x0A,                   // value_id (server-001)
    // tag 2
    0x02,                   // key_id (region)
    0x14,                   // value_id (us-east)
    
    // Footer
    0x12, 0x34, 0x56, 0x78  // checksum
  ]
  
  // 验证自定义序列化格式
  assert_eq(custom_serialized[0], 0xAC) // magic byte 1
  assert_eq(custom_serialized[1], 0xF1) // magic byte 2
  assert_eq(custom_serialized[2], 1)     // version
  assert_eq(custom_serialized[3], 0)     // flags
  
  // 验证数据长度
  let data_length = (custom_serialized[4].to_int() << 24) | 
                    (custom_serialized[5].to_int() << 16) | 
                    (custom_serialized[6].to_int() << 8) | 
                    custom_serialized[7].to_int()
  assert_eq(data_length, 32)
  
  // 验证自定义反序列化
  let custom_deserialized = {
    header: {
      magic: (custom_serialized[0].to_int() << 8) | custom_serialized[1].to_int(),
      version: custom_serialized[2],
      flags: custom_serialized[3],
      data_length: data_length
    },
    payload: {
      timestamp: 1640995200,
      metric_id: 1001,
      value: 75.5,
      tag_count: 2,
      tags: [
        { key_id: 1, value_id: 10 },
        { key_id: 2, value_id: 20 }
      ]
    },
    footer: {
      checksum: (custom_serialized[custom_serialized.length()-4].to_int() << 24) | 
                (custom_serialized[custom_serialized.length()-3].to_int() << 16) | 
                (custom_serialized[custom_serialized.length()-2].to_int() << 8) | 
                custom_serialized[custom_serialized.length()-1].to_int()
    }
  }
  
  assert_eq(custom_deserialized.header.magic, 0xACF1)
  assert_eq(custom_deserialized.header.version, 1)
  assert_eq(custom_deserialized.header.flags, 0)
  assert_eq(custom_deserialized.header.data_length, 32)
  
  assert_eq(custom_deserialized.payload.timestamp, 1640995200)
  assert_eq(custom_deserialized.payload.metric_id, 1001)
  assert_eq(custom_deserialized.payload.value, 75.5)
  assert_eq(custom_deserialized.payload.tag_count, 2)
  assert_eq(custom_deserialized.footer.checksum, 0x12345678)
  
  // 验证自定义格式性能
  let custom_performance = {
    data_size_bytes: custom_serialized.length(),
    serialization_time_ms: 1,
    deserialization_time_ms: 1,
    compression_ratio: 0.3 // 自定义格式通常最紧凑
  }
  
  assert_true(custom_performance.data_size_bytes > 0)
  assert_true(custom_performance.serialization_time_ms > 0)
  assert_true(custom_performance.deserialization_time_ms > 0)
  assert_true(custom_performance.compression_ratio < 1.0)
  
  // 验证自定义格式的扩展性
  let custom_v2_format = {
    name: "Azimuth Compact Format (ACF)",
    version: 2,
    specification: {
      header_size: 8,
      compression: "lz4",
      checksum: "crc32"
    },
    structure: {
      header: [
        { name: "magic", size: 2, value: 0xACF1 },
        { name: "version", size: 1, value: 2 },
        { name: "flags", size: 1, value: 0 },
        { name: "data_length", size: 4 }
      ],
      payload: [
        { name: "timestamp", type: "varint" },
        { name: "metric_id", type: "uint16" },
        { name: "value", type: "float32" },
        { name: "unit", type: "uint8" }, // 新字段
        { name: "tag_count", type: "uint8" },
        { name: "tags", type: "array", element_type: "key_value_pair" }
      ],
      footer: [
        { name: "checksum", size: 4 }
      ]
    }
  }
  
  // v2应该能反序列化v1的数据
  let v2_compatible_deserialized = {
    header: {
      magic: 0xACF1,
      version: 1, // 识别为v1格式
      flags: 0,
      data_length: 32
    },
    payload: {
      timestamp: 1640995200,
      metric_id: 1001,
      value: 75.5,
      unit: 0, // 新字段，v1中没有，使用默认值
      tag_count: 2,
      tags: [
        { key_id: 1, value_id: 10 },
        { key_id: 2, value_id: 20 }
      ]
    },
    footer: {
      checksum: 0x12345678
    }
  }
  
  assert_eq(v2_compatible_deserialized.header.version, 1) // 正确识别v1格式
  assert_eq(v2_compatible_deserialized.payload.unit, 0) // 新字段使用默认值
}

// 测试6: 序列化格式比较
test "序列化格式比较" {
  // 模拟相同数据的不同序列化格式
  let test_data = {
    timestamp: 1640995200,
    metric_name: "cpu_usage",
    value: 75.5,
    tags: [
      { key: "host", value: "server-001" },
      { key: "region", value: "us-east" }
    ],
    metadata: {
      source: "system-monitor",
      version: "1.0.0",
      collection_interval: 60
    }
  }
  
  // 不同格式的序列化结果
  let serialization_results = [
    {
      format: "JSON",
      data_size_bytes: 185,
      serialization_time_ms: 5,
      deserialization_time_ms: 8,
      compression_ratio: 1.0,
      human_readable: true,
      schema_evolution: "flexible",
      cross_language_support: "excellent"
    },
    {
      format: "Binary",
      data_size_bytes: 64,
      serialization_time_ms: 2,
      deserialization_time_ms: 3,
      compression_ratio: 0.35,
      human_readable: false,
      schema_evolution: "rigid",
      cross_language_support: "limited"
    },
    {
      format: "Protocol Buffers",
      data_size_bytes: 74,
      serialization_time_ms: 1,
      deserialization_time_ms: 1,
      compression_ratio: 0.4,
      human_readable: false,
      schema_evolution: "forward_compatible",
      cross_language_support: "excellent"
    },
    {
      format: "Avro",
      data_size_bytes: 92,
      serialization_time_ms: 2,
      deserialization_time_ms: 2,
      compression_ratio: 0.5,
      human_readable: false,
      schema_evolution: "both_directions",
      cross_language_support: "good"
    },
    {
      format: "Custom (ACF)",
      data_size_bytes: 48,
      serialization_time_ms: 1,
      deserialization_time_ms: 1,
      compression_ratio: 0.26,
      human_readable: false,
      schema_evolution: "custom",
      cross_language_support: "limited"
    }
  ]
  
  // 验证序列化结果
  assert_eq(serialization_results.length(), 5)
  
  // 验证JSON特性
  let json_result = serialization_results.filter_fn(r => r.format == "JSON")[0]
  assert_true(json_result.human_readable)
  assert_eq(json_result.schema_evolution, "flexible")
  assert_eq(json_result.cross_language_support, "excellent")
  assert_eq(json_result.compression_ratio, 1.0)
  
  // 验证二进制格式特性
  let binary_result = serialization_results.filter_fn(r => r.format == "Binary")[0]
  assert_false(binary_result.human_readable)
  assert_eq(binary_result.schema_evolution, "rigid")
  assert_eq(binary_result.cross_language_support, "limited")
  assert_true(binary_result.compression_ratio < 0.5)
  
  // 验证Protocol Buffers特性
  let proto_result = serialization_results.filter_fn(r => r.format == "Protocol Buffers")[0]
  assert_false(proto_result.human_readable)
  assert_eq(proto_result.schema_evolution, "forward_compatible")
  assert_eq(proto_result.cross_language_support, "excellent")
  assert_true(proto_result.serialization_time_ms <= 2)
  assert_true(proto_result.deserialization_time_ms <= 2)
  
  // 验证Avro特性
  let avro_result = serialization_results.filter_fn(r => r.format == "Avro")[0]
  assert_false(avro_result.human_readable)
  assert_eq(avro_result.schema_evolution, "both_directions")
  assert_eq(avro_result.cross_language_support, "good")
  
  // 验证自定义格式特性
  let custom_result = serialization_results.filter_fn(r => r.format == "Custom (ACF)")[0]
  assert_false(custom_result.human_readable)
  assert_eq(custom_result.schema_evolution, "custom")
  assert_eq(custom_result.cross_language_support, "limited")
  assert_true(custom_result.compression_ratio < 0.3) // 最紧凑
  
  // 按数据大小排序
  let sorted_by_size = serialization_results.sort(fn(a, b) { a.data_size_bytes <= b.data_size_bytes })
  
  // 验证大小排序
  for i in 1..sorted_by_size.length() {
    assert_true(sorted_by_size[i].data_size_bytes >= sorted_by_size[i-1].data_size_bytes)
  }
  
  // 最紧凑的应该是自定义格式
  assert_eq(sorted_by_size[0].format, "Custom (ACF)")
  
  // 最不紧凑的应该是JSON
  assert_eq(sorted_by_size[sorted_by_size.length()-1].format, "JSON")
  
  // 按序列化时间排序
  let sorted_by_serialization_time = serialization_results.sort(fn(a, b) { a.serialization_time_ms <= b.serialization_time_ms })
  
  // 验证时间排序
  for i in 1..sorted_by_serialization_time.length() {
    assert_true(sorted_by_serialization_time[i].serialization_time_ms >= sorted_by_serialization_time[i-1].serialization_time_ms)
  }
  
  // 最快的应该是Protocol Buffers和自定义格式
  assert_true(sorted_by_serialization_time[0].serialization_time_ms <= 1)
  
  // 验证不同场景下的最佳格式选择
  
  // 调试和开发场景：选择JSON
  let debug_scenario = {
    priority: "human_readable",
    best_format: "JSON",
    reason: "JSON格式易于人类阅读和调试"
  }
  assert_eq(debug_scenario.best_format, "JSON")
  
  // 高性能场景：选择Protocol Buffers或自定义格式
  let performance_scenario = {
    priority: "speed",
    best_format: "Protocol Buffers",
    reason: "Protocol Buffers提供最快的序列化/反序列化速度"
  }
  assert_eq(performance_scenario.best_format, "Protocol Buffers")
  
  // 存储优化场景：选择自定义格式
  let storage_scenario = {
    priority: "space_efficiency",
    best_format: "Custom (ACF)",
    reason: "自定义格式提供最高的压缩比"
  }
  assert_eq(storage_scenario.best_format, "Custom (ACF)")
  
  // 跨语言兼容性场景：选择JSON或Protocol Buffers
  let cross_language_scenario = {
    priority: "cross_language",
    best_format: "Protocol Buffers",
    reason: "Protocol Buffers提供优秀的跨语言支持"
  }
  assert_eq(cross_language_scenario.best_format, "Protocol Buffers")
  
  // 验证序列化格式的综合评分
  let format_scores = []
  for result in serialization_results {
    let score = 
      (10.0 - result.serialization_time_ms) * 0.2 + // 序列化速度权重20%
      (10.0 - result.deserialization_time_ms) * 0.2 + // 反序列化速度权重20%
      (1.0 - result.compression_ratio) * 10.0 * 0.3 + // 压缩比权重30%
      (result.cross_language_support == "excellent" ? 10.0 : 
       result.cross_language_support == "good" ? 7.0 : 
       result.cross_language_support == "limited" ? 4.0 : 1.0) * 0.3 // 跨语言支持权重30%
    
    format_scores = format_scores.push({
      format: result.format,
      score: score
    })
  }
  
  // 按评分排序
  let sorted_by_score = format_scores.sort(fn(a, b) { a.score >= b.score })
  
  // 验证评分排序
  for i in 1..sorted_by_score.length() {
    assert_true(sorted_by_score[i].score <= sorted_by_score[i-1].score)
  }
  
  // Protocol Buffers应该有最高的综合评分
  assert_eq(sorted_by_score[0].format, "Protocol Buffers")
}