// Azimuth Data Integrity and Validation Tests
// 数据完整性和验证测试，验证遥测数据的完整性和一致性

// 测试1: Span数据完整性验证
test "span data integrity validation" {
  // 创建有效的Span上下文
  let valid_span_ctx = SpanContext({
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  // 验证有效Span上下文
  assert_true(@azimuth.Validation::validate_span_context(valid_span_ctx))
  
  // 创建无效的Span上下文
  let invalid_span_ctx = SpanContext({
    trace_id: "invalid", // 太短
    span_id: "1234567890abcdef",
    sampled: true,
    trace_state: ""
  })
  
  // 验证无效Span上下文
  assert_false(@azimuth.Validation::validate_span_context(invalid_span_ctx))
  
  // 创建完整的Span
  let valid_span = Span({
    name: "http.request",
    kind: Server,
    recording: true,
    span_context: valid_span_ctx,
    parent_span_id: Some("parent1234567890"),
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("https://api.example.com/users")),
      ("http.status_code", IntValue(200))
    ],
    events: [
      Event({
        name: "http.request.started",
        timestamp: 1704067200000000000L,
        attributes: [("event.type", StringValue("start"))]
      })
    ],
    links: [
      Link({
        linked_span_context: valid_span_ctx,
        attributes: [("link.type", StringValue("follows_from"))]
      })
    ],
    status: Ok,
    start_time: 1704067200000000000L,
    end_time: Some(1704067200000000001L),
    duration: Some(1000000L)
  })
  
  // 验证完整Span数据
  let span_validation_result = @azimuth.Validation::validate_span(valid_span)
  assert_true(span_validation_result.is_valid)
  assert_eq(span_validation_result.errors.length(), 0)
  
  // 创建有问题的Span
  let invalid_span = Span({
    name: "", // 空名称
    kind: Server,
    recording: true,
    span_context: invalid_span_ctx, // 无效上下文
    parent_span_id: Some("invalid"), // 无效父Span ID
    attributes: [
      ("http.method", StringValue("")), // 空值
      ("http.url", StringValue("invalid-url")), // 无效URL
      ("http.status_code", IntValue(-1)) // 无效状态码
    ],
    events: [
      Event({
        name: "", // 空事件名
        timestamp: -1L, // 无效时间戳
        attributes: [] // 空属性
      })
    ],
    links: [
      Link({
        linked_span_context: invalid_span_ctx, // 无效上下文
        attributes: [] // 空属性
      })
    ],
    status: Error,
    start_time: 1704067200000000000L,
    end_time: Some(1704067199999999999L), // 结束时间早于开始时间
    duration: Some(-1000000L) // 负持续时间
  })
  
  // 验证无效Span数据
  let invalid_span_validation_result = @azimuth.Validation::validate_span(invalid_span)
  assert_false(invalid_span_validation_result.is_valid)
  assert_true(invalid_span_validation_result.errors.length() > 0)
  
  // 验证具体错误
  let mut found_empty_name = false
  let mut found_invalid_context = false
  let mut found_invalid_timing = false
  
  for error in invalid_span_validation_result.errors {
    match error.code {
      "EMPTY_SPAN_NAME" => found_empty_name = true
      "INVALID_SPAN_CONTEXT" => found_invalid_context = true
      "INVALID_TIMING" => found_invalid_timing = true
      _ => assert_true(false) // 意外的错误代码
    }
  }
  
  assert_true(found_empty_name)
  assert_true(found_invalid_context)
  assert_true(found_invalid_timing)
}

// 测试2: LogRecord数据完整性验证
test "log record data integrity validation" {
  // 创建有效的LogRecord
  let valid_log_record = LogRecord({
    timestamp: 1704067200000000000L,
    observed_timestamp: Some(1704067200000000001L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("HTTP request processed successfully"),
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("/api/users")),
      ("http.status_code", IntValue(200)),
      ("user.id", StringValue("12345"))
    ],
    trace_id: Some("1234567890abcdef1234567890abcdef"),
    span_id: Some("1234567890abcdef"),
    trace_flags: Some(1),
    resource: Some(Resource({
      attributes: [
        ("service.name", StringValue("azimuth-api")),
        ("service.version", StringValue("1.2.3"))
      ],
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    })),
    instrumentation_scope: Some(InstrumentationScope({
      name: "azimuth.instrumentation",
      version: Some("1.0.0"),
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    }))
  })
  
  // 验证有效LogRecord
  let log_validation_result = @azimuth.Validation::validate_log_record(valid_log_record)
  assert_true(log_validation_result.is_valid)
  assert_eq(log_validation_result.errors.length(), 0)
  
  // 创建无效的LogRecord
  let invalid_log_record = LogRecord({
    timestamp: -1L, // 无效时间戳
    observed_timestamp: Some(1704067200000000001L), // 观察时间早于时间戳
    severity_number: 999, // 无效严重级别
    severity_text: Some("INVALID"), // 无效严重文本
    body: Some(""), // 空消息体
    attributes: [
      ("http.method", StringValue("")), // 空值
      ("http.url", StringValue("")), // 空URL
      ("http.status_code", IntValue(-1)), // 无效状态码
      ("user.id", StringValue("")) // 空用户ID
    ],
    trace_id: Some("invalid"), // 无效trace ID
    span_id: Some("invalid"), // 无效span ID
    trace_flags: Some(255), // 无效跟踪标志
    resource: Some(Resource({
      attributes: [], // 空属性
      schema_url: Some("") // 空schema URL
    })),
    instrumentation_scope: Some(InstrumentationScope({
      name: "", // 空名称
      version: Some(""), // 空版本
      schema_url: Some("") // 空schema URL
    }))
  })
  
  // 验证无效LogRecord
  let invalid_log_validation_result = @azimuth.Validation::validate_log_record(invalid_log_record)
  assert_false(invalid_log_validation_result.is_valid)
  assert_true(invalid_log_validation_result.errors.length() > 0)
  
  // 验证具体错误
  let mut found_invalid_timestamp = false
  let mut found_invalid_severity = false
  let mut found_invalid_trace_id = false
  let mut found_invalid_resource = false
  
  for error in invalid_log_validation_result.errors {
    match error.code {
      "INVALID_TIMESTAMP" => found_invalid_timestamp = true
      "INVALID_SEVERITY" => found_invalid_severity = true
      "INVALID_TRACE_ID" => found_invalid_trace_id = true
      "INVALID_RESOURCE" => found_invalid_resource = true
      _ => assert_true(false) // 意外的错误代码
    }
  }
  
  assert_true(found_invalid_timestamp)
  assert_true(found_invalid_severity)
  assert_true(found_invalid_trace_id)
  assert_true(found_invalid_resource)
}

// 测试3: Resource数据完整性验证
test "resource data integrity validation" {
  // 创建有效的Resource
  let valid_resource = Resource({
    attributes: [
      ("service.name", StringValue("azimuth-api")),
      ("service.version", StringValue("1.2.3")),
      ("service.instance.id", StringValue("api-001")),
      ("deployment.environment", StringValue("production")),
      ("host.name", StringValue("prod-server-01")),
      ("host.arch", StringValue("amd64")),
      ("os.type", StringValue("linux")),
      ("os.version", StringValue("5.15.0"))
    ],
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  // 验证有效Resource
  let resource_validation_result = @azimuth.Validation::validate_resource(valid_resource)
  assert_true(resource_validation_result.is_valid)
  assert_eq(resource_validation_result.errors.length(), 0)
  
  // 创建无效的Resource
  let invalid_resource = Resource({
    attributes: [
      ("service.name", StringValue("")), // 空服务名
      ("service.version", StringValue("")), // 空版本
      ("service.instance.id", StringValue("")), // 空实例ID
      ("deployment.environment", StringValue("")), // 空环境
      ("host.name", StringValue("")), // 空主机名
      ("host.arch", StringValue("invalid-arch")), // 无效架构
      ("os.type", StringValue("invalid-os")), // 无效操作系统类型
      ("os.version", StringValue("")) // 空版本
    ],
    schema_url: Some("") // 空schema URL
  })
  
  // 验证无效Resource
  let invalid_resource_validation_result = @azimuth.Validation::validate_resource(invalid_resource)
  assert_false(invalid_resource_validation_result.is_valid)
  assert_true(invalid_resource_validation_result.errors.length() > 0)
  
  // 验证具体错误
  let mut found_empty_service_name = false
  let mut found_invalid_arch = false
  let mut found_empty_schema_url = false
  
  for error in invalid_resource_validation_result.errors {
    match error.code {
      "EMPTY_SERVICE_NAME" => found_empty_service_name = true
      "INVALID_ARCHITECTURE" => found_invalid_arch = true
      "EMPTY_SCHEMA_URL" => found_empty_schema_url = true
      _ => assert_true(false) // 意外的错误代码
    }
  }
  
  assert_true(found_empty_service_name)
  assert_true(found_invalid_arch)
  assert_true(found_empty_schema_url)
}

// 测试4: 度量指标数据完整性验证
test "metrics data integrity validation" {
  // 创建有效的Counter度量
  let valid_counter = Counter({
    name: "http.requests.total",
    description: Some("Total number of HTTP requests"),
    unit: Some("requests"),
    data: [
      ("GET", 100),
      ("POST", 50),
      ("PUT", 25),
      ("DELETE", 10)
    ]
  })
  
  // 验证有效Counter
  let counter_validation_result = @azimuth.Validation::validate_counter(valid_counter)
  assert_true(counter_validation_result.is_valid)
  assert_eq(counter_validation_result.errors.length(), 0)
  
  // 创建无效的Counter度量
  let invalid_counter = Counter({
    name: "", // 空名称
    description: Some(""), // 空描述
    unit: Some(""), // 空单位
    data: [
      ("", -100), // 空键和负值
      ("GET", -50), // 负值
      ("PUT", 0), // 零值（对于Counter可能无效）
      ("DELETE", -10) // 负值
    ]
  })
  
  // 验证无效Counter
  let invalid_counter_validation_result = @azimuth.Validation::validate_counter(invalid_counter)
  assert_false(invalid_counter_validation_result.is_valid)
  assert_true(invalid_counter_validation_result.errors.length() > 0)
  
  // 创建有效的Histogram度量
  let valid_histogram = Histogram({
    name: "http.request.duration",
    description: Some("HTTP request duration distribution"),
    unit: Some("ms"),
    buckets: [
      (0.0, 10.0, 50),
      (10.0, 50.0, 100),
      (50.0, 100.0, 75),
      (100.0, 500.0, 25),
      (500.0, Infinity, 5)
    ],
    sum: 12500.0,
    count: 255
  })
  
  // 验证有效Histogram
  let histogram_validation_result = @azimuth.Validation::validate_histogram(valid_histogram)
  assert_true(histogram_validation_result.is_valid)
  assert_eq(histogram_validation_result.errors.length(), 0)
  
  // 创建无效的Histogram度量
  let invalid_histogram = Histogram({
    name: "", // 空名称
    description: Some(""), // 空描述
    unit: Some(""), // 空单位
    buckets: [
      (-1.0, 10.0, 50), // 负下限
      (10.0, 5.0, 100), // 下限大于上限
      (50.0, 100.0, -75), // 负计数
      (100.0, 500.0, 25),
      (500.0, Infinity, -5) // 负计数
    ],
    sum: -12500.0, // 负总和
    count: -255 // 负计数
  })
  
  // 验证无效Histogram
  let invalid_histogram_validation_result = @azimuth.Validation::validate_histogram(invalid_histogram)
  assert_false(invalid_histogram_validation_result.is_valid)
  assert_true(invalid_histogram_validation_result.errors.length() > 0)
  
  // 验证具体错误
  let mut found_empty_name = false
  let mut found_negative_sum = false
  let mut found_invalid_bucket = false
  
  for error in invalid_histogram_validation_result.errors {
    match error.code {
      "EMPTY_METRIC_NAME" => found_empty_name = true
      "NEGATIVE_SUM" => found_negative_sum = true
      "INVALID_BUCKET" => found_invalid_bucket = true
      _ => assert_true(false) // 意外的错误代码
    }
  }
  
  assert_true(found_empty_name)
  assert_true(found_negative_sum)
  assert_true(found_invalid_bucket)
}

// 测试5: 数据一致性验证
test "data consistency validation" {
  // 创建相关的数据集
  let span_ctx = SpanContext({
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    sampled: true,
    trace_state: ""
  })
  
  let span = Span({
    name: "http.request",
    kind: Server,
    recording: true,
    span_context: span_ctx,
    parent_span_id: Some("parent1234567890"),
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("/api/users")),
      ("http.status_code", IntValue(200))
    ],
    events: [
      Event({
        name: "http.request.started",
        timestamp: 1704067200000000000L,
        attributes: [("event.type", StringValue("start"))]
      }),
      Event({
        name: "http.request.completed",
        timestamp: 1704067200000000001L,
        attributes: [
          ("event.type", StringValue("end")),
          ("http.status_code", IntValue(200))
        ]
      })
    ],
    links: [],
    status: Ok,
    start_time: 1704067200000000000L,
    end_time: Some(1704067200000000001L),
    duration: Some(1000000L)
  })
  
  let log_record = LogRecord({
    timestamp: 1704067200000000000L,
    observed_timestamp: Some(1704067200000000000L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("HTTP request started"),
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("/api/users"))
    ],
    trace_id: Some(span_ctx.trace_id),
    span_id: Some(span_ctx.span_id),
    trace_flags: Some(1),
    resource: None,
    instrumentation_scope: None
  })
  
  // 验证数据一致性
  let consistency_result = @azimuth.Validation::validate_data_consistency([span, log_record])
  assert_true(consistency_result.is_consistent)
  assert_eq(consistency_result.inconsistencies.length(), 0)
  
  // 创建不一致的数据集
  let inconsistent_span = Span({
    name: "http.request",
    kind: Server,
    recording: true,
    span_context: SpanContext({
      trace_id: "different_trace_id", // 不同的trace ID
      span_id: "1234567890abcdef",
      sampled: true,
      trace_state: ""
    }),
    parent_span_id: Some("parent1234567890"),
    attributes: [
      ("http.method", StringValue("POST")), // 不同的方法
      ("http.url", StringValue("/api/users")),
      ("http.status_code", IntValue(404)) // 不同的状态码
    ],
    events: [
      Event({
        name: "http.request.started",
        timestamp: 1704067200000000002L, // 不同的时间戳
        attributes: [("event.type", StringValue("start"))]
      })
    ],
    links: [],
    status: Error, // 不同的状态
    start_time: 1704067200000000002L, // 不同的开始时间
    end_time: Some(1704067200000000003L),
    duration: Some(1000000L)
  })
  
  let inconsistent_log_record = LogRecord({
    timestamp: 1704067200000000001L,
    observed_timestamp: Some(1704067200000000001L),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("HTTP request failed"),
    attributes: [
      ("http.method", StringValue("GET")), // 与Span不一致
      ("http.url", StringValue("/api/orders")) // 不同的URL
    ],
    trace_id: Some("different_trace_id"), // 与Span一致
    span_id: Some("1234567890abcdef"),
    trace_flags: Some(1),
    resource: None,
    instrumentation_scope: None
  })
  
  // 验证不一致的数据
  let inconsistency_result = @azimuth.Validation::validate_data_consistency([inconsistent_span, inconsistent_log_record])
  assert_false(inconsistency_result.is_consistent)
  assert_true(inconsistency_result.inconsistencies.length() > 0)
  
  // 验证具体不一致项
  let mut found_method_mismatch = false
  let mut found_url_mismatch = false
  let mut found_status_mismatch = false
  
  for inconsistency in inconsistency_result.inconsistencies {
    match inconsistency.type {
      "HTTP_METHOD_MISMATCH" => found_method_mismatch = true
      "HTTP_URL_MISMATCH" => found_url_mismatch = true
      "STATUS_MISMATCH" => found_status_mismatch = true
      _ => assert_true(false) // 意外的不一致类型
    }
  }
  
  assert_true(found_method_mismatch)
  assert_true(found_url_mismatch)
  assert_true(found_status_mismatch)
}

// 测试6: 数据完整性校验和验证
test "data integrity checksum validation" {
  // 创建测试数据
  let test_data = "test telemetry data for integrity validation"
  
  // 计算校验和
  let checksum = @azimuth.Integrity::calculate_checksum(test_data)
  assert_true(checksum.length() > 0)
  
  // 验证校验和
  let is_valid = @azimuth.Integrity::verify_checksum(test_data, checksum)
  assert_true(is_valid)
  
  // 测试损坏的数据
  let corrupted_data = "test telemetry data for integrity validation!" // 添加了感叹号
  let is_corrupted_valid = @azimuth.Integrity::verify_checksum(corrupted_data, checksum)
  assert_false(is_corrupted_valid)
  
  // 创建带校验和的Span
  let span_with_checksum = Span({
    name: "checksum.test.span",
    kind: Internal,
    recording: true,
    span_context: SpanContext({
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      sampled: true,
      trace_state: ""
    }),
    parent_span_id: None,
    attributes: [
      ("data.content", StringValue(test_data)),
      ("data.checksum", StringValue(checksum))
    ],
    events: [],
    links: [],
    status: Ok,
    start_time: 1704067200000000000L,
    end_time: Some(1704067200000000001L),
    duration: Some(1000000L)
  })
  
  // 验证带校验和的Span
  let checksum_validation_result = @azimuth.Integrity::validate_span_checksum(span_with_checksum)
  assert_true(checksum_validation_result.is_valid)
  
  // 创建带损坏校验和的Span
  let span_with_corrupted_checksum = Span({
    name: "checksum.test.span",
    kind: Internal,
    recording: true,
    span_context: SpanContext({
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      sampled: true,
      trace_state: ""
    }),
    parent_span_id: None,
    attributes: [
      ("data.content", StringValue(corrupted_data)),
      ("data.checksum", StringValue(checksum)) // 原始校验和
    ],
    events: [],
    links: [],
    status: Ok,
    start_time: 1704067200000000000L,
    end_time: Some(1704067200000000001L),
    duration: Some(1000000L)
  })
  
  // 验证带损坏校验和的Span
  let corrupted_checksum_validation_result = @azimuth.Integrity::validate_span_checksum(span_with_corrupted_checksum)
  assert_false(corrupted_checksum_validation_result.is_valid)
  
  // 测试数字签名（如果支持）
  let digital_signature = @azimuth.Integrity::sign_data(test_data, "test_key")
  assert_true(digital_signature.length() > 0)
  
  // 验证数字签名
  let is_signature_valid = @azimuth.Integrity::verify_signature(test_data, digital_signature, "test_key")
  assert_true(is_signature_valid)
  
  // 验证损坏数据的签名
  let is_corrupted_signature_valid = @azimuth.Integrity::verify_signature(corrupted_data, digital_signature, "test_key")
  assert_false(is_corrupted_signature_valid)
}

// 测试7: 数据序列化和反序列化完整性
test "data serialization and deserialization integrity" {
  // 创建原始Span
  let original_span = Span({
    name: "serialization.test.span",
    kind: Server,
    recording: true,
    span_context: SpanContext({
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      sampled: true,
      trace_state: "key1=value1,key2=value2"
    }),
    parent_span_id: Some("parent1234567890"),
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("https://api.example.com/users")),
      ("http.status_code", IntValue(200)),
      ("user.id", StringValue("12345")),
      ("request.size", IntValue(1024)),
      ("response.size", IntValue(2048))
    ],
    events: [
      Event({
        name: "http.request.started",
        timestamp: 1704067200000000000L,
        attributes: [
          ("event.type", StringValue("start")),
          ("request.id", StringValue("req-12345"))
        ]
      }),
      Event({
        name: "http.request.completed",
        timestamp: 1704067200000000001L,
        attributes: [
          ("event.type", StringValue("end")),
          ("request.id", StringValue("req-12345")),
          ("duration.ms", IntValue(100))
        ]
      })
    ],
    links: [
      Link({
        linked_span_context: SpanContext({
          trace_id: "linked1234567890abcdef1234567890",
          span_id: "linked1234567890",
          sampled: false,
          trace_state: ""
        }),
        attributes: [
          ("link.type", StringValue("follows_from")),
          ("relationship", StringValue("causal"))
        ]
      })
    ],
    status: Ok,
    start_time: 1704067200000000000L,
    end_time: Some(1704067200000000001L),
    duration: Some(1000000L)
  })
  
  // 序列化为JSON
  let json_serialized = @azimuth.Serialization::serialize_to_json(original_span)
  assert_true(json_serialized.length() > 0)
  
  // 从JSON反序列化
  let json_deserialized = @azimuth.Serialization::deserialize_from_json(json_serialized)
  assert_true(json_deserialized.is_ok)
  
  // 验证反序列化后的数据
  match json_deserialized {
    Ok(deserialized_span) => {
      assert_eq(deserialized_span.name, original_span.name)
      assert_eq(deserialized_span.kind, original_span.kind)
      assert_eq(deserialized_span.span_context.trace_id, original_span.span_context.trace_id)
      assert_eq(deserialized_span.attributes.length(), original_span.attributes.length())
      assert_eq(deserialized_span.events.length(), original_span.events.length())
      assert_eq(deserialized_span.links.length(), original_span.links.length())
    }
    Err(_) => assert_true(false)
  }
  
  // 序列化为Protocol Buffers（如果支持）
  let protobuf_serialized = @azimuth.Serialization::serialize_to_protobuf(original_span)
  assert_true(protobuf_serialized.length() > 0)
  
  // 从Protocol Buffers反序列化
  let protobuf_deserialized = @azimuth.Serialization::deserialize_from_protobuf(protobuf_serialized)
  assert_true(protobuf_deserialized.is_ok)
  
  // 验证Protocol Buffers反序列化后的数据
  match protobuf_deserialized {
    Ok(deserialized_span) => {
      assert_eq(deserialized_span.name, original_span.name)
      assert_eq(deserialized_span.kind, original_span.kind)
      assert_eq(deserialized_span.span_context.trace_id, original_span.span_context.trace_id)
      assert_eq(deserialized_span.attributes.length(), original_span.attributes.length())
      assert_eq(deserialized_span.events.length(), original_span.events.length())
      assert_eq(deserialized_span.links.length(), original_span.links.length())
    }
    Err(_) => assert_true(false)
  }
  
  // 测试序列化完整性校验
  let serialization_integrity = @azimuth.Integrity::validate_serialization_integrity(original_span, json_serialized)
  assert_true(serialization_integrity.is_intact)
  
  // 测试损坏的序列化数据
  let corrupted_json = json_serialized.substring(0, json_serialized.length() - 1) + "}"
  let corrupted_integrity = @azimuth.Integrity::validate_serialization_integrity(original_span, corrupted_json)
  assert_false(corrupted_integrity.is_intact)
  
  // 测试跨格式序列化一致性
  let cross_format_consistency = @azimuth.Integrity::validate_cross_format_consistency(
    json_serialized,
    protobuf_serialized
  )
  assert_true(cross_format_consistency.is_consistent)
}

// 测试8: 数据传输完整性验证
test "data transmission integrity validation" {
  // 创建测试数据包
  let data_packet = DataPacket({
    id: "packet-12345",
    timestamp: 1704067200000000000L,
    data: "test telemetry data packet",
    checksum: "",
    signature: "",
    metadata: [
      ("source", StringValue("azimuth-collector")),
      ("destination", StringValue("azimuth-processor")),
      ("version", StringValue("1.0"))
    ]
  })
  
  // 计算数据包校验和
  let packet_with_checksum = @azimuth.Transmission::add_checksum(data_packet)
  assert_true(packet_with_checksum.checksum.length() > 0)
  
  // 验证数据包完整性
  let packet_validation = @azimuth.Transmission::validate_packet_integrity(packet_with_checksum)
  assert_true(packet_validation.is_valid)
  
  // 模拟数据传输（添加传输元数据）
  let transmitted_packet = @azimuth.Transmission::mark_transmitted(packet_with_checksum, 1704067200000000001L)
  assert_true(transmitted_packet.metadata.contains("transmitted_at"))
  
  // 验证传输后的数据包
  let transmitted_validation = @azimuth.Transmission::validate_transmitted_packet(transmitted_packet)
  assert_true(transmitted_validation.is_valid)
  
  // 模拟数据包损坏
  let corrupted_packet = DataPacket({
    id: packet_with_checksum.id,
    timestamp: packet_with_checksum.timestamp,
    data: "corrupted telemetry data packet", // 损坏的数据
    checksum: packet_with_checksum.checksum, // 原始校验和
    signature: packet_with_checksum.signature,
    metadata: packet_with_checksum.metadata
  })
  
  // 验证损坏的数据包
  let corrupted_validation = @azimuth.Transmission::validate_packet_integrity(corrupted_packet)
  assert_false(corrupted_validation.is_valid)
  
  // 测试数据包重放攻击检测
  let replay_packet = @azimuth.Transmission::mark_transmitted(packet_with_checksum, 1704067200000000002L)
  let replay_detection = @azimuth.Transmission::detect_replay_attack(replay_packet)
  assert_true(replay_detection.is_replay)
  
  // 测试数据包顺序验证
  let packet1 = @azimuth.Transmission::add_sequence_number(data_packet, 1)
  let packet2 = @azimuth.Transmission::add_sequence_number(data_packet, 2)
  let packet3 = @azimuth.Transmission::add_sequence_number(data_packet, 3)
  
  let ordered_packets = [packet1, packet2, packet3]
  let sequence_validation = @azimuth.Transmission::validate_packet_sequence(ordered_packets)
  assert_true(sequence_validation.is_in_order)
  
  // 测试乱序数据包
  let out_of_order_packets = [packet2, packet1, packet3]
  let out_of_order_validation = @azimuth.Transmission::validate_packet_sequence(out_of_order_packets)
  assert_false(out_of_order_validation.is_in_order)
}