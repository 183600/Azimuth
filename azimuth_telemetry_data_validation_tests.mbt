// Azimuth Telemetry Data Validation Tests
// 遥测数据验证和清理测试用例

test "telemetry data structure validation" {
  // 测试遥测数据结构验证
  let valid_telemetry = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "abcdef1234567890abcdef1234567890",
    span_id : "1234567890abcdef",
    parent_span_id : Some("abcdef1234567890"),
    operation_name : "database.query",
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 250L,
    attributes : [
      ("db.type", @azimuth.StringValue("postgresql")),
      ("db.statement", @azimuth.StringValue("SELECT * FROM users")),
      ("db.connection.pool", @azimuth.StringValue("primary"))
    ],
    events : [
      @azimuth.SpanEvent {
        name : "db.query.executed",
        timestamp : 1640995200100L,
        attributes : [("rows.affected", @azimuth.IntValue(42))]
      }
    ]
  }
  
  // 验证基本结构
  assert_true(valid_telemetry.timestamp > 0L)
  assert_eq(valid_telemetry.trace_id.length(), 32)
  assert_eq(valid_telemetry.span_id.length(), 16)
  assert_true(valid_telemetry.operation_name.length() > 0)
  assert_true(valid_telemetry.duration_ms >= 0L)
  
  // 验证属性结构
  assert_true(valid_telemetry.attributes.length() > 0)
  for attr in valid_telemetry.attributes {
    assert_true(attr.0.length() > 0)
    match attr.1 {
      @azimuth.StringValue(v) => assert_true(v.length() >= 0)
      @azimuth.IntValue(v) => assert_true(v >= 0)
      @azimuth.FloatValue(v) => assert_true(v >= 0.0)
      @azimuth.BoolValue(v) => assert_true(true) // 布尔值总是有效
      _ => assert_true(false)
    }
  }
  
  // 验证事件结构
  assert_true(valid_telemetry.events.length() > 0)
  for event in valid_telemetry.events {
    assert_true(event.name.length() > 0)
    assert_true(event.timestamp >= valid_telemetry.timestamp)
    assert_true(event.timestamp <= valid_telemetry.timestamp + valid_telemetry.duration_ms)
  }
}

test "telemetry data sanitization and cleaning" {
  // 测试遥测数据清理和消毒
  let dirty_telemetry = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "   abcdef1234567890abcdef1234567890   ", // 带空格
    span_id : "1234567890ABCDEF", // 大写字母
    parent_span_id : Some("  abcdef1234567890  "), // 带空格
    operation_name : "  database.query  ", // 带空格
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 250L,
    attributes : [
      ("db.type", @azimuth.StringValue("  postgresql  ")), // 带空格
      ("db.password", @azimuth.StringValue("secret123")), // 敏感信息
      ("user.email", @azimuth.StringValue("user@example.com")), // PII
      ("api.key", @azimuth.StringValue("sk-1234567890abcdef")) // API密钥
    ],
    events : [
      @azimuth.SpanEvent {
        name : "  db.query.executed  ", // 带空格
        timestamp : 1640995200100L,
        attributes : [
          ("rows.affected", @azimuth.IntValue(42)),
          ("query.time", @azimuth.StringValue("  150ms  ")) // 带空格
        ]
      }
    ]
  }
  
  // 清理遥测数据
  let sanitized = @azimuth.sanitize_telemetry_data(dirty_telemetry)
  
  // 验证清理结果
  assert_eq(sanitized.trace_id, "abcdef1234567890abcdef1234567890") // 去除空格
  assert_eq(sanitized.span_id, "1234567890abcdef") // 转换为小写
  match sanitized.parent_span_id {
    Some(id) => assert_eq(id, "abcdef1234567890") // 去除空格
    None => assert_true(false)
  }
  assert_eq(sanitized.operation_name, "database.query") // 去除空格
  
  // 验证敏感信息被移除或屏蔽
  let sensitive_attrs = ["db.password", "user.email", "api.key"]
  for attr in sanitized.attributes {
    assert_false(sensitive_attrs.contains(attr.0))
  }
  
  // 验证非敏感信息保留但被清理
  let db_type = sanitized.attributes.filter(fn(a) { a.0 == "db.type" })
  assert_eq(db_type.length(), 1)
  match db_type[0].1 {
    @azimuth.StringValue(v) => assert_eq(v, "postgresql")
    _ => assert_true(false)
  }
  
  // 验证事件名称被清理
  assert_eq(sanitized.events[0].name, "db.query.executed")
  
  // 验证事件属性被清理
  let query_time = sanitized.events[0].attributes.filter(fn(a) { a.0 == "query.time" })
  assert_eq(query_time.length(), 1)
  match query_time[0].1 {
    @azimuth.StringValue(v) => assert_eq(v, "150ms")
    _ => assert_true(false)
  }
}

test "telemetry data integrity verification" {
  // 测试遥测数据完整性验证
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
  
  // 计算数据哈希值
  let original_hash = @azimuth.calculate_telemetry_hash(telemetry_data)
  
  // 验证数据完整性
  let is_valid = @azimuth.verify_telemetry_integrity(telemetry_data, original_hash)
  assert_true(is_valid)
  
  // 修改数据并验证完整性检查失败
  let modified_data = { telemetry_data |
    duration_ms : 200L // 修改持续时间
  }
  
  let modified_hash = @azimuth.calculate_telemetry_hash(modified_data)
  let is_modified_valid = @azimuth.verify_telemetry_integrity(telemetry_data, modified_hash)
  assert_false(is_modified_valid)
  
  // 验证修改后的数据与其自身哈希匹配
  let is_self_valid = @azimuth.verify_telemetry_integrity(modified_data, modified_hash)
  assert_true(is_self_valid)
}

test "telemetry data schema validation" {
  // 测试遥测数据模式验证
  let valid_schema = @azimuth.TelemetrySchema {
    required_fields : ["timestamp", "trace_id", "span_id", "operation_name"],
    optional_fields : ["parent_span_id", "attributes", "events"],
    field_types : [
      ("timestamp", "long"),
      ("trace_id", "string"),
      ("span_id", "string"),
      ("operation_name", "string"),
      ("duration_ms", "long"),
      ("status", "enum")
    ],
    attribute_constraints : [
      ("http.method", @azimuth.StringEnum(["GET", "POST", "PUT", "DELETE"])),
      ("http.status_code", @azimuth.IntRange(100, 599)),
      ("db.type", @azimuth.StringEnum(["postgresql", "mysql", "mongodb"]))
    ]
  }
  
  // 创建符合模式的遥测数据
  let valid_data = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "abcdef1234567890abcdef1234567890",
    span_id : "1234567890abcdef",
    parent_span_id : Some("abcdef1234567890"),
    operation_name : "http.request",
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 150L,
    attributes : [
      ("http.method", @azimuth.StringValue("GET")),
      ("http.status_code", @azimuth.IntValue(200)),
      ("db.type", @azimuth.StringValue("postgresql"))
    ],
    events : []
  }
  
  // 验证符合模式的数据
  let validation_result = @azimuth.validate_telemetry_schema(valid_data, valid_schema)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // 创建不符合模式的遥测数据
  let invalid_data = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "abcdef1234567890abcdef1234567890",
    span_id : "1234567890abcdef",
    parent_span_id : Some("abcdef1234567890"),
    operation_name : "http.request",
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 150L,
    attributes : [
      ("http.method", @azimuth.StringValue("PATCH")), // 不在允许的方法列表中
      ("http.status_code", @azimuth.IntValue(999)), // 超出状态码范围
      ("db.type", @azimuth.StringValue("oracle")) // 不在允许的数据库类型中
    ],
    events : []
  }
  
  // 验证不符合模式的数据
  let invalid_validation_result = @azimuth.validate_telemetry_schema(invalid_data, valid_schema)
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() >= 3)
  
  // 验证错误消息包含相关信息
  let error_messages = invalid_validation_result.errors.map(fn(err) { err.message })
  assert_true(error_messages.some(fn(msg) { msg.contains("http.method") }))
  assert_true(error_messages.some(fn(msg) { msg.contains("http.status_code") }))
  assert_true(error_messages.some(fn(msg) { msg.contains("db.type") }))
}

test "telemetry data normalization" {
  // 测试遥测数据标准化
  let raw_data = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "ABCDEF1234567890ABCDEF1234567890", // 大写
    span_id : "1234567890ABCDEF", // 大写
    parent_span_id : Some("ABCDEF1234567890"), // 大写
    operation_name : "HTTP/GET", // 非标准格式
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 150, // 整数而非长整型
    attributes : [
      ("HTTP.METHOD", @azimuth.StringValue("GET")), // 大写键
      ("HTTP.URL", @azimuth.StringValue("/API/USERS")), // 大写路径
      ("HTTP.STATUS_CODE", @azimuth.IntValue(200)), // 大写键
      ("TIMESTAMP", @azimuth.StringValue("2022-01-01T00:00:00Z")) // 字符串时间戳
    ],
    events : [
      @azimuth.SpanEvent {
        name : "CACHE/HIT", // 非标准格式
        timestamp : 1640995200050L,
        attributes : [("CACHE.KEY", @azimuth.StringValue("USER:123"))] // 大写键
      }
    ]
  }
  
  // 标准化数据
  let normalized = @azimuth.normalize_telemetry_data(raw_data)
  
  // 验证trace_id和span_id转换为小写
  assert_eq(normalized.trace_id, "abcdef1234567890abcdef1234567890")
  assert_eq(normalized.span_id, "1234567890abcdef")
  match normalized.parent_span_id {
    Some(id) => assert_eq(id, "abcdef1234567890")
    None => assert_true(false)
  }
  
  // 验证操作名称标准化
  assert_eq(normalized.operation_name, "http.get")
  
  // 验证duration_ms转换为长整型
  assert_eq(normalized.duration_ms, 150L)
  
  // 验证属性键转换为小写
  let normalized_attrs = normalized.attributes.to_map()
  assert_true(normalized_attrs.contains("http.method"))
  assert_true(normalized_attrs.contains("http.url"))
  assert_true(normalized_attrs.contains("http.status_code"))
  
  // 验证属性值标准化
  match normalized_attrs.get("http.url") {
    Some(@azimuth.StringValue(url)) => assert_eq(url, "/api/users")
    _ => assert_true(false)
  }
  
  // 验证时间戳字符串转换为长整型
  match normalized_attrs.get("timestamp") {
    Some(@azimuth.IntValue(ts)) => assert_eq(ts, 1640995200000L)
    _ => assert_true(false)
  }
  
  // 验证事件名称标准化
  assert_eq(normalized.events[0].name, "cache.hit")
  
  // 验证事件属性键标准化
  let event_attrs = normalized.events[0].attributes.to_map()
  assert_true(event_attrs.contains("cache.key"))
}