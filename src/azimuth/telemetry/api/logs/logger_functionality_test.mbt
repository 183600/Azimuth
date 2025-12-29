// API Logs模块的日志记录器测试

test "log_record_builder_basic" {
  // 测试LogRecordBuilder的基本功能
  
  let builder = logs::LogRecord::builder()
  
  // 验证初始状态
  assert_eq(builder.timestamp_unix_nanos, None)
  assert_eq(builder.severity_number, None)
  assert_eq(builder.body, None)
  assert_eq(builder.attributes.length(), 0)
  
  // 设置时间戳
  let timestamp = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  let builder_with_timestamp = builder.timestamp(timestamp)
  
  match builder_with_timestamp.timestamp_unix_nanos {
    Some(ts) => assert_eq(ts, timestamp)
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 设置严重性级别
  let builder_with_severity = builder_with_timestamp.severity(logs::Info)
  
  match builder_with_severity.severity_number {
    Some(sev) => assert_eq(sev, logs::Info)
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 设置消息体
  let message = "User authenticated successfully"
  let builder_with_body = builder_with_severity.body(message)
  
  match builder_with_body.body {
    Some(msg) => assert_eq(msg, message)
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 构建LogRecord
  let log_record = builder_with_body.build()
  
  assert_eq(log_record.timestamp_unix_nanos, timestamp)
  assert_eq(log_record.severity_number, logs::Info)
  match log_record.body {
    Some(msg) => assert_eq(msg, message)
    None => assert_eq(false, true)  // 不应该到达这里
  }
}

test "log_record_builder_with_attributes" {
  // 测试LogRecordBuilder的属性功能
  
  let builder = logs::LogRecord::builder()
  
  // 添加属性
  let builder_with_attr1 = builder.with_attribute(
    "http.method", 
    common::AttributeValue::string("GET")
  )
  
  let builder_with_attr2 = builder_with_attr1.with_attribute(
    "http.status_code", 
    common::AttributeValue::int(200L)
  )
  
  let builder_with_attr3 = builder_with_attr2.with_attribute(
    "http.success", 
    common::AttributeValue::bool(true)
  )
  
  let builder_with_attr4 = builder_with_attr3.with_attribute(
    "http.duration", 
    common::AttributeValue::float(123.45)
  )
  
  // 构建LogRecord
  let log_record = builder_with_attr4.build()
  
  // 验证属性数量
  assert_eq(log_record.attributes.length(), 4)
  
  // 验证各个属性
  assert_eq(log_record.attributes[0].0, "http.method")
  match log_record.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "GET")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(log_record.attributes[1].0, "http.status_code")
  match log_record.attributes[1].1 {
    common::IntValue(i) => assert_eq(i, 200L)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(log_record.attributes[2].0, "http.success")
  match log_record.attributes[2].1 {
    common::BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(log_record.attributes[3].0, "http.duration")
  match log_record.attributes[3].1 {
    common::FloatValue(f) => assert_eq(f, 123.45)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "log_record_severity_levels" {
  // 测试日志记录的严重性级别
  
  let base_timestamp = 1640995200000000000L
  let message = "Test log message"
  
  // 测试所有严重性级别
  let trace_record = logs::LogRecord::builder()
    .timestamp(base_timestamp)
    .severity(logs::Trace)
    .body(message)
    .build()
  
  assert_eq(trace_record.severity_number, logs::Trace)
  
  let debug_record = logs::LogRecord::builder()
    .timestamp(base_timestamp)
    .severity(logs::Debug)
    .body(message)
    .build()
  
  assert_eq(debug_record.severity_number, logs::Debug)
  
  let info_record = logs::LogRecord::builder()
    .timestamp(base_timestamp)
    .severity(logs::Info)
    .body(message)
    .build()
  
  assert_eq(info_record.severity_number, logs::Info)
  
  let warn_record = logs::LogRecord::builder()
    .timestamp(base_timestamp)
    .severity(logs::Warn)
    .body(message)
    .build()
  
  assert_eq(warn_record.severity_number, logs::Warn)
  
  let error_record = logs::LogRecord::builder()
    .timestamp(base_timestamp)
    .severity(logs::Error)
    .body(message)
    .build()
  
  assert_eq(error_record.severity_number, logs::Error)
  
  let fatal_record = logs::LogRecord::builder()
    .timestamp(base_timestamp)
    .severity(logs::Fatal)
    .body(message)
    .build()
  
  assert_eq(fatal_record.severity_number, logs::Fatal)
}

test "log_record_complex_attributes" {
  // 测试复杂属性类型
  
  let builder = logs::LogRecord::builder()
  
  // 添加数组属性
  let string_array = ["GET", "POST", "PUT"]
  let builder_with_str_array = builder.with_attribute(
    "http.methods",
    common::AttributeValue::array_string(string_array)
  )
  
  let int_array = [200L, 404L, 500L]
  let builder_with_int_array = builder_with_str_array.with_attribute(
    "http.status_codes",
    common::AttributeValue::array_int(int_array)
  )
  
  let float_array = [1.1, 2.2, 3.3]
  let builder_with_float_array = builder_with_int_array.with_attribute(
    "response.times",
    common::AttributeValue::array_float(float_array)
  )
  
  let bool_array = [true, false, true]
  let builder_with_bool_array = builder_with_float_array.with_attribute(
    "feature.flags",
    common::AttributeValue::array_bool(bool_array)
  )
  
  // 构建LogRecord
  let log_record = builder_with_bool_array.build()
  
  // 验证数组属性
  assert_eq(log_record.attributes.length(), 4)
  
  // 验证字符串数组
  assert_eq(log_record.attributes[0].0, "http.methods")
  match log_record.attributes[0].1 {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "GET")
      assert_eq(arr[1], "POST")
      assert_eq(arr[2], "PUT")
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证整数数组
  assert_eq(log_record.attributes[1].0, "http.status_codes")
  match log_record.attributes[1].1 {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 200L)
      assert_eq(arr[1], 404L)
      assert_eq(arr[2], 500L)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证浮点数数组
  assert_eq(log_record.attributes[2].0, "response.times")
  match log_record.attributes[2].1 {
    common::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证布尔数组
  assert_eq(log_record.attributes[3].0, "feature.flags")
  match log_record.attributes[3].1 {
    common::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "noop_logger_functionality" {
  // 测试NoopLogger的功能
  
  let noop_logger = logs::NoopLogger::{}
  
  // 测试emit方法
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Test message")
    .build()
  
  // NoopLogger的emit方法应该什么都不做，但不应该崩溃
  noop_logger.emit(log_record)
  
  // 测试便捷方法
  noop_logger.debug("Debug message", [])
  noop_logger.info("Info message", [])
  noop_logger.warn("Warning message", [])
  noop_logger.error("Error message", [])
  noop_logger.fatal("Fatal message", [])
  
  // 测试带属性的便捷方法
  let attributes = [
    ("key1", common::AttributeValue::string("value1")),
    ("key2", common::AttributeValue::int(42L))
  ]
  
  noop_logger.debug("Debug with attributes", attributes)
  noop_logger.info("Info with attributes", attributes)
  noop_logger.warn("Warning with attributes", attributes)
  noop_logger.error("Error with attributes", attributes)
  noop_logger.fatal("Fatal with attributes", attributes)
}

test "noop_logger_provider_functionality" {
  // 测试NoopLoggerProvider的功能
  
  let provider = logs::NoopLoggerProvider::{}
  
  // 测试get_logger方法
  let logger1 = provider.get_logger("test.logger", None, None)
  let logger2 = provider.get_logger("test.logger", Some("1.0.0"), None)
  let logger3 = provider.get_logger("test.logger", None, Some("https://example.com/schema"))
  let logger4 = provider.get_logger("test.logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 所有返回的都应该是NoopLogger实例
  // 由于MoonBit的类型系统，我们无法直接测试类型，但可以测试行为
  
  // 测试所有logger都能调用方法而不崩溃
  logger1.debug("Message from logger1", [])
  logger2.info("Message from logger2", [])
  logger3.warn("Message from logger3", [])
  logger4.error("Message from logger4", [])
}

test "log_record_with_trace_context" {
  // 测试带有追踪上下文的日志记录
  
  // 创建模拟的trace_id和span_id
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })  // 16字节的trace_id
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })    // 8字节的span_id
  let trace_flags = 1_byte
  
  // 创建带有追踪上下文的LogRecord
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Operation completed")
    .build()
  
  // 由于当前实现中trace_id和span_id是只读的，我们创建一个新的LogRecord
  let log_record_with_trace = logs::LogRecord::{
    timestamp_unix_nanos: log_record.timestamp_unix_nanos,
    observed_timestamp_unix_nanos: log_record.observed_timestamp_unix_nanos,
    severity_number: log_record.severity_number,
    severity_text: log_record.severity_text,
    body: log_record.body,
    attributes: log_record.attributes,
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(trace_flags),
    resource: log_record.resource,
    instrumentation_scope: log_record.instrumentation_scope
  }
  
  // 验证追踪上下文
  match log_record_with_trace.trace_id {
    Some(tid) => {
      assert_eq(tid.length(), 16)
      assert_eq(tid[0], 1_byte)
    }
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  match log_record_with_trace.span_id {
    Some(sid) => {
      assert_eq(sid.length(), 8)
      assert_eq(sid[0], 2_byte)
    }
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  match log_record_with_trace.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => assert_eq(false, true)  // 不应该到达这里
  }
}

test "log_record_with_resource_and_scope" {
  // 测试带有资源和仪器化范围的日志记录
  
  // 创建资源
  let resource = common::Resource::{
    service_name: "test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.namespace", common::AttributeValue::string("test")),
      ("deployment.environment", common::AttributeValue::string("testing"))
    ]
  }
  
  // 创建仪器化范围
  let scope = common::InstrumentationScope::{
    name: "test.instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  // 创建带有资源和范围的LogRecord
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Test with resource and scope")
    .build()
  
  // 由于当前实现中resource和instrumentation_scope是只读的，我们创建一个新的LogRecord
  let log_record_with_context = logs::LogRecord::{
    timestamp_unix_nanos: log_record.timestamp_unix_nanos,
    observed_timestamp_unix_nanos: log_record.observed_timestamp_unix_nanos,
    severity_number: log_record.severity_number,
    severity_text: log_record.severity_text,
    body: log_record.body,
    attributes: log_record.attributes,
    trace_id: log_record.trace_id,
    span_id: log_record.span_id,
    trace_flags: log_record.trace_flags,
    resource: Some(resource),
    instrumentation_scope: Some(scope)
  }
  
  // 验证资源
  match log_record_with_context.resource {
    Some(res) => {
      assert_eq(res.service_name, "test-service")
      match res.service_version {
        Some(v) => assert_eq(v, "1.0.0")
        None => assert_eq(false, true)  // 不应该到达这里
      }
      assert_eq(res.telemetry_sdk_name, "azimuth")
      assert_eq(res.telemetry_sdk_version, "0.1.0")
      assert_eq(res.attributes.length(), 2)
    }
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证仪器化范围
  match log_record_with_context.instrumentation_scope {
    Some(sc) => {
      assert_eq(sc.name, "test.instrumentation")
      match sc.version {
        Some(v) => assert_eq(v, "1.0.0")
        None => assert_eq(false, true)  // 不应该到达这里
      }
      match sc.schema_url {
        Some(url) => assert_eq(url, "https://example.com/schema")
        None => assert_eq(false, true)  // 不应该到达这里
      }
    }
    None => assert_eq(false, true)  // 不应该到达这里
  }
}