// Logs API高级测试用例
test "log_record_builder_fluent_api" {
  // 测试LogRecordBuilder的流式API
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Database connection failed")
    .with_attribute("error.code", AttributeValue::string("DB_CONN_FAILED"))
    .with_attribute("error.retries", AttributeValue::int(3L))
    .with_attribute("service.name", AttributeValue::string("payment-service"))
    .with_attribute("service.version", AttributeValue::string("2.1.0"))
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.severity_number, Error)
  match log_record.body {
    Some(body) => assert_eq(body, "Database connection failed")
    None => @test.fail("Test failed")
  }
  assert_eq(log_record.attributes.length(), 4)
  
  // 验证属性
  assert_eq(log_record.attributes[0].0, "error.code")
  match log_record.attributes[0].1 {
    StringValue(code) => assert_eq(code, "DB_CONN_FAILED")
    _ => @test.fail("Test failed")
  }
  
  assert_eq(log_record.attributes[1].0, "error.retries")
  match log_record.attributes[1].1 {
    IntValue(retries) => assert_eq(retries, 3L)
    _ => @test.fail("Test failed")
  }
}

test "log_record_with_trace_context" {
  // 测试带有追踪上下文的LogRecord
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x0a_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x0b_byte })
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Warn,
    severity_text: Some("WARNING"),
    body: Some("Cache miss for user profile"),
    attributes: [
      ("cache.key", AttributeValue::string("user:12345:profile")),
      ("cache.ttl", AttributeValue::int(3600L)),
      ("user.id", AttributeValue::string("user-12345"))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: Some(Resource::default("user-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "user-cache",
      version: Some("1.2.0"),
      schema_url: Some("https://example.com/schema")
    })
  }
  
  assert_eq(log_record.severity_number, Warn)
  match log_record.severity_text {
    Some(text) => assert_eq(text, "WARNING")
    None => @test.fail("Test failed")
  }
  
  match log_record.body {
    Some(body) => assert_eq(body, "Cache miss for user profile")
    None => @test.fail("Test failed")
  }
  
  match log_record.trace_id {
    Some(id) => {
      assert_eq(id.length(), 16)
      assert_eq(id[0], 0x0a_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.span_id {
    Some(id) => {
      assert_eq(id.length(), 8)
      assert_eq(id[0], 0x0b_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Test failed")
  }
  
  match log_record.resource {
    Some(resource) => {
      assert_eq(resource.service_name, "user-service")
      assert_eq(resource.telemetry_sdk_name, "azimuth")
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "user-cache")
      match scope.version {
        Some(version) => assert_eq(version, "1.2.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}