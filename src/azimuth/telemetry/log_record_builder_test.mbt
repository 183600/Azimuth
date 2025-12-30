// 日志记录构建器测试用例
// 测试日志记录构建器的各种功能和链式调用

test "log_record_builder_basic_usage" {
  // 测试日志记录构建器的基本用法
  
  let builder = @azimuth.telemetry.api.logs.LogRecord::builder()
  
  // 构建基本的日志记录
  let log_record = builder
    .timestamp(1640995200000000000L) // 2022-01-01 00:00:00 UTC
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Application started successfully")
    .build()
  
  // 验证基本字段
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.severity_number, @azimuth.telemetry.api.logs.Info)
  assert_eq(log_record.body.unwrap_or(""), "Application started successfully")
  assert_eq(log_record.attributes.length(), 0)
}

test "log_record_builder_with_attributes" {
  // 测试带属性的日志记录构建
  
  let builder = @azimuth.telemetry.api.logs.LogRecord::builder()
  
  // 添加多个属性
  let log_record = builder
    .timestamp(1640995300000000000L)
    .severity(@azimuth.telemetry.api.logs.Error)
    .body("Database connection failed")
    .with_attribute("service.name", @azimuth.telemetry.api.common.AttributeValue::string("user-service"))
    .with_attribute("service.version", @azimuth.telemetry.api.common.AttributeValue::string("1.2.3"))
    .with_attribute("error.code", @azimuth.telemetry.api.common.AttributeValue::int(500L))
    .with_attribute("error.type", @azimuth.telemetry.api.common.AttributeValue::string("connection_timeout"))
    .with_attribute("retry.count", @azimuth.telemetry.api.common.AttributeValue::int(3L))
    .build()
  
  // 验证属性数量
  assert_eq(log_record.attributes.length(), 5)
  
  // 验证特定属性
  let mut found_service_name = false
  let mut found_error_code = false
  let mut found_retry_count = false
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "service.name" => {
        match value {
          @azimuth.telemetry.api.common.StringValue(v) => {
            assert_eq(v, "user-service")
            found_service_name = true
          }
          _ => assert_eq(false, true)
        }
      }
      "error.code" => {
        match value {
          @azimuth.telemetry.api.common.IntValue(v) => {
            assert_eq(v, 500L)
            found_error_code = true
          }
          _ => assert_eq(false, true)
        }
      }
      "retry.count" => {
        match value {
          @azimuth.telemetry.api.common.IntValue(v) => {
            assert_eq(v, 3L)
            found_retry_count = true
          }
          _ => assert_eq(false, true)
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  // 确保所有关键属性都找到了
  assert_eq(found_service_name, true)
  assert_eq(found_error_code, true)
  assert_eq(found_retry_count, true)
}

test "log_record_builder_severity_levels" {
  // 测试不同严重性级别的日志记录
  
  let severity_levels = [
    (@azimuth.telemetry.api.logs.Trace, "Detailed trace information"),
    (@azimuth.telemetry.api.logs.Debug, "Debug message for developers"),
    (@azimuth.telemetry.api.logs.Info, "Informational message"),
    (@azimuth.telemetry.api.logs.Warn, "Warning condition"),
    (@azimuth.telemetry.api.logs.Error, "Error occurred"),
    (@azimuth.telemetry.api.logs.Fatal, "Fatal error occurred")
  ]
  
  let mut i = 0
  while i < severity_levels.length() {
    let (severity, message) = severity_levels[i]
    
    let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
      .timestamp(1640995400000000000L + i.to_int64() * 1000000000L)
      .severity(severity)
      .body(message)
      .with_attribute("log.index", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64()))
      .build()
    
    // 验证严重性级别
    assert_eq(log_record.severity_number, severity)
    assert_eq(log_record.body.unwrap_or(""), message)
    
    // 验证索引属性
    assert_eq(log_record.attributes.length(), 1)
    match log_record.attributes[0] {
      ("log.index", @azimuth.telemetry.api.common.IntValue(index)) => {
        assert_eq(index, i.to_int64())
      }
      _ => assert_eq(false, true)
    }
    
    i = i + 1
  }
  
  // 验证所有严重性级别都已测试
  assert_eq(severity_levels.length(), 6)
}

test "log_record_builder_with_trace_context" {
  // 测试带追踪上下文的日志记录
  
  // 模拟trace_id和span_id
  let trace_id = [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                  0x89_byte, 0xab_byte, 0xcd_byte, 0xef_byte,
                  0xfe_byte, 0xdc_byte, 0xba_byte, 0x98_byte,
                  0x76_byte, 0x54_byte, 0x32_byte, 0x10_byte]
  
  let span_id = [0x11_byte, 0x22_byte, 0x33_byte, 0x44_byte,
                 0x55_byte, 0x66_byte, 0x77_byte, 0x88_byte]
  
  let builder = @azimuth.telemetry.api.logs.LogRecord::builder()
  
  // 注意：由于当前API的限制，我们无法直接设置trace_id和span_id
  // 但我们可以测试其他字段
  let log_record = builder
    .timestamp(1640995500000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Processing user request")
    .with_attribute("trace.id.hex", @azimuth.telemetry.api.common.AttributeValue::string("0123456789abcdeffedcba9876543210"))
    .with_attribute("span.id.hex", @azimuth.telemetry.api.common.AttributeValue::string("1122334455667788"))
    .with_attribute("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-12345"))
    .with_attribute("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-67890"))
    .build()
  
  // 验证追踪相关的属性
  assert_eq(log_record.attributes.length(), 4)
  
  let mut found_trace_id = false
  let mut found_span_id = false
  let mut found_user_id = false
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "trace.id.hex" => {
        match value {
          @azimuth.telemetry.api.common.StringValue(v) => {
            assert_eq(v.length(), 32)
            assert_eq(v.has_prefix("0123"), true)
            found_trace_id = true
          }
          _ => assert_eq(false, true)
        }
      }
      "span.id.hex" => {
        match value {
          @azimuth.telemetry.api.common.StringValue(v) => {
            assert_eq(v.length(), 16)
            assert_eq(v.has_prefix("1122"), true)
            found_span_id = true
          }
          _ => assert_eq(false, true)
        }
      }
      "user.id" => {
        match value {
          @azimuth.telemetry.api.common.StringValue(v) => {
            assert_eq(v, "user-12345")
            found_user_id = true
          }
          _ => assert_eq(false, true)
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_trace_id, true)
  assert_eq(found_span_id, true)
  assert_eq(found_user_id, true)
}

test "log_record_builder_complex_attributes" {
  // 测试复杂属性类型的日志记录
  
  let builder = @azimuth.telemetry.api.logs.LogRecord::builder()
  
  // 创建包含复杂属性的日志记录
  let log_record = builder
    .timestamp(1640995600000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Complex operation completed")
    .with_attribute("operation.name", @azimuth.telemetry.api.common.AttributeValue::string("batch_processing"))
    .with_attribute("operation.duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(5432L))
    .with_attribute("operation.success", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    .with_attribute("operation.throughput", @azimuth.telemetry.api.common.AttributeValue::float(1234.5))
    .with_attribute("processed.items", @azimuth.telemetry.api.common.AttributeValue::array_int([100L, 200L, 300L, 400L, 500L]))
    .with_attribute("processed.tags", @azimuth.telemetry.api.common.AttributeValue::array_string(["tag1", "tag2", "tag3"]))
    .with_attribute("validation.results", @azimuth.telemetry.api.common.AttributeValue::array_bool([true, true, false, true]))
    .build()
  
  // 验证复杂属性
  assert_eq(log_record.attributes.length(), 7)
  
  let mut found_array_int = false
  let mut found_array_string = false
  let mut found_array_bool = false
  let mut found_float_value = false
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "processed.items" => {
        match value {
          @azimuth.telemetry.api.common.ArrayIntValue(items) => {
            assert_eq(items.length(), 5)
            assert_eq(items[0], 100L)
            assert_eq(items[4], 500L)
            found_array_int = true
          }
          _ => assert_eq(false, true)
        }
      }
      "processed.tags" => {
        match value {
          @azimuth.telemetry.api.common.ArrayStringValue(tags) => {
            assert_eq(tags.length(), 3)
            assert_eq(tags[0], "tag1")
            assert_eq(tags[2], "tag3")
            found_array_string = true
          }
          _ => assert_eq(false, true)
        }
      }
      "validation.results" => {
        match value {
          @azimuth.telemetry.api.common.ArrayBoolValue(results) => {
            assert_eq(results.length(), 4)
            assert_eq(results[0], true)
            assert_eq(results[2], false)
            found_array_bool = true
          }
          _ => assert_eq(false, true)
        }
      }
      "operation.throughput" => {
        match value {
          @azimuth.telemetry.api.common.FloatValue(throughput) => {
            assert_eq(throughput > 1000.0, true)
            assert_eq(throughput < 2000.0, true)
            found_float_value = true
          }
          _ => assert_eq(false, true)
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  // 确保所有复杂属性都找到了
  assert_eq(found_array_int, true)
  assert_eq(found_array_string, true)
  assert_eq(found_array_bool, true)
  assert_eq(found_float_value, true)
}

test "log_record_builder_chaining" {
  // 测试构建器的链式调用
  
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995700000000000L)
    .severity(@azimuth.telemetry.api.logs.Warn)
    .body("Chain of operations completed")
    .with_attribute("step.1", @azimuth.telemetry.api.common.AttributeValue::string("validation"))
    .with_attribute("step.2", @azimuth.telemetry.api.common.AttributeValue::string("processing"))
    .with_attribute("step.3", @azimuth.telemetry.api.common.AttributeValue::string("transformation"))
    .with_attribute("step.4", @azimuth.telemetry.api.common.AttributeValue::string("storage"))
    .with_attribute("total.steps", @azimuth.telemetry.api.common.AttributeValue::int(4L))
    .with_attribute("success.rate", @azimuth.telemetry.api.common.AttributeValue::float(0.95))
    .build()
  
  // 验证链式调用的结果
  assert_eq(log_record.severity_number, @azimuth.telemetry.api.logs.Warn)
  assert_eq(log_record.body.unwrap_or(""), "Chain of operations completed")
  assert_eq(log_record.attributes.length(), 7)
  
  // 验证步骤属性
  let mut step_count = 0
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    if key.has_prefix("step.") && key.length() == 6 {
      match value {
        @azimuth.telemetry.api.common.StringValue(step_name) => {
          assert_eq(step_name.length() > 0, true)
          step_count = step_count + 1
        }
        _ => assert_eq(false, true)
      }
    }
    
    i = i + 1
  }
  
  assert_eq(step_count, 4) // 应该有4个步骤
}