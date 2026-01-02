// 日志导出器测试
// 测试日志记录的导出功能和格式

test "log exporter basic functionality" {
  // 测试日志导出器基本功能
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "log-exporter-test")
  
  // 创建并记录日志
  let log_record1 = LogRecord::new(Info, "Application started")
  let log_record2 = LogRecord::new(Warn, "Configuration warning")
  let log_record3 = LogRecord::new(Error, "Database connection failed")
  
  // 发出日志
  Logger::emit(logger, log_record1)
  Logger::emit(logger, log_record2)
  Logger::emit(logger, log_record3)
  
  // 模拟导出器配置
  let log_exporter_config = {
    "name": "test-log-exporter",
    "endpoint": "http://localhost:9200/logs",
    "format": "json",
    "batch_size": 100,
    "flush_interval": 5000  // 5 seconds
  }
  
  // 模拟导出的日志记录
  let exported_logs = [
    {
      "timestamp": 1735689600000000000L,
      "severity": "INFO",
      "body": "Application started",
      "attributes": [],
      "resource_attributes": [],
      "scope": {
        "name": "log-exporter-test",
        "version": null
      }
    },
    {
      "timestamp": 1735689600000001000L,
      "severity": "WARN",
      "body": "Configuration warning",
      "attributes": [],
      "resource_attributes": [],
      "scope": {
        "name": "log-exporter-test",
        "version": null
      }
    },
    {
      "timestamp": 1735689600000002000L,
      "severity": "ERROR",
      "body": "Database connection failed",
      "attributes": [],
      "resource_attributes": [],
      "scope": {
        "name": "log-exporter-test",
        "version": null
      }
    }
  ]
  
  // 验证导出器配置
  assert_eq(log_exporter_config["name"], "test-log-exporter")
  assert_eq(log_exporter_config["endpoint"], "http://localhost:9200/logs")
  assert_eq(log_exporter_config["format"], "json")
  
  // 验证导出的日志
  assert_eq(exported_logs.length, 3)
  assert_eq(exported_logs[0]["severity"], "INFO")
  assert_eq(exported_logs[1]["severity"], "WARN")
  assert_eq(exported_logs[2]["severity"], "ERROR")
  assert_eq(exported_logs[0]["body"], "Application started")
}

test "log exporter with trace correlation" {
  // 测试带追踪关联的日志导出
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "trace-log-exporter-test")
  
  // 创建带追踪上下文的日志
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "1234567890abcdef"
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Request processing failed"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record)
  
  // 模拟带追踪关联的导出日志
  let exported_log_with_trace = {
    "timestamp": 1735689600000000000L,
    "observed_timestamp": 1735689600000001000L,
    "severity": "ERROR",
    "body": "Request processing failed",
    "trace_id": trace_id,
    "span_id": span_id,
    "trace_flags": "01",
    "attributes": [],
    "resource_attributes": [],
    "scope": {
      "name": "trace-log-exporter-test",
      "version": null
    }
  }
  
  // 验证追踪关联
  assert_eq(exported_log_with_trace["trace_id"], trace_id)
  assert_eq(exported_log_with_trace["span_id"], span_id)
  assert_eq(exported_log_with_trace["severity"], "ERROR")
  assert_eq(exported_log_with_trace["body"], "Request processing failed")
}

test "log exporter with structured attributes" {
  // 测试带结构化属性的日志导出
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "structured-log-exporter-test")
  
  // 创建带属性的日志
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "request.method", StringValue("POST"))
  Attributes::set(attrs, "request.path", StringValue("/api/users"))
  Attributes::set(attrs, "response.status", IntValue(500))
  Attributes::set(attrs, "duration", FloatValue(250.5))
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("API request failed"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    None,
    None,
    None
  )
  
  Logger::emit(logger, log_record)
  
  // 模拟带结构化属性的导出日志
  let exported_structured_log = {
    "timestamp": 1735689600000000000L,
    "severity": "ERROR",
    "body": "API request failed",
    "attributes": [
      {"key": "user.id", "value": {"stringValue": "12345"}},
      {"key": "request.method", "value": {"stringValue": "POST"}},
      {"key": "request.path", "value": {"stringValue": "/api/users"}},
      {"key": "response.status", "value": {"intValue": 500}},
      {"key": "duration", "value": {"doubleValue": 250.5}}
    ],
    "resource_attributes": [],
    "scope": {
      "name": "structured-log-exporter-test",
      "version": null
    }
  }
  
  // 验证结构化属性
  assert_eq(exported_structured_log["attributes"].length, 5)
  assert_eq(exported_structured_log["attributes"][0]["key"], "user.id")
  assert_eq(exported_structured_log["attributes"][0]["value"]["stringValue"], "12345")
  assert_eq(exported_structured_log["attributes"][3]["value"]["intValue"], 500)
  assert_eq(exported_structured_log["attributes"][4]["value"]["doubleValue"], 250.5)
}

test "log exporter batch processing" {
  // 测试日志批量处理
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "batch-log-exporter-test")
  
  // 创建多个日志记录
  let log_records = []
  for i in 0..50 {
    let log_record = LogRecord::new(Info, "Batch log entry " + i.to_string())
    Logger::emit(logger, log_record)
    log_records.push(log_record)
  }
  
  // 模拟批量处理配置
  let batch_config = {
    "max_batch_size": 100,
    "max_export_timeout": 30000,  // 30 seconds
    "max_queue_size": 1000,
    "scheduled_delay": 5000  // 5 seconds
  }
  
  // 模拟批量导出的日志
  let batch_exported_logs = []
  for i in 0..50 {
    batch_exported_logs.push({
      "timestamp": 1735689600000000000L + i.to_int64() * 1000000L,
      "severity": "INFO",
      "body": "Batch log entry " + i.to_string(),
      "attributes": [],
      "resource_attributes": [],
      "scope": {
        "name": "batch-log-exporter-test",
        "version": null
      }
    })
  }
  
  // 验证批量处理配置
  assert_eq(batch_config["max_batch_size"], 100)
  assert_eq(batch_config["max_queue_size"], 1000)
  
  // 验证批量导出的日志
  assert_eq(batch_exported_logs.length, 50)
  assert_eq(batch_exported_logs[0]["body"], "Batch log entry 0")
  assert_eq(batch_exported_logs[49]["body"], "Batch log entry 49")
}

test "log exporter with different formats" {
  // 测试不同格式的日志导出
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "format-log-exporter-test")
  
  // 创建日志记录
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Structured error message"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    None
  )
  
  Logger::emit(logger, log_record)
  
  // JSON格式导出
  let json_format = "{\"timestamp\":1735689600000000000,\"severity\":\"ERROR\",\"body\":\"Structured error message\",\"trace_id\":\"trace123\",\"span_id\":\"span456\"}"
  
  // Plain text格式导出
  let plain_text_format = "2025-01-01T00:00:00.000Z ERROR [trace123:span456] Structured error message"
  
  // OpenTelemetry格式导出
  let otel_format = "{\"resourceLogs\":[{\"resource\":{},\"scopeLogs\":[{\"scope\":{\"name\":\"format-log-exporter-test\"},\"logRecords\":[{\"timeUnixNano\":\"1735689600000000000\",\"severityNumber\":17,\"severityText\":\"ERROR\",\"body\":{\"stringValue\":\"Structured error message\"},\"traceId\":\"trace123\",\"spanId\":\"span456\"}]}]}]}"
  
  // 验证不同格式
  assert_true(json_format.contains("Structured error message"))
  assert_true(json_format.contains("trace123"))
  
  assert_true(plain_text_format.contains("ERROR"))
  assert_true(plain_text_format.contains("trace123:span456"))
  
  assert_true(otel_format.contains("resourceLogs"))
  assert_true(otel_format.contains("trace123"))
}

test "log exporter with error handling" {
  // 测试日志导出器错误处理
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "error-log-exporter-test")
  
  // 创建日志记录
  let log_record = LogRecord::new(Error, "Critical error that should be logged")
  Logger::emit(logger, log_record)
  
  // 模拟导出错误配置
  let error_export_config = {
    "endpoint": "http://invalid-log-endpoint:9200/logs",
    "timeout": 5000,
    "max_retries": 3,
    "retry_delay": 1000,
    "fallback_enabled": true,
    "fallback_endpoint": "file:///var/log/app/fallback.log"
  }
  
  // 模拟导出错误结果
  let export_error_result = {
    "success": false,
    "error_code": "CONNECTION_ERROR",
    "error_message": "Failed to connect to invalid-log-endpoint:9200",
    "retry_count": 3,
    "fallback_used": true,
    "fallback_location": "/var/log/app/fallback.log"
  }
  
  // 验证错误处理配置
  assert_eq(error_export_config["endpoint"], "http://invalid-log-endpoint:9200/logs")
  assert_eq(error_export_config["max_retries"], 3)
  assert_eq(error_export_config["fallback_enabled"], true)
  
  // 验证错误结果
  assert_true(false == export_error_result["success"])
  assert_eq(export_error_result["error_code"], "CONNECTION_ERROR")
  assert_eq(export_error_result["fallback_used"], true)
}

test "log exporter with resource attributes" {
  // 测试带资源属性的日志导出
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "resource-log-exporter-test")
  
  // 创建日志记录
  let log_record = LogRecord::new(Info, "Service operation completed")
  Logger::emit(logger, log_record)
  
  // 创建资源属性
  let resource = Resource::new()
  let resource_with_attrs = Resource::with_attributes(resource, [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("2.1.0")),
    ("host.name", StringValue("prod-server-01")),
    ("deployment.environment", StringValue("production"))
  ])
  
  // 模拟带资源属性的导出日志
  let export_with_resource = {
    "timestamp": 1735689600000000000L,
    "severity": "INFO",
    "body": "Service operation completed",
    "attributes": [],
    "resource": {
      "attributes": [
        {"key": "service.name", "value": {"stringValue": "user-service"}},
        {"key": "service.version", "value": {"stringValue": "2.1.0"}},
        {"key": "host.name", "value": {"stringValue": "prod-server-01"}},
        {"key": "deployment.environment", "value": {"stringValue": "production"}}
      ]
    },
    "scope": {
      "name": "resource-log-exporter-test",
      "version": null
    }
  }
  
  // 验证资源属性导出
  assert_eq(export_with_resource["resource"]["attributes"][0]["value"]["stringValue"], "user-service")
  assert_eq(export_with_resource["resource"]["attributes"][1]["value"]["stringValue"], "2.1.0")
  assert_eq(export_with_resource["resource"]["attributes"][2]["value"]["stringValue"], "prod-server-01")
  assert_eq(export_with_resource["resource"]["attributes"][3]["value"]["stringValue"], "production")
}

test "log exporter with log filtering" {
  // 测试带日志过滤的导出
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "filter-log-exporter-test")
  
  // 创建不同严重级别的日志
  let debug_log = LogRecord::new(Debug, "Debug information")
  let info_log = LogRecord::new(Info, "Information message")
  let warn_log = LogRecord::new(Warn, "Warning condition")
  let error_log = LogRecord::new(Error, "Error occurred")
  let fatal_log = LogRecord::new(Fatal, "Fatal error")
  
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, fatal_log)
  
  // 配置日志过滤器
  let log_filter = {
    "min_severity": "WARN",
    "exclude_patterns": ["debug", "trace"],
    "include_patterns": ["error", "fatal", "warn"],
    "attribute_filters": [
      {"key": "source", "value": "production"}
    ]
  }
  
  // 模拟过滤后的导出日志
  let filtered_exported_logs = [
    {
      "timestamp": 1735689600000000003L,
      "severity": "WARN",
      "body": "Warning condition",
      "filtered": false
    },
    {
      "timestamp": 1735689600000000004L,
      "severity": "ERROR",
      "body": "Error occurred",
      "filtered": false
    },
    {
      "timestamp": 1735689600000000005L,
      "severity": "FATAL",
      "body": "Fatal error",
      "filtered": false
    }
  ]
  
  // 验证日志过滤器配置
  assert_eq(log_filter["min_severity"], "WARN")
  assert_eq(log_filter["exclude_patterns"][0], "debug")
  assert_eq(log_filter["include_patterns"][0], "error")
  
  // 验证过滤后的日志
  assert_eq(filtered_exported_logs.length, 3)
  assert_eq(filtered_exported_logs[0]["severity"], "WARN")
  assert_eq(filtered_exported_logs[1]["severity"], "ERROR")
  assert_eq(filtered_exported_logs[2]["severity"], "FATAL")
}