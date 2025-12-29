// 遥测数据验证测试用例

test "telemetry_data_schema_validation" {
  // 测试遥测数据模式验证
  
  let valid_telemetry_data = {
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331",
    "parent_span_id": "a1b2c3d4e5f67788",
    "operation_name": "http_request",
    "service_name": "api-gateway",
    "start_time": 1640995200L,
    "end_time": 1640995210L,
    "status_code": 200,
    "attributes": [
      ("http.method", AttributeValue::string("GET")),
      ("http.url", AttributeValue::string("/api/users")),
      ("http.status_code", AttributeValue::int(200))
    ]
  }
  
  let invalid_telemetry_data = {
    "trace_id": "", // 无效：空字符串
    "span_id": "invalid_span_id", // 无效：格式错误
    "parent_span_id": "a1b2c3d4e5f67788",
    "operation_name": "", // 无效：空字符串
    "service_name": "api-gateway",
    "start_time": -1L, // 无效：负数时间戳
    "end_time": 1640995210L,
    "status_code": 999, // 无效：超出范围
    "attributes": [
      ("http.method", AttributeValue::string("GET")),
      ("http.url", AttributeValue::string("/api/users"))
    ]
  }
  
  // 验证规则定义
  let validation_rules = [
    ("trace_id", "required", "32_char_hex"),
    ("span_id", "required", "16_char_hex"),
    ("parent_span_id", "optional", "16_char_hex"),
    ("operation_name", "required", "non_empty_string"),
    ("service_name", "required", "non_empty_string"),
    ("start_time", "required", "positive_timestamp"),
    ("end_time", "required", "positive_timestamp"),
    ("status_code", "required", "valid_http_status")
  ]
  
  // 验证有效数据
  let mut valid_errors = []
  let mut i = 0
  while i < validation_rules.length() {
    let (field_name, requirement, format) = validation_rules[i]
    let field_value = valid_telemetry_data[field_name]
    
    match field_name {
      "trace_id" => {
        if field_value.length() != 32 {
          valid_errors.push("trace_id must be 32 characters")
        }
      }
      "span_id" => {
        if field_value.length() != 16 {
          valid_errors.push("span_id must be 16 characters")
        }
      }
      "operation_name" => {
        if field_value.length() == 0 {
          valid_errors.push("operation_name cannot be empty")
        }
      }
      "service_name" => {
        if field_value.length() == 0 {
          valid_errors.push("service_name cannot be empty")
        }
      }
      "start_time" => {
        if field_value < 0L {
          valid_errors.push("start_time must be positive")
        }
      }
      "end_time" => {
        if field_value < 0L {
          valid_errors.push("end_time must be positive")
        }
      }
      "status_code" => {
        if field_value < 100 || field_value > 599 {
          valid_errors.push("status_code must be between 100-599")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  // 验证没有错误
  assert_eq(valid_errors.length(), 0)
  
  // 验证无效数据
  let mut invalid_errors = []
  i = 0
  while i < validation_rules.length() {
    let (field_name, requirement, format) = validation_rules[i]
    let field_value = invalid_telemetry_data[field_name]
    
    match field_name {
      "trace_id" => {
        if field_value.length() != 32 {
          invalid_errors.push("trace_id must be 32 characters")
        }
      }
      "span_id" => {
        if field_value.length() != 16 {
          invalid_errors.push("span_id must be 16 characters")
        }
      }
      "operation_name" => {
        if field_value.length() == 0 {
          invalid_errors.push("operation_name cannot be empty")
        }
      }
      "start_time" => {
        if field_value < 0L {
          invalid_errors.push("start_time must be positive")
        }
      }
      "status_code" => {
        if field_value < 100 || field_value > 599 {
          invalid_errors.push("status_code must be between 100-599")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  // 验证有错误
  assert_eq(invalid_errors.length(), 5)
  assert_eq(invalid_errors[0], "trace_id must be 32 characters")
  assert_eq(invalid_errors[1], "span_id must be 16 characters")
  assert_eq(invalid_errors[2], "operation_name cannot be empty")
  assert_eq(invalid_errors[3], "start_time must be positive")
  assert_eq(invalid_errors[4], "status_code must be between 100-599")
}

test "telemetry_attribute_validation" {
  // 测试遥测属性验证
  
  let valid_attributes = [
    ("string.attr", AttributeValue::string("valid_value")),
    ("int.attr", AttributeValue::int(42)),
    ("float.attr", AttributeValue::float(3.14159)),
    ("bool.attr", AttributeValue::bool(true)),
    ("array.string", AttributeValue::array_string(["a", "b", "c"])),
    ("array.int", AttributeValue::array_int([1, 2, 3])),
    ("array.float", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array.bool", AttributeValue::array_bool([true, false, true]))
  ]
  
  let invalid_attributes = [
    ("", AttributeValue::string("value")), // 无效：空键名
    ("invalid.chars!", AttributeValue::string("value")), // 无效：特殊字符
    ("string.attr", AttributeValue::string("")), // 无效：空字符串值
    ("int.attr", AttributeValue::int(-1)), // 无效：负整数
    ("float.attr", AttributeValue::float(-1.0)), // 无效：负浮点数
    ("bool.attr", AttributeValue::bool(true)), // 有效
    ("array.string", AttributeValue::array_string([])), // 无效：空数组
    ("array.int", AttributeValue::array_int([-1, 2, 3])) // 无效：包含负数
  ]
  
  // 属性验证规则
  let attribute_rules = [
    ("key_format", "^[a-zA-Z][a-zA-Z0-9_.]*$"), // 键名格式
    ("max_key_length", 64), // 最大键名长度
    ("max_string_length", 255), // 最大字符串长度
    ("min_int_value", 0), // 最小整数值
    ("min_float_value", 0.0), // 最小浮点数值
    ("min_array_length", 1), // 最小数组长度
    ("max_array_length", 100) // 最大数组长度
  ]
  
  // 验证有效属性
  let mut valid_attr_errors = []
  let mut i = 0
  while i < valid_attributes.length() {
    let (key, value) = valid_attributes[i]
    
    // 验证键名格式
    if key.length() == 0 || key.length() > 64 {
      valid_attr_errors.push("Invalid key length: " + key)
    }
    
    // 验证值
    match value {
      AttributeValue::StringValue(s) => {
        if s.length() > 255 {
          valid_attr_errors.push("String value too long: " + key)
        }
      }
      AttributeValue::IntValue(n) => {
        if n < 0 {
          valid_attr_errors.push("Negative int value: " + key)
        }
      }
      AttributeValue::FloatValue(f) => {
        if f < 0.0 {
          valid_attr_errors.push("Negative float value: " + key)
        }
      }
      AttributeValue::ArrayStringValue(arr) => {
        if arr.length() == 0 || arr.length() > 100 {
          valid_attr_errors.push("Invalid array length: " + key)
        }
      }
      AttributeValue::ArrayIntValue(arr) => {
        if arr.length() == 0 || arr.length() > 100 {
          valid_attr_errors.push("Invalid array length: " + key)
        }
        let mut j = 0
        while j < arr.length() {
          if arr[j] < 0 {
            valid_attr_errors.push("Negative int in array: " + key)
            break
          }
          j = j + 1
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  // 验证没有错误
  assert_eq(valid_attr_errors.length(), 0)
  
  // 验证无效属性
  let mut invalid_attr_errors = []
  i = 0
  while i < invalid_attributes.length() {
    let (key, value) = invalid_attributes[i]
    
    // 验证键名格式
    if key.length() == 0 {
      invalid_attr_errors.push("Empty key name")
    } else if key.has_prefix("!") || key.has_suffix("!") {
      invalid_attr_errors.push("Invalid characters in key: " + key)
    }
    
    // 验证值
    match value {
      AttributeValue::StringValue(s) => {
        if s.length() == 0 {
          invalid_attr_errors.push("Empty string value: " + key)
        }
      }
      AttributeValue::IntValue(n) => {
        if n < 0 {
          invalid_attr_errors.push("Negative int value: " + key)
        }
      }
      AttributeValue::FloatValue(f) => {
        if f < 0.0 {
          invalid_attr_errors.push("Negative float value: " + key)
        }
      }
      AttributeValue::ArrayStringValue(arr) => {
        if arr.length() == 0 {
          invalid_attr_errors.push("Empty array: " + key)
        }
      }
      AttributeValue::ArrayIntValue(arr) => {
        if arr.length() > 0 && arr[0] < 0 {
          invalid_attr_errors.push("Negative int in array: " + key)
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  // 验证有错误
  assert_eq(invalid_attr_errors.length(), 7)
  assert_eq(invalid_attr_errors[0], "Empty key name")
  assert_eq(invalid_attr_errors[1], "Invalid characters in key: invalid.chars!")
  assert_eq(invalid_attr_errors[2], "Empty string value: string.attr")
  assert_eq(invalid_attr_errors[3], "Negative int value: int.attr")
  assert_eq(invalid_attr_errors[4], "Negative float value: float.attr")
  assert_eq(invalid_attr_errors[5], "Empty array: array.string")
  assert_eq(invalid_attr_errors[6], "Negative int in array: array.int")
}

test "telemetry_metric_validation" {
  // 测试遥测指标验证
  
  let valid_metrics = [
    {
      "name": "http_requests_total",
      "value": 1234.5,
      "unit": "count",
      "type": "counter",
      "timestamp": 1640995200L,
      "attributes": [
        ("http.method", AttributeValue::string("GET")),
        ("http.status_code", AttributeValue::int(200))
      ]
    },
    {
      "name": "response_time_ms",
      "value": 125.7,
      "unit": "milliseconds",
      "type": "histogram",
      "timestamp": 1640995200L,
      "attributes": [
        ("endpoint", AttributeValue::string("/api/users"))
      ]
    },
    {
      "name": "active_connections",
      "value": 42.0,
      "unit": "connections",
      "type": "gauge",
      "timestamp": 1640995200L,
      "attributes": []
    }
  ]
  
  let invalid_metrics = [
    {
      "name": "", // 无效：空名称
      "value": 1234.5,
      "unit": "count",
      "type": "counter",
      "timestamp": 1640995200L,
      "attributes": []
    },
    {
      "name": "invalid.metric.name!", // 无效：特殊字符
      "value": -1.0, // 无效：负值
      "unit": "count",
      "type": "counter",
      "timestamp": -1L, // 无效：负时间戳
      "attributes": []
    },
    {
      "name": "valid_name",
      "value": "invalid_value", // 无效：字符串值
      "unit": "", // 无效：空单位
      "type": "invalid_type", // 无效：未知类型
      "timestamp": 1640995200L,
      "attributes": []
    }
  ]
  
  // 指标验证规则
  let metric_types = ["counter", "gauge", "histogram", "summary"]
  let metric_units = ["count", "milliseconds", "seconds", "bytes", "connections", "percent"]
  
  // 验证有效指标
  let mut valid_metric_errors = []
  let mut i = 0
  while i < valid_metrics.length() {
    let metric = valid_metrics[i]
    
    // 验证名称
    if metric["name"].length() == 0 {
      valid_metric_errors.push("Empty metric name")
    }
    
    // 验证值
    let metric_value = metric["value"]
    if metric_value < 0.0 {
      valid_metric_errors.push("Negative metric value: " + metric["name"])
    }
    
    // 验证单位
    let mut valid_unit = false
    let mut j = 0
    while j < metric_units.length() {
      if metric_units[j] == metric["unit"] {
        valid_unit = true
        break
      }
      j = j + 1
    }
    if not valid_unit {
      valid_metric_errors.push("Invalid metric unit: " + metric["unit"])
    }
    
    // 验证类型
    let mut valid_type = false
    j = 0
    while j < metric_types.length() {
      if metric_types[j] == metric["type"] {
        valid_type = true
        break
      }
      j = j + 1
    }
    if not valid_type {
      valid_metric_errors.push("Invalid metric type: " + metric["type"])
    }
    
    // 验证时间戳
    if metric["timestamp"] < 0L {
      valid_metric_errors.push("Invalid timestamp: " + metric["name"])
    }
    
    i = i + 1
  }
  
  // 验证没有错误
  assert_eq(valid_metric_errors.length(), 0)
  
  // 验证无效指标
  let mut invalid_metric_errors = []
  i = 0
  while i < invalid_metrics.length() {
    let metric = invalid_metrics[i]
    
    // 验证名称
    if metric["name"].length() == 0 {
      invalid_metric_errors.push("Empty metric name")
    } else if metric["name"].has_suffix("!") {
      invalid_metric_errors.push("Invalid characters in metric name")
    }
    
    // 验证值
    if metric["value"] < 0.0 {
      invalid_metric_errors.push("Negative metric value")
    }
    
    // 验证单位
    if metric["unit"].length() == 0 {
      invalid_metric_errors.push("Empty metric unit")
    }
    
    // 验证类型
    let mut valid_type = false
    let mut j = 0
    while j < metric_types.length() {
      if metric_types[j] == metric["type"] {
        valid_type = true
        break
      }
      j = j + 1
    }
    if not valid_type {
      invalid_metric_errors.push("Invalid metric type: " + metric["type"])
    }
    
    // 验证时间戳
    if metric["timestamp"] < 0L {
      invalid_metric_errors.push("Invalid timestamp")
    }
    
    i = i + 1
  }
  
  // 验证有错误
  assert_eq(invalid_metric_errors.length(), 8)
}

test "telemetry_log_validation" {
  // 测试遥测日志验证
  
  let valid_logs = [
    {
      "timestamp": 1640995200L,
      "severity": "INFO",
      "message": "User login successful",
      "attributes": [
        ("user.id", AttributeValue::string("12345")),
        ("ip.address", AttributeValue::string("192.168.1.100"))
      ]
    },
    {
      "timestamp": 1640995201L,
      "severity": "ERROR",
      "message": "Database connection failed",
      "attributes": [
        ("error.code", AttributeValue::int(500)),
        ("retry.count", AttributeValue::int(3))
      ]
    },
    {
      "timestamp": 1640995202L,
      "severity": "WARN",
      "message": "High memory usage detected",
      "attributes": [
        ("memory.usage", AttributeValue::float(0.85)),
        ("threshold", AttributeValue::float(0.8))
      ]
    }
  ]
  
  let invalid_logs = [
    {
      "timestamp": -1L, // 无效：负时间戳
      "severity": "INVALID", // 无效：未知严重级别
      "message": "", // 无效：空消息
      "attributes": []
    },
    {
      "timestamp": 1640995200L,
      "severity": "DEBUG",
      "message": "x".repeat(10001), // 无效：消息过长
      "attributes": []
    },
    {
      "timestamp": 1640995200L,
      "severity": "INFO",
      "message": "Valid message",
      "attributes": [
        ("", AttributeValue::string("value")) // 无效：空属性键
      ]
    }
  ]
  
  // 日志验证规则
  let log_severity_levels = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
  let max_message_length = 10000
  
  // 验证有效日志
  let mut valid_log_errors = []
  let mut i = 0
  while i < valid_logs.length() {
    let log = valid_logs[i]
    
    // 验证时间戳
    if log["timestamp"] < 0L {
      valid_log_errors.push("Invalid timestamp")
    }
    
    // 验证严重级别
    let mut valid_severity = false
    let mut j = 0
    while j < log_severity_levels.length() {
      if log_severity_levels[j] == log["severity"] {
        valid_severity = true
        break
      }
      j = j + 1
    }
    if not valid_severity {
      valid_log_errors.push("Invalid severity level: " + log["severity"])
    }
    
    // 验证消息
    if log["message"].length() == 0 {
      valid_log_errors.push("Empty log message")
    } else if log["message"].length() > max_message_length {
      valid_log_errors.push("Log message too long")
    }
    
    // 验证属性
    let attributes = log["attributes"]
    let mut j = 0
    while j < attributes.length() {
      let (key, value) = attributes[j]
      if key.length() == 0 {
        valid_log_errors.push("Empty attribute key")
      }
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证没有错误
  assert_eq(valid_log_errors.length(), 0)
  
  // 验证无效日志
  let mut invalid_log_errors = []
  i = 0
  while i < invalid_logs.length() {
    let log = invalid_logs[i]
    
    // 验证时间戳
    if log["timestamp"] < 0L {
      invalid_log_errors.push("Invalid timestamp")
    }
    
    // 验证严重级别
    let mut valid_severity = false
    let mut j = 0
    while j < log_severity_levels.length() {
      if log_severity_levels[j] == log["severity"] {
        valid_severity = true
        break
      }
      j = j + 1
    }
    if not valid_severity {
      invalid_log_errors.push("Invalid severity level: " + log["severity"])
    }
    
    // 验证消息
    if log["message"].length() == 0 {
      invalid_log_errors.push("Empty log message")
    } else if log["message"].length() > max_message_length {
      invalid_log_errors.push("Log message too long")
    }
    
    // 验证属性
    let attributes = log["attributes"]
    let mut j = 0
    while j < attributes.length() {
      let (key, value) = attributes[j]
      if key.length() == 0 {
        invalid_log_errors.push("Empty attribute key")
      }
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证有错误
  assert_eq(invalid_log_errors.length(), 6)
}

test "telemetry_data_consistency_validation" {
  // 测试遥测数据一致性验证
  
  let trace_data = [
    {
      "trace_id": "0af7651916cd43dd8448eb211c80319c",
      "span_id": "a1b2c3d4e5f67788",
      "parent_span_id": "",
      "service_name": "frontend",
      "operation_name": "user_request",
      "start_time": 1640995200L,
      "end_time": 1640995205L
    },
    {
      "trace_id": "0af7651916cd43dd8448eb211c80319c",
      "span_id": "b1c2d3e4f5a67788",
      "parent_span_id": "a1b2c3d4e5f67788",
      "service_name": "api-gateway",
      "operation_name": "proxy_request",
      "start_time": 1640995201L,
      "end_time": 1640995204L
    },
    {
      "trace_id": "0af7651916cd43dd8448eb211c80319c",
      "span_id": "c1d2e3f4a5b67788",
      "parent_span_id": "b1c2d3e4f5a67788",
      "service_name": "user-service",
      "operation_name": "get_user",
      "start_time": 1640995202L,
      "end_time": 1640995203L
    }
  ]
  
  let inconsistent_trace_data = [
    {
      "trace_id": "0af7651916cd43dd8448eb211c80319c",
      "span_id": "a1b2c3d4e5f67788",
      "parent_span_id": "",
      "service_name": "frontend",
      "operation_name": "user_request",
      "start_time": 1640995200L,
      "end_time": 1640995205L
    },
    {
      "trace_id": "different_trace_id", // 不一致：不同的trace_id
      "span_id": "b1c2d3e4f5a67788",
      "parent_span_id": "a1b2c3d4e5f67788",
      "service_name": "api-gateway",
      "operation_name": "proxy_request",
      "start_time": 1640995201L,
      "end_time": 1640995204L
    },
    {
      "trace_id": "0af7651916cd43dd8448eb211c80319c",
      "span_id": "c1d2e3f4a5b67788",
      "parent_span_id": "nonexistent_parent", // 不一致：不存在的父span
      "service_name": "user-service",
      "operation_name": "get_user",
      "start_time": 1640995200L, // 不一致：开始时间早于父span
      "end_time": 1640995203L
    }
  ]
  
  // 一致性验证函数
  let mut consistency_errors = []
  
  // 验证一致的数据
  let trace_id = trace_data[0]["trace_id"]
  let mut span_ids = []
  let mut i = 0
  while i < trace_data.length() {
    span_ids.push(trace_data[i]["span_id"])
    i = i + 1
  }
  
  // 检查trace_id一致性
  i = 0
  while i < trace_data.length() {
    if trace_data[i]["trace_id"] != trace_id {
      consistency_errors.push("Inconsistent trace_id")
    }
    i = i + 1
  }
  
  // 检查父子关系一致性
  i = 0
  while i < trace_data.length() {
    let parent_span_id = trace_data[i]["parent_span_id"]
    if parent_span_id.length() > 0 {
      let mut parent_found = false
      let mut j = 0
      while j < span_ids.length() {
        if span_ids[j] == parent_span_id {
          parent_found = true
          break
        }
        j = j + 1
      }
      if not parent_found {
        consistency_errors.push("Parent span not found: " + parent_span_id)
      }
    }
    i = i + 1
  }
  
  // 检查时间一致性
  i = 0
  while i < trace_data.length() {
    let span = trace_data[i]
    if span["start_time"] > span["end_time"] {
      consistency_errors.push("Span start time after end time")
    }
    
    let parent_span_id = span["parent_span_id"]
    if parent_span_id.length() > 0 {
      let mut j = 0
      while j < trace_data.length() {
        if trace_data[j]["span_id"] == parent_span_id {
          let parent_span = trace_data[j]
          if span["start_time"] < parent_span["start_time"] {
            consistency_errors.push("Child span starts before parent span")
          }
          if span["end_time"] > parent_span["end_time"] {
            consistency_errors.push("Child span ends after parent span")
          }
          break
        }
        j = j + 1
      }
    }
    i = i + 1
  }
  
  // 验证没有错误
  assert_eq(consistency_errors.length(), 0)
  
  // 验证不一致的数据
  consistency_errors = []
  let inconsistent_trace_id = inconsistent_trace_data[0]["trace_id"]
  span_ids = []
  i = 0
  while i < inconsistent_trace_data.length() {
    span_ids.push(inconsistent_trace_data[i]["span_id"])
    i = i + 1
  }
  
  // 检查trace_id一致性
  i = 0
  while i < inconsistent_trace_data.length() {
    if inconsistent_trace_data[i]["trace_id"] != inconsistent_trace_id {
      consistency_errors.push("Inconsistent trace_id")
    }
    i = i + 1
  }
  
  // 检查父子关系一致性
  i = 0
  while i < inconsistent_trace_data.length() {
    let parent_span_id = inconsistent_trace_data[i]["parent_span_id"]
    if parent_span_id.length() > 0 {
      let mut parent_found = false
      let mut j = 0
      while j < span_ids.length() {
        if span_ids[j] == parent_span_id {
          parent_found = true
          break
        }
        j = j + 1
      }
      if not parent_found {
        consistency_errors.push("Parent span not found: " + parent_span_id)
      }
    }
    i = i + 1
  }
  
  // 检查时间一致性
  i = 0
  while i < inconsistent_trace_data.length() {
    let span = inconsistent_trace_data[i]
    if span["start_time"] > span["end_time"] {
      consistency_errors.push("Span start time after end time")
    }
    
    let parent_span_id = span["parent_span_id"]
    if parent_span_id.length() > 0 {
      let mut j = 0
      while j < inconsistent_trace_data.length() {
        if inconsistent_trace_data[j]["span_id"] == parent_span_id {
          let parent_span = inconsistent_trace_data[j]
          if span["start_time"] < parent_span["start_time"] {
            consistency_errors.push("Child span starts before parent span")
          }
          if span["end_time"] > parent_span["end_time"] {
            consistency_errors.push("Child span ends after parent span")
          }
          break
        }
        j = j + 1
      }
    }
    i = i + 1
  }
  
  // 验证有错误
  assert_eq(consistency_errors.length(), 3)
  assert_eq(consistency_errors[0], "Inconsistent trace_id")
  assert_eq(consistency_errors[1], "Parent span not found: nonexistent_parent")
  assert_eq(consistency_errors[2], "Child span starts before parent span")
}