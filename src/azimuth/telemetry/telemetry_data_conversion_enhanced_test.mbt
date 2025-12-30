// 遥测数据转换增强测试用例
// 测试各种数据类型之间的转换和格式化

test "attribute_value_type_conversion_matrix" {
  // 测试属性值类型转换矩阵
  
  // 字符串转换测试
  let string_attr = AttributeValue::string("test_value")
  let string_array_attr = AttributeValue::array_string(["item1", "item2", "item3"])
  
  // 数值转换测试
  let int_attr = AttributeValue::int(42L)
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])
  let float_attr = AttributeValue::float(3.14159)
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  
  // 布尔值转换测试
  let bool_attr = AttributeValue::bool(true)
  let bool_array_attr = AttributeValue::array_bool([true, false, true])
  
  // 验证字符串属性
  match string_attr {
    StringValue(value) => {
      assert_eq(value, "test_value")
      assert_eq(value.length(), 10)
    }
    _ => assert_eq(false, true, "Expected StringValue")
  }
  
  // 验证字符串数组属性
  match string_array_attr {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[2], "item3")
    }
    _ => assert_eq(false, true, "Expected ArrayStringValue")
  }
  
  // 验证整数属性
  match int_attr {
    IntValue(value) => {
      assert_eq(value, 42L)
      assert_eq(value > 40L, true)
    }
    _ => assert_eq(false, true, "Expected IntValue")
  }
  
  // 验证整数数组属性
  match int_array_attr {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1L)
      assert_eq(values[4], 5L)
    }
    _ => assert_eq(false, true, "Expected ArrayIntValue")
  }
  
  // 验证浮点数属性
  match float_attr {
    FloatValue(value) => {
      assert_eq(value > 3.0, true)
      assert_eq(value < 3.2, true)
    }
    _ => assert_eq(false, true, "Expected FloatValue")
  }
  
  // 验证浮点数数组属性
  match float_array_attr {
    ArrayFloatValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1.1)
      assert_eq(values[2], 3.3)
    }
    _ => assert_eq(false, true, "Expected ArrayFloatValue")
  }
  
  // 验证布尔属性
  match bool_attr {
    BoolValue(value) => assert_eq(value, true)
    _ => assert_eq(false, true, "Expected BoolValue")
  }
  
  // 验证布尔数组属性
  match bool_array_attr {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], true)
      assert_eq(values[1], false)
      assert_eq(values[2], true)
    }
    _ => assert_eq(false, true, "Expected ArrayBoolValue")
  }
}

test "telemetry_unit_conversion" {
  // 测试遥测单位转换
  
  let time_units = [("ms", 1.0), ("s", 1000.0), ("min", 60000.0), ("h", 3600000.0)]
  let size_units = [("B", 1.0), ("KB", 1024.0), ("MB", 1048576.0), ("GB", 1073741824.0)]
  
  // 时间单位转换测试
  let base_time_ms = 5000.0
  let mut i = 0
  while i < time_units.length() {
    let (unit, multiplier) = time_units[i]
    let converted_value = base_time_ms / multiplier
    let formatted = base_time_ms.to_string() + "ms = " + converted_value.to_string() + unit
    
    assert_eq(formatted.contains("ms"), true)
    assert_eq(formatted.contains(unit), true)
    i = i + 1
  }
  
  // 大小单位转换测试
  let base_size_bytes = 1048576.0
  i = 0
  while i < size_units.length() {
    let (unit, multiplier) = size_units[i]
    let converted_value = base_size_bytes / multiplier
    let formatted = base_size_bytes.to_string() + "B = " + converted_value.to_string() + unit
    
    assert_eq(formatted.contains("B"), true)
    assert_eq(formatted.contains(unit), true)
    i = i + 1
  }
  
  // 验证特定转换
  assert_eq(base_time_ms / 1000.0, 5.0) // 5000ms = 5s
  assert_eq(base_size_bytes / 1048576.0, 1.0) // 1048576B = 1MB
}

test "metric_format_conversion" {
  // 测试指标格式转换
  
  let metric_name = "response_time"
  let metric_value = 123.456
  let metric_tags = [("method", "GET"), ("status", "200"), ("endpoint", "/api/users")]
  
  // Prometheus格式
  let prometheus_tags = ""
  let mut i = 0
  while i < metric_tags.length() {
    let (key, value) = metric_tags[i]
    if i > 0 { prometheus_tags = prometheus_tags + "," }
    prometheus_tags = prometheus_tags + key + "=\"" + value + "\""
    i = i + 1
  }
  
  let prometheus_format = metric_name + "{" + prometheus_tags + "} " + metric_value.to_string()
  assert_eq(prometheus_format.contains(metric_name), true)
  assert_eq(prometheus_format.contains("method=\"GET\""), true)
  assert_eq(prometheus_format.contains("status=\"200\""), true)
  assert_eq(prometheus_format.contains("endpoint=\"/api/users\""), true)
  
  // JSON格式
  let json_format = "{"
  json_format = json_format + "\"name\":\"" + metric_name + "\"," 
  json_format = json_format + "\"value\":" + metric_value.to_string() + ","
  json_format = json_format + "\"tags\":{"
  
  i = 0
  while i < metric_tags.length() {
    let (key, value) = metric_tags[i]
    if i > 0 { json_format = json_format + "," }
    json_format = json_format + "\"" + key + "\":\"" + value + "\""
    i = i + 1
  }
  
  json_format = json_format + "}}"
  
  assert_eq(json_format.has_prefix("{"), true)
  assert_eq(json_format.has_suffix("}"), true)
  assert_eq(json_format.contains("\"name\":\"response_time\""), true)
  assert_eq(json_format.contains("\"value\":123.456"), true)
}

test "trace_context_parsing" {
  // 测试追踪上下文解析
  
  let traceparent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  let tracestate = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  // 解析traceparent
  let traceparent_parts = traceparent.split("-")
  assert_eq(traceparent_parts.length(), 4)
  assert_eq(traceparent_parts[0], "00") // version
  assert_eq(traceparent_parts[1].length(), 32) // trace_id
  assert_eq(traceparent_parts[2].length(), 16) // span_id
  assert_eq(traceparent_parts[3], "01") // flags
  
  // 验证trace_id
  let trace_id = traceparent_parts[1]
  assert_eq(trace_id.has_prefix("0af7"), true)
  assert_eq(trace_id.has_suffix("319c"), true)
  assert_eq(trace_id.length(), 32)
  
  // 验证span_id
  let span_id = traceparent_parts[2]
  assert_eq(span_id.has_prefix("b7ad"), true)
  assert_eq(span_id.has_suffix("3331"), true)
  assert_eq(span_id.length(), 16)
  
  // 解析tracestate
  let tracestate_entries = tracestate.split(",")
  assert_eq(tracestate_entries.length(), 2)
  assert_eq(tracestate_entries[0], "rojo=00f067aa0ba902b7")
  assert_eq(tracestate_entries[1], "congo=t61rcWkgMzE")
  
  // 验证tracestate条目
  let mut i = 0
  while i < tracestate_entries.length() {
    let entry = tracestate_entries[i]
    let entry_parts = entry.split("=")
    assert_eq(entry_parts.length(), 2)
    assert_eq(entry_parts[0].length() > 0, true)
    assert_eq(entry_parts[1].length() > 0, true)
    i = i + 1
  }
}

test "log_severity_mapping" {
  // 测试日志严重性级别映射
  
  let severity_levels = [
    ("TRACE", 1),
    ("DEBUG", 2),
    ("INFO", 3),
    ("WARN", 4),
    ("ERROR", 5),
    ("FATAL", 6)
  ]
  
  let syslog_levels = [
    ("DEBUG", 7),
    ("INFO", 6),
    ("WARN", 4),
    ("ERROR", 3),
    ("FATAL", 2)
  ]
  
  // 验证严重性级别映射
  let mut i = 0
  while i < severity_levels.length() {
    let (level_name, level_number) = severity_levels[i]
    assert_eq(level_name.length() >= 4, true)
    assert_eq(level_number >= 1, true)
    assert_eq(level_number <= 6, true)
    i = i + 1
  }
  
  // 验证syslog级别映射
  i = 0
  while i < syslog_levels.length() {
    let (level_name, syslog_number) = syslog_levels[i]
    assert_eq(level_name.length() >= 4, true)
    assert_eq(syslog_number >= 2, true)
    assert_eq(syslog_number <= 7, true)
    i = i + 1
  }
  
  // 测试级别转换函数
  let level_to_string = fn(level : Int) -> String {
    match level {
      1 => "TRACE"
      2 => "DEBUG"
      3 => "INFO"
      4 => "WARN"
      5 => "ERROR"
      6 => "FATAL"
      _ => "UNKNOWN"
    }
  }
  
  assert_eq(level_to_string(1), "TRACE")
  assert_eq(level_to_string(3), "INFO")
  assert_eq(level_to_string(6), "FATAL")
  assert_eq(level_to_string(99), "UNKNOWN")
}

test "resource_attribute_normalization" {
  // 测试资源属性标准化
  
  let raw_attributes = [
    ("ServiceName", "payment-service"),
    ("service-version", "1.2.3"),
    ("SERVICE_INSTANCE_ID", "instance-12345"),
    ("deployment.environment", "production"),
    ("http.host", "api.example.com"),
    ("http_port", "8080")
  ]
  
  // 标准化规则：转换为小写，用点替换下划线和连字符
  let normalize_key = fn(key : String) -> String {
    let mut normalized = key.lowercase()
    normalized = normalized.replace("_", ".")
    normalized = normalized.replace("-", ".")
    normalized
  }
  
  // 验证标准化
  let mut i = 0
  while i < raw_attributes.length() {
    let (raw_key, value) = raw_attributes[i]
    let normalized_key = normalize_key(raw_key)
    
    // 验证标准化结果
    assert_eq(normalized_key.contains("_"), false)
    assert_eq(normalized_key.contains("-"), false)
    assert_eq(normalized_key.lowercase(), normalized_key)
    
    // 验证特定标准化
    match raw_key {
      "ServiceName" => assert_eq(normalized_key, "service.name")
      "service-version" => assert_eq(normalized_key, "service.version")
      "SERVICE_INSTANCE_ID" => assert_eq(normalized_key, "service.instance.id")
      "http_port" => assert_eq(normalized_key, "http.port")
      _ => ()
    }
    
    i = i + 1
  }
  
  // 创建标准化属性数组
  let normalized_attributes = []
  i = 0
  while i < raw_attributes.length() {
    let (raw_key, value) = raw_attributes[i]
    let normalized_key = normalize_key(raw_key)
    normalized_attributes.push((normalized_key, AttributeValue::string(value)))
    i = i + 1
  }
  
  assert_eq(normalized_attributes.length(), raw_attributes.length())
  
  // 验证标准化属性
  assert_eq(normalized_attributes[0].0, "service.name")
  assert_eq(normalized_attributes[1].0, "service.version")
  assert_eq(normalized_attributes[2].0, "service.instance.id")
}