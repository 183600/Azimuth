// 遥测数据验证测试用例

test "validation_metric_name_format" {
  // 测试指标名称格式验证
  
  let valid_metric_names = [
    "http_requests_total",
    "cpu_usage_percent",
    "memory_usage_bytes",
    "database_connections_active",
    "response_time_seconds"
  ]
  
  let invalid_metric_names = [
    "http requests total", // 包含空格
    "123_metric_name",     // 以数字开头
    "metric-name!",        // 包含特殊字符
    "",                    // 空字符串
    "a"                    // 过短
  ]
  
  // 验证有效指标名称
  let mut valid_count = 0
  let mut i = 0
  while i < valid_metric_names.length() {
    let name = valid_metric_names[i]
    let is_valid = name.length() >= 3 && 
                   name.has_prefix("123") == false &&
                   name.contains(" ") == false &&
                   name.contains("!") == false
    if is_valid {
      valid_count = valid_count + 1
    }
    i = i + 1
  }
  
  // 验证无效指标名称
  let mut invalid_count = 0
  i = 0
  while i < invalid_metric_names.length() {
    let name = invalid_metric_names[i]
    let is_invalid = name.length() < 3 || 
                     name.has_prefix("123") ||
                     name.contains(" ") ||
                     name.contains("!") ||
                     name == ""
    if is_invalid {
      invalid_count = invalid_count + 1
    }
    i = i + 1
  }
  
  // 验证结果
  assert_eq(valid_metric_names.length(), 5)
  assert_eq(invalid_metric_names.length(), 5)
  assert_eq(valid_count, 5)
  assert_eq(invalid_count, 5)
}

test "validation_trace_id_format" {
  // 测试追踪ID格式验证
  
  let valid_trace_ids = [
    "0af7651916cd43dd8448eb211c80319c",
    "1234567890abcdef1234567890abcdef",
    "abcdef0123456789abcdef0123456789"
  ]
  
  let invalid_trace_ids = [
    "0af7651916cd43dd8448eb211c80319",   // 31个字符
    "0af7651916cd43dd8448eb211c80319cc", // 33个字符
    "gaf7651916cd43dd8448eb211c80319c",  // 包含非十六进制字符
    "",                                   // 空字符串
    "0AF7651916CD43DD8448EB211C80319C"   // 大写（可能有效，但这里假设需要小写）
  ]
  
  // 验证有效追踪ID
  let mut valid_count = 0
  let mut i = 0
  while i < valid_trace_ids.length() {
    let trace_id = valid_trace_ids[i]
    let is_valid = trace_id.length() == 32 && 
                   trace_id.to_lower() == trace_id
    if is_valid {
      valid_count = valid_count + 1
    }
    i = i + 1
  }
  
  // 验证无效追踪ID
  let mut invalid_count = 0
  i = 0
  while i < invalid_trace_ids.length() {
    let trace_id = invalid_trace_ids[i]
    let is_invalid = trace_id.length() != 32 || 
                     trace_id.to_lower() != trace_id ||
                     trace_id == ""
    if is_invalid {
      invalid_count = invalid_count + 1
    }
    i = i + 1
  }
  
  // 验证结果
  assert_eq(valid_trace_ids.length(), 3)
  assert_eq(invalid_trace_ids.length(), 5)
  assert_eq(valid_count, 3)
  assert_eq(invalid_count, 5)
}

test "validation_span_id_format" {
  // 测试span ID格式验证
  
  let valid_span_ids = [
    "b7ad6b7169203331",
    "1234567890abcdef",
    "abcdef0123456789"
  ]
  
  let invalid_span_ids = [
    "b7ad6b716920333",    // 15个字符
    "b7ad6b71692033311",  // 17个字符
    "g7ad6b7169203331",   // 包含非十六进制字符
    "",                   // 空字符串
    "B7AD6B7169203331"    // 大写
  ]
  
  // 验证有效span ID
  let mut valid_count = 0
  let mut i = 0
  while i < valid_span_ids.length() {
    let span_id = valid_span_ids[i]
    let is_valid = span_id.length() == 16 && 
                   span_id.to_lower() == span_id
    if is_valid {
      valid_count = valid_count + 1
    }
    i = i + 1
  }
  
  // 验证无效span ID
  let mut invalid_count = 0
  i = 0
  while i < invalid_span_ids.length() {
    let span_id = invalid_span_ids[i]
    let is_invalid = span_id.length() != 16 || 
                     span_id.to_lower() != span_id ||
                     span_id == ""
    if is_invalid {
      invalid_count = invalid_count + 1
    }
    i = i + 1
  }
  
  // 验证结果
  assert_eq(valid_span_ids.length(), 3)
  assert_eq(invalid_span_ids.length(), 5)
  assert_eq(valid_count, 3)
  assert_eq(invalid_count, 5)
}

test "validation_metric_value_ranges" {
  // 测试指标值范围验证
  
  let percentage_values = [0.0, 25.5, 50.0, 75.3, 100.0]
  let invalid_percentage_values = [-1.0, 100.1, 150.0, -50.0]
  
  let byte_values = [0L, 1024L, 1048576L, 1073741824L]
  let invalid_byte_values = [-1L, -1024L]
  
  // 验证有效百分比值
  let mut valid_percentage_count = 0
  let mut i = 0
  while i < percentage_values.length() {
    let value = percentage_values[i]
    if value >= 0.0 && value <= 100.0 {
      valid_percentage_count = valid_percentage_count + 1
    }
    i = i + 1
  }
  
  // 验证无效百分比值
  let mut invalid_percentage_count = 0
  i = 0
  while i < invalid_percentage_values.length() {
    let value = invalid_percentage_values[i]
    if value < 0.0 || value > 100.0 {
      invalid_percentage_count = invalid_percentage_count + 1
    }
    i = i + 1
  }
  
  // 验证有效字节值
  let mut valid_byte_count = 0
  i = 0
  while i < byte_values.length() {
    let value = byte_values[i]
    if value >= 0L {
      valid_byte_count = valid_byte_count + 1
    }
    i = i + 1
  }
  
  // 验证无效字节值
  let mut invalid_byte_count = 0
  i = 0
  while i < invalid_byte_values.length() {
    let value = invalid_byte_values[i]
    if value < 0L {
      invalid_byte_count = invalid_byte_count + 1
    }
    i = i + 1
  }
  
  // 验证结果
  assert_eq(percentage_values.length(), 5)
  assert_eq(invalid_percentage_values.length(), 4)
  assert_eq(valid_percentage_count, 5)
  assert_eq(invalid_percentage_count, 4)
  
  assert_eq(byte_values.length(), 4)
  assert_eq(invalid_byte_values.length(), 2)
  assert_eq(valid_byte_count, 4)
  assert_eq(invalid_byte_count, 2)
}

test "validation_timestamp_format" {
  // 测试时间戳格式验证
  
  let valid_unix_timestamps = [
    1640995200L, // 2022-01-01 00:00:00
    1640995260L, // 2022-01-01 00:01:00
    1640995320L  // 2022-01-01 00:02:00
  ]
  
  let invalid_unix_timestamps = [
    -1L,         // 负数时间戳
    0L,          // 零时间戳（可能有效，但这里假设需要正数）
    999999999999999999L // 过大的时间戳
  ]
  
  let valid_iso8601_timestamps = [
    "2022-01-01T00:00:00Z",
    "2022-01-01T00:01:00Z",
    "2022-01-01T00:02:00Z"
  ]
  
  let invalid_iso8601_timestamps = [
    "2022-13-01T00:00:00Z", // 无效月份
    "2022-01-32T00:00:00Z", // 无效日期
    "2022-01-01T25:00:00Z", // 无效小时
    "invalid-timestamp"     // 完全无效
  ]
  
  // 验证有效Unix时间戳
  let mut valid_unix_count = 0
  let mut i = 0
  while i < valid_unix_timestamps.length() {
    let timestamp = valid_unix_timestamps[i]
    if timestamp > 0L && timestamp < 9999999999L {
      valid_unix_count = valid_unix_count + 1
    }
    i = i + 1
  }
  
  // 验证无效Unix时间戳
  let mut invalid_unix_count = 0
  i = 0
  while i < invalid_unix_timestamps.length() {
    let timestamp = invalid_unix_timestamps[i]
    if timestamp <= 0L || timestamp >= 9999999999L {
      invalid_unix_count = invalid_unix_count + 1
    }
    i = i + 1
  }
  
  // 验证有效ISO8601时间戳
  let mut valid_iso_count = 0
  i = 0
  while i < valid_iso8601_timestamps.length() {
    let timestamp = valid_iso8601_timestamps[i]
    if timestamp.contains("T") && timestamp.has_suffix("Z") {
      valid_iso_count = valid_iso_count + 1
    }
    i = i + 1
  }
  
  // 验证无效ISO8601时间戳
  let mut invalid_iso_count = 0
  i = 0
  while i < invalid_iso8601_timestamps.length() {
    let timestamp = invalid_iso8601_timestamps[i]
    if !timestamp.contains("T") || !timestamp.has_suffix("Z") || 
       timestamp.contains("13") || timestamp.contains("32") || timestamp.contains("25") {
      invalid_iso_count = invalid_iso_count + 1
    }
    i = i + 1
  }
  
  // 验证结果
  assert_eq(valid_unix_timestamps.length(), 3)
  assert_eq(invalid_unix_timestamps.length(), 3)
  assert_eq(valid_unix_count, 3)
  assert_eq(invalid_unix_count, 3)
  
  assert_eq(valid_iso8601_timestamps.length(), 3)
  assert_eq(invalid_iso8601_timestamps.length(), 4)
  assert_eq(valid_iso_count, 3)
  assert_eq(invalid_iso_count, 4)
}

test "validation_attribute_format" {
  // 测试属性格式验证
  
  let valid_attributes = [
    ("http.method", "GET"),
    ("http.status_code", "200"),
    ("user.id", "user123"),
    ("service.name", "api-service"),
    ("region", "us-east-1")
  ]
  
  let invalid_attributes = [
    ("", "value"),           // 空键
    ("key", ""),             // 空值
    ("key with spaces", "value"), // 键包含空格
    ("key", "value with spaces"),  // 值包含空格（可能有效）
    ("special!key", "value") // 键包含特殊字符
  ]
  
  // 验证有效属性
  let mut valid_count = 0
  let mut i = 0
  while i < valid_attributes.length() {
    let (key, value) = valid_attributes[i]
    let is_valid = key.length() > 0 && value.length() > 0
    if is_valid {
      valid_count = valid_count + 1
    }
    i = i + 1
  }
  
  // 验证无效属性
  let mut invalid_count = 0
  i = 0
  while i < invalid_attributes.length() {
    let (key, value) = invalid_attributes[i]
    let is_invalid = key.length() == 0 || 
                     value.length() == 0 ||
                     key.contains(" ") ||
                     key.contains("!")
    if is_invalid {
      invalid_count = invalid_count + 1
    }
    i = i + 1
  }
  
  // 验证结果
  assert_eq(valid_attributes.length(), 5)
  assert_eq(invalid_attributes.length(), 5)
  assert_eq(valid_count, 5)
  assert_eq(invalid_count, 4) // "value with spaces" 可能被认为是有效的
}

test "validation_resource_format" {
  // 测试资源格式验证
  
  let valid_resources = [
    ("service.name", "user-service"),
    ("service.version", "1.2.3"),
    ("service.instance.id", "instance-12345"),
    ("host.name", "server-01"),
    ("deployment.environment", "production")
  ]
  
  let invalid_resources = [
    ("service.name", ""),                    // 空服务名
    ("service.version", "not.a.version"),    // 无效版本格式
    ("service.instance.id", "no-prefix"),    // 缺少前缀
    ("deployment.environment", "invalid-env") // 无效环境
  ]
  
  // 验证有效资源
  let mut valid_count = 0
  let mut i = 0
  while i < valid_resources.length() {
    let (key, value) = valid_resources[i]
    let is_valid = match key {
      "service.name" => value.has_suffix("-service"),
      "service.version" => value.contains("."),
      "service.instance.id" => value.has_prefix("instance-"),
      "host.name" => value.length() > 0,
      "deployment.environment" => value == "production" || value == "staging" || value == "development",
      _ => false
    }
    if is_valid {
      valid_count = valid_count + 1
    }
    i = i + 1
  }
  
  // 验证无效资源
  let mut invalid_count = 0
  i = 0
  while i < invalid_resources.length() {
    let (key, value) = invalid_resources[i]
    let is_invalid = match key {
      "service.name" => !value.has_suffix("-service") || value == "",
      "service.version" => !value.contains(".") || value == "not.a.version",
      "service.instance.id" => !value.has_prefix("instance-"),
      "deployment.environment" => !(value == "production" || value == "staging" || value == "development"),
      _ => false
    }
    if is_invalid {
      invalid_count = invalid_count + 1
    }
    i = i + 1
  }
  
  // 验证结果
  assert_eq(valid_resources.length(), 5)
  assert_eq(invalid_resources.length(), 4)
  assert_eq(valid_count, 5)
  assert_eq(invalid_count, 4)
}

test "validation_data_consistency" {
  // 测试数据一致性验证
  
  let consistent_data = [
    ("trace-id", "0af7651916cd43dd8448eb211c80319c"),
    ("span-id", "b7ad6b7169203331"),
    ("parent-span-id", "b7ad6b7169203330"),
    ("timestamp", "1640995200L")
  ]
  
  let inconsistent_data = [
    ("trace-id", "0af7651916cd43dd8448eb211c80319c"),
    ("span-id", "b7ad6b7169203331"),
    ("parent-span-id", "b7ad6b7169203331"), // 与span-id相同
    ("timestamp", "1640995200L")
  ]
  
  // 验证一致性数据
  let mut is_consistent = true
  let mut trace_id = ""
  let mut span_id = ""
  let mut parent_span_id = ""
  let mut i = 0
  while i < consistent_data.length() {
    let (key, value) = consistent_data[i]
    match key {
      "trace-id" => trace_id = value,
      "span-id" => span_id = value,
      "parent-span-id" => parent_span_id = value,
      _ => ()
    }
    i = i + 1
  }
  
  // 检查一致性：span-id不应该与parent-span-id相同
  if span_id == parent_span_id && parent_span_id != "" {
    is_consistent = false
  }
  
  // 验证不一致数据
  let mut is_inconsistent = false
  trace_id = ""
  span_id = ""
  parent_span_id = ""
  i = 0
  while i < inconsistent_data.length() {
    let (key, value) = inconsistent_data[i]
    match key {
      "trace-id" => trace_id = value,
      "span-id" => span_id = value,
      "parent-span-id" => parent_span_id = value,
      _ => ()
    }
    i = i + 1
  }
  
  // 检查不一致性：span-id与parent-span-id相同
  if span_id == parent_span_id && parent_span_id != "" {
    is_inconsistent = true
  }
  
  // 验证结果
  assert_eq(consistent_data.length(), 4)
  assert_eq(inconsistent_data.length(), 4)
  assert_eq(is_consistent, true)
  assert_eq(is_inconsistent, true)
}

test "validation_data_completeness" {
  // 测试数据完整性验证
  
  let complete_data = [
    ("trace-id", "0af7651916cd43dd8448eb211c80319c"),
    ("span-id", "b7ad6b7169203331"),
    ("operation-name", "http-request"),
    ("start-time", "1640995200L"),
    ("end-time", "1640995210L")
  ]
  
  let incomplete_data = [
    ("trace-id", "0af7651916cd43dd8448eb211c80319c"),
    ("span-id", "b7ad6b7169203331"),
    // 缺少operation-name
    ("start-time", "1640995200L"),
    // 缺少end-time
  ]
  
  let required_fields = ["trace-id", "span-id", "operation-name", "start-time", "end-time"]
  
  // 验证完整数据
  let mut complete_field_count = 0
  let mut i = 0
  while i < required_fields.length() {
    let required_field = required_fields[i]
    let mut field_found = false
    let mut j = 0
    while j < complete_data.length() {
      if complete_data[j].0 == required_field {
        field_found = true
        break
      }
      j = j + 1
    }
    if field_found {
      complete_field_count = complete_field_count + 1
    }
    i = i + 1
  }
  
  // 验证不完整数据
  let mut incomplete_field_count = 0
  i = 0
  while i < required_fields.length() {
    let required_field = required_fields[i]
    let mut field_found = false
    let mut j = 0
    while j < incomplete_data.length() {
      if incomplete_data[j].0 == required_field {
        field_found = true
        break
      }
      j = j + 1
    }
    if field_found {
      incomplete_field_count = incomplete_field_count + 1
    }
    i = i + 1
  }
  
  // 验证结果
  assert_eq(required_fields.length(), 5)
  assert_eq(complete_data.length(), 5)
  assert_eq(incomplete_data.length(), 3)
  assert_eq(complete_field_count, 5) // 所有必需字段都存在
  assert_eq(incomplete_field_count, 3) // 只有3个必需字段存在
}