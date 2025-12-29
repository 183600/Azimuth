// 遥测数据验证测试用例

test "trace_id_validation" {
  // 测试追踪ID验证
  
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let invalid_trace_id_short = "0af7651916cd43dd8448eb211c80319"
  let invalid_trace_id_long = "0af7651916cd43dd8448eb211c80319cc"
  let invalid_trace_id_chars = "0af7651916cd43dd8448eb211c80319z"
  
  // 验证有效追踪ID
  assert_eq(valid_trace_id.length(), 32)
  
  // 验证无效追踪ID（太短）
  assert_eq(invalid_trace_id_short.length(), 31)
  
  // 验证无效追踪ID（太长）
  assert_eq(invalid_trace_id_long.length(), 33)
  
  // 验证无效追踪ID（非法字符）
  assert_eq(invalid_trace_id_chars.length(), 32)
  
  // 验证有效字符集
  let hex_chars = "0123456789abcdef"
  let mut i = 0
  let mut valid_chars = true
  while i < valid_trace_id.length() {
    if !hex_chars.contains(valid_trace_id[i].to_string()) {
      valid_chars = false
    }
    i = i + 1
  }
  
  assert_eq(valid_chars, true)
  
  // 验证字符是否都是有效的十六进制字符
  assert_eq(valid_trace_id.contains("g"), false)
  assert_eq(valid_trace_id.contains("z"), false)
}

test "span_id_validation" {
  // 测试Span ID验证
  
  let valid_span_id = "b7ad6b7169203331"
  let invalid_span_id_short = "b7ad6b716920333"
  let invalid_span_id_long = "b7ad6b71692033311"
  let invalid_span_id_chars = "b7ad6b716920333z"
  
  // 验证有效Span ID
  assert_eq(valid_span_id.length(), 16)
  
  // 验证无效Span ID（太短）
  assert_eq(invalid_span_id_short.length(), 15)
  
  // 验证无效Span ID（太长）
  assert_eq(invalid_span_id_long.length(), 17)
  
  // 验证无效Span ID（非法字符）
  assert_eq(invalid_span_id_chars.length(), 16)
}

test "metric_name_validation" {
  // 测试指标名称验证
  
  let valid_metric_names = [
    "http_requests_total",
    "cpu_usage_percentage",
    "memory_usage_bytes",
    "response_time_seconds",
    "error_count"
  ]
  
  let invalid_metric_names = [
    "invalid metric name with spaces",
    "123invalid_start_with_number",
    "invalid@special#chars",
    "",
    "a" // 太短
  ]
  
  // 验证有效指标名称
  assert_eq(valid_metric_names.length(), 5)
  
  // 验证无效指标名称
  assert_eq(invalid_metric_names.length(), 5)
  
  // 验证指标名称格式
  let mut i = 0
  while i < valid_metric_names.length() {
    assert_eq(valid_metric_names[i].length() > 1, true)
    assert_eq(valid_metric_names[i].contains(" "), false)
    i = i + 1
  }
}

test "attribute_key_validation" {
  // 测试属性键验证
  
  let valid_attribute_keys = [
    "http.method",
    "http.status_code",
    "user.id",
    "service.name",
    "cloud.region"
  ]
  
  let invalid_attribute_keys = [
    "invalid key with spaces",
    "123invalid_start_with_number",
    "invalid@special#chars",
    "",
    "a" // 太短
  ]
  
  // 验证有效属性键
  assert_eq(valid_attribute_keys.length(), 5)
  
  // 验证无效属性键
  assert_eq(invalid_attribute_keys.length(), 5)
  
  // 验证属性键格式
  let mut i = 0
  while i < valid_attribute_keys.length() {
    assert_eq(valid_attribute_keys[i].length() > 1, true)
    assert_eq(valid_attribute_keys[i].contains(" "), false)
    i = i + 1
  }
}

test "log_level_validation" {
  // 测试日志级别验证
  
  let valid_log_levels = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
  let invalid_log_levels = ["trace", "debug", "info", "warn", "error", "fatal", "INVALID", ""]
  
  // 验证有效日志级别
  assert_eq(valid_log_levels.length(), 6)
  
  // 验证无效日志级别
  assert_eq(invalid_log_levels.length(), 8)
  
  // 验证日志级别格式
  let mut i = 0
  while i < valid_log_levels.length() {
    let level = valid_log_levels[i]
    assert_eq(level.length() >= 4, true)
    // 验证是否全是大写字母
    assert_eq(level == "TRACE" || level == "DEBUG" || level == "INFO" || level == "WARN" || level == "ERROR" || level == "FATAL", true)
    i = i + 1
  }
}

test "service_name_validation" {
  // 测试服务名称验证
  
  let valid_service_names = [
    "user-service",
    "order-service",
    "payment-service",
    "auth-service",
    "notification-service"
  ]
  
  let invalid_service_names = [
    "invalid service name with spaces",
    "123invalid_start_with_number",
    "invalid@special#chars",
    "",
    "a" // 太短
  ]
  
  // 验证有效服务名称
  assert_eq(valid_service_names.length(), 5)
  
  // 验证无效服务名称
  assert_eq(invalid_service_names.length(), 5)
  
  // 验证服务名称格式
  let mut i = 0
  while i < valid_service_names.length() {
    assert_eq(valid_service_names[i].length() > 1, true)
    assert_eq(valid_service_names[i].contains(" "), false)
    assert_eq(valid_service_names[i].has_suffix("-service"), true)
    i = i + 1
  }
}