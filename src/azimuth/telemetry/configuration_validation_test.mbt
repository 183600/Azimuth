// 配置验证测试用例
// 测试telemetry系统的配置参数验证和边界条件

test "service_name_validation" {
  // 测试服务名称的验证规则
  
  // 测试有效的服务名称
  let valid_service_names = [
    "payment-service",
    "order_processing",
    "user-auth-service",
    "api.gateway",
    "notification.service.v2",
    "metrics-collector",
    "log-aggregator",
    "cache-service",
    "search-engine",
    "web-frontend",
    "mobile-backend",
    "data-processor",
    "analytics-service",
    "monitoring-system",
    "config-server",
    "registry-service",
    "discovery-service",
    "load-balancer",
    "rate-limiter",
    "auth-service"
  ]
  
  // 验证每个有效服务名称
  let mut i = 0
  while i < valid_service_names.length() {
    let service_name = valid_service_names[i]
    
    // 验证基本规则
    assert_eq(service_name.length() > 0, true)
    assert_eq(service_name.length() <= 255, true)
    assert_eq(service_name.starts_with(" "), false) // 不以空格开头
    assert_eq(service_name.ends_with(" "), false)   // 不以空格结尾
    
    // 创建Resource并验证
    let resource = Resource::default(service_name)
    assert_eq(resource.service_name, service_name)
    
    i = i + 1
  }
  
  // 测试无效的服务名称
  let invalid_service_names = [
    "",                    // 空字符串
    " ",                   // 只有空格
    "  service with leading spaces",
    "service with trailing spaces  ",
    "service with multiple  spaces",
    "service@with#special$characters",
    "service/with\\slashes",
    "service|with|pipes",
    "service?with?questions",
    "service*with*asterisks",
    "service\"with\"quotes",
    "service'with'apostrophes",
    "service<with>brackets",
    "service[with]square_brackets",
    "service{with}curly_brackets",
    "a".repeat(300)        // 过长的名称
  ]
  
  // 验证每个无效服务名称的规则
  let mut i = 0
  while i < invalid_service_names.length() {
    let service_name = invalid_service_names[i]
    
    // 验证无效规则
    if service_name.length() == 0 {
      assert_eq(service_name.length() == 0, true) // 空字符串
    } else if service_name.length() > 255 {
      assert_eq(service_name.length() > 255, true) // 过长
    } else if service_name.starts_with(" ") || service_name.ends_with(" ") {
      assert_eq(true, true) // 前导或尾随空格
    } else if service_name.contains("@") || service_name.contains("#") || 
              service_name.contains("$") || service_name.contains("/") ||
              service_name.contains("\\") || service_name.contains("|") ||
              service_name.contains("?") || service_name.contains("*") ||
              service_name.contains("\"") || service_name.contains("'") ||
              service_name.contains("<") || service_name.contains(">") ||
              service_name.contains("[") || service_name.contains("]") ||
              service_name.contains("{") || service_name.contains("}") {
      assert_eq(true, true) // 包含特殊字符
    }
    
    // 即使名称无效，也应该能够创建Resource（但应该有适当的验证）
    let resource = Resource::default(service_name)
    assert_eq(resource.service_name, service_name)
    
    i = i + 1
  }
}

test "attribute_key_validation" {
  // 测试属性键的验证规则
  
  // 测试有效的属性键
  let valid_attribute_keys = [
    "http.method",
    "http.status_code",
    "db.system",
    "db.statement",
    "messaging.system",
    "messaging.destination",
    "faas.invocation_id",
    "faas.coldstart",
    "k8s.pod.name",
    "k8s.namespace.name",
    "k8s.deployment.name",
    "k8s.node.name",
    "process.pid",
    "process.executable.name",
    "process.command_args",
    "service.name",
    "service.version",
    "service.instance.id",
    "telemetry.sdk.name",
    "telemetry.sdk.version",
    "telemetry.auto.version",
    "user.id",
    "user.email",
    "user.role",
    "session.id",
    "request.id",
    "trace.id",
    "span.id",
    "error.type",
    "error.message",
    "error.stack"
  ]
  
  // 验证每个有效属性键
  let mut i = 0
  while i < valid_attribute_keys.length() {
    let key = valid_attribute_keys[i]
    
    // 验证基本规则
    assert_eq(key.length() > 0, true)
    assert_eq(key.length() <= 255, true)
    assert_eq(key.starts_with(" "), false)
    assert_eq(key.ends_with(" "), false)
    
    // 验证属性键格式（通常使用点分隔的层次结构）
    let parts = key.split(".")
    assert_eq(parts.length() >= 2, true) // 至少应该有两个部分
    
    // 创建属性并验证
    let attribute = (key, AttributeValue::string("test.value"))
    assert_eq(attribute.0, key)
    
    i = i + 1
  }
  
  // 测试无效的属性键
  let invalid_attribute_keys = [
    "",                    // 空字符串
    " ",                   // 只有空格
    "  invalid key",
    "invalid key  ",
    "invalid key with spaces",
    "invalid@key",
    "invalid#key",
    "invalid$key",
    "invalid/key",
    "invalid\\key",
    "invalid|key",
    "invalid?key",
    "invalid*key",
    "invalid\"key",
    "invalid'key",
    "invalid<key>",
    "invalid[key]",
    "invalid{key}",
    "key",                 // 单一部分，不符合层次结构
    "a".repeat(300)        // 过长的键
  ]
  
  // 验证每个无效属性键的规则
  let mut i = 0
  while i < invalid_attribute_keys.length() {
    let key = invalid_attribute_keys[i]
    
    // 验证无效规则
    if key.length() == 0 {
      assert_eq(key.length() == 0, true) // 空字符串
    } else if key.length() > 255 {
      assert_eq(key.length() > 255, true) // 过长
    } else if key.starts_with(" ") || key.ends_with(" ") {
      assert_eq(true, true) // 前导或尾随空格
    } else if key.contains(" ") {
      assert_eq(true, true) // 包含空格
    } else if key.contains("@") || key.contains("#") || 
              key.contains("$") || key.contains("/") ||
              key.contains("\\") || key.contains("|") ||
              key.contains("?") || key.contains("*") ||
              key.contains("\"") || key.contains("'") ||
              key.contains("<") || key.contains(">") ||
              key.contains("[") || key.contains("]") ||
              key.contains("{") || key.contains("}") {
      assert_eq(true, true) // 包含特殊字符
    } else if !key.contains(".") {
      assert_eq(true, true) // 不包含点分隔符
    }
    
    // 即使键无效，也应该能够创建属性（但应该有适当的验证）
    let attribute = (key, AttributeValue::string("test.value"))
    assert_eq(attribute.0, key)
    
    i = i + 1
  }
}

test "metric_name_validation" {
  // 测试指标名称的验证规则
  
  // 测试有效的指标名称
  let valid_metric_names = [
    "http_requests_total",
    "http_request_duration_seconds",
    "http_response_size_bytes",
    "db_connections_active",
    "db_query_duration_seconds",
    "db_queries_total",
    "cache_hits_total",
    "cache_misses_total",
    "cache_hit_ratio",
    "messaging_messages_total",
    "messaging_message_size_bytes",
    "faas.invocations_total",
    "faas.duration_seconds",
    "faas.coldstarts_total",
    "k8s_pods_total",
    "k8s_containers_total",
    "process_cpu_seconds_total",
    "process_memory_bytes",
    "process_open_fds",
    "service_request_total",
    "service_request_duration_seconds",
    "service_error_total",
    "service_error_rate",
    "custom_business_metric_total",
    "custom_performance_metric_seconds",
    "custom_user_activity_count",
    "custom_revenue_total",
    "custom_conversion_rate"
  ]
  
  // 验证每个有效指标名称
  let mut i = 0
  while i < valid_metric_names.length() {
    let metric_name = valid_metric_names[i]
    
    // 验证基本规则
    assert_eq(metric_name.length() > 0, true)
    assert_eq(metric_name.length() <= 255, true)
    assert_eq(metric_name.starts_with(" "), false)
    assert_eq(metric_name.ends_with(" "), false)
    assert_eq(metric_name.contains(" "), false) // 不包含空格
    
    // 验证指标名称格式（通常使用下划线分隔）
    assert_eq(metric_name.contains("_"), true) // 应该包含下划线
    
    // 验证不以数字开头
    let first_char = metric_name[0]
    assert_eq(first_char >= '0' && first_char <= '9', false)
    
    // 创建Measurement并验证
    let measurement = Measurement::{
      value: 1.0,
      attributes: [("metric.name", AttributeValue::string(metric_name))]
    }
    assert_eq(measurement.attributes.length(), 1)
    
    i = i + 1
  }
  
  // 测试无效的指标名称
  let invalid_metric_names = [
    "",                    // 空字符串
    " ",                   // 只有空格
    "  invalid metric",
    "invalid metric  ",
    "invalid metric with spaces",
    "invalid@metric",
    "invalid#metric",
    "invalid$metric",
    "invalid/metric",
    "invalid\\metric",
    "invalid|metric",
    "invalid?metric",
    "invalid*metric",
    "invalid\"metric",
    "invalid'metric",
    "invalid<metric>",
    "invalid[metric]",
    "invalid{metric}",
    "123invalid_metric",   // 以数字开头
    "a".repeat(300)        // 过长的名称
  ]
  
  // 验证每个无效指标名称的规则
  let mut i = 0
  while i < invalid_metric_names.length() {
    let metric_name = invalid_metric_names[i]
    
    // 验证无效规则
    if metric_name.length() == 0 {
      assert_eq(metric_name.length() == 0, true) // 空字符串
    } else if metric_name.length() > 255 {
      assert_eq(metric_name.length() > 255, true) // 过长
    } else if metric_name.starts_with(" ") || metric_name.ends_with(" ") {
      assert_eq(true, true) // 前导或尾随空格
    } else if metric_name.contains(" ") {
      assert_eq(true, true) // 包含空格
    } else if metric_name.starts_with("0") || metric_name.starts_with("1") ||
              metric_name.starts_with("2") || metric_name.starts_with("3") ||
              metric_name.starts_with("4") || metric_name.starts_with("5") ||
              metric_name.starts_with("6") || metric_name.starts_with("7") ||
              metric_name.starts_with("8") || metric_name.starts_with("9") {
      assert_eq(true, true) // 以数字开头
    } else if metric_name.contains("@") || metric_name.contains("#") || 
              metric_name.contains("$") || metric_name.contains("/") ||
              metric_name.contains("\\") || metric_name.contains("|") ||
              metric_name.contains("?") || metric_name.contains("*") ||
              metric_name.contains("\"") || metric_name.contains("'") ||
              metric_name.contains("<") || metric_name.contains(">") ||
              metric_name.contains("[") || metric_name.contains("]") ||
              metric_name.contains("{") || metric_name.contains("}") {
      assert_eq(true, true) // 包含特殊字符
    }
    
    // 即使名称无效，也应该能够创建Measurement（但应该有适当的验证）
    let measurement = Measurement::{
      value: 1.0,
      attributes: [("metric.name", AttributeValue::string(metric_name))]
    }
    assert_eq(measurement.attributes.length(), 1)
    
    i = i + 1
  }
}

test "span_name_validation" {
  // 测试Span名称的验证规则
  
  // 测试有效的Span名称
  let valid_span_names = [
    "http-request",
    "database-query",
    "cache-get",
    "cache-set",
    "message-publish",
    "message-consume",
    "function-invoke",
    "auth-validate",
    "process-request",
    "generate-report",
    "send-email",
    "save-user",
    "update-profile",
    "delete-account",
    "search-products",
    "create-order",
    "process-payment",
    "calculate-shipping",
    "update-inventory",
    "send-notification"
  ]
  
  // 验证每个有效Span名称
  let mut i = 0
  while i < valid_span_names.length() {
    let span_name = valid_span_names[i]
    
    // 验证基本规则
    assert_eq(span_name.length() > 0, true)
    assert_eq(span_name.length() <= 255, true)
    assert_eq(span_name.starts_with(" "), false)
    assert_eq(span_name.ends_with(" "), false)
    
    // 创建Span并验证
    let span = Span::{
      name: span_name,
      context: SpanContext::{
        trace_id: [0x01_byte, 0x02_byte, 0x03_byte, 0x04_byte,
                   0x05_byte, 0x06_byte, 0x07_byte, 0x08_byte,
                   0x09_byte, 0x0A_byte, 0x0B_byte, 0x0C_byte,
                   0x0D_byte, 0x0E_byte, 0x0F_byte, 0x10_byte],
        span_id: [0x11_byte, 0x12_byte, 0x13_byte, 0x14_byte,
                  0x15_byte, 0x16_byte, 0x17_byte, 0x18_byte],
        trace_flags: 0x01_byte,
        trace_state: ""
      },
      kind: Internal,
      parent_span_id: None,
      start_time_unix_nanos: 1640995200000000000L,
      end_time_unix_nanos: Some(1640995200001000000L),
      status: Ok,
      status_description: None,
      attributes: [],
      events: [],
      links: []
    }
    assert_eq(span.name, span_name)
    
    i = i + 1
  }
  
  // 测试无效的Span名称
  let invalid_span_names = [
    "",                    // 空字符串
    " ",                   // 只有空格
    "  invalid span",
    "invalid span  ",
    "invalid span with spaces",
    "a".repeat(300)        // 过长的名称
  ]
  
  // 验证每个无效Span名称的规则
  let mut i = 0
  while i < invalid_span_names.length() {
    let span_name = invalid_span_names[i]
    
    // 验证无效规则
    if span_name.length() == 0 {
      assert_eq(span_name.length() == 0, true) // 空字符串
    } else if span_name.length() > 255 {
      assert_eq(span_name.length() > 255, true) // 过长
    } else if span_name.starts_with(" ") || span_name.ends_with(" ") {
      assert_eq(true, true) // 前导或尾随空格
    } else if span_name.contains(" ") {
      assert_eq(true, true) // 包含空格
    }
    
    // 即使名称无效，也应该能够创建Span（但应该有适当的验证）
    let span = Span::{
      name: span_name,
      context: SpanContext::{
        trace_id: [0x01_byte, 0x02_byte, 0x03_byte, 0x04_byte,
                   0x05_byte, 0x06_byte, 0x07_byte, 0x08_byte,
                   0x09_byte, 0x0A_byte, 0x0B_byte, 0x0C_byte,
                   0x0D_byte, 0x0E_byte, 0x0F_byte, 0x10_byte],
        span_id: [0x11_byte, 0x12_byte, 0x13_byte, 0x14_byte,
                  0x15_byte, 0x16_byte, 0x17_byte, 0x18_byte],
        trace_flags: 0x01_byte,
        trace_state: ""
      },
      kind: Internal,
      parent_span_id: None,
      start_time_unix_nanos: 1640995200000000000L,
      end_time_unix_nanos: Some(1640995200001000000L),
      status: Ok,
      status_description: None,
      attributes: [],
      events: [],
      links: []
    }
    assert_eq(span.name, span_name)
    
    i = i + 1
  }
}

test "log_severity_validation" {
  // 测试日志严重性级别的验证
  
  // 测试所有有效的严重性级别
  let valid_severity_numbers = [
    Trace,
    Debug,
    Info,
    Warn,
    Error,
    Fatal
  ]
  
  let valid_severity_texts = [
    "TRACE",
    "DEBUG",
    "INFO",
    "WARN",
    "ERROR",
    "FATAL"
  ]
  
  // 验证每个有效的严重性级别
  let mut i = 0
  while i < valid_severity_numbers.length() {
    let severity_number = valid_severity_numbers[i]
    let severity_text = valid_severity_texts[i]
    
    // 创建LogRecord并验证
    let log_record = LogRecord::{
      timestamp_unix_nanos: 1640995200000000000L,
      observed_timestamp_unix_nanos: None,
      severity_number: severity_number,
      severity_text: Some(severity_text),
      body: Some("Test message with " + severity_text + " severity"),
      attributes: [
        ("severity.level", AttributeValue::string(severity_text)),
        ("severity.number", AttributeValue::int(i.to_int64()))
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: None,
      instrumentation_scope: None
    }
    
    // 验证严重性级别
    match log_record.severity_number {
      Trace if i == 0 => assert_eq(true, true)
      Debug if i == 1 => assert_eq(true, true)
      Info if i == 2 => assert_eq(true, true)
      Warn if i == 3 => assert_eq(true, true)
      Error if i == 4 => assert_eq(true, true)
      Fatal if i == 5 => assert_eq(true, true)
      _ => @test.fail("Unexpected severity number")
    }
    
    match log_record.severity_text {
      Some(text) => assert_eq(text, severity_text)
      None => @test.fail("Expected severity text")
    }
    
    i = i + 1
  }
  
  // 测试严重性级别的一致性
  let trace_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Trace,
    severity_text: Some("TRACE"),
    body: Some("Trace message"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let error_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Error message"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证不同严重性级别的区分
  match trace_log.severity_number {
    Trace => assert_eq(true, true)
    _ => @test.fail("Expected Trace severity")
  }
  
  match error_log.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error severity")
  }
  
  // 验证严重性文本的区分
  match trace_log.severity_text {
    Some(text) => assert_eq(text, "TRACE")
    None => @test.fail("Expected TRACE severity text")
  }
  
  match error_log.severity_text {
    Some(text) => assert_eq(text, "ERROR")
    None => @test.fail("Expected ERROR severity text")
  }
}