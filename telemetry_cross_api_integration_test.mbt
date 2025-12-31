// 跨API集成测试 - 测试Logs、Metrics、Trace的协同工作

test "cross_api_telemetry_integration" {
  // 模拟一个完整的业务操作，涉及日志、指标和追踪
  
  // 1. 创建追踪上下文
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "1234567890abcdef"
  let operation_name = "user_registration"
  
  // 验证追踪ID格式
  assert_eq(trace_id.length(), 32)
  assert_eq(span_id.length(), 16)
  
  // 2. 创建日志记录
  let log_timestamp = 1640995200L
  let log_message = "User registration started"
  let log_severity = "INFO"
  
  // 验证日志数据
  assert_eq(log_message.contains("registration"), true)
  assert_eq(log_severity, "INFO")
  assert_eq(log_timestamp > 0L, true)
  
  // 3. 创建指标数据
  let metric_name = "user_registrations_total"
  let metric_value = 1.0
  let metric_attributes = [
    ("service", "auth-service"),
    ("version", "1.0.0"),
    ("region", "us-west-2")
  ]
  
  // 验证指标数据
  assert_eq(metric_name.contains("registrations"), true)
  assert_eq(metric_value, 1.0)
  assert_eq(metric_attributes.length(), 3)
  
  // 4. 验证跨API数据关联
  let correlation_id = "corr_" + trace_id + "_" + span_id
  assert_eq(correlation_id.has_prefix("corr_"), true)
  assert_eq(correlation_id.length(), 49) // "corr_" + 32 + "_" + 16
  
  // 5. 创建完整的遥测会话记录
  let session_data = [
    ("trace_id", trace_id),
    ("span_id", span_id),
    ("operation", operation_name),
    ("log_timestamp", log_timestamp.to_string()),
    ("log_message", log_message),
    ("metric_name", metric_name),
    ("metric_value", metric_value.to_string())
  ]
  
  // 验证会话数据完整性
  assert_eq(session_data.length(), 7)
  assert_eq(session_data[2].1, operation_name)
  assert_eq(session_data[6].1, "1.0")
}

test "telemetry_context_propagation" {
  // 测试遥测上下文在不同API间的传播
  
  // 1. 初始化上下文
  let parent_trace_id = "abcdef1234567890abcdef1234567890"
  let parent_span_id = "abcdef1234567890"
  let context_baggage = [
    ("user_id", "user_12345"),
    ("request_id", "req_67890"),
    ("session_id", "sess_abcde")
  ]
  
  // 验证父级上下文
  assert_eq(parent_trace_id.length(), 32)
  assert_eq(parent_span_id.length(), 16)
  assert_eq(context_baggage.length(), 3)
  
  // 2. 创建子级上下文
  let child_span_id = "fedcba0987654321"
  let child_operation = "validate_user_data"
  
  // 验证子级上下文
  assert_eq(child_span_id.length(), 16)
  assert_eq(child_operation.contains("validate"), true)
  
  // 3. 验证上下文传播
  let propagated_trace_id = parent_trace_id // 子级继承父级的trace_id
  let propagated_baggage = context_baggage // 子级继承父级的baggage
  
  assert_eq(propagated_trace_id, parent_trace_id)
  assert_eq(propagated_baggage.length(), 3)
  assert_eq(propagated_baggage[0].0, "user_id")
  assert_eq(propagated_baggage[0].1, "user_12345")
  
  // 4. 创建上下文链
  let context_chain = [
    ("parent_trace_id", parent_trace_id),
    ("parent_span_id", parent_span_id),
    ("child_span_id", child_span_id),
    ("operation", child_operation)
  ]
  
  // 验证上下文链
  assert_eq(context_chain.length(), 4)
  assert_eq(context_chain[0].1, parent_trace_id)
  assert_eq(context_chain[2].1, child_span_id)
}

test "telemetry_data_correlation" {
  // 测试不同遥测数据源之间的关联性
  
  // 1. 创建统一的请求ID
  let request_id = "req_" + "1234567890abcdef".to_string()
  let timestamp = 1640995200L
  
  // 验证请求ID
  assert_eq(request_id.has_prefix("req_"), true)
  assert_eq(request_id.length(), 20) // "req_" + 16
  
  // 2. 创建关联的日志事件
  let log_events = [
    ("request_start", timestamp.to_string(), "Request processing started"),
    ("auth_check", (timestamp + 100L).to_string(), "Authentication check"),
    ("data_validation", (timestamp + 200L).to_string(), "Data validation completed"),
    ("request_complete", (timestamp + 300L).to_string(), "Request processed successfully")
  ]
  
  // 验证日志事件
  assert_eq(log_events.length(), 4)
  assert_eq(log_events[0].0, "request_start")
  assert_eq(log_events[3].2.contains("successfully"), true)
  
  // 3. 创建关联的指标数据
  let metrics_data = [
    ("request_duration_ms", "300.0"),
    ("auth_check_duration_ms", "100.0"),
    ("validation_duration_ms", "200.0"),
    ("request_size_bytes", "1024.0")
  ]
  
  // 验证指标数据
  assert_eq(metrics_data.length(), 4)
  assert_eq(metrics_data[0].0.contains("duration"), true)
  assert_eq(metrics_data[3].0.contains("size"), true)
  
  // 4. 验证数据关联性
  let correlated_data = []
  let mut i = 0
  while i < log_events.length() {
    let event_key = log_events[i].0
    let event_timestamp = log_events[i].1
    let event_message = log_events[i].2
    
    // 查找相关的指标
    let mut related_metric = ""
    let mut j = 0
    while j < metrics_data.length() {
      if metrics_data[j].0.contains(event_key) {
        related_metric = metrics_data[j].1
        break
      }
      j = j + 1
    }
    
    let correlation_entry = request_id + ":" + event_key + ":" + event_timestamp + ":" + related_metric
    correlated_data.push(correlation_entry)
    i = i + 1
  }
  
  // 验证关联数据
  assert_eq(correlated_data.length(), 4)
  assert_eq(correlated_data[0].contains("req_"), true)
  assert_eq(correlated_data[0].contains("request_start"), true)
}