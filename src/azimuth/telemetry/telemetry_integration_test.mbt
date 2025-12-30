// 遥测集成测试用例
// 测试遥测系统的端到端集成

test "telemetry_end_to_end_trace_flow" {
  // 测试端到端追踪流
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let service_chain = ["gateway", "auth-service", "user-service", "payment-service"]
  let span_ids = ["a1b2c3d4e5f67890", "b7ad6b7169203331", "c8d9e0f1a2b3c4d5", "d1e2f3a4b5c6d7e8"]
  
  // 创建服务链追踪
  let mut trace_flow = []
  let mut i = 0
  while i < service_chain.length() {
    let span_info = {
      "trace_id": trace_id,
      "span_id": span_ids[i],
      "service_name": service_chain[i],
      "parent_span_id": if i > 0 { span_ids[i - 1] } else { "" }
    }
    trace_flow.push(span_info)
    i = i + 1
  }
  
  // 验证追踪流
  assert_eq(trace_flow.length(), 4)
  assert_eq(trace_flow[0]["service_name"], "gateway")
  assert_eq(trace_flow[0]["parent_span_id"], "") // 根span
  assert_eq(trace_flow[1]["parent_span_id"], span_ids[0]) // 第一个子span
  assert_eq(trace_flow[3]["service_name"], "payment-service")
  
  // 验证追踪ID一致性
  i = 0
  while i < trace_flow.length() {
    assert_eq(trace_flow[i]["trace_id"], trace_id)
    i = i + 1
  }
  
  // 创建追踪流字符串
  let mut trace_string = trace_id + ":"
  i = 0
  while i < trace_flow.length() {
    trace_string = trace_string + service_chain[i] + "(" + span_ids[i] + ")"
    if i < trace_flow.length() - 1 {
      trace_string = trace_string + "->"
    }
    i = i + 1
  }
  
  // 验证追踪流字符串
  assert_eq(trace_string.has_prefix(trace_id + ":"), true)
  assert_eq(trace_string.contains("gateway(a1b2c3d4e5f67890)"), true)
  assert_eq(trace_string.contains("payment-service(d1e2f3a4b5c6d7e8)"), true)
  assert_eq(trace_string.contains("->"), true)
}

test "telemetry_cross_service_metrics" {
  // 测试跨服务指标
  
  let services = ["api-gateway", "user-service", "order-service", "payment-service"]
  let service_metrics = [
    [("http_requests_total", "1000"), ("error_rate", "0.1"), ("avg_response_time", "50")],
    [("http_requests_total", "2000"), ("error_rate", "0.2"), ("avg_response_time", "30")],
    [("http_requests_total", "1500"), ("error_rate", "0.5"), ("avg_response_time", "80")],
    [("http_requests_total", "800"), ("error_rate", "0.05"), ("avg_response_time", "120")]
  ]
  
  // 计算全局指标
  let mut total_requests = 0
  let mut total_errors = 0.0
  let mut weighted_response_time = 0.0
  
  let mut i = 0
  while i < services.length() {
    let requests = service_metrics[i][0].1.to_int()
    let error_rate = service_metrics[i][1].1.to_double()
    let response_time = service_metrics[i][2].1.to_double()
    
    total_requests = total_requests + requests
    total_errors = total_errors + (requests.to_double() * error_rate / 100.0)
    weighted_response_time = weighted_response_time + (requests.to_double() * response_time)
    
    i = i + 1
  }
  
  let global_error_rate = (total_errors / total_requests.to_double()) * 100.0
  let global_avg_response_time = weighted_response_time / total_requests.to_double()
  
  // 验证全局指标
  assert_eq(total_requests, 5300) // 1000 + 2000 + 1500 + 800
  assert_eq(global_error_rate > 0.0, true)
  assert_eq(global_error_rate < 1.0, true)
  assert_eq(global_avg_response_time > 40.0, true)
  assert_eq(global_avg_response_time < 80.0, true)
  
  // 找出性能最差的服务
  let mut worst_service_index = 0
  let mut max_response_time = service_metrics[0][2].1.to_double()
  
  i = 1
  while i < services.length() {
    let response_time = service_metrics[i][2].1.to_double()
    if response_time > max_response_time {
      max_response_time = response_time
      worst_service_index = i
    }
    i = i + 1
  }
  
  assert_eq(worst_service_index, 3) // payment-service
  assert_eq(services[worst_service_index], "payment-service")
  assert_eq(max_response_time, 120.0)
  
  // 创建跨服务指标报告
  let cross_service_report = "Total requests: " + total_requests.to_string() + 
                            ", Global error rate: " + global_error_rate.to_string().slice(0, 4) + "%, " +
                            "Global avg response time: " + global_avg_response_time.to_string().slice(0, 5) + "ms"
  
  assert_eq(cross_service_report.contains("Total requests: 5300"), true)
  assert_eq(cross_service_report.contains("Global error rate:"), true)
  assert_eq(cross_service_report.contains("Global avg response time:"), true)
}

test "telemetry_distributed_tracing_integration" {
  // 测试分布式追踪集成
  
  let external_systems = [
    ("database", "mysql-primary.internal", "3306"),
    ("cache", "redis-cluster.internal", "6379"),
    ("queue", "rabbitmq.internal", "5672"),
    ("storage", "s3.amazonaws.com", "443")
  ]
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "a1b2c3d4e5f67890"
  
  // 创建外部系统调用追踪
  let mut external_spans = []
  let mut i = 0
  while i < external_systems.length() {
    let system = external_systems[i]
    let external_span = {
      "trace_id": trace_id,
      "parent_span_id": parent_span_id,
      "span_id": "ext_" + i.to_string(),
      "service_name": system.0,
      "remote_endpoint": system.1 + ":" + system.2,
      "span_kind": "client"
    }
    external_spans.push(external_span)
    i = i + 1
  }
  
  // 验证外部span
  assert_eq(external_spans.length(), 4)
  assert_eq(external_spans[0]["service_name"], "database")
  assert_eq(external_spans[0]["remote_endpoint"], "mysql-primary.internal:3306")
  assert_eq(external_spans[3]["service_name"], "storage")
  assert_eq(external_spans[3]["remote_endpoint"], "s3.amazonaws.com:443")
  
  // 验证所有外部span都有相同的父span
  i = 0
  while i < external_spans.length() {
    assert_eq(external_spans[i]["parent_span_id"], parent_span_id)
    assert_eq(external_spans[i]["trace_id"], trace_id)
    assert_eq(external_spans[i]["span_kind"], "client")
    i = i + 1
  }
  
  // 创建分布式追踪字符串
  let mut distributed_trace = trace_id + ":" + parent_span_id + "->{"
  i = 0
  while i < external_spans.length() {
    distributed_trace = distributed_trace + external_spans[i]["service_name"] + 
                       "(" + external_spans[i]["remote_endpoint"] + ")"
    if i < external_spans.length() - 1 {
      distributed_trace = distributed_trace + ","
    }
    i = i + 1
  }
  distributed_trace = distributed_trace + "}"
  
  // 验证分布式追踪字符串
  assert_eq(distributed_trace.has_prefix(trace_id + ":" + parent_span_id + "->{"), true)
  assert_eq(distributed_trace.has_suffix("}"), true)
  assert_eq(distributed_trace.contains("database(mysql-primary.internal:3306)"), true)
  assert_eq(distributed_trace.contains("storage(s3.amazonaws.com:443)"), true)
}

test "telemetry_log_correlation" {
  // 测试日志关联
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let log_entries = [
    ("INFO", "Request received", "1640995200"),
    ("DEBUG", "Validating user permissions", "1640995201"),
    ("WARN", "Cache miss, querying database", "1640995202"),
    ("INFO", "User authenticated successfully", "1640995203"),
    ("ERROR", "Payment processing failed", "1640995204")
  ]
  
  // 创建带关联信息的日志
  let mut correlated_logs = []
  let mut i = 0
  while i < log_entries.length() {
    let log_entry = log_entries[i]
    let correlated_log = {
      "timestamp": log_entry.2,
      "level": log_entry.0,
      "message": log_entry.1,
      "trace_id": trace_id,
      "span_id": span_id
    }
    correlated_logs.push(correlated_log)
    i = i + 1
  }
  
  // 验证日志关联
  assert_eq(correlated_logs.length(), 5)
  
  // 验证所有日志都有相同的trace_id和span_id
  i = 0
  while i < correlated_logs.length() {
    assert_eq(correlated_logs[i]["trace_id"], trace_id)
    assert_eq(correlated_logs[i]["span_id"], span_id)
    i = i + 1
  }
  
  // 验证日志内容
  assert_eq(correlated_logs[0]["level"], "INFO")
  assert_eq(correlated_logs[0]["message"], "Request received")
  assert_eq(correlated_logs[4]["level"], "ERROR")
  assert_eq(correlated_logs[4]["message"], "Payment processing failed")
  
  // 创建关联日志字符串
  let mut log_correlation_string = ""
  i = 0
  while i < correlated_logs.length() {
    let log_str = correlated_logs[i]["timestamp"] + " [" + correlated_logs[i]["level"] + "] " +
                 correlated_logs[i]["message"] + " (trace=" + trace_id + ", span=" + span_id + ")"
    
    if i > 0 {
      log_correlation_string = log_correlation_string + "\n"
    }
    log_correlation_string = log_correlation_string + log_str
    i = i + 1
  }
  
  // 验证关联日志字符串
  assert_eq(log_correlation_string.contains("(trace=" + trace_id + ", span=" + span_id + ")"), true)
  assert_eq(log_correlation_string.contains("1640995204 [ERROR] Payment processing failed"), true)
  assert_eq(log_correlation_string.contains("1640995200 [INFO] Request received"), true)
  
  // 统计错误日志数量
  let mut error_count = 0
  i = 0
  while i < correlated_logs.length() {
    if correlated_logs[i]["level"] == "ERROR" {
      error_count = error_count + 1
    }
    i = i + 1
  }
  
  assert_eq(error_count, 1)
}

test "telemetry_metric_trace_correlation" {
  // 测试指标与追踪关联
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let operation_name = "http_get_user"
  
  let metrics = [
    ("http_request_duration_seconds", 0.125),
    ("database_query_duration_seconds", 0.045),
    ("cache_hit_ratio", 0.75),
    ("error_count", 0)
  ]
  
  // 创建带追踪关联的指标
  let mut correlated_metrics = []
  let mut i = 0
  while i < metrics.length() {
    let metric = metrics[i]
    let correlated_metric = {
      "name": metric.0,
      "value": metric.1.to_string(),
      "labels": "trace_id=" + trace_id + ",span_id=" + span_id + ",operation=" + operation_name
    }
    correlated_metrics.push(correlated_metric)
    i = i + 1
  }
  
  // 验证指标关联
  assert_eq(correlated_metrics.length(), 4)
  
  // 验证所有指标都有相同的标签
  i = 0
  while i < correlated_metrics.length() {
    assert_eq(correlated_metrics[i]["labels"].contains("trace_id=" + trace_id), true)
    assert_eq(correlated_metrics[i]["labels"].contains("span_id=" + span_id), true)
    assert_eq(correlated_metrics[i]["labels"].contains("operation=" + operation_name), true)
    i = i + 1
  }
  
  // 验证指标内容
  assert_eq(correlated_metrics[0]["name"], "http_request_duration_seconds")
  assert_eq(correlated_metrics[0]["value"], "0.125")
  assert_eq(correlated_metrics[3]["name"], "error_count")
  assert_eq(correlated_metrics[3]["value"], "0")
  
  // 创建关联指标字符串
  let mut metric_correlation_string = ""
  i = 0
  while i < correlated_metrics.length() {
    let metric_str = correlated_metrics[i]["name"] + "{" + correlated_metrics[i]["labels"] + "} " + 
                    correlated_metrics[i]["value"]
    
    if i > 0 {
      metric_correlation_string = metric_correlation_string + "\n"
    }
    metric_correlation_string = metric_correlation_string + metric_str
    i = i + 1
  }
  
  // 验证关联指标字符串
  assert_eq(metric_correlation_string.contains("trace_id=" + trace_id), true)
  assert_eq(metric_correlation_string.contains("span_id=" + span_id), true)
  assert_eq(metric_correlation_string.contains("operation=" + operation_name), true)
  assert_eq(metric_correlation_string.contains("http_request_duration_seconds{"), true)
  assert_eq(metric_correlation_string.contains("error_count{"), true)
}