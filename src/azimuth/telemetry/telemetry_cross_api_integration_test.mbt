// 跨API集成测试用例
// 测试不同遥测API之间的集成和协作

test "trace_to_log_correlation" {
  // 测试追踪到日志的关联
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let operation_name = "http_request"
  
  // 创建追踪上下文
  let trace_context = trace_id + ":" + span_id
  assert_eq(trace_context.length(), 49)
  assert_eq(trace_context.contains(":"), true)
  
  // 创建关联的日志记录
  let log_message = "Operation completed successfully"
  let log_with_trace = log_message + " trace_id=" + trace_id + " span_id=" + span_id
  
  // 验证日志关联
  assert_eq(log_with_trace.has_prefix(log_message), true)
  assert_eq(log_with_trace.contains(trace_id), true)
  assert_eq(log_with_trace.contains(span_id), true)
  assert_eq(log_with_trace.length(), log_message.length() + trace_id.length() + span_id.length() + 17)
  
  // 验证操作名称关联
  let operation_log = "Operation: " + operation_name + " - " + log_message
  assert_eq(operation_log.contains(operation_name), true)
  assert_eq(operation_log.contains(log_message), true)
}

test "metric_to_trace_enrichment" {
  // 测试指标到追踪的丰富化
  
  let metric_name = "http_request_duration"
  let metric_value = 125.5
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // 创建指标数据
  let metric_data = metric_name + "=" + metric_value.to_string()
  assert_eq(metric_data.has_prefix(metric_name), true)
  assert_eq(metric_data.contains("125.5"), true)
  
  // 创建丰富的追踪数据
  let enriched_trace = trace_id + "|metrics:" + metric_data
  assert_eq(enriched_trace.has_prefix(trace_id), true)
  assert_eq(enriched_trace.contains("metrics:"), true)
  assert_eq(enriched_trace.contains(metric_data), true)
  
  // 验证丰富化格式
  let enrichment_parts = enriched_trace.split("|")
  assert_eq(enrichment_parts.length(), 2)
  assert_eq(enrichment_parts[0], trace_id)
  assert_eq(enrichment_parts[1].has_prefix("metrics:"), true)
}

test "log_to_metric_extraction" {
  // 测试日志到指标的提取
  
  let log_entry = "Request processed in 45ms with status 200"
  let log_pattern = "Request processed in "
  let status_pattern = " with status "
  
  // 提取处理时间
  assert_eq(log_entry.has_prefix(log_pattern), true)
  let time_part = log_entry.slice(log_pattern.length(), log_entry.length())
  let time_end = time_part.find("ms").unwrap_or(0)
  let processing_time = time_part.slice(0, time_end)
  assert_eq(processing_time, "45")
  
  // 提取状态码
  assert_eq(log_entry.contains(status_pattern), true)
  let status_start = log_entry.find(status_pattern).unwrap_or(0) + status_pattern.length()
  let status_code = log_entry.slice(status_start, log_entry.length())
  assert_eq(status_code, "200")
  
  // 创建指标
  let duration_metric = "request_duration_ms=" + processing_time
  let status_metric = "request_status=" + status_code
  
  assert_eq(duration_metric, "request_duration_ms=45")
  assert_eq(status_metric, "request_status=200")
}

test "context_propagation_integration" {
  // 测试上下文传播集成
  
  let context_key = "user_id"
  let context_value = "user12345"
  let baggage_key = "request_id"
  let baggage_value = "req-67890"
  
  // 创建上下文
  let context_data = context_key + "=" + context_value
  let baggage_data = baggage_key + "=" + baggage_value
  
  // 验证上下文数据
  assert_eq(context_data.length(), 14)
  assert_eq(context_data.contains("="), true)
  assert_eq(baggage_data.length(), 17)
  assert_eq(baggage_data.contains("="), true)
  
  // 创建传播头
  let traceparent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  let tracestate = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  // 验证传播头格式
  assert_eq(traceparent.has_prefix("00-"), true)
  assert_eq(traceparent.contains("-"), true)
  assert_eq(traceparent.length(), 55)
  
  assert_eq(tracestate.contains("="), true)
  assert_eq(tracestate.contains(","), true)
  assert_eq(tracestate.length(), 39)
  
  // 集成传播数据
  let propagation_data = traceparent + "|" + tracestate + "|" + context_data + "|" + baggage_data
  assert_eq(propagation_data.split("|").length(), 4)
  assert_eq(propagation_data.contains(context_data), true)
  assert_eq(propagation_data.contains(baggage_data), true)
}

test "resource_attribute_sharing" {
  // 测试资源属性共享
  
  let service_name = "payment-service"
  let service_version = "1.2.3"
  let deployment_env = "production"
  let instance_id = "i-1234567890abcdef0"
  
  // 创建资源属性
  let resource_attributes = [
    ("service.name", service_name),
    ("service.version", service_version),
    ("deployment.environment", deployment_env),
    ("service.instance.id", instance_id)
  ]
  
  // 验证资源属性
  assert_eq(resource_attributes.length(), 4)
  assert_eq(resource_attributes[0].0, "service.name")
  assert_eq(resource_attributes[0].1, service_name)
  assert_eq(resource_attributes[3].0, "service.instance.id")
  assert_eq(resource_attributes[3].1, instance_id)
  
  // 创建共享资源字符串
  let mut resource_string = ""
  let mut i = 0
  while i < resource_attributes.length() {
    let (key, value) = resource_attributes[i]
    if i > 0 {
      resource_string = resource_string + ","
    }
    resource_string = resource_string + key + "=" + value
    i = i + 1
  }
  
  // 验证共享资源格式
  assert_eq(resource_string.contains("service.name=payment-service"), true)
  assert_eq(resource_string.contains("service.version=1.2.3"), true)
  assert_eq(resource_string.contains("deployment.environment=production"), true)
  assert_eq(resource_string.contains("service.instance.id=i-1234567890abcdef0"), true)
  assert_eq(resource_string.split(",").length(), 4)
}

test "span_event_metric_correlation" {
  // 测试Span事件与指标的关联
  
  let span_name = "database_query"
  let event_name = "query_execution"
  let metric_name = "db_query_duration"
  let event_duration = 23.7
  let metric_value = 23.7
  
  // 创建Span事件
  let span_event = span_name + ":" + event_name + "=" + event_duration.to_string() + "ms"
  assert_eq(span_event.contains(span_name), true)
  assert_eq(span_event.contains(event_name), true)
  assert_eq(span_event.contains("23.7"), true)
  
  // 创建关联指标
  let correlated_metric = metric_name + "{" + "span=" + span_name + ",event=" + event_name + "}=" + metric_value.to_string()
  assert_eq(correlated_metric.contains(metric_name), true)
  assert_eq(correlated_metric.contains("span=database_query"), true)
  assert_eq(correlated_metric.contains("event=query_execution"), true)
  assert_eq(correlated_metric.contains("23.7"), true)
  
  // 验证关联格式
  let metric_labels = correlated_metric.slice(correlated_metric.find("{").unwrap_or(0) + 1, 
                                            correlated_metric.find("}").unwrap_or(0))
  assert_eq(metric_labels.contains("span=database_query"), true)
  assert_eq(metric_labels.contains("event=query_execution"), true)
  assert_eq(metric_labels.split(",").length(), 2)
}