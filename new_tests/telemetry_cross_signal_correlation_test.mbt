// Azimuth Telemetry - Cross Signal Correlation Test
// 测试跨遥测信号（Trace、Metrics、Logs）的关联性

test "cross_signal_trace_metrics_correlation" {
  // 测试Trace和Metrics信号的关联性
  let trace_id = "1234567890123456"
  let span_id = "12345678"
  let operation_name = "test_operation"
  
  // 模拟Span数据
  let span_data = {
    "name": operation_name,
    "trace_id": trace_id,
    "span_id": span_id,
    "operation.type": "test"
  }
  
  // 模拟Metrics数据
  let metric_data = [
    ("trace.id", trace_id),
    ("span.id", span_id),
    ("operation.name", operation_name)
  ]
  
  // 验证关联性
  assert_eq(span_data["name"], "test_operation")
  assert_eq(metric_data.length(), 3)
  
  // 验证Trace ID在Metrics中的正确表示
  let trace_id_found = metric_data.any(fn((key, value)) { 
    key == "trace.id" && value == trace_id 
  })
  assert_true(trace_id_found)
  
  // 验证Span ID在Metrics中的正确表示
  let span_id_found = metric_data.any(fn((key, value)) { 
    key == "span.id" && value == span_id 
  })
  assert_true(span_id_found)
}

test "cross_signal_trace_logs_correlation" {
  // 测试Trace和Logs信号的关联性
  let trace_id = "1098765432109876"
  let span_id = "10987654"
  
  // 模拟Span事件数据
  let span_event = {
    "name": "log_event",
    "timestamp": "1500000",
    "log.level": "INFO",
    "log.message": "Operation completed successfully"
  }
  
  // 模拟包含事件信息的Span
  let span_data = {
    "name": "operation_with_logs",
    "trace_id": trace_id,
    "span_id": span_id,
    "events": [span_event]
  }
  
  // 验证Span事件与日志的关联性
  assert_eq(span_data["events"].length(), 1)
  let event = span_data["events"][0]
  assert_eq(event["name"], "log_event")
  assert_eq(event["timestamp"], "1500000")
  
  // 验证日志属性
  assert_eq(event["log.level"], "INFO")
  assert_eq(event["log.message"], "Operation completed successfully")
}

test "cross_signal_metrics_logs_correlation" {
  // 测试Metrics和Logs信号的关联性
  let metric_attributes = [
    ("service.name", "test_service"),
    ("operation.type", "batch_processing"),
    ("batch.size", "100"),
    ("log.correlation.id", "log-12345")
  ]
  
  // 模拟日志记录
  let log_attributes = [
    ("service.name", "test_service"),
    ("operation.type", "batch_processing"),
    ("batch.size", "100"),
    ("log.correlation.id", "log-12345"),
    ("log.level", "WARN"),
    ("log.message", "Batch processing approaching limits")
  ]
  
  // 验证Metrics和Logs的关联性
  let common_attrs = metric_attributes.filter(fn((key, _)) {
    log_attributes.any(fn((log_key, _)) { log_key == key })
  })
  
  assert_eq(common_attrs.length(), 4) // service.name, operation.type, batch.size, log.correlation.id
  
  // 验证关联ID的一致性
  let correlation_id_metric = metric_attributes.find(fn((key, value)) { key == "log.correlation.id" })
  let correlation_id_log = log_attributes.find(fn((key, value)) { key == "log.correlation.id" })
  
  assert_some(correlation_id_metric)
  assert_some(correlation_id_log)
  
  let (_, metric_id_value) = correlation_id_metric.unwrap()
  let (_, log_id_value) = correlation_id_log.unwrap()
  
  assert_eq(metric_id_value, log_id_value)
  assert_eq(metric_id_value, "log-12345")
}

test "cross_signal_comprehensive_correlation" {
  // 测试三个信号的综合关联性
  let trace_id = "fffefdfcfbfaf9f8f7f6f5f4f3f2f1f0"
  let span_id = "fffefdfcfbfaf9f8"
  
  // 共同的属性键
  let common_attributes = [
    ("service.name", "comprehensive_test"),
    ("service.version", "1.0.0"),
    ("environment", "testing")
  ]
  
  // Trace信号
  let span_data = {
    "name": "comprehensive_operation",
    "trace_id": trace_id,
    "span_id": span_id,
    "attributes": common_attributes,
    "events": [
      {
        "name": "operation_start",
        "timestamp": "5000000",
        "event.type": "start",
        "log.level": "INFO"
      },
      {
        "name": "operation_end", 
        "timestamp": "8000000",
        "event.type": "end",
        "log.level": "INFO",
        "duration.ms": "3"
      }
    ]
  }
  
  // Metrics信号
  let metric_attributes = common_attributes + [
    ("trace.id", trace_id),
    ("span.id", span_id),
    ("operation.duration", "3.0")
  ]
  
  // Logs信号（通过Span事件体现）
  let log_events = span_data["events"]
  
  // 验证综合关联性
  assert_eq(span_data["attributes"].length(), 3) // 三个共同属性
  assert_eq(metric_attributes.length(), 6) // 三个共同属性 + 三个关联属性
  assert_eq(log_events.length(), 2) // 两个日志事件
  
  // 验证所有信号都包含共同的服务标识
  let service_name_in_span = span_data["attributes"].any(fn((key, value)) { 
    key == "service.name" && value == "comprehensive_test" 
  })
  let service_name_in_metrics = metric_attributes.any(fn((key, value)) { 
    key == "service.name" && value == "comprehensive_test" 
  })
  
  assert_true(service_name_in_span)
  assert_true(service_name_in_metrics)
  
  // 验证时间序列的一致性
  assert_eq(log_events[0]["timestamp"], "5000000")
  assert_eq(log_events[1]["timestamp"], "8000000")
}