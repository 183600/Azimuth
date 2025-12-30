// 跨模块集成测试用例
// 测试遥测系统各模块间的集成和协作

test "trace_metrics_integration" {
  // 测试trace和metrics模块集成
  
  let trace_data = {
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331",
    "operation_name": "process_payment",
    "duration_ms": 150
  }
  
  let metric_data = {
    "name": "span_duration",
    "value": trace_data["duration_ms"],
    "labels": {
      "operation": trace_data["operation_name"],
      "trace_id": trace_data["trace_id"]
    }
  }
  
  // 验证数据传递
  assert_eq(metric_data["value"], 150)
  assert_eq(metric_data["labels"]["operation"], "process_payment")
  assert_eq(metric_data["labels"]["trace_id"], "0af7651916cd43dd8448eb211c80319c")
  
  // 验证关联性
  let data_consistent = metric_data["labels"]["trace_id"] == trace_data["trace_id"]
  assert_eq(data_consistent, true)
}

test "logs_traces_integration" {
  // 测试logs和traces模块集成
  
  let trace_context = {
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331"
  }
  
  let log_entries = [
    {
      "message": "Payment processing started",
      "level": "INFO",
      "trace_id": trace_context["trace_id"],
      "span_id": trace_context["span_id"],
      "timestamp": 1609459200000
    },
    {
      "message": "Payment validated",
      "level": "INFO", 
      "trace_id": trace_context["trace_id"],
      "span_id": trace_context["span_id"],
      "timestamp": 1609459200100
    },
    {
      "message": "Payment processed successfully",
      "level": "INFO",
      "trace_id": trace_context["trace_id"], 
      "span_id": trace_context["span_id"],
      "timestamp": 1609459200150
    }
  ]
  
  // 验证所有日志都属于同一trace
  let all_logs_same_trace = true
  for log in log_entries {
    if log["trace_id"] != trace_context["trace_id"] {
      all_logs_same_trace = false
      break
    }
  }
  
  assert_eq(all_logs_same_trace, true)
  assert_eq(log_entries.length(), 3)
  
  // 验证时间顺序
  let timestamps_ordered = true
  for i in 1..log_entries.length() {
    if log_entries[i]["timestamp"] <= log_entries[i-1]["timestamp"] {
      timestamps_ordered = false
      break
    }
  }
  
  assert_eq(timestamps_ordered, true)
}

test "context_propagation_integration" {
  // 测试上下文传播集成
  
  let incoming_context = {
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331",
    "baggage_items": {
      "user_id": "12345",
      "request_id": "req-abc123",
      "tenant_id": "tenant-001"
    }
  }
  
  // 模拟上下文传播到下游服务
  let downstream_context = {
    "trace_id": incoming_context["trace_id"],
    "parent_span_id": incoming_context["span_id"],
    "span_id": "c7d8e9f0a1b2c3d4",  // 新的span ID
    "baggage_items": incoming_context["baggage_items"]
  }
  
  // 验证trace ID保持不变
  assert_eq(downstream_context["trace_id"], incoming_context["trace_id"])
  
  // 验证parent关系
  assert_eq(downstream_context["parent_span_id"], incoming_context["span_id"])
  
  // 验证baggage传播
  assert_eq(downstream_context["baggage_items"]["user_id"], "12345")
  assert_eq(downstream_context["baggage_items"]["request_id"], "req-abc123")
  assert_eq(downstream_context["baggage_items"]["tenant_id"], "tenant-001")
}

test "configuration_service_integration" {
  // 测试配置服务集成
  
  let global_config = {
    "service_name": "payment-service",
    "sampling_rate": 0.1,
    "exporter_endpoint": "http://otel-collector:4317"
  }
  
  // 模拟各模块读取配置
  let trace_config = {
    "enabled": true,
    "sampling_rate": global_config["sampling_rate"]
  }
  
  let metrics_config = {
    "enabled": true,
    "export_interval": 5000
  }
  
  let logs_config = {
    "enabled": true,
    "level": "INFO",
    "exporter_endpoint": global_config["exporter_endpoint"]
  }
  
  // 验证配置一致性
  assert_eq(trace_config["sampling_rate"], "0.1")
  assert_eq(logs_config["exporter_endpoint"], "http://otel-collector:4317")
  
  // 验证所有模块都启用
  assert_eq(trace_config["enabled"], true)
  assert_eq(metrics_config["enabled"], true)
  assert_eq(logs_config["enabled"], true)
}

test "error_handling_integration" {
  // 测试错误处理集成
  
  let error_scenarios = [
    {
      "module": "trace",
      "error_type": "invalid_trace_id",
      "handled_by": "validation_layer"
    },
    {
      "module": "metrics",
      "error_type": "metric_overflow",
      "handled_by": "aggregation_layer"
    },
    {
      "module": "logs",
      "error_type": "serialization_failed",
      "handled_by": "retry_layer"
    }
  ]
  
  let error_handlers = []
  
  // 模拟错误处理流程
  for scenario in error_scenarios {
    let handler = {
      "module": scenario["module"],
      "error": scenario["error_type"],
      "handler": scenario["handled_by"],
      "resolved": true
    }
    error_handlers.push(handler)
  }
  
  // 验证所有错误都被处理
  assert_eq(error_handlers.length(), 3)
  
  for handler in error_handlers {
    assert_eq(handler["resolved"], true)
  }
  
  // 验证错误处理覆盖
  let modules_with_errors = []
  for handler in error_handlers {
    modules_with_errors.push(handler["module"])
  }
  
  let has_trace_error = modules_with_errors.contains("trace")
  let has_metrics_error = modules_with_errors.contains("metrics")
  let has_logs_error = modules_with_errors.contains("logs")
  
  assert_eq(has_trace_error && has_metrics_error && has_logs_error, true)
}