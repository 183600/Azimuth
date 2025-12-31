// 遥测资源生命周期管理测试用例
// 测试遥测系统中各种资源的创建、使用、清理和回收过程

test "telemetry_span_lifecycle_management" {
  // 测试Span生命周期管理
  
  let span_lifecycle = {
    "span_creation": {
      "trace_id": "12345678901234567890123456789012",
      "span_id": "abcdef1234567890",
      "parent_span_id": "0987654321098765",
      "operation_name": "http_request",
      "start_time": 1672531200000000000,
      "kind": "server",
      "attributes": {
        "http.method": "GET",
        "http.url": "https://api.example.com/users",
        "user.id": "user-123"
      }
    },
    "span_updates": [
      {
        "timestamp": 1672531200100000000,
        "type": "add_attribute",
        "data": {"key": "http.status_code", "value": 200}
      },
      {
        "timestamp": 1672531200150000000,
        "type": "add_event",
        "data": {
          "name": "db.query.start",
          "attributes": {"db.statement": "SELECT * FROM users"}
        }
      },
      {
        "timestamp": 1672531200200000000,
        "type": "add_event",
        "data": {
          "name": "db.query.complete",
          "attributes": {"db.rows_returned": 5}
        }
      },
      {
        "timestamp": 1672531200250000000,
        "type": "set_status",
        "data": {"code": 0, "message": "OK"}
      }
    ],
    "span_completion": {
      "end_time": 1672531200300000000,
      "duration_ms": 30,
      "final_status": "ok",
      "final_attributes": {
        "http.method": "GET",
        "http.url": "https://api.example.com/users",
        "user.id": "user-123",
        "http.status_code": 200
      },
      "events_count": 2,
      "total_attributes": 4
    }
  }
  
  // 验证Span创建
  let span_creation = span_lifecycle.get("span_creation", {})
  assert_eq(span_creation.get("trace_id", ""), "12345678901234567890123456789012")
  assert_eq(span_creation.get("span_id", ""), "abcdef1234567890")
  assert_eq(span_creation.get("operation_name", ""), "http_request")
  assert_eq(span_creation.get("kind", ""), "server")
  
  let initial_attributes = span_creation.get("attributes", {})
  assert_eq(initial_attributes.get("http.method", ""), "GET")
  assert_eq(initial_attributes.get("http.url", ""), "https://api.example.com/users")
  assert_eq(initial_attributes.get("user.id", ""), "user-123")
  
  // 验证Span更新
  let span_updates = span_lifecycle.get("span_updates", [])
  assert_eq(span_updates.length(), 4)
  
  // 验证属性更新
  let attribute_update = span_updates[0]
  assert_eq(attribute_update.get("type", ""), "add_attribute")
  assert_eq(attribute_update.get("timestamp", 0), 1672531200100000000)
  
  let attribute_data = attribute_update.get("data", {})
  assert_eq(attribute_data.get("key", ""), "http.status_code")
  assert_eq(attribute_data.get("value", 0), 200)
  
  // 验证事件添加
  let event_update = span_updates[1]
  assert_eq(event_update.get("type", ""), "add_event")
  let event_data = event_update.get("data", {})
  assert_eq(event_data.get("name", ""), "db.query.start")
  
  // 验证状态设置
  let status_update = span_updates[3]
  assert_eq(status_update.get("type", ""), "set_status")
  let status_data = status_update.get("data", {})
  assert_eq(status_data.get("code", 0), 0)
  assert_eq(status_data.get("message", ""), "OK")
  
  // 验证Span完成
  let span_completion = span_lifecycle.get("span_completion", {})
  assert_eq(span_completion.get("end_time", 0), 1672531200300000000)
  assert_eq(span_completion.get("duration_ms", 0), 30)
  assert_eq(span_completion.get("final_status", ""), "ok")
  
  let final_attributes = span_completion.get("final_attributes", {})
  assert_eq(final_attributes.get("http.status_code", 0), 200)
  assert_eq(span_completion.get("events_count", 0), 2)
  assert_eq(span_completion.get("total_attributes", 0), 4)
  
  // 验证生命周期状态转换
  let lifecycle_states = [
    {"state": "created", "timestamp": 1672531200000000000},
    {"state": "active", "timestamp": 1672531200000000001},
    {"state": "updating", "timestamp": 1672531200100000000},
    {"state": "updating", "timestamp": 1672531200150000000},
    {"state": "updating", "timestamp": 1672531200200000000},
    {"state": "updating", "timestamp": 1672531200250000000},
    {"state": "finished", "timestamp": 1672531200300000000}
  ]
  
  // 验证状态序列
  assert_eq(lifecycle_states.length(), 7)
  assert_eq(lifecycle_states[0].get("state", ""), "created")
  assert_eq(lifecycle_states[1].get("state", ""), "active")
  assert_eq(lifecycle_states[lifecycle_states.length() - 1].get("state", ""), "finished")
  
  // 验证状态转换的合理性
  let mut i = 1
  while i < lifecycle_states.length() {
    let prev_state = lifecycle_states[i - 1].get("state", "")
    let curr_state = lifecycle_states[i].get("state", "")
    let prev_time = lifecycle_states[i - 1].get("timestamp", 0)
    let curr_time = lifecycle_states[i].get("timestamp", 0)
    
    // 时间应该是递增的
    assert_eq(curr_time >= prev_time, true)
    
    // 状态转换应该是合理的
    if prev_state == "finished" {
      assert_eq(curr_state, "finished")  // finished后不能再转换
    }
    
    i = i + 1
  }
}

test "telemetry_metric_lifecycle_management" {
  // 测试Metric生命周期管理
  
  let metric_lifecycle = {
    "metric_creation": {
      "name": "http_requests_total",
      "description": "Total number of HTTP requests",
      "unit": "1",
      "type": "counter",
      "instrumentation_scope": {
        "name": "http.server",
        "version": "1.0.0"
      },
      "attributes": {
        "method": "GET",
        "status": "200"
      }
    },
    "metric_updates": [
      {
        "timestamp": 1672531200000000000,
        "type": "record",
        "value": 1,
        "attributes": {"method": "GET", "status": "200"}
      },
      {
        "timestamp": 1672531200050000000,
        "type": "record",
        "value": 1,
        "attributes": {"method": "GET", "status": "200"}
      },
      {
        "timestamp": 1672531200100000000,
        "type": "record",
        "value": 1,
        "attributes": {"method": "POST", "status": "201"}
      },
      {
        "timestamp": 1672531200150000000,
        "type": "record",
        "value": 1,
        "attributes": {"method": "GET", "status": "404"}
      }
    ],
    "metric_aggregation": {
      "timestamp": 1672531200200000000,
      "data_points": [
        {
          "attributes": {"method": "GET", "status": "200"},
          "value": 2,
          "start_time": 1672531200000000000,
          "end_time": 1672531200200000000
        },
        {
          "attributes": {"method": "POST", "status": "201"},
          "value": 1,
          "start_time": 1672531200100000000,
          "end_time": 1672531200200000000
        },
        {
          "attributes": {"method": "GET", "status": "404"},
          "value": 1,
          "start_time": 1672531200150000000,
          "end_time": 1672531200200000000
        }
      ],
      "total_value": 4
    },
    "metric_export": {
      "timestamp": 1672531200250000000,
      "export_format": "otlp",
      "data_points_exported": 3,
      "export_success": true
    }
  }
  
  // 验证Metric创建
  let metric_creation = metric_lifecycle.get("metric_creation", {})
  assert_eq(metric_creation.get("name", ""), "http_requests_total")
  assert_eq(metric_creation.get("description", ""), "Total number of HTTP requests")
  assert_eq(metric_creation.get("type", ""), "counter")
  
  let scope = metric_creation.get("instrumentation_scope", {})
  assert_eq(scope.get("name", ""), "http.server")
  assert_eq(scope.get("version", ""), "1.0.0")
  
  // 验证Metric记录
  let metric_updates = metric_lifecycle.get("metric_updates", [])
  assert_eq(metric_updates.length(), 4)
  
  // 验证第一次记录
  let first_record = metric_updates[0]
  assert_eq(first_record.get("type", ""), "record")
  assert_eq(first_record.get("value", 0), 1)
  
  let first_attributes = first_record.get("attributes", {})
  assert_eq(first_attributes.get("method", ""), "GET")
  assert_eq(first_attributes.get("status", ""), "200")
  
  // 验证Metric聚合
  let metric_aggregation = metric_lifecycle.get("metric_aggregation", {})
  let data_points = metric_aggregation.get("data_points", [])
  assert_eq(data_points.length(), 3)
  
  // 验证GET 200的数据点
  let get_200_point = data_points[0]
  assert_eq(get_200_point.get("value", 0), 2)
  let get_200_attrs = get_200_point.get("attributes", {})
  assert_eq(get_200_attrs.get("method", ""), "GET")
  assert_eq(get_200_attrs.get("status", ""), "200")
  
  // 验证聚合总值
  assert_eq(metric_aggregation.get("total_value", 0), 4)
  
  // 验证Metric导出
  let metric_export = metric_lifecycle.get("metric_export", {})
  assert_eq(metric_export.get("export_format", ""), "otlp")
  assert_eq(metric_export.get("data_points_exported", 0), 3)
  assert_eq(metric_export.get("export_success", false), true)
  
  // 测试Histogram类型的Metric生命周期
  let histogram_lifecycle = {
    "creation": {
      "name": "http_request_duration_seconds",
      "type": "histogram",
      "unit": "s",
      "bucket_boundaries": [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]
    },
    "records": [
      {"timestamp": 1672531200000000000, "value": 0.023, "attributes": {"method": "GET"}},
      {"timestamp": 1672531200050000000, "value": 0.156, "attributes": {"method": "POST"}},
      {"timestamp": 1672531200100000000, "value": 0.007, "attributes": {"method": "GET"}},
      {"timestamp": 1672531200150000000, "value": 1.234, "attributes": {"method": "GET"}}
    ],
    "aggregation": {
      "data_points": [
        {
          "attributes": {"method": "GET"},
          "bucket_counts": [0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1],
          "count": 3,
          "sum": 1.264
        },
        {
          "attributes": {"method": "POST"},
          "bucket_counts": [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
          "count": 1,
          "sum": 0.156
        }
      ]
    }
  }
  
  // 验证Histogram创建
  let histogram_creation = histogram_lifecycle.get("creation", {})
  assert_eq(histogram_creation.get("type", ""), "histogram")
  let boundaries = histogram_creation.get("bucket_boundaries", [])
  assert_eq(boundaries.length(), 11)
  assert_eq(boundaries[0], 0.005)
  assert_eq(boundaries[10], 10.0)
  
  // 验证Histogram记录
  let histogram_records = histogram_lifecycle.get("records", [])
  assert_eq(histogram_records.length(), 4)
  
  // 验证Histogram聚合
  let histogram_aggregation = histogram_lifecycle.get("aggregation", {})
  let histogram_data_points = histogram_aggregation.get("data_points", [])
  assert_eq(histogram_data_points.length(), 2)
  
  let get_histogram = histogram_data_points[0]
  assert_eq(get_histogram.get("count", 0), 3)
  assert_eq(get_histogram.get("sum", 0.0), 1.264)
  
  let get_bucket_counts = get_histogram.get("bucket_counts", [])
  assert_eq(get_bucket_counts.length(), 12)  // 11 boundaries + overflow bucket
  assert_eq(get_bucket_counts[3], 1)  // 0.023在0.025 bucket中
  assert_eq(get_bucket_counts[10], 1)  // 1.234在overflow bucket中
}

test "telemetry_logger_lifecycle_management" {
  // 测试Logger生命周期管理
  
  let logger_lifecycle = {
    "logger_creation": {
      "name": "application.logger",
      "version": "1.0.0",
      "instrumentation_scope": {
        "name": "my-app",
        "version": "2.1.0",
        "schema_url": "https://example.com/schema/v1"
      },
      "attributes": {
        "service.name": "order-service",
        "service.version": "1.5.0",
        "deployment.environment": "production"
      }
    },
    "log_records": [
      {
        "timestamp": 1672531200000000000,
        "observed_timestamp": 1672531200000000000,
        "severity_number": 9,  // INFO
        "severity_text": "INFO",
        "body": "Processing order request",
        "attributes": {
          "order_id": "order-123",
          "user_id": "user-456",
          "request_id": "req-789"
        },
        "trace_id": "abcdef1234567890abcdef1234567890",
        "span_id": "1234567890123456",
        "trace_flags": 1
      },
      {
        "timestamp": 1672531200050000000,
        "observed_timestamp": 1672531200050000000,
        "severity_number": 17,  // DEBUG
        "severity_text": "DEBUG",
        "body": "Validating user permissions",
        "attributes": {
          "order_id": "order-123",
          "user_id": "user-456",
          "permission": "order:read"
        },
        "trace_id": "abcdef1234567890abcdef1234567890",
        "span_id": "1234567890123456",
        "trace_flags": 1
      },
      {
        "timestamp": 1672531200100000000,
        "observed_timestamp": 1672531200100000000,
        "severity_number": 13,  // WARN
        "severity_text": "WARN",
        "body": "Inventory running low for product",
        "attributes": {
          "order_id": "order-123",
          "product_id": "prod-789",
          "available_quantity": 5,
          "requested_quantity": 10
        },
        "trace_id": "abcdef1234567890abcdef1234567890",
        "span_id": "1234567890123456",
        "trace_flags": 1
      },
      {
        "timestamp": 1672531200150000000,
        "observed_timestamp": 1672531200150000000,
        "severity_number": 17,  // ERROR
        "severity_text": "ERROR",
        "body": "Failed to process payment",
        "attributes": {
          "order_id": "order-123",
          "payment_gateway": "stripe",
          "error_code": "card_declined",
          "retry_count": 2
        },
        "trace_id": "abcdef1234567890abcdef1234567890",
        "span_id": "1234567890123456",
        "trace_flags": 1
      }
    ],
    "log_batching": {
      "batch_size": 4,
      "batch_created_at": 1672531200200000000,
      "batch_timeout_ms": 5000,
      "compression_enabled": true,
      "compression_ratio": 0.65
    },
    "log_export": {
      "timestamp": 1672531200250000000,
      "exporter": "otlp_http",
      "endpoint": "https://otel-collector.example.com:4318/v1/logs",
      "success": true,
      "duration_ms": 45,
      "records_exported": 4
    }
  }
  
  // 验证Logger创建
  let logger_creation = logger_lifecycle.get("logger_creation", {})
  assert_eq(logger_creation.get("name", ""), "application.logger")
  assert_eq(logger_creation.get("version", ""), "1.0.0")
  
  let scope = logger_creation.get("instrumentation_scope", {})
  assert_eq(scope.get("name", ""), "my-app")
  assert_eq(scope.get("version", ""), "2.1.0")
  assert_eq(scope.get("schema_url", ""), "https://example.com/schema/v1")
  
  // 验证日志记录
  let log_records = logger_lifecycle.get("log_records", [])
  assert_eq(log_records.length(), 4)
  
  // 验证INFO日志
  let info_log = log_records[0]
  assert_eq(info_log.get("severity_text", ""), "INFO")
  assert_eq(info_log.get("severity_number", 0), 9)
  assert_eq(info_log.get("body", ""), "Processing order request")
  
  let info_attributes = info_log.get("attributes", {})
  assert_eq(info_attributes.get("order_id", ""), "order-123")
  assert_eq(info_attributes.get("user_id", ""), "user-456")
  
  // 验证ERROR日志
  let error_log = log_records[3]
  assert_eq(error_log.get("severity_text", ""), "ERROR")
  assert_eq(error_log.get("severity_number", 0), 17)
  assert_eq(error_log.get("body", ""), "Failed to process payment")
  
  let error_attributes = error_log.get("attributes", {})
  assert_eq(error_attributes.get("error_code", ""), "card_declined")
  assert_eq(error_attributes.get("retry_count", 0), 2)
  
  // 验证日志批处理
  let log_batching = logger_lifecycle.get("log_batching", {})
  assert_eq(log_batching.get("batch_size", 0), 4)
  assert_eq(log_batching.get("batch_timeout_ms", 0), 5000)
  assert_eq(log_batching.get("compression_enabled", false), true)
  assert_eq(log_batching.get("compression_ratio", 0.0), 0.65)
  
  // 验证日志导出
  let log_export = logger_lifecycle.get("log_export", {})
  assert_eq(log_export.get("exporter", ""), "otlp_http")
  assert_eq(log_export.get("success", false), true)
  assert_eq(log_export.get("records_exported", 0), 4)
  
  // 验证日志级别统计
  let severity_stats = {}
  let mut i = 0
  while i < log_records.length() {
    let severity = log_records[i].get("severity_text", "")
    let current_count = severity_stats.get(severity, 0)
    severity_stats.set(severity, current_count + 1)
    i = i + 1
  }
  
  assert_eq(severity_stats.get("INFO", 0), 1)
  assert_eq(severity_stats.get("DEBUG", 0), 1)
  assert_eq(severity_stats.get("WARN", 0), 1)
  assert_eq(severity_stats.get("ERROR", 0), 1)
  
  // 验证Trace关联
  let mut all_have_trace_id = true
  let mut all_have_span_id = true
  
  i = 0
  while i < log_records.length() {
    let log = log_records[i]
    if log.get("trace_id", "") == "" {
      all_have_trace_id = false
    }
    if log.get("span_id", "") == "" {
      all_have_span_id = false
    }
    i = i + 1
  }
  
  assert_eq(all_have_trace_id, true)
  assert_eq(all_have_span_id, true)
}

test "telemetry_resource_cleanup_and_gc" {
  // 测试资源清理和垃圾回收
  
  let resource_management = {
    "active_resources": {
      "spans": {
        "active_count": 25,
        "max_concurrent": 100,
        "pool_size": 1000,
        "memory_usage_mb": 45
      },
      "metrics": {
        "active_instruments": 12,
        "active_data_points": 156,
        "pool_size": 2000,
        "memory_usage_mb": 78
      },
      "logs": {
        "active_loggers": 8,
        "pending_records": 234,
        "pool_size": 500,
        "memory_usage_mb": 32
      }
    },
    "gc_triggers": [
      {
        "trigger_type": "memory_threshold",
        "threshold_mb": 200,
        "current_usage_mb": 210,
        "triggered_at": 1672531200000000000
      },
      {
        "trigger_type": "time_based",
        "interval_seconds": 300,
        "last_gc": 1672530900000000000,
        "triggered_at": 1672531200000000000
      },
      {
        "trigger_type": "resource_pressure",
        "metric": "span_pool_utilization",
        "threshold": 0.8,
        "current_value": 0.85,
        "triggered_at": 1672531200000000000
      }
    ],
    "gc_operations": [
      {
        "operation": "span_cleanup",
        "started_at": 1672531200010000000,
        "completed_at": 1672531200050000000,
        "duration_ms": 4000,
        "items_processed": 75,
        "memory_freed_mb": 12,
        "success": true
      },
      {
        "operation": "metric_data_compaction",
        "started_at": 1672531200060000000,
        "completed_at": 1672531200080000000,
        "duration_ms": 2000,
        "data_points_compacted": 45,
        "memory_freed_mb": 8,
        "success": true
      },
      {
        "operation": "log_buffer_flush",
        "started_at": 1672531200090000000,
        "completed_at": 1672531200100000000,
        "duration_ms": 1000,
        "records_flushed": 234,
        "memory_freed_mb": 15,
        "success": true
      }
    ],
    "post_gc_state": {
      "spans": {
        "active_count": 18,
        "max_concurrent": 100,
        "pool_size": 1000,
        "memory_usage_mb": 33
      },
      "metrics": {
        "active_instruments": 12,
        "active_data_points": 111,
        "pool_size": 2000,
        "memory_usage_mb": 70
      },
      "logs": {
        "active_loggers": 8,
        "pending_records": 0,
        "pool_size": 500,
        "memory_usage_mb": 17
      }
    }
  }
  
  // 验证资源使用状态
  let active_resources = resource_management.get("active_resources", {})
  let spans = active_resources.get("spans", {})
  let metrics = active_resources.get("metrics", {})
  let logs = active_resources.get("logs", {})
  
  assert_eq(spans.get("active_count", 0), 25)
  assert_eq(spans.get("memory_usage_mb", 0), 45)
  assert_eq(metrics.get("active_instruments", 0), 12)
  assert_eq(metrics.get("memory_usage_mb", 0), 78)
  assert_eq(logs.get("pending_records", 0), 234)
  assert_eq(logs.get("memory_usage_mb", 0), 32)
  
  // 计算总内存使用
  let total_memory_before = spans.get("memory_usage_mb", 0) + 
                           metrics.get("memory_usage_mb", 0) + 
                           logs.get("memory_usage_mb", 0)
  assert_eq(total_memory_before, 155)
  
  // 验证GC触发器
  let gc_triggers = resource_management.get("gc_triggers", [])
  assert_eq(gc_triggers.length(), 3)
  
  // 验证内存阈值触发器
  let memory_trigger = gc_triggers[0]
  assert_eq(memory_trigger.get("trigger_type", ""), "memory_threshold")
  assert_eq(memory_trigger.get("threshold_mb", 0), 200)
  assert_eq(memory_trigger.get("current_usage_mb", 0), 210)
  
  // 验证GC操作
  let gc_operations = resource_management.get("gc_operations", [])
  assert_eq(gc_operations.length(), 3)
  
  // 验证Span清理操作
  let span_cleanup = gc_operations[0]
  assert_eq(span_cleanup.get("operation", ""), "span_cleanup")
  assert_eq(span_cleanup.get("duration_ms", 0), 4000)
  assert_eq(span_cleanup.get("items_processed", 0), 75)
  assert_eq(span_cleanup.get("memory_freed_mb", 0), 12)
  assert_eq(span_cleanup.get("success", false), true)
  
  // 验证Metric数据压缩操作
  let metric_compaction = gc_operations[1]
  assert_eq(metric_compaction.get("operation", ""), "metric_data_compaction")
  assert_eq(metric_compaction.get("data_points_compacted", 0), 45)
  assert_eq(metric_compaction.get("memory_freed_mb", 0), 8)
  
  // 验证GC后的状态
  let post_gc_state = resource_management.get("post_gc_state", {})
  let post_spans = post_gc_state.get("spans", {})
  let post_metrics = post_gc_state.get("metrics", {})
  let post_logs = post_gc_state.get("logs", {})
  
  assert_eq(post_spans.get("active_count", 0), 18)  // 减少了7个
  assert_eq(post_spans.get("memory_usage_mb", 0), 33)  // 减少了12MB
  
  assert_eq(post_metrics.get("active_data_points", 0), 111)  // 减少了45个
  assert_eq(post_metrics.get("memory_usage_mb", 0), 70)  // 减少了8MB
  
  assert_eq(post_logs.get("pending_records", 0), 0)  // 全部刷新
  assert_eq(post_logs.get("memory_usage_mb", 0), 17)  // 减少了15MB
  
  // 验证总内存释放
  let total_memory_after = post_spans.get("memory_usage_mb", 0) + 
                          post_metrics.get("memory_usage_mb", 0) + 
                          post_logs.get("memory_usage_mb", 0)
  assert_eq(total_memory_after, 120)
  
  let total_memory_freed = total_memory_before - total_memory_after
  assert_eq(total_memory_freed, 35)
  
  // 验证GC操作统计
  let mut total_items_processed = 0
  let mut total_memory_freed_by_gc = 0
  let mut total_gc_duration = 0
  
  let mut i = 0
  while i < gc_operations.length() {
    let operation = gc_operations[i]
    total_items_processed = total_items_processed + operation.get("items_processed", 0)
    total_memory_freed_by_gc = total_memory_freed_by_gc + operation.get("memory_freed_mb", 0)
    total_gc_duration = total_gc_duration + operation.get("duration_ms", 0)
    i = i + 1
  }
  
  assert_eq(total_items_processed, 75 + 45 + 234)  // span items + metric points + log records
  assert_eq(total_memory_freed_by_gc, 12 + 8 + 15)
  assert_eq(total_gc_duration, 4000 + 2000 + 1000)
  
  // 测试资源池管理
  let resource_pools = {
    "span_pool": {
      "initial_size": 1000,
      "current_size": 1000,
      "in_use": 18,
      "available": 982,
      "expansion_threshold": 0.9,
      "shrink_threshold": 0.3,
      "max_size": 5000
    },
    "metric_pool": {
      "initial_size": 2000,
      "current_size": 2000,
      "in_use": 111,
      "available": 1889,
      "expansion_threshold": 0.8,
      "shrink_threshold": 0.4,
      "max_size": 10000
    },
    "log_pool": {
      "initial_size": 500,
      "current_size": 500,
      "in_use": 0,
      "available": 500,
      "expansion_threshold": 0.85,
      "shrink_threshold": 0.2,
      "max_size": 2000
    }
  }
  
  // 验证资源池状态
  let span_pool = resource_pools.get("span_pool", {})
  let span_utilization = span_pool.get("in_use", 0).to_double() / span_pool.get("current_size", 0).to_double()
  assert_eq(span_utilization, 0.018)  // 18/1000 = 1.8%
  
  let metric_pool = resource_pools.get("metric_pool", {})
  let metric_utilization = metric_pool.get("in_use", 0).to_double() / metric_pool.get("current_size", 0).to_double()
  assert_eq(metric_utilization, 0.0555)  // 111/2000 = 5.55%
  
  // 验证是否需要池扩展或收缩
  let span_expand_threshold = span_pool.get("expansion_threshold", 0.0)
  let span_shrink_threshold = span_pool.get("shrink_threshold", 0.0)
  
  assert_eq(span_utilization < span_shrink_threshold, true)  // 应该考虑收缩
  assert_eq(span_utilization < span_expand_threshold, true)  // 不需要扩展
  
  // 模拟池收缩决策
  let pool_adjustments = []
  
  if span_utilization < span_shrink_threshold {
    pool_adjustments.push({
      "pool": "span_pool",
      "action": "shrink",
      "current_size": span_pool.get("current_size", 0),
      "target_size": span_pool.get("current_size", 0) / 2,  // 收缩到一半
      "reason": "Low utilization"
    })
  }
  
  if metric_utilization < metric_pool.get("shrink_threshold", 0.0) {
    pool_adjustments.push({
      "pool": "metric_pool",
      "action": "shrink",
      "current_size": metric_pool.get("current_size", 0),
      "target_size": metric_pool.get("current_size", 0) * 3 / 4,  // 收缩到75%
      "reason": "Low utilization"
    })
  }
  
  // 验证池调整决策
  assert_eq(pool_adjustments.length(), 2)  // 两个池都需要收缩
  
  let span_adjustment = pool_adjustments[0]
  assert_eq(span_adjustment.get("pool", ""), "span_pool")
  assert_eq(span_adjustment.get("action", ""), "shrink")
  assert_eq(span_adjustment.get("target_size", 0), 500)
  
  // 生成资源管理报告
  let resource_report = generate_resource_management_report(resource_management, resource_pools)
  
  assert_eq(resource_report.get("total_memory_before_gc_mb", 0), 155)
  assert_eq(resource_report.get("total_memory_after_gc_mb", 0), 120)
  assert_eq(resource_report.get("memory_freed_mb", 0), 35)
  assert_eq(resource_report.get("gc_efficiency_score", 0.0), 87.5)
  assert_eq(resource_report.get("pool_optimization_recommendations", []).length(), 2)
}

// 辅助函数：生成资源管理报告
fn generate_resource_management_report(resource_management, resource_pools) {
  let active_resources = resource_management.get("active_resources", {})
  let post_gc_state = resource_management.get("post_gc_state", {})
  
  let spans_before = active_resources.get("spans", {})
  let metrics_before = active_resources.get("metrics", {})
  let logs_before = active_resources.get("logs", {})
  
  let spans_after = post_gc_state.get("spans", {})
  let metrics_after = post_gc_state.get("metrics", {})
  let logs_after = post_gc_state.get("logs", {})
  
  let total_memory_before = spans_before.get("memory_usage_mb", 0) + 
                           metrics_before.get("memory_usage_mb", 0) + 
                           logs_before.get("memory_usage_mb", 0)
  
  let total_memory_after = spans_after.get("memory_usage_mb", 0) + 
                          metrics_after.get("memory_usage_mb", 0) + 
                          logs_after.get("memory_usage_mb", 0)
  
  let memory_freed = total_memory_before - total_memory_after
  let gc_efficiency = if total_memory_before > 0 { 
    (memory_freed.to_double() / total_memory_before.to_double()) * 100.0 
  } else { 
    0.0 
  }
  
  // 生成池优化建议
  let recommendations = []
  let span_pool = resource_pools.get("span_pool", {})
  let metric_pool = resource_pools.get("metric_pool", {})
  
  let span_utilization = span_pool.get("in_use", 0).to_double() / span_pool.get("current_size", 0).to_double()
  let metric_utilization = metric_pool.get("in_use", 0).to_double() / metric_pool.get("current_size", 0).to_double()
  
  if span_utilization < span_pool.get("shrink_threshold", 0.0) {
    recommendations.push({
      "pool": "span_pool",
      "recommendation": "shrink",
      "current_utilization": span_utilization,
      "target_size": span_pool.get("current_size", 0) / 2
    })
  }
  
  if metric_utilization < metric_pool.get("shrink_threshold", 0.0) {
    recommendations.push({
      "pool": "metric_pool",
      "recommendation": "shrink",
      "current_utilization": metric_utilization,
      "target_size": metric_pool.get("current_size", 0) * 3 / 4
    })
  }
  
  {
    "total_memory_before_gc_mb": total_memory_before,
    "total_memory_after_gc_mb": total_memory_after,
    "memory_freed_mb": memory_freed,
    "gc_efficiency_score": gc_efficiency,
    "pool_optimization_recommendations": recommendations
  }
}