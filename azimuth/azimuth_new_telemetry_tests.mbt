// New Telemetry Tests for Azimuth
// Comprehensive test cases covering telemetry system functionality

// Test 1: Trace context propagation with complex scenarios
pub test "trace context propagation complex scenarios" {
  // Test trace context creation with multiple spans
  let root_ctx = azimuth::SpanContext::new("root-trace-123", "root-span-456", true, "env=production,version=1.0")
  let child_ctx = azimuth::SpanContext::new("root-trace-123", "child-span-789", true, "env=production,version=1.0,service=api")
  
  // Verify trace ID consistency
  assert_eq(root_ctx.trace_id, child_ctx.trace_id)
  
  // Test baggage propagation
  let baggage = azimuth::Baggage::new()
  let enriched_baggage = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let final_baggage = azimuth::Baggage::set_entry(enrichied_baggage, "request.id", "req-67890")
  
  // Verify baggage entries
  let user_id = azimuth::Baggage::get_entry(final_baggage, "user.id")
  let request_id = azimuth::Baggage::get_entry(final_baggage, "request.id")
  assert_eq(user_id, Some("user-12345"))
  assert_eq(request_id, Some("req-67890"))
}

// Test 2: Metrics aggregation and time series operations
pub test "metrics aggregation time series" {
  // Create metrics instruments
  let counter = azimuth::metrics::Counter::new("http.requests.total", "Total HTTP requests", "requests")
  let histogram = azimuth::metrics::Histogram::new("http.request.duration", "HTTP request duration", "ms")
  let gauge = azimuth::metrics::Gauge::new("system.memory.usage", "System memory usage", "percent")
  
  // Record measurements
  azimuth::metrics::Counter::add(counter, 10, {"method": "GET", "status": "200"})
  azimuth::metrics::Counter::add(counter, 3, {"method": "POST", "status": "200"})
  azimuth::metrics::Counter::add(counter, 2, {"method": "GET", "status": "404"})
  
  azimuth::metrics::Histogram::record(histogram, 150.0, {"endpoint": "/api/users"})
  azimuth::metrics::Histogram::record(histogram, 75.0, {"endpoint": "/api/users"})
  azimuth::metrics::Histogram::record(histogram, 300.0, {"endpoint": "/api/orders"})
  
  azimuth::metrics::Gauge::set(gauge, 75.5, {"instance": "server-1"})
  azimuth::metrics::Gauge::set(gauge, 82.3, {"instance": "server-2"})
  
  // Verify metric collection
  let counter_data = azimuth::metrics::Counter::get_data(counter)
  assert_eq(counter_data.total, 15)
  
  let histogram_data = azimuth::metrics::Histogram::get_data(histogram)
  assert_eq(histogram_data.count, 3)
  assert_eq(histogram_data.sum, 525.0)
  
  let gauge_data = azimuth::metrics::Gauge::get_data(gauge)
  assert_eq(gauge_data.value, 82.3) // Latest recorded value
}

// Test 3: Log record structured logging with correlation
pub test "log record structured correlation" {
  // Create logger with structured logging
  let logger = azimuth::logging::Logger::new("azimuth.test", azimuth::logging::INFO)
  
  // Create span context for correlation
  let span_ctx = azimuth::SpanContext::new("log-trace-123", "log-span-456", true, "service=test-service")
  
  // Log with correlation context
  azimuth::logging::Logger::info(logger, "User login successful", {
    "user.id": "user-123",
    "trace.id": span_ctx.trace_id,
    "span.id": span_ctx.span_id,
    "ip.address": "192.168.1.100",
    "user.agent": "Mozilla/5.0"
  })
  
  azimuth::logging::Logger::warn(logger, "Rate limit approaching", {
    "trace.id": span_ctx.trace_id,
    "span.id": span_ctx.span_id,
    "current.rate": 95,
    "limit.rate": 100,
    "time.window": "1m"
  })
  
  azimuth::logging::Logger::error(logger, "Database connection failed", {
    "trace.id": span_ctx.trace_id,
    "span.id": span_ctx.span_id,
    "error.code": "DB_CONN_TIMEOUT",
    "retry.count": 3,
    "max.retries": 5
  })
  
  // Verify log records
  let logs = azimuth::logging::Logger::get_logs(logger)
  assert_eq(logs.length, 3)
  
  let info_log = logs[0]
  assert_eq(info_log.level, azimuth::logging::INFO)
  assert_eq(info_log.message, "User login successful")
  assert_eq(info_log.attributes["trace.id"], span_ctx.trace_id)
  
  let error_log = logs[2]
  assert_eq(error_log.level, azimuth::logging::ERROR)
  assert_eq(error_log.attributes["error.code"], "DB_CONN_TIMEOUT")
}

// Test 4: Resource management and merge strategies
pub test "resource management merge strategies" {
  // Create resources with different attributes
  let resource1 = azimuth::Resource::new({
    "service.name": "api-service",
    "service.version": "1.0.0",
    "deployment.environment": "production",
    "host.name": "api-server-1"
  })
  
  let resource2 = azimuth::Resource::new({
    "service.name": "api-service", // Same key
    "service.instance.id": "instance-12345",
    "telemetry.sdk.name": "azimuth",
    "telemetry.sdk.version": "0.1.0"
  })
  
  // Test resource merge strategy
  let merged_resource = azimuth::Resource::merge(resource1, resource2)
  
  // Verify merged attributes
  let merged_attrs = azimuth::Resource::get_attributes(merged_resource)
  assert_eq(merged_attrs["service.name"], "api-service")
  assert_eq(merged_attrs["service.version"], "1.0.0")
  assert_eq(merged_attrs["deployment.environment"], "production")
  assert_eq(merged_attrs["service.instance.id"], "instance-12345")
  assert_eq(merged_attrs["telemetry.sdk.name"], "azimuth")
  
  // Test resource with default attributes
  let default_resource = azimuth::Resource::default()
  let default_attrs = azimuth::Resource::get_attributes(default_resource)
  assert_eq(default_attrs["telemetry.sdk.name"], "azimuth")
  assert_eq(default_attrs["telemetry.sdk.language"], "moonbit")
}

// Test 5: Span event and link operations
pub test "span event link_operations" {
  // Create span with context
  let span_ctx = azimuth::SpanContext::new("event-trace-123", "event-span-456", true, "")
  let span = azimuth::Span::new("user-operation", azimuth::Internal, span_ctx)
  
  // Add events to span
  azimuth::Span::add_event(span, "operation.started", {
    "timestamp": "2024-01-01T10:00:00Z",
    "user.id": "user-123",
    "operation.type": "data.processing"
  })
  
  azimuth::Span::add_event(span, "cache.hit", {
    "timestamp": "2024-01-01T10:00:05Z",
    "cache.key": "user.profile.123",
    "hit.ratio": "0.95"
  })
  
  azimuth::Span::add_event(span, "operation.completed", {
    "timestamp": "2024-01-01T10:00:10Z",
    "duration.ms": "10000",
    "records.processed": "1500"
  })
  
  // Add links to related spans
  let related_ctx1 = azimuth::SpanContext::new("event-trace-123", "related-span-789", true, "")
  let related_ctx2 = azimuth::SpanContext::new("event-trace-123", "related-span-999", true, "")
  
  azimuth::Span::add_link(span, related_ctx1, {"relation.type": "parent"})
  azimuth::Span::add_link(span, related_ctx2, {"relation.type": "caused-by"})
  
  // Verify span events and links
  let events = azimuth::Span::get_events(span)
  assert_eq(events.length, 3)
  assert_eq(events[0].name, "operation.started")
  assert_eq(events[2].attributes["records.processed"], "1500")
  
  let links = azimuth::Span::get_links(span)
  assert_eq(links.length, 2)
  assert_eq(links[0].span_id, "related-span-789")
  assert_eq(links[1].attributes["relation.type"], "caused-by")
}

// Test 6: Sampling strategies and decision making
pub test "sampling strategies decision" {
  // Test different sampling strategies
  let always_on_sampler = azimuth::sampling::AlwaysOnSampler::new()
  let always_off_sampler = azimuth::sampling::AlwaysOffSampler::new()
  let trace_id_ratio_sampler = azimuth::sampling::TraceIdRatioSampler::new(0.5)
  
  // Create sampling context
  let sampling_ctx = azimuth::sampling::SamplingContext::new("trace-12345", "span-67890", {
    "service.name": "api-service",
    "operation.name": "http.request"
  })
  
  // Test always-on sampler
  let decision1 = azimuth::sampling::Sampler::should_sample(always_on_sampler, sampling_ctx)
  assert_eq(decision1.decision, azimuth::sampling::RECORD_AND_SAMPLE)
  
  // Test always-off sampler
  let decision2 = azimuth::sampling::Sampler::should_sample(always_off_sampler, sampling_ctx)
  assert_eq(decision2.decision, azimuth::sampling::DROP)
  
  // Test trace ID ratio sampler
  let decision3 = azimuth::sampling::Sampler::should_sample(trace_id_ratio_sampler, sampling_ctx)
  // Decision depends on trace ID, should be either RECORD_AND_SAMPLE or DROP
  assert(decision3.decision == azimuth::sampling::RECORD_AND_SAMPLE || 
         decision3.decision == azimuth::sampling::DROP)
}

// Test 7: Performance benchmark for telemetry operations
pub test "performance benchmark telemetry" {
  // Benchmark attributes operations
  let start_time = azimuth::time::now()
  let attrs = azimuth::Attributes::new()
  
  for i = 0; i < 1000; i = i + 1 {
    azimuth::Attributes::set(attrs, "key." + i.to_string(), azimuth::IntValue(i))
  }
  
  for i = 0; i < 1000; i = i + 1 {
    let value = azimuth::Attributes::get(attrs, "key." + i.to_string())
    assert_eq(value, Some(azimuth::IntValue(i)))
  }
  
  let attrs_time = azimuth::time::since(start_time)
  assert(attrs_time < 1000.0) // Should complete within 1 second
  
  // Benchmark span operations
  let span_start = azimuth::time::now()
  let span_ctx = azimuth::SpanContext::new("perf-trace-123", "perf-span-456", true, "")
  
  for i = 0; i < 100; i = i + 1 {
    let span = azimuth::Span::new("operation-" + i.to_string(), azimuth::Internal, span_ctx)
    azimuth::Span::add_event(span, "event-" + i.to_string(), {"iteration": i.to_string()})
    azimuth::Span::set_status(span, azimuth::OK)
  }
  
  let span_time = azimuth::time::since(span_start)
  assert(span_time < 500.0) // Should complete within 0.5 seconds
}

// Test 8: Internationalization and localization support
pub test "internationalization localization" {
  // Test Unicode attribute values
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "user.name", azimuth::StringValue("张三"))
  azimuth::Attributes::set(attrs, "message", azimuth::StringValue("操作成功完成"))
  azimuth::Attributes::set(attrs, "error.msg", azimuth::StringValue("数据库连接失败"))
  azimuth::Attributes::set(attrs, "emoji.test", azimuth::StringValue("测试🚀✅❌"))
  
  // Verify Unicode handling
  let name = azimuth::Attributes::get(attrs, "user.name")
  let message = azimuth::Attributes::get(attrs, "message")
  let error_msg = azimuth::Attributes::get(attrs, "error.msg")
  let emoji = azimuth::Attributes::get(attrs, "emoji.test")
  
  assert_eq(name, Some(azimuth::StringValue("张三")))
  assert_eq(message, Some(azimuth::StringValue("操作成功完成")))
  assert_eq(error_msg, Some(azimuth::StringValue("数据库连接失败")))
  assert_eq(emoji, Some(azimuth::StringValue("测试🚀✅❌")))
  
  // Test localized log messages
  let logger = azimuth::logging::Logger::new("i18n.test", azimuth::logging::INFO)
  azimuth::logging::Logger::info(logger, "用户登录成功", {
    "用户ID": "12345",
    "登录时间": "2024-01-01T10:00:00Z",
    "IP地址": "192.168.1.100"
  })
  
  let logs = azimuth::logging::Logger::get_logs(logger)
  assert_eq(logs[0].message, "用户登录成功")
  assert_eq(logs[0].attributes["用户ID"], "12345")
}

// Test 9: Error handling and recovery scenarios
pub test "error handling recovery" {
  // Test span error handling
  let span_ctx = azimuth::SpanContext::new("error-trace-123", "error-span-456", true, "")
  let span = azimuth::Span::new("error-prone-operation", azimuth::Internal, span_ctx)
  
  // Record error with details
  azimuth::Span::record_error(span, "Database timeout", {
    "error.code": "DB_TIMEOUT",
    "error.type": "timeout",
    "retry.count": "3",
    "timeout.ms": "5000"
  })
  
  azimuth::Span::set_status(span, azimuth::ERROR)
  
  // Verify error recording
  let status = azimuth::Span::get_status(span)
  assert_eq(status.code, azimuth::ERROR)
  
  let events = azimuth::Span::get_events(span)
  let error_event = events.find(fn(e) { e.name == "error" })
  assert(error_event != None)
  
  // Test logger error handling
  let logger = azimuth::logging::Logger::new("error.test", azimuth::logging::ERROR)
  azimuth::logging::Logger::error(logger, "Critical system failure", {
    "error.id": "ERR-001",
    "component": "database",
    "severity": "critical",
    "recovery.action": "restart_service"
  })
  
  let logs = azimuth::logging::Logger::get_logs(logger)
  assert_eq(logs[0].level, azimuth::logging::ERROR)
  assert_eq(logs[0].attributes["recovery.action"], "restart_service")
}

// Test 10: Concurrent operations and thread safety
pub test "concurrent operations thread_safety" {
  // Test concurrent attributes operations
  let shared_attrs = azimuth::Attributes::new()
  let operations = []
  
  // Prepare concurrent operations
  for i = 0; i < 100; i = i + 1 {
    let op = fn() {
      let key = "concurrent.key." + i.to_string()
      let value = azimuth::IntValue(i * 10)
      azimuth::Attributes::set(shared_attrs, key, value)
      
      // Read back the value
      let read_value = azimuth::Attributes::get(shared_attrs, key)
      assert_eq(read_value, Some(value))
    }
    operations.push(op)
  }
  
  // Execute operations concurrently
  azimuth::concurrent::execute_all(operations)
  
  // Verify all attributes were set correctly
  for i = 0; i < 100; i = i + 1 {
    let key = "concurrent.key." + i.to_string()
    let value = azimuth::Attributes::get(shared_attrs, key)
    assert_eq(value, Some(azimuth::IntValue(i * 10)))
  }
  
  // Test concurrent span operations
  let span_operations = []
  let base_ctx = azimuth::SpanContext::new("concurrent-trace-123", "base-span-456", true, "")
  
  for i = 0; i < 50; i = i + 1 {
    let span_op = fn() {
      let span = azimuth::Span::new("concurrent-op-" + i.to_string(), azimuth::Internal, base_ctx)
      azimuth::Span::add_event(span, "start", {"iteration": i.to_string()})
      azimuth::Span::set_status(span, azimuth::OK)
    }
    span_operations.push(span_op)
  }
  
  azimuth::concurrent::execute_all(span_operations)
}