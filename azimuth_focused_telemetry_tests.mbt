// Azimuth Telemetry System - Focused Test Suite
// This file contains focused test cases for core telemetry functionality

// Test 1: Basic Span Creation and Attributes
test "basic span creation and attributes" {
  let span = Span::new("test-span")
  span.set_attribute("user.id", "12345")
  span.set_attribute("request.size", 1024)
  span.set_attribute("cache.hit", true)
  
  assert_eq(span.name, "test-span")
  assert_true(span.has_attribute("user.id"))
  assert_true(span.has_attribute("request.size"))
  assert_true(span.has_attribute("cache.hit"))
  assert_false(span.has_attribute("nonexistent"))
}

// Test 2: Metric Counter Operations
test "metric counter operations" {
  let counter = Metric::counter("requests_total", "Total number of requests")
  counter.add(1)
  counter.add(5)
  counter.add(10)
  
  assert_eq(counter.value(), 16)
  assert_eq(counter.name(), "requests_total")
  assert_eq(counter.description(), "Total number of requests")
}

// Test 3: Log Record Creation and Severity
test "log record creation and severity" {
  let info_log = Log::info("User logged in", { "user": "alice", "ip": "192.168.1.1" })
  let error_log = Log::error("Database connection failed", { "error": "timeout", "retries": 3 })
  
  assert_eq(info_log.severity(), LogSeverity::Info)
  assert_eq(info_log.message(), "User logged in")
  assert_true(info_log.has_attribute("user"))
  
  assert_eq(error_log.severity(), LogSeverity::Error)
  assert_eq(error_log.message(), "Database connection failed")
  assert_true(error_log.has_attribute("error"))
}

// Test 4: Trace Context Propagation
test "trace context propagation" {
  let parent_span = Span::new("parent-operation")
  parent_span.set_trace_id("trace-123")
  parent_span.set_span_id("span-456")
  
  let child_span = Span::new_child("child-operation", parent_span)
  
  assert_eq(child_span.trace_id(), parent_span.trace_id())
  assert_eq(child_span.parent_span_id(), parent_span.span_id())
  assert_not_eq(child_span.span_id(), parent_span.span_id())
}

// Test 5: Gauge Metric Operations
test "gauge metric operations" {
  let gauge = Metric::gauge("memory_usage", "Current memory usage in bytes")
  gauge.set(1024)
  assert_eq(gauge.value(), 1024)
  
  gauge.set(2048)
  assert_eq(gauge.value(), 2048)
  
  gauge.add(512)
  assert_eq(gauge.value(), 2560)
  
  gauge.subtract(256)
  assert_eq(gauge.value(), 2304)
}

// Test 6: Histogram Metric Operations
test "histogram metric operations" {
  let histogram = Metric::histogram("request_duration", "Request duration in seconds", [0.1, 0.5, 1.0, 2.0])
  histogram.observe(0.05)
  histogram.observe(0.3)
  histogram.observe(0.8)
  histogram.observe(1.5)
  histogram.observe(3.0)
  
  assert_eq(histogram.count(), 5)
  assert_true(histogram.sum() > 0.0)
  assert_eq(histogram.buckets().length(), 5)
}

// Test 7: Baggage Item Management
test "baggage item management" {
  let baggage = Baggage::new()
  baggage.set("user.id", "user123")
  baggage.set("session.id", "session456")
  baggage.set("region", "us-west")
  
  assert_eq(baggage.get("user.id"), Some("user123"))
  assert_eq(baggage.get("session.id"), Some("session456"))
  assert_eq(baggage.get("region"), Some("us-west"))
  assert_eq(baggage.get("nonexistent"), None)
  
  baggage.remove("session.id")
  assert_eq(baggage.get("session.id"), None)
  assert_eq(baggage.get("user.id"), Some("user123"))
}

// Test 8: Resource Attributes
test "resource attributes" {
  let resource = Resource::new()
  resource.set_attribute("service.name", "payment-service")
  resource.set_attribute("service.version", "1.2.3")
  resource.set_attribute("deployment.environment", "production")
  
  assert_eq(resource.get_attribute("service.name"), Some("payment-service"))
  assert_eq(resource.get_attribute("service.version"), Some("1.2.3"))
  assert_eq(resource.get_attribute("deployment.environment"), Some("production"))
  
  let all_attrs = resource.attributes()
  assert_eq(all_attrs.length(), 3)
}

// Test 9: Span Status and Error Handling
test "span status and error handling" {
  let span = Span::new("database-query")
  span.set_status(SpanStatus::Ok)
  assert_eq(span.status(), SpanStatus::Ok)
  
  let error_span = Span::new("failing-operation")
  error_span.set_status(SpanStatus::Error, "Connection timeout")
  assert_eq(error_span.status(), SpanStatus::Error)
  assert_eq(error_span.status_message(), "Connection timeout")
  assert_true(error_span.has_error())
}

// Test 10: Telemetry Configuration
test "telemetry configuration" {
  let config = TelemetryConfig::new()
  config.set_service_name("order-service")
  config.set_service_version("2.1.0")
  config.set_sampling_rate(0.1)
  config.enable_metrics(true)
  config.enable_logs(true)
  config.enable_tracing(true)
  
  assert_eq(config.service_name(), "order-service")
  assert_eq(config.service_version(), "2.1.0")
  assert_eq(config.sampling_rate(), 0.1)
  assert_true(config.metrics_enabled())
  assert_true(config.logs_enabled())
  assert_true(config.tracing_enabled())
}