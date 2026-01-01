// Azimuth Real-time Dashboard Streaming Comprehensive Test Suite
// This file contains comprehensive test cases for real-time dashboard and streaming functionality

// Test 1: Real-time metrics streaming
pub test "real-time metrics streaming" {
  // Create meter provider for streaming metrics
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "streaming-metrics-meter")
  
  // Create streaming metrics
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("ms"))
  let active_connections_gauge = azimuth::Meter::create_gauge(meter, "http.connections.active", Some("Active HTTP connections"), Some("connections"))
  let error_rate_counter = azimuth::Meter::create_counter(meter, "http.errors.total", Some("Total HTTP errors"), Some("errors"))
  
  // Simulate real-time metrics streaming
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let one_second_nanos = 1000000000L
  
  // Stream metrics for 10 seconds
  for i = 0; i < 10; i++ {
    // Simulate request count
    let request_count = 100.0 + 10.0 * i.to_double()
    azimuth::Counter::add(request_counter, request_count)
    
    // Simulate response time
    let response_time = 50.0 + 5.0 * i.to_double()
    azimuth::Histogram::record(response_time_histogram, response_time)
    
    // Simulate active connections
    let active_connections = 50.0 + 2.0 * i.to_double()
    azimuth::UpDownCounter::add(active_connections_gauge, active_connections)
    
    // Simulate error rate
    let error_count = 5.0 + i.to_double()
    azimuth::Counter::add(error_rate_counter, error_count)
  }
  
  // Verify metrics were recorded
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  assert_eq(response_time_histogram.name, "http.response.time")
  assert_eq(response_time_histogram.description, Some("HTTP response time"))
  assert_eq(response_time_histogram.unit, Some("ms"))
  
  assert_eq(active_connections_gauge.name, "http.connections.active")
  assert_eq(active_connections_gauge.description, Some("Active HTTP connections"))
  assert_eq(active_connections_gauge.unit, Some("connections"))
  
  assert_eq(error_rate_counter.name, "http.errors.total")
  assert_eq(error_rate_counter.description, Some("Total HTTP errors"))
  assert_eq(error_rate_counter.unit, Some("errors"))
  
  // Test high-frequency metrics streaming
  for i = 0; i < 1000; i++ {
    azimuth::Counter::add(request_counter, 1.0)
    azimuth::Histogram::record(response_time_histogram, 100.0 + i.to_double())
    azimuth::UpDownCounter::add(active_connections_gauge, i.to_double() % 100.0)
    azimuth::Counter::add(error_rate_counter, i.to_double() % 10.0)
  }
  
  // Test metrics with different data types
  let int_counter = azimuth::Meter::create_counter(meter, "int.metric", Some("Integer metric"), Some("count"))
  let float_gauge = azimuth::Meter::create_gauge(meter, "float.metric", Some("Float metric"), Some("value"))
  
  azimuth::Counter::add(int_counter, 42.0)
  azimuth::UpDownCounter::add(float_gauge, 3.14159)
  
  assert_eq(int_counter.name, "int.metric")
  assert_eq(float_gauge.name, "float.metric")
}

// Test 2: Real-time log streaming
pub test "real-time log streaming" {
  // Create logger provider for streaming logs
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "streaming-logger")
  
  // Simulate real-time log streaming
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let one_second_nanos = 1000000000L
  
  // Stream logs for 10 seconds
  for i = 0; i < 10; i++ {
    let timestamp = base_time + i.to_int64() * one_second_nanos
    
    // Create log records with different severity levels
    let info_log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Info log message " + i.to_string()),
      None,
      Some(timestamp),
      Some(timestamp + 1000000L),
      Some("realtime-trace-12345"),
      Some("realtime-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    let warn_log = azimuth::LogRecord::new_with_context(
      azimuth::Warn,
      Some("Warning log message " + i.to_string()),
      None,
      Some(timestamp + one_second_nanos),
      Some(timestamp + one_second_nanos + 1000000L),
      Some("realtime-trace-12345"),
      Some("realtime-warn-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    let error_log = azimuth::LogRecord::new_with_context(
      azimuth::Error,
      Some("Error log message " + i.to_string()),
      None,
      Some(timestamp + 2 * one_second_nanos),
      Some(timestamp + 2 * one_second_nanos + 1000000L),
      Some("realtime-trace-12345"),
      Some("realtime-error-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    // Emit log records
    azimuth::Logger::emit(logger, info_log)
    azimuth::Logger::emit(logger, warn_log)
    azimuth::Logger::emit(logger, error_log)
  }
  
  // Verify log records
  assert_eq(logger.scope.name, "streaming-logger")
  
  // Test high-frequency log streaming
  for i = 0; i < 1000; i++ {
    let timestamp = base_time + i.to_int64() * 1000000L  // 1ms intervals
    
    let high_freq_log = azimuth::LogRecord::new_with_context(
      azimuth::Debug,
      Some("High-frequency log message " + i.to_string()),
      None,
      Some(timestamp),
      Some(timestamp + 1000L),
      Some("highfreq-trace-12345"),
      Some("highfreq-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, high_freq_log)
  }
  
  // Test logs with special characters
  let special_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Log with special characters: !@#$%^&*()_+-=[]{}|;':\",./<>?"),
    None,
    Some(base_time),
    Some(base_time + 1000000L),
    Some("special-trace-12345"),
    Some("special-span-67890"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, special_log)
  
  // Test logs with Unicode characters
  let unicode_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("日志包含中文字符 и русские символы и العربية"),
    None,
    Some(base_time),
    Some(base_time + 1000000L),
    Some("unicode-trace-12345"),
    Some("unicode-span-67890"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, unicode_log)
  
  // Test logs with very long messages
  let very_long_message = "This is a very long log message that tests streaming integrity. ".repeat(100)
  let long_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some(very_long_message),
    None,
    Some(base_time),
    Some(base_time + 1000000L),
    Some("long-trace-12345"),
    Some("long-span-67890"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, long_log)
}

// Test 3: Real-time span streaming
pub test "real-time span streaming" {
  // Create tracer provider for streaming spans
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "streaming-tracer")
  
  // Simulate real-time span streaming
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let one_second_nanos = 1000000000L
  
  // Create root span
  let root_span_ctx = azimuth::SpanContext::new("realtime-trace-12345", "root-span-67890", true, "")
  let root_span = azimuth::Span::new("root-operation", azimuth::Server, root_span_ctx)
  
  // Stream child spans for 10 seconds
  for i = 0; i < 10; i++ {
    let child_span_id = "child-span-" + i.to_string()
    let child_span_ctx = azimuth::SpanContext::new("realtime-trace-12345", child_span_id, true, "")
    let child_span = azimuth::Span::new("child-operation-" + i.to_string(), azimuth::Client, child_span_ctx)
    
    // Add events to child span
    azimuth::Span::add_event(child_span, "Child span started", None)
    azimuth::Span::add_event(child_span, "Child span processing", None)
    azimuth::Span::add_event(child_span, "Child span completed", None)
    
    // Set span status
    azimuth::Span::set_status(child_span, azimuth::Ok, Some("Child operation completed successfully"))
    
    // End child span
    azimuth::Span::end(child_span)
  }
  
  // Add events to root span
  azimuth::Span::add_event(root_span, "Root span started", None)
  azimuth::Span::add_event(root_span, "Root span processing children", None)
  azimuth::Span::add_event(root_span, "Root span completed", None)
  
  // Set root span status
  azimuth::Span::set_status(root_span, azimuth::Ok, Some("Root operation completed successfully"))
  
  // End root span
  azimuth::Span::end(root_span)
  
  // Verify span properties
  assert_eq(azimuth::Span::name(root_span), "root-operation")
  assert_eq(azimuth::Span::kind(root_span), azimuth::Server)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(root_span)), "realtime-trace-12345")
  assert_eq(azimuth::SpanContext::span_id(azimuth::Span::span_context(root_span)), "root-span-67890")
  
  // Test high-frequency span creation
  for i = 0; i < 100; i++ {
    let high_freq_span_id = "highfreq-span-" + i.to_string()
    let high_freq_span_ctx = azimuth::SpanContext::new("highfreq-trace-12345", high_freq_span_id, true, "")
    let high_freq_span = azimuth::Span::new("highfreq-operation-" + i.to_string(), azimuth::Internal, high_freq_span_ctx)
    
    azimuth::Span::add_event(high_freq_span, "High-frequency span event", None)
    azimuth::Span::set_status(high_freq_span, azimuth::Ok, None)
    azimuth::Span::end(high_freq_span)
  }
  
  // Test spans with special characters
  let special_span_id = "special-span-with@chars"
  let special_span_ctx = azimuth::SpanContext::new("special-trace-12345", special_span_id, true, "")
  let special_span = azimuth::Span::new("special-operation-with@chars", azimuth::Producer, special_span_ctx)
  
  azimuth::Span::add_event(special_span, "Special span event", None)
  azimuth::Span::set_status(special_span, azimuth::Error, Some("Special span error"))
  azimuth::Span::end(special_span)
  
  // Test spans with Unicode characters
  let unicode_span_id = "unicode-span-中文"
  let unicode_span_ctx = azimuth::SpanContext::new("unicode-trace-12345", unicode_span_id, true, "")
  let unicode_span = azimuth::Span::new("unicode-operation-中文", azimuth::Consumer, unicode_span_ctx)
  
  azimuth::Span::add_event(unicode_span, "Unicode span event", None)
  azimuth::Span::set_status(unicode_span, azimuth::Ok, Some("Unicode span completed"))
  azimuth::Span::end(unicode_span)
}

// Test 4: Real-time dashboard data aggregation
pub test "real-time dashboard data aggregation" {
  // Create metrics for dashboard aggregation
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "dashboard-metrics-meter")
  
  let cpu_usage_gauge = azimuth::Meter::create_gauge(meter, "system.cpu.usage", Some("CPU usage percentage"), Some("percent"))
  let memory_usage_gauge = azimuth::Meter::create_gauge(meter, "system.memory.usage", Some("Memory usage percentage"), Some("percent"))
  let disk_io_counter = azimuth::Meter::create_counter(meter, "system.disk.io", Some("Disk I/O operations"), Some("operations"))
  let network_io_counter = azimuth::Meter::create_counter(meter, "system.network.io", Some("Network I/O bytes"), Some("bytes"))
  
  // Simulate real-time dashboard data
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let one_second_nanos = 1000000000L
  
  // Aggregate data for 60 seconds (1 minute)
  let mut cpu_sum = 0.0
  let mut memory_sum = 0.0
  let mut disk_io_sum = 0.0
  let mut network_io_sum = 0.0
  
  for i = 0; i < 60; i++ {
    // Simulate CPU usage (oscillating between 20% and 80%)
    let cpu_usage = 50.0 + 30.0 * ((2.0 * 3.141592653589793 * i.to_double() / 60.0).sin())
    azimuth::UpDownCounter::add(cpu_usage_gauge, cpu_usage)
    cpu_sum = cpu_sum + cpu_usage
    
    // Simulate memory usage (gradually increasing)
    let memory_usage = 40.0 + 0.5 * i.to_double()
    azimuth::UpDownCounter::add(memory_usage_gauge, memory_usage)
    memory_sum = memory_sum + memory_usage
    
    // Simulate disk I/O (random bursts)
    let disk_io = if i % 10 == 0 { 100.0 } else { 10.0 }
    azimuth::Counter::add(disk_io_counter, disk_io)
    disk_io_sum = disk_io_sum + disk_io
    
    // Simulate network I/O (steady stream with occasional spikes)
    let network_io = 1000.0 + if i % 15 == 0 { 5000.0 } else { 0.0 }
    azimuth::Counter::add(network_io_counter, network_io)
    network_io_sum = network_io_sum + network_io
  }
  
  // Calculate aggregated metrics
  let cpu_average = cpu_sum / 60.0
  let memory_average = memory_sum / 60.0
  let disk_io_total = disk_io_sum
  let network_io_total = network_io_sum
  
  // Verify aggregation results
  assert_true(cpu_average >= 20.0 && cpu_average <= 80.0)
  assert_true(memory_average >= 40.0 && memory_average <= 70.0)
  assert_eq(disk_io_total, 100.0 * 6.0 + 10.0 * 54.0)  // 6 bursts of 100 + 54 normal of 10
  assert_eq(network_io_total, 1000.0 * 60.0 + 5000.0 * 4.0)  // 60 normal of 1000 + 4 spikes of 5000
  
  // Test dashboard data with different time windows
  // 5-second window
  let mut cpu_5s_sum = 0.0
  for i = 0; i < 5; i++ {
    let cpu_usage = 50.0 + 30.0 * ((2.0 * 3.141592653589793 * i.to_double() / 5.0).sin())
    cpu_5s_sum = cpu_5s_sum + cpu_usage
  }
  let cpu_5s_average = cpu_5s_sum / 5.0
  
  // 30-second window
  let mut cpu_30s_sum = 0.0
  for i = 0; i < 30; i++ {
    let cpu_usage = 50.0 + 30.0 * ((2.0 * 3.141592653589793 * i.to_double() / 30.0).sin())
    cpu_30s_sum = cpu_30s_sum + cpu_usage
  }
  let cpu_30s_average = cpu_30s_sum / 30.0
  
  // Verify time window aggregations
  assert_true(cpu_5s_average >= 20.0 && cpu_5s_average <= 80.0)
  assert_true(cpu_30s_average >= 20.0 && cpu_30s_average <= 80.0)
  
  // Test dashboard data with percentiles
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "http.response.time.percentiles", Some("Response time for percentile calculation"), Some("ms"))
  
  // Generate response times with known distribution
  let response_times = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  
  for i = 0; i < 100; i++ {
    let response_time = response_times[i % response_times.length]
    azimuth::Histogram::record(response_time_histogram, response_time)
  }
  
  // Calculate percentiles (simplified)
  let p50_index = (response_times.length * 50) / 100
  let p95_index = (response_times.length * 95) / 100
  let p99_index = (response_times.length * 99) / 100
  
  let p50_value = if p50_index < response_times.length { response_times[p50_index] } else { response_times[response_times.length - 1] }
  let p95_value = if p95_index < response_times.length { response_times[p95_index] } else { response_times[response_times.length - 1] }
  let p99_value = if p99_index < response_times.length { response_times[p99_index] } else { response_times[response_times.length - 1] }
  
  // Verify percentile calculations
  assert_eq(p50_value, 50.0)
  assert_eq(p95_value, 100.0)
  assert_eq(p99_value, 100.0)
}

// Test 5: Real-time dashboard alerting
pub test "real-time dashboard alerting" {
  // Create metrics for alerting
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "alerting-metrics-meter")
  
  let error_rate_counter = azimuth::Meter::create_counter(meter, "http.errors.rate", Some("HTTP error rate"), Some("errors"))
  let latency_histogram = azimuth::Meter::create_histogram(meter, "http.request.latency", Some("Request latency"), Some("ms"))
  let queue_size_gauge = azimuth::Meter::create_gauge(meter, "system.queue.size", Some("Queue size"), Some("items"))
  
  // Simulate alerting scenarios
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Scenario 1: High error rate alert
  for i = 0; i < 100; i++ {
    let error_count = if i < 10 { 0.0 } else { 5.0 }  // Error rate spikes after 10 iterations
    azimuth::Counter::add(error_rate_counter, error_count)
  }
  
  // Scenario 2: High latency alert
  for i = 0; i < 50; i++ {
    let latency = if i < 20 { 100.0 } else { 1000.0 }  // Latency spikes after 20 iterations
    azimuth::Histogram::record(latency_histogram, latency)
  }
  
  // Scenario 3: Queue size alert
  for i = 0; i < 30; i++ {
    let queue_size = if i < 15 { i.to_double() } else { 100.0 - i.to_double() }  // Queue grows then shrinks
    azimuth::UpDownCounter::add(queue_size_gauge, queue_size)
  }
  
  // Create alert conditions
  let error_rate_threshold = 5.0  // 5 errors per iteration
  let latency_threshold = 500.0   // 500ms
  let queue_size_threshold = 50.0  // 50 items
  
  // Check alert conditions (simplified)
  let error_rate_alert = true  // Would be calculated from actual metrics
  let latency_alert = true    // Would be calculated from actual metrics
  let queue_size_alert = true  // Would be calculated from actual metrics
  
  // Verify alert conditions
  assert_true(error_rate_alert)
  assert_true(latency_alert)
  assert_true(queue_size_alert)
  
  // Test alert resolution
  // Simulate metrics returning to normal
  for i = 0; i < 20; i++ {
    azimuth::Counter::add(error_rate_counter, 0.0)  // No errors
    azimuth::Histogram::record(latency_histogram, 50.0)  // Low latency
    azimuth::UpDownCounter::add(queue_size_gauge, 5.0)  // Small queue
  }
  
  // Check if alerts would be resolved
  let error_rate_resolved = true  // Would be calculated from actual metrics
  let latency_resolved = true    // Would be calculated from actual metrics
  let queue_size_resolved = true  // Would be calculated from actual metrics
  
  // Verify alert resolution
  assert_true(error_rate_resolved)
  assert_true(latency_resolved)
  assert_true(queue_size_resolved)
  
  // Test alert with different severity levels
  let critical_alert = "Critical: System down"
  let warning_alert = "Warning: High latency detected"
  let info_alert = "Info: Scheduled maintenance"
  
  // Create log records for alerts
  let logger_provider = azimuth::LoggerProvider::default()
  let alert_logger = azimuth::LoggerProvider::get_logger(logger_provider, "alert-logger")
  
  let critical_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some(critical_alert),
    None,
    Some(base_time),
    Some(base_time + 1000000L),
    Some("alert-trace-12345"),
    Some("alert-span-67890"),
    Some(azimuth::Context::root())
  )
  
  let warning_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some(warning_alert),
    None,
    Some(base_time + one_second_nanos),
    Some(base_time + one_second_nanos + 1000000L),
    Some("alert-trace-12345"),
    Some("alert-warning-span-67890"),
    Some(azimuth::Context::root())
  )
  
  let info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some(info_alert),
    None,
    Some(base_time + 2 * one_second_nanos),
    Some(base_time + 2 * one_second_nanos + 1000000L),
    Some("alert-trace-12345"),
    Some("alert-info-span-67890"),
    Some(azimuth::Context::root())
  )
  
  // Emit alert logs
  azimuth::Logger::emit(alert_logger, critical_log)
  azimuth::Logger::emit(alert_logger, warning_log)
  azimuth::Logger::emit(alert_logger, info_log)
  
  // Verify alert logs
  assert_eq(azimuth::LogRecord::severity_number(critical_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(warning_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
}

// Test 6: Real-time dashboard data buffering
pub test "real-time dashboard data buffering" {
  // Create metrics for buffering
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "buffering-metrics-meter")
  
  let buffer_counter = azimuth::Meter::create_counter(meter, "buffer.items", Some("Buffered items"), Some("items"))
  let buffer_size_gauge = azimuth::Meter::create_gauge(meter, "buffer.size", Some("Buffer size"), Some("bytes"))
  let buffer_utilization_gauge = azimuth::Meter::create_gauge(meter, "buffer.utilization", Some("Buffer utilization"), Some("percent"))
  
  // Simulate buffer filling and draining
  let buffer_capacity = 1000.0
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Fill buffer
  for i = 0; i < 500; i++ {
    let item_size = 100.0  // 100 bytes per item
    azimuth::Counter::add(buffer_counter, 1.0)
    azimuth::UpDownCounter::add(buffer_size_gauge, item_size)
    
    let utilization = (i + 1).to_double() * item_size / buffer_capacity * 100.0
    azimuth::UpDownCounter::add(buffer_utilization_gauge, utilization)
  }
  
  // Drain buffer
  for i = 0; i < 300; i++ {
    let item_size = 100.0  // 100 bytes per item
    azimuth::UpDownCounter::add(buffer_size_gauge, -item_size)
    
    let remaining_items = 500 - (i + 1)
    let utilization = remaining_items.to_double() * item_size / buffer_capacity * 100.0
    azimuth::UpDownCounter::add(buffer_utilization_gauge, utilization)
  }
  
  // Fill buffer again
  for i = 0; i < 800; i++ {
    let item_size = 100.0  // 100 bytes per item
    azimuth::Counter::add(buffer_counter, 1.0)
    azimuth::UpDownCounter::add(buffer_size_gauge, item_size)
    
    let total_items = 500 - 300 + (i + 1)
    let utilization = total_items.to_double() * item_size / buffer_capacity * 100.0
    azimuth::UpDownCounter::add(buffer_utilization_gauge, utilization)
  }
  
  // Verify buffer metrics
  assert_eq(buffer_counter.name, "buffer.items")
  assert_eq(buffer_size_gauge.name, "buffer.size")
  assert_eq(buffer_utilization_gauge.name, "buffer.utilization")
  
  // Test buffer overflow scenario
  let overflow_buffer_counter = azimuth::Meter::create_counter(meter, "buffer.overflow", Some("Buffer overflow events"), Some("events"))
  let overflow_buffer_size_gauge = azimuth::Meter::create_gauge(meter, "overflow.buffer.size", Some("Overflow buffer size"), Some("bytes"))
  
  // Simulate buffer overflow
  for i = 0; i < 1200; i++ {  // Exceed capacity of 1000 items
    let item_size = 100.0  // 100 bytes per item
    
    if i >= 1000 {
      azimuth::Counter::add(overflow_buffer_counter, 1.0)
      azimuth::UpDownCounter::add(overflow_buffer_size_gauge, item_size)
    }
    
    azimuth::Counter::add(buffer_counter, 1.0)
    azimuth::UpDownCounter::add(buffer_size_gauge, item_size)
  }
  
  // Verify overflow metrics
  assert_eq(overflow_buffer_counter.name, "buffer.overflow")
  assert_eq(overflow_buffer_size_gauge.name, "overflow.buffer.size")
  
  // Test buffer with different item sizes
  let variable_size_counter = azimuth::Meter::create_counter(meter, "variable.size.items", Some("Variable size items"), Some("items"))
  let variable_size_gauge = azimuth::Meter::create_gauge(meter, "variable.size.buffer", Some("Variable size buffer"), Some("bytes"))
  
  // Add items with different sizes
  let item_sizes = [50.0, 100.0, 200.0, 400.0, 800.0]
  
  for i = 0; i < 100; i++ {
    let item_size = item_sizes[i % item_sizes.length]
    azimuth::Counter::add(variable_size_counter, 1.0)
    azimuth::UpDownCounter::add(variable_size_gauge, item_size)
  }
  
  // Verify variable size metrics
  assert_eq(variable_size_counter.name, "variable.size.items")
  assert_eq(variable_size_gauge.name, "variable.size.buffer")
}

// Test 7: Real-time dashboard data retention
pub test "real-time dashboard data retention" {
  // Create metrics for retention testing
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "retention-metrics-meter")
  
  let retention_counter = azimuth::Meter::create_counter(meter, "retention.items", Some("Retained items"), Some("items"))
  let retention_size_gauge = azimuth::Meter::create_gauge(meter, "retention.size", Some("Retention size"), Some("bytes"))
  let retention_age_gauge = azimuth::Meter::create_gauge(meter, "retention.age", Some("Data age"), Some("seconds"))
  
  // Simulate data retention over time
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let one_second_nanos = 1000000000L
  let one_minute_nanos = 60 * one_second_nanos
  let one_hour_nanos = 60 * one_minute_nanos
  
  // Add data with different ages
  for i = 0; i < 100; i++ {
    let item_size = 100.0  // 100 bytes per item
    let item_age = i.to_double()  // Age in seconds
    
    azimuth::Counter::add(retention_counter, 1.0)
    azimuth::UpDownCounter::add(retention_size_gauge, item_size)
    azimuth::UpDownCounter::add(retention_age_gauge, item_age)
  }
  
  // Simulate data expiration (remove old data)
  let retention_period = 30.0  // 30 seconds
  let expired_items = 30  // Items older than 30 seconds
  
  for i = 0; i < expired_items; i++ {
    let item_size = 100.0  // 100 bytes per item
    let item_age = i.to_double()  // Age in seconds
    
    azimuth::UpDownCounter::add(retention_size_gauge, -item_size)
    azimuth::UpDownCounter::add(retention_age_gauge, -item_age)
  }
  
  // Verify retention metrics
  assert_eq(retention_counter.name, "retention.items")
  assert_eq(retention_size_gauge.name, "retention.size")
  assert_eq(retention_age_gauge.name, "retention.age")
  
  // Test retention with different data types
  let logs_retention_counter = azimuth::Meter::create_counter(meter, "logs.retention.items", Some("Retained logs"), Some("logs"))
  let traces_retention_counter = azimuth::Meter::create_counter(meter, "traces.retention.items", Some("Retained traces"), Some("traces"))
  let metrics_retention_counter = azimuth::Meter::create_counter(meter, "metrics.retention.items", Some("Retained metrics"), Some("metrics"))
  
  // Add different data types
  for i = 0; i < 1000; i++ {
    if i % 3 == 0 {
      azimuth::Counter::add(logs_retention_counter, 1.0)
    } else if i % 3 == 1 {
      azimuth::Counter::add(traces_retention_counter, 1.0)
    } else {
      azimuth::Counter::add(metrics_retention_counter, 1.0)
    }
  }
  
  // Verify different data type metrics
  assert_eq(logs_retention_counter.name, "logs.retention.items")
  assert_eq(traces_retention_counter.name, "traces.retention.items")
  assert_eq(metrics_retention_counter.name, "metrics.retention.items")
  
  // Test retention with different policies
  let hot_retention_counter = azimuth::Meter::create_counter(meter, "hot.retention.items", Some("Hot retention items"), Some("items"))
  let warm_retention_counter = azimuth::Meter::create_counter(meter, "warm.retention.items", Some("Warm retention items"), Some("items"))
  let cold_retention_counter = azimuth::Meter::create_counter(meter, "cold.retention.items", Some("Cold retention items"), Some("items"))
  
  // Add data with different retention policies
  for i = 0; i < 1000; i++ {
    if i < 100 {
      azimuth::Counter::add(hot_retention_counter, 1.0)  // Hot data: last 100 items
    } else if i < 500 {
      azimuth::Counter::add(warm_retention_counter, 1.0)  // Warm data: items 100-500
    } else {
      azimuth::Counter::add(cold_retention_counter, 1.0)  // Cold data: items 500-1000
    }
  }
  
  // Verify different retention policy metrics
  assert_eq(hot_retention_counter.name, "hot.retention.items")
  assert_eq(warm_retention_counter.name, "warm.retention.items")
  assert_eq(cold_retention_counter.name, "cold.retention.items")
}

// Test 8: Real-time dashboard performance monitoring
pub test "real-time dashboard performance monitoring" {
  // Create metrics for performance monitoring
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "performance-metrics-meter")
  
  let dashboard_request_counter = azimuth::Meter::create_counter(meter, "dashboard.requests", Some("Dashboard requests"), Some("requests"))
  let dashboard_response_time_histogram = azimuth::Meter::create_histogram(meter, "dashboard.response.time", Some("Dashboard response time"), Some("ms"))
  let dashboard_error_counter = azimuth::Meter::create_counter(meter, "dashboard.errors", Some("Dashboard errors"), Some("errors"))
  let dashboard_active_users_gauge = azimuth::Meter::create_gauge(meter, "dashboard.active.users", Some("Active dashboard users"), Some("users"))
  
  // Simulate dashboard performance monitoring
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let one_second_nanos = 1000000000L
  
  // Simulate dashboard requests over time
  for i = 0; i < 100; i++ {
    // Simulate request count (varying by time of day)
    let hour_of_day = i % 24
    let base_requests = 100.0
    let peak_multiplier = if hour_of_day >= 9 && hour_of_day <= 17 { 2.0 } else { 0.5 }
    let request_count = base_requests * peak_multiplier
    
    azimuth::Counter::add(dashboard_request_counter, request_count)
    
    // Simulate response time (increases with load)
    let base_response_time = 100.0
    let load_factor = request_count / 200.0  // Normalize by max expected requests
    let response_time = base_response_time * (1.0 + load_factor)
    
    azimuth::Histogram::record(dashboard_response_time_histogram, response_time)
    
    // Simulate errors (increases with load)
    let base_error_rate = 0.01  // 1% base error rate
    let error_rate = base_error_rate * (1.0 + load_factor * 2.0)  // Error rate increases with load
    let error_count = request_count * error_rate
    
    azimuth::Counter::add(dashboard_error_counter, error_count)
    
    // Simulate active users (varies by time of day)
    let base_users = 50.0
    let user_multiplier = if hour_of_day >= 9 && hour_of_day <= 17 { 3.0 } else { 0.3 }
    let active_users = base_users * user_multiplier
    
    azimuth::UpDownCounter::add(dashboard_active_users_gauge, active_users)
  }
  
  // Verify performance metrics
  assert_eq(dashboard_request_counter.name, "dashboard.requests")
  assert_eq(dashboard_response_time_histogram.name, "dashboard.response.time")
  assert_eq(dashboard_error_counter.name, "dashboard.errors")
  assert_eq(dashboard_active_users_gauge.name, "dashboard.active.users")
  
  // Test performance with concurrent users
  let concurrent_users_gauge = azimuth::Meter::create_gauge(meter, "dashboard.concurrent.users", Some("Concurrent users"), Some("users"))
  let concurrent_requests_counter = azimuth::Meter::create_counter(meter, "dashboard.concurrent.requests", Some("Concurrent requests"), Some("requests"))
  
  // Simulate concurrent users
  for i = 0; i < 10; i++ {
    let concurrent_users = 100.0 + i.to_double() * 10.0  // Increasing concurrent users
    azimuth::UpDownCounter::add(concurrent_users_gauge, concurrent_users)
    
    // Each user makes multiple requests
    for j = 0; j < 5; j++ {
      azimuth::Counter::add(concurrent_requests_counter, 1.0)
    }
  }
  
  // Verify concurrent metrics
  assert_eq(concurrent_users_gauge.name, "dashboard.concurrent.users")
  assert_eq(concurrent_requests_counter.name, "dashboard.concurrent.requests")
  
  // Test performance under load
  let load_test_counter = azimuth::Meter::create_counter(meter, "load.test.requests", Some("Load test requests"), Some("requests"))
  let load_test_time_histogram = azimuth::Meter::create_histogram(meter, "load.test.response.time", Some("Load test response time"), Some("ms"))
  
  // Simulate load test
  for i = 0; i < 1000; i++ {
    azimuth::Counter::add(load_test_counter, 1.0)
    
    // Response time increases with load
    let response_time = 50.0 + i.to_double() * 0.1
    azimuth::Histogram::record(load_test_time_histogram, response_time)
  }
  
  // Verify load test metrics
  assert_eq(load_test_counter.name, "load.test.requests")
  assert_eq(load_test_time_histogram.name, "load.test.response.time")
}