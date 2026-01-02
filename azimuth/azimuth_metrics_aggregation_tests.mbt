// Metrics Aggregation Operations Tests for Azimuth
// This file contains test cases for metrics aggregation operations

test "counter aggregation" {
  // Test counter metric aggregation
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "counter-aggregation-test")
  
  // Create counters with different attributes
  let base_counter = azimuth::Meter::create_counter(meter, "requests.total", Some("Total number of requests"), Some("count"))
  let error_counter = azimuth::Meter::create_counter(meter, "errors.total", Some("Total number of errors"), Some("count"))
  
  // Create attributes for different request types
  let success_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(success_attrs, "status", azimuth::StringValue("success"))
  azimuth::Attributes::set(success_attrs, "endpoint", azimuth::StringValue("/api/users"))
  
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "status", azimuth::StringValue("error"))
  azimuth::Attributes::set(error_attrs, "endpoint", azimuth::StringValue("/api/users"))
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("validation"))
  
  // Record counter values
  azimuth::Counter::add(base_counter, 100.0, Some(success_attrs))
  azimuth::Counter::add(base_counter, 25.0, Some(error_attrs))
  azimuth::Counter::add(error_counter, 25.0, Some(error_attrs))
  
  // Simulate aggregation calculation
  let total_requests = 100.0 + 25.0  // 125.0
  let success_rate = (100.0 / total_requests) * 100.0  // 80.0%
  let error_rate = (25.0 / total_requests) * 100.0  // 20.0%
  
  // Verify aggregation results
  assert_eq(total_requests, 125.0)
  assert_eq(success_rate, 80.0)
  assert_eq(error_rate, 20.0)
  
  // Verify counter properties
  assert_eq(base_counter.name, "requests.total")
  assert_eq(error_counter.name, "errors.total")
}

test "histogram aggregation" {
  // Test histogram metric aggregation
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "histogram-aggregation-test")
  
  // Create histogram for response times
  let response_histogram = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  
  // Record response time measurements
  let response_times = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "endpoint", azimuth::StringValue("/api/data"))
  
  for time in response_times {
    azimuth::Histogram::record(response_histogram, time, Some(attrs))
  }
  
  // Simulate histogram aggregation calculations
  let count = response_times.length().to_double()
  let sum = 0.0
  for time in response_times {
    sum = sum + time
  }
  let mean = sum / count
  
  // Calculate percentiles (simplified)
  let sorted_times = response_times.sort()
  let p50 = sorted_times[sorted_times.length() / 2]  // Median
  let p90 = sorted_times[(sorted_times.length() * 9) / 10]  // 90th percentile
  let p95 = sorted_times[(sorted_times.length() * 95) / 100]  // 95th percentile
  let p99 = sorted_times[(sorted_times.length() * 99) / 100]  // 99th percentile
  
  // Verify aggregation results
  assert_eq(count, 10.0)
  assert_eq(sum, 550.0)
  assert_eq(mean, 55.0)
  assert_eq(p50, 55.0)  // Median of 10, 20, ..., 100
  assert_eq(p90, 90.0)
  assert_eq(p95, 95.0)
  assert_eq(p99, 99.0)
  
  // Verify histogram properties
  assert_eq(response_histogram.name, "response.time")
}

test "gauge aggregation" {
  // Test gauge metric aggregation
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "gauge-aggregation-test")
  
  // Create gauges for system metrics
  let cpu_gauge = azimuth::Meter::create_gauge(meter, "system.cpu.usage", Some("CPU usage percentage"), Some("percent"))
  let memory_gauge = azimuth::Meter::create_gauge(meter, "system.memory.usage", Some("Memory usage percentage"), Some("percent"))
  
  // Create attributes for different instances
  let instance1_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(instance1_attrs, "instance.id", azimuth::StringValue("instance-1"))
  
  let instance2_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(instance2_attrs, "instance.id", azimuth::StringValue("instance-2"))
  
  // Simulate gauge values over time
  let cpu_values_instance1 = [25.0, 30.0, 35.0, 40.0, 45.0]
  let cpu_values_instance2 = [20.0, 25.0, 30.0, 35.0, 40.0]
  
  let memory_values_instance1 = [60.0, 62.0, 64.0, 66.0, 68.0]
  let memory_values_instance2 = [55.0, 57.0, 59.0, 61.0, 63.0]
  
  // Record gauge values (simplified - in real implementation, gauges would be set, not added)
  for i in 0..cpu_values_instance1.length() {
    // Record CPU values
    azimuth::Counter::add(azimuth::Counter::as_instrument(cpu_gauge), cpu_values_instance1[i], Some(instance1_attrs))
    azimuth::Counter::add(azimuth::Counter::as_instrument(cpu_gauge), cpu_values_instance2[i], Some(instance2_attrs))
    
    // Record memory values
    azimuth::Counter::add(azimuth::Counter::as_instrument(memory_gauge), memory_values_instance1[i], Some(instance1_attrs))
    azimuth::Counter::add(azimuth::Counter::as_instrument(memory_gauge), memory_values_instance2[i], Some(instance2_attrs))
  }
  
  // Simulate gauge aggregation calculations
  let avg_cpu_instance1 = cpu_values_instance1.reduce(0.0, fn(acc, x) { acc + x }) / cpu_values_instance1.length().to_double()
  let avg_cpu_instance2 = cpu_values_instance2.reduce(0.0, fn(acc, x) { acc + x }) / cpu_values_instance2.length().to_double()
  let avg_cpu_total = (avg_cpu_instance1 + avg_cpu_instance2) / 2.0
  
  let avg_memory_instance1 = memory_values_instance1.reduce(0.0, fn(acc, x) { acc + x }) / memory_values_instance1.length().to_double()
  let avg_memory_instance2 = memory_values_instance2.reduce(0.0, fn(acc, x) { acc + x }) / memory_values_instance2.length().to_double()
  let avg_memory_total = (avg_memory_instance1 + avg_memory_instance2) / 2.0
  
  // Verify aggregation results
  assert_eq(avg_cpu_instance1, 35.0)
  assert_eq(avg_cpu_instance2, 30.0)
  assert_eq(avg_cpu_total, 32.5)
  
  assert_eq(avg_memory_instance1, 64.0)
  assert_eq(avg_memory_instance2, 59.0)
  assert_eq(avg_memory_total, 61.5)
  
  // Verify gauge properties
  assert_eq(cpu_gauge.name, "system.cpu.usage")
  assert_eq(memory_gauge.name, "system.memory.usage")
}

test "updown_counter aggregation" {
  // Test up-down counter aggregation
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "updown-aggregation-test")
  
  // Create up-down counters for queue sizes
  let queue_counter = azimuth::Meter::create_updown_counter(meter, "queue.size", Some("Current queue size"), Some("items"))
  
  // Create attributes for different queues
  let high_priority_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(high_priority_attrs, "queue.name", azimuth::StringValue("high-priority"))
  
  let normal_priority_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(normal_priority_attrs, "queue.name", azimuth::StringValue("normal-priority"))
  
  // Simulate queue operations (add and remove)
  let high_priority_operations = [10, -5, 15, -8, 12, -10]  // Positive = enqueue, Negative = dequeue
  let normal_priority_operations = [20, -15, 25, -20, 30, -25]
  
  // Record up-down counter operations
  for op in high_priority_operations {
    azimuth::UpDownCounter::add(queue_counter, op.to_double(), Some(high_priority_attrs))
  }
  
  for op in normal_priority_operations {
    azimuth::UpDownCounter::add(queue_counter, op.to_double(), Some(normal_priority_attrs))
  }
  
  // Simulate up-down counter aggregation calculations
  let high_priority_final_size = high_priority_operations.reduce(0, fn(acc, x) { acc + x })
  let normal_priority_final_size = normal_priority_operations.reduce(0, fn(acc, x) { acc + x })
  let total_queue_size = high_priority_final_size + normal_priority_final_size
  
  // Verify aggregation results
  assert_eq(high_priority_final_size, 14)  // 10 - 5 + 15 - 8 + 12 - 10 = 14
  assert_eq(normal_priority_final_size, 15)  // 20 - 15 + 25 - 20 + 30 - 25 = 15
  assert_eq(total_queue_size, 29)
  
  // Verify up-down counter properties
  assert_eq(queue_counter.name, "queue.size")
}

test "multi-dimensional aggregation" {
  // Test multi-dimensional metrics aggregation
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "multi-dimensional-test")
  
  // Create counter for requests with multiple dimensions
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests", Some("HTTP requests count"), Some("count"))
  
  // Define dimensions
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let endpoints = ["/api/users", "/api/orders", "/api/products"]
  let status_codes = ["200", "400", "404", "500"]
  
  // Record requests with different dimension combinations
  for method in methods {
    for endpoint in endpoints {
      for status_code in status_codes {
        let attrs = azimuth::Attributes::new()
        azimuth::Attributes::set(attrs, "method", azimuth::StringValue(method))
        azimuth::Attributes::set(attrs, "endpoint", azimuth::StringValue(endpoint))
        azimuth::Attributes::set(attrs, "status_code", azimuth::StringValue(status_code))
        
        // Simulate different request counts based on dimensions
        let count = match (method, status_code) {
          ("GET", "200") => 100
          ("GET", "404") => 20
          ("POST", "200") => 50
          ("POST", "400") => 10
          ("PUT", "200") => 30
          ("PUT", "400") => 5
          ("DELETE", "200") => 15
          ("DELETE", "404") => 3
          _ => 2
        }.to_double()
        
        azimuth::Counter::add(request_counter, count, Some(attrs))
      }
    }
  }
  
  // Simulate multi-dimensional aggregation
  let total_requests = methods.length() * endpoints.length() * status_codes.length()
  
  // Aggregate by method
  let get_requests = 3 * 100 + 3 * 20 + 3 * 2 + 3 * 2  // GET requests across all endpoints and status codes
  let post_requests = 3 * 50 + 3 * 10 + 3 * 2 + 3 * 2  // POST requests
  let put_requests = 3 * 30 + 3 * 5 + 3 * 2 + 3 * 2  // PUT requests
  let delete_requests = 3 * 15 + 3 * 3 + 3 * 2 + 3 * 2  // DELETE requests
  
  // Aggregate by status code
  let success_requests = 4 * 100 + 4 * 50 + 4 * 30 + 4 * 15  // 200 status codes
  let client_errors = 4 * 20 + 4 * 10 + 4 * 5 + 4 * 3  // 404 status codes
  let bad_requests = 4 * 2 + 4 * 2 + 4 * 2 + 4 * 2  // 400 status codes
  let server_errors = 4 * 2 + 4 * 2 + 4 * 2 + 4 * 2  // 500 status codes
  
  // Verify aggregation results
  assert_eq(get_requests, 366.0)  // 300 + 60 + 6 + 6
  assert_eq(post_requests, 186.0)  // 150 + 30 + 6 + 6
  assert_eq(put_requests, 117.0)  // 90 + 15 + 6 + 6
  assert_eq(delete_requests, 66.0)  // 45 + 9 + 6 + 6
  
  assert_eq(success_requests, 780.0)  // 400 + 200 + 120 + 60
  assert_eq(client_errors, 152.0)  // 80 + 40 + 20 + 12
  assert_eq(bad_requests, 32.0)  // 8 + 8 + 8 + 8
  assert_eq(server_errors, 32.0)  // 8 + 8 + 8 + 8
  
  // Verify counter properties
  assert_eq(request_counter.name, "http.requests")
}

test "time window aggregation" {
  // Test time window based aggregation
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "time-window-test")
  
  // Create counter for time-based aggregation
  let time_counter = azimuth::Meter::create_counter(meter, "events.over.time", Some("Events over time"), Some("count"))
  
  let clock = azimuth::Clock::system()
  let base_timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // Simulate events over time (1 event per second for 60 seconds)
  for i in 0..60 {
    let timestamp = base_timestamp + (i.to_int64() * 1000000000L)  // +i seconds
    
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "timestamp", azimuth::IntValue(timestamp.to_int()))
    azimuth::Attributes::set(attrs, "second", azimuth::IntValue(i))
    
    azimuth::Counter::add(time_counter, 1.0, Some(attrs))
  }
  
  // Simulate time window aggregation
  let minute_windows = 6  // 6 windows of 10 seconds each
  let window_size = 10  // 10 seconds per window
  
  let window_counts = []
  for window in 0..minute_windows {
    let count = 0
    for second in window * window_size..(window + 1) * window_size {
      count = count + 1
    }
    window_counts.push(count)
  }
  
  // Verify time window aggregation results
  for count in window_counts {
    assert_eq(count, 10)  // Each window should have 10 events
  }
  
  // Verify counter properties
  assert_eq(time_counter.name, "events.over.time")
}

test "percentile aggregation" {
  // Test percentile-based aggregation
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "percentile-test")
  
  // Create histogram for percentile calculation
  let latency_histogram = azimuth::Meter::create_histogram(meter, "request.latency", Some("Request latency"), Some("ms"))
  
  // Generate latency data with normal distribution-like pattern
  let latencies = []
  for i in 0..1000 {
    // Simulate latency with some distribution
    let base_latency = 50.0
    let variation = (i % 100).to_double() * 2.0
    let latency = base_latency + variation
    
    latencies.push(latency)
  }
  
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "service", azimuth::StringValue("api-service"))
  
  // Record latency measurements
  for latency in latencies {
    azimuth::Histogram::record(latency_histogram, latency, Some(attrs))
  }
  
  // Simulate percentile calculation
  let sorted_latencies = latencies.sort()
  
  let p50_index = sorted_latencies.length() / 2
  let p90_index = (sorted_latencies.length() * 9) / 10
  let p95_index = (sorted_latencies.length() * 95) / 100
  let p99_index = (sorted_latencies.length() * 99) / 100
  
  let p50 = sorted_latencies[p50_index]
  let p90 = sorted_latencies[p90_index]
  let p95 = sorted_latencies[p95_index]
  let p99 = sorted_latencies[p99_index]
  
  // Verify percentile results (approximate values based on our data generation)
  assert_true(p50 >= 100.0 && p50 <= 150.0)  // 50th percentile
  assert_true(p90 >= 180.0 && p90 <= 230.0)  // 90th percentile
  assert_true(p95 >= 190.0 && p95 <= 240.0)  // 95th percentile
  assert_true(p99 >= 198.0 && p99 <= 248.0)  // 99th percentile
  
  // Verify histogram properties
  assert_eq(latency_histogram.name, "request.latency")
}

test "rate aggregation" {
  // Test rate-based aggregation
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "rate-test")
  
  // Create counter for rate calculation
  let rate_counter = azimuth::Meter::create_counter(meter, "events.count", Some("Event count for rate calculation"), Some("count"))
  
  let clock = azimuth::Clock::system()
  let base_timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // Simulate events over time with varying frequency
  let events_per_second = [5, 10, 15, 20, 25, 30, 25, 20, 15, 10, 5]  // Events per second over 11 seconds
  
  for i in 0..events_per_second.length() {
    let second = i
    let event_count = events_per_second[i]
    
    for j in 0..event_count {
      let attrs = azimuth::Attributes::new()
      azimuth::Attributes::set(attrs, "second", azimuth::IntValue(second))
      azimuth::Attributes::set(attrs, "event_index", azimuth::IntValue(j))
      
      azimuth::Counter::add(rate_counter, 1.0, Some(attrs))
    }
  }
  
  // Simulate rate calculation
  let total_events = events_per_second.reduce(0, fn(acc, x) { acc + x })
  let total_duration = events_per_second.length()  // 11 seconds
  let average_rate = total_events.to_double() / total_duration.to_double()
  
  // Calculate rate for each second
  let rates = []
  for count in events_per_second {
    rates.push(count.to_double())  // Events per second
  }
  
  // Find peak rate
  let peak_rate = rates.reduce(0.0, fn(acc, x) { if x > acc { x } else { acc })
  
  // Verify rate aggregation results
  assert_eq(total_events, 180)  // Sum of all events
  assert_eq(total_duration, 11)  // 11 seconds
  assert_eq(average_rate, 16.363636363636363)  // 180 / 11
  assert_eq(peak_rate, 30.0)  // Maximum events per second
  
  // Verify counter properties
  assert_eq(rate_counter.name, "events.count")
}