// Multi-dimensional Metric Queries Test Suite
// Test cases for complex metric queries with multiple dimensions

test "multi-dimensional aggregation with group by operations" {
  // Test multi-dimensional aggregation with group by operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi-dim-test")
  
  let request_counter = Meter::create_counter(meter, "http.requests", Some("HTTP requests"), Some("requests"))
  let latency_histogram = Meter::create_histogram(meter, "http.latency", Some("HTTP latency"), Some("ms"))
  
  // Simulate multi-dimensional data (service, endpoint, method, status_code)
  let multi_dim_data = [
    // Service A endpoints
    ("service-a", "/api/users", "GET", 200, 100.0, 50),
    ("service-a", "/api/users", "GET", 200, 110.0, 45),
    ("service-a", "/api/users", "GET", 200, 105.0, 55),
    ("service-a", "/api/users", "POST", 201, 200.0, 20),
    ("service-a", "/api/users", "POST", 400, 150.0, 5),
    ("service-a", "/api/orders", "GET", 200, 80.0, 30),
    ("service-a", "/api/orders", "GET", 200, 85.0, 35),
    ("service-a", "/api/orders", "POST", 201, 250.0, 15),
    ("service-a", "/api/orders", "POST", 500, 300.0, 2),
    // Service B endpoints
    ("service-b", "/api/products", "GET", 200, 120.0, 60),
    ("service-b", "/api/products", "GET", 200, 125.0, 65),
    ("service-b", "/api/products", "GET", 404, 50.0, 10),
    ("service-b", "/api/products", "POST", 201, 300.0, 25),
    ("service-b", "/api/products", "POST", 400, 200.0, 8),
    ("service-b", "/api/inventory", "GET", 200, 90.0, 40),
    ("service-b", "/api/inventory", "GET", 200, 95.0, 42),
    ("service-b", "/api/inventory", "PUT", 200, 180.0, 18),
    ("service-b", "/api/inventory", "PUT", 404, 60.0, 3)
  ]
  
  // Group by service
  let mut service_a_requests = 0
  let mut service_b_requests = 0
  let mut service_a_latency_sum = 0.0
  let mut service_b_latency_sum = 0.0
  
  // Group by endpoint
  let mut endpoint_requests = []
  let mut endpoint_latency = []
  
  // Group by status code
  let mut status_2xx = 0
  let mut status_4xx = 0
  let mut status_5xx = 0
  
  for (service, endpoint, method, status_code, latency, count) in multi_dim_data {
    // Record metrics
    let attrs = Attributes::new()
    Attributes::set(attrs, "service", StringValue(service))
    Attributes::set(attrs, "endpoint", StringValue(endpoint))
    Attributes::set(attrs, "method", StringValue(method))
    Attributes::set(attrs, "status_code", IntValue(status_code))
    
    Counter::add(request_counter, Double::from_int(count), Some(attrs))
    Histogram::record(latency_histogram, latency, Some(attrs))
    
    // Group by service
    if service == "service-a" {
      service_a_requests = service_a_requests + count
      service_a_latency_sum = service_a_latency_sum + (latency * Double::from_int(count))
    } else if service == "service-b" {
      service_b_requests = service_b_requests + count
      service_b_latency_sum = service_b_latency_sum + (latency * Double::from_int(count))
    }
    
    // Group by status code
    if status_code >= 200 && status_code < 300 {
      status_2xx = status_2xx + count
    } else if status_code >= 400 && status_code < 500 {
      status_4xx = status_4xx + count
    } else if status_code >= 500 {
      status_5xx = status_5xx + count
    }
  }
  
  // Verify counter and histogram properties
  assert_eq(request_counter.name, "http.requests")
  assert_eq(latency_histogram.name, "http.latency")
  
  // Verify group by service aggregation
  assert_true(service_a_requests == 227)
  assert_true(service_b_requests == 271)
  assert_true(service_a_latency_sum > 0.0)
  assert_true(service_b_latency_sum > 0.0)
  
  // Verify group by status code aggregation
  assert_true(status_2xx == 435)
  assert_true(status_4xx == 26)
  assert_true(status_5xx == 2)
}

test "time series queries with downsampling and interpolation" {
  // Test time series queries with downsampling and interpolation
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries-test")
  
  let cpu_gauge = Meter::create_gauge(meter, "system.cpu.usage", Some("CPU usage"), Some("percent"))
  let memory_gauge = Meter::create_gauge(meter, "system.memory.usage", Some("Memory usage"), Some("percent"))
  
  // Simulate high-resolution time series data (every second)
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  let high_resolution_data = [
    (0, 25.5, 45.2),    // Second 0
    (1, 26.1, 45.8),    // Second 1
    (2, 25.8, 46.1),    // Second 2
    (3, 27.2, 46.5),    // Second 3
    (4, 28.9, 47.2),    // Second 4
    (5, 31.5, 48.1),    // Second 5
    (6, 35.2, 49.5),    // Second 6
    (7, 38.7, 51.2),    // Second 7
    (8, 42.1, 53.8),    // Second 8
    (9, 45.6, 56.3),    // Second 9
    (10, 48.9, 58.7),   // Second 10
    (11, 52.3, 61.4),   // Second 11
    (12, 55.8, 64.2),   // Second 12
    (13, 58.2, 66.8),   // Second 13
    (14, 59.7, 68.9),   // Second 14
    (15, 60.1, 70.2),   // Second 15
    (16, 58.9, 69.5),   // Second 16
    (17, 56.4, 67.1),   // Second 17
    (18, 53.2, 64.8),   // Second 18
    (19, 49.8, 62.3),   // Second 19
    (20, 46.5, 59.7),   // Second 20
    (21, 43.1, 57.2),   // Second 21
    (22, 40.3, 55.1),   // Second 22
    (23, 38.2, 53.4),   // Second 23
    (24, 36.8, 52.1),   // Second 24
    (25, 35.9, 51.3),   // Second 25
    (26, 35.2, 50.8),   // Second 26
    (27, 34.8, 50.4),   // Second 27
    (28, 34.5, 50.1),   // Second 28
    (29, 34.3, 49.9)    // Second 29
  ]
  
  // Downsample to 5-second intervals
  let downsampled_interval = 5
  let mut downsampled_cpu = []
  let mut downsampled_memory = []
  
  for (second, cpu, memory) in high_resolution_data {
    // Record high-resolution metrics
    let attrs = Attributes::new()
    Attributes::set(attrs, "timestamp", IntValue(second))
    
    // Simulate gauge setting (would normally set gauge value)
    // Gauge::set(cpu_gauge, cpu, Some(attrs))
    // Gauge::set(memory_gauge, memory, Some(attrs))
    
    // Downsample logic
    if second % downsampled_interval == 0 {
      // Calculate average for the interval
      let mut cpu_sum = 0.0
      let mut memory_sum = 0.0
      let mut count = 0
      
      for (s, c, m) in high_resolution_data {
        if s >= second && s < second + downsampled_interval {
          cpu_sum = cpu_sum + c
          memory_sum = memory_sum + m
          count = count + 1
        }
      }
      
      if count > 0 {
        downsampled_cpu = downsampled_cpu.push(cpu_sum / Double::from_int(count))
        downsampled_memory = downsampled_memory.push(memory_sum / Double::from_int(count))
      }
    }
  }
  
  // Interpolation for missing data points
  let interpolated_cpu = []
  let interpolated_memory = []
  
  // Simple linear interpolation between downsampled points
  for i in 0..<downsampled_cpu.length() - 1 {
    let current_cpu = downsampled_cpu[i]
    let next_cpu = downsampled_cpu[i + 1]
    let current_memory = downsampled_memory[i]
    let next_memory = downsampled_memory[i + 1]
    
    // Add current point
    interpolated_cpu = interpolated_cpu.push(current_cpu)
    interpolated_memory = interpolated_memory.push(current_memory)
    
    // Add interpolated point (midpoint)
    interpolated_cpu = interpolated_cpu.push((current_cpu + next_cpu) / 2.0)
    interpolated_memory = interpolated_memory.push((current_memory + next_memory) / 2.0)
  }
  
  // Add last point
  if downsampled_cpu.length() > 0 {
    interpolated_cpu = interpolated_cpu.push(downsampled_cpu[downsampled_cpu.length() - 1])
    interpolated_memory = interpolated_memory.push(downsampled_memory[downsampled_memory.length() - 1])
  }
  
  // Verify gauge properties
  assert_eq(cpu_gauge.name, "system.cpu.usage")
  assert_eq(memory_gauge.name, "system.memory.usage")
  
  // Verify downsampling and interpolation
  assert_true(downsampled_cpu.length() == 6)  // 30 seconds / 5-second intervals
  assert_true(downsampled_memory.length() == 6)
  assert_true(interpolated_cpu.length() == 11)  // 2 * 6 - 1
  assert_true(interpolated_memory.length() == 11)
}

test "hierarchical aggregation with rollup operations" {
  // Test hierarchical aggregation with rollup operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "hierarchical-test")
  
  let request_counter = Meter::create_counter(meter, "requests", Some("Request counter"), Some("requests"))
  
  // Simulate hierarchical data (region -> datacenter -> service -> endpoint)
  let hierarchical_data = [
    // US West region
    ("us-west", "us-west-1", "auth-service", "/login", 1000),
    ("us-west", "us-west-1", "auth-service", "/logout", 800),
    ("us-west", "us-west-1", "user-service", "/profile", 1200),
    ("us-west", "us-west-1", "user-service", "/settings", 600),
    ("us-west", "us-west-2", "auth-service", "/login", 900),
    ("us-west", "us-west-2", "auth-service", "/logout", 700),
    ("us-west", "us-west-2", "user-service", "/profile", 1100),
    ("us-west", "us-west-2", "user-service", "/settings", 500),
    // US East region
    ("us-east", "us-east-1", "auth-service", "/login", 1500),
    ("us-east", "us-east-1", "auth-service", "/logout", 1200),
    ("us-east", "us-east-1", "user-service", "/profile", 1800),
    ("us-east", "us-east-1", "user-service", "/settings", 900),
    ("us-east", "us-east-2", "auth-service", "/login", 1400),
    ("us-east", "us-east-2", "auth-service", "/logout", 1100),
    ("us-east", "us-east-2", "user-service", "/profile", 1700),
    ("us-east", "us-east-2", "user-service", "/settings", 800),
    // Europe region
    ("eu-west", "eu-west-1", "auth-service", "/login", 800),
    ("eu-west", "eu-west-1", "auth-service", "/logout", 600),
    ("eu-west", "eu-west-1", "user-service", "/profile", 1000),
    ("eu-west", "eu-west-1", "user-service", "/settings", 400),
    ("eu-west", "eu-west-2", "auth-service", "/login", 700),
    ("eu-west", "eu-west-2", "auth-service", "/logout", 500),
    ("eu-west", "eu-west-2", "user-service", "/profile", 900),
    ("eu-west", "eu-west-2", "user-service", "/settings", 300)
  ]
  
  // Hierarchical aggregation maps
  let mut region_totals = []
  let mut datacenter_totals = []
  let mut service_totals = []
  let mut endpoint_totals = []
  
  for (region, datacenter, service, endpoint, count) in hierarchical_data {
    // Record metrics with hierarchical attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, "region", StringValue(region))
    Attributes::set(attrs, "datacenter", StringValue(datacenter))
    Attributes::set(attrs, "service", StringValue(service))
    Attributes::set(attrs, "endpoint", StringValue(endpoint))
    
    Counter::add(request_counter, Double::from_int(count), Some(attrs))
    
    // Aggregate by endpoint
    let endpoint_key = service + ":" + endpoint
    let mut found = false
    endpoint_totals = endpoint_totals.map(fn((key, value)) {
      if key == endpoint_key {
        found = true
        (key, value + count)
      } else {
        (key, value)
      }
    })
    if not(found) {
      endpoint_totals = endpoint_totals.push((endpoint_key, count))
    }
    
    // Aggregate by service
    let mut found = false
    service_totals = service_totals.map(fn((key, value)) {
      if key == service {
        found = true
        (key, value + count)
      } else {
        (key, value)
      }
    })
    if not(found) {
      service_totals = service_totals.push((service, count))
    }
    
    // Aggregate by datacenter
    let mut found = false
    datacenter_totals = datacenter_totals.map(fn((key, value)) {
      if key == datacenter {
        found = true
        (key, value + count)
      } else {
        (key, value)
      }
    })
    if not(found) {
      datacenter_totals = datacenter_totals.push((datacenter, count))
    }
    
    // Aggregate by region
    let mut found = false
    region_totals = region_totals.map(fn((key, value)) {
      if key == region {
        found = true
        (key, value + count)
      } else {
        (key, value)
      }
    })
    if not(found) {
      region_totals = region_totals.push((region, count))
    }
  }
  
  // Verify counter properties
  assert_eq(request_counter.name, "requests")
  
  // Verify hierarchical aggregation
  assert_true(region_totals.length() == 3)  // us-west, us-east, eu-west
  assert_true(datacenter_totals.length() == 6)  // 2 datacenters per region
  assert_true(service_totals.length() == 2)  // auth-service, user-service
  assert_true(endpoint_totals.length() == 4)  // 2 endpoints per service
  
  // Verify rollup consistency (sum of children equals parent)
  let mut total_requests = 0
  for (_, count) in region_totals {
    total_requests = total_requests + count
  }
  assert_true(total_requests == 22000)
}

test "cross-dimensional queries with pivot operations" {
  // Test cross-dimensional queries with pivot operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "pivot-test")
  
  let error_counter = Meter::create_counter(meter, "errors", Some("Error counter"), Some("errors"))
  
  // Simulate multi-dimensional error data
  let error_data = [
    // Service A errors by type and severity
    ("service-a", "timeout", "high", 50),
    ("service-a", "timeout", "medium", 120),
    ("service-a", "connection_error", "high", 30),
    ("service-a", "connection_error", "medium", 80),
    ("service-a", "validation_error", "low", 200),
    ("service-a", "validation_error", "medium", 150),
    // Service B errors by type and severity
    ("service-b", "timeout", "high", 40),
    ("service-b", "timeout", "medium", 100),
    ("service-b", "timeout", "low", 60),
    ("service-b", "connection_error", "high", 25),
    ("service-b", "connection_error", "medium", 70),
    ("service-b", "auth_error", "high", 80),
    ("service-b", "auth_error", "medium", 120),
    // Service C errors by type and severity
    ("service-c", "timeout", "high", 35),
    ("service-c", "timeout", "medium", 90),
    ("service-c", "connection_error", "high", 20),
    ("service-c", "connection_error", "medium", 60),
    ("service-c", "rate_limit", "medium", 180),
    ("service-c", "rate_limit", "low", 220)
  ]
  
  // Pivot: services as rows, error types as columns, severity as filter
  let mut pivot_data = []
  let services = ["service-a", "service-b", "service-c"]
  let error_types = ["timeout", "connection_error", "validation_error", "auth_error", "rate_limit"]
  let severities = ["high", "medium", "low"]
  
  for service in services {
    let mut service_row = []
    for error_type in error_types {
      let mut total_count = 0
      
      for (s, et, severity, count) in error_data {
        if s == service && et == error_type {
          // Only count high and medium severity for this pivot
          if severity == "high" || severity == "medium" {
            total_count = total_count + count
          }
        }
      }
      
      service_row = service_row.push(total_count)
    }
    pivot_data = pivot_data.push((service, service_row))
  }
  
  // Record metrics for pivot verification
  for (service, error_type, severity, count) in error_data {
    let attrs = Attributes::new()
    Attributes::set(attrs, "service", StringValue(service))
    Attributes::set(attrs, "error_type", StringValue(error_type))
    Attributes::set(attrs, "severity", StringValue(severity))
    
    Counter::add(error_counter, Double::from_int(count), Some(attrs))
  }
  
  // Verify counter properties
  assert_eq(error_counter.name, "errors")
  
  // Verify pivot operation
  assert_true(pivot_data.length() == 3)  // 3 services
  assert_true(pivot_data[0].1.length() == 5)  // 5 error types per service
  
  // Verify specific pivot values
  // Service A: timeout(170), connection_error(110), validation_error(150), auth_error(0), rate_limit(0)
  assert_true(pivot_data[0].1[0] == 170)  // service-a, timeout
  assert_true(pivot_data[0].1[1] == 110)  // service-a, connection_error
  assert_true(pivot_data[0].1[2] == 150)  // service-a, validation_error
  assert_true(pivot_data[0].1[3] == 0)    // service-a, auth_error
  assert_true(pivot_data[0].1[4] == 0)    // service-a, rate_limit
  
  // Service B: timeout(140), connection_error(95), validation_error(0), auth_error(200), rate_limit(0)
  assert_true(pivot_data[1].1[0] == 140)  // service-b, timeout
  assert_true(pivot_data[1].1[1] == 95)   // service-b, connection_error
  assert_true(pivot_data[1].1[2] == 0)    // service-b, validation_error
  assert_true(pivot_data[1].1[3] == 200)  // service-b, auth_error
  assert_true(pivot_data[1].1[4] == 0)    // service-b, rate_limit
  
  // Service C: timeout(125), connection_error(80), validation_error(0), auth_error(0), rate_limit(180)
  assert_true(pivot_data[2].1[0] == 125)  // service-c, timeout
  assert_true(pivot_data[2].1[1] == 80)   // service-c, connection_error
  assert_true(pivot_data[2].1[2] == 0)    // service-c, validation_error
  assert_true(pivot_data[2].1[3] == 0)    // service-c, auth_error
  assert_true(pivot_data[2].1[4] == 180)  // service-c, rate_limit
}

test "filtering and slicing with multi-dimensional predicates" {
  // Test advanced filtering and slicing with multi-dimensional predicates
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "filtering-test")
  
  let performance_histogram = Meter::create_histogram(meter, "performance", Some("Performance metrics"), Some("ms"))
  
  // Simulate multi-dimensional performance data
  let performance_data = [
    // Service performance across different environments and versions
    ("payment-service", "production", "v1.2.0", "us-east", "fast", 50.0, 1000),
    ("payment-service", "production", "v1.2.0", "us-west", "fast", 55.0, 950),
    ("payment-service", "production", "v1.2.0", "eu-west", "medium", 80.0, 800),
    ("payment-service", "production", "v1.1.0", "us-east", "slow", 120.0, 200),
    ("payment-service", "staging", "v1.3.0-beta", "us-east", "medium", 75.0, 150),
    ("payment-service", "staging", "v1.3.0-beta", "us-west", "fast", 60.0, 120),
    ("user-service", "production", "v2.1.0", "us-east", "fast", 40.0, 2000),
    ("user-service", "production", "v2.1.0", "us-west", "fast", 45.0, 1900),
    ("user-service", "production", "v2.1.0", "eu-west", "fast", 50.0, 1800),
    ("user-service", "production", "v2.0.0", "us-east", "medium", 70.0, 300),
    ("user-service", "staging", "v2.2.0-alpha", "us-east", "medium", 65.0, 100),
    ("user-service", "staging", "v2.2.0-alpha", "us-west", "fast", 42.0, 80),
    ("inventory-service", "production", "v1.5.0", "us-east", "slow", 150.0, 500),
    ("inventory-service", "production", "v1.5.0", "us-west", "slow", 160.0, 480),
    ("inventory-service", "production", "v1.4.0", "us-east", "medium", 90.0, 150),
    ("inventory-service", "staging", "v1.6.0-rc", "us-east", "medium", 85.0, 50)
  ]
  
  // Apply various filters
  
  // Filter 1: Production environment only
  let production_data = []
  for (service, env, version, region, perf_category, latency, request_count) in performance_data {
    if env == "production" {
      production_data = production_data.push((service, env, version, region, perf_category, latency, request_count))
    }
  }
  
  // Filter 2: Fast performing services only
  let fast_performers = []
  for (service, env, version, region, perf_category, latency, request_count) in production_data {
    if perf_category == "fast" {
      fast_performers = fast_performers.push((service, env, version, region, perf_category, latency, request_count))
    }
  }
  
  // Filter 3: Services with latency < 60ms in production
  let low_latency_services = []
  for (service, env, version, region, perf_category, latency, request_count) in production_data {
    if latency < 60.0 {
      low_latency_services = low_latency_services.push((service, env, version, region, perf_category, latency, request_count))
    }
  }
  
  // Filter 4: High traffic services (>1000 requests) in us-east
  let high_traffic_us_east = []
  for (service, env, version, region, perf_category, latency, request_count) in production_data {
    if region == "us-east" && request_count > 1000 {
      high_traffic_us_east = high_traffic_us_east.push((service, env, version, region, perf_category, latency, request_count))
    }
  }
  
  // Record metrics for all data
  for (service, env, version, region, perf_category, latency, request_count) in performance_data {
    let attrs = Attributes::new()
    Attributes::set(attrs, "service", StringValue(service))
    Attributes::set(attrs, "environment", StringValue(env))
    Attributes::set(attrs, "version", StringValue(version))
    Attributes::set(attrs, "region", StringValue(region))
    Attributes::set(attrs, "performance_category", StringValue(perf_category))
    
    Histogram::record(performance_histogram, latency, Some(attrs))
  }
  
  // Verify histogram properties
  assert_eq(performance_histogram.name, "performance")
  
  // Verify filtering results
  assert_true(production_data.length() == 10)  // 10 production entries
  assert_true(fast_performers.length() == 5)   // 5 fast performers in production
  assert_true(low_latency_services.length() == 4)  // 4 services with latency < 60ms
  assert_true(high_traffic_us_east.length() == 3)  // 3 high traffic services in us-east
}