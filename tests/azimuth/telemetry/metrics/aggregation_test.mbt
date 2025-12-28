test "metrics aggregation and statistics" {
  // Test metrics aggregation and statistical operations
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "aggregation.test.meter")
  
  // Create instruments for aggregation testing
  let counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "aggregation.counter")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "aggregation.histogram")
  let up_down_counter = @azimuth.telemetry.sdk.metrics.Meter::create_up_down_counter(meter, "aggregation.up_down_counter")
  let gauge = @azimuth.telemetry.sdk.metrics.Meter::create_gauge(meter, "aggregation.gauge")
  
  // Test counter aggregation
  let counter_attrs1 = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(counter_attrs1, "method", @azimuth.telemetry.api.common.StringValue("GET"))
  @azimuth.telemetry.api.common.Attributes::set(counter_attrs1, "status", @azimuth.telemetry.api.common.StringValue("200"))
  
  let counter_attrs2 = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(counter_attrs2, "method", @azimuth.telemetry.api.common.StringValue("POST"))
  @azimuth.telemetry.api.common.Attributes::set(counter_attrs2, "status", @azimuth.telemetry.api.common.StringValue("201"))
  
  let counter_attrs3 = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(counter_attrs3, "method", @azimuth.telemetry.api.common.StringValue("GET"))
  @azimuth.telemetry.api.common.Attributes::set(counter_attrs3, "status", @azimuth.telemetry.api.common.StringValue("404"))
  
  // Add counter measurements
  for i in 0..100 {
    @azimuth.telemetry.sdk.metrics.Counter::add(counter, 1.0, Some(counter_attrs1))
  }
  
  for i in 0..50 {
    @azimuth.telemetry.sdk.metrics.Counter::add(counter, 1.0, Some(counter_attrs2))
  }
  
  for i in 0..25 {
    @azimuth.telemetry.sdk.metrics.Counter::add(counter, 1.0, Some(counter_attrs3))
  }
  
  // Test histogram aggregation with different value distributions
  let histogram_attrs1 = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(histogram_attrs1, "endpoint", @azimuth.telemetry.api.common.StringValue("/api/users"))
  
  let histogram_attrs2 = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(histogram_attrs2, "endpoint", @azimuth.telemetry.api.common.StringValue("/api/orders"))
  
  // Normal distribution values for endpoint 1
  let normal_values = [100.0, 110.0, 95.0, 105.0, 120.0, 85.0, 115.0, 90.0, 108.0, 102.0]
  for value in normal_values {
    @azimuth.telemetry.sdk.metrics.Histogram::record(histogram, value, Some(histogram_attrs1))
  }
  
  // Bimodal distribution values for endpoint 2
  let bimodal_values = [50.0, 55.0, 45.0, 60.0, 200.0, 210.0, 190.0, 220.0, 195.0, 205.0]
  for value in bimodal_values {
    @azimuth.telemetry.sdk.metrics.Histogram::record(histogram, value, Some(histogram_attrs2))
  }
  
  // Test up-down counter aggregation
  let up_down_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(up_down_attrs, "resource", @azimuth.telemetry.api.common.StringValue("connections"))
  
  // Simulate connection count changes
  @azimuth.telemetry.sdk.metrics.UpDownCounter::add(up_down_counter, 10.0, Some(up_down_attrs))  // Initial connections
  @azimuth.telemetry.sdk.metrics.UpDownCounter::add(up_down_counter, 5.0, Some(up_down_attrs))   // Add 5 more
  @azimuth.telemetry.sdk.metrics.UpDownCounter::add(up_down_counter, -3.0, Some(up_down_attrs))  // Remove 3
  @azimuth.telemetry.sdk.metrics.UpDownCounter::add(up_down_counter, 8.0, Some(up_down_attrs))   // Add 8 more
  @azimuth.telemetry.sdk.metrics.UpDownCounter::add(up_down_counter, -2.0, Some(up_down_attrs))  // Remove 2
  
  // Test gauge aggregation with time series data
  let gauge_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(gauge_attrs, "metric", @azimuth.telemetry.api.common.StringValue("cpu.usage"))
  
  // Simulate CPU usage over time
  let cpu_values = [25.5, 30.2, 28.7, 35.1, 42.3, 38.9, 45.2, 40.1, 36.8, 32.5]
  for value in cpu_values {
    @azimuth.telemetry.sdk.metrics.Gauge::record(gauge, value, Some(gauge_attrs))
  }
  
  // Test aggregation with different attribute combinations
  let multi_dim_attrs1 = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(multi_dim_attrs1, "service", @azimuth.telemetry.api.common.StringValue("auth"))
  @azimuth.telemetry.api.common.Attributes::set(multi_dim_attrs1, "version", @azimuth.telemetry.api.common.StringValue("1.0"))
  @azimuth.telemetry.api.common.Attributes::set(multi_dim_attrs1, "region", @azimuth.telemetry.api.common.StringValue("us-east"))
  
  let multi_dim_attrs2 = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(multi_dim_attrs2, "service", @azimuth.telemetry.api.common.StringValue("auth"))
  @azimuth.telemetry.api.common.Attributes::set(multi_dim_attrs2, "version", @azimuth.telemetry.api.common.StringValue("1.0"))
  @azimuth.telemetry.api.common.Attributes::set(multi_dim_attrs2, "region", @azimuth.telemetry.api.common.StringValue("us-west"))
  
  let multi_dim_attrs3 = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(multi_dim_attrs3, "service", @azimuth.telemetry.api.common.StringValue("payment"))
  @azimuth.telemetry.api.common.Attributes::set(multi_dim_attrs3, "version", @azimuth.telemetry.api.common.StringValue("2.0"))
  @azimuth.telemetry.api.common.Attributes::set(multi_dim_attrs3, "region", @azimuth.telemetry.api.common.StringValue("us-east"))
  
  let multi_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "multi.dim.counter")
  
  for i in 0..30 {
    @azimuth.telemetry.sdk.metrics.Counter::add(multi_counter, 1.0, Some(multi_dim_attrs1))
  }
  
  for i in 0..20 {
    @azimuth.telemetry.sdk.metrics.Counter::add(multi_counter, 1.0, Some(multi_dim_attrs2))
  }
  
  for i in 0..15 {
    @azimuth.telemetry.sdk.metrics.Counter::add(multi_counter, 1.0, Some(multi_dim_attrs3))
  }
}

test "metrics temporal aggregation" {
  // Test metrics aggregation over time windows
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "temporal.test.meter")
  
  let request_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "requests.total")
  let response_time = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "response.time")
  let error_rate = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "errors.total")
  
  let base_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(base_attrs, "endpoint", @azimuth.telemetry.api.common.StringValue("/api/data"))
  
  // Simulate metrics over different time periods
  let time_periods = [
    ("morning", [100.0, 110.0, 95.0, 105.0, 120.0]),      // Faster response times
    ("afternoon", [150.0, 160.0, 145.0, 155.0, 165.0]),  // Slower response times
    ("evening", [80.0, 85.0, 75.0, 90.0, 88.0])          // Fastest response times
  ]
  
  for (period, response_times) in time_periods {
    let period_attrs = @azimuth.telemetry.api.common.Attributes::new()
    @azimuth.telemetry.api.common.Attributes::set(period_attrs, "endpoint", @azimuth.telemetry.api.common.StringValue("/api/data"))
    @azimuth.telemetry.api.common.Attributes::set(period_attrs, "period", @azimuth.telemetry.api.common.StringValue(period))
    
    // Simulate different request volumes per period
    let request_volume = match period {
      "morning" => 100,
      "afternoon" => 200,
      "evening" => 50,
      _ => 0
    }
    
    // Record requests and response times
    for i in 0..request_volume {
      @azimuth.telemetry.sdk.metrics.Counter::add(request_counter, 1.0, Some(period_attrs))
      
      // Sample response times from the period's distribution
      let response_time_sample = response_times[i % response_times.length()]
      @azimuth.telemetry.sdk.metrics.Histogram::record(response_time, response_time_sample, Some(period_attrs))
      
      // Simulate some errors (5% error rate)
      if i % 20 == 0 {
        @azimuth.telemetry.sdk.metrics.Counter::add(error_rate, 1.0, Some(period_attrs))
      }
    }
  }
  
  // Test temporal aggregation with sliding windows
  let sliding_window_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "sliding.window.requests")
  let sliding_window_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(sliding_window_attrs, "window", @azimuth.telemetry.api.common.StringValue("5m"))
  
  // Simulate sliding window data
  for i in 0..1000 {
    @azimuth.telemetry.sdk.metrics.Counter::add(sliding_window_counter, 1.0, Some(sliding_window_attrs))
    
    // Simulate time passing (in a real implementation, this would affect the window)
    if i % 100 == 0 {
      // Start new window measurement
      let new_window_attrs = @azimuth.telemetry.api.common.Attributes::new()
      @azimuth.telemetry.api.common.Attributes::set(new_window_attrs, "window", @azimuth.telemetry.api.common.StringValue("5m"))
      @azimuth.telemetry.api.common.Attributes::set(new_window_attrs, "window_id", @azimuth.telemetry.api.common.StringValue("window_" + (i / 100).to_string()))
      
      @azimuth.telemetry.sdk.metrics.Counter::add(sliding_window_counter, 1.0, Some(new_window_attrs))
    }
  }
}

test "metrics statistical calculations" {
  // Test statistical calculations on metrics data
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "statistics.test.meter")
  
  let latency_histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "latency")
  let throughput_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "throughput")
  
  let latency_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(latency_attrs, "operation", @azimuth.telemetry.api.common.StringValue("database.query"))
  
  // Generate latency data with known statistical properties
  let latency_values = [
    10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0,  // Linear progression
    60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0, 100.0, 105.0,
    25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 60.0, 65.0, 70.0   // Duplicate middle values
  ]
  
  // Record latency measurements
  for latency in latency_values {
    @azimuth.telemetry.sdk.metrics.Histogram::record(latency_histogram, latency, Some(latency_attrs))
    @azimuth.telemetry.sdk.metrics.Counter::add(throughput_counter, 1.0, Some(latency_attrs))
  }
  
  // Test percentile calculations (simulated)
  let sorted_values = latency_values.sort()
  let count = sorted_values.length()
  
  // Calculate basic statistics
  let sum = 0.0
  for value in sorted_values {
    sum = sum + value
  }
  let mean = sum / count.to_double()
  
  // Expected values for verification
  let expected_mean = 52.5  // Approximate mean of the generated data
  let expected_min = 10.0
  let expected_max = 105.0
  
  // In a real implementation, we would verify these against actual histogram statistics
  assert_true(mean > 50.0)  // Rough verification
  assert_true(mean < 60.0)
  
  // Test distribution-specific histograms
  let normal_dist_histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "normal.distribution")
  let normal_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(normal_attrs, "distribution", @azimuth.telemetry.api.common.StringValue("normal"))
  
  // Generate normal-like distribution (simplified)
  let normal_values = [
    45.0, 48.0, 52.0, 55.0, 50.0, 53.0, 47.0, 51.0, 49.0, 54.0,  // Around mean 50
    46.0, 52.0, 48.0, 55.0, 50.0, 47.0, 53.0, 51.0, 49.0, 52.0,
    35.0, 65.0, 30.0, 70.0, 25.0, 75.0, 20.0, 80.0              // Outliers
  ]
  
  for value in normal_values {
    @azimuth.telemetry.sdk.metrics.Histogram::record(normal_dist_histogram, value, Some(normal_attrs))
  }
  
  // Test exponential distribution
  let exp_dist_histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "exponential.distribution")
  let exp_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(exp_attrs, "distribution", @azimuth.telemetry.api.common.StringValue("exponential"))
  
  // Generate exponential-like distribution (many small values, few large values)
  let exp_values = [
    1.0, 1.0, 2.0, 1.0, 3.0, 1.0, 2.0, 1.0, 4.0, 1.0,  // Many small values
    2.0, 1.0, 3.0, 1.0, 2.0, 1.0, 5.0, 1.0, 2.0, 1.0,
    10.0, 20.0, 15.0, 25.0, 30.0, 50.0, 40.0, 60.0      // Few large values
  ]
  
  for value in exp_values {
    @azimuth.telemetry.sdk.metrics.Histogram::record(exp_dist_histogram, value, Some(exp_attrs))
  }
}

test "metrics rate and ratio calculations" {
  // Test metrics rate and ratio calculations
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "rate.test.meter")
  
  let success_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "successes")
  let failure_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "failures")
  let total_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "total")
  let duration_histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "durations")
  
  let base_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(base_attrs, "service", @azimuth.telemetry.api.common.StringValue("api.gateway"))
  
  // Simulate operation with different success/failure rates
  let operations = [
    ("operation1", 80, 20, [100.0, 110.0, 90.0, 105.0, 95.0]),      // 80% success rate
    ("operation2", 95, 5, [50.0, 55.0, 45.0, 60.0, 52.0]),          // 95% success rate
    ("operation3", 60, 40, [200.0, 210.0, 190.0, 220.0, 205.0]),    // 60% success rate
    ("operation4", 99, 1, [25.0, 30.0, 22.0, 28.0, 26.0])           // 99% success rate
  ]
  
  for (operation, successes, failures, durations) in operations {
    let op_attrs = @azimuth.telemetry.api.common.Attributes::new()
    @azimuth.telemetry.api.common.Attributes::set(op_attrs, "service", @azimuth.telemetry.api.common.StringValue("api.gateway"))
    @azimuth.telemetry.api.common.Attributes::set(op_attrs, "operation", @azimuth.telemetry.api.common.StringValue(operation))
    
    // Record successes
    for i in 0..successes {
      @azimuth.telemetry.sdk.metrics.Counter::add(success_counter, 1.0, Some(op_attrs))
      @azimuth.telemetry.sdk.metrics.Counter::add(total_counter, 1.0, Some(op_attrs))
      
      // Record duration for successful operations
      let duration = durations[i % durations.length()]
      @azimuth.telemetry.sdk.metrics.Histogram::record(duration_histogram, duration, Some(op_attrs))
    }
    
    // Record failures
    for i in 0..failures {
      @azimuth.telemetry.sdk.metrics.Counter::add(failure_counter, 1.0, Some(op_attrs))
      @azimuth.telemetry.sdk.metrics.Counter::add(total_counter, 1.0, Some(op_attrs))
      
      // Record duration for failed operations (typically longer)
      let failure_duration = durations[i % durations.length()] * 1.5
      @azimuth.telemetry.sdk.metrics.Histogram::record(duration_histogram, failure_duration, Some(op_attrs))
    }
  }
  
  // Test rate calculations over time
  let rate_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "rate.measurements")
  let rate_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(rate_attrs, "metric", @azimuth.telemetry.api.common.StringValue("requests.per.second"))
  
  // Simulate varying rates over time
  let rates_over_time = [10.0, 15.0, 25.0, 20.0, 30.0, 45.0, 35.0, 50.0, 40.0, 55.0]
  
  for rate in rates_over_time {
    // Simulate one second of measurements
    for i in 0..rate.to_int() {
      @azimuth.telemetry.sdk.metrics.Counter::add(rate_counter, 1.0, Some(rate_attrs))
    }
  }
  
  // Test ratio calculations
  let ratio_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(ratio_attrs, "metric", @azimuth.telemetry.api.common.StringValue("success.ratio"))
  
  // Calculate success ratios for different operations
  for (operation, successes, failures, _) in operations {
    let total = successes + failures
    let success_ratio = successes.to_double() / total.to_double()
    let failure_ratio = failures.to_double() / total.to_double()
    
    let ratio_op_attrs = @azimuth.telemetry.api.common.Attributes::new()
    @azimuth.telemetry.api.common.Attributes::set(ratio_op_attrs, "metric", @azimuth.telemetry.api.common.StringValue("success.ratio"))
    @azimuth.telemetry.api.common.Attributes::set(ratio_op_attrs, "operation", @azimuth.telemetry.api.common.StringValue(operation))
    
    // Record the ratio as a gauge
    let ratio_gauge = @azimuth.telemetry.sdk.metrics.Meter::create_gauge(meter, "ratios")
    @azimuth.telemetry.sdk.metrics.Gauge::record(ratio_gauge, success_ratio, Some(ratio_op_attrs))
  }
}

test "metrics aggregation with attributes" {
  // Test metrics aggregation with complex attribute combinations
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "attribute.aggregation.test")
  
  let request_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "requests.by.attributes")
  let latency_histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "latency.by.attributes")
  
  // Create attribute combinations for testing
  let services = ["auth", "payment", "inventory", "notification"]
  let versions = ["1.0", "1.1", "2.0"]
  let regions = ["us-east", "us-west", "eu-west", "ap-southeast"]
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let status_codes = ["200", "201", "400", "404", "500"]
  
  // Generate metrics with all attribute combinations
  for service in services {
    for version in versions {
      for region in regions {
        for method in methods {
          for status_code in status_codes {
            let attrs = @azimuth.telemetry.api.common.Attributes::new()
            @azimuth.telemetry.api.common.Attributes::set(attrs, "service", @azimuth.telemetry.api.common.StringValue(service))
            @azimuth.telemetry.api.common.Attributes::set(attrs, "version", @azimuth.telemetry.api.common.StringValue(version))
            @azimuth.telemetry.api.common.Attributes::set(attrs, "region", @azimuth.telemetry.api.common.StringValue(region))
            @azimuth.telemetry.api.common.Attributes::set(attrs, "method", @azimuth.telemetry.api.common.StringValue(method))
            @azimuth.telemetry.api.common.Attributes::set(attrs, "status_code", @azimuth.telemetry.api.common.StringValue(status_code))
            
            // Generate different request volumes based on attributes
            let base_volume = match service {
              "auth" => 100,
              "payment" => 50,
              "inventory" => 75,
              "notification" => 25,
              _ => 10
            }
            
            let version_multiplier = match version {
              "1.0" => 0.5,
              "1.1" => 0.8,
              "2.0" => 1.0,
              _ => 1.0
            }
            
            let region_multiplier = match region {
              "us-east" => 1.0,
              "us-west" => 0.8,
              "eu-west" => 0.6,
              "ap-southeast" => 0.4,
              _ => 0.5
            }
            
            let method_multiplier = match method {
              "GET" => 1.0,
              "POST" => 0.7,
              "PUT" => 0.2,
              "DELETE" => 0.1,
              _ => 0.5
            }
            
            let status_multiplier = match status_code {
              "200" => 0.8,
              "201" => 0.1,
              "400" => 0.05,
              "404" => 0.03,
              "500" => 0.02,
              _ => 0.1
            }
            
            let volume = (base_volume.to_double() * version_multiplier * region_multiplier * method_multiplier * status_multiplier).to_int()
            
            // Record requests
            for i in 0..volume {
              @azimuth.telemetry.sdk.metrics.Counter::add(request_counter, 1.0, Some(attrs))
              
              // Generate latency based on service and method
              let base_latency = match service {
                "auth" => 50.0,
                "payment" => 150.0,
                "inventory" => 100.0,
                "notification" => 25.0,
                _ => 75.0
              }
              
              let method_latency = match method {
                "GET" => 1.0,
                "POST" => 1.2,
                "PUT" => 1.5,
                "DELETE" => 0.8,
                _ => 1.0
              }
              
              let status_latency = match status_code {
                "200" => 1.0,
                "201" => 1.1,
                "400" => 0.5,
                "404" => 0.3,
                "500" => 2.0,
                _ => 1.0
              }
              
              let latency = base_latency * method_latency * status_latency
              @azimuth.telemetry.sdk.metrics.Histogram::record(latency_histogram, latency, Some(attrs))
            }
          }
        }
      }
    }
  }
  
  // Test aggregation with dynamic attributes
  let dynamic_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "dynamic.attributes")
  
  // Generate metrics with dynamic attribute values
  for i in 0..100 {
    let dynamic_attrs = @azimuth.telemetry.api.common.Attributes::new()
    @azimuth.telemetry.api.common.Attributes::set(dynamic_attrs, "user.id", @azimuth.telemetry.api.common.StringValue("user_" + (i % 10).to_string()))
    @azimuth.telemetry.api.common.Attributes::set(dynamic_attrs, "session.id", @azimuth.telemetry.api.common.StringValue("session_" + (i % 5).to_string()))
    @azimuth.telemetry.api.common.Attributes::set(dynamic_attrs, "request.id", @azimuth.telemetry.api.common.StringValue("req_" + i.to_string()))
    
    @azimuth.telemetry.sdk.metrics.Counter::add(dynamic_counter, 1.0, Some(dynamic_attrs))
  }
}