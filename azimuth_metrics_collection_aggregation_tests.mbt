// Azimuth Metrics Collection and Aggregation Test Suite
// This file contains comprehensive test cases for metrics collection and aggregation

// Test 1: Basic Counter Collection
test "basic counter collection" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "counter_test_meter")
  
  // Create a counter
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  // Record some values
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5)
  Counter::add(counter, 3.0)
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let counter_metrics = metrics.filter(fn(m) { m.name == "test_counter" })
  
  assert_eq(counter_metrics.length(), 3)
  
  // Verify metric values
  let values = counter_metrics.map(fn(m) { m.value })
  assert_true(values.contains(1.0))
  assert_true(values.contains(2.5))
  assert_true(values.contains(3.0))
  
  // Verify metric properties
  for metric in counter_metrics {
    assert_eq(metric.name, "test_counter")
    assert_eq(metric.description, Some("Test counter"))
    assert_eq(metric.unit, Some("count"))
    assert_eq(metric.metric_type, Counter)
  }
}

// Test 2: Counter with Attributes
test "counter with attributes" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "attribute_counter_meter")
  
  // Create a counter
  let counter = Meter::create_counter(meter, "attribute_counter")
  
  // Record values with different attributes
  let web_attrs = Attributes::new()
  Attributes::set(web_attrs, "service", "web")
  Attributes::set(web_attrs, "endpoint", "/api/users")
  Counter::add(counter, 5.0, Some(web_attrs))
  
  let db_attrs = Attributes::new()
  Attributes::set(db_attrs, "service", "database")
  Attributes::set(db_attrs, "operation", "query")
  Counter::add(counter, 3.0, Some(db_attrs))
  
  let cache_attrs = Attributes::new()
  Attributes::set(cache_attrs, "service", "cache")
  Attributes::set(cache_attrs, "operation", "get")
  Counter::add(counter, 10.0, Some(cache_attrs))
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let counter_metrics = metrics.filter(fn(m) { m.name == "attribute_counter" })
  
  assert_eq(counter_metrics.length(), 3)
  
  // Verify attribute combinations
  let web_metric = counter_metrics.find(fn(m) { 
    m.attributes.contains(("service", "web")) && m.attributes.contains(("endpoint", "/api/users"))
  })
  let db_metric = counter_metrics.find(fn(m) { 
    m.attributes.contains(("service", "database")) && m.attributes.contains(("operation", "query"))
  })
  let cache_metric = counter_metrics.find(fn(m) { 
    m.attributes.contains(("service", "cache")) && m.attributes.contains(("operation", "get"))
  })
  
  match (web_metric, db_metric, cache_metric) {
    (Some(web), Some(db), Some(cache)) => {
      assert_eq(web.value, 5.0)
      assert_eq(db.value, 3.0)
      assert_eq(cache.value, 10.0)
    }
    _ => assert_true(false)
  }
}

// Test 3: Histogram Collection
test "histogram collection" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "histogram_test_meter")
  
  // Create a histogram
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time"), Some("ms"))
  
  // Record some values
  Histogram::record(histogram, 10.0)
  Histogram::record(histogram, 25.0)
  Histogram::record(histogram, 50.0)
  Histogram::record(histogram, 75.0)
  Histogram::record(histogram, 100.0)
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let histogram_metrics = metrics.filter(fn(m) { m.name == "response_time" })
  
  assert_eq(histogram_metrics.length(), 5)
  
  // Verify metric values
  let values = histogram_metrics.map(fn(m) { m.value })
  assert_true(values.contains(10.0))
  assert_true(values.contains(25.0))
  assert_true(values.contains(50.0))
  assert_true(values.contains(75.0))
  assert_true(values.contains(100.0))
  
  // Verify metric properties
  for metric in histogram_metrics {
    assert_eq(metric.name, "response_time")
    assert_eq(metric.description, Some("Response time"))
    assert_eq(metric.unit, Some("ms"))
    assert_eq(metric.metric_type, Histogram)
  }
}

// Test 4: Histogram with Buckets
test "histogram with buckets" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "bucket_histogram_meter")
  
  // Define explicit buckets
  let buckets = [5.0, 10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0]
  
  // Create a histogram with explicit buckets
  let histogram = Meter::create_histogram_with_buckets(meter, "latency", Some("Request latency"), Some("ms"), buckets)
  
  // Record values that fall into different buckets
  let values = [2.0, 7.0, 15.0, 40.0, 75.0, 150.0, 300.0, 750.0, 1200.0]
  for value in values {
    Histogram::record(histogram, value)
  }
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  // We should have one metric for each value plus bucket aggregations
  assert_true(metrics.length() >= values.length())
  
  // Verify bucket aggregations
  let bucket_metrics = metrics.filter(fn(m) { m.name == "latency_bucket" })
  assert_true(bucket_metrics.length() > 0)
  
  // Verify count and sum metrics
  let count_metric = metrics.find(fn(m) { m.name == "latency_count" })
  let sum_metric = metrics.find(fn(m) { m.name == "latency_sum" })
  
  match (count_metric, sum_metric) {
    (Some(count), Some(sum)) => {
      assert_eq(count.value, values.length().to_float())
      assert_eq(sum.value, values.reduce(fn(acc, v) { acc + v }, 0.0))
    }
    _ => assert_true(false)
  }
}

// Test 5: Gauge Collection
test "gauge collection" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "gauge_test_meter")
  
  // Create a gauge
  let gauge = Meter::create_gauge(meter, "memory_usage", Some("Memory usage"), Some("MB"))
  
  // Set some values
  Gauge::set(gauge, 100.0)
  Gauge::set(gauge, 150.0)
  Gauge::set(gauge, 75.0)
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let gauge_metrics = metrics.filter(fn(m) { m.name == "memory_usage" })
  
  assert_eq(gauge_metrics.length(), 3)
  
  // Verify metric values
  let values = gauge_metrics.map(fn(m) { m.value })
  assert_true(values.contains(100.0))
  assert_true(values.contains(150.0))
  assert_true(values.contains(75.0))
  
  // Verify metric properties
  for metric in gauge_metrics {
    assert_eq(metric.name, "memory_usage")
    assert_eq(metric.description, Some("Memory usage"))
    assert_eq(metric.unit, Some("MB"))
    assert_eq(metric.metric_type, Gauge)
  }
}

// Test 6: UpDown Counter Collection
test "updown counter collection" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "updown_counter_meter")
  
  // Create an up-down counter
  let updown_counter = Meter::create_updown_counter(meter, "queue_size", Some("Queue size"), Some("items"))
  
  // Record increments and decrements
  UpDownCounter::add(updown_counter, 10.0)  // Initial size
  UpDownCounter::add(updown_counter, 5.0)   // Add 5 items
  UpDownCounter::add(updown_counter, -3.0)  // Remove 3 items
  UpDownCounter::add(updown_counter, -2.0)  // Remove 2 items
  UpDownCounter::add(updown_counter, 8.0)   // Add 8 items
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let updown_metrics = metrics.filter(fn(m) { m.name == "queue_size" })
  
  assert_eq(updown_metrics.length(), 5)
  
  // Verify metric values
  let values = updown_metrics.map(fn(m) { m.value })
  assert_true(values.contains(10.0))
  assert_true(values.contains(5.0))
  assert_true(values.contains(-3.0))
  assert_true(values.contains(-2.0))
  assert_true(values.contains(8.0))
  
  // Verify metric properties
  for metric in updown_metrics {
    assert_eq(metric.name, "queue_size")
    assert_eq(metric.description, Some("Queue size"))
    assert_eq(metric.unit, Some("items"))
    assert_eq(metric.metric_type, UpDownCounter)
  }
}

// Test 7: Multiple Metric Types Collection
test "multiple metric types collection" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "multi_type_meter")
  
  // Create different metric types
  let counter = Meter::create_counter(meter, "requests_total")
  let histogram = Meter::create_histogram(meter, "request_duration")
  let gauge = Meter::create_gauge(meter, "active_connections")
  let updown_counter = Meter::create_updown_counter(meter, "queue_size")
  
  // Record values for each metric type
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 100.0)
  Gauge::set(gauge, 25.0)
  UpDownCounter::add(updown_counter, 10.0)
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify all metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  let counter_metrics = metrics.filter(fn(m) { m.name == "requests_total" })
  let histogram_metrics = metrics.filter(fn(m) { m.name == "request_duration" })
  let gauge_metrics = metrics.filter(fn(m) { m.name == "active_connections" })
  let updown_metrics = metrics.filter(fn(m) { m.name == "queue_size" })
  
  assert_eq(counter_metrics.length(), 1)
  assert_eq(histogram_metrics.length(), 1)
  assert_eq(gauge_metrics.length(), 1)
  assert_eq(updown_metrics.length(), 1)
  
  // Verify metric types
  assert_eq(counter_metrics[0].metric_type, Counter)
  assert_eq(histogram_metrics[0].metric_type, Histogram)
  assert_eq(gauge_metrics[0].metric_type, Gauge)
  assert_eq(updown_metrics[0].metric_type, UpDownCounter)
  
  // Verify metric values
  assert_eq(counter_metrics[0].value, 1.0)
  assert_eq(histogram_metrics[0].value, 100.0)
  assert_eq(gauge_metrics[0].value, 25.0)
  assert_eq(updown_metrics[0].value, 10.0)
}

// Test 8: Metric Aggregation over Time
test "metric aggregation over time" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "time_aggregation_meter")
  
  // Create metrics for aggregation
  let counter = Meter::create_counter(meter, "requests_total")
  let histogram = Meter::create_histogram(meter, "response_time")
  
  // Simulate metrics over time periods
  let time_periods = 5
  let requests_per_period = 10
  
  for period in 0..time_periods {
    // Record requests for this period
    for i in 0..requests_per_period {
      let attrs = Attributes::new()
      Attributes::set(attrs, "period", period.to_string())
      Counter::add(counter, 1.0, Some(attrs))
      
      // Response times vary by period
      let base_response_time = 50.0 + period.to_float() * 10.0
      let response_time = base_response_time + (i % 5).to_float() * 10.0
      Histogram::record(histogram, response_time, Some(attrs))
    }
    
    // Force export after each period
    TelemetryProvider::force_flush(provider)
  }
  
  // Verify metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  let counter_metrics = metrics.filter(fn(m) { m.name == "requests_total" })
  let histogram_metrics = metrics.filter(fn(m) { m.name == "response_time" })
  
  assert_eq(counter_metrics.length(), time_periods * requests_per_period)
  assert_eq(histogram_metrics.length(), time_periods * requests_per_period)
  
  // Verify aggregation by period
  for period in 0..time_periods {
    let period_counter_metrics = counter_metrics.filter(fn(m) { 
      m.attributes.contains(("period", period.to_string()))
    })
    let period_histogram_metrics = histogram_metrics.filter(fn(m) { 
      m.attributes.contains(("period", period.to_string()))
    })
    
    assert_eq(period_counter_metrics.length(), requests_per_period)
    assert_eq(period_histogram_metrics.length(), requests_per_period)
    
    // Verify counter sum for this period
    let period_counter_sum = period_counter_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
    assert_eq(period_counter_sum, requests_per_period.to_float())
    
    // Verify histogram values for this period
    for i in 0..requests_per_period {
      let base_response_time = 50.0 + period.to_float() * 10.0
      let expected_response_time = base_response_time + (i % 5).to_float() * 10.0
      
      let metric = period_histogram_metrics.find(fn(m) { m.value == expected_response_time })
      assert_true(metric.is_some())
    }
  }
}

// Test 9: Metric Aggregation with Different Attributes
test "metric aggregation with different attributes" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "attribute_aggregation_meter")
  
  // Create metrics for attribute aggregation
  let counter = Meter::create_counter(meter, "api_requests")
  let histogram = Meter::create_histogram(meter, "api_response_time")
  
  // Define attribute combinations
  let services = ["web", "api", "database"]
  let endpoints = ["/users", "/orders", "/products"]
  let methods = ["GET", "POST", "PUT", "DELETE"]
  
  // Record metrics with different attribute combinations
  for service in services {
    for endpoint in endpoints {
      for method in methods {
        let attrs = Attributes::new()
        Attributes::set(attrs, "service", service)
        Attributes::set(attrs, "endpoint", endpoint)
        Attributes::set(attrs, "method", method)
        
        // Record counter
        Counter::add(counter, 1.0, Some(attrs))
        
        // Record histogram with service-specific response times
        let base_time = match service {
          "web" => 50.0
          "api" => 100.0
          "database" => 200.0
          _ => 100.0
        }
        
        let method_modifier = match method {
          "GET" => 0.8
          "POST" => 1.2
          "PUT" => 1.1
          "DELETE" => 0.9
          _ => 1.0
        }
        
        let response_time = base_time * method_modifier
        Histogram::record(histogram, response_time, Some(attrs))
      }
    }
  }
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  let counter_metrics = metrics.filter(fn(m) { m.name == "api_requests" })
  let histogram_metrics = metrics.filter(fn(m) { m.name == "api_response_time" })
  
  let expected_combinations = services.length() * endpoints.length() * methods.length()
  assert_eq(counter_metrics.length(), expected_combinations)
  assert_eq(histogram_metrics.length(), expected_combinations)
  
  // Verify aggregation by service
  for service in services {
    let service_counter_metrics = counter_metrics.filter(fn(m) { 
      m.attributes.contains(("service", service))
    })
    let service_histogram_metrics = histogram_metrics.filter(fn(m) { 
      m.attributes.contains(("service", service))
    })
    
    let expected_service_combinations = endpoints.length() * methods.length()
    assert_eq(service_counter_metrics.length(), expected_service_combinations)
    assert_eq(service_histogram_metrics.length(), expected_service_combinations)
    
    // Verify counter sum for this service
    let service_counter_sum = service_counter_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
    assert_eq(service_counter_sum, expected_service_combinations.to_float())
    
    // Verify histogram values for this service
    let base_time = match service {
      "web" => 50.0
      "api" => 100.0
      "database" => 200.0
      _ => 100.0
    }
    
    for endpoint in endpoints {
      for method in methods {
        let method_modifier = match method {
          "GET" => 0.8
          "POST" => 1.2
          "PUT" => 1.1
          "DELETE" => 0.9
          _ => 1.0
        }
        
        let expected_response_time = base_time * method_modifier
        
        let metric = service_histogram_metrics.find(fn(m) { 
          m.value == expected_response_time &&
          m.attributes.contains(("endpoint", endpoint)) &&
          m.attributes.contains(("method", method))
        })
        assert_true(metric.is_some())
      }
    }
  }
}

// Test 10: Metric Aggregation with Percentiles
test "metric aggregation with percentiles" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "percentile_meter")
  
  // Create a histogram for percentile calculation
  let histogram = Meter::create_histogram(meter, "request_duration", Some("Request duration"), Some("ms"))
  
  // Record values with a specific distribution
  let values = [
    10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0,
    60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0, 100.0, 150.0,
    200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0, 600.0, 700.0, 800.0,
    900.0, 1000.0
  ]
  
  for value in values {
    Histogram::record(histogram, value)
  }
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify metrics were collected
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  // We should have individual values plus percentile aggregations
  assert_true(metrics.length() >= values.length())
  
  // Verify percentile metrics
  let percentiles = [50.0, 90.0, 95.0, 99.0]
  
  for percentile in percentiles {
    let percentile_metric_name = "request_duration_p" + percentile.to_int().to_string()
    let percentile_metric = metrics.find(fn(m) { m.name == percentile_metric_name })
    
    assert_true(percentile_metric.is_some())
    
    match percentile_metric {
      Some(metric) => {
        // Verify percentile value is reasonable
        let percentile_value = metric.value
        
        match percentile {
          50.0 => {
            // 50th percentile should be around the median
            assert_true(percentile_value >= 45.0 && percentile_value <= 55.0)
          }
          90.0 => {
            // 90th percentile should be around the 90% mark
            assert_true(percentile_value >= 900.0 && percentile_value <= 1000.0)
          }
          95.0 => {
            // 95th percentile should be around the 95% mark
            assert_true(percentile_value >= 950.0 && percentile_value <= 1000.0)
          }
          99.0 => {
            // 99th percentile should be close to the max
            assert_true(percentile_value >= 990.0 && percentile_value <= 1000.0)
          }
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Verify count and sum metrics
  let count_metric = metrics.find(fn(m) { m.name == "request_duration_count" })
  let sum_metric = metrics.find(fn(m) { m.name == "request_duration_sum" })
  let min_metric = metrics.find(fn(m) { m.name == "request_duration_min" })
  let max_metric = metrics.find(fn(m) { m.name == "request_duration_max" })
  
  match (count_metric, sum_metric, min_metric, max_metric) {
    (Some(count), Some(sum), Some(min), Some(max)) => {
      assert_eq(count.value, values.length().to_float())
      assert_eq(sum.value, values.reduce(fn(acc, v) { acc + v }, 0.0))
      assert_eq(min.value, 10.0)
      assert_eq(max.value, 1000.0)
    }
    _ => assert_true(false)
  }
}