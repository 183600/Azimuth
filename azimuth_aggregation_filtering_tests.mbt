// Azimuth Telemetry System - Data Aggregation and Filtering Tests
// This file contains test cases for telemetry data aggregation and filtering

// Test 1: Basic Metric Aggregation
test "basic metric aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  // Add some measurements
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5)
  Counter::add(counter, 3.0)
  Counter::add(counter, 4.5)
  
  // Create an aggregator
  let aggregator = MetricAggregator::new()
  
  // Aggregate the counter
  let aggregated = MetricAggregator::aggregate_counter(aggregator, counter)
  
  // Verify aggregation result
  match aggregated {
    AggregatedMetric::Sum(value) => assert_eq(value, 11.0)
    _ => assert_true(false)
  }
}

// Test 2: Histogram Aggregation
test "histogram aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  // Record some measurements
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  Histogram::record(histogram, 300.0)
  Histogram::record(histogram, 250.0)
  
  // Create an aggregator
  let aggregator = MetricAggregator::new()
  
  // Aggregate the histogram
  let aggregated = MetricAggregator::aggregate_histogram(aggregator, histogram)
  
  // Verify aggregation result
  match aggregated {
    AggregatedMetric::Histogram(stats) => {
      assert_eq(stats.count, 5)
      assert_eq(stats.sum, 1000.0)
      assert_eq(stats.min, 100.0)
      assert_eq(stats.max, 300.0)
      assert_eq(stats.mean, 200.0)
    }
    _ => assert_true(false)
  }
}

// Test 3: Attribute-based Filtering
test "attribute based filtering" {
  // Create spans with different attributes
  let spans = []
  
  // Span 1: HTTP GET request
  let span1_attrs = Attributes::new()
  Attributes::set(span1_attrs, "http.method", StringValue("GET"))
  Attributes::set(span1_attrs, "http.status_code", IntValue(200))
  let span1_ctx = SpanContext::new("trace1", "span1", true, "")
  let span1 = Span::new("http_request", Server, span1_ctx)
  Span::set_attributes(span1, span1_attrs)
  spans.push(span1)
  
  // Span 2: HTTP POST request
  let span2_attrs = Attributes::new()
  Attributes::set(span2_attrs, "http.method", StringValue("POST"))
  Attributes::set(span2_attrs, "http.status_code", IntValue(201))
  let span2_ctx = SpanContext::new("trace2", "span2", true, "")
  let span2 = Span::new("http_request", Server, span2_ctx)
  Span::set_attributes(span2, span2_attrs)
  spans.push(span2)
  
  // Span 3: Database query
  let span3_attrs = Attributes::new()
  Attributes::set(span3_attrs, "db.system", StringValue("postgresql"))
  Attributes::set(span3_attrs, "db.operation", StringValue("SELECT"))
  let span3_ctx = SpanContext::new("trace3", "span3", true, "")
  let span3 = Span::new("db_query", Internal, span3_ctx)
  Span::set_attributes(span3, span3_attrs)
  spans.push(span3)
  
  // Create a filter for HTTP GET requests
  let get_filter = SpanFilter::attribute_equals("http.method", "GET")
  let filtered_spans = SpanFilter::filter(get_filter, spans)
  
  // Verify only GET requests are returned
  assert_eq(filtered_spans.length(), 1)
  assert_eq(Span::name(filtered_spans[0]), "http_request")
  
  let filtered_attrs = Span::attributes(filtered_spans[0])
  let method = Attributes::get(filtered_attrs, "http.method")
  match method {
    Some(StringValue("GET")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Create a filter for database operations
  let db_filter = SpanFilter::attribute_exists("db.system")
  let db_spans = SpanFilter::filter(db_filter, spans)
  
  // Verify only database operations are returned
  assert_eq(db_spans.length(), 1)
  assert_eq(Span::name(db_spans[0]), "db_query")
}

// Test 4: Time-based Filtering
test "time based filtering" {
  // Create spans with different timestamps
  let spans = []
  let base_time = 1234567890L
  
  // Span 1: 1 minute ago
  let span1_ctx = SpanContext::new("trace1", "span1", true, "")
  let span1 = Span::new("old_span", Internal, span1_ctx)
  Span::set_start_time(span1, base_time - 60L)
  spans.push(span1)
  
  // Span 2: 10 seconds ago
  let span2_ctx = SpanContext::new("trace2", "span2", true, "")
  let span2 = Span::new("recent_span", Internal, span2_ctx)
  Span::set_start_time(span2, base_time - 10L)
  spans.push(span2)
  
  // Span 3: Current time
  let span3_ctx = SpanContext::new("trace3", "span3", true, "")
  let span3 = Span::new("current_span", Internal, span3_ctx)
  Span::set_start_time(span3, base_time)
  spans.push(span3)
  
  // Create a filter for spans in the last 30 seconds
  let time_filter = SpanFilter::time_range(base_time - 30L, base_time + 30L)
  let recent_spans = SpanFilter::filter(time_filter, spans)
  
  // Verify only recent spans are returned
  assert_eq(recent_spans.length(), 2)
  
  for span in recent_spans {
    let start_time = Span::start_time(span)
    assert_true(start_time >= base_time - 30L && start_time <= base_time + 30L)
  }
}

// Test 5: Composite Filtering
test "composite filtering" {
  // Create spans with different attributes and timestamps
  let spans = []
  let base_time = 1234567890L
  
  // Span 1: Recent HTTP GET request
  let span1_attrs = Attributes::new()
  Attributes::set(span1_attrs, "http.method", StringValue("GET"))
  Attributes::set(span1_attrs, "http.status_code", IntValue(200))
  let span1_ctx = SpanContext::new("trace1", "span1", true, "")
  let span1 = Span::new("http_request", Server, span1_ctx)
  Span::set_attributes(span1, span1_attrs)
  Span::set_start_time(span1, base_time - 10L)
  spans.push(span1)
  
  // Span 2: Old HTTP GET request
  let span2_attrs = Attributes::new()
  Attributes::set(span2_attrs, "http.method", StringValue("GET"))
  Attributes::set(span2_attrs, "http.status_code", IntValue(404))
  let span2_ctx = SpanContext::new("trace2", "span2", true, "")
  let span2 = Span::name("http_request", Server, span2_ctx)
  Span::set_attributes(span2, span2_attrs)
  Span::set_start_time(span2, base_time - 100L)
  spans.push(span2)
  
  // Span 3: Recent HTTP POST request
  let span3_attrs = Attributes::new()
  Attributes::set(span3_attrs, "http.method", StringValue("POST"))
  Attributes::set(span3_attrs, "http.status_code", IntValue(201))
  let span3_ctx = SpanContext::new("trace3", "span3", true, "")
  let span3 = Span::name("http_request", Server, span3_ctx)
  Span::set_attributes(span3, span3_attrs)
  Span::set_start_time(span3, base_time - 5L)
  spans.push(span3)
  
  // Create a composite filter for recent successful GET requests
  let method_filter = SpanFilter::attribute_equals("http.method", "GET")
  let status_filter = SpanFilter::attribute_not_equals("http.status_code", 404)
  let time_filter = SpanFilter::time_range(base_time - 30L, base_time + 30L)
  let composite_filter = SpanFilter::all_of([method_filter, status_filter, time_filter])
  
  let filtered_spans = SpanFilter::filter(composite_filter, spans)
  
  // Verify only spans matching all criteria are returned
  assert_eq(filtered_spans.length(), 1)
  assert_eq(Span::name(filtered_spans[0]), "http_request")
  
  let filtered_attrs = Span::attributes(filtered_spans[0])
  let method = Attributes::get(filtered_attrs, "http.method")
  let status = Attributes::get(filtered_attrs, "http.status_code")
  
  match (method, status) {
    (Some(StringValue("GET")), Some(IntValue(200))) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 6: Grouping and Aggregation
test "grouping and aggregation" {
  // Create spans with different service names
  let spans = []
  
  // Service A spans
  for i in 0..=4 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "service.name", StringValue("service-a"))
    Attributes::set(attrs, "http.method", StringValue("GET"))
    Attributes::set(attrs, "http.status_code", IntValue(200))
    
    let ctx = SpanContext::new("trace" + i.to_string(), "span" + i.to_string(), true, "")
    let span = Span::name("http_request", Server, ctx)
    Span::set_attributes(span, attrs)
    spans.push(span)
  }
  
  // Service B spans
  for i in 5..=9 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "service.name", StringValue("service-b"))
    Attributes::set(attrs, "http.method", StringValue("POST"))
    Attributes::set(attrs, "http.status_code", IntValue(201))
    
    let ctx = SpanContext::new("trace" + i.to_string(), "span" + i.to_string(), true, "")
    let span = Span::name("http_request", Server, ctx)
    Span::set_attributes(span, attrs)
    spans.push(span)
  }
  
  // Group spans by service name
  let grouped = SpanGrouper::group_by_attribute(spans, "service.name")
  
  // Verify grouping
  assert_eq(grouped.size(), 2)
  assert_true(grouped.contains_key("service-a"))
  assert_true(grouped.contains_key("service-b"))
  
  let service_a_spans = grouped.get("service-a")
  match service_a_spans {
    Some(spans) => assert_eq(spans.length(), 5)
    None => assert_true(false)
  }
  
  let service_b_spans = grouped.get("service-b")
  match service_b_spans {
    Some(spans) => assert_eq(spans.length(), 5)
    None => assert_true(false)
  }
  
  // Aggregate counts by service
  let aggregator = SpanAggregator::new()
  let counts = SpanAggregator::count_by_attribute(aggregator, spans, "service.name")
  
  // Verify aggregation
  assert_eq(counts.get("service-a"), Some(5))
  assert_eq(counts.get("service-b"), Some(5))
}

// Test 7: Percentile Calculation
test "percentile calculation" {
  // Create a histogram with measurements
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  // Record measurements with known distribution
  let measurements = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  for measurement in measurements {
    Histogram::record(histogram, measurement)
  }
  
  // Create an aggregator
  let aggregator = MetricAggregator::new()
  
  // Calculate percentiles
  let p50 = MetricAggregator::percentile(aggregator, histogram, 0.5)
  let p90 = MetricAggregator::percentile(aggregator, histogram, 0.9)
  let p95 = MetricAggregator::percentile(aggregator, histogram, 0.95)
  let p99 = MetricAggregator::percentile(aggregator, histogram, 0.99)
  
  // Verify percentiles
  assert_eq(p50, 55.0)  // Median
  assert_eq(p90, 90.0)
  assert_eq(p95, 95.0)
  assert_eq(p99, 99.0)
}

// Test 8: Rate Calculation
test "rate calculation" {
  // Create spans with different timestamps
  let spans = []
  let base_time = 1234567890L
  
  // Create 10 spans over 10 seconds
  for i in 0..=9 {
    let ctx = SpanContext::new("trace" + i.to_string(), "span" + i.to_string(), true, "")
    let span = Span::name("test_span", Internal, ctx)
    Span::set_start_time(span, base_time + (i as Int64))
    spans.push(span)
  }
  
  // Create an aggregator
  let aggregator = SpanAggregator::new()
  
  // Calculate rate over the last 5 seconds
  let rate = SpanAggregator::rate(aggregator, spans, base_time + 5L, base_time + 10L)
  
  // Verify rate (5 spans in 5 seconds = 1 span per second)
  assert_eq(rate, 1.0)
  
  // Calculate rate over the entire 10 seconds
  let total_rate = SpanAggregator::rate(aggregator, spans, base_time, base_time + 10L)
  
  // Verify total rate (10 spans in 10 seconds = 1 span per second)
  assert_eq(total_rate, 1.0)
}

// Test 9: Log Record Filtering
test "log record filtering" {
  // Create log records with different severity levels
  let log_records = []
  
  // Debug log
  let debug_log = LogRecord::new(Debug, "Debug message")
  log_records.push(debug_log)
  
  // Info log
  let info_log = LogRecord::new(Info, "Info message")
  log_records.push(info_log)
  
  // Warning log
  let warning_log = LogRecord::new(Warn, "Warning message")
  log_records.push(warning_log)
  
  // Error log
  let error_log = LogRecord::new(Error, "Error message")
  log_records.push(error_log)
  
  // Create a filter for logs with severity >= Warning
  let severity_filter = LogFilter::min_severity(Warn)
  let filtered_logs = LogFilter::filter(severity_filter, log_records)
  
  // Verify only warning and error logs are returned
  assert_eq(filtered_logs.length(), 2)
  
  for log in filtered_logs {
    let severity = LogRecord::severity_number(log)
    assert_true(severity >= Warn)
  }
  
  // Create a filter for logs containing specific text
  let text_filter = LogFilter::message_contains("Error")
  let error_logs = LogFilter::filter(text_filter, log_records)
  
  // Verify only error logs are returned
  assert_eq(error_logs.length(), 1)
  assert_eq(LogRecord::severity_number(error_logs[0]), Error)
}

// Test 10: Custom Aggregation Function
test "custom aggregation function" {
  // Create spans with different durations
  let spans = []
  
  for i in 0..=9 {
    let ctx = SpanContext::new("trace" + i.to_string(), "span" + i.to_string(), true, "")
    let span = Span::name("test_span", Internal, ctx)
    Span::set_start_time(span, 1234567890L)
    Span::set_end_time(span, 1234567890L + (i * 10 as Int64))
    spans.push(span)
  }
  
  // Create an aggregator with a custom function to calculate average duration
  let aggregator = SpanAggregator::new()
  let avg_duration = SpanAggregator::custom_aggregate(aggregator, spans, |spans| {
    let mut total_duration = 0L
    let mut count = 0
    
    for span in spans {
      let start_time = Span::start_time(span)
      let end_time = Span::end_time(span)
      let duration = end_time - start_time
      total_duration = total_duration + duration
      count = count + 1
    }
    
    if count > 0 {
      total_duration / (count as Int64)
    } else {
      0L
    }
  })
  
  // Verify average duration
  // Durations are: 0, 10, 20, 30, 40, 50, 60, 70, 80, 90
  // Sum is 450, average is 45
  assert_eq(avg_duration, 45L)
}