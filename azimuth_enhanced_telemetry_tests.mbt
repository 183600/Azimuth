// Azimuth Enhanced Telemetry Test Suite
// This file contains enhanced test cases for advanced telemetry features

// Test 1: Distributed Trace Context Propagation
test "distributed trace context propagation" {
  // Test cross-service trace propagation
  let create_trace_context = fn(trace_id: String, span_id: String, flags: Int) {
    let version = "00"
    let trace_flags = flags.to_string(16)
    version + "-" + trace_id + "-" + span_id + "-" + trace_flags
  }
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  let flags = 1
  
  let trace_header = create_trace_context(trace_id, span_id, flags)
  assert_eq(trace_header, "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-1")
  
  // Test trace context parsing
  let parse_trace_context = fn(header: String) {
    let parts = header.split("-")
    if parts.length() == 4 {
      Some({
        version: parts[0],
        trace_id: parts[1],
        span_id: parts[2],
        flags: parts[3]
      })
    } else {
      None
    }
  }
  
  let parsed_context = parse_trace_context(trace_header)
  match parsed_context {
    Some(ctx) => {
      assert_eq(ctx.version, "00")
      assert_eq(ctx.trace_id, trace_id)
      assert_eq(ctx.span_id, span_id)
      assert_eq(ctx.flags, "1")
    }
    None => assert_true(false)
  }
  
  // Test trace context validation
  let validate_trace_id = fn(id: String) {
    id.length() == 32 and id.chars().all(fn(c) {
      (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f')
    })
  }
  
  let validate_span_id = fn(id: String) {
    id.length() == 16 and id.chars().all(fn(c) {
      (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f')
    })
  }
  
  assert_true(validate_trace_id(trace_id))
  assert_true(validate_span_id(span_id))
  assert_false(validate_trace_id("invalid"))
  assert_false(validate_span_id("invalid"))
  
  // Test child span creation
  let generate_child_span_id = fn(parent_span_id: String) {
    // Simplified child span ID generation
    let parent_num = parent_span_id.to_int()  // Simplified
    let child_num = parent_num + 1
    child_num.to_string(16).to_lowercase()
  }
  
  let child_span_id = generate_child_span_id(span_id)
  assert_not_eq(child_span_id, span_id)
  assert_true(child_span_id.length() > 0)
  
  // Test baggage propagation across services
  let baggage_items = [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("service.chain", "api->payment->database")
  ]
  
  let serialize_baggage = fn(items: Array[(String, String)]) {
    let mut result = ""
    for i in 0..items.length() {
      let (key, value) = items[i]
      result = result + key + "=" + value
      if i < items.length() - 1 {
        result = result + ","
      }
    }
    result
  }
  
  let baggage_header = serialize_baggage(baggage_items)
  assert_true(baggage_header.contains("user.id=12345"))
  assert_true(baggage_header.contains("request.id=req-67890"))
  assert_true(baggage_header.contains("service.chain=api->payment->database"))
}

// Test 2: Advanced Metric Aggregation
test "advanced metric aggregation" {
  // Test time series data aggregation
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float
  }
  
  type MetricAggregation = {
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    avg: Float
  }
  
  let aggregate_time_series = fn(points: Array[TimeSeriesPoint]) {
    if points.length() == 0 {
      { count: 0, sum: 0.0, min: 0.0, max: 0.0, avg: 0.0 }
    } else {
      let mut sum = 0.0
      let mut min = points[0].value
      let mut max = points[0].value
      
      for point in points {
        sum = sum + point.value
        if point.value < min {
          min = point.value
        }
        if point.value > max {
          max = point.value
        }
      }
      
      {
        count: points.length(),
        sum,
        min,
        max,
        avg: sum / (points.length() as Float)
      }
    }
  }
  
  let time_series_data = [
    { timestamp: 1640995200, value: 10.5 },
    { timestamp: 1640995260, value: 15.2 },
    { timestamp: 1640995320, value: 8.7 },
    { timestamp: 1640995380, value: 22.1 },
    { timestamp: 1640995440, value: 12.9 }
  ]
  
  let aggregation = aggregate_time_series(time_series_data)
  assert_eq(aggregation.count, 5)
  assert_eq(aggregation.sum.round(), 69.4)
  assert_eq(aggregation.min, 8.7)
  assert_eq(aggregation.max, 22.1)
  assert_true(aggregation.avg > 13.0)
  assert_true(aggregation.avg < 14.0)
  
  // Test percentile calculation
  let calculate_percentile = fn(sorted_values: Array[Float], percentile: Float) {
    if sorted_values.length() == 0 {
      0.0
    } else {
      let index = ((sorted_values.length() as Float) * percentile / 100.0) as Int
      if index >= sorted_values.length() {
        sorted_values[sorted_values.length() - 1]
      } else {
        sorted_values[index]
      }
    }
  }
  
  let values = time_series_data.map(fn(p) { p.value }).sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
  let p50 = calculate_percentile(values, 50.0)
  let p95 = calculate_percentile(values, 95.0)
  let p99 = calculate_percentile(values, 99.0)
  
  assert_eq(p50, 12.9)
  assert_eq(p95, 22.1)
  assert_eq(p99, 22.1)
  
  // Test rate calculation
  let calculate_rate = fn(current_value: Float, previous_value: Float, time_interval: Int) {
    if time_interval > 0 {
      (current_value - previous_value) / (time_interval as Float)
    } else {
      0.0
    }
  }
  
  let rate = calculate_rate(150.0, 100.0, 60)
  assert_eq(rate.round(), 0.8333333333333334)
  
  // Test histogram buckets
  type HistogramBucket = {
    upper_bound: Float,
    count: Int
  }
  
  let create_histogram_buckets = fn(values: Array[Float], bounds: Array[Float]) {
    let mut buckets = []
    
    for bound in bounds {
      let count = values.filter(fn(v) { v <= bound }).length()
      buckets = buckets.push({ upper_bound: bound, count })
    }
    
    buckets
  }
  
  let histogram_bounds = [5.0, 10.0, 25.0, 50.0, 100.0]
  let histogram_values = [3.2, 8.5, 12.7, 23.1, 45.8, 67.3, 89.2, 95.1]
  let buckets = create_histogram_buckets(histogram_values, histogram_bounds)
  
  assert_eq(buckets.length(), 5)
  assert_eq(buckets[0].count, 1)  // <= 5.0
  assert_eq(buckets[1].count, 2)  // <= 10.0
  assert_eq(buckets[2].count, 4)  // <= 25.0
  assert_eq(buckets[3].count, 5)  // <= 50.0
  assert_eq(buckets[4].count, 8)  // <= 100.0
}

// Test 3: Telemetry Sampling Strategies
test "telemetry sampling strategies" {
  // Test deterministic sampling
  let deterministic_sampler = fn(trace_id: String, sample_rate: Float) {
    // Simple hash-based deterministic sampling
    let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
    let normalized = (hash % 1000) as Float / 1000.0
    normalized <= sample_rate
  }
  
  let trace_ids = [
    "trace-001", "trace-002", "trace-003", "trace-004", "trace-005",
    "trace-006", "trace-007", "trace-008", "trace-009", "trace-010"
  ]
  
  // Test with 50% sample rate
  let sample_rate_50 = 0.5
  let sampled_50 = trace_ids.filter(fn(trace_id) { deterministic_sampler(trace_id, sample_rate_50) })
  
  assert_true(sampled_50.length() >= 2)
  assert_true(sampled_50.length() <= 8)
  
  // Test with 100% sample rate
  let sampled_100 = trace_ids.filter(fn(trace_id) { deterministic_sampler(trace_id, 1.0) })
  assert_eq(sampled_100.length(), 10)
  
  // Test with 0% sample rate
  let sampled_0 = trace_ids.filter(fn(trace_id) { deterministic_sampler(trace_id, 0.0) })
  assert_eq(sampled_0.length(), 0)
  
  // Test adaptive sampling based on error rate
  let adaptive_sampler = fn(error_rate: Float, base_sample_rate: Float) {
    if error_rate > 0.1 {
      1.0  // Sample everything during high error rate
    } else if error_rate > 0.05 {
      base_sample_rate * 2.0  // Double sampling during moderate error rate
    } else {
      base_sample_rate  // Use base sample rate during normal operation
    }
  }
  
  let normal_rate = adaptive_sampler(0.02, 0.1)
  let moderate_rate = adaptive_sampler(0.07, 0.1)
  let high_rate = adaptive_sampler(0.15, 0.1)
  
  assert_eq(normal_rate, 0.1)
  assert_eq(moderate_rate, 0.2)
  assert_eq(high_rate, 1.0)
  
  // Test priority-based sampling
  type SamplingPriority = {
    High
    Medium
    Low
  }
  
  let priority_sampler = fn(priority: SamplingPriority, base_sample_rate: Float) {
    match priority {
      SamplingPriority::High => 1.0
      SamplingPriority::Medium => base_sample_rate * 2.0
      SamplingPriority::Low => base_sample_rate * 0.5
    }
  }
  
  let high_priority_rate = priority_sampler(SamplingPriority::High, 0.1)
  let medium_priority_rate = priority_sampler(SamplingPriority::Medium, 0.1)
  let low_priority_rate = priority_sampler(SamplingPriority::Low, 0.1)
  
  assert_eq(high_priority_rate, 1.0)
  assert_eq(medium_priority_rate, 0.2)
  assert_eq(low_priority_rate, 0.05)
  
  // Test token bucket sampling
  type TokenBucket = {
    capacity: Int,
    tokens: Int,
    refill_rate: Int,
    last_refill: Int
  }
  
  let create_token_bucket = fn(capacity: Int, refill_rate: Int) {
    {
      capacity,
      tokens: capacity,
      refill_rate,
      last_refill: 1640995200
    }
  }
  
  let try_sample = fn(bucket: TokenBucket, current_time: Int) {
    let time_passed = current_time - bucket.last_refill
    let tokens_to_add = (time_passed / 60) * bucket.refill_rate  // Refill per minute
    let new_tokens = bucket.tokens + tokens_to_add
    
    let updated_tokens = if new_tokens > bucket.capacity {
      bucket.capacity
    } else {
      new_tokens
    }
    
    if updated_tokens > 0 {
      // Sample successful
      {
        sampled: true,
        bucket: { bucket | tokens: updated_tokens - 1, last_refill: current_time }
      }
    } else {
      // Sample failed
      {
        sampled: false,
        bucket: { bucket | tokens: updated_tokens, last_refill: current_time }
      }
    }
  }
  
  let bucket = create_token_bucket(10, 2)  // 10 tokens, refill 2 per minute
  let result1 = try_sample(bucket, 1640995200)
  
  assert_true(result1.sampled)
  assert_eq(result1.bucket.tokens, 9)
  
  // Exhaust tokens
  let mut exhausted_bucket = result1.bucket
  for i in 0..9 {
    let result = try_sample(exhausted_bucket, 1640995200)
    exhausted_bucket = result.bucket
  }
  
  assert_eq(exhausted_bucket.tokens, 0)
  
  // Next sample should fail
  let exhausted_result = try_sample(exhausted_bucket, 1640995200)
  assert_false(exhausted_result.sampled)
  assert_eq(exhausted_result.bucket.tokens, 0)
  
  // After time passes, tokens should refill
  let refill_result = try_sample(exhausted_bucket, 1640995280)  // 80 seconds later
  assert_true(refill_result.sampled)  // Should have 2 tokens
  assert_eq(refill_result.bucket.tokens, 1)
}

// Test 4: Telemetry Error Analysis
test "telemetry error analysis" {
  // Test error classification
  enum ErrorSeverity {
    Critical
    High
    Medium
    Low
  }
  
  enum ErrorType {
    Network
    Database
    Authentication
    Validation
    System
  }
  
  type ErrorEvent = {
    timestamp: Int,
    error_type: ErrorType,
    severity: ErrorSeverity,
    message: String,
    service: String,
    trace_id: String
  }
  
  // Test error rate calculation
  let calculate_error_rate = fn(events: Array[ErrorEvent], time_window: Int) {
    if events.length() == 0 {
      0.0
    } else {
      let error_count = events.length()
      let total_requests = error_count * 10  // Assume 10x total requests for this test
      
      (error_count as Float) / (total_requests as Float)
    }
  }
  
  let error_events = [
    { timestamp: 1640995200, error_type: ErrorType::Network, severity: ErrorSeverity::High, message: "Connection timeout", service: "api-service", trace_id: "trace-001" },
    { timestamp: 1640995260, error_type: ErrorType::Database, severity: ErrorSeverity::Critical, message: "Connection pool exhausted", service: "payment-service", trace_id: "trace-002" },
    { timestamp: 1640995320, error_type: ErrorType::Authentication, severity: ErrorSeverity::Medium, message: "Invalid token", service: "auth-service", trace_id: "trace-003" },
    { timestamp: 1640995380, error_type: ErrorType::Validation, severity: ErrorSeverity::Low, message: "Invalid input format", service: "api-service", trace_id: "trace-004" },
    { timestamp: 1640995440, error_type: ErrorType::System, severity: ErrorSeverity::High, message: "Out of memory", service: "worker-service", trace_id: "trace-005" }
  ]
  
  let error_rate = calculate_error_rate(error_events, 300)
  assert_eq(error_rate, 0.1)  // 5 errors out of 50 total requests
  
  // Test error aggregation by type
  let aggregate_by_type = fn(events: Array[ErrorEvent]) {
    let mut result = []
    let types = [ErrorType::Network, ErrorType::Database, ErrorType::Authentication, ErrorType::Validation, ErrorType::System]
    
    for error_type in types {
      let count = events.filter(fn(e) { e.error_type == error_type }).length()
      result = result.push((error_type, count))
    }
    
    result
  }
  
  let type_counts = aggregate_by_type(error_events)
  assert_eq(type_counts.length(), 5)
  
  let get_count = fn(counts: Array[(ErrorType, Int)], target_type: ErrorType) {
    let mut found = 0
    for (error_type, count) in counts {
      if error_type == target_type {
        found = count
      }
    }
    found
  }
  
  assert_eq(get_count(type_counts, ErrorType::Network), 1)
  assert_eq(get_count(type_counts, ErrorType::Database), 1)
  assert_eq(get_count(type_counts, ErrorType::Authentication), 1)
  assert_eq(get_count(type_counts, ErrorType::Validation), 1)
  assert_eq(get_count(type_counts, ErrorType::System), 1)
  
  // Test error aggregation by service
  let aggregate_by_service = fn(events: Array[ErrorEvent]) {
    let mut result = []
    let services = ["api-service", "payment-service", "auth-service", "worker-service"]
    
    for service in services {
      let count = events.filter(fn(e) { e.service == service }).length()
      if count > 0 {
        result = result.push((service, count))
      }
    }
    
    result
  }
  
  let service_counts = aggregate_by_service(error_events)
  assert_eq(service_counts.length(), 4)
  
  let get_service_count = fn(counts: Array[(String, Int)], target_service: String) {
    let mut found = 0
    for (service, count) in counts {
      if service == target_service {
        found = count
      }
    }
    found
  }
  
  assert_eq(get_service_count(service_counts, "api-service"), 2)
  assert_eq(get_service_count(service_counts, "payment-service"), 1)
  assert_eq(get_service_count(service_counts, "auth-service"), 1)
  assert_eq(get_service_count(service_counts, "worker-service"), 1)
  
  // Test error severity analysis
  let severity_scores = [
    (ErrorSeverity::Critical, 100),
    (ErrorSeverity::High, 50),
    (ErrorSeverity::Medium, 20),
    (ErrorSeverity::Low, 5)
  ]
  
  let get_severity_score = fn(severity: ErrorSeverity) {
    let mut score = 0
    for (s, s_score) in severity_scores {
      if s == severity {
        score = s_score
      }
    }
    score
  }
  
  let calculate_error_score = fn(events: Array[ErrorEvent]) {
    events.reduce(0, fn(acc, event) {
      acc + get_severity_score(event.severity)
    })
  }
  
  let total_error_score = calculate_error_score(error_events)
  assert_eq(total_error_score, 175)  // Critical(100) + High(50) + High(50) + Medium(20) + Low(5)
  
  // Test error trend analysis
  let calculate_error_trend = fn(events: Array[ErrorEvent], window_minutes: Int) {
    let mut windows = []
    let start_time = events[0].timestamp
    
    for i in 0..5 {
      let window_start = start_time + (i * window_minutes * 60)
      let window_end = window_start + (window_minutes * 60)
      
      let count = events.filter(fn(e) {
        e.timestamp >= window_start and e.timestamp < window_end
      }).length()
      
      windows = windows.push(count)
    }
    
    windows
  }
  
  let error_trend = calculate_error_trend(error_events, 1)
  assert_eq(error_trend.length(), 5)
  assert_eq(error_trend[0], 1)
  assert_eq(error_trend[1], 1)
  assert_eq(error_trend[2], 1)
  assert_eq(error_trend[3], 1)
  assert_eq(error_trend[4], 1)
}

// Test 5: Telemetry Performance Optimization
test "telemetry performance optimization" {
  // Test batch processing optimization
  type BatchConfig = {
    max_batch_size: Int,
    max_wait_time_ms: Int,
    max_batch_bytes: Int
  }
  
  type TelemetryEvent = {
    id: String,
    timestamp: Int,
    data: String
  }
  
  let estimate_event_size = fn(event: TelemetryEvent) {
    // Rough estimation in bytes
    event.id.length() + event.timestamp.to_string().length() + event.data.length()
  }
  
  let create_batches = fn(events: Array[TelemetryEvent], config: BatchConfig) {
    let mut batches = []
    let mut current_batch = []
    let mut current_size = 0
    
    for event in events {
      let event_size = estimate_event_size(event)
      
      // Check if adding this event would exceed limits
      let would_exceed_size = current_size + event_size > config.max_batch_bytes
      let would_exceed_count = current_batch.length() >= config.max_batch_size
      
      if (would_exceed_size or would_exceed_count) and current_batch.length() > 0 {
        // Start new batch
        batches = batches.push(current_batch)
        current_batch = []
        current_size = 0
      }
      
      current_batch = current_batch.push(event)
      current_size = current_size + event_size
    }
    
    // Add the last batch if not empty
    if current_batch.length() > 0 {
      batches = batches.push(current_batch)
    }
    
    batches
  }
  
  let batch_config = {
    max_batch_size: 3,
    max_wait_time_ms: 1000,
    max_batch_bytes: 100
  }
  
  let telemetry_events = [
    { id: "event-1", timestamp: 1640995200, data: "small data" },
    { id: "event-2", timestamp: 1640995210, data: "medium sized data payload" },
    { id: "event-3", timestamp: 1640995220, data: "very large data payload that exceeds size limit" },
    { id: "event-4", timestamp: 1640995230, data: "small data" },
    { id: "event-5", timestamp: 1640995240, data: "small data" },
    { id: "event-6", timestamp: 1640995250, data: "small data" }
  ]
  
  let batches = create_batches(telemetry_events, batch_config)
  
  // Should create multiple batches due to size and count limits
  assert_true(batches.length() >= 2)
  
  // First batch should have at most 3 events
  assert_true(batches[0].length() <= 3)
  
  // Test compression simulation
  let simulate_compression = fn(data: String) {
    // Simple compression simulation: remove spaces and convert to shorter representation
    let compressed = data.replace(" ", "").replace("data", "d").replace("payload", "p")
    (compressed, compressed.length() as Float / data.length() as Float)
  }
  
  let original_data = "very large data payload that exceeds size limit"
  let (compressed_data, compression_ratio) = simulate_compression(original_data)
  
  assert_true(compressed_data.length() < original_data.length())
  assert_true(compression_ratio < 1.0)
  assert_true(compression_ratio > 0.0)
  
  // Test memory pool simulation
  type MemoryPool = {
    total_objects: Int,
    available_objects: Int,
    object_size: Int
  }
  
  let create_memory_pool = fn(total_objects: Int, object_size: Int) {
    {
      total_objects,
      available_objects: total_objects,
      object_size
    }
  }
  
  let acquire_from_pool = fn(pool: MemoryPool) {
    if pool.available_objects > 0 {
      Some({
        object: "object-" + pool.available_objects.to_string(),
        updated_pool: { pool | available_objects: pool.available_objects - 1 }
      })
    } else {
      None
    }
  }
  
  let release_to_pool = fn(pool: MemoryPool) {
    if pool.available_objects < pool.total_objects {
      { pool | available_objects: pool.available_objects + 1 }
    } else {
      pool
    }
  }
  
  let pool = create_memory_pool(10, 100)
  
  // Acquire objects
  let result1 = acquire_from_pool(pool)
  assert_true(result1.is_some())
  
  match result1 {
    Some({ object, updated_pool }) => {
      assert_eq(updated_pool.available_objects, 9)
      
      // Acquire another
      let result2 = acquire_from_pool(updated_pool)
      assert_true(result2.is_some())
      
      match result2 {
        Some({ object: object2, updated_pool: pool2 }) => {
          assert_eq(pool2.available_objects, 8)
          
          // Release object back
          let pool3 = release_to_pool(pool2)
          assert_eq(pool3.available_objects, 9)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test throughput optimization
  let calculate_optimal_batch_size = fn(events_per_second: Int, processing_time_ms: Int, target_latency_ms: Int) {
    // Simplified calculation: how many events can we process within target latency
    let max_events = target_latency_ms / processing_time_ms
    if max_events > 0 {
      max_events
    } else {
      1  // Minimum batch size
    }
  }
  
  let optimal_size = calculate_optimal_batch_size(1000, 5, 50)
  assert_eq(optimal_size, 10)
  
  let optimal_size_low = calculate_optimal_batch_size(100, 20, 50)
  assert_eq(optimal_size_low, 2)
  
  // Test adaptive throttling
  type Throttler = {
    current_rate: Int,
    max_rate: Int,
    min_rate: Int,
    error_rate: Float,
    last_adjustment: Int
  }
  
  let adjust_throttling = fn(throttler: Throttler, current_error_rate: Float) {
    if current_error_rate > 0.1 {
      // High error rate, reduce throttling
      let new_rate = (throttler.current_rate * 7) / 10  // Reduce by 30%
      { throttler | current_rate: new_rate.max(throttler.min_rate) }
    } else if current_error_rate < 0.02 {
      // Low error rate, increase throttling
      let new_rate = (throttler.current_rate * 12) / 10  // Increase by 20%
      { throttler | current_rate: new_rate.min(throttler.max_rate) }
    } else {
      // Error rate is acceptable, no change
      throttler
    }
  }
  
  let throttler = {
    current_rate: 100,
    max_rate: 1000,
    min_rate: 10,
    error_rate: 0.05,
    last_adjustment: 1640995200
  }
  
  // High error rate scenario
  let adjusted_throttler_high = adjust_throttling(throttler, 0.15)
  assert_eq(adjusted_throttler_high.current_rate, 70)  // Reduced by 30%
  
  // Low error rate scenario
  let adjusted_throttler_low = adjust_throttling(throttler, 0.01)
  assert_eq(adjusted_throttler_low.current_rate, 120)  // Increased by 20%
  
  // Normal error rate scenario
  let adjusted_throttler_normal = adjust_throttling(throttler, 0.03)
  assert_eq(adjusted_throttler_normal.current_rate, 100)  // No change
}

// Test 6: Telemetry Data Retention
test "telemetry data retention" {
  // Test data retention policies
  enum RetentionPeriod {
    OneDay
    OneWeek
    OneMonth
    ThreeMonths
    OneYear
    Indefinite
  }
  
  type RetentionPolicy = {
    period: RetentionPeriod,
    max_size_mb: Int,
    priority: Int  // Higher priority = longer retention
  }
  
  let get_retention_days = fn(period: RetentionPeriod) {
    match period {
      RetentionPeriod::OneDay => 1
      RetentionPeriod::OneWeek => 7
      RetentionPeriod::OneMonth => 30
      RetentionPeriod::ThreeMonths => 90
      RetentionPeriod::OneYear => 365
      RetentionPeriod::Indefinite => -1  // Never expire
    }
  }
  
  let policies = [
    { period: RetentionPeriod::OneDay, max_size_mb: 100, priority: 1 },
    { period: RetentionPeriod::OneWeek, max_size_mb: 500, priority: 2 },
    { period: RetentionPeriod::OneMonth, max_size_mb: 2000, priority: 3 },
    { period: RetentionPeriod::ThreeMonths, max_size_mb: 5000, priority: 4 },
    { period: RetentionPeriod::OneYear, max_size_mb: 10000, priority: 5 }
  ]
  
  // Test retention period calculation
  assert_eq(get_retention_days(RetentionPeriod::OneDay), 1)
  assert_eq(get_retention_days(RetentionPeriod::OneWeek), 7)
  assert_eq(get_retention_days(RetentionPeriod::OneMonth), 30)
  assert_eq(get_retention_days(RetentionPeriod::ThreeMonths), 90)
  assert_eq(get_retention_days(RetentionPeriod::OneYear), 365)
  assert_eq(get_retention_days(RetentionPeriod::Indefinite), -1)
  
  // Test data expiration check
  type TelemetryData = {
    id: String,
    timestamp: Int,
    size_mb: Float,
    data_type: String,
    retention_policy: RetentionPolicy
  }
  
  let is_data_expired = fn(data: TelemetryData, current_time: Int) {
    let retention_days = get_retention_days(data.retention_policy.period)
    if retention_days == -1 {
      false  // Indefinite retention
    } else {
      let age_seconds = current_time - data.timestamp
      let age_days = age_seconds / 86400
      age_days > retention_days
    }
  }
  
  let current_time = 1641600000  // 7 days after base timestamp
  
  let telemetry_data = [
    { id: "data-1", timestamp: 1640995200, size_mb: 10.5, data_type: "trace", retention_policy: RetentionPeriod::OneDay },
    { id: "data-2", timestamp: 1640995200, size_mb: 25.3, data_type: "metric", retention_policy: RetentionPeriod::OneWeek },
    { id: "data-3", timestamp: 1640995200, size_mb: 50.7, data_type: "log", retention_policy: RetentionPeriod::OneMonth },
    { id: "data-4", timestamp: 1640995200, size_mb: 15.2, data_type: "trace", retention_policy: RetentionPeriod::Indefinite }
  ]
  
  let expired_data = telemetry_data.filter(fn(data) { is_data_expired(data, current_time) })
  assert_eq(expired_data.length(), 1)
  assert_eq(expired_data[0].id, "data-1")  // Only the 1-day retention data should be expired
  
  // Test storage size management
  let calculate_total_size = fn(data: Array[TelemetryData]) {
    data.reduce(0.0, fn(acc, d) { acc + d.size_mb })
  }
  
  let total_size = calculate_total_size(telemetry_data)
  assert_eq(total_size.round(), 101.7)
  
  let enforce_size_limit = fn(data: Array[TelemetryData], max_size_mb: Float) {
    let total = calculate_total_size(data)
    if total <= max_size_mb {
      data
    } else {
      // Sort by priority (lower priority first) and remove until under limit
      let sorted_data = data.sort(fn(a, b) {
        let a_priority = match a.retention_policy.period {
          RetentionPeriod::OneDay => 1
          RetentionPeriod::OneWeek => 2
          RetentionPeriod::OneMonth => 3
          RetentionPeriod::ThreeMonths => 4
          RetentionPeriod::OneYear => 5
          RetentionPeriod::Indefinite => 6
        }
        let b_priority = match b.retention_policy.period {
          RetentionPeriod::OneDay => 1
          RetentionPeriod::OneWeek => 2
          RetentionPeriod::OneMonth => 3
          RetentionPeriod::ThreeMonths => 4
          RetentionPeriod::OneYear => 5
          RetentionPeriod::Indefinite => 6
        }
        a_priority - b_priority
      })
      
      let mut result = []
      let mut current_size = 0.0
      
      for d in sorted_data {
        if current_size + d.size_mb <= max_size_mb {
          result = result.push(d)
          current_size = current_size + d.size_mb
        }
      }
      
      result
    }
  }
  
  let size_limited_data = enforce_size_limit(telemetry_data, 80.0)
  assert_true(calculate_total_size(size_limited_data) <= 80.0)
  
  // Test data archiving
  type ArchivedData = {
    original_id: String,
    archive_location: String,
    archived_at: Int,
    compressed_size_mb: Float
  }
  
  let archive_data = fn(data: TelemetryData, archive_location: String) {
    let compression_ratio = 0.3  // Assume 70% compression
    {
      original_id: data.id,
      archive_location,
      archived_at: current_time,
      compressed_size_mb: data.size_mb * compression_ratio
    }
  }
  
  let archived_data = archive_data(telemetry_data[0], "s3://archive-bucket/telemetry/")
  assert_eq(archived_data.original_id, "data-1")
  assert_eq(archived_data.archive_location, "s3://archive-bucket/telemetry/")
  assert_eq(archived_data.archived_at, current_time)
  assert_eq(archived_data.compressed_size_mb, 3.15)  // 10.5 * 0.3
  
  // Test retention policy optimization
  let optimize_retention_policy = fn(data_usage: Array[(String, Float)], policies: Array[RetentionPolicy]) {
    // Simple optimization: prioritize frequently accessed data
    let sorted_usage = data_usage.sort(fn(a, b) {
      if a.1 > b.1 { -1 } else if a.1 < b.1 { 1 } else { 0 }
    })
    
    // Assign longer retention to more frequently accessed data
    let mut result = []
    for i in 0..sorted_usage.length() {
      let (data_type, usage) = sorted_usage[i]
      let policy_index = if i < policies.length() { policies.length() - 1 - i } else { 0 }
      result = result.push((data_type, policies[policy_index]))
    }
    
    result
  }
  
  let data_usage = [
    ("trace", 0.8),
    ("metric", 0.6),
    ("log", 0.3),
    ("event", 0.1)
  ]
  
  let optimized_policies = optimize_retention_policy(data_usage, policies)
  assert_eq(optimized_policies.length(), 4)
  
  // Most frequently accessed (trace) should get longest retention
  assert_eq(optimized_policies[0].0, "trace")
  assert_eq(optimized_policies[0].1.period, RetentionPeriod::OneYear)
  
  // Least frequently accessed (event) should get shortest retention
  assert_eq(optimized_policies[3].0, "event")
  assert_eq(optimized_policies[3].1.period, RetentionPeriod::OneDay)
}

// Test 7: Telemetry Security and Privacy
test "telemetry security and privacy" {
  // Test sensitive data detection
  let sensitive_patterns = [
    ("email", r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"),
    ("phone", r"\d{3}-\d{3}-\d{4}"),
    ("ssn", r"\d{3}-\d{2}-\d{4}"),
    ("credit_card", r"\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}"),
    ("api_key", r"[a-zA-Z0-9]{32,}")
  ]
  
  let detect_sensitive_data = fn(text: String) {
    let mut detected = []
    
    for (pattern_name, pattern) in sensitive_patterns {
      // Simplified pattern matching (real implementation would use regex)
      let is_match = match pattern_name {
        "email" => text.contains("@") and text.contains(".")
        "phone" => text.length() >= 12 and text.contains("-")
        "ssn" => text.length() >= 11 and text.contains("-")
        "credit_card" => text.length() >= 16 and (text.contains("-") or text.contains(" "))
        "api_key" => text.length() >= 32
        _ => false
      }
      
      if is_match {
        detected = detected.push(pattern_name)
      }
    }
    
    detected
  }
  
  let test_data = [
    "User email: user@example.com",
    "Phone number: 123-456-7890",
    "SSN: 123-45-6789",
    "Credit card: 1234-5678-9012-3456",
    "API key: abcdef1234567890abcdef1234567890",
    "Normal log message: Request processed successfully"
  ]
  
  let detected_sensitive = test_data.map(fn(data) {
    (data, detect_sensitive_data(data))
  })
  
  assert_eq(detected_sensitive[0].1.length(), 1)  // email
  assert_true(detected_sensitive[0].1.contains("email"))
  
  assert_eq(detected_sensitive[1].1.length(), 1)  // phone
  assert_true(detected_sensitive[1].1.contains("phone"))
  
  assert_eq(detected_sensitive[2].1.length(), 1)  // ssn
  assert_true(detected_sensitive[2].1.contains("ssn"))
  
  assert_eq(detected_sensitive[3].1.length(), 1)  // credit card
  assert_true(detected_sensitive[3].1.contains("credit_card"))
  
  assert_eq(detected_sensitive[4].1.length(), 1)  // api key
  assert_true(detected_sensitive[4].1.contains("api_key"))
  
  assert_eq(detected_sensitive[5].1.length(), 0)  // no sensitive data
  
  // Test data masking
  let mask_sensitive_data = fn(text: String, detected_types: Array[String]) {
    let mut masked_text = text
    
    for data_type in detected_types {
      masked_text = match data_type {
        "email" => masked_text.replace(
          masked_text.substring(masked_text.index_of("@") - 3, masked_text.length()),
          "***@***.com"
        )
        "phone" => "***-***-****"
        "ssn" => "***-**-****"
        "credit_card" => "****-****-****-****"
        "api_key" => "***" + "*".repeat(masked_text.length() - 3)
        _ => masked_text
      }
    }
    
    masked_text
  }
  
  let masked_data = detected_sensitive.map(fn(data) {
    let (text, detected) = data
    mask_sensitive_data(text, detected)
  })
  
  assert_eq(masked_data[0], "User email: ***@***.com")
  assert_eq(masked_data[1], "Phone number: ***-***-****")
  assert_eq(masked_data[2], "SSN: ***-**-****")
  assert_eq(masked_data[3], "Credit card: ****-****-****-****")
  assert_true(masked_data[4].starts_with("API key: ***"))
  
  // Test data encryption simulation
  let encrypt_data = fn(data: String) {
    // Simple Caesar cipher simulation (not real encryption)
    let key = 3
    let encrypted_chars = data.chars().map(fn(c) {
      if c >= 'a' and c <= 'z' {
        ((c.to_int() - 'a'.to_int() + key) % 26 + 'a'.to_int()).to_char()
      } else if c >= 'A' and c <= 'Z' {
        ((c.to_int() - 'A'.to_int() + key) % 26 + 'A'.to_int()).to_char()
      } else {
        c
      }
    })
    
    let mut encrypted = ""
    for ch in encrypted_chars {
      encrypted = encrypted + ch.to_string()
    }
    encrypted
  }
  
  let decrypt_data = fn(encrypted_data: String) {
    // Reverse of encrypt_data
    let key = 3
    let decrypted_chars = encrypted_data.chars().map(fn(c) {
      if c >= 'a' and c <= 'z' {
        ((c.to_int() - 'a'.to_int() - key + 26) % 26 + 'a'.to_int()).to_char()
      } else if c >= 'A' and c <= 'Z' {
        ((c.to_int() - 'A'.to_int() - key + 26) % 26 + 'A'.to_int()).to_char()
      } else {
        c
      }
    })
    
    let mut decrypted = ""
    for ch in decrypted_chars {
      decrypted = decrypted + ch.to_string()
    }
    decrypted
  }
  
  let original_data = "Sensitive telemetry data"
  let encrypted = encrypt_data(original_data)
  let decrypted = decrypt_data(encrypted)
  
  assert_not_eq(encrypted, original_data)
  assert_eq(decrypted, original_data)
  
  // Test access control
  type AccessLevel = {
    Read
    Write
    Admin
  }
  
  type User = {
    id: String,
    name: String,
    access_level: AccessLevel,
    department: String
  }
  
  type TelemetryResource = {
    id: String,
    name: String,
    sensitivity_level: Int,  // 1-5, higher is more sensitive
    owner_department: String
  }
  
  let check_access = fn(user: User, resource: TelemetryResource, required_level: AccessLevel) {
    let has_permission = match required_level {
      AccessLevel::Read => true  // Everyone can read
      AccessLevel::Write => user.access_level != AccessLevel::Read
      AccessLevel::Admin => user.access_level == AccessLevel::Admin
    }
    
    let can_access_sensitive = user.access_level == AccessLevel::Admin or 
                              (user.access_level == AccessLevel::Write and resource.sensitivity_level <= 3) or
                              (user.access_level == AccessLevel::Read and resource.sensitivity_level <= 2)
    
    let same_department = user.department == resource.owner_department or user.access_level == AccessLevel::Admin
    
    has_permission and can_access_sensitive and same_department
  }
  
  let admin_user = { id: "user-1", name: "Admin", access_level: AccessLevel::Admin, department: "IT" }
  let write_user = { id: "user-2", name: "Developer", access_level: AccessLevel::Write, department: "Engineering" }
  let read_user = { id: "user-3", name: "Analyst", access_level: AccessLevel::Read, department: "Analytics" }
  
  let public_resource = { id: "res-1", name: "Public Metrics", sensitivity_level: 1, owner_department: "Engineering" }
  let sensitive_resource = { id: "res-2", name: "Error Logs", sensitivity_level: 4, owner_department: "Engineering" }
  let restricted_resource = { id: "res-3", name: "PII Data", sensitivity_level: 5, owner_department: "HR" }
  
  // Admin should have full access
  assert_true(check_access(admin_user, public_resource, AccessLevel::Read))
  assert_true(check_access(admin_user, sensitive_resource, AccessLevel::Write))
  assert_true(check_access(admin_user, restricted_resource, AccessLevel::Admin))
  
  // Write user should have limited access
  assert_true(check_access(write_user, public_resource, AccessLevel::Read))
  assert_true(check_access(write_user, public_resource, AccessLevel::Write))
  assert_false(check_access(write_user, sensitive_resource, AccessLevel::Write))  // Too sensitive
  assert_false(check_access(write_user, restricted_resource, AccessLevel::Read))  // Different department
  
  // Read user should have very limited access
  assert_true(check_access(read_user, public_resource, AccessLevel::Read))
  assert_false(check_access(read_user, sensitive_resource, AccessLevel::Read))  // Too sensitive
  assert_false(check_access(read_user, restricted_resource, AccessLevel::Read))  // Different department and too sensitive
  
  // Test audit logging
  type AuditLog = {
    timestamp: Int,
    user_id: String,
    action: String,
    resource_id: String,
    success: Bool
  }
  
  let mut audit_logs = []
  
  let log_access = fn(user: User, resource: TelemetryResource, action: String, success: Bool) {
    {
      timestamp: 1640995200,
      user_id: user.id,
      action,
      resource_id: resource.id,
      success
    }
  }
  
  audit_logs = audit_logs.push(log_access(read_user, public_resource, "read", true))
  audit_logs = audit_logs.push(log_access(write_user, restricted_resource, "read", false))
  
  assert_eq(audit_logs.length(), 2)
  assert_eq(audit_logs[0].user_id, "user-3")
  assert_eq(audit_logs[0].action, "read")
  assert_eq(audit_logs[0].resource_id, "res-1")
  assert_true(audit_logs[0].success)
  
  assert_eq(audit_logs[1].user_id, "user-2")
  assert_eq(audit_logs[1].action, "read")
  assert_eq(audit_logs[1].resource_id, "res-3")
  assert_false(audit_logs[1].success)
}

// Test 8: Telemetry Anomaly Detection
test "telemetry anomaly detection" {
  // Test statistical anomaly detection
  type DataPoint = {
    timestamp: Int,
    value: Float,
    metric_name: String
  }
  
  let calculate_mean = fn(values: Array[Float]) {
    if values.length() == 0 {
      0.0
    } else {
      values.reduce(0.0, fn(acc, v) { acc + v }) / (values.length() as Float)
    }
  }
  
  let calculate_std_dev = fn(values: Array[Float], mean: Float) {
    if values.length() == 0 {
      0.0
    } else {
      let sum_squared_diff = values.reduce(0.0, fn(acc, v) {
        let diff = v - mean
        acc + (diff * diff)
      })
      (sum_squared_diff / (values.length() as Float)).sqrt()
    }
  }
  
  let detect_statistical_anomalies = fn(data_points: Array[DataPoint], threshold: Float) {
    let values = data_points.map(fn(dp) { dp.value })
    let mean = calculate_mean(values)
    let std_dev = calculate_std_dev(values, mean)
    
    data_points.filter(fn(dp) {
      let z_score = (dp.value - mean) / std_dev
      z_score.abs() > threshold
    })
  }
  
  let metric_data = [
    { timestamp: 1640995200, value: 10.5, metric_name: "response_time" },
    { timestamp: 1640995260, value: 12.2, metric_name: "response_time" },
    { timestamp: 1640995320, value: 11.8, metric_name: "response_time" },
    { timestamp: 1640995380, value: 45.7, metric_name: "response_time" },  // Anomaly
    { timestamp: 1640995440, value: 10.9, metric_name: "response_time" },
    { timestamp: 1640995500, value: 11.3, metric_name: "response_time" },
    { timestamp: 1640995560, value: 9.8, metric_name: "response_time" },
    { timestamp: 1640995620, value: 52.1, metric_name: "response_time" }   // Anomaly
  ]
  
  let anomalies = detect_statistical_anomalies(metric_data, 2.0)  // 2 standard deviations
  assert_eq(anomalies.length(), 2)
  assert_eq(anomalies[0].value, 45.7)
  assert_eq(anomalies[1].value, 52.1)
  
  // Test trend-based anomaly detection
  let calculate_trend = fn(values: Array[Float]) {
    if values.length() < 2 {
      0.0
    } else {
      let first_half = values.slice(0, values.length() / 2)
      let second_half = values.slice(values.length() / 2, values.length())
      
      let first_mean = calculate_mean(first_half)
      let second_mean = calculate_mean(second_half)
      
      second_mean - first_mean
    }
  }
  
  let detect_trend_anomalies = fn(data_points: Array[DataPoint], trend_threshold: Float) {
    let values = data_points.map(fn(dp) { dp.value })
    let trend = calculate_trend(values)
    
    if trend.abs() > trend_threshold {
      [data_points[data_points.length() - 1]]  // Return the last point as anomalous
    } else {
      []
    }
  }
  
  let trend_data = [
    { timestamp: 1640995200, value: 10.0, metric_name: "cpu_usage" },
    { timestamp: 1640995260, value: 12.0, metric_name: "cpu_usage" },
    { timestamp: 1640995320, value: 15.0, metric_name: "cpu_usage" },
    { timestamp: 1640995380, value: 25.0, metric_name: "cpu_usage" },
    { timestamp: 1640995440, value: 40.0, metric_name: "cpu_usage" },
    { timestamp: 1640995500, value: 60.0, metric_name: "cpu_usage" }
  ]
  
  let trend_anomalies = detect_trend_anomalies(trend_data, 20.0)
  assert_eq(trend_anomalies.length(), 1)
  assert_eq(trend_anomalies[0].value, 60.0)
  
  // Test pattern-based anomaly detection
  type Pattern = {
    name: String,
    condition: (DataPoint) -> Bool
  }
  
  let detect_pattern_anomalies = fn(data_points: Array[DataPoint], patterns: Array[Pattern]) {
    let mut anomalies = []
    
    for data_point in data_points {
      for pattern in patterns {
        if pattern.condition(data_point) {
          anomalies = anomalies.push({
            data_point,
            pattern_name: pattern.name
          })
        }
      }
    }
    
    anomalies
  }
  
  let anomaly_patterns = [
    {
      name: "Sudden Spike",
      condition: fn(dp: DataPoint) { dp.value > 100.0 }
    },
    {
      name: "Zero Value",
      condition: fn(dp: DataPoint) { dp.value == 0.0 }
    },
    {
      name: "Negative Value",
      condition: fn(dp: DataPoint) { dp.value < 0.0 }
    }
  ]
  
  let pattern_data = [
    { timestamp: 1640995200, value: 15.5, metric_name: "memory_usage" },
    { timestamp: 1640995260, value: 0.0, metric_name: "memory_usage" },      // Zero Value
    { timestamp: 1640995320, value: 25.3, metric_name: "memory_usage" },
    { timestamp: 1640995380, value: 150.7, metric_name: "memory_usage" },     // Sudden Spike
    { timestamp: 1640995440, value: -5.2, metric_name: "memory_usage" },     // Negative Value
    { timestamp: 1640995500, value: 30.1, metric_name: "memory_usage" }
  ]
  
  let pattern_anomalies = detect_pattern_anomalies(pattern_data, anomaly_patterns)
  assert_eq(pattern_anomalies.length(), 3)
  
  let pattern_names = pattern_anomalies.map(fn(a) { a.pattern_name })
  assert_true(pattern_names.contains("Zero Value"))
  assert_true(pattern_names.contains("Sudden Spike"))
  assert_true(pattern_names.contains("Negative Value"))
  
  // Test seasonal anomaly detection
  let detect_seasonal_anomalies = fn(data_points: Array[DataPoint], seasonal_period: Int, threshold: Float) {
    if data_points.length() < seasonal_period * 2 {
      []
    } else {
      let mut anomalies = []
      
      for i in seasonal_period..data_points.length() {
        let current = data_points[i]
        let seasonal_index = i % seasonal_period
        let historical_values = []
        
        // Collect historical values for the same seasonal position
        for j in seasonal_index..data_points.length() - seasonal_period {
          historical_values = historical_values.push(data_points[j + seasonal_period].value)
        }
        
        if historical_values.length() > 0 {
          let historical_mean = calculate_mean(historical_values)
          let historical_std_dev = calculate_std_dev(historical_values, historical_mean)
          
          if historical_std_dev > 0.0 {
            let z_score = (current.value - historical_mean) / historical_std_dev
            if z_score.abs() > threshold {
              anomalies = anomalies.push({
                data_point: current,
                expected_mean: historical_mean,
                z_score
              })
            }
          }
        }
      }
      
      anomalies
    }
  }
  
  // Create data with daily pattern (24 points per day, 3 days)
  let seasonal_data = [
    // Day 1
    { timestamp: 1640995200, value: 10.0, metric_name: "hourly_traffic" },  // Hour 0
    { timestamp: 1640998800, value: 15.0, metric_name: "hourly_traffic" },  // Hour 1
    { timestamp: 1641002400, value: 25.0, metric_name: "hourly_traffic" },  // Hour 2
    { timestamp: 1641006000, value: 40.0, metric_name: "hourly_traffic" },  // Hour 3
    { timestamp: 1641009600, value: 60.0, metric_name: "hourly_traffic" },  // Hour 4
    // ... more hours for Day 1
    
    // Day 2 (similar pattern)
    { timestamp: 1641081600, value: 12.0, metric_name: "hourly_traffic" },  // Hour 0
    { timestamp: 1641085200, value: 18.0, metric_name: "hourly_traffic" },  // Hour 1
    { timestamp: 1641088800, value: 28.0, metric_name: "hourly_traffic" },  // Hour 2
    { timestamp: 1641092400, value: 45.0, metric_name: "hourly_traffic" },  // Hour 3
    { timestamp: 1641096000, value: 65.0, metric_name: "hourly_traffic" },  // Hour 4
    // ... more hours for Day 2
    
    // Day 3 (with anomaly at hour 3)
    { timestamp: 1641168000, value: 11.0, metric_name: "hourly_traffic" },  // Hour 0
    { timestamp: 1641171600, value: 16.0, metric_name: "hourly_traffic" },  // Hour 1
    { timestamp: 1641175200, value: 26.0, metric_name: "hourly_traffic" },  // Hour 2
    { timestamp: 1641178800, value: 150.0, metric_name: "hourly_traffic" }, // Hour 3 (Anomaly!)
    { timestamp: 1641182400, value: 62.0, metric_name: "hourly_traffic" },  // Hour 4
    // ... more hours for Day 3
  ]
  
  let seasonal_anomalies = detect_seasonal_anomalies(seasonal_data, 5, 2.0)  // 5-hour period, 2 std dev threshold
  assert_eq(seasonal_anomalies.length(), 1)
  assert_eq(seasonal_anomalies[0].data_point.value, 150.0)
  assert_true(seasonal_anomalies[0].z_score > 2.0)
  
  // Test anomaly alerting
  type AnomalyAlert = {
    timestamp: Int,
    anomaly_type: String,
    severity: String,
    description: String,
    metric_name: String,
    value: Float
  }
  
  let create_alert = fn(anomaly: DataPoint, anomaly_type: String, severity: String) {
    {
      timestamp: 1640995700,
      anomaly_type,
      severity,
      description: anomaly_type + " detected in " + anomaly.metric_name,
      metric_name: anomaly.metric_name,
      value: anomaly.value
    }
  }
  
  let mut alerts = []
  
  for anomaly in anomalies {
    alerts = alerts.push(create_alert(anomaly, "Statistical Anomaly", "High"))
  }
  
  for anomaly in trend_anomalies {
    alerts = alerts.push(create_alert(anomaly, "Trend Anomaly", "Medium"))
  }
  
  for pattern_anomaly in pattern_anomalies {
    alerts = alerts.push(create_alert(pattern_anomaly.data_point, pattern_anomaly.pattern_name, "Critical"))
  }
  
  for seasonal_anomaly in seasonal_anomalies {
    alerts = alerts.push(create_alert(seasonal_anomaly.data_point, "Seasonal Anomaly", "High"))
  }
  
  assert_true(alerts.length() >= 5)  // At least 5 anomalies detected
  
  let critical_alerts = alerts.filter(fn(alert) { alert.severity == "Critical" })
  assert_eq(critical_alerts.length(), 3)  // Zero Value, Sudden Spike, Negative Value
}