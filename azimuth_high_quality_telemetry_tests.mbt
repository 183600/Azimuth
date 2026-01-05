// Azimuth High-Quality Telemetry Test Suite
// This file contains advanced MoonBit test cases focusing on telemetry, monitoring, and distributed tracing

// Test 1: Telemetry Data Aggregation and Processing
test "telemetry data aggregation and processing" {
  // Define telemetry metrics
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  type MetricBatch = {
    metrics: Array[Metric],
    processed: Bool,
    aggregation_strategy: String
  }
  
  // Create sample metrics
  let cpu_metrics = [
    { name: "cpu_usage", value: 45.2, timestamp: 1640995200, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "cpu_usage", value: 52.8, timestamp: 1640995260, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "cpu_usage", value: 38.9, timestamp: 1640995320, tags: [("host", "server2"), ("region", "us-east")] }
  ]
  
  let memory_metrics = [
    { name: "memory_usage", value: 68.5, timestamp: 1640995200, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "memory_usage", value: 72.1, timestamp: 1640995260, tags: [("host", "server1"), ("region", "us-west")] }
  ]
  
  // Test metric aggregation function
  let aggregate_metrics = fn(metrics: Array[Metric], operation: String) {
    let mut result = 0.0
    let count = metrics.length()
    
    if count == 0 {
      result
    } else {
      match operation {
        "avg" => {
          for metric in metrics {
            result = result + metric.value
          }
          result / count.to_float()
        }
        "max" => {
          result = metrics[0].value
          for metric in metrics {
            if metric.value > result {
              result = metric.value
            }
          }
          result
        }
        "min" => {
          result = metrics[0].value
          for metric in metrics {
            if metric.value < result {
              result = metric.value
            }
          }
          result
        }
        "sum" => {
          for metric in metrics {
            result = result + metric.value
          }
          result
        }
        _ => 0.0
      }
    }
  }
  
  // Test metric aggregation
  let avg_cpu = aggregate_metrics(cpu_metrics, "avg")
  assert_true(avg_cpu > 45.0 and avg_cpu < 46.0)
  
  let max_cpu = aggregate_metrics(cpu_metrics, "max")
  assert_eq(max_cpu, 52.8)
  
  let min_cpu = aggregate_metrics(cpu_metrics, "min")
  assert_eq(min_cpu, 38.9)
  
  let sum_cpu = aggregate_metrics(cpu_metrics, "sum")
  assert_true(sum_cpu > 136.0 and sum_cpu < 137.0)
  
  // Test metric filtering by tags
  let filter_by_tag = fn(metrics: Array[Metric], key: String, value: String) {
    let mut filtered = []
    for metric in metrics {
      let mut found = false
      for (k, v) in metric.tags {
        if k == key and v == value {
          found = true
        }
      }
      if found {
        filtered = filtered.push(metric)
      }
    }
    filtered
  }
  
  let server1_metrics = filter_by_tag(cpu_metrics, "host", "server1")
  assert_eq(server1_metrics.length(), 2)
  
  let us_west_metrics = filter_by_tag(cpu_metrics, "region", "us-west")
  assert_eq(us_west_metrics.length(), 2)
  
  // Test metric batch creation and processing
  let create_batch = fn(metrics: Array[Metric], strategy: String) {
    {
      metrics,
      processed: false,
      aggregation_strategy: strategy
    }
  }
  
  let cpu_batch = create_batch(cpu_metrics, "time_weighted_avg")
  assert_false(cpu_batch.processed)
  assert_eq(cpu_batch.aggregation_strategy, "time_weighted_avg")
  assert_eq(cpu_batch.metrics.length(), 3)
  
  // Test batch processing simulation
  let process_batch = fn(batch: MetricBatch) {
    let aggregated = aggregate_metrics(batch.metrics, "avg")
    {
      batch | processed: true
    }
  }
  
  let processed_batch = process_batch(cpu_batch)
  assert_true(processed_batch.processed)
}

// Test 2: Distributed Trace Context Propagation
test "distributed trace context propagation" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    sampled: Bool,
    baggage: Array[(String, String)]
  }
  
  type Span = {
    name: String,
    context: TraceContext,
    start_time: Int,
    end_time: Option[Int],
    status: String
  }
  
  // Test trace context creation
  let create_trace_context = fn(trace_id: String, span_id: String, parent_span_id: Option[String]) {
    {
      trace_id,
      span_id,
      parent_span_id,
      sampled: true,
      baggage: []
    }
  }
  
  let root_context = create_trace_context("trace-12345", "span-10001", None)
  assert_eq(root_context.trace_id, "trace-12345")
  assert_eq(root_context.span_id, "span-10001")
  assert_eq(root_context.parent_span_id, None)
  assert_true(root_context.sampled)
  assert_eq(root_context.baggage.length(), 0)
  
  // Test child context creation
  let create_child_context = fn(parent: TraceContext, span_id: String) {
    {
      trace_id: parent.trace_id,
      span_id,
      parent_span_id: Some(parent.span_id),
      sampled: parent.sampled,
      baggage: parent.baggage
    }
  }
  
  let child_context = create_child_context(root_context, "span-10002")
  assert_eq(child_context.trace_id, "trace-12345")
  assert_eq(child_context.span_id, "span-10002")
  assert_eq(child_context.parent_span_id, Some("span-10001"))
  assert_true(child_context.sampled)
  
  // Test baggage propagation
  let add_baggage_item = fn(context: TraceContext, key: String, value: String) {
    let new_baggage = context.baggage.push((key, value))
    {
      context | baggage: new_baggage
    }
  }
  
  let context_with_baggage = add_baggage_item(root_context, "user.id", "user-123")
  assert_eq(context_with_baggage.baggage.length(), 1)
  assert_true(context_with_baggage.baggage.contains(("user.id", "user-123")))
  
  let context_with_more_baggage = add_baggage_item(context_with_baggage, "service.version", "1.2.3")
  assert_eq(context_with_more_baggage.baggage.length(), 2)
  
  // Test baggage inheritance in child contexts
  let child_with_baggage = create_child_context(context_with_more_baggage, "span-10003")
  assert_eq(child_with_baggage.baggage.length(), 2)
  assert_true(child_with_baggage.baggage.contains(("user.id", "user-123")))
  assert_true(child_with_baggage.baggage.contains(("service.version", "1.2.3")))
  
  // Test span creation and management
  let create_span = fn(name: String, context: TraceContext, start_time: Int) {
    {
      name,
      context,
      start_time,
      end_time: None,
      status: "running"
    }
  }
  
  let root_span = create_span("http.request", root_context, 1640995200)
  assert_eq(root_span.name, "http.request")
  assert_eq(root_span.context.trace_id, "trace-12345")
  assert_eq(root_span.start_time, 1640995200)
  assert_eq(root_span.end_time, None)
  assert_eq(root_span.status, "running")
  
  // Test span completion
  let complete_span = fn(span: Span, end_time: Int, status: String) {
    {
      span | end_time: Some(end_time), status: status
    }
  }
  
  let completed_span = complete_span(root_span, 1640995250, "ok")
  assert_eq(completed_span.end_time, Some(1640995250))
  assert_eq(completed_span.status, "ok")
  
  // Test span duration calculation
  let calculate_duration = fn(span: Span) {
    match span.end_time {
      Some(end) => end - span.start_time
      None => 0
    }
  }
  
  let duration = calculate_duration(completed_span)
  assert_eq(duration, 50)
}

// Test 3: Performance Monitoring and Alerting
test "performance monitoring and alerting" {
  // Define performance metrics
  type PerformanceMetric = {
    name: String,
    value: Float,
    threshold: Float,
    unit: String,
    timestamp: Int
  }
  
  type Alert = {
    id: String,
    metric_name: String,
    current_value: Float,
    threshold: Float,
    severity: String,
    message: String,
    timestamp: Int
  }
  
  // Test threshold checking
  let check_threshold = fn(metric: PerformanceMetric) {
    metric.value > metric.threshold
  }
  
  let response_time_metric = {
    name: "response_time",
    value: 125.5,
    threshold: 100.0,
    unit: "ms",
    timestamp: 1640995200
  }
  
  assert_true(check_threshold(response_time_metric))
  
  let cpu_metric = {
    name: "cpu_usage",
    value: 65.2,
    threshold: 80.0,
    unit: "percent",
    timestamp: 1640995200
  }
  
  assert_false(check_threshold(cpu_metric))
  
  // Test alert generation
  let generate_alert = fn(metric: PerformanceMetric, alert_id: String) {
    if check_threshold(metric) {
      let severity = if metric.value > metric.threshold * 1.5 {
        "critical"
      } else if metric.value > metric.threshold * 1.2 {
        "warning"
      } else {
        "info"
      }
      
      Some({
        id: alert_id,
        metric_name: metric.name,
        current_value: metric.value,
        threshold: metric.threshold,
        severity,
        message: metric.name + " exceeded threshold: " + metric.value.to_string() + " " + metric.unit + " > " + metric.threshold.to_string() + " " + metric.unit,
        timestamp: metric.timestamp
      })
    } else {
      None
    }
  }
  
  let response_time_alert = generate_alert(response_time_metric, "alert-001")
  match response_time_alert {
    Some(alert) => {
      assert_eq(alert.metric_name, "response_time")
      assert_eq(alert.current_value, 125.5)
      assert_eq(alert.threshold, 100.0)
      assert_eq(alert.severity, "info")
      assert_true(alert.message.contains("exceeded threshold"))
    }
    None => assert_true(false)
  }
  
  let cpu_alert = generate_alert(cpu_metric, "alert-002")
  assert_eq(cpu_alert, None)
  
  // Test critical alert generation
  let critical_metric = {
    name: "error_rate",
    value: 15.0,
    threshold: 5.0,
    unit: "percent",
    timestamp: 1640995200
  }
  
  let critical_alert = generate_alert(critical_metric, "alert-003")
  match critical_alert {
    Some(alert) => assert_eq(alert.severity, "critical")
    None => assert_true(false)
  }
  
  // Test alert aggregation
  let aggregate_alerts = fn(alerts: Array[Alert], time_window: Int) {
    let mut grouped = []
    
    // Group alerts by metric name
    for alert in alerts {
      let mut found_group = false
      let mut updated_groups = []
      
      for group in grouped {
        if group.metric_name == alert.metric_name {
          found_group = true
          updated_groups = updated_groups.push({
            group | 
            count: group.count + 1,
            max_value: if alert.current_value > group.max_value { alert.current_value } else { group.max_value },
            latest_timestamp: alert.timestamp
          })
        } else {
          updated_groups = updated_groups.push(group)
        }
      }
      
      if not(found_group) {
        updated_groups = updated_groups.push({
          metric_name: alert.metric_name,
          count: 1,
          max_value: alert.current_value,
          latest_timestamp: alert.timestamp
        })
      }
      
      grouped = updated_groups
    }
    
    grouped
  }
  
  // Create multiple alerts for aggregation test
  let alerts = [
    {
      id: "alert-001",
      metric_name: "response_time",
      current_value: 125.5,
      threshold: 100.0,
      severity: "info",
      message: "Response time exceeded threshold",
      timestamp: 1640995200
    },
    {
      id: "alert-002",
      metric_name: "response_time",
      current_value: 135.7,
      threshold: 100.0,
      severity: "warning",
      message: "Response time exceeded threshold",
      timestamp: 1640995260
    },
    {
      id: "alert-003",
      metric_name: "cpu_usage",
      current_value: 85.2,
      threshold: 80.0,
      severity: "warning",
      message: "CPU usage exceeded threshold",
      timestamp: 1640995320
    }
  ]
  
  let aggregated = aggregate_alerts(alerts, 300)  // 5 minute window
  assert_eq(aggregated.length(), 2)
  
  let response_time_group = aggregated.find(fn(g) { g.metric_name == "response_time" })
  match response_time_group {
    Some(group) => {
      assert_eq(group.count, 2)
      assert_eq(group.max_value, 135.7)
      assert_eq(group.latest_timestamp, 1640995260)
    }
    None => assert_true(false)
  }
  
  let cpu_group = aggregated.find(fn(g) { g.metric_name == "cpu_usage" })
  match cpu_group {
    Some(group) => {
      assert_eq(group.count, 1)
      assert_eq(group.max_value, 85.2)
      assert_eq(group.latest_timestamp, 1640995320)
    }
    None => assert_true(false)
  }
}

// Test 4: Telemetry Data Sampling Strategies
test "telemetry data sampling strategies" {
  // Define sampling strategies
  enum SamplingStrategy {
    Always
    Never
    Probabilistic(Float)  // Sample rate between 0.0 and 1.0
    RateLimited(Int)      // Max samples per second
  }
  
  type SamplingDecision = {
    should_sample: Bool,
    strategy: SamplingStrategy,
    reason: String
  }
  
  // Test always sampling strategy
  let make_sampling_decision = fn(strategy: SamplingStrategy, trace_id: String) {
    match strategy {
      SamplingStrategy::Always => {
        {
          should_sample: true,
          strategy,
          reason: "always sample"
        }
      }
      SamplingStrategy::Never => {
        {
          should_sample: false,
          strategy,
          reason: "never sample"
        }
      }
      SamplingStrategy::Probabilistic(rate) => {
        // Simple hash-based sampling
        let hash = trace_id.length() % 100
        let should_sample = (hash.to_float() / 100.0) < rate
        {
          should_sample,
          strategy,
          reason: "probabilistic sampling with rate: " + rate.to_string()
        }
      }
      SamplingStrategy::RateLimited(max_per_second) => {
        // Simplified rate limiting based on timestamp
        let current_second = 1640995200
        let trace_hash = trace_id.length() % max_per_second
        let should_sample = trace_hash < (max_per_second / 2)
        {
          should_sample,
          strategy,
          reason: "rate limited sampling: max " + max_per_second.to_string() + " per second"
        }
      }
    }
  }
  
  // Test always sampling
  let always_decision = make_sampling_decision(SamplingStrategy::Always, "trace-123")
  assert_true(always_decision.should_sample)
  assert_eq(always_decision.reason, "always sample")
  
  // Test never sampling
  let never_decision = make_sampling_decision(SamplingStrategy::Never, "trace-456")
  assert_false(never_decision.should_sample)
  assert_eq(never_decision.reason, "never sample")
  
  // Test probabilistic sampling
  let high_rate_decision = make_sampling_decision(SamplingStrategy::Probabilistic(0.9), "trace-789")
  // With high rate, most traces should be sampled
  // Note: This is a simplified test, actual implementation would use proper hash function
  
  let low_rate_decision = make_sampling_decision(SamplingStrategy::Probabilistic(0.1), "trace-101112")
  // With low rate, most traces should not be sampled
  
  // Test rate limited sampling
  let rate_limited_decision = make_sampling_decision(SamplingStrategy::RateLimited(100), "trace-131415")
  // Rate limiting decision based on simplified hash
  
  // Test sampling consistency for same trace ID
  let consistent_sampling = fn(strategy: SamplingStrategy, trace_id: String) {
    let decision1 = make_sampling_decision(strategy, trace_id)
    let decision2 = make_sampling_decision(strategy, trace_id)
    decision1.should_sample == decision2.should_sample
  }
  
  // Always and Never strategies should be consistent
  assert_true(consistent_sampling(SamplingStrategy::Always, "trace-123"))
  assert_true(consistent_sampling(SamplingStrategy::Never, "trace-456"))
  
  // Probabilistic and RateLimited strategies should also be consistent for same trace ID
  assert_true(consistent_sampling(SamplingStrategy::Probabilistic(0.5), "trace-789"))
  assert_true(consistent_sampling(SamplingStrategy::RateLimited(50), "trace-101112"))
  
  // Test adaptive sampling based on trace attributes
  type TraceAttributes = {
    service_name: String,
    endpoint: String,
    error_rate: Float,
    latency_p99: Float
  }
  
  let adaptive_sampling_decision = fn(attributes: TraceAttributes) {
    let base_strategy = SamplingStrategy::Probabilistic(0.1)  // 10% base sampling
    
    // Increase sampling rate for high-error or high-latency services
    let adjusted_strategy = if attributes.error_rate > 5.0 {
      SamplingStrategy::Probabilistic(0.8)  // 80% for high error rate
    } else if attributes.latency_p99 > 1000.0 {
      SamplingStrategy::Probabilistic(0.5)  // 50% for high latency
    } else {
      base_strategy
    }
    
    make_sampling_decision(adjusted_strategy, attributes.service_name + ":" + attributes.endpoint)
  }
  
  let normal_attrs = {
    service_name: "api-service",
    endpoint: "/health",
    error_rate: 0.5,
    latency_p99: 50.0
  }
  
  let normal_decision = adaptive_sampling_decision(normal_attrs)
  match normal_decision.strategy {
    SamplingStrategy::Probabilistic(rate) => assert_eq(rate, 0.1)
    _ => assert_true(false)
  }
  
  let high_error_attrs = {
    service_name: "payment-service",
    endpoint: "/process",
    error_rate: 8.5,
    latency_p99: 200.0
  }
  
  let high_error_decision = adaptive_sampling_decision(high_error_attrs)
  match high_error_decision.strategy {
    SamplingStrategy::Probabilistic(rate) => assert_eq(rate, 0.8)
    _ => assert_true(false)
  }
  
  let high_latency_attrs = {
    service_name: "analytics-service",
    endpoint: "/report",
    error_rate: 1.0,
    latency_p99: 1500.0
  }
  
  let high_latency_decision = adaptive_sampling_decision(high_latency_attrs)
  match high_latency_decision.strategy {
    SamplingStrategy::Probabilistic(rate) => assert_eq(rate, 0.5)
    _ => assert_true(false)
  }
}

// Test 5: Telemetry Data Compression and Storage
test "telemetry data compression and storage" {
  // Define telemetry data point
  type DataPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    data_points: Array[DataPoint],
    compression_method: String
  }
  
  // Create sample time series data
  let create_time_series = fn(name: String, data_points: Array[DataPoint]) {
    {
      name,
      data_points,
      compression_method: "none"
    }
  }
  
  let cpu_series = create_time_series("cpu_usage", [
    { timestamp: 1640995200, value: 45.2, metadata: [("host", "server1")] },
    { timestamp: 1640995260, value: 47.8, metadata: [("host", "server1")] },
    { timestamp: 1640995320, value: 46.5, metadata: [("host", "server1")] },
    { timestamp: 1640995380, value: 48.1, metadata: [("host", "server1")] },
    { timestamp: 1640995440, value: 49.3, metadata: [("host", "server1")] }
  ])
  
  assert_eq(cpu_series.name, "cpu_usage")
  assert_eq(cpu_series.data_points.length(), 5)
  assert_eq(cpu_series.compression_method, "none")
  
  // Test delta compression (simple implementation)
  let delta_compress = fn(series: TimeSeries) {
    let mut compressed = []
    let mut prev_value = None
    
    for point in series.data_points {
      let delta_value = match prev_value {
        Some(prev) => point.value - prev
        None => point.value
      }
      
      compressed = compressed.push({
        timestamp: point.timestamp,
        value: delta_value,
        metadata: point.metadata
      })
      
      prev_value = Some(point.value)
    }
    
    {
      name: series.name,
      data_points: compressed,
      compression_method: "delta"
    }
  }
  
  let compressed_cpu = delta_compress(cpu_series)
  assert_eq(compressed_cpu.compression_method, "delta")
  assert_eq(compressed_cpu.data_points.length(), 5)
  
  // First value should be unchanged (original value)
  assert_eq(compressed_cpu.data_points[0].value, 45.2)
  
  // Subsequent values should be deltas
  assert_eq(compressed_cpu.data_points[1].value, 2.6)  // 47.8 - 45.2
  assert_eq(compressed_cpu.data_points[2].value, -1.3) // 46.5 - 47.8
  assert_eq(compressed_cpu.data_points[3].value, 1.6)  // 48.1 - 46.5
  assert_eq(compressed_cpu.data_points[4].value, 1.2)  // 49.3 - 48.1
  
  // Test delta decompression
  let delta_decompress = fn(compressed_series: TimeSeries) {
    let mut decompressed = []
    let mut prev_value = None
    
    for point in compressed_series.data_points {
      let original_value = match prev_value {
        Some(prev) => prev + point.value
        None => point.value
      }
      
      decompressed = decompressed.push({
        timestamp: point.timestamp,
        value: original_value,
        metadata: point.metadata
      })
      
      prev_value = Some(original_value)
    }
    
    {
      name: compressed_series.name,
      data_points: decompressed,
      compression_method: "none"
    }
  }
  
  let decompressed_cpu = delta_decompress(compressed_cpu)
  assert_eq(decompressed_cpu.compression_method, "none")
  assert_eq(decompressed_cpu.data_points.length(), 5)
  
  // Verify decompression matches original
  for i in 0..cpu_series.data_points.length() {
    assert_eq(decompressed_cpu.data_points[i].value, cpu_series.data_points[i].value)
    assert_eq(decompressed_cpu.data_points[i].timestamp, cpu_series.data_points[i].timestamp)
  }
  
  // Test time-based downsampling
  let downsample = fn(series: TimeSeries, interval_seconds: Int) {
    let mut downsampled = []
    let mut current_window_start = series.data_points[0].timestamp
    let mut window_points = []
    
    for point in series.data_points {
      if point.timestamp < current_window_start + interval_seconds {
        window_points = window_points.push(point)
      } else {
        // Process current window
        if window_points.length() > 0 {
          let avg_value = window_points.reduce(fn(acc, p) { acc + p.value }, 0.0) / window_points.length().to_float()
          downsampled = downsampled.push({
            timestamp: current_window_start + interval_seconds / 2,
            value: avg_value,
            metadata: window_points[0].metadata
          })
        }
        
        // Start new window
        current_window_start = point.timestamp
        window_points = [point]
      }
    }
    
    // Process last window
    if window_points.length() > 0 {
      let avg_value = window_points.reduce(fn(acc, p) { acc + p.value }, 0.0) / window_points.length().to_float()
      downsampled = downsampled.push({
        timestamp: current_window_start + interval_seconds / 2,
        value: avg_value,
        metadata: window_points[0].metadata
      })
    }
    
    {
      name: series.name,
      data_points: downsampled,
      compression_method: "downsampled_" + interval_seconds.to_string() + "s"
    }
  }
  
  let downsampled_cpu = downsample(cpu_series, 120)  // 2-minute intervals
  assert_eq(downsampled_cpu.compression_method, "downsampled_120s")
  
  // With 5 data points at 60-second intervals and 120-second downsampling,
  // we should get 3 downsampled points
  assert_eq(downsampled_cpu.data_points.length(), 3)
  
  // Test data retention policy
  type RetentionPolicy = {
    max_age_seconds: Int,
    max_points: Int,
    priority: String  // "high", "medium", "low"
  }
  
  let apply_retention_policy = fn(series: TimeSeries, policy: RetentionPolicy, current_time: Int) {
    let mut filtered = []
    
    for point in series.data_points {
      let age = current_time - point.timestamp
      if age <= policy.max_age_seconds {
        filtered = filtered.push(point)
      }
    }
    
    // If still too many points, keep the most recent ones
    if filtered.length() > policy.max_points {
      let start_index = filtered.length() - policy.max_points
      filtered = filtered.slice(start_index, policy.max_points)
    }
    
    {
      name: series.name,
      data_points: filtered,
      compression_method: series.compression_method + "_retained"
    }
  }
  
  let retention_policy = {
    max_age_seconds: 300,  // 5 minutes
    max_points: 3,
    priority: "medium"
  }
  
  let retained_cpu = apply_retention_policy(cpu_series, retention_policy, 1640995800)
  assert_eq(retained_cpu.compression_method, "none_retained")
  assert_eq(retained_cpu.data_points.length(), 3)
  
  // Should keep the 3 most recent points
  assert_eq(retained_cpu.data_points[0].timestamp, 1640995320)
  assert_eq(retained_cpu.data_points[1].timestamp, 1640995380)
  assert_eq(retained_cpu.data_points[2].timestamp, 1640995440)
}

// Test 6: Telemetry Query and Analysis
test "telemetry query and analysis" {
  // Define query types
  enum QueryFilter {
    TimeRange(Int, Int)  // start_time, end_time
    TagEqual(String, String)  // key, value
    ValueRange(Float, Float)  // min, max
    NameEquals(String)
  }
  
  type Query = {
    filters: Array[QueryFilter],
    aggregation: Option[String],
    group_by: Option[String]
  }
  
  type TelemetryData = {
    name: String,
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // Create sample telemetry data
  let telemetry_data = [
    { name: "cpu_usage", timestamp: 1640995200, value: 45.2, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "cpu_usage", timestamp: 1640995260, value: 47.8, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "cpu_usage", timestamp: 1640995320, value: 46.5, tags: [("host", "server2"), ("region", "us-west")] },
    { name: "memory_usage", timestamp: 1640995200, value: 68.5, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "memory_usage", timestamp: 1640995380, value: 72.1, tags: [("host", "server2"), ("region", "us-east")] },
    { name: "response_time", timestamp: 1640995400, value: 125.5, tags: [("endpoint", "/api/users"), ("method", "GET")] },
    { name: "response_time", timestamp: 1640995460, value: 85.3, tags: [("endpoint", "/api/products"), ("method", "GET")] }
  ]
  
  // Test filter application
  let apply_filter = fn(data: TelemetryData, filter: QueryFilter) {
    match filter {
      QueryFilter::TimeRange(start, end) => data.timestamp >= start and data.timestamp <= end,
      QueryFilter::TagEqual(key, value) => {
        let mut found = false
        for (k, v) in data.tags {
          if k == key and v == value {
            found = true
          }
        }
        found
      }
      QueryFilter::ValueRange(min, max) => data.value >= min and data.value <= max,
      QueryFilter::NameEquals(name) => data.name == name
    }
  }
  
  // Test query execution
  let execute_query = fn(data: Array[TelemetryData], query: Query) {
    let mut filtered_data = data
    
    // Apply all filters
    for filter in query.filters {
      let mut new_filtered = []
      for point in filtered_data {
        if apply_filter(point, filter) {
          new_filtered = new_filtered.push(point)
        }
      }
      filtered_data = new_filtered
    }
    
    // Apply aggregation if specified
    match query.aggregation {
      Some("avg") => {
        if filtered_data.length() > 0 {
          let sum = filtered_data.reduce(fn(acc, p) { acc + p.value }, 0.0)
          let avg = sum / filtered_data.length().to_float()
          [{
            name: "aggregated",
            timestamp: 0,
            value: avg,
            tags: []
          }]
        } else {
          []
        }
      }
      Some("max") => {
        if filtered_data.length() > 0 {
          let max_value = filtered_data.reduce(fn(acc, p) { if p.value > acc { p.value } else { acc } }, filtered_data[0].value)
          [{
            name: "aggregated",
            timestamp: 0,
            value: max_value,
            tags: []
          }]
        } else {
          []
        }
      }
      Some("min") => {
        if filtered_data.length() > 0 {
          let min_value = filtered_data.reduce(fn(acc, p) { if p.value < acc { p.value } else { acc } }, filtered_data[0].value)
          [{
            name: "aggregated",
            timestamp: 0,
            value: min_value,
            tags: []
          }]
        } else {
          []
        }
      }
      Some("count") => {
        [{
          name: "aggregated",
          timestamp: 0,
          value: filtered_data.length().to_float(),
          tags: []
        }]
      }
      None => filtered_data
    }
  }
  
  // Test time range query
  let time_range_query = {
    filters: [QueryFilter::TimeRange(1640995200, 1640995300)],
    aggregation: None,
    group_by: None
  }
  
  let time_range_results = execute_query(telemetry_data, time_range_query)
  assert_eq(time_range_results.length(), 3)
  
  // Test tag filter query
  let tag_filter_query = {
    filters: [QueryFilter::TagEqual("host", "server1")],
    aggregation: None,
    group_by: None
  }
  
  let tag_filter_results = execute_query(telemetry_data, tag_filter_query)
  assert_eq(tag_filter_results.length(), 3)
  
  // Test name filter query
  let name_filter_query = {
    filters: [QueryFilter::NameEquals("cpu_usage")],
    aggregation: None,
    group_by: None
  }
  
  let name_filter_results = execute_query(telemetry_data, name_filter_query)
  assert_eq(name_filter_results.length(), 3)
  
  // Test value range query
  let value_range_query = {
    filters: [QueryFilter::ValueRange(70.0, 80.0)],
    aggregation: None,
    group_by: None
  }
  
  let value_range_results = execute_query(telemetry_data, value_range_query)
  assert_eq(value_range_results.length(), 2)
  
  // Test multiple filters
  let multi_filter_query = {
    filters: [
      QueryFilter::NameEquals("cpu_usage"),
      QueryFilter::TagEqual("region", "us-west")
    ],
    aggregation: None,
    group_by: None
  }
  
  let multi_filter_results = execute_query(telemetry_data, multi_filter_query)
  assert_eq(multi_filter_results.length(), 3)
  
  // Test aggregation query
  let aggregation_query = {
    filters: [QueryFilter::NameEquals("cpu_usage")],
    aggregation: Some("avg"),
    group_by: None
  }
  
  let aggregation_results = execute_query(telemetry_data, aggregation_query)
  assert_eq(aggregation_results.length(), 1)
  assert_eq(aggregation_results[0].name, "aggregated")
  let avg_cpu = (45.2 + 47.8 + 46.5) / 3.0
  assert_eq(aggregation_results[0].value, avg_cpu)
  
  // Test max aggregation
  let max_query = {
    filters: [QueryFilter::NameEquals("response_time")],
    aggregation: Some("max"),
    group_by: None
  }
  
  let max_results = execute_query(telemetry_data, max_query)
  assert_eq(max_results.length(), 1)
  assert_eq(max_results[0].value, 125.5)
  
  // Test count aggregation
  let count_query = {
    filters: [QueryFilter::TagEqual("region", "us-west")],
    aggregation: Some("count"),
    group_by: None
  }
  
  let count_results = execute_query(telemetry_data, count_query)
  assert_eq(count_results.length(), 1)
  assert_eq(count_results[0].value, 4.0)
  
  // Test complex query with multiple filters and aggregation
  let complex_query = {
    filters: [
      QueryFilter::TimeRange(1640995200, 1640995400),
      QueryFilter::ValueRange(40.0, 80.0)
    ],
    aggregation: Some("avg"),
    group_by: None
  }
  
  let complex_results = execute_query(telemetry_data, complex_query)
  assert_eq(complex_results.length(), 1)
  // Should include cpu_usage and memory_usage values within range
  let expected_values = [45.2, 47.8, 46.5, 68.5]
  let expected_avg = expected_values.reduce(fn(acc, v) { acc + v }, 0.0) / expected_values.length().to_float()
  assert_eq(complex_results[0].value, expected_avg)
}

// Test 7: Anomaly Detection in Telemetry Data
test "anomaly detection in telemetry data" {
  // Define anomaly detection types
  enum AnomalyType {
    Spike
    Drop
    Outlier
    PatternDeviation
  }
  
  type Anomaly = {
    timestamp: Int,
    value: Float,
    expected_value: Float,
    anomaly_type: AnomalyType,
    severity: Float,  // 0.0 to 1.0
    confidence: Float  // 0.0 to 1.0
  }
  
  type AnomalyDetector = {
    algorithm: String,
    sensitivity: Float,
    window_size: Int
  }
  
  // Create sample telemetry data with anomalies
  let normal_data = [45.2, 47.8, 46.5, 48.1, 49.3, 47.6, 46.9, 48.5]
  let anomalous_data = [45.2, 47.8, 46.5, 78.5, 49.3, 47.6, 15.2, 48.5]  // Contains spike and drop
  
  // Test statistical anomaly detection
  let detect_statistical_anomalies = fn(data: Array[Float], detector: AnomalyDetector) {
    let mut anomalies = []
    
    if data.length() < detector.window_size {
      return anomalies
    }
    
    // Calculate mean and standard deviation for initial window
    let initial_window = data.slice(0, detector.window_size)
    let mean = initial_window.reduce(fn(acc, v) { acc + v }, 0.0) / initial_window.length().to_float()
    
    let variance = initial_window.reduce(fn(acc, v) { 
      let diff = v - mean
      acc + diff * diff 
    }, 0.0) / initial_window.length().to_float()
    
    let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
    
    // Check each data point for anomalies
    for i in 0..data.length() {
      let value = data[i]
      let z_score = if std_dev > 0.0 { (value - mean).abs() / std_dev } else { 0.0 }
      
      // Higher z-score indicates higher probability of anomaly
      if z_score > detector.sensitivity {
        let anomaly_type = if value > mean {
          AnomalyType::Spike
        } else {
          AnomalyType::Drop
        }
        
        let severity = if z_score > 3.0 { 1.0 } else { z_score / 3.0 }
        let confidence = if z_score > 2.5 { 0.9 } else { z_score / 2.5 }
        
        anomalies = anomalies.push({
          timestamp: 1640995200 + i * 60,  // 1-minute intervals
          value,
          expected_value: mean,
          anomaly_type,
          severity,
          confidence
        })
      }
    }
    
    anomalies
  }
  
  let detector = {
    algorithm: "statistical",
    sensitivity: 2.0,  // Detect values 2 standard deviations from mean
    window_size: 3
  }
  
  // Test with normal data (should detect few or no anomalies)
  let normal_anomalies = detect_statistical_anomalies(normal_data, detector)
  assert_eq(normal_anomalies.length(), 0)
  
  // Test with anomalous data (should detect spike and drop)
  let detected_anomalies = detect_statistical_anomalies(anomalous_data, detector)
  assert_true(detected_anomalies.length() >= 2)
  
  // Verify spike detection
  let spike_anomalies = detected_anomalies.filter(fn(a) { 
    match a.anomaly_type {
      AnomalyType::Spike => true
      _ => false
    }
  })
  assert_true(spike_anomalies.length() >= 1)
  
  // Verify drop detection
  let drop_anomalies = detected_anomalies.filter(fn(a) { 
    match a.anomaly_type {
      AnomalyType::Drop => true
      _ => false
    }
  })
  assert_true(drop_anomalies.length() >= 1)
  
  // Test moving average anomaly detection
  let detect_moving_average_anomalies = fn(data: Array[Float], window_size: Int, threshold: Float) {
    let mut anomalies = []
    
    if data.length() < window_size * 2 {
      return anomalies
    }
    
    for i in window_size..(data.length() - window_size) {
      // Calculate moving average of surrounding points
      let mut sum = 0.0
      let mut count = 0
      
      for j in (i - window_size)..(i + window_size + 1) {
        if j != i and j >= 0 and j < data.length() {
          sum = sum + data[j]
          count = count + 1
        }
      }
      
      if count > 0 {
        let moving_avg = sum / count.to_float()
        let current_value = data[i]
        let deviation = (current_value - moving_avg).abs() / moving_avg
        
        if deviation > threshold {
          let anomaly_type = if current_value > moving_avg {
            AnomalyType::Spike
          } else {
            AnomalyType::Drop
          }
          
          anomalies = anomalies.push({
            timestamp: 1640995200 + i * 60,
            value: current_value,
            expected_value: moving_avg,
            anomaly_type,
            severity: if deviation > threshold * 2 { 1.0 } else { deviation / (threshold * 2) },
            confidence: if deviation > threshold * 1.5 { 0.9 } else { deviation / (threshold * 1.5) }
          })
        }
      }
    }
    
    anomalies
  }
  
  let moving_average_anomalies = detect_moving_average_anomalies(anomalous_data, 2, 0.3)
  assert_true(moving_average_anomalies.length() >= 2)
  
  // Test pattern-based anomaly detection
  let detect_pattern_anomalies = fn(data: Array[Float], pattern_length: Int) {
    let mut anomalies = []
    
    if data.length() < pattern_length * 3 {
      return anomalies
    }
    
    // Extract pattern from first segment
    let pattern = data.slice(0, pattern_length)
    
    // Compare pattern with subsequent segments
    let segment_count = data.length() / pattern_length
    for i in 1..segment_count {
      let start_index = i * pattern_length
      if start_index + pattern_length <= data.length() {
        let segment = data.slice(start_index, pattern_length)
        
        // Calculate pattern similarity (simplified)
        let mut similarity = 0.0
        for j in 0..pattern_length {
          let diff = (pattern[j] - segment[j]).abs()
          similarity = similarity + (1.0 - diff / pattern[j])
        }
        similarity = similarity / pattern_length.to_float()
        
        // If similarity is low, flag as pattern deviation
        if similarity < 0.7 {
          for j in 0..pattern_length {
            anomalies = anomalies.push({
              timestamp: 1640995200 + (start_index + j) * 60,
              value: segment[j],
              expected_value: pattern[j],
              anomaly_type: AnomalyType::PatternDeviation,
              severity: 1.0 - similarity,
              confidence: if similarity < 0.5 { 0.9 } else { 0.7 }
            })
          }
        }
      }
    }
    
    anomalies
  }
  
  // Create data with repeating pattern followed by deviation
  let pattern_data = [10.0, 20.0, 15.0, 10.0, 20.0, 15.0, 10.0, 20.0, 15.0, 50.0, 60.0, 55.0]
  let pattern_anomalies = detect_pattern_anomalies(pattern_data, 3)
  assert_true(pattern_anomalies.length() >= 3)
  
  // Test anomaly severity classification
  let classify_anomaly_severity = fn(anomalies: Array[Anomaly]) {
    let mut high = []
    let mut medium = []
    let mut low = []
    
    for anomaly in anomalies {
      if anomaly.severity >= 0.8 {
        high = high.push(anomaly)
      } else if anomaly.severity >= 0.5 {
        medium = medium.push(anomaly)
      } else {
        low = low.push(anomaly)
      }
    }
    
    { high, medium, low }
  }
  
  let classified = classify_anomaly_severity(detected_anomalies)
  
  // Verify classification
  for anomaly in classified.high {
    assert_true(anomaly.severity >= 0.8)
  }
  
  for anomaly in classified.medium {
    assert_true(anomaly.severity >= 0.5 and anomaly.severity < 0.8)
  }
  
  for anomaly in classified.low {
    assert_true(anomaly.severity < 0.5)
  }
}

// Test 8: Telemetry Data Correlation Analysis
test "telemetry data correlation analysis" {
  // Define correlation types
  enum CorrelationType {
    Positive
    Negative
    None
  }
  
  type CorrelationResult = {
    metric1: String,
    metric2: String,
    correlation_coefficient: Float,
    correlation_type: CorrelationType,
    confidence: Float
  }
  
  type MetricSeries = {
    name: String,
    values: Array[Float],
    timestamps: Array[Int]
  }
  
  // Create sample metric series
  let cpu_series = {
    name: "cpu_usage",
    values: [45.2, 47.8, 46.5, 48.1, 49.3, 47.6, 46.9, 48.5, 49.8, 50.2],
    timestamps: [1640995200, 1640995260, 1640995320, 1640995380, 1640995440, 1640995500, 1640995560, 1640995620, 1640995680, 1640995740]
  }
  
  let memory_series = {
    name: "memory_usage",
    values: [68.5, 69.2, 70.1, 71.3, 72.5, 73.1, 74.2, 75.0, 75.8, 76.5],
    timestamps: [1640995200, 1640995260, 1640995320, 1640995380, 1640995440, 1640995500, 1640995560, 1640995620, 1640995680, 1640995740]
  }
  
  let response_time_series = {
    name: "response_time",
    values: [120.5, 125.3, 130.1, 145.7, 160.2, 155.8, 165.3, 170.5, 175.2, 180.1],
    timestamps: [1640995200, 1640995260, 1640995320, 1640995380, 1640995440, 1640995500, 1640995560, 1640995620, 1640995680, 1640995740]
  }
  
  let error_rate_series = {
    name: "error_rate",
    values: [2.1, 2.3, 2.0, 1.8, 1.5, 1.2, 1.0, 0.8, 0.5, 0.3],
    timestamps: [1640995200, 1640995260, 1640995320, 1640995380, 1640995440, 1640995500, 1640995560, 1640995620, 1640995680, 1640995740]
  }
  
  // Test Pearson correlation coefficient calculation
  let calculate_pearson_correlation = fn(x: Array[Float], y: Array[Float]) {
    if x.length() != y.length() or x.length() == 0 {
      0.0
    } else {
      let n = x.length().to_float()
      
      // Calculate means
      let x_mean = x.reduce(fn(acc, v) { acc + v }, 0.0) / n
      let y_mean = y.reduce(fn(acc, v) { acc + v }, 0.0) / n
      
      // Calculate covariance and standard deviations
      let mut covariance = 0.0
      let mut x_variance = 0.0
      let mut y_variance = 0.0
      
      for i in 0..x.length() {
        let x_diff = x[i] - x_mean
        let y_diff = y[i] - y_mean
        
        covariance = covariance + x_diff * y_diff
        x_variance = x_variance + x_diff * x_diff
        y_variance = y_variance + y_diff * y_diff
      }
      
      let x_std = if x_variance > 0.0 { x_variance.sqrt() } else { 0.0 }
      let y_std = if y_variance > 0.0 { y_variance.sqrt() } else { 0.0 }
      
      if x_std > 0.0 and y_std > 0.0 {
        covariance / (x_std * y_std)
      } else {
        0.0
      }
    }
  }
  
  // Test correlation type determination
  let determine_correlation_type = fn(coefficient: Float) {
    if coefficient.abs() < 0.3 {
      CorrelationType::None
    } else if coefficient > 0.0 {
      CorrelationType::Positive
    } else {
      CorrelationType::Negative
    }
  }
  
  // Test correlation confidence calculation
  let calculate_confidence = fn(coefficient: Float, sample_size: Int) {
    let t_statistic = coefficient.abs() * ((sample_size - 2).to_float() / (1.0 - coefficient * coefficient)).sqrt()
    // Simplified confidence calculation
    if t_statistic > 2.0 {
      0.95
    } else if t_statistic > 1.5 {
      0.8
    } else if t_statistic > 1.0 {
      0.6
    } else {
      0.4
    }
  }
  
  // Test correlation analysis between two series
  let analyze_correlation = fn(series1: MetricSeries, series2: MetricSeries) {
    let coefficient = calculate_pearson_correlation(series1.values, series2.values)
    let correlation_type = determine_correlation_type(coefficient)
    let confidence = calculate_confidence(coefficient, series1.values.length())
    
    {
      metric1: series1.name,
      metric2: series2.name,
      correlation_coefficient: coefficient,
      correlation_type,
      confidence
    }
  }
  
  // Test positive correlation (CPU and Memory)
  let cpu_memory_correlation = analyze_correlation(cpu_series, memory_series)
  assert_eq(cpu_memory_correlation.metric1, "cpu_usage")
  assert_eq(cpu_memory_correlation.metric2, "memory_usage")
  assert_true(cpu_memory_correlation.correlation_coefficient > 0.7)
  match cpu_memory_correlation.correlation_type {
    CorrelationType::Positive => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(cpu_memory_correlation.confidence > 0.8)
  
  // Test positive correlation (CPU and Response Time)
  let cpu_response_correlation = analyze_correlation(cpu_series, response_time_series)
  assert_true(cpu_response_correlation.correlation_coefficient > 0.8)
  match cpu_response_correlation.correlation_type {
    CorrelationType::Positive => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test negative correlation (CPU and Error Rate)
  let cpu_error_correlation = analyze_correlation(cpu_series, error_rate_series)
  assert_true(cpu_error_correlation.correlation_coefficient < -0.7)
  match cpu_error_correlation.correlation_type {
    CorrelationType::Negative => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test weak correlation
  let memory_error_correlation = analyze_correlation(memory_series, error_rate_series)
  assert_true(memory_error_correlation.correlation_coefficient < -0.5)
  
  // Test multi-series correlation analysis
  let analyze_multiple_correlations = fn(series_list: Array[MetricSeries]) {
    let mut correlations = []
    
    for i in 0..series_list.length() {
      for j in (i + 1)..series_list.length() {
        let correlation = analyze_correlation(series_list[i], series_list[j])
        correlations = correlations.push(correlation)
      }
    }
    
    correlations
  }
  
  let all_series = [cpu_series, memory_series, response_time_series, error_rate_series]
  let all_correlations = analyze_multiple_correlations(all_series)
  assert_eq(all_correlations.length(), 6)  // 4 choose 2 = 6 correlations
  
  // Test finding strongest correlations
  let find_strongest_correlations = fn(correlations: Array[CorrelationResult], min_strength: Float) {
    correlations.filter(fn(c) { c.correlation_coefficient.abs() >= min_strength })
  }
  
  let strong_correlations = find_strongest_correlations(all_correlations, 0.7)
  assert_true(strong_correlations.length() >= 3)
  
  // Test correlation ranking
  let rank_correlations = fn(correlations: Array[CorrelationResult]) {
    correlations.sort(fn(a, b) { 
      let abs_a = a.correlation_coefficient.abs()
      let abs_b = b.correlation_coefficient.abs()
      if abs_a > abs_b { -1 } else if abs_a < abs_b { 1 } else { 0 }
    })
  }
  
  let ranked_correlations = rank_correlations(all_correlations)
  
  // Verify ranking (strongest correlation first)
  for i in 1..ranked_correlations.length() {
    let prev_strength = ranked_correlations[i-1].correlation_coefficient.abs()
    let curr_strength = ranked_correlations[i].correlation_coefficient.abs()
    assert_true(prev_strength >= curr_strength)
  }
  
  // Test time-lagged correlation
  let calculate_lagged_correlation = fn(series1: MetricSeries, series2: MetricSeries, lag: Int) {
    if lag >= 0 and lag < series1.values.length() and lag < series2.values.length() {
      let trimmed_x = series1.values.slice(0, series1.values.length() - lag)
      let trimmed_y = series2.values.slice(lag, series2.values.length())
      calculate_pearson_correlation(trimmed_x, trimmed_y)
    } else if lag < 0 and (-lag) < series1.values.length() and (-lag) < series2.values.length() {
      let trimmed_x = series1.values.slice(-lag, series1.values.length())
      let trimmed_y = series2.values.slice(0, series2.values.length() + lag)
      calculate_pearson_correlation(trimmed_x, trimmed_y)
    } else {
      0.0
    }
  }
  
  // Test finding optimal lag
  let find_optimal_lag = fn(series1: MetricSeries, series2: MetricSeries, max_lag: Int) {
    let mut best_lag = 0
    let mut best_correlation = 0.0
    
    for lag in -max_lag..(max_lag + 1) {
      let correlation = calculate_lagged_correlation(series1, series2, lag).abs()
      if correlation > best_correlation {
        best_correlation = correlation
        best_lag = lag
      }
    }
    
    { lag: best_lag, correlation: best_correlation }
  }
  
  let optimal_lag = find_optimal_lag(cpu_series, response_time_series, 2)
  assert_true(optimal_lag.lag >= -2 and optimal_lag.lag <= 2)
  assert_true(optimal_lag.correlation > 0.0)
}

// Test 9: Telemetry Data Pipeline Processing
test "telemetry data pipeline processing" {
  // Define pipeline stages
  enum PipelineStage {
    Ingestion
    Validation
    Transformation
    Enrichment
    Aggregation
    Storage
  }
  
  type PipelineResult = {
    stage: PipelineStage,
    success: Bool,
    input_count: Int,
    output_count: Int,
    errors: Array[String]
  }
  
  type TelemetryEvent = {
    id: String,
    timestamp: Int,
    source: String,
    event_type: String,
    data: Array[(String, String)],
    processed: Bool
  }
  
  // Create sample telemetry events
  let raw_events = [
    { id: "event-001", timestamp: 1640995200, source: "api-service", event_type: "request", data: [("endpoint", "/users"), ("method", "GET"), ("status", "200")], processed: false },
    { id: "event-002", timestamp: 1640995260, source: "api-service", event_type: "request", data: [("endpoint", "/products"), ("method", "POST"), ("status", "201")], processed: false },
    { id: "event-003", timestamp: 1640995320, source: "db-service", event_type: "query", data: [("table", "users"), ("duration", "45")], processed: false },
    { id: "event-004", timestamp: 1640995380, source: "api-service", event_type: "error", data: [("error_type", "timeout"), ("endpoint", "/orders")], processed: false },
    { id: "event-005", timestamp: 1640995440, source: "cache-service", event_type: "hit", data: [("key", "user:123"), ("ttl", "300")], processed: false }
  ]
  
  // Test ingestion stage
  let ingest_events = fn(events: Array[TelemetryEvent]) {
    let mut ingested = []
    let mut errors = []
    
    for event in events {
      // Basic validation during ingestion
      if event.id.length() > 0 and event.timestamp > 0 and event.source.length() > 0 {
        ingested = ingested.push(event)
      } else {
        errors = errors.push("Invalid event: " + event.id)
      }
    }
    
    {
      stage: PipelineStage::Ingestion,
      success: errors.length() == 0,
      input_count: events.length(),
      output_count: ingested.length(),
      errors
    }
  }
  
  let ingestion_result = ingest_events(raw_events)
  assert_eq(ingestion_result.stage, PipelineStage::Ingestion)
  assert_true(ingestion_result.success)
  assert_eq(ingestion_result.input_count, 5)
  assert_eq(ingestion_result.output_count, 5)
  assert_eq(ingestion_result.errors.length(), 0)
  
  // Test validation stage
  let validate_events = fn(events: Array[TelemetryEvent]) {
    let mut validated = []
    let mut errors = []
    
    for event in events {
      let mut valid = true
      let mut event_errors = []
      
      // Validate timestamp is within reasonable range
      if event.timestamp < 1640000000 or event.timestamp > 1650000000 {
        valid = false
        event_errors = event_errors.push("Invalid timestamp")
      }
      
      // Validate required fields
      if event.event_type.length() == 0 {
        valid = false
        event_errors = event_errors.push("Missing event type")
      }
      
      if event.data.length() == 0 {
        valid = false
        event_errors = event_errors.push("Missing event data")
      }
      
      if valid {
        validated = validated.push({ event | processed: true })
      } else {
        errors = errors.push("Validation failed for event " + event.id + ": " + event_errors.join(", "))
      }
    }
    
    {
      stage: PipelineStage::Validation,
      success: errors.length() == 0,
      input_count: events.length(),
      output_count: validated.length(),
      errors
    }
  }
  
  let validation_result = validate_events(raw_events)
  assert_eq(validation_result.stage, PipelineStage::Validation)
  assert_true(validation_result.success)
  assert_eq(validation_result.input_count, 5)
  assert_eq(validation_result.output_count, 5)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test transformation stage
  let transform_events = fn(events: Array[TelemetryEvent]) {
    let mut transformed = []
    let mut errors = []
    
    for event in events {
      let mut transformed_data = []
      
      // Transform data based on event type
      match event.event_type {
        "request" => {
          // Extract and transform request-specific fields
          for (key, value) in event.data {
            if key == "status" {
              // Convert status code to category
              let status_category = match value {
                "200" | "201" | "204" => "success"
                "400" | "401" | "403" | "404" => "client_error"
                "500" | "502" | "503" => "server_error"
                _ => "unknown"
              }
              transformed_data = transformed_data.push(("status_category", status_category))
            } else {
              transformed_data = transformed_data.push((key, value))
            }
          }
        }
        "query" => {
          // Transform query-specific fields
          for (key, value) in event.data {
            if key == "duration" {
              // Convert duration string to numeric category
              let duration_ms = value.to_int()
              let duration_category = if duration_ms < 10 {
                "fast"
              } else if duration_ms < 100 {
                "medium"
              } else {
                "slow"
              }
              transformed_data = transformed_data.push(("duration_category", duration_category))
              transformed_data = transformed_data.push(("duration_ms", value))
            } else {
              transformed_data = transformed_data.push((key, value))
            }
          }
        }
        _ => {
          // Pass through unchanged for other event types
          transformed_data = event.data
        }
      }
      
      transformed = transformed.push({
        id: event.id,
        timestamp: event.timestamp,
        source: event.source,
        event_type: event.event_type,
        data: transformed_data,
        processed: event.processed
      })
    }
    
    {
      stage: PipelineStage::Transformation,
      success: errors.length() == 0,
      input_count: events.length(),
      output_count: transformed.length(),
      errors
    }
  }
  
  let transformation_result = transform_events(raw_events)
  assert_eq(transformation_result.stage, PipelineStage::Transformation)
  assert_true(transformation_result.success)
  assert_eq(transformation_result.input_count, 5)
  assert_eq(transformation_result.output_count, 5)
  assert_eq(transformation_result.errors.length(), 0)
  
  // Test enrichment stage
  let enrich_events = fn(events: Array[TelemetryEvent]) {
    let mut enriched = []
    let mut errors = []
    
    for event in events {
      let mut enriched_data = event.data
      
      // Add enrichment data based on source
      match event.source {
        "api-service" => {
          enriched_data = enriched_data.push(("service_version", "1.2.3"))
          enriched_data = enriched_data.push(("environment", "production"))
        }
        "db-service" => {
          enriched_data = enriched_data.push(("database_type", "postgresql"))
          enriched_data = enriched_data.push(("cluster", "primary"))
        }
        "cache-service" => {
          enriched_data = enriched_data.push(("cache_type", "redis"))
          enriched_data = enriched_data.push(("cluster", "cache-cluster-1"))
        }
        _ => {}
      }
      
      // Add common enrichment
      enriched_data = enriched_data.push(("processed_at", "1640995500"))
      enriched_data = enriched_data.push(("region", "us-west-2"))
      
      enriched = enriched.push({
        id: event.id,
        timestamp: event.timestamp,
        source: event.source,
        event_type: event.event_type,
        data: enriched_data,
        processed: event.processed
      })
    }
    
    {
      stage: PipelineStage::Enrichment,
      success: errors.length() == 0,
      input_count: events.length(),
      output_count: enriched.length(),
      errors
    }
  }
  
  let enrichment_result = enrich_events(raw_events)
  assert_eq(enrichment_result.stage, PipelineStage::Enrichment)
  assert_true(enrichment_result.success)
  assert_eq(enrichment_result.input_count, 5)
  assert_eq(enrichment_result.output_count, 5)
  assert_eq(enrichment_result.errors.length(), 0)
  
  // Test aggregation stage
  let aggregate_events = fn(events: Array[TelemetryEvent]) {
    let mut aggregated = []
    let mut errors = []
    
    // Group events by source and event type
    let mut groups = []
    
    for event in events {
      let mut found_group = false
      let mut updated_groups = []
      
      for group in groups {
        if group.source == event.source and group.event_type == event.event_type {
          found_group = true
          updated_groups = updated_groups.push({
            source: group.source,
            event_type: group.event_type,
            count: group.count + 1,
            latest_timestamp: if event.timestamp > group.latest_timestamp { event.timestamp } else { group.latest_timestamp }
          })
        } else {
          updated_groups = updated_groups.push(group)
        }
      }
      
      if not(found_group) {
        updated_groups = updated_groups.push({
          source: event.source,
          event_type: event.event_type,
          count: 1,
          latest_timestamp: event.timestamp
        })
      }
      
      groups = updated_groups
    }
    
    // Create aggregated events
    for group in groups {
      aggregated = aggregated.push({
        id: "aggregated-" + group.source + "-" + group.event_type,
        timestamp: group.latest_timestamp,
        source: "aggregator",
        event_type: "aggregated",
        data: [
          ("source", group.source),
          ("original_event_type", group.event_type),
          ("count", group.count.to_string())
        ],
        processed: true
      })
    }
    
    {
      stage: PipelineStage::Aggregation,
      success: errors.length() == 0,
      input_count: events.length(),
      output_count: aggregated.length(),
      errors
    }
  }
  
  let aggregation_result = aggregate_events(raw_events)
  assert_eq(aggregation_result.stage, PipelineStage::Aggregation)
  assert_true(aggregation_result.success)
  assert_eq(aggregation_result.input_count, 5)
  assert_eq(aggregation_result.output_count, 4)  // api-service(2 types), db-service(1), cache-service(1)
  assert_eq(aggregation_result.errors.length(), 0)
  
  // Test full pipeline execution
  let execute_pipeline = fn(events: Array[TelemetryEvent]) {
    let results = []
    
    // Execute each stage
    let ingestion = ingest_events(events)
    let validation = validate_events(events)
    let transformation = transform_events(events)
    let enrichment = enrich_events(events)
    let aggregation = aggregate_events(events)
    
    [ingestion, validation, transformation, enrichment, aggregation]
  }
  
  let pipeline_results = execute_pipeline(raw_events)
  assert_eq(pipeline_results.length(), 5)
  
  // Verify all stages succeeded
  for result in pipeline_results {
    assert_true(result.success)
    assert_eq(result.errors.length(), 0)
  }
  
  // Test pipeline error handling
  let invalid_events = [
    { id: "", timestamp: 0, source: "", event_type: "", data: [], processed: false },
    { id: "valid-event", timestamp: 1640995200, source: "api-service", event_type: "request", data: [("endpoint", "/test")], processed: false }
  ]
  
  let error_pipeline_results = execute_pipeline(invalid_events)
  
  // Ingestion should fail due to invalid event
  assert_false(error_pipeline_results[0].success)
  assert_true(error_pipeline_results[0].errors.length() > 0)
  
  // Validation should also fail
  assert_false(error_pipeline_results[1].success)
  assert_true(error_pipeline_results[1].errors.length() > 0)
}

// Test 10: Telemetry Data Export and Reporting
test "telemetry data export and reporting" {
  // Define export formats
  enum ExportFormat {
    JSON
    CSV
    Prometheus
    Custom(String)
  }
  
  type ExportReport = {
    format: ExportFormat,
    records_count: Int,
    file_size: Int,
    duration_ms: Int,
    success: Bool
  }
  
  type TelemetryRecord = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // Create sample telemetry records
  let telemetry_records = [
    { timestamp: 1640995200, metric_name: "cpu_usage", value: 45.2, tags: [("host", "server1"), ("region", "us-west")] },
    { timestamp: 1640995260, metric_name: "memory_usage", value: 68.5, tags: [("host", "server1"), ("region", "us-west")] },
    { timestamp: 1640995320, metric_name: "cpu_usage", value: 47.8, tags: [("host", "server2"), ("region", "us-west")] },
    { timestamp: 1640995380, metric_name: "response_time", value: 125.5, tags: [("endpoint", "/api/users"), ("method", "GET")] },
    { timestamp: 1640995440, metric_name: "error_rate", value: 2.1, tags: [("service", "api-service"), ("version", "1.2.3")] }
  ]
  
  // Test JSON export
  let export_to_json = fn(records: Array[TelemetryRecord]) {
    let mut json_lines = []
    
    for record in records {
      let tags_str = record.tags.map(fn(t) { "\"" + t.0 + "\":\"" + t.1 + "\"" }).join(",")
      let json_line = "{"
        + "\"timestamp\":" + record.timestamp.to_string() + ","
        + "\"metric_name\":\"" + record.metric_name + "\"," 
        + "\"value\":" + record.value.to_string() + ","
        + "\"tags\":{" + tags_str + "}"
        + "}"
      json_lines = json_lines.push(json_line)
    }
    
    let json_content = "[" + json_lines.join(",") + "]"
    json_content
  }
  
  let json_export = export_to_json(telemetry_records)
  assert_true(json_export.contains("cpu_usage"))
  assert_true(json_export.contains("memory_usage"))
  assert_true(json_export.contains("response_time"))
  assert_true(json_export.contains("error_rate"))
  assert_true(json_export.contains("server1"))
  assert_true(json_export.contains("us-west"))
  
  // Test CSV export
  let export_to_csv = fn(records: Array[TelemetryRecord]) {
    let mut csv_lines = ["timestamp,metric_name,value,tags"]
    
    for record in records {
      let tags_str = record.tags.map(fn(t) { t.0 + "=" + t.1 }).join(";")
      let csv_line = record.timestamp.to_string() + ","
        + record.metric_name + ","
        + record.value.to_string() + ","
        + tags_str
      csv_lines = csv_lines.push(csv_line)
    }
    
    csv_lines.join("\n")
  }
  
  let csv_export = export_to_csv(telemetry_records)
  assert_true(csv_export.contains("timestamp,metric_name,value,tags"))
  assert_true(csv_export.contains("cpu_usage"))
  assert_true(csv_export.contains("host=server1"))
  
  // Test Prometheus export
  let export_to_prometheus = fn(records: Array[TelemetryRecord]) {
    let mut prometheus_lines = []
    
    // Group records by metric name
    let mut metric_groups = []
    
    for record in records {
      let mut found_group = false
      let mut updated_groups = []
      
      for group in metric_groups {
        if group.metric_name == record.metric_name {
          found_group = true
          updated_groups = updated_groups.push({
            metric_name: group.metric_name,
            records: group.records.push(record)
          })
        } else {
          updated_groups = updated_groups.push(group)
        }
      }
      
      if not(found_group) {
        metric_groups = metric_groups.push({
          metric_name: record.metric_name,
          records: [record]
        })
      }
    }
    
    // Generate Prometheus format
    for group in metric_groups {
      for record in group.records {
        let tags_str = record.tags.map(fn(t) { t.0 + "=\"" + t.1 + "\"" }).join(",")
        let prometheus_line = group.metric_name
          + "{" + tags_str + "} "
          + record.value.to_string() + " "
          + record.timestamp.to_string()
        prometheus_lines = prometheus_lines.push(prometheus_line)
      }
    }
    
    prometheus_lines.join("\n")
  }
  
  let prometheus_export = export_to_prometheus(telemetry_records)
  assert_true(prometheus_export.contains("cpu_usage"))
  assert_true(prometheus_export.contains("host=\"server1\""))
  assert_true(prometheus_export.contains("1640995200"))
  
  // Test custom export format
  let export_to_custom = fn(records: Array[TelemetryRecord], template: String) {
    let mut custom_lines = []
    
    for record in records {
      let mut line = template
        .replace("{timestamp}", record.timestamp.to_string())
        .replace("{metric}", record.metric_name)
        .replace("{value}", record.value.to_string())
      
      // Replace tags
      for (key, value) in record.tags {
        line = line.replace("{" + key + "}", value)
      }
      
      custom_lines = custom_lines.push(line)
    }
    
    custom_lines.join("\n")
  }
  
  let custom_template = "[{timestamp}] {metric}={value} host={host} region={region}"
  let custom_export = export_to_custom(telemetry_records, custom_template)
  assert_true(custom_export.contains("[1640995200] cpu_usage=45.2 host=server1 region=us-west"))
  
  // Test export report generation
  let generate_export_report = fn(format: ExportFormat, records: Array[TelemetryRecord], content: String, duration_ms: Int) {
    {
      format,
      records_count: records.length(),
      file_size: content.length(),
      duration_ms,
      success: content.length() > 0
    }
  }
  
  let json_report = generate_export_report(ExportFormat::JSON, telemetry_records, json_export, 50)
  assert_eq(json_report.format, ExportFormat::JSON)
  assert_eq(json_report.records_count, 5)
  assert_true(json_report.file_size > 0)
  assert_eq(json_report.duration_ms, 50)
  assert_true(json_report.success)
  
  let csv_report = generate_export_report(ExportFormat::CSV, telemetry_records, csv_export, 30)
  assert_eq(csv_report.format, ExportFormat::CSV)
  assert_eq(csv_report.records_count, 5)
  assert_true(csv_report.file_size > 0)
  assert_eq(csv_report.duration_ms, 30)
  assert_true(csv_report.success)
  
  // Test export filtering
  let filter_records_for_export = fn(records: Array[TelemetryRecord], metric_names: Array[String]) {
    records.filter(fn(record) { metric_names.contains(record.metric_name) })
  }
  
  let cpu_memory_records = filter_records_for_export(telemetry_records, ["cpu_usage", "memory_usage"])
  assert_eq(cpu_memory_records.length(), 3)
  
  let filtered_json_export = export_to_json(cpu_memory_records)
  assert_true(filtered_json_export.contains("cpu_usage"))
  assert_true(filtered_json_export.contains("memory_usage"))
  assert_false(filtered_json_export.contains("response_time"))
  
  // Test export time range filtering
  let filter_records_by_time = fn(records: Array[TelemetryRecord], start_time: Int, end_time: Int) {
    records.filter(fn(record) { 
      record.timestamp >= start_time and record.timestamp <= end_time 
    })
  }
  
  let time_filtered_records = filter_records_by_time(telemetry_records, 1640995200, 1640995300)
  assert_eq(time_filtered_records.length(), 2)
  
  // Test export aggregation
  let aggregate_for_export = fn(records: Array[TelemetryRecord], aggregation_type: String) {
    let mut aggregated = []
    
    // Group by metric name
    let mut metric_groups = []
    
    for record in records {
      let mut found_group = false
      let mut updated_groups = []
      
      for group in metric_groups {
        if group.metric_name == record.metric_name {
          found_group = true
          updated_groups = updated_groups.push({
            metric_name: group.metric_name,
            records: group.records.push(record)
          })
        } else {
          updated_groups = updated_groups.push(group)
        }
      }
      
      if not(found_group) {
        metric_groups = metric_groups.push({
          metric_name: record.metric_name,
          records: [record]
        })
      }
    }
    
    // Apply aggregation
    for group in metric_groups {
      let aggregated_value = match aggregation_type {
        "avg" => {
          let sum = group.records.reduce(fn(acc, r) { acc + r.value }, 0.0)
          sum / group.records.length().to_float()
        }
        "max" => {
          group.records.reduce(fn(acc, r) { if r.value > acc { r.value } else { acc } }, group.records[0].value)
        }
        "min" => {
          group.records.reduce(fn(acc, r) { if r.value < acc { r.value } else { acc } }, group.records[0].value)
        }
        "sum" => {
          group.records.reduce(fn(acc, r) { acc + r.value }, 0.0)
        }
        _ => 0.0
      }
      
      aggregated = aggregated.push({
        timestamp: 1640995500,  // Aggregation timestamp
        metric_name: group.metric_name + "_" + aggregation_type,
        value: aggregated_value,
        tags: [("aggregated", "true")]
      })
    }
    
    aggregated
  }
  
  let aggregated_records = aggregate_for_export(telemetry_records, "avg")
  assert_eq(aggregated_records.length(), 4)  // 4 unique metric names
  
  let aggregated_json_export = export_to_json(aggregated_records)
  assert_true(aggregated_json_export.contains("cpu_usage_avg"))
  assert_true(aggregated_json_export.contains("memory_usage_avg"))
}