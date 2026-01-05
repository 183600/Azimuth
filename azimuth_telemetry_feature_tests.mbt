// Azimuth Telemetry Feature Test Suite
// This file contains advanced telemetry test cases focusing on distributed tracing, metrics, and monitoring

// Test 1: Distributed Tracing
test "distributed tracing functionality" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)]
  }
  
  // Create a trace context
  let create_trace_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      parent_span_id: None,
      baggage: []
    }
  }
  
  // Create child span
  let create_child_span = fn(parent: TraceContext, child_span_id: String) {
    {
      trace_id: parent.trace_id,
      span_id: child_span_id,
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage
    }
  }
  
  // Add baggage item
  let add_baggage = fn(context: TraceContext, key: String, value: String) {
    { context | baggage: context.baggage.push((key, value)) }
  }
  
  // Test trace context creation
  let root_context = create_trace_context("trace-12345", "span-abcde")
  assert_eq(root_context.trace_id, "trace-12345")
  assert_eq(root_context.span_id, "span-abcde")
  assert_eq(root_context.parent_span_id, None)
  assert_eq(root_context.baggage.length(), 0)
  
  // Test child span creation
  let child_context = create_child_span(root_context, "span-fghij")
  assert_eq(child_context.trace_id, "trace-12345")
  assert_eq(child_context.span_id, "span-fghij")
  assert_eq(child_context.parent_span_id, Some("span-abcde"))
  
  // Test baggage propagation
  let with_baggage = add_baggage(root_context, "user.id", "user-123")
  assert_eq(with_baggage.baggage.length(), 1)
  assert_eq(with_baggage.baggage[0], ("user.id", "user-123"))
  
  // Test baggage propagation to child
  let child_with_baggage = add_baggage(with_baggage, "service.name", "payment-service")
  assert_eq(child_with_baggage.baggage.length(), 2)
  assert_true(child_with_baggage.baggage.contains(("user.id", "user-123")))
  assert_true(child_with_baggage.baggage.contains(("service.name", "payment-service")))
}

// Test 2: Performance Metrics Collection
test "performance metrics collection" {
  // Define metric types
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // Define metric data structure
  type Metric = {
    name: String,
    metric_type: MetricType,
    value: Float,
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // Define metrics collector
  type MetricsCollector = {
    metrics: Array[Metric],
    start_time: Int
  }
  
  // Create a metrics collector
  let create_collector = fn() {
    {
      metrics: [],
      start_time: 1640995200
    }
  }
  
  // Record a metric
  let record_metric = fn(collector: MetricsCollector, name: String, metric_type: MetricType, value: Float, labels: Array[(String, String)]) {
    let metric = {
      name,
      metric_type,
      value,
      labels,
      timestamp: 1640995200
    }
    { collector | metrics: collector.metrics.push(metric) }
  }
  
  // Get metrics by name
  let get_metrics_by_name = fn(collector: MetricsCollector, name: String) {
    collector.metrics.filter(fn(m) { m.name == name })
  }
  
  // Calculate metric statistics
  let calculate_stats = fn(metrics: Array[Metric]) {
    if metrics.length() == 0 {
      { min: 0.0, max: 0.0, avg: 0.0, count: 0 }
    } else {
      let values = metrics.map(fn(m) { m.value })
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
      let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
      
      {
        min,
        max,
        avg: sum / values.length().to_float(),
        count: values.length()
      }
    }
  }
  
  // Test metrics collection
  let collector = create_collector()
  assert_eq(collector.metrics.length(), 0)
  
  // Record some metrics
  let with_counter = record_metric(collector, "request_count", MetricType::Counter, 100.0, [("method", "GET"), ("/api/users", "endpoint")])
  let with_gauge = record_metric(with_counter, "active_connections", MetricType::Gauge, 25.0, [("service", "api")])
  let with_histogram = record_metric(with_gauge, "response_time", MetricType::Histogram, 150.5, [("status", "200")])
  
  assert_eq(with_histogram.metrics.length(), 3)
  
  // Test metric retrieval
  let request_metrics = get_metrics_by_name(with_histogram, "request_count")
  assert_eq(request_metrics.length(), 1)
  assert_eq(request_metrics[0].name, "request_count")
  assert_eq(request_metrics[0].value, 100.0)
  
  // Test histogram metrics with multiple values
  let with_multiple_histograms = record_metric(with_histogram, "response_time", MetricType::Histogram, 200.0, [("status", "200")])
  let more_histograms = record_metric(with_multiple_histograms, "response_time", MetricType::Histogram, 75.5, [("status", "200")])
  
  let response_time_metrics = get_metrics_by_name(more_histograms, "response_time")
  assert_eq(response_time_metrics.length(), 3)
  
  // Test statistics calculation
  let stats = calculate_stats(response_time_metrics)
  assert_eq(stats.min, 75.5)
  assert_eq(stats.max, 200.0)
  assert_eq(stats.count, 3)
  assert_eq(stats.avg, (150.5 + 200.0 + 75.5) / 3.0)
}

// Test 3: Log Aggregation
test "log aggregation functionality" {
  // Define log levels
  enum LogLevel {
    Debug
    Info
    Warning
    Error
    Critical
  }
  
  // Define log entry structure
  type LogEntry = {
    timestamp: Int,
    level: LogLevel,
    message: String,
    service: String,
    trace_id: Option[String],
    attributes: Array[(String, String)]
  }
  
  // Define log aggregator
  type LogAggregator = {
    entries: Array[LogEntry],
    services: Array[String],
    error_count: Int
  }
  
  // Create a log aggregator
  let create_aggregator = fn() {
    {
      entries: [],
      services: [],
      error_count: 0
    }
  }
  
  // Add a log entry
  let add_log = fn(aggregator: LogAggregator, timestamp: Int, level: LogLevel, message: String, service: String, trace_id: Option[String], attributes: Array[(String, String)]) {
    let entry = {
      timestamp,
      level,
      message,
      service,
      trace_id,
      attributes
    }
    
    let new_services = if aggregator.services.contains(service) {
      aggregator.services
    } else {
      aggregator.services.push(service)
    }
    
    let new_error_count = match level {
      LogLevel::Error or LogLevel::Critical => aggregator.error_count + 1
      _ => aggregator.error_count
    }
    
    {
      entries: aggregator.entries.push(entry),
      services: new_services,
      error_count: new_error_count
    }
  }
  
  // Filter logs by level
  let filter_by_level = fn(aggregator: LogAggregator, level: LogLevel) {
    aggregator.entries.filter(fn(entry) {
      match entry.level {
        l => l == level
      }
    })
  }
  
  // Filter logs by service
  let filter_by_service = fn(aggregator: LogAggregator, service: String) {
    aggregator.entries.filter(fn(entry) { entry.service == service })
  }
  
  // Filter logs by time range
  let filter_by_time_range = fn(aggregator: LogAggregator, start: Int, end: Int) {
    aggregator.entries.filter(fn(entry) { entry.timestamp >= start and entry.timestamp <= end })
  }
  
  // Test log aggregation
  let aggregator = create_aggregator()
  assert_eq(aggregator.entries.length(), 0)
  assert_eq(aggregator.services.length(), 0)
  assert_eq(aggregator.error_count, 0)
  
  // Add some log entries
  let with_logs = add_log(aggregator, 1640995200, LogLevel::Info, "Service started", "auth-service", Some("trace-123"), [])
  let more_logs = add_log(with_logs, 1640995210, LogLevel::Warning, "High memory usage", "auth-service", Some("trace-123"), [("memory_usage", "85%")])
  let error_logs = add_log(more_logs, 1640995220, LogLevel::Error, "Database connection failed", "db-service", Some("trace-456"), [("error_code", "CONN_FAILED")])
  let critical_logs = add_log(error_logs, 1640995230, LogLevel::Critical, "Service unavailable", "api-gateway", Some("trace-456"), [])
  
  assert_eq(critical_logs.entries.length(), 4)
  assert_eq(critical_logs.services.length(), 3)
  assert_eq(critical_logs.error_count, 2)  // Error and Critical
  
  // Test filtering by level
  let error_entries = filter_by_level(critical_logs, LogLevel::Error)
  assert_eq(error_entries.length(), 1)
  assert_eq(error_entries[0].message, "Database connection failed")
  
  let critical_entries = filter_by_level(critical_logs, LogLevel::Critical)
  assert_eq(critical_entries.length(), 1)
  assert_eq(critical_entries[0].service, "api-gateway")
  
  // Test filtering by service
  let auth_logs = filter_by_service(critical_logs, "auth-service")
  assert_eq(auth_logs.length(), 2)
  
  let db_logs = filter_by_service(critical_logs, "db-service")
  assert_eq(db_logs.length(), 1)
  
  // Test filtering by time range
  let recent_logs = filter_by_time_range(critical_logs, 1640995215, 1640995230)
  assert_eq(recent_logs.length(), 2)  // Error and Critical logs
}

// Test 4: Sampling Strategies
test "sampling strategies functionality" {
  // Define sampling decision
  enum SamplingDecision {
    Drop
    Record
    RecordAndSample
  }
  
  // Define sampler types
  enum SamplerType {
    AlwaysOn
    AlwaysOff
    Probabilistic(Float)
    RateLimiting(Int)  // Max samples per second
  }
  
  // Define sampler configuration
  type Sampler = {
    sampler_type: SamplerType,
    samples_taken: Int,
    samples_dropped: Int
  }
  
  // Create a sampler
  let create_sampler = fn(sampler_type: SamplerType) {
    {
      sampler_type,
      samples_taken: 0,
      samples_dropped: 0
    }
  }
  
  // Make sampling decision
  let should_sample = fn(sampler: Sampler, trace_id: String) {
    match sampler.sampler_type {
      SamplerType::AlwaysOn => (SamplingDecision::RecordAndSample, { sampler | samples_taken: sampler.samples_taken + 1 })
      SamplerType::AlwaysOff => (SamplingDecision::Drop, { sampler | samples_dropped: sampler.samples_dropped + 1 })
      SamplerType::Probabilistic(probability) => {
        // Simple hash-based sampling
        let hash = trace_id.length() % 100
        if hash < (probability * 100.0).to_int() {
          (SamplingDecision::RecordAndSample, { sampler | samples_taken: sampler.samples_taken + 1 })
        } else {
          (SamplingDecision::Drop, { sampler | samples_dropped: sampler.samples_dropped + 1 })
        }
      }
      SamplerType::RateLimiting(max_per_second) => {
        if sampler.samples_taken < max_per_second {
          (SamplingDecision::RecordAndSample, { sampler | samples_taken: sampler.samples_taken + 1 })
        } else {
          (SamplingDecision::Drop, { sampler | samples_dropped: sampler.samples_dropped + 1 })
        }
      }
    }
  }
  
  // Test always-on sampler
  let always_on_sampler = create_sampler(SamplerType::AlwaysOn)
  let (decision1, sampler1) = should_sample(always_on_sampler, "trace-123")
  assert_eq(decision1, SamplingDecision::RecordAndSample)
  assert_eq(sampler1.samples_taken, 1)
  assert_eq(sampler1.samples_dropped, 0)
  
  // Test always-off sampler
  let always_off_sampler = create_sampler(SamplerType::AlwaysOff)
  let (decision2, sampler2) = should_sample(always_off_sampler, "trace-456")
  assert_eq(decision2, SamplingDecision::Drop)
  assert_eq(sampler2.samples_taken, 0)
  assert_eq(sampler2.samples_dropped, 1)
  
  // Test probabilistic sampler
  let prob_sampler = create_sampler(SamplerType::Probabilistic(0.5))  // 50% sampling
  let (decision3, sampler3) = should_sample(prob_sampler, "short")  // Short trace ID, more likely to be sampled
  let (decision4, sampler4) = should_sample(sampler3, "very-long-trace-id")  // Long trace ID, less likely to be sampled
  
  // Check that some traces are sampled and some are dropped
  let total_samples = sampler4.samples_taken
  let total_dropped = sampler4.samples_dropped
  assert_eq(total_samples + total_dropped, 2)
  
  // Test rate limiting sampler
  let rate_limit_sampler = create_sampler(SamplerType::RateLimiting(2))  // Max 2 samples
  let (decision5, sampler5) = should_sample(rate_limit_sampler, "trace-1")
  let (decision6, sampler6) = should_sample(sampler5, "trace-2")
  let (decision7, sampler7) = should_sample(sampler6, "trace-3")  // This should be dropped
  
  assert_eq(decision5, SamplingDecision::RecordAndSample)
  assert_eq(decision6, SamplingDecision::RecordAndSample)
  assert_eq(decision7, SamplingDecision::Drop)
  assert_eq(sampler7.samples_taken, 2)
  assert_eq(sampler7.samples_dropped, 1)
}

// Test 5: Data Serialization
test "data serialization functionality" {
  // Define serialization format
  enum SerializationFormat {
    Json
    Protobuf
    Xml
    Csv
  }
  
  // Define serialized data
  type SerializedData = {
    format: SerializationFormat,
    data: String,
    size: Int
  }
  
  // Define telemetry data to serialize
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    start_time: Int,
    duration: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // Serialize telemetry data to JSON (simplified)
  let serialize_to_json = fn(data: TelemetryData) {
    let tags_string = data.tags.map(fn(t) { "\"" + t.0 + "\":\"" + t.1 + "\"" }).join(",")
    
    "{"
      + "\"trace_id\":\"" + data.trace_id + "\","
      + "\"span_id\":\"" + data.span_id + "\","
      + "\"service_name\":\"" + data.service_name + "\","
      + "\"operation_name\":\"" + data.operation_name + "\","
      + "\"start_time\":" + data.start_time.to_string() + ","
      + "\"duration\":" + data.duration.to_string() + ","
      + "\"status\":\"" + data.status + "\","
      + "\"tags\":{" + tags_string + "}"
      + "}"
  }
  
  // Serialize telemetry data to CSV (simplified)
  let serialize_to_csv = fn(data: TelemetryData) {
    let tags_string = data.tags.map(fn(t) { t.0 + "=" + t.1 }).join(";")
    
    data.trace_id + ","
    + data.span_id + ","
    + data.service_name + ","
    + data.operation_name + ","
    + data.start_time.to_string() + ","
    + data.duration.to_string() + ","
    + data.status + ","
    + tags_string
  }
  
  // Serialize telemetry data
  let serialize = fn(data: TelemetryData, format: SerializationFormat) {
    match format {
      SerializationFormat::Json => {
        let json_string = serialize_to_json(data)
        {
          format,
          data: json_string,
          size: json_string.length()
        }
      }
      SerializationFormat::Csv => {
        let csv_string = serialize_to_csv(data)
        {
          format,
          data: csv_string,
          size: csv_string.length()
        }
      }
      _ => {
        // Placeholder for other formats
        {
          format,
          data: "placeholder",
          size: 9
        }
      }
    }
  }
  
  // Create test telemetry data
  let telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-abcde",
    service_name: "payment-service",
    operation_name: "process_payment",
    start_time: 1640995200,
    duration: 250,
    status: "success",
    tags: [
      ("user.id", "user-123"),
      ("payment.method", "credit_card"),
      ("amount", "99.99")
    ]
  }
  
  // Test JSON serialization
  let json_result = serialize(telemetry_data, SerializationFormat::Json)
  assert_eq(json_result.format, SerializationFormat::Json)
  assert_true(json_result.data.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json_result.data.contains("\"service_name\":\"payment-service\""))
  assert_true(json_result.data.contains("\"tags\":{"))
  assert_true(json_result.size > 0)
  
  // Test CSV serialization
  let csv_result = serialize(telemetry_data, SerializationFormat::Csv)
  assert_eq(csv_result.format, SerializationFormat::Csv)
  assert_true(csv_result.data.contains("trace-12345"))
  assert_true(csv_result.data.contains("payment-service"))
  assert_true(csv_result.data.contains("user.id=user-123"))
  assert_true(csv_result.size > 0)
  
  // Compare sizes (CSV should be smaller than JSON for this data)
  assert_true(csv_result.size < json_result.size)
  
  // Test serialization with different tag counts
  let minimal_data = {
    trace_id: "trace-min",
    span_id: "span-min",
    service_name: "minimal-service",
    operation_name: "minimal_op",
    start_time: 1640995200,
    duration: 10,
    status: "ok",
    tags: []
  }
  
  let minimal_json = serialize(minimal_data, SerializationFormat::Json)
  let minimal_csv = serialize(minimal_data, SerializationFormat::Csv)
  
  assert_true(minimal_json.size < json_result.size)  // Less data
  assert_true(minimal_csv.size < csv_result.size)    // Less data
}

// Test 6: Context Propagation
test "context propagation functionality" {
  // Define context entry
  type ContextEntry = {
    key: String,
    value: String
  }
  
  // Define propagation context
  type PropagationContext = {
    trace_id: String,
    span_id: String,
    entries: Array[ContextEntry]
  }
  
  // Define context carrier
  type ContextCarrier = {
    headers: Array[(String, String)],
    metadata: Array[(String, String)]
  }
  
  // Extract context from carrier
  let extract_context = fn(carrier: ContextCarrier) {
    let mut trace_id = ""
    let mut span_id = ""
    let mut entries = []
    
    for (key, value) in carrier.headers {
      match key {
        "x-trace-id" => trace_id = value
        "x-span-id" => span_id = value
        "x-baggage" => {
          // Parse baggage header: key1=value1,key2=value2
          let pairs = value.split(",")
          for pair in pairs {
            let kv = pair.split("=")
            if kv.length() == 2 {
              entries = entries.push({ key: kv[0], value: kv[1] })
            }
          }
        }
        _ => ()  // Ignore other headers
      }
    }
    
    if trace_id != "" and span_id != "" {
      Some({
        trace_id,
        span_id,
        entries
      })
    } else {
      None
    }
  }
  
  // Inject context into carrier
  let inject_context = fn(context: PropagationContext, carrier: ContextCarrier) {
    let baggage_string = context.entries
      .map(fn(entry) { entry.key + "=" + entry.value })
      .join(",")
    
    let new_headers = carrier.headers
      .push(("x-trace-id", context.trace_id))
      .push(("x-span-id", context.span_id))
      .push(("x-baggage", baggage_string))
    
    { carrier | headers: new_headers }
  }
  
  // Create a context carrier
  let create_carrier = fn() {
    {
      headers: [
        ("host", "api.example.com"),
        ("user-agent", "telemetry-client/1.0")
      ],
      metadata: []
    }
  }
  
  // Test context injection
  let context = {
    trace_id: "trace-12345",
    span_id: "span-abcde",
    entries: [
      { key: "user.id", value: "user-123" },
      { key: "service.name", value: "payment-service" }
    ]
  }
  
  let carrier = create_carrier()
  let injected_carrier = inject_context(context, carrier)
  
  // Verify injected headers
  let trace_header = injected_carrier.headers.filter(fn(h) { h.0 == "x-trace-id" })
  let span_header = injected_carrier.headers.filter(fn(h) { h.0 == "x-span-id" })
  let baggage_header = injected_carrier.headers.filter(fn(h) { h.0 == "x-baggage" })
  
  assert_eq(trace_header.length(), 1)
  assert_eq(trace_header[0].1, "trace-12345")
  
  assert_eq(span_header.length(), 1)
  assert_eq(span_header[0].1, "span-abcde")
  
  assert_eq(baggage_header.length(), 1)
  assert_true(baggage_header[0].1.contains("user.id=user-123"))
  assert_true(baggage_header[0].1.contains("service.name=payment-service"))
  
  // Test context extraction
  let extracted_context = extract_context(injected_carrier)
  
  match extracted_context {
    Some(ctx) => {
      assert_eq(ctx.trace_id, "trace-12345")
      assert_eq(ctx.span_id, "span-abcde")
      assert_eq(ctx.entries.length(), 2)
      
      let user_entry = ctx.entries.filter(fn(e) { e.key == "user.id" })
      assert_eq(user_entry.length(), 1)
      assert_eq(user_entry[0].value, "user-123")
      
      let service_entry = ctx.entries.filter(fn(e) { e.key == "service.name" })
      assert_eq(service_entry.length(), 1)
      assert_eq(service_entry[0].value, "payment-service")
    }
    None => assert_true(false)  // Should have extracted context
  }
  
  // Test extraction with missing headers
  let incomplete_carrier = {
    headers: [
      ("x-trace-id", "trace-12345"),
      // Missing x-span-id
      ("x-baggage", "key=value")
    ],
    metadata: []
  }
  
  let incomplete_context = extract_context(incomplete_carrier)
  assert_eq(incomplete_context, None)
  
  // Test extraction with empty carrier
  let empty_carrier = { headers: [], metadata: [] }
  let empty_context = extract_context(empty_carrier)
  assert_eq(empty_context, None)
}

// Test 7: Resource Monitoring
test "resource monitoring functionality" {
  // Define resource types
  enum ResourceType {
    Cpu
    Memory
    Disk
    Network
  }
  
  // Define resource metric
  type ResourceMetric = {
    resource_type: ResourceType,
    name: String,
    value: Float,
    unit: String,
    timestamp: Int
  }
  
  // Define resource monitor
  type ResourceMonitor = {
    metrics: Array[ResourceMetric],
    alerts: Array[String],
    thresholds: Array[(ResourceType, Float)]
  }
  
  // Create a resource monitor
  let create_monitor = fn() {
    {
      metrics: [],
      alerts: [],
      thresholds: [
        (ResourceType::Cpu, 80.0),      // Alert if CPU > 80%
        (ResourceType::Memory, 90.0),   // Alert if Memory > 90%
        (ResourceType::Disk, 85.0),     // Alert if Disk > 85%
        (ResourceType::Network, 1000.0) // Alert if Network > 1000 Mbps
      ]
    }
  }
  
  // Record a resource metric
  let record_metric = fn(monitor: ResourceMonitor, resource_type: ResourceType, name: String, value: Float, unit: String) {
    let metric = {
      resource_type,
      name,
      value,
      unit,
      timestamp: 1640995200
    }
    
    let new_metrics = monitor.metrics.push(metric)
    
    // Check for alerts
    let threshold = monitor.thresholds.filter(fn(t) { t.0 == resource_type })
    let new_alerts = if threshold.length() > 0 and value > threshold[0].1 {
      monitor.alerts.push("ALERT: " + name + " " + value.to_string() + unit + " exceeds threshold " + threshold[0].1.to_string() + unit)
    } else {
      monitor.alerts
    }
    
    {
      metrics: new_metrics,
      alerts: new_alerts,
      thresholds: monitor.thresholds
    }
  }
  
  // Get metrics by resource type
  let get_metrics_by_type = fn(monitor: ResourceMonitor, resource_type: ResourceType) {
    monitor.metrics.filter(fn(m) { m.resource_type == resource_type })
  }
  
  // Calculate average resource usage
  let calculate_average_usage = fn(monitor: ResourceMonitor, resource_type: ResourceType) {
    let type_metrics = get_metrics_by_type(monitor, resource_type)
    if type_metrics.length() == 0 {
      0.0
    } else {
      let sum = type_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
      sum / type_metrics.length().to_float()
    }
  }
  
  // Test resource monitoring
  let monitor = create_monitor()
  assert_eq(monitor.metrics.length(), 0)
  assert_eq(monitor.alerts.length(), 0)
  
  // Record some metrics
  let with_cpu = record_metric(monitor, ResourceType::Cpu, "cpu_usage", 45.5, "%")
  let with_memory = record_metric(with_cpu, ResourceType::Memory, "memory_usage", 60.2, "%")
  let with_disk = record_metric(with_memory, ResourceType::Disk, "disk_usage", 92.5, "%")  // Should trigger alert
  let with_network = record_metric(with_disk, ResourceType::Network, "network_throughput", 850.0, "Mbps")
  
  assert_eq(with_network.metrics.length(), 4)
  assert_eq(with_network.alerts.length(), 1)  // Disk usage alert
  assert_true(with_network.alerts[0].contains("disk_usage"))
  assert_true(with_network.alerts[0].contains("92.5%"))
  
  // Test metrics retrieval by type
  let cpu_metrics = get_metrics_by_type(with_network, ResourceType::Cpu)
  assert_eq(cpu_metrics.length(), 1)
  assert_eq(cpu_metrics[0].name, "cpu_usage")
  assert_eq(cpu_metrics[0].value, 45.5)
  
  let memory_metrics = get_metrics_by_type(with_network, ResourceType::Memory)
  assert_eq(memory_metrics.length(), 1)
  assert_eq(memory_metrics[0].name, "memory_usage")
  assert_eq(memory_metrics[0].value, 60.2)
  
  // Test average usage calculation
  let avg_cpu = calculate_average_usage(with_network, ResourceType::Cpu)
  assert_eq(avg_cpu, 45.5)
  
  let avg_memory = calculate_average_usage(with_network, ResourceType::Memory)
  assert_eq(avg_memory, 60.2)
  
  // Record multiple CPU metrics and test average
  let with_more_cpu = record_metric(with_network, ResourceType::Cpu, "cpu_usage", 75.0, "%")
  let even_more_cpu = record_metric(with_more_cpu, ResourceType::Cpu, "cpu_usage", 55.0, "%")
  
  let avg_cpu_multiple = calculate_average_usage(even_more_cpu, ResourceType::Cpu)
  assert_eq(avg_cpu_multiple, (45.5 + 75.0 + 55.0) / 3.0)
  
  // Test threshold alert for CPU
  let with_cpu_alert = record_metric(even_more_cpu, ResourceType::Cpu, "cpu_usage", 85.0, "%")  // Should trigger alert
  assert_eq(with_cpu_alert.alerts.length(), 2)  // Disk and CPU alerts
  assert_true(with_cpu_alert.alerts[1].contains("cpu_usage"))
  assert_true(with_cpu_alert.alerts[1].contains("85.0%"))
}

// Test 8: Exception Handling
test "exception handling functionality" {
  // Define exception types
  enum ExceptionType {
    NetworkError
    DatabaseError
    ValidationError
    TimeoutError
    SystemError
  }
  
  // Define exception details
  type Exception = {
    exception_type: ExceptionType,
    message: String,
    stack_trace: Array[String],
    timestamp: Int,
    context: Array[(String, String)]
  }
  
  // Define exception handler
  type ExceptionHandler = {
    exceptions: Array[Exception],
    handled_count: Int,
    retried_count: Int
  }
  
  // Create an exception handler
  let create_handler = fn() {
    {
      exceptions: [],
      handled_count: 0,
      retried_count: 0
    }
  }
  
  // Handle an exception
  let handle_exception = fn(handler: ExceptionHandler, exception: Exception, retry: Bool) {
    let new_exceptions = handler.exceptions.push(exception)
    let new_handled_count = handler.handled_count + 1
    let new_retried_count = if retry { handler.retried_count + 1 } else { handler.retried_count }
    
    {
      exceptions: new_exceptions,
      handled_count: new_handled_count,
      retried_count: new_retried_count
    }
  }
  
  // Group exceptions by type
  let group_by_type = fn(handler: ExceptionHandler) {
    let mut result = []
    for exception in handler.exceptions {
      let existing = result.filter(fn(g) { g.0 == exception.exception_type })
      if existing.length() > 0 {
        // Update existing group
        result = result.map(fn(g) {
          if g.0 == exception.exception_type {
            (g.0, g.1 + 1)
          } else {
            g
          }
        })
      } else {
        // Add new group
        result = result.push((exception.exception_type, 1))
      }
    }
    result
  }
  
  // Find exceptions by time range
  let find_by_time_range = fn(handler: ExceptionHandler, start: Int, end: Int) {
    handler.exceptions.filter(fn(e) { e.timestamp >= start and e.timestamp <= end })
  }
  
  // Test exception handling
  let handler = create_handler()
  assert_eq(handler.exceptions.length(), 0)
  assert_eq(handler.handled_count, 0)
  assert_eq(handler.retried_count, 0)
  
  // Create some exceptions
  let network_exception = {
    exception_type: ExceptionType::NetworkError,
    message: "Connection timeout",
    stack_trace: [
      "at connect_to_server()",
      "at execute_request()",
      "at main()"
    ],
    timestamp: 1640995200,
    context: [
      ("server", "api.example.com"),
      ("port", "443"),
      ("timeout", "5000")
    ]
  }
  
  let db_exception = {
    exception_type: ExceptionType::DatabaseError,
    message: "Query execution failed",
    stack_trace: [
      "at execute_query()",
      "at get_user_data()",
      "at process_request()"
    ],
    timestamp: 1640995210,
    context: [
      ("query", "SELECT * FROM users"),
      ("error_code", "SQL_ERROR")
    ]
  }
  
  let validation_exception = {
    exception_type: ExceptionType::ValidationError,
    message: "Invalid input parameters",
    stack_trace: [
      "at validate_input()",
      "at create_user()",
      "at handle_request()"
    ],
    timestamp: 1640995220,
    context: [
      ("field", "email"),
      ("value", "invalid-email")
    ]
  }
  
  // Handle exceptions
  let with_network = handle_exception(handler, network_exception, true)  // Retry network error
  let with_db = handle_exception(with_network, db_exception, false)     // Don't retry DB error
  let with_validation = handle_exception(with_db, validation_exception, false)
  
  assert_eq(with_validation.exceptions.length(), 3)
  assert_eq(with_validation.handled_count, 3)
  assert_eq(with_validation.retried_count, 1)  // Only network error was retried
  
  // Test grouping by type
  let grouped = group_by_type(with_validation)
  assert_eq(grouped.length(), 3)
  
  let network_group = grouped.filter(fn(g) { match g.0 { ExceptionType::NetworkError => true, _ => false } })
  assert_eq(network_group.length(), 1)
  assert_eq(network_group[0].1, 1)
  
  let db_group = grouped.filter(fn(g) { match g.0 { ExceptionType::DatabaseError => true, _ => false } })
  assert_eq(db_group.length(), 1)
  assert_eq(db_group[0].1, 1)
  
  let validation_group = grouped.filter(fn(g) { match g.0 { ExceptionType::ValidationError => true, _ => false } })
  assert_eq(validation_group.length(), 1)
  assert_eq(validation_group[0].1, 1)
  
  // Test finding exceptions by time range
  let recent_exceptions = find_by_time_range(with_validation, 1640995215, 1640995225)
  assert_eq(recent_exceptions.length(), 2)  // DB and Validation exceptions
  
  let early_exceptions = find_by_time_range(with_validation, 1640995195, 1640995205)
  assert_eq(early_exceptions.length(), 1)  // Network exception only
  
  // Test exception context
  let network_context = network_exception.context
  assert_eq(network_context.length(), 3)
  assert_true(network_context.contains(("server", "api.example.com")))
  assert_true(network_context.contains(("port", "443")))
  assert_true(network_context.contains(("timeout", "5000")))
  
  // Test stack trace
  let network_stack = network_exception.stack_trace
  assert_eq(network_stack.length(), 3)
  assert_eq(network_stack[0], "at connect_to_server()")
  assert_eq(network_stack[2], "at main()")
}

// Test 9: Data Export
test "data export functionality" {
  // Define export format
  enum ExportFormat {
    Json
    Csv
    Parquet
    Avro
  }
  
  // Define export destination
  enum ExportDestination {
    LocalFile(String)
    S3Bucket(String, String)  // Bucket, Key
    Database(String)          // Table name
    ApiEndpoint(String)       // URL
  }
  
  // Define export job
  type ExportJob = {
    job_id: String,
    format: ExportFormat,
    destination: ExportDestination,
    status: String,
    records_count: Int,
    file_size: Int,
    created_at: Int,
    completed_at: Option[Int]
  }
  
  // Define export manager
  type ExportManager = {
    jobs: Array[ExportJob],
    next_job_id: Int
  }
  
  // Create an export manager
  let create_manager = fn() {
    {
      jobs: [],
      next_job_id: 1
    }
  }
  
  // Create an export job
  let create_job = fn(manager: ExportManager, format: ExportFormat, destination: ExportDestination) {
    let job_id = "job-" + manager.next_job_id.to_string()
    let job = {
      job_id,
      format,
      destination,
      status: "pending",
      records_count: 0,
      file_size: 0,
      created_at: 1640995200,
      completed_at: None
    }
    
    {
      jobs: manager.jobs.push(job),
      next_job_id: manager.next_job_id + 1
    }
  }
  
  // Update job status
  let update_job_status = fn(manager: ExportManager, job_id: String, status: String, records_count: Int, file_size: Int) {
    let updated_jobs = manager.jobs.map(fn(job) {
      if job.job_id == job_id {
        {
          job |
          status: status,
          records_count: records_count,
          file_size: file_size,
          completed_at: if status == "completed" { Some(1640995300) } else { job.completed_at }
        }
      } else {
        job
      }
    })
    
    { manager | jobs: updated_jobs }
  }
  
  // Get jobs by status
  let get_jobs_by_status = fn(manager: ExportManager, status: String) {
    manager.jobs.filter(fn(job) { job.status == status })
  }
  
  // Get jobs by format
  let get_jobs_by_format = fn(manager: ExportManager, format: ExportFormat) {
    manager.jobs.filter(fn(job) { match job.format { f => f == format } })
  }
  
  // Calculate export statistics
  let calculate_stats = fn(manager: ExportManager) {
    let total_jobs = manager.jobs.length()
    let completed_jobs = get_jobs_by_status(manager, "completed").length()
    let pending_jobs = get_jobs_by_status(manager, "pending").length()
    let total_records = manager.jobs.reduce(fn(acc, job) { acc + job.records_count }, 0)
    let total_size = manager.jobs.reduce(fn(acc, job) { acc + job.file_size }, 0)
    
    {
      total_jobs,
      completed_jobs,
      pending_jobs,
      total_records,
      total_size
    }
  }
  
  // Test export functionality
  let manager = create_manager()
  assert_eq(manager.jobs.length(), 0)
  assert_eq(manager.next_job_id, 1)
  
  // Create some export jobs
  let with_json_job = create_job(manager, ExportFormat::Json, ExportDestination::LocalFile("export.json"))
  let with_csv_job = create_job(with_json_job, ExportFormat::Csv, ExportDestination::S3Bucket("my-bucket", "export.csv"))
  let with_parquet_job = create_job(with_csv_job, ExportFormat::Parquet, ExportDestination::Database("metrics_table"))
  
  assert_eq(with_parquet_job.jobs.length(), 3)
  assert_eq(with_parquet_job.next_job_id, 4)
  
  // Update job statuses
  let updated_manager = update_job_status(with_parquet_job, "job-1", "completed", 1000, 2048)
  let more_updated = update_job_status(updated_manager, "job-2", "completed", 2000, 1536)
  let final_updated = update_job_status(more_updated, "job-3", "failed", 0, 0)
  
  // Test status filtering
  let completed_jobs = get_jobs_by_status(final_updated, "completed")
  assert_eq(completed_jobs.length(), 2)
  assert_true(completed_jobs.map(fn(j) { j.job_id }).contains("job-1"))
  assert_true(completed_jobs.map(fn(j) { j.job_id }).contains("job-2"))
  
  let failed_jobs = get_jobs_by_status(final_updated, "failed")
  assert_eq(failed_jobs.length(), 1)
  assert_eq(failed_jobs[0].job_id, "job-3")
  
  // Test format filtering
  let json_jobs = get_jobs_by_format(final_updated, ExportFormat::Json)
  assert_eq(json_jobs.length(), 1)
  assert_eq(json_jobs[0].job_id, "job-1")
  assert_eq(json_jobs[0].records_count, 1000)
  
  let csv_jobs = get_jobs_by_format(final_updated, ExportFormat::Csv)
  assert_eq(csv_jobs.length(), 1)
  assert_eq(csv_jobs[0].job_id, "job-2")
  assert_eq(csv_jobs[0].records_count, 2000)
  
  // Test statistics calculation
  let stats = calculate_stats(final_updated)
  assert_eq(stats.total_jobs, 3)
  assert_eq(stats.completed_jobs, 2)
  assert_eq(stats.pending_jobs, 0)
  assert_eq(stats.total_records, 3000)  // 1000 + 2000
  assert_eq(stats.total_size, 3584)     // 2048 + 1536
  
  // Test destination types
  let json_job = final_updated.jobs.filter(fn(j) { j.job_id == "job-1" })[0]
  match json_job.destination {
    ExportDestination::LocalFile(filename) => assert_eq(filename, "export.json")
    _ => assert_true(false)
  }
  
  let csv_job = final_updated.jobs.filter(fn(j) { j.job_id == "job-2" })[0]
  match csv_job.destination {
    ExportDestination::S3Bucket(bucket, key) => {
      assert_eq(bucket, "my-bucket")
      assert_eq(key, "export.csv")
    }
    _ => assert_true(false)
  }
  
  let parquet_job = final_updated.jobs.filter(fn(j) { j.job_id == "job-3" })[0]
  match parquet_job.destination {
    ExportDestination::Database(table) => assert_eq(table, "metrics_table")
    _ => assert_true(false)
  }
  
  // Test completion timestamps
  assert_eq(json_job.completed_at, Some(1640995300))
  assert_eq(csv_job.completed_at, Some(1640995300))
  assert_eq(parquet_job.completed_at, None)  // Failed job
}

// Test 10: Configuration Management
test "configuration management functionality" {
  // Define configuration value type
  enum ConfigValue {
    String(String)
    Int(Int)
    Float(Float)
    Bool(Bool)
    Array(Array[String])
  }
  
  // Define configuration entry
  type ConfigEntry = {
    key: String,
    value: ConfigValue,
    description: String,
    is_sensitive: Bool,
    last_modified: Int
  }
  
  // Define configuration manager
  type ConfigManager = {
    entries: Array[ConfigEntry],
    version: Int,
    last_updated: Int
  }
  
  // Create a configuration manager
  let create_manager = fn() {
    {
      entries: [],
      version: 1,
      last_updated: 1640995200
    }
  }
  
  // Add a configuration entry
  let add_entry = fn(manager: ConfigManager, key: String, value: ConfigValue, description: String, is_sensitive: Bool) {
    let entry = {
      key,
      value,
      description,
      is_sensitive,
      last_modified: 1640995200
    }
    
    // Check if key already exists
    let existing_index = manager.entries.index_of(fn(e) { e.key == key })
    let new_entries = if existing_index >= 0 {
      // Update existing entry
      manager.entries.map(fn(e) {
        if e.key == key {
          { e | value, last_modified: 1640995200 }
        } else {
          e
        }
      })
    } else {
      // Add new entry
      manager.entries.push(entry)
    }
    
    {
      entries: new_entries,
      version: manager.version + 1,
      last_updated: 1640995200
    }
  }
  
  // Get configuration value
  let get_value = fn(manager: ConfigManager, key: String) {
    let entry = manager.entries.filter(fn(e) { e.key == key })
    if entry.length() > 0 {
      Some(entry[0].value)
    } else {
      None
    }
  }
  
  // Get configuration entries by prefix
  let get_by_prefix = fn(manager: ConfigManager, prefix: String) {
    manager.entries.filter(fn(e) { e.key.starts_with(prefix) })
  }
  
  // Get sensitive configuration entries
  let get_sensitive_entries = fn(manager: ConfigManager) {
    manager.entries.filter(fn(e) { e.is_sensitive })
  }
  
  // Validate configuration
  let validate_config = fn(manager: ConfigManager) {
    let mut errors = []
    
    for entry in manager.entries {
      // Check required fields
      if entry.key == "" {
        errors = errors.push("Empty key found")
      }
      
      // Validate values based on key patterns
      if entry.key.ends_with(".port") {
        match entry.value {
          ConfigValue::Int(port) => {
            if port < 1 or port > 65535 {
              errors = errors.push("Invalid port value for " + entry.key + ": " + port.to_string())
            }
          }
          _ => errors = errors.push("Port value must be integer for " + entry.key)
        }
      }
      
      if entry.key.ends_with(".url") {
        match entry.value {
          ConfigValue::String(url) => {
            if not(url.starts_with("http://") or url.starts_with("https://")) {
              errors = errors.push("Invalid URL format for " + entry.key + ": " + url)
            }
          }
          _ => errors = errors.push("URL value must be string for " + entry.key)
        }
      }
    }
    
    errors
  }
  
  // Test configuration management
  let manager = create_manager()
  assert_eq(manager.entries.length(), 0)
  assert_eq(manager.version, 1)
  
  // Add some configuration entries
  let with_db_config = add_entry(manager, "database.port", ConfigValue::Int(5432), "Database port", false)
  let with_url_config = add_entry(with_db_config, "api.url", ConfigValue::String("https://api.example.com"), "API endpoint URL", false)
  let with_debug_config = add_entry(with_url_config, "debug.enabled", ConfigValue::Bool(true), "Enable debug mode", false)
  let with_api_key = add_entry(with_debug_config, "api.key", ConfigValue::String("secret-key-123"), "API authentication key", true)
  let with_array_config = add_entry(with_api_key, "services.list", ConfigValue::Array(["auth", "payment", "notification"]), "List of services", false)
  
  assert_eq(with_array_config.entries.length(), 5)
  assert_eq(with_array_config.version, 6)
  
  // Test getting values
  let db_port = get_value(with_array_config, "database.port")
  match db_port {
    Some(ConfigValue::Int(port)) => assert_eq(port, 5432)
    _ => assert_true(false)
  }
  
  let api_url = get_value(with_array_config, "api.url")
  match api_url {
    Some(ConfigValue::String(url)) => assert_eq(url, "https://api.example.com")
    _ => assert_true(false)
  }
  
  let debug_enabled = get_value(with_array_config, "debug.enabled")
  match debug_enabled {
    Some(ConfigValue::Bool(enabled)) => assert_true(enabled)
    _ => assert_true(false)
  }
  
  let api_key = get_value(with_array_config, "api.key")
  match api_key {
    Some(ConfigValue::String(key)) => assert_eq(key, "secret-key-123")
    _ => assert_true(false)
  }
  
  let services_list = get_value(with_array_config, "services.list")
  match services_list {
    Some(ConfigValue::Array(services)) => {
      assert_eq(services.length(), 3)
      assert_true(services.contains("auth"))
      assert_true(services.contains("payment"))
      assert_true(services.contains("notification"))
    }
    _ => assert_true(false)
  }
  
  // Test getting non-existent value
  let missing_value = get_value(with_array_config, "non.existent.key")
  assert_eq(missing_value, None)
  
  // Test getting entries by prefix
  let db_entries = get_by_prefix(with_array_config, "database.")
  assert_eq(db_entries.length(), 1)
  assert_eq(db_entries[0].key, "database.port")
  
  let api_entries = get_by_prefix(with_array_config, "api.")
  assert_eq(api_entries.length(), 2)
  assert_true(api_entries.map(fn(e) { e.key }).contains("api.url"))
  assert_true(api_entries.map(fn(e) { e.key }).contains("api.key"))
  
  // Test getting sensitive entries
  let sensitive_entries = get_sensitive_entries(with_array_config)
  assert_eq(sensitive_entries.length(), 1)
  assert_eq(sensitive_entries[0].key, "api.key")
  assert_true(sensitive_entries[0].is_sensitive)
  
  // Test configuration validation
  let errors = validate_config(with_array_config)
  assert_eq(errors.length(), 0)  // All configurations should be valid
  
  // Test with invalid configuration
  let with_invalid_port = add_entry(with_array_config, "service.port", ConfigValue::Int(70000), "Invalid service port", false)
  let with_invalid_url = add_entry(with_invalid_port, "webhook.url", ConfigValue::String("invalid-url"), "Invalid webhook URL", false)
  let with_invalid_port_type = add_entry(with_invalid_url, "cache.port", ConfigValue::String("not-a-number"), "Port as string", false)
  
  let validation_errors = validate_config(with_invalid_port_type)
  assert_eq(validation_errors.length(), 3)
  assert_true(validation_errors[0].contains("Invalid port value"))
  assert_true(validation_errors[1].contains("Invalid URL format"))
  assert_true(validation_errors[2].contains("Port value must be integer"))
  
  // Test updating existing configuration
  let updated_manager = add_entry(with_array_config, "database.port", ConfigValue::Int(3306), "Updated database port", false)
  assert_eq(updated_manager.entries.length(), 5)  // Still 5 entries, one updated
  assert_eq(updated_manager.version, 7)  // Version incremented
  
  let updated_port = get_value(updated_manager, "database.port")
  match updated_port {
    Some(ConfigValue::Int(port)) => assert_eq(port, 3306)
    _ => assert_true(false)
  }
}