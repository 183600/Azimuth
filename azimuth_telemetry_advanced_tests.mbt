// Azimuth Advanced Telemetry Test Suite
// This file contains advanced MoonBit test cases focusing on telemetry functionality

// Test 1: Trace Context Propagation
test "trace context propagation across service boundaries" {
  // Define trace context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    flags: Int
  }
  
  // Create a root trace context
  let root_context = {
    trace_id: "trace-12345",
    span_id: "span-100",
    parent_span_id: None,
    baggage: [("user.id", "user-789"), ("request.id", "req-456")],
    flags: 1
  }
  
  // Function to create child context
  let create_child_context = fn(parent: TraceContext, operation_name: String) {
    {
      trace_id: parent.trace_id,
      span_id: "span-" + operation_name.length().to_string(),
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage,
      flags: parent.flags
    }
  }
  
  // Create child contexts for different operations
  let db_context = create_child_context(root_context, "database_query")
  let cache_context = create_child_context(root_context, "cache_lookup")
  
  // Verify trace propagation
  assert_eq(db_context.trace_id, root_context.trace_id)
  assert_eq(cache_context.trace_id, root_context.trace_id)
  assert_eq(db_context.parent_span_id, Some(root_context.span_id))
  assert_eq(cache_context.parent_span_id, Some(root_context.span_id))
  
  // Verify baggage propagation
  assert_eq(db_context.baggage.length(), 2)
  assert_eq(cache_context.baggage.length(), 2)
  assert_true(db_context.baggage.contains(("user.id", "user-789")))
  assert_true(cache_context.baggage.contains(("request.id", "req-456")))
  
  // Function to add baggage item
  let add_baggage_item = fn(context: TraceContext, key: String, value: String) {
    { context | baggage: context.baggage.push((key, value)) }
  }
  
  // Add baggage to child context
  let db_with_baggage = add_baggage_item(db_context, "db.query", "SELECT * FROM users")
  assert_eq(db_with_baggage.baggage.length(), 3)
  assert_true(db_with_baggage.baggage.contains(("db.query", "SELECT * FROM users")))
  
  // Verify original context is unchanged
  assert_eq(db_context.baggage.length(), 2)
}

// Test 2: Metric Collection and Aggregation
test "metric collection and aggregation patterns" {
  // Define metric types
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // Define metric structure
  type Metric = {
    name: String,
    metric_type: MetricType,
    value: Float,
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // Define metric registry
  type MetricRegistry = {
    metrics: Array[Metric],
    aggregations: Array[(String, Float)]
  }
  
  // Create metric registry
  let create_registry = fn() {
    {
      metrics: [],
      aggregations: []
    }
  }
  
  // Function to record metric
  let record_metric = fn(registry: MetricRegistry, name: String, metric_type: MetricType, value: Float, labels: Array[(String, String)]) {
    let metric = {
      name,
      metric_type,
      value,
      labels,
      timestamp: 1640995200
    }
    { registry | metrics: registry.metrics.push(metric) }
  }
  
  // Initialize registry
  let mut registry = create_registry()
  
  // Record different types of metrics
  registry = record_metric(registry, "http_requests_total", MetricType::Counter, 1000.0, [("method", "GET"), ("/api/users", "path")])
  registry = record_metric(registry, "memory_usage_bytes", MetricType::Gauge, 524288000.0, [("service", "api")])
  registry = record_metric(registry, "request_duration_seconds", MetricType::Histogram, 0.125, [("le", "0.1")])
  registry = record_metric(registry, "response_size_bytes", MetricType::Summary, 1024.0, [("quantile", "0.5")])
  
  // Verify metrics are recorded
  assert_eq(registry.metrics.length(), 4)
  assert_eq(registry.metrics[0].name, "http_requests_total")
  assert_eq(registry.metrics[1].metric_type, MetricType::Gauge)
  assert_eq(registry.metrics[2].value, 0.125)
  assert_eq(registry.metrics[3].labels[0], ("quantile", "0.5"))
  
  // Function to aggregate metrics by name
  let aggregate_by_name = fn(registry: MetricRegistry) {
    let mut result = []
    let mut processed = []
    
    for metric in registry.metrics {
      if not(processed.contains(metric.name)) {
        let mut sum = 0.0
        let mut count = 0
        
        for m in registry.metrics {
          if m.name == metric.name {
            sum = sum + m.value
            count = count + 1
          }
        }
        
        result = result.push((metric.name, sum / count.to_float()))
        processed = processed.push(metric.name)
      }
    }
    
    result
  }
  
  // Test aggregation
  let aggregations = aggregate_by_name(registry)
  assert_eq(aggregations.length(), 4)
  
  // Find specific aggregation
  let find_aggregation = fn(aggs: Array[(String, Float)], name: String) {
    let mut found = None
    for (n, v) in aggs {
      if n == name {
        found = Some(v)
      }
    }
    found
  }
  
  let http_avg = find_aggregation(aggregations, "http_requests_total")
  assert_eq(http_avg, Some(1000.0))
  
  let memory_avg = find_aggregation(aggregations, "memory_usage_bytes")
  assert_eq(memory_avg, Some(524288000.0))
}

// Test 3: Sampling Strategies
test "telemetry sampling strategies" {
  // Define sampling decision types
  enum SamplingDecision {
    RecordAndSample
    RecordOnly
    Drop
  }
  
  // Define sampler types
  enum SamplerType {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)
    ParentBased
  }
  
  // Define sampler structure
  type Sampler = {
    sampler_type: SamplerType,
    description: String
  }
  
  // Function to make sampling decision
  let make_sampling_decision = fn(sampler: Sampler, trace_id: String, parent_decision: Option[SamplingDecision]) {
    match sampler.sampler_type {
      SamplerType::AlwaysOn => SamplingDecision::RecordAndSample
      SamplerType::AlwaysOff => SamplingDecision::Drop
      SamplerType::TraceIdRatio(ratio) => {
        // Simple hash-based sampling simulation
        let hash = trace_id.length() % 100
        if (hash.to_float() / 100.0) < ratio {
          SamplingDecision::RecordAndSample
        } else {
          SamplingDecision::RecordOnly
        }
      }
      SamplerType::ParentBased => {
        match parent_decision {
          Some(decision) => decision
          None => SamplingDecision::RecordAndSample
        }
      }
    }
  }
  
  // Create samplers
  let always_on_sampler = { sampler_type: SamplerType::AlwaysOn, description: "Always sample" }
  let always_off_sampler = { sampler_type: SamplerType::AlwaysOff, description: "Never sample" }
  let ratio_sampler = { sampler_type: SamplerType::TraceIdRatio(0.5), description: "50% sampling" }
  let parent_based_sampler = { sampler_type: SamplerType::ParentBased, description: "Parent-based sampling" }
  
  // Test always on sampler
  let decision1 = make_sampling_decision(always_on_sampler, "trace-123", None)
  assert_eq(decision1, SamplingDecision::RecordAndSample)
  
  // Test always off sampler
  let decision2 = make_sampling_decision(always_off_sampler, "trace-456", None)
  assert_eq(decision2, SamplingDecision::Drop)
  
  // Test ratio sampler
  let decision3 = make_sampling_decision(ratio_sampler, "short", None)  // length 5, 5% chance
  // This might be RecordOnly based on our simple hash
  
  let decision4 = make_sampling_decision(ratio_sampler, "very-long-trace-id", None)  // length 18, 18% chance
  // This might be RecordOnly based on our simple hash
  
  // Test parent-based sampler with parent
  let decision5 = make_sampling_decision(parent_based_sampler, "trace-789", Some(SamplingDecision::RecordOnly))
  assert_eq(decision5, SamplingDecision::RecordOnly)
  
  // Test parent-based sampler without parent
  let decision6 = make_sampling_decision(parent_based_sampler, "trace-000", None)
  assert_eq(decision6, SamplingDecision::RecordAndSample)
}

// Test 4: Span Event and Link Processing
test "span event and link processing" {
  // Define span event structure
  type SpanEvent = {
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  // Define span link structure
  type SpanLink = {
    trace_id: String,
    span_id: String,
    attributes: Array[(String, String)]
  }
  
  // Define span structure with events and links
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    events: Array[SpanEvent],
    links: Array[SpanLink],
    status: String
  }
  
  // Function to add event to span
  let add_event = fn(span: Span, event_name: String, attributes: Array[(String, String)]) {
    let event = {
      name: event_name,
      timestamp: 1640995200,
      attributes
    }
    { span | events: span.events.push(event) }
  }
  
  // Function to add link to span
  let add_link = fn(span: Span, linked_trace_id: String, linked_span_id: String, attributes: Array[(String, String)]) {
    let link = {
      trace_id: linked_trace_id,
      span_id: linked_span_id,
      attributes
    }
    { span | links: span.links.push(link) }
  }
  
  // Create initial span
  let initial_span = {
    name: "operation",
    trace_id: "trace-123",
    span_id: "span-456",
    events: [],
    links: [],
    status: "ok"
  }
  
  // Add events to span
  let span_with_events = initial_span
    |> add_event("db.query.start", [("query", "SELECT * FROM users")])
    |> add_event("db.query.end", [("rows", "42"), ("duration_ms", "125")])
    |> add_event("cache.miss", [("key", "user:123")])
  
  // Add links to span
  let span_with_links = span_with_events
    |> add_link("trace-789", "span-001", [("operation", "predecessor")])
    |> add_link("trace-456", "span-002", [("operation", "correlation")])
  
  // Verify events
  assert_eq(span_with_links.events.length(), 3)
  assert_eq(span_with_links.events[0].name, "db.query.start")
  assert_eq(span_with_links.events[1].attributes[0], ("rows", "42"))
  assert_eq(span_with_links.events[2].name, "cache.miss")
  
  // Verify links
  assert_eq(span_with_links.links.length(), 2)
  assert_eq(span_with_links.links[0].trace_id, "trace-789")
  assert_eq(span_with_links.links[1].attributes[0], ("operation", "correlation"))
  
  // Function to find events by name
  let find_events_by_name = fn(span: Span, event_name: String) {
    let mut result = []
    for event in span.events {
      if event.name == event_name {
        result = result.push(event)
      }
    }
    result
  }
  
  // Test event finding
  let db_start_events = find_events_by_name(span_with_links, "db.query.start")
  assert_eq(db_start_events.length(), 1)
  assert_eq(db_start_events[0].attributes[0], ("query", "SELECT * FROM users"))
  
  let cache_events = find_events_by_name(span_with_links, "cache.miss")
  assert_eq(cache_events.length(), 1)
  assert_eq(cache_events[0].attributes[0], ("key", "user:123"))
  
  let non_existent_events = find_events_by_name(span_with_links, "non.existent")
  assert_eq(non_existent_events.length(), 0)
}

// Test 5: Resource and Attribute Management
test "resource and attribute management" {
  // Define resource structure
  type Resource = {
    attributes: Array[(String, String)],
    schema_url: Option[String]
  }
  
  // Define instrumentation library structure
  type InstrumentationLibrary = {
    name: String,
    version: String,
    schema_url: Option[String]
  }
  
  // Function to create resource with standard attributes
  let create_resource = fn(service_name: String, service_version: String, additional_attrs: Array[(String, String)]) {
    let standard_attrs = [
      ("service.name", service_name),
      ("service.version", service_version),
      ("telemetry.sdk.name", "azimuth"),
      ("telemetry.sdk.version", "1.0.0"),
      ("telemetry.sdk.language", "moonbit")
    ]
    
    {
      attributes: standard_attrs + additional_attrs,
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    }
  }
  
  // Function to merge resources
  let merge_resources = fn(primary: Resource, secondary: Resource) {
    let mut merged_attrs = primary.attributes
    let existing_keys = primary.attributes.map(fn(attr) { attr.0 })
    
    for attr in secondary.attributes {
      if not(existing_keys.contains(attr.0)) {
        merged_attrs = merged_attrs.push(attr)
      }
    }
    
    {
      attributes: merged_attrs,
      schema_url: primary.schema_url
    }
  }
  
  // Create primary resource
  let primary_resource = create_resource(
    "payment-service",
    "1.2.3",
    [
      ("deployment.environment", "production"),
      ("host.name", "payment-server-01"),
      ("region", "us-west-2")
    ]
  )
  
  // Create secondary resource
  let secondary_resource = {
    attributes: [
      ("service.instance.id", "instance-123"),
      ("k8s.pod.name", "payment-service-7d4f8c9b-xyz"),
      ("k8s.namespace.name", "production")
    ],
    schema_url: None
  }
  
  // Merge resources
  let merged_resource = merge_resources(primary_resource, secondary_resource)
  
  // Verify merged resource
  assert_eq(merged_resource.attributes.length(), 9)
  assert_true(merged_resource.attributes.contains(("service.name", "payment-service")))
  assert_true(merged_resource.attributes.contains(("service.version", "1.2.3")))
  assert_true(merged_resource.attributes.contains(("deployment.environment", "production")))
  assert_true(merged_resource.attributes.contains(("service.instance.id", "instance-123")))
  assert_true(merged_resource.attributes.contains(("k8s.pod.name", "payment-service-7d4f8c9b-xyz")))
  
  // Function to filter attributes by prefix
  let filter_by_prefix = fn(resource: Resource, prefix: String) {
    let mut filtered = []
    for attr in resource.attributes {
      if attr.0.starts_with(prefix) {
        filtered = filtered.push(attr)
      }
    }
    filtered
  }
  
  // Test attribute filtering
  let service_attrs = filter_by_prefix(merged_resource, "service.")
  assert_eq(service_attrs.length(), 3)
  
  let k8s_attrs = filter_by_prefix(merged_resource, "k8s.")
  assert_eq(k8s_attrs.length(), 2)
  
  let telemetry_attrs = filter_by_prefix(merged_resource, "telemetry.")
  assert_eq(telemetry_attrs.length(), 3)
  
  // Function to create instrumentation library
  let create_instrumentation_library = fn(name: String, version: String) {
    {
      name,
      version,
      schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
    }
  }
  
  // Create instrumentation library
  let instrumentation = create_instrumentation_library("azimuth-db", "0.1.0")
  assert_eq(instrumentation.name, "azimuth-db")
  assert_eq(instrumentation.version, "0.1.0")
  assert_eq(instrumentation.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
}

// Test 6: Batch Processing and Exporting
test "batch processing and exporting telemetry data" {
  // Define exportable item
  type ExportableItem = {
    item_type: String,  // "span", "metric", "log"
    data: String,       // Serialized data
    timestamp: Int
  }
  
  // Define batch configuration
  type BatchConfig = {
    max_batch_size: Int,
    max_export_timeout_ms: Int,
    max_queue_size: Int
  }
  
  // Define batch processor
  type BatchProcessor = {
    config: BatchConfig,
    queue: Array[ExportableItem],
    exported_batches: Array[Array[ExportableItem]]
  }
  
  // Function to create batch processor
  let create_batch_processor = fn(max_batch_size: Int, max_export_timeout_ms: Int, max_queue_size: Int) {
    {
      config: {
        max_batch_size,
        max_export_timeout_ms,
        max_queue_size
      },
      queue: [],
      exported_batches: []
    }
  }
  
  // Function to add item to queue
  let add_to_queue = fn(processor: BatchProcessor, item: ExportableItem) {
    if processor.queue.length() < processor.config.max_queue_size {
      { processor | queue: processor.queue.push(item) }
    } else {
      // Queue is full, drop oldest item and add new one
      let mut new_queue = processor.queue
      new_queue = new_queue.slice(1)  // Remove first item
      { processor | queue: new_queue.push(item) }
    }
  }
  
  // Function to export batch
  let export_batch = fn(processor: BatchProcessor) {
    if processor.queue.length() > 0 {
      let batch_size = if processor.queue.length() > processor.config.max_batch_size {
        processor.config.max_batch_size
      } else {
        processor.queue.length()
      }
      
      let batch = processor.queue.slice(0, batch_size)
      let remaining_queue = processor.queue.slice(batch_size)
      
      {
        config: processor.config,
        queue: remaining_queue,
        exported_batches: processor.exported_batches.push(batch)
      }
    } else {
      processor
    }
  }
  
  // Create batch processor
  let mut processor = create_batch_processor(3, 5000, 10)
  
  // Add items to queue
  let item1 = { item_type: "span", data: "span-data-1", timestamp: 1640995200 }
  let item2 = { item_type: "metric", data: "metric-data-1", timestamp: 1640995201 }
  let item3 = { item_type: "log", data: "log-data-1", timestamp: 1640995202 }
  let item4 = { item_type: "span", data: "span-data-2", timestamp: 1640995203 }
  
  processor = add_to_queue(processor, item1)
  processor = add_to_queue(processor, item2)
  processor = add_to_queue(processor, item3)
  processor = add_to_queue(processor, item4)
  
  // Verify queue state
  assert_eq(processor.queue.length(), 4)
  
  // Export first batch
  processor = export_batch(processor)
  
  // Verify export
  assert_eq(processor.exported_batches.length(), 1)
  assert_eq(processor.exported_batches[0].length(), 3)  // Max batch size
  assert_eq(processor.queue.length(), 1)  // One item remaining
  
  // Verify batch content
  let first_batch = processor.exported_batches[0]
  assert_eq(first_batch[0].item_type, "span")
  assert_eq(first_batch[1].item_type, "metric")
  assert_eq(first_batch[2].item_type, "log")
  
  // Export remaining items
  processor = export_batch(processor)
  
  // Verify final state
  assert_eq(processor.exported_batches.length(), 2)
  assert_eq(processor.exported_batches[1].length(), 1)  // Remaining item
  assert_eq(processor.queue.length(), 0)  // Queue empty
  assert_eq(processor.exported_batches[1][0].item_type, "span")
  assert_eq(processor.exported_batches[1][0].data, "span-data-2")
  
  // Function to get export statistics
  let get_export_stats = fn(processor: BatchProcessor) {
    let mut span_count = 0
    let mut metric_count = 0
    let mut log_count = 0
    
    for batch in processor.exported_batches {
      for item in batch {
        match item.item_type {
          "span" => span_count = span_count + 1
          "metric" => metric_count = metric_count + 1
          "log" => log_count = log_count + 1
          _ => {}
        }
      }
    }
    
    {
      total_batches: processor.exported_batches.length(),
      total_items: span_count + metric_count + log_count,
      span_count,
      metric_count,
      log_count
    }
  }
  
  // Test statistics
  let stats = get_export_stats(processor)
  assert_eq(stats.total_batches, 2)
  assert_eq(stats.total_items, 4)
  assert_eq(stats.span_count, 2)
  assert_eq(stats.metric_count, 1)
  assert_eq(stats.log_count, 1)
}

// Test 7: Performance Metrics Collection
test "performance metrics collection and analysis" {
  // Define performance metric types
  enum PerfMetricType {
    CPU
    Memory
    DiskIO
    NetworkIO
    Latency
  }
  
  // Define performance metric
  type PerfMetric = {
    metric_type: PerfMetricType,
    value: Float,
    unit: String,
    tags: Array[(String, String)],
    timestamp: Int
  }
  
  // Define performance analyzer
  type PerfAnalyzer = {
    metrics: Array[PerfMetric],
    alerts: Array[String]
  }
  
  // Function to create performance analyzer
  let create_analyzer = fn() {
    {
      metrics: [],
      alerts: []
    }
  }
  
  // Function to add metric
  let add_metric = fn(analyzer: PerfAnalyzer, metric_type: PerfMetricType, value: Float, unit: String, tags: Array[(String, String)]) {
    let metric = {
      metric_type,
      value,
      unit,
      tags,
      timestamp: 1640995200
    }
    { analyzer | metrics: analyzer.metrics.push(metric) }
  }
  
  // Function to check thresholds and generate alerts
  let check_thresholds = fn(analyzer: PerfAnalyzer) {
    let mut new_alerts = analyzer.alerts
    
    for metric in analyzer.metrics {
      match metric.metric_type {
        PerfMetricType::CPU => {
          if metric.value > 80.0 {
            new_alerts = new_alerts.push("High CPU usage: " + metric.value.to_string() + "%")
          }
        }
        PerfMetricType::Memory => {
          if metric.value > 90.0 {
            new_alerts = new_alerts.push("High memory usage: " + metric.value.to_string() + "%")
          }
        }
        PerfMetricType::Latency => {
          if metric.value > 1000.0 {
            new_alerts = new_alerts.push("High latency: " + metric.value.to_string() + "ms")
          }
        }
        _ => {}
      }
    }
    
    { analyzer | alerts: new_alerts }
  }
  
  // Create analyzer and add metrics
  let mut analyzer = create_analyzer()
  
  analyzer = add_metric(analyzer, PerfMetricType::CPU, 65.5, "percent", [("host", "server-01")])
  analyzer = add_metric(analyzer, PerfMetricType::Memory, 45.2, "percent", [("host", "server-01")])
  analyzer = add_metric(analyzer, PerfMetricType::Latency, 250.0, "ms", [("endpoint", "/api/users")])
  analyzer = add_metric(analyzer, PerfMetricType::CPU, 85.0, "percent", [("host", "server-02")])
  analyzer = add_metric(analyzer, PerfMetricType::Memory, 95.0, "percent", [("host", "server-02")])
  analyzer = add_metric(analyzer, PerfMetricType::Latency, 1500.0, "ms", [("endpoint", "/api/orders")])
  
  // Check thresholds and generate alerts
  analyzer = check_thresholds(analyzer)
  
  // Verify metrics
  assert_eq(analyzer.metrics.length(), 6)
  assert_eq(analyzer.metrics[0].metric_type, PerfMetricType::CPU)
  assert_eq(analyzer.metrics[0].value, 65.5)
  assert_eq(analyzer.metrics[3].value, 85.0)
  
  // Verify alerts
  assert_eq(analyzer.alerts.length(), 3)
  assert_true(analyzer.alerts[0].contains("High CPU usage: 85.0%"))
  assert_true(analyzer.alerts[1].contains("High memory usage: 95.0%"))
  assert_true(analyzer.alerts[2].contains("High latency: 1500.0ms"))
  
  // Function to calculate averages by metric type
  let calculate_averages = fn(analyzer: PerfAnalyzer) {
    let mut cpu_sum = 0.0
    let mut cpu_count = 0
    let mut memory_sum = 0.0
    let mut memory_count = 0
    let mut latency_sum = 0.0
    let mut latency_count = 0
    
    for metric in analyzer.metrics {
      match metric.metric_type {
        PerfMetricType::CPU => {
          cpu_sum = cpu_sum + metric.value
          cpu_count = cpu_count + 1
        }
        PerfMetricType::Memory => {
          memory_sum = memory_sum + metric.value
          memory_count = memory_count + 1
        }
        PerfMetricType::Latency => {
          latency_sum = latency_sum + metric.value
          latency_count = latency_count + 1
        }
        _ => {}
      }
    }
    
    {
      cpu_avg: if cpu_count > 0 { cpu_sum / cpu_count.to_float() } else { 0.0 },
      memory_avg: if memory_count > 0 { memory_sum / memory_count.to_float() } else { 0.0 },
      latency_avg: if latency_count > 0 { latency_sum / latency_count.to_float() } else { 0.0 }
    }
  }
  
  // Test averages calculation
  let averages = calculate_averages(analyzer)
  assert_eq(averages.cpu_avg, (65.5 + 85.0) / 2.0)
  assert_eq(averages.memory_avg, (45.2 + 95.0) / 2.0)
  assert_eq(averages.latency_avg, (250.0 + 1500.0) / 2.0)
}

// Test 8: Distributed Tracing Correlation
test "distributed tracing correlation across services" {
  // Define trace correlation structure
  type TraceCorrelation = {
    trace_id: String,
    service_graph: Array[(String, String)],  // (service, parent_service)
    span_hierarchy: Array[(String, String)],  // (span_id, parent_span_id)
    cross_service_calls: Array[(String, String, String)]  // (from_service, to_service, span_id)
  }
  
  // Function to initialize trace correlation
  let init_correlation = fn(trace_id: String, root_service: String) {
    {
      trace_id,
      service_graph: [(root_service, "")],  // Root service has no parent
      span_hierarchy: [],
      cross_service_calls: []
    }
  }
  
  // Function to add service to graph
  let add_service = fn(correlation: TraceCorrelation, service: String, parent_service: String) {
    { correlation | service_graph: correlation.service_graph.push((service, parent_service)) }
  }
  
  // Function to add span to hierarchy
  let add_span = fn(correlation: TraceCorrelation, span_id: String, parent_span_id: String) {
    { correlation | span_hierarchy: correlation.span_hierarchy.push((span_id, parent_span_id)) }
  }
  
  // Function to add cross-service call
  let add_cross_service_call = fn(correlation: TraceCorrelation, from_service: String, to_service: String, span_id: String) {
    { correlation | cross_service_calls: correlation.cross_service_calls.push((from_service, to_service, span_id)) }
  }
  
  // Initialize trace correlation for API Gateway
  let mut correlation = init_correlation("trace-12345", "api-gateway")
  
  // Add services to the graph
  correlation = add_service(correlation, "auth-service", "api-gateway")
  correlation = add_service(correlation, "user-service", "api-gateway")
  correlation = add_service(correlation, "payment-service", "user-service")
  correlation = add_service(correlation, "notification-service", "payment-service")
  
  // Add span hierarchy
  correlation = add_span(correlation, "span-100", "")  // Root span
  correlation = add_span(correlation, "span-101", "span-100")  // Auth call
  correlation = add_span(correlation, "span-102", "span-100")  // User service call
  correlation = add_span(correlation, "span-103", "span-102")  // Payment call
  correlation = add_span(correlation, "span-104", "span-103")  // Notification call
  
  // Add cross-service calls
  correlation = add_cross_service_call(correlation, "api-gateway", "auth-service", "span-101")
  correlation = add_cross_service_call(correlation, "api-gateway", "user-service", "span-102")
  correlation = add_cross_service_call(correlation, "user-service", "payment-service", "span-103")
  correlation = add_cross_service_call(correlation, "payment-service", "notification-service", "span-104")
  
  // Verify service graph
  assert_eq(correlation.service_graph.length(), 5)
  assert_eq(correlation.service_graph[0], ("api-gateway", ""))
  assert_eq(correlation.service_graph[1], ("auth-service", "api-gateway"))
  assert_eq(correlation.service_graph[3], ("payment-service", "user-service"))
  
  // Verify span hierarchy
  assert_eq(correlation.span_hierarchy.length(), 5)
  assert_eq(correlation.span_hierarchy[0], ("span-100", ""))
  assert_eq(correlation.span_hierarchy[2], ("span-102", "span-100"))
  assert_eq(correlation.span_hierarchy[4], ("span-104", "span-103"))
  
  // Verify cross-service calls
  assert_eq(correlation.cross_service_calls.length(), 4)
  assert_eq(correlation.cross_service_calls[0], ("api-gateway", "auth-service", "span-101"))
  assert_eq(correlation.cross_service_calls[2], ("user-service", "payment-service", "span-103"))
  
  // Function to find service chain
  let find_service_chain = fn(correlation: TraceCorrelation, start_service: String, end_service: String) {
    let mut chain = []
    let mut current = start_service
    let mut found = false
    
    for i in 0..10 {  // Prevent infinite loops
      if current == end_service {
        found = true
        break
      }
      
      chain = chain.push(current)
      
      // Find next service in chain
      let mut next = ""
      for (service, parent) in correlation.service_graph {
        if parent == current {
          next = service
          break
        }
      }
      
      if next == "" {
        break  // No more services in chain
      }
      
      current = next
    }
    
    if found {
      chain.push(end_service)
      Some(chain)
    } else {
      None
    }
  }
  
  // Test service chain finding
  let chain1 = find_service_chain(correlation, "api-gateway", "payment-service")
  assert_eq(chain1, Some(["api-gateway", "user-service", "payment-service"]))
  
  let chain2 = find_service_chain(correlation, "user-service", "notification-service")
  assert_eq(chain2, Some(["user-service", "payment-service", "notification-service"]))
  
  let chain3 = find_service_chain(correlation, "auth-service", "notification-service")
  assert_eq(chain3, None)  // No direct chain
  
  // Function to calculate service depth
  let calculate_service_depth = fn(correlation: TraceCorrelation, service: String) {
    let mut depth = 0
    let mut current = service
    
    for i in 0..10 {  // Prevent infinite loops
      // Find parent service
      let mut parent = ""
      for (s, p) in correlation.service_graph {
        if s == current {
          parent = p
          break
        }
      }
      
      if parent == "" {
        break  // Root service reached
      }
      
      depth = depth + 1
      current = parent
    }
    
    depth
  }
  
  // Test service depth calculation
  assert_eq(calculate_service_depth(correlation, "api-gateway"), 0)
  assert_eq(calculate_service_depth(correlation, "auth-service"), 1)
  assert_eq(calculate_service_depth(correlation, "user-service"), 1)
  assert_eq(calculate_service_depth(correlation, "payment-service"), 2)
  assert_eq(calculate_service_depth(correlation, "notification-service"), 3)
}

// Test 9: Telemetry Data Compression
test "telemetry data compression and serialization" {
  // Define compressed data structure
  type CompressedData = {
    original_size: Int,
    compressed_size: Int,
    compression_ratio: Float,
    algorithm: String,
    data: String  // Simulated compressed data
  }
  
  // Define telemetry batch
  type TelemetryBatch = {
    spans: Array[String],
    metrics: Array[String],
    logs: Array[String],
    timestamp: Int
  }
  
  // Function to simulate compression
  let compress_data = fn(data: String, algorithm: String) {
    let original_size = data.length()
    
    // Simulate different compression ratios based on algorithm
    let compression_ratio = match algorithm {
      "gzip" => 0.3
      "lz4" => 0.5
      "snappy" => 0.4
      _ => 0.7  // Default
    }
    
    let compressed_size = (original_size.to_float() * compression_ratio).to_int()
    
    {
      original_size,
      compressed_size,
      compression_ratio,
      algorithm,
      data: "compressed_" + algorithm + "_" + compressed_size.to_string()
    }
  }
  
  // Function to create telemetry batch
  let create_batch = fn(span_count: Int, metric_count: Int, log_count: Int) {
    let mut spans = []
    let mut metrics = []
    let mut logs = []
    
    for i in 0..span_count {
      spans = spans.push("span_data_" + i.to_string())
    }
    
    for i in 0..metric_count {
      metrics = metrics.push("metric_data_" + i.to_string())
    }
    
    for i in 0..log_count {
      logs = logs.push("log_data_" + i.to_string())
    }
    
    {
      spans,
      metrics,
      logs,
      timestamp: 1640995200
    }
  }
  
  // Function to serialize batch to string
  let serialize_batch = fn(batch: TelemetryBatch) {
    let mut serialized = "BATCH:" + batch.timestamp.to_string() + "\n"
    
    serialized = serialized + "SPANS:" + batch.spans.length().to_string() + "\n"
    for span in batch.spans {
      serialized = serialized + span + "\n"
    }
    
    serialized = serialized + "METRICS:" + batch.metrics.length().to_string() + "\n"
    for metric in batch.metrics {
      serialized = serialized + metric + "\n"
    }
    
    serialized = serialized + "LOGS:" + batch.logs.length().to_string() + "\n"
    for log in batch.logs {
      serialized = serialized + log + "\n"
    }
    
    serialized
  }
  
  // Create telemetry batch
  let batch = create_batch(10, 5, 8)
  
  // Serialize batch
  let serialized = serialize_batch(batch)
  
  // Verify serialization
  assert_true(serialized.contains("BATCH:1640995200"))
  assert_true(serialized.contains("SPANS:10"))
  assert_true(serialized.contains("METRICS:5"))
  assert_true(serialized.contains("LOGS:8"))
  assert_true(serialized.contains("span_data_0"))
  assert_true(serialized.contains("metric_data_4"))
  assert_true(serialized.contains("log_data_7"))
  
  // Test compression with different algorithms
  let gzip_compressed = compress_data(serialized, "gzip")
  let lz4_compressed = compress_data(serialized, "lz4")
  let snappy_compressed = compress_data(serialized, "snappy")
  
  // Verify gzip compression
  assert_eq(gzip_compressed.original_size, serialized.length())
  assert_eq(gzip_compressed.algorithm, "gzip")
  assert_eq(gzip_compressed.compression_ratio, 0.3)
  assert_eq(gzip_compressed.compressed_size, (serialized.length().to_float() * 0.3).to_int())
  assert_true(gzip_compressed.data.contains("compressed_gzip_"))
  
  // Verify lz4 compression
  assert_eq(lz4_compressed.original_size, serialized.length())
  assert_eq(lz4_compressed.algorithm, "lz4")
  assert_eq(lz4_compressed.compression_ratio, 0.5)
  assert_eq(lz4_compressed.compressed_size, (serialized.length().to_float() * 0.5).to_int())
  assert_true(lz4_compressed.data.contains("compressed_lz4_"))
  
  // Verify snappy compression
  assert_eq(snappy_compressed.original_size, serialized.length())
  assert_eq(snappy_compressed.algorithm, "snappy")
  assert_eq(snappy_compressed.compression_ratio, 0.4)
  assert_eq(snappy_compressed.compressed_size, (serialized.length().to_float() * 0.4).to_int())
  assert_true(snappy_compressed.data.contains("compressed_snappy_"))
  
  // Function to compare compression algorithms
  let compare_algorithms = fn(compressed_data: Array[CompressedData]) {
    let mut best = compressed_data[0]
    
    for data in compressed_data {
      if data.compressed_size < best.compressed_size {
        best = data
      }
    }
    
    best
  }
  
  // Test algorithm comparison
  let all_compressed = [gzip_compressed, lz4_compressed, snappy_compressed]
  let best_compression = compare_algorithms(all_compressed)
  
  // GZIP should have the best compression (smallest size)
  assert_eq(best_compression.algorithm, "gzip")
  assert_eq(best_compression.compressed_size, gzip_compressed.compressed_size)
  
  // Function to calculate space savings
  let calculate_space_savings = fn(compressed: CompressedData) {
    let saved = compressed.original_size - compressed.compressed_size
    let percentage = (saved.to_float() / compressed.original_size.to_float()) * 100.0
    {
      bytes_saved: saved,
      percentage_saved: percentage
    }
  }
  
  // Test space savings calculation
  let gzip_savings = calculate_space_savings(gzip_compressed)
  assert_eq(gzip_savings.bytes_saved, gzip_compressed.original_size - gzip_compressed.compressed_size)
  assert_eq(gzip_savings.percentage_saved, 70.0)  // 30% compression ratio = 70% savings
  
  let lz4_savings = calculate_space_savings(lz4_compressed)
  assert_eq(lz4_savings.percentage_saved, 50.0)  // 50% compression ratio = 50% savings
}

// Test 10: Telemetry Data Retention and Cleanup
test "telemetry data retention and cleanup policies" {
  // Define retention policy
  type RetentionPolicy = {
    spans_retention_days: Int,
    metrics_retention_days: Int,
    logs_retention_days: Int,
    max_data_size_mb: Int
  }
  
  // Define telemetry storage
  type TelemetryStorage = {
    spans: Array[(String, Int)],  // (span_id, timestamp)
    metrics: Array[(String, Int)],  // (metric_id, timestamp)
    logs: Array[(String, Int)],  // (log_id, timestamp)
    current_size_mb: Int
  }
  
  // Function to create storage
  let create_storage = fn() {
    {
      spans: [],
      metrics: [],
      logs: [],
      current_size_mb: 0
    }
  }
  
  // Function to add data to storage
  let add_data = fn(storage: TelemetryStorage, data_type: String, id: String, timestamp: Int, size_mb: Int) {
    match data_type {
      "span" => {
        {
          spans: storage.spans.push((id, timestamp)),
          metrics: storage.metrics,
          logs: storage.logs,
          current_size_mb: storage.current_size_mb + size_mb
        }
      }
      "metric" => {
        {
          spans: storage.spans,
          metrics: storage.metrics.push((id, timestamp)),
          logs: storage.logs,
          current_size_mb: storage.current_size_mb + size_mb
        }
      }
      "log" => {
        {
          spans: storage.spans,
          metrics: storage.metrics,
          logs: storage.logs.push((id, timestamp)),
          current_size_mb: storage.current_size_mb + size_mb
        }
      }
      _ => storage
    }
  }
  
  // Function to cleanup old data based on retention policy
  let cleanup_old_data = fn(storage: TelemetryStorage, policy: RetentionPolicy, current_timestamp: Int) {
    let seconds_per_day = 86400
    let spans_cutoff = current_timestamp - (policy.spans_retention_days * seconds_per_day)
    let metrics_cutoff = current_timestamp - (policy.metrics_retention_days * seconds_per_day)
    let logs_cutoff = current_timestamp - (policy.logs_retention_days * seconds_per_day)
    
    // Filter spans by retention period
    let mut retained_spans = []
    let mut spans_removed = 0
    for (id, timestamp) in storage.spans {
      if timestamp >= spans_cutoff {
        retained_spans = retained_spans.push((id, timestamp))
      } else {
        spans_removed = spans_removed + 1
      }
    }
    
    // Filter metrics by retention period
    let mut retained_metrics = []
    let mut metrics_removed = 0
    for (id, timestamp) in storage.metrics {
      if timestamp >= metrics_cutoff {
        retained_metrics = retained_metrics.push((id, timestamp))
      } else {
        metrics_removed = metrics_removed + 1
      }
    }
    
    // Filter logs by retention period
    let mut retained_logs = []
    let mut logs_removed = 0
    for (id, timestamp) in storage.logs {
      if timestamp >= logs_cutoff {
        retained_logs = retained_logs.push((id, timestamp))
      } else {
        logs_removed = logs_removed + 1
      }
    }
    
    // Calculate new size (simplified)
    let items_removed = spans_removed + metrics_removed + logs_removed
    let size_reduction = items_removed * 10  // Assume 10MB per item
    let new_size = if storage.current_size_mb > size_reduction {
      storage.current_size_mb - size_reduction
    } else {
      0
    }
    
    {
      spans: retained_spans,
      metrics: retained_metrics,
      logs: retained_logs,
      current_size_mb: new_size
    }
  }
  
  // Function to cleanup by size limit
  let cleanup_by_size = fn(storage: TelemetryStorage, max_size_mb: Int) {
    if storage.current_size_mb <= max_size_mb {
      storage
    } else {
      // Simple strategy: remove oldest items until under limit
      let mut combined = []
      
      // Add all items with type and timestamp
      for (id, timestamp) in storage.spans {
        combined = combined.push(("span", id, timestamp))
      }
      for (id, timestamp) in storage.metrics {
        combined = combined.push(("metric", id, timestamp))
      }
      for (id, timestamp) in storage.logs {
        combined = combined.push(("log", id, timestamp))
      }
      
      // Sort by timestamp (oldest first)
      // Note: This is a simplified sort, real implementation would be more efficient
      let mut sorted = []
      for item in combined {
        let mut inserted = false
        let mut new_sorted = []
        
        for existing in sorted {
          if not(inserted) and item.2 < existing.2 {
            new_sorted = new_sorted.push(item)
            inserted = true
          }
          new_sorted = new_sorted.push(existing)
        }
        
        if not(inserted) {
          new_sorted = new_sorted.push(item)
        }
        
        sorted = new_sorted
      }
      
      // Remove oldest items until under size limit
      let mut current_size = storage.current_size_mb
      let mut to_remove = 0
      
      for item in sorted {
        if current_size <= max_size_mb {
          break
        }
        current_size = current_size - 10  // Assume 10MB per item
        to_remove = to_remove + 1
      }
      
      // Keep items after the ones to remove
      let retained = sorted.slice(to_remove)
      
      // Separate back into original arrays
      let mut final_spans = []
      let mut final_metrics = []
      let mut final_logs = []
      
      for (data_type, id, timestamp) in retained {
        match data_type {
          "span" => final_spans = final_spans.push((id, timestamp))
          "metric" => final_metrics = final_metrics.push((id, timestamp))
          "log" => final_logs = final_logs.push((id, timestamp))
          _ => {}
        }
      }
      
      {
        spans: final_spans,
        metrics: final_metrics,
        logs: final_logs,
        current_size_mb: current_size
      }
    }
  }
  
  // Create storage and add data
  let mut storage = create_storage()
  
  // Add data with different timestamps (assuming current timestamp is 1640995200)
  storage = add_data(storage, "span", "span-1", 1640995200, 10)  // Current
  storage = add_data(storage, "span", "span-2", 1640908800, 10)  // 1 day ago
  storage = add_data(storage, "span", "span-3", 1640822400, 10)  // 2 days ago
  storage = add_data(storage, "span", "span-4", 1640736000, 10)  // 3 days ago
  storage = add_data(storage, "span", "span-5", 1640649600, 10)  // 4 days ago
  
  storage = add_data(storage, "metric", "metric-1", 1640995200, 10)  // Current
  storage = add_data(storage, "metric", "metric-2", 1640908800, 10)  // 1 day ago
  storage = add_data(storage, "metric", "metric-3", 1640822400, 10)  // 2 days ago
  storage = add_data(storage, "metric", "metric-4", 1640736000, 10)  // 3 days ago
  storage = add_data(storage, "metric", "metric-5", 1640649600, 10)  // 4 days ago
  
  storage = add_data(storage, "log", "log-1", 1640995200, 10)  // Current
  storage = add_data(storage, "log", "log-2", 1640908800, 10)  // 1 day ago
  storage = add_data(storage, "log", "log-3", 1640822400, 10)  // 2 days ago
  storage = add_data(storage, "log", "log-4", 1640736000, 10)  // 3 days ago
  storage = add_data(storage, "log", "log-5", 1640649600, 10)  // 4 days ago
  
  // Verify initial state
  assert_eq(storage.spans.length(), 5)
  assert_eq(storage.metrics.length(), 5)
  assert_eq(storage.logs.length(), 5)
  assert_eq(storage.current_size_mb, 150)  // 15 items * 10MB each
  
  // Create retention policy
  let policy = {
    spans_retention_days: 3,
    metrics_retention_days: 7,
    logs_retention_days: 1,
    max_data_size_mb: 100
  }
  
  // Apply time-based cleanup
  let time_cleaned = cleanup_old_data(storage, policy, 1640995200)
  
  // Verify time-based cleanup
  assert_eq(time_cleaned.spans.length(), 3)  // Keep 3 days
  assert_eq(time_cleaned.metrics.length(), 5)  // Keep 7 days
  assert_eq(time_cleaned.logs.length(), 1)   // Keep 1 day
  
  // Apply size-based cleanup
  let size_cleaned = cleanup_by_size(time_cleaned, 80)
  
  // Verify size-based cleanup
  assert_true(size_cleaned.current_size_mb <= 80)
  
  // Function to get cleanup statistics
  let get_cleanup_stats = fn(original: TelemetryStorage, cleaned: TelemetryStorage) {
    {
      spans_removed: original.spans.length() - cleaned.spans.length(),
      metrics_removed: original.metrics.length() - cleaned.metrics.length(),
      logs_removed: original.logs.length() - cleaned.logs.length(),
      size_freed_mb: original.current_size_mb - cleaned.current_size_mb
    }
  }
  
  // Test cleanup statistics
  let stats = get_cleanup_stats(storage, size_cleaned)
  assert_eq(stats.spans_removed + stats.metrics_removed + stats.logs_removed, 15 - (size_cleaned.spans.length() + size_cleaned.metrics.length() + size_cleaned.logs.length()))
  assert_eq(stats.size_freed_mb, 150 - size_cleaned.current_size_mb)
}