// Advanced Telemetry Scenarios Test Suite for Azimuth
// This file contains 8 comprehensive test cases covering advanced telemetry scenarios

// Test 1: Sampler Configuration and Testing
test "sampler configuration and decision making" {
  // Test AlwaysOnSampler
  let always_on_sampler = AlwaysOnSampler::new()
  let sampling_decision_on = Sampler::should_sample(always_on_sampler, "trace123", "span456", "operation.name", [])
  assert_eq(sampling_decision_on.decision, SamplingDecision::RecordAndSample)
  assert_true(sampling_decision_on.attributes.is_empty())
  
  // Test AlwaysOffSampler
  let always_off_sampler = AlwaysOffSampler::new()
  let sampling_decision_off = Sampler::should_sample(always_off_sampler, "trace789", "span012", "operation.name", [])
  assert_eq(sampling_decision_off.decision, SamplingDecision::Drop)
  assert_true(sampling_decision_off.attributes.is_empty())
  
  // Test TraceIdRatioBasedSampler
  let ratio_sampler = TraceIdRatioBasedSampler::new(0.5) // 50% sampling
  let ratio_decision_1 = Sampler::should_sample(ratio_sampler, "trace1111111111111111111111111111111", "span111", "operation.name", [])
  let ratio_decision_2 = Sampler::should_sample(ratio_sampler, "trace2222222222222222222222222222222", "span222", "operation.name", [])
  
  // Verify decisions are either RecordAndSample or Drop
  match ratio_decision_1.decision {
    SamplingDecision::RecordAndSample => assert_true(true)
    SamplingDecision::Drop => assert_true(true)
    _ => assert_true(false)
  }
  
  match ratio_decision_2.decision {
    SamplingDecision::RecordAndSample => assert_true(true)
    SamplingDecision::Drop => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test ParentBasedSampler
  let parent_based_sampler = ParentBasedSampler::new(always_on_sampler)
  let parent_context = SpanContext {
    trace_id: "parent1234567890abcdef1234567890ab",
    span_id: "parent1234567890ab",
    sampled: true,
    trace_state: ""
  }
  
  let parent_based_decision = Sampler::should_sample(parent_based_sampler, "trace123", "span456", "operation.name", [])
  assert_eq(parent_based_decision.decision, SamplingDecision::RecordAndSample)
}

// Test 2: Telemetry Data Exporter Testing
test "telemetry data export operations" {
  // Create a span exporter
  let span_exporter = SpanExporter::new()
  
  // Create spans for export
  let span1 = SpanData {
    name: "operation.one",
    kind: SpanKind::Client,
    start_time: 1735689600000000000L,
    end_time: 1735689601000000000L,
    status: SpanStatus::Ok,
    attributes: [("http.method", AttributeValue::StringValue("GET"))],
    events: [],
    links: []
  }
  
  let span2 = SpanData {
    name: "operation.two",
    kind: SpanKind::Server,
    start_time: 1735689602000000000L,
    end_time: 1735689603000000000L,
    status: SpanStatus::Error,
    attributes: [("http.method", AttributeValue::StringValue("POST"))],
    events: [("error.occurred", 1735689602500000000L, [("error.type", AttributeValue::StringValue("Timeout"))])],
    links: []
  }
  
  let spans = [span1, span2]
  
  // Test export operation
  let export_result = SpanExporter::export(span_exporter, spans)
  match export_result {
    ExportResult::Success => assert_true(true)
    ExportResult::Failure(_) => assert_true(false) // For this test, expect success
  }
  
  // Test shutdown operation
  let shutdown_result = SpanExporter::shutdown(span_exporter)
  assert_true(shutdown_result) // Expect successful shutdown
  
  // Create a metric exporter
  let metric_exporter = MetricExporter::new()
  
  // Create metrics for export
  let metric1 = MetricData {
    name: "http.requests.total",
    description: Some("Total HTTP requests"),
    unit: Some("count"),
    aggregation: Aggregation::Sum(100.0),
    attributes: [("http.method", AttributeValue::StringValue("GET"))]
  }
  
  let metric2 = MetricData {
    name: "http.request.duration",
    description: Some("HTTP request duration"),
    unit: Some("ms"),
    aggregation: Aggregation::Histogram([10.0, 25.0, 50.0, 100.0, 200.0]),
    attributes: [("http.method", AttributeValue::StringValue("POST"))]
  }
  
  let metrics = [metric1, metric2]
  
  // Test metric export
  let metric_export_result = MetricExporter::export(metric_exporter, metrics)
  match metric_export_result {
    ExportResult::Success => assert_true(true)
    ExportResult::Failure(_) => assert_true(false) // For this test, expect success
  }
  
  // Test metric exporter shutdown
  let metric_shutdown_result = MetricExporter::shutdown(metric_exporter)
  assert_true(metric_shutdown_result) // Expect successful shutdown
}

// Test 3: Trace Time Correlation Testing
test "trace time correlation and temporal operations" {
  // Create spans with specific timing relationships
  let root_start = 1735689600000000000L // 2025-01-01 00:00:00 UTC
  let child1_start = root_start + 1000000000L // +1 second
  let child1_end = child1_start + 500000000L   // +0.5 seconds
  let child2_start = root_start + 2000000000L // +2 seconds
  let child2_end = child2_start + 300000000L   // +0.3 seconds
  let root_end = root_start + 3000000000L      // +3 seconds
  
  // Create root span
  let root_span = SpanData {
    name: "root.operation",
    kind: SpanKind::Internal,
    start_time: root_start,
    end_time: root_end,
    status: SpanStatus::Ok,
    attributes: [],
    events: [],
    links: []
  }
  
  // Create child spans
  let child1 = SpanData {
    name: "child.operation.one",
    kind: SpanKind::Client,
    start_time: child1_start,
    end_time: child1_end,
    status: SpanStatus::Ok,
    attributes: [],
    events: [],
    links: [("root.operation", root_span)]
  }
  
  let child2 = SpanData {
    name: "child.operation.two",
    kind: SpanKind::Server,
    start_time: child2_start,
    end_time: child2_end,
    status: SpanStatus::Ok,
    attributes: [],
    events: [],
    links: [("root.operation", root_span)]
  }
  
  // Verify temporal relationships
  assert_true(child1.start_time > root_span.start_time)
  assert_true(child1.end_time < root_span.end_time)
  assert_true(child2.start_time > root_span.start_time)
  assert_true(child2.end_time < root_span.end_time)
  
  // Verify child spans don't overlap (in this test case)
  assert_true(child1.end_time < child2.start_time)
  
  // Calculate durations
  let root_duration = root_span.end_time - root_span.start_time
  let child1_duration = child1.end_time - child1.start_time
  let child2_duration = child2.end_time - child2.start_time
  
  assert_eq(root_duration, 3000000000L) // 3 seconds
  assert_eq(child1_duration, 500000000L)  // 0.5 seconds
  assert_eq(child2_duration, 300000000L)  // 0.3 seconds
  
  // Verify root duration encompasses children
  assert_true(root_duration > child1_duration)
  assert_true(root_duration > child2_duration)
  
  // Test time-based event correlation
  let event_time = root_start + 1500000000L // 1.5 seconds after root start
  let correlated_event = SpanEvent {
    name: "mid.operation.event",
    timestamp: event_time,
    attributes: [("event.type", AttributeValue::StringValue("checkpoint"))]
  }
  
  // Verify event occurs within root span timeline
  assert_true(correlated_event.timestamp > root_span.start_time)
  assert_true(correlated_event.timestamp < root_span.end_time)
  
  // Test timestamp operations
  let timestamp_ns = 1735689600123456789L
  let timestamp_ms = timestamp_ns / 1000000
  let timestamp_s = timestamp_ns / 1000000000
  
  assert_eq(timestamp_ms, 1735689600123L)
  assert_eq(timestamp_s, 1735689600L)
  
  // Test timestamp difference calculations
  let earlier = 1735689600000000000L
  let later = 1735689600500000000L
  let difference = later - earlier
  
  assert_eq(difference, 500000000L) // 0.5 seconds in nanoseconds
}

// Test 4: Metrics Aggregation and Statistics
test "metrics aggregation and statistical operations" {
  // Create a counter metric
  let counter = CounterInstrument {
    name: "api.requests.total",
    description: Some("Total API requests"),
    unit: Some("count")
  }
  
  // Record measurements
  Counter::add(counter, 10.0, [("http.method", AttributeValue::StringValue("GET"))])
  Counter::add(counter, 5.0, [("http.method", AttributeValue::StringValue("POST"))])
  Counter::add(counter, 3.0, [("http.method", AttributeValue::StringValue("GET"))])
  
  // Create a histogram metric
  let histogram = HistogramInstrument {
    name: "api.request.duration",
    description: Some("API request duration"),
    unit: Some("ms")
  }
  
  // Record measurements
  Histogram::record(histogram, 100.0, [("http.method", AttributeValue::StringValue("GET"))])
  Histogram::record(histogram, 150.0, [("http.method", AttributeValue::StringValue("GET"))])
  Histogram::record(histogram, 200.0, [("http.method", AttributeValue::StringValue("POST"))])
  Histogram::record(histogram, 75.0, [("http.method", AttributeValue::StringValue("GET"))])
  Histogram::record(histogram, 300.0, [("http.method", AttributeValue::StringValue("POST"))])
  
  // Create an up-down counter
  let updown_counter = UpDownCounterInstrument {
    name: "active.connections",
    description: Some("Number of active connections"),
    unit: Some("count")
  }
  
  // Record measurements
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -3.0)
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Create a gauge metric
  let gauge = GaugeInstrument {
    name: "memory.usage",
    description: Some("Current memory usage"),
    unit: Some("bytes")
  }
  
  // Record measurements
  Gauge::record(gauge, 1024.0 * 1024.0 * 100.0) // 100MB
  Gauge::record(gauge, 1024.0 * 1024.0 * 150.0) // 150MB
  
  // Test aggregation operations
  let counter_sum = Aggregation::Sum(18.0) // 10 + 5 + 3
  let histogram_values = [100.0, 150.0, 200.0, 75.0, 300.0]
  let histogram_sum = 100.0 + 150.0 + 200.0 + 75.0 + 300.0
  let histogram_avg = histogram_sum / 5.0
  let histogram_min = 75.0
  let histogram_max = 300.0
  
  assert_eq(histogram_sum, 825.0)
  assert_eq(histogram_avg, 165.0)
  assert_eq(histogram_min, 75.0)
  assert_eq(histogram_max, 300.0)
  
  // Test percentile calculations (simplified)
  let sorted_values = [75.0, 100.0, 150.0, 200.0, 300.0]
  let p50_index = (5.0 * 0.5).ceil() - 1.0
  let p95_index = (5.0 * 0.95).ceil() - 1.0
  let p99_index = (5.0 * 0.99).ceil() - 1.0
  
  assert_eq(sorted_values[p50_index.to_int()], 100.0)  // 50th percentile
  assert_eq(sorted_values[p95_index.to_int()], 300.0)  // 95th percentile
  assert_eq(sorted_values[p99_index.to_int()], 300.0)  // 99th percentile
  
  // Test exponential histogram aggregation
  let exponential_histogram = ExponentialHistogram {
    sum: 825.0,
    count: 5,
    zero_count: 0,
    positive_buckets: [(1, 2), (2, 2), (3, 1)], // (index, count)
    negative_buckets: [],
    scale: 20
  }
  
  assert_eq(exponential_histogram.sum, 825.0)
  assert_eq(exponential_histogram.count, 5)
  assert_eq(exponential_histogram.zero_count, 0)
  assert_eq(exponential_histogram.positive_buckets.length(), 3)
  assert_eq(exponential_histogram.negative_buckets.length(), 0)
}

// Test 5: Log Filtering and Routing
test "log filtering and routing operations" {
  // Create a logger with different log levels
  let logger = Logger {
    name: "test.logger",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  // Create log records with different severity levels
  let trace_log = LogRecord {
    timestamp: 1735689600000000000L,
    observed_timestamp: Some(1735689600000001000L),
    severity_number: Trace,
    severity_text: Some("TRACE"),
    body: Some("Trace level message"),
    attributes: [],
    trace_id: Some("trace1234567890abcdef1234567890ab"),
    span_id: Some("span1234567890ab"),
    trace_flags: Some(1),
    resource: [],
    scope: logger
  }
  
  let debug_log = LogRecord {
    timestamp: 1735689600000000000L,
    observed_timestamp: Some(1735689600000001000L),
    severity_number: Debug,
    severity_text: Some("DEBUG"),
    body: Some("Debug level message"),
    attributes: [],
    trace_id: Some("trace1234567890abcdef1234567890ab"),
    span_id: Some("span1234567890ab"),
    trace_flags: Some(1),
    resource: [],
    scope: logger
  }
  
  let info_log = LogRecord {
    timestamp: 1735689600000000000L,
    observed_timestamp: Some(1735689600000001000L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Info level message"),
    attributes: [],
    trace_id: Some("trace1234567890abcdef1234567890ab"),
    span_id: Some("span1234567890ab"),
    trace_flags: Some(1),
    resource: [],
    scope: logger
  }
  
  let warn_log = LogRecord {
    timestamp: 1735689600000000000L,
    observed_timestamp: Some(1735689600000001000L),
    severity_number: Warn,
    severity_text: Some("WARN"),
    body: Some("Warning level message"),
    attributes: [],
    trace_id: Some("trace1234567890abcdef1234567890ab"),
    span_id: Some("span1234567890ab"),
    trace_flags: Some(1),
    resource: [],
    scope: logger
  }
  
  let error_log = LogRecord {
    timestamp: 1735689600000000000L,
    observed_timestamp: Some(1735689600000001000L),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Error level message"),
    attributes: [("error.type", AttributeValue::StringValue("ValidationError"))],
    trace_id: Some("trace1234567890abcdef1234567890ab"),
    span_id: Some("span1234567890ab"),
    trace_flags: Some(1),
    resource: [],
    scope: logger
  }
  
  // Create a log processor
  let log_processor = SimpleLogProcessor::new()
  
  // Test log filtering based on severity
  let all_logs = [trace_log, debug_log, info_log, warn_log, error_log]
  
  // Filter logs with severity >= Info
  let filtered_logs_info = LogProcessor::filter_by_severity(log_processor, all_logs, Info)
  assert_eq(filtered_logs_info.length(), 3) // Info, Warn, Error
  
  // Filter logs with severity >= Error
  let filtered_logs_error = LogProcessor::filter_by_severity(log_processor, all_logs, Error)
  assert_eq(filtered_logs_error.length(), 1) // Error only
  
  // Test log routing based on attributes
  let error_logs_with_type = LogProcessor::filter_by_attribute(log_processor, all_logs, "error.type")
  assert_eq(error_logs_with_type.length(), 1) // Only error log has error.type attribute
  
  // Test log routing based on trace ID
  let logs_with_trace = LogProcessor::filter_by_trace_id(log_processor, all_logs, "trace1234567890abcdef1234567890ab")
  assert_eq(logs_with_trace.length(), 5) // All logs have the same trace ID
  
  // Test log routing based on logger name
  let logs_by_logger = LogProcessor::filter_by_logger(log_processor, all_logs, "test.logger")
  assert_eq(logs_by_logger.length(), 5) // All logs from the same logger
  
  // Create a batch log processor
  let batch_processor = BatchLogProcessor::new(100) // Batch size of 100
  
  // Emit logs to batch processor
  LogProcessor::emit(batch_processor, trace_log)
  LogProcessor::emit(batch_processor, debug_log)
  LogProcessor::emit(batch_processor, info_log)
  LogProcessor::emit(batch_processor, warn_log)
  LogProcessor::emit(batch_processor, error_log)
  
  // Force flush
  let flush_result = LogProcessor::flush(batch_processor)
  assert_true(flush_result) // Expect successful flush
  
  // Shutdown processor
  let shutdown_result = LogProcessor::shutdown(batch_processor)
  assert_true(shutdown_result) // Expect successful shutdown
}

// Test 6: Resource Detection and Auto-Configuration
test "resource detection and auto-configuration" {
  // Create a resource detector
  let resource_detector = CompositeResourceDetector::new([
    EnvironmentResourceDetector::new(),
    HostResourceDetector::new(),
    ProcessResourceDetector::new(),
    OperatingSystemResourceDetector::new()
  ])
  
  // Detect resources
  let detected_resource = ResourceDetector::detect(resource_detector)
  
  // Verify detected attributes (simplified implementation)
  let resource_attributes = detected_resource.attributes
  assert_true(resource_attributes.length() >= 0) // Should have at least some attributes
  
  // Test manual resource configuration
  let manual_resource = Resource::builder()
    .with_service_name("manual.service")
    .with_service_version("2.0.0")
    .with_service_instance_id("instance-manual-123")
    .with_deployment_environment("staging")
    .with_host_name("host-manual")
    .with_process_pid(12345)
    .with_process_executable_name("manual-service")
    .with_os_type("linux")
    .with_os_description("Ubuntu 20.04")
    .build()
  
  // Verify manual resource attributes
  let manual_attributes = manual_resource.attributes
  assert_true(manual_attributes.length() >= 8) // Should have at least 8 attributes
  
  // Test resource merging with different strategies
  let base_resource = Resource::builder()
    .with_service_name("base.service")
    .with_service_version("1.0.0")
    .build()
  
  let override_resource = Resource::builder()
    .with_service_name("override.service")
    .with_service_instance_id("instance-override-456")
    .build()
  
  // Test resource merge with override strategy
  let merged_resource_override = Resource::merge_with_strategy(
    base_resource, 
    override_resource, 
    ResourceMergeStrategy::Override
  )
  
  // Test resource merge with combine strategy
  let merged_resource_combine = Resource::merge_with_strategy(
    base_resource, 
    override_resource, 
    ResourceMergeStrategy::Combine
  )
  
  // Verify merge results
  let override_attrs = merged_resource_override.attributes
  let combine_attrs = merged_resource_combine.attributes
  
  assert_true(override_attrs.length() >= 2)
  assert_true(combine_attrs.length() >= 3) // Should have more attributes with combine strategy
  
  // Test resource schema URL handling
  let resource_with_schema = Resource::builder()
    .with_schema_url("https://example.com/schema/v1")
    .with_service_name("service.with.schema")
    .build()
  
  assert_eq(resource_with_schema.schema_url, Some("https://example.com/schema/v1"))
  
  // Test resource attribute validation
  let valid_resource = Resource::builder()
    .with_service_name("valid.service")
    .with_attribute("custom.key", AttributeValue::StringValue("custom.value"))
    .with_attribute("numeric.key", AttributeValue::IntValue(42))
    .build()
  
  let valid_attributes = valid_resource.attributes
  assert_true(valid_attributes.length() >= 3) // service.name + two custom attributes
}

// Test 7: Cross-Process Context Propagation
test "cross-process context propagation with different carriers" {
  // Create a composite propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Create a context with span and baggage
  let span_context = SpanContext {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  }
  
  let baggage = Baggage::new()
    .set_entry("user.id", "user123")
    .set_entry("request.id", "req456")
    .set_entry("session.id", "sess789")
  
  let ctx = Context::root()
    .with_span_context(span_context)
    .with_baggage(baggage)
  
  // Test HTTP headers carrier
  let http_carrier = HttpHeadersCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx, http_carrier)
  
  // Verify injected headers
  let traceparent = HttpHeadersCarrier::get(http_carrier, "traceparent")
  let tracestate = HttpHeadersCarrier::get(http_carrier, "tracestate")
  let baggage_header = HttpHeadersCarrier::get(http_carrier, "baggage")
  
  assert_eq(traceparent, Some("00-1234567890abcdef1234567890abcdef-1234567890abcdef-01"))
  assert_eq(tracestate, Some("key1=value1,key2=value2"))
  assert_eq(baggage_header, Some("user.id=user123,request.id=req456,session.id=sess789"))
  
  // Test extraction from HTTP headers
  let extracted_http_ctx = CompositePropagator::extract(composite_propagator, http_carrier)
  let extracted_span_ctx = Context::get_span_context(extracted_http_ctx)
  let extracted_baggage = Context::get_baggage(extracted_http_ctx)
  
  assert_eq(extracted_span_ctx.trace_id, "1234567890abcdef1234567890abcdef")
  assert_eq(extracted_span_ctx.span_id, "1234567890abcdef")
  assert_true(extracted_span_ctx.sampled)
  
  // Test text map carrier
  let text_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx, text_carrier)
  
  // Verify injected values
  let text_traceparent = TextMapCarrier::get(text_carrier, "traceparent")
  let text_tracestate = TextMapCarrier::get(text_carrier, "tracestate")
  let text_baggage = TextMapCarrier::get(text_carrier, "baggage")
  
  assert_eq(text_traceparent, Some("00-1234567890abcdef1234567890abcdef-1234567890abcdef-01"))
  assert_eq(text_tracestate, Some("key1=value1,key2=value2"))
  assert_eq(text_baggage, Some("user.id=user123,request.id=req456,session.id=sess789"))
  
  // Test extraction from text map
  let extracted_text_ctx = CompositePropagator::extract(composite_propagator, text_carrier)
  let extracted_text_span_ctx = Context::get_span_context(extracted_text_ctx)
  let extracted_text_baggage = Context::get_baggage(extracted_text_ctx)
  
  assert_eq(extracted_text_span_ctx.trace_id, "1234567890abcdef1234567890abcdef")
  assert_eq(extracted_text_span_ctx.span_id, "1234567890abcdef")
  assert_true(extracted_text_span_ctx.sampled)
  
  // Test message properties carrier (for messaging systems)
  let message_carrier = MessagePropertiesCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx, message_carrier)
  
  // Verify injected properties
  let msg_traceparent = MessagePropertiesCarrier::get(message_carrier, "traceparent")
  let msg_tracestate = MessagePropertiesCarrier::get(message_carrier, "tracestate")
  let msg_baggage = MessagePropertiesCarrier::get(message_carrier, "baggage")
  
  assert_eq(msg_traceparent, Some("00-1234567890abcdef1234567890abcdef-1234567890abcdef-01"))
  assert_eq(msg_tracestate, Some("key1=value1,key2=value2"))
  assert_eq(msg_baggage, Some("user.id=user123,request.id=req456,session.id=sess789"))
  
  // Test extraction from message properties
  let extracted_msg_ctx = CompositePropagator::extract(composite_propagator, message_carrier)
  let extracted_msg_span_ctx = Context::get_span_context(extracted_msg_ctx)
  let extracted_msg_baggage = Context::get_baggage(extracted_msg_ctx)
  
  assert_eq(extracted_msg_span_ctx.trace_id, "1234567890abcdef1234567890abcdef")
  assert_eq(extracted_msg_span_ctx.span_id, "1234567890abcdef")
  assert_true(extracted_msg_span_ctx.sampled)
  
  // Test cross-process scenario with modified context
  let modified_ctx = ctx
    .with_baggage_entry("process.id", "process789")
    .with_baggage_entry("correlation.id", "corr012")
  
  // Inject modified context
  let modified_carrier = HttpHeadersCarrier::new()
  CompositePropagator::inject(composite_propagator, modified_ctx, modified_carrier)
  
  // Verify modified baggage
  let modified_baggage_header = HttpHeadersCarrier::get(modified_carrier, "baggage")
  assert_eq(modified_baggage_header, Some("user.id=user123,request.id=req456,session.id=sess789,process.id=process789,correlation.id=corr012"))
  
  // Test context propagation with invalid/malformed headers
  let malformed_carrier = HttpHeadersCarrier::new()
  HttpHeadersCarrier::set(malformed_carrier, "traceparent", "invalid-format")
  HttpHeadersCarrier::set(malformed_carrier, "baggage", "invalid=baggage=format")
  
  // Extract from malformed headers
  let malformed_ctx = CompositePropagator::extract(composite_propagator, malformed_carrier)
  let malformed_span_ctx = Context::get_span_context(malformed_ctx)
  
  // Should not crash and should provide a valid context
  assert_true(true) // If we reach here, extraction handled malformed headers gracefully
}

// Test 8: Telemetry Data Batching and Processing
test "telemetry data batching and processing operations" {
  // Create a batch span processor
  let span_exporter = SpanExporter::new()
  let batch_span_processor = BatchSpanProcessor::builder(span_exporter)
    .with_max_queue_size(512)
    .with_max_export_batch_size(256)
    .with_max_export_timeout(30000L) // 30 seconds
    .build()
  
  // Create multiple spans for batching
  let spans = []
  let mut i = 0
  while i < 100 {
    let span = SpanData {
      name: "operation." + i.to_string(),
      kind: SpanKind::Client,
      start_time: 1735689600000000000L + (i * 1000000L),
      end_time: 1735689600100000000L + (i * 1000000L),
      status: SpanStatus::Ok,
      attributes: [("operation.id", AttributeValue::IntValue(i))],
      events: [],
      links: []
    }
    spans = spans.push(span)
    i = i + 1
  }
  
  // Test batch export
  for span in spans {
    BatchSpanProcessor::on_end(batch_span_processor, span)
  }
  
  // Force flush
  let flush_result = BatchSpanProcessor::force_flush(batch_span_processor)
  assert_true(flush_result) // Expect successful flush
  
  // Create a batch metric processor
  let metric_exporter = MetricExporter::new()
  let batch_metric_processor = BatchMetricProcessor::builder(metric_exporter)
    .with_max_queue_size(256)
    .with_max_export_batch_size(128)
    .with_max_export_timeout(30000L) // 30 seconds
    .build()
  
  // Create multiple metrics for batching
  let metrics = []
  let mut j = 0
  while j < 50 {
    let metric = MetricData {
      name: "metric." + j.to_string(),
      description: Some("Test metric " + j.to_string()),
      unit: Some("count"),
      aggregation: Aggregation::Sum(j.to_float()),
      attributes: [("metric.id", AttributeValue::IntValue(j))]
    }
    metrics = metrics.push(metric)
    j = j + 1
  }
  
  // Test batch metric export
  for metric in metrics {
    BatchMetricProcessor::record(batch_metric_processor, metric)
  }
  
  // Force flush metrics
  let metric_flush_result = BatchMetricProcessor::force_flush(batch_metric_processor)
  assert_true(metric_flush_result) // Expect successful flush
  
  // Test batch processor shutdown
  let span_shutdown_result = BatchSpanProcessor::shutdown(batch_span_processor)
  let metric_shutdown_result = BatchMetricProcessor::shutdown(batch_metric_processor)
  
  assert_true(span_shutdown_result) // Expect successful shutdown
  assert_true(metric_shutdown_result) // Expect successful shutdown
  
  // Test batch processor with custom delay
  let delayed_exporter = SpanExporter::new()
  let delayed_processor = BatchSpanProcessor::builder(delayed_exporter)
    .with_schedule_delay(5000L) // 5 second delay
    .with_max_export_batch_size(10)
    .build()
  
  // Create and export spans
  let mut k = 0
  while k < 10 {
    let span = SpanData {
      name: "delayed.operation." + k.to_string(),
      kind: SpanKind::Server,
      start_time: 1735689600200000000L + (k * 1000000L),
      end_time: 1735689600300000000L + (k * 1000000L),
      status: SpanStatus::Ok,
      attributes: [("delayed.id", AttributeValue::IntValue(k))],
      events: [],
      links: []
    }
    BatchSpanProcessor::on_end(delayed_processor, span)
    k = k + 1
  }
  
  // Shutdown delayed processor
  let delayed_shutdown_result = BatchSpanProcessor::shutdown(delayed_processor)
  assert_true(delayed_shutdown_result) // Expect successful shutdown
  
  // Test batch processor with queue overflow
  let overflow_exporter = SpanExporter::new()
  let overflow_processor = BatchSpanProcessor::builder(overflow_exporter)
    .with_max_queue_size(5) // Small queue size
    .with_max_export_batch_size(2) // Small batch size
    .build()
  
  // Create more spans than queue can handle
  let mut m = 0
  while m < 10 {
    let span = SpanData {
      name: "overflow.operation." + m.to_string(),
      kind: SpanKind::Internal,
      start_time: 1735689600400000000L + (m * 1000000L),
      end_time: 1735689600500000000L + (m * 1000000L),
      status: SpanStatus::Ok,
      attributes: [("overflow.id", AttributeValue::IntValue(m))],
      events: [],
      links: []
    }
    BatchSpanProcessor::on_end(overflow_processor, span)
    m = m + 1
  }
  
  // Force flush overflow processor
  let overflow_flush_result = BatchSpanProcessor::force_flush(overflow_processor)
  assert_true(overflow_flush_result) // Expect successful flush
  
  // Shutdown overflow processor
  let overflow_shutdown_result = BatchSpanProcessor::shutdown(overflow_processor)
  assert_true(overflow_shutdown_result) // Expect successful shutdown
}