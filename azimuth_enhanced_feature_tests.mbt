// Azimuth Enhanced Feature Test Suite
// This file contains enhanced MoonBit test cases focusing on advanced features and telemetry integration

// Test 1: Advanced String Processing and Pattern Matching
test "advanced string processing with telemetry context" {
  let trace_id = "azimuth-trace-12345"
  let service_name = "payment-service"
  let operation_name = "process-payment"
  
  // Extract trace components
  let trace_prefix = trace_id.substring(0, 7)
  let trace_suffix = trace_id.substring(trace_id.length() - 5, 5)
  
  assert_eq(trace_prefix, "azimuth")
  assert_eq(trace_suffix, "12345")
  
  // Build telemetry context string
  let telemetry_context = trace_id + "|" + service_name + "|" + operation_name
  let context_parts = telemetry_context.split("|")
  
  assert_eq(context_parts.length(), 3)
  assert_eq(context_parts[0], trace_id)
  assert_eq(context_parts[1], service_name)
  assert_eq(context_parts[2], operation_name)
  
  // Pattern matching on service types
  let service_category = match service_name {
    s if s.contains("payment") => "financial"
    s if s.contains("auth") => "security"
    s if s.contains("notification") => "communication"
    _ => "general"
  }
  
  assert_eq(service_category, "financial")
  
  // String transformation for telemetry formatting
  let formatted_operation = operation_name.replace("-", " ").to_title_case()
  assert_eq(formatted_operation, "Process Payment")
}

// Test 2: Telemetry Metric Collection and Aggregation
test "telemetry metric collection and aggregation" {
  // Define metric types
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // Define metric point
  type MetricPoint = {
    name: String,
    metric_type: MetricType,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Create test metrics
  let metrics = [
    { name: "request_count", metric_type: MetricType::Counter, value: 100.0, timestamp: 1000, tags: [("method", "GET"), ("status", "200")] },
    { name: "response_time", metric_type: MetricType::Histogram, value: 150.5, timestamp: 1005, tags: [("method", "GET"), ("status", "200")] },
    { name: "active_connections", metric_type: MetricType::Gauge, value: 25.0, timestamp: 1010, tags: [("service", "api")] },
    { name: "request_count", metric_type: MetricType::Counter, value: 50.0, timestamp: 1015, tags: [("method", "POST"), ("status", "201")] },
    { name: "error_rate", metric_type: MetricType::Summary, value: 0.05, timestamp: 1020, tags: [("service", "api")] }
  ]
  
  // Filter metrics by type
  let counters = metrics.filter(fn(m) { match m.metric_type { MetricType::Counter => true, _ => false } })
  let gauges = metrics.filter(fn(m) { match m.metric_type { MetricType::Gauge => true, _ => false } })
  
  assert_eq(counters.length(), 2)
  assert_eq(gauges.length(), 1)
  
  // Aggregate counter values
  let total_counter_value = counters.reduce(fn(acc, m) { acc + m.value }, 0.0)
  assert_eq(total_counter_value, 150.0)
  
  // Group metrics by name
  let grouped_metrics = metrics.reduce(fn(acc, m) {
    let existing = match Map::get(acc, m.name) {
      Some(group) => group
      None => []
    }
    Map::insert(acc, m.name, existing.push(m))
  }, Map::empty())
  
  let request_count_metrics = match Map::get(grouped_metrics, "request_count") {
    Some(metrics) => metrics
    None => []
  }
  
  assert_eq(request_count_metrics.length(), 2)
  
  // Calculate average for specific metric type
  let avg_response_time = match metrics.find(fn(m) { m.name == "response_time" }) {
    Some(metric) => metric.value
    None => 0.0
  }
  
  assert_eq(avg_response_time, 150.5)
}

// Test 3: Distributed Tracing Context Propagation
test "distributed tracing context propagation" {
  // Define span context
  type SpanContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    flags: Int
  }
  
  // Create root span
  let root_span = {
    trace_id: "trace-12345",
    span_id: "span-abcde",
    parent_span_id: None,
    baggage: [("user.id", "user123"), ("request.id", "req456")],
    flags: 1
  }
  
  // Create child span
  let create_child_span = fn(parent: SpanContext, operation_name: String) {
    {
      trace_id: parent.trace_id,
      span_id: "span-" + operation_name.hash().to_string().substring(0, 5),
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage,
      flags: parent.flags
    }
  }
  
  // Create child spans
  let child_span1 = create_child_span(root_span, "database-query")
  let child_span2 = create_child_span(root_span, "cache-lookup")
  
  // Verify trace propagation
  assert_eq(child_span1.trace_id, root_span.trace_id)
  assert_eq(child_span2.trace_id, root_span.trace_id)
  assert_eq(child_span1.parent_span_id, Some(root_span.span_id))
  assert_eq(child_span2.parent_span_id, Some(root_span.span_id))
  
  // Verify baggage propagation
  assert_eq(child_span1.baggage.length(), 2)
  assert_eq(child_span2.baggage.length(), 2)
  assert_true(child_span1.baggage.any(fn(item) { item.0 == "user.id" && item.1 == "user123" }))
  
  // Add baggage to child span
  let add_baggage = fn(span: SpanContext, key: String, value: String) {
    { span with baggage = span.baggage.push((key, value)) }
  }
  
  let enriched_child1 = add_baggage(child_span1, "db.table", "users")
  assert_eq(enriched_child1.baggage.length(), 3)
  assert_true(enriched_child1.baggage.any(fn(item) { item.0 == "db.table" && item.1 == "users" }))
  
  // Verify baggage doesn't affect parent
  assert_eq(root_span.baggage.length(), 2)
  assert_false(root_span.baggage.any(fn(item) { item.0 == "db.table" }))
}

// Test 4: Telemetry Sampling Strategies
test "telemetry sampling strategies" {
  // Define sampling decision
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
    ParentBased(Box[SamplerType])
  }
  
  // Create sampling decision function
  let make_sampling_decision = fn(sampler: SamplerType, trace_id: String, parent_decision: Option[SamplingDecision]) {
    match sampler {
      SamplerType::AlwaysOn => SamplingDecision::RecordAndSample
      SamplerType::AlwaysOff => SamplingDecision::Drop
      SamplerType::TraceIdRatio(ratio) => {
        // Simple hash-based sampling
        let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
        let normalized = (hash % 100) as Float / 100.0
        if normalized <= ratio {
          SamplingDecision::RecordAndSample
        } else {
          SamplingDecision::RecordOnly
        }
      }
      SamplerType::ParentBased(base_sampler) => {
        match parent_decision {
          Some(SamplingDecision::Drop) => SamplingDecision::Drop
          Some(SamplingDecision::RecordOnly) => SamplingDecision::RecordOnly
          Some(SamplingDecision::RecordAndSample) => SamplingDecision::RecordAndSample
          None => make_sampling_decision(base_sampler, trace_id, None)
        }
      }
    }
  }
  
  // Test always on sampler
  let always_on_result = make_sampling_decision(SamplerType::AlwaysOn, "trace-123", None)
  assert_eq(always_on_result, SamplingDecision::RecordAndSample)
  
  // Test always off sampler
  let always_off_result = make_sampling_decision(SamplerType::AlwaysOff, "trace-456", None)
  assert_eq(always_off_result, SamplingDecision::Drop)
  
  // Test trace ID ratio sampler
  let ratio_result1 = make_sampling_decision(SamplerType::TraceIdRatio(0.5), "trace-789", None)
  let ratio_result2 = make_sampling_decision(SamplerType::TraceIdRatio(0.5), "trace-999", None)
  
  // Results depend on hash calculation, but should be either RecordAndSample or RecordOnly
  assert_true(ratio_result1 == SamplingDecision::RecordAndSample || ratio_result1 == SamplingDecision::RecordOnly)
  assert_true(ratio_result2 == SamplingDecision::RecordAndSample || ratio_result2 == SamplingDecision::RecordOnly)
  
  // Test parent-based sampler
  let parent_based_sampler = SamplerType::ParentBased(Box::new(SamplerType::AlwaysOn))
  let parent_based_result1 = make_sampling_decision(parent_based_sampler, "trace-111", Some(SamplingDecision::RecordAndSample))
  let parent_based_result2 = make_sampling_decision(parent_based_sampler, "trace-222", Some(SamplingDecision::Drop))
  let parent_based_result3 = make_sampling_decision(parent_based_sampler, "trace-333", None)
  
  assert_eq(parent_based_result1, SamplingDecision::RecordAndSample)
  assert_eq(parent_based_result2, SamplingDecision::Drop)
  assert_eq(parent_based_result3, SamplingDecision::RecordAndSample)
}

// Test 5: Telemetry Configuration Management
test "telemetry configuration management" {
  // Define configuration types
  type ExporterConfig = {
    endpoint: String,
    protocol: String,
    timeout_ms: Int,
    retry_count: Int
  }
  
  type SamplerConfig = {
    sampler_type: String,
    ratio: Option[Float]
  }
  
  type TelemetryConfig = {
    service_name: String,
    service_version: String,
    environment: String,
    exporter: ExporterConfig,
    sampler: SamplerConfig,
    batch_size: Int,
    flush_interval_ms: Int
  }
  
  // Create default configuration
  let default_config = {
    service_name: "unknown-service",
    service_version: "1.0.0",
    environment: "development",
    exporter: {
      endpoint: "http://localhost:4317",
      protocol: "grpc",
      timeout_ms: 30000,
      retry_count: 3
    },
    sampler: {
      sampler_type: "always_on",
      ratio: None
    },
    batch_size: 512,
    flush_interval_ms: 5000
  }
  
  // Configuration validation function
  let validate_config = fn(config: TelemetryConfig) {
    let errors = []
    
    if config.service_name == "" {
      errors = errors.push("service_name cannot be empty")
    }
    
    if config.service_version == "" {
      errors = errors.push("service_version cannot be empty")
    }
    
    if config.exporter.endpoint == "" {
      errors = errors.push("exporter endpoint cannot be empty")
    }
    
    if config.exporter.timeout_ms <= 0 {
      errors = errors.push("exporter timeout must be positive")
    }
    
    if config.batch_size <= 0 {
      errors = errors.push("batch_size must be positive")
    }
    
    match config.sampler.sampler_type {
      "trace_id_ratio" => {
        match config.sampler.ratio {
          Some(ratio) => {
            if ratio <= 0.0 || ratio > 1.0 {
              errors = errors.push("sampler ratio must be between 0 and 1")
            }
          }
          None => errors = errors.push("trace_id_ratio sampler requires a ratio")
        }
      }
      "always_on" | "always_off" | "parent_based" => {
        // These samplers don't require ratio
      }
      _ => errors = errors.push("unknown sampler type")
    }
    
    errors
  }
  
  // Test default configuration validation
  let default_errors = validate_config(default_config)
  assert_eq(default_errors.length(), 0)
  
  // Test invalid configuration
  let invalid_config = {
    service_name: "",
    service_version: "1.0.0",
    environment: "production",
    exporter: {
      endpoint: "http://otel-collector:4317",
      protocol: "http",
      timeout_ms: -1,
      retry_count: 3
    },
    sampler: {
      sampler_type: "trace_id_ratio",
      ratio: None
    },
    batch_size: 0,
    flush_interval_ms: 10000
  }
  
  let invalid_errors = validate_config(invalid_config)
  assert_eq(invalid_errors.length(), 4)
  assert_true(invalid_errors.contains("service_name cannot be empty"))
  assert_true(invalid_errors.contains("exporter timeout must be positive"))
  assert_true(invalid_errors.contains("batch_size must be positive"))
  assert_true(invalid_errors.contains("trace_id_ratio sampler requires a ratio"))
  
  // Test valid ratio sampler configuration
  let valid_ratio_config = {
    service_name: "api-service",
    service_version: "2.1.0",
    environment: "production",
    exporter: default_config.exporter,
    sampler: {
      sampler_type: "trace_id_ratio",
      ratio: Some(0.25)
    },
    batch_size: 1024,
    flush_interval_ms: 3000
  }
  
  let valid_ratio_errors = validate_config(valid_ratio_config)
  assert_eq(valid_ratio_errors.length(), 0)
  
  // Test configuration merging
  let merge_configs = fn(base: TelemetryConfig, override: TelemetryConfig) {
    {
      service_name: if override.service_name != "" { override.service_name } else { base.service_name },
      service_version: if override.service_version != "" { override.service_version } else { base.service_version },
      environment: if override.environment != "" { override.environment } else { base.environment },
      exporter: {
        endpoint: if override.exporter.endpoint != "" { override.exporter.endpoint } else { base.exporter.endpoint },
        protocol: if override.exporter.protocol != "" { override.exporter.protocol } else { base.exporter.protocol },
        timeout_ms: if override.exporter.timeout_ms > 0 { override.exporter.timeout_ms } else { base.exporter.timeout_ms },
        retry_count: override.exporter.retry_count
      },
      sampler: override.sampler,
      batch_size: if override.batch_size > 0 { override.batch_size } else { base.batch_size },
      flush_interval_ms: if override.flush_interval_ms > 0 { override.flush_interval_ms } else { base.flush_interval_ms }
    }
  }
  
  let partial_override = {
    service_name: "payment-service",
    service_version: "",
    environment: "production",
    exporter: {
      endpoint: "http://otel-collector.prod:4317",
      protocol: "",
      timeout_ms: 60000,
      retry_count: 5
    },
    sampler: {
      sampler_type: "parent_based",
      ratio: None
    },
    batch_size: 2048,
    flush_interval_ms: 0
  }
  
  let merged_config = merge_configs(default_config, partial_override)
  assert_eq(merged_config.service_name, "payment-service")
  assert_eq(merged_config.service_version, "1.0.0")  // From default
  assert_eq(merged_config.environment, "production")
  assert_eq(merged_config.exporter.endpoint, "http://otel-collector.prod:4317")
  assert_eq(merged_config.exporter.protocol, "grpc")  // From default
  assert_eq(merged_config.exporter.timeout_ms, 60000)
  assert_eq(merged_config.exporter.retry_count, 5)
  assert_eq(merged_config.sampler.sampler_type, "parent_based")
  assert_eq(merged_config.batch_size, 2048)
  assert_eq(merged_config.flush_interval_ms, 5000)  // From default
}

// Test 6: Telemetry Data Transformation and Enrichment
test "telemetry data transformation and enrichment" {
  // Define telemetry event types
  enum EventType {
    SpanStart
    SpanEnd
    MetricRecord
    LogRecord
  }
  
  // Define telemetry event
  type TelemetryEvent = {
    event_type: EventType,
    timestamp: Int,
    trace_id: String,
    span_id: String,
    attributes: Array[(String, String)]
  }
  
  // Attribute enrichment functions
  let enrich_with_host_info = fn(event: TelemetryEvent) {
    let host_attributes = [
      ("host.name", "prod-server-01"),
      ("host.ip", "10.0.1.100"),
      ("host.os", "linux")
    ]
    { event with attributes = event.attributes + host_attributes }
  }
  
  let enrich_with_service_info = fn(event: TelemetryEvent, service_name: String, service_version: String) {
    let service_attributes = [
      ("service.name", service_name),
      ("service.version", service_version),
      ("service.instance.id", service_name + "-" + event.span_id)
    ]
    { event with attributes = event.attributes + service_attributes }
  }
  
  let enrich_with_environment = fn(event: TelemetryEvent, environment: String) {
    let env_attributes = [
      ("deployment.environment", environment),
      ("telemetry.sdk.language", "moonbit"),
      ("telemetry.sdk.name", "azimuth")
    ]
    { event with attributes = event.attributes + env_attributes }
  }
  
  // Create base event
  let base_event = {
    event_type: EventType::SpanStart,
    timestamp: 1640995200000,  // 2022-01-01 00:00:00 UTC
    trace_id: "trace-12345",
    span_id: "span-abcde",
    attributes: [
      ("operation.name", "process-payment"),
      ("user.id", "user123")
    ]
  }
  
  // Apply enrichments
  let enriched_event = base_event
    |> enrich_with_host_info
    |> fn(e) { enrich_with_service_info(e, "payment-service", "2.1.0") }
    |> fn(e) { enrich_with_environment(e, "production") }
  
  // Verify enrichment
  assert_eq(enriched_event.attributes.length(), 11)
  assert_true(enriched_event.attributes.any(fn(attr) { attr.0 == "host.name" && attr.1 == "prod-server-01" }))
  assert_true(enriched_event.attributes.any(fn(attr) { attr.0 == "service.name" && attr.1 == "payment-service" }))
  assert_true(enriched_event.attributes.any(fn(attr) { attr.0 == "deployment.environment" && attr.1 == "production" }))
  
  // Attribute filtering
  let filter_attributes = fn(event: TelemetryEvent, allowed_prefixes: Array[String]) {
    let filtered = event.attributes.filter(fn(attr) {
      let (key, _) = attr
      allowed_prefixes.any(fn(prefix) { key.starts_with(prefix) })
    })
    { event with attributes = filtered }
  }
  
  let service_only_event = filter_attributes(enriched_event, ["service.", "operation."])
  assert_eq(service_only_event.attributes.length(), 4)
  assert_true(service_only_event.attributes.any(fn(attr) { attr.0 == "service.name" }))
  assert_true(service_only_event.attributes.any(fn(attr) { attr.0 == "service.version" }))
  assert_true(service_only_event.attributes.any(fn(attr) { attr.0 == "service.instance.id" }))
  assert_true(service_only_event.attributes.any(fn(attr) { attr.0 == "operation.name" }))
  
  // Attribute transformation
  let transform_attribute_names = fn(event: TelemetryEvent, transformations: Array[(String, String)]) {
    let transformed = event.attributes.map(fn(attr) {
      let (key, value) = attr
      let new_key = match transformations.find(fn(t) { t.0 == key }) {
        Some((_, new_name)) => new_name
        None => key
      }
      (new_key, value)
    })
    { event with attributes = transformed }
  }
  
  let snake_case_transformations = [
    ("operation.name", "operation_name"),
    ("user.id", "user_id"),
    ("service.name", "service_name"),
    ("service.version", "service_version"),
    ("host.name", "host_name"),
    ("deployment.environment", "deployment_environment")
  ]
  
  let snake_case_event = transform_attribute_names(enriched_event, snake_case_transformations)
  assert_true(snake_case_event.attributes.any(fn(attr) { attr.0 == "operation_name" }))
  assert_true(snake_case_event.attributes.any(fn(attr) { attr.0 == "user_id" }))
  assert_false(snake_case_event.attributes.any(fn(attr) { attr.0 == "operation.name" }))
}

// Test 7: Telemetry Error Handling and Recovery
test "telemetry error handling and recovery" {
  // Define error types
  enum TelemetryError {
    NetworkError(String)
    SerializationError(String)
    ConfigurationError(String)
    RateLimitError(Int)
    ResourceExhaustedError(String)
    InvalidArgumentError(String)
  }
  
  // Define error severity
  enum ErrorSeverity {
    Debug
    Info
    Warning
    Error
    Critical
  }
  
  // Error classification function
  let classify_error = fn(error: TelemetryError) {
    match error {
      TelemetryError::NetworkError(_) => (ErrorSeverity::Error, "network")
      TelemetryError::SerializationError(_) => (ErrorSeverity::Warning, "serialization")
      TelemetryError::ConfigurationError(_) => (ErrorSeverity::Critical, "configuration")
      TelemetryError::RateLimitError(_) => (ErrorSeverity::Warning, "rate_limit")
      TelemetryError::ResourceExhaustedError(_) => (ErrorSeverity::Error, "resource")
      TelemetryError::InvalidArgumentError(_) => (ErrorSeverity::Info, "argument")
    }
  }
  
  // Define recovery strategy
  enum RecoveryStrategy {
    Retry(Int, Int)  // max_attempts, delay_ms
    Backoff(Int, Int, Int)  // max_attempts, initial_delay_ms, max_delay_ms
    CircuitBreaker(Int, Int)  // failure_threshold, recovery_timeout_ms
    Fallback(() -> Unit)
    Ignore
    Fail
  }
  
  // Recovery strategy selection
  let select_recovery_strategy = fn(error: TelemetryError) {
    match error {
      TelemetryError::NetworkError(_) => RecoveryStrategy::Backoff(3, 1000, 10000)
      TelemetryError::SerializationError(_) => RecoveryStrategy::Retry(2, 500)
      TelemetryError::ConfigurationError(_) => RecoveryStrategy::Fail
      TelemetryError::RateLimitError(delay_ms) => RecoveryStrategy::Retry(1, delay_ms)
      TelemetryError::ResourceExhaustedError(_) => RecoveryStrategy::CircuitBreaker(5, 60000)
      TelemetryError::InvalidArgumentError(_) => RecoveryStrategy::Ignore
    }
  }
  
  // Test error classification
  let network_error = TelemetryError::NetworkError("Connection timeout")
  let (network_severity, network_category) = classify_error(network_error)
  assert_eq(network_severity, ErrorSeverity::Error)
  assert_eq(network_category, "network")
  
  let config_error = TelemetryError::ConfigurationError("Missing service name")
  let (config_severity, config_category) = classify_error(config_error)
  assert_eq(config_severity, ErrorSeverity::Critical)
  assert_eq(config_category, "configuration")
  
  // Test recovery strategy selection
  let network_recovery = select_recovery_strategy(network_error)
  assert_eq(network_recovery, RecoveryStrategy::Backoff(3, 1000, 10000))
  
  let config_recovery = select_recovery_strategy(config_error)
  assert_eq(config_recovery, RecoveryStrategy::Fail)
  
  let rate_limit_error = TelemetryError::RateLimitError(5000)
  let rate_limit_recovery = select_recovery_strategy(rate_limit_error)
  assert_eq(rate_limit_recovery, RecoveryStrategy::Retry(1, 5000))
  
  // Error recovery execution
  let execute_recovery = fn(strategy: RecoveryStrategy, error: TelemetryError) {
    match strategy {
      RecoveryStrategy::Retry(max_attempts, delay_ms) => {
        "Will retry up to " + max_attempts.to_string() + " times with " + delay_ms.to_string() + "ms delay"
      }
      RecoveryStrategy::Backoff(max_attempts, initial_delay, max_delay) => {
        "Will retry with exponential backoff up to " + max_attempts.to_string() + " times"
      }
      RecoveryStrategy::CircuitBreaker(threshold, timeout) => {
        "Circuit breaker will open after " + threshold.to_string() + " failures"
      }
      RecoveryStrategy::Fallback(_) => {
        "Will execute fallback operation"
      }
      RecoveryStrategy::Ignore => {
        "Error will be ignored"
      }
      RecoveryStrategy::Fail => {
        "Operation will fail immediately"
      }
    }
  }
  
  let network_recovery_message = execute_recovery(network_recovery, network_error)
  assert_eq(network_recovery_message, "Will retry with exponential backoff up to 3 times")
  
  let config_recovery_message = execute_recovery(config_recovery, config_error)
  assert_eq(config_recovery_message, "Operation will fail immediately")
  
  // Error aggregation and reporting
  type ErrorReport = {
    total_errors: Int,
    errors_by_category: Map[String, Int],
    errors_by_severity: Map[ErrorSeverity, Int],
    most_recent_error: Option[TelemetryError]
  }
  
  let update_error_report = fn(report: ErrorReport, error: TelemetryError) {
    let (severity, category) = classify_error(error)
    
    let category_counts = match Map::get(report.errors_by_category, category) {
      Some(count) => Map::insert(report.errors_by_category, category, count + 1)
      None => Map::insert(report.errors_by_category, category, 1)
    }
    
    let severity_counts = match Map::get(report.errors_by_severity, severity) {
      Some(count) => Map::insert(report.errors_by_severity, severity, count + 1)
      None => Map::insert(report.errors_by_severity, severity, 1)
    }
    
    {
      total_errors: report.total_errors + 1,
      errors_by_category: category_counts,
      errors_by_severity: severity_counts,
      most_recent_error: Some(error)
    }
  }
  
  // Test error reporting
  let initial_report = {
    total_errors: 0,
    errors_by_category: Map::empty(),
    errors_by_severity: Map::empty(),
    most_recent_error: None
  }
  
  let report1 = update_error_report(initial_report, network_error)
  assert_eq(report1.total_errors, 1)
  
  let report2 = update_error_report(report1, config_error)
  assert_eq(report2.total_errors, 2)
  
  let report3 = update_error_report(report2, rate_limit_error)
  assert_eq(report3.total_errors, 3)
  
  // Verify error counts by category
  let network_count = match Map::get(report3.errors_by_category, "network") {
    Some(count) => count
    None => 0
  }
  assert_eq(network_count, 1)
  
  let config_count = match Map::get(report3.errors_by_category, "configuration") {
    Some(count) => count
    None => 0
  }
  assert_eq(config_count, 1)
  
  // Verify error counts by severity
  let critical_count = match Map::get(report3.errors_by_severity, ErrorSeverity::Critical) {
    Some(count) => count
    None => 0
  }
  assert_eq(critical_count, 1)
  
  let error_count = match Map::get(report3.errors_by_severity, ErrorSeverity::Error) {
    Some(count) => count
    None => 0
  }
  assert_eq(error_count, 1)
}

// Test 8: Telemetry Performance Optimization
test "telemetry performance optimization" {
  // Define performance metrics
  type PerformanceMetrics = {
    cpu_usage: Float,
    memory_usage: Float,
    batch_processing_time: Int,
    throughput: Float,
    latency_p50: Float,
    latency_p95: Float,
    latency_p99: Float
  }
  
  // Define optimization strategies
  enum OptimizationStrategy {
    IncreaseBatchSize
    DecreaseBatchSize
    IncreaseFlushInterval
    DecreaseFlushInterval
    EnableCompression
    DisableCompression
    EnableAsyncProcessing
    DisableAsyncProcessing
  }
  
  // Performance evaluation function
  let evaluate_performance = fn(metrics: PerformanceMetrics) {
    let issues = []
    
    if metrics.cpu_usage > 80.0 {
      issues = issues.push("high_cpu_usage")
    }
    
    if metrics.memory_usage > 80.0 {
      issues = issues.push("high_memory_usage")
    }
    
    if metrics.batch_processing_time > 1000 {
      issues = issues.push("slow_batch_processing")
    }
    
    if metrics.throughput < 1000.0 {
      issues = issues.push("low_throughput")
    }
    
    if metrics.latency_p95 > 100.0 {
      issues = issues.push("high_latency_p95")
    }
    
    if metrics.latency_p99 > 500.0 {
      issues = issues.push("high_latency_p99")
    }
    
    issues
  }
  
  // Optimization recommendation function
  let recommend_optimizations = fn(performance_issues: Array[String]) {
    let recommendations = []
    
    for issue in performance_issues {
      match issue {
        "high_cpu_usage" => {
          recommendations = recommendations.push(OptimizationStrategy::IncreaseBatchSize)
          recommendations = recommendations.push(OptimizationStrategy::EnableAsyncProcessing)
        }
        "high_memory_usage" => {
          recommendations = recommendations.push(OptimizationStrategy::DecreaseBatchSize)
          recommendations = recommendations.push(OptimizationStrategy::IncreaseFlushInterval)
        }
        "slow_batch_processing" => {
          recommendations = recommendations.push(OptimizationStrategy::DecreaseBatchSize)
          recommendations = recommendations.push(OptimizationStrategy::EnableAsyncProcessing)
        }
        "low_throughput" => {
          recommendations = recommendations.push(OptimizationStrategy::IncreaseBatchSize)
          recommendations = recommendations.push(OptimizationStrategy::EnableCompression)
        }
        "high_latency_p95" | "high_latency_p99" => {
          recommendations = recommendations.push(OptimizationStrategy::EnableAsyncProcessing)
          recommendations = recommendations.push(OptimizationStrategy::DecreaseFlushInterval)
        }
        _ => {}
      }
    }
    
    recommendations
  }
  
  // Test with good performance metrics
  let good_metrics = {
    cpu_usage: 45.0,
    memory_usage: 60.0,
    batch_processing_time: 500,
    throughput: 5000.0,
    latency_p50: 10.0,
    latency_p95: 50.0,
    latency_p99: 100.0
  }
  
  let good_issues = evaluate_performance(good_metrics)
  assert_eq(good_issues.length(), 0)
  
  let good_recommendations = recommend_optimizations(good_issues)
  assert_eq(good_recommendations.length(), 0)
  
  // Test with poor performance metrics
  let poor_metrics = {
    cpu_usage: 85.0,
    memory_usage: 90.0,
    batch_processing_time: 1500,
    throughput: 500.0,
    latency_p50: 20.0,
    latency_p95: 150.0,
    latency_p99: 600.0
  }
  
  let poor_issues = evaluate_performance(poor_metrics)
  assert_eq(poor_issues.length(), 6)
  assert_true(poor_issues.contains("high_cpu_usage"))
  assert_true(poor_issues.contains("high_memory_usage"))
  assert_true(poor_issues.contains("slow_batch_processing"))
  assert_true(poor_issues.contains("low_throughput"))
  assert_true(poor_issues.contains("high_latency_p95"))
  assert_true(poor_issues.contains("high_latency_p99"))
  
  let poor_recommendations = recommend_optimizations(poor_issues)
  assert_eq(poor_recommendations.length(), 8)
  assert_true(poor_recommendations.any(fn(r) { r == OptimizationStrategy::IncreaseBatchSize }))
  assert_true(poor_recommendations.any(fn(r) { r == OptimizationStrategy::DecreaseBatchSize }))
  assert_true(poor_recommendations.any(fn(r) { r == OptimizationStrategy::EnableAsyncProcessing }))
  
  // Optimization impact simulation
  let apply_optimization = fn(metrics: PerformanceMetrics, optimization: OptimizationStrategy) {
    match optimization {
      OptimizationStrategy::IncreaseBatchSize => {
        { metrics with 
          cpu_usage = metrics.cpu_usage * 0.9,
          throughput = metrics.throughput * 1.2,
          latency_p95 = metrics.latency_p95 * 1.1,
          latency_p99 = metrics.latency_p99 * 1.15
        }
      }
      OptimizationStrategy::DecreaseBatchSize => {
        { metrics with 
          memory_usage = metrics.memory_usage * 0.8,
          batch_processing_time = metrics.batch_processing_time / 2,
          throughput = metrics.throughput * 0.9
        }
      }
      OptimizationStrategy::EnableAsyncProcessing => {
        { metrics with 
          cpu_usage = metrics.cpu_usage * 0.95,
          latency_p50 = metrics.latency_p50 * 0.7,
          latency_p95 = metrics.latency_p95 * 0.6,
          latency_p99 = metrics.latency_p99 * 0.5
        }
      }
      OptimizationStrategy::EnableCompression => {
        { metrics with 
          cpu_usage = metrics.cpu_usage * 1.1,
          throughput = metrics.throughput * 1.5
        }
      }
      _ => metrics
    }
  }
  
  // Test optimization impact
  let optimized_metrics = poor_metrics
    |> fn(m) { apply_optimization(m, OptimizationStrategy::IncreaseBatchSize) }
    |> fn(m) { apply_optimization(m, OptimizationStrategy::EnableAsyncProcessing) }
    |> fn(m) { apply_optimization(m, OptimizationStrategy::EnableCompression) }
  
  let optimized_issues = evaluate_performance(optimized_metrics)
  assert_true(optimized_issues.length() < poor_issues.length())
  
  // Verify specific improvements
  assert_true(optimized_metrics.cpu_usage < poor_metrics.cpu_usage)
  assert_true(optimized_metrics.throughput > poor_metrics.throughput)
  assert_true(optimized_metrics.latency_p95 < poor_metrics.latency_p95)
  assert_true(optimized_metrics.latency_p99 < poor_metrics.latency_p99)
}

// Test 9: Telemetry Data Retention and Archival
test "telemetry data retention and archival" {
  // Define data retention policies
  type RetentionPolicy = {
    name: String,
    retention_days: Int,
    archival_enabled: Bool,
    compression_enabled: Bool,
    conditions: Array[String]
  }
  
  // Define telemetry record
  type TelemetryRecord = {
    id: String,
    timestamp: Int,
    data: String,
    size_bytes: Int,
    tags: Array[String]
  }
  
  // Create retention policies
  let policies = [
    {
      name: "critical-traces",
      retention_days: 90,
      archival_enabled: true,
      compression_enabled: true,
      conditions: ["trace.type=critical", "service.name=payment"]
    },
    {
      name: "standard-metrics",
      retention_days: 30,
      archival_enabled: true,
      compression_enabled: true,
      conditions: ["record.type=metrics"]
    },
    {
      name: "debug-logs",
      retention_days: 7,
      archival_enabled: false,
      compression_enabled: false,
      conditions: ["log.level=debug", "environment=development"]
    },
    {
      name: "error-logs",
      retention_days: 45,
      archival_enabled: true,
      compression_enabled: true,
      conditions: ["log.level=error", "log.level=critical"]
    }
  ]
  
  // Policy matching function
  let match_policy = fn(record: TelemetryRecord, policy: RetentionPolicy) {
    for condition in policy.conditions {
      let parts = condition.split("=")
      if parts.length() == 2 {
        let key = parts[0]
        let value = parts[1]
        
        let match_found = match key {
          "trace.type" => record.tags.any(fn(tag) { tag == "trace.type:" + value })
          "service.name" => record.tags.any(fn(tag) { tag == "service.name:" + value })
          "record.type" => record.tags.any(fn(tag) { tag == "record.type:" + value })
          "log.level" => record.tags.any(fn(tag) { tag == "log.level:" + value })
          "environment" => record.tags.any(fn(tag) { tag == "environment:" + value })
          _ => false
        }
        
        if not match_found {
          return false
        }
      }
    }
    true
  }
  
  // Create test records
  let current_time = 1640995200000  // 2022-01-01 00:00:00 UTC
  let records = [
    {
      id: "trace-001",
      timestamp: current_time,
      data: "critical payment trace data",
      size_bytes: 1024,
      tags: ["trace.type:critical", "service.name:payment", "user.id:123"]
    },
    {
      id: "metric-001",
      timestamp: current_time - 86400000,  // 1 day ago
      data: "cpu usage metrics",
      size_bytes: 512,
      tags: ["record.type:metrics", "service.name:api", "host:server1"]
    },
    {
      id: "log-001",
      timestamp: current_time - 172800000,  // 2 days ago
      data: "debug log message",
      size_bytes: 256,
      tags: ["log.level:debug", "environment:development", "service.name:auth"]
    },
    {
      id: "log-002",
      timestamp: current_time - 2592000000,  // 30 days ago
      data: "error log message",
      size_bytes: 384,
      tags: ["log.level:error", "environment:production", "service.name:payment"]
    },
    {
      id: "trace-002",
      timestamp: current_time - 7776000000,  // 90 days ago
      data: "standard trace data",
      size_bytes: 768,
      tags: ["trace.type:standard", "service.name:user", "request.id:456"]
    }
  ]
  
  // Test policy matching
  let critical_trace = records[0]
  let matched_policies = policies.filter(fn(policy) { match_policy(critical_trace, policy) })
  assert_eq(matched_policies.length(), 1)
  assert_eq(matched_policies[0].name, "critical-traces")
  
  let debug_log = records[2]
  let debug_matched_policies = policies.filter(fn(policy) { match_policy(debug_log, policy) })
  assert_eq(debug_matched_policies.length(), 1)
  assert_eq(debug_matched_policies[0].name, "debug-logs")
  
  let error_log = records[3]
  let error_matched_policies = policies.filter(fn(policy) { match_policy(error_log, policy) })
  assert_eq(error_matched_policies.length(), 1)
  assert_eq(error_matched_policies[0].name, "error-logs")
  
  // Record retention evaluation
  let should_retain = fn(record: TelemetryRecord, reference_time: Int) {
    let matched_policies = policies.filter(fn(policy) { match_policy(record, policy) })
    
    if matched_policies.length() == 0 {
      return false  // No matching policy, don't retain
    }
    
    // Check if any matching policy allows retention
    matched_policies.any(fn(policy) {
      let age_days = (reference_time - record.timestamp) / 86400000
      age_days <= policy.retention_days
    })
  }
  
  // Test retention evaluation
  assert_true(should_retain(critical_trace, current_time))  // Current time, should retain
  assert_true(should_retain(records[1], current_time))  // 1 day old, should retain
  assert_true(should_retain(records[2], current_time))  // 2 days old, should retain
  assert_true(should_retain(records[3], current_time))  // 30 days old, should retain
  assert_false(should_retain(records[4], current_time))  // 90 days old, should not retain
  
  // Test with future reference time
  let future_time = current_time + 7776000000  // 90 days in the future
  assert_false(should_retain(critical_trace, future_time))  // 90 days old, should not retain
  assert_false(should_retain(records[1], future_time))  // 91 days old, should not retain
  assert_false(should_retain(records[2], future_time))  // 92 days old, should not retain
  assert_false(should_retain(records[3], future_time))  // 120 days old, should not retain
  assert_false(should_retain(records[4], future_time))  // 180 days old, should not retain
  
  // Archival preparation
  let prepare_for_archival = fn(record: TelemetryRecord, policy: RetentionPolicy) {
    if not policy.archival_enabled {
      return None
    }
    
    let archival_record = {
      id: record.id,
      original_timestamp: record.timestamp,
      archival_timestamp: current_time,
      policy_name: policy.name,
      data: if policy.compression_enabled { 
        "[COMPRESSED]" + record.data 
      } else { 
        record.data 
      },
      original_size: record.size_bytes,
      compressed_size: if policy.compression_enabled { 
        record.size_bytes / 2 
      } else { 
        record.size_bytes 
      },
      tags: record.tags
    }
    
    Some(archival_record)
  }
  
  // Test archival preparation
  let critical_policy = policies[0]
  let critical_archival = prepare_for_archival(critical_trace, critical_policy)
  assert_true(critical_archival.is_some())
  
  match critical_archival {
    Some(archival_record) => {
      assert_eq(archival_record.id, "trace-001")
      assert_eq(archival_record.policy_name, "critical-traces")
      assert_eq(archival_record.original_size, 1024)
      assert_eq(archival_record.compressed_size, 512)
      assert_true(archival_record.data.starts_with("[COMPRESSED]"))
    }
    None => assert_true(false)
  }
  
  let debug_policy = policies[2]
  let debug_archival = prepare_for_archival(debug_log, debug_policy)
  assert_true(debug_archival.is_none())  // Archival not enabled for debug logs
}

// Test 10: Telemetry Security and Privacy
test "telemetry security and privacy" {
  // Define data sensitivity levels
  enum SensitivityLevel {
    Public
    Internal
    Confidential
    Restricted
  }
  
  // Define PII types
  enum PIIType {
    Email
    PhoneNumber
    SSN
    CreditCard
    IPAddress
    UserID
  }
  
  // Define telemetry field
  type TelemetryField = {
    name: String,
    value: String,
    sensitivity: SensitivityLevel,
    pii_types: Array[PIIType]
  }
  
  // Define security policies
  type SecurityPolicy = {
    name: String,
    encryption_enabled: Bool,
    anonymization_enabled: Bool,
    access_controls: Array[String],
    retention_rules: Array[(SensitivityLevel, Int)]
  }
  
  // PII detection functions
  let detect_pii_types = fn(value: String) {
    let detected = []
    
    // Simple email detection
    if value.contains("@") && value.contains(".") {
      detected = detected.push(PIIType::Email)
    }
    
    // Simple phone number detection
    if value.regex_match("\\d{3}-\\d{3}-\\d{4}") || value.regex_match("\\d{10}") {
      detected = detected.push(PIIType::PhoneNumber)
    }
    
    // Simple SSN detection
    if value.regex_match("\\d{3}-\\d{2}-\\d{4}") {
      detected = detected.push(PIIType::SSN)
    }
    
    // Simple credit card detection
    if value.regex_match("\\d{4}[- ]?\\d{4}[- ]?\\d{4}[- ]?\\d{4}") {
      detected = detected.push(PIIType::CreditCard)
    }
    
    // Simple IP address detection
    if value.regex_match("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}") {
      detected = detected.push(PIIType::IPAddress)
    }
    
    // User ID detection
    if value.starts_with("user-") || value.starts_with("userid-") {
      detected = detected.push(PIIType::UserID)
    }
    
    detected
  }
  
  // Data anonymization functions
  let anonymize_value = fn(value: String, pii_types: Array[PIIType]) {
    let anonymized = value
    
    for pii_type in pii_types {
      anonymized = match pii_type {
        PIIType::Email => {
          let parts = anonymized.split("@")
          if parts.length() == 2 {
            let username = parts[0]
            let domain = parts[1]
            username.substring(0, 2) + "***@" + domain
          } else {
            "***@***.***"
          }
        }
        PIIType::PhoneNumber => {
          if anonymized.length() >= 4 {
            "***-***-" + anonymized.substring(anonymized.length() - 4, 4)
          } else {
            "***-***-****"
          }
        }
        PIIType::SSN => "***-**-****"
        PIIType::CreditCard => "****-****-****-" + anonymized.substring(anonymized.length() - 4, 4)
        PIIType::IPAddress => "***.***.***.*"
        PIIType::UserID => {
          if anonymized.starts_with("user-") {
            "user-***"
          } else if anonymized.starts_with("userid-") {
            "userid-***"
          } else {
            "***"
          }
        }
      }
    }
    
    anonymized
  }
  
  // Create test fields
  let fields = [
    { name: "user.email", value: "john.doe@example.com", sensitivity: SensitivityLevel::Confidential, pii_types: [] },
    { name: "user.phone", value: "555-123-4567", sensitivity: SensitivityLevel::Confidential, pii_types: [] },
    { name: "user.ssn", value: "123-45-6789", sensitivity: SensitivityLevel::Restricted, pii_types: [] },
    { name: "payment.card", value: "4111-1111-1111-1111", sensitivity: SensitivityLevel::Restricted, pii_types: [] },
    { name: "client.ip", value: "192.168.1.100", sensitivity: SensitivityLevel::Internal, pii_types: [] },
    { name: "user.id", value: "user-12345", sensitivity: SensitivityLevel::Internal, pii_types: [] },
    { name: "request.path", value: "/api/users", sensitivity: SensitivityLevel::Public, pii_types: [] },
    { name: "service.name", value: "payment-service", sensitivity: SensitivityLevel::Public, pii_types: [] }
  ]
  
  // Detect PII in fields
  let fields_with_pii = fields.map(fn(field) {
    let pii_types = detect_pii_types(field.value)
    { field with pii_types = pii_types }
  })
  
  // Verify PII detection
  let email_field = fields_with_pii[0]
  assert_true(email_field.pii_types.contains(PIIType::Email))
  
  let phone_field = fields_with_pii[1]
  assert_true(phone_field.pii_types.contains(PIIType::PhoneNumber))
  
  let ssn_field = fields_with_pii[2]
  assert_true(ssn_field.pii_types.contains(PIIType::SSN))
  
  let card_field = fields_with_pii[3]
  assert_true(card_field.pii_types.contains(PIIType::CreditCard))
  
  let ip_field = fields_with_pii[4]
  assert_true(ip_field.pii_types.contains(PIIType::IPAddress))
  
  let user_id_field = fields_with_pii[5]
  assert_true(user_id_field.pii_types.contains(PIIType::UserID))
  
  let path_field = fields_with_pii[6]
  assert_eq(path_field.pii_types.length(), 0)
  
  let service_field = fields_with_pii[7]
  assert_eq(service_field.pii_types.length(), 0)
  
  // Apply security policy
  let apply_security_policy = fn(fields: Array[TelemetryField], policy: SecurityPolicy) {
    fields.map(fn(field) {
      let secured_value = if policy.anonymization_enabled && field.pii_types.length() > 0 {
        anonymize_value(field.value, field.pii_types)
      } else {
        field.value
      }
      
      { field with value = secured_value }
    })
  }
  
  // Create security policy
  let security_policy = {
    name: "production-security",
    encryption_enabled: true,
    anonymization_enabled: true,
    access_controls: ["role:admin", "role:analyst"],
    retention_rules: [
      (SensitivityLevel::Public, 365),
      (SensitivityLevel::Internal, 180),
      (SensitivityLevel::Confidential, 90),
      (SensitivityLevel::Restricted, 30)
    ]
  }
  
  // Apply security policy
  let secured_fields = apply_security_policy(fields_with_pii, security_policy)
  
  // Verify anonymization
  assert_eq(secured_fields[0].value, "jo***@example.com")  // Email anonymized
  assert_eq(secured_fields[1].value, "***-***-4567")  // Phone anonymized
  assert_eq(secured_fields[2].value, "***-**-****")  // SSN anonymized
  assert_eq(secured_fields[3].value, "****-****-****-1111")  // Credit card anonymized
  assert_eq(secured_fields[4].value, "***.***.***.*")  // IP anonymized
  assert_eq(secured_fields[5].value, "user-***")  // User ID anonymized
  assert_eq(secured_fields[6].value, "/api/users")  // Public field unchanged
  assert_eq(secured_fields[7].value, "payment-service")  // Public field unchanged
  
  // Access control verification
  let check_access = fn(field: TelemetryField, user_roles: Array[String], policy: SecurityPolicy) {
    match field.sensitivity {
      SensitivityLevel::Public => true
      SensitivityLevel::Internal => user_roles.any(fn(role) { policy.access_controls.contains("role:" + role) })
      SensitivityLevel::Confidential => user_roles.any(fn(role) { role == "admin" || role == "analyst" })
      SensitivityLevel::Restricted => user_roles.any(fn(role) { role == "admin" })
    }
  }
  
  // Test access control
  let admin_roles = ["admin"]
  let analyst_roles = ["analyst"]
  let user_roles = ["user"]
  
  assert_true(check_access(secured_fields[0], admin_roles, security_policy))  // Admin can access confidential
  assert_true(check_access(secured_fields[0], analyst_roles, security_policy))  // Analyst can access confidential
  assert_false(check_access(secured_fields[0], user_roles, security_policy))  // User cannot access confidential
  
  assert_true(check_access(secured_fields[2], admin_roles, security_policy))  // Admin can access restricted
  assert_false(check_access(secured_fields[2], analyst_roles, security_policy))  // Analyst cannot access restricted
  assert_false(check_access(secured_fields[2], user_roles, security_policy))  // User cannot access restricted
  
  assert_true(check_access(secured_fields[6], user_roles, security_policy))  // Anyone can access public
}