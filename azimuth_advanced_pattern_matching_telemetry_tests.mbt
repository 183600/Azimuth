// Azimuth Advanced Pattern Matching Telemetry Tests
// This file contains advanced pattern matching test cases for telemetry data processing

// Test 1: Complex Telemetry Data Pattern Matching
test "complex telemetry data pattern matching" {
  // Define complex telemetry data structures
  enum TelemetryData {
    Metric(String, Float, Array[(String, String)])  // name, value, attributes
    Log(String, Int, String, Array[(String, String)])  // message, level, timestamp, attributes
    Span(String, String, String, Int, Int, Array[(String, String)])  // name, trace_id, span_id, start, end, attributes
    Trace(Array[Span])  // collection of spans
    Event(String, Int, Array[(String, String)])  // name, timestamp, attributes
  }
  
  // Advanced pattern matching with guards
  let classify_telemetry = fn(data: TelemetryData) {
    match data {
      TelemetryData::Metric(name, value, attrs) if value > 1000.0 => {
        "high_value_metric: " + name
      }
      TelemetryData::Metric(name, value, attrs) if attrs.length() > 5 => {
        "rich_attribute_metric: " + name
      }
      TelemetryData::Metric(name, _, _) => {
        "standard_metric: " + name
      }
      TelemetryData::Log(message, level, _, attrs) if level >= 4 && message.contains("error") => {
        "critical_error_log"
      }
      TelemetryData::Log(message, level, _, attrs) if level >= 3 && attrs.some(fn(a) { a.0 == "service" }) => {
        "service_warning_log"
      }
      TelemetryData::Log(_, _, _, _) => {
        "standard_log"
      }
      TelemetryData::Span(name, trace_id, _, start, end, attrs) if end - start > 5000 => {
        "long_running_span: " + name
      }
      TelemetryData::Span(name, trace_id, span_id, _, _, attrs) if attrs.some(fn(a) { a.0 == "error" }) => {
        "error_span: " + name
      }
      TelemetryData::Span(_, _, _, _, _, _) => {
        "standard_span"
      }
      TelemetryData::Trace(spans) if spans.length() > 10 => {
        "complex_trace"
      }
      TelemetryData::Trace(spans) if spans.any(fn(s) { match s { TelemetryData::Span(_, _, _, start, end, _) => end - start > 1000; _ => false } }) => {
        "trace_with_long_span"
      }
      TelemetryData::Trace(_) => {
        "standard_trace"
      }
      TelemetryData::Event(name, _, attrs) if attrs.some(fn(a) { a.0 == "alert" && a.1 == "critical" }) => {
        "critical_event: " + name
      }
      TelemetryData::Event(name, _, _) => {
        "standard_event: " + name
      }
    }
  }
  
  // Test high-value metric classification
  let high_metric = TelemetryData::Metric("cpu_usage", 95.5, [("host", "server1")])
  assert_eq(classify_telemetry(high_metric), "standard_metric: cpu_usage")
  
  let very_high_metric = TelemetryData::Metric("memory_usage", 1500.0, [("host", "server1")])
  assert_eq(classify_telemetry(very_high_metric), "high_value_metric: memory_usage")
  
  // Test rich attribute metric
  let rich_metric = TelemetryData::Metric("complex_metric", 50.0, [
    ("service", "api"),
    ("version", "1.2.3"),
    ("environment", "prod"),
    ("region", "us-west"),
    ("datacenter", "dc1"),
    ("rack", "r42")
  ])
  assert_eq(classify_telemetry(rich_metric), "rich_attribute_metric: complex_metric")
  
  // Test critical error log
  let error_log = TelemetryData::Log("Database connection failed", 5, 1640995200, [
    ("service", "payment"),
    ("component", "database")
  ])
  assert_eq(classify_telemetry(error_log), "critical_error_log")
  
  // Test service warning log
  let warning_log = TelemetryData::Log("High latency detected", 3, 1640995300, [
    ("service", "api"),
    ("endpoint", "/users")
  ])
  assert_eq(classify_telemetry(warning_log), "service_warning_log")
  
  // Test long-running span
  let long_span = TelemetryData::Span("database_query", "trace-123", "span-456", 1640995200, 1641000300, [
    ("service", "payment"),
    ("operation", "select")
  ])
  assert_eq(classify_telemetry(long_span), "long_running_span: database_query")
  
  // Test error span
  let error_span = TelemetryData::Span("api_call", "trace-789", "span-012", 1640995400, 1640995450, [
    ("service", "api"),
    ("error", "timeout")
  ])
  assert_eq(classify_telemetry(error_span), "error_span: api_call")
  
  // Test complex trace
  let spans = [
    TelemetryData::Span("span1", "trace-1", "span-1", 1000, 1500, []),
    TelemetryData::Span("span2", "trace-1", "span-2", 1500, 2000, []),
    TelemetryData::Span("span3", "trace-1", "span-3", 2000, 2500, []),
    TelemetryData::Span("span4", "trace-1", "span-4", 2500, 3000, []),
    TelemetryData::Span("span5", "trace-1", "span-5", 3000, 3500, []),
    TelemetryData::Span("span6", "trace-1", "span-6", 3500, 4000, []),
    TelemetryData::Span("span7", "trace-1", "span-7", 4000, 4500, []),
    TelemetryData::Span("span8", "trace-1", "span-8", 4500, 5000, []),
    TelemetryData::Span("span9", "trace-1", "span-9", 5000, 5500, []),
    TelemetryData::Span("span10", "trace-1", "span-10", 5500, 6000, []),
    TelemetryData::Span("span11", "trace-1", "span-11", 6000, 6500, [])
  ]
  let complex_trace = TelemetryData::Trace(spans)
  assert_eq(classify_telemetry(complex_trace), "complex_trace")
  
  // Test trace with long span
  let spans_with_long = [
    TelemetryData::Span("short", "trace-2", "span-1", 1000, 1500, []),
    TelemetryData::Span("long", "trace-2", "span-2", 1500, 3000, []),
    TelemetryData::Span("medium", "trace-2", "span-3", 3000, 3500, [])
  ]
  let trace_with_long = TelemetryData::Trace(spans_with_long)
  assert_eq(classify_telemetry(trace_with_long), "trace_with_long_span")
  
  // Test critical event
  let critical_event = TelemetryData::Event("system_failure", 1640995500, [
    ("alert", "critical"),
    ("service", "payment"),
    ("severity", "high")
  ])
  assert_eq(classify_telemetry(critical_event), "critical_event: system_failure")
}

// Test 2: Nested Pattern Matching with Telemetry Context
test "nested pattern matching with telemetry context" {
  // Define telemetry context structure
  enum TelemetryContext {
    Root(String, Array[(String, String)])  // trace_id, attributes
    Child(String, String, Array[(String, String)])  // parent_id, span_id, attributes
    Linked(String, Array[String], Array[(String, String)])  // trace_id, linked_trace_ids, attributes
  }
  
  // Define enriched telemetry data
  type EnrichedTelemetry = {
    data: String,
    context: TelemetryContext,
    timestamp: Int,
    metadata: Array[(String, String)]
  }
  
  // Complex nested pattern matching
  let analyze_telemetry_context = fn(telemetry: EnrichedTelemetry) {
    match telemetry {
      { data, context: TelemetryContext::Root(trace_id, attrs), timestamp, metadata } 
        if attrs.some(fn(a) { a.0 == "service" && a.1 == "critical" }) => {
        "critical_root_trace: " + trace_id
      }
      { data, context: TelemetryContext::Root(trace_id, attrs), timestamp, metadata }
        if metadata.length() > 10 => {
        "metadata_rich_root: " + trace_id
      }
      { data, context: TelemetryContext::Root(trace_id, attrs), timestamp, metadata } => {
        "standard_root: " + trace_id
      }
      { data, context: TelemetryContext::Child(parent_id, span_id, attrs), timestamp, metadata }
        if attrs.some(fn(a) { a.0 == "error" }) => {
        "error_child_span: " + span_id + " (parent: " + parent_id + ")"
      }
      { data, context: TelemetryContext::Child(parent_id, span_id, attrs), timestamp, metadata }
        if timestamp % 1000 == 0 => {
        "timestamp_aligned_child: " + span_id
      }
      { data, context: TelemetryContext::Child(parent_id, span_id, attrs), timestamp, metadata } => {
        "standard_child: " + span_id
      }
      { data, context: TelemetryContext::Linked(trace_id, linked_traces, attrs), timestamp, metadata }
        if linked_traces.length() > 5 => {
        "highly_linked_trace: " + trace_id
      }
      { data, context: TelemetryContext::Linked(trace_id, linked_traces, attrs), timestamp, metadata }
        if attrs.some(fn(a) { a.0 == "correlation_id" }) => {
        "correlated_linked_trace: " + trace_id
      }
      { data, context: TelemetryContext::Linked(trace_id, linked_traces, attrs), timestamp, metadata } => {
        "standard_linked: " + trace_id
      }
    }
  }
  
  // Test critical root trace
  let critical_root = {
    data: "critical_operation",
    context: TelemetryContext::Root("trace-critical-123", [
      ("service", "critical"),
      ("environment", "production")
    ]),
    timestamp: 1640995200,
    metadata: [("version", "1.0.0")]
  }
  assert_eq(analyze_telemetry_context(critical_root), "critical_root_trace: trace-critical-123")
  
  // Test metadata rich root
  let metadata_rich_root = {
    data: "complex_operation",
    context: TelemetryContext::Root("trace-complex-456", [
      ("service", "api"),
      ("environment", "production")
    ]),
    timestamp: 1640995300,
    metadata: [
      ("version", "1.2.3"),
      ("build", "abc123"),
      ("commit", "def456"),
      ("branch", "main"),
      ("environment", "prod"),
      ("region", "us-west"),
      ("datacenter", "dc1"),
      ("host", "server-1"),
      ("pod", "pod-123"),
      ("container", "container-456"),
      ("process", "process-789")
    ]
  }
  assert_eq(analyze_telemetry_context(metadata_rich_root), "metadata_rich_root: trace-complex-456")
  
  // Test error child span
  let error_child = {
    data: "failed_operation",
    context: TelemetryContext::Child("parent-123", "child-456", [
      ("service", "payment"),
      ("error", "timeout"),
      ("retry_count", "3")
    ]),
    timestamp: 1640995400,
    metadata: [("attempt", "1")]
  }
  assert_eq(analyze_telemetry_context(error_child), "error_child_span: child-456 (parent: parent-123)")
  
  // Test timestamp aligned child
  let timestamp_child = {
    data: "timed_operation",
    context: TelemetryContext::Child("parent-789", "child-012", [
      ("service", "api"),
      ("operation", "query")
    ]),
    timestamp: 1640996000,  // Aligned to 1000ms
    metadata: [("type", "scheduled")]
  }
  assert_eq(analyze_telemetry_context(timestamp_child), "timestamp_aligned_child: child-012")
  
  // Test highly linked trace
  let highly_linked = {
    data: "distributed_operation",
    context: TelemetryContext::Linked("trace-main-123", [
      "trace-service-1",
      "trace-service-2", 
      "trace-service-3",
      "trace-service-4",
      "trace-service-5",
      "trace-service-6"
    ], [
      ("service", "orchestrator"),
      ("operation", "distributed_transaction")
    ]),
    timestamp: 1640996500,
    metadata: [("type", "coordination")]
  }
  assert_eq(analyze_telemetry_context(highly_linked), "highly_linked_trace: trace-main-123")
  
  // Test correlated linked trace
  let correlated_linked = {
    data: "correlated_operation",
    context: TelemetryContext::Linked("trace-corr-789", [
      "trace-related-1",
      "trace-related-2"
    ], [
      ("service", "processor"),
      ("correlation_id", "corr-12345"),
      ("session_id", "session-67890")
    ]),
    timestamp: 1640997000,
    metadata: [("type", "correlation")]
  }
  assert_eq(analyze_telemetry_context(correlated_linked), "correlated_linked_trace: trace-corr-789")
}

// Test 3: Pattern Matching with Telemetry Quality Validation
test "pattern matching with telemetry quality validation" {
  // Define telemetry quality levels
  enum QualityLevel {
    Excellent
    Good  
    Fair
    Poor
    Invalid
  }
  
  // Define telemetry validation result
  type ValidationResult = {
    level: QualityLevel,
    issues: Array[String],
    score: Float
  }
  
  // Define telemetry data with quality metadata
  type QualityTelemetry = {
    id: String,
    completeness: Float,  // 0.0 to 1.0
    accuracy: Float,      // 0.0 to 1.0  
    timeliness: Float,    // 0.0 to 1.0
    consistency: Float,   // 0.0 to 1.0
    validation_rules: Array[String]
  }
  
  // Quality validation with pattern matching
  let validate_telemetry_quality = fn(telemetry: QualityTelemetry) {
    let score = (telemetry.completeness + telemetry.accuracy + 
                 telemetry.timeliness + telemetry.consistency) / 4.0
    
    let (level, issues) = match telemetry {
      { completeness: c, accuracy: a, timeliness: t, consistency: s, validation_rules: rules }
        if c >= 0.95 && a >= 0.95 && t >= 0.95 && s >= 0.95 => {
        (QualityLevel::Excellent, [])
      }
      { completeness: c, accuracy: a, timeliness: t, consistency: s, validation_rules: rules }
        if c >= 0.8 && a >= 0.8 && t >= 0.8 && s >= 0.8 => {
        let issues = []
        let issues = if c < 0.9 { issues.push("moderate_completeness") } else { issues }
        let issues = if a < 0.9 { issues.push("moderate_accuracy") } else { issues }
        let issues = if t < 0.9 { issues.push("moderate_timeliness") } else { issues }
        let issues = if s < 0.9 { issues.push("moderate_consistency") } else { issues }
        (QualityLevel::Good, issues)
      }
      { completeness: c, accuracy: a, timeliness: t, consistency: s, validation_rules: rules }
        if c >= 0.6 && a >= 0.6 && t >= 0.6 && s >= 0.6 => {
        let issues = []
        let issues = if c < 0.8 { issues.push("low_completeness") } else { issues }
        let issues = if a < 0.8 { issues.push("low_accuracy") } else { issues }
        let issues = if t < 0.8 { issues.push("low_timeliness") } else { issues }
        let issues = if s < 0.8 { issues.push("low_consistency") } else { issues }
        (QualityLevel::Fair, issues)
      }
      { completeness: c, accuracy: a, timeliness: t, consistency: s, validation_rules: rules }
        if c >= 0.3 && a >= 0.3 && t >= 0.3 && s >= 0.3 => {
        let issues = ["very_low_completeness", "very_low_accuracy", "very_low_timeliness", "very_low_consistency"]
        (QualityLevel::Poor, issues)
      }
      _ => {
        (QualityLevel::Invalid, ["invalid_telemetry_data"])
      }
    }
    
    { level: level, issues: issues, score: score }
  }
  
  // Test excellent quality telemetry
  let excellent_telemetry = {
    id: "tele-excellent-123",
    completeness: 0.98,
    accuracy: 0.97,
    timeliness: 0.99,
    consistency: 0.96,
    validation_rules: ["standard", "production"]
  }
  
  let excellent_result = validate_telemetry_quality(excellent_telemetry)
  match excellent_result.level {
    QualityLevel::Excellent => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(excellent_result.issues.length(), 0)
  assert_true(excellent_result.score >= 0.95)
  
  // Test good quality telemetry with moderate issues
  let good_telemetry = {
    id: "tele-good-456",
    completeness: 0.85,
    accuracy: 0.92,
    timeliness: 0.88,
    consistency: 0.83,
    validation_rules: ["standard"]
  }
  
  let good_result = validate_telemetry_quality(good_telemetry)
  match good_result.level {
    QualityLevel::Good => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(good_result.issues.length() > 0)
  assert_true(good_result.issues.contains("moderate_completeness"))
  assert_true(good_result.score >= 0.8 && good_result.score < 0.95)
  
  // Test fair quality telemetry
  let fair_telemetry = {
    id: "tele-fair-789",
    completeness: 0.75,
    accuracy: 0.68,
    timeliness: 0.72,
    consistency: 0.65,
    validation_rules: ["basic"]
  }
  
  let fair_result = validate_telemetry_quality(fair_telemetry)
  match fair_result.level {
    QualityLevel::Fair => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(fair_result.issues.contains("low_accuracy"))
  assert_true(fair_result.issues.contains("low_consistency"))
  assert_true(fair_result.score >= 0.6 && fair_result.score < 0.8)
  
  // Test poor quality telemetry
  let poor_telemetry = {
    id: "tele-poor-012",
    completeness: 0.45,
    accuracy: 0.38,
    timeliness: 0.42,
    consistency: 0.35,
    validation_rules: ["minimal"]
  }
  
  let poor_result = validate_telemetry_quality(poor_telemetry)
  match poor_result.level {
    QualityLevel::Poor => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(poor_result.issues.length(), 4)
  assert_true(poor_result.score >= 0.3 && poor_result.score < 0.6)
  
  // Test invalid telemetry
  let invalid_telemetry = {
    id: "tele-invalid-345",
    completeness: -0.1,  // Invalid negative value
    accuracy: 1.5,       // Invalid value > 1.0
    timeliness: 0.2,
    consistency: 0.1,
    validation_rules: []
  }
  
  let invalid_result = validate_telemetry_quality(invalid_telemetry)
  match invalid_result.level {
    QualityLevel::Invalid => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(invalid_result.issues.length(), 1)
  assert_eq(invalid_result.issues[0], "invalid_telemetry_data")
}

// Test 4: Pattern Matching with Telemetry Transformation Pipeline
test "pattern matching with telemetry transformation pipeline" {
  // Define transformation operations
  enum TransformOperation {
    Filter(String, String)  // attribute_key, attribute_value
    Map(String, String)     // from_attribute, to_attribute
    Aggregate(String)       // aggregation_function
    Enrich(String, String)  // key, value
    Validate(String)        // validation_rule
  }
  
  // Define transformation pipeline
  type TransformPipeline = {
    id: String,
    operations: Array[TransformOperation],
    input_count: Int,
    output_count: Int,
    error_count: Int
  }
  
  // Define transformation result
  enum TransformResult {
    Success(Array[String], Int)  // transformed_data, record_count
    PartialSuccess(Array[String], Array[String], Int)  // success_data, error_data, record_count
    Failure(String, Int)  // error_message, attempted_count
  }
  
  // Execute transformation pipeline with pattern matching
  let execute_pipeline = fn(pipeline: TransformPipeline, input_data: Array[String]) {
    let mut current_data = input_data
    let mut errors = []
    let mut processed_count = 0
    
    for operation in pipeline.operations {
      match operation {
        TransformOperation::Filter(key, value) => {
          let filtered = current_data.filter_fn(item) { 
            item.contains(key + "=" + value) 
          }
          if filtered.length() == 0 {
            errors = errors.push("filter_no_matches: " + key + "=" + value)
          }
          current_data = filtered
        }
        TransformOperation::Map(from_key, to_key) => {
          let mapped = current_data.map_fn(item) {
            if item.contains(from_key) {
              item.replace(from_key, to_key)
            } else {
              item
            }
          }
          current_data = mapped
        }
        TransformOperation::Aggregate(function) => {
          match function {
            "count" => {
              let count = current_data.length().to_string()
              current_data = ["count=" + count]
            }
            "unique" => {
              let unique = current_data.fold([], fn(acc, item) {
                if not(acc.contains(item)) { acc.push(item) } else { acc }
              })
              current_data = unique
            }
            _ => {
              errors = errors.push("unknown_aggregation: " + function)
            }
          }
        }
        TransformOperation::Enrich(key, value) => {
          let enriched = current_data.map_fn(item) {
            item + "|" + key + "=" + value
          }
          current_data = enriched
        }
        TransformOperation::Validate(rule) => {
          match rule {
            "non_empty" => {
              let valid = current_data.filter_fn(item) { item.length() > 0 }
              let invalid_count = current_data.length() - valid.length()
              if invalid_count > 0 {
                errors = errors.push("validation_failed: " + invalid_count.to_string() + " empty items")
              }
              current_data = valid
            }
            _ => {
              errors = errors.push("unknown_validation: " + rule)
            }
          }
        }
      }
      processed_count = processed_count + 1
    }
    
    // Determine result based on errors and data
    if errors.length() == 0 {
      TransformResult::Success(current_data, current_data.length())
    } else if current_data.length() > 0 {
      TransformResult::PartialSuccess(current_data, errors, current_data.length())
    } else {
      TransformResult::Failure(errors.join("; "), processed_count)
    }
  }
  
  // Test successful pipeline
  let success_pipeline = {
    id: "success-pipeline-1",
    operations: [
      TransformOperation::Filter("service", "api"),
      TransformOperation::Enrich("environment", "production"),
      TransformOperation::Map("service", "service_name")
    ],
    input_count: 10,
    output_count: 0,
    error_count: 0
  }
  
  let input_data = [
    "service=api|endpoint=/users",
    "service=database|operation=query",
    "service=api|endpoint=/orders",
    "service=cache|operation=get",
    "service=api|endpoint=/products"
  ]
  
  let success_result = execute_pipeline(success_pipeline, input_data)
  match success_result {
    TransformResult::Success(data, count) => {
      assert_eq(count, 3)
      assert_true(data[0].contains("service_name=api"))
      assert_true(data[0].contains("environment=production"))
    }
    _ => assert_true(false)
  }
  
  // Test partial success pipeline
  let partial_pipeline = {
    id: "partial-pipeline-1", 
    operations: [
      TransformOperation::Filter("service", "nonexistent"),
      TransformOperation::Aggregate("count"),
      TransformOperation::Enrich("environment", "production")
    ],
    input_count: 5,
    output_count: 0,
    error_count: 0
  }
  
  let partial_result = execute_pipeline(partial_pipeline, input_data)
  match partial_result {
    TransformResult::PartialSuccess(data, errors, count) => {
      assert_eq(count, 0)
      assert_true(errors.length() > 0)
      assert_true(errors[0].contains("filter_no_matches"))
    }
    _ => assert_true(false)
  }
  
  // Test failure pipeline
  let failure_pipeline = {
    id: "failure-pipeline-1",
    operations: [
      TransformOperation::Validate("non_empty"),
      TransformOperation::Filter("service", "api"),
      TransformOperation::Aggregate("unknown_function")
    ],
    input_count: 3,
    output_count: 0,
    error_count: 0
  }
  
  let failure_input = [
    "",
    "service=api|endpoint=/test",
    "service=web|endpoint=/home"
  ]
  
  let failure_result = execute_pipeline(failure_pipeline, failure_input)
  match failure_result {
    TransformResult::Failure(error_message, attempted_count) => {
      assert_eq(attempted_count, 3)
      assert_true(error_message.contains("validation_failed"))
      assert_true(error_message.contains("unknown_aggregation"))
    }
    _ => assert_true(false)
  }
}