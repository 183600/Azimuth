// Azimuth Premium High-Quality Test Suite
// This file contains 10 high-quality test cases for the Azimuth telemetry system

// Test 1: Telemetry Data Collection and Aggregation
test "telemetry data collection and aggregation" {
  // Define telemetry data types
  type Metric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[String]
  }
  
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  type TelemetryData = {
    metrics: Array[Metric],
    spans: Array[Span],
    logs: Array[String]
  }
  
  // Create sample metrics
  let cpu_metric = {
    name: "cpu_usage",
    value: 75.5,
    unit: "percent",
    timestamp: 1640995200,
    tags: ["host:server1", "region:us-west"]
  }
  
  let memory_metric = {
    name: "memory_usage",
    value: 1024.0,
    unit: "mb",
    timestamp: 1640995200,
    tags: ["host:server1", "region:us-west"]
  }
  
  // Create sample span
  let db_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users"),
      ("db.duration", "50")
    ]
  }
  
  // Create telemetry data collection
  let telemetry_data = {
    metrics: [cpu_metric, memory_metric],
    spans: [db_span],
    logs: ["Application started", "Database connected"]
  }
  
  // Test metric aggregation
  let aggregate_metrics = fn(metrics: Array[Metric]) {
    let mut total_value = 0.0
    let mut count = 0
    
    for metric in metrics {
      total_value = total_value + metric.value
      count = count + 1
    }
    
    if count > 0 {
      total_value / count.to_float()
    } else {
      0.0
    }
  }
  
  let avg_metric_value = aggregate_metrics(telemetry_data.metrics)
  assert_eq(avg_metric_value, 549.75)  // (75.5 + 1024.0) / 2
  
  // Test span duration calculation
  let calculate_span_duration = fn(span: Span) {
    span.end_time - span.start_time
  }
  
  let db_duration = calculate_span_duration(db_span)
  assert_eq(db_duration, 50)
  
  // Test filtering metrics by tags
  let filter_by_tag = fn(metrics: Array[Metric], tag: String) {
    let mut filtered = []
    for metric in metrics {
      if metric.tags.contains(tag) {
        filtered = filtered.push(metric)
      }
    }
    filtered
  }
  
  let host_filtered = filter_by_tag(telemetry_data.metrics, "host:server1")
  assert_eq(host_filtered.length(), 2)
  
  let region_filtered = filter_by_tag(telemetry_data.metrics, "region:us-east")
  assert_eq(region_filtered.length(), 0)
  
  // Test span attribute lookup
  let get_attribute = fn(span: Span, key: String) {
    let mut found = None
    for (k, v) in span.attributes {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  let db_type = get_attribute(db_span, "db.type")
  assert_eq(db_type, Some("postgresql"))
  
  let missing_attr = get_attribute(db_span, "missing.key")
  assert_eq(missing_attr, None)
}

// Test 2: Cross-Service Distributed Tracing
test "cross-service distributed tracing" {
  // Define distributed tracing types
  type TraceContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)]
  }
  
  type ServiceSpan = {
    service_name: String,
    operation: String,
    context: TraceContext,
    start_time: Int,
    end_time: Int,
    children: Array[ServiceSpan]
  }
  
  type DistributedTrace = {
    root_span: ServiceSpan,
    all_spans: Array[ServiceSpan]
  }
  
  // Create trace context propagation
  let create_child_context = fn(parent_context: TraceContext, new_span_id: String) {
    {
      trace_id: parent_context.trace_id,
      span_id: new_span_id,
      baggage: parent_context.baggage
    }
  }
  
  let add_baggage_item = fn(context: TraceContext, key: String, value: String) {
    let mut new_baggage = context.baggage
    new_baggage = new_baggage.push((key, value))
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      baggage: new_baggage
    }
  }
  
  // Create root span for API Gateway
  let root_context = {
    trace_id: "trace-abc123",
    span_id: "span-root",
    baggage: [
      ("user.id", "user-456"),
      ("request.id", "req-789")
    ]
  }
  
  let api_gateway_span = {
    service_name: "api-gateway",
    operation: "process_request",
    context: root_context,
    start_time: 1640995200,
    end_time: 1640995210,
    children: []
  }
  
  // Create child span for Auth Service
  let auth_context = create_child_context(root_context, "span-auth")
  let auth_context_with_baggage = add_baggage_item(auth_context, "auth.method", "jwt")
  
  let auth_span = {
    service_name: "auth-service",
    operation: "authenticate_token",
    context: auth_context_with_baggage,
    start_time: 1640995201,
    end_time: 1640995205,
    children: []
  }
  
  // Create child span for User Service
  let user_context = create_child_context(root_context, "span-user")
  let user_span = {
    service_name: "user-service",
    operation: "get_user_profile",
    context: user_context,
    start_time: 1640995206,
    end_time: 1640995208,
    children: []
  }
  
  // Build distributed trace
  let api_with_children = {
    service_name: api_gateway_span.service_name,
    operation: api_gateway_span.operation,
    context: api_gateway_span.context,
    start_time: api_gateway_span.start_time,
    end_time: api_gateway_span.end_time,
    children: [auth_span, user_span]
  }
  
  let distributed_trace = {
    root_span: api_with_children,
    all_spans: [api_with_children, auth_span, user_span]
  }
  
  // Test trace consistency
  let validate_trace_consistency = fn(trace: DistributedTrace) {
    let root_trace_id = trace.root_span.context.trace_id
    let mut consistent = true
    
    for span in trace.all_spans {
      if span.context.trace_id != root_trace_id {
        consistent = false
      }
    }
    
    consistent
  }
  
  assert_true(validate_trace_consistency(distributed_trace))
  
  // Test baggage propagation
  let get_baggage_value = fn(context: TraceContext, key: String) {
    let mut found = None
    for (k, v) in context.baggage {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  let user_id_in_auth = get_baggage_value(auth_span.context, "user.id")
  assert_eq(user_id_in_auth, Some("user-456"))
  
  let auth_method = get_baggage_value(auth_span.context, "auth.method")
  assert_eq(auth_method, Some("jwt"))
  
  // Test trace timeline analysis
  let get_trace_duration = fn(trace: DistributedTrace) {
    trace.root_span.end_time - trace.root_span.start_time
  }
  
  let trace_duration = get_trace_duration(distributed_trace)
  assert_eq(trace_duration, 10)
  
  // Test service dependency analysis
  let get_service_dependencies = fn(trace: DistributedTrace) {
    let mut dependencies = []
    for span in trace.all_spans {
      if span.children.length() > 0 {
        for child in span.children {
          dependencies = dependencies.push((span.service_name, child.service_name))
        }
      }
    }
    dependencies
  }
  
  let dependencies = get_service_dependencies(distributed_trace)
  assert_eq(dependencies.length(), 2)
  assert_true(dependencies.contains(("api-gateway", "auth-service")))
  assert_true(dependencies.contains(("api-gateway", "user-service")))
  
  // Test critical path analysis
  let get_critical_path = fn(trace: DistributedTrace) {
    let mut critical_path = []
    let mut current_span = trace.root_span
    
    while true {
      critical_path = critical_path.push(current_span.service_name + ":" + current_span.operation)
      
      if current_span.children.length() > 0 {
        // Find the child with the longest duration
        let mut longest_child = current_span.children[0]
        let mut longest_duration = longest_child.end_time - longest_child.start_time
        
        for child in current_span.children {
          let duration = child.end_time - child.start_time
          if duration > longest_duration {
            longest_duration = duration
            longest_child = child
          }
        }
        
        current_span = longest_child
      } else {
        break
      }
    }
    
    critical_path
  }
  
  let critical_path = get_critical_path(distributed_trace)
  assert_eq(critical_path.length(), 3)
  assert_eq(critical_path[0], "api-gateway:process_request")
  assert_eq(critical_path[1], "auth-service:authenticate_token")  // Longer duration (4ms vs 2ms)
}

// Test 3: Performance Monitoring and Benchmarking
test "performance monitoring and benchmarking" {
  // Define performance monitoring types
  type PerformanceMetric = {
    name: String,
    value: Float,
    threshold: Float,
    unit: String,
    timestamp: Int
  }
  
  type BenchmarkResult = {
    test_name: String,
    execution_time: Int,
    memory_usage: Int,
    cpu_usage: Float,
    passed: Bool
  }
  
  type PerformanceReport = {
    metrics: Array[PerformanceMetric],
    benchmarks: Array[BenchmarkResult],
    overall_score: Float
  }
  
  // Create performance thresholds
  let performance_thresholds = [
    ("response_time", 100.0, "ms"),
    ("memory_usage", 512.0, "mb"),
    ("cpu_usage", 80.0, "percent"),
    ("error_rate", 5.0, "percent")
  ]
  
  // Simulate performance metrics collection
  let collect_metrics = fn() {
    let current_time = 1640995200
    
    [
      {
        name: "response_time",
        value: 85.5,
        threshold: 100.0,
        unit: "ms",
        timestamp: current_time
      },
      {
        name: "memory_usage",
        value: 425.0,
        threshold: 512.0,
        unit: "mb",
        timestamp: current_time
      },
      {
        name: "cpu_usage",
        value: 65.2,
        threshold: 80.0,
        unit: "percent",
        timestamp: current_time
      },
      {
        name: "error_rate",
        value: 2.1,
        threshold: 5.0,
        unit: "percent",
        timestamp: current_time
      }
    ]
  }
  
  // Test performance evaluation
  let evaluate_performance = fn(metrics: Array[PerformanceMetric]) {
    let mut passed_count = 0
    let mut total_count = 0
    
    for metric in metrics {
      total_count = total_count + 1
      if metric.value <= metric.threshold {
        passed_count = passed_count + 1
      }
    }
    
    if total_count > 0 {
      passed_count.to_float() / total_count.to_float()
    } else {
      0.0
    }
  }
  
  let metrics = collect_metrics()
  let performance_score = evaluate_performance(metrics)
  assert_eq(performance_score, 1.0)  // All metrics within thresholds
  
  // Test benchmark execution simulation
  let run_benchmark = fn(test_name: String, complexity: Int) {
    let base_time = 10
    let base_memory = 50
    let base_cpu = 20.0
    
    // Simulate execution based on complexity
    let execution_time = base_time * complexity
    let memory_usage = base_memory * complexity
    let cpu_usage = base_cpu * complexity.to_float()
    
    // Determine if benchmark passed based on thresholds
    let passed = execution_time < 1000 and memory_usage < 1000 and cpu_usage < 90.0
    
    {
      test_name,
      execution_time,
      memory_usage,
      cpu_usage,
      passed
    }
  }
  
  let benchmarks = [
    run_benchmark("simple_operation", 1),
    run_benchmark("medium_complexity", 5),
    run_benchmark("high_complexity", 10)
  ]
  
  // Test benchmark analysis
  let analyze_benchmarks = fn(results: Array[BenchmarkResult]) {
    let mut passed = 0
    let mut total_time = 0
    let mut total_memory = 0
    let mut total_cpu = 0.0
    
    for result in results {
      if result.passed {
        passed = passed + 1
      }
      total_time = total_time + result.execution_time
      total_memory = total_memory + result.memory_usage
      total_cpu = total_cpu + result.cpu_usage
    }
    
    let count = results.length()
    {
      pass_rate: passed.to_float() / count.to_float(),
      avg_execution_time: total_time / count,
      avg_memory_usage: total_memory / count,
      avg_cpu_usage: total_cpu / count.to_float()
    }
  }
  
  let benchmark_analysis = analyze_benchmarks(benchmarks)
  assert_eq(benchmark_analysis.pass_rate, 1.0)  // All benchmarks passed
  assert_eq(benchmark_analysis.avg_execution_time, 60)  // (10 + 50 + 100) / 3
  assert_eq(benchmark_analysis.avg_memory_usage, 300)  // (50 + 250 + 500) / 3
  assert_eq(benchmark_analysis.avg_cpu_usage, 90.0)  // (20 + 100 + 200) / 3
  
  // Test performance trend analysis
  let analyze_trend = fn(current: Float, previous: Float) {
    if current < previous {
      "improving"
    } else if current > previous * 1.1 {
      "degrading"
    } else {
      "stable"
    }
  }
  
  let current_response_time = 85.5
  let previous_response_time = 92.0
  let response_time_trend = analyze_trend(current_response_time, previous_response_time)
  assert_eq(response_time_trend, "improving")
  
  let current_memory = 425.0
  let previous_memory = 380.0
  let memory_trend = analyze_trend(current_memory, previous_memory)
  assert_eq(memory_trend, "stable")
  
  // Test performance alerting
  let check_alerts = fn(metrics: Array[PerformanceMetric]) {
    let mut alerts = []
    for metric in metrics {
      if metric.value > metric.threshold {
        alerts = alerts.push(metric.name + " exceeded threshold: " + 
                         metric.value.to_string() + " > " + 
                         metric.threshold.to_string())
      }
    }
    alerts
  }
  
  // Create metrics with one exceeding threshold
  let metrics_with_alert = [
    {
      name: "response_time",
      value: 120.0,  // Exceeds threshold
      threshold: 100.0,
      unit: "ms",
      timestamp: 1640995200
    },
    {
      name: "memory_usage",
      value: 425.0,  // Within threshold
      threshold: 512.0,
      unit: "mb",
      timestamp: 1640995200
    }
  ]
  
  let alerts = check_alerts(metrics_with_alert)
  assert_eq(alerts.length(), 1)
  assert_true(alerts[0].contains("response_time exceeded threshold"))
  
  // Create comprehensive performance report
  let performance_report = {
    metrics,
    benchmarks,
    overall_score: performance_score
  }
  
  assert_eq(performance_report.metrics.length(), 4)
  assert_eq(performance_report.benchmarks.length(), 3)
  assert_eq(performance_report.overall_score, 1.0)
}

// Test 4: Advanced Error Handling and Recovery
test "advanced error handling and recovery" {
  // Define error types
  enum TelemetryError {
    NetworkTimeout(String, Int)
    SerializationError(String)
    ValidationError(String)
    ResourceExhausted(String, Int)
    ServiceUnavailable(String)
    CircuitBreakerOpen(String)
  }
  
  // Define result type
  type TelemetryResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError],
    retry_count: Int
  }
  
  // Define retry strategy
  type RetryStrategy = {
    max_attempts: Int,
    base_delay: Int,
    max_delay: Int,
    backoff_multiplier: Float
  }
  
  // Create retry strategy
  let retry_strategy = {
    max_attempts: 3,
    base_delay: 100,
    max_delay: 1000,
    backoff_multiplier: 2.0
  }
  
  // Test error classification
  let classify_error = fn(error: TelemetryError) {
    match error {
      TelemetryError::NetworkTimeout(_, _) => "retryable"
      TelemetryError::SerializationError(_) => "retryable"
      TelemetryError::ValidationError(_) => "non_retryable"
      TelemetryError::ResourceExhausted(_, _) => "retryable"
      TelemetryError::ServiceUnavailable(_) => "retryable"
      TelemetryError::CircuitBreakerOpen(_) => "non_retryable"
    }
  }
  
  let network_error = TelemetryError::NetworkTimeout("api.service", 5000)
  let validation_error = TelemetryError::ValidationError("Invalid span ID")
  
  assert_eq(classify_error(network_error), "retryable")
  assert_eq(classify_error(validation_error), "non_retryable")
  
  // Test retry delay calculation
  let calculate_retry_delay = fn(attempt: Int, strategy: RetryStrategy) {
    let delay = strategy.base_delay * (strategy.backoff_multiplier.pow(attempt - 1)).to_int()
    if delay > strategy.max_delay {
      strategy.max_delay
    } else {
      delay
    }
  }
  
  let delay1 = calculate_retry_delay(1, retry_strategy)
  let delay2 = calculate_retry_delay(2, retry_strategy)
  let delay3 = calculate_retry_delay(3, retry_strategy)
  
  assert_eq(delay1, 100)
  assert_eq(delay2, 200)
  assert_eq(delay3, 400)
  
  // Test error recovery with fallback
  let execute_with_fallback = fn(primary: () -> TelemetryResult[String], 
                                fallback: () -> TelemetryResult[String]) {
    let primary_result = primary()
    if primary_result.success {
      primary_result
    } else {
      match classify_error(primary_result.error.unwrap()) {
        "retryable" => fallback()
        "non_retryable" => primary_result
        _ => primary_result
      }
    }
  }
  
  // Simulate operations
  let failing_operation = fn() {
    {
      success: false,
      data: None,
      error: Some(TelemetryError::NetworkTimeout("service.api", 5000)),
      retry_count: 0
    }
  }
  
  let fallback_operation = fn() {
    {
      success: true,
      data: Some("fallback_result"),
      error: None,
      retry_count: 0
    }
  }
  
  let result_with_fallback = execute_with_fallback(failing_operation, fallback_operation)
  assert_true(result_with_fallback.success)
  assert_eq(result_with_fallback.data, Some("fallback_result"))
  
  // Test circuit breaker pattern
  type CircuitBreaker = {
    failure_count: Int,
    failure_threshold: Int,
    state: String,  // "closed", "open", "half_open"
    last_failure_time: Int
  }
  
  let create_circuit_breaker = fn(threshold: Int) {
    {
      failure_count: 0,
      failure_threshold: threshold,
      state: "closed",
      last_failure_time: 0
    }
  }
  
  let circuit_breaker = create_circuit_breaker(3)
  
  // Test circuit breaker state transitions
  let update_circuit_breaker = fn(cb: CircuitBreaker, success: Bool, current_time: Int) {
    if success {
      if cb.state == "half_open" {
        {
          failure_count: 0,
          failure_threshold: cb.failure_threshold,
          state: "closed",
          last_failure_time: cb.last_failure_time
        }
      } else {
        {
          failure_count: 0,
          failure_threshold: cb.failure_threshold,
          state: cb.state,
          last_failure_time: cb.last_failure_time
        }
      }
    } else {
      let new_failure_count = cb.failure_count + 1
      if new_failure_count >= cb.failure_threshold {
        {
          failure_count: new_failure_count,
          failure_threshold: cb.failure_threshold,
          state: "open",
          last_failure_time: current_time
        }
      } else {
        {
          failure_count: new_failure_count,
          failure_threshold: cb.failure_threshold,
          state: cb.state,
          last_failure_time: current_time
        }
      }
    }
  }
  
  // Simulate circuit breaker operations
  let cb_after_failure1 = update_circuit_breaker(circuit_breaker, false, 1640995200)
  assert_eq(cb_after_failure1.failure_count, 1)
  assert_eq(cb_after_failure1.state, "closed")
  
  let cb_after_failure2 = update_circuit_breaker(cb_after_failure1, false, 1640995200)
  assert_eq(cb_after_failure2.failure_count, 2)
  assert_eq(cb_after_failure2.state, "closed")
  
  let cb_after_failure3 = update_circuit_breaker(cb_after_failure2, false, 1640995200)
  assert_eq(cb_after_failure3.failure_count, 3)
  assert_eq(cb_after_failure3.state, "open")
  
  // Test error aggregation and reporting
  type ErrorReport = {
    total_errors: Int,
    error_types: Array[(String, Int)],
    most_common_error: String,
    error_rate: Float
  }
  
  let aggregate_errors = fn(errors: Array[TelemetryError], total_operations: Int) {
    let mut error_counts = []
    let mut counted_errors = []
    
    for error in errors {
      let error_type = match error {
        TelemetryError::NetworkTimeout(_, _) => "NetworkTimeout"
        TelemetryError::SerializationError(_) => "SerializationError"
        TelemetryError::ValidationError(_) => "ValidationError"
        TelemetryError::ResourceExhausted(_, _) => "ResourceExhausted"
        TelemetryError::ServiceUnavailable(_) => "ServiceUnavailable"
        TelemetryError::CircuitBreakerOpen(_) => "CircuitBreakerOpen"
      }
      
      if not(counted_errors.contains(error_type)) {
        counted_errors = counted_errors.push(error_type)
        let mut count = 0
        for e in errors {
          let e_type = match e {
            TelemetryError::NetworkTimeout(_, _) => "NetworkTimeout"
            TelemetryError::SerializationError(_) => "SerializationError"
            TelemetryError::ValidationError(_) => "ValidationError"
            TelemetryError::ResourceExhausted(_, _) => "ResourceExhausted"
            TelemetryError::ServiceUnavailable(_) => "ServiceUnavailable"
            TelemetryError::CircuitBreakerOpen(_) => "CircuitBreakerOpen"
          }
          if e_type == error_type {
            count = count + 1
          }
        }
        error_counts = error_counts.push((error_type, count))
      }
    }
    
    let mut most_common = ""
    let mut max_count = 0
    for (error_type, count) in error_counts {
      if count > max_count {
        max_count = count
        most_common = error_type
      }
    }
    
    {
      total_errors: errors.length(),
      error_types: error_counts,
      most_common_error: most_common,
      error_rate: errors.length().to_float() / total_operations.to_float()
    }
  }
  
  let sample_errors = [
    TelemetryError::NetworkTimeout("service1", 5000),
    TelemetryError::NetworkTimeout("service2", 3000),
    TelemetryError::ValidationError("invalid_id"),
    TelemetryError::NetworkTimeout("service3", 2000),
    TelemetryError::ServiceUnavailable("service4")
  ]
  
  let error_report = aggregate_errors(sample_errors, 100)
  assert_eq(error_report.total_errors, 5)
  assert_eq(error_report.most_common_error, "NetworkTimeout")
  assert_eq(error_report.error_rate, 0.05)
  
  // Test resilience metrics
  let calculate_resilience_score = fn(successful_operations: Int, total_operations: Int, 
                                    recovery_time: Int, max_acceptable_recovery_time: Int) {
    let availability_score = successful_operations.to_float() / total_operations.to_float()
    let recovery_score = if recovery_time <= max_acceptable_recovery_time {
      1.0
    } else {
      max_acceptable_recovery_time.to_float() / recovery_time.to_float()
    }
    
    (availability_score + recovery_score) / 2.0
  }
  
  let resilience_score = calculate_resilience_score(95, 100, 30, 60)
  assert_eq(resilience_score, 0.975)  // (0.95 + 1.0) / 2.0
}

// Test 5: Data Serialization and Deserialization
test "data serialization and deserialization" {
  // Define serialization formats
  enum SerializationFormat {
    JSON
    XML
    Binary
    CSV
  }
  
  // Define telemetry data types for serialization
  type TelemetryEvent = {
    event_id: String,
    event_type: String,
    timestamp: Int,
    source: String,
    data: Array[(String, String)]
  }
  
  type SerializedData = {
    format: SerializationFormat,
    data: String,
    size: Int,
    compressed: Bool
  }
  
  // Create sample telemetry event
  let telemetry_event = {
    event_id: "event-12345",
    event_type: "span_completed",
    timestamp: 1640995200,
    source: "payment-service",
    data: [
      ("trace_id", "trace-abc123"),
      ("span_id", "span-def456"),
      ("operation", "process_payment"),
      ("duration", "250"),
      ("status", "success")
    ]
  }
  
  // Test JSON serialization
  let serialize_to_json = fn(event: TelemetryEvent) {
    let json_string = "{" +
      "\"event_id\":\"" + event.event_id + "\"," +
      "\"event_type\":\"" + event.event_type + "\"," +
      "\"timestamp\":" + event.timestamp.to_string() + "," +
      "\"source\":\"" + event.source + "\"," +
      "\"data\":{"
    
    let mut data_parts = []
    for (key, value) in event.data {
      data_parts = data_parts.push("\"" + key + "\":\"" + value + "\"")
    }
    
    json_string + data_parts.join(",") + "}}"
  }
  
  let json_data = serialize_to_json(telemetry_event)
  assert_true(json_data.contains("\"event_id\":\"event-12345\""))
  assert_true(json_data.contains("\"event_type\":\"span_completed\""))
  assert_true(json_data.contains("\"timestamp\":1640995200"))
  assert_true(json_data.contains("\"source\":\"payment-service\""))
  assert_true(json_data.contains("\"trace_id\":\"trace-abc123\""))
  
  // Test XML serialization
  let serialize_to_xml = fn(event: TelemetryEvent) {
    let xml_string = "<telemetry_event>" +
      "<event_id>" + event.event_id + "</event_id>" +
      "<event_type>" + event.event_type + "</event_type>" +
      "<timestamp>" + event.timestamp.to_string() + "</timestamp>" +
      "<source>" + event.source + "</source>" +
      "<data>"
    
    let mut data_elements = []
    for (key, value) in event.data {
      data_elements = data_elements.push("<" + key + ">" + value + "</" + key + ">")
    }
    
    xml_string + data_elements.join("") + "</data></telemetry_event>"
  }
  
  let xml_data = serialize_to_xml(telemetry_event)
  assert_true(xml_data.contains("<event_id>event-12345</event_id>"))
  assert_true(xml_data.contains("<event_type>span_completed</event_type>"))
  assert_true(xml_data.contains("<timestamp>1640995200</timestamp>"))
  assert_true(xml_data.contains("<source>payment-service</source>"))
  assert_true(xml_data.contains("<trace_id>trace-abc123</trace_id>"))
  
  // Test CSV serialization
  let serialize_to_csv = fn(event: TelemetryEvent) {
    let headers = "event_id,event_type,timestamp,source," + 
                 event.data.map(fn(pair) { pair.0 }).join(",")
    
    let values = event.event_id + "," + event.event_type + "," + 
                event.timestamp.to_string() + "," + event.source + "," +
                event.data.map(fn(pair) { pair.1 }).join(",")
    
    headers + "\n" + values
  }
  
  let csv_data = serialize_to_csv(telemetry_event)
  assert_true(csv_data.contains("event_id,event_type,timestamp,source"))
  assert_true(csv_data.contains("trace_id,span_id,operation,duration,status"))
  assert_true(csv_data.contains("event-12345,span_completed,1640995200,payment-service"))
  assert_true(csv_data.contains("trace-abc123,span-def456,process_payment,250,success"))
  
  // Test compression simulation
  let compress_data = fn(data: String) {
    // Simple compression simulation: remove repeated characters
    let mut compressed = ""
    let mut i = 0
    
    while i < data.length() {
      let current_char = data[i]
      let mut count = 1
      
      while i + count < data.length() and data[i + count] == current_char {
        count = count + 1
      }
      
      if count > 3 {
        compressed = compressed + current_char.to_string() + "[" + count.to_string() + "]"
      } else {
        for j in 0..count {
          compressed = compressed + current_char.to_string()
        }
      }
      
      i = i + count
    }
    
    compressed
  }
  
  let compressible_data = "aaaaabbbbcccccddddd"
  let compressed_data = compress_data(compressible_data)
  assert_eq(compressed_data, "a[5]b[4]c[5]d[5]")
  
  // Test serialization format comparison
  let compare_formats = fn(event: TelemetryEvent) {
    let json_size = serialize_to_json(event).length()
    let xml_size = serialize_to_xml(event).length()
    let csv_size = serialize_to_csv(event).length()
    
    [
      ("JSON", json_size),
      ("XML", xml_size),
      ("CSV", csv_size)
    ].sort_by(fn(pair1, pair2) { 
      if pair1.1 < pair2.1 { -1 } else if pair1.1 > pair2.1 { 1 } else { 0 }
    })
  }
  
  let format_comparison = compare_formats(telemetry_event)
  assert_eq(format_comparison[0].0, "CSV")  // CSV should be most compact
  assert_eq(format_comparison[2].0, "XML")  // XML should be most verbose
  
  // Test deserialization validation
  let validate_serialized_data = fn(data: String, format: SerializationFormat) {
    let mut valid = true
    let mut errors = []
    
    match format {
      SerializationFormat::JSON => {
        if not(data.starts_with("{")) or not(data.ends_with("}")) {
          valid = false
          errors = errors.push("Invalid JSON structure")
        }
      }
      SerializationFormat::XML => {
        if not(data.starts_with("<")) or not(data.ends_with(">")) {
          valid = false
          errors = errors.push("Invalid XML structure")
        }
      }
      SerializationFormat::CSV => {
        if not(data.contains(",")) {
          valid = false
          errors = errors.push("Invalid CSV format")
        }
      }
      SerializationFormat::Binary => {
        // Binary validation would be more complex
        valid = true
      }
    }
    
    {
      valid,
      errors
    }
  }
  
  let json_validation = validate_serialized_data(json_data, SerializationFormat::JSON)
  assert_true(json_validation.valid)
  assert_eq(json_validation.errors.length(), 0)
  
  let invalid_json = "{invalid json"
  let invalid_json_validation = validate_serialized_data(invalid_json, SerializationFormat::JSON)
  assert_false(invalid_json_validation.valid)
  assert_eq(invalid_json_validation.errors.length(), 1)
  
  // Test serialization performance
  let benchmark_serialization = fn(event: TelemetryEvent, iterations: Int) {
    let start_time = 1640995200
    
    // JSON serialization
    let mut json_results = []
    for i in 0..iterations {
      let result = serialize_to_json(event)
      json_results = json_results.push(result)
    }
    
    // XML serialization
    let mut xml_results = []
    for i in 0..iterations {
      let result = serialize_to_xml(event)
      xml_results = xml_results.push(result)
    }
    
    // CSV serialization
    let mut csv_results = []
    for i in 0..iterations {
      let result = serialize_to_csv(event)
      csv_results = csv_results.push(result)
    }
    
    let end_time = 1640995300  // Simulated end time
    
    {
      json_count: json_results.length(),
      xml_count: xml_results.length(),
      csv_count: csv_results.length(),
      duration: end_time - start_time
    }
  }
  
  let serialization_benchmark = benchmark_serialization(telemetry_event, 10)
  assert_eq(serialization_benchmark.json_count, 10)
  assert_eq(serialization_benchmark.xml_count, 10)
  assert_eq(serialization_benchmark.csv_count, 10)
  assert_eq(serialization_benchmark.duration, 100)
  
  // Create serialized data records
  let serialized_json = {
    format: SerializationFormat::JSON,
    data: json_data,
    size: json_data.length(),
    compressed: false
  }
  
  let serialized_xml = {
    format: SerializationFormat::XML,
    data: xml_data,
    size: xml_data.length(),
    compressed: false
  }
  
  let serialized_csv = {
    format: SerializationFormat::CSV,
    data: csv_data,
    size: csv_data.length(),
    compressed: false
  }
  
  assert_true(serialized_json.size > 0)
  assert_true(serialized_xml.size > 0)
  assert_true(serialized_csv.size > 0)
  assert_eq(serialized_json.format, SerializationFormat::JSON)
  assert_eq(serialized_xml.format, SerializationFormat::XML)
  assert_eq(serialized_csv.format, SerializationFormat::CSV)
}

// Test 6: Resource Management and Lifecycle
test "resource management and lifecycle" {
  // Define resource types
  type Resource = {
    id: String,
    name: String,
    resource_type: String,
    allocated: Bool,
    allocation_time: Int,
    last_accessed: Int,
    usage_count: Int
  }
  
  type ResourceManager = {
    resources: Array[Resource],
    max_resources: Int,
    allocation_strategy: String
  }
  
  // Create resource manager
  let create_resource_manager = fn(max_resources: Int, strategy: String) {
    {
      resources: [],
      max_resources,
      allocation_strategy: strategy
    }
  }
  
  let resource_manager = create_resource_manager(10, "least_recently_used")
  
  // Test resource allocation
  let allocate_resource = fn(manager: ResourceManager, name: String, resource_type: String) {
    if manager.resources.length() >= manager.max_resources {
      None
    } else {
      let current_time = 1640995200
      let new_resource = {
        id: "res-" + manager.resources.length().to_string(),
        name,
        resource_type,
        allocated: true,
        allocation_time: current_time,
        last_accessed: current_time,
        usage_count: 0
      }
      
      Some(new_resource)
    }
  }
  
  let db_connection = allocate_resource(resource_manager, "db_connection", "database")
  assert_true(db_connection.is_some())
  
  match db_connection {
    Some(resource) => {
      assert_eq(resource.name, "db_connection")
      assert_eq(resource.resource_type, "database")
      assert_true(resource.allocated)
      assert_eq(resource.usage_count, 0)
    }
    None => assert_true(false)
  }
  
  // Test resource deallocation
  let deallocate_resource = fn(manager: ResourceManager, resource_id: String) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in manager.resources {
      if resource.id == resource_id {
        found = true
        updated_resources = updated_resources.push({
          id: resource.id,
          name: resource.name,
          resource_type: resource.resource_type,
          allocated: false,
          allocation_time: resource.allocation_time,
          last_accessed: resource.last_accessed,
          usage_count: resource.usage_count
        })
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    {
      resources: updated_resources,
      max_resources: manager.max_resources,
      allocation_strategy: manager.allocation_strategy
    }
  }
  
  // Test resource usage tracking
  let track_resource_usage = fn(resource: Resource, current_time: Int) {
    {
      id: resource.id,
      name: resource.name,
      resource_type: resource.resource_type,
      allocated: resource.allocated,
      allocation_time: resource.allocation_time,
      last_accessed: current_time,
      usage_count: resource.usage_count + 1
    }
  }
  
  // Simulate resource usage
  match db_connection {
    Some(resource) => {
      let updated_resource = track_resource_usage(resource, 1640995250)
      assert_eq(updated_resource.usage_count, 1)
      assert_eq(updated_resource.last_accessed, 1640995250)
    }
    None => assert_true(false)
  }
  
  // Test resource cleanup based on LRU strategy
  let cleanup_unused_resources = fn(manager: ResourceManager, max_idle_time: Int, current_time: Int) {
    let mut active_resources = []
    let mut cleaned_resources = []
    
    for resource in manager.resources {
      if resource.allocated and (current_time - resource.last_accessed) > max_idle_time {
        cleaned_resources = cleaned_resources.push(resource.id)
      } else {
        active_resources = active_resources.push(resource)
      }
    }
    
    {
      resources: active_resources,
      max_resources: manager.max_resources,
      allocation_strategy: manager.allocation_strategy
    }
  }
  
  // Test resource pool management
  type ResourcePool = {
    available: Array[Resource],
    in_use: Array[Resource],
    max_size: Int
  }
  
  let create_resource_pool = fn(max_size: Int) {
    {
      available: [],
      in_use: [],
      max_size
    }
  }
  
  let acquire_from_pool = fn(pool: ResourcePool, resource_type: String) {
    if pool.available.length() > 0 {
      let resource = pool.available[0]
      let remaining_available = pool.available.slice(1)
      
      {
        available: remaining_available,
        in_use: pool.in_use.push(resource),
        max_size: pool.max_size
      }
    } else if pool.available.length() + pool.in_use.length() < pool.max_size {
      let new_resource = {
        id: "pool-" + (pool.available.length() + pool.in_use.length()).to_string(),
        name: "pooled_resource",
        resource_type,
        allocated: true,
        allocation_time: 1640995200,
        last_accessed: 1640995200,
        usage_count: 0
      }
      
      {
        available: pool.available,
        in_use: pool.in_use.push(new_resource),
        max_size: pool.max_size
      }
    } else {
      pool  // No available resources and at max capacity
    }
  }
  
  let release_to_pool = fn(pool: ResourcePool, resource_id: String) {
    let mut updated_in_use = []
    let mut released_resource = None
    
    for resource in pool.in_use {
      if resource.id == resource_id {
        released_resource = Some({
          id: resource.id,
          name: resource.name,
          resource_type: resource.resource_type,
          allocated: false,
          allocation_time: resource.allocation_time,
          last_accessed: resource.last_accessed,
          usage_count: resource.usage_count
        })
      } else {
        updated_in_use = updated_in_use.push(resource)
      }
    }
    
    match released_resource {
      Some(resource) => {
        {
          available: pool.available.push(resource),
          in_use: updated_in_use,
          max_size: pool.max_size
        }
      }
      None => pool
    }
  }
  
  // Test resource pool operations
  let resource_pool = create_resource_pool(5)
  let pool_after_acquire = acquire_from_pool(resource_pool, "database")
  assert_eq(pool_after_acquire.in_use.length(), 1)
  assert_eq(pool_after_acquire.available.length(), 0)
  
  let pool_after_release = release_to_pool(pool_after_acquire, "pool-0")
  assert_eq(pool_after_release.in_use.length(), 0)
  assert_eq(pool_after_release.available.length(), 1)
  
  // Test resource monitoring and metrics
  type ResourceMetrics = {
    total_resources: Int,
    allocated_resources: Int,
    utilization_rate: Float,
    average_usage_count: Float,
    oldest_resource_age: Int
  }
  
  let calculate_resource_metrics = fn(manager: ResourceManager, current_time: Int) {
    let total = manager.resources.length()
    let allocated = manager.resources.filter(fn(r) { r.allocated }).length()
    
    let utilization_rate = if total > 0 {
      allocated.to_float() / total.to_float()
    } else {
      0.0
    }
    
    let total_usage = manager.resources.reduce(fn(acc, r) { acc + r.usage_count }, 0)
    let average_usage = if total > 0 {
      total_usage.to_float() / total.to_float()
    } else {
      0.0
    }
    
    let oldest_age = if total > 0 {
      current_time - manager.resources.reduce(fn(acc, r) { 
        if r.allocation_time < acc { r.allocation_time } else { acc } 
      }, current_time)
    } else {
      0
    }
    
    {
      total_resources: total,
      allocated_resources: allocated,
      utilization_rate,
      average_usage_count: average_usage,
      oldest_resource_age: oldest_age
    }
  }
  
  // Test resource leak detection
  let detect_resource_leaks = fn(manager: ResourceManager, max_age: Int, current_time: Int) {
    let mut leaked_resources = []
    
    for resource in manager.resources {
      if resource.allocated and (current_time - resource.allocation_time) > max_age {
        leaked_resources = leaked_resources.push(resource.id)
      }
    }
    
    leaked_resources
  }
  
  // Test resource optimization recommendations
  let generate_optimization_recommendations = fn(metrics: ResourceMetrics) {
    let mut recommendations = []
    
    if metrics.utilization_rate < 0.5 {
      recommendations = recommendations.push("Consider reducing resource pool size")
    } else if metrics.utilization_rate > 0.9 {
      recommendations = recommendations.push("Consider increasing resource pool size")
    }
    
    if metrics.average_usage_count < 1.0 {
      recommendations = recommendations.push("Resources are underutilized")
    }
    
    if metrics.oldest_resource_age > 3600 {
      recommendations = recommendations.push("Some resources have been allocated for a long time")
    }
    
    recommendations
  }
  
  // Create a populated resource manager for testing
  let populated_manager = {
    resources: [
      {
        id: "res-0",
        name: "db_connection_1",
        resource_type: "database",
        allocated: true,
        allocation_time: 1640995000,
        last_accessed: 1640995200,
        usage_count: 5
      },
      {
        id: "res-1",
        name: "cache_connection",
        resource_type: "cache",
        allocated: true,
        allocation_time: 1640995100,
        last_accessed: 1640995250,
        usage_count: 10
      },
      {
        id: "res-2",
        name: "db_connection_2",
        resource_type: "database",
        allocated: false,
        allocation_time: 1640995000,
        last_accessed: 1640995100,
        usage_count: 2
      }
    ],
    max_resources: 10,
    allocation_strategy: "least_recently_used"
  }
  
  let resource_metrics = calculate_resource_metrics(populated_manager, 1640995300)
  assert_eq(resource_metrics.total_resources, 3)
  assert_eq(resource_metrics.allocated_resources, 2)
  assert_eq(resource_metrics.utilization_rate, 2.0 / 3.0)
  assert_eq(resource_metrics.average_usage_count, (5 + 10 + 2).to_float() / 3.0)
  
  let leaked_resources = detect_resource_leaks(populated_manager, 1000, 1640995300)
  assert_eq(leaked_resources.length(), 2)  // Both allocated resources are older than 1000 seconds
  
  let recommendations = generate_optimization_recommendations(resource_metrics)
  assert_eq(recommendations.length(), 0)  // No recommendations needed for healthy metrics
}

// Test 7: Concurrent Safety and Thread Management
test "concurrent safety and thread management" {
  // Define concurrent operation types
  type ConcurrentOperation = {
    operation_id: String,
    operation_type: String,
    thread_id: String,
    start_time: Int,
    end_time: Int,
    status: String,
    resource_accessed: Array[String]
  }
  
  type ThreadSafetyMetrics = {
    total_operations: Int,
    concurrent_operations: Int,
    resource_conflicts: Int,
    deadlock_detected: Bool,
    average_execution_time: Float
  }
  
  type Lock = {
    lock_id: String,
    resource_id: String,
    thread_id: String,
    acquired_time: Int,
    lock_type: String  // "read", "write", "exclusive"
  }
  
  // Create sample concurrent operations
  let operation1 = {
    operation_id: "op-1",
    operation_type: "read",
    thread_id: "thread-1",
    start_time: 1640995200,
    end_time: 1640995210,
    status: "completed",
    resource_accessed: ["resource-1", "resource-2"]
  }
  
  let operation2 = {
    operation_id: "op-2",
    operation_type: "write",
    thread_id: "thread-2",
    start_time: 1640995205,
    end_time: 1640995220,
    status: "completed",
    resource_accessed: ["resource-2", "resource-3"]
  }
  
  let operation3 = {
    operation_id: "op-3",
    operation_type: "read",
    thread_id: "thread-3",
    start_time: 1640995208,
    end_time: 1640995215,
    status: "completed",
    resource_accessed: ["resource-1"]
  }
  
  let concurrent_operations = [operation1, operation2, operation3]
  
  // Test concurrent operation analysis
  let analyze_concurrency = fn(operations: Array[ConcurrentOperation]) {
    let mut max_concurrent = 0
    let mut concurrent_count = 0
    let mut resource_access_counts = []
    
    // Count concurrent operations at each time point
    for time in 1640995200..=1640995220 {
      concurrent_count = 0
      for op in operations {
        if time >= op.start_time and time <= op.end_time {
          concurrent_count = concurrent_count + 1
        }
      }
      if concurrent_count > max_concurrent {
        max_concurrent = concurrent_count
      }
    }
    
    // Analyze resource access patterns
    for op in operations {
      for resource in op.resource_accessed {
        let mut found = false
        let mut updated_counts = []
        
        for (res, count) in resource_access_counts {
          if res == resource {
            updated_counts = updated_counts.push((res, count + 1))
            found = true
          } else {
            updated_counts = updated_counts.push((res, count))
          }
        }
        
        if not(found) {
          updated_counts = updated_counts.push((resource, 1))
        }
        
        resource_access_counts = updated_counts
      }
    }
    
    // Detect potential resource conflicts
    let mut conflicts = 0
    for (resource, count) in resource_access_counts {
      if count > 1 {
        conflicts = conflicts + 1
      }
    }
    
    {
      max_concurrent_operations: max_concurrent,
      resource_conflicts: conflicts,
      resource_access_counts
    }
  }
  
  let concurrency_analysis = analyze_concurrency(concurrent_operations)
  assert_eq(concurrency_analysis.max_concurrent_operations, 3)  // All three overlap
  assert_eq(concurrency_analysis.resource_conflicts, 2)  // resource-1 and resource-2 accessed by multiple operations
  
  // Test lock management
  let acquire_lock = fn(locks: Array[Lock], resource_id: String, thread_id: String, lock_type: String, current_time: Int) {
    // Check if lock can be acquired
    let mut can_acquire = true
    let mut blocking_lock = None
    
    for lock in locks {
      if lock.resource_id == resource_id {
        // Check lock compatibility
        match (lock.lock_type, lock_type) {
          ("read", "read") => can_acquire = true  // Multiple reads allowed
          ("read", "write") => can_acquire = false
          ("write", "read") => can_acquire = false
          ("write", "write") => can_acquire = false
          _ => can_acquire = false
        }
        
        if not(can_acquire) {
          blocking_lock = Some(lock)
          break
        }
      }
    }
    
    if can_acquire {
      let new_lock = {
        lock_id: "lock-" + locks.length().to_string(),
        resource_id,
        thread_id,
        acquired_time: current_time,
        lock_type
      }
      
      {
        acquired: true,
        locks: locks.push(new_lock),
        blocking_lock
      }
    } else {
      {
        acquired: false,
        locks,
        blocking_lock
      }
    }
  }
  
  let release_lock = fn(locks: Array[Lock], lock_id: String) {
    let mut updated_locks = []
    let mut released = false
    
    for lock in locks {
      if lock.lock_id == lock_id {
        released = true
      } else {
        updated_locks = updated_locks.push(lock)
      }
    }
    
    {
      released,
      locks: updated_locks
    }
  }
  
  // Test lock acquisition and release
  let initial_locks = []
  
  let lock_result1 = acquire_lock(initial_locks, "resource-1", "thread-1", "read", 1640995200)
  assert_true(lock_result1.acquired)
  assert_eq(lock_result1.locks.length(), 1)
  
  let lock_result2 = acquire_lock(lock_result1.locks, "resource-1", "thread-2", "read", 1640995205)
  assert_true(lock_result2.acquired)  // Multiple reads should be allowed
  assert_eq(lock_result2.locks.length(), 2)
  
  let lock_result3 = acquire_lock(lock_result2.locks, "resource-1", "thread-3", "write", 1640995210)
  assert_false(lock_result3.acquired)  // Write should be blocked by read locks
  assert_true(lock_result3.blocking_lock.is_some())
  
  // Test deadlock detection
  let detect_deadlock = fn(locks: Array[Lock], waiting_operations: Array[(String, String, String)]) {
    // waiting_operations: (thread_id, resource_id, lock_type)
    let mut deadlock_detected = false
    let mut deadlock_cycle = []
    
    // Simple deadlock detection: check for circular wait
    for waiting_op in waiting_operations {
      let (waiting_thread, waiting_resource, _) = waiting_op
      
      // Find thread holding the waiting resource
      let mut holding_thread = None
      for lock in locks {
        if lock.resource_id == waiting_resource {
          holding_thread = Some(lock.thread_id)
        }
      }
      
      match holding_thread {
        Some(holder) => {
          // Check if holder is waiting for a resource held by waiting_thread
          for other_waiting in waiting_operations {
            let (other_thread, other_resource, _) = other_waiting
            if other_thread == holder {
              for lock in locks {
                if lock.resource_id == other_resource and lock.thread_id == waiting_thread {
                  deadlock_detected = true
                  deadlock_cycle = [waiting_thread, holder, waiting_thread]
                }
              }
            }
          }
        }
        None => {}
      }
    }
    
    {
      deadlock_detected,
      deadlock_cycle
    }
  }
  
  // Test deadlock scenario
  let deadlock_locks = [
    {
      lock_id: "lock-1",
      resource_id: "resource-1",
      thread_id: "thread-1",
      acquired_time: 1640995200,
      lock_type: "write"
    },
    {
      lock_id: "lock-2",
      resource_id: "resource-2",
      thread_id: "thread-2",
      acquired_time: 1640995205,
      lock_type: "write"
    }
  ]
  
  let waiting_operations = [
    ("thread-1", "resource-2", "write"),  // thread-1 waits for resource-2 held by thread-2
    ("thread-2", "resource-1", "write")   // thread-2 waits for resource-1 held by thread-1
  ]
  
  let deadlock_result = detect_deadlock(deadlock_locks, waiting_operations)
  assert_true(deadlock_result.deadlock_detected)
  assert_eq(deadlock_result.deadlock_cycle.length(), 3)
  
  // Test thread safety metrics calculation
  let calculate_thread_safety_metrics = fn(operations: Array[ConcurrentOperation], locks: Array[Lock]) {
    let total_ops = operations.length()
    
    // Calculate concurrent operations
    let mut concurrent_ops = 0
    for i in 0..operations.length() {
      for j in i + 1..operations.length() {
        let op1 = operations[i]
        let op2 = operations[j]
        
        // Check if operations overlap in time
        if (op1.start_time <= op2.end_time) and (op2.start_time <= op1.end_time) {
          concurrent_ops = concurrent_ops + 1
        }
      }
    }
    
    // Calculate resource conflicts
    let mut resource_conflicts = 0
    let resource_access_map = []
    
    for op in operations {
      for resource in op.resource_accessed {
        let mut found = false
        let mut updated_map = []
        
        for (res, ops) in resource_access_map {
          if res == resource {
            updated_map = updated_map.push((res, ops.push(op.operation_id)))
            found = true
          } else {
            updated_map = updated_map.push((res, ops))
          }
        }
        
        if not(found) {
          updated_map = updated_map.push((resource, [op.operation_id]))
        }
        
        resource_access_map = updated_map
      }
    }
    
    for (resource, ops) in resource_access_map {
      if ops.length() > 1 {
        resource_conflicts = resource_conflicts + 1
      }
    }
    
    // Calculate average execution time
    let total_time = operations.reduce(fn(acc, op) { 
      acc + (op.end_time - op.start_time) 
    }, 0)
    
    let avg_time = if total_ops > 0 {
      total_time.to_float() / total_ops.to_float()
    } else {
      0.0
    }
    
    {
      total_operations: total_ops,
      concurrent_operations: concurrent_ops,
      resource_conflicts,
      deadlock_detected: false,
      average_execution_time: avg_time
    }
  }
  
  let thread_safety_metrics = calculate_thread_safety_metrics(concurrent_operations, [])
  assert_eq(thread_safety_metrics.total_operations, 3)
  assert_eq(thread_safety_metrics.concurrent_operations, 3)  // All pairs overlap
  assert_eq(thread_safety_metrics.resource_conflicts, 2)
  assert_eq(thread_safety_metrics.average_execution_time, (10 + 15 + 7).to_float() / 3.0)
  
  // Test race condition detection
  let detect_race_conditions = fn(operations: Array[ConcurrentOperation]) {
    let mut race_conditions = []
    
    // Look for operations that access the same resource with different types
    for i in 0..operations.length() {
      for j in i + 1..operations.length() {
        let op1 = operations[i]
        let op2 = operations[j]
        
        // Check if operations overlap in time
        if (op1.start_time <= op2.end_time) and (op2.start_time <= op1.end_time) {
          // Check if they access the same resources
          for resource1 in op1.resource_accessed {
            for resource2 in op2.resource_accessed {
              if resource1 == resource2 and op1.operation_type != op2.operation_type {
                race_conditions = race_conditions.push({
                  resource: resource1,
                  operation1: op1.operation_id,
                  operation2: op2.operation_id,
                  operation_types: (op1.operation_type, op2.operation_type)
                })
              }
            }
          }
        }
      }
    }
    
    race_conditions
  }
  
  let race_conditions = detect_race_conditions(concurrent_operations)
  assert_eq(race_conditions.length(), 2)  // resource-2 has read/write conflict, resource-1 has read/read (no conflict)
}

// Test 8: Configuration Management and Dynamic Updates
test "configuration management and dynamic updates" {
  // Define configuration types
  type ConfigValue = {
    key: String,
    value: String,
    value_type: String,  // "string", "int", "float", "bool", "array"
    default_value: String,
    description: String,
    required: Bool,
    sensitive: Bool
  }
  
  type Configuration = {
    name: String,
    version: String,
    values: Array[ConfigValue],
    last_updated: Int,
    environment: String
  }
  
  type ConfigChange = {
    key: String,
    old_value: String,
    new_value: String,
    changed_by: String,
    timestamp: Int,
    reason: String
  }
  
  // Create base configuration
  let base_config_values = [
    {
      key: "service.name",
      value: "azimuth-telemetry",
      value_type: "string",
      default_value: "azimuth-telemetry",
      description: "Name of the telemetry service",
      required: true,
      sensitive: false
    },
    {
      key: "service.port",
      value: "8080",
      value_type: "int",
      default_value: "8080",
      description: "Port number for the service",
      required: true,
      sensitive: false
    },
    {
      key: "database.url",
      value: "postgresql://localhost:5432/telemetry",
      value_type: "string",
      default_value: "postgresql://localhost:5432/telemetry",
      description: "Database connection URL",
      required: true,
      sensitive: true
    },
    {
      key: "log.level",
      value: "info",
      value_type: "string",
      default_value: "info",
      description: "Logging level",
      required: false,
      sensitive: false
    },
    {
      key: "metrics.enabled",
      value: "true",
      value_type: "bool",
      default_value: "true",
      description: "Enable metrics collection",
      required: false,
      sensitive: false
    }
  ]
  
  let base_configuration = {
    name: "azimuth-config",
    version: "1.0.0",
    values: base_config_values,
    last_updated: 1640995200,
    environment: "development"
  }
  
  // Test configuration value lookup
  let get_config_value = fn(config: Configuration, key: String) {
    let mut found = None
    for value in config.values {
      if value.key == key {
        found = Some(value)
      }
    }
    found
  }
  
  let service_name = get_config_value(base_configuration, "service.name")
  assert_true(service_name.is_some())
  
  match service_name {
    Some(value) => {
      assert_eq(value.key, "service.name")
      assert_eq(value.value, "azimuth-telemetry")
      assert_eq(value.value_type, "string")
      assert_true(value.required)
      assert_false(value.sensitive)
    }
    None => assert_true(false)
  }
  
  let missing_config = get_config_value(base_configuration, "missing.key")
  assert_true(missing_config.is_none())
  
  // Test configuration validation
  let validate_configuration = fn(config: Configuration) {
    let mut errors = []
    let mut warnings = []
    
    // Check required values
    for value in config.values {
      if value.required and (value.value == "" or value.value == value.default_value) {
        errors = errors.push("Required configuration missing: " + value.key)
      }
      
      // Validate value types
      match value.value_type {
        "int" => {
          let parsed = value.value.to_int()
          if parsed.is_none() {
            errors = errors.push("Invalid integer value for " + value.key)
          }
        }
        "float" => {
          let parsed = value.value.to_float()
          if parsed.is_none() {
            errors = errors.push("Invalid float value for " + value.key)
          }
        }
        "bool" => {
          if value.value != "true" and value.value != "false" {
            errors = errors.push("Invalid boolean value for " + value.key)
          }
        }
        _ => {}
      }
    }
    
    // Check for deprecated values
    if config.environment == "production" and get_config_value(config, "log.level").unwrap().value == "debug" {
      warnings = warnings.push("Debug logging in production environment")
    }
    
    {
      valid: errors.length() == 0,
      errors,
      warnings
    }
  }
  
  let config_validation = validate_configuration(base_configuration)
  assert_true(config_validation.valid)
  assert_eq(config_validation.errors.length(), 0)
  assert_eq(config_validation.warnings.length(), 0)
  
  // Test configuration update
  let update_config_value = fn(config: Configuration, key: String, new_value: String, changed_by: String, reason: String) {
    let mut updated_values = []
    let mut old_value = ""
    let mut found = false
    
    for value in config.values {
      if value.key == key {
        old_value = value.value
        updated_values = updated_values.push({
          key: value.key,
          value: new_value,
          value_type: value.value_type,
          default_value: value.default_value,
          description: value.description,
          required: value.required,
          sensitive: value.sensitive
        })
        found = true
      } else {
        updated_values = updated_values.push(value)
      }
    }
    
    if found {
      {
        configuration: {
          name: config.name,
          version: config.version,
          values: updated_values,
          last_updated: 1640995250,
          environment: config.environment
        },
        change: Some({
          key,
          old_value,
          new_value,
          changed_by,
          timestamp: 1640995250,
          reason
        })
      }
    } else {
      {
        configuration: config,
        change: None
      }
    }
  }
  
  let config_update = update_config_value(base_configuration, "service.port", "9090", "admin", "Port conflict resolution")
  assert_true(config_update.change.is_some())
  
  match config_update.change {
    Some(change) => {
      assert_eq(change.key, "service.port")
      assert_eq(change.old_value, "8080")
      assert_eq(change.new_value, "9090")
      assert_eq(change.changed_by, "admin")
      assert_eq(change.reason, "Port conflict resolution")
    }
    None => assert_true(false)
  }
  
  // Test configuration rollback
  let rollback_config_change = fn(config: Configuration, change: ConfigChange, original_config: Configuration) {
    let rollback_result = update_config_value(config, change.key, change.old_value, "system", "Rollback: " + change.reason)
    
    match rollback_result.change {
      Some(rollback_change) => {
        {
          configuration: rollback_result.configuration,
          rollback_change: Some(rollback_change),
          success: true
        }
      }
      None => {
        {
          configuration: config,
          rollback_change: None,
          success: false
        }
      }
    }
  }
  
  // Test configuration inheritance
  let inherit_configuration = fn(base_config: Configuration, override_config: Configuration) {
    let mut merged_values = []
    let processed_keys = []
    
    // Start with base configuration
    for value in base_config.values {
      merged_values = merged_values.push(value)
      processed_keys = processed_keys.push(value.key)
    }
    
    // Apply overrides
    for value in override_config.values {
      if not(processed_keys.contains(value.key)) {
        merged_values = merged_values.push(value)
      } else {
        // Replace existing value
        let mut updated_values = []
        for merged_value in merged_values {
          if merged_value.key == value.key {
            updated_values = updated_values.push(value)
          } else {
            updated_values = updated_values.push(merged_value)
          }
        }
        merged_values = updated_values
      }
    }
    
    {
      name: base_config.name + "-merged",
      version: base_config.version,
      values: merged_values,
      last_updated: 1640995300,
      environment: base_config.environment
    }
  }
  
  // Test configuration environment-specific overrides
  let get_environment_config = fn(config: Configuration, environment: String) {
    let mut env_values = []
    
    for value in config.values {
      let env_value = match environment {
        "development" => {
          match value.key {
            "log.level" => "debug"
            "metrics.enabled" => "true"
            _ => value.value
          }
        }
        "production" => {
          match value.key {
            "log.level" => "warn"
            "metrics.enabled" => "true"
            _ => value.value
          }
        }
        "testing" => {
          match value.key {
            "log.level" => "error"
            "metrics.enabled" => "false"
            _ => value.value
          }
        }
        _ => value.value
      }
      
      env_values = env_values.push({
        key: value.key,
        value: env_value,
        value_type: value.value_type,
        default_value: value.default_value,
        description: value.description,
        required: value.required,
        sensitive: value.sensitive
      })
    }
    
    {
      name: config.name + "-" + environment,
      version: config.version,
      values: env_values,
      last_updated: config.last_updated,
      environment
    }
  }
  
  let dev_config = get_environment_config(base_configuration, "development")
  let prod_config = get_environment_config(base_configuration, "production")
  let test_config = get_environment_config(base_configuration, "testing")
  
  let dev_log_level = get_config_value(dev_config, "log.level")
  let prod_log_level = get_config_value(prod_config, "log.level")
  let test_log_level = get_config_value(test_config, "log.level")
  
  assert_eq(dev_log_level.unwrap().value, "debug")
  assert_eq(prod_log_level.unwrap().value, "warn")
  assert_eq(test_log_level.unwrap().value, "error")
  
  // Test configuration change tracking
  type ConfigHistory = {
    configuration: Configuration,
    changes: Array[ConfigChange]
  }
  
  let track_config_changes = fn(history: ConfigHistory, new_change: ConfigChange) {
    {
      configuration: history.configuration,
      changes: history.changes.push(new_change)
    }
  }
  
  let get_config_history = fn(history: ConfigHistory, key: String) {
    history.changes.filter(fn(change) { change.key == key })
  }
  
  // Test configuration diff
  let diff_configurations = fn(config1: Configuration, config2: Configuration) {
    let mut differences = []
    
    for value1 in config1.values {
      match get_config_value(config2, value1.key) {
        Some(value2) => {
          if value1.value != value2.value {
            differences = differences.push({
              key: value1.key,
              value1: value1.value,
              value2: value2.value,
              change_type: "modified"
            })
          }
        }
        None => {
          differences = differences.push({
            key: value1.key,
            value1: value1.value,
            value2: "",
            change_type: "removed"
          })
        }
      }
    }
    
    for value2 in config2.values {
      if get_config_value(config1, value2.key).is_none() {
        differences = differences.push({
          key: value2.key,
          value1: "",
          value2: value2.value,
          change_type: "added"
        })
      }
    }
    
    differences
  }
  
  // Test configuration export/import
  let export_configuration = fn(config: Configuration, format: String) {
    match format {
      "json" => {
        let json_string = "{" +
          "\"name\":\"" + config.name + "\"," +
          "\"version\":\"" + config.version + "\"," +
          "\"environment\":\"" + config.environment + "\"," +
          "\"last_updated\":" + config.last_updated.to_string() + "," +
          "\"values\":{"
        
        let mut value_pairs = []
        for value in config.values {
          value_pairs = value_pairs.push("\"" + value.key + "\":\"" + value.value + "\"")
        }
        
        json_string + value_pairs.join(",") + "}}"
      }
      "properties" => {
        let mut lines = []
        for value in config.values {
          lines = lines.push(value.key + "=" + value.value)
        }
        lines.join("\n")
      }
      _ => "Unsupported format"
    }
  }
  
  let json_export = export_configuration(base_configuration, "json")
  assert_true(json_export.contains("\"name\":\"azimuth-config\""))
  assert_true(json_export.contains("\"service.name\":\"azimuth-telemetry\""))
  
  let properties_export = export_configuration(base_configuration, "properties")
  assert_true(properties_export.contains("service.name=azimuth-telemetry"))
  assert_true(properties_export.contains("service.port=8080"))
}

// Test 9: Advanced Caching Mechanisms
test "advanced caching mechanisms" {
  // Define cache types
  type CacheEntry = {
    key: String,
    value: String,
    created_time: Int,
    last_accessed: Int,
    access_count: Int,
    size: Int,
    ttl: Int  // Time to live in seconds
  }
  
  type Cache = {
    entries: Array[CacheEntry],
    max_size: Int,
    max_memory: Int,
    eviction_policy: String,  // "lru", "lfu", "ttl", "random"
    current_memory: Int
  }
  
  type CacheMetrics = {
    hits: Int,
    misses: Int,
    evictions: Int,
    hit_rate: Float,
    average_access_time: Float
  }
  
  // Create cache
  let create_cache = fn(max_size: Int, max_memory: Int, eviction_policy: String) {
    {
      entries: [],
      max_size,
      max_memory,
      eviction_policy,
      current_memory: 0
    }
  }
  
  let cache = create_cache(100, 1024 * 1024, "lru")  // 100 entries, 1MB memory
  
  // Test cache entry creation
  let create_cache_entry = fn(key: String, value: String, ttl: Int, current_time: Int) {
    {
      key,
      value,
      created_time: current_time,
      last_accessed: current_time,
      access_count: 0,
      size: key.length() + value.length(),
      ttl
    }
  }
  
  // Test cache put operation
  let cache_put = fn(cache: Cache, key: String, value: String, ttl: Int, current_time: Int) {
    let new_entry = create_cache_entry(key, value, ttl, current_time)
    
    // Check if entry already exists
    let mut existing_index = -1
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        existing_index = i
      }
    }
    
    if existing_index >= 0 {
      // Update existing entry
      let mut updated_entries = []
      for i in 0..cache.entries.length() {
        if i == existing_index {
          updated_entries = updated_entries.push(new_entry)
        } else {
          updated_entries = updated_entries.push(cache.entries[i])
        }
      }
      
      {
        entries: updated_entries,
        max_size: cache.max_size,
        max_memory: cache.max_memory,
        eviction_policy: cache.eviction_policy,
        current_memory: cache.current_memory - cache.entries[existing_index].size + new_entry.size
      }
    } else {
      // Add new entry
      if cache.entries.length() < cache.max_size and 
         cache.current_memory + new_entry.size <= cache.max_memory {
        {
          entries: cache.entries.push(new_entry),
          max_size: cache.max_size,
          max_memory: cache.max_memory,
          eviction_policy: cache.eviction_policy,
          current_memory: cache.current_memory + new_entry.size
        }
      } else {
        // Need to evict entries
        cache  // Simplified - would implement eviction logic
      }
    }
  }
  
  // Test cache get operation
  let cache_get = fn(cache: Cache, key: String, current_time: Int) {
    let mut found_entry = None
    let mut found_index = -1
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        // Check TTL
        if (current_time - cache.entries[i].created_time) <= cache.entries[i].ttl {
          found_entry = Some(cache.entries[i])
          found_index = i
        }
      }
    }
    
    match found_entry {
      Some(entry) => {
        // Update access information
        let updated_entry = {
          key: entry.key,
          value: entry.value,
          created_time: entry.created_time,
          last_accessed: current_time,
          access_count: entry.access_count + 1,
          size: entry.size,
          ttl: entry.ttl
        }
        
        let mut updated_entries = []
        for i in 0..cache.entries.length() {
          if i == found_index {
            updated_entries = updated_entries.push(updated_entry)
          } else {
            updated_entries = updated_entries.push(cache.entries[i])
          }
        }
        
        {
          cache: {
            entries: updated_entries,
            max_size: cache.max_size,
            max_memory: cache.max_memory,
            eviction_policy: cache.eviction_policy,
            current_memory: cache.current_memory
          },
          value: Some(entry.value),
          hit: true
        }
      }
      None => {
        {
          cache,
          value: None,
          hit: false
        }
      }
    }
  }
  
  // Test cache operations
  let cache_after_put = cache_put(cache, "user:123", "John Doe", 3600, 1640995200)
  assert_eq(cache_after_put.entries.length(), 1)
  
  let get_result = cache_get(cache_after_put, "user:123", 1640995250)
  assert_true(get_result.hit)
  assert_eq(get_result.value, Some("John Doe"))
  assert_eq(get_result.cache.entries[0].access_count, 1)
  assert_eq(get_result.cache.entries[0].last_accessed, 1640995250)
  
  let get_missing = cache_get(cache_after_put, "user:456", 1640995250)
  assert_false(get_missing.hit)
  assert_eq(get_missing.value, None)
  
  // Test TTL expiration
  let expired_get = cache_get(cache_after_put, "user:123", 1640995200 + 3700)  // After TTL
  assert_false(expired_get.hit)
  assert_eq(expired_get.value, None)
  
  // Test LRU eviction
  let evict_lru = fn(cache: Cache) {
    if cache.entries.length() == 0 {
      cache
    } else {
      // Find least recently used entry
      let mut lru_index = 0
      let mut lru_time = cache.entries[0].last_accessed
      
      for i in 1..cache.entries.length() {
        if cache.entries[i].last_accessed < lru_time {
          lru_time = cache.entries[i].last_accessed
          lru_index = i
        }
      }
      
      // Remove LRU entry
      let mut updated_entries = []
      let evicted_size = cache.entries[lru_index].size
      
      for i in 0..cache.entries.length() {
        if i != lru_index {
          updated_entries = updated_entries.push(cache.entries[i])
        }
      }
      
      {
        entries: updated_entries,
        max_size: cache.max_size,
        max_memory: cache.max_memory,
        eviction_policy: cache.eviction_policy,
        current_memory: cache.current_memory - evicted_size
      }
    }
  }
  
  // Test LFU eviction
  let evict_lfu = fn(cache: Cache) {
    if cache.entries.length() == 0 {
      cache
    } else {
      // Find least frequently used entry
      let mut lfu_index = 0
      let mut lfu_count = cache.entries[0].access_count
      
      for i in 1..cache.entries.length() {
        if cache.entries[i].access_count < lfu_count {
          lfu_count = cache.entries[i].access_count
          lfu_index = i
        }
      }
      
      // Remove LFU entry
      let mut updated_entries = []
      let evicted_size = cache.entries[lfu_index].size
      
      for i in 0..cache.entries.length() {
        if i != lfu_index {
          updated_entries = updated_entries.push(cache.entries[i])
        }
      }
      
      {
        entries: updated_entries,
        max_size: cache.max_size,
        max_memory: cache.max_memory,
        eviction_policy: cache.eviction_policy,
        current_memory: cache.current_memory - evicted_size
      }
    }
  }
  
  // Test cache statistics
  let calculate_cache_stats = fn(metrics: CacheMetrics) {
    let total_requests = metrics.hits + metrics.misses
    let hit_rate = if total_requests > 0 {
      metrics.hits.to_float() / total_requests.to_float()
    } else {
      0.0
    }
    
    {
      hit_rate,
      total_requests,
      eviction_rate: if total_requests > 0 {
        metrics.evictions.to_float() / total_requests.to_float()
      } else {
        0.0
      }
    }
  }
  
  // Test cache warming
  let warm_cache = fn(cache: Cache, data: Array[(String, String)], ttl: Int, current_time: Int) {
    let mut warmed_cache = cache
    
    for (key, value) in data {
      warmed_cache = cache_put(warmed_cache, key, value, ttl, current_time)
    }
    
    warmed_cache
  }
  
  // Test cache invalidation
  let invalidate_cache_entry = fn(cache: Cache, key: String) {
    let mut updated_entries = []
    let mut removed_size = 0
    
    for entry in cache.entries {
      if entry.key != key {
        updated_entries = updated_entries.push(entry)
      } else {
        removed_size = entry.size
      }
    }
    
    {
      entries: updated_entries,
      max_size: cache.max_size,
      max_memory: cache.max_memory,
      eviction_policy: cache.eviction_policy,
      current_memory: cache.current_memory - removed_size
    }
  }
  
  let invalidate_by_pattern = fn(cache: Cache, pattern: String) {
    let mut updated_entries = []
    let mut removed_size = 0
    
    for entry in cache.entries {
      if not(entry.key.contains(pattern)) {
        updated_entries = updated_entries.push(entry)
      } else {
        removed_size = removed_size + entry.size
      }
    }
    
    {
      entries: updated_entries,
      max_size: cache.max_size,
      max_memory: cache.max_memory,
      eviction_policy: cache.eviction_policy,
      current_memory: cache.current_memory - removed_size
    }
  }
  
  // Test cache performance optimization
  let optimize_cache_performance = fn(cache: Cache) {
    // Analyze access patterns and suggest optimizations
    let mut hot_keys = []
    let mut cold_keys = []
    
    for entry in cache.entries {
      if entry.access_count > 10 {
        hot_keys = hot_keys.push(entry.key)
      } else if entry.access_count < 2 {
        cold_keys = cold_keys.push(entry.key)
      }
    }
    
    {
      hot_keys,
      cold_keys,
      optimization_suggestions: [
        if hot_keys.length() > 0 {
          "Consider increasing TTL for hot keys: " + hot_keys.join(", ")
        } else {
          ""
        },
        if cold_keys.length() > 0 {
          "Consider preloading or removing cold keys: " + cold_keys.join(", ")
        } else {
          ""
        }
      ].filter(fn(s) { s != "" })
    }
  }
  
  // Create a populated cache for testing
  let populated_cache = {
    let mut cache = create_cache(10, 1024, "lru")
    cache = cache_put(cache, "key1", "value1", 3600, 1640995200)
    cache = cache_put(cache, "key2", "value2", 3600, 1640995210)
    cache = cache_put(cache, "key3", "value3", 3600, 1640995220)
    
    // Simulate access patterns
    let get1 = cache_get(cache, "key1", 1640995230)
    cache = get1.cache
    
    let get2 = cache_get(cache, "key1", 1640995240)
    cache = get2.cache
    
    let get3 = cache_get(cache, "key2", 1640995250)
    cache = get3.cache
    
    cache
  }
  
  assert_eq(populated_cache.entries.length(), 3)
  
  // Test cache metrics calculation
  let cache_metrics = {
    hits: 15,
    misses: 5,
    evictions: 2,
    hit_rate: 0.0,  // Will be calculated
    average_access_time: 2.5
  }
  
  let stats = calculate_cache_stats(cache_metrics)
  assert_eq(stats.hit_rate, 0.75)  // 15 / (15 + 5)
  assert_eq(stats.total_requests, 20)
  assert_eq(stats.eviction_rate, 0.1)  // 2 / 20
  
  // Test cache invalidation
  let cache_after_invalidation = invalidate_cache_entry(populated_cache, "key2")
  assert_eq(cache_after_invalidation.entries.length(), 2)
  
  let cache_after_pattern_invalidation = invalidate_by_pattern(populated_cache, "key1")
  assert_eq(cache_after_pattern_invalidation.entries.length(), 2)  // key1 removed, key2 and key3 remain
}

// Test 10: Advanced Data Analysis and Pattern Recognition
test "advanced data analysis and pattern recognition" {
  // Define data analysis types
  type DataPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    data_points: Array[DataPoint],
    unit: String
  }
  
  type Anomaly = {
    timestamp: Int,
    value: Float,
    expected_range: (Float, Float),
    severity: String,  // "low", "medium", "high"
    description: String
  }
  
  type Pattern = {
    pattern_type: String,
    confidence: Float,
    description: String,
    time_range: (Int, Int)
  }
  
  // Create sample time series data
  let create_time_series = fn(name: String, values: Array[Float], start_time: Int, interval: Int) {
    let mut data_points = []
    
    for i in 0..values.length() {
      data_points = data_points.push({
        timestamp: start_time + i * interval,
        value: values[i],
        metadata: []
      })
    }
    
    {
      name,
      data_points,
      unit: "ms"
    }
  }
  
  let response_times = [120.0, 125.0, 118.0, 130.0, 500.0, 122.0, 115.0, 128.0, 121.0, 124.0]
  let response_time_series = create_time_series("response_time", response_times, 1640995200, 60)
  
  // Test statistical analysis
  let calculate_statistics = fn(data: Array[Float]) {
    if data.length() == 0 {
      {
        mean: 0.0,
        median: 0.0,
        min: 0.0,
        max: 0.0,
        std_dev: 0.0,
        variance: 0.0
      }
    } else {
      // Calculate mean
      let sum = data.reduce(fn(acc, x) { acc + x }, 0.0)
      let mean = sum / data.length().to_float()
      
      // Calculate min and max
      let mut min = data[0]
      let mut max = data[0]
      
      for value in data {
        if value < min {
          min = value
        }
        if value > max {
          max = value
        }
      }
      
      // Calculate median
      let sorted_data = data.sort(fn(a, b) { 
        if a < b { -1 } else if a > b { 1 } else { 0 }
      })
      
      let median = if sorted_data.length() % 2 == 0 {
        let mid = sorted_data.length() / 2
        (sorted_data[mid - 1] + sorted_data[mid]) / 2.0
      } else {
        sorted_data[sorted_data.length() / 2]
      }
      
      // Calculate variance and standard deviation
      let variance = data.reduce(fn(acc, x) { 
        acc + (x - mean) * (x - mean) 
      }, 0.0) / data.length().to_float()
      
      let std_dev = variance.sqrt()
      
      {
        mean,
        median,
        min,
        max,
        std_dev,
        variance
      }
    }
  }
  
  let stats = calculate_statistics(response_times)
  
  // Expected: mean ≈ 162.3 (including the outlier 500.0)
  assert_eq(stats.mean, (120.0 + 125.0 + 118.0 + 130.0 + 500.0 + 122.0 + 115.0 + 128.0 + 121.0 + 124.0) / 10.0)
  assert_eq(stats.min, 115.0)
  assert_eq(stats.max, 500.0)
  
  // Test anomaly detection
  let detect_anomalies = fn(series: TimeSeries, threshold_multiplier: Float) {
    let values = series.data_points.map(fn(dp) { dp.value })
    let stats = calculate_statistics(values)
    
    let mut anomalies = []
    let threshold = stats.std_dev * threshold_multiplier
    
    for data_point in series.data_points {
      let deviation = (data_point.value - stats.mean).abs()
      if deviation > threshold {
        let severity = if deviation > threshold * 2.0 {
          "high"
        } else if deviation > threshold * 1.5 {
          "medium"
        } else {
          "low"
        }
        
        anomalies = anomalies.push({
          timestamp: data_point.timestamp,
          value: data_point.value,
          expected_range: (stats.mean - threshold, stats.mean + threshold),
          severity,
          description: "Value deviates from expected range"
        })
      }
    }
    
    anomalies
  }
  
  let anomalies = detect_anomalies(response_time_series, 2.0)
  assert_eq(anomalies.length(), 1)  // Only the 500.0 value should be detected
  assert_eq(anomalies[0].value, 500.0)
  assert_eq(anomalies[0].severity, "high")
  
  // Test trend analysis
  enum Trend {
    Increasing
    Decreasing
    Stable
    Volatile
  }
  
  let analyze_trend = fn(series: TimeSeries, window_size: Int) {
    if series.data_points.length() < window_size {
      Trend::Stable
    } else {
      let mut increasing_count = 0
      let mut decreasing_count = 0
      
      for i in window_size..series.data_points.length() {
        let current = series.data_points[i].value
        let previous = series.data_points[i - window_size].value
        
        if current > previous * 1.05 {  // 5% increase threshold
          increasing_count = increasing_count + 1
        } else if current < previous * 0.95 {  // 5% decrease threshold
          decreasing_count = decreasing_count + 1
        }
      }
      
      let total_comparisons = series.data_points.length() - window_size
      let increasing_ratio = increasing_count.to_float() / total_comparisons.to_float()
      let decreasing_ratio = decreasing_count.to_float() / total_comparisons.to_float()
      
      if increasing_ratio > 0.6 {
        Trend::Increasing
      } else if decreasing_ratio > 0.6 {
        Trend::Decreasing
      } else if (increasing_ratio + decreasing_ratio) > 0.7 {
        Trend::Volatile
      } else {
        Trend::Stable
      }
    }
  }
  
  let trend = analyze_trend(response_time_series, 2)
  assert_eq(trend, Trend::Stable)  // Overall trend is stable despite one outlier
  
  // Test pattern recognition
  let recognize_patterns = fn(series: TimeSeries) {
    let values = series.data_points.map(fn(dp) { dp.value })
    let mut patterns = []
    
    // Detect spike pattern
    let stats = calculate_statistics(values)
    let spike_threshold = stats.mean + stats.std_dev * 3.0
    
    for i in 1..values.length() - 1 {
      if values[i] > spike_threshold and 
         values[i-1] < spike_threshold and 
         values[i+1] < spike_threshold {
        patterns = patterns.push({
          pattern_type: "spike",
          confidence: 0.8,
          description: "Temporary spike in values",
          time_range: (series.data_points[i-1].timestamp, series.data_points[i+1].timestamp)
        })
      }
    }
    
    // Detect gradual increase pattern
    let mut consecutive_increases = 0
    let mut max_consecutive_increases = 0
    
    for i in 1..values.length() {
      if values[i] > values[i-1] {
        consecutive_increases = consecutive_increases + 1
        if consecutive_increases > max_consecutive_increases {
          max_consecutive_increases = consecutive_increases
        }
      } else {
        consecutive_increases = 0
      }
    }
    
    if max_consecutive_increases >= 3 {
      patterns = patterns.push({
        pattern_type: "gradual_increase",
        confidence: 0.7,
        description: "Values show gradual increase over time",
        time_range: (series.data_points[0].timestamp, series.data_points[series.data_points.length() - 1].timestamp)
      })
    }
    
    patterns
  }
  
  let patterns = recognize_patterns(response_time_series)
  assert_eq(patterns.length(), 1)  // Should detect the spike pattern
  assert_eq(patterns[0].pattern_type, "spike")
  
  // Test correlation analysis
  let calculate_correlation = fn(series1: TimeSeries, series2: TimeSeries) {
    if series1.data_points.length() != series2.data_points.length() {
      0.0  // Cannot calculate correlation for different lengths
    } else {
      let values1 = series1.data_points.map(fn(dp) { dp.value })
      let values2 = series2.data_points.map(fn(dp) { dp.value })
      
      let stats1 = calculate_statistics(values1)
      let stats2 = calculate_statistics(values2)
      
      let mut numerator = 0.0
      let mut denominator1 = 0.0
      let mut denominator2 = 0.0
      
      for i in 0..values1.length() {
        let diff1 = values1[i] - stats1.mean
        let diff2 = values2[i] - stats2.mean
        
        numerator = numerator + diff1 * diff2
        denominator1 = denominator1 + diff1 * diff1
        denominator2 = denominator2 + diff2 * diff2
      }
      
      if denominator1 > 0.0 and denominator2 > 0.0 {
        numerator / (denominator1.sqrt() * denominator2.sqrt())
      } else {
        0.0
      }
    }
  }
  
  // Create correlated data for testing
  let cpu_usage = [45.0, 48.0, 44.0, 52.0, 75.0, 47.0, 43.0, 50.0, 46.0, 49.0]
  let cpu_series = create_time_series("cpu_usage", cpu_usage, 1640995200, 60)
  
  let correlation = calculate_correlation(response_time_series, cpu_series)
  assert_true(correlation > 0.8)  // Should be highly correlated due to the spike
  
  // Test forecasting
  let simple_forecast = fn(series: TimeSeries, forecast_periods: Int) {
    let values = series.data_points.map(fn(dp) { dp.value })
    let stats = calculate_statistics(values)
    
    // Simple moving average forecast
    let window_size = 3
    let mut forecasts = []
    
    if values.length() >= window_size {
      for i in 0..forecast_periods {
        let start_index = values.length() - window_size + i
        let end_index = values.length() + i
        
        if start_index >= 0 and end_index <= values.length() {
          let window_sum = values.slice(start_index, end_index).reduce(fn(acc, x) { acc + x }, 0.0)
          let forecast_value = window_sum / window_size.to_float()
          
          forecasts = forecasts.push({
            timestamp: series.data_points[series.data_points.length() - 1].timestamp + (i + 1) * 60,
            value: forecast_value,
            metadata: [("forecast", "true"), ("method", "moving_average")]
          })
        }
      }
    }
    
    forecasts
  }
  
  let forecasts = simple_forecast(response_time_series, 3)
  assert_eq(forecasts.length(), 3)
  
  // Test seasonality detection
  let detect_seasonality = fn(series: TimeSeries, period: Int) {
    if series.data_points.length() < period * 2 {
      false
    } else {
      let values = series.data_points.map(fn(dp) { dp.value })
      let mut seasonal_correlations = []
      
      for lag in 1..=period {
        let mut correlation_sum = 0.0
        let count = values.length() - lag
        
        for i in 0..count {
          correlation_sum = correlation_sum + values[i] * values[i + lag]
        }
        
        if count > 0 {
          seasonal_correlations = seasonal_correlations.push(correlation_sum / count.to_float())
        }
      }
      
      // Check if any lag shows strong correlation
      seasonal_correlations.any(fn(c) { c > 0.7 })
    }
  }
  
  // Test data aggregation
  let aggregate_data = fn(series: TimeSeries, aggregation_type: String, window_size: Int) {
    let values = series.data_points.map(fn(dp) { dp.value })
    let mut aggregated = []
    
    for i in 0..values.length() by window_size {
      let end = if i + window_size < values.length() {
        i + window_size
      } else {
        values.length()
      }
      
      let window = values.slice(i, end)
      
      let aggregated_value = match aggregation_type {
        "mean" => calculate_statistics(window).mean
        "sum" => window.reduce(fn(acc, x) { acc + x }, 0.0)
        "min" => calculate_statistics(window).min
        "max" => calculate_statistics(window).max
        _ => calculate_statistics(window).mean
      }
      
      aggregated = aggregated.push({
        timestamp: series.data_points[i].timestamp,
        value: aggregated_value,
        metadata: [("aggregated", "true"), ("type", aggregation_type)]
      })
    }
    
    aggregated
  }
  
  let aggregated_data = aggregate_data(response_time_series, "mean", 3)
  assert_eq(aggregated_data.length(), 4)  // 10 values with window size 3 = 4 aggregates
  
  // Test outlier removal
  let remove_outliers = fn(series: TimeSeries, threshold_multiplier: Float) {
    let values = series.data_points.map(fn(dp) { dp.value })
    let stats = calculate_statistics(values)
    let threshold = stats.std_dev * threshold_multiplier
    
    let mut filtered_data_points = []
    
    for data_point in series.data_points {
      let deviation = (data_point.value - stats.mean).abs()
      if deviation <= threshold {
        filtered_data_points = filtered_data_points.push(data_point)
      }
    }
    
    {
      name: series.name + "_filtered",
      data_points: filtered_data_points,
      unit: series.unit
    }
  }
  
  let filtered_series = remove_outliers(response_time_series, 2.0)
  assert_eq(filtered_series.data_points.length(), 9)  // One outlier removed
  
  // Verify the outlier was removed
  let filtered_values = filtered_series.data_points.map(fn(dp) { dp.value })
  assert_false(filtered_values.contains(500.0))
}