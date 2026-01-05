// Azimuth Error Handling and Recovery Test Suite
// This file contains test cases for error handling and recovery functionality

// Test 1: Error Type Definition and Classification
test "error type definition and classification" {
  type ErrorSeverity = {
    LOW: Int,
    MEDIUM: Int,
    HIGH: Int,
    CRITICAL: Int
  }
  
  type ErrorCategory = {
    NETWORK: Int,
    DATABASE: Int,
    AUTHENTICATION: Int,
    VALIDATION: Int,
    SYSTEM: Int,
    BUSINESS: Int
  }
  
  type TelemetryError = {
    code: String,
    message: String,
    severity: Int,
    category: Int,
    timestamp: Int,
    trace_id: Option[String],
    context: Array[(String, String)]
  }
  
  let error_severity = {
    LOW: 1,
    MEDIUM: 2,
    HIGH: 3,
    CRITICAL: 4
  }
  
  let error_category = {
    NETWORK: 1,
    DATABASE: 2,
    AUTHENTICATION: 3,
    VALIDATION: 4,
    SYSTEM: 5,
    BUSINESS: 6
  }
  
  let create_error = fn(code: String, message: String, severity: Int, category: Int) {
    {
      code,
      message,
      severity,
      category,
      timestamp: 1640995200,
      trace_id: None,
      context: []
    }
  }
  
  let with_trace_id = fn(error: TelemetryError, trace_id: String) {
    { error | trace_id: Some(trace_id) }
  }
  
  let with_context = fn(error: TelemetryError, key: String, value: String) {
    { error | context: error.context + [(key, value)] }
  }
  
  let is_critical = fn(error: TelemetryError) {
    error.severity >= error_severity.HIGH
  }
  
  let is_network_error = fn(error: TelemetryError) {
    error.category == error_category.NETWORK
  }
  
  let network_error = create_error("NET_001", "Connection timeout", error_severity.HIGH, error_category.NETWORK)
  assert_eq(network_error.code, "NET_001")
  assert_eq(network_error.message, "Connection timeout")
  assert_eq(network_error.severity, error_severity.HIGH)
  assert_eq(network_error.category, error_category.NETWORK)
  assert_true(is_critical(network_error))
  assert_true(is_network_error(network_error))
  
  let db_error = create_error("DB_001", "Database connection failed", error_severity.MEDIUM, error_category.DATABASE)
  assert_false(is_critical(db_error))
  assert_false(is_network_error(db_error))
  
  let traced_error = with_trace_id(network_error, "trace-123")
  assert_eq(traced_error.trace_id, Some("trace-123"))
  
  let contextual_error = with_context(traced_error, "host", "db.example.com")
  assert_eq(contextual_error.context.length(), 1)
  assert_true(contextual_error.context.contains(("host", "db.example.com")))
  
  let multi_context_error = with_context(contextual_error, "port", "5432")
  assert_eq(multi_context_error.context.length(), 2)
  assert_true(multi_context_error.context.contains(("host", "db.example.com")))
  assert_true(multi_context_error.context.contains(("port", "5432")))
}

// Test 2: Error Recovery Strategies
test "error recovery strategies" {
  type RecoveryStrategy = {
    RETRY: Int,
    FALLBACK: Int,
    CIRCUIT_BREAKER: Int,
    GRACEFUL_DEGRADATION: Int
  }
  
  type RecoveryResult = {
    success: Bool,
    strategy: Int,
    attempts: Int,
    message: String
  }
  
  let recovery_strategy = {
    RETRY: 1,
    FALLBACK: 2,
    CIRCUIT_BREAKER: 3,
    GRACEFUL_DEGRADATION: 4
  }
  
  let retry_strategy = fn(max_attempts: Int, operation: () -> Bool) {
    let mut attempts = 0
    let mut success = false
    
    while attempts < max_attempts && not(success) {
      attempts = attempts + 1
      success = operation()
    }
    
    {
      success,
      strategy: recovery_strategy.RETRY,
      attempts,
      message: if success { "Operation succeeded after " + attempts.to_string() + " attempts" } else { "Operation failed after " + attempts.to_string() + " attempts" }
    }
  }
  
  let fallback_strategy = fn(primary: () -> Bool, fallback: () -> Bool) {
    let primary_success = primary()
    
    if primary_success {
      {
        success: true,
        strategy: recovery_strategy.FALLBACK,
        attempts: 1,
        message: "Primary operation succeeded"
      }
    } else {
      let fallback_success = fallback()
      {
        success: fallback_success,
        strategy: recovery_strategy.FALLBACK,
        attempts: 2,
        message: if fallback_success { "Fallback operation succeeded" } else { "Both primary and fallback operations failed" }
      }
    }
  }
  
  let circuit_breaker_strategy = fn(failure_count: Int, failure_threshold: Int, operation: () -> Bool) {
    if failure_count >= failure_threshold {
      {
        success: false,
        strategy: recovery_strategy.CIRCUIT_BREAKER,
        attempts: 0,
        message: "Circuit breaker is open due to " + failure_count.to_string() + " failures"
      }
    } else {
      let success = operation()
      {
        success,
        strategy: recovery_strategy.CIRCUIT_BREAKER,
        attempts: 1,
        message: if success { "Operation succeeded" } else { "Operation failed" }
      }
    }
  }
  
  let graceful_degradation_strategy = fn(primary: () -> Bool, degraded: () -> Bool) {
    let primary_success = primary()
    
    if primary_success {
      {
        success: true,
        strategy: recovery_strategy.GRACEFUL_DEGRADATION,
        attempts: 1,
        message: "Primary operation succeeded"
      }
    } else {
      let degraded_success = degraded()
      {
        success: true,  // Considered success even if degraded
        strategy: recovery_strategy.GRACEFUL_DEGRADATION,
        attempts: 2,
        message: if degraded_success { "Operation succeeded in degraded mode" } else { "Operation failed even in degraded mode" }
      }
    }
  }
  
  // Test retry strategy
  let mut attempt_count = 0
  let flaky_operation = fn() {
    attempt_count = attempt_count + 1
    attempt_count >= 3  // Succeeds on 3rd attempt
  }
  
  let retry_result = retry_strategy(5, flaky_operation)
  assert_true(retry_result.success)
  assert_eq(retry_result.strategy, recovery_strategy.RETRY)
  assert_eq(retry_result.attempts, 3)
  
  // Reset for next test
  attempt_count = 0
  let failing_operation = fn() {
    attempt_count = attempt_count + 1
    false  // Always fails
  }
  
  let failed_retry_result = retry_strategy(3, failing_operation)
  assert_false(failed_retry_result.success)
  assert_eq(failed_retry_result.strategy, recovery_strategy.RETRY)
  assert_eq(failed_retry_result.attempts, 3)
  
  // Test fallback strategy
  let failing_primary = fn() { false }
  let working_fallback = fn() { true }
  
  let fallback_result = fallback_strategy(failing_primary, working_fallback)
  assert_true(fallback_result.success)
  assert_eq(fallback_result.strategy, recovery_strategy.FALLBACK)
  assert_eq(fallback_result.attempts, 2)
  
  // Test circuit breaker strategy
  let working_operation = fn() { true }
  let circuit_breaker_open_result = circuit_breaker_strategy(5, 3, working_operation)
  assert_false(circuit_breaker_open_result.success)
  assert_eq(circuit_breaker_open_result.strategy, recovery_strategy.CIRCUIT_BREAKER)
  
  let circuit_breaker_closed_result = circuit_breaker_strategy(2, 3, working_operation)
  assert_true(circuit_breaker_closed_result.success)
  assert_eq(circuit_breaker_closed_result.strategy, recovery_strategy.CIRCUIT_BREAKER)
  
  // Test graceful degradation strategy
  let failing_primary_op = fn() { false }
  let working_degraded_op = fn() { true }
  
  let degradation_result = graceful_degradation_strategy(failing_primary_op, working_degraded_op)
  assert_true(degradation_result.success)
  assert_eq(degradation_result.strategy, recovery_strategy.GRACEFUL_DEGRADATION)
  assert_eq(degradation_result.attempts, 2)
}

// Test 3: Error Rate Monitoring
test "error rate monitoring" {
  type ErrorEvent = {
    timestamp: Int,
    error_code: String,
    service: String
  }
  
  type ErrorRate = {
    time_window: Int,
    total_requests: Int,
    error_count: Int,
    error_rate: Float,
    errors_by_code: Array[(String, Int)]
  }
  
  let create_error_event = fn(timestamp: Int, error_code: String, service: String) {
    {
      timestamp,
      error_code,
      service
    }
  }
  
  let calculate_error_rate = fn(error_events: Array[ErrorEvent], total_requests: Int, window_start: Int, window_end: Int) {
    let window_errors = error_events.filter(fn(event) { 
      event.timestamp >= window_start && event.timestamp < window_end 
    })
    
    let error_count = window_errors.length()
    let error_rate = if total_requests > 0 { 
      error_count.to_float() / total_requests.to_float() 
    } else { 
      0.0 
    }
    
    // Group by error code
    let error_counts = {}
    for event in window_errors {
      match error_counts[event.error_code] {
        Some(count) => error_counts[event.error_code] = Some(count + 1)
        None => error_counts[event.error_code] = Some(1)
      }
    }
    
    let errors_by_code = error_counts.keys().map(fn(code) { 
      (code, error_counts[code].unwrap_or(0)) 
    })
    
    {
      time_window: window_end - window_start,
      total_requests,
      error_count,
      error_rate,
      errors_by_code
    }
  }
  
  let error_events = [
    create_error_event(1000, "NET_001", "web-service"),
    create_error_event(1010, "DB_001", "web-service"),
    create_error_event(1020, "NET_001", "web-service"),
    create_error_event(1030, "AUTH_001", "auth-service"),
    create_error_event(1040, "NET_001", "web-service"),
    create_error_event(2000, "DB_001", "web-service"),
    create_error_event(2010, "VAL_001", "api-service"),
    create_error_event(2020, "NET_001", "web-service")
  ]
  
  let total_requests = 100
  let error_rate1 = calculate_error_rate(error_events, total_requests, 1000, 1100)
  assert_eq(error_rate1.time_window, 100)
  assert_eq(error_rate1.total_requests, 100)
  assert_eq(error_rate1.error_count, 5)
  assert_eq(error_rate1.error_rate, 0.05)
  assert_eq(error_rate1.errors_by_code.length(), 3)  // NET_001, DB_001, AUTH_001
  
  let error_rate2 = calculate_error_rate(error_events, 50, 2000, 2100)
  assert_eq(error_rate2.time_window, 100)
  assert_eq(error_rate2.total_requests, 50)
  assert_eq(error_rate2.error_count, 3)
  assert_eq(error_rate2.error_rate, 0.06)
  assert_eq(error_rate2.errors_by_code.length(), 3)  // DB_001, VAL_001, NET_001
  
  let error_rate3 = calculate_error_rate(error_events, 0, 3000, 3100)
  assert_eq(error_rate3.time_window, 100)
  assert_eq(error_rate3.total_requests, 0)
  assert_eq(error_rate3.error_count, 0)
  assert_eq(error_rate3.error_rate, 0.0)
  assert_eq(error_rate3.errors_by_code.length(), 0)
}

// Test 4: Error Context Propagation
test "error context propagation" {
  type ErrorContext = {
    trace_id: String,
    span_id: String,
    service_name: String,
    operation: String,
    user_id: Option[String],
    request_id: Option[String],
    additional_data: Array[(String, String)]
  }
  
  type PropagatedError = {
    original_error: String,
    context: ErrorContext,
    stack_trace: Array[String],
    propagated_by: Array[String]
  }
  
  let create_context = fn(trace_id: String, span_id: String, service_name: String, operation: String) {
    {
      trace_id,
      span_id,
      service_name,
      operation,
      user_id: None,
      request_id: None,
      additional_data: []
    }
  }
  
  let with_user_id = fn(context: ErrorContext, user_id: String) {
    { context | user_id: Some(user_id) }
  }
  
  let with_request_id = fn(context: ErrorContext, request_id: String) {
    { context | request_id: Some(request_id) }
  }
  
  let with_additional_data = fn(context: ErrorContext, key: String, value: String) {
    { context | additional_data: context.additional_data + [(key, value)] }
  }
  
  let create_propagated_error = fn(original_error: String, context: ErrorContext) {
    {
      original_error,
      context,
      stack_trace: [context.service_name + ":" + context.operation],
      propagated_by: [context.service_name]
    }
  }
  
  let propagate_error = fn(error: PropagatedError, new_service: String, new_operation: String) {
    let new_context = { error.context | 
      service_name: new_service, 
      span_id: new_service + "-" + new_operation,
      operation: new_operation
    }
    
    {
      original_error: error.original_error,
      context: new_context,
      stack_trace: error.stack_trace + [new_service + ":" + new_operation],
      propagated_by: error.propagated_by + [new_service]
    }
  }
  
  let initial_context = create_context("trace-123", "span-1", "web-service", "handle_request")
  let context_with_user = with_user_id(initial_context, "user-456")
  let context_with_request = with_request_id(context_with_user, "req-789")
  let full_context = with_additional_data(context_with_request, "client_ip", "192.168.1.1")
  
  assert_eq(full_context.trace_id, "trace-123")
  assert_eq(full_context.span_id, "span-1")
  assert_eq(full_context.service_name, "web-service")
  assert_eq(full_context.operation, "handle_request")
  assert_eq(full_context.user_id, Some("user-456"))
  assert_eq(full_context.request_id, Some("req-789"))
  assert_eq(full_context.additional_data.length(), 1)
  assert_true(full_context.additional_data.contains(("client_ip", "192.168.1.1")))
  
  let initial_error = create_propagated_error("Database connection failed", full_context)
  assert_eq(initial_error.original_error, "Database connection failed")
  assert_eq(initial_error.context.service_name, "web-service")
  assert_eq(initial_error.stack_trace.length(), 1)
  assert_eq(initial_error.stack_trace[0], "web-service:handle_request")
  assert_eq(initial_error.propagated_by.length(), 1)
  assert_eq(initial_error.propagated_by[0], "web-service")
  
  let propagated_error = propagate_error(initial_error, "auth-service", "authenticate_user")
  assert_eq(propagated_error.original_error, "Database connection failed")
  assert_eq(propagated_error.context.service_name, "auth-service")
  assert_eq(propagated_error.context.operation, "authenticate_user")
  assert_eq(propagated_error.context.span_id, "auth-service-authenticate_user")
  assert_eq(propagated_error.stack_trace.length(), 2)
  assert_eq(propagated_error.stack_trace[0], "web-service:handle_request")
  assert_eq(propagated_error.stack_trace[1], "auth-service:authenticate_user")
  assert_eq(propagated_error.propagated_by.length(), 2)
  assert_eq(propagated_error.propagated_by[0], "web-service")
  assert_eq(propagated_error.propagated_by[1], "auth-service")
  
  let double_propagated_error = propagate_error(propagated_error, "user-service", "get_user_profile")
  assert_eq(double_propagated_error.context.service_name, "user-service")
  assert_eq(double_propagated_error.context.operation, "get_user_profile")
  assert_eq(double_propagated_error.stack_trace.length(), 3)
  assert_eq(double_propagated_error.propagated_by.length(), 3)
}

// Test 5: Error Aggregation and Analysis
test "error aggregation and analysis" {
  type ErrorSummary = {
    error_code: String,
    count: Int,
    first_seen: Int,
    last_seen: Int,
    affected_services: Array[String],
    affected_users: Array[String],
    severity: Int
  }
  
  type ErrorAnalysis = {
    total_errors: Int,
    unique_error_codes: Int,
    top_errors: Array[ErrorSummary],
    error_trend: String  // "increasing", "decreasing", "stable"
  }
  
  let create_error_summary = fn(error_code: String, timestamp: Int, service: String, user: Option[String>, severity: Int) {
    {
      error_code,
      count: 1,
      first_seen: timestamp,
      last_seen: timestamp,
      affected_services: [service],
      affected_users: match user {
        Some(u) => [u]
        None => []
      },
      severity
    }
  }
  
  let merge_error_summaries = fn(summary1: ErrorSummary, summary2: ErrorSummary) {
    let combined_services = summary1.affected_services + summary2.affected_services
    let unique_services = fn(services: Array[String]) {
      let mut result = []
      for service in services {
        if not(result.contains(service)) {
          result = result.push(service)
        }
      }
      result
    }
    
    let combined_users = summary1.affected_users + summary2.affected_users
    let unique_users = fn(users: Array[String]) {
      let mut result = []
      for user in users {
        if not(result.contains(user)) {
          result = result.push(user)
        }
      }
      result
    }
    
    {
      error_code: summary1.error_code,
      count: summary1.count + summary2.count,
      first_seen: if summary1.first_seen < summary2.first_seen { summary1.first_seen } else { summary2.first_seen },
      last_seen: if summary1.last_seen > summary2.last_seen { summary1.last_seen } else { summary2.last_seen },
      affected_services: unique_services(combined_services),
      affected_users: unique_users(combined_users),
      severity: summary1.severity.max(summary2.severity)
    }
  }
  
  let analyze_errors = fn(error_summaries: Array[ErrorSummary], time_window: Int) {
    let total_errors = error_summaries.map(fn(s) { s.count }).reduce(fn(acc, count) { acc + count }, 0)
    let unique_error_codes = error_summaries.length()
    
    let top_errors = error_summaries.sort(fn(a, b) { 
      if a.count != b.count { a.count > b.count } 
      else { a.severity > b.severity } 
    }).slice(0, 5)
    
    // Simple trend analysis (would be more sophisticated in real implementation)
    let error_trend = if total_errors > 100 { "increasing" } else if total_errors < 10 { "decreasing" } else { "stable" }
    
    {
      total_errors,
      unique_error_codes,
      top_errors,
      error_trend
    }
  }
  
  let summary1 = create_error_summary("NET_001", 1000, "web-service", Some("user-1"), 3)
  let summary2 = create_error_summary("NET_001", 1010, "web-service", Some("user-2"), 3)
  let summary3 = create_error_summary("DB_001", 1020, "db-service", None, 4)
  let summary4 = create_error_summary("AUTH_001", 1030, "auth-service", Some("user-1"), 2)
  let summary5 = create_error_summary("NET_001", 1040, "api-service", Some("user-3"), 3)
  
  let merged_net = merge_error_summaries(summary1, summary2)
  let merged_net_final = merge_error_summaries(merged_net, summary5)
  
  assert_eq(merged_net_final.error_code, "NET_001")
  assert_eq(merged_net_final.count, 3)
  assert_eq(merged_net_final.first_seen, 1000)
  assert_eq(merged_net_final.last_seen, 1040)
  assert_eq(merged_net_final.affected_services.length(), 2)
  assert_true(merged_net_final.affected_services.contains("web-service"))
  assert_true(merged_net_final.affected_services.contains("api-service"))
  assert_eq(merged_net_final.affected_users.length(), 3)
  assert_true(merged_net_final.affected_users.contains("user-1"))
  assert_true(merged_net_final.affected_users.contains("user-2"))
  assert_true(merged_net_final.affected_users.contains("user-3"))
  assert_eq(merged_net_final.severity, 3)
  
  let all_summaries = [merged_net_final, summary3, summary4]
  let analysis = analyze_errors(all_summaries, 100)
  
  assert_eq(analysis.total_errors, 5)  // 3 NET_001 + 1 DB_001 + 1 AUTH_001
  assert_eq(analysis.unique_error_codes, 3)
  assert_eq(analysis.top_errors.length(), 3)
  assert_eq(analysis.error_trend, "decreasing")  // Since total_errors is 5 < 10
}

// Test 6: Error Alerting
test "error alerting" {
  type Alert = {
    id: String,
    error_code: String,
    severity: Int,
    message: String,
    timestamp: Int,
    service: String,
    triggered_by: String
  }
  
  type AlertRule = {
    name: String,
    error_code_pattern: String,
    severity_threshold: Int,
    count_threshold: Int,
    time_window: Int,
    enabled: Bool
  }
  
  type AlertSystem = {
    rules: Array[AlertRule],
    alerts: Array[Alert]
  }
  
  let create_alert_system = fn() {
    {
      rules: [],
      alerts: []
    }
  }
  
  let add_rule = fn(system: AlertSystem, rule: AlertRule) {
    { system | rules: system.rules + [rule] }
  }
  
  let create_alert = fn(id: String, error_code: String, severity: Int, message: String, timestamp: Int, service: String) {
    {
      id,
      error_code,
      severity,
      message,
      timestamp,
      service,
      triggered_by: "system"
    }
  }
  
  let check_alert_conditions = fn(system: AlertSystem, error_events: Array[String], error_code: String, severity: Int, service: String, current_time: Int) {
    let mut new_alerts = []
    
    for rule in system.rules {
      if rule.enabled && 
         error_code.contains(rule.error_code_pattern) && 
         severity >= rule.severity_threshold {
        
        let matching_errors = error_events.filter(fn(code) { code.contains(rule.error_code_pattern) })
        
        if matching_errors.length() >= rule.count_threshold {
          let alert = create_alert(
            "alert-" + current_time.to_string(),
            error_code,
            severity,
            rule.name + ": " + error_code + " occurred " + matching_errors.length().to_string() + " times",
            current_time,
            service
          )
          new_alerts = new_alerts + [alert]
        }
      }
    }
    
    { system | alerts: system.alerts + new_alerts }
  }
  
  let system = create_alert_system()
  
  let rule1 = {
    name: "High Error Rate",
    error_code_pattern: "NET_",
    severity_threshold: 2,
    count_threshold: 3,
    time_window: 300,
    enabled: true
  }
  
  let rule2 = {
    name: "Critical Database Errors",
    error_code_pattern: "DB_",
    severity_threshold: 3,
    count_threshold: 1,
    time_window: 300,
    enabled: true
  }
  
  let system_with_rules = add_rule(add_rule(system, rule1), rule2)
  assert_eq(system_with_rules.rules.length(), 2)
  
  let error_events = ["NET_001", "NET_002", "NET_001", "DB_001", "NET_001"]
  
  // Test NET_ error (should trigger rule1)
  let system_after_net = check_alert_conditions(
    system_with_rules, 
    error_events, 
    "NET_001", 
    2, 
    "web-service", 
    1640995200
  )
  assert_eq(system_after_net.alerts.length(), 1)
  assert_eq(system_after_net.alerts[0].error_code, "NET_001")
  assert_eq(system_after_net.alerts[0].severity, 2)
  assert_true(system_after_net.alerts[0].message.contains("High Error Rate"))
  
  // Test DB_ error (should trigger rule2)
  let system_after_db = check_alert_conditions(
    system_after_net,
    error_events,
    "DB_001",
    4,
    "db-service",
    1640995300
  )
  assert_eq(system_after_db.alerts.length(), 2)
  assert_eq(system_after_db.alerts[1].error_code, "DB_001")
  assert_eq(system_after_db.alerts[1].severity, 4)
  assert_true(system_after_db.alerts[1].message.contains("Critical Database Errors"))
  
  // Test low severity error (should not trigger any rule)
  let system_after_low = check_alert_conditions(
    system_after_db,
    error_events,
    "VAL_001",
    1,
    "api-service",
    1640995400
  )
  assert_eq(system_after_low.alerts.length(), 2)  // No new alerts
}

// Test 7: Error Recovery State Machine
test "error recovery state machine" {
  type RecoveryState = {
    NORMAL: Int,
    DEGRADED: Int,
    RECOVERING: Int,
    FAILED: Int
  }
  
  type StateTransition = {
    from_state: Int,
    to_state: Int,
    condition: String
  }
  
  type RecoveryStateMachine = {
    current_state: Int,
    transitions: Array[StateTransition],
    failure_count: Int,
    success_count: Int,
    last_failure_time: Int,
    last_success_time: Int
  }
  
  let recovery_state = {
    NORMAL: 1,
    DEGRADED: 2,
    RECOVERING: 3,
    FAILED: 4
  }
  
  let create_state_machine = fn(initial_state: Int) {
    {
      current_state: initial_state,
      transitions: [
        { from_state: recovery_state.NORMAL, to_state: recovery_state.DEGRADED, condition: "consecutive_failures >= 3" },
        { from_state: recovery_state.DEGRADED, to_state: recovery_state.RECOVERING, condition: "success_after_failure" },
        { from_state: recovery_state.RECOVERING, to_state: recovery_state.NORMAL, condition: "consecutive_successes >= 5" },
        { from_state: recovery_state.RECOVERING, to_state: recovery_state.DEGRADED, condition: "failure_during_recovery" },
        { from_state: recovery_state.DEGRADED, to_state: recovery_state.FAILED, condition: "consecutive_failures >= 10" },
        { from_state: recovery_state.FAILED, to_state: recovery_state.RECOVERING, condition: "manual_intervention" }
      ],
      failure_count: 0,
      success_count: 0,
      last_failure_time: 0,
      last_success_time: 0
    }
  }
  
  let process_result = fn(machine: RecoveryStateMachine, success: Bool, current_time: Int) {
    let mut new_failure_count = machine.failure_count
    let mut new_success_count = machine.success_count
    let mut new_last_failure_time = machine.last_failure_time
    let mut new_last_success_time = machine.last_success_time
    let mut new_state = machine.current_state
    
    if success {
      new_success_count = new_success_count + 1
      new_last_success_time = current_time
      
      // Check for state transitions based on success
      if machine.current_state == recovery_state.DEGRADED && new_success_count >= 3 {
        new_state = recovery_state.RECOVERING
        new_success_count = 0  // Reset counter for recovery state
      } else if machine.current_state == recovery_state.RECOVERING && new_success_count >= 5 {
        new_state = recovery_state.NORMAL
        new_success_count = 0  // Reset counter for normal state
      }
      
      // Reset failure count on success
      new_failure_count = 0
    } else {
      new_failure_count = new_failure_count + 1
      new_last_failure_time = current_time
      
      // Check for state transitions based on failure
      if machine.current_state == recovery_state.NORMAL && new_failure_count >= 3 {
        new_state = recovery_state.DEGRADED
      } else if machine.current_state == recovery_state.DEGRADED && new_failure_count >= 10 {
        new_state = recovery_state.FAILED
      } else if machine.current_state == recovery_state.RECOVERING {
        new_state = recovery_state.DEGRADED
        new_failure_count = 3  // Set to threshold for degraded state
      }
      
      // Reset success count on failure
      new_success_count = 0
    }
    
    {
      current_state: new_state,
      transitions: machine.transitions,
      failure_count: new_failure_count,
      success_count: new_success_count,
      last_failure_time: new_last_failure_time,
      last_success_time: new_last_success_time
    }
  }
  
  let machine = create_state_machine(recovery_state.NORMAL)
  assert_eq(machine.current_state, recovery_state.NORMAL)
  assert_eq(machine.failure_count, 0)
  assert_eq(machine.success_count, 0)
  
  // Simulate failures
  let machine1 = process_result(machine, false, 1000)
  assert_eq(machine1.current_state, recovery_state.NORMAL)
  assert_eq(machine1.failure_count, 1)
  
  let machine2 = process_result(machine1, false, 1010)
  assert_eq(machine2.current_state, recovery_state.NORMAL)
  assert_eq(machine2.failure_count, 2)
  
  let machine3 = process_result(machine2, false, 1020)
  assert_eq(machine3.current_state, recovery_state.DEGRADED)
  assert_eq(machine3.failure_count, 3)
  
  // Simulate recovery
  let machine4 = process_result(machine3, true, 1030)
  assert_eq(machine4.current_state, recovery_state.DEGRADED)
  assert_eq(machine4.failure_count, 0)
  assert_eq(machine4.success_count, 1)
  
  let machine5 = process_result(machine4, true, 1040)
  assert_eq(machine5.current_state, recovery_state.DEGRADED)
  assert_eq(machine5.success_count, 2)
  
  let machine6 = process_result(machine5, true, 1050)
  assert_eq(machine6.current_state, recovery_state.RECOVERING)
  assert_eq(machine6.success_count, 0)
  
  // Continue recovery
  let machine7 = process_result(machine6, true, 1060)
  assert_eq(machine7.current_state, recovery_state.RECOVERING)
  assert_eq(machine7.success_count, 1)
  
  let machine8 = process_result(machine7, true, 1070)
  assert_eq(machine8.current_state, recovery_state.RECOVERING)
  assert_eq(machine8.success_count, 2)
  
  let machine9 = process_result(machine8, true, 1080)
  assert_eq(machine9.current_state, recovery_state.RECOVERING)
  assert_eq(machine9.success_count, 3)
  
  let machine10 = process_result(machine9, true, 1090)
  assert_eq(machine10.current_state, recovery_state.RECOVERING)
  assert_eq(machine10.success_count, 4)
  
  let machine11 = process_result(machine10, true, 1100)
  assert_eq(machine11.current_state, recovery_state.NORMAL)
  assert_eq(machine11.success_count, 0)
}

// Test 8: Error Handling in Distributed Systems
test "error handling in distributed systems" {
  type ServiceError = {
    service_name: String,
    error_code: String,
    message: String,
    timestamp: Int,
    upstream_service: Option[String>,
    downstream_impact: Array[String]
  }
  
  type ErrorPropagation = {
    root_error: ServiceError,
    propagation_chain: Array[ServiceError],
    impact_analysis: {
      affected_services: Array[String],
      total_impact_score: Int,
      recovery_priority: String
    }
  }
  
  let create_service_error = fn(service_name: String, error_code: String, message: String, timestamp: Int, upstream_service: Option[String>) {
    {
      service_name,
      error_code,
      message,
      timestamp,
      upstream_service,
      downstream_impact: []
    }
  }
  
  let add_downstream_impact = fn(error: ServiceError, downstream_service: String) {
    { error | downstream_impact: error.downstream_impact + [downstream_service] }
  }
  
  let trace_error_propagation = fn(errors: Array[ServiceError]) {
    if errors.length() == 0 {
      None
    } else {
      // Find the root error (error with no upstream service)
      let root_errors = errors.filter(fn(e) { e.upstream_service.is_none() })
      
      if root_errors.length() == 0 {
        None
      } else {
        let root_error = root_errors[0]
        
        // Build propagation chain
        let mut chain = [root_error]
        let mut current_service = root_error.service_name
        
        while true {
          let downstream_errors = errors.filter(fn(e) { 
            e.upstream_service == Some(current_service) 
          })
          
          if downstream_errors.length() == 0 {
            break
          }
          
          chain = chain + [downstream_errors[0]]
          current_service = downstream_errors[0].service_name
        }
        
        // Analyze impact
        let affected_services = chain.map(fn(e) { e.service_name })
        let total_impact_score = chain.length() * 10  // Simple scoring
        let recovery_priority = if total_impact_score >= 30 { "high" } else if total_impact_score >= 20 { "medium" } else { "low" }
        
        Some({
          root_error,
          propagation_chain: chain,
          impact_analysis: {
            affected_services,
            total_impact_score,
            recovery_priority
          }
        })
      }
    }
  }
  
  let db_error = create_service_error("database-service", "DB_001", "Connection pool exhausted", 1000, None)
  let auth_error = create_service_error("auth-service", "AUTH_001", "Authentication failed", 1010, Some("database-service"))
  let web_error = create_service_error("web-service", "WEB_001", "Request failed", 1020, Some("auth-service"))
  
  let db_with_impact = add_downstream_impact(db_error, "auth-service")
  let auth_with_impact = add_downstream_impact(auth_error, "web-service")
  
  let errors = [db_with_impact, auth_with_impact, web_error]
  
  let propagation = trace_error_propagation(errors)
  assert_true(propagation.is_some())
  
  match propagation {
    Some(p) => {
      assert_eq(p.root_error.service_name, "database-service")
      assert_eq(p.root_error.error_code, "DB_001")
      assert_eq(p.propagation_chain.length(), 3)
      assert_eq(p.propagation_chain[0].service_name, "database-service")
      assert_eq(p.propagation_chain[1].service_name, "auth-service")
      assert_eq(p.propagation_chain[2].service_name, "web-service")
      assert_eq(p.impact_analysis.affected_services.length(), 3)
      assert_eq(p.impact_analysis.total_impact_score, 30)
      assert_eq(p.impact_analysis.recovery_priority, "high")
    }
    None => assert_true(false)
  }
  
  // Test with circular dependencies (should handle gracefully)
  let service_a_error = create_service_error("service-a", "A_001", "Error in A", 1000, Some("service-b"))
  let service_b_error = create_service_error("service-b", "B_001", "Error in B", 1010, Some("service-c"))
  let service_c_error = create_service_error("service-c", "C_001", "Error in C", 1020, Some("service-a"))
  
  let circular_errors = [service_a_error, service_b_error, service_c_error]
  let circular_propagation = trace_error_propagation(circular_errors)
  assert_true(circular_propagation.is_none())  // No root error found
}