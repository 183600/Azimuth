// Azimuth Error Tracking Tests
// This file contains test cases for error tracking functionality

// Test 1: Error Event Collection and Classification
test "error event collection and classification" {
  // Define error event structure
  type ErrorEvent = {
    timestamp: Int,
    error_id: String,
    error_type: String,
    severity: String,
    message: String,
    stack_trace: String,
    service: String,
    context: Array[(String, String)]
  }
  
  // Create sample error events
  let error_events = [
    {
      timestamp: 1640995200,
      error_id: "err-001",
      error_type: "DatabaseError",
      severity: "high",
      message: "Connection timeout to database",
      stack_trace: "at query.execute() at line 42",
      service: "payment-service",
      context: [("query", "SELECT * FROM users"), ("timeout", "5000ms")]
    },
    {
      timestamp: 1640995250,
      error_id: "err-002",
      error_type: "ValidationError",
      severity: "medium",
      message: "Invalid input parameter",
      stack_trace: "at validate.input() at line 128",
      service: "api-gateway",
      context: [("parameter", "user_id"), ("value", "invalid")]
    },
    {
      timestamp: 1640995300,
      error_id: "err-003",
      error_type: "NetworkError",
      severity: "high",
      message: "Failed to connect to external service",
      stack_trace: "at http.request() at line 67",
      service: "notification-service",
      context: [("url", "https://api.example.com"), ("attempt", "3")]
    },
    {
      timestamp: 1640995350,
      error_id: "err-004",
      error_type: "AuthenticationError",
      severity: "critical",
      message: "Invalid authentication token",
      stack_trace: "at auth.validate() at line 89",
      service: "auth-service",
      context: [("token", "expired"), ("user", "user123")]
    }
  ]
  
  // Classify errors by type
  let classify_by_type = fn(errors: Array[ErrorEvent]) {
    let mut result = []
    let mut processed_types = []
    
    for error in errors {
      if not(processed_types.contains(error.error_type)) {
        processed_types = processed_types.push(error.error_type)
        
        let type_errors = errors.filter(fn(e) { e.error_type == error.error_type })
        let severity_counts = {
          let mut counts = []
          let mut processed_severities = []
          
          for e in type_errors {
            if not(processed_severities.contains(e.severity)) {
              processed_severities = processed_severities.push(e.severity)
              let count = type_errors.filter_fn(severity_error) { severity_error.severity == e.severity }.length()
              counts = counts.push((e.severity, count))
            }
          }
          
          counts
        }
        
        result = result.push({
          error_type: error.error_type,
          count: type_errors.length(),
          severities: severity_counts,
          services: type_errors.map(fn(e) { e.service })
        })
      }
    }
    
    result
  }
  
  let classified = classify_by_type(error_events)
  assert_eq(classified.length(), 4)
  
  // Check DatabaseError classification
  let db_error = classified.find_fn(c) { c.error_type == "DatabaseError" }
  match db_error {
    Some(classification) => {
      assert_eq(classification.count, 1)
      assert_eq(classification.severities.length(), 1)
      assert_eq(classification.severities[0], ("high", 1))
      assert_eq(classification.services[0], "payment-service")
    }
    None => assert_true(false)
  }
  
  // Filter errors by severity
  let filter_by_severity = fn(errors: Array[ErrorEvent], severity: String) {
    errors.filter_fn(e) { e.severity == severity }
  }
  
  let critical_errors = filter_by_severity(error_events, "critical")
  assert_eq(critical_errors.length(), 1)
  assert_eq(critical_errors[0].error_type, "AuthenticationError")
  
  let high_errors = filter_by_severity(error_events, "high")
  assert_eq(high_errors.length(), 2)
  
  // Group errors by service
  let group_by_service = fn(errors: Array[ErrorEvent]) {
    let mut result = []
    let mut processed_services = []
    
    for error in errors {
      if not(processed_services.contains(error.service)) {
        processed_services = processed_services.push(error.service)
        
        let service_errors = errors.filter_fn(e) { e.service == error.service }
        let error_types = service_errors.map_fn(e) { e.error_type }
        let unique_types = {
          let mut unique = []
          for type_str in error_types {
            if not(unique.contains(type_str)) {
              unique = unique.push(type_str)
            }
          }
          unique
        }
        
        result = result.push({
          service: error.service,
          error_count: service_errors.length(),
          error_types: unique_types,
          latest_error: service_errors[service_errors.length() - 1].timestamp
        })
      }
    }
    
    result
  }
  
  let by_service = group_by_service(error_events)
  assert_eq(by_service.length(), 4)
}

// Test 2: Error Frequency Analysis
test "error frequency analysis" {
  type ErrorFrequency = {
    error_type: String,
    service: String,
    count: Int,
    time_window: Int,
    frequency: Float  // errors per minute
  }
  
  type TimestampRange = {
    start: Int,
    end: Int
  }
  
  // Create sample error events with timestamps
  let error_events = [
    { timestamp: 1640995200, error_type: "DatabaseError", service: "payment-service" },
    { timestamp: 1640995210, error_type: "NetworkError", service: "payment-service" },
    { timestamp: 1640995220, error_type: "DatabaseError", service: "payment-service" },
    { timestamp: 1640995230, error_type: "DatabaseError", service: "payment-service" },
    { timestamp: 1640995240, error_type: "ValidationError", service: "api-gateway" },
    { timestamp: 1640995250, error_type: "ValidationError", service: "api-gateway" },
    { timestamp: 1640995260, error_type: "NetworkError", service: "payment-service" },
    { timestamp: 1640995270, error_type: "DatabaseError", service: "payment-service" },
    { timestamp: 1640995280, error_type: "NetworkError", service: "notification-service" },
    { timestamp: 1640995290, error_type: "AuthenticationError", service: "auth-service" },
    { timestamp: 1640995300, error_type: "DatabaseError", service: "payment-service" },
    { timestamp: 1640995310, error_type: "ValidationError", service: "api-gateway" }
  ]
  
  // Calculate error frequency in time windows
  let calculate_frequency = fn(errors: Array[ErrorEvent], window: TimestampRange) {
    let window_errors = errors.filter_fn(e) { 
      e.timestamp >= window.start and e.timestamp <= window.end 
    }
    
    let window_duration_minutes = (window.end - window.start).to_float() / 60.0
    
    if window_duration_minutes == 0.0 {
      []  // Avoid division by zero
    } else {
      let mut result = []
      let mut processed_combinations = []
      
      for error in window_errors {
        let key = error.error_type + ":" + error.service
        
        if not(processed_combinations.contains(key)) {
          processed_combinations = processed_combinations.push(key)
          
          let combination_errors = window_errors.filter_fn(e) { 
            e.error_type == error.error_type and e.service == error.service 
          }
          
          let frequency = combination_errors.length().to_float() / window_duration_minutes
          
          result = result.push({
            error_type: error.error_type,
            service: error.service,
            count: combination_errors.length(),
            time_window: window.end - window.start,
            frequency: frequency
          })
        }
      }
      
      result
    }
  }
  
  // Analyze frequency in a 5-minute window
  let time_window = { start: 1640995200, end: 1640995500 }
  let frequencies = calculate_frequency(error_events, time_window)
  
  assert_true(frequencies.length() > 0)
  
  // Check DatabaseError frequency for payment-service
  let db_payment_freq = frequencies.find_fn(f) { 
    f.error_type == "DatabaseError" and f.service == "payment-service" 
  }
  match db_payment_freq {
    Some(freq) => {
      assert_eq(freq.count, 5)  // 5 DatabaseErrors in payment-service
      assert_eq(freq.time_window, 300)  // 5 minutes = 300 seconds
      assert_eq(freq.frequency, 5.0 / 5.0)  // 5 errors in 5 minutes = 1.0 per minute
    }
    None => assert_true(false)
  }
  
  // Check ValidationError frequency for api-gateway
  let validation_api_freq = frequencies.find_fn(f) { 
    f.error_type == "ValidationError" and f.service == "api-gateway" 
  }
  match validation_api_freq {
    Some(freq) => {
      assert_eq(freq.count, 3)
      assert_eq(freq.frequency, 3.0 / 5.0)  // 3 errors in 5 minutes = 0.6 per minute
    }
    None => assert_true(false)
  }
  
  // Find high-frequency errors (more than 1 per minute)
  let high_frequency_errors = frequencies.filter_fn(f) { f.frequency > 1.0 }
  assert_true(high_frequency_errors.length() >= 1)
  
  // Find the most frequent error
  let most_frequent = frequencies.reduce(fn(acc, f) { 
    if f.frequency > acc.frequency { f } else { acc } 
  }, frequencies[0])
  
  assert_eq(most_frequent.error_type, "DatabaseError")
  assert_eq(most_frequent.service, "payment-service")
}

// Test 3: Error Correlation Analysis
test "error correlation analysis" {
  type ErrorCorrelation = {
    primary_error: String,
    correlated_errors: Array[String],
    correlation_strength: Float,
    time_pattern: String
  }
  
  // Create sample error events with causal relationships
  let error_stream = [
    { timestamp: 1640995200, error_type: "NetworkError", service: "payment-service", trace_id: "trace-001" },
    { timestamp: 1640995205, error_type: "TimeoutError", service: "payment-service", trace_id: "trace-001" },
    { timestamp: 1640995210, error_type: "DatabaseError", service: "payment-service", trace_id: "trace-001" },
    { timestamp: 1640995220, error_type: "NetworkError", service: "api-gateway", trace_id: "trace-002" },
    { timestamp: 1640995225, error_type: "TimeoutError", service: "api-gateway", trace_id: "trace-002" },
    { timestamp: 1640995230, error_type: "ValidationError", service: "api-gateway", trace_id: "trace-003" },
    { timestamp: 1640995240, error_type: "NetworkError", service: "notification-service", trace_id: "trace-004" },
    { timestamp: 1640995245, error_type: "TimeoutError", service: "notification-service", trace_id: "trace-004" },
    { timestamp: 1640995250, error_type: "DatabaseError", service: "notification-service", trace_id: "trace-004" },
    { timestamp: 1640995260, error_type: "AuthenticationError", service: "auth-service", trace_id: "trace-005" }
  ]
  
  // Find correlations by trace ID
  let find_correlations = fn(errors: Array[ErrorEvent]) {
    let mut result = []
    let mut processed_traces = []
    
    for error in errors {
      if not(processed_traces.contains(error.trace_id)) {
        processed_traces = processed_traces.push(error.trace_id)
        
        let trace_errors = errors.filter_fn(e) { e.trace_id == error.trace_id }
        
        if trace_errors.length() > 1 {
          // Sort by timestamp
          let sorted_errors = {
            let mut sorted = trace_errors
            // Simple bubble sort for demonstration
            let mut i = 0
            while i < sorted.length() {
              let mut j = i + 1
              while j < sorted.length() {
                if sorted[j].timestamp < sorted[i].timestamp {
                  let temp = sorted[i]
                  sorted[i] = sorted[j]
                  sorted[j] = temp
                }
                j = j + 1
              }
              i = i + 1
            }
            sorted
          }
          
          // Analyze error sequence patterns
          let error_sequence = sorted_errors.map_fn(e) { e.error_type }
          
          // Check for common patterns
          let time_pattern = if sorted_errors.length() == 3 {
            "cascade_failure"
          } else if sorted_errors.length() == 2 {
            "direct_correlation"
          } else {
            "complex_pattern"
          }
          
          result = result.push({
            primary_error: sorted_errors[0].error_type,
            correlated_errors: error_sequence.slice(1, error_sequence.length()),
            correlation_strength: 1.0 / sorted_errors.length().to_float(),
            time_pattern: time_pattern
          })
        }
      }
    }
    
    result
  }
  
  let correlations = find_correlations(error_stream)
  assert_eq(correlations.length(), 4)  // 4 traces with multiple errors
  
  // Check NetworkError -> TimeoutError -> DatabaseError pattern
  let network_correlation = correlations.find_fn(c) { c.primary_error == "NetworkError" }
  match network_correlation {
    Some(correlation) => {
      assert_eq(correlation.correlated_errors.length(), 2)
      assert_eq(correlation.correlated_errors[0], "TimeoutError")
      assert_eq(correlation.correlated_errors[1], "DatabaseError")
      assert_eq(correlation.time_pattern, "cascade_failure")
    }
    None => assert_true(false)
  }
  
  // Group by primary error type
  let group_by_primary = fn(correlations: Array[ErrorCorrelation]) {
    let mut result = []
    let mut processed_types = []
    
    for correlation in correlations {
      if not(processed_types.contains(correlation.primary_error)) {
        processed_types = processed_types.push(correlation.primary_error)
        
        let primary_correlations = correlations.filter_fn(c) { 
          c.primary_error == correlation.primary_error 
        }
        
        let all_correlated = {
          let mut all = []
          for c in primary_correlations {
            all = all.concat(c.correlated_errors)
          }
          all
        }
        
        let unique_correlated = {
          let mut unique = []
          for error_type in all_correlated {
            if not(unique.contains(error_type)) {
              unique = unique.push(error_type)
            }
          }
          unique
        }
        
        result = result.push({
          primary_error: correlation.primary_error,
          occurrence_count: primary_correlations.length(),
          correlated_errors: unique_correlated,
          avg_correlation_strength: primary_correlations.reduce(fn(acc, c) { 
            acc + c.correlation_strength 
          }, 0.0) / primary_correlations.length().to_float()
        })
      }
    }
    
    result
  }
  
  let grouped = group_by_primary(correlations)
  assert_eq(grouped.length(), 2)  // NetworkError and ValidationError as primary errors
  
  // Check NetworkError correlations
  let network_grouped = grouped.find_fn(g) { g.primary_error == "NetworkError" }
  match network_grouped {
    Some(group) => {
      assert_eq(group.occurrence_count, 3)  // NetworkError appears 3 times as primary
      assert_true(group.correlated_errors.contains("TimeoutError"))
      assert_true(group.correlated_errors.contains("DatabaseError"))
    }
    None => assert_true(false)
  }
}

// Test 4: Error Impact Assessment
test "error impact assessment" {
  type ErrorImpact = {
    error_id: String,
    error_type: String,
    affected_users: Int,
    affected_operations: Int,
    business_impact: String,  // "low", "medium", "high", "critical"
    recovery_time: Int  // minutes
  }
  
  type ServiceMetrics = {
    service: String,
    total_operations: Int,
    successful_operations: Int,
    failed_operations: Int,
    availability: Float  // percentage
  }
  
  // Create sample error impact data
  let error_impacts = [
    {
      error_id: "err-001",
      error_type: "DatabaseError",
      affected_users: 1500,
      affected_operations: 5000,
      business_impact: "high",
      recovery_time: 15
    },
    {
      error_id: "err-002",
      error_type: "ValidationError",
      affected_users: 50,
      affected_operations: 200,
      business_impact: "low",
      recovery_time: 2
    },
    {
      error_id: "err-003",
      error_type: "NetworkError",
      affected_users: 800,
      affected_operations: 2500,
      business_impact: "medium",
      recovery_time: 8
    },
    {
      error_id: "err-004",
      error_type: "AuthenticationError",
      affected_users: 3000,
      affected_operations: 8000,
      business_impact: "critical",
      recovery_time: 25
    }
  ]
  
  // Create service metrics
  let service_metrics = [
    {
      service: "payment-service",
      total_operations: 10000,
      successful_operations: 8500,
      failed_operations: 1500,
      availability: 85.0
    },
    {
      service: "api-gateway",
      total_operations: 5000,
      successful_operations: 4800,
      failed_operations: 200,
      availability: 96.0
    },
    {
      service: "notification-service",
      total_operations: 3000,
      successful_operations: 2500,
      failed_operations: 500,
      availability: 83.33
    },
    {
      service: "auth-service",
      total_operations: 12000,
      successful_operations: 9000,
      failed_operations: 3000,
      availability: 75.0
    }
  ]
  
  // Calculate impact score
  let calculate_impact_score = fn(impact: ErrorImpact) {
    let user_impact = impact.affected_users.to_float() / 100.0  // Scale users
    let operation_impact = impact.affected_operations.to_float() / 1000.0  // Scale operations
    let recovery_impact = impact.recovery_time.to_float() * 10.0  // Weight recovery time
    
    let business_weight = match impact.business_impact {
      "low" => 1.0,
      "medium" => 2.0,
      "high" => 3.0,
      "critical" => 5.0,
      _ => 1.0
    }
    
    (user_impact + operation_impact + recovery_impact) * business_weight
  }
  
  // Calculate impact scores for all errors
  let scored_impacts = error_impacts.map_fn(impact) {
    {
      error_id: impact.error_id,
      error_type: impact.error_type,
      impact_score: calculate_impact_score(impact),
      business_impact: impact.business_impact,
      affected_users: impact.affected_users
    }
  }
  
  // Sort by impact score (highest first)
  let sorted_by_impact = {
    let mut sorted = scored_impacts
    // Simple bubble sort for demonstration
    let mut i = 0
    while i < sorted.length() {
      let mut j = i + 1
      while j < sorted.length() {
        if sorted[j].impact_score > sorted[i].impact_score {
          let temp = sorted[i]
          sorted[i] = sorted[j]
          sorted[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    sorted
  }
  
  // Verify highest impact error
  let highest_impact = sorted_by_impact[0]
  assert_eq(highest_impact.error_type, "AuthenticationError")
  assert_eq(highest_impact.business_impact, "critical")
  
  // Calculate error impact by type
  let impact_by_type = fn(impacts: Array[ErrorImpact]) {
    let mut result = []
    let mut processed_types = []
    
    for impact in impacts {
      if not(processed_types.contains(impact.error_type)) {
        processed_types = processed_types.push(impact.error_type)
        
        let type_impacts = impacts.filter_fn(i) { i.error_type == impact.error_type }
        let total_affected_users = type_impacts.reduce(fn(acc, i) { acc + i.affected_users }, 0)
        let total_affected_operations = type_impacts.reduce(fn(acc, i) { acc + i.affected_operations }, 0)
        let avg_recovery_time = type_impacts.reduce(fn(acc, i) { acc + i.recovery_time }, 0) / type_impacts.length()
        
        result = result.push({
          error_type: impact.error_type,
          occurrence_count: type_impacts.length(),
          total_affected_users: total_affected_users,
          total_affected_operations: total_affected_operations,
          avg_recovery_time: avg_recovery_time
        })
      }
    }
    
    result
  }
  
  let by_type = impact_by_type(error_impacts)
  assert_eq(by_type.length(), 4)
  
  // Check DatabaseError impact
  let db_impact = by_type.find_fn(i) { i.error_type == "DatabaseError" }
  match db_impact {
    Some(impact) => {
      assert_eq(impact.occurrence_count, 1)
      assert_eq(impact.total_affected_users, 1500)
      assert_eq(impact.total_affected_operations, 5000)
      assert_eq(impact.avg_recovery_time, 15)
    }
    None => assert_true(false)
  }
  
  // Find services most affected by errors
  let calculate_service_error_rate = fn(metrics: ServiceMetrics) {
    metrics.failed_operations.to_float() / metrics.total_operations.to_float() * 100.0
  }
  
  let service_error_rates = service_metrics.map_fn(m) {
    {
      service: m.service,
      error_rate: calculate_service_error_rate(m),
      availability: m.availability,
      failed_operations: m.failed_operations
    }
  }
  
  // Sort by error rate (highest first)
  let sorted_by_error_rate = {
    let mut sorted = service_error_rates
    let mut i = 0
    while i < sorted.length() {
      let mut j = i + 1
      while j < sorted.length() {
        if sorted[j].error_rate > sorted[i].error_rate {
          let temp = sorted[i]
          sorted[i] = sorted[j]
          sorted[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    sorted
  }
  
  // Verify service with highest error rate
  let highest_error_rate = sorted_by_error_rate[0]
  assert_eq(highest_error_rate.service, "auth-service")
  assert_eq(highest_error_rate.error_rate, 25.0)  // 3000/12000 * 100 = 25%
  assert_eq(highest_error_rate.availability, 75.0)
}

// Test 5: Error Resolution Tracking
test "error resolution tracking" {
  type ErrorResolution = {
    error_id: String,
    error_type: String,
    detected_at: Int,
    resolved_at: Int,
    resolution_method: String,
    resolver: String,
    verified: Bool
  }
  
  type ResolutionMetrics = {
    error_type: String,
    total_errors: Int,
    resolved_errors: Int,
    avg_resolution_time: Float,  // hours
    resolution_rate: Float,  // percentage
    common_methods: Array[String]
  }
  
  // Create sample error resolution data
  let resolutions = [
    {
      error_id: "err-001",
      error_type: "DatabaseError",
      detected_at: 1640995200,
      resolved_at: 1640995300,
      resolution_method: "connection_pool_restart",
      resolver: "ops-team",
      verified: true
    },
    {
      error_id: "err-002",
      error_type: "ValidationError",
      detected_at: 1640995250,
      resolved_at: 1640995270,
      resolution_method: "input_validation_fix",
      resolver: "dev-team",
      verified: true
    },
    {
      error_id: "err-003",
      error_type: "NetworkError",
      detected_at: 1640995300,
      resolved_at: 1640995400,
      resolution_method: "retry_mechanism",
      resolver: "ops-team",
      verified: true
    },
    {
      error_id: "err-004",
      error_type: "DatabaseError",
      detected_at: 1640995350,
      resolved_at: 1640995500,
      resolution_method: "query_optimization",
      resolver: "dev-team",
      verified: false
    },
    {
      error_id: "err-005",
      error_type: "AuthenticationError",
      detected_at: 1640995400,
      resolved_at: 1640995700,
      resolution_method: "token_refresh",
      resolver: "ops-team",
      verified: true
    },
    {
      error_id: "err-006",
      error_type: "NetworkError",
      detected_at: 1640995450,
      resolved_at: 0,  // Not yet resolved
      resolution_method: "",
      resolver: "",
      verified: false
    }
  ]
  
  // Calculate resolution metrics by error type
  let calculate_resolution_metrics = fn(resolutions: Array[ErrorResolution]) {
    let mut result = []
    let mut processed_types = []
    
    for resolution in resolutions {
      if not(processed_types.contains(resolution.error_type)) {
        processed_types = processed_types.push(resolution.error_type)
        
        let type_resolutions = resolutions.filter_fn(r) { r.error_type == resolution.error_type }
        let resolved = type_resolutions.filter_fn(r) { r.resolved_at > 0 }
        
        let resolution_times = resolved.map_fn(r) { 
          (r.resolved_at - r.detected_at).to_float() / 3600.0  // Convert to hours
        }
        
        let avg_resolution_time = if resolution_times.length() > 0 {
          resolution_times.reduce(fn(acc, t) { acc + t }, 0.0) / resolution_times.length().to_float()
        } else {
          0.0
        }
        
        let resolution_methods = resolved.map_fn(r) { r.resolution_method }
        let unique_methods = {
          let mut unique = []
          for method in resolution_methods {
            if not(unique.contains(method)) {
              unique = unique.push(method)
            }
          }
          unique
        }
        
        result = result.push({
          error_type: resolution.error_type,
          total_errors: type_resolutions.length(),
          resolved_errors: resolved.length(),
          avg_resolution_time: avg_resolution_time,
          resolution_rate: if type_resolutions.length() > 0 {
            resolved.length().to_float() / type_resolutions.length().to_float() * 100.0
          } else {
            0.0
          },
          common_methods: unique_methods
        })
      }
    }
    
    result
  }
  
  let metrics = calculate_resolution_metrics(resolutions)
  assert_eq(metrics.length(), 4)
  
  // Check DatabaseError metrics
  let db_metrics = metrics.find_fn(m) { m.error_type == "DatabaseError" }
  match db_metrics {
    Some(metric) => {
      assert_eq(metric.total_errors, 2)
      assert_eq(metric.resolved_errors, 2)
      assert_eq(metric.resolution_rate, 100.0)
      assert_eq(metric.avg_resolution_time, ((100 + 150) / 3600.0))  // (100s + 150s) / 2 / 3600
      assert_eq(metric.common_methods.length(), 2)
      assert_true(metric.common_methods.contains("connection_pool_restart"))
      assert_true(metric.common_methods.contains("query_optimization"))
    }
    None => assert_true(false)
  }
  
  // Check NetworkError metrics
  let network_metrics = metrics.find_fn(m) { m.error_type == "NetworkError" }
  match network_metrics {
    Some(metric) => {
      assert_eq(metric.total_errors, 2)
      assert_eq(metric.resolved_errors, 1)
      assert_eq(metric.resolution_rate, 50.0)
      assert_eq(metric.avg_resolution_time, 100.0 / 3600.0)  // 100s / 3600
      assert_eq(metric.common_methods.length(), 1)
      assert_true(metric.common_methods.contains("retry_mechanism"))
    }
    None => assert_true(false)
  }
  
  // Find unresolved errors
  let unresolved_errors = resolutions.filter_fn(r) { r.resolved_at == 0 }
  assert_eq(unresolved_errors.length(), 1)
  assert_eq(unresolved_errors[0].error_type, "NetworkError")
  
  // Calculate resolution time by resolver
  let resolution_by_resolver = fn(resolutions: Array[ErrorResolution]) {
    let mut result = []
    let mut processed_resolvers = []
    
    for resolution in resolutions {
      if resolution.resolver.length() > 0 and not(processed_resolvers.contains(resolution.resolver)) {
        processed_resolvers = processed_resolvers.push(resolution.resolver)
        
        let resolver_resolutions = resolutions.filter_fn(r) { 
          r.resolver == resolution.resolver and r.resolved_at > 0 
        }
        
        let resolution_times = resolver_resolutions.map_fn(r) { 
          (r.resolved_at - r.detected_at).to_float() / 3600.0
        }
        
        let avg_time = if resolution_times.length() > 0 {
          resolution_times.reduce(fn(acc, t) { acc + t }, 0.0) / resolution_times.length().to_float()
        } else {
          0.0
        }
        
        result = result.push({
          resolver: resolution.resolver,
          resolved_count: resolver_resolutions.length(),
          avg_resolution_time: avg_time
        })
      }
    }
    
    result
  }
  
  let by_resolver = resolution_by_resolver(resolutions)
  assert_eq(by_resolver.length(), 2)
  
  // Check ops-team metrics
  let ops_metrics = by_resolver.find_fn(m) { m.resolver == "ops-team" }
  match ops_metrics {
    Some(metric) => {
      assert_eq(metric.resolved_count, 3)
      assert_eq(metric.avg_resolution_time, ((100 + 100 + 300) / 3.0 / 3600.0))
    }
    None => assert_true(false)
  }
  
  // Check dev-team metrics
  let dev_metrics = by_resolver.find_fn(m) { m.resolver == "dev-team" }
  match dev_metrics {
    Some(metric) => {
      assert_eq(metric.resolved_count, 2)
      assert_eq(metric.avg_resolution_time, ((20 + 150) / 2.0 / 3600.0))
    }
    None => assert_true(false)
  }
}