// Azimuth Telemetry System - New Logging Correlation Tests
// This file contains test cases for logging and correlation functionality

// Test 1: Log Record Creation and Basic Operations
test "log record creation and basic operations" {
  // Create log record
  let log_record = {
    timestamp: 1640995200,
    severity_number: 9, // Info
    severity_text: "INFO",
    body: "User authentication successful",
    attributes: [
      ("user.id", "user-123"),
      ("service.name", "auth-service"),
      ("operation.name", "authenticate")
    ],
    trace_id: Some("trace-001"),
    span_id: Some("span-001"),
    trace_flags: Some(1)
  }
  
  // Test log record properties
  assert_eq(log_record.timestamp, 1640995200)
  assert_eq(log_record.severity_number, 9)
  assert_eq(log_record.severity_text, "INFO")
  assert_eq(log_record.body, "User authentication successful")
  assert_eq(log_record.attributes.length(), 3)
  assert_eq(log_record.trace_id, Some("trace-001"))
  assert_eq(log_record.span_id, Some("span-001"))
  assert_eq(log_record.trace_flags, Some(1))
  
  // Test attribute access
  let get_attribute = fn(attributes: Array[(String, String)], key: String) {
    let attribute = attributes.find_fn(attr) { attr.0 == key }
    match attribute {
      Some((_, value)) => Some(value)
      None => None
    }
  }
  
  assert_eq(get_attribute(log_record.attributes, "user.id"), Some("user-123"))
  assert_eq(get_attribute(log_record.attributes, "service.name"), Some("auth-service"))
  assert_eq(get_attribute(log_record.attributes, "operation.name"), Some("authenticate"))
  assert_eq(get_attribute(log_record.attributes, "nonexistent"), None)
  
  // Test adding attribute
  let add_attribute = fn(log: Dynamic, key: String, value: String) {
    let existing_attr = log.attributes.find_fn(attr) { attr.0 == key }
    
    match existing_attr {
      Some(_) => {
        // Update existing attribute
        { log | 
          attributes: log.attributes.map_fn(attr) {
            if attr.0 == key {
              (key, value)
            } else {
              attr
            }
          }
        }
      }
      None => {
        // Add new attribute
        { log | 
          attributes: log.attributes.push((key, value))
        }
      }
    }
  }
  
  let updated_log = add_attribute(log_record, "session.id", "session-456")
  assert_eq(updated_log.attributes.length(), 4)
  assert_eq(get_attribute(updated_log.attributes, "session.id"), Some("session-456"))
  
  // Test updating existing attribute
  let updated_user = add_attribute(updated_log, "user.id", "user-789")
  assert_eq(get_attribute(updated_user.attributes, "user.id"), Some("user-789"))
  assert_eq(updated_user.attributes.length(), 4) // Still 4, not 5
}

// Test 2: Log Severity Levels
test "log severity levels operations" {
  // Define severity levels
  let severity_levels = [
    (1, "TRACE"),
    (2, "DEBUG"),
    (3, "DEBUG"),
    (4, "INFO"),
    (5, "INFO"),
    (6, "INFO"),
    (7, "WARN"),
    (8, "WARN"),
    (9, "INFO"),
    (10, "FATAL"),
    (11, "FATAL"),
    (12, "FATAL"),
    (13, "FATAL"),
    (14, "FATAL"),
    (15, "FATAL"),
    (16, "FATAL"),
    (17, "FATAL"),
    (18, "FATAL"),
    (19, "FATAL"),
    (20, "FATAL"),
    (21, "FATAL"),
    (22, "FATAL"),
    (23, "FATAL"),
    (24, "FATAL")
  ]
  
  // Get severity text function
  let get_severity_text = fn(severity_number: Int) {
    let level = severity_levels.find_fn(l) { l.0 == severity_number }
    match level {
      Some((_, text)) => text
      None => "UNKNOWN"
    }
  }
  
  // Check if severity is error or higher
  let is_error_or_higher = fn(severity_number: Int) {
    severity_number >= 17 // ERROR is level 17
  }
  
  // Check if severity is warning or higher
  let is_warning_or_higher = fn(severity_number: Int) {
    severity_number >= 13 // WARN is level 13
  }
  
  // Test severity level lookups
  assert_eq(get_severity_text(1), "TRACE")
  assert_eq(get_severity_text(4), "INFO")
  assert_eq(get_severity_text(9), "INFO")
  assert_eq(get_severity_text(13), "WARN")
  assert_eq(get_severity_text(17), "FATAL")
  assert_eq(get_severity_text(99), "UNKNOWN") // Not defined
  
  // Test severity level checks
  assert_false(is_error_or_higher(9))  // INFO
  assert_false(is_error_or_higher(13)) // WARN
  assert_true(is_error_or_higher(17))  // ERROR
  assert_true(is_error_or_higher(20))  // FATAL
  
  assert_false(is_warning_or_higher(9))  // INFO
  assert_true(is_warning_or_higher(13)) // WARN
  assert_true(is_warning_or_higher(17)) // ERROR
  assert_true(is_warning_or_higher(20)) // FATAL
  
  // Create logs with different severity levels
  let trace_log = {
    timestamp: 1640995200,
    severity_number: 1,
    severity_text: "TRACE",
    body: "Entering function",
    attributes: []
  }
  
  let info_log = {
    timestamp: 1640995210,
    severity_number: 9,
    severity_text: "INFO",
    body: "Processing request",
    attributes: []
  }
  
  let warn_log = {
    timestamp: 1640995220,
    severity_number: 13,
    severity_text: "WARN",
    body: "High memory usage detected",
    attributes: []
  }
  
  let error_log = {
    timestamp: 1640995230,
    severity_number: 17,
    severity_text: "ERROR",
    body: "Database connection failed",
    attributes: []
  }
  
  let logs = [trace_log, info_log, warn_log, error_log]
  
  // Filter logs by severity
  let error_logs = logs.filter_fn(log) { is_error_or_higher(log.severity_number) }
  assert_eq(error_logs.length(), 1)
  assert_eq(error_logs[0].severity_text, "ERROR")
  
  let warning_logs = logs.filter_fn(log) { is_warning_or_higher(log.severity_number) }
  assert_eq(warning_logs.length(), 2) // WARN and ERROR
  assert_true(warning_logs.map_fn(l) { l.severity_text }.contains("WARN"))
  assert_true(warning_logs.map_fn(l) { l.severity_text }.contains("ERROR"))
}

// Test 3: Log and Trace Correlation
test "log and trace correlation operations" {
  // Create trace context
  let trace_context = {
    trace_id: "trace-correlation-001",
    span_id: "span-correlation-001",
    trace_flags: 1,
    baggage: [
      ("user.id", "user-123"),
      ("request.id", "req-456")
    ]
  }
  
  // Create log with trace context
  let create_log_with_trace_context = fn(trace_ctx: Dynamic, body: String, severity_number: Int) {
    {
      timestamp: 1640995200,
      severity_number: severity_number,
      severity_text: "INFO", // Simplified
      body: body,
      attributes: [
        ("trace.id", trace_ctx.trace_id),
        ("span.id", trace_ctx.span_id),
        ("trace.flags", trace_ctx.trace_flags.to_string())
      ],
      trace_id: Some(trace_ctx.trace_id),
      span_id: Some(trace_ctx.span_id),
      trace_flags: Some(trace_ctx.trace_flags)
    }
  }
  
  // Create correlated logs
  let request_log = create_log_with_trace_context(trace_context, "Request received", 9)
  let processing_log = create_log_with_trace_context(trace_context, "Processing request", 9)
  let completion_log = create_log_with_trace_context(trace_context, "Request completed", 9)
  
  // Test trace correlation
  assert_eq(request_log.trace_id, Some("trace-correlation-001"))
  assert_eq(request_log.span_id, Some("span-correlation-001"))
  assert_eq(processing_log.trace_id, Some("trace-correlation-001"))
  assert_eq(processing_log.span_id, Some("span-correlation-001"))
  assert_eq(completion_log.trace_id, Some("trace-correlation-001"))
  assert_eq(completion_log.span_id, Some("span-correlation-001"))
  
  // Test attribute correlation
  let get_attribute = fn(attributes: Array[(String, String)], key: String) {
    let attribute = attributes.find_fn(attr) { attr.0 == key }
    match attribute {
      Some((_, value)) => Some(value)
      None => None
    }
  }
  
  assert_eq(get_attribute(request_log.attributes, "trace.id"), Some("trace-correlation-001"))
  assert_eq(get_attribute(request_log.attributes, "span.id"), Some("span-correlation-001"))
  
  // Test log filtering by trace ID
  let correlated_logs = [request_log, processing_log, completion_log]
  let logs_by_trace_id = correlated_logs.filter_fn(log) { 
    log.trace_id == Some("trace-correlation-001") 
  }
  
  assert_eq(logs_by_trace_id.length(), 3)
  
  // Test with different trace context
  let different_trace_context = {
    trace_id: "trace-correlation-002",
    span_id: "span-correlation-002",
    trace_flags: 1,
    baggage: []
  }
  
  let different_log = create_log_with_trace_context(different_trace_context, "Different request", 9)
  let all_logs = correlated_logs.push(different_log)
  
  let logs_by_first_trace = all_logs.filter_fn(log) { 
    log.trace_id == Some("trace-correlation-001") 
  }
  
  assert_eq(logs_by_first_trace.length(), 3)
  
  let logs_by_second_trace = all_logs.filter_fn(log) { 
    log.trace_id == Some("trace-correlation-002") 
  }
  
  assert_eq(logs_by_second_trace.length(), 1)
}

// Test 4: Log Aggregation and Analysis
test "log aggregation and analysis operations" {
  // Create log data for analysis
  let log_data = [
    {
      timestamp: 1640995200,
      service: "auth-service",
      severity: "INFO",
      message: "User login successful",
      user_id: "user-123"
    },
    {
      timestamp: 1640995210,
      service: "auth-service",
      severity: "ERROR",
      message: "Authentication failed",
      user_id: "user-456"
    },
    {
      timestamp: 1640995220,
      service: "payment-service",
      severity: "INFO",
      message: "Payment processed",
      user_id: "user-123"
    },
    {
      timestamp: 1640995230,
      service: "payment-service",
      severity: "WARN",
      message: "Payment retry attempted",
      user_id: "user-789"
    },
    {
      timestamp: 1640995240,
      service: "auth-service",
      severity: "ERROR",
      message: "Authentication failed",
      user_id: "user-789"
    },
    {
      timestamp: 1640995250,
      service: "payment-service",
      severity: "INFO",
      message: "Payment processed",
      user_id: "user-456"
    }
  ]
  
  // Aggregate logs by service
  let aggregate_by_service = fn(logs: Array[Dynamic>) {
    let mut services = []
    let mut processed_services = []
    
    for log in logs {
      let service_name = log.service
      
      if not(processed_services.contains(service_name)) {
        let service_logs = logs.filter_fn(l) { l.service == service_name }
        let error_count = service_logs.filter_fn(l) { l.severity == "ERROR" }.length()
        let warn_count = service_logs.filter_fn(l) { l.severity == "WARN" }.length()
        let info_count = service_logs.filter_fn(l) { l.severity == "INFO" }.length()
        
        services = services.push({
          service: service_name,
          total_logs: service_logs.length(),
          error_count: error_count,
          warn_count: warn_count,
          info_count: info_count,
          error_rate: (error_count * 100) / service_logs.length()
        })
        
        processed_services = processed_services.push(service_name)
      }
    }
    
    services
  }
  
  // Aggregate logs by user
  let aggregate_by_user = fn(logs: Array[Dynamic>) {
    let mut users = []
    let mut processed_users = []
    
    for log in logs {
      let user_id = log.user_id
      
      if not(processed_users.contains(user_id)) {
        let user_logs = logs.filter_fn(l) { l.user_id == user_id }
        let services = user_logs.map_fn(l) { l.service }.unique()
        
        users = users.push({
          user_id: user_id,
          total_logs: user_logs.length(),
          services: services,
          error_count: user_logs.filter_fn(l) { l.severity == "ERROR" }.length()
        })
        
        processed_users = processed_users.push(user_id)
      }
    }
    
    users
  }
  
  // Find error patterns
  let find_error_patterns = fn(logs: Array[Dynamic>) {
    let error_logs = logs.filter_fn(l) { l.severity == "ERROR" }
    let mut patterns = []
    let mut processed_messages = []
    
    for error_log in error_logs {
      let message = error_log.message
      
      if not(processed_messages.contains(message)) {
        let same_message_logs = error_logs.filter_fn(l) { l.message == message }
        
        patterns = patterns.push({
          message: message,
          count: same_message_logs.length(),
          services: same_message_logs.map_fn(l) { l.service }.unique()
        })
        
        processed_messages = processed_messages.push(message)
      }
    }
    
    patterns.sort(fn(a, b) { b.count - a.count })
  }
  
  // Test service aggregation
  let service_stats = aggregate_by_service(log_data)
  assert_eq(service_stats.length(), 2) // auth-service, payment-service
  
  let auth_service = service_stats.find_fn(s) { s.service == "auth-service" }
  assert_eq(auth_service.total_logs, 3)
  assert_eq(auth_service.error_count, 2)
  assert_eq(auth_service.warn_count, 0)
  assert_eq(auth_service.info_count, 1)
  assert_eq(auth_service.error_rate, 66) // floor(2 * 100 / 3)
  
  let payment_service = service_stats.find_fn(s) { s.service == "payment-service" }
  assert_eq(payment_service.total_logs, 3)
  assert_eq(payment_service.error_count, 0)
  assert_eq(payment_service.warn_count, 1)
  assert_eq(payment_service.info_count, 2)
  assert_eq(payment_service.error_rate, 0)
  
  // Test user aggregation
  let user_stats = aggregate_by_user(log_data)
  assert_eq(user_stats.length(), 3) // user-123, user-456, user-789
  
  let user123 = user_stats.find_fn(u) { u.user_id == "user-123" }
  assert_eq(user123.total_logs, 2)
  assert_eq(user123.services.length(), 2) // auth-service, payment-service
  assert_eq(user123.error_count, 0)
  
  let user456 = user_stats.find_fn(u) { u.user_id == "user-456" }
  assert_eq(user456.total_logs, 2)
  assert_eq(user456.services.length(), 2) // auth-service, payment-service
  assert_eq(user456.error_count, 1)
  
  let user789 = user_stats.find_fn(u) { u.user_id == "user-789" }
  assert_eq(user789.total_logs, 2)
  assert_eq(user789.services.length(), 2) // payment-service, auth-service
  assert_eq(user789.error_count, 1)
  
  // Test error pattern analysis
  let error_patterns = find_error_patterns(log_data)
  assert_eq(error_patterns.length(), 1) // Only "Authentication failed"
  assert_eq(error_patterns[0].message, "Authentication failed")
  assert_eq(error_patterns[0].count, 2)
  assert_eq(error_patterns[0].services.length(), 1) // Only auth-service
}

// Test 5: Log Filtering and Search
test "log filtering and search operations" {
  // Create log data for filtering
  let log_entries = [
    {
      timestamp: 1640995200,
      severity: "INFO",
      service: "auth-service",
      message: "User login successful",
      attributes: [("user.id", "user-123"), ("ip", "192.168.1.1")]
    },
    {
      timestamp: 1640995210,
      severity: "ERROR",
      service: "payment-service",
      message: "Payment processing failed",
      attributes: [("user.id", "user-456"), ("amount", "100.50")]
    },
    {
      timestamp: 1640995220,
      severity: "WARN",
      service: "auth-service",
      message: "Rate limit exceeded",
      attributes: [("user.id", "user-789"), ("ip", "192.168.1.2")]
    },
    {
      timestamp: 1640995230,
      severity: "INFO",
      service: "notification-service",
      message: "Email sent successfully",
      attributes: [("user.id", "user-123"), ("email", "user@example.com")]
    },
    {
      timestamp: 1640995240,
      severity: "ERROR",
      service: "auth-service",
      message: "Database connection failed",
      attributes: [("error.code", "500"), ("retry.count", "3")]
    }
  ]
  
  // Filter by severity
  let filter_by_severity = fn(logs: Array[Dynamic>, severity: String) {
    logs.filter_fn(log) { log.severity == severity }
  }
  
  // Filter by service
  let filter_by_service = fn(logs: Array[Dynamic>, service: String) {
    logs.filter_fn(log) { log.service == service }
  }
  
  // Filter by time range
  let filter_by_time_range = fn(logs: Array[Dynamic>, start_time: Int, end_time: Int) {
    logs.filter_fn(log) { 
      log.timestamp >= start_time and log.timestamp <= end_time 
    }
  }
  
  // Filter by attribute
  let filter_by_attribute = fn(logs: Array[Dynamic>, key: String, value: String) {
    logs.filter_fn(log) { 
      log.attributes.find_fn(attr) { 
        attr.0 == key and attr.1 == value 
      } != None
    }
  }
  
  // Search by message content
  let search_by_message = fn(logs: Array[Dynamic>, query: String) {
    logs.filter_fn(log) { 
      log.message.contains(query)
    }
  }
  
  // Test severity filtering
  let error_logs = filter_by_severity(log_entries, "ERROR")
  assert_eq(error_logs.length(), 2)
  assert_true(error_logs.map_fn(l) { l.service }.contains("payment-service"))
  assert_true(error_logs.map_fn(l) { l.service }.contains("auth-service"))
  
  let info_logs = filter_by_severity(log_entries, "INFO")
  assert_eq(info_logs.length(), 2)
  assert_true(info_logs.map_fn(l) { l.service }.contains("auth-service"))
  assert_true(info_logs.map_fn(l) { l.service }.contains("notification-service"))
  
  // Test service filtering
  let auth_logs = filter_by_service(log_entries, "auth-service")
  assert_eq(auth_logs.length(), 3)
  
  let payment_logs = filter_by_service(log_entries, "payment-service")
  assert_eq(payment_logs.length(), 1)
  
  // Test time range filtering
  let time_filtered = filter_by_time_range(log_entries, 1640995210, 1640995230)
  assert_eq(time_filtered.length(), 3) // Entries at 1640995210, 1640995220, 1640995230
  
  // Test attribute filtering
  let user123_logs = filter_by_attribute(log_entries, "user.id", "user-123")
  assert_eq(user123_logs.length(), 2)
  assert_eq(user123_logs[0].service, "auth-service")
  assert_eq(user123_logs[1].service, "notification-service")
  
  // Test message search
  let user_logs = search_by_message(log_entries, "user")
  assert_eq(user_logs.length(), 2) // "User login successful", "Email sent successfully"
  
  let failed_logs = search_by_message(log_entries, "failed")
  assert_eq(failed_logs.length(), 2) // "Payment processing failed", "Database connection failed"
  
  // Test combined filtering
  let auth_error_logs = filter_by_service(log_entries, "auth-service")
    .filter_fn(log) { log.severity == "ERROR" }
  
  assert_eq(auth_error_logs.length(), 1)
  assert_eq(auth_error_logs[0].message, "Database connection failed")
}