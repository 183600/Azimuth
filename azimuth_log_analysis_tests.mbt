// Azimuth Log Analysis Test Suite
// This file contains test cases for log analysis and processing

// Test 1: Log Parsing and Structure
test "log parsing and structure validation" {
  // Define log entry structure
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    source: String,
    attributes: Array[(String, String)]
  }
  
  // Parse log line
  let parse_log_line = fn(line: String) {
    // Simple log format: [timestamp] [level] [source] message
    let parts = line.split(" ")
    if parts.length() >= 4 {
      let timestamp_str = parts[0].substring(1, parts[0].length() - 2)
      let level = parts[1].substring(1, parts[1].length() - 2)
      let source = parts[2].substring(1, parts[2].length() - 2)
      let message = parts.slice(3, parts.length()).join(" ")
      
      // Parse timestamp
      let timestamp = match timestamp_str.parse_int() {
        Some(ts) => ts
        None => 0
      }
      
      Some({
        timestamp,
        level,
        message,
        source,
        attributes: []
      })
    } else {
      None
    }
  }
  
  // Test valid log line parsing
  let valid_log = "[1640995200] [INFO] [api-gateway] Request received for /api/users"
  let parsed_log = parse_log_line(valid_log)
  
  match parsed_log {
    Some(entry) => {
      assert_eq(entry.timestamp, 1640995200)
      assert_eq(entry.level, "INFO")
      assert_eq(entry.source, "api-gateway")
      assert_eq(entry.message, "Request received for /api/users")
      assert_eq(entry.attributes.length(), 0)
    }
    None => assert_true(false)
  }
  
  // Test invalid log line parsing
  let invalid_log = "Invalid log format without proper structure"
  let parsed_invalid = parse_log_line(invalid_log)
  assert_eq(parsed_invalid, None)
  
  // Test log with attributes
  let log_with_attrs = "[1640995250] [ERROR] [database] Connection failed: timeout=30s retries=3"
  let parsed_with_attrs = parse_log_line(log_with_attrs)
  
  match parsed_with_attrs {
    Some(entry) => {
      assert_eq(entry.level, "ERROR")
      assert_eq(entry.source, "database")
      assert_true(entry.message.contains("Connection failed"))
    }
    None => assert_true(false)
  }
  
  // Test log level validation
  let valid_levels = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
  let is_valid_level = fn(level: String) {
    valid_levels.contains(level.to_uppercase())
  }
  
  assert_true(is_valid_level("INFO"))
  assert_true(is_valid_level("error"))  // Case insensitive
  assert_false(is_valid_level("INVALID"))
  
  // Test timestamp validation
  let is_valid_timestamp = fn(timestamp: Int) {
    timestamp > 1000000000 && timestamp < 2000000000  // Reasonable timestamp range
  }
  
  assert_true(is_valid_timestamp(1640995200))
  assert_false(is_valid_timestamp(0))
  assert_false(is_valid_timestamp(999999999))
  assert_false(is_valid_timestamp(2000000000))
}

// Test 2: Log Filtering and Querying
test "log filtering and querying operations" {
  // Define log entry
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    source: String,
    attributes: Array[(String, String)]
  }
  
  // Create sample log entries
  let log_entries = [
    {
      timestamp: 1640995200,
      level: "INFO",
      message: "User login successful",
      source: "auth-service",
      attributes: [("user_id", "12345"), ("ip", "192.168.1.100")]
    },
    {
      timestamp: 1640995210,
      level: "ERROR",
      message: "Database connection failed",
      source: "user-service",
      attributes: [("error_code", "DB001"), ("retry_count", "3")]
    },
    {
      timestamp: 1640995220,
      level: "WARN",
      message: "High memory usage detected",
      source: "monitoring",
      attributes: [("memory_usage", "85%"), ("threshold", "80%")]
    },
    {
      timestamp: 1640995230,
      level: "INFO",
      message: "Request processed",
      source: "api-gateway",
      attributes: [("method", "GET"), ("/api/users", "path"), ("duration", "150ms")]
    },
    {
      timestamp: 1640995240,
      level: "ERROR",
      message: "Authentication failed",
      source: "auth-service",
      attributes: [("user_id", "67890"), ("reason", "invalid_token")]
    }
  ]
  
  // Filter logs by level
  let filter_by_level = fn(logs: Array[LogEntry], level: String) {
    logs.filter(fn(log) { log.level.to_uppercase() == level.to_uppercase() })
  }
  
  let error_logs = filter_by_level(log_entries, "ERROR")
  assert_eq(error_logs.length(), 2)
  assert_eq(error_logs[0].source, "user-service")
  assert_eq(error_logs[1].source, "auth-service")
  
  let info_logs = filter_by_level(log_entries, "info")
  assert_eq(info_logs.length(), 2)
  assert_eq(info_logs[0].message, "User login successful")
  assert_eq(info_logs[1].message, "Request processed")
  
  // Filter logs by source
  let filter_by_source = fn(logs: Array[LogEntry], source: String) {
    logs.filter(fn(log) { log.source == source })
  }
  
  let auth_logs = filter_by_source(log_entries, "auth-service")
  assert_eq(auth_logs.length(), 2)
  assert_eq(auth_logs[0].level, "INFO")
  assert_eq(auth_logs[1].level, "ERROR")
  
  // Filter logs by time range
  let filter_by_time_range = fn(logs: Array[LogEntry], start_time: Int, end_time: Int) {
    logs.filter(fn(log) { log.timestamp >= start_time && log.timestamp <= end_time })
  }
  
  let time_filtered = filter_by_time_range(log_entries, 1640995210, 1640995230)
  assert_eq(time_filtered.length(), 3)
  assert_eq(time_filtered[0].level, "ERROR")
  assert_eq(time_filtered[1].level, "WARN")
  assert_eq(time_filtered[2].level, "INFO")
  
  // Filter logs by message content
  let filter_by_message = fn(logs: Array[LogEntry], keyword: String) {
    logs.filter(fn(log) { log.message.to_lowercase().contains(keyword.to_lowercase()) })
  }
  
  let user_logs = filter_by_message(log_entries, "user")
  assert_eq(user_logs.length(), 3)
  assert_true(user_logs[0].message.contains("User"))
  assert_true(user_logs[1].message.contains("user"))
  assert_true(user_logs[2].message.contains("user"))
  
  // Filter logs by attributes
  let filter_by_attribute = fn(logs: Array[LogEntry], key: String, value: String) {
    logs.filter(fn(log) { 
      log.attributes.any(fn(attr) { attr.0 == key && attr.1 == value })
    })
  }
  
  let user_12345_logs = filter_by_attribute(log_entries, "user_id", "12345")
  assert_eq(user_12345_logs.length(), 1)
  assert_eq(user_12345_logs[0].message, "User login successful")
  
  // Complex filter with multiple criteria
  let complex_filter = fn(logs: Array[LogEntry], criteria: {
    level: Option[String],
    source: Option[String],
    start_time: Option[Int],
    end_time: Option[Int],
    message_keyword: Option[String]
  }) {
    logs.filter(fn(log) {
      let level_match = match criteria.level {
        Some(level) => log.level.to_uppercase() == level.to_uppercase()
        None => true
      }
      
      let source_match = match criteria.source {
        Some(source) => log.source == source
        None => true
      }
      
      let time_match = match (criteria.start_time, criteria.end_time) {
        (Some(start), Some(end)) => log.timestamp >= start && log.timestamp <= end
        (Some(start), None) => log.timestamp >= start
        (None, Some(end)) => log.timestamp <= end
        (None, None) => true
      }
      
      let message_match = match criteria.message_keyword {
        Some(keyword) => log.message.to_lowercase().contains(keyword.to_lowercase())
        None => true
      }
      
      level_match && source_match && time_match && message_match
    })
  }
  
  let complex_criteria = {
    level: Some("ERROR"),
    source: None,
    start_time: Some(1640995200),
    end_time: Some(1640995300),
    message_keyword: Some("failed")
  }
  
  let complex_filtered = complex_filter(log_entries, complex_criteria)
  assert_eq(complex_filtered.length(), 2)
  assert_true(complex_filtered[0].message.contains("failed"))
  assert_true(complex_filtered[1].message.contains("failed"))
}

// Test 3: Log Aggregation and Statistics
test "log aggregation and statistics calculation" {
  // Define log entry
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    source: String,
    attributes: Array[(String, String)]
  }
  
  // Define log statistics
  type LogStats = {
    total_count: Int,
    level_counts: Array[(String, Int)],
    source_counts: Array[(String, Int)],
    time_range: (Int, Int),
    error_rate: Float
  }
  
  // Create sample log entries
  let log_entries = [
    { timestamp: 1640995200, level: "INFO", message: "Server started", source: "api", attributes: [] },
    { timestamp: 1640995210, level: "INFO", message: "User login", source: "auth", attributes: [] },
    { timestamp: 1640995220, level: "ERROR", message: "DB connection failed", source: "db", attributes: [] },
    { timestamp: 1640995230, level: "WARN", message: "High memory", source: "monitor", attributes: [] },
    { timestamp: 1640995240, level: "INFO", message: "Request processed", source: "api", attributes: [] },
    { timestamp: 1640995250, level: "ERROR", message: "Auth failed", source: "auth", attributes: [] },
    { timestamp: 1640995260, level: "INFO", message: "Cache hit", source: "cache", attributes: [] },
    { timestamp: 1640995270, level: "WARN", message: "Slow query", source: "db", attributes: [] }
  ]
  
  // Calculate log statistics
  let calculate_stats = fn(logs: Array[LogEntry]) {
    let total_count = logs.length()
    
    // Count by level
    let level_counts = []
    let levels = ["INFO", "WARN", "ERROR", "DEBUG", "TRACE", "FATAL"]
    for level in levels {
      let count = logs.filter(fn(log) { log.level == level }).length()
      if count > 0 {
        level_counts = level_counts.push((level, count))
      }
    }
    
    // Count by source
    let source_counts = []
    let sources = logs.map(fn(log) { log.source })
    let unique_sources = sources.filter(fn(source, index) {
      not(sources.slice(0, index).contains(source))
    })
    
    for source in unique_sources {
      let count = logs.filter(fn(log) { log.source == source }).length()
      source_counts = source_counts.push((source, count))
    }
    
    // Calculate time range
    let timestamps = logs.map(fn(log) { log.timestamp })
    let min_time = timestamps.reduce(fn(min, ts) { if ts < min { ts } else { min } }, timestamps[0])
    let max_time = timestamps.reduce(fn(max, ts) { if ts > max { ts } else { max } }, timestamps[0])
    
    // Calculate error rate
    let error_count = logs.filter(fn(log) { log.level == "ERROR" }).length()
    let error_rate = if total_count > 0 {
      (error_count as Float / total_count as Float) * 100.0
    } else {
      0.0
    }
    
    {
      total_count,
      level_counts,
      source_counts,
      time_range: (min_time, max_time),
      error_rate
    }
  }
  
  // Calculate and test statistics
  let stats = calculate_stats(log_entries)
  assert_eq(stats.total_count, 8)
  assert_eq(stats.level_counts.length(), 3)  // INFO, WARN, ERROR
  assert_eq(stats.source_counts.length(), 4)  // api, auth, db, monitor, cache
  
  // Check level counts
  let info_count = stats.level_counts.find(fn(lc) { lc.0 == "INFO" })
  match info_count {
    Some(count) => assert_eq(count.1, 4)
    None => assert_true(false)
  }
  
  let error_count = stats.level_counts.find(fn(lc) { lc.0 == "ERROR" })
  match error_count {
    Some(count) => assert_eq(count.1, 2)
    None => assert_true(false)
  }
  
  let warn_count = stats.level_counts.find(fn(lc) { lc.0 == "WARN" })
  match warn_count {
    Some(count) => assert_eq(count.1, 2)
    None => assert_true(false)
  }
  
  // Check source counts
  let api_count = stats.source_counts.find(fn(sc) { sc.0 == "api" })
  match api_count {
    Some(count) => assert_eq(count.1, 2)
    None => assert_true(false)
  }
  
  let auth_count = stats.source_counts.find(fn(sc) { sc.0 == "auth" })
  match auth_count {
    Some(count) => assert_eq(count.1, 2)
    None => assert_true(false)
  }
  
  // Check time range
  assert_eq(stats.time_range.0, 1640995200)
  assert_eq(stats.time_range.1, 1640995270)
  
  // Check error rate
  assert_eq(stats.error_rate, 25.0)  // 2 errors / 8 total * 100
  
  // Test time-based aggregation
  let aggregate_by_time_window = fn(logs: Array[LogEntry], window_size: Int) {
    if logs.length() == 0 {
      []
    } else {
      let timestamps = logs.map(fn(log) { log.timestamp })
      let min_time = timestamps.reduce(fn(min, ts) { if ts < min { ts } else { min } }, timestamps[0])
      let max_time = timestamps.reduce(fn(max, ts) { if ts > max { ts } else { max } }, timestamps[0])
      
      let mut windows = []
      let mut current_start = min_time
      
      while current_start <= max_time {
        let current_end = current_start + window_size
        let window_logs = logs.filter(fn(log) { 
          log.timestamp >= current_start && log.timestamp < current_end 
        })
        
        windows = windows.push({
          start_time: current_start,
          end_time: current_end,
          count: window_logs.length(),
          error_count: window_logs.filter(fn(log) { log.level == "ERROR" }).length()
        })
        
        current_start = current_end
      }
      
      windows
    }
  }
  
  let time_windows = aggregate_by_time_window(log_entries, 30)  // 30-second windows
  assert_eq(time_windows.length(), 3)  // 70 seconds total / 30-second windows = 3 windows
  
  // First window should have logs from 1640995200 to 1640995230
  assert_eq(time_windows[0].count, 4)
  assert_eq(time_windows[0].error_count, 1)
  
  // Second window should have logs from 1640995230 to 1640995260
  assert_eq(time_windows[1].count, 3)
  assert_eq(time_windows[1].error_count, 1)
  
  // Third window should have logs from 1640995260 to 1640995290
  assert_eq(time_windows[2].count, 1)
  assert_eq(time_windows[2].error_count, 0)
}

// Test 4: Log Pattern Recognition
test "log pattern recognition and anomaly detection" {
  // Define log entry
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    source: String,
    attributes: Array[(String, String)]
  }
  
  // Define log pattern
  type LogPattern = {
    pattern_id: String,
    regex_pattern: String,
    description: String,
    severity: String
  }
  
  // Create sample log entries
  let log_entries = [
    { timestamp: 1640995200, level: "INFO", message: "User 12345 logged in successfully", source: "auth", attributes: [] },
    { timestamp: 1640995210, level: "INFO", message: "User 67890 logged in successfully", source: "auth", attributes: [] },
    { timestamp: 1640995220, level: "ERROR", message: "Database connection failed: timeout after 30s", source: "db", attributes: [] },
    { timestamp: 1640995230, level: "WARN", message: "Memory usage at 85%, threshold is 80%", source: "monitor", attributes: [] },
    { timestamp: 1640995240, level: "ERROR", message: "Database connection failed: timeout after 30s", source: "db", attributes: [] },
    { timestamp: 1640995250, level: "INFO", message: "User 11111 logged in successfully", source: "auth", attributes: [] },
    { timestamp: 1640995260, level: "ERROR", message: "Authentication failed for user 22222: invalid token", source: "auth", attributes: [] },
    { timestamp: 1640995270, level: "WARN", message: "Memory usage at 90%, threshold is 80%", source: "monitor", attributes: [] }
  ]
  
  // Define log patterns
  let patterns = [
    {
      pattern_id: "USER_LOGIN_SUCCESS",
      regex_pattern: "User \\d+ logged in successfully",
      description: "Successful user login",
      severity: "INFO"
    },
    {
      pattern_id: "DB_CONNECTION_FAILED",
      regex_pattern: "Database connection failed",
      description: "Database connection failure",
      severity: "ERROR"
    },
    {
      pattern_id: "HIGH_MEMORY_USAGE",
      regex_pattern: "Memory usage at \\d+%, threshold is \\d+%",
      description: "High memory usage warning",
      severity: "WARN"
    },
    {
      pattern_id: "AUTH_FAILED",
      regex_pattern: "Authentication failed",
      description: "Authentication failure",
      severity: "ERROR"
    }
  ]
  
  // Simple pattern matching (simulating regex)
  let matches_pattern = fn(message: String, pattern: String) {
    // This is a simplified pattern matching - in real implementation, use proper regex
    if pattern.contains("\\d+") {
      let base_pattern = pattern.replace("\\d+", "")
      let words = message.split(" ")
      let pattern_words = base_pattern.split(" ")
      
      let mut match_count = 0
      for pattern_word in pattern_words {
        if pattern_word.length() > 0 && message.contains(pattern_word) {
          match_count = match_count + 1
        }
      }
      
      match_count >= pattern_words.length() - 1  // Allow for some flexibility
    } else {
      message.contains(pattern)
    }
  }
  
  // Match logs against patterns
  let match_log_patterns = fn(logs: Array[LogEntry], log_patterns: Array[LogPattern]) {
    logs.map(fn(log) {
      let matched_patterns = log_patterns.filter(fn(pattern) {
        matches_pattern(log.message, pattern.regex_pattern)
      })
      
      (log, matched_patterns)
    })
  }
  
  // Test pattern matching
  let matched_logs = match_log_patterns(log_entries, patterns)
  
  // Check first log (user login success)
  assert_eq(matched_logs[0].1.length(), 1)
  assert_eq(matched_logs[0].1[0].pattern_id, "USER_LOGIN_SUCCESS")
  
  // Check third log (DB connection failed)
  assert_eq(matched_logs[2].1.length(), 1)
  assert_eq(matched_logs[2].1[0].pattern_id, "DB_CONNECTION_FAILED")
  
  // Check fourth log (high memory usage)
  assert_eq(matched_logs[3].1.length(), 1)
  assert_eq(matched_logs[3].1[0].pattern_id, "HIGH_MEMORY_USAGE")
  
  // Test pattern frequency analysis
  let analyze_pattern_frequency = fn(matched_logs: Array[(LogEntry, Array[LogPattern])]) {
    let mut pattern_counts = []
    
    for (_, patterns) in matched_logs {
      for pattern in patterns {
        let existing = pattern_counts.find_index(fn(pc) { pc.0 == pattern.pattern_id })
        match existing {
          Some(index) => {
            pattern_counts[index] = (pattern_counts[index].0, pattern_counts[index].1 + 1)
          }
          None => {
            pattern_counts = pattern_counts.push((pattern.pattern_id, 1))
          }
        }
      }
    }
    
    // Sort by frequency (descending)
    pattern_counts.sort(fn(a, b) {
      if a.1 > b.1 { -1 } else if a.1 < b.1 { 1 } else { 0 }
    })
  }
  
  let pattern_frequencies = analyze_pattern_frequency(matched_logs)
  assert_eq(pattern_frequencies.length(), 4)
  
  // USER_LOGIN_SUCCESS should be most frequent (3 times)
  assert_eq(pattern_frequencies[0], ("USER_LOGIN_SUCCESS", 3))
  
  // DB_CONNECTION_FAILED should be second (2 times)
  assert_eq(pattern_frequencies[1], ("DB_CONNECTION_FAILED", 2))
  
  // HIGH_MEMORY_USAGE should be third (2 times)
  assert_eq(pattern_frequencies[2], ("HIGH_MEMORY_USAGE", 2))
  
  // AUTH_FAILED should be fourth (1 time)
  assert_eq(pattern_frequencies[3], ("AUTH_FAILED", 1))
  
  // Test anomaly detection based on pattern frequency
  let detect_anomalies = fn(pattern_frequencies: Array[(String, Int)], threshold: Float) {
    if pattern_frequencies.length() == 0 {
      []
    } else {
      let total_patterns = pattern_frequencies.reduce(fn(sum, pf) { sum + pf.1 }, 0)
      let avg_frequency = total_patterns as Float / pattern_frequencies.length() as Float
      
      pattern_frequencies.filter(fn(pf) {
        let frequency_ratio = pf.1 as Float / avg_frequency
        frequency_ratio > threshold
      })
    }
  }
  
  // Detect anomalies with threshold of 1.5x average frequency
  let anomalies = detect_anomalies(pattern_frequencies, 1.5)
  assert_eq(anomalies.length(), 1)  // Only USER_LOGIN_SUCCESS should be anomalous
  assert_eq(anomalies[0], ("USER_LOGIN_SUCCESS", 3))
}

// Test 5: Log Correlation and Causality
test "log correlation and causality analysis" {
  // Define log entry
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    source: String,
    attributes: Array[(String, String)],
    trace_id: Option[String],
    span_id: Option[String]
  }
  
  // Define correlation rule
  type CorrelationRule = {
    rule_id: String,
    trigger_pattern: String,
    correlated_pattern: String,
    time_window: Int,  // seconds
    description: String
  }
  
  // Create sample log entries with trace context
  let log_entries = [
    {
      timestamp: 1640995200,
      level: "INFO",
      message: "HTTP request received",
      source: "api-gateway",
      attributes: [("method", "POST"), ("/api/login", "path")],
      trace_id: Some("trace-123"),
      span_id: Some("span-001")
    },
    {
      timestamp: 1640995205,
      level: "INFO",
      message: "Authenticating user",
      source: "auth-service",
      attributes: [("user_id", "12345")],
      trace_id: Some("trace-123"),
      span_id: Some("span-002")
    },
    {
      timestamp: 1640995210,
      level: "ERROR",
      message: "Database connection failed",
      source: "user-service",
      attributes: [("error", "timeout")],
      trace_id: Some("trace-123"),
      span_id: Some("span-003")
    },
    {
      timestamp: 1640995215,
      level: "ERROR",
      message: "User authentication failed",
      source: "auth-service",
      attributes: [("reason", "db_error")],
      trace_id: Some("trace-123"),
      span_id: Some("span-004")
    },
    {
      timestamp: 1640995220,
      level: "ERROR",
      message: "HTTP request failed",
      source: "api-gateway",
      attributes: [("status", "500")],
      trace_id: Some("trace-123"),
      span_id: Some("span-005")
    },
    {
      timestamp: 1640995230,
      level: "INFO",
      message: "HTTP request received",
      source: "api-gateway",
      attributes: [("method", "GET"), ("/api/users", "path")],
      trace_id: Some("trace-456"),
      span_id: Some("span-006")
    },
    {
      timestamp: 1640995235,
      level: "INFO",
      message: "User data retrieved",
      source: "user-service",
      attributes: [("user_count", "100")],
      trace_id: Some("trace-456"),
      span_id: Some("span-007")
    },
    {
      timestamp: 1640995240,
      level: "INFO",
      message: "HTTP request completed",
      source: "api-gateway",
      attributes: [("status", "200")],
      trace_id: Some("trace-456"),
      span_id: Some("span-008")
    }
  ]
  
  // Define correlation rules
  let correlation_rules = [
    {
      rule_id: "DB_ERROR_CAUSES_AUTH_FAILURE",
      trigger_pattern: "Database connection failed",
      correlated_pattern: "authentication failed",
      time_window: 30,
      description: "Database errors cause authentication failures"
    },
    {
      rule_id: "AUTH_FAILURE_CAUSES_HTTP_ERROR",
      trigger_pattern: "authentication failed",
      correlated_pattern: "HTTP request failed",
      time_window: 30,
      description: "Authentication failures cause HTTP errors"
    }
  ]
  
  // Group logs by trace ID
  let group_by_trace = fn(logs: Array[LogEntry]) {
    let mut trace_groups = []
    
    for log in logs {
      match log.trace_id {
        Some(trace_id) => {
          let existing = trace_groups.find_index(fn(tg) { tg.0 == trace_id })
          match existing {
            Some(index) => {
              trace_groups[index] = (trace_groups[index].0, trace_groups[index].1.push(log))
            }
            None => {
              trace_groups = trace_groups.push((trace_id, [log]))
            }
          }
        }
        None => {}
      }
    }
    
    trace_groups
  }
  
  // Test trace grouping
  let trace_groups = group_by_trace(log_entries)
  assert_eq(trace_groups.length(), 2)
  
  let trace_123 = trace_groups.find(fn(tg) { tg.0 == "trace-123" })
  match trace_123 {
    Some(group) => assert_eq(group.1.length(), 5)
    None => assert_true(false)
  }
  
  let trace_456 = trace_groups.find(fn(tg) { tg.0 == "trace-456" })
  match trace_456 {
    Some(group) => assert_eq(group.1.length(), 3)
    None => assert_true(false)
  }
  
  // Analyze causality within traces
  let analyze_causality = fn(trace_logs: Array[LogEntry], rules: Array[CorrelationRule]) {
    let correlations = []
    
    // Sort logs by timestamp
    let sorted_logs = trace_logs.sort(fn(a, b) {
      if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 }
    })
    
    // Check each rule
    for rule in rules {
      for i in 0..sorted_logs.length() {
        let trigger_log = sorted_logs[i]
        
        if trigger_log.message.contains(rule.trigger_pattern) {
          // Look for correlated logs within time window
          for j in (i + 1)..sorted_logs.length() {
            let correlated_log = sorted_logs[j]
            
            if correlated_log.timestamp - trigger_log.timestamp <= rule.time_window {
              if correlated_log.message.contains(rule.correlated_pattern) {
                correlations = correlations.push({
                  rule_id: rule.rule_id,
                  trigger_log: trigger_log,
                  correlated_log: correlated_log,
                  time_diff: correlated_log.timestamp - trigger_log.timestamp
                })
                break  // Only count first correlation per trigger
              }
            } else {
              break  // Outside time window
            }
          }
        }
      }
    }
    
    correlations
  }
  
  // Test causality analysis
  let trace_123_logs = match trace_123 {
    Some(group) => group.1
    None => []
  }
  
  let causality_results = analyze_causality(trace_123_logs, correlation_rules)
  assert_eq(causality_results.length(), 2)
  
  // Check first correlation (DB error -> Auth failure)
  assert_eq(causality_results[0].rule_id, "DB_ERROR_CAUSES_AUTH_FAILURE")
  assert_eq(causality_results[0].trigger_log.message, "Database connection failed")
  assert_eq(causality_results[0].correlated_log.message, "User authentication failed")
  assert_eq(causality_results[0].time_diff, 5)
  
  // Check second correlation (Auth failure -> HTTP error)
  assert_eq(causality_results[1].rule_id, "AUTH_FAILURE_CAUSES_HTTP_ERROR")
  assert_eq(causality_results[1].trigger_log.message, "User authentication failed")
  assert_eq(causality_results[1].correlated_log.message, "HTTP request failed")
  assert_eq(causality_results[1].time_diff, 5)
  
  // Test cross-trace correlation
  let cross_trace_correlation = fn(logs: Array[LogEntry], pattern: String, time_window: Int) {
    let pattern_logs = logs.filter(fn(log) { log.message.contains(pattern) })
    
    if pattern_logs.length() < 2 {
      []
    } else {
      let correlations = []
      
      for i in 0..pattern_logs.length() {
        for j in (i + 1)..pattern_logs.length() {
          let log1 = pattern_logs[i]
          let log2 = pattern_logs[j]
          
          let time_diff = (log1.timestamp - log2.timestamp).abs()
          
          if time_diff <= time_window && log1.trace_id != log2.trace_id {
            correlations = correlations.push({
              log1,
              log2,
              time_diff,
              same_trace: false
            })
          }
        }
      }
      
      correlations
    }
  }
  
  // Look for database errors across different traces
  let db_error_correlations = cross_trace_correlation(log_entries, "Database connection failed", 60)
  assert_eq(db_error_correlations.length(), 0)  // Only one DB error in our data
  
  // Look for authentication patterns across different traces
  let auth_correlations = cross_trace_correlation(log_entries, "auth", 60)
  assert_eq(auth_correlations.length(), 3)  // Multiple auth-related logs
}

// Test 6: Log Retention and Archival
test "log retention and archival policies" {
  // Define log entry
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    source: String,
    attributes: Array[(String, String)]
  }
  
  // Define retention policy
  type RetentionPolicy = {
    policy_id: String,
    level_rules: Array[(String, Int)],  // (level, retention_days)
    source_rules: Array[(String, Int)],  // (source, retention_days)
    default_retention_days: Int,
    archive_after_days: Int
  }
  
  // Create sample log entries with different timestamps
  let current_time = 1640995200  // Current time
  let day_in_seconds = 86400
  
  let log_entries = [
    { timestamp: current_time, level: "INFO", message: "Recent info log", source: "api", attributes: [] },
    { timestamp: current_time - (2 * day_in_seconds), level: "ERROR", message: "2-day-old error", source: "db", attributes: [] },
    { timestamp: current_time - (7 * day_in_seconds), level: "WARN", message: "7-day-old warning", source: "auth", attributes: [] },
    { timestamp: current_time - (15 * day_in_seconds), level: "INFO", message: "15-day-old info", source: "cache", attributes: [] },
    { timestamp: current_time - (30 * day_in_seconds), level: "ERROR", message: "30-day-old error", source: "api", attributes: [] },
    { timestamp: current_time - (60 * day_in_seconds), level: "INFO", message: "60-day-old info", source: "db", attributes: [] },
    { timestamp: current_time - (90 * day_in_seconds), level: "WARN", message: "90-day-old warning", source: "monitor", attributes: [] }
  ]
  
  // Create retention policy
  let retention_policy = {
    policy_id: "standard-policy",
    level_rules: [
      ("ERROR", 30),    // Keep errors for 30 days
      ("WARN", 14),     // Keep warnings for 14 days
      ("INFO", 7),      // Keep info for 7 days
      ("DEBUG", 3)      // Keep debug for 3 days
    ],
    source_rules: [
      ("api", 14),      // Keep api logs for 14 days
      ("auth", 30),     // Keep auth logs for 30 days
      ("db", 60)        // Keep db logs for 60 days
    ],
    default_retention_days: 7,
    archive_after_days: 30
  }
  
  // Calculate log age in days
  let get_log_age_days = fn(log: LogEntry, current_timestamp: Int) {
    (current_timestamp - log.timestamp) / day_in_seconds
  }
  
  // Get retention period for a log
  let get_retention_days = fn(log: LogEntry, policy: RetentionPolicy) {
    // Check level-specific rules first
    let level_rule = policy.level_rules.find(fn(rule) { rule.0 == log.level })
    match level_rule {
      Some((_, days)) => days
      None => {
        // Check source-specific rules
        let source_rule = policy.source_rules.find(fn(rule) { rule.0 == log.source })
        match source_rule {
          Some((_, days)) => days
          None => policy.default_retention_days
        }
      }
    }
  }
  
  // Test retention calculation
  let recent_log_age = get_log_age_days(log_entries[0], current_time)
  assert_eq(recent_log_age, 0)
  
  let old_log_age = get_log_age_days(log_entries[6], current_time)
  assert_eq(old_log_age, 90)
  
  let error_retention = get_retention_days(
    { timestamp: 0, level: "ERROR", message: "", source: "unknown", attributes: [] },
    retention_policy
  )
  assert_eq(error_retention, 30)
  
  let api_retention = get_retention_days(
    { timestamp: 0, level: "INFO", message: "", source: "api", attributes: [] },
    retention_policy
  )
  assert_eq(api_retention, 14)  // Source rule takes precedence over level rule
  
  let default_retention = get_retention_days(
    { timestamp: 0, level: "DEBUG", message: "", source: "unknown", attributes: [] },
    retention_policy
  )
  assert_eq(default_retention, 3)
  
  // Determine log disposition
  enum LogDisposition {
    Keep
    Archive
    Delete
  }
  
  let get_log_disposition = fn(log: LogEntry, policy: RetentionPolicy, current_timestamp: Int) {
    let age_days = get_log_age_days(log, current_timestamp)
    let retention_days = get_retention_days(log, policy)
    
    if age_days <= retention_days {
      LogDisposition::Keep
    } else if age_days <= policy.archive_after_days {
      LogDisposition::Archive
    } else {
      LogDisposition::Delete
    }
  }
  
  // Test log disposition
  let recent_disposition = get_log_disposition(log_entries[0], retention_policy, current_time)
  assert_eq(recent_disposition, LogDisposition::Keep)
  
  let old_error_disposition = get_log_disposition(log_entries[4], retention_policy, current_time)
  assert_eq(old_error_disposition, LogDisposition::Archive)  // 30 days old, matches archive threshold
  
  let very_old_disposition = get_log_disposition(log_entries[6], retention_policy, current_time)
  assert_eq(very_old_disposition, LogDisposition::Delete)  // 90 days old, past archive threshold
  
  // Apply retention policy to log set
  let apply_retention_policy = fn(logs: Array[LogEntry], policy: RetentionPolicy, current_timestamp: Int) {
    let mut keep_logs = []
    let mut archive_logs = []
    let mut delete_logs = []
    
    for log in logs {
      let disposition = get_log_disposition(log, policy, current_timestamp)
      
      match disposition {
        LogDisposition::Keep => keep_logs = keep_logs.push(log)
        LogDisposition::Archive => archive_logs = archive_logs.push(log)
        LogDisposition::Delete => delete_logs = delete_logs.push(log)
      }
    }
    
    {
      keep_logs,
      archive_logs,
      delete_logs,
      total_processed: logs.length()
    }
  }
  
  // Test retention policy application
  let retention_results = apply_retention_policy(log_entries, retention_policy, current_time)
  assert_eq(retention_results.total_processed, 7)
  
  // Should keep recent logs
  assert_eq(retention_results.keep_logs.length(), 3)
  
  // Should archive logs older than retention but within archive window
  assert_eq(retention_results.archive_logs.length(), 2)
  
  // Should delete very old logs
  assert_eq(retention_results.delete_logs.length(), 2)
  
  // Verify specific logs
  let kept_sources = retention_results.keep_logs.map(fn(log) { log.source })
  assert_true(kept_sources.contains("api"))
  assert_true(kept_sources.contains("db"))
  assert_true(kept_sources.contains("auth"))
  
  let archived_sources = retention_results.archive_logs.map(fn(log) { log.source })
  assert_true(archived_sources.contains("cache"))
  assert_true(archived_sources.contains("api"))
  
  let deleted_sources = retention_results.delete_logs.map(fn(log) { log.source })
  assert_true(deleted_sources.contains("db"))
  assert_true(deleted_sources.contains("monitor"))
}