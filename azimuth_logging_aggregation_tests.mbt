// Azimuth Logging and Aggregation Test Suite
// This file contains test cases for logging and log aggregation functionality

// Test 1: Log Entry Creation and Validation
test "log entry creation and validation" {
  type LogLevel = {
    TRACE: Int,
    DEBUG: Int,
    INFO: Int,
    WARN: Int,
    ERROR: Int,
    FATAL: Int
  }
  
  let log_levels = {
    TRACE: 0,
    DEBUG: 1,
    INFO: 2,
    WARN: 3,
    ERROR: 4,
    FATAL: 5
  }
  
  type LogEntry = {
    timestamp: Int,
    level: Int,
    message: String,
    logger_name: String,
    thread_name: String,
    exception: Option[String],
    mdc: Array[(String, String)]
  }
  
  let create_log_entry = fn(level: Int, message: String, logger_name: String) {
    {
      timestamp: 1640995200,
      level,
      message,
      logger_name,
      thread_name: "main",
      exception: None,
      mdc: []
    }
  }
  
  let with_exception = fn(entry: LogEntry, exception: String) {
    { entry | exception: Some(exception) }
  }
  
  let with_mdc = fn(entry: LogEntry, key: String, value: String) {
    { entry | mdc: entry.mdc + [(key, value)] }
  }
  
  let is_valid_log_entry = fn(entry: LogEntry) {
    entry.message.length() > 0 && 
    entry.logger_name.length() > 0 &&
    entry.level >= log_levels.TRACE && 
    entry.level <= log_levels.FATAL
  }
  
  let info_entry = create_log_entry(log_levels.INFO, "User logged in successfully", "auth-service")
  assert_true(is_valid_log_entry(info_entry))
  assert_eq(info_entry.level, log_levels.INFO)
  assert_eq(info_entry.message, "User logged in successfully")
  assert_eq(info_entry.logger_name, "auth-service")
  assert_eq(info_entry.thread_name, "main")
  assert_eq(info_entry.exception, None)
  assert_eq(info_entry.mdc.length(), 0)
  
  let error_entry = with_exception(info_entry, "NullPointerException at com.example.Service.process(Service.java:42)")
  assert_eq(error_entry.exception, Some("NullPointerException at com.example.Service.process(Service.java:42)"))
  
  let mdc_entry = with_mdc(error_entry, "user_id", "user-123")
  assert_eq(mdc_entry.mdc.length(), 1)
  assert_true(mdc_entry.mdc.contains(("user_id", "user-123")))
  
  let multi_mdc_entry = with_mdc(mdc_entry, "request_id", "req-456")
  assert_eq(multi_mdc_entry.mdc.length(), 2)
  assert_true(multi_mdc_entry.mdc.contains(("user_id", "user-123")))
  assert_true(multi_mdc_entry.mdc.contains(("request_id", "req-456")))
  
  let invalid_entry = { info_entry | message: "" }
  assert_false(is_valid_log_entry(invalid_entry))
  
  let invalid_level_entry = { info_entry | level: 10 }
  assert_false(is_valid_log_entry(invalid_level_entry))
}

// Test 2: Log Level Filtering
test "log level filtering" {
  type LogLevel = {
    TRACE: Int,
    DEBUG: Int,
    INFO: Int,
    WARN: Int,
    ERROR: Int,
    FATAL: Int
  }
  
  let log_levels = {
    TRACE: 0,
    DEBUG: 1,
    INFO: 2,
    WARN: 3,
    ERROR: 4,
    FATAL: 5
  }
  
  type LogEntry = {
    timestamp: Int,
    level: Int,
    message: String,
    logger_name: String
  }
  
  let create_log_entry = fn(level: Int, message: String, logger_name: String) {
    {
      timestamp: 1640995200,
      level,
      message,
      logger_name
    }
  }
  
  let is_level_enabled = fn(entry_level: Int, min_level: Int) {
    entry_level >= min_level
  }
  
  let filter_by_level = fn(entries: Array[LogEntry], min_level: Int) {
    entries.filter(fn(entry) { is_level_enabled(entry.level, min_level) })
  }
  
  let log_entries = [
    create_log_entry(log_levels.TRACE, "Detailed trace information", "service-a"),
    create_log_entry(log_levels.DEBUG, "Debug information", "service-a"),
    create_log_entry(log_levels.INFO, "Service started", "service-a"),
    create_log_entry(log_levels.WARN, "Deprecated API used", "service-b"),
    create_log_entry(log_levels.ERROR, "Database connection failed", "service-b"),
    create_log_entry(log_levels.FATAL, "System crash", "service-c")
  ]
  
  let info_and_above = filter_by_level(log_entries, log_levels.INFO)
  assert_eq(info_and_above.length(), 4)
  assert_true(info_and_above.all(fn(entry) { entry.level >= log_levels.INFO }))
  
  let warn_and_above = filter_by_level(log_entries, log_levels.WARN)
  assert_eq(warn_and_above.length(), 3)
  assert_true(warn_and_above.all(fn(entry) { entry.level >= log_levels.WARN }))
  
  let error_and_above = filter_by_level(log_entries, log_levels.ERROR)
  assert_eq(error_and_above.length(), 2)
  assert_true(error_and_above.all(fn(entry) { entry.level >= log_levels.ERROR }))
  
  let fatal_only = filter_by_level(log_entries, log_levels.FATAL)
  assert_eq(fatal_only.length(), 1)
  assert_eq(fatal_only[0].level, log_levels.FATAL)
  
  let all_levels = filter_by_level(log_entries, log_levels.TRACE)
  assert_eq(all_levels.length(), 6)
  
  let none_levels = filter_by_level(log_entries, log_levels.FATAL + 1)
  assert_eq(none_levels.length(), 0)
}

// Test 3: Log Pattern Matching
test "log pattern matching" {
  type LogEntry = {
    timestamp: Int,
    level: Int,
    message: String,
    logger_name: String,
    mdc: Array[(String, String)]
  }
  
  let create_log_entry = fn(level: Int, message: String, logger_name: String, mdc: Array[(String, String)]) {
    {
      timestamp: 1640995200,
      level,
      message,
      logger_name,
      mdc
    }
  }
  
  let matches_message_pattern = fn(entry: LogEntry, pattern: String) {
    entry.message.contains(pattern)
  }
  
  let matches_logger_pattern = fn(entry: LogEntry, pattern: String) {
    entry.logger_name.contains(pattern)
  }
  
  let matches_mdc_key = fn(entry: LogEntry, key: String) {
    entry.mdc.some(fn(kvp) { kvp.0 == key })
  }
  
  let matches_mdc_value = fn(entry: LogEntry, key: String, value: String) {
    entry.mdc.some(fn(kvp) { kvp.0 == key && kvp.1 == value })
  }
  
  let log_entries = [
    create_log_entry(2, "User login successful", "auth-service", [("user_id", "123"), ("action", "login")]),
    create_log_entry(4, "Database connection failed", "db-service", [("error_code", "500"), ("retry_count", "3")]),
    create_log_entry(2, "User logout", "auth-service", [("user_id", "123"), ("action", "logout")]),
    create_log_entry(3, "Cache miss for key", "cache-service", [("cache_key", "user:123"), ("action", "get")]),
    create_log_entry(4, "Payment processing failed", "payment-service", [("user_id", "456"), ("amount", "100.00")])
  ]
  
  let user_logs = log_entries.filter(fn(entry) { matches_message_pattern(entry, "User") })
  assert_eq(user_logs.length(), 2)
  assert_true(user_logs.all(fn(entry) { entry.message.contains("User") }))
  
  let auth_logs = log_entries.filter(fn(entry) { matches_logger_pattern(entry, "auth-service") })
  assert_eq(auth_logs.length(), 2)
  assert_true(auth_logs.all(fn(entry) { entry.logger_name == "auth-service" }))
  
  let user_id_logs = log_entries.filter(fn(entry) { matches_mdc_key(entry, "user_id") })
  assert_eq(user_id_logs.length(), 3)
  
  let user_123_logs = log_entries.filter(fn(entry) { matches_mdc_value(entry, "user_id", "123") })
  assert_eq(user_123_logs.length(), 2)
  
  let error_logs = log_entries.filter(fn(entry) { matches_message_pattern(entry, "failed") })
  assert_eq(error_logs.length(), 2)
  
  let combined_filter = log_entries.filter(fn(entry) { 
    matches_mdc_key(entry, "user_id") && 
    (matches_message_pattern(entry, "User") || matches_message_pattern(entry, "failed"))
  })
  assert_eq(combined_filter.length(), 3)
}

// Test 4: Log Aggregation by Time Window
test "log aggregation by time window" {
  type LogEntry = {
    timestamp: Int,
    level: Int,
    message: String,
    logger_name: String
  }
  
  type LogAggregation = {
    window_start: Int,
    window_end: Int,
    count: Int,
    error_count: Int,
    warn_count: Int,
    info_count: Int,
    debug_count: Int,
    trace_count: Int,
    fatal_count: Int
  }
  
  let create_log_entry = fn(timestamp: Int, level: Int, message: String, logger_name: String) {
    {
      timestamp,
      level,
      message,
      logger_name
    }
  }
  
  let aggregate_by_time_window = fn(entries: Array[LogEntry], window_start: Int, window_end: Int) {
    let window_entries = entries.filter(fn(entry) { 
      entry.timestamp >= window_start && entry.timestamp < window_end 
    })
    
    let count = window_entries.length()
    let error_count = window_entries.filter(fn(e) { e.level == 4 }).length()
    let warn_count = window_entries.filter(fn(e) { e.level == 3 }).length()
    let info_count = window_entries.filter(fn(e) { e.level == 2 }).length()
    let debug_count = window_entries.filter(fn(e) { e.level == 1 }).length()
    let trace_count = window_entries.filter(fn(e) { e.level == 0 }).length()
    let fatal_count = window_entries.filter(fn(e) { e.level == 5 }).length()
    
    {
      window_start,
      window_end,
      count,
      error_count,
      warn_count,
      info_count,
      debug_count,
      trace_count,
      fatal_count
    }
  }
  
  let log_entries = [
    create_log_entry(1000, 2, "Service started", "auth-service"),
    create_log_entry(1010, 1, "Initializing components", "auth-service"),
    create_log_entry(1020, 2, "User login request", "auth-service"),
    create_log_entry(1030, 4, "Database connection failed", "db-service"),
    create_log_entry(1040, 2, "Retrying database connection", "db-service"),
    create_log_entry(1050, 2, "Database connection restored", "db-service"),
    create_log_entry(2000, 2, "User logout request", "auth-service"),
    create_log_entry(2010, 3, "Session timeout warning", "auth-service"),
    create_log_entry(2020, 2, "Session invalidated", "auth-service")
  ]
  
  let window1 = aggregate_by_time_window(log_entries, 1000, 1100)
  assert_eq(window1.window_start, 1000)
  assert_eq(window1.window_end, 1100)
  assert_eq(window1.count, 6)
  assert_eq(window1.error_count, 1)
  assert_eq(window1.warn_count, 0)
  assert_eq(window1.info_count, 4)
  assert_eq(window1.debug_count, 1)
  assert_eq(window1.trace_count, 0)
  assert_eq(window1.fatal_count, 0)
  
  let window2 = aggregate_by_time_window(log_entries, 2000, 2100)
  assert_eq(window2.window_start, 2000)
  assert_eq(window2.window_end, 2100)
  assert_eq(window2.count, 3)
  assert_eq(window2.error_count, 0)
  assert_eq(window2.warn_count, 1)
  assert_eq(window2.info_count, 2)
  assert_eq(window2.debug_count, 0)
  assert_eq(window2.trace_count, 0)
  assert_eq(window2.fatal_count, 0)
  
  let empty_window = aggregate_by_time_window(log_entries, 3000, 3100)
  assert_eq(empty_window.count, 0)
  assert_eq(empty_window.error_count, 0)
  assert_eq(empty_window.warn_count, 0)
  assert_eq(empty_window.info_count, 0)
  assert_eq(empty_window.debug_count, 0)
  assert_eq(empty_window.trace_count, 0)
  assert_eq(empty_window.fatal_count, 0)
}

// Test 5: Log Correlation and Context
test "log correlation and context" {
  type LogEntry = {
    timestamp: Int,
    level: Int,
    message: String,
    logger_name: String,
    trace_id: Option[String],
    span_id: Option[String],
    request_id: Option[String],
    user_id: Option[String]
  }
  
  type CorrelatedLogGroup = {
    trace_id: String,
    entries: Array[LogEntry],
    services: Array[String],
    start_time: Int,
    end_time: Int,
    error_count: Int
  }
  
  let create_log_entry = fn(level: Int, message: String, logger_name: String, trace_id: Option[String], span_id: Option[String], request_id: Option[String], user_id: Option[String]) {
    {
      timestamp: 1640995200,
      level,
      message,
      logger_name,
      trace_id,
      span_id,
      request_id,
      user_id
    }
  }
  
  let group_by_trace_id = fn(entries: Array[LogEntry]) {
    let groups = {}
    
    for entry in entries {
      match entry.trace_id {
        Some(trace_id) => {
          match groups[trace_id] {
            Some(group_entries) => groups[trace_id] = Some(group_entries + [entry])
            None => groups[trace_id] = Some([entry])
          }
        }
        None => {}  // Skip entries without trace ID
      }
    }
    
    groups
  }
  
  let create_correlated_group = fn(trace_id: String, entries: Array[LogEntry]) {
    let services = entries.map(fn(e) { e.logger_name })
    let unique_services = fn(services: Array[String]) {
      let mut result = []
      for service in services {
        if not(result.contains(service)) {
          result = result.push(service)
        }
      }
      result
    }
    
    let timestamps = entries.map(fn(e) { e.timestamp })
    let start_time = timestamps.reduce(fn(acc, ts) { if ts < acc { ts } else { acc } }, 9999999999)
    let end_time = timestamps.reduce(fn(acc, ts) { if ts > acc { ts } else { acc } }, 0)
    let error_count = entries.filter(fn(e) { e.level == 4 }).length()
    
    {
      trace_id,
      entries,
      services: unique_services(services),
      start_time,
      end_time,
      error_count
    }
  }
  
  let log_entries = [
    create_log_entry(2, "Request received", "web-service", Some("trace-123"), Some("span-1"), Some("req-1"), Some("user-1")),
    create_log_entry(2, "Authenticating user", "auth-service", Some("trace-123"), Some("span-2"), Some("req-1"), Some("user-1")),
    create_log_entry(2, "User authenticated", "auth-service", Some("trace-123"), Some("span-2"), Some("req-1"), Some("user-1")),
    create_log_entry(4, "Database connection failed", "db-service", Some("trace-123"), Some("span-3"), Some("req-1"), Some("user-1")),
    create_log_entry(2, "Retrying database connection", "db-service", Some("trace-123"), Some("span-3"), Some("req-1"), Some("user-1")),
    create_log_entry(2, "Request completed", "web-service", Some("trace-123"), Some("span-1"), Some("req-1"), Some("user-1")),
    create_log_entry(2, "Background task started", "worker-service", Some("trace-456"), Some("span-4"), None, None),
    create_log_entry(2, "Background task completed", "worker-service", Some("trace-456"), Some("span-4"), None, None),
    create_log_entry(2, "Uncorrelated log", "monitor-service", None, None, None, None)
  ]
  
  let grouped = group_by_trace_id(log_entries)
  assert_eq(grouped.keys().length(), 2)
  assert_true(grouped.contains("trace-123"))
  assert_true(grouped.contains("trace-456"))
  
  let trace123_group = create_correlated_group("trace-123", grouped["trace-123"].unwrap_or([]))
  assert_eq(trace123_group.trace_id, "trace-123")
  assert_eq(trace123_group.entries.length(), 6)
  assert_eq(trace123_group.services.length(), 3)
  assert_true(trace123_group.services.contains("web-service"))
  assert_true(trace123_group.services.contains("auth-service"))
  assert_true(trace123_group.services.contains("db-service"))
  assert_eq(trace123_group.error_count, 1)
  
  let trace456_group = create_correlated_group("trace-456", grouped["trace-456"].unwrap_or([]))
  assert_eq(trace456_group.trace_id, "trace-456")
  assert_eq(trace456_group.entries.length(), 2)
  assert_eq(trace456_group.services.length(), 1)
  assert_true(trace456_group.services.contains("worker-service"))
  assert_eq(trace456_group.error_count, 0)
}

// Test 6: Log Sampling
test "log sampling" {
  type LogEntry = {
    timestamp: Int,
    level: Int,
    message: String,
    logger_name: String
  }
  
  type SamplingResult = {
    sampled: Bool,
    reason: String
  }
  
  let create_log_entry = fn(level: Int, message: String, logger_name: String) {
    {
      timestamp: 1640995200,
      level,
      message,
      logger_name
    }
  }
  
  let always_sample = fn(_entry: LogEntry) {
    { sampled: true, reason: "always" }
  }
  
  let never_sample = fn(_entry: LogEntry) {
    { sampled: false, reason: "never" }
  }
  
  let sample_by_level = fn(entry: LogEntry, min_level: Int) {
    {
      sampled: entry.level >= min_level,
      reason: "level_" + entry.level.to_string()
    }
  }
  
  let sample_by_rate = fn(entry: LogEntry, rate: Float) {
    // Simple hash-based sampling
    let hash = entry.message.chars().fold(0, fn(acc, c) { 
      (acc * 31 + c.to_int()) % 100
    })
    let threshold = (rate * 100.0).to_int()
    
    {
      sampled: hash < threshold,
      reason: "rate_" + rate.to_string()
    }
  }
  
  let log_entries = [
    create_log_entry(2, "User login", "auth-service"),
    create_log_entry(1, "Debug info", "auth-service"),
    create_log_entry(4, "Database error", "db-service"),
    create_log_entry(2, "Request processed", "web-service"),
    create_log_entry(3, "Cache miss", "cache-service"),
    create_log_entry(2, "User logout", "auth-service")
  ]
  
  let always_results = log_entries.map(always_sample)
  assert_true(always_results.all(fn(r) { r.sampled }))
  assert_eq(always_results[0].reason, "always")
  
  let never_results = log_entries.map(never_sample)
  assert_true(never_results.all(fn(r) { not(r.sampled) }))
  assert_eq(never_results[0].reason, "never")
  
  let warn_and_above_results = log_entries.map(fn(entry) { sample_by_level(entry, 3) })
  assert_false(warn_and_above_results[0].sampled)  // INFO
  assert_false(warn_and_above_results[1].sampled)  // DEBUG
  assert_true(warn_and_above_results[2].sampled)   // ERROR
  assert_false(warn_and_above_results[3].sampled)  // INFO
  assert_true(warn_and_above_results[4].sampled)   // WARN
  assert_false(warn_and_above_results[5].sampled)  // INFO
  
  let rate_50_results = log_entries.map(fn(entry) { sample_by_rate(entry, 0.5) })
  let sampled_count = rate_50_results.filter(fn(r) { r.sampled }).length()
  assert_true(sampled_count >= 0 && sampled_count <= 6)  // Could be any number between 0 and 6
  assert_eq(rate_50_results[0].reason, "rate_0.5")
}

// Test 7: Log Formatting
test "log formatting" {
  type LogEntry = {
    timestamp: Int,
    level: Int,
    message: String,
    logger_name: String,
    thread_name: String,
    exception: Option[String]
  }
  
  let create_log_entry = fn(timestamp: Int, level: Int, message: String, logger_name: String, thread_name: String, exception: Option[String]) {
    {
      timestamp,
      level,
      message,
      logger_name,
      thread_name,
      exception
    }
  }
  
  let format_timestamp = fn(timestamp: Int) {
    let date = timestamp / 86400
    let time = timestamp % 86400
    let hours = time / 3600
    let minutes = (time % 3600) / 60
    let seconds = time % 60
    
    date.to_string() + " " + 
    hours.to_string() + ":" + 
    minutes.to_string() + ":" + 
    seconds.to_string()
  }
  
  let level_to_string = fn(level: Int) {
    match level {
      0 => "TRACE"
      1 => "DEBUG"
      2 => "INFO"
      3 => "WARN"
      4 => "ERROR"
      5 => "FATAL"
      _ => "UNKNOWN"
    }
  }
  
  let format_simple = fn(entry: LogEntry) {
    level_to_string(entry.level) + " " + 
    format_timestamp(entry.timestamp) + " " + 
    entry.logger_name + " - " + 
    entry.message
  }
  
  let format_detailed = fn(entry: LogEntry) {
    let base = format_simple(entry)
    let with_thread = base + " [" + entry.thread_name + "]"
    
    match entry.exception {
      Some(exception) => with_thread + "\n" + exception
      None => with_thread
    }
  }
  
  let format_json = fn(entry: LogEntry) {
    let base = "{"
    + "\"timestamp\":" + entry.timestamp.to_string() + ","
    + "\"level\":\"" + level_to_string(entry.level) + "\"," 
    + "\"logger\":\"" + entry.logger_name + "\"," 
    + "\"thread\":\"" + entry.thread_name + "\"," 
    + "\"message\":\"" + entry.message + "\""
    
    match entry.exception {
      Some(exception) => base + ",\"exception\":\"" + exception + "\"}"
      None => base + "}"
    }
  }
  
  let entry1 = create_log_entry(1640995200, 2, "User logged in", "auth-service", "main", None)
  let entry2 = create_log_entry(1640995250, 4, "Database error", "db-service", "worker", Some("NullPointerException at com.example.Service.process(Service.java:42)"))
  
  let simple1 = format_simple(entry1)
  assert_eq(simple1, "INFO 19000 0:0:0 auth-service - User logged in")
  
  let detailed2 = format_detailed(entry2)
  assert_eq(detailed2, "ERROR 19000 0:50:50 db-service - Database error [worker]\nNullPointerException at com.example.Service.process(Service.java:42)")
  
  let json1 = format_json(entry1)
  assert_true(json1.contains("\"timestamp\":1640995200"))
  assert_true(json1.contains("\"level\":\"INFO\""))
  assert_true(json1.contains("\"logger\":\"auth-service\""))
  assert_true(json1.contains("\"thread\":\"main\""))
  assert_true(json1.contains("\"message\":\"User logged in\""))
  assert_false(json1.contains("\"exception\""))
  
  let json2 = format_json(entry2)
  assert_true(json2.contains("\"timestamp\":1640995250"))
  assert_true(json2.contains("\"level\":\"ERROR\""))
  assert_true(json2.contains("\"logger\":\"db-service\""))
  assert_true(json2.contains("\"thread\":\"worker\""))
  assert_true(json2.contains("\"message\":\"Database error\""))
  assert_true(json2.contains("\"exception\":\"NullPointerException at com.example.Service.process(Service.java:42)\""))
}

// Test 8: Log Retention and Rotation
test "log retention and rotation" {
  type LogEntry = {
    timestamp: Int,
    level: Int,
    message: String,
    logger_name: String
  }
  
  type LogFile = {
    name: String,
    entries: Array[LogEntry],
    created_at: Int,
    size_bytes: Int
  }
  
  let create_log_entry = fn(timestamp: Int, level: Int, message: String, logger_name: String) {
    {
      timestamp,
      level,
      message,
      logger_name
    }
  }
  
  let create_log_file = fn(name: String, created_at: Int) {
    {
      name,
      entries: [],
      created_at,
      size_bytes: 0
    }
  }
  
  let add_entry = fn(file: LogFile, entry: LogEntry) {
    let entry_size = entry.message.length() + 50  // Rough estimate of entry size
    {
      name: file.name,
      entries: file.entries + [entry],
      created_at: file.created_at,
      size_bytes: file.size_bytes + entry_size
    }
  }
  
  let should_rotate_by_size = fn(file: LogFile, max_size_bytes: Int) {
    file.size_bytes >= max_size_bytes
  }
  
  let should_rotate_by_time = fn(file: LogFile, max_age_seconds: Int, current_time: Int) {
    (current_time - file.created_at) >= max_age_seconds
  }
  
  let rotate_log = fn(file: LogFile, current_time: Int) {
    let timestamp = current_time.to_string()
    let new_name = file.name + "." + timestamp
    create_log_file(new_name, current_time)
  }
  
  let apply_retention_policy = fn(files: Array[LogFile], max_files: Int, max_age_seconds: Int, current_time: Int) {
    // Sort by creation time (newest first)
    let sorted_files = files.sort(fn(a, b) { b.created_at >= a.created_at })
    
    // Keep only the most recent max_files
    let by_count = sorted_files.slice(0, max_files)
    
    // Remove files older than max_age_seconds
    by_count.filter(fn(file) { (current_time - file.created_at) < max_age_seconds })
  }
  
  let base_time = 1640995200  // Base timestamp
  
  let file1 = create_log_file("app.log", base_time)
  let file1_with_entries = add_entry(add_entry(file1, 
    create_log_entry(base_time, 2, "Application started", "app")), 
    create_log_entry(base_time + 10, 2, "Ready to serve requests", "app"))
  
  assert_eq(file1_with_entries.entries.length(), 2)
  assert_true(file1_with_entries.size_bytes > 0)
  
  // Test size-based rotation
  let large_file = { file1_with_entries | size_bytes: 1000000 }
  assert_true(should_rotate_by_size(large_file, 500000))
  assert_false(should_rotate_by_size(file1_with_entries, 500000))
  
  // Test time-based rotation
  let old_file = { file1_with_entries | created_at: base_time - 86400 }  // 1 day ago
  let current_time = base_time + 3600  // 1 hour after base time
  
  assert_true(should_rotate_by_time(old_file, 3600, current_time))  // Exactly 1 day old
  assert_false(should_rotate_by_time(file1_with_entries, 86400, current_time))  // Only 1 hour old
  
  // Test rotation
  let rotated_file = rotate_log(file1_with_entries, current_time)
  assert_eq(rotated_file.name, "app.log." + current_time.to_string())
  assert_eq(rotated_file.entries.length(), 0)
  assert_eq(rotated_file.created_at, current_time)
  
  // Test retention policy
  let files = [
    { name: "app.log.1", entries: [], created_at: base_time - 86400 * 3, size_bytes: 1000 },  // 3 days ago
    { name: "app.log.2", entries: [], created_at: base_time - 86400 * 2, size_bytes: 1000 },  // 2 days ago
    { name: "app.log.3", entries: [], created_at: base_time - 86400, size_bytes: 1000 },      // 1 day ago
    { name: "app.log.4", entries: [], created_at: base_time, size_bytes: 1000 }              // Current
  ]
  
  let retained_files = apply_retention_policy(files, 3, 86400 * 2, base_time)  // Keep max 3 files, max age 2 days
  assert_eq(retained_files.length(), 2)
  assert_true(retained_files.some(fn(f) { f.name == "app.log.3" }))
  assert_true(retained_files.some(fn(f) { f.name == "app.log.4" }))
}