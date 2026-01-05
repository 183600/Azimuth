// Azimuth Data Validation Tests
// This file contains comprehensive test cases for data validation scenarios in the Azimuth telemetry system

// Test 1: Input Validation for Telemetry Data
test "telemetry data input validation" {
  // Define validation rules
  let validate_span_name = fn(name: String) {
    let length = name.length()
    length >= 1 and length <= 128 and name != ""
  }
  
  let validate_trace_id = fn(trace_id: String) {
    let length = trace_id.length()
    length == 16 or length == 32  // 8-byte or 16-byte hex strings
  }
  
  let validate_metric_value = fn(value: Float) {
    value >= 0.0 and value <= 1000000.0  // Reasonable range for metrics
  }
  
  // Test valid inputs
  assert_true(validate_span_name("database_query"))
  assert_true(validate_span_name("a"))  // Minimum length
  assert_true(validate_span_name("a".repeat(128)))  // Maximum length
  
  assert_true(validate_trace_id("1234567890abcdef"))  // 16 chars
  assert_true(validate_trace_id("1234567890abcdef1234567890abcdef"))  // 32 chars
  
  assert_true(validate_metric_value(0.0))
  assert_true(validate_metric_value(500.5))
  assert_true(validate_metric_value(1000000.0))
  
  // Test invalid inputs
  assert_false(validate_span_name(""))  // Empty string
  assert_false(validate_span_name("a".repeat(129)))  // Too long
  
  assert_false(validate_trace_id("short"))  // Too short
  assert_false(validate_trace_id("1234567890abcde"))  // 15 chars
  assert_false(validate_trace_id("1234567890abcdef1234567890abcde"))  // 31 chars
  assert_false(validate_trace_id("1234567890abcde!"))  // Invalid character
  
  assert_false(validate_metric_value(-1.0))
  assert_false(validate_metric_value(1000000.1))
}

// Test 2: Data Type Conversion and Validation
test "data type conversion and validation" {
  // String to integer conversion with validation
  let safe_string_to_int = fn(s: String) {
    let mut result = 0
    let mut valid = true
    let chars = s.to_char_array()
    
    for i in 0..chars.length() {
      let c = chars[i]
      if c >= '0' and c <= '9' {
        result = result * 10 + (c.to_int() - '0'.to_int())
      } else {
        valid = false
        break
      }
    }
    
    if valid { Some(result) } else { None }
  }
  
  // Test valid conversions
  assert_eq(safe_string_to_int("0"), Some(0))
  assert_eq(safe_string_to_int("42"), Some(42))
  assert_eq(safe_string_to_int("123456789"), Some(123456789))
  
  // Test invalid conversions
  assert_eq(safe_string_to_int(""), None)
  assert_eq(safe_string_to_int("-1"), None)  // Negative not supported
  assert_eq(safe_string_to_int("12.34"), None)  // Decimal not supported
  assert_eq(safe_string_to_int("abc"), None)  // Non-numeric
  assert_eq(safe_string_to_int("12a34"), None)  // Mixed content
  
  // Float to string conversion with precision control
  let float_to_string = fn(value: Float, precision: Int) {
    let scaled = value * (10.0 ^ precision.to_float())
    let rounded = scaled.round()
    let int_part = (rounded / (10.0 ^ precision.to_float())).to_int()
    let decimal_part = (rounded % (10.0 ^ precision.to_float())).to_int()
    
    int_part.to_string() + "." + decimal_part.to_string()
  }
  
  assert_eq(float_to_string(3.14159, 2), "3.14")
  assert_eq(float_to_string(2.5, 1), "2.5")
  assert_eq(float_to_string(1.0, 3), "1.0")
  
  // Boolean string conversion
  let string_to_bool = fn(s: String) {
    match s.to_lowercase() {
      "true" => Some(true),
      "false" => Some(false),
      "1" => Some(true),
      "0" => Some(false),
      "yes" => Some(true),
      "no" => Some(false),
      _ => None
    }
  }
  
  assert_eq(string_to_bool("true"), Some(true))
  assert_eq(string_to_bool("TRUE"), Some(true))  // Case insensitive
  assert_eq(string_to_bool("1"), Some(true))
  assert_eq(string_to_bool("yes"), Some(true))
  
  assert_eq(string_to_bool("false"), Some(false))
  assert_eq(string_to_bool("0"), Some(false))
  assert_eq(string_to_bool("no"), Some(false))
  
  assert_eq(string_to_bool("maybe"), None)
  assert_eq(string_to_bool(""), None)
}

// Test 3: Range and Boundary Validation
test "range and boundary validation" {
  // Numeric range validation
  let validate_range = fn(value: Int, min: Int, max: Int) {
    value >= min and value <= max
  }
  
  // Test range validation
  assert_true(validate_range(5, 1, 10))
  assert_true(validate_range(1, 1, 10))  // At minimum
  assert_true(validate_range(10, 1, 10))  // At maximum
  
  assert_false(validate_range(0, 1, 10))  // Below minimum
  assert_false(validate_range(11, 1, 10))  // Above maximum
  
  // Date range validation
  let validate_date_range = fn(start: Int, end: Int, max_duration: Int) {
    let duration = end - start
    duration >= 0 and duration <= max_duration
  }
  
  let base_time = 1640995200  // 2022-01-01 00:00:00 UTC
  let one_day = 86400  // Seconds in a day
  let one_week = 604800  // Seconds in a week
  
  assert_true(validate_date_range(base_time, base_time + one_day, one_week))
  assert_true(validate_date_range(base_time, base_time + one_week, one_week))  // At maximum
  
  assert_false(validate_date_range(base_time + one_day, base_time, one_week))  // Negative duration
  assert_false(validate_date_range(base_time, base_time + one_week + 1, one_week))  // Exceeds maximum
  
  // Array length validation
  let validate_array_length = fn(arr: Array[String], min: Int, max: Int) {
    let length = arr.length()
    length >= min and length <= max
  }
  
  let empty_array = []
  let small_array = ["a", "b", "c"]
  let large_array = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
  
  assert_true(validate_array_length(empty_array, 0, 10))
  assert_true(validate_array_length(small_array, 1, 10))
  assert_true(validate_array_length(large_array, 5, 15))
  
  assert_false(validate_array_length(empty_array, 1, 10))  // Below minimum
  assert_false(validate_array_length(large_array, 1, 5))  // Above maximum
}

// Test 4: Format Validation
test "format validation" {
  // Email format validation
  let validate_email = fn(email: String) {
    let parts = email.split("@")
    if parts.length() != 2 { return false }
    
    let local = parts[0]
    let domain = parts[1]
    
    // Basic checks
    local.length() > 0 and domain.length() > 0 and
    domain.contains(".") and not(email.contains(" "))
  }
  
  assert_true(validate_email("user@example.com"))
  assert_true(validate_email("test.email+tag@domain.co.uk"))
  
  assert_false(validate_email("invalid-email"))  // Missing @
  assert_false(validate_email("@domain.com"))  // Empty local part
  assert_false(validate_email("user@"))  // Empty domain
  assert_false(validate_email("user@domain"))  // Domain missing dot
  assert_false(validate_email("user name@domain.com"))  // Contains space
  
  // URL format validation
  let validate_url = fn(url: String) {
    let protocols = ["http://", "https://", "ftp://"]
    let has_valid_protocol = protocols.any(fn(p) { url.starts_with(p) })
    
    if not(has_valid_protocol) { return false }
    
    let without_protocol = protocols.reduce(fn(acc, p) {
      if acc.starts_with(p) { acc.substring(p.length(), acc.length() - p.length()) } else { acc }
    }, url)
    
    without_protocol.length() > 0 and without_protocol.contains(".")
  }
  
  assert_true(validate_url("http://example.com"))
  assert_true(validate_url("https://api.example.com/v1"))
  assert_true(validate_url("ftp://files.example.org"))
  
  assert_false(validate_url("example.com"))  // Missing protocol
  assert_false(validate_url("http://"))  // Missing domain
  assert_false(validate_url("http://localhost"))  // No dot in domain
  
  // IP address validation
  let validate_ip = fn(ip: String) {
    let parts = ip.split(".")
    if parts.length() != 4 { return false }
    
    parts.all(fn(part) {
      match safe_string_to_int(part) {
        Some(num) => num >= 0 and num <= 255
        None => false
      }
    })
  }
  
  assert_true(validate_ip("192.168.1.1"))
  assert_true(validate_ip("0.0.0.0"))
  assert_true(validate_ip("255.255.255.255"))
  assert_true(validate_ip("10.0.0.1"))
  
  assert_false(validate_ip("192.168.1"))  // Too few parts
  assert_false(validate_ip("192.168.1.1.1"))  // Too many parts
  assert_false(validate_ip("256.168.1.1"))  // Octet too large
  assert_false(validate_ip("192.168.-1.1"))  // Negative octet
  assert_false(validate_ip("192.168.a.1"))  // Non-numeric octet
}

// Test 5: Business Rule Validation
test "business rule validation" {
  // Service dependency validation
  let validate_service_dependencies = fn(services: Array[String], dependencies: Array[(String, String)]) {
    // Check that all dependencies reference existing services
    dependencies.all(fn(dep) {
      let (service, depends_on) = dep
      services.contains(service) and services.contains(depends_on)
    })
  }
  
  let services = ["auth", "database", "cache", "api"]
  let valid_deps = [("api", "auth"), ("api", "database"), ("auth", "database")]
  let invalid_deps = [("api", "auth"), ("api", "external_service")]  // external_service doesn't exist
  
  assert_true(validate_service_dependencies(services, valid_deps))
  assert_false(validate_service_dependencies(services, invalid_deps))
  
  // Circular dependency detection
  let has_circular_dependency = fn(services: Array[String], dependencies: Array[(String, String)]) {
    let mut visited = []
    let mut recursion_stack = []
    
    let visit = fn(service: String) {
      if recursion_stack.contains(service) {
        return true  // Circular dependency found
      }
      
      if visited.contains(service) {
        return false  // Already processed
      }
      
      recursion_stack = recursion_stack.push(service)
      
      let mut has_cycle = false
      for (from, to) in dependencies {
        if from == service {
          if visit(to) {
            has_cycle = true
          }
        }
      }
      
      recursion_stack = recursion_stack.filter(fn(s) { s != service })
      visited = visited.push(service)
      
      has_cycle
    }
    
    services.any(visit)
  }
  
  let no_cycle_deps = [("api", "auth"), ("auth", "database")]
  let cycle_deps = [("api", "auth"), ("auth", "api")]  // Circular
  
  assert_false(has_circular_dependency(services, no_cycle_deps))
  assert_true(has_circular_dependency(services, cycle_deps))
  
  // Resource quota validation
  let validate_resource_quotas = fn(resources: Array[(String, Int)>, quotas: Array[(String, Int)>) {
    resources.all(fn(resource) {
      let (name, usage) = resource
      match quotas.find(fn(q) { q.0 == name }) {
        Some((_, limit)) => usage <= limit
        None => false  // No quota defined
      }
    })
  }
  
  let quotas = [("cpu", 1000), ("memory", 4096), ("disk", 10240)]
  let valid_usage = [("cpu", 500), ("memory", 2048), ("disk", 5120)]
  let invalid_usage = [("cpu", 1500), ("memory", 2048), ("disk", 5120)]  // CPU exceeds quota
  
  assert_true(validate_resource_quotas(valid_usage, quotas))
  assert_false(validate_resource_quotas(invalid_usage, quotas))
}

// Test 6: Data Consistency Validation
test "data consistency validation" {
  // Referential integrity validation
  let validate_referential_integrity = fn(
    spans: Array[(String, String)>,  // (span_id, trace_id)
    parent_refs: Array<(String, String)>  // (span_id, parent_span_id)
  ) {
    parent_refs.all(fn(ref) {
      let (span_id, parent_id) = ref
      
      // Check that span exists
      let span_exists = spans.any(fn(s) { s.0 == span_id })
      if not(span_exists) { return false }
      
      // Check that parent exists (if not root)
      if parent_id != "" {
        spans.any(fn(s) { s.0 == parent_id })
      } else {
        true  // Root span, no parent required
      }
    })
  }
  
  let spans = [
    ("span-1", "trace-1"),
    ("span-2", "trace-1"),
    ("span-3", "trace-1"),
    ("span-4", "trace-2")
  ]
  
  let valid_refs = [
    ("span-2", "span-1"),
    ("span-3", "span-2"),
    ("span-4", "")  // Root span
  ]
  
  let invalid_refs = [
    ("span-2", "span-1"),
    ("span-3", "span-99"),  // Non-existent parent
    ("span-4", "")
  ]
  
  assert_true(validate_referential_integrity(spans, valid_refs))
  assert_false(validate_referential_integrity(spans, invalid_refs))
  
  // Temporal consistency validation
  let validate_temporal_consistency = fn(events: Array[(String, Int)>) {
    // Events should be in chronological order
    let events_sorted = events.sort_by(fn(e) { e.1 })
    events == events_sorted
  }
  
  let valid_events = [
    ("start", 1000),
    ("process", 1010),
    ("complete", 1020)
  ]
  
  let invalid_events = [
    ("start", 1000),
    ("complete", 1020),
    ("process", 1010)  // Out of order
  ]
  
  assert_true(validate_temporal_consistency(valid_events))
  assert_false(validate_temporal_consistency(invalid_events))
  
  // Statistical consistency validation
  let validate_statistical_consistency = fn(
    metrics: Array[(String, Float)>,
    aggregates: Array[(String, Float)>
  ) {
    // Check that aggregates are consistent with individual metrics
    let total_sum = metrics.reduce(fn(acc, m) { acc + m.1 }, 0.0)
    let count = metrics.length().to_float()
    let avg = total_sum / count
    
    match aggregates.find(fn(a) { a.0 == "sum" }) {
      Some((_, sum)) => sum == total_sum
      None => false
    } and match aggregates.find(fn(a) { a.0 == "avg" }) {
      Some((_, avg_val)) => avg_val == avg
      None => false
    }
  }
  
  let metrics = [("metric1", 10.0), ("metric2", 20.0), ("metric3", 30.0)]
  let valid_aggregates = [("sum", 60.0), ("avg", 20.0)]
  let invalid_aggregates = [("sum", 60.0), ("avg", 25.0)]  // Wrong average
  
  assert_true(validate_statistical_consistency(metrics, valid_aggregates))
  assert_false(validate_statistical_consistency(metrics, invalid_aggregates))
}

// Test 7: Security Validation
test "security validation" {
  // Input sanitization
  let sanitize_string = fn(input: String) {
    let mut result = ""
    let chars = input.to_char_array()
    
    for i in 0..chars.length() {
      let c = chars[i]
      // Allow only alphanumeric and basic punctuation
      if (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or 
         (c >= '0' and c <= '9') or c == '_' or c == '-' or c == '.' {
        result = result + c.to_string()
      }
    }
    
    result
  }
  
  assert_eq(sanitize_string("valid_string123"), "valid_string123")
  assert_eq(sanitize_string("invalid@string#"), "invalidstring")  // Special chars removed
  assert_eq(sanitize_string("spaces are not allowed"), "spacesarenotallowed")
  
  // SQL injection detection
  let contains_sql_injection = fn(input: String) {
    let sql_patterns = [
      "SELECT", "INSERT", "UPDATE", "DELETE", "DROP", "UNION",
      "OR 1=1", "AND 1=1", "--", "/*", "*/", "xp_", "sp_"
    ]
    
    let upper_input = input.to_uppercase()
    sql_patterns.any(fn(pattern) { upper_input.contains(pattern) })
  }
  
  assert_false(contains_sql_injection("normal input"))
  assert_false(contains_sql_injection("user123"))
  
  assert_true(contains_sql_injection("SELECT * FROM users"))
  assert_true(contains_sql_injection("1' OR 1=1 --"))
  assert_true(contains_sql_injection("DROP TABLE users"))
  
  // XSS detection
  let contains_xss = fn(input: String) {
    let xss_patterns = [
      "<script>", "</script>", "javascript:", "onload=", "onerror=",
      "onclick=", "onmouseover=", "alert(", "document.cookie"
    ]
    
    let lower_input = input.to_lowercase()
    xss_patterns.any(fn(pattern) { lower_input.contains(pattern) })
  }
  
  assert_false(contains_xss("normal text"))
  assert_false(contains_xss("user@example.com"))
  
  assert_true(contains_xss("<script>alert('xss')</script>"))
  assert_true(contains_xss("<img onload='alert(1)'>"))
  assert_true(contains_xss("javascript:alert('xss')"))
  
  // Path traversal detection
  let contains_path_traversal = fn(input: String) {
    let traversal_patterns = ["../", "..\\", "%2e%2e%2f", "%2e%2e\\", "..%2f"]
    traversal_patterns.any(fn(pattern) { input.to_lowercase().contains(pattern) })
  }
  
  assert_false(contains_path_traversal("normal/path/file.txt"))
  assert_false(contains_path_traversal("config.json"))
  
  assert_true(contains_path_traversal("../../../etc/passwd"))
  assert_true(contains_path_traversal("..\\..\\windows\\system32"))
  assert_true(contains_path_traversal("%2e%2e%2f%2e%2e%2f"))
}

// Test 8: Performance Validation
test "performance validation" {
  // Response time validation
  let validate_response_time = fn(response_time: Int, threshold: Int) {
    response_time <= threshold
  }
  
  assert_true(validate_response_time(100, 200))  // Within threshold
  assert_true(validate_response_time(200, 200))  // At threshold
  
  assert_false(validate_response_time(300, 200))  // Exceeds threshold
  
  // Throughput validation
  let validate_throughput = fn(requests: Int, duration_ms: Int, min_throughput: Float) {
    let duration_s = duration_ms.to_float() / 1000.0
    let throughput = requests.to_float() / duration_s
    throughput >= min_throughput
  }
  
  assert_true(validate_throughput(1000, 1000, 1000.0))  // 1000 req/s
  assert_true(validate_throughput(500, 500, 1000.0))    // 1000 req/s
  
  assert_false(validate_throughput(500, 1000, 1000.0))  // 500 req/s
  
  // Memory usage validation
  let validate_memory_usage = fn(used_mb: Int, total_mb: Int, max_usage_percent: Float) {
    let usage_percent = used_mb.to_float() / total_mb.to_float() * 100.0
    usage_percent <= max_usage_percent
  }
  
  assert_true(validate_memory_usage(512, 4096, 20.0))  // 12.5% usage
  assert_true(validate_memory_usage(819, 4096, 20.0))  // 20% usage
  
  assert_false(validate_memory_usage(1024, 4096, 20.0))  // 25% usage
  
  // CPU usage validation
  let validate_cpu_usage = fn(usage_percent: Float, max_allowed: Float) {
    usage_percent <= max_allowed
  }
  
  assert_true(validate_cpu_usage(50.0, 80.0))
  assert_true(validate_cpu_usage(80.0, 80.0))
  
  assert_false(validate_cpu_usage(90.0, 80.0))
  
  // Error rate validation
  let validate_error_rate = fn(total_requests: Int, error_requests: Int, max_error_rate: Float) {
    if total_requests == 0 { return true }  // No requests, no errors
    let error_rate = error_requests.to_float() / total_requests.to_float() * 100.0
    error_rate <= max_error_rate
  }
  
  assert_true(validate_error_rate(1000, 10, 1.0))   // 1% error rate
  assert_true(validate_error_rate(1000, 5, 1.0))    // 0.5% error rate
  
  assert_false(validate_error_rate(1000, 20, 1.0))  // 2% error rate
  assert_true(validate_error_rate(0, 0, 1.0))       // No requests
}

// Test 9: Configuration Validation
test "configuration validation" {
  // Port number validation
  let validate_port = fn(port: Int) {
    port >= 1 and port <= 65535
  }
  
  assert_true(validate_port(80))    // HTTP
  assert_true(validate_port(443))   // HTTPS
  assert_true(validate_port(8080))  // Alternative HTTP
  assert_true(validate_port(65535)) // Maximum port
  
  assert_false(validate_port(0))    // Too low
  assert_false(validate_port(65536))// Too high
  assert_false(validate_port(-1))   // Negative
  
  // Timeout validation
  let validate_timeout = fn(timeout_ms: Int, min_ms: Int, max_ms: Int) {
    timeout_ms >= min_ms and timeout_ms <= max_ms
  }
  
  assert_true(validate_timeout(5000, 1000, 30000))  // 5 seconds
  assert_true(validate_timeout(1000, 1000, 30000))  // At minimum
  assert_true(validate_timeout(30000, 1000, 30000)) // At maximum
  
  assert_false(validate_timeout(500, 1000, 30000))   // Below minimum
  assert_false(validate_timeout(60000, 1000, 30000)) // Above maximum
  
  // Log level validation
  let validate_log_level = fn(level: String) {
    let valid_levels = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
    valid_levels.contains(level.to_uppercase())
  }
  
  assert_true(validate_log_level("INFO"))
  assert_true(validate_log_level("info"))  // Case insensitive
  assert_true(validate_log_level("ERROR"))
  assert_true(validate_log_level("debug"))
  
  assert_false(validate_log_level("VERBOSE"))
  assert_false(validate_log_level("WARNING"))  // Should be WARN
  assert_false(validate_log_level(""))
  
  // Configuration completeness validation
  let validate_config_completeness = fn(
    config: Array[(String, String)>,
    required_fields: Array[String]
  ) {
    required_fields.all(fn(field) {
      config.any(fn(item) { item.0 == field and item.1 != "" })
    })
  }
  
  let config = [
    ("host", "localhost"),
    ("port", "8080"),
    ("database_url", "postgresql://localhost/db"),
    ("log_level", "INFO")
  ]
  
  let required_fields = ["host", "port", "database_url"]
  let missing_fields = ["host", "port", "database_url", "auth_token"]
  
  assert_true(validate_config_completeness(config, required_fields))
  assert_false(validate_config_completeness(config, missing_fields))
  
  // Configuration consistency validation
  let validate_config_consistency = fn(config: Array[(String, String)>) {
    // Check for consistent SSL configuration
    let ssl_enabled = config.any(fn(item) { item.0 == "ssl_enabled" and item.1 == "true" })
    let has_cert = config.any(fn(item) { item.0 == "ssl_cert_path" and item.1 != "" })
    let has_key = config.any(fn(item) { item.0 == "ssl_key_path" and item.1 != "" })
    
    // If SSL is enabled, both cert and key should be present
    if ssl_enabled {
      has_cert and has_key
    } else {
      true  // SSL not enabled, no requirement
    }
  }
  
  let ssl_config = [
    ("ssl_enabled", "true"),
    ("ssl_cert_path", "/path/to/cert.pem"),
    ("ssl_key_path", "/path/to/key.pem")
  ]
  
  let incomplete_ssl_config = [
    ("ssl_enabled", "true"),
    ("ssl_cert_path", "/path/to/cert.pem")
    // Missing ssl_key_path
  ]
  
  let no_ssl_config = [
    ("ssl_enabled", "false")
  ]
  
  assert_true(validate_config_consistency(ssl_config))
  assert_false(validate_config_consistency(incomplete_ssl_config))
  assert_true(validate_config_consistency(no_ssl_config))
}

// Test 10: Data Transformation Validation
test "data transformation validation" {
  // Data normalization
  let normalize_string = fn(input: String) {
    input.trim().to_lowercase()
  }
  
  assert_eq(normalize_string("  Hello World  "), "hello world")
  assert_eq(normalize_string("TEST STRING"), "test string")
  assert_eq(normalize_string("  Mixed CASE  "), "mixed case")
  
  // Data aggregation validation
  let validate_aggregation = fn(
    values: Array[Float],
    aggregation_type: String,
    expected: Float
  ) {
    let actual = match aggregation_type {
      "sum" => values.reduce(fn(acc, v) { acc + v }, 0.0)
      "avg" => {
        let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
        sum / values.length().to_float()
      }
      "min" => values.reduce(fn(acc, v) { if v < acc { v } else { acc }, values[0])
      "max" => values.reduce(fn(acc, v) { if v > acc { v } else { acc }, values[0])
      _ => 0.0
    }
    
    // Allow for small floating point differences
    (actual - expected).abs() < 0.0001
  }
  
  let values = [1.0, 2.0, 3.0, 4.0, 5.0]
  
  assert_true(validate_aggregation(values, "sum", 15.0))
  assert_true(validate_aggregation(values, "avg", 3.0))
  assert_true(validate_aggregation(values, "min", 1.0))
  assert_true(validate_aggregation(values, "max", 5.0))
  
  // Data transformation validation
  let validate_transformation = fn(input: Array[Int], transform: Int -> Int, expected: Array[Int]) {
    let actual = input.map(transform)
    actual == expected
  }
  
  let numbers = [1, 2, 3, 4, 5]
  
  assert_true(validate_transformation(
    numbers,
    fn(x) { x * 2 },
    [2, 4, 6, 8, 10]
  ))
  
  assert_true(validate_transformation(
    numbers,
    fn(x) { x + 1 },
    [2, 3, 4, 5, 6]
  ))
  
  assert_true(validate_transformation(
    numbers,
    fn(x) { x * x },
    [1, 4, 9, 16, 25]
  ))
  
  // Data filtering validation
  let validate_filtering = fn(
    input: Array[Int],
    predicate: Int -> Bool,
    expected: Array[Int]
  ) {
    let actual = input.filter(predicate)
    actual == expected
  }
  
  assert_true(validate_filtering(
    numbers,
    fn(x) { x % 2 == 0 },
    [2, 4]
  ))
  
  assert_true(validate_filtering(
    numbers,
    fn(x) { x > 3 },
    [4, 5]
  ))
  
  assert_true(validate_filtering(
    numbers,
    fn(x) { x < 5 },
    [1, 2, 3, 4]
  ))
  
  // Data grouping validation
  let validate_grouping = fn(
    items: Array[(String, Int)>,
    key_fn: (String, Int) -> String,
    expected_groups: Array[(String, Array[Int])>
  ) {
    // Create groups
    let mut groups = []
    
    for item in items {
      let (name, value) = item
      let key = key_fn(name, value)
      
      match groups.find_index(fn(g) { g.0 == key }) {
        Some(index) => {
          let (group_key, group_values) = groups[index]
          groups = groups.set(index, (group_key, group_values.push(value)))
        }
        None => {
          groups = groups.push((key, [value]))
        }
      }
    }
    
    // Sort groups by key for comparison
    let sorted_groups = groups.sort_by(fn(g) { g.0 })
    let sorted_expected = expected_groups.sort_by(fn(g) { g.0 })
    
    sorted_groups == sorted_expected
  }
  
  let items = [
    ("apple", 1),
    ("banana", 2),
    ("apple", 3),
    ("orange", 4),
    ("banana", 5)
  ]
  
  assert_true(validate_grouping(
    items,
    fn(name, _) { name },
    [
      ("apple", [1, 3]),
      ("banana", [2, 5]),
      ("orange", [4])
    ]
  ))
}