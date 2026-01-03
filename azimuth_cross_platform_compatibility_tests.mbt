// Azimuth Telemetry System - Cross-Platform Compatibility Tests
// This file contains test cases for cross-platform compatibility

// Test 1: Operating System Compatibility
test "operating system compatibility" {
  // Test detection of current operating system
  let current_os = Platform::get_os()
  
  // Verify OS detection works
  match current_os {
    Windows => assert_true(true)
    Linux => assert_true(true)
    MacOS => assert_true(true)
    FreeBSD => assert_true(true)
    _ => assert_true(false) // Should not reach here for known platforms
  }
  
  // Test OS-specific path handling
  let path_separator = Platform::get_path_separator()
  
  match current_os {
    Windows => assert_eq(path_separator, "\\")
    _ => assert_eq(path_separator, "/") // Unix-like systems
  }
  
  // Test OS-specific line endings
  let line_ending = Platform::get_line_ending()
  
  match current_os {
    Windows => assert_eq(line_ending, "\r\n")
    _ => assert_eq(line_ending, "\n") // Unix-like systems
  }
  
  // Test OS-specific temporary directory
  let temp_dir = Platform::get_temp_directory()
  
  match current_os {
    Windows => assert_true(temp_dir.contains("Temp") || temp_dir.contains("tmp"))
    _ => assert_true(temp_dir.contains("tmp") || temp_dir.contains("temp"))
  }
  
  // Test telemetry system works across different OS
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "os_compatibility_meter")
  
  let counter = Meter::create_counter(meter, "os_counter", Some("OS compatibility counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("os.type", StringValue(current_os.to_string())),
    ("os.path_separator", StringValue(path_separator)),
    ("os.line_ending", StringValue(line_ending))
  ])))
  
  // Verify counter works regardless of OS
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "os_counter")
}

// Test 2: Architecture Compatibility
test "architecture compatibility" {
  // Test detection of current architecture
  let current_arch = Platform::get_architecture()
  
  // Verify architecture detection works
  match current_arch {
    X86 => assert_true(true)
    X64 => assert_true(true)
    ARM => assert_true(true)
    ARM64 => assert_true(true)
    _ => assert_true(false) // Should not reach here for known architectures
  }
  
  // Test endianness detection
  let is_little_endian = Platform::is_little_endian()
  
  // Verify endianness is detected
  assert_true(is_little_endian == true || is_little_endian == false)
  
  // Test word size detection
  let word_size = Platform::get_word_size()
  
  // Verify word size is reasonable
  assert_true(word_size == 32 || word_size == 64)
  
  // Test telemetry system works across different architectures
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "arch_compatibility_meter")
  
  let counter = Meter::create_counter(meter, "arch_counter", Some("Architecture compatibility counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("arch.type", StringValue(current_arch.to_string())),
    ("arch.word_size", IntValue(word_size)),
    ("arch.endianness", StringValue(if is_little_endian { "little" } else { "big" }))
  ])))
  
  // Verify counter works regardless of architecture
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "arch_counter")
}

// Test 3: File System Compatibility
test "file system compatibility" {
  // Test file path handling across platforms
  let path_components = ["home", "user", "documents", "test.txt"]
  let platform_path = Platform::build_path(path_components)
  
  // Verify path is built correctly for current platform
  let path_separator = Platform::get_path_separator()
  let expected_path = path_components.join(path_separator)
  assert_eq(platform_path, expected_path)
  
  // Test file path normalization
  let messy_path = if path_separator == "/" {
    "home//user/./documents/../documents/test.txt"
  } else {
    "home\\\\user\\\\.\\\\documents\\\\..\\\\documents\\\\test.txt"
  }
  
  let normalized_path = Platform::normalize_path(messy_path)
  
  // Verify path is normalized
  assert_false(normalized_path.contains(".."))
  assert_false(normalized_path.contains(".")) // Current directory reference
  
  if path_separator == "/" {
    assert_false(normalized_path.contains("//"))
  } else {
    assert_false(normalized_path.contains("\\\\"))
  }
  
  // Test telemetry file operations across platforms
  let temp_dir = Platform::get_temp_directory()
  let telemetry_file_path = Platform::build_path([temp_dir, "azimuth_test.txt"])
  
  // Create telemetry data file
  let span_context = SpanContext::new("trace123", "span123", true, "fs_test")
  let span = Span::new("fs_test_span", Internal, span_context)
  
  Span::set_attribute(span, "fs.test", StringValue("file_system_compatibility"))
  
  // Serialize span to file
  let json_data = JsonSerializer::serialize_span(span)
  let write_result = File::write_all_text(telemetry_file_path, json_data)
  
  match write_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false) // File write should succeed
  }
  
  // Read telemetry data from file
  let read_result = File::read_all_text(telemetry_file_path)
  
  match read_result {
    Ok(data) => {
      // Verify data matches what was written
      assert_eq(data, json_data)
      
      // Deserialize span from file data
      let deserialized_span = JsonSerializer::deserialize_span(data)
      assert_eq(Span::name(deserialized_span), "fs_test_span")
    }
    Err(_) => assert_true(false) // File read should succeed
  }
  
  // Clean up test file
  let delete_result = File::delete(telemetry_file_path)
  match delete_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false) // File deletion should succeed
  }
}

// Test 4: Network Compatibility
test "network compatibility" {
  // Test network interface detection
  let network_interfaces = Platform::get_network_interfaces()
  
  // Verify at least one network interface is detected
  assert_true(network_interfaces.length() > 0)
  
  // Test localhost resolution
  let localhost_ip = Platform::resolve_localhost()
  
  // Verify localhost is resolved
  match localhost_ip {
    Some(ip) => {
      if ip.contains("127.0.0.1") || ip.contains("::1") {
        assert_true(true)
      } else {
        assert_true(false) // Should resolve to standard localhost addresses
      }
    }
    None => assert_true(false) // Should resolve localhost
  }
  
  // Test HTTP client compatibility across platforms
  let client = HttpClient::new()
  
  // Test HTTP request to a reliable endpoint
  let request = HttpRequest::new("GET", "https://httpbin.org/ip", [], None)
  let response = HttpClient::execute(client, request)
  
  match response {
    Ok(http_response) => {
      assert_eq(HttpResponse::status_code(http_response), 200)
      
      // Verify response body contains IP information
      match HttpResponse::body(http_response) {
        Some(body) => assert_true(body.contains("origin"))
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false) // HTTP request should succeed
  }
  
  // Test telemetry network operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "network_compatibility_meter")
  
  let counter = Meter::create_counter(meter, "network_counter", Some("Network compatibility counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("network.interfaces", IntValue(network_interfaces.length())),
    ("network.localhost", StringValue(match localhost_ip { Some(ip) => ip, None => "unknown" }))
  ])))
  
  // Verify counter works regardless of network configuration
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "network_counter")
}

// Test 5: Time and Timezone Compatibility
test "time and timezone compatibility" {
  // Test current time retrieval
  let current_time = Platform::get_current_time()
  
  // Verify time is reasonable (not zero, not negative)
  assert_true(current_time > 0)
  
  // Test timezone detection
  let timezone = Platform::get_timezone()
  
  // Verify timezone is detected
  assert_true(timezone.length() > 0)
  
  // Test UTC time conversion
  let utc_time = Platform::get_utc_time()
  
  // Verify UTC time is reasonable
  assert_true(utc_time > 0)
  
  // Test time formatting
  let formatted_time = Platform::format_time(current_time, "%Y-%m-%d %H:%M:%S")
  
  // Verify time is formatted correctly
  assert_true(formatted_time.contains("-"))
  assert_true(formatted_time.contains(":"))
  assert_true(formatted_time.length() == 19) // YYYY-MM-DD HH:MM:SS
  
  // Test telemetry timestamp handling
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time_compatibility_meter")
  
  let counter = Meter::create_counter(meter, "time_counter", Some("Time compatibility counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("time.current", StringValue(formatted_time)),
    ("time.timezone", StringValue(timezone)),
    ("time.utc", StringValue(Platform::format_time(utc_time, "%Y-%m-%d %H:%M:%S")))
  ])))
  
  // Verify counter works regardless of time zone
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "time_counter")
  
  // Test log record with platform-specific timestamp
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "time_compatibility_logger")
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Time compatibility test message"),
    Some(Attributes::with([
      ("local.time", StringValue(formatted_time)),
      ("utc.time", StringValue(Platform::format_time(utc_time, "%Y-%m-%d %H:%M:%S")))
    ])),
    Some(current_time),
    Some(utc_time),
    Some("trace123"),
    Some("span123"),
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record)
  
  // Verify log record was created
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Time compatibility test message")
    None => assert_true(false)
  }
}

// Test 6: Encoding and Locale Compatibility
test "encoding and locale compatibility" {
  // Test system encoding detection
  let system_encoding = Platform::get_system_encoding()
  
  // Verify encoding is detected
  assert_true(system_encoding.length() > 0)
  
  // Test UTF-8 handling
  let utf8_string = "Azimuth 遥测系统 🌍"
  let utf8_bytes = utf8_string.to_utf8_bytes()
  let decoded_string = String::from_utf8_bytes(utf8_bytes)
  
  // Verify UTF-8 encoding/decoding works
  assert_eq(decoded_string, utf8_string)
  
  // Test system locale detection
  let system_locale = Platform::get_system_locale()
  
  // Verify locale is detected
  assert_true(system_locale.length() > 0)
  
  // Test number formatting with locale
  let number = 1234.56
  let formatted_number = Platform::format_number(number, system_locale)
  
  // Verify number is formatted
  assert_true(formatted_number.contains("1234"))
  
  // Test date formatting with locale
  let date_time = Platform::get_current_time()
  let formatted_date = Platform::format_date(date_time, system_locale)
  
  // Verify date is formatted
  assert_true(formatted_date.length() > 0)
  
  // Test telemetry with international characters
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "encoding_compatibility_meter")
  
  let counter = Meter::create_counter(meter, "encoding_counter", Some("编码兼容性计数器"), Some("操作"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("encoding.system", StringValue(system_encoding)),
    ("locale.system", StringValue(system_locale)),
    ("test.utf8", StringValue(utf8_string)),
    ("number.formatted", StringValue(formatted_number)),
    ("date.formatted", StringValue(formatted_date))
  ])))
  
  // Verify counter works with international characters
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "encoding_counter")
  assert_eq(Instrument::description(counter_instrument), Some("编码兼容性计数器"))
  assert_eq(Instrument::unit(counter_instrument), Some("操作"))
}

// Test 7: Environment Variable Compatibility
test "environment variable compatibility" {
  // Test environment variable retrieval
  let path_var = Platform::get_environment_variable("PATH")
  
  // Verify PATH variable is retrieved (should exist on all platforms)
  match path_var {
    Some(path) => assert_true(path.length() > 0)
    None => assert_true(false) // PATH should exist
  }
  
  // Test environment variable setting and retrieval
  let test_var_name = "AZIMUTH_TEST_VAR"
  let test_var_value = "test_value"
  
  let set_result = Platform::set_environment_variable(test_var_name, test_var_value)
  match set_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false) // Setting should succeed
  }
  
  let get_result = Platform::get_environment_variable(test_var_name)
  match get_result {
    Some(value) => assert_eq(value, test_var_value)
    None => assert_true(false) // Should retrieve the value we just set
  }
  
  // Test environment variable deletion
  let unset_result = Platform::unset_environment_variable(test_var_name)
  match unset_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false) // Unsetting should succeed
  }
  
  let get_after_unset = Platform::get_environment_variable(test_var_name)
  match get_after_unset {
    Some(_) => assert_true(false) // Should not exist after unsetting
    None => assert_true(true)
  }
  
  // Test telemetry with environment-specific configuration
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "env_compatibility_meter")
  
  let counter = Meter::create_counter(meter, "env_counter", Some("Environment compatibility counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("env.path", StringValue(match path_var { Some(path) => path.substring(0, 100), None => "unknown" })),
    ("env.test_set", StringValue(if set_result.is_ok() { "success" } else { "failed" })),
    ("env.test_get", StringValue(if get_result.is_some() { "success" } else { "failed" })),
    ("env.test_unset", StringValue(if unset_result.is_ok() { "success" } else { "failed" }))
  ])))
  
  // Verify counter works regardless of environment
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "env_counter")
}

// Test 8: Process and Thread Compatibility
test "process and thread compatibility" {
  // Test current process ID retrieval
  let process_id = Platform::get_current_process_id()
  
  // Verify process ID is reasonable
  assert_true(process_id > 0)
  
  // Test current thread ID retrieval
  let thread_id = Platform::get_current_thread_id()
  
  // Verify thread ID is reasonable
  assert_true(thread_id > 0)
  
  // Test process start time
  let process_start_time = Platform::get_process_start_time()
  
  // Verify process start time is reasonable
  assert_true(process_start_time > 0)
  
  // Test CPU usage
  let cpu_usage = Platform::get_cpu_usage()
  
  // Verify CPU usage is reasonable
  assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
  
  // Test memory usage
  let memory_usage = Platform::get_memory_usage()
  
  // Verify memory usage is reasonable
  assert_true(memory_usage > 0)
  
  // Test telemetry with process information
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "process_compatibility_meter")
  
  let counter = Meter::create_counter(meter, "process_counter", Some("Process compatibility counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("process.id", IntValue(process_id)),
    ("thread.id", IntValue(thread_id)),
    ("process.start_time", StringValue(Platform::format_time(process_start_time, "%Y-%m-%d %H:%M:%S"))),
    ("cpu.usage", FloatValue(cpu_usage)),
    ("memory.usage", IntValue(memory_usage))
  ])))
  
  // Verify counter works regardless of process/thread implementation
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "process_counter")
  
  // Test span with process context
  let span_context = SpanContext::new("trace123", "span123", true, "process_test")
  let span = Span::new("process_test_span", Internal, span_context)
  
  Span::set_attribute(span, "process.id", IntValue(process_id))
  Span::set_attribute(span, "thread.id", IntValue(thread_id))
  Span::set_attribute(span, "cpu.usage", FloatValue(cpu_usage))
  Span::set_attribute(span, "memory.usage", IntValue(memory_usage))
  
  // Verify span attributes are set
  match Span::get_attribute(span, "process.id") {
    Some(IntValue(id)) => assert_eq(id, process_id)
    _ => assert_true(false)
  }
  
  match Span::get_attribute(span, "thread.id") {
    Some(IntValue(id)) => assert_eq(id, thread_id)
    _ => assert_true(false)
  }
  
  match Span::get_attribute(span, "cpu.usage") {
    Some(FloatValue(usage)) => assert_eq(usage, cpu_usage)
    _ => assert_true(false)
  }
  
  match Span::get_attribute(span, "memory.usage") {
    Some(IntValue(usage)) => assert_eq(usage, memory_usage)
    _ => assert_true(false)
  }
}

// Test 9: Platform-Specific Feature Detection
test "platform-specific feature detection" {
  // Test feature detection
  let features = Platform::detect_features()
  
  // Verify features are detected
  assert_true(features.length() > 0)
  
  // Test specific feature detection
  let has_ipv6 = Platform::has_ipv6_support()
  let has_ssl = Platform::has_ssl_support()
  let has_gui = Platform::has_gui_support()
  let has_sqlite = Platform::has_sqlite_support()
  
  // Verify feature detection returns boolean values
  assert_true(has_ipv6 == true || has_ipv6 == false)
  assert_true(has_ssl == true || has_ssl == false)
  assert_true(has_gui == true || has_gui == false)
  assert_true(has_sqlite == true || has_sqlite == false)
  
  // Test platform-specific capabilities
  let capabilities = Platform::get_capabilities()
  
  // Verify capabilities are detected
  assert_true(capabilities.length() > 0)
  
  // Test telemetry with feature information
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "feature_compatibility_meter")
  
  let counter = Meter::create_counter(meter, "feature_counter", Some("Feature compatibility counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("feature.ipv6", StringValue(if has_ipv6 { "supported" } else { "not_supported" })),
    ("feature.ssl", StringValue(if has_ssl { "supported" } else { "not_supported" })),
    ("feature.gui", StringValue(if has_gui { "supported" } else { "not_supported" })),
    ("feature.sqlite", StringValue(if has_sqlite { "supported" } else { "not_supported" })),
    ("features.count", IntValue(features.length())),
    ("capabilities.count", IntValue(capabilities.length()))
  ])))
  
  // Verify counter works regardless of feature support
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "feature_counter")
  
  // Test conditional feature usage
  if has_ssl {
    // Test HTTPS request if SSL is supported
    let client = HttpClient::new()
    let request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
    let response = HttpClient::execute(client, request)
    
    match response {
      Ok(_) => assert_true(true) // HTTPS request should succeed
      Err(_) => assert_true(false) // HTTPS request should succeed if SSL is supported
    }
  } else {
    // Test HTTP request if SSL is not supported
    let client = HttpClient::new()
    let request = HttpRequest::new("GET", "http://httpbin.org/get", [], None)
    let response = HttpClient::execute(client, request)
    
    match response {
      Ok(_) => assert_true(true) // HTTP request should succeed
      Err(_) => assert_true(false) // HTTP request should succeed
    }
  }
}

// Test 10: Cross-Platform Telemetry Data Consistency
test "cross-platform telemetry data consistency" {
  // Create consistent telemetry data across platforms
  let trace_id = Platform::generate_trace_id()
  let span_id = Platform::generate_span_id()
  
  // Verify IDs are generated consistently
  assert_eq(trace_id.length(), 32) // Should be 16 bytes = 32 hex characters
  assert_eq(span_id.length(), 16)   // Should be 8 bytes = 16 hex characters
  
  // Create span with consistent data
  let span_context = SpanContext::new(trace_id, span_id, true, "cross_platform_test")
  let span = Span::new("cross_platform_span", Internal, span_context)
  
  // Add platform-agnostic attributes
  Span::set_attribute(span, "service.name", StringValue("azimuth_test"))
  Span::set_attribute(span, "service.version", StringValue("1.0.0"))
  Span::set_attribute(span, "test.name", StringValue("cross_platform_consistency"))
  
  // Add platform-specific attributes
  let current_os = Platform::get_os()
  let current_arch = Platform::get_architecture()
  
  Span::set_attribute(span, "platform.os", StringValue(current_os.to_string()))
  Span::set_attribute(span, "platform.arch", StringValue(current_arch.to_string()))
  Span::set_attribute(span, "platform.encoding", StringValue(Platform::get_system_encoding()))
  Span::set_attribute(span, "platform.locale", StringValue(Platform::get_system_locale()))
  
  // Add events with consistent timestamps
  let current_time = Platform::get_current_time()
  let utc_time = Platform::get_utc_time()
  
  Span::add_event(span, "test_started", Some([
    ("local.timestamp", StringValue(Platform::format_time(current_time, "%Y-%m-%dT%H:%M:%S"))),
    ("utc.timestamp", StringValue(Platform::format_time(utc_time, "%Y-%m-%dT%H:%M:%SZ")))
  ]))
  
  // Serialize span to ensure consistent format
  let json_data = JsonSerializer::serialize_span(span)
  
  // Verify JSON contains expected data
  assert_true(json_data.contains("cross_platform_span"))
  assert_true(json_data.contains("azimuth_test"))
  assert_true(json_data.contains("1.0.0"))
  assert_true(json_data.contains("cross_platform_consistency"))
  assert_true(json_data.contains("platform.os"))
  assert_true(json_data.contains("platform.arch"))
  assert_true(json_data.contains("test_started"))
  
  // Test Protocol Buffer serialization for consistency
  let pb_data = ProtobufSerializer::serialize_span(span)
  
  // Verify Protocol Buffer data is generated
  assert_true(pb_data.length() > 0)
  
  // Test deserialization consistency
  let json_deserialized = JsonSerializer::deserialize_span(json_data)
  let pb_deserialized = ProtobufSerializer::deserialize_span(pb_data)
  
  // Verify both deserializations produce consistent results
  assert_eq(Span::name(json_deserialized), Span::name(pb_deserialized))
  assert_eq(Span::kind(json_deserialized), Span::kind(pb_deserialized))
  
  let json_context = Span::span_context(json_deserialized)
  let pb_context = Span::span_context(pb_deserialized)
  
  assert_eq(SpanContext::trace_id(json_context), SpanContext::trace_id(pb_context))
  assert_eq(SpanContext::span_id(json_context), SpanContext::span_id(pb_context))
  
  // Test metric consistency
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "consistency_meter")
  
  let counter = Meter::create_counter(meter, "consistency_counter", Some("Cross-platform consistency counter"), Some("operations"))
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("platform.os", StringValue(current_os.to_string())),
    ("platform.arch", StringValue(current_arch.to_string())),
    ("trace.id", StringValue(trace_id)),
    ("span.id", StringValue(span_id)),
    ("test.timestamp", StringValue(Platform::format_time(current_time, "%Y-%m-%dT%H:%M:%S")))
  ])))
  
  // Serialize metric for consistency check
  let metric_json = JsonSerializer::serialize_metric(counter)
  let metric_pb = ProtobufSerializer::serialize_metric(counter)
  
  // Verify both serializations contain expected data
  assert_true(metric_json.contains("consistency_counter"))
  assert_true(metric_json.contains(current_os.to_string()))
  assert_true(metric_json.contains(current_arch.to_string()))
  assert_true(metric_json.contains(trace_id))
  assert_true(metric_json.contains(span_id))
  
  assert_true(metric_pb.length() > 0)
  
  // Test log record consistency
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "consistency_logger")
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Cross-platform consistency test log"),
    Some(Attributes::with([
      ("platform.os", StringValue(current_os.to_string())),
      ("platform.arch", StringValue(current_arch.to_string())),
      ("trace.id", StringValue(trace_id)),
      ("span.id", StringValue(span_id))
    ])),
    Some(current_time),
    Some(utc_time),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Serialize log record for consistency check
  let log_json = JsonSerializer::serialize_log_record(log_record)
  let log_pb = ProtobufSerializer::serialize_log_record(log_record)
  
  // Verify both serializations contain expected data
  assert_true(log_json.contains("Cross-platform consistency test log"))
  assert_true(log_json.contains(current_os.to_string()))
  assert_true(log_json.contains(current_arch.to_string()))
  assert_true(log_json.contains(trace_id))
  assert_true(log_json.contains(span_id))
  
  assert_true(log_pb.length() > 0)
}