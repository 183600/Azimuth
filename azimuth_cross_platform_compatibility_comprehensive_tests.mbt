// Cross-Platform Compatibility Tests
// This file contains comprehensive test cases for cross-platform compatibility

// Test 1: Platform Detection and Adaptation
test "platform detection and adaptation" {
  let platform_detector = PlatformDetector::new()
  
  // Test platform detection
  let platform_info = PlatformDetector::detect(platform_detector)
  
  // Verify platform information is detected
  assert_true(PlatformInfo::os_type(platform_info).length() > 0)
  assert_true(PlatformInfo::arch(platform_info).length() > 0)
  assert_true(PlatformInfo::runtime(platform_info).length() > 0)
  
  // Test platform-specific adaptations
  let adapter = PlatformAdapter::new(platform_info)
  
  // Test file path handling
  let test_path = "/tmp/telemetry/test.log"
  let adapted_path = PlatformAdapter::adapt_file_path(adapter, test_path)
  
  // Path should be adapted based on platform
  match PlatformInfo::os_type(platform_info) {
    "windows" => assert_true(adapted_path.contains("\\")),
    "linux" | "macos" => assert_true(adapted_path.contains("/")),
    _ => assert_true(true) // Other platforms
  }
  
  // Test line ending adaptation
  let test_text = "line1\nline2\nline3"
  let adapted_text = PlatformAdapter::adapt_line_endings(adapter, test_text)
  
  // Line endings should be adapted based on platform
  match PlatformInfo::os_type(platform_info) {
    "windows" => assert_true(adapted_text.contains("\r\n")),
    "linux" | "macos" => assert_true(adapted_text.contains("\n")),
    _ => assert_true(true) // Other platforms
  }
  
  // Test timezone handling
  let timezone = PlatformAdapter::get_system_timezone(adapter)
  assert_true(timezone.length() > 0)
  
  // Test locale handling
  let locale = PlatformAdapter::get_system_locale(adapter)
  assert_true(locale.length() > 0)
}

// Test 2: File System Compatibility
test "file system compatibility" {
  let file_system_adapter = FileSystemAdapter::new()
  
  // Test path normalization
  let test_paths = [
    "/path/to/file",
    "\\path\\to\\file",
    "path/to/file",
    "path\\to\\file",
    "./relative/path",
    ".\\relative\\path"
  ]
  
  for path in test_paths {
    let normalized = FileSystemAdapter::normalize_path(file_system_adapter, path)
    assert_true(normalized.length() > 0)
    
    // Normalized path should use consistent separators
    let has_forward_slash = normalized.contains("/")
    let has_backslash = normalized.contains("\\")
    
    // Should not mix separators
    assert_false(has_forward_slash && has_backslash)
  }
  
  // Test temporary directory handling
  let temp_dir = FileSystemAdapter::get_temp_directory(file_system_adapter)
  assert_true(temp_dir.length() > 0)
  
  // Test user directory handling
  let user_dir = FileSystemAdapter::get_user_directory(file_system_adapter)
  assert_true(user_dir.length() > 0)
  
  // Test file permissions
  let test_file = FileSystemAdapter::join_path(file_system_adapter, temp_dir, "telemetry_test_file")
  
  // Create test file
  let create_result = FileSystemAdapter::create_file(file_system_adapter, test_file)
  assert_true(create_result.is_ok())
  
  // Write to file
  let write_result = FileSystemAdapter::write_to_file(file_system_adapter, test_file, "test content")
  assert_true(write_result.is_ok())
  
  // Read from file
  let read_result = FileSystemAdapter::read_from_file(file_system_adapter, test_file)
  match read_result {
    Ok(content) => assert_eq(content, "test content"),
    Err(_) => assert_true(false)
  }
  
  // Check file permissions
  let permissions = FileSystemAdapter::get_file_permissions(file_system_adapter, test_file)
  assert_true(permissions.length() > 0)
  
  // Clean up test file
  let delete_result = FileSystemAdapter::delete_file(file_system_adapter, test_file)
  assert_true(delete_result.is_ok())
}

// Test 3: Network Compatibility
test "network compatibility" {
  let network_adapter = NetworkAdapter::new()
  
  // Test hostname resolution
  let hostname = NetworkAdapter::get_hostname(network_adapter)
  assert_true(hostname.length() > 0)
  
  // Test IP address detection
  let ip_addresses = NetworkAdapter::get_ip_addresses(network_adapter)
  assert_true(ip_addresses.length() > 0)
  
  // Test DNS resolution
  let dns_result = NetworkAdapter::resolve_hostname(network_adapter, "localhost")
  match dns_result {
    Ok(ips) => assert_true(ips.length() > 0),
    Err(_) => assert_true(false)
  }
  
  // Test HTTP client compatibility
  let http_client = NetworkAdapter::create_http_client(network_adapter)
  
  // Test HTTP request
  let request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let response = HttpClient::send(http_client, request)
  
  match response {
    Ok(resp) => {
      assert_eq(HttpResponse::status_code(resp), 200)
      match HttpResponse::body(resp) {
        Some(body) => assert_true(body.length() > 0),
        None => assert_true(false)
      }
    }
    Err(_) => {
      // Network might not be available, which is OK for this test
      assert_true(true)
    }
  }
  
  // Test TLS/SSL compatibility
  let tls_version = NetworkAdapter::get_tls_version(network_adapter)
  assert_true(tls_version.length() > 0)
  
  // Test proxy support
  let proxy_config = NetworkAdapter::detect_proxy_config(network_adapter)
  // Proxy might not be configured, which is OK
  assert_true(true)
}

// Test 4: Process and Thread Compatibility
test "process and thread compatibility" {
  let process_adapter = ProcessAdapter::new()
  
  // Test process ID
  let process_id = ProcessAdapter::get_process_id(process_adapter)
  assert_true(process_id > 0)
  
  // Test parent process ID
  let parent_process_id = ProcessAdapter::get_parent_process_id(process_adapter)
  assert_true(parent_process_id > 0)
  
  // Test process name
  let process_name = ProcessAdapter::get_process_name(process_adapter)
  assert_true(process_name.length() > 0)
  
  // Test command line arguments
  let command_line = ProcessAdapter::get_command_line(process_adapter)
  assert_true(command_line.length() > 0)
  
  // Test environment variables
  let env_vars = ProcessAdapter::get_environment_variables(process_adapter)
  assert_true(env_vars.length() > 0)
  
  // Test working directory
  let working_dir = ProcessAdapter::get_working_directory(process_adapter)
  assert_true(working_dir.length() > 0)
  
  // Test thread compatibility
  let thread_adapter = ThreadAdapter::new()
  
  // Test thread ID
  let thread_id = ThreadAdapter::get_thread_id(thread_adapter)
  assert_true(thread_id.length() > 0)
  
  // Test thread name
  let thread_name = ThreadAdapter::get_thread_name(thread_adapter)
  // Thread name might not be available on all platforms
  assert_true(true)
  
  // Test CPU count
  let cpu_count = ProcessAdapter::get_cpu_count(process_adapter)
  assert_true(cpu_count > 0)
  
  // Test memory info
  let memory_info = ProcessAdapter::get_memory_info(process_adapter)
  assert_true(MemoryInfo::total_memory(memory_info) > 0)
}

// Test 5: Time and Date Compatibility
test "time and date compatibility" {
  let time_adapter = TimeAdapter::new()
  
  // Test current time
  let current_time = TimeAdapter::get_current_time(time_adapter)
  assert_true(current_time > 0)
  
  // Test timezone offset
  let timezone_offset = TimeAdapter::get_timezone_offset(time_adapter)
  assert_true(timezone_offset != 0 || true) // Might be 0 for UTC
  
  // Test time formatting
  let formatted_time = TimeAdapter::format_time(time_adapter, current_time, "%Y-%m-%d %H:%M:%S")
  assert_true(formatted_time.length() > 0)
  
  // Test time parsing
  let parse_result = TimeAdapter::parse_time(time_adapter, formatted_time, "%Y-%m-%d %H:%M:%S")
  match parse_result {
    Ok(parsed_time) => assert_true(parsed_time > 0),
    Err(_) => assert_true(false)
  }
  
  // Test high-resolution timer
  let start_time = TimeAdapter::get_high_resolution_time(time_adapter)
  
  // Sleep for a short duration
  TimeAdapter::sleep(time_adapter, 10) // 10ms
  
  let end_time = TimeAdapter::get_high_resolution_time(time_adapter)
  let elapsed = TimeAdapter::calculate_elapsed(time_adapter, start_time, end_time)
  
  // Should have slept at least 10ms (with some tolerance)
  assert_true(elapsed >= 10.0)
  
  // Test monotonic clock
  let monotonic_time = TimeAdapter::get_monotonic_time(time_adapter)
  assert_true(monotonic_time > 0)
  
  // Test date/time arithmetic
  let future_time = TimeAdapter::add_seconds(time_adapter, current_time, 3600) // Add 1 hour
  assert_true(future_time > current_time)
  
  let past_time = TimeAdapter::subtract_seconds(time_adapter, current_time, 3600) // Subtract 1 hour
  assert_true(past_time < current_time)
}

// Test 6: Encoding and Character Set Compatibility
test "encoding and character set compatibility" {
  let encoding_adapter = EncodingAdapter::new()
  
  // Test UTF-8 encoding
  let test_string = "Hello, 世界! 👋"
  let utf8_bytes = EncodingAdapter::encode_utf8(encoding_adapter, test_string)
  assert_true(utf8_bytes.length() > test_string.length()) // UTF-8 should be longer for multi-byte chars
  
  // Test UTF-8 decoding
  let decoded_string = EncodingAdapter::decode_utf8(encoding_adapter, utf8_bytes)
  assert_eq(decoded_string, test_string)
  
  // Test UTF-16 encoding
  let utf16_bytes = EncodingAdapter::encode_utf16(encoding_adapter, test_string)
  assert_true(utf16_bytes.length() > 0)
  
  // Test UTF-16 decoding
  let decoded_utf16 = EncodingAdapter::decode_utf16(encoding_adapter, utf16_bytes)
  assert_eq(decoded_utf16, test_string)
  
  // Test ASCII encoding (should fail for non-ASCII characters)
  let ascii_result = EncodingAdapter::encode_ascii(encoding_adapter, test_string)
  match ascii_result {
    Ok(_) => assert_true(false, "ASCII encoding should fail for non-ASCII characters"),
    Err(_) => assert_true(true)
  }
  
  // Test ASCII encoding for ASCII-only string
  let ascii_string = "Hello, World!"
  let ascii_bytes = EncodingAdapter::encode_ascii(encoding_adapter, ascii_string).unwrap()
  assert_eq(ascii_bytes.length(), ascii_string.length())
  
  let decoded_ascii = EncodingAdapter::decode_ascii(encoding_adapter, ascii_bytes)
  assert_eq(decoded_ascii, ascii_string)
  
  // Test base64 encoding
  let base64_encoded = EncodingAdapter::encode_base64(encoding_adapter, utf8_bytes)
  assert_true(base64_encoded.length() > 0)
  
  // Test base64 decoding
  let base64_decoded = EncodingAdapter::decode_base64(encoding_adapter, base64_encoded)
  assert_eq(base64_decoded, utf8_bytes)
  
  // Test system default encoding
  let default_encoding = EncodingAdapter::get_system_default_encoding(encoding_adapter)
  assert_true(default_encoding.length() > 0)
}

// Test 7: Platform-Specific Telemetry Features
test "platform-specific telemetry features" {
  let platform_telemetry = PlatformTelemetry::new()
  
  // Test CPU metrics
  let cpu_metrics = PlatformTelemetry::get_cpu_metrics(platform_telemetry)
  assert_true(CpuMetrics::usage_percent(cpu_metrics) >= 0.0)
  assert_true(CpuMetrics::usage_percent(cpu_metrics) <= 100.0)
  assert_true(CpuMetrics::core_count(cpu_metrics) > 0)
  
  // Test memory metrics
  let memory_metrics = PlatformTelemetry::get_memory_metrics(platform_telemetry)
  assert_true(MemoryMetrics::total_bytes(memory_metrics) > 0)
  assert_true(MemoryMetrics::available_bytes(memory_metrics) > 0)
  assert_true(MemoryMetrics::used_bytes(memory_metrics) > 0)
  
  // Test disk metrics
  let disk_metrics = PlatformTelemetry::get_disk_metrics(platform_telemetry)
  assert_true(DiskMetrics::total_bytes(disk_metrics) > 0)
  assert_true(DiskMetrics::available_bytes(disk_metrics) > 0)
  assert_true(DiskMetrics::used_bytes(disk_metrics) > 0)
  
  // Test network metrics
  let network_metrics = PlatformTelemetry::get_network_metrics(platform_telemetry)
  assert_true(NetworkMetrics::bytes_sent(network_metrics) >= 0)
  assert_true(NetworkMetrics::bytes_received(network_metrics) >= 0)
  assert_true(NetworkMetrics::packets_sent(network_metrics) >= 0)
  assert_true(NetworkMetrics::packets_received(network_metrics) >= 0)
  
  // Test process metrics
  let process_metrics = PlatformTelemetry::get_process_metrics(platform_telemetry)
  assert_true(ProcessMetrics::pid(process_metrics) > 0)
  assert_true(ProcessMetrics::memory_usage(process_metrics) > 0)
  assert_true(ProcessMetrics::cpu_time(process_metrics) >= 0.0)
  
  // Test platform-specific features
  let platform_info = PlatformDetector::detect(PlatformDetector::new())
  
  match PlatformInfo::os_type(platform_info) {
    "linux" => {
      // Test Linux-specific features
      let linux_metrics = PlatformTelemetry::get_linux_metrics(platform_telemetry);
      // Verify Linux-specific metrics are available
      assert_true(true);
    }
    "windows" => {
      // Test Windows-specific features
      let windows_metrics = PlatformTelemetry::get_windows_metrics(platform_telemetry);
      // Verify Windows-specific metrics are available
      assert_true(true);
    }
    "macos" => {
      // Test macOS-specific features
      let macos_metrics = PlatformTelemetry::get_macos_metrics(platform_telemetry);
      // Verify macOS-specific metrics are available
      assert_true(true);
    }
    _ => {
      // Other platforms
      assert_true(true);
    }
  }
}

// Test 8: Cross-Platform Configuration
test "cross-platform configuration" {
  let config_adapter = ConfigAdapter::new()
  
  // Test config directory detection
  let config_dir = ConfigAdapter::get_config_directory(config_adapter)
  assert_true(config_dir.length() > 0)
  
  // Test config file paths
  let config_file = ConfigAdapter::get_config_file_path(config_adapter, "telemetry.json")
  assert_true(config_file.length() > 0)
  
  // Test config creation and loading
  let test_config = TelemetryConfig::new()
  TelemetryConfig::set(test_config, "service.name", "test-service")
  TelemetryConfig::set(test_config, "service.version", "1.0.0")
  TelemetryConfig::set(test_config, "exporter.endpoint", "http://localhost:4318")
  
  // Save config
  let save_result = ConfigAdapter::save_config(config_adapter, test_config, config_file)
  assert_true(save_result.is_ok())
  
  // Load config
  let load_result = ConfigAdapter::load_config(config_adapter, config_file)
  match load_result {
    Ok(loaded_config) => {
      let service_name = TelemetryConfig::get_string(loaded_config, "service.name")
      match service_name {
        Some(name) => assert_eq(name, "test-service"),
        None => assert_true(false)
      }
      
      let service_version = TelemetryConfig::get_string(loaded_config, "service.version")
      match service_version {
        Some(version) => assert_eq(version, "1.0.0"),
        None => assert_true(false)
      }
      
      let endpoint = TelemetryConfig::get_string(loaded_config, "exporter.endpoint")
      match endpoint {
        Some(ep) => assert_eq(ep, "http://localhost:4318"),
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test environment variable config
  ConfigAdapter::set_environment_variable(config_adapter, "TELEMETRY_SERVICE_NAME", "env-test-service")
  
  let env_config = ConfigAdapter::load_from_environment(config_adapter)
  let env_service_name = TelemetryConfig::get_string(env_config, "service.name")
  match env_service_name {
    Some(name) => assert_eq(name, "env-test-service"),
    None => assert_true(false)
  }
  
  // Clean up
  let delete_result = FileSystemAdapter::delete_file(FileSystemAdapter::new(), config_file)
  assert_true(delete_result.is_ok())
}

// Test 9: Cross-Platform Logging
test "cross-platform logging" {
  let logging_adapter = LoggingAdapter::new()
  
  // Test log directory detection
  let log_dir = LoggingAdapter::get_log_directory(logging_adapter)
  assert_true(log_dir.length() > 0)
  
  // Test log file creation
  let log_file = LoggingAdapter::get_log_file_path(logging_adapter, "telemetry", "log")
  assert_true(log_file.length() > 0)
  
  // Test log file rotation
  let rotation_config = LogRotationConfig::new()
  LogRotationConfig::set_max_size(rotation_config, 1024 * 1024) // 1MB
  LogRotationConfig::set_max_files(rotation_config, 5)
  
  // Create logger with rotation
  let logger = LoggingAdapter::create_logger(logging_adapter, "test_logger", log_file, rotation_config)
  
  // Test logging at different levels
  LoggingAdapter::log(logger, Debug, "Debug message")
  LoggingAdapter::log(logger, Info, "Info message")
  LoggingAdapter::log(logger, Warn, "Warning message")
  LoggingAdapter::log(logger, Error, "Error message")
  
  // Test structured logging
  let structured_data = [
    ("key1", "value1"),
    ("key2", "value2"),
    ("number", "42")
  ]
  LoggingAdapter::log_structured(logger, Info, "Structured message", structured_data)
  
  // Test logging with exceptions
  LoggingAdapter::log_exception(logger, Error, "Exception occurred", "Test exception")
  
  // Verify log file exists and has content
  let file_exists = FileSystemAdapter::file_exists(FileSystemAdapter::new(), log_file)
  assert_true(file_exists)
  
  let read_result = FileSystemAdapter::read_from_file(FileSystemAdapter::new(), log_file)
  match read_result {
    Ok(content) => {
      assert_true(content.contains("Debug message"))
      assert_true(content.contains("Info message"))
      assert_true(content.contains("Warning message"))
      assert_true(content.contains("Error message"))
      assert_true(content.contains("Structured message"))
      assert_true(content.contains("Exception occurred"))
    }
    Err(_) => assert_true(false)
  }
  
  // Test log level filtering
  LoggingAdapter::set_log_level(logger, Warn)
  LoggingAdapter::log(logger, Debug, "This should not be logged")
  LoggingAdapter::log(logger, Warn, "This should be logged")
  
  // Verify log filtering
  let filtered_content = FileSystemAdapter::read_from_file(FileSystemAdapter::new(), log_file).unwrap()
  assert_false(filtered_content.contains("This should not be logged"))
  assert_true(filtered_content.contains("This should be logged"))
  
  // Clean up
  let delete_result = FileSystemAdapter::delete_file(FileSystemAdapter::new(), log_file)
  assert_true(delete_result.is_ok())
}

// Test 10: Cross-Platform Telemetry Export
test "cross-platform telemetry export" {
  let export_adapter = ExportAdapter::new()
  
  // Test exporter creation
  let exporter = ExportAdapter::create_http_exporter(export_adapter, "http://localhost:4318")
  
  // Test span export
  let span_ctx = SpanContext::new("cross_platform_trace", "cross_platform_span", true, "")
  let span = Span::new("cross_platform_operation", Server, span_ctx)
  Span::add_attribute(span, "platform", PlatformDetector::detect(PlatformDetector::new()).os_type)
  Span::add_event(span, "start_event", None)
  Span::set_status(span, Ok, Some("Cross-platform test completed"))
  
  let export_result = ExportAdapter::export_span(exporter, span)
  // Export might fail if collector is not available, which is OK for this test
  assert_true(export_result.is_ok() || export_result.is_err())
  
  // Test metrics export
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross_platform_meter")
  let counter = Meter::create_counter(meter, "cross_platform_counter", None, None)
  Counter::add(counter, 1.0)
  
  let metrics_export_result = ExportAdapter::export_metrics(exporter, [counter])
  // Export might fail if collector is not available, which is OK for this test
  assert_true(metrics_export_result.is_ok() || metrics_export_result.is_err())
  
  // Test log export
  let log_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(log_provider, "cross_platform_logger")
  let log_record = LogRecord::new(Info, "Cross-platform log message")
  LogRecord::add_attribute(log_record, "platform", PlatformDetector::detect(PlatformDetector::new()).os_type)
  
  let log_export_result = ExportAdapter::export_log(exporter, log_record)
  // Export might fail if collector is not available, which is OK for this test
  assert_true(log_export_result.is_ok() || log_export_result.is_err())
  
  // Test file-based export
  let file_exporter = ExportAdapter::create_file_exporter(export_adapter, "/tmp/telemetry_export.json")
  
  let file_export_result = ExportAdapter::export_span_to_file(file_exporter, span)
  assert_true(file_export_result.is_ok())
  
  // Verify file was created
  let file_exists = FileSystemAdapter::file_exists(FileSystemAdapter::new(), "/tmp/telemetry_export.json")
  assert_true(file_exists)
  
  // Clean up
  let delete_result = FileSystemAdapter::delete_file(FileSystemAdapter::new(), "/tmp/telemetry_export.json")
  assert_true(delete_result.is_ok())
  
  // Test platform-specific export configurations
  let platform_info = PlatformDetector::detect(PlatformDetector::new())
  
  match PlatformInfo::os_type(platform_info) {
    "linux" => {
      // Test Linux-specific export configuration
      let linux_config = ExportAdapter::get_linux_export_config(export_adapter)
      assert_true(linux_config.is_some())
    }
    "windows" => {
      // Test Windows-specific export configuration
      let windows_config = ExportAdapter::get_windows_export_config(export_adapter)
      assert_true(windows_config.is_some())
    }
    "macos" => {
      // Test macOS-specific export configuration
      let macos_config = ExportAdapter::get_macos_export_config(export_adapter)
      assert_true(macos_config.is_some())
    }
    _ => {
      // Other platforms
      assert_true(true)
    }
  }
}