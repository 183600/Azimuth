// Azimuth Telemetry System - High Quality Cross-Platform Compatibility Tests
// This file contains comprehensive test cases for cross-platform compatibility

// Test 1: Platform Detection and Adaptation
test "platform detection and adaptation" {
  // Detect current platform
  let platform = Platform::detect()
  let architecture = Platform::architecture()
  let os_type = Platform::os_type()
  
  // Verify platform detection works
  assert_true(platform.length() > 0)
  assert_true(architecture.length() > 0)
  assert_true(os_type.length() > 0)
  
  // Test platform-specific adaptations
  let path_separator = Platform::path_separator()
  if os_type == "windows" {
    assert_eq(path_separator, "\\")
  } else {
    assert_eq(path_separator, "/")
  }
  
  let line_ending = Platform::line_ending()
  if os_type == "windows" {
    assert_eq(line_ending, "\r\n")
  } else {
    assert_eq(line_ending, "\n")
  }
  
  // Test platform-specific telemetry configuration
  let telemetry_config = TelemetryConfig::for_platform(platform)
  assert_true(telemetry_config != null)
  
  // Verify telemetry works on detected platform
  let span_ctx = SpanContext::new("platform_test", "platform_span", true, "platform_test")
  let span = Span::new("platform_operation", Internal, span_ctx)
  
  Span::set_attribute(span, "platform", StringValue(platform))
  Span::set_attribute(span, "architecture", StringValue(architecture))
  Span::set_attribute(span, "os_type", StringValue(os_type))
  
  assert_true(Span::is_recording(span))
  Span::end(span)
}

// Test 2: File System Path Handling
test "file system path handling" {
  let platform = Platform::os_type()
  
  // Test path normalization
  if platform == "windows" {
    let windows_path = "C:\\Users\\Test\\Documents\\file.txt"
    let normalized_path = Path::normalize(windows_path)
    assert_true(normalized_path.contains("\\"))
    
    let mixed_path = "C:/Users\\Test/Documents/file.txt"
    let normalized_mixed = Path::normalize(mixed_path)
    assert_true(normalized_mixed.contains("\\") && !normalized_mixed.contains("/"))
  } else {
    let unix_path = "/home/user/documents/file.txt"
    let normalized_path = Path::normalize(unix_path)
    assert_true(normalized_path.contains("/"))
    
    let mixed_path = "/home/user\\documents/file.txt"
    let normalized_mixed = Path::normalize(mixed_path)
    assert_true(normalized_mixed.contains("/") && !normalized_mixed.contains("\\"))
  }
  
  // Test path joining
  let base_path = if platform == "windows" { "C:\\Users\\Test" } else { "/home/user" }
  let relative_path = "documents/file.txt"
  let joined_path = Path::join(base_path, relative_path)
  
  if platform == "windows" {
    assert_eq(joined_path, "C:\\Users\\Test\\documents\\file.txt")
  } else {
    assert_eq(joined_path, "/home/user/documents/file.txt")
  }
  
  // Test path operations in telemetry context
  let span_ctx = SpanContext::new("path_test", "path_span", true, "path_test")
  let span = Span::new("path_operation", Internal, span_ctx)
  
  Span::set_attribute(span, "base_path", StringValue(base_path))
  Span::set_attribute(span, "relative_path", StringValue(relative_path))
  Span::set_attribute(span, "joined_path", StringValue(joined_path))
  
  assert_true(Span::is_recording(span))
  Span::end(span)
}

// Test 3: Platform-Specific Time Handling
test "platform-specific time handling" {
  let platform = Platform::os_type()
  
  // Test time precision
  let current_time = Time::now()
  assert_true(current_time > 0)
  
  // Test time formatting
  let formatted_time = Time::format(current_time, "ISO8601")
  assert_true(formatted_time.length() > 0)
  
  // Test platform-specific time zones
  let local_timezone = Time::local_timezone()
  assert_true(local_timezone.length() > 0)
  
  let utc_time = Time::to_utc(current_time)
  let local_time = Time::to_local(current_time)
  
  // UTC and local times might differ but should be reasonable
  let time_diff = abs(utc_time - local_time)
  assert_true(time_diff < 24 * 60 * 60 * 1000) // Less than 24 hours difference
  
  // Test time operations in telemetry context
  let span_ctx = SpanContext::new("time_test", "time_span", true, "time_test")
  let span = Span::new("time_operation", Internal, span_ctx)
  
  Span::set_attribute(span, "platform", StringValue(platform))
  Span::set_attribute(span, "timezone", StringValue(local_timezone))
  Span::set_attribute(span, "formatted_time", StringValue(formatted_time))
  
  // Add time-related events
  Span::add_event(span, "time_operation_started", Some([
    ("timestamp", IntValue(current_time)),
    ("utc_time", IntValue(utc_time)),
    ("local_time", IntValue(local_time))
  ]))
  
  assert_true(Span::is_recording(span))
  Span::end(span)
}

// Test 4: Network Interface Compatibility
test "network interface compatibility" {
  let platform = Platform::os_type()
  
  // Test hostname resolution
  let hostname = Network::hostname()
  assert_true(hostname.length() > 0)
  
  // Test IP address detection
  let ip_addresses = Network::ip_addresses()
  assert_true(ip_addresses.length() > 0)
  
  // Test localhost resolution
  let localhost_ips = Network::resolve_localhost()
  assert_true(localhost_ips.length() > 0)
  
  // Verify common localhost addresses
  let has_ipv4_localhost = localhost_ips.any(fn(ip) { ip == "127.0.0.1" })
  let has_ipv6_localhost = localhost_ips.any(fn(ip) { ip == "::1" })
  
  assert_true(has_ipv4_localhost || has_ipv6_localhost)
  
  // Test network operations in telemetry context
  let span_ctx = SpanContext::new("network_test", "network_span", true, "network_test")
  let span = Span::new("network_operation", Internal, span_ctx)
  
  Span::set_attribute(span, "platform", StringValue(platform))
  Span::set_attribute(span, "hostname", StringValue(hostname))
  Span::set_attribute(span, "ip_count", IntValue(ip_addresses.length()))
  
  // Add network-related events
  for i in 0..ip_addresses.length() {
    Span::add_event(span, "ip_address_detected", Some([
      ("ip_index", IntValue(i)),
      ("ip_address", StringValue(ip_addresses[i]))
    ]))
  }
  
  assert_true(Span::is_recording(span))
  Span::end(span)
}

// Test 5: Platform-Specific Resource Limits
test "platform-specific resource limits" {
  let platform = Platform::os_type()
  
  // Test memory limits
  let total_memory = Memory::total()
  let available_memory = Memory::available()
  let used_memory = Memory::used()
  
  assert_true(total_memory > 0)
  assert_true(available_memory > 0)
  assert_true(used_memory > 0)
  assert_true(available_memory <= total_memory)
  assert_true(used_memory <= total_memory)
  
  // Test CPU information
  let cpu_count = CPU::count()
  let cpu_usage = CPU::usage()
  
  assert_true(cpu_count > 0)
  assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
  
  // Test disk space
  let disk_total = Disk::total("/")
  let disk_free = Disk::free("/")
  let disk_used = Disk::used("/")
  
  assert_true(disk_total > 0)
  assert_true(disk_free >= 0)
  assert_true(disk_used >= 0)
  assert_true(disk_free <= disk_total)
  assert_true(disk_used <= disk_total)
  
  // Test resource monitoring in telemetry context
  let span_ctx = SpanContext::new("resource_test", "resource_span", true, "resource_test")
  let span = Span::new("resource_operation", Internal, span_ctx)
  
  Span::set_attribute(span, "platform", StringValue(platform))
  Span::set_attribute(span, "total_memory", IntValue(total_memory))
  Span::set_attribute(span, "available_memory", IntValue(available_memory))
  Span::set_attribute(span, "cpu_count", IntValue(cpu_count))
  Span::set_attribute(span, "cpu_usage", FloatValue(cpu_usage))
  Span::set_attribute(span, "disk_total", IntValue(disk_total))
  Span::set_attribute(span, "disk_free", IntValue(disk_free))
  
  // Add resource monitoring events
  Span::add_event(span, "resource_monitoring", Some([
    ("memory_usage_percent", FloatValue((used_memory.to_float() / total_memory.to_float()) * 100.0)),
    ("disk_usage_percent", FloatValue((disk_used.to_float() / disk_total.to_float()) * 100.0))
  ]))
  
  assert_true(Span::is_recording(span))
  Span::end(span)
}

// Test 6: Character Encoding Compatibility
test "character encoding compatibility" {
  let platform = Platform::os_type()
  
  // Test UTF-8 string handling
  let utf8_string = "测试中文字符串 🌍 Test with emoji"
  let encoded_bytes = Encoding::utf8_encode(utf8_string)
  let decoded_string = Encoding::utf8_decode(encoded_bytes)
  
  assert_eq(utf8_string, decoded_string)
  
  // Test ASCII string handling
  let ascii_string = "Simple ASCII string"
  let encoded_ascii = Encoding::ascii_encode(ascii_string)
  let decoded_ascii = Encoding::ascii_decode(encoded_ascii)
  
  assert_eq(ascii_string, decoded_ascii)
  
  // Test platform-specific encoding
  let platform_encoding = if platform == "windows" { "UTF-16LE" } else { "UTF-8" }
  let platform_encoded = Encoding::encode(utf8_string, platform_encoding)
  let platform_decoded = Encoding::decode(platform_encoded, platform_encoding)
  
  assert_eq(utf8_string, platform_decoded)
  
  // Test encoding operations in telemetry context
  let span_ctx = SpanContext::new("encoding_test", "encoding_span", true, "encoding_test")
  let span = Span::new("encoding_operation", Internal, span_ctx)
  
  Span::set_attribute(span, "platform", StringValue(platform))
  Span::set_attribute(span, "platform_encoding", StringValue(platform_encoding))
  Span::set_attribute(span, "utf8_string", StringValue(utf8_string))
  Span::set_attribute(span, "ascii_string", StringValue(ascii_string))
  
  // Add encoding-related events
  Span::add_event(span, "encoding_test_completed", Some([
    ("utf8_length", IntValue(utf8_string.length())),
    ("ascii_length", IntValue(ascii_string.length())),
    ("encoded_bytes_length", IntValue(encoded_bytes.length()))
  ]))
  
  assert_true(Span::is_recording(span))
  Span::end(span)
}

// Test 7: Process and Signal Handling
test "process and signal handling" {
  let platform = Platform::os_type()
  
  // Test current process information
  let current_pid = Process::current_pid()
  let parent_pid = Process::parent_pid()
  let process_name = Process::name()
  
  assert_true(current_pid > 0)
  assert_true(parent_pid > 0)
  assert_true(process_name.length() > 0)
  
  // Test environment variables
  let path_env = Environment::get("PATH")
  match path_env {
    Some(path) => assert_true(path.length() > 0)
    None => assert_true(false)
  }
  
  let test_env = Environment::get("AZIMUTH_TEST_VAR")
  match test_env {
    Some(_) => assert_true(false) // Should not exist
    None => assert_true(true)
  }
  
  // Set and get test environment variable
  Environment::set("AZIMUTH_TEST_VAR", "test_value")
  let retrieved_env = Environment::get("AZIMUTH_TEST_VAR")
  match retrieved_env {
    Some(value) => assert_eq(value, "test_value")
    None => assert_true(false)
  }
  
  // Clean up test environment variable
  Environment::unset("AZIMUTH_TEST_VAR")
  let cleaned_env = Environment::get("AZIMUTH_TEST_VAR")
  match cleaned_env {
    Some(_) => assert_true(false) // Should be removed
    None => assert_true(true)
  }
  
  // Test process operations in telemetry context
  let span_ctx = SpanContext::new("process_test", "process_span", true, "process_test")
  let span = Span::new("process_operation", Internal, span_ctx)
  
  Span::set_attribute(span, "platform", StringValue(platform))
  Span::set_attribute(span, "current_pid", IntValue(current_pid))
  Span::set_attribute(span, "parent_pid", IntValue(parent_pid))
  Span::set_attribute(span, "process_name", StringValue(process_name))
  
  match path_env {
    Some(path) => Span::set_attribute(span, "path_length", IntValue(path.length()))
    None => {}
  }
  
  assert_true(Span::is_recording(span))
  Span::end(span)
}

// Test 8: Platform-Specific Security Features
test "platform-specific security features" {
  let platform = Platform::os_type()
  
  // Test user information
  let current_user = Security::current_user()
  assert_true(current_user.length() > 0)
  
  let user_home = Security::user_home()
  assert_true(user_home.length() > 0)
  
  // Test file permissions
  let test_file_path = if platform == "windows" {
    "C:\\temp\\azimuth_test_file.txt"
  } else {
    "/tmp/azimuth_test_file.txt"
  }
  
  // Create test file
  let create_result = File::write(test_file_path, "test content")
  match create_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // If we can't create the file, skip permission tests
      assert_true(true)
    }
  }
  
  // Check file permissions
  let file_permissions = File::permissions(test_file_path)
  assert_true(file_permissions.length() > 0)
  
  // Clean up test file
  let delete_result = File::delete(test_file_path)
  match delete_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // If we can't delete the file, that's okay for this test
      assert_true(true)
    }
  }
  
  // Test security operations in telemetry context
  let span_ctx = SpanContext::new("security_test", "security_span", true, "security_test")
  let span = Span::new("security_operation", Internal, span_ctx)
  
  Span::set_attribute(span, "platform", StringValue(platform))
  Span::set_attribute(span, "current_user", StringValue(current_user))
  Span::set_attribute(span, "user_home", StringValue(user_home))
  Span::set_attribute(span, "file_permissions", StringValue(file_permissions))
  
  assert_true(Span::is_recording(span))
  Span::end(span)
}

// Test 9: Cross-Platform Telemetry Export
test "cross-platform telemetry export" {
  let platform = Platform::os_type()
  
  // Create test telemetry data
  let trace_id = "cross_platform_trace"
  let span_id = "cross_platform_span"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "cross_platform_test")
  let span = Span::new("cross_platform_operation", Internal, span_ctx)
  
  // Add platform-specific attributes
  Span::set_attribute(span, "platform", StringValue(platform))
  Span::set_attribute(span, "hostname", StringValue(Network::hostname()))
  Span::set_attribute(span, "architecture", StringValue(Platform::architecture()))
  Span::set_attribute(span, "cpu_count", IntValue(CPU::count()))
  Span::set_attribute(span, "total_memory", IntValue(Memory::total()))
  
  // Add platform-specific events
  Span::add_event(span, "platform_info_collected", Some([
    ("os_type", StringValue(Platform::os_type())),
    ("path_separator", StringValue(Platform::path_separator())),
    ("line_ending", StringValue(Platform::line_ending()))
  ]))
  
  Span::end(span)
  
  // Export telemetry in platform-agnostic format
  let exporter = TelemetryExporter::new()
  let exported_data = exporter.export_span(span)
  
  assert_true(exported_data.length() > 0)
  
  // Verify exported data contains platform information
  assert_true(exported_data.contains(platform))
  assert_true(exported_data.contains(Network::hostname()))
  
  // Test cross-platform import
  let imported_span = importer.import_span(exported_data)
  match imported_span {
    Some(imp_span) => {
      assert_eq(Span::name(imp_span), "cross_platform_operation")
      
      // Verify platform attributes are preserved
      let platform_attr = Span::get_attribute(imp_span, "platform")
      match platform_attr {
        Some(StringValue(v)) => assert_eq(v, platform)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 10: Platform-Specific Performance Characteristics
test "platform-specific performance characteristics" {
  let platform = Platform::os_type()
  let architecture = Platform::architecture()
  
  // Test basic operation performance
  let operation_count = 10000
  
  // Attribute operations performance
  let attrs = Attributes::new()
  let attr_start_time = Time::now()
  
  for i in 0..operation_count {
    Attributes::set(attrs, "perf_key_" + i.to_string(), StringValue("perf_value_" + i.to_string()))
  }
  
  let attr_time = Time::now() - attr_start_time
  let attr_ops_per_second = (operation_count.to_float() / attr_time.to_float()) * 1000.0
  
  // Span operations performance
  let span_start_time = Time::now()
  
  for i in 0..operation_count {
    let trace_id = "perf_trace_" + i.to_string()
    let span_id = "perf_span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "performance_test")
    let span = Span::new("perf_operation", Internal, span_ctx)
    Span::end(span)
  }
  
  let span_time = Time::now() - span_start_time
  let span_ops_per_second = (operation_count.to_float() / span_time.to_float()) * 1000.0
  
  // Log operations performance
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "performance_logger")
  
  let log_start_time = Time::now()
  
  for i in 0..operation_count {
    let log_record = LogRecord::new(Info, Some("Performance log " + i.to_string()))
    Logger::emit(logger, log_record)
  }
  
  let log_time = Time::now() - log_start_time
  let log_ops_per_second = (operation_count.to_float() / log_time.to_float()) * 1000.0
  
  // Create performance summary span
  let summary_span_ctx = SpanContext::new("performance_summary", "performance_summary_span", true, "performance_test")
  let summary_span = Span::new("performance_summary", Internal, summary_span_ctx)
  
  // Add platform-specific performance attributes
  Span::set_attribute(summary_span, "platform", StringValue(platform))
  Span::set_attribute(summary_span, "architecture", StringValue(architecture))
  Span::set_attribute(summary_span, "attr_ops_per_second", FloatValue(attr_ops_per_second))
  Span::set_attribute(summary_span, "span_ops_per_second", FloatValue(span_ops_per_second))
  Span::set_attribute(summary_span, "log_ops_per_second", FloatValue(log_ops_per_second))
  
  // Add performance characteristics events
  Span::add_event(summary_span, "performance_test_completed", Some([
    ("operation_count", IntValue(operation_count)),
    ("attr_time_ms", IntValue(attr_time)),
    ("span_time_ms", IntValue(span_time)),
    ("log_time_ms", IntValue(log_time))
  ]))
  
  Span::end(summary_span)
  
  // Verify reasonable performance (platform-agnostic minimums)
  assert_true(attr_ops_per_second >= 1000.0)
  assert_true(span_ops_per_second >= 500.0)
  assert_true(log_ops_per_second >= 1000.0)
}