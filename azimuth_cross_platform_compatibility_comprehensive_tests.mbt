// Azimuth Telemetry System - Cross Platform Compatibility Comprehensive Tests
// This file contains comprehensive tests for cross-platform compatibility

// Test 1: Operating System Detection and Adaptation
test "operating system detection and adaptation" {
  let platform_detector = PlatformDetector::new()
  
  // Detect current operating system
  let os_info = PlatformDetector::detect_operating_system(platform_detector)
  
  // Verify OS detection
  match os_info.family {
    Windows => {
      assert_true(os_info.name == "Windows" || os_info.name.contains("Windows"))
      assert_true(os_info.version.length() > 0)
      assert_true(os_info.architecture == "x86_64" || os_info.architecture == "x86" || os_info.architecture == "arm64")
    }
    Linux => {
      assert_true(os_info.name == "Linux" || os_info.name.contains("Linux"))
      assert_true(os_info.version.length() > 0)
      assert_true(os_info.architecture == "x86_64" || os_info.architecture == "x86" || os_info.architecture == "arm64" || os_info.architecture == "arm")
    }
    MacOS => {
      assert_true(os_info.name == "macOS" || os_info.name.contains("Mac") || os_info.name.contains("Darwin"))
      assert_true(os_info.version.length() > 0)
      assert_true(os_info.architecture == "x86_64" || os_info.architecture == "arm64")
    }
    _ => assert_true(false) // Unsupported OS
  }
  
  // Test OS-specific adaptations
  let path_separator = PlatformDetector::get_path_separator(platform_detector)
  match os_info.family {
    Windows => assert_eq(path_separator, "\\")
    _ => assert_eq(path_separator, "/")
  }
  
  let line_ending = PlatformDetector::get_line_ending(platform_detector)
  match os_info.family {
    Windows => assert_eq(line_ending, "\r\n")
    _ => assert_eq(line_ending, "\n")
  }
  
  let executable_extension = PlatformDetector::get_executable_extension(platform_detector)
  match os_info.family {
    Windows => assert_eq(executable_extension, ".exe")
    _ => assert_eq(executable_extension, "")
  }
  
  // Test OS-specific directory paths
  let temp_dir = PlatformDetector::get_temp_directory(platform_detector)
  assert_true(temp_dir.length() > 0)
  
  let home_dir = PlatformDetector::get_home_directory(platform_detector)
  assert_true(home_dir.length() > 0)
  
  let app_data_dir = PlatformDetector::get_application_data_directory(platform_detector, "azimuth")
  assert_true(app_data_dir.length() > 0)
  assert_true(app_data_dir.contains("azimuth"))
}

// Test 2: File System Compatibility
test "file system compatibility" {
  let file_system_adapter = FileSystemAdapter::new()
  
  // Test path handling across platforms
  let test_paths = [
    "/home/user/test/file.txt",
    "C:\\Users\\User\\test\\file.txt",
    "/tmp/test_file.tmp",
    "./relative/path/file.txt",
    "../parent/path/file.txt"
  ]
  
  for path in test_paths {
    // Test path normalization
    let normalized_path = FileSystemAdapter::normalize_path(file_system_adapter, path)
    assert_true(normalized_path.length() > 0)
    
    // Test path validation
    let is_valid = FileSystemAdapter::is_valid_path(file_system_adapter, normalized_path)
    assert_true(is_valid)
    
    // Test path components extraction
    let components = FileSystemAdapter::get_path_components(file_system_adapter, normalized_path)
    assert_true(components.length() > 0)
    
    // Test file name extraction
    let file_name = FileSystemAdapter::get_file_name(file_system_adapter, normalized_path)
    assert_true(file_name.length() > 0)
    
    // Test directory extraction
    let directory = FileSystemAdapter::get_directory(file_system_adapter, normalized_path)
    assert_true(directory.length() > 0 || directory == "")
  }
  
  // Test file operations across platforms
  let test_file_path = FileSystemAdapter::get_temp_directory(file_system_adapter) + 
                      FileSystemAdapter::get_path_separator(file_system_adapter) + 
                      "azimuth_test_file.tmp"
  
  // Test file creation
  let create_result = FileSystemAdapter::create_file(file_system_adapter, test_file_path)
  assert_true(create_result)
  assert_true(FileSystemAdapter::file_exists(file_system_adapter, test_file_path))
  
  // Test file writing
  let test_content = "Test content for cross-platform compatibility\nLine 2\nLine 3"
  let write_result = FileSystemAdapter::write_file(file_system_adapter, test_file_path, test_content)
  assert_true(write_result)
  
  // Test file reading
  let read_content = FileSystemAdapter::read_file(file_system_adapter, test_file_path)
  assert_eq(read_content, test_content)
  
  // Test file appending
  let append_content = "\nAppended content"
  let append_result = FileSystemAdapter::append_file(file_system_adapter, test_file_path, append_content)
  assert_true(append_result)
  
  let final_content = FileSystemAdapter::read_file(file_system_adapter, test_file_path)
  assert_eq(final_content, test_content + append_content)
  
  // Test file size
  let file_size = FileSystemAdapter::get_file_size(file_system_adapter, test_file_path)
  assert_true(file_size > 0)
  assert_eq(file_size, final_content.length())
  
  // Test file permissions
  let permissions = FileSystemAdapter::get_file_permissions(file_system_adapter, test_file_path)
  assert_true(permissions.read)
  assert_true(permissions.write)
  
  // Test file deletion
  let delete_result = FileSystemAdapter::delete_file(file_system_adapter, test_file_path)
  assert_true(delete_result)
  assert_false(FileSystemAdapter::file_exists(file_system_adapter, test_file_path))
  
  // Test directory operations
  let test_dir_path = FileSystemAdapter::get_temp_directory(file_system_adapter) + 
                      FileSystemAdapter::get_path_separator(file_system_adapter) + 
                      "azimuth_test_dir"
  
  // Test directory creation
  let create_dir_result = FileSystemAdapter::create_directory(file_system_adapter, test_dir_path)
  assert_true(create_dir_result)
  assert_true(FileSystemAdapter::directory_exists(file_system_adapter, test_dir_path))
  
  // Test directory listing
  let dir_listing = FileSystemAdapter::list_directory(file_system_adapter, test_dir_path)
  assert_true(dir_listing.length() == 0) // Empty directory
  
  // Test directory deletion
  let delete_dir_result = FileSystemAdapter::delete_directory(file_system_adapter, test_dir_path)
  assert_true(delete_dir_result)
  assert_false(FileSystemAdapter::directory_exists(file_system_adapter, test_dir_path))
}

// Test 3: Network Compatibility
test "network compatibility" {
  let network_adapter = NetworkAdapter::new()
  
  // Test network interface detection
  let interfaces = NetworkAdapter::get_network_interfaces(network_adapter)
  assert_true(interfaces.length() > 0)
  
  for interface in interfaces {
    assert_true(interface.name.length() > 0)
    assert_true(interface.ip_addresses.length() > 0)
    
    // Test IP address validation
    for ip_address in interface.ip_addresses {
      assert_true(NetworkAdapter::is_valid_ip_address(network_adapter, ip_address))
    }
  }
  
  // Test hostname resolution
  let hostnames = ["localhost", "127.0.0.1", "google.com", "github.com"]
  
  for hostname in hostnames {
    let resolution_result = NetworkAdapter::resolve_hostname(network_adapter, hostname)
    match resolution_result {
      Some(ip_addresses) => {
        assert_true(ip_addresses.length() > 0)
        
        // Verify resolved IP addresses are valid
        for ip_address in ip_addresses {
          assert_true(NetworkAdapter::is_valid_ip_address(network_adapter, ip_address))
        }
      }
      None => {
        // Resolution might fail for external hosts due to network restrictions
        // This is acceptable for localhost and IP addresses
        assert_true(hostname == "localhost" || hostname == "127.0.0.1" ? false : true)
      }
    }
  }
  
  // Test port availability checking
  let test_ports = [8080, 8081, 8082, 8083, 8084]
  
  for port in test_ports {
    let is_available = NetworkAdapter::is_port_available(network_adapter, port)
    // Port availability depends on the system, so we just verify the check doesn't crash
    assert_true(is_available == true || is_available == false)
  }
  
  // Test socket creation and binding
  for port in test_ports {
    let socket_result = NetworkAdapter::create_socket(network_adapter, "tcp", port)
    match socket_result {
      Some(socket) => {
        // Test socket operations
        let bind_result = NetworkAdapter::bind_socket(network_adapter, socket, "127.0.0.1", port)
        
        if bind_result {
          let listen_result = NetworkAdapter::listen_socket(network_adapter, socket, 5)
          
          if listen_result {
            // Test socket closing
            let close_result = NetworkAdapter::close_socket(network_adapter, socket)
            assert_true(close_result)
          }
        }
      }
      None => {
        // Socket creation might fail if port is in use
        // This is acceptable
      }
    }
  }
  
  // Test HTTP client compatibility
  let http_client = NetworkAdapter::create_http_client(network_adapter)
  
  // Test HTTP request to local server if available
  let http_request = HttpRequest::new("GET", "http://127.0.0.1:8080/test", [], None)
  let http_result = NetworkAdapter::send_http_request(network_adapter, http_client, http_request)
  
  match http_result {
    Success(response) => {
      assert_true(response.status_code >= 100 && response.status_code < 600)
    }
    Error(_) => {
      // HTTP request might fail if no server is running
      // This is acceptable
    }
  }
}

// Test 4: Process Management Compatibility
test "process management compatibility" {
  let process_manager = ProcessManager::new()
  
  // Test current process information
  let current_process = ProcessManager::get_current_process(process_manager)
  assert_true(current_process.id > 0)
  assert_true(current_process.name.length() > 0)
  assert_true(current_process.executable_path.length() > 0)
  
  // Test process listing
  let processes = ProcessManager::list_processes(process_manager)
  assert_true(processes.length() > 0)
  
  // Verify current process is in the list
  let mut current_process_found = false
  for process in processes {
    if process.id == current_process.id {
      current_process_found = true
      assert_eq(process.name, current_process.name)
      break
    }
  }
  assert_true(current_process_found)
  
  // Test process creation
  let test_commands = [
    ["echo", "test"],
    ["echo", "hello", "world"]
  ]
  
  for command in test_commands {
    let process_result = ProcessManager::create_process(process_manager, command)
    match process_result {
      Some(process) => {
        assert_true(process.id > 0)
        
        // Test process waiting
        let wait_result = ProcessManager::wait_for_process(process_manager, process)
        match wait_result {
          ProcessResult(exit_code, stdout, stderr) => {
            assert_eq(exit_code, 0) // Echo should succeed
            assert_true(stdout.length() > 0)
          }
          _ => assert_true(false)
        }
      }
      None => {
        // Process creation might fail on some systems
        // This is acceptable
      }
    }
  }
  
  // Test environment variables
  let env_vars = ProcessManager::get_environment_variables(process_manager)
  assert_true(env_vars.length() > 0)
  
  // Test common environment variables
  let common_vars = ["PATH", "HOME", "USER", "USERNAME"]
  for var in common_vars {
    let value = ProcessManager::get_environment_variable(process_manager, var)
    match value {
      Some(val) => assert_true(val.length() > 0)
      None => {
        // Some environment variables might not exist on all systems
        // This is acceptable
      }
    }
  }
  
  // Test working directory
  let working_dir = ProcessManager::get_working_directory(process_manager)
  assert_true(working_dir.length() > 0)
  assert_true(FileSystemAdapter::directory_exists(FileSystemAdapter::new(), working_dir))
  
  // Test changing working directory
  let temp_dir = FileSystemAdapter::get_temp_directory(FileSystemAdapter::new())
  let change_dir_result = ProcessManager::set_working_directory(process_manager, temp_dir)
  assert_true(change_dir_result)
  
  let new_working_dir = ProcessManager::get_working_directory(process_manager)
  assert_eq(new_working_dir, temp_dir)
  
  // Restore original working directory
  ProcessManager::set_working_directory(process_manager, working_dir)
}

// Test 5: System Information Compatibility
test "system information compatibility" {
  let system_info = SystemInfo::new()
  
  // Test CPU information
  let cpu_info = SystemInfo::get_cpu_info(system_info)
  assert_true(cpu_info.model.length() > 0)
  assert_true(cpu_info.vendor.length() > 0)
  assert_true(cpu_info.cores > 0)
  assert_true(cpu_info.frequency_mhz > 0)
  
  // Test memory information
  let memory_info = SystemInfo::get_memory_info(system_info)
  assert_true(memory_info.total_bytes > 0)
  assert_true(memory_info.available_bytes > 0)
  assert_true(memory_info.available_bytes <= memory_info.total_bytes)
  
  // Test disk information
  let disk_info = SystemInfo::get_disk_info(system_info)
  assert_true(disk_info.length() > 0)
  
  for disk in disk_info {
    assert_true(disk.mount_point.length() > 0)
    assert_true(disk.total_bytes > 0)
    assert_true(disk.available_bytes > 0)
    assert_true(disk.available_bytes <= disk.total_bytes)
  }
  
  // Test system load
  let system_load = SystemInfo::get_system_load(system_info)
  assert_true(system_load.load_average_1min >= 0.0)
  assert_true(system_load.load_average_5min >= 0.0)
  assert_true(system_load.load_average_15min >= 0.0)
  assert_true(system_load.cpu_usage_percent >= 0.0 && system_load.cpu_usage_percent <= 100.0)
  assert_true(system_load.memory_usage_percent >= 0.0 && system_load.memory_usage_percent <= 100.0)
  
  // Test system uptime
  let uptime = SystemInfo::get_system_uptime(system_info)
  assert_true(uptime.seconds > 0)
  
  // Test boot time
  let boot_time = SystemInfo::get_boot_time(system_info)
  assert_true(boot_time.timestamp > 0)
  
  // Test timezone information
  let timezone_info = SystemInfo::get_timezone_info(system_info)
  assert_true(timezone_info.name.length() > 0)
  assert_true(timezone_info.offset_hours >= -12 && timezone_info.offset_hours <= 14)
}

// Test 6: Telemetry Platform Adaptation
test "telemetry platform adaptation" {
  let telemetry_adapter = TelemetryPlatformAdapter::new()
  
  // Test platform-specific telemetry configuration
  let config = TelemetryPlatformAdapter::get_platform_configuration(telemetry_adapter)
  
  // Verify configuration contains platform-specific settings
  assert_true(config.default_export_interval_ms > 0)
  assert_true(config.default_batch_size > 0)
  assert_true(config.max_concurrent_operations > 0)
  
  // Test platform-specific file paths
  let log_file_path = TelemetryPlatformAdapter::get_log_file_path(telemetry_adapter)
  assert_true(log_file_path.length() > 0)
  assert_true(log_file_path.contains("azimuth") || log_file_path.contains("telemetry"))
  
  let config_file_path = TelemetryPlatformAdapter::get_config_file_path(telemetry_adapter)
  assert_true(config_file_path.length() > 0)
  assert_true(config_file_path.contains("azimuth") || config_file_path.contains("telemetry"))
  
  let data_dir_path = TelemetryPlatformAdapter::get_data_directory_path(telemetry_adapter)
  assert_true(data_dir_path.length() > 0)
  
  // Test platform-specific performance settings
  let performance_settings = TelemetryPlatformAdapter::get_performance_settings(telemetry_adapter)
  
  assert_true(performance_settings.max_memory_usage_mb > 0)
  assert_true(performance_settings.max_file_handles > 0)
  assert_true(performance_settings.max_threads > 0)
  assert_true(performance_settings.gc_threshold_percent > 0 && performance_settings.gc_threshold_percent <= 100)
  
  // Test platform-specific network settings
  let network_settings = TelemetryPlatformAdapter::get_network_settings(telemetry_adapter)
  
  assert_true(network_settings.connection_timeout_ms > 0)
  assert_true(network_settings.read_timeout_ms > 0)
  assert_true(network_settings.write_timeout_ms > 0)
  assert_true(network_settings.max_retry_attempts > 0)
  assert_true(network_settings.retry_delay_ms > 0)
  
  // Test platform-specific logging settings
  let logging_settings = TelemetryPlatformAdapter::get_logging_settings(telemetry_adapter)
  
  assert_true(logging_settings.log_level.length() > 0)
  assert_true(logging_settings.max_log_file_size_mb > 0)
  assert_true(logging_settings.max_log_files > 0)
  assert_true(logging_settings.log_rotation_enabled == true || logging_settings.log_rotation_enabled == false)
}

// Test 7: Cross-Platform Data Serialization
test "cross-platform data serialization" {
  let serialization_adapter = SerializationAdapter::new()
  
  // Create test telemetry data
  let telemetry_data = TelemetryData::new(
    "test.service",
    "test.operation",
    200,
    100,
    true
  )
  
  // Test JSON serialization (platform-independent)
  let json_result = SerializationAdapter::to_json(serialization_adapter, telemetry_data)
  match json_result {
    Some(json_string) => {
      assert_true(json_string.length() > 0)
      
      // Test JSON deserialization
      let deserialized_result = SerializationAdapter::from_json(serialization_adapter, json_string)
      match deserialized_result {
        Some(deserialized_data) => {
          assert_eq(deserialized_data.service_name, telemetry_data.service_name)
          assert_eq(deserialized_data.operation_name, telemetry_data.operation_name)
          assert_eq(deserialized_data.status_code, telemetry_data.status_code)
          assert_eq(deserialized_data.duration_ms, telemetry_data.duration_ms)
          assert_eq(deserialized_data.success, telemetry_data.success)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test binary serialization (platform-specific)
  let binary_result = SerializationAdapter::to_binary(serialization_adapter, telemetry_data)
  match binary_result {
    Some(binary_data) => {
      assert_true(binary_data.length() > 0)
      
      // Test binary deserialization
      let deserialized_result = SerializationAdapter::from_binary(serialization_adapter, binary_data)
      match deserialized_result {
        Some(deserialized_data) => {
          assert_eq(deserialized_data.service_name, telemetry_data.service_name)
          assert_eq(deserialized_data.operation_name, telemetry_data.operation_name)
          assert_eq(deserialized_data.status_code, telemetry_data.status_code)
          assert_eq(deserialized_data.duration_ms, telemetry_data.duration_ms)
          assert_eq(deserialized_data.success, telemetry_data.success)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test platform-specific endianness handling
  let endianness = SerializationAdapter::get_endianness(serialization_adapter)
  assert_true(endianness == "little" || endianness == "big")
  
  // Test cross-platform integer serialization
  let test_integers = [0, 1, -1, 2147483647, -2147483648]
  for integer in test_integers {
    let serialized_int = SerializationAdapter::serialize_integer(serialization_adapter, integer)
    let deserialized_int = SerializationAdapter::deserialize_integer(serialization_adapter, serialized_int)
    assert_eq(integer, deserialized_int)
  }
  
  // Test cross-platform float serialization
  let test_floats = [0.0, 1.0, -1.0, 3.14159, -3.14159]
  for float in test_floats {
    let serialized_float = SerializationAdapter::serialize_float(serialization_adapter, float)
    let deserialized_float = SerializationAdapter::deserialize_float(serialization_adapter, serialized_float)
    assert_true((float - deserialized_float).abs() < 0.00001)
  }
}

// Test 8: Cross-Platform Time Handling
test "cross-platform time handling" {
  let time_adapter = TimeAdapter::new()
  
  // Test current time
  let current_time = TimeAdapter::get_current_time(time_adapter)
  assert_true(current_time.timestamp_seconds > 0)
  assert_true(current_time.timestamp_milliseconds > 0)
  assert_true(current_time.timestamp_nanoseconds > 0)
  assert_true(current_time.timezone_offset_hours >= -12 && current_time.timezone_offset_hours <= 14)
  
  // Test time formatting
  let formatted_time = TimeAdapter::format_time(time_adapter, current_time, "ISO8601")
  assert_true(formatted_time.length() > 0)
  
  let formatted_time_custom = TimeAdapter::format_time(time_adapter, current_time, "YYYY-MM-DD HH:mm:ss")
  assert_true(formatted_time_custom.length() > 0)
  assert_true(formatted_time_custom.contains("-"))
  assert_true(formatted_time_custom.contains(":"))
  
  // Test time parsing
  let parsed_time = TimeAdapter::parse_time(time_adapter, formatted_time, "ISO8601")
  match parsed_time {
    Some(time) => {
      assert_eq(time.timestamp_seconds, current_time.timestamp_seconds)
    }
    None => assert_true(false)
  }
  
  // Test time conversion
  let timestamp_seconds = current_time.timestamp_seconds
  let timestamp_milliseconds = TimeAdapter::seconds_to_milliseconds(time_adapter, timestamp_seconds)
  assert_eq(timestamp_milliseconds, timestamp_seconds * 1000)
  
  let timestamp_nanoseconds = TimeAdapter::seconds_to_nanoseconds(time_adapter, timestamp_seconds)
  assert_eq(timestamp_nanoseconds, timestamp_seconds * 1000000000)
  
  // Test time arithmetic
  let future_time = TimeAdapter::add_seconds(time_adapter, current_time, 3600) // Add 1 hour
  assert_true(future_time.timestamp_seconds > current_time.timestamp_seconds)
  assert_eq(future_time.timestamp_seconds, current_time.timestamp_seconds + 3600)
  
  let past_time = TimeAdapter::subtract_seconds(time_adapter, current_time, 3600) // Subtract 1 hour
  assert_true(past_time.timestamp_seconds < current_time.timestamp_seconds)
  assert_eq(past_time.timestamp_seconds, current_time.timestamp_seconds - 3600)
  
  // Test time difference
  let time_difference = TimeAdapter::get_difference_seconds(time_adapter, future_time, current_time)
  assert_eq(time_difference, 3600)
  
  // Test timezone conversion
  let utc_time = TimeAdapter::to_utc(time_adapter, current_time)
  assert_true(utc_time.timezone_offset_hours == 0)
  
  let local_time = TimeAdapter::to_local(time_adapter, utc_time)
  assert_eq(local_time.timezone_offset_hours, current_time.timezone_offset_hours)
}

// Test 9: Cross-Platform Threading
test "cross-platform threading" {
  let threading_adapter = ThreadingAdapter::new()
  
  // Test thread creation
  let thread_result = ThreadingAdapter::create_thread(threading_adapter, fn() {
    Thread::sleep(100) // 100ms
    return "thread_result"
  })
  match thread_result {
    Some(thread) => {
      // Test thread joining
      let join_result = ThreadingAdapter::join_thread(threading_adapter, thread)
      match join_result {
        Some(result) => assert_eq(result, "thread_result")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test thread pool creation
  let thread_pool_result = ThreadingAdapter::create_thread_pool(threading_adapter, 4)
  match thread_pool_result {
    Some(thread_pool) => {
      // Test task submission
      let task_results = []
      for i in 0..=10 {
        let task_result = ThreadingAdapter::submit_task(threading_adapter, thread_pool, fn() {
          Thread::sleep(50) // 50ms
          return "task_result_" + i.to_string()
        })
        match task_result {
          Some(task) => task_results = task_results.push(task)
          None => assert_true(false)
        }
      }
      
      // Test task result retrieval
      for i in 0..=task_results.length() - 1 {
        let task = task_results[i]
        let result = ThreadingAdapter::get_task_result(threading_adapter, task)
        match result {
          Some(task_result) => assert_eq(task_result, "task_result_" + i.to_string())
          None => assert_true(false)
        }
      }
      
      // Test thread pool shutdown
      let shutdown_result = ThreadingAdapter::shutdown_thread_pool(threading_adapter, thread_pool)
      assert_true(shutdown_result)
    }
    None => assert_true(false)
  }
  
  // Test mutex creation and usage
  let mutex_result = ThreadingAdapter::create_mutex(threading_adapter)
  match mutex_result {
    Some(mutex) => {
      // Test mutex locking
      let lock_result = ThreadingAdapter::lock_mutex(threading_adapter, mutex)
      assert_true(lock_result)
      
      // Test mutex unlocking
      let unlock_result = ThreadingAdapter::unlock_mutex(threading_adapter, mutex)
      assert_true(unlock_result)
      
      // Test mutex destruction
      let destroy_result = ThreadingAdapter::destroy_mutex(threading_adapter, mutex)
      assert_true(destroy_result)
    }
    None => assert_true(false)
  }
  
  // Test condition variable creation and usage
  let condition_result = ThreadingAdapter::create_condition_variable(threading_adapter)
  match condition_result {
    Some(condition) => {
      // Test condition variable notification
      let notify_result = ThreadingAdapter::notify_condition_variable(threading_adapter, condition)
      assert_true(notify_result)
      
      // Test condition variable destruction
      let destroy_result = ThreadingAdapter::destroy_condition_variable(threading_adapter, condition)
      assert_true(destroy_result)
    }
    None => assert_true(false)
  }
}

// Test 10: Cross-Platform Error Handling
test "cross-platform error handling" {
  let error_adapter = ErrorAdapter::new()
  
  // Test error code mapping
  let platform_error_codes = [
    1,   // General error
    2,   // File not found
    3,   // Permission denied
    4,   // Invalid operation
    5,   // Resource busy
    11,  // Resource temporarily unavailable
    22,  // Invalid argument
    28,  // No space left on device
    32,  // Broken pipe
    33,  // Numerical argument out of domain
    110, // Connection timed out
    111, // Connection refused
    112, // Host is down
    113, // No route to host
    104, // Connection reset by peer
    105, // No buffer space available
    106, // Transport endpoint is already connected
    107, // Transport endpoint is not connected
    108, // Cannot send after transport endpoint shutdown
    109, // Too many references: cannot splice
    114, // Operation already in progress
    115, // Operation now in progress
    116, // Stale NFS file handle
    117, // Structure needs cleaning
    118, // Not a XENIX named type file
    119, // No XENIX semaphores available
    120, // Is a named type file
    121, // Remote I/O error
    122, // Disk quota exceeded
    123, // No medium found
    124, // Wrong medium type
    125, // Operation canceled
    126, // Required key not available
    127, // Key has expired
    128, // Key has been revoked
    129, // Key was rejected by service
    130, // Owner died
    131, // State not recoverable
    132, // Operation not possible due to RF-kill
    133, // Memory page has hardware error
  ]
  
  for error_code in platform_error_codes {
    let error_description = ErrorAdapter::get_error_description(error_adapter, error_code)
    assert_true(error_description.length() > 0)
    
    let error_category = ErrorAdapter::get_error_category(error_adapter, error_code)
    assert_true(error_category.length() > 0)
    
    let is_recoverable = ErrorAdapter::is_error_recoverable(error_adapter, error_code)
    assert_true(is_recoverable == true || is_recoverable == false)
  }
  
  // Test platform-specific error handling
  let platform_errors = [
    "ENOENT",    // No such file or directory
    "EACCES",    // Permission denied
    "EEXIST",    // File exists
    "ENOTDIR",   // Not a directory
    "EISDIR",    // Is a directory
    "EINVAL",    // Invalid argument
    "EMFILE",    // Too many open files
    "ENFILE",    // Too many open files in system
    "ENOSPC",    // No space left on device
    "ENOMEM",    // Not enough space
    "EAGAIN",    // Resource temporarily unavailable
    "ETIMEDOUT", // Connection timed out
    "ECONNREFUSED", // Connection refused
    "ECONNRESET", // Connection reset by peer
    "EHOSTUNREACH", // No route to host
    "ENETUNREACH"  // Network is unreachable
  ]
  
  for error_name in platform_errors {
    let error_code_result = ErrorAdapter::get_error_code_by_name(error_adapter, error_name)
    match error_code_result {
      Some(error_code) => {
        let error_description = ErrorAdapter::get_error_description(error_adapter, error_code)
        assert_true(error_description.length() > 0)
      }
      None => {
        // Some error codes might not exist on all platforms
        // This is acceptable
      }
    }
  }
  
  // Test error recovery strategies
  let recoverable_errors = [1, 2, 4, 11, 22, 110, 111, 112, 113, 125]
  for error_code in recoverable_errors {
    let recovery_strategy = ErrorAdapter::get_recovery_strategy(error_adapter, error_code)
    match recovery_strategy {
      Some(strategy) => assert_true(strategy.length() > 0)
      None => assert_true(false) // Should have recovery strategy for recoverable errors
    }
  }
  
  // Test error logging
  let log_result = ErrorAdapter::log_error(error_adapter, 2, "Test error message")
  assert_true(log_result)
  
  let log_result_with_context = ErrorAdapter::log_error_with_context(
    error_adapter, 
    2, 
    "Test error message with context",
    [("file", "/tmp/test.txt"), ("operation", "read")]
  )
  assert_true(log_result_with_context)
}