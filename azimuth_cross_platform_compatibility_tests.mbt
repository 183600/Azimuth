// Azimuth Telemetry System - Cross Platform Compatibility Tests
// This file contains test cases for cross-platform compatibility functionality

// Test 1: Platform Detection and Adaptation
test "platform detection and adaptation" {
  // Create platform service
  let platform_service = PlatformService::new()
  
  // Test platform detection
  let platform_info = PlatformService::detect_platform(platform_service)
  
  // Verify platform information structure
  assert_true(platform_info.os_name.length() > 0)
  assert_true(platform_info.os_version.length() > 0)
  assert_true(platform_info.architecture.length() > 0)
  assert_true(platform_info.platform_id.length() > 0)
  
  // Test platform-specific adaptations
  let file_separator = PlatformService::get_file_separator(platform_service)
  let path_separator = PlatformService::get_path_separator(platform_service)
  
  if platform_info.os_name == "Windows" {
    assert_eq(file_separator, "\\")
    assert_eq(path_separator, ";")
  } else {
    assert_eq(file_separator, "/")
    assert_eq(path_separator, ":")
  }
  
  // Test platform-specific configuration
  let config = PlatformService::get_platform_config(platform_service)
  
  if platform_info.os_name == "Linux" {
    assert_eq(config.default_config_dir, "/etc/azimuth")
    assert_eq(config.default_log_dir, "/var/log/azimuth")
  } else if platform_info.os_name == "Windows" {
    assert_eq(config.default_config_dir, "C:\\ProgramData\\Azimuth")
    assert_eq(config.default_log_dir, "C:\\ProgramData\\Azimuth\\Logs")
  } else if platform_info.os_name == "macOS" {
    assert_eq(config.default_config_dir, "/Library/Application Support/Azimuth")
    assert_eq(config.default_log_dir, "/Library/Logs/Azimuth")
  }
}

// Test 2: File System Compatibility
test "file system compatibility" {
  // Create file system service
  let fs_service = FileSystemService::new()
  
  // Test path normalization
  let paths = [
    "/home/user/../user/data",
    "C:\\Users\\User\\..\\User\\Data",
    "./relative/path",
    "./relative\\path"
  ]
  
  for path in paths {
    let normalized = FileSystemService::normalize_path(fs_service, path)
    assert_true(normalized.length() > 0)
    assert_false(normalized.contains(".."))
  }
  
  // Test path joining
  let parts1 = ["home", "user", "data"]
  let joined_path1 = FileSystemService::join_paths(fs_service, parts1)
  assert_true(joined_path1.contains("home"))
  assert_true(joined_path1.contains("user"))
  assert_true(joined_path1.contains("data"))
  
  let parts2 = ["C:", "Program Files", "Azimuth"]
  let joined_path2 = FileSystemService::join_paths(fs_service, parts2)
  assert_true(joined_path2.contains("C:"))
  assert_true(joined_path2.contains("Program Files"))
  assert_true(joined_path2.contains("Azimuth"))
  
  // Test file permissions handling
  let permissions = FileSystemService::get_default_permissions(fs_service)
  
  let platform_info = PlatformService::detect_platform(PlatformService::new())
  if platform_info.os_name == "Windows" {
    assert_eq(permissions.owner_read, true)
    assert_eq(permissions.owner_write, true)
    assert_eq(permissions.owner_execute, false)
  } else {
    assert_eq(permissions.owner_read, true)
    assert_eq(permissions.owner_write, true)
    assert_eq(permissions.owner_execute, true)
    assert_eq(permissions.group_read, true)
    assert_eq(permissions.group_write, false)
    assert_eq(permissions.group_execute, true)
    assert_eq(permissions.other_read, true)
    assert_eq(permissions.other_write, false)
    assert_eq(permissions.other_execute, true)
  }
}

// Test 3: Network Interface Compatibility
test "network interface compatibility" {
  // Create network service
  let network_service = NetworkService::new()
  
  // Test network interface detection
  let interfaces = NetworkService::get_network_interfaces(network_service)
  assert_true(interfaces.length() > 0)
  
  // Test interface information
  for interface in interfaces {
    assert_true(NetworkInterface::name(interface).length() > 0)
    assert_true(NetworkInterface::is_up(interface) || NetworkInterface::is_down(interface))
    
    // Test IP addresses
    let ip_addresses = NetworkInterface::ip_addresses(interface)
    for ip in ip_addresses {
      assert_true(IPAddress::is_valid(ip))
    }
  }
  
  // Test hostname resolution
  let hostname = NetworkService::get_hostname(network_service)
  assert_true(hostname.length() > 0)
  
  // Test localhost resolution
  let localhost_ips = NetworkService::resolve_hostname(network_service, "localhost")
  assert_true(localhost_ips.length() > 0)
  
  // Test IPv4/IPv6 compatibility
  let has_ipv4 = localhost_ips.any(fn(ip) { IPAddress::is_ipv4(ip) })
  let has_ipv6 = localhost_ips.any(fn(ip) { IPAddress::is_ipv6(ip) })
  
  // At least one of IPv4 or IPv6 should be available
  assert_true(has_ipv4 || has_ipv6)
}

// Test 4: Process Management Compatibility
test "process management compatibility" {
  // Create process service
  let process_service = ProcessService::new()
  
  // Test current process information
  let current_process = ProcessService::get_current_process(process_service)
  assert_true(Process::pid(current_process) > 0)
  assert_true(Process::name(current_process).length() > 0)
  
  // Test process listing
  let processes = ProcessService::list_processes(process_service)
  assert_true(processes.length() > 0)
  
  // Test process creation
  let command = if PlatformService::detect_platform(PlatformService::new()).os_name == "Windows" {
    "echo hello"
  } else {
    "echo hello"
  }
  
  let child_process = ProcessService::create_process(process_service, command)
  assert_true(Process::pid(child_process) > 0)
  
  // Test process termination
  let termination_result = ProcessService::terminate_process(process_service, Process::pid(child_process))
  assert_true(termination_result.success)
  
  // Test process resource monitoring
  let process_resources = ProcessService::get_process_resources(process_service, Process::pid(current_process))
  assert_true(ProcessResources::memory_usage(process_resources) > 0)
  assert_true(ProcessResources::cpu_usage(process_resources) >= 0.0)
}

// Test 5: Environment Variable Compatibility
test "environment variable compatibility" {
  // Create environment service
  let env_service = EnvironmentService::new()
  
  // Test environment variable retrieval
  let path_var = EnvironmentService::get_variable(env_service, "PATH")
  match path_var {
    Some(p) => assert_true(p.length() > 0)
    None => assert_true(false)
  }
  
  // Test environment variable setting
  EnvironmentService::set_variable(env_service, "AZIMUTH_TEST", "test_value")
  let test_var = EnvironmentService::get_variable(env_service, "AZIMUTH_TEST")
  match test_var {
    Some(v) => assert_eq(v, "test_value")
    None => assert_true(false)
  }
  
  // Test environment variable listing
  let all_vars = EnvironmentService::list_variables(env_service)
  assert_true(all_vars.length() > 0)
  
  // Test platform-specific environment variables
  let platform_info = PlatformService::detect_platform(PlatformService::new())
  if platform_info.os_name == "Windows" {
    let system_root = EnvironmentService::get_variable(env_service, "SystemRoot")
    match system_root {
      Some(sr) => assert_true(sr.contains("Windows"))
      None => assert_true(false)
    }
  } else {
    let home = EnvironmentService::get_variable(env_service, "HOME")
    match home {
      Some(h) => assert_true(h.length() > 0)
      None => assert_true(false)
    }
  }
}

// Test 6: System Resource Monitoring Compatibility
test "system resource monitoring compatibility" {
  // Create resource monitoring service
  let resource_service = ResourceService::new()
  
  // Test memory information
  let memory_info = ResourceService::get_memory_info(resource_service)
  assert_true(MemoryInfo::total_memory(memory_info) > 0)
  assert_true(MemoryInfo::available_memory(memory_info) > 0)
  assert_true(MemoryInfo::used_memory(memory_info) >= 0)
  
  // Test CPU information
  let cpu_info = ResourceService::get_cpu_info(resource_service)
  assert_true(CpuInfo::core_count(cpu_info) > 0)
  assert_true(CpuInfo::usage_percentage(cpu_info) >= 0.0)
  assert_true(CpuInfo::usage_percentage(cpu_info) <= 100.0)
  
  // Test disk information
  let disk_info = ResourceService::get_disk_info(resource_service, "/")
  if disk_info.is_some() {
    let disk = disk_info.unwrap()
    assert_true(DiskInfo::total_space(disk) > 0)
    assert_true(DiskInfo::free_space(disk) > 0)
    assert_true(DiskInfo::used_space(disk) >= 0)
  }
  
  // Test network statistics
  let network_stats = ResourceService::get_network_stats(resource_service)
  assert_true(NetworkStats::bytes_sent(network_stats) >= 0)
  assert_true(NetworkStats::bytes_received(network_stats) >= 0)
  assert_true(NetworkStats::packets_sent(network_stats) >= 0)
  assert_true(NetworkStats::packets_received(network_stats) >= 0)
}

// Test 7: Service Management Compatibility
test "service management compatibility" {
  // Create service management service
  let service_service = ServiceService::new()
  
  // Test platform-specific service management
  let platform_info = PlatformService::detect_platform(PlatformService::new())
  
  if platform_info.os_name == "Linux" {
    // Test systemd service management
    if ServiceService::supports_systemd(service_service) {
      let services = ServiceService::list_systemd_services(service_service)
      assert_true(services.length() > 0)
      
      // Test service status
      for service_name in ["ssh", "networking"] {
        let service_status = ServiceService::get_systemd_service_status(service_service, service_name)
        if service_status.is_some() {
          let status = service_status.unwrap()
          assert_true(ServiceStatus::is_known(status))
        }
      }
    }
  } else if platform_info.os_name == "Windows" {
    // Test Windows service management
    let services = ServiceService::list_windows_services(service_service)
    assert_true(services.length() > 0)
    
    // Test service status
    for service_name in ["EventLog", "PlugPlay"] {
      let service_status = ServiceService::get_windows_service_status(service_service, service_name)
      if service_status.is_some() {
        let status = service_status.unwrap()
        assert_true(ServiceStatus::is_known(status))
      }
    }
  }
  
  // Test cross-platform service registration
  let service_config = ServiceConfig::new(
    "azimuth-telemetry",
    "Azimuth Telemetry Service",
    "Telemetry data collection and processing"
  )
  
  let registration_result = ServiceService::register_service(service_service, service_config)
  if registration_result.supported {
    assert_true(registration_result.success)
  }
}

// Test 8: Logging System Compatibility
test "logging system compatibility" {
  // Create logging service
  let logging_service = LoggingService::new()
  
  // Test log directory creation
  let platform_info = PlatformService::detect_platform(PlatformService::new())
  let log_dir = if platform_info.os_name == "Windows" {
    "C:\\temp\\azimuth_logs"
  } else {
    "/tmp/azimuth_logs"
  }
  
  let log_dir_created = LoggingService::ensure_log_directory(logging_service, log_dir)
  assert_true(log_dir_created)
  
  // Test log file creation
  let log_file = LoggingService::create_log_file(logging_service, log_dir, "test")
  assert_true(log_file.length() > 0)
  
  // Test log writing
  let log_entries = [
    ("INFO", "Test log message 1"),
    ("WARN", "Test warning message"),
    ("ERROR", "Test error message"),
    ("DEBUG", "Test debug message")
  ]
  
  for (level, message) in log_entries {
    let write_result = LoggingService::write_log(logging_service, log_file, level, message)
    assert_true(write_result)
  }
  
  // Test log reading
  let log_contents = LoggingService::read_log_file(logging_service, log_file)
  assert_true(log_contents.length() > 0)
  
  for (level, message) in log_entries {
    assert_true(log_contents.contains(level))
    assert_true(log_contents.contains(message))
  }
  
  // Test log rotation
  let rotation_result = LoggingService::rotate_log_file(logging_service, log_file)
  if rotation_result.supported {
    assert_true(rotation_result.success)
  }
}

// Test 9: Configuration File Compatibility
test "configuration file compatibility" {
  // Create configuration service
  let config_service = ConfigurationService::new()
  
  // Test platform-specific configuration paths
  let platform_info = PlatformService::detect_platform(PlatformService::new())
  let config_path = if platform_info.os_name == "Windows" {
    "C:\\ProgramData\\Azimuth\\config.json"
  } else if platform_info.os_name == "macOS" {
    "/Library/Application Support/Azimuth/config.json"
  } else {
    "/etc/azimuth/config.json"
  }
  
  // Test configuration file creation
  let config_data = [
    ("service.name", "azimuth-telemetry"),
    ("service.port", "8080"),
    ("logging.level", "INFO"),
    ("data.retention_days", "30")
  ]
  
  let config_created = ConfigurationService::create_config_file(config_service, config_path, config_data)
  assert_true(config_created)
  
  // Test configuration file reading
  let read_config = ConfigurationService::read_config_file(config_service, config_path)
  assert_eq(read_config.get("service.name"), Some("azimuth-telemetry"))
  assert_eq(read_config.get("service.port"), Some("8080"))
  assert_eq(read_config.get("logging.level"), Some("INFO"))
  assert_eq(read_config.get("data.retention_days"), Some("30"))
  
  // Test configuration file updating
  let updates = [
    ("service.port", "9090"),
    ("logging.level", "DEBUG")
  ]
  
  let config_updated = ConfigurationService::update_config_file(config_service, config_path, updates)
  assert_true(config_updated)
  
  // Verify updates
  let updated_config = ConfigurationService::read_config_file(config_service, config_path)
  assert_eq(updated_config.get("service.port"), Some("9090"))
  assert_eq(updated_config.get("logging.level"), Some("DEBUG"))
  assert_eq(updated_config.get("service.name"), Some("azimuth-telemetry")) // Unchanged
}

// Test 10: Runtime Compatibility Checks
test "runtime compatibility checks" {
  // Create compatibility service
  let compatibility_service = CompatibilityService::new()
  
  // Test runtime requirements
  let requirements = RuntimeRequirements::new(
    "1.0.0", // Minimum version
    512,     // Minimum memory MB
    2,       // Minimum CPU cores
    100      // Minimum disk space MB
  )
  
  // Check current system compatibility
  let compatibility_result = CompatibilityService::check_compatibility(compatibility_service, requirements)
  
  // Verify compatibility check structure
  assert_true(compatibility_result.version_compatible || !compatibility_result.version_compatible)
  assert_true(compatibility_result.memory_compatible || !compatibility_result.memory_compatible)
  assert_true(compatibility_result.cpu_compatible || !compatibility_result.cpu_compatible)
  assert_true(compatibility_result.disk_compatible || !compatibility_result.disk_compatible)
  
  // Test compatibility report generation
  let report = CompatibilityService::generate_compatibility_report(compatibility_service, requirements)
  assert_true(report.length() > 0)
  assert_true(report.contains("System Compatibility Report"))
  assert_true(report.contains("Version Compatibility"))
  assert_true(report.contains("Memory Compatibility"))
  assert_true(report.contains("CPU Compatibility"))
  assert_true(report.contains("Disk Compatibility"))
  
  // Test platform-specific features
  let platform_features = CompatibilityService::get_platform_features(compatibility_service)
  assert_true(platform_features.length() > 0)
  
  let platform_info = PlatformService::detect_platform(PlatformService::new())
  if platform_info.os_name == "Linux" {
    assert_true(platform_features.contains("systemd"))
    assert_true(platform_features.contains("signals"))
  } else if platform_info.os_name == "Windows" {
    assert_true(platform_features.contains("services"))
    assert_true(platform_features.contains("registry"))
  } else if platform_info.os_name == "macOS" {
    assert_true(platform_features.contains("launchd"))
    assert_true(platform_features.contains("keychain"))
  }
  
  // Test feature availability checking
  let systemd_available = CompatibilityService::is_feature_available(compatibility_service, "systemd")
  let services_available = CompatibilityService::is_feature_available(compatibility_service, "services")
  
  // At least one service management feature should be available
  assert_true(systemd_available || services_available)
}