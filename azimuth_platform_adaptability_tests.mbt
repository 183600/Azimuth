// Azimuth Telemetry System - Platform Adaptability Tests
// This file contains comprehensive test cases for platform adaptability functionality

// Test 1: Operating System Detection
test "operating system detection" {
  // Test OS detector
  let os_detector = OSDetector::new()
  let os_info = OSDetector::detect(os_detector)
  
  // Verify OS information
  assert_true(os_info.name != "")
  assert_true(os_info.version != "")
  assert_true(os_info.architecture != "")
  assert_true(os_info.kernel_version != "")
  
  // Test OS type detection
  match os_info.type {
    Windows => {
      assert_true(os_info.name.contains("Windows"))
      assert_true(os_info.file_separator == "\\")
    }
    Linux => {
      assert_true(os_info.name.contains("Linux"))
      assert_true(os_info.file_separator == "/")
    }
    MacOS => {
      assert_true(os_info.name.contains("Mac") || os_info.name.contains("Darwin"))
      assert_true(os_info.file_separator == "/")
    }
    _ => assert_true(false)  // Should not reach here
  }
  
  // Test OS-specific features
  let features = OSDetector::get_supported_features(os_detector)
  assert_true(features.length() > 0)
  
  // Test OS capabilities
  let capabilities = OSDetector::get_capabilities(os_detector)
  assert_true(capabilities.supports_threads)
  assert_true(capabilities.supports_networking)
  assert_true(capabilities.supports_file_system)
}

// Test 2: Hardware Detection
test "hardware detection" {
  // Test hardware detector
  let hw_detector = HardwareDetector::new()
  let hw_info = HardwareDetector::detect(hw_detector)
  
  // Verify CPU information
  assert_true(hw_info.cpu.vendor != "")
  assert_true(hw_info.cpu.model != "")
  assert_true(hw_info.cpu.cores > 0)
  assert_true(hw_info.cpu.logical_cores > 0)
  assert_true(hw_info.cpu.frequency > 0.0)
  
  // Verify memory information
  assert_true(hw_info.memory.total > 0)
  assert_true(hw_info.memory.available > 0)
  assert_true(hw_info.memory.available <= hw_info.memory.total)
  
  // Verify disk information
  assert_true(hw_info.disk.total > 0)
  assert_true(hw_info.disk.available > 0)
  assert_true(hw_info.disk.available <= hw_info.disk.total)
  
  // Test hardware capabilities
  let capabilities = HardwareDetector::get_capabilities(hw_detector)
  assert_true(capabilities.cpu_instructions.length() > 0)
  assert_true(capabilities.cache_size.l1 > 0)
  assert_true(capabilities.cache_size.l2 > 0)
  
  // Test hardware-specific optimizations
  let optimizations = HardwareDetector::get_optimizations(hw_detector)
  assert_true(optimizations.length() > 0)
}

// Test 3: Cross-Platform File Operations
test "cross-platform file operations" {
  // Test cross-platform file manager
  let file_manager = CrossPlatformFileManager::new()
  
  // Test path normalization
  let windows_path = "C:\\Users\\Test\\Documents\\file.txt"
  let unix_path = "/home/user/documents/file.txt"
  
  let normalized_windows = CrossPlatformFileManager::normalize_path(file_manager, windows_path)
  let normalized_unix = CrossPlatformFileManager::normalize_path(file_manager, unix_path)
  
  // Both should be normalized to the current platform format
  assert_true(normalized_windows.contains("file.txt"))
  assert_true(normalized_unix.contains("file.txt"))
  
  // Test path joining
  let joined_path = CrossPlatformFileManager::join_paths(file_manager, ["home", "user", "documents"])
  assert_true(joined_path.contains("home"))
  assert_true(joined_path.contains("user"))
  assert_true(joined_path.contains("documents"))
  
  // Test temporary directory
  let temp_dir = CrossPlatformFileManager::get_temp_directory(file_manager)
  assert_true(temp_dir != "")
  
  // Test temporary file creation
  let temp_file = CrossPlatformFileManager::create_temp_file(file_manager, "test", ".tmp")
  assert_true(File::exists(temp_file))
  
  // Test file permissions
  CrossPlatformFileManager::set_permissions(file_manager, temp_file, Readable + Writable)
  let permissions = CrossPlatformFileManager::get_permissions(file_manager, temp_file)
  assert_true(permissions.contains(Readable))
  assert_true(permissions.contains(Writable))
  
  // Clean up
  File::remove(temp_file)
}

// Test 4: Cross-Platform Network Operations
test "cross-platform network operations" {
  // Test cross-platform network manager
  let network_manager = CrossPlatformNetworkManager::new()
  
  // Test network interface detection
  let interfaces = CrossPlatformNetworkManager::get_network_interfaces(network_manager)
  assert_true(interfaces.length() > 0)
  
  // Verify interface information
  for interface in interfaces {
    assert_true(interface.name != "")
    assert_true(interface.ip_addresses.length() > 0)
  }
  
  // Test hostname resolution
  let localhost_ip = CrossPlatformNetworkManager::resolve_hostname(network_manager, "localhost")
  assert_true(localhost_ip.length() > 0)
  
  // Test port availability check
  let available_port = CrossPlatformNetworkManager::find_available_port(network_manager)
  assert_true(available_port > 0 && available_port < 65536)
  
  // Test socket creation
  let socket = CrossPlatformNetworkManager::create_socket(network_manager, TCP)
  assert_true(CrossPlatformNetworkManager::is_valid_socket(socket))
  
  // Test socket options
  CrossPlatformNetworkManager::set_socket_option(socket, ReuseAddress, true)
  let reuse_address = CrossPlatformNetworkManager::get_socket_option(socket, ReuseAddress)
  assert_true(reuse_address)
  
  // Clean up
  CrossPlatformNetworkManager::close_socket(socket)
}

// Test 5: Platform-Specific Optimizations
test "platform-specific optimizations" {
  // Test optimization manager
  let optimization_manager = PlatformOptimizationManager::new()
  
  // Detect current platform
  let platform = PlatformOptimizationManager::detect_platform(optimization_manager)
  
  // Test platform-specific optimizations
  match platform {
    Windows => {
      // Test Windows-specific optimizations
      let windows_opts = PlatformOptimizationManager::get_windows_optimizations(optimization_manager)
      assert_true(windows_opts.use_windows_api)
      assert_true(windows_opts.optimize_thread_pool)
    }
    Linux => {
      // Test Linux-specific optimizations
      let linux_opts = PlatformOptimizationManager::get_linux_optimizations(optimization_manager)
      assert_true(linux_opts.use_epoll)
      assert_true(linux_opts.optimize_memory_allocation)
    }
    MacOS => {
      // Test macOS-specific optimizations
      let macos_opts = PlatformOptimizationManager::get_macos_optimizations(optimization_manager)
      assert_true(macos_opts.use_grand_central_dispatch)
      assert_true(macos_opts.optimize_file_io)
    }
    _ => assert_true(false)  // Should not reach here
  }
  
  // Test applying optimizations
  let optimization_result = PlatformOptimizationManager::apply_optimizations(optimization_manager)
  assert_true(optimization_result.success)
  assert_true(optimization_result.applied_optimizations.length() > 0)
  
  // Test optimization performance
  let baseline_metrics = PlatformOptimizationManager::measure_performance(optimization_manager)
  PlatformOptimizationManager::apply_optimizations(optimization_manager)
  let optimized_metrics = PlatformOptimizationManager::measure_performance(optimization_manager)
  
  // Optimized metrics should be better or equal
  assert_true(optimized_metrics.memory_usage <= baseline_metrics.memory_usage)
  assert_true(optimized_metrics.cpu_usage <= baseline_metrics.cpu_usage * 1.1)  // Allow 10% variance
}

// Test 6: Environment Variable Handling
test "environment variable handling" {
  // Test environment variable manager
  let env_manager = EnvironmentVariableManager::new()
  
  // Test setting and getting environment variables
  EnvironmentVariableManager::set(env_manager, "TEST_VAR", "test_value")
  let value = EnvironmentVariableManager::get(env_manager, "TEST_VAR")
  assert_eq(value, Some("test_value"))
  
  // Test non-existent variable
  let non_existent = EnvironmentVariableManager::get(env_manager, "NON_EXISTENT_VAR")
  assert_eq(non_existent, None)
  
  // Test environment variable expansion
  EnvironmentVariableManager::set(env_manager, "HOME", "/home/user")
  EnvironmentVariableManager::set(env_manager, "CONFIG_PATH", "$HOME/config")
  
  let expanded_path = EnvironmentVariableManager::expand(env_manager, "$HOME/config")
  assert_eq(expanded_path, "/home/user/config")
  
  // Test platform-specific environment variables
  let platform_vars = EnvironmentVariableManager::get_platform_variables(env_manager)
  assert_true(platform_vars.length() > 0)
  
  // Test environment variable persistence
  let persistent_vars = EnvironmentVariableManager::get_persistent_variables(env_manager)
  EnvironmentVariableManager::set_persistent(env_manager, "PERSISTENT_VAR", "persistent_value")
  
  let updated_persistent_vars = EnvironmentVariableManager::get_persistent_variables(env_manager)
  assert_true(updated_persistent_vars.contains_key("PERSISTENT_VAR"))
  assert_eq(updated_persistent_vars.get("PERSISTENT_VAR"), Some("persistent_value"))
  
  // Clean up
  EnvironmentVariableManager::unset(env_manager, "TEST_VAR")
  EnvironmentVariableManager::unset(env_manager, "HOME")
  EnvironmentVariableManager::unset(env_manager, "CONFIG_PATH")
  EnvironmentVariableManager::unset_persistent(env_manager, "PERSISTENT_VAR")
}

// Test 7: Service Management
test "service management" {
  // Test service manager
  let service_manager = ServiceManager::new()
  
  // Test service creation
  let service_config = ServiceConfig::new("test_service")
    .with_executable("/usr/bin/test_service")
    .with_args(["--config", "/etc/test_service.conf"])
    .with_working_directory("/var/lib/test_service")
    .with_environment([("LOG_LEVEL", "INFO")])
    .with_auto_restart(true)
  
  let service = ServiceManager::create_service(service_manager, service_config)
  assert_true(ServiceManager::is_valid_service(service))
  
  // Test service installation
  let install_result = ServiceManager::install_service(service_manager, service)
  assert_true(install_result.success)
  
  // Test service start
  let start_result = ServiceManager::start_service(service_manager, service)
  assert_true(start_result.success)
  
  // Test service status
  let status = ServiceManager::get_service_status(service_manager, service)
  assert_eq(status, Running)
  
  // Test service configuration
  let config = ServiceManager::get_service_config(service_manager, service)
  assert_eq(config.name, "test_service")
  assert_eq(config.executable, "/usr/bin/test_service")
  
  // Test service stop
  let stop_result = ServiceManager::stop_service(service_manager, service)
  assert_true(stop_result.success)
  
  // Test service uninstallation
  let uninstall_result = ServiceManager::uninstall_service(service_manager, service)
  assert_true(uninstall_result.success)
}

// Test 8: Platform-Specific Logging
test "platform-specific logging" {
  // Test platform logger
  let platform_logger = PlatformLogger::new()
  
  // Test log levels
  PlatformLogger::set_level(platform_logger, Debug)
  assert_eq(PlatformLogger::get_level(platform_logger), Debug)
  
  // Test log formatting
  PlatformLogger::set_format(platform_logger, "[%(level)s] %(timestamp)s - %(message)s")
  
  // Test logging to different outputs
  let file_output = LogOutput::File("/tmp/platform_test.log")
  let console_output = LogOutput::Console
  
  PlatformLogger::add_output(platform_logger, file_output)
  PlatformLogger::add_output(platform_logger, console_output)
  
  // Test logging messages
  PlatformLogger::debug(platform_logger, "Debug message")
  PlatformLogger::info(platform_logger, "Info message")
  PlatformLogger::warn(platform_logger, "Warning message")
  PlatformLogger::error(platform_logger, "Error message")
  
  // Test platform-specific log destinations
  match PlatformDetector::detect(OSDetector::new()).type {
    Windows => {
      // Test Windows Event Log
      let event_log_output = LogOutput::WindowsEventLog("AzimuthTest")
      PlatformLogger::add_output(platform_logger, event_log_output)
      PlatformLogger::info(platform_logger, "Test message to Windows Event Log")
    }
    Linux => {
      // Test syslog
      let syslog_output = LogOutput::Syslog("azimuth-test")
      PlatformLogger::add_output(platform_logger, syslog_output)
      PlatformLogger::info(platform_logger, "Test message to syslog")
    }
    MacOS => {
      // Test macOS unified logging
      let unified_log_output = LogOutput::MacOSUnifiedLog("azimuth-test")
      PlatformLogger::add_output(platform_logger, unified_log_output)
      PlatformLogger::info(platform_logger, "Test message to unified log")
    }
    _ => ()
  }
  
  // Test log rotation
  PlatformLogger::enable_rotation(platform_logger, 1024 * 1024, 5)  // 1MB, keep 5 files
  
  // Test log statistics
  let stats = PlatformLogger::get_statistics(platform_logger)
  assert_true(stats.messages_logged >= 4)
  assert_true(stats.bytes_written > 0)
  
  // Clean up
  File::remove("/tmp/platform_test.log")
}

// Test 9: Platform-Specific Configuration
test "platform-specific configuration" {
  // Test configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Test configuration file paths
  let config_paths = ConfigurationManager::get_config_paths(config_manager)
  assert_true(config_paths.length() > 0)
  
  // Test platform-specific configuration loading
  let config = ConfigurationManager::load_platform_config(config_manager)
  assert_true(ConfigurationManager::is_valid_config(config))
  
  // Test configuration with platform-specific sections
  ConfigurationManager::set_value(config, "general.setting", "value")
  ConfigurationManager::set_value(config, "windows.setting", "windows_value")
  ConfigurationManager::set_value(config, "linux.setting", "linux_value")
  ConfigurationManager::set_value(config, "macos.setting", "macos_value")
  
  // Test getting platform-specific values
  let platform = PlatformDetector::detect(OSDetector::new()).type
  let platform_value = ConfigurationManager::get_platform_value(config, "setting", platform)
  
  match platform {
    Windows => assert_eq(platform_value, Some("windows_value"))
    Linux => assert_eq(platform_value, Some("linux_value"))
    MacOS => assert_eq(platform_value, Some("macos_value"))
    _ => assert_eq(platform_value, None)
  }
  
  // Test configuration saving
  let save_result = ConfigurationManager::save_config(config_manager, config)
  assert_true(save_result.success)
  
  // Test configuration validation
  let validation_result = ConfigurationManager::validate_config(config)
  assert_true(validation_result.valid)
  
  // Test configuration merging
  let base_config = ConfigurationManager::create_default_config(config_manager)
  let merged_config = ConfigurationManager::merge_configs(config_manager, base_config, config)
  assert_true(ConfigurationManager::is_valid_config(merged_config))
}

// Test 10: Cross-Platform Process Management
test "cross-platform process management" {
  // Test process manager
  let process_manager = ProcessManager::new()
  
  // Test process creation
  let process_config = ProcessConfig::new("/bin/echo")
    .with_args(["Hello, World!"])
    .with_environment([("TEST_VAR", "test_value")])
    .with_working_directory("/tmp")
    .with_redirect_output(true)
  
  let process = ProcessManager::create_process(process_manager, process_config)
  assert_true(ProcessManager::is_valid_process(process))
  
  // Test process execution
  let execution_result = ProcessManager::execute(process_manager, process)
  assert_true(execution_result.success)
  assert_eq(execution_result.exit_code, Some(0))
  assert_true(execution_result.output.contains("Hello, World!"))
  
  // Test process monitoring
  let long_running_config = ProcessConfig::new("/bin/sleep")
    .with_args(["5"])
  
  let long_running_process = ProcessManager::create_process(process_manager, long_running_config)
  ProcessManager::start(process_manager, long_running_process)
  
  // Test process status
  let status = ProcessManager::get_status(process_manager, long_running_process)
  assert_eq(status, Running)
  
  // Test process information
  let process_info = ProcessManager::get_process_info(process_manager, long_running_process)
  assert_true(process_info.pid > 0)
  assert_true(process_info.start_time > 0)
  assert_eq(process_info.status, Running)
  
  // Test process termination
  let terminate_result = ProcessManager::terminate(process_manager, long_running_process)
  assert_true(terminate_result.success)
  
  // Test process listing
  let processes = ProcessManager::list_processes(process_manager)
  assert_true(processes.length() > 0)
  
  // Find our own process
  let current_pid = ProcessManager::get_current_pid()
  let current_process = processes.find(|p| p.pid == current_pid)
  assert_true(current_process.is_some())
  
  // Test process tree
  let process_tree = ProcessManager::get_process_tree(process_manager, current_pid)
  assert_true(process_tree.length() >= 1)
  assert_eq(process_tree[0].pid, current_pid)
}