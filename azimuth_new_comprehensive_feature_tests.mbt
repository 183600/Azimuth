// Azimuth New Comprehensive Feature Tests
// This file contains new comprehensive test cases for various Azimuth features

// Test 1: Data Conversion and Formatting
test "data conversion and formatting" {
  // Test number to string conversion with different formats
  let int_value = 12345
  let float_value = 12345.6789
  
  let int_string = int_value.to_string()
  let float_string = float_value.to_string()
  
  assert_eq(int_string, "12345")
  assert_true(float_string.contains("12345"))
  
  // Test string to number conversion
  let string_int = "98765"
  let string_float = "98765.4321"
  
  match string_int.to_int() {
    Some(value) => assert_eq(value, 98765)
    None => assert_true(false)
  }
  
  match string_float.to_float() {
    Some(value) => assert_true(value > 98765.0 && value < 98766.0)
    None => assert_true(false)
  }
  
  // Test date/time formatting
  let timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  let date_string = TimeUtil::format_date(timestamp, "YYYY-MM-DD")
  assert_eq(date_string, "2022-01-01")
  
  let time_string = TimeUtil::format_time(timestamp, "HH:mm:ss")
  assert_eq(time_string, "00:00:00")
  
  // Test currency formatting
  let currency_value = 1234.56
  let usd_format = FormatUtil::format_currency(currency_value, "USD")
  let eur_format = FormatUtil::format_currency(currency_value, "EUR")
  
  assert_true(usd_format.contains("$1,234.56"))
  assert_true(eur_format.contains("€1,234.56"))
}

// Test 2: Error Boundary and Exception Handling
test "error boundary and exception handling" {
  let error_handler = ErrorHandler::new()
  
  // Test graceful error handling
  let result = ErrorHandler::execute_with_catch(error_handler, fn() {
    // Simulate an operation that might fail
    let divisor = 0
    if divisor == 0 {
      return Error("Division by zero")
    } else {
      return Ok(100 / divisor)
    }
  })
  
  match result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(message) => assert_eq(message, "Division by zero")
  }
  
  // Test error recovery with fallback
  let fallback_result = ErrorHandler::execute_with_fallback(
    error_handler,
    fn() { Error("Primary operation failed") },
    fn() { Ok("Fallback operation succeeded") }
  )
  
  match fallback_result {
    Ok(value) => assert_eq(value, "Fallback operation succeeded")
    Error(_) => assert_true(false)
  }
  
  // Test error chaining
  let chained_result = ErrorHandler::execute_and_chain(error_handler, fn() {
    Error("Initial error")
  }, fn(prev_error) {
    Error("Chained error: " + prev_error)
  })
  
  match chained_result {
    Error(message) => assert_true(message.contains("Chained error"))
    Ok(_) => assert_true(false)
  }
  
  // Test timeout handling
  let timeout_result = ErrorHandler::execute_with_timeout(
    error_handler,
    fn() {
      // Simulate a long operation
      TimeUtil::sleep(5000) // 5 seconds
      return Ok("Completed")
    },
    2000 // 2 seconds timeout
  )
  
  match timeout_result {
    Error(message) => assert_true(message.contains("timeout"))
    Ok(_) => assert_true(false) // Should timeout
  }
}

// Test 3: Resource Management and Memory Leak Protection
test "resource management and memory leak protection" {
  let resource_manager = ResourceManager::new()
  
  // Test automatic resource cleanup
  let resource = ResourceManager::create_resource(resource_manager, "test_resource")
  assert_true(ResourceManager::is_resource_active(resource_manager, resource))
  
  // Resource should be automatically cleaned up when out of scope
  ResourceManager::cleanup_resource(resource_manager, resource)
  assert_false(ResourceManager::is_resource_active(resource_manager, resource))
  
  // Test memory pool management
  let memory_pool = ResourceManager::create_memory_pool(resource_manager, 1024) // 1KB pool
  
  let allocation1 = ResourceManager::allocate_from_pool(memory_pool, 256)
  let allocation2 = ResourceManager::allocate_from_pool(memory_pool, 512)
  
  assert_true(ResourceManager::is_allocation_valid(memory_pool, allocation1))
  assert_true(ResourceManager::is_allocation_valid(memory_pool, allocation2))
  
  // Pool should be exhausted after these allocations
  let allocation3 = ResourceManager::allocate_from_pool(memory_pool, 300)
  assert_false(ResourceManager::is_allocation_valid(memory_pool, allocation3))
  
  // Free allocations
  ResourceManager::free_allocation(memory_pool, allocation1)
  ResourceManager::free_allocation(memory_pool, allocation2)
  
  // Now allocation should succeed
  let allocation4 = ResourceManager::allocate_from_pool(memory_pool, 300)
  assert_true(ResourceManager::is_allocation_valid(memory_pool, allocation4))
  
  // Test resource leak detection
  let leak_detector = ResourceManager::create_leak_detector(resource_manager)
  
  // Simulate resource leak
  let leaked_resource = ResourceManager::create_resource(resource_manager, "leaked_resource")
  // Don't clean it up to simulate leak
  
  let leak_report = ResourceManager::detect_leaks(leak_detector)
  assert_true(leak_report.leaked_resources.length() > 0)
  
  // Clean up leaked resource
  ResourceManager::cleanup_resource(resource_manager, leaked_resource)
  
  // No leaks should be detected now
  let no_leak_report = ResourceManager::detect_leaks(leak_detector)
  assert_eq(no_leak_report.leaked_resources.length(), 0)
}

// Test 4: Network Communication and API Integration
test "network communication and api integration" {
  let network_client = NetworkClient::new()
  
  // Test HTTP GET request
  let get_result = NetworkClient::get(network_client, "https://httpbin.org/get")
  match get_result {
    Ok(response) => {
      assert_true(response.status_code >= 200 && response.status_code < 300)
      assert_true(response.body.length() > 0)
    }
    Error(_) => assert_true(false)
  }
  
  // Test HTTP POST request
  let post_data = '{"key": "value", "number": 42}'
  let post_result = NetworkClient::post(
    network_client, 
    "https://httpbin.org/post", 
    post_data,
    "application/json"
  )
  
  match post_result {
    Ok(response) => {
      assert_true(response.status_code >= 200 && response.status_code < 300)
      assert_true(response.body.contains("key"))
      assert_true(response.body.contains("value"))
    }
    Error(_) => assert_true(false)
  }
  
  // Test request retry with exponential backoff
  let retry_config = RetryConfig::exponential_backoff(3, 1000) // 3 retries, 1s initial delay
  let retry_result = NetworkClient::get_with_retry(
    network_client,
    "https://httpbin.org/status/500", // This will fail
    retry_config
  )
  
  match retry_result {
    Ok(_) => assert_true(false) // Should fail after retries
    Error(error) => assert_true(error.contains("max retries exceeded"))
  }
  
  // Test concurrent requests
  let urls = [
    "https://httpbin.org/get",
    "https://httpbin.org/user-agent",
    "https://httpbin.org/headers"
  ]
  
  let concurrent_results = NetworkClient::concurrent_get(network_client, urls)
  assert_eq(concurrent_results.length(), 3)
  
  for result in concurrent_results {
    match result {
      Ok(response) => assert_true(response.status_code >= 200 && response.status_code < 300)
      Error(_) => assert_true(false)
    }
  }
  
  // Test request timeout
  let timeout_config = TimeoutConfig::new(2000) // 2 seconds timeout
  let timeout_result = NetworkClient::get_with_timeout(
    network_client,
    "https://httpbin.org/delay/5", // This will take 5 seconds
    timeout_config
  )
  
  match timeout_result {
    Ok(_) => assert_true(false) // Should timeout
    Error(error) => assert_true(error.contains("timeout"))
  }
}

// Test 5: Caching Strategy and Data Persistence
test "caching strategy and data persistence" {
  let cache_manager = CacheManager::new()
  
  // Test multi-level caching (L1: memory, L2: disk)
  let l1_cache = CacheManager::create_memory_cache(cache_manager, 100) // 100 items
  let l2_cache = CacheManager::create_disk_cache(cache_manager, "/tmp/azimuth_cache", 1000) // 1000 items
  
  let multi_cache = CacheManager::create_multi_level_cache(cache_manager, [l1_cache, l2_cache])
  
  // Test cache write-through
  CacheManager::put(multi_cache, "user:123", '{"name": "John", "age": 30}')
  
  // Should be in L1 cache
  match CacheManager::get(multi_cache, "user:123") {
    Some(value) => assert_eq(value, '{"name": "John", "age": 30}')
    None => assert_true(false)
  }
  
  // Simulate L1 cache eviction
  CacheManager::evict_all(l1_cache)
  
  // Should still be available from L2 cache
  match CacheManager::get(multi_cache, "user:123") {
    Some(value) => assert_eq(value, '{"name": "John", "age": 30}')
    None => assert_true(false)
  }
  
  // Test cache invalidation strategy
  let invalidation_cache = CacheManager::create_cache_with_invalidation(
    cache_manager,
    InvalidationStrategy::TimeToLive(5000) // 5 seconds TTL
  )
  
  CacheManager::put(invalidation_cache, "temp_data", "will_expire_soon")
  
  // Should be available immediately
  match CacheManager::get(invalidation_cache, "temp_data") {
    Some(value) => assert_eq(value, "will_expire_soon")
    None => assert_true(false)
  }
  
  // Simulate time passage
  TimeUtil::sleep(6000) // 6 seconds
  
  // Should be expired
  match CacheManager::get(invalidation_cache, "temp_data") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test cache warming strategy
  let warm_cache = CacheManager::create_warmable_cache(cache_manager)
  
  // Define warming function
  CacheManager::set_warming_function(warm_cache, fn(key) {
    if key == "popular_item" {
      return Some("preloaded_value")
    } else {
      return None
    }
  })
  
  // Warm specific keys
  CacheManager::warm_keys(warm_cache, ["popular_item", "another_item"])
  
  // Popular item should be available without explicit put
  match CacheManager::get(warm_cache, "popular_item") {
    Some(value) => assert_eq(value, "preloaded_value")
    None => assert_true(false)
  }
  
  // Non-popular item should not be available
  match CacheManager::get(warm_cache, "another_item") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: Configuration Management and Dynamic Updates
test "configuration management and dynamic updates" {
  let config_manager = ConfigurationManager::new()
  
  // Test hierarchical configuration
  let base_config = ConfigurationManager::load_from_file(config_manager, "config/base.json")
  let env_config = ConfigurationManager::load_from_file(config_manager, "config/production.json")
  
  let merged_config = ConfigurationManager::merge_configs(config_manager, [base_config, env_config])
  
  // Test configuration inheritance
  match ConfigurationManager::get_string(merged_config, "database.host") {
    Some(host) => assert_true(host.length() > 0)
    None => assert_true(false)
  }
  
  match ConfigurationManager::get_int(merged_config, "server.port") {
    Some(port) => assert_true(port > 0 && port < 65536)
    None => assert_true(false)
  }
  
  // Test dynamic configuration updates
  let dynamic_config = ConfigurationManager::create_dynamic_config(config_manager, merged_config)
  
  // Register configuration change listener
  ConfigurationManager::add_change_listener(dynamic_config, "feature.enabled", fn(old_value, new_value) {
    // This would be called when the value changes
    assert_true(true)
  })
  
  // Update configuration value
  ConfigurationManager::set_value(dynamic_config, "feature.enabled", true)
  
  // Verify the change
  match ConfigurationManager::get_bool(dynamic_config, "feature.enabled") {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  // Test configuration validation
  let schema = ValidationSchema::object([
    ("database", ValidationSchema::object([
      ("host", ValidationSchema::string([ValidationRule::required()])),
      ("port", ValidationSchema::number([ValidationRule::min_value(1), ValidationRule::max_value(65535)])),
      ("max_connections", ValidationSchema::number([ValidationRule::min_value(1), ValidationRule::max_value(1000)]))
    ])),
    ("feature", ValidationSchema::object([
      ("enabled", ValidationSchema::boolean([])),
      ("timeout", ValidationSchema::number([ValidationRule::min_value(0)]))
    ]))
  ])
  
  let validation_result = ConfigurationManager::validate_schema(dynamic_config, schema)
  assert_true(validation_result.is_valid)
  
  // Test invalid configuration
  ConfigurationManager::set_value(dynamic_config, "database.port", 70000) // Invalid port
  let invalid_result = ConfigurationManager::validate_schema(dynamic_config, schema)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() > 0)
  
  // Test configuration rollback
  ConfigurationManager::save_checkpoint(dynamic_config, "before_change")
  ConfigurationManager::set_value(dynamic_config, "server.port", 9999)
  
  // Verify the change
  match ConfigurationManager::get_int(dynamic_config, "server.port") {
    Some(port) => assert_eq(port, 9999)
    None => assert_true(false)
  }
  
  // Rollback to checkpoint
  ConfigurationManager::rollback_to_checkpoint(dynamic_config, "before_change")
  
  // Verify rollback
  match ConfigurationManager::get_int(dynamic_config, "server.port") {
    Some(port) => assert_not_eq(port, 9999)
    None => assert_true(false)
  }
}

// Test 7: Security and Encryption
test "security and encryption" {
  let security_manager = SecurityManager::new()
  
  // Test data encryption and decryption
  let plaintext = "Sensitive data that needs encryption"
  let encryption_key = SecurityManager::generate_key(security_manager, 256) // 256-bit key
  
  let encrypted_data = SecurityManager::encrypt(security_manager, plaintext, encryption_key)
  assert_true(encrypted_data.length() > 0)
  assert_not_eq(encrypted_data, plaintext)
  
  let decrypted_data = SecurityManager::decrypt(security_manager, encrypted_data, encryption_key)
  assert_eq(decrypted_data, plaintext)
  
  // Test hashing and verification
  let password = "user_password_123"
  let hashed_password = SecurityManager::hash_password(security_manager, password)
  assert_true(hashed_password.length() > 0)
  assert_not_eq(hashed_password, password)
  
  assert_true(SecurityManager::verify_password(security_manager, password, hashed_password))
  assert_false(SecurityManager::verify_password(security_manager, "wrong_password", hashed_password))
  
  // Test token generation and validation
  let user_id = "user123"
  let token = SecurityManager::generate_token(security_manager, user_id, 3600) // 1 hour expiry
  
  assert_true(token.length() > 0)
  
  let token_validation = SecurityManager::validate_token(security_manager, token)
  match token_validation {
    TokenValidation::Valid(user) => assert_eq(user, user_id)
    TokenValidation::Invalid(reason) => assert_true(false)
    TokenValidation::Expired => assert_true(false)
  }
  
  // Test token expiry
  let short_lived_token = SecurityManager::generate_token(security_manager, user_id, 1) // 1 second expiry
  TimeUtil::sleep(2000) // Wait 2 seconds
  
  let expired_validation = SecurityManager::validate_token(security_manager, short_lived_token)
  match expired_validation {
    TokenValidation::Valid(_) => assert_true(false)
    TokenValidation::Invalid(_) => assert_true(false)
    TokenValidation::Expired => assert_true(true)
  }
  
  // Test secure random number generation
  let random_bytes = SecurityManager::secure_random(security_manager, 32)
  assert_eq(random_bytes.length(), 32)
  
  // Verify randomness (simple check - not a true randomness test)
  let random_bytes2 = SecurityManager::secure_random(security_manager, 32)
  assert_not_eq(random_bytes, random_bytes2)
  
  // Test data masking
  let sensitive_data = "Credit Card: 4532-1234-5678-9012"
  let masked_data = SecurityManager::mask_sensitive_data(security_manager, sensitive_data, "credit_card")
  
  assert_true(masked_data.contains("4532"))
  assert_false(masked_data.contains("5678"))
  assert_false(masked_data.contains("9012"))
}

// Test 8: Performance Optimization and Load Balancing
test "performance optimization and load balancing" {
  let performance_manager = PerformanceManager::new()
  
  // Test connection pooling
  let connection_pool = PerformanceManager::create_connection_pool(
    performance_manager,
    "database_connection_string",
    10, // min connections
    50  // max connections
  )
  
  // Get connections from pool
  let connections = []
  for i = 0; i < 20; i = i + 1 {
    let conn = PerformanceManager::get_connection(connection_pool)
    connections.push(conn)
  }
  
  assert_eq(connections.length(), 20)
  
  // Return connections to pool
  for conn in connections {
    PerformanceManager::return_connection(connection_pool, conn)
  }
  
  // Verify pool statistics
  let pool_stats = PerformanceManager::get_pool_stats(connection_pool)
  assert_true(pool_stats.active_connections >= 10)
  assert_true(pool_stats.total_connections <= 50)
  
  // Test load balancing strategies
  let servers = [
    "server1.example.com",
    "server2.example.com",
    "server3.example.com"
  ]
  
  // Round-robin load balancing
  let round_robin_balancer = PerformanceManager::create_round_robin_balancer(performance_manager, servers)
  
  let selections = []
  for i = 0; i < 9; i = i + 1 {
    let server = PerformanceManager::select_server(round_robin_balancer)
    selections.push(server)
  }
  
  // Each server should be selected 3 times
  assert_eq(selections.filter(fn(s) { s == "server1.example.com" }).length(), 3)
  assert_eq(selections.filter(fn(s) { s == "server2.example.com" }).length(), 3)
  assert_eq(selections.filter(fn(s) { s == "server3.example.com" }).length(), 3)
  
  // Weighted load balancing
  let weighted_servers = [
    ("server1.example.com", 1),  // Weight 1
    ("server2.example.com", 2),  // Weight 2
    ("server3.example.com", 3)   // Weight 3
  ]
  
  let weighted_balancer = PerformanceManager::create_weighted_balancer(performance_manager, weighted_servers)
  
  let weighted_selections = []
  for i = 0; i < 60; i = i + 1 {
    let server = PerformanceManager::select_server(weighted_balancer)
    weighted_selections.push(server)
  }
  
  // Server3 should be selected most often (weight 3)
  let server3_count = weighted_selections.filter(fn(s) { s == "server3.example.com" }).length()
  let server2_count = weighted_selections.filter(fn(s) { s == "server2.example.com" }).length()
  let server1_count = weighted_selections.filter(fn(s) { s == "server1.example.com" }).length()
  
  assert_true(server3_count > server2_count)
  assert_true(server2_count > server1_count)
  
  // Test adaptive throttling
  let throttler = PerformanceManager::create_adaptive_throttler(
    performance_manager,
    100,  // initial rate limit (requests per second)
    1000  // burst capacity
  )
  
  // Make requests within limit
  let within_limit_results = []
  for i = 0; i < 50; i = i + 1 {
    let result = PerformanceManager::check_rate_limit(throttler)
    within_limit_results.push(result)
  }
  
  // All should be allowed
  assert_true(within_limit_results.filter(fn(r) { r == false }).length() == 0)
  
  // Make requests that exceed limit
  let exceed_limit_results = []
  for i = 0; i < 2000; i = i + 1 {
    let result = PerformanceManager::check_rate_limit(throttler)
    exceed_limit_results.push(result)
  }
  
  // Some should be throttled
  assert_true(exceed_limit_results.filter(fn(r) { r == false }).length() > 0)
}

// Test 9: Logging and Audit Trail
test "logging and audit trail" {
  let logging_manager = LoggingManager::new()
  
  // Test structured logging
  let logger = LoggingManager::create_logger(logging_manager, "test_component")
  
  LoggingManager::log_info(logger, "Test message", [
    ("user_id", "12345"),
    ("action", "test_action"),
    ("timestamp", TimeUtil::current_timestamp())
  ])
  
  LoggingManager::log_warning(logger, "Test warning", [
    ("user_id", "12345"),
    ("warning_code", "TEST_001")
  ])
  
  LoggingManager::log_error(logger, "Test error", [
    ("user_id", "12345"),
    ("error_code", "TEST_002"),
    ("stack_trace", "at test_function:10")
  ])
  
  // Test log filtering and searching
  let log_filter = LogFilter::new()
  LogFilter::add_level_filter(log_filter, LogLevel::Warning)
  LogFilter::add_time_range_filter(log_filter, TimeUtil::current_timestamp() - 3600000, TimeUtil::current_timestamp())
  
  let filtered_logs = LoggingManager::search_logs(logging_manager, log_filter)
  assert_true(filtered_logs.length() >= 1) // At least the warning message
  
  // Test audit trail
  let audit_trail = LoggingManager::create_audit_trail(logging_manager, "security_audit")
  
  // Record audit events
  LoggingManager::audit_event(audit_trail, "user_login", [
    ("user_id", "12345"),
    ("ip_address", "192.168.1.100"),
    ("timestamp", TimeUtil::current_timestamp())
  ])
  
  LoggingManager::audit_event(audit_trail, "data_access", [
    ("user_id", "12345"),
    ("resource", "sensitive_data"),
    ("action", "read"),
    ("timestamp", TimeUtil::current_timestamp())
  ])
  
  LoggingManager::audit_event(audit_trail, "user_logout", [
    ("user_id", "12345"),
    ("session_duration", 3600000), // 1 hour in milliseconds
    ("timestamp", TimeUtil::current_timestamp())
  ])
  
  // Test audit trail querying
  let user_audit_events = LoggingManager::query_audit_trail(audit_trail, [
    ("user_id", "12345")
  ])
  
  assert_eq(user_audit_events.length(), 3)
  
  // Verify events are in chronological order
  assert_true(user_audit_events[0].event_type == "user_login")
  assert_true(user_audit_events[1].event_type == "data_access")
  assert_true(user_audit_events[2].event_type == "user_logout")
  
  // Test log retention policies
  let retention_policy = RetentionPolicy::new()
  RetentionPolicy::add_rule(retention_policy, LogLevel::Info, 2592000000) // 30 days
  RetentionPolicy::add_rule(retention_policy, LogLevel::Warning, 7776000000) // 90 days
  RetentionPolicy::add_rule(retention_policy, LogLevel::Error, 15552000000) // 180 days
  
  LoggingManager::apply_retention_policy(logging_manager, retention_policy)
  
  // Test log aggregation and metrics
  let log_metrics = LoggingManager::generate_log_metrics(logging_manager, TimeUtil::current_timestamp() - 86400000) // Last 24 hours
  
  assert_true(log_metrics.total_logs > 0)
  assert_true(log_metrics.error_count >= 0)
  assert_true(log_metrics.warning_count >= 0)
  assert_true(log_metrics.info_count >= 0)
}

// Test 10: Cross-Platform Compatibility and Portability
test "cross-platform compatibility and portability" {
  let platform_adapter = PlatformAdapter::new()
  
  // Test platform detection
  let platform_info = PlatformAdapter::detect_platform(platform_adapter)
  assert_true(platform_info.os_name.length() > 0)
  assert_true(platform_info.architecture.length() > 0)
  
  // Test path handling across platforms
  let path_components = ["home", "user", "documents", "test.txt"]
  let platform_path = PlatformAdapter::join_paths(platform_adapter, path_components)
  
  // Path should contain all components
  for component in path_components {
    assert_true(platform_path.contains(component))
  }
  
  // Test file system operations
  let test_content = "Test content for cross-platform compatibility"
  let temp_dir = PlatformAdapter::create_temp_directory(platform_adapter)
  let test_file = PlatformAdapter::join_paths(platform_adapter, [temp_dir, "test_file.txt"])
  
  // Write to file
  let write_result = PlatformAdapter::write_file(platform_adapter, test_file, test_content)
  assert_true(write_result.is_success)
  
  // Read from file
  let read_result = PlatformAdapter::read_file(platform_adapter, test_file)
  match read_result {
    Success(content) => assert_eq(content, test_content)
    Error(_) => assert_true(false)
  }
  
  // Test file permissions
  let permissions = FilePermissions::new()
  FilePermissions::set_read(permissions, true)
  FilePermissions::set_write(permissions, false)
  FilePermissions::set_execute(permissions, false)
  
  let permission_result = PlatformAdapter::set_file_permissions(platform_adapter, test_file, permissions)
  assert_true(permission_result.is_success)
  
  // Test environment variable handling
  let test_env_key = "AZIMUTH_PLATFORM_TEST"
  let test_env_value = "test_value_" + platform_info.os_name
  
  // Set environment variable
  PlatformAdapter::set_environment_variable(platform_adapter, test_env_key, test_env_value)
  
  // Get environment variable
  match PlatformAdapter::get_environment_variable(platform_adapter, test_env_key) {
    Some(value) => assert_eq(value, test_env_value)
    None => assert_true(false)
  }
  
  // Test process management
  let process_command = if platform_info.os_name.contains("Windows") {
    "echo Hello from Windows"
  } else {
    "echo 'Hello from Unix-like system'"
  }
  
  let process_result = PlatformAdapter::execute_command(platform_adapter, process_command)
  match process_result {
    Success(output) => assert_true(output.contains("Hello"))
    Error(_) => assert_true(false)
  }
  
  // Test network interface detection
  let network_interfaces = PlatformAdapter::get_network_interfaces(platform_adapter)
  assert_true(network_interfaces.length() > 0)
  
  // Verify at least one interface has an IP address
  let has_ip = network_interfaces.any(fn(interface) {
    interface.ip_address.length() > 0
  })
  assert_true(has_ip)
  
  // Test system resource monitoring
  let system_resources = PlatformAdapter::get_system_resources(platform_adapter)
  assert_true(system_resources.total_memory > 0)
  assert_true(system_resources.available_memory > 0)
  assert_true(system_resources.available_memory <= system_resources.total_memory)
  assert_true(system_resources.cpu_count > 0)
  
  // Clean up
  PlatformAdapter::delete_file(platform_adapter, test_file)
  PlatformAdapter::delete_directory(platform_adapter, temp_dir)
  PlatformAdapter::unset_environment_variable(platform_adapter, test_env_key)
}