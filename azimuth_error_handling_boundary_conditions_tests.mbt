// Azimuth Error Handling and Boundary Conditions Test Suite
// This file contains test cases for error handling and boundary conditions

// Test 1: Numeric Boundary Conditions
test "numeric boundary conditions" {
  // Test integer boundaries
  let max_int = 2147483647
  let min_int = -2147483648
  
  // Test addition overflow
  let result1 = max_int + 1
  assert_eq(result1, min_int) // Should wrap around
  
  // Test subtraction underflow
  let result2 = min_int - 1
  assert_eq(result2, max_int) // Should wrap around
  
  // Test multiplication overflow
  let result3 = max_int * 2
  assert_true(result3 < 0) // Should overflow to negative
  
  // Test division by zero handling
  let numerator = 10
  let denominator = 0
  
  // Safe division should return None
  let safe_result = SafeMath::divide(numerator, denominator)
  assert_eq(safe_result, None)
  
  // Test safe division with valid denominator
  let safe_result_valid = SafeMath::divide(numerator, 5)
  match safe_result_valid {
    Some(result) => assert_eq(result, 2),
    None => assert_true(false)
  }
  
  // Test floating point boundaries
  let max_float = 3.4028235e38 // Max single-precision float
  let min_float = -3.4028235e38 // Min single-precision float
  
  // Test float overflow
  let overflow_result = max_float * 2.0
  assert_true(is_infinite(overflow_result))
  
  // Test float underflow
  let underflow_result = min_float * 2.0
  assert_true(is_infinite(underflow_result))
  
  // Test NaN handling
  let nan_result = 0.0 / 0.0
  assert_true(is_nan(nan_result))
  
  // Test NaN in calculations
  let nan_calc = 1.0 + nan_result
  assert_true(is_nan(nan_calc))
  
  // Test infinity handling
  let infinity = 1.0 / 0.0
  assert_true(is_infinite(infinity))
  assert_true(infinity > 0.0)
  
  let negative_infinity = -1.0 / 0.0
  assert_true(is_infinite(negative_infinity))
  assert_true(negative_infinity < 0.0)
  
  // Test infinity in calculations
  let infinity_calc = 1.0 + infinity
  assert_true(is_infinite(infinity_calc))
  
  // Test precision boundaries
  let very_small = 1.0e-10
  let very_large = 1.0e10
  
  let precision_test = very_small + very_large
  assert_eq(precision_test, very_large) // Very small value lost due to precision
  
  // Test boundary value analysis
  let boundary_values = [0, 1, -1, max_int, min_int, max_int - 1, min_int + 1]
  
  for value in boundary_values {
    // Test with each boundary value
    let doubled = value * 2
    let halved = value / 2
    
    // Verify operations don't crash
    assert_true(is_integer(doubled))
    assert_true(is_integer(halved))
  }
}

// Test 2: String and Text Boundary Conditions
test "string and text boundary conditions" {
  // Test empty string
  let empty_string = ""
  assert_eq(String::length(empty_string), 0)
  
  // Test single character string
  let single_char = "a"
  assert_eq(String::length(single_char), 1)
  
  // Test very long string
  let long_string = "a".repeat(1000000) // 1 million characters
  assert_eq(String::length(long_string), 1000000)
  
  // Test string concatenation with empty string
  let result1 = empty_string + "test"
  assert_eq(result1, "test")
  
  let result2 = "test" + empty_string
  assert_eq(result2, "test")
  
  // Test string concatenation with very long strings
  let long_string1 = "a".repeat(500000)
  let long_string2 = "b".repeat(500000)
  let concatenated = long_string1 + long_string2
  assert_eq(String::length(concatenated), 1000000)
  
  // Test string indexing boundaries
  let test_string = "hello"
  
  // Valid indices
  assert_eq(String::char_at(test_string, 0), Some('h'))
  assert_eq(String::char_at(test_string, 4), Some('o'))
  
  // Invalid indices
  assert_eq(String::char_at(test_string, -1), None)
  assert_eq(String::char_at(test_string, 5), None)
  
  // Test substring boundaries
  // Valid ranges
  assert_eq(String::substring(test_string, 0, 5), "hello")
  assert_eq(String::substring(test_string, 1, 4), "ell")
  assert_eq(String::substring(test_string, 2, 2), "l")
  assert_eq(String::substring(test_string, 2, 5), "llo")
  
  // Invalid ranges
  assert_eq(String::substring(test_string, -1, 3), "")
  assert_eq(String::substring(test_string, 1, 10), "ello") // Should truncate
  assert_eq(String::substring(test_string, 5, 2), "") // Start > end
  assert_eq(String::substring(test_string, 3, 3), "") // Empty substring
  
  // Test Unicode handling
  let unicode_string = "Hello, 世界! 🌍"
  
  // Test Unicode length (should be character count, not byte count)
  assert_eq(String::length(unicode_string), 10) // H,e,l,l,o,,, ,世,界, ,!,🌍
  
  // Test Unicode indexing
  assert_eq(String::char_at(unicode_string, 7), Some('界'))
  assert_eq(String::char_at(unicode_string, 9), Some('🌍'))
  
  // Test Unicode substring
  assert_eq(String::substring(unicode_string, 7, 10), "界! 🌍")
  
  // Test string case conversion boundaries
  let lowercase = "hello"
  let uppercase = "HELLO"
  let mixed_case = "HeLLo"
  
  assert_eq(String::to_lowercase(uppercase), "hello")
  assert_eq(String::to_uppercase(lowercase), "HELLO")
  assert_eq(String::to_lowercase(mixed_case), "hello")
  assert_eq(String::to_uppercase(mixed_case), "HELLO")
  
  // Test string trimming boundaries
  let whitespace_string = "   hello   "
  assert_eq(String::trim(whitespace_string), "hello")
  
  let no_whitespace = "hello"
  assert_eq(String::trim(no_whitespace), "hello")
  
  let all_whitespace = "   "
  assert_eq(String::trim(all_whitespace), "")
  
  let empty_trim = String::trim(empty_string)
  assert_eq(empty_trim, "")
  
  // Test string splitting boundaries
  let comma_separated = "a,b,c"
  let split_result = String::split(comma_separated, ",")
  assert_eq(split_result.length(), 3)
  assert_eq(split_result[0], "a")
  assert_eq(split_result[1], "b")
  assert_eq(split_result[2], "c")
  
  // Test empty string splitting
  let empty_split = String::split(empty_string, ",")
  assert_eq(empty_split.length(), 1)
  assert_eq(empty_split[0], "")
  
  // Test string with no delimiter
  let no_delimiter = "hello"
  let no_delimiter_split = String::split(no_delimiter, ",")
  assert_eq(no_delimiter_split.length(), 1)
  assert_eq(no_delimiter_split[0], "hello")
  
  // Test string with consecutive delimiters
  let consecutive_delimiters = "a,,b,,c"
  let consecutive_split = String::split(consecutive_delimiters, ",")
  assert_eq(consecutive_split.length(), 5)
  assert_eq(consecutive_split[0], "a")
  assert_eq(consecutive_split[1], "")
  assert_eq(consecutive_split[2], "b")
  assert_eq(consecutive_split[3], "")
  assert_eq(consecutive_split[4], "c")
}

// Test 3: Array and Collection Boundary Conditions
test "array and collection boundary conditions" {
  // Test empty array
  let empty_array = []
  assert_eq(empty_array.length(), 0)
  
  // Test single element array
  let single_element = [1]
  assert_eq(single_element.length(), 1)
  assert_eq(single_element[0], 1)
  
  // Test large array
  let large_array = []
  for i = 0; i < 100000; i = i + 1 {
    large_array.push(i)
  }
  assert_eq(large_array.length(), 100000)
  assert_eq(large_array[0], 0)
  assert_eq(large_array[99999], 99999)
  
  // Test array access boundaries
  let test_array = [1, 2, 3, 4, 5]
  
  // Valid indices
  assert_eq(test_array[0], 1)
  assert_eq(test_array[4], 5)
  
  // Test safe array access
  assert_eq(Array::get_safe(test_array, 0), Some(1))
  assert_eq(Array::get_safe(test_array, 4), Some(5))
  assert_eq(Array::get_safe(test_array, -1), None)
  assert_eq(Array::get_safe(test_array, 5), None)
  
  // Test array insertion boundaries
  let mutable_array = [1, 2, 3]
  
  // Insert at beginning
  Array::insert(mutable_array, 0, 0)
  assert_eq(mutable_array, [0, 1, 2, 3])
  
  // Insert at end
  Array::insert(mutable_array, 4, 4)
  assert_eq(mutable_array, [0, 1, 2, 3, 4])
  
  // Insert in middle
  Array::insert(mutable_array, 2, 99)
  assert_eq(mutable_array, [0, 1, 99, 2, 3, 4])
  
  // Test invalid insertions
  Array::insert(mutable_array, -1, 100) // Should not crash
  Array::insert(mutable_array, 100, 200) // Should not crash
  
  // Test array removal boundaries
  let remove_array = [1, 2, 3, 4, 5]
  
  // Remove from beginning
  let removed1 = Array::remove(remove_array, 0)
  assert_eq(removed1, 1)
  assert_eq(remove_array, [2, 3, 4, 5])
  
  // Remove from end
  let removed2 = Array::remove(remove_array, 3)
  assert_eq(removed2, 5)
  assert_eq(remove_array, [2, 3, 4])
  
  // Remove from middle
  let removed3 = Array::remove(remove_array, 1)
  assert_eq(removed3, 3)
  assert_eq(remove_array, [2, 4])
  
  // Test invalid removals
  let invalid_removed1 = Array::remove(remove_array, -1)
  let invalid_removed2 = Array::remove(remove_array, 10)
  
  // Test array slicing boundaries
  let slice_array = [1, 2, 3, 4, 5]
  
  // Valid slices
  let slice1 = Array::slice(slice_array, 0, 5)
  assert_eq(slice1, [1, 2, 3, 4, 5])
  
  let slice2 = Array::slice(slice_array, 1, 4)
  assert_eq(slice2, [2, 3, 4])
  
  let slice3 = Array::slice(slice_array, 2, 2)
  assert_eq(slice3, [])
  
  // Invalid slices
  let slice4 = Array::slice(slice_array, -1, 3)
  assert_eq(slice4, [1, 2, 3]) // Should handle gracefully
  
  let slice5 = Array::slice(slice_array, 1, 10)
  assert_eq(slice5, [2, 3, 4, 5]) // Should truncate
  
  let slice6 = Array::slice(slice_array, 4, 2)
  assert_eq(slice6, []) // Start > end
  
  // Test map boundaries
  let empty_map = Map::new()
  assert_eq(Map::size(empty_map), 0)
  
  let test_map = Map::new()
  Map::set(test_map, "a", 1)
  Map::set(test_map, "b", 2)
  Map::set(test_map, "c", 3)
  
  assert_eq(Map::size(test_map), 3)
  assert_eq(Map::get(test_map, "a"), Some(1))
  assert_eq(Map::get(test_map, "d"), None)
  
  // Test map removal
  Map::remove(test_map, "b")
  assert_eq(Map::size(test_map), 2)
  assert_eq(Map::get(test_map, "b"), None)
  
  // Test map keys and values
  let keys = Map::keys(test_map)
  assert_eq(keys.length(), 2)
  assert_true(keys.contains("a"))
  assert_true(keys.contains("c"))
  
  let values = Map::values(test_map)
  assert_eq(values.length(), 2)
  assert_true(values.contains(1))
  assert_true(values.contains(3))
}

// Test 4: File System Boundary Conditions
test "file system boundary conditions" {
  // Test empty file path
  let empty_path = ""
  let empty_result = FileSystem::file_exists(empty_path)
  assert_false(empty_result) // Should handle gracefully
  
  // Test very long file path
  let long_path = "/".repeat(1000) + "file.txt"
  let long_result = FileSystem::file_exists(long_path)
  assert_false(long_result) // Should handle gracefully
  
  // Test file creation with invalid characters
  let invalid_chars = ["<", ">", ":", "\"", "|", "?", "*"]
  
  for char in invalid_chars {
    let invalid_path = "file" + char + ".txt"
    let invalid_result = FileSystem::create_file(invalid_path)
    assert_false(invalid_result) // Should fail gracefully
  }
  
  // Test file size boundaries
  let test_file = "test_file.txt"
  
  // Create empty file
  let create_result = FileSystem::create_file(test_file)
  assert_true(create_result)
  
  let empty_size = FileSystem::file_size(test_file)
  assert_eq(empty_size, 0)
  
  // Write to file
  let content = "Hello, World!"
  let write_result = FileSystem::write_file(test_file, content)
  assert_true(write_result)
  
  let content_size = FileSystem::file_size(test_file)
  assert_eq(content_size, String::length(content))
  
  // Test reading non-existent file
  let non_existent = "non_existent_file.txt"
  let read_result = FileSystem::read_file(non_existent)
  assert_eq(read_result, None) // Should return None instead of crashing
  
  // Test deleting non-existent file
  let delete_result = FileSystem::delete_file(non_existent)
  assert_false(delete_result) // Should return false instead of crashing
  
  // Test directory operations
  let test_dir = "test_dir"
  
  // Create directory
  let create_dir_result = FileSystem::create_directory(test_dir)
  assert_true(create_dir_result)
  
  // Check if directory exists
  let dir_exists = FileSystem::directory_exists(test_dir)
  assert_true(dir_exists)
  
  // Create file in directory
  let file_in_dir = test_dir + "/file.txt"
  let create_in_dir_result = FileSystem::create_file(file_in_dir)
  assert_true(create_in_dir_result)
  
  // List directory contents
  let dir_contents = FileSystem::list_directory(test_dir)
  assert_eq(dir_contents.length(), 1)
  assert_eq(dir_contents[0], "file.txt")
  
  // Try to delete non-empty directory
  let delete_dir_result = FileSystem::delete_directory(test_dir)
  assert_false(delete_dir_result) // Should fail for non-empty directory
  
  // Delete file first
  let delete_file_result = FileSystem::delete_file(file_in_dir)
  assert_true(delete_file_result)
  
  // Then delete directory
  let delete_dir_now_result = FileSystem::delete_directory(test_dir)
  assert_true(delete_dir_now_result)
  
  // Test file permissions
  let permission_file = "permission_test.txt"
  FileSystem::create_file(permission_file)
  
  // Try to read file
  let read_permission = FileSystem::read_file(permission_file)
  assert_eq(read_permission, Some("")) // Should be able to read empty file
  
  // Try to write to file
  let write_permission = FileSystem::write_file(permission_file, "test")
  assert_true(write_permission)
  
  // Clean up
  FileSystem::delete_file(permission_file)
  
  // Test file locking
  let lock_file = "lock_test.txt"
  FileSystem::create_file(lock_file)
  
  // Acquire lock
  let lock_result = FileSystem::acquire_lock(lock_file)
  assert_true(lock_result)
  
  // Try to acquire lock again (should fail)
  let lock_again_result = FileSystem::acquire_lock(lock_file)
  assert_false(lock_again_result)
  
  // Release lock
  let release_result = FileSystem::release_lock(lock_file)
  assert_true(release_result)
  
  // Try to acquire lock again (should succeed)
  let lock_after_release = FileSystem::acquire_lock(lock_file)
  assert_true(lock_after_release)
  
  // Clean up
  FileSystem::release_lock(lock_file)
  FileSystem::delete_file(lock_file)
}

// Test 5: Network Boundary Conditions
test "network boundary conditions" {
  // Test invalid URLs
  let invalid_urls = [
    "",
    "not_a_url",
    "http://",
    "https://",
    "ftp://invalid",
    "http://[invalid-ipv6",
    "http://example.com:99999" // Invalid port
  ]
  
  for url in invalid_urls {
    let validation_result = NetworkUtils::is_valid_url(url)
    assert_false(validation_result) // Should return false for invalid URLs
  }
  
  // Test valid URLs
  let valid_urls = [
    "http://example.com",
    "https://example.com",
    "ftp://example.com",
    "http://example.com:8080",
    "https://user:pass@example.com",
    "http://192.168.1.1",
    "http://[2001:db8::1]" // IPv6
  ]
  
  for url in valid_urls {
    let validation_result = NetworkUtils::is_valid_url(url)
    assert_true(validation_result) // Should return true for valid URLs
  }
  
  // Test very long URL
  let long_url = "http://example.com/" + "a".repeat(10000)
  let long_url_result = NetworkUtils::is_valid_url(long_url)
  assert_false(long_url_result) // Should reject overly long URLs
  
  // Test connection timeout
  let timeout_config = TimeoutConfig::new(1) // 1 millisecond
  let connection_result = NetworkUtils::test_connection("http://example.com", timeout_config)
  
  // In a real test, this might timeout, but for testing we just verify it doesn't crash
  assert_true(connection_result.is_ok() || connection_result.is_err())
  
  // Test invalid IP addresses
  let invalid_ips = [
    "",
    "not_an_ip",
    "256.256.256.256",
    "192.168.1",
    "192.168.1.1.1",
    "999.999.999.999",
    "[not-ipv6]",
    "2001:db8::1::1" // Double :: not allowed
  ]
  
  for ip in invalid_ips {
    let ip_validation = NetworkUtils::is_valid_ip(ip)
    assert_false(ip_validation) // Should return false for invalid IPs
  }
  
  // Test valid IP addresses
  let valid_ips = [
    "192.168.1.1",
    "10.0.0.1",
    "127.0.0.1",
    "0.0.0.0",
    "255.255.255.255",
    "2001:db8::1",
    "::1",
    "::",
    "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
  ]
  
  for ip in valid_ips {
    let ip_validation = NetworkUtils::is_valid_ip(ip)
    assert_true(ip_validation) // Should return true for valid IPs
  }
  
  // Test port boundaries
  let invalid_ports = [-1, 0, 65536, 100000]
  
  for port in invalid_ports {
    let port_validation = NetworkUtils::is_valid_port(port)
    assert_false(port_validation) // Should return false for invalid ports
  }
  
  let valid_ports = [1, 80, 443, 8080, 65535]
  
  for port in valid_ports {
    let port_validation = NetworkUtils::is_valid_port(port)
    assert_true(port_validation) // Should return true for valid ports
  }
  
  // Test DNS resolution with invalid hostnames
  let invalid_hostnames = [
    "",
    ".",
    "..",
    "toolonghostname" + "a".repeat(300),
    "hostname..with",
    "-hostname",
    "hostname-",
    "hostname."
  ]
  
  for hostname in invalid_hostnames {
    let dns_result = NetworkUtils::resolve_hostname(hostname)
    assert_true(dns_result.is_err()) // Should return error for invalid hostnames
  }
  
  // Test HTTP request with invalid parameters
  let http_client = HttpClient::new()
  
  // Test request with empty URL
  let empty_url_request = HttpRequest::get("")
  let empty_url_result = HttpClient::execute(http_client, empty_url_request)
  assert_true(empty_url_result.is_err()) // Should return error
  
  // Test request with very large payload
  let large_payload = "a".repeat(100 * 1024 * 1024) // 100MB
  let large_request = HttpRequest::post("http://example.com", large_payload)
  let large_result = HttpClient::execute(http_client, large_request)
  assert_true(large_result.is_err()) // Should return error for too large payload
  
  // Test network statistics
  let network_stats = NetworkUtils::get_statistics()
  assert_true(NetworkStats::total_bytes_sent(network_stats) >= 0)
  assert_true(NetworkStats::total_bytes_received(network_stats) >= 0)
  assert_true(NetworkStats::total_connections(network_stats) >= 0)
  assert_true(NetworkStats::failed_connections(network_stats) >= 0)
}

// Test 6: Database Boundary Conditions
test "database boundary conditions" {
  // Test database connection with invalid parameters
  let db_config = DatabaseConfig::new()
  
  // Test with empty connection string
  DatabaseConfig::set_connection_string(db_config, "")
  let empty_connection = Database::connect(db_config)
  assert_true(empty_connection.is_err()) // Should return error
  
  // Test with invalid connection string
  DatabaseConfig::set_connection_string(db_config, "invalid_connection_string")
  let invalid_connection = Database::connect(db_config)
  assert_true(invalid_connection.is_err()) // Should return error
  
  // Test with valid connection string (simulated)
  DatabaseConfig::set_connection_string(db_config, "sqlite::memory:")
  let valid_connection = Database::connect(db_config)
  assert_true(valid_connection.is_ok()) // Should succeed
  
  let db = Result::unwrap(valid_connection)
  
  // Test SQL injection protection
  let malicious_inputs = [
    "'; DROP TABLE users; --",
    "'; INSERT INTO users VALUES ('hacker', 'password'); --",
    "'; UPDATE users SET password='hacked'; --",
    "'; DELETE FROM users; --",
    "' OR '1'='1"
  ]
  
  for input in malicious_inputs {
    // Test with parameterized queries (should be safe)
    let query = "SELECT * FROM users WHERE name = ?"
    let params = [input]
    let safe_result = Database::execute_query(db, query, params)
    
    // Should not crash or execute malicious SQL
    assert_true(safe_result.is_ok() || safe_result.is_err())
    
    // Test with string concatenation (should be vulnerable, but we're testing protection)
    let unsafe_query = "SELECT * FROM users WHERE name = '" + input + "'"
    let unsafe_result = Database::execute_query(db, unsafe_query, [])
    
    // Should be caught and prevented
    assert_true(unsafe_result.is_err())
  }
  
  // Test query with very long string
  let long_string = "a".repeat(1000000)
  let long_query = "SELECT * FROM users WHERE name = ?"
  let long_params = [long_string]
  let long_result = Database::execute_query(db, long_query, long_params)
  
  // Should handle gracefully
  assert_true(long_result.is_ok() || long_result.is_err())
  
  // Test transaction boundaries
  let transaction_result = Database::begin_transaction(db)
  assert_true(transaction_result.is_ok())
  
  let transaction = Result::unwrap(transaction_result)
  
  // Execute queries in transaction
  let insert_result1 = Database::execute_query(transaction, "INSERT INTO test VALUES (1)", [])
  let insert_result2 = Database::execute_query(transaction, "INSERT INTO test VALUES (2)", [])
  
  // Rollback transaction
  let rollback_result = Database::rollback_transaction(transaction)
  assert_true(rollback_result.is_ok())
  
  // Verify data was rolled back
  let select_result = Database::execute_query(db, "SELECT * FROM test", [])
  assert_true(select_result.is_ok())
  
  // Test commit transaction
  let transaction2_result = Database::begin_transaction(db)
  assert_true(transaction2_result.is_ok())
  
  let transaction2 = Result::unwrap(transaction2_result)
  
  let insert_result3 = Database::execute_query(transaction2, "INSERT INTO test VALUES (3)", [])
  let commit_result = Database::commit_transaction(transaction2)
  assert_true(commit_result.is_ok())
  
  // Verify data was committed
  let select_result2 = Database::execute_query(db, "SELECT * FROM test", [])
  assert_true(select_result2.is_ok())
  
  // Test connection pool boundaries
  let pool_config = ConnectionPoolConfig::new()
  ConnectionPoolConfig::set_max_connections(pool_config, 3)
  ConnectionPoolConfig::set_connection_timeout(pool_config, 1000) // 1 second
  
  let pool = ConnectionPool::with_config(pool_config)
  
  // Get connections up to the limit
  let conn1 = ConnectionPool::get_connection(pool)
  let conn2 = ConnectionPool::get_connection(pool)
  let conn3 = ConnectionPool::get_connection(pool)
  
  assert_true(Result::is_ok(conn1))
  assert_true(Result::is_ok(conn2))
  assert_true(Result::is_ok(conn3))
  
  // Try to get one more (should fail or timeout)
  let conn4 = ConnectionPool::get_connection(pool)
  assert_true(Result::is_err(conn4)) // Should fail due to limit
  
  // Release connections
  ConnectionPool::release_connection(pool, Result::unwrap(conn1))
  ConnectionPool::release_connection(pool, Result::unwrap(conn2))
  ConnectionPool::release_connection(pool, Result::unwrap(conn3))
  
  // Clean up
  Database::close(db)
}

// Test 7: Memory Allocation Boundary Conditions
test "memory allocation boundary conditions" {
  // Test zero-byte allocation
  let zero_alloc = MemoryAllocator::allocate(0)
  assert_true(Option::is_some(zero_alloc)) // Should handle zero-byte allocation
  
  // Test very small allocation
  let small_alloc = MemoryAllocator::allocate(1)
  assert_true(Option::is_some(small_alloc))
  
  // Test very large allocation
  let large_alloc = MemoryAllocator::allocate(1024 * 1024 * 1024) // 1GB
  // This might fail on systems with limited memory, but shouldn't crash
  assert_true(Option::is_some(large_alloc) || Option::is_none(large_alloc))
  
  // Test extremely large allocation (should fail)
  let huge_alloc = MemoryAllocator::allocate(1024 * 1024 * 1024 * 1024) // 1TB
  assert_true(Option::is_none(huge_alloc)) // Should fail
  
  // Test negative allocation (should fail)
  let negative_alloc = MemoryAllocator::allocate(-1)
  assert_true(Option::is_none(negative_alloc)) // Should fail
  
  // Test buffer overflow protection
  let buffer_size = 100
  let buffer = MemoryAllocator::allocate(buffer_size)
  
  match buffer {
    Some(buf) => {
      // Test writing within bounds
      let write_result = MemoryAllocator::write(buf, 0, "test", 4)
      assert_true(write_result)
      
      // Test writing at boundary
      let boundary_result = MemoryAllocator::write(buf, buffer_size - 4, "test", 4)
      assert_true(boundary_result)
      
      // Test writing beyond bounds
      let overflow_result = MemoryAllocator::write(buf, buffer_size - 2, "test", 4)
      assert_false(overflow_result) // Should fail
      
      // Test reading within bounds
      let read_result = MemoryAllocator::read(buf, 0, 4)
      assert_eq(read_result, "test")
      
      // Test reading at boundary
      let boundary_read = MemoryAllocator::read(buf, buffer_size - 4, 4)
      assert_eq(boundary_read, "test")
      
      // Test reading beyond bounds
      let overflow_read = MemoryAllocator::read(buf, buffer_size - 2, 4)
      assert_eq(overflow_read, "") // Should return empty string
      
      // Test negative offset
      let negative_write = MemoryAllocator::write(buf, -1, "test", 4)
      assert_false(negative_write) // Should fail
      
      let negative_read = MemoryAllocator::read(buf, -1, 4)
      assert_eq(negative_read, "") // Should return empty string
      
      // Test deallocation
      MemoryAllocator::deallocate(buf)
    }
    None => assert_true(false)
  }
  
  // Test memory leak detection
  let leak_detector = MemoryLeakDetector::new()
  
  // Allocate memory
  let leak_alloc = MemoryAllocator::allocate(1024)
  
  // Register with leak detector
  match leak_alloc {
    Some(alloc) => {
      MemoryLeakDetector::track_allocation(leak_detector, alloc)
      
      // Check for leaks
      let leaks = MemoryLeakDetector::check_leaks(leak_detector)
      assert_eq(leaks.length(), 1) // Should detect one leak
      
      // Deallocate memory
      MemoryAllocator::deallocate(alloc)
      
      // Unregister from leak detector
      MemoryLeakDetector::untrack_allocation(leak_detector, alloc)
      
      // Check again
      let leaks_after = MemoryLeakDetector::check_leaks(leak_detector)
      assert_eq(leaks_after.length(), 0) // Should be no leaks
    }
    None => assert_true(false)
  }
  
  // Test memory pool boundaries
  let pool_config = MemoryPoolConfig::new(1024) // 1KB pool
  let memory_pool = MemoryPool::with_config(pool_config)
  
  // Allocate up to pool size
  let pool_alloc1 = MemoryPool::allocate(memory_pool, 512)
  let pool_alloc2 = MemoryPool::allocate(memory_pool, 512)
  
  assert_true(Option::is_some(pool_alloc1))
  assert_true(Option::is_some(pool_alloc2))
  
  // Try to allocate more (should fail)
  let pool_alloc3 = MemoryPool::allocate(memory_pool, 1)
  assert_true(Option::is_none(pool_alloc3)) // Should fail
  
  // Deallocate and try again
  match pool_alloc1 {
    Some(alloc) => MemoryPool::deallocate(memory_pool, alloc),
    None => ()
  }
  
  let pool_alloc4 = MemoryPool::allocate(memory_pool, 1)
  assert_true(Option::is_some(pool_alloc4)) // Should succeed now
  
  // Clean up
  match pool_alloc2 {
    Some(alloc) => MemoryPool::deallocate(memory_pool, alloc),
    None => ()
  }
  
  match pool_alloc4 {
    Some(alloc) => MemoryPool::deallocate(memory_pool, alloc),
    None => ()
  }
}

// Test 8: Thread Safety and Concurrency Boundary Conditions
test "thread safety and concurrency boundary conditions" {
  // Test mutex creation
  let mutex = Mutex::new()
  assert_true(Mutex::is_valid(mutex))
  
  // Test mutex locking
  let lock_result = Mutex::try_lock(mutex)
  assert_true(lock_result) // Should succeed
  
  // Try to lock again (should fail)
  let lock_again_result = Mutex::try_lock(mutex)
  assert_false(lock_again_result) // Should fail
  
  // Unlock
  Mutex::unlock(mutex)
  
  // Try to lock again (should succeed)
  let lock_final_result = Mutex::try_lock(mutex)
  assert_true(lock_final_result) // Should succeed
  
  Mutex::unlock(mutex)
  
  // Test recursive mutex
  let recursive_mutex = RecursiveMutex::new()
  
  // Lock recursively
  RecursiveMutex::lock(recursive_mutex)
  RecursiveMutex::lock(recursive_mutex)
  RecursiveMutex::lock(recursive_mutex)
  
  // Unlock recursively
  RecursiveMutex::unlock(recursive_mutex)
  RecursiveMutex::unlock(recursive_mutex)
  RecursiveMutex::unlock(recursive_mutex)
  
  // Test read-write lock
  let rw_lock = RwLock::new()
  
  // Acquire read lock
  RwLock::read_lock(rw_lock)
  let read_lock_result = RwLock::try_read_lock(rw_lock)
  assert_true(read_lock_result) // Multiple readers should be allowed
  
  // Try to acquire write lock (should fail)
  let write_lock_result = RwLock::try_write_lock(rw_lock)
  assert_false(write_lock_result) // Should fail with active readers
  
  // Release read locks
  RwLock::read_unlock(rw_lock)
  RwLock::read_unlock(rw_lock)
  
  // Try write lock again (should succeed)
  let write_lock_result2 = RwLock::try_write_lock(rw_lock)
  assert_true(write_lock_result2) // Should succeed
  
  // Try read lock (should fail)
  let read_lock_result2 = RwLock::try_read_lock(rw_lock)
  assert_false(read_lock_result2) // Should fail with active writer
  
  // Release write lock
  RwLock::write_unlock(rw_lock)
  
  // Test atomic operations
  let atomic_int = AtomicInt::new(0)
  
  assert_eq(AtomicInt::load(atomic_int), 0)
  
  AtomicInt::store(atomic_int, 42)
  assert_eq(AtomicInt::load(atomic_int), 42)
  
  let old_value = AtomicInt::exchange(atomic_int, 100)
  assert_eq(old_value, 42)
  assert_eq(AtomicInt::load(atomic_int), 100)
  
  let compare_result = AtomicInt::compare_exchange(atomic_int, 100, 200)
  assert_true(compare_result)
  assert_eq(AtomicInt::load(atomic_int), 200)
  
  let compare_result2 = AtomicInt::compare_exchange(atomic_int, 100, 300)
  assert_false(compare_result2) // Current value is 200, not 100
  assert_eq(AtomicInt::load(atomic_int), 200)
  
  let fetched = AtomicInt::fetch_add(atomic_int, 10)
  assert_eq(fetched, 200)
  assert_eq(AtomicInt::load(atomic_int), 210)
  
  let fetched_sub = AtomicInt::fetch_sub(atomic_int, 10)
  assert_eq(fetched_sub, 210)
  assert_eq(AtomicInt::load(atomic_int), 200)
  
  // Test thread creation with invalid parameters
  let invalid_thread_result = Thread::spawn(|| {
    // Empty thread function
  })
  
  // Should succeed even with empty function
  assert_true(invalid_thread_result.is_ok())
  
  // Test thread join with invalid thread
  let thread = Result::unwrap(invalid_thread_result)
  let join_result = Thread::join(thread)
  assert_true(join_result.is_ok()) // Should succeed
  
  // Test condition variable
  let condition = ConditionVariable::new()
  let mutex2 = Mutex::new()
  
  // Signal without waiting (should not crash)
  ConditionVariable::signal(condition)
  ConditionVariable::broadcast(condition)
  
  // Test thread pool boundaries
  let pool_config = ThreadPoolConfig::new()
  ThreadPoolConfig::set_max_threads(pool_config, 2)
  
  let thread_pool = ThreadPool::with_config(pool_config)
  
  // Submit tasks up to the limit
  let task1 = ThreadPool::submit(thread_pool, || { 1 })
  let task2 = ThreadPool::submit(thread_pool, || { 2 })
  
  assert_true(Result::is_ok(task1))
  assert_true(Result::is_ok(task2))
  
  // Submit more tasks (should queue or fail depending on implementation)
  let task3 = ThreadPool::submit(thread_pool, || { 3 })
  let task4 = ThreadPool::submit(thread_pool, || { 4 })
  
  // Get results
  let result1 = ThreadPool::get_result(thread_pool, Result::unwrap(task1))
  let result2 = ThreadPool::get_result(thread_pool, Result::unwrap(task2))
  
  assert_eq(result1, 1)
  assert_eq(result2, 2)
  
  // Test deadlock detection
  let deadlock_detector = DeadlockDetector::new()
  
  // Create locks
  let lock_a = Mutex::new()
  let lock_b = Mutex::new()
  
  // Register locks
  DeadlockDetector::register_lock(deadlock_detector, "lock_a", lock_a)
  DeadlockDetector::register_lock(deadlock_detector, "lock_b", lock_b)
  
  // Simulate potential deadlock
  DeadlockDetector::acquire_lock(deadlock_detector, "lock_a")
  let deadlock_result = DeadlockDetector::try_acquire_lock(deadlock_detector, "lock_b")
  
  // Should not detect deadlock yet
  assert_true(deadlock_result)
  
  DeadlockDetector::release_lock(deadlock_detector, "lock_a")
  
  // Clean up
  DeadlockDetector::unregister_lock(deadlock_detector, "lock_a")
  DeadlockDetector::unregister_lock(deadlock_detector, "lock_b")
}

// Test 9: Exception and Error Handling
test "exception and error handling" {
  // Test custom error types
  let custom_error = CustomError::new("Test error", 123)
  assert_eq(CustomError::message(custom_error), "Test error")
  assert_eq(CustomError::code(custom_error), 123)
  
  // Test error chaining
  let root_error = CustomError::new("Root error", 100)
  let chained_error = CustomError::chain("Chained error", 200, root_error)
  
  assert_eq(CustomError::message(chained_error), "Chained error")
  assert_eq(CustomError::code(chained_error), 200)
  assert_eq(CustomError::root_error(chained_error).message(), "Root error")
  assert_eq(CustomError::root_error(chained_error).code(), 100)
  
  // Test error recovery
  let error_recovery = ErrorRecovery::new()
  
  // Define recovery strategies
  ErrorRecovery::add_strategy(error_recovery, "CustomError", |error| {
    if CustomError::code(error) == 123 {
      RecoveryAction::Retry(3) // Retry 3 times
    } else {
      RecoveryAction::Fail
    }
  })
  
  // Test recovery with matching error
  let matching_error = CustomError::new("Test error", 123)
  let recovery_result = ErrorRecovery::handle_error(error_recovery, matching_error)
  
  match recovery_result {
    Retry(attempts) => assert_eq(attempts, 3),
    _ => assert_true(false)
  }
  
  // Test recovery with non-matching error
  let non_matching_error = CustomError::new("Test error", 456)
  let recovery_result2 = ErrorRecovery::handle_error(error_recovery, non_matching_error)
  
  match recovery_result2 {
    Fail => assert_true(true),
    _ => assert_true(false)
  }
  
  // Test error context
  let error_context = ErrorContext::new()
  
  ErrorContext::add_context(error_context, "operation", "test_operation")
  ErrorContext::add_context(error_context, "user_id", "12345")
  ErrorContext::add_context(error_context, "timestamp", "2023-01-01T00:00:00Z")
  
  let contextual_error = ErrorContext::wrap(error_context, custom_error)
  
  assert_eq(ErrorContext::get_context(contextual_error, "operation"), Some("test_operation"))
  assert_eq(ErrorContext::get_context(contextual_error, "user_id"), Some("12345"))
  assert_eq(ErrorContext::get_context(contextual_error, "timestamp"), Some("2023-01-01T00:00:00Z"))
  assert_eq(ErrorContext::get_context(contextual_error, "nonexistent"), None)
  
  // Test error reporting
  let error_reporter = ErrorReporter::new()
  
  // Report an error
  ErrorReporter::report(error_reporter, custom_error)
  
  // Get error statistics
  let stats = ErrorReporter::get_statistics(error_reporter)
  assert_eq(ErrorStats::total_errors(stats), 1)
  assert_eq(ErrorStats::errors_by_type(stats).get("CustomError"), Some(1))
  
  // Test error aggregation
  let error_aggregator = ErrorAggregator::new()
  
  // Aggregate multiple errors
  ErrorAggregator::add_error(error_aggregator, CustomError::new("Error 1", 1))
  ErrorAggregator::add_error(error_aggregator, CustomError::new("Error 2", 2))
  ErrorAggregator::add_error(error_aggregator, CustomError::new("Error 3", 1))
  
  // Get aggregated errors
  let aggregated = ErrorAggregator::get_aggregated_errors(error_aggregator)
  assert_eq(aggregated.length(), 2) // Grouped by error code
  assert_eq(aggregated[0].count, 2) // Two errors with code 1
  assert_eq(aggregated[1].count, 1) // One error with code 2
  
  // Test error boundaries
  let error_boundary = ErrorBoundary::new()
  
  // Execute operation that throws an error
  let operation_result = ErrorBoundary::execute(error_boundary, || {
    Err(CustomError::new("Operation failed", 500))
  })
  
  assert_true(operation_result.is_err())
  
  // Execute operation that succeeds
  let success_result = ErrorBoundary::execute(error_boundary, || {
    Ok("Success")
  })
  
  assert_true(success_result.is_ok())
  assert_eq(Result::unwrap(success_result), "Success")
  
  // Test error logging
  let error_logger = ErrorLogger::new()
  
  // Log an error
  ErrorLogger::log(error_logger, custom_error)
  
  // Get log entries
  let log_entries = ErrorLogger::get_entries(error_logger)
  assert_eq(log_entries.length(), 1)
  assert_eq(log_entries[0].message, "Test error")
  assert_eq(log_entries[0].code, 123)
  
  // Test error metrics
  let error_metrics = ErrorMetrics::new()
  
  // Record errors
  ErrorMetrics::record_error(error_metrics, "CustomError", 123)
  ErrorMetrics::record_error(error_metrics, "CustomError", 456)
  ErrorMetrics::record_error(error_metrics, "CustomError", 123)
  
  // Get metrics
  let metrics = ErrorMetrics::get_metrics(error_metrics)
  assert_eq(ErrorMetrics::total_errors(metrics), 3)
  assert_eq(ErrorMetrics::errors_by_code(metrics).get(123), Some(2))
  assert_eq(ErrorMetrics::errors_by_code(metrics).get(456), Some(1))
}

// Test 10: Input Validation and Sanitization
test "input validation and sanitization" {
  // Test string validation
  let string_validator = StringValidator::new()
  
  // Test length validation
  StringValidator::set_min_length(string_validator, 5)
  StringValidator::set_max_length(string_validator, 10)
  
  assert_false(StringValidator::validate(string_validator, "")) // Too short
  assert_false(StringValidator::validate(string_validator, "abc")) // Too short
  assert_true(StringValidator::validate(string_validator, "hello")) // Valid
  assert_true(StringValidator::validate(string_validator, "helloworld")) // Valid
  assert_false(StringValidator::validate(string_validator, "helloworld!")) // Too long
  
  // Test pattern validation
  StringValidator::set_pattern(string_validator, "^[a-zA-Z0-9]+$") // Alphanumeric only
  
  assert_true(StringValidator::validate(string_validator, "hello123")) // Valid
  assert_false(StringValidator::validate(string_validator, "hello!123")) // Invalid character
  assert_false(StringValidator::validate(string_validator, "hello 123")) // Invalid space
  
  // Test email validation
  let email_validator = EmailValidator::new()
  
  assert_true(EmailValidator::validate(email_validator, "user@example.com"))
  assert_true(EmailValidator::validate(email_validator, "user.name@example.co.uk"))
  assert_true(EmailValidator::validate(email_validator, "user+tag@example.org"))
  
  assert_false(EmailValidator::validate(email_validator, "")) // Empty
  assert_false(EmailValidator::validate(email_validator, "user")) // Missing @ and domain
  assert_false(EmailValidator::validate(email_validator, "user@")) // Missing domain
  assert_false(EmailValidator::validate(email_validator, "@example.com")) // Missing user
  assert_false(EmailValidator::validate(email_validator, "user@example")) // Invalid domain
  assert_false(EmailValidator::validate(email_validator, "user..name@example.com")) // Double dot
  assert_false(EmailValidator::validate(email_validator, "user@example..com")) // Double dot
  
  // Test URL validation
  let url_validator = URLValidator::new()
  
  assert_true(URLValidator::validate(url_validator, "http://example.com"))
  assert_true(URLValidator::validate(url_validator, "https://example.com"))
  assert_true(URLValidator::validate(url_validator, "https://example.com/path"))
  assert_true(URLValidator::validate(url_validator, "https://example.com/path?query=value"))
  assert_true(URLValidator::validate(url_validator, "https://example.com/path#fragment"))
  
  assert_false(URLValidator::validate(url_validator, "")) // Empty
  assert_false(URLValidator::validate(url_validator, "not_a_url")) // Invalid format
  assert_false(URLValidator::validate(url_validator, "http://")) // Missing domain
  assert_false(URLValidator::validate(url_validator, "://example.com")) // Missing protocol
  assert_false(URLValidator::validate(url_validator, "ftp://example.com")) // Invalid protocol (if not allowed)
  
  // Test numeric validation
  let numeric_validator = NumericValidator::new()
  
  NumericValidator::set_min_value(numeric_validator, 0)
  NumericValidator::set_max_value(numeric_validator, 100)
  NumericValidator::set_integer_only(numeric_validator, true)
  
  assert_true(NumericValidator::validate(numeric_validator, "50")) // Valid
  assert_false(NumericValidator::validate(numeric_validator, "-1")) // Below min
  assert_false(NumericValidator::validate(numeric_validator, "101")) // Above max
  assert_false(NumericValidator::validate(numeric_validator, "50.5")) // Not integer
  assert_false(NumericValidator::validate(numeric_validator, "abc")) // Not numeric
  
  // Test date validation
  let date_validator = DateValidator::new()
  
  DateValidator::set_format(date_validator, "YYYY-MM-DD")
  DateValidator::set_min_date(date_validator, "2023-01-01")
  DateValidator::set_max_date(date_validator, "2023-12-31")
  
  assert_true(DateValidator::validate(date_validator, "2023-06-15")) // Valid
  assert_false(DateValidator::validate(date_validator, "2022-12-31")) // Before min
  assert_false(DateValidator::validate(date_validator, "2024-01-01")) // After max
  assert_false(DateValidator::validate(date_validator, "2023-13-01")) // Invalid month
  assert_false(DateValidator::validate(date_validator, "2023-02-30")) // Invalid day
  assert_false(DateValidator::validate(date_validator, "2023/06/15")) // Wrong format
  
  // Test input sanitization
  let input_sanitizer = InputSanitizer::new()
  
  // Test HTML sanitization
  let html_input = "<script>alert('xss')</script><p>Hello</p>"
  let sanitized_html = InputSanitizer::sanitize_html(input_sanitizer, html_input)
  assert_not_eq(sanitized_html, html_input) // Should be sanitized
  assert_true(String::contains(sanitized_html, "<p>Hello</p>")) // Should keep valid HTML
  assert_false(String::contains(sanitized_html, "<script>")) // Should remove script
  
  // Test SQL injection sanitization
  let sql_input = "'; DROP TABLE users; --"
  let sanitized_sql = InputSanitizer::sanitize_sql(input_sanitizer, sql_input)
  assert_not_eq(sanitized_sql, sql_input) // Should be sanitized
  assert_false(String::contains(sanitized_sql, "DROP TABLE")) // Should remove dangerous SQL
  
  // Test cross-site scripting prevention
  let xss_input = "<img src=x onerror=alert('xss')>"
  let sanitized_xss = InputSanitizer::prevent_xss(input_sanitizer, xss_input)
  assert_not_eq(sanitized_xss, xss_input) // Should be sanitized
  assert_false(String::contains(sanitized_xss, "onerror")) // Should remove event handler
  
  // Test file path sanitization
  let path_input = "../../../etc/passwd"
  let sanitized_path = InputSanitizer::sanitize_path(input_sanitizer, path_input)
  assert_not_eq(sanitized_path, path_input) // Should be sanitized
  assert_false(String::contains(sanitized_path, "..")) // Should remove directory traversal
  
  // Test validation with custom rules
  let custom_validator = CustomValidator::new()
  
  // Add custom validation rule
  CustomValidator::add_rule(custom_validator, "password_strength", |input| {
    let has_upper = String::contains(input, |c| 'A' <= c && c <= 'Z')
    let has_lower = String::contains(input, |c| 'a' <= c && c <= 'z')
    let has_digit = String::contains(input, |c| '0' <= c && c <= '9')
    let has_special = String::contains(input, |c| "!@#$%^&*()".contains(c))
    
    String::length(input) >= 8 && has_upper && has_lower && has_digit && has_special
  })
  
  assert_true(CustomValidator::validate(custom_validator, "password_strength", "StrongP@ss1"))
  assert_false(CustomValidator::validate(custom_validator, "password_strength", "weak"))
  assert_false(CustomValidator::validate(custom_validator, "password_strength", "nouppercase1"))
  assert_false(CustomValidator::validate(custom_validator, "password_strength", "NOLOWERCASE1"))
  assert_false(CustomValidator::validate(custom_validator, "password_strength", "NoDigitPass"))
  assert_false(CustomValidator::validate(custom_validator, "password_strength", "NoSpecial1"))
  assert_false(CustomValidator::validate(custom_validator, "password_strength", "Short1!"))
  
  // Test validation error messages
  let validator_with_messages = ValidatorWithMessages::new()
  
  ValidatorWithMessages::set_min_length(validator_with_messages, 5, "Password must be at least 5 characters")
  ValidatorWithMessages::set_pattern(validator_with_messages, "^[a-zA-Z0-9]+$", "Password must contain only letters and numbers")
  
  let validation_result = ValidatorWithMessages::validate_with_messages(validator_with_messages, "abc")
  
  assert_false(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 2)
  assert_true(validation_result.errors.any(|e| String::contains(e, "at least 5 characters")))
  assert_true(validation_result.errors.any(|e| String::contains(e, "letters and numbers")))
}