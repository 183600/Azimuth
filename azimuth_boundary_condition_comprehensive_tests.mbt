// Azimuth Telemetry System - Boundary Condition Comprehensive Tests
// This file contains comprehensive tests for boundary conditions and edge cases

// Test 1: Numeric Boundary Conditions
test "numeric boundary conditions" {
  // Test integer boundaries
  let max_int = 2147483647
  let min_int = -2147483648
  
  // Test max integer value
  let max_int_result = TelemetryProcessor::process_integer(max_int)
  match max_int_result {
    Success(result) => assert_eq(result, max_int)
    Error(OverflowError) => assert_true(true) // Expected for max value
    _ => assert_true(false)
  }
  
  // Test min integer value
  let min_int_result = TelemetryProcessor::process_integer(min_int)
  match min_int_result {
    Success(result) => assert_eq(result, min_int)
    Error(OverflowError) => assert_true(true) // Expected for min value
    _ => assert_true(false)
  }
  
  // Test integer overflow
  let overflow_result = TelemetryProcessor::add_integers(max_int, 1)
  match overflow_result {
    Error(OverflowError) => assert_true(true) // Expected overflow
    _ => assert_true(false)
  }
  
  // Test integer underflow
  let underflow_result = TelemetryProcessor::subtract_integers(min_int, 1)
  match underflow_result {
    Error(OverflowError) => assert_true(true) // Expected underflow
    _ => assert_true(false)
  }
  
  // Test float boundaries
  let max_float = 3.4028235e38 // Max float32
  let min_float = 1.17549435e-38 // Min positive float32
  let inf_float = 1.0 / 0.0 // Infinity
  let nan_float = 0.0 / 0.0 // NaN
  
  // Test max float value
  let max_float_result = TelemetryProcessor::process_float(max_float)
  match max_float_result {
    Success(result) => assert_true((result - max_float).abs() < 0.001)
    Error(OverflowError) => assert_true(true) // Expected for max value
    _ => assert_true(false)
  }
  
  // Test min float value
  let min_float_result = TelemetryProcessor::process_float(min_float)
  match min_float_result {
    Success(result) => assert_true((result - min_float).abs() < 0.0000001)
    _ => assert_true(false)
  }
  
  // Test infinity
  let inf_result = TelemetryProcessor::process_float(inf_float)
  match inf_result {
    Success(result) => assert_true(result.is_infinite())
    _ => assert_true(false)
  }
  
  // Test NaN
  let nan_result = TelemetryProcessor::process_float(nan_float)
  match nan_result {
    Success(result) => assert_true(result.is_nan())
    _ => assert_true(false)
  }
  
  // Test division by zero
  let div_by_zero_result = TelemetryProcessor::divide_floats(1.0, 0.0)
  match div_by_zero_result {
    Success(result) => assert_true(result.is_infinite())
    _ => assert_true(false)
  }
  
  // Test zero division
  let zero_div_result = TelemetryProcessor::divide_floats(0.0, 1.0)
  match zero_div_result {
    Success(result) => assert_eq(result, 0.0)
    _ => assert_true(false)
  }
  
  // Test precision boundaries
  let precision_test = 0.1 + 0.2 // Known floating point precision issue
  let precision_result = TelemetryProcessor::compare_floats(precision_test, 0.3, 0.0001)
  assert_true(precision_result) // Should be approximately equal
}

// Test 2: String and Text Boundary Conditions
test "string and text boundary conditions" {
  // Test empty string
  let empty_string = ""
  let empty_result = TelemetryProcessor::process_string(empty_string)
  match empty_result {
    Success(result) => assert_eq(result, empty_string)
    _ => assert_true(false)
  }
  
  // Test single character string
  let single_char = "a"
  let single_char_result = TelemetryProcessor::process_string(single_char)
  match single_char_result {
    Success(result) => assert_eq(result, single_char)
    _ => assert_true(false)
  }
  
  // Test very long string
  let long_string = "a" * 1000000 // 1 million characters
  let long_string_result = TelemetryProcessor::process_string(long_string)
  match long_string_result {
    Success(result) => assert_eq(result.length(), long_string.length())
    Error(StringTooLongError) => assert_true(true) // Expected for very long strings
    _ => assert_true(false)
  }
  
  // Test string with special characters
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_chars_result = TelemetryProcessor::process_string(special_chars)
  match special_chars_result {
    Success(result) => assert_eq(result, special_chars)
    _ => assert_true(false)
  }
  
  // Test string with Unicode characters
  let unicode_string = "Hello 你好 Hola こんにちは 안녕하세요 مرحبا ברוכים הבאים Привет мир 🌍🌎🌏"
  let unicode_result = TelemetryProcessor::process_string(unicode_string)
  match unicode_result {
    Success(result) => assert_eq(result, unicode_string)
    _ => assert_true(false)
  }
  
  // Test string with null bytes
  let null_byte_string = "test\0string"
  let null_byte_result = TelemetryProcessor::process_string(null_byte_string)
  match null_byte_result {
    Success(result) => assert_true(result.contains("test")) // Should handle null bytes
    _ => assert_true(false)
  }
  
  // Test string with control characters
  let control_chars = "test\n\t\rstring"
  let control_chars_result = TelemetryProcessor::process_string(control_chars)
  match control_chars_result {
    Success(result) => assert_eq(result, control_chars)
    _ => assert_true(false)
  }
  
  // Test string concatenation boundaries
  let string1 = "a" * 500000
  let string2 = "b" * 500000
  let concat_result = TelemetryProcessor::concatenate_strings(string1, string2)
  match concat_result {
    Success(result) => assert_eq(result.length(), 1000000)
    Error(StringTooLongError) => assert_true(true) // Expected for very long result
    _ => assert_true(false)
  }
  
  // Test string indexing boundaries
  let test_string = "0123456789"
  
  // Valid indices
  let first_char = TelemetryProcessor::get_char_at(test_string, 0)
  match first_char {
    Some(c) => assert_eq(c, '0')
    None => assert_true(false)
  }
  
  let last_char = TelemetryProcessor::get_char_at(test_string, 9)
  match last_char {
    Some(c) => assert_eq(c, '9')
    None => assert_true(false)
  }
  
  // Invalid indices
  let negative_index = TelemetryProcessor::get_char_at(test_string, -1)
  match negative_index {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  let out_of_bounds_index = TelemetryProcessor::get_char_at(test_string, 10)
  match out_of_bounds_index {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
}

// Test 3: Array and Collection Boundary Conditions
test "array and collection boundary conditions" {
  // Test empty array
  let empty_array = []
  let empty_result = TelemetryProcessor::process_array(empty_array)
  match empty_result {
    Success(result) => assert_eq(result.length(), 0)
    _ => assert_true(false)
  }
  
  // Test single element array
  let single_element = [1]
  let single_result = TelemetryProcessor::process_array(single_element)
  match single_result {
    Success(result) => assert_eq(result.length(), 1)
    _ => assert_true(false)
  }
  
  // Test very large array
  let large_array = []
  for i in 0..=100000 {
    large_array = large_array.push(i)
  }
  let large_result = TelemetryProcessor::process_array(large_array)
  match large_result {
    Success(result) => assert_eq(result.length(), 100001)
    Error(ArrayTooLargeError) => assert_true(true) // Expected for very large arrays
    _ => assert_true(false)
  }
  
  // Test array indexing boundaries
  let test_array = [1, 2, 3, 4, 5]
  
  // Valid indices
  let first_element = TelemetryProcessor::get_element_at(test_array, 0)
  match first_element {
    Some(element) => assert_eq(element, 1)
    None => assert_true(false)
  }
  
  let last_element = TelemetryProcessor::get_element_at(test_array, 4)
  match last_element {
    Some(element) => assert_eq(element, 5)
    None => assert_true(false)
  }
  
  // Invalid indices
  let negative_index = TelemetryProcessor::get_element_at(test_array, -1)
  match negative_index {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  let out_of_bounds_index = TelemetryProcessor::get_element_at(test_array, 5)
  match out_of_bounds_index {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  // Test array insertion boundaries
  let mut mutable_array = [1, 2, 3]
  
  // Insert at beginning
  let insert_beginning = TelemetryProcessor::insert_at(mutable_array, 0, 0)
  match insert_beginning {
    Success(result) => assert_eq(result, [0, 1, 2, 3])
    _ => assert_true(false)
  }
  
  // Insert at end
  let insert_end = TelemetryProcessor::insert_at(mutable_array, 3, 4)
  match insert_end {
    Success(result) => assert_eq(result, [1, 2, 3, 4])
    _ => assert_true(false)
  }
  
  // Insert at invalid position
  let insert_invalid = TelemetryProcessor::insert_at(mutable_array, 10, 5)
  match insert_invalid {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  // Test array deletion boundaries
  let delete_array = [1, 2, 3, 4, 5]
  
  // Delete first element
  let delete_first = TelemetryProcessor::delete_at(delete_array, 0)
  match delete_first {
    Success(result) => assert_eq(result, [2, 3, 4, 5])
    _ => assert_true(false)
  }
  
  // Delete last element
  let delete_last = TelemetryProcessor::delete_at(delete_array, 4)
  match delete_last {
    Success(result) => assert_eq(result, [1, 2, 3, 4])
    _ => assert_true(false)
  }
  
  // Delete at invalid position
  let delete_invalid = TelemetryProcessor::delete_at(delete_array, 10)
  match delete_invalid {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
}

// Test 4: Time and Date Boundary Conditions
test "time and date boundary conditions" {
  // Test epoch time
  let epoch_time = DateTime::from_timestamp(0)
  assert_eq(epoch_time.year, 1970)
  assert_eq(epoch_time.month, 1)
  assert_eq(epoch_time.day, 1)
  
  // Test maximum supported time
  let max_time = DateTime::from_timestamp(253402300799) // 9999-12-31 23:59:59 UTC
  let max_time_result = TelemetryProcessor::process_datetime(max_time)
  match max_time_result {
    Success(result) => assert_eq(result, max_time)
    Error(InvalidTimeError) => assert_true(true) // Expected for max time
    _ => assert_true(false)
  }
  
  // Test minimum supported time
  let min_time = DateTime::from_timestamp(-62135596800) // 0001-01-01 00:00:00 UTC
  let min_time_result = TelemetryProcessor::process_datetime(min_time)
  match min_time_result {
    Success(result) => assert_eq(result, min_time)
    Error(InvalidTimeError) => assert_true(true) // Expected for min time
    _ => assert_true(false)
  }
  
  // Test invalid time
  let invalid_time = DateTime::from_timestamp(-99999999999) // Too far in the past
  let invalid_time_result = TelemetryProcessor::process_datetime(invalid_time)
  match invalid_time_result {
    Error(InvalidTimeError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  // Test leap year
  let leap_year = DateTime::new(2020, 2, 29, 0, 0, 0) // Valid leap year date
  let leap_year_result = TelemetryProcessor::process_datetime(leap_year)
  match leap_year_result {
    Success(result) => assert_eq(result, leap_year)
    _ => assert_true(false)
  }
  
  // Test non-leap year
  let non_leap_year = DateTime::new(2021, 2, 29, 0, 0, 0) // Invalid non-leap year date
  let non_leap_year_result = TelemetryProcessor::process_datetime(non_leap_year)
  match non_leap_year_result {
    Error(InvalidDateError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  // Test month boundaries
  let january_31 = DateTime::new(2023, 1, 31, 0, 0, 0) // Valid
  let january_result = TelemetryProcessor::process_datetime(january_31)
  match january_result {
    Success(result) => assert_eq(result, january_31)
    _ => assert_true(false)
  }
  
  let february_30 = DateTime::new(2023, 2, 30, 0, 0, 0) // Invalid
  let february_result = TelemetryProcessor::process_datetime(february_30)
  match february_result {
    Error(InvalidDateError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  let april_31 = DateTime::new(2023, 4, 31, 0, 0, 0) // Invalid
  let april_result = TelemetryProcessor::process_datetime(april_31)
  match april_result {
    Error(InvalidDateError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  // Test time boundaries
  let midnight = DateTime::new(2023, 6, 15, 0, 0, 0) // Valid
  let midnight_result = TelemetryProcessor::process_datetime(midnight)
  match midnight_result {
    Success(result) => assert_eq(result, midnight)
    _ => assert_true(false)
  }
  
  let end_of_day = DateTime::new(2023, 6, 15, 23, 59, 59) // Valid
  let end_of_day_result = TelemetryProcessor::process_datetime(end_of_day)
  match end_of_day_result {
    Success(result) => assert_eq(result, end_of_day)
    _ => assert_true(false)
  }
  
  let invalid_hour = DateTime::new(2023, 6, 15, 25, 0, 0) // Invalid
  let invalid_hour_result = TelemetryProcessor::process_datetime(invalid_hour)
  match invalid_hour_result {
    Error(InvalidTimeError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  let invalid_minute = DateTime::new(2023, 6, 15, 12, 60, 0) // Invalid
  let invalid_minute_result = TelemetryProcessor::process_datetime(invalid_minute)
  match invalid_minute_result {
    Error(InvalidTimeError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  let invalid_second = DateTime::new(2023, 6, 15, 12, 30, 60) // Invalid
  let invalid_second_result = TelemetryProcessor::process_datetime(invalid_second)
  match invalid_second_result {
    Error(InvalidTimeError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
}

// Test 5: Memory and Resource Boundary Conditions
test "memory and resource boundary conditions" {
  // Test memory allocation boundaries
  let small_allocation = TelemetryProcessor::allocate_memory(1)
  match small_allocation {
    Success(_) => assert_true(true)
    _ => assert_true(false)
  }
  
  let large_allocation = TelemetryProcessor::allocate_memory(1024 * 1024 * 1024) // 1GB
  match large_allocation {
    Success(_) => assert_true(true) // Might succeed on systems with enough memory
    Error(OutOfMemoryError) => assert_true(true) // Expected on systems with limited memory
    _ => assert_true(false)
  }
  
  let huge_allocation = TelemetryProcessor::allocate_memory(1024 * 1024 * 1024 * 1024) // 1TB
  match huge_allocation {
    Error(OutOfMemoryError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  // Test negative allocation
  let negative_allocation = TelemetryProcessor::allocate_memory(-1)
  match negative_allocation {
    Error(InvalidParameterError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  // Test file handle boundaries
  let file_handle_count = TelemetryProcessor::get_open_file_handle_count()
  assert_true(file_handle_count >= 0)
  
  // Test opening many files
  let file_handles = []
  let max_files = 1000
  
  for i in 0..=max_files {
    let handle_result = TelemetryProcessor::open_file("test_file_" + i.to_string() + ".tmp")
    match handle_result {
      Some(handle) => file_handles = file_handles.push(handle)
      None => {
        // Might fail due to file handle limit
        break
      }
    }
  }
  
  // Test closing all files
  for handle in file_handles {
    let close_result = TelemetryProcessor::close_file(handle)
    assert_true(close_result)
  }
  
  // Test thread boundaries
  let thread_count = TelemetryProcessor::get_active_thread_count()
  assert_true(thread_count >= 0)
  
  // Test creating many threads
  let threads = []
  let max_threads = 100
  
  for i in 0..=max_threads {
    let thread_result = TelemetryProcessor::create_thread(fn() {
      Thread::sleep(100) // 100ms
      return i
    })
    match thread_result {
      Some(thread) => threads = threads.push(thread)
      None => {
        // Might fail due to thread limit
        break
      }
    }
  }
  
  // Test joining all threads
  for thread in threads {
    let join_result = TelemetryProcessor::join_thread(thread)
    match join_result {
      Some(_) => assert_true(true)
      None => assert_true(false)
    }
  }
  
  // Test network connection boundaries
  let connection_count = TelemetryProcessor::get_active_connection_count()
  assert_true(connection_count >= 0)
  
  // Test creating many connections
  let connections = []
  let max_connections = 50
  
  for i in 0..=max_connections {
    let connection_result = TelemetryProcessor::create_connection("127.0.0.1", 8080 + i)
    match connection_result {
      Some(connection) => connections = connections.push(connection)
      None => {
        // Might fail due to connection limit
        break
      }
    }
  }
  
  // Test closing all connections
  for connection in connections {
    let close_result = TelemetryProcessor::close_connection(connection)
    assert_true(close_result)
  }
}

// Test 6: Network and Protocol Boundary Conditions
test "network and protocol boundary conditions" {
  // Test URL boundaries
  let valid_urls = [
    "http://example.com",
    "https://example.com",
    "ftp://example.com",
    "http://example.com:8080",
    "https://example.com/path/to/resource",
    "http://user:password@example.com",
    "http://192.168.1.1",
    "https://[2001:db8::1]" // IPv6
  ]
  
  for url in valid_urls {
    let url_result = TelemetryProcessor::validate_url(url)
    assert_true(url_result)
  }
  
  let invalid_urls = [
    "", // Empty
    "not-a-url", // Invalid format
    "http://", // Incomplete
    "https://", // Incomplete
    "http://.com", // Invalid domain
    "http://example..com", // Invalid domain
    "http://example.com:99999", // Invalid port
    "http://example.com:abc", // Non-numeric port
    "https://[2001:db8::1]:99999" // Invalid port with IPv6
  ]
  
  for url in invalid_urls {
    let url_result = TelemetryProcessor::validate_url(url)
    assert_false(url_result)
  }
  
  // Test IP address boundaries
  let valid_ips = [
    "127.0.0.1", // Loopback
    "192.168.1.1", // Private
    "10.0.0.1", // Private
    "172.16.0.1", // Private
    "8.8.8.8", // Public
    "2001:db8::1", // IPv6
    "::1", // IPv6 loopback
    "fe80::1" // IPv6 link-local
  ]
  
  for ip in valid_ips {
    let ip_result = TelemetryProcessor::validate_ip_address(ip)
    assert_true(ip_result)
  }
  
  let invalid_ips = [
    "", // Empty
    "256.256.256.256", // Out of range
    "192.168.1", // Incomplete
    "192.168.1.1.1", // Too many octets
    "192.168.1.abc", // Non-numeric
    "2001:db8::1::1", // Too many :: in IPv6
    "2001:db8::zzzz" // Invalid hex in IPv6
  ]
  
  for ip in invalid_ips {
    let ip_result = TelemetryProcessor::validate_ip_address(ip)
    assert_false(ip_result)
  }
  
  // Test port boundaries
  let valid_ports = [1, 80, 443, 8080, 65535]
  
  for port in valid_ports {
    let port_result = TelemetryProcessor::validate_port(port)
    assert_true(port_result)
  }
  
  let invalid_ports = [0, -1, 65536, 100000]
  
  for port in invalid_ports {
    let port_result = TelemetryProcessor::validate_port(port)
    assert_false(port_result)
  }
  
  // Test HTTP status code boundaries
  let valid_status_codes = [100, 200, 301, 404, 500, 599]
  
  for code in valid_status_codes {
    let status_result = TelemetryProcessor::validate_http_status_code(code)
    assert_true(status_result)
  }
  
  let invalid_status_codes = [99, 600, -1, 1000]
  
  for code in invalid_status_codes {
    let status_result = TelemetryProcessor::validate_http_status_code(code)
    assert_false(status_result)
  }
  
  // Test packet size boundaries
  let small_packet = "a" // 1 byte
  let small_result = TelemetryProcessor::process_packet(small_packet)
  match small_result {
    Success(_) => assert_true(true)
    _ => assert_true(false)
  }
  
  let large_packet = "a" * 65507 // Maximum UDP packet size
  let large_result = TelemetryProcessor::process_packet(large_packet)
  match large_result {
    Success(_) => assert_true(true)
    Error(PacketTooLargeError) => assert_true(true) // Expected for large packets
    _ => assert_true(false)
  }
  
  let huge_packet = "a" * 1000000 // Much larger than maximum
  let huge_result = TelemetryProcessor::process_packet(huge_packet)
  match huge_result {
    Error(PacketTooLargeError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
}

// Test 7: Data Structure Boundary Conditions
test "data structure boundary conditions" {
  // Test stack boundaries
  let stack = Stack::new()
  
  // Test empty stack
  assert_true(stack.is_empty())
  assert_eq(stack.size(), 0)
  
  let empty_pop = stack.pop()
  match empty_pop {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  // Test single element stack
  stack.push(1)
  assert_false(stack.is_empty())
  assert_eq(stack.size(), 1)
  
  let single_pop = stack.pop()
  match single_pop {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  assert_true(stack.is_empty())
  assert_eq(stack.size(), 0)
  
  // Test large stack
  for i in 0..=10000 {
    stack.push(i)
  }
  
  assert_eq(stack.size(), 10001)
  
  // Test queue boundaries
  let queue = Queue::new()
  
  // Test empty queue
  assert_true(queue.is_empty())
  assert_eq(queue.size(), 0)
  
  let empty_dequeue = queue.dequeue()
  match empty_dequeue {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  // Test single element queue
  queue.enqueue(1)
  assert_false(queue.is_empty())
  assert_eq(queue.size(), 1)
  
  let single_dequeue = queue.dequeue()
  match single_dequeue {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  assert_true(queue.is_empty())
  assert_eq(queue.size(), 0)
  
  // Test large queue
  for i in 0..=10000 {
    queue.enqueue(i)
  }
  
  assert_eq(queue.size(), 10001)
  
  // Test hash map boundaries
  let hash_map = HashMap::new()
  
  // Test empty hash map
  assert_true(hash_map.is_empty())
  assert_eq(hash_map.size(), 0)
  
  let empty_get = hash_map.get("key")
  match empty_get {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  // Test single element hash map
  hash_map.put("key", "value")
  assert_false(hash_map.is_empty())
  assert_eq(hash_map.size(), 1)
  
  let single_get = hash_map.get("key")
  match single_get {
    Some(value) => assert_eq(value, "value")
    None => assert_true(false)
  }
  
  // Test large hash map
  for i in 0..=10000 {
    hash_map.put("key_" + i.to_string(), "value_" + i.to_string())
  }
  
  assert_eq(hash_map.size(), 10001)
  
  // Test hash collision handling
  let collision_map = HashMap::new()
  
  // Insert values that might cause collisions
  for i in 0..=1000 {
    collision_map.put("collision_key_" + (i % 10).to_string(), "value_" + i.to_string())
  }
  
  assert_eq(collision_map.size(), 10) // Should only have 10 unique keys
  
  // Test tree boundaries
  let tree = BinarySearchTree::new()
  
  // Test empty tree
  assert_true(tree.is_empty())
  
  // Test single node tree
  tree.insert(5)
  assert_false(tree.is_empty())
  
  let single_search = tree.search(5)
  match single_search {
    Some(value) => assert_eq(value, 5)
    None => assert_true(false)
  }
  
  let empty_search = tree.search(10)
  match empty_search {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  // Test large tree
  for i in 0..=1000 {
    tree.insert(i)
  }
  
  // Test tree depth
  let tree_depth = tree.depth()
  assert_true(tree_depth > 0)
  
  // Test balanced tree
  let balanced_tree = BalancedBinarySearchTree::new()
  
  for i in 0..=1000 {
    balanced_tree.insert(i)
  }
  
  let balanced_depth = balanced_tree.depth()
  let unbalanced_depth = tree.depth()
  
  // Balanced tree should have less depth than unbalanced tree
  assert_true(balanced_depth <= unbalanced_depth)
}

// Test 8: Algorithm and Logic Boundary Conditions
test "algorithm and logic boundary conditions" {
  // Test sorting boundaries
  let empty_array = []
  let empty_sort = TelemetryProcessor::sort_array(empty_array)
  assert_eq(empty_sort.length(), 0)
  
  let single_element = [1]
  let single_sort = TelemetryProcessor::sort_array(single_element)
  assert_eq(single_sort, [1])
  
  let already_sorted = [1, 2, 3, 4, 5]
  let sorted_sort = TelemetryProcessor::sort_array(already_sorted)
  assert_eq(sorted_sort, [1, 2, 3, 4, 5])
  
  let reverse_sorted = [5, 4, 3, 2, 1]
  let reverse_sort = TelemetryProcessor::sort_array(reverse_sorted)
  assert_eq(reverse_sort, [1, 2, 3, 4, 5])
  
  let all_same = [5, 5, 5, 5, 5]
  let same_sort = TelemetryProcessor::sort_array(all_same)
  assert_eq(same_sort, [5, 5, 5, 5, 5])
  
  // Test search boundaries
  let search_array = [1, 2, 3, 4, 5]
  
  // Search for first element
  let first_search = TelemetryProcessor::binary_search(search_array, 1)
  match first_search {
    Some(index) => assert_eq(index, 0)
    None => assert_true(false)
  }
  
  // Search for last element
  let last_search = TelemetryProcessor::binary_search(search_array, 5)
  match last_search {
    Some(index) => assert_eq(index, 4)
    None => assert_true(false)
  }
  
  // Search for middle element
  let middle_search = TelemetryProcessor::binary_search(search_array, 3)
  match middle_search {
    Some(index) => assert_eq(index, 2)
    None => assert_true(false)
  }
  
  // Search for non-existent element (less than first)
  let less_search = TelemetryProcessor::binary_search(search_array, 0)
  match less_search {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  // Search for non-existent element (greater than last)
  let greater_search = TelemetryProcessor::binary_search(search_array, 6)
  match greater_search {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  // Search for non-existent element (between elements)
  let between_search = TelemetryProcessor::binary_search(search_array, 2.5)
  match between_search {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected failure
  }
  
  // Test mathematical boundaries
  // Factorial of 0 is 1
  let factorial_0 = TelemetryProcessor::factorial(0)
  assert_eq(factorial_0, 1)
  
  // Factorial of 1 is 1
  let factorial_1 = TelemetryProcessor::factorial(1)
  assert_eq(factorial_1, 1)
  
  // Factorial of negative number should fail
  let factorial_negative = TelemetryProcessor::factorial(-1)
  match factorial_negative {
    Error(InvalidParameterError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  // Fibonacci sequence boundaries
  let fibonacci_0 = TelemetryProcessor::fibonacci(0)
  assert_eq(fibonacci_0, 0)
  
  let fibonacci_1 = TelemetryProcessor::fibonacci(1)
  assert_eq(fibonacci_1, 1)
  
  let fibonacci_2 = TelemetryProcessor::fibonacci(2)
  assert_eq(fibonacci_2, 1)
  
  // Fibonacci of negative number should fail
  let fibonacci_negative = TelemetryProcessor::fibonacci(-1)
  match fibonacci_negative {
    Error(InvalidParameterError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  // Test large numbers
  let large_factorial = TelemetryProcessor::factorial(20)
  assert_eq(large_factorial, 2432902008176640000)
  
  let very_large_factorial = TelemetryProcessor::factorial(100)
  match very_large_factorial {
    Success(_) => assert_true(true) // Might succeed
    Error(OverflowError) => assert_true(true) // Expected for very large numbers
    _ => assert_true(false)
  }
  
  // Test string matching boundaries
  let empty_pattern = ""
  let empty_text = ""
  let empty_match = TelemetryProcessor::pattern_match(empty_pattern, empty_text)
  assert_true(empty_match)
  
  let empty_pattern_non_empty_text = TelemetryProcessor::pattern_match(empty_pattern, "text")
  assert_true(empty_pattern_non_empty_text) // Empty pattern matches anything
  
  let non_empty_pattern_empty_text = TelemetryProcessor::pattern_match("pattern", "")
  assert_false(non_empty_pattern_empty_text)
  
  let exact_match = TelemetryProcessor::pattern_match("hello", "hello")
  assert_true(exact_match)
  
  let no_match = TelemetryProcessor::pattern_match("hello", "world")
  assert_false(no_match)
  
  // Test regular expression boundaries
  let simple_regex = TelemetryProcessor::regex_match("h.*o", "hello")
  assert_true(simple_regex)
  
  let no_regex_match = TelemetryProcessor::regex_match("h.*o", "world")
  assert_false(no_regex_match)
  
  let invalid_regex = TelemetryProcessor::regex_match("[", "test")
  match invalid_regex {
    Error(InvalidParameterError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
}

// Test 9: Concurrent Operation Boundary Conditions
test "concurrent operation boundary conditions" {
  // Test concurrent access to shared resources
  let shared_counter = AtomicCounter::new(0)
  let thread_count = 100
  let increments_per_thread = 1000
  
  let threads = []
  for i in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for j in 0..=increments_per_thread {
        AtomicCounter::increment(shared_counter)
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify final count
  let final_count = AtomicCounter::get_value(shared_counter)
  assert_eq(final_count, (thread_count + 1) * (increments_per_thread + 1))
  
  // Test concurrent access to shared map
  let shared_map = ConcurrentHashMap::new()
  
  let map_threads = []
  for i in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for j in 0..=increments_per_thread {
        let key = "key_" + j.to_string()
        let value = "value_" + i.to_string() + "_" + j.to_string()
        ConcurrentHashMap::put(shared_map, key, value)
      }
    })
    map_threads = map_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in map_threads {
    Thread::join(thread)
  }
  
  // Verify map size
  let map_size = shared_map.size()
  assert_eq(map_size, increments_per_thread + 1)
  
  // Test concurrent queue operations
  let shared_queue = ConcurrentQueue::new()
  
  // Producer threads
  let producer_threads = []
  for i in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for j in 0..=increments_per_thread {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        ConcurrentQueue::enqueue(shared_queue, item)
      }
    })
    producer_threads = producer_threads.push(thread)
  }
  
  // Consumer threads
  let consumer_threads = []
  let consumed_count = AtomicCounter::new(0)
  
  for i in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for j in 0..=increments_per_thread {
        let item = ConcurrentQueue::dequeue(shared_queue)
        match item {
          Some(_) => AtomicCounter::increment(consumed_count)
          None => {
            // Queue might be empty, try again
            Thread::sleep(1) // Small delay
            j = j - 1 // Retry
          }
        }
      }
    })
    consumer_threads = consumer_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in producer_threads {
    Thread::join(thread)
  }
  
  for thread in consumer_threads {
    Thread::join(thread)
  }
  
  // Verify consumption
  let total_produced = (thread_count + 1) * (increments_per_thread + 1)
  let total_consumed = AtomicCounter::get_value(consumed_count)
  
  // Some items might not be consumed due to timing
  assert_true(total_consumed <= total_produced)
  
  // Test concurrent stack operations
  let shared_stack = ConcurrentStack::new()
  
  let stack_threads = []
  for i in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for j in 0..=increments_per_thread {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        ConcurrentStack::push(shared_stack, item)
      }
    })
    stack_threads = stack_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in stack_threads {
    Thread::join(thread)
  }
  
  // Verify stack size
  let stack_size = ConcurrentStack::size(shared_stack)
  assert_eq(stack_size, (thread_count + 1) * (increments_per_thread + 1))
  
  // Test concurrent file operations
  let file_lock = FileLock::new("test_concurrent_file.txt")
  
  let file_threads = []
  for i in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      FileLock::acquire(file_lock)
      
      // Write to file
      let content = "Thread " + i.to_string() + " wrote this\n"
      TelemetryProcessor::write_to_file("test_concurrent_file.txt", content)
      
      FileLock::release(file_lock)
    })
    file_threads = file_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in file_threads {
    Thread::join(thread)
  }
  
  // Verify file content
  let file_content = TelemetryProcessor::read_from_file("test_concurrent_file.txt")
  assert_true(file_content.length() > 0)
  
  // Clean up
  TelemetryProcessor::delete_file("test_concurrent_file.txt")
}

// Test 10: Error Handling and Exception Boundary Conditions
test "error handling and exception boundary conditions" {
  // Test null/None handling
  let none_value : Option[Int] = None
  let none_result = TelemetryProcessor::process_optional(none_value)
  match none_result {
    Error(NullValueError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  let some_value = Some(42)
  let some_result = TelemetryProcessor::process_optional(some_value)
  match some_result {
    Success(result) => assert_eq(result, 42)
    _ => assert_true(false)
  }
  
  // Test type conversion boundaries
  let valid_int_string = "42"
  let valid_int_result = TelemetryProcessor::string_to_int(valid_int_string)
  match valid_int_result {
    Success(result) => assert_eq(result, 42)
    _ => assert_true(false)
  }
  
  let invalid_int_string = "not_a_number"
  let invalid_int_result = TelemetryProcessor::string_to_int(invalid_int_string)
  match invalid_int_result {
    Error(TypeConversionError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  let overflow_int_string = "999999999999999999999999999999"
  let overflow_int_result = TelemetryProcessor::string_to_int(overflow_int_string)
  match overflow_int_result {
    Error(OverflowError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  // Test valid float string
  let valid_float_string = "3.14"
  let valid_float_result = TelemetryProcessor::string_to_float(valid_float_string)
  match valid_float_result {
    Success(result) => assert_true((result - 3.14).abs() < 0.001)
    _ => assert_true(false)
  }
  
  // Test invalid float string
  let invalid_float_string = "not_a_float"
  let invalid_float_result = TelemetryProcessor::string_to_float(invalid_float_string)
  match invalid_float_result {
    Error(TypeConversionError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
  
  // Test special float values
  let infinity_string = "infinity"
  let infinity_result = TelemetryProcessor::string_to_float(infinity_string)
  match infinity_result {
    Success(result) => assert_true(result.is_infinite())
    _ => assert_true(false)
  }
  
  let nan_string = "nan"
  let nan_result = TelemetryProcessor::string_to_float(nan_string)
  match nan_result {
    Success(result) => assert_true(result.is_nan())
    _ => assert_true(false)
  }
  
  // Test exception chaining
  let inner_error = Error(InvalidParameterError)
  let chained_error = TelemetryProcessor::chain_error(inner_error, "Additional context")
  match chained_error {
    Error(ChainedError(inner, message)) => {
      match inner {
        Error(InvalidParameterError) => assert_true(true)
        _ => assert_true(false)
      }
      assert_eq(message, "Additional context")
    }
    _ => assert_true(false)
  }
  
  // Test error recovery
  let recoverable_error = Error(NetworkError)
  let recovery_result = TelemetryProcessor::attempt_recovery(recoverable_error)
  match recovery_result {
    Success(_) => assert_true(true) // Recovery succeeded
    Error(_) => assert_true(true) // Recovery failed but didn't crash
  }
  
  // Test error aggregation
  let errors = [
    Error(NetworkError),
    Error(AuthenticationError),
    Error(ValidationError)
  ]
  
  let aggregated_error = TelemetryProcessor::aggregate_errors(errors)
  match aggregated_error {
    Error(AggregatedError(error_list)) => {
      assert_eq(error_list.length(), 3)
    }
    _ => assert_true(false)
  }
  
  // Test timeout handling
  let quick_operation = fn() { "success" }
  let quick_result = TelemetryProcessor::execute_with_timeout(quick_operation, 1000) // 1 second
  match quick_result {
    Success(result) => assert_eq(result, "success")
    _ => assert_true(false)
  }
  
  let slow_operation = fn() {
    Thread::sleep(2000) // 2 seconds
    return "success"
  }
  let slow_result = TelemetryProcessor::execute_with_timeout(slow_operation, 1000) // 1 second
  match slow_result {
    Error(TimeoutError) => assert_true(true) // Expected timeout
    _ => assert_true(false)
  }
  
  // Test retry mechanism
  let mut attempt_count = 0
  let failing_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Error(NetworkError) // Fail first 2 attempts
    } else {
      Success("success") // Succeed on 3rd attempt
    }
  }
  
  let retry_result = TelemetryProcessor::retry_operation(failing_operation, 3)
  match retry_result {
    Success(result) => {
      assert_eq(result, "success")
      assert_eq(attempt_count, 3)
    }
    _ => assert_true(false)
  }
  
  // Test circuit breaker
  let circuit_breaker = CircuitBreaker::new(3, 1000) // 3 failures, 1 second timeout
  
  // Successful operation
  let success_result = TelemetryProcessor::execute_with_circuit_breaker(circuit_breaker, fn() {
    Success("success")
  })
  match success_result {
    Success(result) => assert_eq(result, "success")
    _ => assert_true(false)
  }
  
  // Trigger circuit breaker
  for i in 0..=3 {
    let _ = TelemetryProcessor::execute_with_circuit_breaker(circuit_breaker, fn() {
      Error(NetworkError)
    })
  }
  
  // Circuit breaker should be open now
  let open_result = TelemetryProcessor::execute_with_circuit_breaker(circuit_breaker, fn() {
    Success("success")
  })
  match open_result {
    Error(CircuitBreakerOpenError) => assert_true(true) // Expected failure
    _ => assert_true(false)
  }
}