// Azimuth Utility Functions Test Suite
// This file contains test cases for utility functions used in the telemetry system

// Test 1: ID Generation and Validation
test "id generation and validation" {
  // Test trace ID generation
  let trace_id = generate_trace_id()
  assert_eq(trace_id.length(), 32)
  assert_true(is_valid_hex_string(trace_id))
  
  // Test span ID generation
  let span_id = generate_span_id()
  assert_eq(span_id.length(), 16)
  assert_true(is_valid_hex_string(span_id))
  
  // Test invalid ID validation
  assert_false(is_valid_trace_id(""))
  assert_false(is_valid_trace_id("invalid"))
  assert_false(is_valid_span_id(""))
  assert_false(is_valid_span_id("invalid"))
}

// Test 2: Timestamp Operations
test "timestamp operations" {
  // Test current timestamp
  let current_ts = current_timestamp()
  assert_true(current_ts > 0L)
  
  // Test timestamp conversion
  let ts_str = timestamp_to_string(current_ts)
  assert_true(ts_str.length() > 0)
  
  let parsed_ts = string_to_timestamp(ts_str)
  match parsed_ts {
    Some(ts) => assert_eq(ts, current_ts)
    None => assert_true(false)
  }
  
  // Test duration calculation
  let start_ts = current_timestamp()
  // Simulate some operations
  let end_ts = start_ts + 1000L
  let duration = calculate_duration(start_ts, end_ts)
  assert_eq(duration, 1000L)
}

// Test 3: Data Formatting Functions
test "data formatting functions" {
  // Test size formatting
  assert_eq(format_size(0), "0 B")
  assert_eq(format_size(1024), "1.0 KB")
  assert_eq(format_size(1048576), "1.0 MB")
  assert_eq(format_size(1073741824), "1.0 GB")
  
  // Test time formatting
  assert_eq(format_duration(0), "0ms")
  assert_eq(format_duration(1000), "1s")
  assert_eq(format_duration(61000), "1m 1s")
  assert_eq(format_duration(3661000), "1h 1m 1s")
  
  // Test percentage formatting
  assert_eq(format_percentage(0.0), "0%")
  assert_eq(format_percentage(0.5), "50%")
  assert_eq(format_percentage(1.0), "100%")
}

// Test 4: String Utilities
test "string utilities" {
  // Test random string generation
  let random_str = generate_random_string(10)
  assert_eq(random_str.length(), 10)
  
  // Test string sanitization
  let dirty_str = "test<script>alert('xss')</script>"
  let clean_str = sanitize_string(dirty_str)
  assert_eq(clean_str, "testalert('xss')")
  
  // Test string truncation
  let long_str = "this is a very long string that needs to be truncated"
  let truncated = truncate_string(long_str, 20)
  assert_eq(truncated.length(), 20)
  assert_true(truncated.ends_with("..."))
  
  // Test camel case conversion
  assert_eq(to_camel_case("hello_world"), "helloWorld")
  assert_eq(to_camel_case("test-case"), "testCase")
  assert_eq(to_camel_case("alreadyCamel"), "alreadyCamel")
}

// Test 5: Collection Utilities
test "collection utilities" {
  // Test array deduplication
  let with_duplicates = [1, 2, 2, 3, 4, 4, 5]
  let deduplicated = deduplicate_array(with_duplicates)
  assert_eq(deduplicated, [1, 2, 3, 4, 5])
  
  // Test array chunking
  let large_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let chunks = chunk_array(large_array, 3)
  assert_eq(chunks.length(), 4)
  assert_eq(chunks[0], [1, 2, 3])
  assert_eq(chunks[3], [10])
  
  // Test map merging
  let map1 = [("a", 1), ("b", 2)]
  let map2 = [("b", 3), ("c", 4)]
  let merged = merge_maps(map1, map2)
  assert_eq(merged.get("a"), Some(1))
  assert_eq(merged.get("b"), Some(3)) // map2 should override
  assert_eq(merged.get("c"), Some(4))
}

// Test 6: Numeric Utilities
test "numeric utilities" {
  // Test number rounding
  assert_eq(round_to_decimal(3.14159, 2), 3.14)
  assert_eq(round_to_decimal(3.15, 1), 3.2)
  assert_eq(round_to_decimal(3.0, 0), 3.0)
  
  // Test range checking
  assert_true(is_in_range(5, 1, 10))
  assert_true(is_in_range(1, 1, 10))
  assert_true(is_in_range(10, 1, 10))
  assert_false(is_in_range(0, 1, 10))
  assert_false(is_in_range(11, 1, 10))
  
  // Test clamp function
  assert_eq(clamp(5, 1, 10), 5)
  assert_eq(clamp(0, 1, 10), 1)
  assert_eq(clamp(15, 1, 10), 10)
}

// Test 7: URL and Path Utilities
test "url and path utilities" {
  // Test URL parsing
  let url = "https://example.com:8080/path/to/resource?param1=value1&param2=value2#section"
  let parsed = parse_url(url)
  assert_eq(parsed.scheme, "https")
  assert_eq(parsed.host, "example.com")
  assert_eq(parsed.port, Some(8080))
  assert_eq(parsed.path, "/path/to/resource")
  assert_eq(parsed.fragment, Some("section"))
  
  // Test path joining
  assert_eq(join_paths("/home", "user", "documents"), "/home/user/documents")
  assert_eq(join_paths("/home/", "/user/", "/documents/"), "/home/user/documents")
  
  // Test file extension extraction
  assert_eq(get_file_extension("test.txt"), "txt")
  assert_eq(get_file_extension("archive.tar.gz"), "gz")
  assert_eq(get_file_extension("no_extension"), "")
}

// Test 8: Validation Utilities
test "validation utilities" {
  // Test email validation
  assert_true(is_valid_email("user@example.com"))
  assert_true(is_valid_email("test.email+tag@domain.co.uk"))
  assert_false(is_valid_email("invalid-email"))
  assert_false(is_valid_email("@domain.com"))
  assert_false(is_valid_email("user@"))
  
  // Test JSON validation
  assert_true(is_valid_json("{\"key\": \"value\"}"))
  assert_true(is_valid_json("[1, 2, 3]"))
  assert_false(is_valid_json("{invalid json}"))
  assert_false(is_valid_json(""))
  
  // Test regex validation
  assert_true(matches_regex("\\d+", "123"))
  assert_false(matches_regex("\\d+", "abc"))
  assert_true(matches_regex("^[a-zA-Z]+$", "TestString"))
  assert_false(matches_regex("^[a-zA-Z]+$", "Test123"))
}

// Test 9: Encoding and Decoding Utilities
test "encoding and decoding utilities" {
  // Test base64 encoding/decoding
  let original = "Hello, World!"
  let encoded = base64_encode(original)
  let decoded = base64_decode(encoded)
  match decoded {
    Some(s) => assert_eq(s, original)
    None => assert_true(false)
  }
  
  // Test URL encoding/decoding
  let url_original = "hello world?param=value&other=test"
  let url_encoded = url_encode(url_original)
  let url_decoded = url_decode(url_encoded)
  assert_eq(url_decoded, url_original)
  
  // Test hex encoding/decoding
  let hex_encoded = hex_encode("test")
  let hex_decoded = hex_decode(hex_encoded)
  match hex_decoded {
    Some(s) => assert_eq(s, "test")
    None => assert_true(false)
  }
}

// Test 10: Hash and Checksum Utilities
test "hash and checksum utilities" {
  // Test MD5 hash
  let md5_hash = calculate_md5("test")
  assert_eq(md5_hash.length(), 32)
  
  // Test SHA256 hash
  let sha256_hash = calculate_sha256("test")
  assert_eq(sha256_hash.length(), 64)
  
  // Test checksum calculation
  let data = [1, 2, 3, 4, 5]
  let checksum = calculate_checksum(data)
  assert_true(checksum > 0)
  
  // Test hash consistency
  let hash1 = calculate_md5("consistent")
  let hash2 = calculate_md5("consistent")
  assert_eq(hash1, hash2)
}