// Azimuth Telemetry System - Data Compression Tests
// This file contains test cases for data compression functionality

// Test 1: Basic Compression Operations
test "basic compression operations" {
  let original_data = "This is a test string for compression"
  let compressed_data = Compression::compress(original_data)
  let decompressed_data = Compression::decompress(compressed_data)
  
  assert_eq(original_data, decompressed_data)
  assert_true(compressed_data.length() < original_data.length())
}

// Test 2: Compression with Different Algorithms
test "compression with different algorithms" {
  let test_data = "Repeated data pattern for better compression ratio. Repeated data pattern for better compression ratio."
  
  // Test GZIP compression
  let gzip_compressed = Compression::compress_with_algorithm(test_data, Gzip)
  let gzip_decompressed = Compression::decompress_with_algorithm(gzip_compressed, Gzip)
  assert_eq(test_data, gzip_decompressed)
  
  // Test LZ4 compression
  let lz4_compressed = Compression::compress_with_algorithm(test_data, Lz4)
  let lz4_decompressed = Compression::decompress_with_algorithm(lz4_compressed, Lz4)
  assert_eq(test_data, lz4_decompressed)
  
  // Test ZSTD compression
  let zstd_compressed = Compression::compress_with_algorithm(test_data, Zstd)
  let zstd_decompressed = Compression::decompress_with_algorithm(zstd_compressed, Zstd)
  assert_eq(test_data, zstd_decompressed)
}

// Test 3: Compression Ratio Analysis
test "compression ratio analysis" {
  let small_data = "Small"
  let large_data = "Large data with many repeated patterns for better compression effectiveness. Large data with many repeated patterns for better compression effectiveness."
  
  // Small data might not compress well
  let small_compressed = Compression::compress(small_data)
  let small_ratio = small_compressed.length() as Float / small_data.length() as Float
  
  // Large data should compress better
  let large_compressed = Compression::compress(large_data)
  let large_ratio = large_compressed.length() as Float / large_data.length() as Float
  
  // Large data should have better compression ratio (smaller value)
  assert_true(large_ratio < small_ratio)
}

// Test 4: Empty and Edge Cases
test "compression with empty and edge cases" {
  // Test empty string
  let empty_data = ""
  let empty_compressed = Compression::compress(empty_data)
  let empty_decompressed = Compression::decompress(empty_compressed)
  assert_eq(empty_data, empty_decompressed)
  
  // Test single character
  let single_char = "a"
  let single_compressed = Compression::compress(single_char)
  let single_decompressed = Compression::decompress(single_compressed)
  assert_eq(single_char, single_decompressed)
  
  // Test very large data
  let large_data = "x".repeat(10000)
  let large_compressed = Compression::compress(large_data)
  let large_decompressed = Compression::decompress(large_compressed)
  assert_eq(large_data, large_decompressed)
}

// Test 5: Binary Data Compression
test "binary data compression" {
  // Create binary data
  let binary_data = [0x00, 0x01, 0x02, 0x03, 0xFF, 0xFE, 0xFD]
  let binary_string = binary_data.map(fn(x) { x.to_char() }).join("")
  
  let compressed_binary = Compression::compress(binary_string)
  let decompressed_binary = Compression::decompress(compressed_binary)
  
  assert_eq(binary_string, decompressed_binary)
}

// Test 6: Compression Performance Metrics
test "compression performance metrics" {
  let test_data = "Performance test data for compression metrics evaluation"
  
  let start_time = Time::now()
  let compressed = Compression::compress(test_data)
  let compression_time = Time::now() - start_time
  
  let start_time = Time::now()
  let decompressed = Compression::decompress(compressed)
  let decompression_time = Time::now() - start_time
  
  // Verify correctness
  assert_eq(test_data, decompressed)
  
  // Performance should be reasonable (less than 1 second for this small data)
  assert_true(compression_time < 1000)  // milliseconds
  assert_true(decompression_time < 1000)  // milliseconds
}

// Test 7: Compression with Telemetry Data
test "compression with telemetry data" {
  // Create sample telemetry data
  let telemetry_data = "{\"trace_id\":\"abc123\",\"span_id\":\"def456\",\"service\":\"test_service\",\"duration\":150,\"status\":\"ok\",\"timestamp\":1234567890}"
  
  let compressed_telemetry = Compression::compress(telemetry_data)
  let decompressed_telemetry = Compression::decompress(compressed_telemetry)
  
  assert_eq(telemetry_data, decompressed_telemetry)
  
  // Telemetry data should compress reasonably well
  let compression_ratio = compressed_telemetry.length() as Float / telemetry_data.length() as Float
  assert_true(compression_ratio < 0.9)  // At least 10% compression
}

// Test 8: Streaming Compression
test "streaming compression" {
  let data_chunks = [
    "First chunk of data ",
    "second chunk of data ",
    "third chunk of data ",
    "fourth chunk of data"
  ]
  
  let compressor = StreamingCompressor::new(Gzip)
  
  // Compress chunks
  let compressed_chunks = []
  for chunk in data_chunks {
    let compressed_chunk = StreamingCompressor::compress_chunk(compressor, chunk)
    compressed_chunks.push(compressed_chunk)
  }
  
  let final_compressed = StreamingCompressor::finish(compressor)
  
  // Decompress
  let decompressor = StreamingDecompressor::new(Gzip)
  let mut decompressed_data = ""
  
  for chunk in compressed_chunks {
    let decompressed_chunk = StreamingDecompressor::decompress_chunk(decompressor, chunk)
    decompressed_data = decompressed_data + decompressed_chunk
  }
  
  let final_decompressed = StreamingDecompressor::finish(decompressor)
  decompressed_data = decompressed_data + final_decompressed
  
  let expected_data = data_chunks.join("")
  assert_eq(decompressed_data, expected_data)
}

// Test 9: Compression Error Handling
test "compression error handling" {
  let valid_data = "Valid data"
  
  // Test decompressing invalid data
  let invalid_data = "Invalid compressed data"
  
  // Should handle gracefully
  match Compression::safe_decompress(invalid_data) {
    Ok(data) => assert_true(false)  // Should not succeed
    Error(_) => assert_true(true)   // Should fail gracefully
  }
  
  // Valid data should work
  match Compression::safe_decompress(Compression::compress(valid_data)) {
    Ok(data) => assert_eq(data, valid_data)
    Error(_) => assert_true(false)
  }
}

// Test 10: Adaptive Compression Strategy
test "adaptive compression strategy" {
  let small_data = "Small"
  let medium_data = "Medium data with some patterns".repeat(10)
  let large_data = "Large data with many repeated patterns".repeat(100)
  
  // Adaptive strategy should choose different algorithms based on data size
  let small_compressed = AdaptiveCompression::compress(small_data)
  let medium_compressed = AdaptiveCompression::compress(medium_data)
  let large_compressed = AdaptiveCompression::compress(large_data)
  
  // All should decompress correctly
  assert_eq(AdaptiveCompression::decompress(small_compressed), small_data)
  assert_eq(AdaptiveCompression::decompress(medium_compressed), medium_data)
  assert_eq(AdaptiveCompression::decompress(large_compressed), large_data)
  
  // Large data should have better compression ratio
  let small_ratio = small_compressed.length() as Float / small_data.length() as Float
  let large_ratio = large_compressed.length() as Float / large_data.length() as Float
  assert_true(large_ratio < small_ratio)
}