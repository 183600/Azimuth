// Azimuth Telemetry System - Data Compression and Decompression Tests
// This file contains test cases for data compression and decompression functionality

// Test 1: Basic String Compression
test "basic string compression" {
  let original_string = "This is a test string for compression testing. It contains repeated patterns that should compress well."
  
  // Compress the string
  let compressed_data = Compression::compress_string(original_string)
  
  // Verify compression was successful (compressed data should be smaller than original)
  assert_true(compressed_data.length() < original_string.length())
  
  // Decompress the string
  let decompressed_string = Compression::decompress_string(compressed_data)
  
  // Verify decompressed string matches original
  assert_eq(decompressed_string, original_string)
}

// Test 2: Empty String Compression
test "empty string compression" {
  let empty_string = ""
  
  // Compress empty string
  let compressed_data = Compression::compress_string(empty_string)
  
  // Decompress empty string
  let decompressed_string = Compression::decompress_string(compressed_data)
  
  // Verify decompressed string matches original
  assert_eq(decompressed_string, empty_string)
}

// Test 3: Large String Compression
test "large string compression" {
  // Create a large string with repeated patterns
  let base_pattern = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
  let mut large_string = ""
  
  for i in 0..100 {
    large_string = large_string + base_pattern
  }
  
  // Compress the large string
  let compressed_data = Compression::compress_string(large_string)
  
  // Verify compression was effective (should be significantly smaller)
  let compression_ratio = compressed_data.length() as Float / large_string.length() as Float
  assert_true(compression_ratio < 0.5)  // Should be at least 50% compression
  
  // Decompress the string
  let decompressed_string = Compression::decompress_string(compressed_data)
  
  // Verify decompressed string matches original
  assert_eq(decompressed_string, large_string)
}

// Test 4: Binary Data Compression
test "binary data compression" {
  // Create binary data with patterns
  let binary_data = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0xFF, 0xFE, 0xFD, 0xFC, 0xFB, 0xFA, 0xF9, 0xF8,
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0xFF, 0xFE, 0xFD, 0xFC, 0xFB, 0xFA, 0xF9, 0xF8
  ]
  
  // Compress binary data
  let compressed_data = Compression::compress_bytes(binary_data)
  
  // Verify compression was successful
  assert_true(compressed_data.length() < binary_data.length())
  
  // Decompress binary data
  let decompressed_data = Compression::decompress_bytes(compressed_data)
  
  // Verify decompressed data matches original
  assert_eq(decompressed_data.length(), binary_data.length())
  for i in 0..binary_data.length() {
    assert_eq(decompressed_data[i], binary_data[i])
  }
}

// Test 5: JSON Data Compression
test "json data compression" {
  // Create a JSON string with telemetry data
  let json_data = "{\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\",\"span_id\":\"b7ad6b7169203331\",\"timestamp\":1640995200000,\"attributes\":{\"service.name\":\"azimuth-telemetry\",\"service.version\":\"1.0.0\",\"http.method\":\"GET\",\"http.url\":\"https://example.com/api\",\"http.status_code\":200,\"http.response_time_ms\":150}}"
  
  // Compress JSON data
  let compressed_json = Compression::compress_string(json_data)
  
  // Verify compression was successful
  assert_true(compressed_json.length() < json_data.length())
  
  // Decompress JSON data
  let decompressed_json = Compression::decompress_string(compressed_json)
  
  // Verify decompressed JSON matches original
  assert_eq(decompressed_json, json_data)
}

// Test 6: Batch Compression
test "batch compression" {
  // Create multiple strings to compress
  let strings = [
    "First test string for batch compression",
    "Second test string for batch compression",
    "Third test string for batch compression",
    "Fourth test string for batch compression"
  ]
  
  // Compress all strings in batch
  let compressed_batch = Compression::compress_batch(strings)
  
  // Verify batch compression
  assert_eq(compressed_batch.length(), strings.length())
  
  // Decompress all strings in batch
  let decompressed_batch = Compression::decompress_batch(compressed_batch)
  
  // Verify decompressed strings match originals
  assert_eq(decompressed_batch.length(), strings.length())
  for i in 0..strings.length() {
    assert_eq(decompressed_batch[i], strings[i])
  }
}

// Test 7: Streaming Compression
test "streaming compression" {
  // Create a large string that would be processed in chunks
  let chunk_pattern = "This is a chunk of data that will be compressed in streaming mode. "
  let mut large_data = ""
  
  for i in 0..50 {
    large_data = large_data + chunk_pattern
  }
  
  // Initialize streaming compression
  let compressor = Compression::create_stream_compressor()
  
  // Process data in chunks
  let chunk_size = 100
  let mut compressed_chunks = []
  
  for i in range_step(0, large_data.length(), chunk_size) {
    let end_index = min(i + chunk_size, large_data.length())
    let chunk = large_data.substring(i, end_index)
    
    let compressed_chunk = Compression::stream_compress_chunk(compressor, chunk)
    compressed_chunks.push(compressed_chunk)
  }
  
  // Finalize compression
  let final_chunk = Compression::stream_finalize(compressor)
  compressed_chunks.push(final_chunk)
  
  // Combine compressed chunks
  let mut compressed_data = ""
  for chunk in compressed_chunks {
    compressed_data = compressed_data + chunk
  }
  
  // Initialize streaming decompression
  let decompressor = Compression::create_stream_decompressor()
  
  // Decompress data in chunks
  let mut decompressed_data = ""
  
  for chunk in compressed_chunks {
    let decompressed_chunk = Compression::stream_decompress_chunk(decompressor, chunk)
    decompressed_data = decompressed_data + decompressed_chunk
  }
  
  // Verify decompressed data matches original
  assert_eq(decompressed_data, large_data)
}

// Test 8: Compression with Different Algorithms
test "compression with different algorithms" {
  let test_string = "This is a test string for comparing different compression algorithms. It contains repeated patterns that should compress well with different levels of efficiency."
  
  // Test with GZIP algorithm
  let gzip_compressed = Compression::compress_with_algorithm(test_string, CompressionAlgorithm::Gzip)
  let gzip_decompressed = Compression::decompress_with_algorithm(gzip_compressed, CompressionAlgorithm::Gzip)
  assert_eq(gzip_decompressed, test_string)
  
  // Test with DEFLATE algorithm
  let deflate_compressed = Compression::compress_with_algorithm(test_string, CompressionAlgorithm::Deflate)
  let deflate_decompressed = Compression::decompress_with_algorithm(deflate_compressed, CompressionAlgorithm::Deflate)
  assert_eq(deflate_decompressed, test_string)
  
  // Test with LZ4 algorithm (if available)
  let lz4_compressed = Compression::compress_with_algorithm(test_string, CompressionAlgorithm::Lz4)
  let lz4_decompressed = Compression::decompress_with_algorithm(lz4_compressed, CompressionAlgorithm::Lz4)
  assert_eq(lz4_decompressed, test_string)
  
  // Compare compression ratios
  let gzip_ratio = gzip_compressed.length() as Float / test_string.length() as Float
  let deflate_ratio = deflate_compressed.length() as Float / test_string.length() as Float
  let lz4_ratio = lz4_compressed.length() as Float / test_string.length() as Float
  
  // All should provide some level of compression
  assert_true(gzip_ratio < 1.0)
  assert_true(deflate_ratio < 1.0)
  assert_true(lz4_ratio < 1.0)
}

// Test 9: Compression Level Settings
test "compression level settings" {
  let test_string = "This is a test string for testing different compression levels. It contains repeated patterns that should compress better with higher compression levels, though at the cost of slower compression speed."
  
  // Test with different compression levels
  let fastest_compressed = Compression::compress_with_level(test_string, CompressionLevel::Fastest)
  let fast_compressed = Compression::compress_with_level(test_string, CompressionLevel::Fast)
  let balanced_compressed = Compression::compress_with_level(test_string, CompressionLevel::Balanced)
  let best_compressed = Compression::compress_with_level(test_string, CompressionLevel::Best)
  
  // Verify all can be decompressed correctly
  assert_eq(Compression::decompress_string(fastest_compressed), test_string)
  assert_eq(Compression::decompress_string(fast_compressed), test_string)
  assert_eq(Compression::decompress_string(balanced_compressed), test_string)
  assert_eq(Compression::decompress_string(best_compressed), test_string)
  
  // Verify compression ratios improve with higher levels
  let fastest_ratio = fastest_compressed.length() as Float / test_string.length() as Float
  let fast_ratio = fast_compressed.length() as Float / test_string.length() as Float
  let balanced_ratio = balanced_compressed.length() as Float / test_string.length() as Float
  let best_ratio = best_compressed.length() as Float / test_string.length() as Float
  
  // Higher compression levels should generally provide better ratios
  assert_true(fast_ratio <= fastest_ratio)
  assert_true(balanced_ratio <= fast_ratio)
  assert_true(best_ratio <= balanced_ratio)
}

// Test 10: Error Handling in Compression
test "error handling in compression" {
  // Test decompressing invalid data
  let invalid_data = "This is not valid compressed data"
  
  // Attempting to decompress invalid data should return an error
  match Compression::decompress_string(invalid_data) {
    Ok(_) => assert_true(false)  // Should not succeed
    Err(_) => assert_true(true)  // Expected to fail
  }
  
  // Test decompressing truncated data
  let valid_string = "Test string for compression"
  let compressed_data = Compression::compress_string(valid_string)
  
  // Truncate the compressed data
  let truncated_data = compressed_data.substring(0, compressed_data.length() / 2)
  
  // Attempting to decompress truncated data should return an error
  match Compression::decompress_string(truncated_data) {
    Ok(_) => assert_true(false)  // Should not succeed
    Err(_) => assert_true(true)  // Expected to fail
  }
  
  // Test decompressing with wrong algorithm
  let gzip_compressed = Compression::compress_with_algorithm(valid_string, CompressionAlgorithm::Gzip)
  
  // Attempting to decompress with wrong algorithm should return an error
  match Compression::decompress_with_algorithm(gzip_compressed, CompressionAlgorithm::Deflate) {
    Ok(_) => assert_true(false)  // Should not succeed
    Err(_) => assert_true(true)  // Expected to fail
  }
}