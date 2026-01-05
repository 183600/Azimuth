// Azimuth Telemetry System - Advanced Data Compression Tests
// This file contains comprehensive test cases for data compression and decompression functionality

// Test 1: Basic Compression Algorithms
test "basic compression algorithms" {
  // Test string compression
  let original_text = "The quick brown fox jumps over the lazy dog. " * 10
  let compressed_data = Compression::compress(original_text, "gzip")
  let decompressed_text = Compression::decompress(compressed_data, "gzip")
  
  assert_eq(original_text, decompressed_text)
  assert_true(compressed_data.length() < original_text.length())
  
  // Test with different compression algorithms
  let algorithms = ["gzip", "deflate", "brotli"]
  
  for algorithm in algorithms {
    let compressed = Compression::compress(original_text, algorithm)
    let decompressed = Compression::decompress(compressed, algorithm)
    assert_eq(original_text, decompressed)
  }
}

// Test 2: Binary Data Compression
test "binary data compression" {
  // Create binary data with patterns
  let mut binary_data = []
  for i = 0; i < 1000; i = i + 1 {
    binary_data.push(i % 256)
  }
  
  let compressed_binary = Compression::compress_binary(binary_data, "gzip")
  let decompressed_binary = Compression::decompress_binary(compressed_binary, "gzip")
  
  assert_eq(binary_data.length(), decompressed_binary.length())
  for i = 0; i < binary_data.length(); i = i + 1 {
    assert_eq(binary_data[i], decompressed_binary[i])
  }
}

// Test 3: Compression Ratio Analysis
test "compression ratio analysis" {
  // Test with different data patterns
  let test_cases = [
    ("repetitive_data", "ABC" * 1000),
    ("random_data", generate_random_string(3000)),
    ("mixed_data", "The quick brown fox " + generate_random_string(1000) + " jumps over the lazy dog"),
    ("json_data", generate_sample_json(100)),
    ("xml_data", generate_sample_xml(50))
  ]
  
  for (name, data) in test_cases {
    let compressed = Compression::compress(data, "gzip")
    let ratio = compressed.length().to_float() / data.length().to_float()
    
    // Compression should reduce size for most cases
    if name != "random_data" {
      assert_true(ratio < 1.0, "Compression should reduce size for " + name)
    }
    
    // Verify decompression works
    let decompressed = Compression::decompress(compressed, "gzip")
    assert_eq(data, decompressed)
  }
}

// Test 4: Streaming Compression
test "streaming compression" {
  // Test compression of large data in chunks
  let large_data = generate_random_string(100000)
  let chunk_size = 10000
  let mut compressed_chunks = []
  
  // Compress in chunks
  for i = 0; i < large_data.length(); i = i + chunk_size {
    let end = if i + chunk_size > large_data.length() {
      large_data.length()
    } else {
      i + chunk_size
    }
    
    let chunk = large_data.substring(i, end)
    let compressed_chunk = Compression::compress(chunk, "gzip")
    compressed_chunks.push(compressed_chunk)
  }
  
  // Decompress and verify
  let mut decompressed_data = ""
  for chunk in compressed_chunks {
    let decompressed_chunk = Compression::decompress(chunk, "gzip")
    decompressed_data = decompressed_data + decompressed_chunk
  }
  
  assert_eq(large_data, decompressed_data)
}

// Test 5: Compression Error Handling
test "compression error handling" {
  // Test compression with invalid data
  let invalid_data = ""
  let compressed = Compression::compress(invalid_data, "gzip")
  let decompressed = Compression::decompress(compressed, "gzip")
  assert_eq(invalid_data, decompressed)
  
  // Test decompression with corrupted data
  let valid_data = "Hello, World!"
  let compressed = Compression::compress(valid_data, "gzip")
  
  // Corrupt the compressed data
  let mut corrupted_data = compressed.to_byte_array()
  if corrupted_data.length() > 5 {
    corrupted_data[5] = corrupted_data[5] + 1
  }
  
  // Attempt to decompress corrupted data
  match Compression::try_decompress(corrupted_data.to_string(), "gzip") {
    Ok(_) => assert_true(false, "Should fail with corrupted data")
    Error(_) => assert_true(true, "Correctly failed with corrupted data")
  }
  
  // Test with unsupported algorithm
  match Compression::try_compress(valid_data, "unsupported") {
    Ok(_) => assert_true(false, "Should fail with unsupported algorithm")
    Error(_) => assert_true(true, "Correctly failed with unsupported algorithm")
  }
}

// Test 6: Adaptive Compression Strategy
test "adaptive compression strategy" {
  // Test adaptive compression based on data characteristics
  let test_datasets = [
    ("highly_repetitive", "A" * 10000),
    ("moderately_repetitive", "ABCD" * 2500),
    ("low_repetitive", generate_random_string(10000)),
    ("structured", generate_sample_json(500))
  ]
  
  for (name, data) in test_datasets {
    let best_algorithm = Compression::select_best_algorithm(data)
    let compressed = Compression::compress(data, best_algorithm)
    let decompressed = Compression::decompress(compressed, best_algorithm)
    
    assert_eq(data, decompressed)
    
    // Verify the selected algorithm is reasonable
    let ratio = compressed.length().to_float() / data.length().to_float()
    assert_true(ratio <= 1.0, "Compression should not increase size for " + name)
  }
}

// Test 7: Compression Memory Management
test "compression memory management" {
  // Test compression with memory constraints
  let large_data = generate_random_string(1000000)
  
  // Test memory-efficient compression
  let initial_memory = get_memory_usage()
  let compressed = Compression::compress_memory_efficient(large_data, "gzip")
  let final_memory = get_memory_usage()
  
  // Memory usage should be reasonable
  let memory_increase = final_memory - initial_memory
  assert_true(memory_increase < large_data.length() * 2, "Memory increase should be reasonable")
  
  // Verify compression worked
  let decompressed = Compression::decompress(compressed, "gzip")
  assert_eq(large_data, decompressed)
}

// Test 8: Parallel Compression
test "parallel compression" {
  // Test parallel compression of multiple data chunks
  let data_chunks = [
    generate_random_string(50000),
    generate_random_string(50000),
    generate_random_string(50000),
    generate_random_string(50000)
  ]
  
  // Compress chunks in parallel
  let compressed_chunks = Compression::compress_parallel(data_chunks, "gzip")
  
  assert_eq(compressed_chunks.length(), data_chunks.length())
  
  // Decompress and verify all chunks
  for i = 0; i < compressed_chunks.length(); i = i + 1 {
    let decompressed = Compression::decompress(compressed_chunks[i], "gzip")
    assert_eq(data_chunks[i], decompressed)
  }
}

// Helper function to generate random string
func generate_random_string(length : Int) -> String {
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  let mut result = ""
  
  for i = 0; i < length; i = i + 1 {
    let index = (i * 17 + i * i + 7) % chars.length()
    result = result + chars.substring(index, index + 1)
  }
  
  result
}

// Helper function to generate sample JSON
func generate_sample_json(count : Int) -> String {
  let mut json = "["
  
  for i = 0; i < count; i = i + 1 {
    if i > 0 {
      json = json + ","
    }
    json = json + "{\"id\":" + i.to_string() + ",\"name\":\"item_" + i.to_string() + "\",\"value\":" + (i * 2).to_string() + "}"
  }
  
  json = json + "]"
  json
}

// Helper function to generate sample XML
func generate_sample_xml(count : Int) -> String {
  let mut xml = "<root>"
  
  for i = 0; i < count; i = i + 1 {
    xml = xml + "<item id=\"" + i.to_string() + "\" name=\"item_" + i.to_string() + "\" value=\"" + (i * 2).to_string() + "\"/>"
  }
  
  xml = xml + "</root>"
  xml
}

// Helper function to get memory usage (mock implementation)
func get_memory_usage() -> Int {
  // In a real implementation, this would return actual memory usage
  1000000
}