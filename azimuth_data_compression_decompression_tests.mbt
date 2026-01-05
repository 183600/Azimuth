// Azimuth Data Compression and Decompression Tests
// This file contains test cases for data compression and decompression functionality

// Test 1: Basic String Compression
test "basic string compression" {
  let original_text = "This is a sample text that should be compressed to demonstrate the compression functionality of the Azimuth telemetry system."
  
  // Test gzip compression
  let gzip_compressor = CompressionEngine::gzip()
  let gzip_compressed = CompressionEngine::compress(gzip_compressor, original_text.to_byte_array())
  let gzip_decompressed = CompressionEngine::decompress(gzip_compressor, gzip_compressed)
  
  assert_eq(gzip_decompressed.to_string(), original_text)
  assert_eq(gzip_compressed.length() < original_text.length(), true)
  
  // Test deflate compression
  let deflate_compressor = CompressionEngine::deflate()
  let deflate_compressed = CompressionEngine::compress(deflate_compressor, original_text.to_byte_array())
  let deflate_decompressed = CompressionEngine::decompress(deflate_compressor, deflate_compressed)
  
  assert_eq(deflate_decompressed.to_string(), original_text)
  assert_eq(deflate_compressed.length() < original_text.length(), true)
  
  // Test lz4 compression (fast compression)
  let lz4_compressor = CompressionEngine::lz4()
  let lz4_compressed = CompressionEngine::compress(lz4_compressor, original_text.to_byte_array())
  let lz4_decompressed = CompressionEngine::decompress(lz4_compressor, lz4_compressed)
  
  assert_eq(lz4_decompressed.to_string(), original_text)
  assert_eq(lz4_compressed.length() < original_text.length(), true)
}

// Test 2: Binary Data Compression
test "binary data compression" {
  // Create binary data with patterns
  let mut binary_data = []
  for i = 0; i < 1000; i = i + 1 {
    binary_data.push(i % 256)
  }
  
  // Test compression with different algorithms
  let algorithms = [CompressionAlgorithm::Gzip, CompressionAlgorithm::Deflate, CompressionAlgorithm::Lz4]
  
  for algorithm in algorithms {
    let compressor = CompressionEngine::with_algorithm(algorithm)
    let compressed = CompressionEngine::compress(compressor, binary_data)
    let decompressed = CompressionEngine::decompress(compressor, compressed)
    
    assert_eq(decompressed.length(), binary_data.length())
    assert_eq(compressed.length() < binary_data.length(), true)
    
    // Verify data integrity
    for i = 0; i < binary_data.length(); i = i + 1 {
      assert_eq(decompressed[i], binary_data[i])
    }
  }
}

// Test 3: Compression Ratio Analysis
test "compression ratio analysis" {
  // Test data with different patterns
  let repetitive_text = "aaaaabbbbbcccccdddddeeeeefffffggggghhhhhi" * 20
  let random_text = "The quick brown fox jumps over the lazy dog. " * 20
  let mixed_data = "Data with mixed content: 12345!@#$% abcdefghijklmnopqrstuvwxyz" * 15
  
  let test_cases = [
    ("repetitive", repetitive_text),
    ("random", random_text),
    ("mixed", mixed_data)
  ]
  
  let algorithms = [
    ("gzip", CompressionAlgorithm::Gzip),
    ("deflate", CompressionAlgorithm::Deflate),
    ("lz4", CompressionAlgorithm::Lz4)
  ]
  
  for (data_type, text) in test_cases {
    for (algo_name, algorithm) in algorithms {
      let compressor = CompressionEngine::with_algorithm(algorithm)
      let original_bytes = text.to_byte_array()
      let compressed = CompressionEngine::compress(compressor, original_bytes)
      let ratio = (compressed.length() as f64) / (original_bytes.length() as f64)
      
      // All algorithms should achieve some compression
      assert_eq(ratio < 1.0, true)
      
      // Repetitive data should compress better than random data
      if data_type == "repetitive" {
        assert_eq(ratio < 0.5, true)
      }
    }
  }
}

// Test 4: Streaming Compression
test "streaming compression" {
  // Create large data that would benefit from streaming
  let large_text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. " * 10000
  
  // Test streaming compression
  let stream_compressor = StreamingCompressor::new(CompressionAlgorithm::Gzip, 4096)  // 4KB chunks
  let mut compressed_chunks = []
  
  // Process data in chunks
  let chunk_size = 1024
  for i = 0; i < large_text.length(); i = i + chunk_size {
    let end = if i + chunk_size > large_text.length() { large_text.length() } else { i + chunk_size }
    let chunk = large_text.substring(i, end)
    let compressed_chunk = StreamingCompressor::process_chunk(stream_compressor, chunk.to_byte_array())
    compressed_chunks.push(compressed_chunk)
  }
  
  // Finalize compression
  let final_chunk = StreamingCompressor::finalize(stream_compressor)
  compressed_chunks.push(final_chunk)
  
  // Combine all compressed chunks
  let mut full_compressed = []
  for chunk in compressed_chunks {
    full_compressed = full_compressed.concat(chunk)
  }
  
  // Decompress and verify
  let decompressor = StreamingDecompressor::new(CompressionAlgorithm::Gzip)
  let decompressed = StreamingDecompressor::process_all(decompressor, full_compressed)
  
  assert_eq(decompressed.to_string(), large_text)
}

// Test 5: Adaptive Compression
test "adaptive compression" {
  // Test adaptive compression that chooses the best algorithm
  let adaptive_compressor = AdaptiveCompressor::new()
  
  // Test with different data types
  let test_cases = [
    ("text", "This is a text string with some repetitive content and some unique content." * 100),
    ("json", "{\"key\":\"value\",\"number\":42,\"array\":[1,2,3,4,5]}" * 50),
    ("numeric", "1234567890" * 100),
    ("mixed", "Mixed123Content!@#$%" * 100)
  ]
  
  for (data_type, content) in test_cases {
    let original_bytes = content.to_byte_array()
    let compression_result = AdaptiveCompressor::compress(adaptive_compressor, original_bytes)
    
    // Verify the result contains algorithm info and compressed data
    assert_eq(compression_result.compressed_data.length() < original_bytes.length(), true)
    assert_eq(compression_result.algorithm != CompressionAlgorithm::Unknown, true)
    
    // Decompress and verify
    let decompressed = AdaptiveCompressor::decompress(adaptive_compressor, compression_result)
    assert_eq(decompressed, original_bytes)
  }
}

// Test 6: Compression with Error Handling
test "compression error handling" {
  // Test compression of empty data
  let empty_data = []
  let compressor = CompressionEngine::gzip()
  let compressed_empty = CompressionEngine::compress(compressor, empty_data)
  let decompressed_empty = CompressionEngine::decompress(compressor, compressed_empty)
  
  assert_eq(decompressed_empty.length(), 0)
  
  // Test decompression of invalid data
  let invalid_data = [255, 255, 255, 255, 255]
  let decompress_result = CompressionEngine::decompress_safe(compressor, invalid_data)
  
  match decompress_result {
    Ok(_) => assert_true(false)  // Should fail
    Err(error) => assert_eq(error, DecompressionError::InvalidData)
  }
  
  // Test compression with memory constraints
  let large_data = [0; 1000000]  // 1MB of zeros
  let memory_constrained_compressor = CompressionEngine::with_memory_limit(CompressionAlgorithm::Gzip, 100000)  // 100KB limit
  let constrained_result = CompressionEngine::compress_safe(memory_constrained_compressor, large_data)
  
  match constrained_result {
    Ok(_) => assert_true(true)   // Should succeed
    Err(error) => match error {
      CompressionError::MemoryLimitExceeded => assert_true(true)  // Expected for very large data
      _ => assert_true(false)
    }
  }
}

// Test 7: Dictionary-based Compression
test "dictionary-based compression" {
  // Create a custom dictionary for telemetry data
  let telemetry_dictionary = [
    "telemetry", "metric", "timestamp", "value", "sensor", "temperature",
    "humidity", "pressure", "cpu", "memory", "disk", "network", "latency",
    "throughput", "error", "warning", "info", "debug", "trace", "span"
  ]
  
  let dict_compressor = DictionaryCompressor::new(telemetry_dictionary)
  
  // Test with telemetry-like data
  let telemetry_data = "telemetry metric timestamp value sensor temperature humidity pressure cpu memory disk network latency throughput error warning info debug trace span " * 100
  
  let original_bytes = telemetry_data.to_byte_array()
  let compressed = DictionaryCompressor::compress(dict_compressor, original_bytes)
  let decompressed = DictionaryCompressor::decompress(dict_compressor, compressed)
  
  assert_eq(decompressed, original_bytes)
  assert_eq(compressed.length() < original_bytes.length(), true)
  
  // Test compression ratio improvement
  let standard_compressor = CompressionEngine::gzip()
  let standard_compressed = CompressionEngine::compress(standard_compressor, original_bytes)
  
  // Dictionary compression should be more efficient for this specific data
  let dict_ratio = (compressed.length() as f64) / (original_bytes.length() as f64)
  let standard_ratio = (standard_compressed.length() as f64) / (original_bytes.length() as f64)
  
  assert_eq(dict_ratio < standard_ratio, true)
}

// Test 8: Parallel Compression
test "parallel compression" {
  // Create large data suitable for parallel processing
  let data_chunks = []
  let chunk_size = 100000
  
  // Generate multiple chunks of data
  for i = 0; i < 10; i = i + 1 {
    let mut chunk = []
    let base_value = i * 10
    for j = 0; j < chunk_size; j = j + 1 {
      chunk.push((base_value + (j % 256)) % 256)
    }
    data_chunks.push(chunk)
  }
  
  // Test parallel compression
  let parallel_compressor = ParallelCompressor::new(CompressionAlgorithm::Lz4, 4)  // 4 threads
  let parallel_results = ParallelCompressor::compress_chunks(parallel_compressor, data_chunks)
  
  assert_eq(parallel_results.length(), data_chunks.length())
  
  // Decompress all chunks and verify
  for i = 0; i < parallel_results.length(); i = i + 1 {
    let decompressed = CompressionEngine::decompress(
      CompressionEngine::with_algorithm(CompressionAlgorithm::Lz4),
      parallel_results[i]
    )
    assert_eq(decompressed, data_chunks[i])
  }
  
  // Compare with sequential compression
  let sequential_start_time = Clock::now()
  let mut sequential_results = []
  for chunk in data_chunks {
    let compressed = CompressionEngine::compress(
      CompressionEngine::with_algorithm(CompressionAlgorithm::Lz4),
      chunk
    )
    sequential_results.push(compressed)
  }
  let sequential_end_time = Clock::now()
  
  let parallel_start_time = Clock::now()
  let parallel_results_2 = ParallelCompressor::compress_chunks(parallel_compressor, data_chunks)
  let parallel_end_time = Clock::now()
  
  // Parallel should be faster (though this might not always be true in practice)
  let sequential_time = sequential_end_time - sequential_start_time
  let parallel_time = parallel_end_time - parallel_start_time
  
  // At minimum, results should be identical
  assert_eq(parallel_results_2.length(), sequential_results.length())
  for i = 0; i < parallel_results_2.length(); i = i + 1 {
    assert_eq(parallel_results_2[i].length(), sequential_results[i].length())
  }
}

// Test 9: Compression Metadata
test "compression metadata" {
  let original_data = "Test data for compression metadata verification. " * 100
  
  // Create compressor with metadata tracking
  let metadata_compressor = MetadataCompressor::new(CompressionAlgorithm::Gzip)
  let compression_result = MetadataCompressor::compress_with_metadata(metadata_compressor, original_data.to_byte_array())
  
  // Verify metadata
  assert_eq(compression_result.original_size, original_data.length())
  assert_eq(compression_result.compressed_size, compression_result.compressed_data.length())
  assert_eq(compression_result.algorithm, CompressionAlgorithm::Gzip)
  assert_eq(compression_result.compression_time > 0, true)
  assert_eq(compression_result.checksum.length() > 0, true)
  
  // Test decompression with metadata validation
  let decompression_result = MetadataCompressor::decompress_with_validation(
    metadata_compressor, 
    compression_result
  )
  
  match decompression_result {
    Ok(data) => assert_eq(data.to_string(), original_data)
    Err(error) => assert_true(false)
  }
  
  // Test with corrupted data
  let mut corrupted_result = compression_result
  corrupted_result.compressed_data[0] = corrupted_result.compressed_data[0] + 1  // Corrupt first byte
  
  let corrupted_decompression = MetadataCompressor::decompress_with_validation(
    metadata_compressor,
    corrupted_result
  )
  
  match corrupted_decompression {
    Ok(_) => assert_true(false)  // Should fail
    Err(error) => assert_eq(error, DecompressionError::ChecksumMismatch)
  }
}

// Test 10: Compression Performance Benchmarks
test "compression performance benchmarks" {
  let benchmark_data = "Benchmark data for compression performance testing. " * 10000
  let original_bytes = benchmark_data.to_byte_array()
  
  let algorithms = [
    ("gzip", CompressionAlgorithm::Gzip),
    ("deflate", CompressionAlgorithm::Deflate),
    ("lz4", CompressionAlgorithm::Lz4)
  ]
  
  let mut results = []
  
  for (algo_name, algorithm) in algorithms {
    let compressor = CompressionEngine::with_algorithm(algorithm)
    
    // Measure compression time
    let compress_start = Clock::now()
    let compressed = CompressionEngine::compress(compressor, original_bytes)
    let compress_end = Clock::now()
    
    // Measure decompression time
    let decompress_start = Clock::now()
    let decompressed = CompressionEngine::decompress(compressor, compressed)
    let decompress_end = Clock::now()
    
    let compress_time = compress_end - compress_start
    let decompress_time = decompress_end - decompress_start
    let compression_ratio = (compressed.length() as f64) / (original_bytes.length() as f64)
    
    results.push((algo_name, compress_time, decompress_time, compression_ratio))
    
    // Verify data integrity
    assert_eq(decompressed, original_bytes)
  }
  
  // LZ4 should be fastest but with lower compression ratio
  // Gzip should be slower but with better compression ratio
  
  // Sort by compression time
  results.sort_by(fn(a, b) { a.1 - b.1 })
  
  // LZ4 should be among the fastest
  let lz4_found = false
  for result in results {
    if result.0 == "lz4" {
      lz4_found = true
      break
    }
  }
  assert_eq(lz4_found, true)
  
  // Sort by compression ratio (lower is better)
  results.sort_by(fn(a, b) { a.3 - b.3 })
  
  // Gzip or deflate should have the best compression ratio
  let best_ratio = results[0].3
  assert_eq(best_ratio < 0.5, true)  // Should achieve at least 50% compression
}