// Data Compression/Decompression Performance Tests for Azimuth Telemetry System
// This file contains tests for compression algorithms and performance optimization

test "gzip compression performance with telemetry data" {
  let compression_engine = CompressionEngine::new(Gzip)
  let telemetry_data = generate_large_telemetry_dataset(1000)  // 1000 telemetry records
  
  // Measure compression time
  let start_time = Clock::now_unix_nanos(Clock::system())
  let compressed_data = CompressionEngine::compress(compression_engine, telemetry_data)
  let compression_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // Measure decompression time
  let start_decompress_time = Clock::now_unix_nanos(Clock::system())
  let decompressed_data = CompressionEngine::decompress(compression_engine, compressed_data)
  let decompression_time = Clock::now_unix_nanos(Clock::system()) - start_decompress_time
  
  // Verify data integrity
  assert_eq(decompressed_data, telemetry_data)
  
  // Verify compression ratio
  let original_size = telemetry_data.length()
  let compressed_size = compressed_data.length()
  let compression_ratio = (compressed_size.to_float() / original_size.to_float())
  
  assert_true(compression_ratio < 0.5)  // Should compress to less than 50% of original size
  
  // Verify performance benchmarks
  let compression_throughput = (original_size.to_float() / (compression_time.to_float() / 1000000.0))  // MB/s
  let decompression_throughput = (original_size.to_float() / (decompression_time.to_float() / 1000000.0))  // MB/s
  
  assert_true(compression_throughput > 10.0)  // Should compress at least 10 MB/s
  assert_true(decompression_throughput > 20.0)  // Should decompress at least 20 MB/s
}

test "lz4 compression performance with telemetry data" {
  let compression_engine = CompressionEngine::new(Lz4)
  let telemetry_data = generate_large_telemetry_dataset(1000)  // 1000 telemetry records
  
  // Measure compression time
  let start_time = Clock::now_unix_nanos(Clock::system())
  let compressed_data = CompressionEngine::compress(compression_engine, telemetry_data)
  let compression_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // Measure decompression time
  let start_decompress_time = Clock::now_unix_nanos(Clock::system())
  let decompressed_data = CompressionEngine::decompress(compression_engine, compressed_data)
  let decompression_time = Clock::now_unix_nanos(Clock::system()) - start_decompress_time
  
  // Verify data integrity
  assert_eq(decompressed_data, telemetry_data)
  
  // Verify compression ratio
  let original_size = telemetry_data.length()
  let compressed_size = compressed_data.length()
  let compression_ratio = (compressed_size.to_float() / original_size.to_float())
  
  assert_true(compression_ratio < 0.7)  // Should compress to less than 70% of original size
  
  // Verify performance benchmarks (LZ4 should be faster than Gzip)
  let compression_throughput = (original_size.to_float() / (compression_time.to_float() / 1000000.0))  // MB/s
  let decompression_throughput = (original_size.to_float() / (decompression_time.to_float() / 1000000.0))  // MB/s
  
  assert_true(compression_throughput > 50.0)  // Should compress at least 50 MB/s
  assert_true(decompression_throughput > 100.0)  // Should decompress at least 100 MB/s
}

test "zstd compression performance with telemetry data" {
  let compression_engine = CompressionEngine::new(Zstd)
  let telemetry_data = generate_large_telemetry_dataset(1000)  // 1000 telemetry records
  
  // Measure compression time
  let start_time = Clock::now_unix_nanos(Clock::system())
  let compressed_data = CompressionEngine::compress(compression_engine, telemetry_data)
  let compression_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // Measure decompression time
  let start_decompress_time = Clock::now_unix_nanos(Clock::system())
  let decompressed_data = CompressionEngine::decompress(compression_engine, compressed_data)
  let decompression_time = Clock::now_unix_nanos(Clock::system()) - start_decompress_time
  
  // Verify data integrity
  assert_eq(decompressed_data, telemetry_data)
  
  // Verify compression ratio (Zstd should have better compression than Gzip)
  let original_size = telemetry_data.length()
  let compressed_size = compressed_data.length()
  let compression_ratio = (compressed_size.to_float() / original_size.to_float())
  
  assert_true(compression_ratio < 0.4)  // Should compress to less than 40% of original size
  
  // Verify performance benchmarks
  let compression_throughput = (original_size.to_float() / (compression_time.to_float() / 1000000.0))  // MB/s
  let decompression_throughput = (original_size.to_float() / (decompression_time.to_float() / 1000000.0))  // MB/s
  
  assert_true(compression_throughput > 20.0)  // Should compress at least 20 MB/s
  assert_true(decompression_throughput > 50.0)  // Should decompress at least 50 MB/s
}

test "adaptive compression algorithm selection" {
  let adaptive_compressor = AdaptiveCompressor::new()
  let small_dataset = generate_large_telemetry_dataset(100)   // Small dataset
  let medium_dataset = generate_large_telemetry_dataset(1000) // Medium dataset
  let large_dataset = generate_large_telemetry_dataset(10000) // Large dataset
  
  // Test adaptive algorithm selection for different data sizes
  let small_algorithm = AdaptiveCompressor::select_algorithm(adaptive_compressor, small_dataset)
  let medium_algorithm = AdaptiveCompressor::select_algorithm(adaptive_compressor, medium_dataset)
  let large_algorithm = AdaptiveCompressor::select_algorithm(adaptive_compressor, large_dataset)
  
  // Verify algorithm selection based on data characteristics
  assert_eq(small_algorithm, Lz4)      // Small data: fastest compression
  assert_eq(medium_algorithm, Zstd)     // Medium data: balance of speed and ratio
  assert_eq(large_algorithm, Gzip)      // Large data: best compression ratio
  
  // Test compression with selected algorithms
  let small_compressed = AdaptiveCompressor::compress_with_algorithm(adaptive_compressor, small_dataset, small_algorithm)
  let medium_compressed = AdaptiveCompressor::compress_with_algorithm(adaptive_compressor, medium_dataset, medium_algorithm)
  let large_compressed = AdaptiveCompressor::compress_with_algorithm(adaptive_compressor, large_dataset, large_algorithm)
  
  // Verify all compressed successfully
  assert_true(small_compressed.length() > 0)
  assert_true(medium_compressed.length() > 0)
  assert_true(large_compressed.length() > 0)
  
  // Verify decompression works
  let small_decompressed = AdaptiveCompressor::decompress_with_algorithm(adaptive_compressor, small_compressed, small_algorithm)
  let medium_decompressed = AdaptiveCompressor::decompress_with_algorithm(adaptive_compressor, medium_compressed, medium_algorithm)
  let large_decompressed = AdaptiveCompressor::decompress_with_algorithm(adaptive_compressor, large_compressed, large_algorithm)
  
  assert_eq(small_decompressed, small_dataset)
  assert_eq(medium_decompressed, medium_dataset)
  assert_eq(large_decompressed, large_dataset)
}

test "compression level optimization" {
  let compression_engine = CompressionEngine::new(Gzip)
  let telemetry_data = generate_large_telemetry_dataset(1000)
  
  // Test different compression levels
  let level1_compressed = CompressionEngine::compress_with_level(compression_engine, telemetry_data, 1)   // Fastest
  let level5_compressed = CompressionEngine::compress_with_level(compression_engine, telemetry_data, 5)   // Balanced
  let level9_compressed = CompressionEngine::compress_with_level(compression_engine, telemetry_data, 9)   // Best compression
  
  // Verify all levels produce valid compressed data
  assert_true(level1_compressed.length() > 0)
  assert_true(level5_compressed.length() > 0)
  assert_true(level9_compressed.length() > 0)
  
  // Verify compression ratio improves with higher levels
  let level1_ratio = (level1_compressed.length().to_float() / telemetry_data.length().to_float())
  let level5_ratio = (level5_compressed.length().to_float() / telemetry_data.length().to_float())
  let level9_ratio = (level9_compressed.length().to_float() / telemetry_data.length().to_float())
  
  assert_true(level9_ratio < level5_ratio)  // Level 9 should compress better than level 5
  assert_true(level5_ratio < level1_ratio)  // Level 5 should compress better than level 1
  
  // Verify all can be decompressed correctly
  let level1_decompressed = CompressionEngine::decompress(compression_engine, level1_compressed)
  let level5_decompressed = CompressionEngine::decompress(compression_engine, level5_compressed)
  let level9_decompressed = CompressionEngine::decompress(compression_engine, level9_compressed)
  
  assert_eq(level1_decompressed, telemetry_data)
  assert_eq(level5_decompressed, telemetry_data)
  assert_eq(level9_decompressed, telemetry_data)
}

test "streaming compression for large datasets" {
  let streaming_compressor = StreamingCompressor::new(Gzip)
  let telemetry_data = generate_large_telemetry_dataset(10000)  // 10000 telemetry records
  
  // Measure streaming compression time
  let start_time = Clock::now_unix_nanos(Clock::system())
  let compressed_stream = StreamingCompressor::compress_stream(streaming_compressor, telemetry_data, 1024)  // 1KB chunks
  let compression_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // Measure streaming decompression time
  let start_decompress_time = Clock::now_unix_nanos(Clock::system())
  let decompressed_stream = StreamingCompressor::decompress_stream(streaming_compressor, compressed_stream, 1024)  // 1KB chunks
  let decompression_time = Clock::now_unix_nanos(Clock::system()) - start_decompress_time
  
  // Verify data integrity
  assert_eq(decompressed_stream, telemetry_data)
  
  // Verify streaming compression has reasonable memory usage
  let memory_usage = StreamingCompressor::get_memory_usage(streaming_compressor)
  assert_true(memory_usage < 10 * 1024 * 1024)  // Should use less than 10MB for 10K records
  
  // Verify performance benchmarks
  let original_size = telemetry_data.length()
  let compression_throughput = (original_size.to_float() / (compression_time.to_float() / 1000000.0))  // MB/s
  let decompression_throughput = (original_size.to_float() / (decompression_time.to_float() / 1000000.0))  // MB/s
  
  assert_true(compression_throughput > 5.0)   // Should compress at least 5 MB/s with streaming
  assert_true(decompression_throughput > 10.0)  // Should decompress at least 10 MB/s with streaming
}

test "compression with telemetry batching optimization" {
  let batch_compressor = BatchCompressionOptimizer::new()
  let telemetry_batches = [
    generate_large_telemetry_dataset(100),
    generate_large_telemetry_dataset(200),
    generate_large_telemetry_dataset(300),
    generate_large_telemetry_dataset(400),
    generate_large_telemetry_dataset(500)
  ]
  
  // Compress individual batches
  let mut individually_compressed = []
  let mut individual_compression_times = []
  
  for batch in telemetry_batches {
    let start_time = Clock::now_unix_nanos(Clock::system())
    let compressed = BatchCompressionOptimizer::compress_batch(batch_compressor, batch)
    let compression_time = Clock::now_unix_nanos(Clock::system()) - start_time
    
    individually_compressed.push(compressed)
    individual_compression_times.push(compression_time)
  }
  
  // Compress with batch optimization
  let start_time = Clock::now_unix_nanos(Clock::system())
  let optimized_compressed = BatchCompressionOptimizer::compress_optimized(batch_compressor, telemetry_batches)
  let optimized_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // Verify optimized compression is more efficient
  let individual_total_time = individual_compression_times.fold(0L, fn(acc, time) { acc + time })
  let optimization_ratio = (optimized_time.to_float() / individual_total_time.to_float())
  
  assert_true(optimization_ratio < 0.8)  // Should be at least 20% faster
  
  // Verify decompression works for both methods
  let mut individually_decompressed = []
  for compressed in individually_compressed {
    let decompressed = BatchCompressionOptimizer::decompress_batch(batch_compressor, compressed)
    individually_decompressed.push(decompressed)
  }
  
  let optimized_decompressed = BatchCompressionOptimizer::decompress_optimized(batch_compressor, optimized_compressed)
  
  // Verify data integrity
  assert_eq(individually_decompressed, telemetry_batches)
  assert_eq(optimized_decompressed, telemetry_batches)
}

test "compression error handling and edge cases" {
  let compression_engine = CompressionEngine::new(Gzip)
  
  // Test compression of empty data
  let empty_data = ""
  let compressed_empty = CompressionEngine::compress(compression_engine, empty_data)
  let decompressed_empty = CompressionEngine::decompress(compression_engine, compressed_empty)
  assert_eq(decompressed_empty, empty_data)
  
  // Test compression of very small data
  let small_data = "x"
  let compressed_small = CompressionEngine::compress(compression_engine, small_data)
  let decompressed_small = CompressionEngine::decompress(compression_engine, compressed_small)
  assert_eq(decompressed_small, small_data)
  
  // Test decompression of corrupted data
  let corrupted_data = [0x1F, 0x8B, 0x08, 0x00, 0xFF, 0xFF, 0xFF, 0xFF]  // Invalid gzip header
  let decompress_result = CompressionEngine::decompress(compression_engine, corrupted_data)
  
  match decompress_result {
    DecompressionError(_) => assert_true(true)  // Expected error
    _ => assert_true(false)  // Should not succeed
  }
  
  // Test compression with invalid level
  let telemetry_data = generate_large_telemetry_dataset(100)
  let invalid_level_compression = CompressionEngine::compress_with_level(compression_engine, telemetry_data, 15)  // Invalid level
  
  match invalid_level_compression {
    CompressionError(_) => assert_true(true)  // Expected error
    _ => assert_true(false)  // Should not succeed
  }
  
  // Test decompression of truncated data
  let valid_compressed = CompressionEngine::compress(compression_engine, telemetry_data)
  let truncated_data = valid_compressed.slice(0, valid_compressed.length() / 2)  // Take half the data
  let truncated_decompress = CompressionEngine::decompress(compression_engine, truncated_data)
  
  match truncated_decompress {
    DecompressionError(_) => assert_true(true)  // Expected error
    _ => assert_true(false)  // Should not succeed
  }
}

test "compression performance under memory constraints" {
  let memory_constrained_compressor = MemoryConstrainedCompressor::new(5 * 1024 * 1024)  // 5MB limit
  let telemetry_data = generate_large_telemetry_dataset(5000)  // Large dataset
  
  // Test compression under memory constraint
  let start_time = Clock::now_unix_nanos(Clock::system())
  let compressed_data = MemoryConstrainedCompressor::compress(memory_constrained_compressor, telemetry_data)
  let compression_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // Verify memory constraint was respected
  let memory_usage = MemoryConstrainedCompressor::get_peak_memory_usage(memory_constrained_compressor)
  assert_true(memory_usage <= 5 * 1024 * 1024)  // Should not exceed 5MB
  
  // Verify compression succeeded
  assert_true(compressed_data.length() > 0)
  
  // Test decompression under memory constraint
  let start_decompress_time = Clock::now_unix_nanos(Clock::system())
  let decompressed_data = MemoryConstrainedCompressor::decompress(memory_constrained_compressor, compressed_data)
  let decompression_time = Clock::now_unix_nanos(Clock::system()) - start_decompress_time
  
  // Verify decompression memory constraint
  let decompress_memory_usage = MemoryConstrainedCompressor::get_peak_memory_usage(memory_constrained_compressor)
  assert_true(decompress_memory_usage <= 5 * 1024 * 1024)  // Should not exceed 5MB
  
  // Verify data integrity
  assert_eq(decompressed_data, telemetry_data)
  
  // Verify performance under memory constraint
  let original_size = telemetry_data.length()
  let compression_throughput = (original_size.to_float() / (compression_time.to_float() / 1000000.0))  // MB/s
  let decompression_throughput = (original_size.to_float() / (decompression_time.to_float() / 1000000.0))  // MB/s
  
  assert_true(compression_throughput > 1.0)   // Should compress at least 1 MB/s under memory constraint
  assert_true(decompression_throughput > 2.0)  // Should decompress at least 2 MB/s under memory constraint
}

// Helper function to generate large telemetry dataset for testing
fn generate_large_telemetry_dataset(record_count : Int) -> String {
  let mut builder = StringBuilder::new()
  
  for i = 0; i < record_count; i = i + 1 {
    let span_data = "{\"traceId\":\"trace" + i.to_string() + "\",\"spanId\":\"span" + i.to_string() + "\",\"name\":\"telemetry-span-" + i.to_string() + "\",\"kind\":\"INTERNAL\",\"status\":{\"code\":\"OK\"},\"attributes\":{\"key1\":\"value1\",\"key2\":" + i.to_string() + ",\"key3\":3.14},\"events\":[{\"name\":\"event-" + i.to_string() + "\",\"timestamp\":" + (1234567890L + i.to_long()).to_string() + ",\"attributes\":{\"event.key\":\"event.value\"}}],\"startTime\":" + (1234567890L + i.to_long()).to_string() + ",\"endTime\":" + (1234567890L + 100L + i.to_long()).to_string() + "}"
    
    builder.append(span_data)
    if i < record_count - 1 {
      builder.append("\n")
    }
  }
  
  builder.to_string()
}