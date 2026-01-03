// Azimuth Telemetry System - Data Compression Tests
// This file contains test cases for data compression functionality

// Test 1: Basic Compression Algorithms
test "basic compression algorithms" {
  let compression_manager = CompressionManager::new()
  
  // Test data
  let test_data = "This is a test string for compression. It contains repetitive text that should compress well. This is a test string for compression. It contains repetitive text that should compress well."
  
  // Test GZIP compression
  let gzip_compressed = CompressionManager::compress(compression_manager, test_data, "gzip")
  let gzip_decompressed = CompressionManager::decompress(compression_manager, gzip_compressed, "gzip")
  
  assert_eq(gzip_decompressed, test_data)
  assert_true(gzip_compressed.length() < test_data.length()) // Should be smaller
  
  // Test DEFLATE compression
  let deflate_compressed = CompressionManager::compress(compression_manager, test_data, "deflate")
  let deflate_decompressed = CompressionManager::decompress(compression_manager, deflate_compressed, "deflate")
  
  assert_eq(deflate_decompressed, test_data)
  assert_true(deflate_compressed.length() < test_data.length()) // Should be smaller
  
  // Test LZ4 compression
  let lz4_compressed = CompressionManager::compress(compression_manager, test_data, "lz4")
  let lz4_decompressed = CompressionManager::decompress(compression_manager, lz4_compressed, "lz4")
  
  assert_eq(lz4_decompressed, test_data)
  assert_true(lz4_compressed.length() < test_data.length()) // Should be smaller
  
  // Test BROTLI compression
  let brotli_compressed = CompressionManager::compress(compression_manager, test_data, "brotli")
  let brotli_decompressed = CompressionManager::decompress(compression_manager, brotli_compressed, "brotli")
  
  assert_eq(brotli_decompressed, test_data)
  assert_true(brotli_compressed.length() < test_data.length()) // Should be smaller
  
  // Compare compression ratios
  let gzip_ratio = test_data.length() / gzip_compressed.length()
  let deflate_ratio = test_data.length() / deflate_compressed.length()
  let lz4_ratio = test_data.length() / lz4_compressed.length()
  let brotli_ratio = test_data.length() / brotli_compressed.length()
  
  // All should have reasonable compression ratios
  assert_true(gzip_ratio > 1.0)
  assert_true(deflate_ratio > 1.0)
  assert_true(lz4_ratio > 1.0)
  assert_true(brotli_ratio > 1.0)
}

// Test 2: Telemetry Data Compression
test "telemetry data compression" {
  let telemetry_compressor = TelemetryCompressor::new()
  
  // Create test telemetry data
  let span_ctx = SpanContext::new("trace_id_12345", "span_id_67890", true, "test_state")
  let span = Span::new("test_operation", Internal, span_ctx)
  Span::add_event(span, "test_event", Some([("attribute1", StringValue("value1")), ("attribute2", StringValue("value2"))]))
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Serialize the span
  let serialized_span = SpanSerializer::serialize(span)
  
  // Compress the serialized span
  let compressed_span = TelemetryCompressor::compress_span(telemetry_compressor, serialized_span)
  
  // Verify compression
  assert_true(compressed_span.length() < serialized_span.length())
  
  // Decompress and verify integrity
  let decompressed_span = TelemetryCompressor::decompress_span(telemetry_compressor, compressed_span)
  let restored_span = SpanSerializer::deserialize(decompressed_span)
  
  assert_eq(Span::name(restored_span), "test_operation")
  assert_eq(SpanContext::trace_id(Span::span_context(restored_span)), "trace_id_12345")
  assert_eq(SpanContext::span_id(Span::span_context(restored_span)), "span_id_67890")
  
  // Test batch compression
  let spans = []
  for i in 0..=99 {
    let span_ctx = SpanContext::new("trace_id_" + i.to_string(), "span_id_" + i.to_string(), true, "")
    let span = Span::new("batch_test_span", Internal, span_ctx)
    spans.push(span)
  }
  
  let serialized_spans = []
  for span in spans {
    serialized_spans.push(SpanSerializer::serialize(span))
  }
  
  let compressed_batch = TelemetryCompressor::compress_batch(telemetry_compressor, serialized_spans)
  let decompressed_batch = TelemetryCompressor::decompress_batch(telemetry_compressor, compressed_batch)
  
  assert_eq(decompressed_batch.length(), serialized_spans.length())
  
  for i in 0..=decompressed_batch.length() - 1 {
    let restored_span = SpanSerializer::deserialize(decompressed_batch[i])
    assert_eq(Span::name(restored_span), "batch_test_span")
  }
}

// Test 3: Adaptive Compression
test "adaptive compression" {
  let adaptive_compressor = AdaptiveCompressor::new()
  
  // Configure adaptive compression
  AdaptiveCompressor::set_thresholds(adaptive_compressor, {
    "size_threshold": 1024, // Use compression for data larger than 1KB
    "compression_ratio_threshold": 1.2, // Only use compression if ratio > 1.2
    "time_threshold": 10 // Only use compression if it takes less than 10ms
  })
  
  // Test with small data (should not compress)
  let small_data = "Small data"
  let small_result = AdaptiveCompressor::compress_if_beneficial(adaptive_compressor, small_data)
  match small_result {
    Compressed(_) => assert_true(false) // Should not compress small data
    Uncompressed(data) => assert_eq(data, small_data)
  }
  
  // Test with large, repetitive data (should compress)
  let large_data = "Large repetitive data. " * 100
  let large_result = AdaptiveCompressor::compress_if_beneficial(adaptive_compressor, large_data)
  match large_result {
    Compressed(compressed) => {
      let decompressed = AdaptiveCompressor::decompress(adaptive_compressor, compressed)
      assert_eq(decompressed, large_data)
    }
    Uncompressed(_) => assert_true(false) // Should compress large data
  }
  
  // Test with large, non-repetitive data (might not compress well)
  let random_data = generate_random_string(2048)
  let random_result = AdaptiveCompressor::compress_if_beneficial(adaptive_compressor, random_data)
  match random_result {
    Compressed(_) => assert_true(true) // Might compress
    Uncompressed(data) => assert_eq(data, random_data) // Or might not compress if not beneficial
  }
  
  // Test performance-based adaptation
  AdaptiveCompressor::enable_performance_adaptation(adaptive_compressor)
  
  let performance_data = "Performance test data. " * 200
  let performance_result = AdaptiveCompressor::compress_with_performance_check(adaptive_compressor, performance_data)
  
  match performance_result {
    Compressed(_) => assert_true(true) // Should compress if fast enough
    Uncompressed(data) => assert_eq(data, performance_data) // Or not compress if too slow
  }
}

// Test 4: Streaming Compression
test "streaming compression" {
  let streaming_compressor = StreamingCompressor::new()
  
  // Create a large amount of data to stream
  let data_chunks = []
  for i in 0..=99 {
    data_chunks.push("Data chunk " + i.to_string() + ". ")
  }
  
  // Initialize streaming compression
  StreamingCompressor::initialize_compression(streaming_compressor, "gzip")
  
  // Compress data chunks
  let compressed_chunks = []
  for chunk in data_chunks {
    let compressed_chunk = StreamingCompressor::compress_chunk(streaming_compressor, chunk)
    compressed_chunks.push(compressed_chunk)
  }
  
  // Finalize compression
  let final_chunk = StreamingCompressor::finalize_compression(streaming_compressor)
  compressed_chunks.push(final_chunk)
  
  // Initialize streaming decompression
  StreamingCompressor::initialize_decompression(streaming_compressor, "gzip")
  
  // Decompress data chunks
  let decompressed_chunks = []
  for chunk in compressed_chunks {
    let decompressed_chunk = StreamingCompressor::decompress_chunk(streaming_compressor, chunk)
    match decompressed_chunk {
      Some(data) => decompressed_chunks.push(data)
      None => {} // Some chunks might not produce output
    }
  }
  
  // Verify decompressed data
  let decompressed_data = decompressed_chunks.join("")
  let original_data = data_chunks.join("")
  assert_eq(decompressed_data, original_data)
  
  // Test memory efficiency of streaming
  let memory_usage = StreamingCompressor::get_memory_usage(streaming_compressor)
  assert_true(memory_usage < original_data.length() / 2) // Should use less memory than full data
}

// Test 5: Compression with Encryption
test "compression with encryption" {
  let secure_compressor = SecureCompressor::new()
  
  // Set encryption key
  SecureCompressor::set_encryption_key(secure_compressor, "encryption_key_12345")
  
  // Test data
  let test_data = "Sensitive telemetry data that needs both compression and encryption"
  
  // Compress and encrypt
  let compressed_encrypted = SecureCompressor::compress_and_encrypt(secure_compressor, test_data)
  
  // Verify it's different from original
  assert_false(compressed_encrypted.equals(test_data))
  
  // Decrypt and decompress
  let decrypted_decompressed = SecureCompressor::decrypt_and_decompress(secure_compressor, compressed_encrypted)
  
  // Verify integrity
  assert_eq(decrypted_decompressed, test_data)
  
  // Test with wrong key
  SecureCompressor::set_encryption_key(secure_compressor, "wrong_key")
  let decryption_result = SecureCompressor::decrypt_and_decompress(secure_compressor, compressed_encrypted)
  match decryption_result {
    Error(_) => assert_true(true) // Should fail with wrong key
    Success(_) => assert_true(false)
  }
  
  // Restore correct key
  SecureCompressor::set_encryption_key(secure_compressor, "encryption_key_12345")
  
  // Test compress-then-encrypt vs encrypt-then-compress
  let compress_first = SecureCompressor::compress_then_encrypt(secure_compressor, test_data)
  let encrypt_first = SecureCompressor::encrypt_then_compress(secure_compressor, test_data)
  
  // Both should produce valid results
  let result1 = SecureCompressor::decrypt_and_decompress(secure_compressor, compress_first)
  let result2 = SecureCompressor::decompress_and_decrypt(secure_compressor, encrypt_first)
  
  match result1 {
    Success(data) => assert_eq(data, test_data)
    Error(_) => assert_true(false)
  }
  
  match result2 {
    Success(data) => assert_eq(data, test_data)
    Error(_) => assert_true(false)
  }
}

// Test 6: Compression Performance
test "compression performance" {
  let performance_compressor = PerformanceCompressor::new()
  
  // Generate test data of different sizes
  let small_data = "Small test data"
  let medium_data = "Medium test data. " * 100
  let large_data = "Large test data. " * 1000
  
  // Test compression speed
  let small_compression_time = PerformanceCompressor::measure_compression_time(performance_compressor, small_data, "gzip")
  let medium_compression_time = PerformanceCompressor::measure_compression_time(performance_compressor, medium_data, "gzip")
  let large_compression_time = PerformanceCompressor::measure_compression_time(performance_compressor, large_data, "gzip")
  
  // Compression time should scale reasonably with data size
  assert_true(small_compression_time < medium_compression_time)
  assert_true(medium_compression_time < large_compression_time)
  
  // Test decompression speed
  let small_compressed = PerformanceCompressor::compress(performance_compressor, small_data, "gzip")
  let medium_compressed = PerformanceCompressor::compress(performance_compressor, medium_data, "gzip")
  let large_compressed = PerformanceCompressor::compress(performance_compressor, large_data, "gzip")
  
  let small_decompression_time = PerformanceCompressor::measure_decompression_time(performance_compressor, small_compressed, "gzip")
  let medium_decompression_time = PerformanceCompressor::measure_decompression_time(performance_compressor, medium_compressed, "gzip")
  let large_decompression_time = PerformanceCompressor::measure_decompression_time(performance_compressor, large_compressed, "gzip")
  
  // Decompression time should also scale reasonably
  assert_true(small_decompression_time < medium_decompression_time)
  assert_true(medium_decompression_time < large_decompression_time)
  
  // Test different algorithms
  let algorithms = ["gzip", "deflate", "lz4", "brotli"]
  let compression_times = []
  let compression_ratios = []
  
  for algorithm in algorithms {
    let time = PerformanceCompressor::measure_compression_time(performance_compressor, medium_data, algorithm)
    let compressed = PerformanceCompressor::compress(performance_compressor, medium_data, algorithm)
    let ratio = medium_data.length() / compressed.length()
    
    compression_times.push((algorithm, time))
    compression_ratios.push((algorithm, ratio))
  }
  
  // Find fastest algorithm
  let fastest = compression_times.min_by(|(_, time1), (_, time2)| time1 < time2)
  match fastest {
    Some((algorithm, _)) => assert_true(true) // Found fastest algorithm
    None => assert_true(false)
  }
  
  // Find best compression ratio
  let best_ratio = compression_ratios.max_by(|(_, ratio1), (_, ratio2)| ratio1 > ratio2)
  match best_ratio {
    Some((algorithm, _)) => assert_true(true) // Found best ratio
    None => assert_true(false)
  }
}

// Test 7: Compression Level Tuning
test "compression level tuning" {
  let tuning_compressor = TuningCompressor::new()
  
  // Test data
  let test_data = "Test data for compression level tuning. " * 50
  
  // Test different compression levels
  let levels = [1, 3, 5, 7, 9] // 1 = fastest, 9 = best compression
  let results = []
  
  for level in levels {
    let start_time = TuningCompressor::get_timestamp(tuning_compressor)
    let compressed = TuningCompressor::compress_with_level(tuning_compressor, test_data, "gzip", level)
    let end_time = TuningCompressor::get_timestamp(tuning_compressor)
    
    let compression_time = end_time - start_time
    let compression_ratio = test_data.length() / compressed.length()
    
    results.push((level, compression_time, compression_ratio, compressed.length()))
  }
  
  // Higher compression levels should generally:
  // 1. Take more time
  // 2. Produce better compression ratios
  // 3. Result in smaller compressed data
  
  let first_result = results[0]
  let last_result = results[4]
  
  // Last result (highest level) should take longer
  assert_true(last_result.1 > first_result.1)
  
  // Last result should have better compression ratio
  assert_true(last_result.2 >= first_result.2)
  
  // Last result should produce smaller data
  assert_true(last_result.3 <= first_result.3)
  
  // Test automatic level selection
  TuningCompressor::enable_auto_level_selection(tuning_compressor)
  
  let auto_level = TuningCompressor::select_optimal_level(tuning_compressor, test_data, "gzip")
  assert_true(auto_level >= 1 && auto_level <= 9)
  
  // Test with different constraints
  let time_constraint_level = TuningCompressor::select_level_with_time_constraint(tuning_compressor, test_data, "gzip", 50) // 50ms max
  assert_true(time_constraint_level >= 1 && time_constraint_level <= 9)
  
  let ratio_constraint_level = TuningCompressor::select_level_with_ratio_constraint(tuning_compressor, test_data, "gzip", 2.0) // Minimum 2:1 ratio
  assert_true(ratio_constraint_level >= 1 && ratio_constraint_level <= 9)
}

// Test 8: Dictionary-based Compression
test "dictionary-based compression" {
  let dictionary_compressor = DictionaryCompressor::new()
  
  // Create a dictionary from common telemetry patterns
  let training_data = [
    "trace_id_",
    "span_id_",
    "operation_name",
    "http.method",
    "http.status_code",
    "http.url",
    "db.statement",
    "error.message",
    "service.name",
    "telemetry.sdk"
  ]
  
  DictionaryCompressor::create_dictionary(dictionary_compressor, training_data)
  
  // Test compression with dictionary
  let test_data = "trace_id_12345 span_id_67890 operation_name:http.method GET http.status_code:200 http.url:/api/endpoint"
  let compressed_with_dict = DictionaryCompressor::compress_with_dictionary(dictionary_compressor, test_data)
  let compressed_without_dict = DictionaryCompressor::compress_without_dictionary(dictionary_compressor, test_data)
  
  // Dictionary compression should be more efficient for this data
  assert_true(compressed_with_dict.length() <= compressed_without_dict.length())
  
  // Decompress and verify
  let decompressed_with_dict = DictionaryCompressor::decompress_with_dictionary(dictionary_compressor, compressed_with_dict)
  assert_eq(decompressed_with_dict, test_data)
  
  // Test with data not in dictionary
  let unrelated_data = "This data contains no patterns from the dictionary"
  let unrelated_compressed_with_dict = DictionaryCompressor::compress_with_dictionary(dictionary_compressor, unrelated_data)
  let unrelated_compressed_without_dict = DictionaryCompressor::compress_without_dictionary(dictionary_compressor, unrelated_data)
  
  // Dictionary might not help with unrelated data
  assert_true(unrelated_compressed_with_dict.length() >= unrelated_compressed_without_dict.length())
  
  // Test dictionary optimization
  let optimization_result = DictionaryCompressor::optimize_dictionary(dictionary_compressor, [
    "trace_id_12345",
    "span_id_67890",
    "operation_name:http.method",
    "http.status_code:200"
  ])
  
  match optimization_result {
    Success(improvement) => assert_true(improvement > 0) // Dictionary should improve
    Error(_) => assert_true(false)
  }
}

// Test 9: Compression for Time Series Data
test "compression for time series data" {
  let timeseries_compressor = TimeSeriesCompressor::new()
  
  // Create time series data
  let time_series = []
  for i in 0..=99 {
    let timestamp = 1609459200L + (i * 60L) // Every minute
    let value = 42.0 + (i * 0.1) // Gradually increasing
    time_series.push(TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Compress with time series algorithm
  let compressed = TimeSeriesCompressor::compress_time_series(timeseries_compressor, time_series)
  
  // Decompress and verify
  let decompressed = TimeSeriesCompressor::decompress_time_series(timeseries_compressor, compressed)
  
  assert_eq(decompressed.length(), time_series.length())
  
  for i in 0..=decompressed.length() - 1 {
    assert_eq(TimeSeriesDataPoint::timestamp(decompressed[i]), TimeSeriesDataPoint::timestamp(time_series[i]))
    assert_eq(TimeSeriesDataPoint::value(decompressed[i]), TimeSeriesDataPoint::value(time_series[i]))
  }
  
  // Test with different time series patterns
  let constant_series = []
  for i in 0..=99 {
    let timestamp = 1609459200L + (i * 60L)
    constant_series.push(TimeSeriesDataPoint::new(timestamp, 42.0)) // Constant value
  }
  
  let compressed_constant = TimeSeriesCompressor::compress_time_series(timeseries_compressor, constant_series)
  let decompressed_constant = TimeSeriesCompressor::decompress_time_series(timeseries_compressor, compressed_constant)
  
  assert_eq(decompressed_constant.length(), constant_series.length())
  
  // Test with sparse time series
  let sparse_series = []
  for i in 0..=99 {
    if i % 10 == 0 { // Only every 10th point
      let timestamp = 1609459200L + (i * 60L)
      sparse_series.push(TimeSeriesDataPoint::new(timestamp, 42.0))
    }
  }
  
  let compressed_sparse = TimeSeriesCompressor::compress_time_series(timeseries_compressor, sparse_series)
  let decompressed_sparse = TimeSeriesCompressor::decompress_time_series(timeseries_compressor, compressed_sparse)
  
  assert_eq(decompressed_sparse.length(), sparse_series.length())
  
  // Compare compression ratios for different patterns
  let regular_ratio = (time_series.length() * 16) / compressed.length() // Assuming 16 bytes per point
  let constant_ratio = (constant_series.length() * 16) / compressed_constant.length()
  let sparse_ratio = (sparse_series.length() * 16) / compressed_sparse.length()
  
  // Constant data should compress best
  assert_true(constant_ratio >= regular_ratio)
  // Sparse data should also compress well
  assert_true(sparse_ratio >= regular_ratio)
}

// Test 10: Compression Error Handling and Recovery
test "compression error handling and recovery" {
  let error_compressor = ErrorHandlingCompressor::new()
  
  // Test with corrupted compressed data
  let test_data = "Test data for error handling"
  let compressed = ErrorHandlingCompressor::compress(error_compressor, test_data, "gzip")
  
  // Corrupt the compressed data
  let corrupted_data = corrupted_data(compressed)
  
  // Attempt to decompress corrupted data
  let decompress_result = ErrorHandlingCompressor::safe_decompress(error_compressor, corrupted_data, "gzip")
  match decompress_result {
    Error(error) => assert_true(error.contains("corruption") || error.contains("invalid"))
    Success(_) => assert_true(false) // Should not succeed with corrupted data
  }
  
  // Test with unsupported algorithm
  let unsupported_result = ErrorHandlingCompressor::compress(error_compressor, test_data, "unsupported_algorithm")
  match unsupported_result {
    Error(error) => assert_true(error.contains("unsupported"))
    Success(_) => assert_true(false) // Should not succeed with unsupported algorithm
  }
  
  // Test recovery mechanisms
  let recovery_compressor = RecoveryCompressor::new()
  
  // Configure recovery strategies
  RecoveryCompressor::set_recovery_strategy(recovery_compressor, {
    "max_retries": 3,
    "fallback_algorithms": ["gzip", "deflate", "lz4"],
    "error_threshold": 0.1 // 10% error rate threshold
  })
  
  // Test with data that causes compression failure
  let problematic_data = generate_problematic_data()
  
  let recovery_result = RecoveryCompressor::compress_with_recovery(recovery_compressor, problematic_data)
  match recovery_result {
    Success(compressed_data) => {
      // Should succeed with fallback
      let decompressed = RecoveryCompressor::safe_decompress(recovery_compressor, compressed_data)
      match decompressed {
        Success(data) => assert_eq(data, problematic_data)
        Error(_) => assert_true(false)
      }
    }
    Error(_) => {
      // If all algorithms fail, should have partial data or fallback
      let fallback_data = RecoveryCompressor::get_fallback_data(recovery_compressor)
      assert_true(fallback_data.is_some())
    }
  }
  
  // Test compression monitoring
  let monitoring_compressor = MonitoringCompressor::new()
  
  MonitoringCompressor::enable_monitoring(monitoring_compressor)
  
  // Perform several compression operations
  for i in 0..=9 {
    let data = "Monitoring test data " + i.to_string()
    MonitoringCompressor::compress(monitoring_compressor, data, "gzip")
  }
  
  // Check monitoring statistics
  let stats = MonitoringCompressor::get_statistics(monitoring_compressor)
  
  assert_eq(stats.total_operations, 10)
  assert_eq(stats.successful_operations, 10)
  assert_eq(stats.failed_operations, 0)
  assert_true(stats.average_compression_time > 0)
  assert_true(stats.average_compression_ratio > 1.0)
  
  // Simulate some failures
  MonitoringCompressor::simulate_failure(monitoring_compressor, 3) // 3 failures
  
  // Perform more operations
  for i in 0..=6 {
    let data = "Monitoring test data with failures " + i.to_string()
    MonitoringCompressor::compress(monitoring_compressor, data, "gzip")
  }
  
  // Check updated statistics
  let updated_stats = MonitoringCompressor::get_statistics(monitoring_compressor)
  
  assert_eq(updated_stats.total_operations, 17)
  assert_eq(updated_stats.successful_operations, 14)
  assert_eq(updated_stats.failed_operations, 3)
  assert_true(updated_stats.error_rate > 0.0)
}

// Helper function to generate random string
func generate_random_string(length : Int) -> String {
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  let result = ""
  
  for i in 0..=length - 1 {
    let index = Math::random() % chars.length()
    result = result + chars.substring(index, 1)
  }
  
  result
}

// Helper function to generate problematic data
func generate_problematic_data() -> String {
  let result = ""
  
  // Add various problematic patterns
  for i in 0..=1000 {
    if i % 100 == 0 {
      result = result + "\0\0\0\0" // Null bytes
    } else if i % 50 == 0 {
      result = result + String::from_char(255) // High byte values
    } else {
      result = result + "Normal text "
    }
  }
  
  result
}

// Helper function to corrupt data
func corrupted_data(data : String) -> String {
  if data.length() == 0 {
    return data
  }
  
  let bytes = data.to_bytes()
  
  // Flip some bits
  for i in 0..=min(bytes.length() - 1, 10) {
    bytes[i] = bytes[i] ^ 0xFF // Flip all bits
  }
  
  String::from_bytes(bytes)
}

// Helper function to get minimum
func min(a : Int, b : Int) -> Int {
  if a < b {
    a
  } else {
    b
  }
}