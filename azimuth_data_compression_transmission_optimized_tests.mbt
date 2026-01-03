// Azimuth Telemetry System - Data Compression and Transmission Optimization Tests
// This file contains comprehensive test cases for data compression and transmission optimization

// Test 1: GZIP Compression and Decompression
test "gzip compression and decompression" {
  let compressor = GzipCompressor::new()
  
  // Create test telemetry data
  let telemetry_data = create_large_telemetry_data(10000) // 10KB of data
  
  // Compress the data
  let compressed_data = Compressor::compress(compressor, telemetry_data)
  
  // Verify compression ratio
  let compression_ratio = compressed_data.length().to_float() / telemetry_data.length().to_float()
  assert_true(compression_ratio < 0.3) // Should achieve at least 70% compression
  
  // Decompress the data
  let decompressed_data = Compressor::decompress(compressor, compressed_data)
  
  // Verify decompressed data matches original
  assert_eq(decompressed_data, telemetry_data)
  
  // Test with smaller data (might not compress well)
  let small_data = "small telemetry data"
  let compressed_small = Compressor::compress(compressor, small_data)
  
  // Small data might actually be larger after compression due to GZIP header
  let small_compression_ratio = compressed_small.length().to_float() / small_data.length().to_float()
  assert_true(small_compression_ratio > 0.5) // Acceptable for small data
  
  let decompressed_small = Compressor::decompress(compressor, compressed_small)
  assert_eq(decompressed_small, small_data)
}

// Test 2: LZ4 Compression and Decompression
test "lz4 compression and decompression" {
  let compressor = Lz4Compressor::new()
  
  // Create test telemetry data
  let telemetry_data = create_large_telemetry_data(50000) // 50KB of data
  
  // Compress the data
  let compressed_data = Compressor::compress(compressor, telemetry_data)
  
  // Verify compression ratio (LZ4 is faster but less compression than GZIP)
  let compression_ratio = compressed_data.length().to_float() / telemetry_data.length().to_float()
  assert_true(compression_ratio < 0.5) // Should achieve at least 50% compression
  
  // Decompress the data
  let decompressed_data = Compressor::decompress(compressor, compressed_data)
  
  // Verify decompressed data matches original
  assert_eq(decompressed_data, telemetry_data)
  
  // Test compression speed
  let start_time = get_current_time_millis()
  
  for i in 0..100 {
    let test_data = create_medium_telemetry_data(5000)
    Compressor::compress(compressor, test_data)
  }
  
  let compression_time = get_current_time_millis() - start_time
  
  // LZ4 should be fast
  assert_true(compression_time < 1000) // Should compress 100 datasets within 1 second
  
  // Test decompression speed
  let start_time = get_current_time_millis()
  
  for i in 0..100 {
    let test_data = create_medium_telemetry_data(5000)
    let compressed = Compressor::compress(compressor, test_data)
    Compressor::decompress(compressor, compressed)
  }
  
  let decompression_time = get_current_time_millis() - start_time
  
  // LZ4 decompression should also be fast
  assert_true(decompression_time < 500) // Should decompress 100 datasets within 0.5 seconds
}

// Test 3: ZSTD Compression and Decompression
test "zstd compression and decompression" {
  let compressor = ZstdCompressor::new()
  
  // Test different compression levels
  let telemetry_data = create_large_telemetry_data(100000) // 100KB of data
  
  // Test with different compression levels
  for level in [1, 3, 5, 9, 15] {
    ZstdCompressor::set_level(compressor, level)
    
    let compressed_data = Compressor::compress(compressor, telemetry_data)
    let compression_ratio = compressed_data.length().to_float() / telemetry_data.length().to_float()
    
    // Higher compression levels should achieve better ratios
    match level {
      1 | 3 => assert_true(compression_ratio < 0.4)
      5 | 9 => assert_true(compression_ratio < 0.3)
      15 => assert_true(compression_ratio < 0.2)
      _ => assert_true(true)
    }
    
    let decompressed_data = Compressor::decompress(compressor, compressed_data)
    assert_eq(decompressed_data, telemetry_data)
  }
  
  // Test streaming compression
  let stream_compressor = ZstdCompressor::new_stream_compressor(5)
  let chunks = create_telemetry_chunks(20000, 10) // 10 chunks of 20KB each
  
  let mut compressed_chunks = []
  for chunk in chunks {
    let compressed_chunk = ZstdCompressor::compress_stream(stream_compressor, chunk)
    compressed_chunks = compressed_chunks + [compressed_chunk]
  }
  
  let final_chunk = ZstdCompressor::finish_stream(stream_compressor)
  compressed_chunks = compressed_chunks + [final_chunk]
  
  // Test streaming decompression
  let stream_decompressor = ZstdCompressor::new_stream_decompressor()
  let mut decompressed_data = ""
  
  for compressed_chunk in compressed_chunks {
    let decompressed_chunk = ZstdCompressor::decompress_stream(stream_decompressor, compressed_chunk)
    decompressed_data = decompressed_data + decompressed_chunk
  }
  
  // Verify decompressed data matches original
  let original_data = concatenate_chunks(chunks)
  assert_eq(decompressed_data, original_data)
}

// Test 4: Adaptive Compression Strategy
test "adaptive compression strategy" {
  let adaptive_compressor = AdaptiveCompressor::new()
  
  // Test with different data types and sizes
  
  // Small text data (should use no compression or light compression)
  let small_text = "small telemetry message"
  let compressed_small = AdaptiveCompressor::compress(adaptive_compressor, small_text)
  let strategy_small = AdaptiveCompressor::get_used_strategy(adaptive_compressor)
  assert_eq(strategy_small, "none") // No compression for very small data
  
  // Medium repetitive data (should use LZ4 for speed)
  let repetitive_data = create_repetitive_data(5000)
  let compressed_repetitive = AdaptiveCompressor::compress(adaptive_compressor, repetitive_data)
  let strategy_repetitive = AdaptiveCompressor::get_used_strategy(adaptive_compressor)
  assert_eq(strategy_repetitive, "lz4") // LZ4 for repetitive data
  
  // Large structured data (should use ZSTD for best compression)
  let structured_data = create_large_telemetry_data(100000)
  let compressed_structured = AdaptiveCompressor::compress(adaptive_compressor, structured_data)
  let strategy_structured = AdaptiveCompressor::get_used_strategy(adaptive_compressor)
  assert_eq(strategy_structured, "zstd") // ZSTD for large structured data
  
  // Test decompression
  let decompressed_small = AdaptiveCompressor::decompress(adaptive_compressor, compressed_small)
  assert_eq(decompressed_small, small_text)
  
  let decompressed_repetitive = AdaptiveCompressor::decompress(adaptive_compressor, compressed_repetitive)
  assert_eq(decompressed_repetitive, repetitive_data)
  
  let decompressed_structured = AdaptiveCompressor::decompress(adaptive_compressor, compressed_structured)
  assert_eq(decompressed_structured, structured_data)
  
  // Test performance metrics
  let metrics = AdaptiveCompressor::get_performance_metrics(adaptive_compressor)
  assert_true(metrics.total_compression_time > 0)
  assert_true(metrics.total_decompression_time > 0)
  assert_true(metrics.average_compression_ratio > 0.0)
  assert_true(metrics.average_compression_ratio < 1.0)
}

// Test 5: Batch Compression Optimization
test "batch compression optimization" {
  let batch_compressor = BatchCompressor::new()
  
  // Create multiple telemetry records
  let mut records = []
  for i in 0..100 {
    let record = create_telemetry_record(i)
    records = records + [record]
  }
  
  // Compress records individually
  let start_time = get_current_time_millis()
  let mut individually_compressed = []
  
  for record in records {
    let compressed = BatchCompressor::compress_record(batch_compressor, record)
    individually_compressed = individually_compressed + [compressed]
  }
  
  let individual_time = get_current_time_millis() - start_time
  
  // Compress records in batch
  let start_time = get_current_time_millis()
  let batch_compressed = BatchCompressor::compress_batch(batch_compressor, records)
  let batch_time = get_current_time_millis() - start_time
  
  // Batch compression should be faster
  assert_true(batch_time < individual_time)
  
  // Calculate compression ratios
  let original_size = calculate_total_size(records)
  let individual_compressed_size = calculate_compressed_size(individually_compressed)
  let batch_compressed_size = batch_compressed.length()
  
  let individual_ratio = individual_compressed_size.to_float() / original_size.to_float()
  let batch_ratio = batch_compressed_size.to_float() / original_size.to_float()
  
  // Batch compression should achieve similar or better compression
  assert_true(batch_ratio <= individual_ratio)
  
  // Test batch decompression
  let start_time = get_current_time_millis()
  let batch_decompressed = BatchCompressor::decompress_batch(batch_compressor, batch_compressed)
  let batch_decompression_time = get_current_time_millis() - start_time
  
  // Verify decompressed data
  assert_eq(batch_decompressed.length(), records.length())
  for i in 0..records.length() {
    assert_eq(batch_decompressed[i], records[i])
  }
  
  // Test individual decompression
  let start_time = get_current_time_millis()
  let mut individually_decompressed = []
  
  for compressed in individually_compressed {
    let decompressed = BatchCompressor::decompress_record(batch_compressor, compressed)
    individually_decompressed = individually_decompressed + [decompressed]
  }
  
  let individual_decompression_time = get_current_time_millis() - start_time
  
  // Batch decompression should be faster
  assert_true(batch_decompression_time < individual_decompression_time)
}

// Test 6: Network Transmission Optimization
test "network transmission optimization" {
  let transmitter = NetworkTransmitter::new()
  
  // Create large telemetry dataset
  let telemetry_data = create_large_telemetry_data(500000) // 500KB of data
  
  // Test without optimization
  transmitter.disable_optimization()
  let start_time = get_current_time_millis()
  let result_no_opt = NetworkTransmitter::transmit(transmitter, telemetry_data)
  let time_no_opt = get_current_time_millis() - start_time
  
  match result_no_opt {
    Success(transmitted_bytes) => assert_eq(transmitted_bytes, telemetry_data.length())
    Error(_) => assert_true(false)
  }
  
  // Test with compression optimization
  transmitter.enable_compression_optimization()
  let start_time = get_current_time_millis()
  let result_compression = NetworkTransmitter::transmit(transmitter, telemetry_data)
  let time_compression = get_current_time_millis() - start_time
  
  match result_compression {
    Success(transmitted_bytes) => {
      // Should transmit fewer bytes due to compression
      assert_true(transmitted_bytes < telemetry_data.length())
    }
    Error(_) => assert_true(false)
  }
  
  // Test with chunking optimization
  transmitter.enable_chunking_optimization(50000) // 50KB chunks
  let start_time = get_current_time_millis()
  let result_chunking = NetworkTransmitter::transmit(transmitter, telemetry_data)
  let time_chunking = get_current_time_millis() - start_time
  
  match result_chunking {
    Success(transmitted_bytes) => {
      // Should transmit approximately the same number of bytes (chunking overhead)
      let overhead_ratio = transmitted_bytes.to_float() / telemetry_data.length().to_float()
      assert_true(overhead_ratio < 1.1) // Less than 10% overhead
    }
    Error(_) => assert_true(false)
  }
  
  // Test with both compression and chunking
  transmitter.enable_compression_optimization()
  transmitter.enable_chunking_optimization(50000)
  let start_time = get_current_time_millis()
  let result_both = NetworkTransmitter::transmit(transmitter, telemetry_data)
  let time_both = get_current_time_millis() - start_time
  
  match result_both {
    Success(transmitted_bytes) => {
      // Should transmit fewer bytes due to compression
      assert_true(transmitted_bytes < telemetry_data.length())
    }
    Error(_) => assert_true(false)
  }
  
  // Verify transmission time improvements
  assert_true(time_compression < time_no_opt) // Compression should reduce transmission time
  assert_true(time_chunking < time_no_opt) // Chunking should reduce transmission time
  assert_true(time_both < time_no_opt) // Both optimizations should be best
}

// Test 7: Delta Encoding for Time Series Data
test "delta encoding for time series data" {
  let delta_encoder = DeltaEncoder::new()
  
  // Create time series data with sequential timestamps
  let time_series_data = create_time_series_data(1000)
  
  // Encode using delta encoding
  let encoded_data = DeltaEncoder::encode(delta_encoder, time_series_data)
  
  // Verify compression ratio
  let compression_ratio = encoded_data.length().to_float() / time_series_data.length().to_float()
  assert_true(compression_ratio < 0.5) // Should achieve at least 50% compression
  
  // Decode the data
  let decoded_data = DeltaEncoder::decode(delta_encoder, encoded_data)
  
  // Verify decoded data matches original
  assert_eq(decoded_data, time_series_data)
  
  // Test with non-sequential timestamps
  let irregular_data = create_irregular_time_series_data(500)
  let encoded_irregular = DeltaEncoder::encode(delta_encoder, irregular_data)
  let decoded_irregular = DeltaEncoder::decode(delta_encoder, encoded_irregular)
  
  assert_eq(decoded_irregular, irregular_data)
  
  // Test delta encoding with floating point values
  let float_series = create_float_time_series_data(1000)
  let encoded_float = DeltaEncoder::encode_float(delta_encoder, float_series)
  let decoded_float = DeltaEncoder::decode_float(delta_encoder, encoded_float)
  
  assert_eq(decoded_float, float_series)
}

// Test 8: Dictionary Compression for Repeated Strings
test "dictionary compression for repeated strings" {
  let dict_compressor = DictionaryCompressor::new()
  
  // Create telemetry data with many repeated strings
  let telemetry_data = create_repetitive_string_data(1000)
  
  // Build dictionary from sample data
  let sample_data = create_repetitive_string_data(100)
  DictionaryCompressor::build_dictionary(dict_compressor, sample_data)
  
  // Compress using dictionary
  let compressed_data = DictionaryCompressor::compress(dict_compressor, telemetry_data)
  
  // Verify compression ratio
  let compression_ratio = compressed_data.length().to_float() / telemetry_data.length().to_float()
  assert_true(compression_ratio < 0.3) // Should achieve good compression for repetitive data
  
  // Decompress using dictionary
  let decompressed_data = DictionaryCompressor::decompress(dict_compressor, compressed_data)
  
  // Verify decompressed data matches original
  assert_eq(decompressed_data, telemetry_data)
  
  // Test with data not in dictionary
  let new_data = create_new_string_data(500)
  let compressed_new = DictionaryCompressor::compress(dict_compressor, new_data)
  let decompressed_new = DictionaryCompressor::decompress(dict_compressor, compressed_new)
  
  assert_eq(decompressed_new, new_data)
  
  // Test dictionary update
  DictionaryCompressor::update_dictionary(dict_compressor, new_data)
  
  let compressed_new_after_update = DictionaryCompressor::compress(dict_compressor, new_data)
  let compression_ratio_after_update = compressed_new_after_update.length().to_float() / new_data.length().to_float()
  
  // Should achieve better compression after dictionary update
  assert_true(compression_ratio_after_update < compression_ratio)
}

// Test 9: Adaptive Transmission Protocol Selection
test "adaptive transmission protocol selection" {
  let protocol_selector = AdaptiveProtocolSelector::new()
  
  // Test with different data sizes and network conditions
  
  // Small data, good network (should use UDP for speed)
  let small_data = create_small_telemetry_data(1024)
  let good_network = NetworkCondition::new(100, 10, 0.01) // 100Mbps, 10ms latency, 1% loss
  
  let protocol_small_good = ProtocolSelector::select_protocol(protocol_selector, small_data.length(), good_network)
  assert_eq(protocol_small_good, "udp")
  
  // Large data, good network (should use TCP for reliability)
  let large_data = create_large_telemetry_data(100000)
  let protocol_large_good = ProtocolSelector::select_protocol(protocol_selector, large_data.length(), good_network)
  assert_eq(protocol_large_good, "tcp")
  
  // Medium data, poor network (should use QUIC for reliability + speed)
  let medium_data = create_medium_telemetry_data(10000)
  let poor_network = NetworkCondition::new(10, 200, 0.05) // 10Mbps, 200ms latency, 5% loss
  
  let protocol_medium_poor = ProtocolSelector::select_protocol(protocol_selector, medium_data.length(), poor_network)
  assert_eq(protocol_medium_poor, "quic")
  
  // Critical data, any network (should use TCP for reliability)
  let critical_data = create_critical_telemetry_data(5000)
  let protocol_critical = ProtocolSelector::select_protocol(protocol_selector, critical_data.length(), poor_network)
  assert_eq(protocol_critical, "tcp")
  
  // Test protocol performance
  let udp_transmitter = ProtocolTransmitter::new("udp")
  let tcp_transmitter = ProtocolTransmitter::new("tcp")
  let quic_transmitter = ProtocolTransmitter::new("quic")
  
  let udp_result = ProtocolTransmitter::transmit(udp_transmitter, small_data)
  let tcp_result = ProtocolTransmitter::transmit(tcp_transmitter, large_data)
  let quic_result = ProtocolTransmitter::transmit(quic_transmitter, medium_data)
  
  match udp_result {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  match tcp_result {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  match quic_result {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
}

// Test 10: End-to-End Compression and Transmission Pipeline
test "end-to-end compression and transmission pipeline" {
  let pipeline = CompressionTransmissionPipeline::new()
  
  // Configure pipeline
  Pipeline::enable_adaptive_compression(pipeline)
  Pipeline::enable_batch_processing(pipeline, 50)
  Pipeline::enable_protocol_selection(pipeline)
  Pipeline::enable_error_recovery(pipeline, 3)
  
  // Create telemetry data
  let mut telemetry_batches = []
  for i in 0..10 {
    let batch = create_telemetry_batch(100, i)
    telemetry_batches = telemetry_batches + [batch]
  }
  
  // Process through pipeline
  let start_time = get_current_time_millis()
  let results = Pipeline::process_batches(pipeline, telemetry_batches)
  let total_time = get_current_time_millis() - start_time
  
  // Verify all batches were processed successfully
  assert_eq(results.length(), 10)
  for result in results {
    match result {
      Success(_) => assert_true(true)
      Error(_) => assert_true(false)
    }
  }
  
  // Calculate pipeline metrics
  let metrics = Pipeline::get_metrics(pipeline)
  
  assert_true(metrics.total_input_size > 0)
  assert_true(metrics.total_output_size > 0)
  assert_true(metrics.total_output_size < metrics.total_input_size) // Compression should reduce size
  assert_true(metrics.total_transmission_time > 0)
  assert_true(metrics.total_compression_time > 0)
  assert_true(metrics.average_compression_ratio > 0.0)
  assert_true(metrics.average_compression_ratio < 1.0)
  
  // Performance assertion
  assert_true(total_time < 10000) // Should complete within 10 seconds
  
  // Test pipeline with error conditions
  Pipeline::simulate_network_errors(pipeline, 0.1) // 10% error rate
  
  let error_results = Pipeline::process_batches(pipeline, telemetry_batches)
  
  // Should handle errors gracefully
  let mut success_count = 0
  let mut error_count = 0
  
  for result in error_results {
    match result {
      Success(_) => success_count = success_count + 1
      Error(_) => error_count = error_count + 1
    }
  }
  
  assert_true(success_count > 0) // Some should succeed
  assert_true(error_count > 0) // Some should fail due to simulated errors
  
  // Verify error recovery
  assert_true(success_count > error_count) // Recovery should ensure more successes than failures
}

// Helper functions for creating test data
fn create_large_telemetry_data(size : Int) -> String {
  let mut result = ""
  let base_data = "{\"timestamp\":1609459200000,\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\",\"span_id\":\"b7ad6b7169203331\",\"service_name\":\"test-service\",\"operation_name\":\"test-operation\",\"duration_ms\":123,\"status\":\"ok\",\"attributes\":{\"key1\":\"value1\",\"key2\":\"value2\",\"key3\":\"value3\"}}"
  
  while result.length() < size {
    result = result + base_data + "\n"
  }
  
  result
}

fn create_medium_telemetry_data(size : Int) -> String {
  let mut result = ""
  let base_data = "{\"service\":\"test\",\"metric\":\"cpu\",\"value\":75.5,\"timestamp\":1609459200000}"
  
  while result.length() < size {
    result = result + base_data + "\n"
  }
  
  result
}

fn create_small_telemetry_data(size : Int) -> String {
  let mut result = ""
  let base_data = "{\"log\":\"info\",\"message\":\"test log\"}"
  
  while result.length() < size {
    result = result + base_data + "\n"
  }
  
  result
}

fn create_repetitive_data(size : Int) -> String {
  let mut result = ""
  let repetitive_pattern = "repetitive_telemetry_data_pattern_12345"
  
  while result.length() < size {
    result = result + repetitive_pattern
  }
  
  result
}

fn create_telemetry_chunks(chunk_size : Int, chunk_count : Int) -> Array[String] {
  let mut chunks = []
  
  for i in 0..chunk_count {
    let chunk = create_medium_telemetry_data(chunk_size)
    chunks = chunks + [chunk]
  }
  
  chunks
}

fn concatenate_chunks(chunks : Array[String]) -> String {
  let mut result = ""
  
  for chunk in chunks {
    result = result + chunk
  }
  
  result
}

fn create_telemetry_record(index : Int) -> String {
  "{\"record_id\":" + index.to_string() + ",\"timestamp\":" + (1609459200000 + index).to_string() + ",\"data\":\"test_data_" + index.to_string() + "\"}"
}

fn calculate_total_size(records : Array[String]) -> Int {
  let mut total = 0
  
  for record in records {
    total = total + record.length()
  }
  
  total
}

fn calculate_compressed_size(compressed_records : Array[Array[Byte]]) -> Int {
  let mut total = 0
  
  for compressed in compressed_records {
    total = total + compressed.length()
  }
  
  total
}

fn create_time_series_data(count : Int) -> String {
  let mut result = ""
  let base_timestamp = 1609459200000
  
  for i in 0..count {
    let timestamp = base_timestamp + i * 1000 // 1 second intervals
    let value = 50.0 + (i % 20).to_float() * 2.5 // Varying values
    result = result + "{\"timestamp\":" + timestamp.to_string() + ",\"value\":" + value.to_string() + "}"
    if i < count - 1 {
      result = result + ","
    }
  }
  
  "[" + result + "]"
}

fn create_irregular_time_series_data(count : Int) -> String {
  let mut result = ""
  let base_timestamp = 1609459200000
  
  for i in 0..count {
    let timestamp = base_timestamp + i * 1000 + Random::int(0, 5000) // Irregular intervals
    let value = 50.0 + Random::float(0.0, 100.0) // Random values
    result = result + "{\"timestamp\":" + timestamp.to_string() + ",\"value\":" + value.to_string() + "}"
    if i < count - 1 {
      result = result + ","
    }
  }
  
  "[" + result + "]"
}

fn create_float_time_series_data(count : Int) -> String {
  let mut result = ""
  
  for i in 0..count {
    let value = Random::float(0.0, 100.0)
    result = result + value.to_string()
    if i < count - 1 {
      result = result + ","
    }
  }
  
  "[" + result + "]"
}

fn create_repetitive_string_data(count : Int) -> String {
  let mut result = ""
  let common_strings = ["service.name", "test-service", "operation.name", "test-operation", "status", "ok", "error", "timeout", "database", "http"]
  
  for i in 0..count {
    let string_index = i % common_strings.length()
    result = result + common_strings[string_index]
    if i < count - 1 {
      result = result + ","
    }
  }
  
  result
}

fn create_new_string_data(count : Int) -> String {
  let mut result = ""
  
  for i in 0..count {
    let unique_string = "unique_string_" + i.to_string()
    result = result + unique_string
    if i < count - 1 {
      result = result + ","
    }
  }
  
  result
}

fn create_telemetry_batch(batch_size : Int, batch_id : Int) -> Array[String] {
  let mut batch = []
  
  for i in 0..batch_size {
    let record = create_telemetry_record(batch_id * batch_size + i)
    batch = batch + [record]
  }
  
  batch
}

fn create_critical_telemetry_data(size : Int) -> String {
  let mut result = ""
  let base_data = "{\"priority\":\"critical\",\"alert\":\"system_failure\",\"severity\":\"fatal\",\"timestamp\":" + get_current_time_millis().to_string() + "}"
  
  while result.length() < size {
    result = result + base_data + "\n"
  }
  
  result
}

// Helper function to get current time in milliseconds
fn get_current_time_millis() -> Int {
  // Mock implementation - in real code would use system time
  1609459200000 + Random::int(0, 86400000) // Random time within a day
}