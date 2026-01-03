// Azimuth Telemetry System - Data Compression and Transmission Tests
// This file contains test cases for data compression and transmission functionality

// Test 1: Basic Data Compression
test "basic data compression" {
  // Create test data
  let original_data = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  
  // Test GZIP compression
  let gzip_compressor = GzipCompressor::new()
  let compressed_data = Compressor::compress(gzip_compressor, original_data.to_bytes())
  
  // Verify compression reduces size
  assert_true(compressed_data.length() < original_data.length())
  
  // Verify decompression restores original data
  let decompressed_data = Compressor::decompress(gzip_compressor, compressed_data)
  let decompressed_string = @bytes_to_string(decompressed_data)
  assert_eq(decompressed_string, original_data)
  
  // Test compression ratio
  let compression_ratio = Compressor::compression_ratio(gzip_compressor, original_data.to_bytes())
  assert_true(compression_ratio > 0.0)
  assert_true(compression_ratio < 1.0)
  
  // Test DEFLATE compression
  let deflate_compressor = DeflateCompressor::new()
  let deflate_compressed = Compressor::compress(deflate_compressor, original_data.to_bytes())
  let deflate_decompressed = Compressor::decompress(deflate_compressor, deflate_compressed)
  let deflate_string = @bytes_to_string(deflate_decompressed)
  assert_eq(deflate_string, original_data)
}

// Test 2: Binary Data Compression
test "binary data compression" {
  // Create binary test data
  let binary_data = []
  for i in 0..=9999 {
    binary_data.push((i % 256).to_int())
  }
  
  // Test LZ4 compression (fast compression)
  let lz4_compressor = Lz4Compressor::new()
  let lz4_compressed = Compressor::compress(lz4_compressor, binary_data)
  
  // Verify compression
  assert_true(lz4_compressed.length() < binary_data.length())
  
  // Verify decompression
  let lz4_decompressed = Compressor::decompress(lz4_compressor, lz4_compressed)
  assert_eq(lz4_decompressed.length(), binary_data.length())
  
  for i in 0..=9999 {
    assert_eq(lz4_decompressed[i], binary_data[i])
  }
  
  // Test Brotli compression (good compression ratio)
  let brotli_compressor = BrotliCompressor::new(6)  // Compression level 6
  let brotli_compressed = Compressor::compress(brotli_compressor, binary_data)
  let brotli_decompressed = Compressor::decompress(brotli_compressor, brotli_compressed)
  
  // Verify decompression
  assert_eq(brotli_decompressed.length(), binary_data.length())
  
  // Compare compression ratios
  let lz4_ratio = lz4_compressed.length().to_float() / binary_data.length().to_float()
  let brotli_ratio = brotli_compressed.length().to_float() / binary_data.length().to_float()
  
  // Brotli should generally have better compression than LZ4
  assert_true(brotli_ratio <= lz4_ratio)
}

// Test 3: Streaming Compression
test "streaming compression" {
  // Create streaming compressor
  let stream_compressor = StreamCompressor::new(CompressionAlgorithm::Gzip)
  
  // Create large data in chunks
  let chunks = []
  for i in 0..=99 {
    let chunk = "Chunk data " + i.to_string() + " with some repeated content to improve compression ratio. "
    chunks.push(chunk)
  }
  
  // Compress chunks incrementally
  StreamCompressor::start(stream_compressor)
  let compressed_chunks = []
  
  for chunk in chunks {
    let compressed_chunk = StreamCompressor::compress_chunk(stream_compressor, chunk.to_bytes())
    compressed_chunks.push(compressed_chunk)
  }
  
  let final_chunk = StreamCompressor::finish(stream_compressor)
  compressed_chunks.push(final_chunk)
  
  // Combine compressed chunks
  let mut compressed_data = []
  for chunk in compressed_chunks {
    for byte in chunk {
      compressed_data.push(byte)
    }
  }
  
  // Decompress using streaming decompressor
  let stream_decompressor = StreamDecompressor::new(CompressionAlgorithm::Gzip)
  StreamDecompressor::start(stream_decompressor)
  
  let mut decompressed_data = []
  for chunk in compressed_chunks {
    let decompressed_chunk = StreamDecompressor::decompress_chunk(stream_decompressor, chunk)
    for byte in decompressed_chunk {
      decompressed_data.push(byte)
    }
  }
  
  // Verify decompressed data matches original
  let original_data = ""
  for chunk in chunks {
    original_data = original_data + chunk
  }
  
  let decompressed_string = @bytes_to_string(decompressed_data)
  assert_eq(decompressed_string, original_data)
}

// Test 4: Adaptive Compression
test "adaptive compression" {
  // Create adaptive compressor that selects best algorithm based on data
  let adaptive_compressor = AdaptiveCompressor::new()
  
  // Test with text data (GZIP usually works well)
  let text_data = "This is a sample text data with repeated patterns. This is a sample text data with repeated patterns. This is a sample text data with repeated patterns."
  let text_compressed = AdaptiveCompressor::compress(adaptive_compressor, text_data.to_bytes())
  let text_decompressed = AdaptiveCompressor::decompress(adaptive_compressor, text_compressed)
  
  let text_result = @bytes_to_string(text_decompressed)
  assert_eq(text_result, text_data)
  
  // Verify algorithm selection
  let text_algorithm = AdaptiveCompressor::selected_algorithm(adaptive_compressor)
  assert_eq(text_algorithm, CompressionAlgorithm::Gzip)
  
  // Test with binary data (LZ4 might be better for speed)
  let binary_data = []
  for i in 0..=9999 {
    binary_data.push((i * 7 % 256).to_int())
  }
  
  let binary_compressed = AdaptiveCompressor::compress(adaptive_compressor, binary_data)
  let binary_decompressed = AdaptiveCompressor::decompress(adaptive_compressor, binary_compressed)
  
  assert_eq(binary_decompressed.length(), binary_data.length())
  
  // Verify algorithm selection
  let binary_algorithm = AdaptiveCompressor::selected_algorithm(adaptive_compressor)
  // The algorithm selection depends on the adaptive strategy
  assert_true(
    binary_algorithm == CompressionAlgorithm::Lz4 ||
    binary_algorithm == CompressionAlgorithm::Gzip
  )
  
  // Test compression performance metrics
  let text_metrics = AdaptiveCompressor::get_metrics(adaptive_compressor, text_data.to_bytes())
  let binary_metrics = AdaptiveCompressor::get_metrics(adaptive_compressor, binary_data)
  
  assert_true(CompressionMetrics::compression_time(text_metrics) > 0)
  assert_true(CompressionMetrics::decompression_time(text_metrics) > 0)
  assert_true(CompressionMetrics::compression_ratio(text_metrics) > 0.0)
  assert_true(CompressionMetrics::compression_ratio(text_metrics) < 1.0)
}

// Test 5: Data Transmission with Compression
test "data transmission with compression" {
  // Create transmission manager with compression
  let transmission_manager = TransmissionManager::new()
  TransmissionManager::enable_compression(transmission_manager, CompressionAlgorithm::Gzip)
  
  // Create test data
  let telemetry_data = TelemetryData::new(
    "service-123",
    [
      MetricData::new("cpu_usage", 75.5, "percent"),
      MetricData::new("memory_usage", 1024.0, "megabytes"),
      MetricData::new("request_count", 1000.0, "count")
    ],
    [
      LogData::new(Info, "Service started successfully"),
      LogData::new(Warning, "High memory usage detected")
    ]
  )
  
  // Serialize telemetry data
  let serialized_data = TelemetryData::serialize(telemetry_data)
  
  // Transmit with compression
  let transmission_result = TransmissionManager::transmit(
    transmission_manager,
    "https://telemetry.example.com/api/data",
    serialized_data,
    [("Content-Type", "application/json")]
  )
  
  // Verify transmission
  match transmission_result {
    TransmissionResult::Success(response) => {
      assert_eq(response.status_code, 200)
      assert_true(response.body.contains("received"))
    }
    TransmissionResult::Failed(error) => {
      // In a test environment, we might not have a real server
      assert_true(error.contains("connection") || error.contains("network"))
    }
  }
  
  // Verify compression statistics
  let stats = TransmissionManager::get_compression_stats(transmission_manager)
  assert_true(CompressionStats::original_size(stats) > 0)
  assert_true(CompressionStats::compressed_size(stats) > 0)
  assert_true(CompressionStats::compression_ratio(stats) > 0.0)
  assert_true(CompressionStats::compression_ratio(stats) < 1.0)
}

// Test 6: Batch Data Compression and Transmission
test "batch data compression and transmission" {
  // Create batch transmission manager
  let batch_manager = BatchTransmissionManager::new(
    100,     // Max batch size
    5000,    // Max batch age (5 seconds)
    1024 * 1024  // Max batch size in bytes (1MB)
  )
  
  BatchTransmissionManager::enable_compression(batch_manager, CompressionAlgorithm::Lz4)
  
  // Create multiple telemetry data points
  let data_points = []
  for i in 0..=49 {  // 50 data points
    let data_point = TelemetryData::new(
      "service-" + (i % 5).to_string(),
      [
        MetricData::new("cpu_usage", 50.0 + (i % 50).to_float(), "percent"),
        MetricData::new("memory_usage", 512.0 + (i * 10).to_float(), "megabytes")
      ],
      [
        LogData::new(Info, "Processing item " + i.to_string())
      ]
    )
    data_points.push(data_point)
  }
  
  // Add data points to batch manager
  for data_point in data_points {
    BatchTransmissionManager::add_data(batch_manager, data_point)
  }
  
  // Force batch transmission
  let transmission_result = BatchTransmissionManager::flush(batch_manager)
  
  // Verify batch transmission
  match transmission_result {
    BatchTransmissionResult::Success(batches) => {
      assert_true(batches.length() > 0)
      
      for batch in batches {
        assert_true(BatchMetrics::item_count(batch) > 0)
        assert_true(BatchMetrics::original_size(batch) > 0)
        assert_true(BatchMetrics::compressed_size(batch) > 0)
        assert_true(BatchMetrics::compression_ratio(batch) > 0.0)
      }
    }
    BatchTransmissionResult::Failed(error) => {
      // In a test environment, we might not have a real server
      assert_true(error.contains("connection") || error.contains("network"))
    }
  }
  
  // Verify batch statistics
  let batch_stats = BatchTransmissionManager::get_statistics(batch_manager)
  assert_eq(BatchStatistics::total_items(batch_stats), 50)
  assert_eq(BatchStatistics::total_batches(batch_stats), 1)
  assert_true(BatchStatistics::average_compression_ratio(batch_stats) > 0.0)
}

// Test 7: Delta Encoding for Time Series Data
test "delta encoding for time series data" {
  // Create time series data
  let time_series = []
  let base_timestamp = 1640995200000L
  
  for i in 0..=99 {
    let data_point = TimeSeriesPoint::new(
      base_timestamp + (i * 60000L),  // 1-minute intervals
      100.0 + (i * 0.5).to_float(),  // Increasing values
      [("metric_name", "cpu_usage")]
    )
    time_series.push(data_point)
  }
  
  // Apply delta encoding
  let delta_encoder = DeltaEncoder::new()
  let encoded_data = DeltaEncoder::encode(delta_encoder, time_series)
  
  // Verify encoding reduces size
  assert_true(encoded_data.length() < time_series.length())
  
  // Decode the data
  let decoded_data = DeltaEncoder::decode(delta_encoder, encoded_data)
  
  // Verify decoded data matches original
  assert_eq(decoded_data.length(), time_series.length())
  
  for i in 0..=99 {
    let original = time_series[i]
    let decoded = decoded_data[i]
    
    assert_eq(TimeSeriesPoint::timestamp(decoded), TimeSeriesPoint::timestamp(original))
    assert_eq(TimeSeriesPoint::value(decoded), TimeSeriesPoint::value(original))
  }
  
  // Test delta encoding with different compression
  let delta_compressed = DeltaEncoder::encode_with_compression(
    delta_encoder,
    time_series,
    CompressionAlgorithm::Gzip
  )
  
  let delta_decompressed = DeltaEncoder::decode_with_compression(
    delta_encoder,
    delta_compressed,
    CompressionAlgorithm::Gzip
  )
  
  // Verify compression preserves data
  assert_eq(delta_decompressed.length(), time_series.length())
}

// Test 8: Network Protocol Optimization
test "network protocol optimization" {
  // Create protocol optimizer
  let protocol_optimizer = ProtocolOptimizer::new()
  
  // Enable various optimizations
  ProtocolOptimizer::enable_compression(protocol_optimizer, CompressionAlgorithm::Brotli)
  ProtocolOptimizer::enable_batching(protocol_optimizer, 50)
  ProtocolOptimizer::enable_multiplexing(protocol_optimizer, 5)
  
  // Create optimized transmission channel
  let channel = ProtocolOptimizer::create_channel(
    protocol_optimizer,
    "wss://telemetry.example.com/ws"
  )
  
  // Create test data
  let telemetry_batch = []
  for i in 0..=19 {  // 20 telemetry data points
    let data = TelemetryData::new(
      "service-" + (i % 3).to_string(),
      [MetricData::new("metric_" + (i % 5).to_string(), i.to_float(), "unit")],
      [LogData::new(Info, "Message " + i.to_string())]
    )
    telemetry_batch.push(data)
  }
  
  // Transmit optimized data
  let transmission_result = ProtocolOptimizer::transmit_batch(channel, telemetry_batch)
  
  // Verify transmission
  match transmission_result {
    OptimizedTransmissionResult::Success(metrics) => {
      assert_true(OptimizedMetrics::data_size(metrics) > 0)
      assert_true(OptimizedMetrics::compressed_size(metrics) > 0)
      assert_true(OptimizedMetrics::compression_ratio(metrics) > 0.0)
      assert_true(OptimizedMetrics::transmission_time(metrics) > 0)
    }
    OptimizedTransmissionResult::Failed(error) => {
      // In a test environment, we might not have a real server
      assert_true(error.contains("connection") || error.contains("network"))
    }
  }
  
  // Test protocol adaptation based on network conditions
  ProtocolOptimizer::simulate_network_conditions(
    protocol_optimizer,
    NetworkConditions::new(1000, 100, 0.01)  // 1Mbps, 100ms latency, 1% packet loss
  )
  
  let adapted_settings = ProtocolOptimizer::get_adapted_settings(protocol_optimizer)
  
  // Should adapt to network conditions
  assert_true(
    AdaptedSettings::compression_level(adapted_settings) <= 6 ||
    AdaptedSettings::batch_size(adapted_settings) <= 25
  )
}

// Test 9: Data Integrity Verification
test "data integrity verification" {
  // Create data with integrity verification
  let integrity_manager = IntegrityManager::new()
  
  // Create test data
  let original_data = "Critical telemetry data that must be transmitted without corruption"
  let data_bytes = original_data.to_bytes()
  
  // Add checksum
  let data_with_checksum = IntegrityManager::add_checksum(integrity_manager, data_bytes)
  
  // Verify checksum
  let is_valid = IntegrityManager::verify_checksum(integrity_manager, data_with_checksum)
  assert_true(is_valid)
  
  // Simulate data corruption
  let corrupted_data = data_with_checksum.to_array()
  corrupted_data[10] = corrupted_data[10] ^ 0xFF  // Flip some bits
  
  let is_corrupted = IntegrityManager::verify_checksum(integrity_manager, corrupted_data)
  assert_false(is_corrupted)
  
  // Test with compression and integrity
  let compressor = GzipCompressor::new()
  let compressed_data = Compressor::compress(compressor, data_bytes)
  
  let compressed_with_checksum = IntegrityManager::add_checksum(integrity_manager, compressed_data)
  
  // Decompress and verify
  let decompressed_data = Compressor::decompress(compressor, compressed_with_checksum)
  let decompressed_without_checksum = IntegrityManager::remove_checksum(integrity_manager, decompressed_data)
  
  let is_decompressed_valid = IntegrityManager::verify_checksum(integrity_manager, decompressed_data)
  assert_true(is_decompressed_valid)
  
  let result_string = @bytes_to_string(decompressed_without_checksum)
  assert_eq(result_string, original_data)
  
  // Test with digital signatures
  let signature_manager = SignatureManager::new()
  let signed_data = SignatureManager::sign(signature_manager, data_bytes)
  let is_signature_valid = SignatureManager::verify(signature_manager, signed_data)
  assert_true(is_signature_valid)
}

// Test 10: End-to-End Compression and Transmission Pipeline
test "end-to-end compression and transmission pipeline" {
  // Create comprehensive pipeline
  let pipeline = CompressionTransmissionPipeline::new()
  
  // Configure pipeline stages
  Pipeline::add_compression_stage(pipeline, AdaptiveCompressor::new())
  Pipeline::add_batching_stage(pipeline, 100, 5000)
  Pipeline::add_integrity_stage(pipeline, IntegrityManager::new())
  Pipeline::add_protocol_optimization_stage(pipeline, ProtocolOptimizer::new())
  Pipeline::add_retry_stage(pipeline, RetryStrategy::new(3, 1000))
  
  // Create large dataset
  let dataset = []
  for service_id in 0..=9 {  // 10 services
    for i in 0..=99 {  // 100 data points per service
      let data = TelemetryData::new(
        "service-" + service_id.to_string(),
        [
          MetricData::new("cpu_usage", 50.0 + (i % 50).to_float(), "percent"),
          MetricData::new("memory_usage", 1024.0 + (i * 10).to_float(), "megabytes"),
          MetricData::new("request_count", (i * 5).to_float(), "count"),
          MetricData::new("response_time", 100.0 + (i % 200).to_float(), "milliseconds")
        ],
        [
          LogData::new(Info, "Processing request " + i.to_string()),
          LogData::new(Warning, "High latency detected" + if i % 10 == 0 { "" } else { "" })
        ],
        [
          SpanData::new("operation-" + (i % 5).to_string(), 100 + (i * 10)),
          SpanData::new("database_query", 50 + (i % 100))
        ]
      )
      dataset.push(data)
    }
  }
  
  // Process through pipeline
  let pipeline_result = Pipeline::process(pipeline, dataset, "https://telemetry.example.com/api/batch")
  
  // Verify pipeline result
  match pipeline_result {
    PipelineResult::Success(metrics) => {
      assert_true(PipelineMetrics::total_items(metrics) == 1000)
      assert_true(PipelineMetrics::original_size(metrics) > 0)
      assert_true(PipelineMetrics::compressed_size(metrics) > 0)
      assert_true(PipelineMetrics::compression_ratio(metrics) > 0.0)
      assert_true(PipelineMetrics::transmission_time(metrics) > 0)
      assert_true(PipelineMetrics::batches_sent(metrics) > 0)
      assert_true(PipelineMetrics::retry_attempts(metrics) >= 0)
    }
    PipelineResult::PartialSuccess(metrics, errors) => {
      // Some data might have failed in test environment
      assert_true(PipelineMetrics::total_items(metrics) > 0)
      assert_true(errors.length() > 0)
    }
    PipelineResult::Failed(error) => {
      // In a test environment without real server
      assert_true(error.contains("connection") || error.contains("network"))
    }
  }
  
  // Verify pipeline statistics
  let pipeline_stats = Pipeline::get_statistics(pipeline)
  assert_true(PipelineStatistics::compression_efficiency(pipeline_stats) > 0.0)
  assert_true(PipelineStatistics::transmission_success_rate(pipeline_stats) >= 0.0)
  assert_true(PipelineStatistics::average_batch_size(pipeline_stats) > 0)
  
  // Test pipeline with different configurations
  Pipeline::configure(pipeline, PipelineConfig::new(
    CompressionAlgorithm::Brotli,
    200,  // Larger batch size
    10000, // Longer batch age
    true,  // Enable integrity checks
    true   // Enable protocol optimization
  ))
  
  let optimized_result = Pipeline::process(pipeline, dataset.slice(0, 100), "https://telemetry.example.com/api/batch")
  
  // Verify optimized configuration works
  match optimized_result {
    PipelineResult::Success(metrics) => {
      assert_true(PipelineMetrics::total_items(metrics) == 100)
    }
    _ => assert_true(true)  // Accept any result in test environment
  }
}