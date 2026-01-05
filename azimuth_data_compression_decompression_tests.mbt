// Azimuth Telemetry System - Data Compression and Decompression Tests
// This file contains test cases for data compression and decompression functionality

// Test 1: GZIP Compression
test "gzip compression and decompression" {
  // Test string compression
  let original_string = "This is a test string for compression. It contains repeated words like test, compression, and string. " * 10
  
  let compressed_data = Compression::gzip_compress(original_string.to_bytes())
  assert_true(compressed_data.length() < original_string.to_bytes().length())
  
  let decompressed_data = Compression::gzip_decompress(compressed_data)
  let decompressed_string = decompressed_data.to_string()
  assert_eq(decompressed_string, original_string)
  
  // Test empty string
  let empty_string = ""
  let compressed_empty = Compression::gzip_compress(empty_string.to_bytes())
  let decompressed_empty = Compression::gzip_decompress(compressed_empty)
  assert_eq(decompressed_empty.to_string(), empty_string)
  
  // Test large data
  let large_data = "A" * 10000
  let compressed_large = Compression::gzip_compress(large_data.to_bytes())
  let decompressed_large = Compression::gzip_decompress(compressed_large)
  assert_eq(decompressed_large.to_string(), large_data)
}

// Test 2: Deflate Compression
test "deflate compression and decompression" {
  // Test string compression
  let original_string = "Deflate compression test with repeated patterns: abcdefghijklmnopqrstuvwxyz" * 20
  
  let compressed_data = Compression::deflate_compress(original_string.to_bytes())
  assert_true(compressed_data.length() < original_string.to_bytes().length())
  
  let decompressed_data = Compression::deflate_decompress(compressed_data)
  let decompressed_string = decompressed_data.to_string()
  assert_eq(decompressed_string, original_string)
  
  // Test binary data
  let binary_data = [0x00, 0x01, 0x02, 0x03, 0xFF, 0xFE, 0xFD, 0xFC] * 100
  let compressed_binary = Compression::deflate_compress(binary_data)
  let decompressed_binary = Compression::deflate_decompress(compressed_binary)
  assert_eq(decompressed_binary, binary_data)
}

// Test 3: LZ4 Compression
test "lz4 compression and decompression" {
  // Test string compression
  let original_string = "LZ4 is a fast compression algorithm. This string tests LZ4 compression capabilities with repeated data patterns." * 15
  
  let compressed_data = Compression::lz4_compress(original_string.to_bytes())
  assert_true(compressed_data.length() < original_string.to_bytes().length())
  
  let decompressed_data = Compression::lz4_decompress(compressed_data)
  let decompressed_string = decompressed_data.to_string()
  assert_eq(decompressed_string, original_string)
  
  // Test JSON data compression
  let json_data = "{\"name\":\"test\",\"value\":42,\"array\":[1,2,3,4,5],\"nested\":{\"key\":\"value\",\"number\":123}}" * 50
  let compressed_json = Compression::lz4_compress(json_data.to_bytes())
  let decompressed_json = Compression::lz4_decompress(compressed_json)
  assert_eq(decompressed_json.to_string(), json_data)
}

// Test 4: Compression Level Testing
test "compression level testing" {
  let test_data = "Compression level test data with high redundancy. " * 100
  let original_bytes = test_data.to_bytes()
  
  // Test different compression levels for GZIP
  let gzip_level_1 = Compression::gzip_compress_with_level(original_bytes, 1)
  let gzip_level_6 = Compression::gzip_compress_with_level(original_bytes, 6)
  let gzip_level_9 = Compression::gzip_compress_with_level(original_bytes, 9)
  
  // Higher compression levels should produce smaller output
  assert_true(gzip_level_9.length() <= gzip_level_6.length())
  assert_true(gzip_level_6.length() <= gzip_level_1.length())
  
  // All should decompress to the original
  assert_eq(Compression::gzip_decompress(gzip_level_1).to_string(), test_data)
  assert_eq(Compression::gzip_decompress(gzip_level_6).to_string(), test_data)
  assert_eq(Compression::gzip_decompress(gzip_level_9).to_string(), test_data)
  
  // Test different compression levels for Deflate
  let deflate_level_1 = Compression::deflate_compress_with_level(original_bytes, 1)
  let deflate_level_6 = Compression::deflate_compress_with_level(original_bytes, 6)
  let deflate_level_9 = Compression::deflate_compress_with_level(original_bytes, 9)
  
  assert_true(deflate_level_9.length() <= deflate_level_6.length())
  assert_true(deflate_level_6.length() <= deflate_level_1.length())
  
  assert_eq(Compression::deflate_decompress(deflate_level_1).to_string(), test_data)
  assert_eq(Compression::deflate_decompress(deflate_level_6).to_string(), test_data)
  assert_eq(Compression::deflate_decompress(deflate_level_9).to_string(), test_data)
}

// Test 5: Telemetry Data Compression
test "telemetry data compression" {
  // Create test telemetry data
  let telemetry_data = TelemetryData::new()
  
  // Add multiple metrics
  for i in 0..=99 {
    let metric_name = "metric_" + i.to_string()
    let metric_value = i.to_float() * 1.5
    TelemetryData::add_metric(telemetry_data, metric_name, metric_value)
  }
  
  // Add multiple events
  for i in 0..=49 {
    let event_name = "event_" + i.to_string()
    let event_attrs = Attributes::new()
    Attributes::set(event_attrs, "event_id", StringValue("evt_" + i.to_string()))
    Attributes::set(event_attrs, "timestamp", IntValue(1609459200L + i.to_int64()))
    TelemetryData::add_event(telemetry_data, event_name, event_attrs)
  }
  
  // Serialize to JSON
  let serialized_data = TelemetryData::to_json(telemetry_data)
  
  // Compress the serialized data
  let compressed_telemetry = Compression::gzip_compress(serialized_data.to_bytes())
  
  // Verify compression reduces size
  assert_true(compressed_telemetry.length() < serialized_data.length())
  
  // Decompress and verify integrity
  let decompressed_data = Compression::gzip_decompress(compressed_telemetry)
  let restored_telemetry = TelemetryData::from_json(decompressed_data.to_string())
  
  // Verify the restored data matches the original
  assert_eq(TelemetryData::metrics_count(restored_telemetry), TelemetryData::metrics_count(telemetry_data))
  assert_eq(TelemetryData::events_count(restored_telemetry), TelemetryData::events_count(telemetry_data))
}

// Test 6: Batch Compression
test "batch compression operations" {
  // Create multiple telemetry batches
  let batch_size = 10
  let batches = []
  
  for i in 0..=batch_size - 1 {
    let batch = TelemetryBatch::new("batch_" + i.to_string())
    
    // Add metrics to each batch
    for j in 0..=9 {
      let metric_name = "metric_" + j.to_string()
      let metric_value = (i * 10 + j).to_float()
      TelemetryBatch::add_metric(batch, metric_name, metric_value)
    }
    
    batches.push(batch)
  }
  
  // Compress all batches
  let compressed_batches = []
  for batch in batches {
    let serialized = TelemetryBatch::to_json(batch)
    let compressed = Compression::lz4_compress(serialized.to_bytes())
    compressed_batches.push(compressed)
  }
  
  // Decompress and verify
  for i in 0..=batch_size - 1 {
    let decompressed = Compression::lz4_decompress(compressed_batches[i])
    let restored_batch = TelemetryBatch::from_json(decompressed.to_string())
    
    assert_eq(TelemetryBatch::name(restored_batch), "batch_" + i.to_string())
    assert_eq(TelemetryBatch::metrics_count(restored_batch), 10)
  }
}

// Test 7: Streaming Compression
test "streaming compression" {
  // Create a stream compressor
  let stream_compressor = StreamCompressor::new(Gzip, 6)
  
  // Compress data in chunks
  let chunk1 = "This is the first chunk of data. " * 20
  let chunk2 = "This is the second chunk of data. " * 20
  let chunk3 = "This is the third and final chunk. " * 20
  
  let compressed_chunk1 = StreamCompressor::compress(stream_compressor, chunk1.to_bytes())
  let compressed_chunk2 = StreamCompressor::compress(stream_compressor, chunk2.to_bytes())
  let compressed_chunk3 = StreamCompressor::compress(stream_compressor, chunk3.to_bytes())
  
  // Finalize compression
  let final_compressed = StreamCompressor::finalize(stream_compressor)
  
  // Combine all compressed chunks
  let full_compressed = compressed_chunk1 + compressed_chunk2 + compressed_chunk3 + final_compressed
  
  // Create a stream decompressor
  let stream_decompressor = StreamDecompressor::new(Gzip)
  
  // Decompress data
  let decompressed_chunk1 = StreamDecompressor::decompress(stream_decompressor, compressed_chunk1)
  let decompressed_chunk2 = StreamDecompressor::decompress(stream_decompressor, compressed_chunk2)
  let decompressed_chunk3 = StreamDecompressor::decompress(stream_decompressor, compressed_chunk3)
  let final_decompressed = StreamDecompressor::finalize(stream_decompressor)
  
  // Combine all decompressed chunks
  let full_decompressed = decompressed_chunk1 + decompressed_chunk2 + decompressed_chunk3 + final_decompressed
  
  // Verify the result
  let original_data = chunk1 + chunk2 + chunk3
  assert_eq(full_decompressed.to_string(), original_data)
}

// Test 8: Compression Error Handling
test "compression error handling" {
  // Test decompressing invalid data
  let invalid_data = [0x00, 0x01, 0x02, 0x03]
  
  // Should handle errors gracefully
  let result = Compression::gzip_decompress(invalid_data)
  match result {
    Ok(_) => assert_true(false) // Should not succeed
    Err(_) => assert_true(true)  // Should fail gracefully
  }
  
  // Test decompressing truncated data
  let valid_data = "Valid compression test data" * 10
  let compressed = Compression::gzip_compress(valid_data.to_bytes())
  
  // Truncate the compressed data
  let truncated = compressed.slice(0, compressed.length() / 2)
  let truncated_result = Compression::gzip_decompress(truncated)
  match truncated_result {
    Ok(_) => assert_true(false) // Should not succeed
    Err(_) => assert_true(true)  // Should fail gracefully
  }
  
  // Test decompressing data with wrong format
  let gzip_compressed = Compression::gzip_compress(valid_data.to_bytes())
  let deflate_result = Compression::deflate_decompress(gzip_compressed)
  match deflate_result {
    Ok(_) => assert_true(false) // Should not succeed
    Err(_) => assert_true(true)  // Should fail gracefully
  }
}