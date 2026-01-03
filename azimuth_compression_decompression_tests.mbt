// Azimuth Data Compression and Decompression Tests
// This file contains test cases for data compression and decompression functionality

// Test 1: Gzip Compression
test "gzip compression" {
  let original_data = "This is a test string for compression. ".repeat(100)
  let compressor = GzipCompressor::new()
  
  // Test compression
  let compressed_data = Compressor::compress(compressor, original_data.to_bytes())
  assert_true(compressed_data.length() < original_data.length())
  
  // Test decompression
  let decompressor = GzipDecompressor::new()
  let decompressed_data = Decompressor::decompress(decompressor, compressed_data)
  assert_eq(decompressed_data.to_string(), original_data)
}

// Test 2: Deflate Compression
test "deflate compression" {
  let original_data = "This is another test string for deflate compression. ".repeat(100)
  let compressor = DeflateCompressor::new()
  
  // Test compression
  let compressed_data = Compressor::compress(compressor, original_data.to_bytes())
  assert_true(compressed_data.length() < original_data.length())
  
  // Test decompression
  let decompressor = DeflateDecompressor::new()
  let decompressed_data = Decompressor::decompress(decompressor, compressed_data)
  assert_eq(decompressed_data.to_string(), original_data)
}

// Test 3: LZ4 Compression
test "lz4 compression" {
  let original_data = "LZ4 is a fast compression algorithm. ".repeat(100)
  let compressor = LZ4Compressor::new()
  
  // Test compression
  let compressed_data = Compressor::compress(compressor, original_data.to_bytes())
  assert_true(compressed_data.length() < original_data.length())
  
  // Test decompression
  let decompressor = LZ4Decompressor::new()
  let decompressed_data = Decompressor::decompress(decompressor, compressed_data)
  assert_eq(decompressed_data.to_string(), original_data)
}

// Test 4: Compression with Different Data Types
test "compression with different data types" {
  let compressor = GzipCompressor::new()
  let decompressor = GzipDecompressor::new()
  
  // Test string data
  let string_data = "String data for compression testing. "
  let compressed_string = Compressor::compress(compressor, string_data.to_bytes())
  let decompressed_string = Decompressor::decompress(decompressor, compressed_string)
  assert_eq(decompressed_string.to_string(), string_data)
  
  // Test JSON data
  let json_data = "{\"key1\":\"value1\",\"key2\":123,\"key3\":true,\"key4\":[1,2,3]}"
  let compressed_json = Compressor::compress(compressor, json_data.to_bytes())
  let decompressed_json = Decompressor::decompress(decompressor, compressed_json)
  assert_eq(decompressed_json.to_string(), json_data)
  
  // Test binary data
  let binary_data = [0x01, 0x02, 0x03, 0x04, 0x05].repeat(100)
  let compressed_binary = Compressor::compress(compressor, binary_data)
  let decompressed_binary = Decompressor::decompress(decompressor, compressed_binary)
  assert_eq(decompressed_binary, binary_data)
}

// Test 5: Compression Level Testing
test "compression level testing" {
  let original_data = "Testing different compression levels. ".repeat(200)
  
  // Test different compression levels
  let level_1 = GzipCompressor::with_level(1)  // Fastest compression
  let level_6 = GzipCompressor::with_level(6)  // Default compression
  let level_9 = GzipCompressor::with_level(9)  // Best compression
  
  let compressed_1 = Compressor::compress(level_1, original_data.to_bytes())
  let compressed_6 = Compressor::compress(level_6, original_data.to_bytes())
  let compressed_9 = Compressor::compress(level_9, original_data.to_bytes())
  
  // Higher compression level should produce smaller output
  assert_true(compressed_9.length() <= compressed_6.length())
  assert_true(compressed_6.length() <= compressed_1.length())
  
  // All should decompress to the same original data
  let decompressor = GzipDecompressor::new()
  let decompressed_1 = Decompressor::decompress(decompressor, compressed_1)
  let decompressed_6 = Decompressor::decompress(decompressor, compressed_6)
  let decompressed_9 = Decompressor::decompress(decompressor, compressed_9)
  
  assert_eq(decompressed_1.to_string(), original_data)
  assert_eq(decompressed_6.to_string(), original_data)
  assert_eq(decompressed_9.to_string(), original_data)
}

// Test 6: Streaming Compression
test "streaming compression" {
  let compressor = GzipCompressor::new()
  let decompressor = GzipDecompressor::new()
  
  // Create a streaming compressor
  let stream_compressor = StreamingCompressor::new(compressor)
  
  // Compress data in chunks
  let chunk1 = "First chunk of data. "
  let chunk2 = "Second chunk of data. "
  let chunk3 = "Third chunk of data. "
  
  let compressed_chunk1 = StreamingCompressor::compress(stream_compressor, chunk1.to_bytes())
  let compressed_chunk2 = StreamingCompressor::compress(stream_compressor, chunk2.to_bytes())
  let compressed_chunk3 = StreamingCompressor::compress(stream_compressor, chunk3.to_bytes())
  
  // Finalize compression
  let final_compressed = StreamingCompressor::finalize(stream_compressor)
  
  // Combine all compressed chunks
  let total_compressed = compressed_chunk1 + compressed_chunk2 + compressed_chunk3 + final_compressed
  
  // Decompress and verify
  let stream_decompressor = StreamingDecompressor::new(decompressor)
  let decompressed_chunk1 = StreamingDecompressor::decompress(stream_decompressor, compressed_chunk1)
  let decompressed_chunk2 = StreamingDecompressor::decompress(stream_decompressor, compressed_chunk2)
  let decompressed_chunk3 = StreamingDecompressor::decompress(stream_decompressor, compressed_chunk3)
  let final_decompressed = StreamingDecompressor::finalize(stream_decompressor, final_compressed)
  
  assert_eq(decompressed_chunk1.to_string(), chunk1)
  assert_eq(decompressed_chunk2.to_string(), chunk2)
  assert_eq(decompressed_chunk3.to_string(), chunk3)
  assert_eq(final_decompressed.length(), 0) // No additional data expected
}

// Test 7: Telemetry Data Compression
test "telemetry data compression" {
  // Create telemetry data
  let telemetry_data = TelemetryData::new(
    "service-123",
    "operation-456",
    [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("/api/users")),
      ("http.status_code", IntValue(200)),
      ("user.id", StringValue("user-789")),
      ("request.size", IntValue(1024)),
      ("response.size", IntValue(2048))
    ],
    1234567890L
  )
  
  // Serialize telemetry data
  let serialized_data = TelemetryData::serialize(telemetry_data)
  
  // Compress serialized data
  let compressor = GzipCompressor::new()
  let compressed_data = Compressor::compress(compressor, serialized_data)
  
  // Verify compression reduced size
  assert_true(compressed_data.length() < serialized_data.length())
  
  // Decompress and verify
  let decompressor = GzipDecompressor::new()
  let decompressed_data = Decompressor::decompress(decompressor, compressed_data)
  
  // Deserialize and verify
  let deserialized_data = TelemetryData::deserialize(decompressed_data)
  assert_eq(TelemetryData::service_name(deserialized_data), "service-123")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation-456")
}

// Test 8: Batch Compression
test "batch compression" {
  let batch_data = [
    TelemetryData::new("service-1", "op-1", [], 1234567890L),
    TelemetryData::new("service-2", "op-2", [], 1234567891L),
    TelemetryData::new("service-3", "op-3", [], 1234567892L),
    TelemetryData::new("service-4", "op-4", [], 1234567893L),
    TelemetryData::new("service-5", "op-5", [], 1234567894L)
  ]
  
  // Compress each item individually
  let compressor = GzipCompressor::new()
  let individually_compressed = []
  
  for data in batch_data {
    let serialized = TelemetryData::serialize(data)
    let compressed = Compressor::compress(compressor, serialized)
    individually_compressed.push(compressed)
  }
  
  // Compress as a batch
  let batch = BatchTelemetryData::new(batch_data)
  let serialized_batch = BatchTelemetryData::serialize(batch)
  let compressed_batch = Compressor::compress(compressor, serialized_batch)
  
  // Batch compression should be more efficient
  let individual_total = individually_compressed.fold(0, { acc, data => acc + data.length() })
  assert_true(compressed_batch.length() < individual_total)
  
  // Verify batch decompression
  let decompressor = GzipDecompressor::new()
  let decompressed_batch = Decompressor::decompress(decompressor, compressed_batch)
  let deserialized_batch = BatchTelemetryData::deserialize(decompressed_batch)
  
  assert_eq(BatchTelemetryData::count(deserialized_batch), 5)
}

// Test 9: Compression Error Handling
test "compression error handling" {
  let compressor = GzipCompressor::new()
  let decompressor = GzipDecompressor::new()
  
  // Test compression with invalid data
  let invalid_data = []
  let compressed_data = Compressor::compress(compressor, invalid_data)
  assert_true(compressed_data.length() > 0) // Empty data should compress fine
  
  // Test decompression with corrupted data
  let corrupted_data = [0x1f, 0x8b, 0x08, 0x00, 0xff, 0xff, 0xff, 0xff] // Invalid gzip header
  let result = Decompressor::decompress(decompressor, corrupted_data)
  
  match result {
    Ok(_) => assert_true(false)
    Error(error) => {
      assert_eq(error.code, DecompressionError)
      assert_true(error.message.contains("invalid"))
    }
  }
  
  // Test decompression with truncated data
  let valid_data = "Valid data".to_bytes()
  let valid_compressed = Compressor::compress(compressor, valid_data)
  let truncated_data = valid_compressed.slice(0, valid_compressed.length() / 2)
  
  let result2 = Decompressor::decompress(decompressor, truncated_data)
  match result2 {
    Ok(_) => assert_true(false)
    Error(error2) => {
      assert_eq(error2.code, DecompressionError)
    }
  }
}

// Test 10: Adaptive Compression Strategy
test "adaptive compression strategy" {
  let strategy = AdaptiveCompressionStrategy::new()
  
  // Test with small data (should use no compression or fast compression)
  let small_data = "Small data"
  let small_compressed = AdaptiveCompressionStrategy::compress(strategy, small_data.to_bytes())
  let small_ratio = small_compressed.length() as Float / small_data.length() as Float
  
  // Test with large repetitive data (should use best compression)
  let large_repetitive_data = " repetitive ".repeat(1000)
  let large_compressed = AdaptiveCompressionStrategy::compress(strategy, large_repetitive_data.to_bytes())
  let large_ratio = large_compressed.length() as Float / large_repetitive_data.length() as Float
  
  // Large repetitive data should have better compression ratio
  assert_true(large_ratio < small_ratio)
  
  // Verify both can be decompressed correctly
  let small_decompressed = AdaptiveCompressionStrategy::decompress(strategy, small_compressed)
  let large_decompressed = AdaptiveCompressionStrategy::decompress(strategy, large_compressed)
  
  assert_eq(small_decompressed.to_string(), small_data)
  assert_eq(large_decompressed.to_string(), large_repetitive_data)
}