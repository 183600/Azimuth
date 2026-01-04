// Azimuth Telemetry Data Compression Tests
// This file contains test cases for telemetry data compression functionality

// Test 1: Basic Telemetry Data Compression
test "basic telemetry data compression" {
  // Define compression algorithms
  enum CompressionAlgorithm {
    Gzip
    Deflate
    Lz4
    Snappy
  }
  
  // Define compressed data structure
  type CompressedData = {
    algorithm: CompressionAlgorithm,
    original_size: Int,
    compressed_size: Int,
    data: Array[Byte]
  }
  
  // Define telemetry span data
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // Create sample telemetry span
  let span = {
    trace_id: "trace-12345678901234567890123456789012",
    span_id: "span-1234567890123456",
    parent_span_id: Some("span-1234567890123455"),
    operation_name: "http.request.get.user.profile",
    start_time: 1640995200000,
    end_time: 1640995200250,
    status: "ok",
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/v1/users/12345/profile"),
      ("http.status_code", "200"),
      ("user.id", "12345"),
      ("service.name", "user-service"),
      ("service.version", "1.2.3"),
      ("deployment.environment", "production")
    ]
  }
  
  // Simulate compression function
  let compress_data = fn(data: String, algorithm: CompressionAlgorithm) {
    // Simulate different compression ratios for different algorithms
    let compression_ratio = match algorithm {
      CompressionAlgorithm::Gzip => 0.35
      CompressionAlgorithm::Deflate => 0.40
      CompressionAlgorithm::Lz4 => 0.55
      CompressionAlgorithm::Snappy => 0.60
    }
    
    let original_size = data.length()
    let compressed_size = (original_size.to_float() * compression_ratio).to_int()
    
    // Create mock compressed data (just for testing)
    let mut compressed_data = []
    for i in 0..compressed_size {
      compressed_data = compressed_data.push((i % 256).to_byte())
    }
    
    {
      algorithm,
      original_size,
      compressed_size,
      data: compressed_data
    }
  }
  
  // Serialize span to string (simplified)
  let serialize_span = fn(span: TelemetrySpan) {
    let mut result = span.trace_id + "|" + span.span_id + "|"
    
    match span.parent_span_id {
      Some(parent_id) => result = result + parent_id
      None => result = result + ""
    }
    
    result = result + "|" + span.operation_name + "|" + 
             span.start_time.to_string() + "|" + 
             span.end_time.to_string() + "|" + 
             span.status
    
    for (key, value) in span.attributes {
      result = result + "|" + key + "=" + value
    }
    
    result
  }
  
  // Serialize the span
  let serialized_span = serialize_span(span)
  assert_true(serialized_span.length() > 0)
  
  // Test compression with different algorithms
  let gzip_compressed = compress_data(serialized_span, CompressionAlgorithm::Gzip)
  let deflate_compressed = compress_data(serialized_span, CompressionAlgorithm::Deflate)
  let lz4_compressed = compress_data(serialized_span, CompressionAlgorithm::Lz4)
  let snappy_compressed = compress_data(serialized_span, CompressionAlgorithm::Snappy)
  
  // Verify compression results
  assert_eq(gzip_compressed.original_size, serialized_span.length())
  assert_eq(deflate_compressed.original_size, serialized_span.length())
  assert_eq(lz4_compressed.original_size, serialized_span.length())
  assert_eq(snappy_compressed.original_size, serialized_span.length())
  
  // Verify compression reduced size
  assert_true(gzip_compressed.compressed_size < gzip_compressed.original_size)
  assert_true(deflate_compressed.compressed_size < deflate_compressed.original_size)
  assert_true(lz4_compressed.compressed_size < lz4_compressed.original_size)
  assert_true(snappy_compressed.compressed_size < snappy_compressed.original_size)
  
  // Verify compression algorithms are correctly identified
  match gzip_compressed.algorithm {
    CompressionAlgorithm::Gzip => assert_true(true)
    _ => assert_true(false)
  }
  
  match deflate_compressed.algorithm {
    CompressionAlgorithm::Deflate => assert_true(true)
    _ => assert_true(false)
  }
  
  match lz4_compressed.algorithm {
    CompressionAlgorithm::Lz4 => assert_true(true)
    _ => assert_true(false)
  }
  
  match snappy_compressed.algorithm {
    CompressionAlgorithm::Snappy => assert_true(true)
    _ => assert_true(false)
  }
  
  // Verify compression efficiency (gzip should be most efficient)
  assert_true(gzip_compressed.compressed_size <= deflate_compressed.compressed_size)
  assert_true(deflate_compressed.compressed_size <= lz4_compressed.compressed_size)
  assert_true(lz4_compressed.compressed_size <= snappy_compressed.compressed_size)
  
  // Calculate compression ratios
  let gzip_ratio = gzip_compressed.compressed_size.to_float() / gzip_compressed.original_size.to_float()
  let deflate_ratio = deflate_compressed.compressed_size.to_float() / deflate_compressed.original_size.to_float()
  let lz4_ratio = lz4_compressed.compressed_size.to_float() / lz4_compressed.original_size.to_float()
  let snappy_ratio = snappy_compressed.compressed_size.to_float() / snappy_compressed.original_size.to_float()
  
  // Verify compression ratios are within expected ranges
  assert_true(gzip_ratio >= 0.3 and gzip_ratio <= 0.4)
  assert_true(deflate_ratio >= 0.35 and deflate_ratio <= 0.45)
  assert_true(lz4_ratio >= 0.5 and lz4_ratio <= 0.6)
  assert_true(snappy_ratio >= 0.55 and snappy_ratio <= 0.65)
}

// Test 2: Batch Telemetry Data Compression
test "batch telemetry data compression" {
  // Define batch compression configuration
  type BatchCompressionConfig = {
    algorithm: String,  // "gzip", "deflate", "lz4", "snappy"
    batch_size: Int,    // Number of spans in each batch
    compression_level: Int  // 1-9, higher means more compression but slower
  }
  
  // Define batch compression result
  type BatchCompressionResult = {
    original_count: Int,
    original_size: Int,
    compressed_size: Int,
    compression_ratio: Float,
    compression_time_ms: Int,
    batches: Array[Array[Byte]]
  }
  
  // Create sample telemetry spans
  let create_test_span = fn(id: Int) {
    {
      trace_id: "trace-12345678901234567890123456789012",
      span_id: "span-" + id.to_string(),
      parent_span_id: if id > 1 { Some("span-" + (id - 1).to_string()) } else { None },
      operation_name: "operation." + id.to_string(),
      start_time: 1640995200000 + (id * 100),
      end_time: 1640995200000 + (id * 100) + 50,
      status: if id % 10 == 0 { "error" } else { "ok" },
      attributes: [
        ("service.name", "test-service"),
        ("service.version", "1.0.0"),
        ("span.index", id.to_string()),
        ("data.size", (1000 + id).to_string())
      ]
    }
  }
  
  // Create test spans
  let spans = []
  for i in 1..=100 {
    spans = spans.push(create_test_span(i))
  }
  assert_eq(spans.length(), 100)
  
  // Serialize spans function
  let serialize_spans = fn(spans: Array[T]) {
    let mut result = []
    for span in spans {
      let serialized = span.trace_id + "|" + span.span_id + "|" + 
                      span.operation_name + "|" + 
                      span.start_time.to_string() + "|" + 
                      span.end_time.to_string() + "|" + span.status
      
      for (key, value) in span.attributes {
        serialized = serialized + "|" + key + "=" + value
      }
      
      result = result.push(serialized)
    }
    result
  }
  
  // Serialize all spans
  let serialized_spans = serialize_spans(spans)
  let total_original_size = serialized_spans.reduce(fn(acc, span) { acc + span.length() }, 0)
  
  // Batch compression function
  let batch_compress = fn(spans: Array[String], config: BatchCompressionConfig) {
    let start_time = 1640995200000  // Mock current time
    
    // Split spans into batches
    let mut batches = []
    let mut current_batch = []
    
    for span in spans {
      current_batch = current_batch.push(span)
      
      if current_batch.length() >= config.batch_size {
        batches = batches.push(current_batch)
        current_batch = []
      }
    }
    
    // Add the last batch if it's not empty
    if current_batch.length() > 0 {
      batches = batches.push(current_batch)
    }
    
    // Compress each batch
    let mut compressed_batches = []
    let mut total_compressed_size = 0
    
    for batch in batches {
      let batch_data = batch.join("\n")
      
      // Simulate compression based on algorithm and level
      let base_ratio = match config.algorithm {
        "gzip" => 0.35
        "deflate" => 0.40
        "lz4" => 0.55
        "snappy" => 0.60
        _ => 0.50
      }
      
      // Adjust ratio based on compression level (1-9)
      let level_adjustment = (config.compression_level - 5) * 0.02
      let compression_ratio = (base_ratio - level_adjustment).max(0.2).min(0.8)
      
      let compressed_size = (batch_data.length().to_float() * compression_ratio).to_int()
      total_compressed_size = total_compressed_size + compressed_size
      
      // Create mock compressed data
      let mut compressed_data = []
      for i in 0..compressed_size {
        compressed_data = compressed_data.push((i % 256).to_byte())
      }
      
      compressed_batches = compressed_batches.push(compressed_data)
    }
    
    let end_time = start_time + 250  // Mock compression time
    
    {
      original_count: spans.length(),
      original_size: total_original_size,
      compressed_size: total_compressed_size,
      compression_ratio: total_compressed_size.to_float() / total_original_size.to_float(),
      compression_time_ms: end_time - start_time,
      batches: compressed_batches
    }
  }
  
  // Test different compression configurations
  let gzip_config = { algorithm: "gzip", batch_size: 10, compression_level: 6 }
  let deflate_config = { algorithm: "deflate", batch_size: 20, compression_level: 6 }
  let lz4_config = { algorithm: "lz4", batch_size: 25, compression_level: 3 }
  let snappy_config = { algorithm: "snappy", batch_size: 50, compression_level: 3 }
  
  // Compress with different configurations
  let gzip_result = batch_compress(serialized_spans, gzip_config)
  let deflate_result = batch_compress(serialized_spans, deflate_config)
  let lz4_result = batch_compress(serialized_spans, lz4_config)
  let snappy_result = batch_compress(serialized_spans, snappy_config)
  
  // Verify compression results
  assert_eq(gzip_result.original_count, 100)
  assert_eq(deflate_result.original_count, 100)
  assert_eq(lz4_result.original_count, 100)
  assert_eq(snappy_result.original_count, 100)
  
  assert_eq(gzip_result.original_size, total_original_size)
  assert_eq(deflate_result.original_size, total_original_size)
  assert_eq(lz4_result.original_size, total_original_size)
  assert_eq(snappy_result.original_size, total_original_size)
  
  // Verify compression reduced size
  assert_true(gzip_result.compressed_size < gzip_result.original_size)
  assert_true(deflate_result.compressed_size < deflate_result.original_size)
  assert_true(lz4_result.compressed_size < lz4_result.original_size)
  assert_true(snappy_result.compressed_size < snappy_result.original_size)
  
  // Verify batch counts
  assert_eq(gzip_result.batches.length(), 10)  // 100 spans / 10 batch_size
  assert_eq(deflate_result.batches.length(), 5)  // 100 spans / 20 batch_size
  assert_eq(lz4_result.batches.length(), 4)  // 100 spans / 25 batch_size (last batch smaller)
  assert_eq(snappy_result.batches.length(), 2)  // 100 spans / 50 batch_size
  
  // Verify compression ratios
  assert_true(gzip_result.compression_ratio >= 0.3 and gzip_result.compression_ratio <= 0.4)
  assert_true(deflate_result.compression_ratio >= 0.35 and deflate_result.compression_ratio <= 0.45)
  assert_true(lz4_result.compression_ratio >= 0.5 and lz4_result.compression_ratio <= 0.6)
  assert_true(snappy_result.compression_ratio >= 0.55 and snappy_result.compression_ratio <= 0.65)
  
  // Verify compression time is reasonable
  assert_true(gzip_result.compression_time_ms > 0 and gzip_result.compression_time_ms <= 1000)
  assert_true(deflate_result.compression_time_ms > 0 and deflate_result.compression_time_ms <= 1000)
  assert_true(lz4_result.compression_time_ms > 0 and lz4_result.compression_time_ms <= 1000)
  assert_true(snappy_result.compression_time_ms > 0 and snappy_result.compression_time_ms <= 1000)
  
  // Compare compression efficiency
  assert_true(gzip_result.compression_ratio <= deflate_result.compression_ratio)
  assert_true(deflate_result.compression_ratio <= lz4_result.compression_ratio)
  assert_true(lz4_result.compression_ratio <= snappy_result.compression_ratio)
  
  // Compare compression speed (snappy should be fastest)
  assert_true(snappy_result.compression_time_ms <= lz4_result.compression_time_ms)
  assert_true(lz4_result.compression_time_ms <= deflate_result.compression_time_ms)
  assert_true(deflate_result.compression_time_ms <= gzip_result.compression_time_ms)
}

// Test 3: Adaptive Compression Strategy
test "adaptive compression strategy" {
  // Define data characteristics
  type DataCharacteristics = {
    entropy: Float,        // 0.0-1.0, higher means more random/less compressible
    size: Int,             // Size in bytes
    repetition: Float      // 0.0-1.0, higher means more repetitive patterns
  }
  
  // Define compression recommendation
  type CompressionRecommendation = {
    algorithm: String,
    level: Int,
    expected_ratio: Float,
    reason: String
  }
  
  // Analyze data characteristics
  let analyze_data = fn(data: String) {
    // Calculate entropy (simplified)
    let char_counts = {}
    let mut total_chars = 0
    
    for char in data.to_char_array() {
      let count = match char_counts.get(char) {
        Some(c) => c + 1
        None => 1
      }
      char_counts = char_counts.set(char, count)
      total_chars = total_chars + 1
    }
    
    let mut entropy = 0.0
    for (_, count) in char_counts {
      let probability = count.to_float() / total_chars.to_float()
      entropy = entropy - (probability * probability.log2())
    }
    
    // Normalize entropy to 0-1 range (max entropy for ASCII is ~7 bits)
    entropy = entropy / 7.0
    
    // Calculate repetition (simplified - count consecutive repeated chars)
    let mut repetitions = 0
    let mut total_pairs = 0
    
    for i in 0..(data.length() - 1) {
      if data[i] == data[i + 1] {
        repetitions = repetitions + 1
      }
      total_pairs = total_pairs + 1
    }
    
    let repetition = repetitions.to_float() / total_pairs.to_float()
    
    {
      entropy,
      size: data.length(),
      repetition
    }
  }
  
  // Recommend compression algorithm based on data characteristics
  let recommend_compression = fn(characteristics: DataCharacteristics) {
    if characteristics.size < 100 {
      // Small data, use fast compression
      {
        algorithm: "snappy",
        level: 3,
        expected_ratio: 0.8,
        reason: "Small data size, prioritize speed over ratio"
      }
    } else if characteristics.entropy > 0.8 {
      // High entropy (random data), use fast compression
      {
        algorithm: "lz4",
        level: 1,
        expected_ratio: 0.7,
        reason: "High entropy data, compression will be limited"
      }
    } else if characteristics.repetition > 0.3 {
      // High repetition, use best compression
      {
        algorithm: "gzip",
        level: 9,
        expected_ratio: 0.3,
        reason: "High repetition data, maximize compression ratio"
      }
    } else if characteristics.entropy < 0.5 {
      // Low entropy, use good compression
      {
        algorithm: "deflate",
        level: 6,
        expected_ratio: 0.4,
        reason: "Low entropy data, good compression expected"
      }
    } else {
      // Balanced approach
      {
        algorithm: "gzip",
        level: 6,
        expected_ratio: 0.5,
        reason: "Balanced characteristics, use standard compression"
      }
    }
  }
  
  // Test with different data patterns
  
  // 1. Highly repetitive data
  let repetitive_data = "pattern123456789".repeat(100)
  let repetitive_chars = analyze_data(repetitive_data)
  let repetitive_recommendation = recommend_compression(repetitive_chars)
  
  assert_eq(repetitive_recommendation.algorithm, "gzip")
  assert_eq(repetitive_recommendation.level, 9)
  assert_eq(repetitive_recommendation.expected_ratio, 0.3)
  assert_eq(repetitive_recommendation.reason, "High repetition data, maximize compression ratio")
  
  // 2. Random-like data (high entropy)
  let random_like_data = "xYz123!@#abcDEF456ghiJKL789mnoPQR012stuVWX345"
  let random_chars = analyze_data(random_like_data)
  let random_recommendation = recommend_compression(random_chars)
  
  assert_eq(random_recommendation.algorithm, "lz4")
  assert_eq(random_recommendation.level, 1)
  assert_eq(random_recommendation.expected_ratio, 0.7)
  assert_eq(random_recommendation.reason, "High entropy data, compression will be limited")
  
  // 3. Small data
  let small_data = "small"
  let small_chars = analyze_data(small_data)
  let small_recommendation = recommend_compression(small_chars)
  
  assert_eq(small_recommendation.algorithm, "snappy")
  assert_eq(small_recommendation.level, 3)
  assert_eq(small_recommendation.expected_ratio, 0.8)
  assert_eq(small_recommendation.reason, "Small data size, prioritize speed over ratio")
  
  // 4. Low entropy data
  let low_entropy_data = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
  let low_entropy_chars = analyze_data(low_entropy_data)
  let low_entropy_recommendation = recommend_compression(low_entropy_chars)
  
  assert_eq(low_entropy_recommendation.algorithm, "deflate")
  assert_eq(low_entropy_recommendation.level, 6)
  assert_eq(low_entropy_recommendation.expected_ratio, 0.4)
  assert_eq(low_entropy_recommendation.reason, "Low entropy data, good compression expected")
  
  // 5. Balanced data
  let balanced_data = "The quick brown fox jumps over the lazy dog. " +
                     "The quick brown fox jumps over the lazy dog. " +
                     "Pack my box with five dozen liquor jugs."
  let balanced_chars = analyze_data(balanced_data)
  let balanced_recommendation = recommend_compression(balanced_chars)
  
  assert_eq(balanced_recommendation.algorithm, "gzip")
  assert_eq(balanced_recommendation.level, 6)
  assert_eq(balanced_recommendation.expected_ratio, 0.5)
  assert_eq(balanced_recommendation.reason, "Balanced characteristics, use standard compression")
  
  // Verify characteristics calculations
  assert_true(repetitive_chars.repetition > 0.3)
  assert_true(random_chars.entropy > 0.8)
  assert_true(small_chars.size < 100)
  assert_true(low_entropy_chars.entropy < 0.5)
  
  // Verify all recommendations have valid values
  assert_true(repetitive_recommendation.level >= 1 and repetitive_recommendation.level <= 9)
  assert_true(random_recommendation.level >= 1 and random_recommendation.level <= 9)
  assert_true(small_recommendation.level >= 1 and small_recommendation.level <= 9)
  assert_true(low_entropy_recommendation.level >= 1 and low_entropy_recommendation.level <= 9)
  assert_true(balanced_recommendation.level >= 1 and balanced_recommendation.level <= 9)
  
  assert_true(repetitive_recommendation.expected_ratio > 0.0 and repetitive_recommendation.expected_ratio < 1.0)
  assert_true(random_recommendation.expected_ratio > 0.0 and random_recommendation.expected_ratio < 1.0)
  assert_true(small_recommendation.expected_ratio > 0.0 and small_recommendation.expected_ratio < 1.0)
  assert_true(low_entropy_recommendation.expected_ratio > 0.0 and low_entropy_recommendation.expected_ratio < 1.0)
  assert_true(balanced_recommendation.expected_ratio > 0.0 and balanced_recommendation.expected_ratio < 1.0)
}