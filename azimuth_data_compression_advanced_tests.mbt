// Azimuth Data Compression Advanced Tests
// This file contains comprehensive test cases for various data compression algorithms and techniques

// Test 1: Delta Encoding Compression
test "delta encoding compression" {
  let compressor = DeltaCompressor::new()
  let telemetry_data = []
  
  // Generate telemetry data with patterns suitable for delta encoding
  let base_timestamp = 1609459200000L
  let base_value = 100.0
  
  for i in 0..=10000 {
    let timestamp = base_timestamp + (i * 60000L) // 1-minute intervals
    let value = base_value + (0.1 * i.to_float()) // Linear increase
    let data_point = TelemetryData::new("cpu.usage", value, timestamp, Attributes::new())
    telemetry_data.push(data_point)
  }
  
  // Compress data using delta encoding
  let compressed_data = DeltaCompressor::compress(compressor, telemetry_data)
  let original_size = calculate_data_size(telemetry_data)
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // Delta encoding should achieve good compression for sequential data
  assert_true(compression_ratio < 0.5) // At least 50% compression
  
  // Decompress data
  let decompressed_data = DeltaCompressor::decompress(compressor, compressed_data)
  
  // Verify decompression accuracy
  assert_eq(decompressed_data.length(), telemetry_data.length())
  
  for i in 0..=telemetry_data.length() - 1 {
    let original = telemetry_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.metric_name, decompressed.metric_name)
    assert_eq(original.value, decompressed.value)
    assert_eq(original.timestamp, decompressed.timestamp)
  }
  
  // Test with non-sequential data (should be less effective)
  let random_data = generate_random_telemetry_data(1000)
  let compressed_random = DeltaCompressor::compress(compressor, random_data)
  let random_original_size = calculate_data_size(random_data)
  let random_compressed_size = compressed_random.length()
  let random_compression_ratio = random_compressed_size.to_float() / random_original_size.to_float()
  
  // Delta encoding should be less effective for random data
  assert_true(random_compression_ratio > compression_ratio)
  
  // Cleanup
  DeltaCompressor::cleanup(compressor)
}

// Test 2: Run-Length Encoding (RLE) Compression
test "run-length encoding compression" {
  let compressor = RLECompressor::new()
  
  // Test with data containing runs
  let run_data = []
  
  // Create data with runs of identical values
  for i in 0..=100 {
    let run_length = 5 + (i % 20) // Variable run lengths
    let value = 50.0 + (i % 10).to_float()
    
    for j in 0..=run_length-1 {
      let timestamp = 1609459200000L + (i * 1000L) + (j * 1000L)
      let data_point = TelemetryData::new("test.metric", value, timestamp, Attributes::new())
      run_data.push(data_point)
    }
  }
  
  // Compress using RLE
  let compressed_data = RLECompressor::compress(compressor, run_data)
  let original_size = calculate_data_size(run_data)
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // RLE should achieve excellent compression for data with runs
  assert_true(compression_ratio < 0.3) // At least 70% compression
  
  // Decompress data
  let decompressed_data = RLECompressor::decompress(compressor, compressed_data)
  
  // Verify decompression accuracy
  assert_eq(decompressed_data.length(), run_data.length())
  
  for i in 0..=run_data.length() - 1 {
    let original = run_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.metric_name, decompressed.metric_name)
    assert_eq(original.value, decompressed.value)
    assert_eq(original.timestamp, decompressed.timestamp)
  }
  
  // Test with data without runs (should be less effective)
  let non_run_data = generate_random_telemetry_data(1000)
  let compressed_non_run = RLECompressor::compress(compressor, non_run_data)
  let non_run_original_size = calculate_data_size(non_run_data)
  let non_run_compressed_size = compressed_non_run.length()
  let non_run_compression_ratio = non_run_compressed_size.to_float() / non_run_original_size.to_float()
  
  // RLE should be less effective for data without runs
  assert_true(non_run_compression_ratio > compression_ratio)
  
  // Cleanup
  RLECompressor::cleanup(compressor)
}

// Test 3: Huffman Coding Compression
test "huffman coding compression" {
  let compressor = HuffmanCompressor::new()
  
  // Generate data with skewed value distribution (good for Huffman)
  let skewed_data = []
  
  for i in 0..=10000 {
    let value = 
      if i % 10 < 7 { 100.0 } // 70% of values are 100.0
      else if i % 10 < 9 { 200.0 } // 20% of values are 200.0
      else { 300.0 } // 10% of values are 300.0
    
    let timestamp = 1609459200000L + (i * 1000L)
    let data_point = TelemetryData::new("skewed.metric", value, timestamp, Attributes::new())
    skewed_data.push(data_point)
  }
  
  // Compress using Huffman coding
  let compressed_data = HuffmanCompressor::compress(compressor, skewed_data)
  let original_size = calculate_data_size(skewed_data)
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // Huffman should achieve good compression for skewed data
  assert_true(compression_ratio < 0.6) // At least 40% compression
  
  // Decompress data
  let decompressed_data = HuffmanCompressor::decompress(compressor, compressed_data)
  
  // Verify decompression accuracy
  assert_eq(decompressed_data.length(), skewed_data.length())
  
  for i in 0..=skewed_data.length() - 1 {
    let original = skewed_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.metric_name, decompressed.metric_name)
    assert_eq(original.value, decompressed.value)
    assert_eq(original.timestamp, decompressed.timestamp)
  }
  
  // Test with uniform data (should be less effective)
  let uniform_data = []
  for i in 0..=1000 {
    let value = (i % 256).to_float() // Uniform distribution
    let timestamp = 1609459200000L + (i * 1000L)
    let data_point = TelemetryData::new("uniform.metric", value, timestamp, Attributes::new())
    uniform_data.push(data_point)
  }
  
  let compressed_uniform = HuffmanCompressor::compress(compressor, uniform_data)
  let uniform_original_size = calculate_data_size(uniform_data)
  let uniform_compressed_size = compressed_uniform.length()
  let uniform_compression_ratio = uniform_compressed_size.to_float() / uniform_original_size.to_float()
  
  // Huffman should be less effective for uniform data
  assert_true(uniform_compression_ratio > compression_ratio)
  
  // Cleanup
  HuffmanCompressor::cleanup(compressor)
}

// Test 4: Lempel-Ziv (LZ77) Compression
test "lempel-ziv compression" {
  let compressor = LZ77Compressor::new(4096, 32768) // 4KB window, 32KB buffer
  
  // Generate data with repeated patterns (good for LZ77)
  let pattern_data = []
  let base_pattern = "repeating_pattern_with_some_length"
  
  for i in 0..=1000 {
    let timestamp = 1609459200000L + (i * 1000L)
    let value = i.to_float()
    
    // Create attributes with repeated patterns
    let attrs = Attributes::new()
    Attributes::set(attrs, "pattern", StringValue(base_pattern))
    Attributes::set(attrs, "iteration", StringValue("iteration_" + (i % 100).to_string()))
    
    let data_point = TelemetryData::new("pattern.metric", value, timestamp, attrs)
    pattern_data.push(data_point)
  }
  
  // Compress using LZ77
  let compressed_data = LZ77Compressor::compress(compressor, pattern_data)
  let original_size = calculate_data_size(pattern_data)
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // LZ77 should achieve good compression for data with repeated patterns
  assert_true(compression_ratio < 0.4) // At least 60% compression
  
  // Decompress data
  let decompressed_data = LZ77Compressor::decompress(compressor, compressed_data)
  
  // Verify decompression accuracy
  assert_eq(decompressed_data.length(), pattern_data.length())
  
  for i in 0..=pattern_data.length() - 1 {
    let original = pattern_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.metric_name, decompressed.metric_name)
    assert_eq(original.value, decompressed.value)
    assert_eq(original.timestamp, decompressed.timestamp)
    
    // Verify attributes
    let original_pattern = Attributes::get(original.attributes, "pattern")
    let decompressed_pattern = Attributes::get(decompressed.attributes, "pattern")
    
    match (original_pattern, decompressed_pattern) {
      (Some(StringValue(orig)), Some(StringValue(decomp))) => assert_eq(orig, decomp)
      _ => assert_true(false)
    }
  }
  
  // Cleanup
  LZ77Compressor::cleanup(compressor)
}

// Test 5: Gorilla Time Series Compression
test "gorilla time series compression" {
  let compressor = GorillaCompressor::new()
  
  // Generate time series data (good for Gorilla)
  let time_series_data = []
  let base_timestamp = 1609459200000L
  let base_value = 100.0
  
  for i in 0..=10000 {
    let timestamp = base_timestamp + (i * 60000L) // 1-minute intervals
    // Small changes in value (good for XOR compression)
    let value = base_value + (0.1 * ((i % 100).to_float() - 50.0))
    let data_point = TelemetryData::new("timeseries.metric", value, timestamp, Attributes::new())
    time_series_data.push(data_point)
  }
  
  // Compress using Gorilla
  let compressed_data = GorillaCompressor::compress(compressor, time_series_data)
  let original_size = calculate_data_size(time_series_data)
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // Gorilla should achieve excellent compression for time series data
  assert_true(compression_ratio < 0.2) // At least 80% compression
  
  // Decompress data
  let decompressed_data = GorillaCompressor::decompress(compressor, compressed_data)
  
  // Verify decompression accuracy
  assert_eq(decompressed_data.length(), time_series_data.length())
  
  for i in 0..=time_series_data.length() - 1 {
    let original = time_series_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.metric_name, decompressed.metric_name)
    assert_eq(original.timestamp, decompressed.timestamp)
    
    // Gorilla may have small floating point precision differences
    let value_diff = (original.value - decompressed.value).abs()
    assert_true(value_diff < 0.001) // Allow small precision differences
  }
  
  // Test with high-variance data (should be less effective)
  let variance_data = []
  for i in 0..=1000 {
    let timestamp = base_timestamp + (i * 60000L)
    let value = 100.0 * ((2.0 * 3.14159 * i.to_float()) / 100.0).sin() // High variance
    let data_point = TelemetryData::new("variance.metric", value, timestamp, Attributes::new())
    variance_data.push(data_point)
  }
  
  let compressed_variance = GorillaCompressor::compress(compressor, variance_data)
  let variance_original_size = calculate_data_size(variance_data)
  let variance_compressed_size = compressed_variance.length()
  let variance_compression_ratio = variance_compressed_size.to_float() / variance_original_size.to_float()
  
  // Gorilla should be less effective for high-variance data
  assert_true(variance_compression_ratio > compression_ratio)
  
  // Cleanup
  GorillaCompressor::cleanup(compressor)
}

// Test 6: Adaptive Compression
test "adaptive compression" {
  let compressor = AdaptiveCompressor::new()
  
  // Test with mixed data types
  let mixed_data = []
  
  // Add different types of data
  // Sequential data (good for delta)
  for i in 0..=1000 {
    let timestamp = 1609459200000L + (i * 60000L)
    let value = 100.0 + (0.1 * i.to_float())
    let data_point = TelemetryData::new("sequential.metric", value, timestamp, Attributes::new())
    mixed_data.push(data_point)
  }
  
  // Data with runs (good for RLE)
  for i in 0..=500 {
    let value = if i % 10 < 7 { 200.0 } else { 300.0 }
    let timestamp = 1609459200000L + (1001 * 60000L) + (i * 60000L)
    let data_point = TelemetryData::new("run.metric", value, timestamp, Attributes::new())
    mixed_data.push(data_point)
  }
  
  // Data with repeated patterns (good for LZ77)
  for i in 0..=500 {
    let timestamp = 1609459200000L + (1501 * 60000L) + (i * 60000L)
    let value = i.to_float()
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "pattern", StringValue("repeating_pattern"))
    
    let data_point = TelemetryData::new("pattern.metric", value, timestamp, attrs)
    mixed_data.push(data_point)
  }
  
  // Compress using adaptive compressor
  let compressed_data = AdaptiveCompressor::compress(compressor, mixed_data)
  let original_size = calculate_data_size(mixed_data)
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // Adaptive compressor should achieve reasonable compression
  assert_true(compression_ratio < 0.5) // At least 50% compression
  
  // Get compression strategy breakdown
  let strategy_breakdown = AdaptiveCompressor::get_strategy_breakdown(compressor)
  assert_true(strategy_breakdown.length() > 0)
  
  // Verify multiple strategies were used
  let unique_strategies = strategy_breakdown.map(|(strategy, _)| strategy).unique()
  assert_true(unique_strategies.length() > 1)
  
  // Decompress data
  let decompressed_data = AdaptiveCompressor::decompress(compressor, compressed_data)
  
  // Verify decompression accuracy
  assert_eq(decompressed_data.length(), mixed_data.length())
  
  for i in 0..=mixed_data.length() - 1 {
    let original = mixed_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.metric_name, decompressed.metric_name)
    assert_eq(original.value, decompressed.value)
    assert_eq(original.timestamp, decompressed.timestamp)
  }
  
  // Cleanup
  AdaptiveCompressor::cleanup(compressor)
}

// Test 7: Compression with Tolerance (Lossy Compression)
test "compression with tolerance" {
  let compressor = TolerantCompressor::new(0.01) // 1% tolerance
  
  // Generate data with small variations
  let precise_data = []
  let base_timestamp = 1609459200000L
  let base_value = 100.0
  
  for i in 0..=5000 {
    let timestamp = base_timestamp + (i * 60000L)
    // Very small variations that can be compressed with tolerance
    let value = base_value + (0.001 * ((i % 10).to_float() - 5.0))
    let data_point = TelemetryData::new("precise.metric", value, timestamp, Attributes::new())
    precise_data.push(data_point)
  }
  
  // Compress with tolerance
  let compressed_data = TolerantCompressor::compress(compressor, precise_data)
  let original_size = calculate_data_size(precise_data)
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // Tolerant compression should achieve very high compression
  assert_true(compression_ratio < 0.1) // At least 90% compression
  
  // Decompress data
  let decompressed_data = TolerantCompressor::decompress(compressor, compressed_data)
  
  // Verify decompression is within tolerance
  assert_eq(decompressed_data.length(), precise_data.length())
  
  for i in 0..=precise_data.length() - 1 {
    let original = precise_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.metric_name, decompressed.metric_name)
    assert_eq(original.timestamp, decompressed.timestamp)
    
    // Values should be within tolerance
    let value_diff = (original.value - decompressed.value).abs()
    let tolerance = original.value * 0.01
    assert_true(value_diff <= tolerance)
  }
  
  // Test with different tolerance levels
  let high_tolerance_compressor = TolerantCompressor::new(0.1) // 10% tolerance
  let low_tolerance_compressor = TolerantCompressor::new(0.001) // 0.1% tolerance
  
  let compressed_high_tolerance = TolerantCompressor::compress(high_tolerance_compressor, precise_data)
  let compressed_low_tolerance = TolerantCompressor::compress(low_tolerance_compressor, precise_data)
  
  let high_tolerance_size = compressed_high_tolerance.length()
  let low_tolerance_size = compressed_low_tolerance.length()
  
  // Higher tolerance should result in better compression
  assert_true(high_tolerance_size < low_tolerance_size)
  
  // Cleanup
  TolerantCompressor::cleanup(compressor)
  TolerantCompressor::cleanup(high_tolerance_compressor)
  TolerantCompressor::cleanup(low_tolerance_compressor)
}

// Test 8: Dictionary-based Compression
test "dictionary-based compression" {
  let compressor = DictionaryCompressor::new()
  
  // Generate data with repeated attribute values
  let dictionary_data = []
  let common_values = ["server_1", "server_2", "server_3", "server_4", "server_5"]
  let common_regions = ["us-east-1", "us-west-2", "eu-west-1", "ap-southeast-1"]
  
  for i in 0..=2000 {
    let timestamp = 1609459200000L + (i * 60000L)
    let value = i.to_float()
    
    // Use common values repeatedly
    let server = common_values[i % common_values.length()]
    let region = common_regions[i % common_regions.length()]
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "server", StringValue(server))
    Attributes::set(attrs, "region", StringValue(region))
    Attributes::set(attrs, "environment", StringValue("production"))
    
    let data_point = TelemetryData::new("dictionary.metric", value, timestamp, attrs)
    dictionary_data.push(data_point)
  }
  
  // Compress using dictionary compression
  let compressed_data = DictionaryCompressor::compress(compressor, dictionary_data)
  let original_size = calculate_data_size(dictionary_data)
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // Dictionary compression should achieve good compression for repeated values
  assert_true(compression_ratio < 0.4) // At least 60% compression
  
  // Get dictionary statistics
  let dict_stats = DictionaryCompressor::get_dictionary_statistics(compressor)
  assert_true(dict_stats.entry_count > 0)
  assert_true(dict_stats.total_savings > 0)
  
  // Verify common values are in dictionary
  for value in common_values {
    assert_true(DictionaryCompressor::is_in_dictionary(compressor, value))
  }
  
  for region in common_regions {
    assert_true(DictionaryCompressor::is_in_dictionary(compressor, region))
  }
  
  // Decompress data
  let decompressed_data = DictionaryCompressor::decompress(compressor, compressed_data)
  
  // Verify decompression accuracy
  assert_eq(decompressed_data.length(), dictionary_data.length())
  
  for i in 0..=dictionary_data.length() - 1 {
    let original = dictionary_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.metric_name, decompressed.metric_name)
    assert_eq(original.value, decompressed.value)
    assert_eq(original.timestamp, decompressed.timestamp)
    
    // Verify attributes
    let original_server = Attributes::get(original.attributes, "server")
    let decompressed_server = Attributes::get(decompressed.attributes, "server")
    
    match (original_server, decompressed_server) {
      (Some(StringValue(orig)), Some(StringValue(decomp))) => assert_eq(orig, decomp)
      _ => assert_true(false)
    }
  }
  
  // Cleanup
  DictionaryCompressor::cleanup(compressor)
}

// Test 9: Multi-stage Compression Pipeline
test "multi-stage compression pipeline" {
  let pipeline = CompressionPipeline::new()
  
  // Configure pipeline with multiple stages
  CompressionPipeline::add_stage(pipeline, DeltaCompressor::new())
  CompressionPipeline::add_stage(pipeline, RLECompressor::new())
  CompressionPipeline::add_stage(pipeline, HuffmanCompressor::new())
  
  // Generate data suitable for multi-stage compression
  let pipeline_data = []
  let base_timestamp = 1609459200000L
  let base_value = 100.0
  
  for i in 0..=5000 {
    let timestamp = base_timestamp + (i * 60000L)
    
    // Sequential with runs (good for delta + RLE)
    let value = 
      if i % 50 < 30 { base_value + (0.1 * i.to_float()) } // Sequential
      else if i % 50 < 40 { base_value + (0.1 * (i - 1).to_float()) } // Run of same values
      else { base_value + (0.1 * (i - 2).to_float()) } // Another run
    
    let attrs = Attributes::new()
    
    // Skewed attribute values (good for Huffman)
    let attr_value = 
      if i % 10 < 6 { "common_value_1" }
      else if i % 10 < 9 { "common_value_2" }
      else { "rare_value_" + (i % 3).to_string() }
    
    Attributes::set(attrs, "attr", StringValue(attr_value))
    
    let data_point = TelemetryData::new("pipeline.metric", value, timestamp, attrs)
    pipeline_data.push(data_point)
  }
  
  // Compress using pipeline
  let compressed_data = CompressionPipeline::compress(pipeline, pipeline_data)
  let original_size = calculate_data_size(pipeline_data)
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // Multi-stage pipeline should achieve excellent compression
  assert_true(compression_ratio < 0.2) // At least 80% compression
  
  // Get stage statistics
  let stage_stats = CompressionPipeline::get_stage_statistics(pipeline)
  assert_eq(stage_stats.length(), 3)
  
  // Verify each stage contributed to compression
  for i in 1..=stage_stats.length() - 1 {
    let prev_size = stage_stats[i-1].output_size
    let curr_size = stage_stats[i].output_size
    assert_true(curr_size < prev_size) // Each stage should reduce size
  }
  
  // Decompress data
  let decompressed_data = CompressionPipeline::decompress(pipeline, compressed_data)
  
  // Verify decompression accuracy
  assert_eq(decompressed_data.length(), pipeline_data.length())
  
  for i in 0..=pipeline_data.length() - 1 {
    let original = pipeline_data[i]
    let decompressed = decompressed_data[i]
    
    assert_eq(original.metric_name, decompressed.metric_name)
    assert_eq(original.value, decompressed.value)
    assert_eq(original.timestamp, decompressed.timestamp)
  }
  
  // Cleanup
  CompressionPipeline::cleanup(pipeline)
}

// Test 10: Compression Performance Benchmarking
test "compression performance benchmarking" {
  let benchmark_data = generate_benchmark_telemetry_data(10000)
  let original_size = calculate_data_size(benchmark_data)
  
  // Benchmark different compression algorithms
  let algorithms = [
    ("Delta", DeltaCompressor::new()),
    ("RLE", RLECompressor::new()),
    ("Huffman", HuffmanCompressor::new()),
    ("LZ77", LZ77Compressor::new(4096, 32768)),
    ("Gorilla", GorillaCompressor::new()),
    ("Adaptive", AdaptiveCompressor::new())
  ]
  
  let results = []
  
  for (name, compressor) in algorithms {
    // Benchmark compression
    let start_time = Time::now()
    let compressed_data = match name {
      "Delta" => DeltaCompressor::compress(compressor, benchmark_data)
      "RLE" => RLECompressor::compress(compressor, benchmark_data)
      "Huffman" => HuffmanCompressor::compress(compressor, benchmark_data)
      "LZ77" => LZ77Compressor::compress(compressor, benchmark_data)
      "Gorilla" => GorillaCompressor::compress(compressor, benchmark_data)
      "Adaptive" => AdaptiveCompressor::compress(compressor, benchmark_data)
      _ => ByteArray::new(0)
    }
    let compression_time = Time::now() - start_time
    
    // Benchmark decompression
    let start_time = Time::now()
    let decompressed_data = match name {
      "Delta" => DeltaCompressor::decompress(compressor, compressed_data)
      "RLE" => RLECompressor::decompress(compressor, compressed_data)
      "Huffman" => HuffmanCompressor::decompress(compressor, compressed_data)
      "LZ77" => LZ77Compressor::decompress(compressor, compressed_data)
      "Gorilla" => GorillaCompressor::decompress(compressor, compressed_data)
      "Adaptive" => AdaptiveCompressor::decompress(compressor, compressed_data)
      _ => []
    }
    let decompression_time = Time::now() - start_time
    
    let compressed_size = compressed_data.length()
    let compression_ratio = compressed_size.to_float() / original_size.to_float()
    
    results.push((name, compression_ratio, compression_time, decompression_time))
    
    // Cleanup
    match name {
      "Delta" => DeltaCompressor::cleanup(compressor)
      "RLE" => RLECompressor::cleanup(compressor)
      "Huffman" => HuffmanCompressor::cleanup(compressor)
      "LZ77" => LZ77Compressor::cleanup(compressor)
      "Gorilla" => GorillaCompressor::cleanup(compressor)
      "Adaptive" => AdaptiveCompressor::cleanup(compressor)
      _ => ()
    }
  }
  
  // Verify all algorithms completed successfully
  assert_eq(results.length(), algorithms.length())
  
  // Find best compression ratio
  let best_compression = results.min_by(|(_, ratio1, _, _), (_, ratio2, _, _)| ratio1 < ratio2)
  match best_compression {
    Some((name, _, _, _)) => assert_true(true) // Found best compression
    None => assert_true(false)
  }
  
  // Find fastest compression
  let fastest_compression = results.min_by(|(_, _, time1, _), (_, _, time2, _)| time1 < time2)
  match fastest_compression {
    Some((name, _, _, _)) => assert_true(true) // Found fastest compression
    None => assert_true(false)
  }
  
  // Find fastest decompression
  let fastest_decompression = results.min_by(|(_, _, _, time1), (_, _, _, time2)| time1 < time2)
  match fastest_decompression {
    Some((name, _, _, _)) => assert_true(true) // Found fastest decompression
    None => assert_true(false)
  }
  
  // Verify all algorithms achieve some compression
  for (name, ratio, _, _) in results {
    assert_true(ratio < 1.0) // All should achieve some compression
  }
}

// Helper functions
fn calculate_data_size(data : Array[TelemetryData]) -> Int {
  // Approximate size calculation
  let mut size = 0
  
  for data_point in data {
    size = size + data_point.metric_name.length() // Metric name
    size = size + 8 // Value (float)
    size = size + 8 // Timestamp (int64)
    
    // Attributes
    for (key, value) in data_point.attributes {
      size = size + key.length()
      match value {
        StringValue(s) => size = size + s.length()
        IntValue(_) => size = size + 8
        FloatValue(_) => size = size + 8
        BoolValue(_) => size = size + 1
        ArrayStringValue(arr) => {
          size = size + 8 // Array reference
          for s in arr {
            size = size + s.length()
          }
        }
        ArrayIntValue(arr) => {
          size = size + 8 // Array reference
          size = size + arr.length() * 8
        }
      }
    }
  }
  
  size
}

fn generate_random_telemetry_data(count : Int) -> Array[TelemetryData] {
  let data = []
  let base_timestamp = 1609459200000L
  
  for i in 0..=count-1 {
    let timestamp = base_timestamp + (i * 60000L)
    let value = 100.0 * ((i * 17) % 100).to_float() // Pseudo-random
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "random_attr", StringValue("value_" + (i % 50).to_string()))
    
    let data_point = TelemetryData::new("random.metric", value, timestamp, attrs)
    data.push(data_point)
  }
  
  data
}

fn generate_benchmark_telemetry_data(count : Int) -> Array[TelemetryData] {
  let data = []
  let base_timestamp = 1609459200000L
  
  for i in 0..=count-1 {
    let timestamp = base_timestamp + (i * 60000L)
    
    // Mix of different patterns
    let value = 
      if i % 4 == 0 { 100.0 + (0.1 * i.to_float()) } // Sequential
      else if i % 4 == 1 { 200.0 } // Constant
      else if i % 4 == 2 { 150.0 + (10.0 * ((2.0 * 3.14159 * (i % 100).to_float()) / 100.0).sin()) } // Periodic
      else { 120.0 * ((i % 10).to_float() / 10.0) } // Stepped
    
    let attrs = Attributes::new()
    
    // Mix of attribute patterns
    let attr_value = 
      if i % 3 == 0 { "common_value" }
      else if i % 3 == 1 { "another_common_value" }
      else { "unique_value_" + i.to_string() }
    
    Attributes::set(attrs, "pattern", StringValue(attr_value))
    
    let data_point = TelemetryData::new("benchmark.metric", value, timestamp, attrs)
    data.push(data_point)
  }
  
  data
}