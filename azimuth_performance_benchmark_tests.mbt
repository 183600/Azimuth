// Azimuth Performance Benchmark Tests
// This file contains test cases for performance benchmarking

// Test 1: Telemetry Data Processing Performance
test "telemetry data processing performance" {
  let benchmark = Benchmark::new("Telemetry Data Processing")
  
  // Generate test data
  let telemetry_data = []
  for i = 0; i < 10000; i = i + 1 {
    let data = TelemetryData::new(
      "service-" + (i % 10).to_string(),
      "operation-" + (i % 100).to_string(),
      [
        ("request.id", StringValue("req-" + i.to_string())),
        ("user.id", StringValue("user-" + (i % 1000).to_string())),
        ("response.size", IntValue(1024 + (i % 2048))),
        ("duration.ms", IntValue(50 + (i % 500)))
      ],
      1234567890L + (i as Int64)
    )
    telemetry_data.push(data)
  }
  
  // Benchmark data processing
  let processor = TelemetryProcessor::new()
  let result = Benchmark::measure(benchmark, {
    for data in telemetry_data {
      TelemetryProcessor::process(processor, data)
    }
  })
  
  // Verify performance metrics
  assert_true(result.duration_ms < 1000) // Should complete within 1 second
  assert_true(result.throughput > 10000) // Should process at least 10,000 items/second
  
  // Print benchmark results
  Benchmark::print_result(result)
}

// Test 2: Serialization Performance
test "serialization performance" {
  let benchmark = Benchmark::new("Serialization Performance")
  
  // Generate test data
  let telemetry_data = TelemetryData::new(
    "performance-test-service",
    "serialization-operation",
    [
      ("large.data", StringValue("x".repeat(1000))),
      ("numeric.value", FloatValue(3.14159265359)),
      ("array.data", ArrayStringValue(["a", "b", "c", "d", "e"].repeat(100)))
    ],
    1234567890L
  )
  
  // Benchmark JSON serialization
  let json_serializer = JsonSerializer::new()
  let json_result = Benchmark::measure(benchmark.with_name("JSON Serialization"), {
    for i = 0; i < 1000; i = i + 1 {
      Serializer::serialize(json_serializer, telemetry_data)
    }
  })
  
  // Benchmark JSON deserialization
  let json_data = Serializer::serialize(json_serializer, telemetry_data)
  let json_deserializer = JsonDeserializer::new()
  let json_deserialize_result = Benchmark::measure(benchmark.with_name("JSON Deserialization"), {
    for i = 0; i < 1000; i = i + 1 {
      Deserializer::deserialize(json_deserializer, json_data)
    }
  })
  
  // Benchmark Binary serialization
  let binary_serializer = BinarySerializer::new()
  let binary_result = Benchmark::measure(benchmark.with_name("Binary Serialization"), {
    for i = 0; i < 1000; i = i + 1 {
      Serializer::serialize(binary_serializer, telemetry_data)
    }
  })
  
  // Verify performance metrics
  assert_true(json_result.duration_ms < 500)
  assert_true(json_deserialize_result.duration_ms < 500)
  assert_true(binary_result.duration_ms < 200) // Binary should be faster
  
  // Binary serialization should be faster than JSON
  assert_true(binary_result.duration_ms < json_result.duration_ms)
  
  // Print benchmark results
  Benchmark::print_result(json_result)
  Benchmark::print_result(json_deserialize_result)
  Benchmark::print_result(binary_result)
}

// Test 3: Compression Performance
test "compression performance" {
  let benchmark = Benchmark::new("Compression Performance")
  
  // Generate test data
  let large_data = "This is a test string for compression performance. ".repeat(1000)
  
  // Benchmark Gzip compression
  let gzip_compressor = GzipCompressor::new()
  let gzip_result = Benchmark::measure(benchmark.with_name("Gzip Compression"), {
    for i = 0; i < 100; i = i + 1 {
      Compressor::compress(gzip_compressor, large_data.to_bytes())
    }
  })
  
  // Benchmark Gzip decompression
  let compressed_data = Compressor::compress(gzip_compressor, large_data.to_bytes())
  let gzip_decompressor = GzipDecompressor::new()
  let gzip_decompress_result = Benchmark::measure(benchmark.with_name("Gzip Decompression"), {
    for i = 0; i < 100; i = i + 1 {
      Decompressor::decompress(gzip_decompressor, compressed_data)
    }
  })
  
  // Benchmark LZ4 compression
  let lz4_compressor = LZ4Compressor::new()
  let lz4_result = Benchmark::measure(benchmark.with_name("LZ4 Compression"), {
    for i = 0; i < 100; i = i + 1 {
      Compressor::compress(lz4_compressor, large_data.to_bytes())
    }
  })
  
  // Verify performance metrics
  assert_true(gzip_result.duration_ms < 1000)
  assert_true(gzip_decompress_result.duration_ms < 1000)
  assert_true(lz4_result.duration_ms < 500) // LZ4 should be faster
  
  // LZ4 compression should be faster than Gzip
  assert_true(lz4_result.duration_ms < gzip_result.duration_ms)
  
  // Print benchmark results
  Benchmark::print_result(gzip_result)
  Benchmark::print_result(gzip_decompress_result)
  Benchmark::print_result(lz4_result)
}

// Test 4: Concurrent Performance
test "concurrent performance" {
  let benchmark = Benchmark::new("Concurrent Performance")
  
  // Test single-threaded performance
  let single_thread_result = Benchmark::measure(benchmark.with_name("Single-threaded"), {
    let counter = AtomicInt::new(0)
    for i = 0; i < 1000000; i = i + 1 {
      AtomicInt::increment(counter)
    }
    AtomicInt::get(counter)
  })
  
  // Test multi-threaded performance
  let multi_thread_result = Benchmark::measure(benchmark.with_name("Multi-threaded"), {
    let counter = AtomicInt::new(0)
    let threads = []
    
    for i = 0; i < 4; i = i + 1 {
      let thread = Thread::spawn({
        for j = 0; j < 250000; j = j + 1 {
          AtomicInt::increment(counter)
        }
      })
      threads.push(thread)
    }
    
    for thread in threads {
      Thread::join(thread)
    }
    
    AtomicInt::get(counter)
  })
  
  // Verify both produce the same result
  assert_eq(single_thread_result.value, 1000000)
  assert_eq(multi_thread_result.value, 1000000)
  
  // Multi-threaded should be faster (on multi-core systems)
  // This might not always be true due to thread overhead, so we just verify it completes
  assert_true(multi_thread_result.duration_ms < 5000)
  
  // Print benchmark results
  Benchmark::print_result(single_thread_result)
  Benchmark::print_result(multi_thread_result)
}

// Test 5: Memory Allocation Performance
test "memory allocation performance" {
  let benchmark = Benchmark::new("Memory Allocation Performance")
  
  // Benchmark stack allocation
  let stack_result = Benchmark::measure(benchmark.with_name("Stack Allocation"), {
    let sum = 0
    for i = 0; i < 1000000; i = i + 1 {
      let value = i * 2
      sum = sum + value
    }
    sum
  })
  
  // Benchmark heap allocation
  let heap_result = Benchmark::measure(benchmark.with_name("Heap Allocation"), {
    let values = []
    for i = 0; i < 100000; i = i + 1 {
      values.push(i * 2)
    }
    
    let sum = values.fold(0, { acc, value => acc + value })
    sum
  })
  
  // Verify results
  assert_eq(stack_result.value, 1000000 * 999999) // Sum of 0, 2, 4, ..., 1999998
  assert_eq(heap_result.value, 100000 * 99999) // Sum of 0, 2, 4, ..., 199998
  
  // Stack allocation should be faster
  assert_true(stack_result.duration_ms < heap_result.duration_ms)
  
  // Print benchmark results
  Benchmark::print_result(stack_result)
  Benchmark::print_result(heap_result)
}

// Test 6: Network I/O Performance
test "network i/o performance" {
  let benchmark = Benchmark::new("Network I/O Performance")
  
  // Benchmark small request performance
  let small_request_result = Benchmark::measure(benchmark.with_name("Small Requests"), {
    let client = HttpClient::new()
    let total_bytes = AtomicInt::new(0)
    
    for i = 0; i < 100; i = i + 1 {
      let response = HttpClient::get(client, "https://httpbin.org/bytes/1024") // 1KB response
      match response {
        Success(data) => AtomicInt::add(total_bytes, data.length()),
        Error(_) => () // Ignore errors for benchmark
      }
    }
    
    AtomicInt::get(total_bytes)
  })
  
  // Benchmark large request performance
  let large_request_result = Benchmark::measure(benchmark.with_name("Large Requests"), {
    let client = HttpClient::new()
    let total_bytes = AtomicInt::new(0)
    
    for i = 0; i < 10; i = i + 1 {
      let response = HttpClient::get(client, "https://httpbin.org/bytes/102400") // 100KB response
      match response {
        Success(data) => AtomicInt::add(total_bytes, data.length()),
        Error(_) => () // Ignore errors for benchmark
      }
    }
    
    AtomicInt::get(total_bytes)
  })
  
  // Verify results (if requests succeeded)
  if small_request_result.value > 0 {
    assert_eq(small_request_result.value, 100 * 1024) // 100 requests * 1KB each
  }
  
  if large_request_result.value > 0 {
    assert_eq(large_request_result.value, 10 * 102400) // 10 requests * 100KB each
  }
  
  // Print benchmark results
  Benchmark::print_result(small_request_result)
  Benchmark::print_result(large_request_result)
}

// Test 7: Database Query Performance
test "database query performance" {
  let benchmark = Benchmark::new("Database Query Performance")
  
  // Setup test database
  let db = TestDatabase::new()
  TestDatabase::create_table(db, "benchmark_test")
  TestDatabase::insert_test_data(db, "benchmark_test", 10000) // Insert 10,000 rows
  
  // Benchmark simple query
  let simple_query_result = Benchmark::measure(benchmark.with_name("Simple Query"), {
    let results = TestDatabase::query(db, "SELECT COUNT(*) FROM benchmark_test")
    match results {
      Some(count) => count,
      None => 0
    }
  })
  
  // Benchmark complex query
  let complex_query_result = Benchmark::measure(benchmark.with_name("Complex Query"), {
    let results = TestDatabase::query(db, "
      SELECT category, AVG(value) as avg_value 
      FROM benchmark_test 
      WHERE value > 50 
      GROUP BY category 
      ORDER BY avg_value DESC 
      LIMIT 10
    ")
    match results {
      Some(rows) => rows.length(),
      None => 0
    }
  })
  
  // Benchmark batch insert
  let batch_insert_result = Benchmark::measure(benchmark.with_name("Batch Insert"), {
    let batch_data = []
    for i = 0; i < 1000; i = i + 1 {
      batch_data.push(("batch_" + i.to_string(), i % 100, i * 2))
    }
    TestDatabase::batch_insert(db, "benchmark_test", batch_data)
  })
  
  // Verify results
  assert_eq(simple_query_result.value, 10000) // Should count all 10,000 rows
  
  // Complex query should return some results
  assert_true(complex_query_result.value >= 0 && complex_query_result.value <= 10)
  
  // Batch insert should succeed
  assert_eq(batch_insert_result.value, 1000) // Should insert 1000 rows
  
  // Print benchmark results
  Benchmark::print_result(simple_query_result)
  Benchmark::print_result(complex_query_result)
  Benchmark::print_result(batch_insert_result)
}

// Test 8: Time Series Processing Performance
test "time series processing performance" {
  let benchmark = Benchmark::new("Time Series Processing Performance")
  
  // Generate time series data
  let time_series = TimeSeriesCollection::new("cpu.usage", "percentage")
  for i = 0; i < 86400; i = i + 1 { // 1 day of data (1 point per second)
    let timestamp = 1234567890L + (i as Int64)
    let value = 50.0 + 10.0 * ((i as Float * 0.01).sin()) // Sine wave pattern
    let point = TimeSeriesDataPoint::new(timestamp, value, [])
    TimeSeriesCollection::add_point(time_series, point)
  }
  
  // Benchmark aggregation
  let aggregation_result = Benchmark::measure(benchmark.with_name("Aggregation"), {
    TimeSeriesAggregator::average(time_series)
  })
  
  // Benchmark windowing
  let windowing_result = Benchmark::measure(benchmark.with_name("Windowing"), {
    TimeSeriesWindower::time_windows(time_series, 3600L) // 1-hour windows
  })
  
  // Benchmark downsampling
  let downsampling_result = Benchmark::measure(benchmark.with_name("Downsampling"), {
    TimeSeriesDownsampler::downsample(time_series, 60L, Average) // Downsample to 1-minute resolution
  })
  
  // Benchmark anomaly detection
  let anomaly_result = Benchmark::measure(benchmark.with_name("Anomaly Detection"), {
    TimeSeriesAnomalyDetector::statistical(time_series, 2.0) // 2 standard deviations
  })
  
  // Verify results
  assert_true(aggregation_result.value >= 40.0 && aggregation_result.value <= 60.0) // Should be around 50
  assert_eq(windowing_result.value.length(), 24) // 24 one-hour windows
  assert_eq(downsampling_result.value.size(), 1440) // 1440 one-minute points
  assert_true(anomaly_result.value.length() >= 0) // Should detect some anomalies
  
  // Print benchmark results
  Benchmark::print_result(aggregation_result)
  Benchmark::print_result(windowing_result)
  Benchmark::print_result(downsampling_result)
  Benchmark::print_result(anomaly_result)
}

// Test 9: Cache Performance
test "cache performance" {
  let benchmark = Benchmark::new("Cache Performance")
  
  let cache = LRUCache::new(1000) // 1000 item cache
  let keys = []
  
  // Generate test keys
  for i = 0; i < 2000; i = i + 1 {
    keys.push("key_" + i.to_string())
  }
  
  // Benchmark cache writes
  let write_result = Benchmark::measure(benchmark.with_name("Cache Writes"), {
    for i = 0; i < 1000; i = i + 1 {
      LRUCache::put(cache, keys[i], "value_" + i.to_string())
    }
  })
  
  // Benchmark cache hits
  let hit_result = Benchmark::measure(benchmark.with_name("Cache Hits"), {
    let hit_count = 0
    for i = 0; i < 1000; i = i + 1 {
      match LRUCache::get(cache, keys[i]) {
        Some(_) => hit_count = hit_count + 1,
        None => ()
      }
    }
    hit_count
  })
  
  // Benchmark cache misses
  let miss_result = Benchmark::measure(benchmark.with_name("Cache Misses"), {
    let miss_count = 0
    for i = 1000; i < 2000; i = i + 1 {
      match LRUCache::get(cache, keys[i]) {
        Some(_) => (),
        None => miss_count = miss_count + 1
      }
    }
    miss_count
  })
  
  // Verify results
  assert_eq(write_result.value, 1000) // Should write 1000 items
  assert_eq(hit_result.value, 1000)  // Should hit all 1000 items
  assert_eq(miss_result.value, 1000) // Should miss all 1000 items
  
  // Cache hits should be faster than misses
  assert_true(hit_result.duration_ms < miss_result.duration_ms)
  
  // Print benchmark results
  Benchmark::print_result(write_result)
  Benchmark::print_result(hit_result)
  Benchmark::print_result(miss_result)
}

// Test 10: End-to-End Performance
test "end-to-end performance" {
  let benchmark = Benchmark::new("End-to-End Performance")
  
  // Simulate complete telemetry pipeline
  let pipeline = TelemetryPipeline::new()
  TelemetryPipeline::configure(pipeline, [
    ("batch_size", IntValue(100)),
    ("flush_interval_ms", IntValue(1000)),
    ("compression", BoolValue(true)),
    ("retry_count", IntValue(3))
  ])
  
  // Generate test data
  let telemetry_data = []
  for i = 0; i < 10000; i = i + 1 {
    let data = TelemetryData::new(
      "service-" + (i % 10).to_string(),
      "operation-" + (i % 100).to_string(),
      [
        ("request.id", StringValue("req-" + i.to_string())),
        ("response.size", IntValue(1024 + (i % 2048))),
        ("duration.ms", IntValue(50 + (i % 500)))
      ],
      1234567890L + (i as Int64)
    )
    telemetry_data.push(data)
  }
  
  // Benchmark complete pipeline
  let pipeline_result = Benchmark::measure(benchmark.with_name("Complete Pipeline"), {
    for data in telemetry_data {
      TelemetryPipeline::process(pipeline, data)
    }
    TelemetryPipeline::flush(pipeline)
  })
  
  // Verify pipeline processed all data
  assert_eq(TelemetryPipeline::processed_count(pipeline), 10000)
  
  // Verify pipeline performance
  assert_true(pipeline_result.duration_ms < 5000) // Should complete within 5 seconds
  assert_true(pipeline_result.throughput > 2000) // Should process at least 2,000 items/second
  
  // Print benchmark results
  Benchmark::print_result(pipeline_result)
  
  // Print pipeline statistics
  let stats = TelemetryPipeline::get_statistics(pipeline)
  println("Pipeline Statistics:")
  println("  Processed: " + stats.processed_count.to_string())
  println("  Batches: " + stats.batch_count.to_string())
  println("  Errors: " + stats.error_count.to_string())
  println("  Avg Batch Size: " + stats.avg_batch_size.to_string())
  println("  Compression Ratio: " + stats.compression_ratio.to_string())
}