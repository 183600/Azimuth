// Azimuth Telemetry System - Performance Benchmark Comprehensive Tests
// This file contains comprehensive test cases for performance benchmarking and optimization

// Test 1: Telemetry Data Processing Performance
test "telemetry data processing performance" {
  // Create performance benchmark
  let benchmark = PerformanceBenchmark::new("telemetry_data_processing")
  
  // Generate test data
  let data_sizes = [100, 1000, 10000, 100000, 1000000]
  
  for size in data_sizes {
    let telemetry_data = generate_telemetry_data(size)
    
    // Benchmark data aggregation
    let aggregation_time = benchmark.measure(fn() {
      aggregate_telemetry_data(telemetry_data)
    })
    
    // Benchmark data filtering
    let filtering_time = benchmark.measure(fn() {
      filter_telemetry_data(telemetry_data, "cpu_usage > 50.0")
    })
    
    // Benchmark data transformation
    let transformation_time = benchmark.measure(fn() {
      transform_telemetry_data(telemetry_data, fn(point) {
        TelemetryPoint::new(
          point.metric + "_normalized",
          point.value / 100.0,
          point.attributes
        )
      })
    })
    
    // Benchmark data serialization
    let serialization_time = benchmark.measure(fn() {
      serialize_telemetry_data(telemetry_data, "json")
    })
    
    // Benchmark data deserialization
    let serialized_data = serialize_telemetry_data(telemetry_data, "json")
    let deserialization_time = benchmark.measure(fn() {
      deserialize_telemetry_data(serialized_data, "json")
    })
    
    // Record benchmark results
    benchmark.record_result("aggregation", size, aggregation_time)
    benchmark.record_result("filtering", size, filtering_time)
    benchmark.record_result("transformation", size, transformation_time)
    benchmark.record_result("serialization", size, serialization_time)
    benchmark.record_result("deserialization", size, deserialization_time)
    
    // Verify performance expectations
    // Processing time should scale reasonably with data size
    assert_true(aggregation_time < size * 100) // Less than 100 microseconds per item
    assert_true(filtering_time < size * 50)   // Less than 50 microseconds per item
    assert_true(transformation_time < size * 75) // Less than 75 microseconds per item
    
    // Serialization/deserialization should be efficient
    assert_true(serialization_time < size * 20)   // Less than 20 microseconds per item
    assert_true(deserialization_time < size * 30) // Less than 30 microseconds per item
  }
  
  // Analyze performance scaling
  let scaling_analysis = benchmark.analyze_scaling()
  
  // Verify linear or near-linear scaling
  assert_true(scaling_analysis.aggregation_complexity < 2.0) // Should be less than O(n²)
  assert_true(scaling_analysis.filtering_complexity < 2.0)   // Should be less than O(n²)
  assert_true(scaling_analysis.transformation_complexity < 2.0) // Should be less than O(n²)
  
  // Generate performance report
  let report = benchmark.generate_report()
  assert_true(report.contains("Telemetry Data Processing Performance"))
  assert_true(report.contains("Aggregation"))
  assert_true(report.contains("Filtering"))
  assert_true(report.contains("Transformation"))
  assert_true(report.contains("Serialization"))
  assert_true(report.contains("Deserialization"))
}

// Test 2: Memory Usage and Allocation
test "memory usage and allocation" {
  // Create memory profiler
  let memory_profiler = MemoryProfiler::new()
  
  // Test memory usage with different data sizes
  let data_sizes = [1000, 10000, 100000, 1000000]
  
  for size in data_sizes {
    // Measure baseline memory
    let baseline_memory = memory_profiler.get_current_usage()
    
    // Create telemetry data
    let telemetry_data = generate_telemetry_data(size)
    let after_creation_memory = memory_profiler.get_current_usage()
    
    // Process telemetry data
    let processed_data = process_telemetry_data(telemetry_data)
    let after_processing_memory = memory_profiler.get_current_usage()
    
    // Serialize data
    let serialized_data = serialize_telemetry_data(processed_data, "json")
    let after_serialization_memory = memory_profiler.get_current_usage()
    
    // Calculate memory usage
    let creation_memory = after_creation_memory - baseline_memory
    let processing_memory = after_processing_memory - after_creation_memory
    let serialization_memory = after_serialization_memory - after_processing_memory
    let total_memory = after_serialization_memory - baseline_memory
    
    // Record memory usage
    memory_profiler.record_usage("creation", size, creation_memory)
    memory_profiler.record_usage("processing", size, processing_memory)
    memory_profiler.record_usage("serialization", size, serialization_memory)
    memory_profiler.record_usage("total", size, total_memory)
    
    // Verify memory efficiency
    // Memory usage should be reasonable for the data size
    assert_true(creation_memory < size * 500) // Less than 500 bytes per item
    assert_true(processing_memory < size * 200) // Less than 200 bytes per item for processing
    assert_true(serialization_memory < size * 100) // Less than 100 bytes per item for serialization
    
    // Total memory should be less than 1KB per item
    assert_true(total_memory < size * 1024)
    
    // Clean up
    drop(telemetry_data)
    drop(processed_data)
    drop(serialized_data)
    
    // Force garbage collection
    Memory::collect()
    
    // Verify memory is reclaimed
    let final_memory = memory_profiler.get_current_usage()
    let memory_reclaimed = after_serialization_memory - final_memory
    
    // At least 80% of memory should be reclaimed
    assert_true(memory_reclaimed > total_memory * 0.8)
  }
  
  // Test memory leak detection
  memory_profiler.enable_leak_detection()
  
  // Perform operations that might leak memory
  for i in 0..=1000 {
    let data = generate_telemetry_data(100)
    let processed = process_telemetry_data(data)
    let _ = serialize_telemetry_data(processed, "json")
    
    // Explicitly drop references
    drop(data)
    drop(processed)
  }
  
  // Force garbage collection
  Memory::collect()
  
  // Check for memory leaks
  let leak_report = memory_profiler.detect_leaks()
  assert_true(leak_report.leaked_objects == 0)
  
  // Generate memory usage report
  let memory_report = memory_profiler.generate_report()
  assert_true(memory_report.contains("Memory Usage Report"))
  assert_true(memory_report.contains("Creation"))
  assert_true(memory_report.contains("Processing"))
  assert_true(memory_report.contains("Serialization"))
  assert_true(memory_report.contains("Memory Leak Detection"))
}

// Test 3: Concurrent Processing Performance
test "concurrent processing performance" {
  // Create concurrent benchmark
  let concurrent_benchmark = ConcurrentBenchmark::new("concurrent_processing")
  
  // Test with different thread counts
  let thread_counts = [1, 2, 4, 8, 16]
  let data_size = 100000
  
  for thread_count in thread_counts {
    let telemetry_data = generate_telemetry_data(data_size)
    
    // Split data for concurrent processing
    let data_chunks = split_telemetry_data(telemetry_data, thread_count)
    
    // Benchmark concurrent processing
    let concurrent_time = concurrent_benchmark.measure_concurrent(
      thread_count,
      fn(chunk) {
        process_telemetry_data(chunk)
      },
      data_chunks
    )
    
    // Benchmark sequential processing
    let sequential_time = concurrent_benchmark.measure(fn() {
      process_telemetry_data(telemetry_data)
    })
    
    // Calculate speedup
    let speedup = sequential_time / concurrent_time
    
    // Calculate efficiency
    let efficiency = speedup / thread_count.to_float()
    
    // Record results
    concurrent_benchmark.record_result(thread_count, concurrent_time, speedup, efficiency)
    
    // Verify performance improvement
    if thread_count > 1 {
      assert_true(speedup > 1.0) // Should be faster than sequential
      
      // Efficiency should be reasonable (not perfect due to overhead)
      assert_true(efficiency > 0.3) // At least 30% efficiency
    }
    
    // Processing time should decrease with more threads (up to a point)
    if thread_count <= 8 {
      assert_true(concurrent_time < sequential_time)
    }
  }
  
  // Analyze scalability
  let scalability_analysis = concurrent_benchmark.analyze_scalability()
  
  // Verify good scalability
  assert_true(scalability_analysis.max_speedup > 2.0) // Should achieve at least 2x speedup
  assert_true(scalability_analysis.efficiency_at_8_threads > 0.4) // At least 40% efficiency at 8 threads
  
  // Test thread safety
  let thread_safety_tester = ThreadSafetyTester::new()
  
  // Create shared resources
  let shared_counter = AtomicCounter::new(0)
  let shared_data = ConcurrentMap::new()
  
  // Test concurrent access to shared resources
  thread_safety_tester.run_concurrent_test(10, fn() {
    for i in 0..=1000 {
      AtomicCounter::increment(shared_counter)
      ConcurrentMap::insert(shared_data, i.to_string(), i)
    }
  })
  
  // Verify thread safety
  assert_eq(AtomicCounter::get(shared_counter), 10000) // 10 threads * 1000 increments
  assert_eq(ConcurrentMap::size(shared_data), 1000) // Should have 1000 unique keys
  
  // Generate concurrent processing report
  let concurrent_report = concurrent_benchmark.generate_report()
  assert_true(concurrent_report.contains("Concurrent Processing Performance"))
  assert_true(concurrent_report.contains("Speedup"))
  assert_true(concurrent_report.contains("Efficiency"))
  assert_true(concurrent_report.contains("Scalability Analysis"))
}

// Test 4: Network Performance
test "network performance" {
  // Create network benchmark
  let network_benchmark = NetworkBenchmark::new("network_performance")
  
  // Test different payload sizes
  let payload_sizes = [1024, 10240, 102400, 1024000] // 1KB, 10KB, 100KB, 1MB
  
  for payload_size in payload_sizes {
    // Generate test payload
    let payload = generate_test_payload(payload_size)
    
    // Benchmark upload performance
    let upload_time = network_benchmark.measure_upload(payload)
    let upload_throughput = payload_size / upload_time
    
    // Benchmark download performance
    let download_time = network_benchmark.measure_download(payload_size)
    let download_throughput = payload_size / download_time
    
    // Benchmark round-trip time
    let rtt = network_benchmark.measure_rtt()
    
    // Record results
    network_benchmark.record_result(payload_size, upload_time, upload_throughput, download_time, download_throughput, rtt)
    
    // Verify performance expectations
    // Throughput should be reasonable
    assert_true(upload_throughput > 1024) // At least 1KB/s
    assert_true(download_throughput > 1024) // At least 1KB/s
    
    // RTT should be reasonable
    assert_true(rtt < 5000) // Less than 5 seconds
    
    // Larger payloads should have better throughput (amortized overhead)
    if payload_size > 1024 {
      let small_payload_result = network_benchmark.get_result(1024)
      assert_true(upload_throughput >= small_payload_result.upload_throughput * 0.8) // Within 80% of small payload throughput
    }
  }
  
  // Test connection pooling performance
  let connection_pool_benchmark = ConnectionPoolBenchmark::new()
  
  // Test with different pool sizes
  let pool_sizes = [1, 5, 10, 20]
  
  for pool_size in pool_sizes {
    // Benchmark connection acquisition time
    let acquisition_time = connection_pool_benchmark.measure_acquisition_time(pool_size)
    
    // Benchmark concurrent connection usage
    let concurrent_time = connection_pool_benchmark.measure_concurrent_usage(pool_size, 10)
    
    // Record results
    connection_pool_benchmark.record_result(pool_size, acquisition_time, concurrent_time)
    
    // Verify performance
    assert_true(acquisition_time < 100) // Less than 100ms to acquire connection
    
    // Larger pools should handle concurrent usage better
    if pool_size >= 10 {
      assert_true(concurrent_time < 1000) // Less than 1 second for concurrent usage
    }
  }
  
  // Test network error handling performance
  let error_handling_time = network_benchmark.measure_error_handling_performance()
  
  // Error handling should be fast
  assert_true(error_handling_time < 100) // Less than 100ms
  
  // Generate network performance report
  let network_report = network_benchmark.generate_report()
  assert_true(network_report.contains("Network Performance Report"))
  assert_true(network_report.contains("Upload Throughput"))
  assert_true(network_report.contains("Download Throughput"))
  assert_true(network_report.contains("Round-Trip Time"))
  assert_true(network_report.contains("Connection Pool Performance"))
}

// Test 5: Database Performance
test "database performance" {
  // Create database benchmark
  let db_benchmark = DatabaseBenchmark::new("database_performance")
  
  // Test different data volumes
  let data_volumes = [1000, 10000, 100000]
  
  for volume in data_volumes {
    // Generate test data
    let test_data = generate_database_test_data(volume)
    
    // Benchmark insert performance
    let insert_time = db_benchmark.measure_insert(test_data)
    let insert_throughput = volume / insert_time
    
    // Benchmark select performance
    let select_time = db_benchmark.measure_select(volume)
    let select_throughput = volume / select_time
    
    // Benchmark update performance
    let update_time = db_benchmark.measure_update(volume)
    let update_throughput = volume / update_time
    
    // Benchmark delete performance
    let delete_time = db_benchmark.measure_delete(volume)
    let delete_throughput = volume / delete_time
    
    // Benchmark query performance with different complexities
    let simple_query_time = db_benchmark.measure_query("SELECT * FROM telemetry WHERE id = ?", 1)
    let complex_query_time = db_benchmark.measure_query("SELECT * FROM telemetry WHERE metric_type = ? AND value > ? ORDER BY timestamp DESC LIMIT 10", 2)
    
    // Record results
    db_benchmark.record_result(volume, insert_time, insert_throughput, select_time, select_throughput, 
                              update_time, update_throughput, delete_time, delete_throughput,
                              simple_query_time, complex_query_time)
    
    // Verify performance expectations
    // Throughput should be reasonable
    assert_true(insert_throughput > 100)   // At least 100 inserts/second
    assert_true(select_throughput > 1000)  // At least 1000 selects/second
    assert_true(update_throughput > 100)   // At least 100 updates/second
    assert_true(delete_throughput > 100)   // At least 100 deletes/second
    
    // Queries should be fast
    assert_true(simple_query_time < 10)    // Less than 10ms for simple query
    assert_true(complex_query_time < 100)  // Less than 100ms for complex query
    
    // Performance should scale reasonably
    if volume > 1000 {
      let small_volume_result = db_benchmark.get_result(1000)
      
      // Select throughput shouldn't degrade too much with larger volumes
      assert_true(select_throughput >= small_volume_result.select_throughput * 0.5) // Within 50% of small volume throughput
    }
  }
  
  // Test index performance
  let index_benchmark = db_benchmark.measure_index_performance()
  
  // Indexes should improve query performance
  assert_true(index_benchmark.indexed_query_time < index_benchmark.non_indexed_query_time)
  assert_true(index_benchmark.index_improvement_ratio > 2.0) // At least 2x improvement
  
  // Test transaction performance
  let transaction_benchmark = db_benchmark.measure_transaction_performance(1000)
  
  // Transactions should have reasonable overhead
  assert_true(transaction_benchmark.transaction_time < transaction_benchmark.non_transaction_time * 2.0) // Less than 2x overhead
  
  // Generate database performance report
  let db_report = db_benchmark.generate_report()
  assert_true(db_report.contains("Database Performance Report"))
  assert_true(db_report.contains("Insert Throughput"))
  assert_true(db_report.contains("Select Throughput"))
  assert_true(db_report.contains("Update Throughput"))
  assert_true(db_report.contains("Delete Throughput"))
  assert_true(db_report.contains("Query Performance"))
  assert_true(db_report.contains("Index Performance"))
  assert_true(db_report.contains("Transaction Performance"))
}

// Test 6: Caching Performance
test "caching performance" {
  // Create cache benchmark
  let cache_benchmark = CacheBenchmark::new("cache_performance")
  
  // Test different cache sizes
  let cache_sizes = [1000, 10000, 100000]
  
  for cache_size in cache_sizes {
    // Create cache with specified size
    let cache = LRUCache::new(cache_size)
    
    // Populate cache
    let test_data = generate_cache_test_data(cache_size)
    for (key, value) in test_data {
      LRUCache::put(cache, key, value)
    }
    
    // Benchmark cache hit performance
    let hit_time = cache_benchmark.measure_cache_hit(cache)
    
    // Benchmark cache miss performance
    let miss_time = cache_benchmark.measure_cache_miss(cache)
    
    // Benchmark cache eviction performance
    let eviction_time = cache_benchmark.measure_cache_eviction(cache)
    
    // Benchmark concurrent cache access
    let concurrent_time = cache_benchmark.measure_concurrent_access(cache, 10)
    
    // Record results
    cache_benchmark.record_result(cache_size, hit_time, miss_time, eviction_time, concurrent_time)
    
    // Verify performance expectations
    // Cache hits should be very fast
    assert_true(hit_time < 1) // Less than 1 microsecond
    
    // Cache misses should be faster than database access
    assert_true(miss_time < 100) // Less than 100 microseconds
    
    // Eviction should be efficient
    assert_true(eviction_time < 10) // Less than 10 microseconds
    
    // Concurrent access should be efficient
    assert_true(concurrent_time < 1000) // Less than 1 millisecond for 10 concurrent operations
    
    // Cache hit should be much faster than miss
    assert_true(hit_time < miss_time / 10) // At least 10x faster
  }
  
  // Test cache hit ratio
  let hit_ratio_test = cache_benchmark.measure_hit_ratio(10000, 0.8) // 80% hit ratio
  assert_true(hit_ratio_test.actual_hit_ratio > 0.75) // Within 5% of target
  
  // Test cache warming performance
  let warming_time = cache_benchmark.measure_cache_warming(10000)
  assert_true(warming_time < 5000) // Less than 5 seconds to warm up cache
  
  // Test different cache policies
  let lru_cache = LRUCache::new(10000)
  let lfu_cache = LFUCache::new(10000)
  let fifo_cache = FIFOCache::new(10000)
  
  let lru_performance = cache_benchmark.measure_cache_policy(lru_cache)
  let lfu_performance = cache_benchmark.measure_cache_policy(lfu_cache)
  let fifo_performance = cache_benchmark.measure_cache_policy(fifo_cache)
  
  // All policies should have reasonable performance
  assert_true(lru_performance.average_access_time < 10)
  assert_true(lfu_performance.average_access_time < 10)
  assert_true(fifo_performance.average_access_time < 10)
  
  // Generate cache performance report
  let cache_report = cache_benchmark.generate_report()
  assert_true(cache_report.contains("Cache Performance Report"))
  assert_true(cache_report.contains("Cache Hit Time"))
  assert_true(cache_report.contains("Cache Miss Time"))
  assert_true(cache_report.contains("Cache Eviction Time"))
  assert_true(cache_report.contains("Concurrent Access"))
  assert_true(cache_report.contains("Hit Ratio"))
  assert_true(cache_report.contains("Cache Warming"))
  assert_true(cache_report.contains("Cache Policy Comparison"))
}

// Test 7: Serialization Performance
test "serialization performance" {
  // Create serialization benchmark
  let serialization_benchmark = SerializationBenchmark::new("serialization_performance")
  
  // Test different data sizes
  let data_sizes = [1000, 10000, 100000]
  
  // Test different serialization formats
  let formats = ["json", "protobuf", "msgpack", "avro", "xml"]
  
  for data_size in data_sizes {
    // Generate test data
    let test_data = generate_serialization_test_data(data_size)
    
    for format in formats {
      // Benchmark serialization
      let serialization_time = serialization_benchmark.measure_serialization(test_data, format)
      let serialized_size = serialization_benchmark.get_serialized_size(test_data, format)
      
      // Benchmark deserialization
      let deserialization_time = serialization_benchmark.measure_deserialization(test_data, format)
      
      // Calculate throughput
      let serialization_throughput = data_size / serialization_time
      let deserialization_throughput = data_size / deserialization_time
      
      // Calculate compression ratio (compared to a baseline)
      let baseline_size = data_size * 50 // Assume 50 bytes per item as baseline
      let compression_ratio = serialized_size / baseline_size
      
      // Record results
      serialization_benchmark.record_result(data_size, format, serialization_time, serialization_throughput,
                                           deserialization_time, deserialization_throughput,
                                           serialized_size, compression_ratio)
      
      // Verify performance expectations
      // Throughput should be reasonable
      assert_true(serialization_throughput > 100)   // At least 100 items/second
      assert_true(deserialization_throughput > 100) // At least 100 items/second
      
      // Binary formats should be more compact than text formats
      if format == "protobuf" || format == "msgpack" || format == "avro" {
        let json_result = serialization_benchmark.get_result(data_size, "json")
        assert_true(compression_ratio < json_result.compression_ratio) // Should be more compact
      }
      
      // Binary formats should be faster than text formats
      if format == "protobuf" || format == "msgpack" {
        let json_result = serialization_benchmark.get_result(data_size, "json")
        assert_true(serialization_throughput > json_result.serialization_throughput * 0.5) // At least 50% of JSON throughput
      }
    }
  }
  
  // Test streaming serialization
  let streaming_time = serialization_benchmark.measure_streaming_serialization(1000000)
  assert_true(streaming_time < 10000) // Less than 10 seconds for 1M items
  
  // Test partial deserialization
  let partial_time = serialization_benchmark.measure_partial_deserialization(100000)
  assert_true(partial_time < serialization_benchmark.measure_deserialization(generate_serialization_test_data(100000), "json"))
  
  // Generate serialization performance report
  let serialization_report = serialization_benchmark.generate_report()
  assert_true(serialization_report.contains("Serialization Performance Report"))
  assert_true(serialization_report.contains("JSON"))
  assert_true(serialization_report.contains("Protocol Buffers"))
  assert_true(serialization_report.contains("MessagePack"))
  assert_true(serialization_report.contains("Avro"))
  assert_true(serialization_report.contains("XML"))
  assert_true(serialization_report.contains("Streaming Serialization"))
  assert_true(serialization_report.contains("Partial Deserialization"))
}

// Helper functions for generating test data
fn generate_telemetry_data(count : Int) -> Array[TelemetryPoint] {
  let data = []
  
  for i in 0..=count {
    let point = TelemetryPoint::new(
      "metric_" + (i % 10).to_string(),
      (Math::random() * 100).to_float(),
      Attributes::with_data([
        ("service", StringValue("service_" + (i % 5).to_string())),
        ("environment", StringValue(if i % 2 == 0 { "prod" } else { "dev" }))
      ])
    )
    data.push(point)
  }
  
  data
}

fn generate_test_payload(size : Int) -> String {
  "x".repeat(size)
}

fn generate_database_test_data(count : Int) : Array[DatabaseRecord] {
  let data = []
  
  for i in 0..=count {
    let record = DatabaseRecord::new()
    DatabaseRecord::set_field(record, "id", i)
    DatabaseRecord::set_field(record, "metric_type", "metric_" + (i % 10).to_string())
    DatabaseRecord::set_field(record, "value", Math::random() * 100)
    DatabaseRecord::set_field(record, "timestamp", Time::now())
    data.push(record)
  }
  
  data
}

fn generate_cache_test_data(count : Int) : Map[String, String] {
  let data = Map::new()
  
  for i in 0..=count {
    Map::insert(data, "key_" + i.to_string(), "value_" + i.to_string())
  }
  
  data
}

fn generate_serialization_test_data(count : Int) : Array[TelemetryPoint] {
  generate_telemetry_data(count)
}

fn aggregate_telemetry_data(data : Array[TelemetryPoint]) : AggregatedTelemetry {
  let aggregator = TelemetryAggregator::new()
  
  for point in data {
    TelemetryAggregator::add_point(aggregator, point)
  }
  
  TelemetryAggregator::aggregate(aggregator)
}

fn filter_telemetry_data(data : Array[TelemetryPoint], filter : String) : Array[TelemetryPoint] {
  let filter_engine = FilterEngine::new(filter)
  let filtered_data = []
  
  for point in data {
    if FilterEngine::matches(filter_engine, point) {
      filtered_data.push(point)
    }
  }
  
  filtered_data
}

fn transform_telemetry_data(data : Array[TelemetryPoint], transform_fn : (TelemetryPoint) -> TelemetryPoint) : Array[TelemetryPoint] {
  let transformed_data = []
  
  for point in data {
    transformed_data.push(transform_fn(point))
  }
  
  transformed_data
}

fn process_telemetry_data(data : Array[TelemetryPoint]) : ProcessedTelemetryData {
  let processor = TelemetryProcessor::new()
  
  for point in data {
    TelemetryProcessor::process_point(processor, point)
  }
  
  TelemetryProcessor::get_result(processor)
}

fn serialize_telemetry_data(data : ProcessedTelemetryData, format : String) : String {
  match format {
    "json" => ProcessedTelemetryData::to_json(data),
    "protobuf" => ProcessedTelemetryData::to_protobuf(data),
    "msgpack" => ProcessedTelemetryData::to_msgpack(data),
    "avro" => ProcessedTelemetryData::to_avro(data),
    "xml" => ProcessedTelemetryData::to_xml(data),
    _ => ProcessedTelemetryData::to_json(data)
  }
}

fn deserialize_telemetry_data(data : String, format : String) : ProcessedTelemetryData {
  match format {
    "json" => ProcessedTelemetryData::from_json(data),
    "protobuf" => ProcessedTelemetryData::from_protobuf(data),
    "msgpack" => ProcessedTelemetryData::from_msgpack(data),
    "avro" => ProcessedTelemetryData::from_avro(data),
    "xml" => ProcessedTelemetryData::from_xml(data),
    _ => ProcessedTelemetryData::from_json(data)
  }
}

fn split_telemetry_data(data : Array[TelemetryPoint], chunk_count : Int) : Array[Array[TelemetryPoint]] {
  let chunks = []
  let chunk_size = data.length() / chunk_count
  
  for i in 0..=chunk_count - 1 {
    let start = i * chunk_size
    let end = if i == chunk_count - 1 { data.length() } else { (i + 1) * chunk_size }
    let chunk = []
    
    for j in start..=end - 1 {
      chunk.push(data[j])
    }
    
    chunks.push(chunk)
  }
  
  chunks
}