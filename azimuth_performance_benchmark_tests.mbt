// Azimuth Performance Benchmark Tests
// This file contains comprehensive performance benchmark tests for the telemetry system

// Test 1: Telemetry Data Processing Throughput
test "telemetry data processing throughput benchmark" {
  // Create performance benchmark
  let benchmark = azimuth::PerformanceBenchmark::new("data_processing_throughput")
  
  // Configure benchmark parameters
  let data_sizes = [1000, 5000, 10000, 50000, 100000]
  let throughput_results = []
  
  for data_size in data_sizes {
    // Generate test data
    let test_data = []
    for i = 0; i < data_size; i = i + 1 {
      let data_point = azimuth::TelemetryData::new(
        timestamp = 1640995200000 + i * 1000,
        service_name = "benchmark-service",
        metric_name = "throughput.metric",
        metric_value = (i % 1000).to_double(),
        attributes = [
          ("batch.id", (i / 1000).to_string()),
          ("category", "performance-test")
        ]
      )
      test_data.push(data_point)
    }
    
    // Measure processing time
    let start_time = azimuth::Time::now()
    
    let processor = azimuth::TelemetryProcessor::new()
    let result = azimuth::TelemetryProcessor::process_batch(processor, test_data)
    
    let end_time = azimuth::Time::now()
    let processing_time_ms = end_time - start_time
    
    // Calculate throughput metrics
    let throughput_ops_per_sec = (data_size.to_double() / processing_time_ms.to_double()) * 1000.0
    let avg_latency_ms = processing_time_ms.to_double() / data_size.to_double()
    
    let benchmark_result = {
      "data_size": data_size,
      "processing_time_ms": processing_time_ms,
      "throughput_ops_per_sec": throughput_ops_per_sec,
      "avg_latency_ms": avg_latency_ms,
      "success_rate": (result.processed_count.to_double() / data_size.to_double()) * 100.0
    }
    
    throughput_results.push(benchmark_result)
    
    // Verify performance expectations
    assert_true(result.success)
    assert_eq(result.processed_count, data_size)
    
    // Throughput should scale reasonably with data size
    assert_true(throughput_ops_per_sec > 1000.0) // Minimum 1000 ops/sec
    
    // Average latency should be reasonable
    assert_true(avg_latency_ms < 10.0) // Less than 10ms per operation
  }
  
  // Verify throughput scaling
  assert_eq(throughput_results.length(), 5)
  
  // Larger batches should have better throughput (amortized overhead)
  assert_true(throughput_results[4].throughput_ops_per_sec >= throughput_results[0].throughput_ops_per_sec)
  
  // Success rate should be 100% for all batch sizes
  for result in throughput_results {
    assert_eq(result.success_rate, 100.0)
  }
  
  // Record benchmark results
  azimuth::PerformanceBenchmark::record_results(benchmark, throughput_results)
}

// Test 2: Memory Usage and Efficiency Benchmark
test "memory usage and efficiency benchmark" {
  let memory_benchmark = azimuth::MemoryBenchmark::new("memory_efficiency")
  
  // Test different data volumes
  let test_volumes = [10000, 50000, 100000, 500000]
  let memory_results = []
  
  for volume in test_volumes {
    // Measure baseline memory
    let baseline_memory = azimuth::MemoryProfiler::get_current_usage_mb()
    
    // Generate and process data
    let test_data = []
    for i = 0; i < volume; i = i + 1 {
      let data_point = azimuth::TelemetryData::new(
        timestamp = 1640995200000 + i * 1000,
        service_name = "memory-test-service",
        metric_name = "memory.metric",
        metric_value = i.to_double(),
        attributes = [
          ("index", i.to_string()),
          ("category", "memory-test"),
          ("batch", (i / 10000).to_string()),
          ("payload", "test-data-with-some-content-to-increase-memory-usage")
        ]
      )
      test_data.push(data_point)
    }
    
    // Process data
    let processor = azimuth::MemoryEfficientProcessor::new(
      batch_size = 10000,
      enable_compression = true
    )
    
    let start_time = azimuth::Time::now()
    let result = azimuth::MemoryEfficientProcessor::process_batch(processor, test_data)
    let end_time = azimuth::Time::now()
    
    // Measure peak memory usage
    let peak_memory = azimuth::MemoryProfiler::get_peak_usage_mb()
    let final_memory = azimuth::MemoryProfiler::get_current_usage_mb()
    
    // Calculate memory efficiency metrics
    let memory_per_data_point_bytes = ((peak_memory - baseline_memory) * 1024 * 1024) / volume
    let memory_efficiency_score = if memory_per_data_point_bytes > 0 {
      10000.0 / memory_per_data_point_bytes.to_double() // Normalized efficiency score
    } else {
      100.0
    }
    
    let memory_result = {
      "data_volume": volume,
      "baseline_memory_mb": baseline_memory,
      "peak_memory_mb": peak_memory,
      "final_memory_mb": final_memory,
      "memory_per_data_point_bytes": memory_per_data_point_bytes,
      "memory_efficiency_score": memory_efficiency_score,
      "processing_time_ms": end_time - start_time,
      "compression_ratio": result.compression_ratio
    }
    
    memory_results.push(memory_result)
    
    // Verify memory efficiency
    assert_true(result.success)
    assert_true(memory_per_data_point_bytes < 1000) // Less than 1KB per data point
    assert_true(memory_efficiency_score > 1.0) // Reasonable efficiency score
    
    // Memory should be released after processing
    assert_true(final_memory <= peak_memory)
  }
  
  // Verify memory scaling
  assert_eq(memory_results.length(), 4)
  
  // Memory per data point should not increase significantly with volume
  let base_efficiency = memory_results[0].memory_efficiency_score
  for i = 1; i < memory_results.length(); i = i + 1 {
    let current_efficiency = memory_results[i].memory_efficiency_score
    // Efficiency should not degrade by more than 50%
    assert_true(current_efficiency >= base_efficiency * 0.5)
  }
  
  // Record memory benchmark results
  azimuth::MemoryBenchmark::record_results(memory_benchmark, memory_results)
}

// Test 3: Concurrent Processing Performance
test "concurrent processing performance benchmark" {
  let concurrent_benchmark = azimuth::ConcurrencyBenchmark::new("concurrent_performance")
  
  // Test with different thread counts
  let thread_counts = [1, 2, 4, 8, 16]
  let concurrent_results = []
  
  let total_data_size = 100000
  
  for thread_count in thread_counts {
    // Generate test data
    let test_data = []
    for i = 0; i < total_data_size; i = i + 1 {
      let data_point = azimuth::TelemetryData::new(
        timestamp = 1640995200000 + i * 1000,
        service_name = "concurrent-service-" + (i % thread_count).to_string(),
        metric_name = "concurrent.metric",
        metric_value = i.to_double(),
        attributes = [
          ("thread.assigned", (i % thread_count).to_string()),
          ("batch.id", (i / 10000).to_string())
        ]
      )
      test_data.push(data_point)
    }
    
    // Configure concurrent processor
    let concurrent_processor = azimuth::ConcurrentTelemetryProcessor::new(
      max_workers = thread_count,
      queue_size = 10000,
      workload_balancing = true
    )
    
    // Measure concurrent processing time
    let start_time = azimuth::Time::now()
    
    let result = azimuth::ConcurrentTelemetryProcessor::process_concurrent(
      concurrent_processor, 
      test_data
    )
    
    let end_time = azimuth::Time::now()
    let processing_time_ms = end_time - start_time
    
    // Calculate concurrency metrics
    let throughput_ops_per_sec = (total_data_size.to_double() / processing_time_ms.to_double()) * 1000.0
    let parallelism_efficiency = if thread_count > 1 {
      let single_thread_estimate = processing_time_ms * thread_count
      (single_thread_estimate.to_double() / processing_time_ms.to_double()) * 100.0
    } else {
      100.0
    }
    
    let concurrent_result = {
      "thread_count": thread_count,
      "processing_time_ms": processing_time_ms,
      "throughput_ops_per_sec": throughput_ops_per_sec,
      "parallelism_efficiency": parallelism_efficiency,
      "cpu_utilization": result.cpu_utilization,
      "context_switches": result.context_switches,
      "cache_misses": result.cache_misses
    }
    
    concurrent_results.push(concurrent_result)
    
    // Verify concurrent processing
    assert_true(result.success)
    assert_eq(result.processed_count, total_data_size)
    
    // CPU utilization should be reasonable
    assert_true(result.cpu_utilization <= 100.0)
    
    // Throughput should improve with more threads (up to a point)
    assert_true(throughput_ops_per_sec > 1000.0)
  }
  
  // Verify concurrency scaling
  assert_eq(concurrent_results.length(), 5)
  
  // Find optimal thread count (best throughput)
  let optimal_thread_count = 1
  let max_throughput = concurrent_results[0].throughput_ops_per_sec
  
  for i = 1; i < concurrent_results.length(); i = i + 1 {
    if concurrent_results[i].throughput_ops_per_sec > max_throughput {
      max_throughput = concurrent_results[i].throughput_ops_per_sec
      optimal_thread_count = concurrent_results[i].thread_count
    }
  }
  
  // Optimal thread count should be reasonable
  assert_true(optimal_thread_count >= 2 && optimal_thread_count <= 16)
  
  // Parallelism efficiency should be reasonable
  for result in concurrent_results {
    if result.thread_count > 1 {
      // Efficiency should be at least 50% (some overhead is expected)
      assert_true(result.parallelism_efficiency >= 50.0)
    }
  }
  
  // Record concurrent benchmark results
  azimuth::ConcurrencyBenchmark::record_results(concurrent_benchmark, concurrent_results)
}

// Test 4: Serialization and Deserialization Performance
test "serialization and deserialization performance benchmark" {
  let serialization_benchmark = azimuth::SerializationBenchmark::new("serialization_performance")
  
  // Test different data sizes
  let data_sizes = [1000, 5000, 10000, 50000]
  let serialization_formats = ["json", "binary", "protobuf", "avro"]
  let serialization_results = []
  
  for data_size in data_sizes {
    for format in serialization_formats {
      // Generate test data
      let test_data = []
      for i = 0; i < data_size; i = i + 1 {
        let data_point = azimuth::TelemetryData::new(
          timestamp = 1640995200000 + i * 1000,
          service_name = "serialization-service",
          metric_name = "serialization.metric",
          metric_value = i.to_double(),
          attributes = [
            ("index", i.to_string()),
            ("format", format),
            ("payload", "test-data-with-content-for-serialization-testing")
          ]
        )
        test_data.push(data_point)
      }
      
      // Create serializer for format
      let serializer = azimuth::SerializerFactory::create(format)
      
      // Benchmark serialization
      let serialize_start = azimuth::Time::now()
      let serialize_result = azimuth::Serializer::serialize_batch(serializer, test_data)
      let serialize_end = azimuth::Time::now()
      let serialize_time_ms = serialize_end - serialize_start
      
      assert_true(serialize_result.success)
      
      // Benchmark deserialization
      let deserialize_start = azimuth::Time::now()
      let deserialize_result = azimuth::Serializer::deserialize_batch(
        serializer, 
        serialize_result.serialized_data
      )
      let deserialize_end = azimuth::Time::now()
      let deserialize_time_ms = deserialize_end - deserialize_start
      
      assert_true(deserialize_result.success)
      assert_eq(deserialize_result.deserialized_data.length(), data_size)
      
      // Calculate performance metrics
      let serialize_throughput_ops_per_sec = (data_size.to_double() / serialize_time_ms.to_double()) * 1000.0
      let deserialize_throughput_ops_per_sec = (data_size.to_double() / deserialize_time_ms.to_double()) * 1000.0
      let compression_ratio = serialize_result.original_size.to_double() / serialize_result.serialized_size.to_double()
      
      let serialization_result = {
        "data_size": data_size,
        "format": format,
        "serialize_time_ms": serialize_time_ms,
        "deserialize_time_ms": deserialize_time_ms,
        "serialize_throughput_ops_per_sec": serialize_throughput_ops_per_sec,
        "deserialize_throughput_ops_per_sec": deserialize_throughput_ops_per_sec,
        "compression_ratio": compression_ratio,
        "serialized_size_kb": serialize_result.serialized_size / 1024,
        "original_size_kb": serialize_result.original_size / 1024
      }
      
      serialization_results.push(serialization_result)
      
      // Verify performance expectations
      assert_true(serialize_throughput_ops_per_sec > 100.0) // Minimum 100 ops/sec
      assert_true(deserialize_throughput_ops_per_sec > 100.0) // Minimum 100 ops/sec
      assert_true(compression_ratio >= 1.0) // Should compress at least somewhat
      
      // Binary formats should be more efficient than JSON
      if (format == "binary" || format == "protobuf") && data_size >= 5000 {
        let json_result = serialization_results.find(|r| r.data_size == data_size && r.format == "json")
        if json_result.is_some {
          assert_true(compression_ratio > json_result.unwrap.compression_ratio)
        }
      }
    }
  }
  
  // Verify serialization results
  assert_eq(serialization_results.length(), 16) // 4 data sizes * 4 formats
  
  // Find best performing format for each data size
  for data_size in data_sizes {
    let size_results = serialization_results.filter(|r| r.data_size == data_size)
    let best_serialize = size_results.reduce(|best, current| 
      if current.serialize_throughput_ops_per_sec > best.serialize_throughput_ops_per_sec {
        current
      } else {
        best
      }
    )
    
    let best_deserialize = size_results.reduce(|best, current| 
      if current.deserialize_throughput_ops_per_sec > best.deserialize_throughput_ops_per_sec {
        current
      } else {
        best
      }
    )
    
    // Best formats should be binary-based
    assert_true(best_serialize.format == "binary" || best_serialize.format == "protobuf")
    assert_true(best_deserialize.format == "binary" || best_deserialize.format == "protobuf")
  }
  
  // Record serialization benchmark results
  azimuth::SerializationBenchmark::record_results(serialization_benchmark, serialization_results)
}

// Test 5: Network I/O Performance Benchmark
test "network I/O performance benchmark" {
  let network_benchmark = azimuth::NetworkBenchmark::new("network_io_performance")
  
  // Test different payload sizes and batch sizes
  let payload_sizes = [100, 1000, 10000, 100000] // Bytes
  let batch_sizes = [10, 50, 100, 500]
  let network_results = []
  
  for payload_size in payload_sizes {
    for batch_size in batch_sizes {
      // Create test data with specified payload size
      let test_data = []
      for i = 0; i < batch_size; i = i + 1 {
        let payload = "x".repeat(payload_size)
        let data_point = azimuth::TelemetryData::new(
          timestamp = 1640995200000 + i * 1000,
          service_name = "network-test-service",
          metric_name = "network.metric",
          metric_value = i.to_double(),
          attributes = [
            ("index", i.to_string()),
            ("payload", payload),
            ("batch_size", batch_size.to_string())
          ]
        )
        test_data.push(data_point)
      }
      
      // Create network client
      let network_client = azimuth::NetworkTelemetryClient::new()
      azimuth::NetworkTelemetryClient::configure(network_client, {
        "timeout_ms": 5000,
        "compression": true,
        "retry_count": 3
      })
      
      // Benchmark network transmission
      let transmit_start = azimuth::Time::now()
      
      let transmit_result = azimuth::NetworkTelemetryClient::send_batch(
        network_client, 
        test_data
      )
      
      let transmit_end = azimuth::Time::now()
      let transmit_time_ms = transmit_end - transmit_start
      
      assert_true(transmit_result.success)
      
      // Calculate network performance metrics
      let total_bytes = transmit_result.bytes_transmitted
      let throughput_mbps = (total_bytes.to_double() / transmit_time_ms.to_double()) * 8.0 / 1000.0
      let latency_per_item_ms = transmit_time_ms.to_double() / batch_size.to_double()
      let network_efficiency = if payload_size > 0 {
        (total_bytes.to_double() / (payload_size * batch_size).to_double()) * 100.0
      } else {
        100.0
      }
      
      let network_result = {
        "payload_size": payload_size,
        "batch_size": batch_size,
        "transmit_time_ms": transmit_time_ms,
        "total_bytes": total_bytes,
        "throughput_mbps": throughput_mbps,
        "latency_per_item_ms": latency_per_item_ms,
        "network_efficiency": network_efficiency,
        "retries": transmit_result.retry_count,
        "compression_ratio": transmit_result.compression_ratio
      }
      
      network_results.push(network_result)
      
      // Verify network performance
      assert_true(throughput_mbps > 0.1) // Minimum 0.1 Mbps
      assert_true(latency_per_item_ms < 1000.0) // Less than 1 second per item
      assert_true(network_efficiency > 50.0) // At least 50% efficiency
    }
  }
  
  // Verify network results
  assert_eq(network_results.length(), 16) // 4 payload sizes * 4 batch sizes
  
  // Analyze performance trends
  for payload_size in payload_sizes {
    let size_results = network_results.filter(|r| r.payload_size == payload_size)
    
    // Larger batches should have better throughput
    let small_batch = size_results.find(|r| r.batch_size == 10)
    let large_batch = size_results.find(|r| r.batch_size == 500)
    
    if small_batch.is_some && large_batch.is_some {
      assert_true(large_batch.unwrap.throughput_mbps >= small_batch.unwrap.throughput_mbps * 0.8)
    }
    
    // Latency per item should decrease with larger batches
    if small_batch.is_some && large_batch.is_some {
      assert_true(large_batch.unwrap.latency_per_item_ms <= small_batch.unwrap.latency_per_item_ms)
    }
  }
  
  // Record network benchmark results
  azimuth::NetworkBenchmark::record_results(network_benchmark, network_results)
}

// Test 6: Database I/O Performance Benchmark
test "database I/O performance benchmark" {
  let database_benchmark = azimuth::DatabaseBenchmark::new("database_io_performance")
  
  // Test different database operations
  let operations = ["insert", "select", "update", "delete"]
  let batch_sizes = [100, 500, 1000, 5000]
  let database_results = []
  
  for operation in operations {
    for batch_size in batch_sizes {
      // Create test data
      let test_data = []
      for i = 0; i < batch_size; i = i + 1 {
        let record = azimuth::TelemetryRecord::new(
          id = "test-record-" + i.to_string(),
          timestamp = 1640995200000 + i * 1000,
          service_name = "database-test-service",
          metric_name = "database.metric",
          metric_value = i.to_double(),
          attributes = [
            ("operation", operation),
            ("batch_id", (batch_size / 100).to_string())
          ]
        )
        test_data.push(record)
      }
      
      // Create database connection
      let db_connection = azimuth::DatabaseConnection::new()
      azimuth::DatabaseConnection::configure(db_connection, {
        "connection_pool_size": 10,
        "batch_mode": true,
        "transaction_size": 1000
      })
      
      // Benchmark database operation
      let db_start = azimuth::Time::now()
      
      let db_result = match operation {
        "insert" => azimuth::DatabaseConnection::insert_batch(db_connection, test_data),
        "select" => azimuth::DatabaseConnection::select_batch(db_connection, test_data.map(|r| r.id)),
        "update" => azimuth::DatabaseConnection::update_batch(db_connection, test_data),
        "delete" => azimuth::DatabaseConnection::delete_batch(db_connection, test_data.map(|r| r.id)),
        _ => { assert_true(false); azimuth::DatabaseResult::new(false, 0, 0) }
      }
      
      let db_end = azimuth::Time::now()
      let db_time_ms = db_end - db_start
      
      assert_true(db_result.success)
      
      // Calculate database performance metrics
      let throughput_ops_per_sec = (batch_size.to_double() / db_time_ms.to_double()) * 1000.0
      let avg_latency_ms = db_time_ms.to_double() / batch_size.to_double()
      let efficiency = if db_result.rows_affected > 0 {
        (db_result.rows_affected.to_double() / batch_size.to_double()) * 100.0
      } else {
        100.0
      }
      
      let database_result = {
        "operation": operation,
        "batch_size": batch_size,
        "db_time_ms": db_time_ms,
        "throughput_ops_per_sec": throughput_ops_per_sec,
        "avg_latency_ms": avg_latency_ms,
        "rows_affected": db_result.rows_affected,
        "efficiency": efficiency,
        "connection_pool_usage": db_result.connection_pool_usage
      }
      
      database_results.push(database_result)
      
      // Verify database performance
      assert_true(throughput_ops_per_sec > 10.0) // Minimum 10 ops/sec
      assert_true(avg_latency_ms < 100.0) // Less than 100ms per operation
      assert_true(efficiency >= 90.0) // At least 90% efficiency
    }
  }
  
  // Verify database results
  assert_eq(database_results.length(), 16) // 4 operations * 4 batch sizes
  
  // Analyze operation performance
  for operation in operations {
    let operation_results = database_results.filter(|r| r.operation == operation)
    
    // Larger batches should have better throughput
    let small_batch = operation_results.find(|r| r.batch_size == 100)
    let large_batch = operation_results.find(|r| r.batch_size == 5000)
    
    if small_batch.is_some && large_batch.is_some {
      assert_true(large_batch.unwrap.throughput_ops_per_sec >= small_batch.unwrap.throughput_ops_per_sec * 0.5)
    }
  }
  
  // Select operations should be faster than insert/update/delete
  let select_results = database_results.filter(|r| r.operation == "select")
  let insert_results = database_results.filter(|r| r.operation == "insert")
  
  let avg_select_throughput = select_results.reduce(|sum, r| sum + r.throughput_ops_per_sec, 0.0) / select_results.length().to_double()
  let avg_insert_throughput = insert_results.reduce(|sum, r| sum + r.throughput_ops_per_sec, 0.0) / insert_results.length().to_double()
  
  assert_true(avg_select_throughput >= avg_insert_throughput * 0.8) // Select should be at least 80% as fast as insert
  
  // Record database benchmark results
  azimuth::DatabaseBenchmark::record_results(database_benchmark, database_results)
}

// Test 7: Aggregation and Analytics Performance
test "aggregation and analytics performance benchmark" {
  let analytics_benchmark = azimuth::AnalyticsBenchmark::new("aggregation_performance")
  
  // Test different aggregation operations
  let aggregation_types = ["sum", "avg", "min", "max", "percentile", "histogram"]
  let data_volumes = [10000, 50000, 100000, 500000]
  let analytics_results = []
  
  for aggregation_type in aggregation_types {
    for volume in data_volumes {
      // Generate time series data
      let time_series_data = []
      for i = 0; i < volume; i = i + 1 {
        let data_point = {
          "timestamp": 1640995200000 + i * 1000,
          "value": (i % 1000).to_double() + 50.0 * ((2 * 3.14159 * i.to_double()) / 10000.0).sin(),
          "tags": [
            ("service", "analytics-service-" + (i % 5).to_string()),
            ("region", "region-" + (i % 3).to_string()),
            ("environment", "prod")
          ]
        }
        time_series_data.push(data_point)
      }
      
      // Create analytics engine
      let analytics_engine = azimuth::AnalyticsEngine::new()
      azimuth::AnalyticsEngine::configure(analytics_engine, {
        "parallel_processing": true,
        "memory_optimization": true
      })
      
      // Benchmark aggregation
      let agg_start = azimuth::Time::now()
      
      let agg_result = match aggregation_type {
        "sum" => azimuth::AnalyticsEngine::aggregate_sum(analytics_engine, time_series_data),
        "avg" => azimuth::AnalyticsEngine::aggregate_avg(analytics_engine, time_series_data),
        "min" => azimuth::AnalyticsEngine::aggregate_min(analytics_engine, time_series_data),
        "max" => azimuth::AnalyticsEngine::aggregate_max(analytics_engine, time_series_data),
        "percentile" => azimuth::AnalyticsEngine::aggregate_percentile(analytics_engine, time_series_data, 95.0),
        "histogram" => azimuth::AnalyticsEngine::aggregate_histogram(analytics_engine, time_series_data, 10),
        _ => { assert_true(false); azimuth::AggregationResult::new(false, 0.0) }
      }
      
      let agg_end = azimuth::Time::now()
      let agg_time_ms = agg_end - agg_start
      
      assert_true(agg_result.success)
      
      // Calculate analytics performance metrics
      let throughput_ops_per_sec = (volume.to_double() / agg_time_ms.to_double()) * 1000.0
      let memory_efficiency = if agg_result.memory_used_mb > 0 {
        volume.to_double() / (agg_result.memory_used_mb * 1024 * 1024).to_double()
      } else {
        1000.0
      }
      
      let analytics_result = {
        "aggregation_type": aggregation_type,
        "data_volume": volume,
        "agg_time_ms": agg_time_ms,
        "throughput_ops_per_sec": throughput_ops_per_sec,
        "memory_used_mb": agg_result.memory_used_mb,
        "memory_efficiency": memory_efficiency,
        "result": agg_result.result,
        "cpu_utilization": agg_result.cpu_utilization
      }
      
      analytics_results.push(analytics_result)
      
      // Verify analytics performance
      assert_true(throughput_ops_per_sec > 1000.0) // Minimum 1000 ops/sec
      assert_true(memory_efficiency > 10.0) // Reasonable memory efficiency
      assert_true(agg_result.memory_used_mb < volume * 0.001) // Memory usage should be reasonable
    }
  }
  
  // Verify analytics results
  assert_eq(analytics_results.length(), 24) // 6 aggregation types * 4 data volumes
  
  // Analyze scaling performance
  for aggregation_type in aggregation_types {
    let type_results = analytics_results.filter(|r| r.aggregation_type == aggregation_type)
    
    // Throughput should scale reasonably with data volume
    let small_volume = type_results.find(|r| r.data_volume == 10000)
    let large_volume = type_results.find(|r| r.data_volume == 500000)
    
    if small_volume.is_some && large_volume.is_some {
      // Large volume throughput should be at least 20% of small volume
      assert_true(large_volume.unwrap.throughput_ops_per_sec >= small_volume.unwrap.throughput_ops_per_sec * 0.2)
    }
  }
  
  // Simpler aggregations (sum, avg, min, max) should be faster than complex ones
  let simple_results = analytics_results.filter(|r| 
    r.aggregation_type == "sum" || r.aggregation_type == "avg" || 
    r.aggregation_type == "min" || r.aggregation_type == "max"
  )
  let complex_results = analytics_results.filter(|r| 
    r.aggregation_type == "percentile" || r.aggregation_type == "histogram"
  )
  
  let avg_simple_throughput = simple_results.reduce(|sum, r| sum + r.throughput_ops_per_sec, 0.0) / simple_results.length().to_double()
  let avg_complex_throughput = complex_results.reduce(|sum, r| sum + r.throughput_ops_per_sec, 0.0) / complex_results.length().to_double()
  
  assert_true(avg_simple_throughput >= avg_complex_throughput * 0.5) // Simple should be at least 50% faster
  
  // Record analytics benchmark results
  azimuth::AnalyticsBenchmark::record_results(analytics_benchmark, analytics_results)
}

// Test 8: Real-time Stream Processing Performance
test "real-time stream processing performance benchmark" {
  let stream_benchmark = azimuth::StreamBenchmark::new("realtime_stream_performance")
  
  // Test different stream configurations
  let stream_rates = [1000, 5000, 10000, 50000] // Events per second
  let window_sizes = [1000, 5000, 10000] // Milliseconds
  let stream_results = []
  
  for stream_rate in stream_rates {
    for window_size in window_sizes {
      // Create stream processor
      let stream_processor = azimuth::RealTimeStreamProcessor::new()
      azimuth::RealTimeStreamProcessor::configure(stream_processor, {
        "window_size_ms": window_size,
        "buffer_size": stream_rate * 2,
        "parallel_processing": true
      })
      
      // Simulate real-time stream
      let stream_start = azimuth::Time::now()
      let processed_events = 0
      let total_latency = 0
      
      // Process events for 10 seconds
      let duration_ms = 10000
      let events_to_process = (stream_rate * duration_ms) / 1000
      
      for i = 0; i < events_to_process; i = i + 1 {
        let event_start = azimuth::Time::now()
        
        let event = azimuth::StreamEvent::new(
          timestamp = stream_start + i * (1000 / stream_rate),
          event_type = "telemetry-event",
          data = {
            "service": "stream-service",
            "metric": "stream.metric",
            "value": (i % 1000).to_double(),
            "attributes": [
              ("event_id", i.to_string()),
              ("stream_rate", stream_rate.to_string())
            ]
          }
        )
        
        let result = azimuth::RealTimeStreamProcessor::process_event(stream_processor, event)
        
        let event_end = azimuth::Time::now()
        let event_latency = event_end - event_start
        
        if result.processed {
          processed_events = processed_events + 1
          total_latency = total_latency + event_latency
        }
        
        // Simulate real-time pacing
        let expected_time = i * (1000 / stream_rate)
        let actual_time = event_end - stream_start
        if actual_time < expected_time {
          azimuth::Thread::sleep(expected_time - actual_time)
        }
      }
      
      let stream_end = azimuth::Time::now()
      let actual_duration_ms = stream_end - stream_start
      
      // Calculate stream performance metrics
      let actual_throughput = (processed_events.to_double() / actual_duration_ms.to_double()) * 1000.0
      let avg_latency_ms = if processed_events > 0 {
        total_latency.to_double() / processed_events.to_double()
      } else {
        0.0
      }
      let processing_efficiency = (processed_events.to_double() / events_to_process.to_double()) * 100.0
      
      let stream_result = {
        "stream_rate": stream_rate,
        "window_size_ms": window_size,
        "events_to_process": events_to_process,
        "processed_events": processed_events,
        "actual_duration_ms": actual_duration_ms,
        "actual_throughput": actual_throughput,
        "avg_latency_ms": avg_latency_ms,
        "processing_efficiency": processing_efficiency,
        "buffer_utilization": azimuth::RealTimeStreamProcessor::get_buffer_utilization(stream_processor)
      }
      
      stream_results.push(stream_result)
      
      // Verify stream performance
      assert_true(processing_efficiency >= 90.0) // At least 90% of events processed
      assert_true(actual_throughput >= stream_rate * 0.8) // At least 80% of target rate
      assert_true(avg_latency_ms < 100.0) // Average latency under 100ms
    }
  }
  
  // Verify stream results
  assert_eq(stream_results.length(), 12) // 4 stream rates * 3 window sizes
  
  // Analyze stream performance scaling
  for stream_rate in stream_rates {
    let rate_results = stream_results.filter(|r| r.stream_rate == stream_rate)
    
    // Processing efficiency should be high for all configurations
    for result in rate_results {
      assert_true(result.processing_efficiency >= 90.0)
    }
    
    // Latency should increase with stream rate but remain reasonable
    let min_window = rate_results.find(|r| r.window_size_ms == 1000)
    let max_window = rate_results.find(|r| r.window_size_ms == 10000)
    
    if min_window.is_some && max_window.is_some {
      // Larger windows might have slightly higher latency but should still be reasonable
      assert_true(max_window.unwrap.avg_latency_ms <= min_window.unwrap.avg_latency_ms * 2.0)
    }
  }
  
  // Record stream benchmark results
  azimuth::StreamBenchmark::record_results(stream_benchmark, stream_results)
}

// Test 9: Cache Performance Benchmark
test "cache performance benchmark" {
  let cache_benchmark = azimuth::CacheBenchmark::new("cache_performance")
  
  // Test different cache configurations
  let cache_sizes = [1000, 5000, 10000, 50000] // Number of items
  let eviction_policies = ["lru", "lfu", "fifo", "random"]
  let cache_results = []
  
  for cache_size in cache_sizes {
    for policy in eviction_policies {
      // Create cache with specific configuration
      let cache = azimuth::TelemetryCache::new()
      azimuth::TelemetryCache::configure(cache, {
        "max_size": cache_size,
        "eviction_policy": policy,
        "ttl_ms": 60000 // 1 minute TTL
      })
      
      // Benchmark cache operations
      let operations = 100000 // Total operations to perform
      
      let cache_start = azimuth::Time::now()
      let hits = 0
      let misses = 0
      let insert_time = 0
      let get_time = 0
      
      // Mix of cache operations (70% gets, 30% inserts)
      for i = 0; i < operations; i = i + 1 {
        let key = "cache-key-" + (i % (cache_size * 2)).to_string()
        let value = {
          "timestamp": 1640995200000 + i * 1000,
          "service": "cache-service",
          "metric": "cache.metric",
          "value": i.to_double(),
          "attributes": [
            ("cache_policy", policy),
            ("cache_size", cache_size.to_string())
          ]
        }
        
        if i % 10 < 7 {
          // Get operation (70%)
          let get_start = azimuth::Time::now()
          let result = azimuth::TelemetryCache::get(cache, key)
          get_time = get_time + (azimuth::Time::now() - get_start)
          
          if result.is_some {
            hits = hits + 1
          } else {
            misses = misses + 1
          }
        } else {
          // Insert operation (30%)
          let insert_start = azimuth::Time::now()
          azimuth::TelemetryCache::put(cache, key, value)
          insert_time = insert_time + (azimuth::Time::now() - insert_start)
        }
      }
      
      let cache_end = azimuth::Time::now()
      let total_time_ms = cache_end - cache_start
      
      // Calculate cache performance metrics
      let hit_rate = (hits.to_double() / (hits + misses).to_double()) * 100.0
      let ops_per_sec = (operations.to_double() / total_time_ms.to_double()) * 1000.0
      let avg_get_time_us = if hits + misses > 0 {
        (get_time.to_double() / (hits + misses).to_double()) * 1000.0
      } else {
        0.0
      }
      let avg_insert_time_us = if insert_time > 0 {
        (insert_time.to_double() / ((operations / 10) * 3).to_double()) * 1000.0
      } else {
        0.0
      }
      
      let cache_result = {
        "cache_size": cache_size,
        "eviction_policy": policy,
        "operations": operations,
        "hits": hits,
        "misses": misses,
        "hit_rate": hit_rate,
        "ops_per_sec": ops_per_sec,
        "avg_get_time_us": avg_get_time_us,
        "avg_insert_time_us": avg_insert_time_us,
        "memory_usage_mb": azimuth::TelemetryCache::get_memory_usage(cache),
        "evictions": azimuth::TelemetryCache::get_eviction_count(cache)
      }
      
      cache_results.push(cache_result)
      
      // Verify cache performance
      assert_true(hit_rate >= 30.0) // Minimum 30% hit rate
      assert_true(ops_per_sec > 10000.0) // Minimum 10K ops/sec
      assert_true(avg_get_time_us < 100.0) // Gets under 100 microseconds
      assert_true(avg_insert_time_us < 200.0) // Inserts under 200 microseconds
    }
  }
  
  // Verify cache results
  assert_eq(cache_results.length(), 16) // 4 cache sizes * 4 eviction policies
  
  // Analyze eviction policy performance
  for cache_size in cache_sizes {
    let size_results = cache_results.filter(|r| r.cache_size == cache_size)
    
    // LRU and LFU should generally have better hit rates than FIFO and Random
    let lru_result = size_results.find(|r| r.eviction_policy == "lru")
    let lfu_result = size_results.find(|r| r.eviction_policy == "lfu")
    let fifo_result = size_results.find(|r| r.eviction_policy == "fifo")
    let random_result = size_results.find(|r| r.eviction_policy == "random")
    
    if lru_result.is_some && fifo_result.is_some {
      assert_true(lru_result.unwrap.hit_rate >= fifo_result.unwrap.hit_rate * 0.9)
    }
    
    if lfu_result.is_some && random_result.is_some {
      assert_true(lfu_result.unwrap.hit_rate >= random_result.unwrap.hit_rate * 0.9)
    }
  }
  
  // Hit rate should improve with larger cache sizes
  let small_cache = cache_results.filter(|r| r.cache_size == 1000)
  let large_cache = cache_results.filter(|r| r.cache_size == 50000)
  
  let avg_small_hit_rate = small_cache.reduce(|sum, r| sum + r.hit_rate, 0.0) / small_cache.length().to_double()
  let avg_large_hit_rate = large_cache.reduce(|sum, r| sum + r.hit_rate, 0.0) / large_cache.length().to_double()
  
  assert_true(avg_large_hit_rate >= avg_small_hit_rate * 1.2) // Larger cache should have at least 20% better hit rate
  
  // Record cache benchmark results
  azimuth::CacheBenchmark::record_results(cache_benchmark, cache_results)
}

// Test 10: End-to-End System Performance
test "end-to-end system performance benchmark" {
  let e2e_benchmark = azimuth::E2EBenchmark::new("end_to_end_performance")
  
  // Test complete telemetry pipeline
  let pipeline_configs = [
    {"name": "minimal", "features": ["basic_processing"]},
    {"name": "standard", "features": ["basic_processing", "aggregation", "filtering"]},
    {"name": "full", "features": ["basic_processing", "aggregation", "filtering", "anomaly_detection", "real_time_alerting"]}
  ]
  
  let data_volumes = [10000, 50000, 100000]
  let e2e_results = []
  
  for config in pipeline_configs {
    for volume in data_volumes {
      // Create complete telemetry pipeline
      let pipeline = azimuth::TelemetryPipeline::new()
      
      // Configure pipeline based on features
      if config.features.contains("basic_processing") {
        azimuth::TelemetryPipeline::add_processor(pipeline, "basic_processor")
      }
      
      if config.features.contains("aggregation") {
        azimuth::TelemetryPipeline::add_processor(pipeline, "aggregation_processor")
      }
      
      if config.features.contains("filtering") {
        azimuth::TelemetryPipeline::add_processor(pipeline, "filtering_processor")
      }
      
      if config.features.contains("anomaly_detection") {
        azimuth::TelemetryPipeline::add_processor(pipeline, "anomaly_detection_processor")
      }
      
      if config.features.contains("real_time_alerting") {
        azimuth::TelemetryPipeline::add_processor(pipeline, "alerting_processor")
      }
      
      // Generate test data
      let test_data = []
      for i = 0; i < volume; i = i + 1 {
        let data_point = azimuth::TelemetryData::new(
          timestamp = 1640995200000 + i * 1000,
          service_name = "e2e-test-service",
          metric_name = "e2e.metric",
          metric_value = i.to_double(),
          attributes = [
            ("pipeline_config", config.name),
            "index", i.to_string(),
            "category", "end-to-end-test"
          ]
        )
        test_data.push(data_point)
      }
      
      // Benchmark complete pipeline
      let e2e_start = azimuth::Time::now()
      let start_memory = azimuth::MemoryProfiler::get_current_usage_mb()
      let start_cpu = azimuth::CPUProfiler::get_cpu_usage()
      
      let pipeline_result = azimuth::TelemetryPipeline::process(pipeline, test_data)
      
      let e2e_end = azimuth::Time::now()
      let end_memory = azimuth::MemoryProfiler::get_current_usage_mb()
      let end_cpu = azimuth::CPUProfiler::get_cpu_usage()
      
      assert_true(pipeline_result.success)
      
      // Calculate end-to-end performance metrics
      let total_time_ms = e2e_end - e2e_start
      let throughput_ops_per_sec = (volume.to_double() / total_time_ms.to_double()) * 1000.0
      let memory_used_mb = end_memory - start_memory
      let cpu_usage = end_cpu - start_cpu
      let latency_p99 = pipeline_result.latency_p99_ms
      let latency_p95 = pipeline_result.latency_p95_ms
      let latency_avg = pipeline_result.latency_avg_ms
      
      let e2e_result = {
        "pipeline_config": config.name,
        "features_count": config.features.length(),
        "data_volume": volume,
        "total_time_ms": total_time_ms,
        "throughput_ops_per_sec": throughput_ops_per_sec,
        "memory_used_mb": memory_used_mb,
        "cpu_usage": cpu_usage,
        "latency_avg_ms": latency_avg,
        "latency_p95_ms": latency_p95,
        "latency_p99_ms": latency_p99,
        "processed_count": pipeline_result.processed_count,
        "error_count": pipeline_result.error_count,
        "success_rate": (pipeline_result.processed_count.to_double() / volume.to_double()) * 100.0
      }
      
      e2e_results.push(e2e_result)
      
      // Verify end-to-end performance
      assert_true(throughput_ops_per_sec > 100.0) // Minimum 100 ops/sec
      assert_true(latency_p99 < 1000.0) // P99 latency under 1 second
      assert_true(latency_avg < 100.0) // Average latency under 100ms
      assert_true(pipeline_result.success_rate >= 99.0) // At least 99% success rate
      assert_true(memory_used_mb < volume * 0.01) // Memory usage should be reasonable
    }
  }
  
  // Verify end-to-end results
  assert_eq(e2e_results.length(), 9) // 3 configs * 3 volumes
  
  // Analyze pipeline complexity vs performance
  for volume in data_volumes {
    let volume_results = e2e_results.filter(|r| r.data_volume == volume)
    
    let minimal_result = volume_results.find(|r| r.pipeline_config == "minimal")
    let standard_result = volume_results.find(|r| r.pipeline_config == "standard")
    let full_result = volume_results.find(|r| r.pipeline_config == "full")
    
    if minimal_result.is_some && standard_result.is_some && full_result.is_some {
      // More features should reduce throughput but not dramatically
      assert_true(standard_result.unwrap.throughput_ops_per_sec >= minimal_result.unwrap.throughput_ops_per_sec * 0.5)
      assert_true(full_result.unwrap.throughput_ops_per_sec >= minimal_result.unwrap.throughput_ops_per_sec * 0.3)
      
      // Latency should increase with more features
      assert_true(standard_result.unwrap.latency_avg_ms >= minimal_result.unwrap.latency_avg_ms)
      assert_true(full_result.unwrap.latency_avg_ms >= standard_result.unwrap.latency_avg_ms)
    }
  }
  
  // Performance should scale reasonably with data volume
  for config in pipeline_configs {
    let config_results = e2e_results.filter(|r| r.pipeline_config == config.name)
    
    let small_volume = config_results.find(|r| r.data_volume == 10000)
    let large_volume = config_results.find(|r| r.data_volume == 100000)
    
    if small_volume.is_some && large_volume.is_some {
      // Large volume throughput should be at least 30% of small volume
      assert_true(large_volume.unwrap.throughput_ops_per_sec >= small_volume.unwrap.throughput_ops_per_sec * 0.3)
    }
  }
  
  // Record end-to-end benchmark results
  azimuth::E2EBenchmark::record_results(e2e_benchmark, e2e_results)
  
  // Generate performance report
  let performance_report = azimuth::PerformanceReportGenerator::generate(e2e_results)
  assert_true(performance_report.contains("throughput"))
  assert_true(performance_report.contains("latency"))
  assert_true(performance_report.contains("memory"))
  assert_true(performance_report.contains("recommendations"))
}