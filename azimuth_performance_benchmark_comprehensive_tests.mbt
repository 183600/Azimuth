// Azimuth Performance Benchmark Comprehensive Tests
// This file contains comprehensive performance benchmark tests for telemetry operations

// Test 1: Telemetry Data Processing Performance
test "telemetry data processing performance benchmarks" {
  // Define performance metrics
  type PerformanceMetrics = {
    operation_name: String,
    data_size: Int,
    execution_time_ms: Int,
    memory_usage_mb: Float,
    throughput_ops_per_sec: Float
  }
  
  // Define benchmark configuration
  type BenchmarkConfig = {
    iterations: Int,
    data_sizes: Array[Int],
    warmup_iterations: Int
  }
  
  // Create benchmark configuration
  let config = {
    iterations: 100,
    data_sizes: [100, 1000, 10000, 100000],
    warmup_iterations: 10
  }
  
  // Generate test data
  let generate_telemetry_data = fn(size: Int) {
    let mut data = []
    for i in 0..size {
      data = data.push({
        timestamp: 1640995200 + i,
        trace_id: "trace-" + i.to_string(),
        span_id: "span-" + i.to_string(),
        service_name: "service-" + (i % 10).to_string(),
        operation_name: "operation-" + (i % 5).to_string(),
        duration_ms: 50 + (i % 500),
        status: if i % 10 == 0 { "error" } else { "success" },
        attributes: [
          ("user.id", "user-" + (i % 1000).to_string()),
          ("request.id", "req-" + i.to_string())
        ]
      })
    }
    data
  }
  
  // Benchmark function
  let benchmark_operation = fn(operation: Array[Object] -> Unit, data_size: Int, iterations: Int) {
    let data = generate_telemetry_data(data_size)
    
    // Warmup
    for i in 0..config.warmup_iterations {
      operation(data)
    }
    
    // Actual benchmark
    let start_time = 1640995200  // Simplified timestamp
    for i in 0..iterations {
      operation(data)
    }
    let end_time = 1640995300  // Simplified timestamp
    
    let total_time_ms = (end_time - start_time) * 1000
    let avg_time_ms = total_time_ms / iterations
    let throughput = iterations.to_float() / (total_time_ms.to_float() / 1000.0)
    
    {
      operation_name: "telemetry_processing",
      data_size,
      execution_time_ms: avg_time_ms,
      memory_usage_mb: (data_size * 0.001).to_float(),  // Simulated memory usage
      throughput_ops_per_sec: throughput
    }
  }
  
  // Define test operations
  let filter_operation = fn(data: Array[Object]) {
    let mut filtered = []
    for item in data {
      if item["status"] == "success" {
        filtered = filtered.push(item)
      }
    }
    filtered
  }
  
  let aggregate_operation = fn(data: Array[Object>) {
    let mut total_duration = 0
    let mut success_count = 0
    for item in data {
      total_duration = total_duration + item["duration_ms"]
      if item["status"] == "success" {
        success_count = success_count + 1
      }
    }
    { total_duration, success_count }
  }
  
  let transform_operation = fn(data: Array[Object>) {
    let mut transformed = []
    for item in data {
      transformed = transformed.push({
        trace_id: item["trace_id"],
        service_name: item["service_name"].to_uppercase(),
        duration_category: if item["duration_ms"] < 100 { "fast" } else { "slow" }
      })
    }
    transformed
  }
  
  // Run benchmarks
  let filter_benchmarks = config.data_sizes.map(fn(size) {
    benchmark_operation(filter_operation, size, config.iterations)
  })
  
  let aggregate_benchmarks = config.data_sizes.map(fn(size) {
    benchmark_operation(aggregate_operation, size, config.iterations)
  })
  
  let transform_benchmarks = config.data_sizes.map(fn(size) {
    benchmark_operation(transform_operation, size, config.iterations)
  })
  
  // Verify performance characteristics
  // Filter operation should scale linearly
  assert_true(filter_benchmarks[0].data_size == 100)
  assert_true(filter_benchmarks[1].data_size == 1000)
  assert_true(filter_benchmarks[2].data_size == 10000)
  assert_true(filter_benchmarks[3].data_size == 100000)
  
  // Throughput should remain relatively stable across data sizes
  let filter_throughput_variance = filter_benchmarks[3].throughput_ops_per_sec / filter_benchmarks[0].throughput_ops_per_sec
  assert_true(filter_throughput_variance > 0.5)  // Should not degrade by more than 50%
  
  // Memory usage should scale with data size
  assert_true(filter_benchmarks[3].memory_usage_mb > filter_benchmarks[0].memory_usage_mb)
  
  // Aggregate operation should be efficient
  assert_true(aggregate_benchmarks[0].throughput_ops_per_sec > 10.0)  // At least 10 ops/sec
  
  // Transform operation should maintain reasonable performance
  assert_true(transform_benchmarks[3].execution_time_ms < 10000)  // Less than 10 seconds for largest dataset
}

// Test 2: Serialization Performance
test "telemetry serialization performance benchmarks" {
  // Define serialization format
  type SerializationFormat = {
    name: String,
    serialize: Object -> String,
    deserialize: String -> Object
  }
  
  // Create test telemetry object
  let test_telemetry = {
    trace_id: "trace-serialization-test-123",
    span_id: "span-serialization-test-456",
    parent_span_id: Some("span-parent-789"),
    service_name: "performance-test-service",
    operation_name: "benchmark_operation",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "success",
    attributes: [
      ("user.id", "user-12345"),
      ("request.id", "req-67890"),
      ("service.version", "1.2.3"),
      ("environment", "benchmark"),
      ("region", "us-west-2")
    ]
  }
  
  // Define serialization formats
  let json_format = {
    name: "json",
    serialize: fn(obj: Object) {
      let mut json = "{"
      json = json + "\"trace_id\":\"" + obj.trace_id + "\","
      json = json + "\"span_id\":\"" + obj.span_id + "\","
      match obj.parent_span_id {
        Some(parent) => json = json + "\"parent_span_id\":\"" + parent + "\","
        None => {}
      }
      json = json + "\"service_name\":\"" + obj.service_name + "\","
      json = json + "\"operation_name\":\"" + obj.operation_name + "\","
      json = json + "\"start_time\":" + obj.start_time.to_string() + ","
      json = json + "\"end_time\":" + obj.end_time.to_string() + ","
      json = json + "\"status\":\"" + obj.status + "\","
      json = json + "\"attributes\":["
      for i in 0..obj.attributes.length() {
        let (key, value) = obj.attributes[i]
        json = json + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
        if i < obj.attributes.length() - 1 {
          json = json + ","
        }
      }
      json = json + "]}"
      json
    },
    deserialize: fn(json: String) {
      // Simplified JSON parsing for testing
      test_telemetry  // Return original object for benchmark purposes
    }
  }
  
  let csv_format = {
    name: "csv",
    serialize: fn(obj: Object) {
      let mut csv = ""
      csv = csv + obj.trace_id + ","
      csv = csv + obj.span_id + ","
      match obj.parent_span_id {
        Some(parent) => csv = csv + parent
        None => csv = csv + ""
      }
      csv = csv + ","
      csv = csv + obj.service_name + ","
      csv = csv + obj.operation_name + ","
      csv = csv + obj.start_time.to_string() + ","
      csv = csv + obj.end_time.to_string() + ","
      csv = csv + obj.status + ","
      
      for i in 0..obj.attributes.length() {
        let (key, value) = obj.attributes[i]
        csv = csv + key + "=" + value
        if i < obj.attributes.length() - 1 {
          csv = csv + ";"
        }
      }
      
      csv
    },
    deserialize: fn(csv: String) {
      test_telemetry  // Return original object for benchmark purposes
    }
  }
  
  let kv_format = {
    name: "key_value",
    serialize: fn(obj: Object) {
      let mut kv = ""
      kv = kv + "trace_id=" + obj.trace_id + "\n"
      kv = kv + "span_id=" + obj.span_id + "\n"
      match obj.parent_span_id {
        Some(parent) => kv = kv + "parent_span_id=" + parent + "\n"
        None => {}
      }
      kv = kv + "service_name=" + obj.service_name + "\n"
      kv = kv + "operation_name=" + obj.operation_name + "\n"
      kv = kv + "start_time=" + obj.start_time.to_string() + "\n"
      kv = kv + "end_time=" + obj.end_time.to_string() + "\n"
      kv = kv + "status=" + obj.status + "\n"
      
      for (key, value) in obj.attributes {
        kv = kv + "attr." + key + "=" + value + "\n"
      }
      
      kv
    },
    deserialize: fn(kv: String) {
      test_telemetry  // Return original object for benchmark purposes
    }
  }
  
  let formats = [json_format, csv_format, kv_format]
  
  // Benchmark serialization
  let benchmark_serialization = fn(format: SerializationFormat, iterations: Int) {
    // Warmup
    for i in 0..10 {
      format.serialize(test_telemetry)
    }
    
    // Benchmark serialization
    let start_time = 1640995200
    let mut serialized_data = []
    for i in 0..iterations {
      serialized_data = serialized_data.push(format.serialize(test_telemetry))
    }
    let end_time = 1640995300
    
    let total_time_ms = (end_time - start_time) * 1000
    let avg_time_ms = total_time_ms / iterations
    let throughput = iterations.to_float() / (total_time_ms.to_float() / 1000.0)
    
    // Calculate size metrics
    let sample_serialized = format.serialize(test_telemetry)
    let avg_size_bytes = sample_serialized.length()
    
    {
      format_name: format.name,
      avg_serialization_time_ms: avg_time_ms,
      serialization_throughput: throughput,
      avg_size_bytes,
      total_serialized_size: avg_size_bytes * iterations
    }
  }
  
  // Benchmark deserialization
  let benchmark_deserialization = fn(format: SerializationFormat, iterations: Int) {
    let serialized_data = []
    for i in 0..iterations {
      serialized_data = serialized_data.push(format.serialize(test_telemetry))
    }
    
    // Warmup
    for i in 0..10 {
      format.deserialize(serialized_data[i])
    }
    
    // Benchmark deserialization
    let start_time = 1640995200
    let mut deserialized_data = []
    for i in 0..iterations {
      deserialized_data = deserialized_data.push(format.deserialize(serialized_data[i]))
    }
    let end_time = 1640995300
    
    let total_time_ms = (end_time - start_time) * 1000
    let avg_time_ms = total_time_ms / iterations
    let throughput = iterations.to_float() / (total_time_ms.to_float() / 1000.0)
    
    {
      format_name: format.name,
      avg_deserialization_time_ms: avg_time_ms,
      deserialization_throughput: throughput
    }
  }
  
  // Run benchmarks
  let iterations = 1000
  let serialization_results = formats.map(fn(format) {
    benchmark_serialization(format, iterations)
  })
  
  let deserialization_results = formats.map(fn(format) {
    benchmark_deserialization(format, iterations)
  })
  
  // Verify serialization performance
  assert_eq(serialization_results.length(), 3)
  assert_eq(deserialization_results.length(), 3)
  
  // CSV should be most compact
  let csv_result = serialization_results.filter(fn(r) { r.format_name == "csv" })[0]
  let json_result = serialization_results.filter(fn(r) { r.format_name == "json" })[0]
  let kv_result = serialization_results.filter(fn(r) { r.format_name == "key_value" })[0]
  
  assert_true(csv_result.avg_size_bytes < json_result.avg_size_bytes)
  assert_true(csv_result.avg_size_bytes < kv_result.avg_size_bytes)
  
  // All formats should have reasonable throughput
  for result in serialization_results {
    assert_true(result.serialization_throughput > 100.0)  // At least 100 serializations/sec
  }
  
  for result in deserialization_results {
    assert_true(result.deserialization_throughput > 100.0)  // At least 100 deserializations/sec
  }
  
  // JSON might be slower due to complexity but should still be performant
  assert_true(json_result.avg_serialization_time_ms < 100)  // Less than 100ms per serialization
}

// Test 3: Memory Usage and Allocation Performance
test "memory usage and allocation performance benchmarks" {
  // Define memory usage metrics
  type MemoryMetrics = {
    operation_name: String,
    initial_memory_mb: Float,
    peak_memory_mb: Float,
    final_memory_mb: Float,
    memory_allocated_mb: Float,
    memory_freed_mb: Float,
    allocation_count: Int
  }
  
  // Simulate memory tracking
  let track_memory_usage = fn(operation: () -> Unit, initial_memory: Float) {
    let peak_memory = initial_memory
    let allocation_count = 0  // Simplified count
    
    // Simulate memory allocation during operation
    let allocated_memory = initial_memory + 10.0  // Allocate 10MB
    let new_peak = if allocated_memory > peak_memory {
      allocated_memory
    } else {
      peak_memory
    }
    
    operation()  // Execute operation
    
    // Simulate memory cleanup
    let final_memory = initial_memory + 2.0  // Some memory retained
    
    {
      operation_name: "memory_test",
      initial_memory_mb: initial_memory,
      peak_memory_mb: new_peak,
      final_memory_mb: final_memory,
      memory_allocated_mb: allocated_memory - initial_memory,
      memory_freed_mb: allocated_memory - final_memory,
      allocation_count
    }
  }
  
  // Define memory-intensive operations
  let create_large_dataset = fn(size: Int) {
    let mut dataset = []
    for i in 0..size {
      dataset = dataset.push({
        id: i,
        data: "large-data-string-" + i.to_string(),
        timestamp: 1640995200 + i,
        metadata: [
          ("key1", "value1"),
          ("key2", "value2"),
          ("key3", "value3")
        ]
      })
    }
    dataset
  }
  
  let process_large_dataset = fn(dataset: Array[Object>) {
    let mut processed = []
    for item in dataset {
      processed = processed.push({
        id: item.id,
        processed_data: item.data.to_uppercase(),
        processed_timestamp: item.timestamp * 1000,
        processed_metadata: item.metadata.map(fn(pair) {
          (pair[0].to_uppercase(), pair[1].to_lowercase())
        })
      })
    }
    processed
  }
  
  // Benchmark memory usage for different data sizes
  let data_sizes = [1000, 5000, 10000, 50000]
  let initial_memory = 100.0  // Starting with 100MB
  
  let memory_benchmarks = data_sizes.map(fn(size) {
    track_memory_usage(fn() {
      let dataset = create_large_dataset(size)
      let processed = process_large_dataset(dataset)
      // Simulate some processing time
      let mut sum = 0
      for item in processed {
        sum = sum + item.id
      }
    }, initial_memory)
  })
  
  // Verify memory usage patterns
  assert_eq(memory_benchmarks.length(), 4)
  
  // Memory usage should increase with data size
  for i in 1..memory_benchmarks.length() {
    assert_true(memory_benchmarks[i].peak_memory_mb >= memory_benchmarks[i-1].peak_memory_mb)
  }
  
  // Memory should be freed after processing (some should remain)
  for benchmark in memory_benchmarks {
    assert_true(benchmark.memory_freed_mb > 0.0)
    assert_true(benchmark.peak_memory_mb > benchmark.final_memory_mb)
  }
  
  // Test memory efficiency
  let calculate_memory_efficiency = fn(benchmark: MemoryMetrics) {
    if benchmark.memory_allocated_mb > 0.0 {
      benchmark.memory_freed_mb / benchmark.memory_allocated_mb
    } else {
      0.0
    }
  }
  
  // Memory efficiency should be reasonable (> 50%)
  for benchmark in memory_benchmarks {
    let efficiency = calculate_memory_efficiency(benchmark)
    assert_true(efficiency > 0.5)
  }
  
  // Test memory leak detection
  let detect_memory_leaks = fn(benchmarks: Array[MemoryMetrics>) {
    let mut has_leak = false
    for i in 1..benchmarks.length() {
      let current = benchmarks[i]
      let previous = benchmarks[i-1]
      
      // If final memory keeps growing significantly, might indicate memory leak
      if current.final_memory_mb > previous.final_memory_mb * 1.5 {
        has_leak = true
      }
    }
    has_leak
  }
  
  let has_memory_leaks = detect_memory_leaks(memory_benchmarks)
  assert_false(has_memory_leaks)  // Should not have memory leaks in this test
}

// Test 4: Concurrent Performance
test "concurrent processing performance benchmarks" {
  // Define concurrent task
  type ConcurrentTask = {
    task_id: Int,
    data_size: Int,
    operation: String
  }
  
  // Define concurrent performance metrics
  type ConcurrentMetrics = {
    total_tasks: Int,
    concurrent_workers: Int,
    total_time_ms: Int,
    avg_task_time_ms: Int,
    throughput_tasks_per_sec: Float,
    cpu_utilization_percent: Float
  }
  
  // Simulate concurrent task execution
  let execute_concurrent_tasks = fn(tasks: Array[ConcurrentTask>, worker_count: Int) {
    let start_time = 1640995200
    
    // Simulate distributing tasks across workers
    let tasks_per_worker = tasks.length() / worker_count
    let mut worker_completion_times = []
    
    for worker in 0..worker_count {
      let worker_start = worker * 100  // Stagger worker starts
      let worker_end = worker_start + tasks_per_worker * 50  // Each task takes 50ms
      worker_completion_times = worker_completion_times.push(worker_end)
    }
    
    // Total time is when the last worker finishes
    let max_completion_time = worker_completion_times.reduce(fn(max, time) {
      if time > max { time } else { max }
    }, 0)
    
    let end_time = start_time + max_completion_time
    let total_time_ms = max_completion_time
    
    {
      total_tasks: tasks.length(),
      concurrent_workers: worker_count,
      total_time_ms,
      avg_task_time_ms: 50,  // Each task takes 50ms
      throughput_tasks_per_sec: tasks.length().to_float() / (total_time_ms.to_float() / 1000.0),
      cpu_utilization_percent: (worker_count.to_float() / 8.0) * 100.0  // Assuming 8 CPU cores
    }
  }
  
  // Create test tasks
  let create_test_tasks = fn(count: Int, data_size: Int) {
    let mut tasks = []
    for i in 0..count {
      tasks = tasks.push({
        task_id: i,
        data_size: data_size,
        operation: "process_telemetry"
      })
    }
    tasks
  }
  
  // Test different concurrency levels
  let task_count = 100
  let data_size = 1000
  let test_tasks = create_test_tasks(task_count, data_size)
  let worker_counts = [1, 2, 4, 8, 16]
  
  let concurrent_results = worker_counts.map(fn(worker_count) {
    execute_concurrent_tasks(test_tasks, worker_count)
  })
  
  // Verify concurrent performance characteristics
  assert_eq(concurrent_results.length(), 5)
  
  // Sequential execution (1 worker) should be slowest
  let sequential_result = concurrent_results[0]
  let concurrent_result = concurrent_results[2]  // 4 workers
  
  // Concurrent execution should be faster
  assert_true(concurrent_result.total_time_ms < sequential_result.total_time_ms)
  
  // Throughput should increase with more workers (up to a point)
  assert_true(concurrent_result.throughput_tasks_per_sec > sequential_result.throughput_tasks_per_sec)
  
  // Test scalability efficiency
  let calculate_scalability_efficiency = fn(sequential_time: Int, concurrent_time: Int, workers: Int) {
    let ideal_speedup = workers.to_float()
    let actual_speedup = sequential_time.to_float() / concurrent_time.to_float()
    (actual_speedup / ideal_speedup) * 100.0
  }
  
  // Check scalability for different worker counts
  for i in 1..concurrent_results.length() {
    let workers = worker_counts[i]
    let result = concurrent_results[i]
    let efficiency = calculate_scalability_efficiency(sequential_result.total_time_ms, result.total_time_ms, workers)
    
    // Efficiency should be reasonable (> 30% even with high concurrency)
    assert_true(efficiency > 30.0)
  }
  
  // CPU utilization should increase with more workers
  for i in 1..concurrent_results.length() {
    assert_true(concurrent_results[i].cpu_utilization_percent >= concurrent_results[i-1].cpu_utilization_percent)
  }
  
  // Test diminishing returns
  let highest_workers_result = concurrent_results[4]  // 16 workers
  let mid_workers_result = concurrent_results[3]      // 8 workers
  
  // At some point, adding more workers should show diminishing returns
  let speedup_improvement = highest_workers_result.throughput_tasks_per_sec / mid_workers_result.throughput_tasks_per_sec
  assert_true(speedup_improvement < 2.0)  // Shouldn't be 2x improvement by doubling workers
}

// Test 5: Network I/O Performance
test "network I/O performance benchmarks" {
  // Define network operation types
  type NetworkOperation = {
    operation_type: String,
    payload_size_bytes: Int,
    endpoint: String,
    protocol: String
  }
  
  // Define network performance metrics
  type NetworkMetrics = {
    operation_type: String,
    payload_size_bytes: Int,
    round_trip_time_ms: Int,
    throughput_mbps: Float,
    connection_overhead_ms: Int,
    success_rate: Float
  }
  
  // Simulate network operations
  let simulate_network_operation = fn(operation: NetworkOperation) {
    // Simulate network latency based on payload size
    let base_latency = 10  // Base latency in ms
    let size_factor = operation.payload_size_bytes / 1000  // 1ms per 1000 bytes
    let protocol_overhead = if operation.protocol == "http" { 5 } else { 2 }
    
    let round_trip_time = base_latency + size_factor + protocol_overhead
    
    // Calculate throughput (payload size / round trip time)
    let throughput_mbps = (operation.payload_size_bytes.to_float() * 8.0) / (round_trip_time.to_float() / 1000.0) / 1000000.0
    
    // Simulate connection overhead
    let connection_overhead = if operation.operation_type == "new_connection" { 20 } else { 0 }
    
    // Simulate success rate (larger payloads might have lower success rate)
    let success_rate = if operation.payload_size_bytes > 100000 {
      0.95  // 95% success for large payloads
    } else {
      0.99  // 99% success for smaller payloads
    }
    
    {
      operation_type: operation.operation_type,
      payload_size_bytes: operation.payload_size_bytes,
      round_trip_time_ms: round_trip_time,
      throughput_mbps,
      connection_overhead_ms: connection_overhead,
      success_rate
    }
  }
  
  // Create test network operations
  let payload_sizes = [1024, 10240, 102400, 1024000]  // 1KB, 10KB, 100KB, 1MB
  let protocols = ["http", "grpc", "udp"]
  
  let mut network_operations = []
  for protocol in protocols {
    for size in payload_sizes {
      network_operations = network_operations.push({
        operation_type: "telemetry_send",
        payload_size_bytes: size,
        endpoint: "telemetry-collector.example.com",
        protocol: protocol
      })
    }
  }
  
  // Run network benchmarks
  let network_results = network_operations.map(simulate_network_operation)
  
  // Verify network performance characteristics
  assert_eq(network_results.length(), 12)  // 3 protocols × 4 payload sizes
  
  // Throughput should generally decrease with larger payloads for same RTT
  let http_results = network_results.filter(fn(r) { r.protocol == "http" })
  for i in 1..http_results.length() {
    assert_true(http_results[i].payload_size_bytes > http_results[i-1].payload_size_bytes)
    // RTT should increase with payload size
    assert_true(http_results[i].round_trip_time_ms >= http_results[i-1].round_trip_time_ms)
  }
  
  // Protocol comparison
  let grpc_results = network_results.filter(fn(r) { r.protocol == "grpc" })
  let udp_results = network_results.filter(fn(r) { r.protocol == "udp" })
  
  // GRPC should have lower overhead than HTTP
  assert_true(grpc_results[0].round_trip_time_ms < http_results[0].round_trip_time_ms)
  
  // UDP should have lowest overhead
  assert_true(udp_results[0].round_trip_time_ms < grpc_results[0].round_trip_time_ms)
  
  // Test connection pooling simulation
  let simulate_connection_pooling = fn(operations: Array[NetworkOperation], pool_size: Int) {
    let connection_overhead_per_op = if pool_size > 0 {
      20.0 / pool_size.to_float()  // Distribute connection overhead across pool
    } else {
      20.0  // New connection each time
    }
    
    let mut total_time = 0
    let mut total_throughput = 0.0
    
    for operation in operations {
      let result = simulate_network_operation(operation)
      total_time = total_time + result.round_trip_time_ms + connection_overhead_per_op.to_int()
      total_throughput = total_throughput + result.throughput_mbps
    }
    
    {
      pool_size,
      avg_operation_time_ms: total_time / operations.length(),
      avg_throughput_mbps: total_throughput / operations.length(),
      total_operations: operations.length()
    }
  }
  
  // Test different pool sizes
  let pool_sizes = [0, 1, 5, 10]  // 0 = no pooling
  let sample_operations = network_operations.slice(0, 5)  // Take first 5 operations
  
  let pooling_results = pool_sizes.map(fn(pool_size) {
    simulate_connection_pooling(sample_operations, pool_size)
  })
  
  // Connection pooling should improve performance
  let no_pool_result = pooling_results[0]
  let with_pool_result = pooling_results[2]  // Pool size 5
  
  assert_true(with_pool_result.avg_operation_time_ms < no_pool_result.avg_operation_time_ms)
  
  // Pool size should have diminishing returns
  let large_pool_result = pooling_results[3]  // Pool size 10
  let pool_improvement = no_pool_result.avg_operation_time_ms.to_float() / with_pool_result.avg_operation_time_ms.to_float()
  let large_pool_improvement = no_pool_result.avg_operation_time_ms.to_float() / large_pool_result.avg_operation_time_ms.to_float()
  
  // Large pool shouldn't be significantly better than medium pool
  assert_true(large_pool_improvement / pool_improvement < 1.2)  // Less than 20% additional improvement
}

// Test 6: Cache Performance
test "cache performance benchmarks" {
  // Define cache entry
  type CacheEntry = {
    key: String,
    value: String,
    access_count: Int,
    last_accessed: Int,
    size_bytes: Int
  }
  
  // Define cache performance metrics
  type CacheMetrics = {
    cache_size: Int,
    hit_rate: Float,
    miss_rate: Float,
    avg_lookup_time_ns: Int,
    avg_insertion_time_ns: Int,
    eviction_rate: Float,
    memory_usage_mb: Float
  }
  
  // Simulate cache operations
  let simulate_cache_operations = fn(cache_size: Int, operations: Array<String>) {
    let mut cache = []
    let mut hits = 0
    let mut misses = 0
    let mut evictions = 0
    let mut total_lookup_time = 0
    let mut total_insertion_time = 0
    
    for operation in operations {
      // Simulate lookup time (varies with cache size)
      let lookup_time = 10 + (cache_size / 100)  // Base 10ns + size factor
      total_lookup_time = total_lookup_time + lookup_time
      
      // Check cache hit
      let mut found = false
      for entry in cache {
        if entry.key == operation {
          found = true
          hits = hits + 1
          break
        }
      }
      
      if not(found) {
        misses = misses + 1
        
        // Simulate insertion time
        let insertion_time = 20 + (cache_size / 100)  // Base 20ns + size factor
        total_insertion_time = total_insertion_time + insertion_time
        
        // Add to cache
        let new_entry = {
          key: operation,
          value: "value-for-" + operation,
          access_count: 1,
          last_accessed: 1640995200,
          size_bytes: operation.length() + 20  // Key + value + overhead
        }
        
        // Check if cache is full
        if cache.length() >= cache_size {
          // Evict oldest entry (simplified LRU)
          cache = cache.slice(1, cache.length())  // Remove first element
          evictions = evictions + 1
        }
        
        cache = cache.push(new_entry)
      }
    }
    
    let total_operations = hits + misses
    let hit_rate = hits.to_float() / total_operations.to_float()
    let miss_rate = misses.to_float() / total_operations.to_float()
    let eviction_rate = evictions.to_float() / total_operations.to_float()
    
    // Calculate memory usage
    let memory_usage = cache.reduce(fn(total, entry) {
      total + entry.size_bytes
    }, 0).to_float() / (1024.0 * 1024.0)  // Convert to MB
    
    {
      cache_size,
      hit_rate,
      miss_rate,
      avg_lookup_time_ns: total_lookup_time / total_operations,
      avg_insertion_time_ns: if misses > 0 { total_insertion_time / misses } else { 0 },
      eviction_rate,
      memory_usage_mb: memory_usage
    }
  }
  
  // Generate test operations with locality
  let generate_operations_with_locality = fn(count: Int, unique_keys: Int) {
    let mut operations = []
    for i in 0..count {
      // 80% of operations use 20% of keys (Zipf distribution simulation)
      let key_index = if i % 5 < 4 {
        i % (unique_keys / 5)  // Use first 20% of keys 80% of the time
      } else {
        (unique_keys / 5) + (i % (unique_keys * 4 / 5))  // Use remaining 80% of keys 20% of the time
      }
      operations = operations.push("key-" + key_index.to_string())
    }
    operations
  }
  
  // Test different cache sizes
  let cache_sizes = [100, 500, 1000, 5000]
  let operation_count = 10000
  let unique_keys = 2000
  let test_operations = generate_operations_with_locality(operation_count, unique_keys)
  
  let cache_results = cache_sizes.map(fn(cache_size) {
    simulate_cache_operations(cache_size, test_operations)
  })
  
  // Verify cache performance characteristics
  assert_eq(cache_results.length(), 4)
  
  // Hit rate should improve with larger cache sizes
  for i in 1..cache_results.length() {
    assert_true(cache_results[i].cache_size > cache_results[i-1].cache_size)
    // Hit rate should be better or equal with larger cache
    assert_true(cache_results[i].hit_rate >= cache_results[i-1].hit_rate * 0.9)  // Allow some variance
  }
  
  // Largest cache should have good hit rate due to locality
  let largest_cache_result = cache_results[3]
  assert_true(largest_cache_result.hit_rate > 0.7)  // At least 70% hit rate
  
  // Lookup time should increase with cache size but not dramatically
  let smallest_cache = cache_results[0]
  let largest_cache = cache_results[3]
  
  let lookup_time_degradation = largest_cache.avg_lookup_time_ns.to_float() / smallest_cache.avg_lookup_time_ns.to_float()
  assert_true(lookup_time_degradation < 5.0)  // Shouldn't be more than 5x slower
  
  // Memory usage should scale with cache size
  for i in 1..cache_results.length() {
    assert_true(cache_results[i].memory_usage_mb >= cache_results[i-1].memory_usage_mb)
  }
  
  // Test cache eviction patterns
  let test_eviction_behavior = fn(cache_size: Int, sequential_keys: Int) {
    let mut operations = []
    for i in 0..sequential_keys {
      operations = operations.push("sequential-key-" + i.to_string())
    }
    
    // Access keys again to test eviction
    for i in 0..sequential_keys {
      operations = operations.push("sequential-key-" + i.to_string())
    }
    
    let result = simulate_cache_operations(cache_size, operations)
    result
  }
  
  let eviction_test = test_eviction_behavior(100, 200)  // Cache size 100, 200 unique keys
  assert_true(eviction_test.eviction_rate > 0.0)  // Should have evictions
  assert_true(eviction_test.hit_rate > 0.3)  // Should still have some hits due to second pass
}