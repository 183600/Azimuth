// Azimuth Memory Efficient Telemetry Tests
// This file contains test cases for memory-efficient telemetry data processing

// Test 1: Memory-Efficient Telemetry Data Structures
test "memory-efficient telemetry data structures" {
  // Define memory-efficient telemetry point using compact representation
  type CompactTelemetryPoint = {
    timestamp: Int,           // 8 bytes
    metric_id: Int,           // 4 bytes - instead of string
    value: Float,             // 8 bytes
    flags: Int,               // 4 bytes - bit flags for attributes
  }
  
  // Define attribute bit flags for memory efficiency
  let FLAG_SERVICE_API = 1      // 0b0001
  let FLAG_SERVICE_DB = 2       // 0b0010  
  let FLAG_SERVICE_CACHE = 4    // 0b0100
  let FLAG_ENV_PROD = 8         // 0b1000
  let FLAG_ENV_DEV = 16         // 0b10000
  let FLAG_ERROR = 32           // 0b100000
  let FLAG_CRITICAL = 64        // 0b1000000
  
  // Create memory-efficient telemetry batch
  type TelemetryBatch = {
    points: Array[CompactTelemetryPoint],
    batch_id: String,
    compression_ratio: Float
  }
  
  // Memory-efficient operations
  let create_compact_point = fn(timestamp: Int, metric_id: Int, value: Float, flags: Int) {
    { timestamp: timestamp, metric_id: metric_id, value: value, flags: flags }
  }
  
  let has_flag = fn(point: CompactTelemetryPoint, flag: Int) {
    (point.flags & flag) != 0
  }
  
  let set_flag = fn(point: CompactTelemetryPoint, flag: Int) {
    { point | flags: point.flags | flag }
  }
  
  let clear_flag = fn(point: CompactTelemetryPoint, flag: Int) {
    { point | flags: point.flags & ~flag }
  }
  
  // Test compact point creation and flag operations
  let point1 = create_compact_point(1640995200, 1001, 95.5, FLAG_SERVICE_API | FLAG_ENV_PROD)
  assert_eq(point1.timestamp, 1640995200)
  assert_eq(point1.metric_id, 1001)
  assert_eq(point1.value, 95.5)
  assert_true(has_flag(point1, FLAG_SERVICE_API))
  assert_true(has_flag(point1, FLAG_ENV_PROD))
  assert_false(has_flag(point1, FLAG_SERVICE_DB))
  assert_false(has_flag(point1, FLAG_ERROR))
  
  // Test flag setting and clearing
  let point2 = set_flag(point1, FLAG_ERROR)
  assert_true(has_flag(point2, FLAG_ERROR))
  assert_true(has_flag(point2, FLAG_SERVICE_API))  // Original flags preserved
  
  let point3 = clear_flag(point2, FLAG_ENV_PROD)
  assert_false(has_flag(point3, FLAG_ENV_PROD))
  assert_true(has_flag(point3, FLAG_ERROR))  // Other flags preserved
  
  // Test memory-efficient batch processing
  let points = [
    create_compact_point(1640995200, 1001, 95.5, FLAG_SERVICE_API | FLAG_ENV_PROD),
    create_compact_point(1640995300, 1002, 120.3, FLAG_SERVICE_DB | FLAG_ENV_PROD | FLAG_ERROR),
    create_compact_point(1640995400, 1003, 45.2, FLAG_SERVICE_CACHE | FLAG_ENV_DEV),
    create_compact_point(1640995500, 1004, 200.1, FLAG_SERVICE_API | FLAG_ENV_PROD | FLAG_CRITICAL),
    create_compact_point(1640995600, 1005, 78.9, FLAG_SERVICE_DB | FLAG_ENV_DEV)
  ]
  
  let batch = {
    points: points,
    batch_id: "batch-12345",
    compression_ratio: 0.65
  }
  
  assert_eq(batch.points.length(), 5)
  assert_eq(batch.batch_id, "batch-12345")
  assert_eq(batch.compression_ratio, 0.65)
  
  // Memory-efficient filtering using bit flags
  let filter_by_flags = fn(batch: TelemetryBatch, required_flags: Int) {
    batch.points.filter_fn(p) { (p.flags & required_flags) == required_flags }
  }
  
  let prod_points = filter_by_flags(batch, FLAG_ENV_PROD)
  assert_eq(prod_points.length(), 3)
  
  let error_points = filter_by_flags(batch, FLAG_ERROR)
  assert_eq(error_points.length(), 1)
  
  let api_prod_points = filter_by_flags(batch, FLAG_SERVICE_API | FLAG_ENV_PROD)
  assert_eq(api_prod_points.length(), 2)
  
  // Memory-efficient aggregation
  let aggregate_by_metric_id = fn(points: Array[CompactTelemetryPoint]) {
    let mut aggregations = Map::empty()
    
    for point in points {
      let current = match Map::get(aggregations, point.metric_id) {
        Some((count, sum, min, max)) => (count, sum, min, max)
        None => (0, 0.0, point.value, point.value)
      }
      
      let updated = (
        current.0 + 1,
        current.1 + point.value,
        if point.value < current.2 { point.value } else { current.2 },
        if point.value > current.3 { point.value } else { current.3 }
      )
      
      let _ = Map::insert(aggregations, point.metric_id, updated)
    }
    
    aggregations
  }
  
  let aggregations = aggregate_by_metric_id(batch.points)
  assert_eq(Map::size(aggregations), 5)
  
  // Memory-efficient streaming processing
  let process_stream = fn(points: Array[CompactTelemetryPoint>, window_size: Int) {
    let mut results = []
    let mut i = 0
    
    while i < points.length() {
      let end = if i + window_size < points.length() { i + window_size } else { points.length() }
      let window = points.slice(i, end)
      
      let window_sum = window.fold(0.0, fn(acc, p) { acc + p.value })
      let window_avg = window_sum / (window.length() as Float)
      
      results = results.push({
        start_index: i,
        end_index: end - 1,
        count: window.length(),
        average: window_avg
      })
      
      i = i + window_size
    }
    
    results
  }
  
  let window_results = process_stream(batch.points, 2)
  assert_eq(window_results.length(), 3)  // 5 points with window size 2 = 3 windows
  assert_eq(window_results[0].count, 2)
  assert_eq(window_results[1].count, 2) 
  assert_eq(window_results[2].count, 1)  // Last window with remaining point
}

// Test 2: Memory Pool Management for Telemetry
test "memory pool management for telemetry" {
  // Define memory pool for telemetry objects
  type TelemetryMemoryPool = {
    pool_size: Int,
    used_count: Int,
    free_indices: Array[Int>,
    telemetry_data: Array[Array[String]>  // Pooled telemetry data
  }
  
  // Create memory pool
  let create_memory_pool = fn(size: Int) {
    let mut free_indices = []
    let mut i = 0
    while i < size {
      free_indices = free_indices.push(i)
      i = i + 1
    }
    
    {
      pool_size: size,
      used_count: 0,
      free_indices: free_indices,
      telemetry_data: Array::with_capacity(size)
    }
  }
  
  // Allocate from pool
  let allocate_from_pool = fn(pool: TelemetryMemoryPool, data: Array[String]) {
    if pool.free_indices.length() > 0 {
      let index = pool.free_indices[0]
      let remaining_free = pool.free_indices.slice(1, pool.free_indices.length())
      
      let updated_pool = {
        pool_size: pool.pool_size,
        used_count: pool.used_count + 1,
        free_indices: remaining_free,
        telemetry_data: pool.telemetry_data.set(index, data)
      }
      
      (Some(index), updated_pool)
    } else {
      (None, pool)
    }
  }
  
  // Deallocate to pool
  let deallocate_to_pool = fn(pool: TelemetryMemoryPool, index: Int) {
    if index >= 0 && index < pool.pool_size {
      let updated_pool = {
        pool_size: pool.pool_size,
        used_count: pool.used_count - 1,
        free_indices: pool.free_indices.push(index),
        telemetry_data: pool.telemetry_data.set(index, [])
      }
      updated_pool
    } else {
      pool
    }
  }
  
  // Test pool creation and allocation
  let pool = create_memory_pool(5)
  assert_eq(pool.pool_size, 5)
  assert_eq(pool.used_count, 0)
  assert_eq(pool.free_indices.length(), 5)
  
  let telemetry_data1 = ["metric=cpu", "value=80.5", "timestamp=1640995200"]
  let (index1, pool1) = allocate_from_pool(pool, telemetry_data1)
  
  match index1 {
    Some(idx) => {
      assert_true(idx >= 0 && idx < 5)
      assert_eq(pool1.used_count, 1)
      assert_eq(pool1.free_indices.length(), 4)
    }
    None => assert_true(false)
  }
  
  // Test multiple allocations
  let telemetry_data2 = ["metric=memory", "value=60.2", "timestamp=1640995300"]
  let telemetry_data3 = ["metric=disk", "value=45.8", "timestamp=1640995400"]
  
  let (index2, pool2) = allocate_from_pool(pool1, telemetry_data2)
  let (index3, pool3) = allocate_from_pool(pool2, telemetry_data3)
  
  match (index2, index3) {
    (Some(idx2), Some(idx3)) => {
      assert_true(idx2 != idx3)  // Different indices
      assert_eq(pool3.used_count, 3)
      assert_eq(pool3.free_indices.length(), 2)
    }
    _ => assert_true(false)
  }
  
  // Test deallocation
  let pool4 = deallocate_to_pool(pool3, index1.unwrap())
  assert_eq(pool4.used_count, 2)
  assert_eq(pool4.free_indices.length(), 3)
  assert_true(pool4.free_indices.contains(index1.unwrap()))
  
  // Test pool exhaustion
  let mut current_pool = pool4
  let mut allocated_indices = []
  
  // Allocate remaining slots
  for i in 0..2 {
    let data = ["test=data" + i.to_string()]
    let (index, new_pool) = allocate_from_pool(current_pool, data)
    match index {
      Some(idx) => {
        allocated_indices = allocated_indices.push(idx)
        current_pool = new_pool
      }
      None => assert_true(false)
    }
  }
  
  assert_eq(current_pool.used_count, 5)  // Pool is full
  assert_eq(current_pool.free_indices.length(), 0)
  
  // Try to allocate from full pool
  let (overflow_index, overflow_pool) = allocate_from_pool(current_pool, ["overflow=data"])
  assert_eq(overflow_index, None)
  assert_eq(overflow_pool.used_count, 5)  // Pool unchanged
  
  // Test deallocation and reuse
  let pool5 = deallocate_to_pool(overflow_pool, allocated_indices[0])
  assert_eq(pool5.used_count, 4)
  assert_eq(pool5.free_indices.length(), 1)
  
  // Reallocate freed slot
  let (reuse_index, pool6) = allocate_from_pool(pool5, ["reused=data"])
  match reuse_index {
    Some(idx) => {
      assert_eq(idx, allocated_indices[0])  // Reused the same index
      assert_eq(pool6.used_count, 5)
      assert_eq(pool6.free_indices.length(), 0)
    }
    None => assert_true(false)
  }
}

// Test 3: Memory-Efficient Telemetry Compression
test "memory-efficient telemetry compression" {
  // Define compression strategies
  enum CompressionStrategy {
    None
    Dictionary(Array[(String, String)])  // value -> code mapping
    RunLength                            // For repeated values
    Delta                                // For sequential timestamps
  }
  
  // Define compressed telemetry data
  type CompressedTelemetry = {
    original_size: Int,
    compressed_size: Int,
    strategy: CompressionStrategy,
    data: Array[Int]  // Compressed representation
  }
  
  // Dictionary compression for string values
  let create_dictionary_compression = fn(strings: Array[String]) {
    let mut dictionary = Map::empty()
    let mut compressed = []
    let mut code_counter = 0
    
    for str in strings {
      match Map::get(dictionary, str) {
        Some(code) => {
          compressed = compressed.push(code)
        }
        None => {
          let _ = Map::insert(dictionary, str, code_counter)
          compressed = compressed.push(code_counter)
          code_counter = code_counter + 1
        }
      }
    }
    
    let dict_array = Map::to_array(dictionary)
    (CompressedTelemetry {
      original_size: strings.length() * 10,  // Assume average 10 chars per string
      compressed_size: compressed.length() * 4,  // 4 bytes per int
      strategy: CompressionStrategy::Dictionary(dict_array),
      data: compressed
    }, dict_array)
  }
  
  // Run-length compression for repeated values
  let run_length_compress = fn(values: Array[Int]) {
    if values.length() == 0 {
      return CompressedTelemetry {
        original_size: 0,
        compressed_size: 0,
        strategy: CompressionStrategy::RunLength,
        data: []
      }
    }
    
    let mut compressed = []
    let mut current_value = values[0]
    let mut count = 1
    
    for i in 1..values.length() {
      if values[i] == current_value {
        count = count + 1
      } else {
        compressed = compressed.push(current_value)
        compressed = compressed.push(count)
        current_value = values[i]
        count = 1
      }
    }
    
    // Add last run
    compressed = compressed.push(current_value)
    compressed = compressed.push(count)
    
    CompressedTelemetry {
      original_size: values.length() * 4,
      compressed_size: compressed.length() * 4,
      strategy: CompressionStrategy::RunLength,
      data: compressed
    }
  }
  
  // Delta compression for sequential timestamps
  let delta_compress = fn(timestamps: Array[Int]) {
    if timestamps.length() == 0 {
      return CompressedTelemetry {
        original_size: 0,
        compressed_size: 0,
        strategy: CompressionStrategy::Delta,
        data: []
      }
    }
    
    let mut compressed = [timestamps[0]]  // First timestamp as-is
    let mut prev = timestamps[0]
    
    for i in 1..timestamps.length() {
      let delta = timestamps[i] - prev
      compressed = compressed.push(delta)
      prev = timestamps[i]
    }
    
    CompressedTelemetry {
      original_size: timestamps.length() * 4,
      compressed_size: compressed.length() * 4,
      strategy: CompressionStrategy::Delta,
      data: compressed
    }
  }
  
  // Test dictionary compression
  let telemetry_strings = [
    "service=api",
    "service=database", 
    "service=api",
    "service=cache",
    "service=api",
    "service=database",
    "service=api"
  ]
  
  let (dict_compressed, dictionary) = create_dictionary_compression(telemetry_strings)
  
  match dict_compressed.strategy {
    CompressionStrategy::Dictionary(dict) => {
      assert_eq(dict.length(), 3)  // 3 unique strings
      assert_eq(dict_compressed.data.length(), 7)  // Same as original
      assert_true(dict_compressed.compressed_size < dict_compressed.original_size)
    }
    _ => assert_true(false)
  }
  
  // Test run-length compression
  let repeated_values = [100, 100, 100, 200, 200, 300, 300, 300, 300]
  let rl_compressed = run_length_compress(repeated_values)
  
  match rl_compressed.strategy {
    CompressionStrategy::RunLength => {
      assert_eq(rl_compressed.data.length(), 6)  // 3 value-count pairs
      assert_eq(rl_compressed.data[0], 100)
      assert_eq(rl_compressed.data[1], 3)
      assert_eq(rl_compressed.data[2], 200)
      assert_eq(rl_compressed.data[3], 2)
      assert_eq(rl_compressed.data[4], 300)
      assert_eq(rl_compressed.data[5], 4)
      assert_true(rl_compressed.compressed_size < rl_compressed.original_size)
    }
    _ => assert_true(false)
  }
  
  // Test delta compression
  let sequential_timestamps = [1000, 1010, 1020, 1030, 1040, 1050]
  let delta_compressed = delta_compress(sequential_timestamps)
  
  match delta_compressed.strategy {
    CompressionStrategy::Delta => {
      assert_eq(delta_compressed.data[0], 1000)  // First timestamp
      assert_eq(delta_compressed.data[1], 10)   // Delta
      assert_eq(delta_compressed.data[2], 10)   // Delta
      assert_eq(delta_compressed.data[3], 10)   // Delta
      assert_eq(delta_compressed.data[4], 10)   // Delta
      assert_eq(delta_compressed.data[5], 10)   // Delta
      assert_eq(delta_compressed.data.length(), 6)
    }
    _ => assert_true(false)
  }
  
  // Test compression ratio calculation
  let compression_ratio = fn(compressed: CompressedTelemetry) {
    if compressed.original_size > 0 {
      (compressed.compressed_size as Float) / (compressed.original_size as Float)
    } else {
      1.0
    }
  }
  
  let dict_ratio = compression_ratio(dict_compressed)
  let rl_ratio = compression_ratio(rl_compressed)
  let delta_ratio = compression_ratio(delta_compressed)
  
  assert_true(dict_ratio < 1.0)  // Compression achieved
  assert_true(rl_ratio < 1.0)   // Compression achieved
  assert_true(delta_ratio <= 1.0)  // At worst no compression
  
  // Test best compression strategy selection
  let select_best_compression = fn(data: Array[String]) {
    // Convert strings to integers for run-length and delta
    let int_data = data.map_fn(s) { s.length() }
    
    let (dict_comp, _) = create_dictionary_compression(data)
    let rl_comp = run_length_compress(int_data)
    let delta_comp = delta_compress(int_data)
    
    let dict_ratio = compression_ratio(dict_comp)
    let rl_ratio = compression_ratio(rl_comp)
    let delta_ratio = compression_ratio(delta_comp)
    
    if dict_ratio <= rl_ratio && dict_ratio <= delta_ratio {
      dict_comp
    } else if rl_ratio <= delta_ratio {
      rl_comp
    } else {
      delta_comp
    }
  }
  
  let best_compressed = select_best_compression(telemetry_strings)
  let best_ratio = compression_ratio(best_compressed)
  
  // Best compression should be at least as good as dictionary compression
  assert_true(best_ratio <= dict_ratio)
}

// Test 4: Memory-Efficient Telemetry Streaming
test "memory-efficient telemetry streaming" {
  // Define streaming telemetry processor
  type StreamProcessor = {
    buffer_size: Int,
    processed_count: Int,
    memory_usage: Int,
    buffer: Array[String]
  }
  
  // Create stream processor
  let create_stream_processor = fn(buffer_size: Int) {
    {
      buffer_size: buffer_size,
      processed_count: 0,
      memory_usage: 0,
      buffer: []
    }
  }
  
  // Process telemetry data in streaming fashion
  let process_stream_data = fn(processor: StreamProcessor, data: Array[String]) {
    let mut current_processor = processor
    let mut results = []
    
    for item in data {
      // Add item to buffer
      current_processor.buffer = current_processor.buffer.push(item)
      current_processor.memory_usage = current_processor.memory_usage + item.length()
      
      // Process buffer if full
      if current_processor.buffer.length() >= current_processor.buffer_size {
        let processed_batch = current_processor.buffer.map_fn(item) {
          "processed:" + item
        }
        
        results = results.concat(processed_batch)
        
        // Clear buffer and update memory usage
        let buffer_memory = current_processor.buffer.fold(0, fn(acc, item) { acc + item.length() })
        current_processor.buffer = []
        current_processor.memory_usage = current_processor.memory_usage - buffer_memory
        current_processor.processed_count = current_processor.processed_count + current_processor.buffer_size
      }
    }
    
    // Process remaining items in buffer
    if current_processor.buffer.length() > 0 {
      let remaining_processed = current_processor.buffer.map_fn(item) {
        "processed:" + item
      }
      results = results.concat(remaining_processed)
      current_processor.processed_count = current_processor.processed_count + current_processor.buffer.length()
      current_processor.buffer = []
      current_processor.memory_usage = 0
    }
    
    (results, current_processor)
  }
  
  // Test streaming processing
  let processor = create_stream_processor(3)
  assert_eq(processor.buffer_size, 3)
  assert_eq(processor.processed_count, 0)
  assert_eq(processor.memory_usage, 0)
  assert_eq(processor.buffer.length(), 0)
  
  let telemetry_data = [
    "metric1:value1",
    "metric2:value2", 
    "metric3:value3",
    "metric4:value4",
    "metric5:value5",
    "metric6:value6",
    "metric7:value7"
  ]
  
  let (processed_results, final_processor) = process_stream_data(processor, telemetry_data)
  
  assert_eq(processed_results.length(), 7)
  assert_true(processed_results[0].starts_with("processed:"))
  assert_eq(final_processor.processed_count, 7)
  assert_eq(final_processor.memory_usage, 0)
  assert_eq(final_processor.buffer.length(), 0)
  
  // Test memory usage tracking
  let process_with_memory_tracking = fn(processor: StreamProcessor, data: Array[String]) {
    let mut max_memory = 0
    let mut current_processor = processor
    
    for item in data {
      current_processor.buffer = current_processor.buffer.push(item)
      current_processor.memory_usage = current_processor.memory_usage + item.length()
      
      if current_processor.memory_usage > max_memory {
        max_memory = current_processor.memory_usage
      }
      
      if current_processor.buffer.length() >= current_processor.buffer_size {
        let buffer_memory = current_processor.buffer.fold(0, fn(acc, item) { acc + item.length() })
        current_processor.buffer = []
        current_processor.memory_usage = current_processor.memory_usage - buffer_memory
        current_processor.processed_count = current_processor.processed_count + current_processor.buffer_size
      }
    }
    
    (max_memory, current_processor)
  }
  
  let (max_memory_mem, mem_processor) = process_with_memory_tracking(processor, telemetry_data)
  assert_true(max_memory_mem > 0)
  
  // Test adaptive buffer sizing based on memory constraints
  let adaptive_buffer_size = fn(data: Array[String], memory_limit: Int) {
    let mut buffer_size = 1
    let mut best_size = 1
    
    while buffer_size <= data.length() {
      let test_processor = create_stream_processor(buffer_size)
      let (max_mem, _) = process_with_memory_tracking(test_processor, data)
      
      if max_mem <= memory_limit {
        best_size = buffer_size
      } else {
        break
      }
      
      buffer_size = buffer_size * 2
    }
    
    best_size
  }
  
  let adaptive_size = adaptive_buffer_size(telemetry_data, 100)
  assert_true(adaptive_size >= 1)
  assert_true(adaptive_size <= telemetry_data.length())
  
  // Test with adaptive buffer size
  let adaptive_processor = create_stream_processor(adaptive_size)
  let (adaptive_results, _) = process_stream_data(adaptive_processor, telemetry_data)
  
  assert_eq(adaptive_results.length(), telemetry_data.length())
}