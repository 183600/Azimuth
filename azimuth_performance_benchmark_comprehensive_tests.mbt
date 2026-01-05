// Azimuth Performance Benchmark Comprehensive Test Suite
// This file contains comprehensive tests for performance benchmarking in the Azimuth telemetry system

// Test 1: Telemetry Data Processing Performance
test "telemetry data processing performance" {
  // Performance measurement utilities
  type PerformanceMetrics = {
    operation_count: Int,
    total_time: Int,
    average_time: Float,
    min_time: Int,
    max_time: Int,
    throughput: Float
  }
  
  let measure_performance = fn(operation: () -> Unit, iterations: Int) {
    let start_time = 1640995200  // Simulated timestamp
    let mut times = []
    
    for i in 0..iterations {
      let op_start = 1640995200 + i
      operation()
      let op_end = 1640995200 + i + 1  // Simulated operation time
      times = times.push(op_end - op_start)
    }
    
    let total_time = times.reduce(fn(acc, time) { acc + time }, 0)
    let average_time = total_time.to_float() / iterations.to_float()
    let min_time = times.reduce(fn(acc, time) { if time < acc { time } else { acc } }, times[0])
    let max_time = times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, times[0])
    let throughput = iterations.to_float() / total_time.to_float()
    
    {
      operation_count: iterations,
      total_time,
      average_time,
      min_time,
      max_time,
      throughput
    }
  }
  
  // Test telemetry span creation performance
  let create_span = fn(name: String, trace_id: String, parent_span_id: Option[String]) {
    let span_id = "span-" + (1640995200 + name.length()).to_string()
    let start_time = 1640995200
    
    {
      span_id,
      trace_id,
      parent_span_id,
      name,
      start_time,
      end_time: None,
      status: "running",
      attributes: []
    }
  }
  
  let span_creation_op = fn() {
    let span = create_span("database_query", "trace-12345", Some("span-67890"))
    span.span_id.length()  // Simulate some work with the span
  }
  
  let span_creation_metrics = measure_performance(span_creation_op, 1000)
  
  assert_eq(span_creation_metrics.operation_count, 1000)
  assert_true(span_creation_metrics.total_time > 0)
  assert_true(span_creation_metrics.average_time > 0.0)
  assert_true(span_creation_metrics.min_time > 0)
  assert_true(span_creation_metrics.max_time >= span_creation_metrics.min_time)
  assert_true(span_creation_metrics.throughput > 0.0)
  
  // Test telemetry data serialization performance
  let serialize_span = fn(span: {span_id: String, trace_id: String, name: String}) {
    span.span_id + "|" + span.trace_id + "|" + span.name
  }
  
  let test_span = {
    span_id: "span-12345",
    trace_id: "trace-67890",
    name: "database_query"
  }
  
  let serialization_op = fn() {
    let serialized = serialize_span(test_span)
    serialized.length()  // Simulate some work with the serialized data
  }
  
  let serialization_metrics = measure_performance(serialization_op, 1000)
  
  assert_eq(serialization_metrics.operation_count, 1000)
  assert_true(serialization_metrics.total_time > 0)
  assert_true(serialization_metrics.average_time > 0.0)
  assert_true(serialization_metrics.throughput > 0.0)
  
  // Compare serialization vs creation performance
  let creation_vs_serialization = span_creation_metrics.average_time / serialization_metrics.average_time
  assert_true(creation_vs_serialization > 0.0)
}

// Test 2: Memory Usage Benchmarking
test "memory usage benchmarking" {
  // Memory usage tracking
  type MemoryUsage = {
    allocated_objects: Int,
    total_memory: Int,
    peak_memory: Int,
    gc_count: Int
  }
  
  let track_memory_usage = fn(operation: () -> Unit) {
    let initial_memory = 1024 * 1024  // 1MB initial memory
    let initial_objects = 100  // Initial object count
    
    operation()
    
    let final_memory = initial_memory + 512 * 1024  // 512KB additional memory
    let final_objects = initial_objects + 50  // 50 additional objects
    
    {
      allocated_objects: final_objects - initial_objects,
      total_memory: final_memory,
      peak_memory: final_memory + 256 * 1024,  // Peak was 256KB higher
      gc_count: 2  // Simulated GC runs
    }
  }
  
  // Test memory usage for array operations
  let array_operation = fn() {
    let mut large_array = []
    for i in 0..1000 {
      large_array = large_array.push("telemetry-data-" + i.to_string())
    }
    
    let mut sum = 0
    for item in large_array {
      sum = sum + item.length()
    }
    
    sum  // Return to avoid optimization
  }
  
  let array_memory_usage = track_memory_usage(array_operation)
  
  assert_true(array_memory_usage.allocated_objects > 0)
  assert_true(array_memory_usage.total_memory > 1024 * 1024)
  assert_true(array_memory_usage.peak_memory >= array_memory_usage.total_memory)
  assert_true(array_memory_usage.gc_count >= 0)
  
  // Test memory usage for string operations
  let string_operation = fn() {
    let mut result = ""
    for i in 0..100 {
      result = result + "telemetry-data-" + i.to_string() + "-"
    }
    result.length()
  }
  
  let string_memory_usage = track_memory_usage(string_operation)
  
  assert_true(string_memory_usage.allocated_objects > 0)
  assert_true(string_memory_usage.total_memory > 1024 * 1024)
  
  // Compare memory efficiency
  let memory_efficiency_ratio = array_memory_usage.allocated_objects.to_float() / string_memory_usage.allocated_objects.to_float()
  assert_true(memory_efficiency_ratio > 0.0)
}

// Test 3: Concurrent Processing Performance
test "concurrent processing performance" {
  // Concurrent processing simulation
  type ConcurrentTask = {
    task_id: Int,
    data: String,
    processing_time: Int
  }
  
  type ConcurrentResult = {
    task_id: Int,
    result: String,
    execution_time: Int
  }
  
  let process_task = fn(task: ConcurrentTask) {
    // Simulate processing time
    let start_time = 1640995200
    
    // Simulate some work
    let mut result = ""
    for i in 0..task.data.length() {
      result = result + task.data[i].to_string()
    }
    
    let end_time = start_time + task.processing_time
    
    {
      task_id: task.task_id,
      result: result,
      execution_time: end_time - start_time
    }
  }
  
  let process_tasks_sequentially = fn(tasks: Array[ConcurrentTask]) {
    let mut results = []
    let start_time = 1640995200
    
    for task in tasks {
      let result = process_task(task)
      results = results.push(result)
    }
    
    let end_time = 1640995200 + tasks.reduce(fn(acc, task) { acc + task.processing_time }, 0)
    
    {
      results,
      total_time: end_time - start_time
    }
  }
  
  let process_tasks_concurrently = fn(tasks: Array[ConcurrentTask]) {
    let mut results = []
    let start_time = 1640995200
    
    // Simulate concurrent processing by finding the max processing time
    let max_processing_time = tasks.reduce(fn(acc, task) { 
      if task.processing_time > acc { task.processing_time } else { acc } 
    }, 0)
    
    // Process all tasks (simulated)
    for task in tasks {
      let result = process_task(task)
      results = results.push(result)
    }
    
    let end_time = start_time + max_processing_time
    
    {
      results,
      total_time: end_time - start_time
    }
  }
  
  // Create test tasks
  let mut tasks = []
  for i in 0..10 {
    tasks = tasks.push({
      task_id: i,
      data: "telemetry-data-" + i.to_string(),
      processing_time: 100 + i * 10  // Varying processing times
    })
  }
  
  // Test sequential processing
  let sequential_result = process_tasks_sequentially(tasks)
  assert_eq(sequential_result.results.length(), 10)
  
  // Test concurrent processing
  let concurrent_result = process_tasks_concurrently(tasks)
  assert_eq(concurrent_result.results.length(), 10)
  
  // Verify performance improvement
  assert_true(concurrent_result.total_time < sequential_result.total_time)
  
  // Calculate speedup
  let speedup = sequential_result.total_time.to_float() / concurrent_result.total_time.to_float()
  assert_true(speedup > 1.0)
  
  // Verify all tasks completed correctly
  for i in 0..tasks.length() {
    assert_eq(sequential_result.results[i].task_id, i)
    assert_eq(concurrent_result.results[i].task_id, i)
  }
}

// Test 4: Data Structure Performance Comparison
test "data structure performance comparison" {
  // Compare performance of different data structures
  
  // Array operations
  let array_operations = fn() {
    let mut array = []
    
    // Insert elements
    for i in 0..1000 {
      array = array.push(i)
    }
    
    // Search elements
    let mut found = 0
    for i in 0..100 {
      for j in 0..array.length() {
        if array[j] == i * 10 {
          found = found + 1
          break
        }
      }
    }
    
    // Remove elements (by creating new array)
    let mut filtered = []
    for item in array {
      if item % 2 == 0 {
        filtered = filtered.push(item)
      }
    }
    
    found + filtered.length()
  }
  
  // Simulated hash table operations
  let hash_table_operations = fn() {
    let mut hash_table = {}
    
    // Insert elements
    for i in 0..1000 {
      let key = "key-" + i.to_string()
      let value = i
      hash_table = hash_table.insert(key, value)
    }
    
    // Search elements
    let mut found = 0
    for i in 0..100 {
      let key = "key-" + (i * 10).to_string()
      match hash_table[key] {
        Some(_) => found = found + 1
        None => {}
      }
    }
    
    // Remove elements
    let mut filtered_table = {}
    for (key, value) in hash_table {
      if value % 2 == 0 {
        filtered_table = filtered_table.insert(key, value)
      }
    }
    
    found + filtered_table.size()
  }
  
  // Tree operations (simplified binary tree)
  type TreeNode = {
    value: Int,
    left: Option[TreeNode],
    right: Option[TreeNode]
  }
  
  let tree_insert = fn(node: Option[TreeNode], value: Int) {
    match node {
      None => Some({
        value,
        left: None,
        right: None
      })
      Some(n) => {
        if value < n.value {
          Some({ n | left: tree_insert(n.left, value) })
        } else if value > n.value {
          Some({ n | right: tree_insert(n.right, value) })
        } else {
          Some(n)  // Duplicate, ignore
        }
      }
    }
  }
  
  let tree_search = fn(node: Option[TreeNode], value: Int) {
    match node {
      None => false
      Some(n) => {
        if value == n.value {
          true
        } else if value < n.value {
          tree_search(n.left, value)
        } else {
          tree_search(n.right, value)
        }
      }
    }
  }
  
  let tree_operations = fn() {
    let mut tree = None
    
    // Insert elements
    for i in 0..1000 {
      tree = tree_insert(tree, i)
    }
    
    // Search elements
    let mut found = 0
    for i in 0..100 {
      if tree_search(tree, i * 10) {
        found = found + 1
      }
    }
    
    found
  }
  
  // Measure performance of each data structure
  let measure_time = fn(operation: () -> Int) {
    let start_time = 1640995200
    let result = operation()
    let end_time = start_time + 100  // Simulated execution time
    (result, end_time - start_time)
  }
  
  let (array_result, array_time) = measure_time(array_operations)
  let (hash_result, hash_time) = measure_time(hash_table_operations)
  let (tree_result, tree_time) = measure_time(tree_operations)
  
  // Verify all operations completed successfully
  assert_true(array_result > 0)
  assert_true(hash_result > 0)
  assert_true(tree_result > 0)
  
  // Compare performance
  assert_true(array_time > 0)
  assert_true(hash_time > 0)
  assert_true(tree_time > 0)
  
  // Hash table should be faster for search operations
  let array_vs_hash_speedup = array_time.to_float() / hash_time.to_float()
  assert_true(array_vs_hash_speedup > 1.0)
  
  // Tree should be faster than array for search
  let array_vs_tree_speedup = array_time.to_float() / tree_time.to_float()
  assert_true(array_vs_tree_speedup > 1.0)
}

// Test 5: Algorithm Complexity Benchmarking
test "algorithm complexity benchmarking" {
  // Compare O(n) vs O(n^2) vs O(log n) algorithms
  
  // Linear search - O(n)
  let linear_search = fn(arr: Array[Int], target: Int) {
    for i in 0..arr.length() {
      if arr[i] == target {
        return Some(i)
      }
    }
    None
  }
  
  // Binary search - O(log n)
  let binary_search = fn(arr: Array[Int], target: Int) {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = left + (right - left) / 2
      
      if arr[mid] == target {
        return Some(mid)
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    None
  }
  
  // Bubble sort - O(n^2)
  let bubble_sort = fn(arr: Array[Int]) {
    let mut result = arr.clone()
    let n = result.length()
    
    for i in 0..n {
      for j in 0..(n - i - 1) {
        if result[j] > result[j + 1] {
          let temp = result[j]
          result[j] = result[j + 1]
          result[j + 1] = temp
        }
      }
    }
    
    result
  }
  
  // Quick sort - O(n log n) average
  let quick_sort = fn(arr: Array[Int]) {
    if arr.length() <= 1 {
      arr
    } else {
      let pivot = arr[0]
      let mut less = []
      let mut equal = []
      let mut greater = []
      
      for item in arr {
        if item < pivot {
          less = less.push(item)
        } else if item > pivot {
          greater = greater.push(item)
        } else {
          equal = equal.push(item)
        }
      }
      
      quick_sort(less) + equal + quick_sort(greater)
    }
  }
  
  // Create test data
  let mut sorted_data = []
  for i in 0..1000 {
    sorted_data = sorted_data.push(i)
  }
  
  let mut unsorted_data = []
  for i in 0..1000 {
    unsorted_data = unsorted_data.push((999 - i))  // Reverse order
  }
  
  // Benchmark search algorithms
  let benchmark_search = fn(search_fn: (Array[Int], Int) -> Option[Int], data: Array[Int], target: Int) {
    let start_time = 1640995200
    let result = search_fn(data, target)
    let end_time = start_time + 10  // Simulated execution time
    
    match result {
      Some(index) => (index, end_time - start_time)
      None => (-1, end_time - start_time)
    }
  }
  
  let (linear_index, linear_time) = benchmark_search(linear_search, sorted_data, 500)
  let (binary_index, binary_time) = benchmark_search(binary_search, sorted_data, 500)
  
  // Verify both found the target
  assert_eq(linear_index, 500)
  assert_eq(binary_index, 500)
  
  // Binary search should be faster
  assert_true(binary_time < linear_time)
  
  // Benchmark sorting algorithms
  let benchmark_sort = fn(sort_fn: (Array[Int]) -> Array[Int], data: Array[Int]) {
    let start_time = 1640995200
    let result = sort_fn(data)
    let end_time = start_time + 100  // Simulated execution time
    
    (result, end_time - start_time)
  }
  
  let (bubble_sorted, bubble_time) = benchmark_sort(bubble_sort, unsorted_data.clone())
  let (quick_sorted, quick_time) = benchmark_sort(quick_sort, unsorted_data)
  
  // Verify both sorted correctly
  assert_eq(bubble_sorted[0], 0)
  assert_eq(bubble_sorted[999], 999)
  assert_eq(quick_sorted[0], 0)
  assert_eq(quick_sorted[999], 999)
  
  // Quick sort should be faster
  assert_true(quick_time < bubble_time)
  
  // Calculate speedup
  let sort_speedup = bubble_time.to_float() / quick_time.to_float()
  assert_true(sort_speedup > 1.0)
  
  let search_speedup = linear_time.to_float() / binary_time.to_float()
  assert_true(search_speedup > 1.0)
}

// Test 6: I/O Performance Benchmarking
test "io performance benchmarking" {
  // Simulate I/O operations performance
  
  // File read simulation
  let simulate_file_read = fn(file_size: Int) {
    let start_time = 1640995200
    
    // Simulate reading data in chunks
    let chunk_size = 1024  // 1KB chunks
    let chunks = file_size / chunk_size
    let mut total_read = 0
    
    for i in 0..chunks {
      // Simulate reading a chunk
      total_read = total_read + chunk_size
    }
    
    // Handle remaining bytes
    if file_size % chunk_size > 0 {
      total_read = total_read + (file_size % chunk_size)
    }
    
    let end_time = start_time + (file_size / 1024)  // 1ms per KB
    
    (total_read, end_time - start_time)
  }
  
  // File write simulation
  let simulate_file_write = fn(data_size: Int) {
    let start_time = 1640995200
    
    // Simulate writing data in chunks
    let chunk_size = 1024  // 1KB chunks
    let chunks = data_size / chunk_size
    let mut total_written = 0
    
    for i in 0..chunks {
      // Simulate writing a chunk
      total_written = total_written + chunk_size
    }
    
    // Handle remaining bytes
    if data_size % chunk_size > 0 {
      total_written = total_written + (data_size % chunk_size)
    }
    
    let end_time = start_time + (data_size / 512)  // 2ms per KB (slower than read)
    
    (total_written, end_time - start_time)
  }
  
  // Network transmission simulation
  let simulate_network_transmission = fn(data_size: Int, packet_size: Int) {
    let start_time = 1640995200
    
    // Simulate transmitting data in packets
    let packets = data_size / packet_size
    let mut total_transmitted = 0
    
    for i in 0..packets {
      // Simulate network latency per packet
      total_transmitted = total_transmitted + packet_size
    }
    
    // Handle remaining data
    if data_size % packet_size > 0 {
      total_transmitted = total_transmitted + (data_size % packet_size)
    }
    
    // Network transmission is slower with more packets
    let latency_per_packet = 5  // 5ms per packet
    let end_time = start_time + (packets * latency_per_packet)
    
    (total_transmitted, end_time - start_time)
  }
  
  // Test different file sizes
  let small_file = 1024 * 10      // 10KB
  let medium_file = 1024 * 1024   // 1MB
  let large_file = 1024 * 1024 * 10  // 10MB
  
  // Benchmark file reads
  let (small_read, small_read_time) = simulate_file_read(small_file)
  let (medium_read, medium_read_time) = simulate_file_read(medium_file)
  let (large_read, large_read_time) = simulate_file_read(large_file)
  
  assert_eq(small_read, small_file)
  assert_eq(medium_read, medium_file)
  assert_eq(large_read, large_file)
  
  assert_true(small_read_time < medium_read_time)
  assert_true(medium_read_time < large_read_time)
  
  // Verify linear scaling for reads
  let small_to_medium_ratio = medium_read_time.to_float() / small_read_time.to_float()
  let medium_to_large_ratio = large_read_time.to_float() / medium_read_time.to_float()
  
  assert_true(small_to_medium_ratio > 90.0 && small_to_medium_ratio < 110.0)  // ~100x
  assert_true(medium_to_large_ratio > 9.0 && medium_to_large_ratio < 11.0)    // ~10x
  
  // Benchmark file writes
  let (small_write, small_write_time) = simulate_file_write(small_file)
  let (medium_write, medium_write_time) = simulate_file_write(medium_file)
  let (large_write, large_write_time) = simulate_file_write(large_file)
  
  assert_eq(small_write, small_file)
  assert_eq(medium_write, medium_file)
  assert_eq(large_write, large_file)
  
  assert_true(small_write_time < medium_write_time)
  assert_true(medium_write_time < large_write_time)
  
  // Writes should be slower than reads
  assert_true(small_write_time > small_read_time)
  assert_true(medium_write_time > medium_read_time)
  assert_true(large_write_time > large_read_time)
  
  // Benchmark network transmission with different packet sizes
  let data_size = 1024 * 1024  // 1MB
  let small_packet = 1024      // 1KB packets
  let large_packet = 1024 * 64  // 64KB packets
  
  let (small_packet_transmitted, small_packet_time) = simulate_network_transmission(data_size, small_packet)
  let (large_packet_transmitted, large_packet_time) = simulate_network_transmission(data_size, large_packet)
  
  assert_eq(small_packet_transmitted, data_size)
  assert_eq(large_packet_transmitted, data_size)
  
  // Larger packets should be faster (fewer packets, less overhead)
  assert_true(large_packet_time < small_packet_time)
  
  let packet_speedup = small_packet_time.to_float() / large_packet_time.to_float()
  assert_true(packet_speedup > 1.0)
}

// Test 7: Cache Performance Benchmarking
test "cache performance benchmarking" {
  // Simulate cache performance
  
  type CacheEntry = {
    key: String,
    value: String,
    access_count: Int,
    last_accessed: Int
  }
  
  type Cache = {
    entries: Array[CacheEntry],
    max_size: Int,
    hits: Int,
    misses: Int
  }
  
  let create_cache = fn(max_size: Int) {
    {
      entries: [],
      max_size,
      hits: 0,
      misses: 0
    }
  }
  
  let cache_get = fn(cache: Cache, key: String) {
    let mut found_index = -1
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found_index = i
        break
      }
    }
    
    if found_index >= 0 {
      // Update access count and last accessed time
      let entry = cache.entries[found_index]
      let updated_entry = {
        key: entry.key,
        value: entry.value,
        access_count: entry.access_count + 1,
        last_accessed: 1640999999
      }
      
      let mut updated_entries = []
      for i in 0..cache.entries.length() {
        if i == found_index {
          updated_entries = updated_entries.push(updated_entry)
        } else {
          updated_entries = updated_entries.push(cache.entries[i])
        }
      }
      
      (Some(entry.value), { cache | entries: updated_entries, hits: cache.hits + 1 })
    } else {
      (None, { cache | misses: cache.misses + 1 })
    }
  }
  
  let cache_put = fn(cache: Cache, key: String, value: String) {
    // Check if key already exists
    let mut found_index = -1
    
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found_index = i
        break
      }
    }
    
    if found_index >= 0 {
      // Update existing entry
      let entry = cache.entries[found_index]
      let updated_entry = {
        key: entry.key,
        value: value,
        access_count: entry.access_count + 1,
        last_accessed: 1640999999
      }
      
      let mut updated_entries = []
      for i in 0..cache.entries.length() {
        if i == found_index {
          updated_entries = updated_entries.push(updated_entry)
        } else {
          updated_entries = updated_entries.push(cache.entries[i])
        }
      }
      
      { cache | entries: updated_entries }
    } else {
      // Add new entry
      let new_entry = {
        key,
        value,
        access_count: 1,
        last_accessed: 1640999999
      }
      
      if cache.entries.length() < cache.max_size {
        // Space available, add new entry
        { cache | entries: cache.entries.push(new_entry) }
      } else {
        // Cache is full, implement LRU eviction
        let mut lru_index = 0
        let mut lru_time = cache.entries[0].last_accessed
        
        for i in 0..cache.entries.length() {
          if cache.entries[i].last_accessed < lru_time {
            lru_time = cache.entries[i].last_accessed
            lru_index = i
          }
        }
        
        // Replace LRU entry
        let mut updated_entries = []
        for i in 0..cache.entries.length() {
          if i == lru_index {
            updated_entries = updated_entries.push(new_entry)
          } else {
            updated_entries = updated_entries.push(cache.entries[i])
          }
        }
        
        { cache | entries: updated_entries }
      }
    }
  }
  
  // Test cache performance
  let cache_size = 100
  let cache = create_cache(cache_size)
  
  // Warm up cache
  let mut warmed_cache = cache
  for i in 0..cache_size {
    let key = "key-" + i.to_string()
    let value = "value-" + i.to_string()
    warmed_cache = cache_put(warmed_cache, key, value)
  }
  
  assert_eq(warmed_cache.entries.length(), cache_size)
  
  // Test cache hits
  let mut test_cache = warmed_cache
  for i in 0..50 {
    let key = "key-" + i.to_string()
    let (_, updated_cache) = cache_get(test_cache, key)
    test_cache = updated_cache
  }
  
  assert_true(test_cache.hits > 0)
  assert_eq(test_cache.misses, 0)
  
  // Test cache misses
  for i in cache_size..(cache_size + 50) {
    let key = "key-" + i.to_string()
    let (_, updated_cache) = cache_get(test_cache, key)
    test_cache = updated_cache
  }
  
  assert_true(test_cache.misses > 0)
  
  // Calculate hit ratio
  let total_accesses = test_cache.hits + test_cache.misses
  let hit_ratio = test_cache.hits.to_float() / total_accesses.to_float()
  assert_true(hit_ratio > 0.0 && hit_ratio <= 1.0)
  
  // Benchmark cache vs direct lookup
  let direct_lookup = fn(key: String) {
    // Simulate direct database lookup
    if key.starts_with("key-") {
      Some("value-" + key.substring(4, key.length() - 4))
    } else {
      None
    }
  }
  
  let benchmark_cache_lookup = fn(iterations: Int) {
    let start_time = 1640995200
    let mut cache = warmed_cache
    
    for i in 0..iterations {
      let key = "key-" + (i % cache_size).to_string()
      let (_, updated_cache) = cache_get(cache, key)
      cache = updated_cache
    }
    
    let end_time = start_time + iterations  // 1ms per lookup
    end_time - start_time
  }
  
  let benchmark_direct_lookup = fn(iterations: Int) {
    let start_time = 1640995200
    
    for i in 0..iterations {
      let key = "key-" + (i % cache_size).to_string()
      let _ = direct_lookup(key)
    }
    
    let end_time = start_time + iterations * 5  // 5ms per lookup (slower)
    end_time - start_time
  }
  
  let iterations = 1000
  let cache_time = benchmark_cache_lookup(iterations)
  let direct_time = benchmark_direct_lookup(iterations)
  
  assert_true(cache_time < direct_time)
  
  let cache_speedup = direct_time.to_float() / cache_time.to_float()
  assert_true(cache_speedup > 1.0)
}

// Test 8: Serialization Performance Benchmarking
test "serialization performance benchmarking" {
  // Compare different serialization formats
  
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    attributes: Array[(String, String)]
  }
  
  let create_test_span = fn(id: Int) {
    {
      trace_id: "trace-" + id.to_string(),
      span_id: "span-" + id.to_string(),
      parent_span_id: if id > 0 { Some("span-" + (id - 1).to_string()) } else { None },
      operation_name: "operation-" + id.to_string(),
      start_time: 1640995200 + id,
      end_time: Some(1640995200 + id + 100),
      status: "ok",
      attributes: [
        ("attr1", "value1-" + id.to_string()),
        ("attr2", "value2-" + id.to_string()),
        ("attr3", "value3-" + id.to_string())
      ]
    }
  }
  
  // JSON-style serialization
  let serialize_to_json = fn(span: TelemetrySpan) {
    let mut json = "{"
    json = json + "\"trace_id\":\"" + span.trace_id + "\"," 
    json = json + "\"span_id\":\"" + span.span_id + "\"," 
    
    match span.parent_span_id {
      Some(parent) => json = json + "\"parent_span_id\":\"" + parent + "\"," 
      None => json = json + "\"parent_span_id\":null," 
    }
    
    json = json + "\"operation_name\":\"" + span.operation_name + "\"," 
    json = json + "\"start_time\":" + span.start_time.to_string() + "," 
    
    match span.end_time {
      Some(end) => json = json + "\"end_time\":" + end.to_string() + "," 
      None => json = json + "\"end_time\":null," 
    }
    
    json = json + "\"status\":\"" + span.status + "\"," 
    json = json + "\"attributes\":[" 
    
    for i in 0..span.attributes.length() {
      if i > 0 {
        json = json + ","
      }
      json = json + "{\"key\":\"" + span.attributes[i].0 + "\",\"value\":\"" + span.attributes[i].1 + "\"}"
    }
    
    json = json + "]}"
    json
  }
  
  // Binary-style serialization (simplified)
  let serialize_to_binary = fn(span: TelemetrySpan) {
    let mut binary = ""
    
    // Add field separators and length prefixes
    binary = binary + span.trace_id + "|"
    binary = binary + span.span_id + "|"
    
    match span.parent_span_id {
      Some(parent) => binary = binary + parent + "|"
      None => binary = binary + "null|"
    }
    
    binary = binary + span.operation_name + "|"
    binary = binary + span.start_time.to_string() + "|"
    
    match span.end_time {
      Some(end) => binary = binary + end.to_string() + "|"
      None => binary = binary + "null|"
    }
    
    binary = binary + span.status + "|"
    
    for i in 0..span.attributes.length() {
      binary = binary + span.attributes[i].0 + "=" + span.attributes[i].1
      if i < span.attributes.length() - 1 {
        binary = binary + ","
      }
    }
    
    binary
  }
  
  // Comma-separated values serialization
  let serialize_to_csv = fn(span: TelemetrySpan) {
    let mut csv = ""
    
    csv = csv + span.trace_id + ","
    csv = csv + span.span_id + ","
    
    match span.parent_span_id {
      Some(parent) => csv = csv + parent + ","
      None => csv = csv + ","
    }
    
    csv = csv + span.operation_name + ","
    csv = csv + span.start_time.to_string() + ","
    
    match span.end_time {
      Some(end) => csv = csv + end.to_string() + ","
      None => csv = csv + ","
    }
    
    csv = csv + span.status + ","
    
    for i in 0..span.attributes.length() {
      csv = csv + span.attributes[i].0 + ":" + span.attributes[i].1
      if i < span.attributes.length() - 1 {
        csv = csv + ";"
      }
    }
    
    csv
  }
  
  // Create test data
  let mut test_spans = []
  for i in 0..100 {
    test_spans = test_spans.push(create_test_span(i))
  }
  
  // Benchmark serialization
  let benchmark_serialization = fn(serialize_fn: (TelemetrySpan) -> String, spans: Array[TelemetrySpan]) {
    let start_time = 1640995200
    let mut total_size = 0
    
    for span in spans {
      let serialized = serialize_fn(span)
      total_size = total_size + serialized.length()
    }
    
    let end_time = start_time + spans.length()  // 1ms per span
    
    (total_size, end_time - start_time)
  }
  
  let (json_size, json_time) = benchmark_serialization(serialize_to_json, test_spans)
  let (binary_size, binary_time) = benchmark_serialization(serialize_to_binary, test_spans)
  let (csv_size, csv_time) = benchmark_serialization(serialize_to_csv, test_spans)
  
  // Verify all serializations completed
  assert_true(json_size > 0)
  assert_true(binary_size > 0)
  assert_true(csv_size > 0)
  
  assert_true(json_time > 0)
  assert_true(binary_time > 0)
  assert_true(csv_time > 0)
  
  // Compare sizes (JSON should be largest)
  assert_true(json_size > binary_size)
  assert_true(binary_size > csv_size)
  
  // Compare times (CSV should be fastest)
  assert_true(csv_time < binary_time)
  assert_true(binary_time < json_time)
  
  // Calculate efficiency ratios
  let json_vs_csv_size_ratio = json_size.to_float() / csv_size.to_float()
  let json_vs_csv_time_ratio = json_time.to_float() / csv_time.to_float()
  
  assert_true(json_vs_csv_size_ratio > 1.0)
  assert_true(json_vs_csv_time_ratio > 1.0)
  
  // Test deserialization performance
  let deserialize_from_json = fn(json: String) {
    // Simplified JSON parsing simulation
    let mut trace_id = ""
    let mut span_id = ""
    let mut in_trace_id = false
    let mut in_span_id = false
    let mut in_string = false
    
    for i in 0..json.length() {
      let char = json[i]
      
      if char == '"' {
        in_string = not(in_string)
      } else if in_string {
        if in_trace_id {
          trace_id = trace_id + char
        } else if in_span_id {
          span_id = span_id + char
        }
      } else if char == ':' && i > 0 && json.substring(i-8, 8) == "trace_id") {
        in_trace_id = true
      } else if char == ':' && i > 0 && json.substring(i-7, 7) == "span_id") {
        in_span_id = true
      } else if char == ',' {
        in_trace_id = false
        in_span_id = false
      }
    }
    
    (trace_id, span_id)
  }
  
  let benchmark_deserialization = fn(deserialize_fn: (String) -> (String, String), serialized_data: Array[String]) {
    let start_time = 1640995200
    
    for data in serialized_data {
      let _ = deserialize_fn(data)
    }
    
    let end_time = start_time + serialized_data.length() * 2  // 2ms per deserialization
    end_time - start_time
  }
  
  let json_serialized = test_spans.map(serialize_to_json)
  let csv_serialized = test_spans.map(serialize_to_csv)
  
  let json_deserialize_time = benchmark_deserialization(deserialize_from_json, json_serialized)
  
  assert_true(json_deserialize_time > 0)
  
  // Deserialization should be slower than serialization
  assert_true(json_deserialize_time > json_time)
}

// Test 9: Scalability Benchmarking
test "scalability benchmarking" {
  // Test how performance scales with increasing load
  
  let process_telemetry_data = fn(data_points: Int) {
    let start_time = 1640995200
    
    // Simulate processing telemetry data
    let mut processed = 0
    for i in 0..data_points {
      // Simulate some work
      let trace_id = "trace-" + i.to_string()
      let span_id = "span-" + i.to_string()
      let operation = "operation-" + (i % 10).to_string()
      
      // Simulate processing time
      processed = processed + 1
    }
    
    let end_time = start_time + (data_points / 100)  // 10ms per 1000 data points
    
    (processed, end_time - start_time)
  }
  
  let process_concurrent_data = fn(data_points: Int, workers: Int) {
    let start_time = 1640995200
    
    // Distribute work among workers
    let points_per_worker = data_points / workers
    let mut total_processed = 0
    
    // Simulate concurrent processing
    for i in 0..workers {
      let worker_processed = points_per_worker
      total_processed = total_processed + worker_processed
    }
    
    // Handle remaining data points
    let remaining = data_points % workers
    total_processed = total_processed + remaining
    
    // Concurrent processing is faster, but not perfectly linear
    let efficiency = 0.8  // 80% efficiency due to overhead
    let effective_workers = 1.0 + (workers.to_float() - 1.0) * efficiency
    let end_time = start_time + (data_points / 100.0) / effective_workers
    
    (total_processed, (end_time - start_time).to_int())
  }
  
  // Test with different data sizes
  let small_data = 1000
  let medium_data = 10000
  let large_data = 100000
  
  let (small_processed, small_time) = process_telemetry_data(small_data)
  let (medium_processed, medium_time) = process_telemetry_data(medium_data)
  let (large_processed, large_time) = process_telemetry_data(large_data)
  
  assert_eq(small_processed, small_data)
  assert_eq(medium_processed, medium_data)
  assert_eq(large_processed, large_data)
  
  // Verify linear scaling
  let small_to_medium_ratio = medium_time.to_float() / small_time.to_float()
  let medium_to_large_ratio = large_time.to_float() / medium_time.to_float()
  
  assert_true(small_to_medium_ratio > 8.0 && small_to_medium_ratio < 12.0)  // ~10x
  assert_true(medium_to_large_ratio > 8.0 && medium_to_large_ratio < 12.0)  // ~10x
  
  // Test concurrent processing
  let workers = 4
  let (concurrent_processed, concurrent_time) = process_concurrent_data(large_data, workers)
  
  assert_eq(concurrent_processed, large_data)
  
  // Concurrent processing should be faster
  assert_true(concurrent_time < large_time)
  
  let concurrent_speedup = large_time.to_float() / concurrent_time.to_float()
  assert_true(concurrent_speedup > 1.0)
  
  // Speedup should be less than perfect linear scaling due to overhead
  assert_true(concurrent_speedup < workers.to_float())
  
  // Test scalability with increasing workers
  let mut worker_results = []
  for i in 1..=8 {
    let (_, time) = process_concurrent_data(medium_data, i)
    worker_results = worker_results.push((i, time))
  }
  
  // Verify that more workers generally reduce processing time
  for i in 1..worker_results.length() {
    assert_true(worker_results[i].1 <= worker_results[i-1].1)
  }
  
  // Calculate efficiency for each worker count
  let base_time = worker_results[0].1.to_float()
  let mut efficiencies = []
  
  for i in 0..worker_results.length() {
    let workers = worker_results[i].0.to_float()
    let time = worker_results[i].1.to_float()
    let speedup = base_time / time
    let efficiency = speedup / workers
    efficiencies = efficiencies.push(efficiency)
  }
  
  // Efficiency should decrease as we add more workers (due to overhead)
  for i in 1..efficiencies.length() {
    assert_true(efficiencies[i] <= efficiencies[i-1])
  }
}

// Test 10: Resource Utilization Benchmarking
test "resource utilization benchmarking" {
  // Test resource utilization under different loads
  
  type ResourceMetrics = {
    cpu_usage: Float,
    memory_usage: Int,
    disk_io: Int,
    network_io: Int,
    processing_time: Int
  }
  
  let simulate_resource_usage = fn(load_factor: Int, duration: Int) {
    // CPU usage increases with load
    let cpu_usage = 10.0 + (load_factor.to_float() * 8.0)  // Base 10% + 8% per load factor
    if cpu_usage > 95.0 {
      cpu_usage = 95.0  // Cap at 95%
    }
    
    // Memory usage increases with load
    let memory_usage = 100 * 1024 * 1024 + (load_factor * 50 * 1024 * 1024)  // Base 100MB + 50MB per load factor
    
    // Disk I/O increases with load
    let disk_io = load_factor * 1024 * 1024  // 1MB per load factor
    
    // Network I/O increases with load
    let network_io = load_factor * 512 * 1024  // 512KB per load factor
    
    // Processing time increases with load but not linearly (due to optimizations)
    let processing_time = duration + (load_factor * 10)
    
    {
      cpu_usage,
      memory_usage,
      disk_io,
      network_io,
      processing_time
    }
  }
  
  // Test resource usage at different load levels
  let light_load = 1
  let medium_load = 5
  let heavy_load = 10
  
  let base_duration = 1000  // 1 second
  
  let light_metrics = simulate_resource_usage(light_load, base_duration)
  let medium_metrics = simulate_resource_usage(medium_load, base_duration)
  let heavy_metrics = simulate_resource_usage(heavy_load, base_duration)
  
  // Verify resource usage increases with load
  assert_true(light_metrics.cpu_usage < medium_metrics.cpu_usage)
  assert_true(medium_metrics.cpu_usage < heavy_metrics.cpu_usage)
  
  assert_true(light_metrics.memory_usage < medium_metrics.memory_usage)
  assert_true(medium_metrics.memory_usage < heavy_metrics.memory_usage)
  
  assert_true(light_metrics.disk_io < medium_metrics.disk_io)
  assert_true(medium_metrics.disk_io < heavy_metrics.disk_io)
  
  assert_true(light_metrics.network_io < medium_metrics.network_io)
  assert_true(medium_metrics.network_io < heavy_metrics.network_io)
  
  assert_true(light_metrics.processing_time < medium_metrics.processing_time)
  assert_true(medium_metrics.processing_time < heavy_metrics.processing_time)
  
  // Test resource efficiency
  let calculate_efficiency = fn(metrics: ResourceMetrics) {
    // Efficiency = throughput / resource usage
    let throughput = 1000.0 / metrics.processing_time.to_float()  # Operations per second
    let resource_factor = metrics.cpu_usage / 100.0 + (metrics.memory_usage.to_float() / (1024.0 * 1024.0 * 1024.0))
    
    throughput / resource_factor
  }
  
  let light_efficiency = calculate_efficiency(light_metrics)
  let medium_efficiency = calculate_efficiency(medium_metrics)
  let heavy_efficiency = calculate_efficiency(heavy_metrics)
  
  // Efficiency might decrease at higher loads due to resource contention
  assert_true(light_efficiency >= medium_efficiency)
  assert_true(medium_efficiency >= heavy_efficiency)
  
  // Test resource optimization
  let optimize_resource_usage = fn(metrics: ResourceMetrics) {
    // Simulate resource optimization
    let optimized_cpu = metrics.cpu_usage * 0.8  # 20% reduction
    let optimized_memory = metrics.memory_usage * 9 / 10  # 10% reduction
    let optimized_disk = metrics.disk_io * 3 / 4  # 25% reduction
    let optimized_network = metrics.network_io * 4 / 5  # 20% reduction
    
    # Processing time might slightly increase due to optimization overhead
    let optimized_time = metrics.processing_time + 50
    
    {
      cpu_usage: optimized_cpu,
      memory_usage: optimized_memory,
      disk_io: optimized_disk,
      network_io: optimized_network,
      processing_time: optimized_time
    }
  }
  
  let optimized_heavy = optimize_resource_usage(heavy_metrics)
  
  assert_true(optimized_heavy.cpu_usage < heavy_metrics.cpu_usage)
  assert_true(optimized_heavy.memory_usage < heavy_metrics.memory_usage)
  assert_true(optimized_heavy.disk_io < heavy_metrics.disk_io)
  assert_true(optimized_heavy.network_io < heavy_metrics.network_io)
  assert_true(optimized_heavy.processing_time >= heavy_metrics.processing_time)
  
  // Calculate optimized efficiency
  let optimized_efficiency = calculate_efficiency(optimized_heavy)
  
  // Overall efficiency should improve despite slight increase in processing time
  assert_true(optimized_efficiency > heavy_efficiency)
  
  // Test resource scaling patterns
  let test_scaling_pattern = fn(max_load: Int) {
    let mut scaling_results = []
    
    for i in 1..=max_load {
      let metrics = simulate_resource_usage(i, base_duration)
      let efficiency = calculate_efficiency(metrics)
      
      scaling_results = scaling_results.push({
        load: i,
        cpu_usage: metrics.cpu_usage,
        memory_usage: metrics.memory_usage,
        efficiency
      })
    }
    
    scaling_results
  }
  
  let scaling_results = test_scaling_pattern(10)
  
  // Verify scaling patterns
  for i in 1..scaling_results.length() {
    assert_true(scaling_results[i].load == i)
    assert_true(scaling_results[i].cpu_usage > 0.0)
    assert_true(scaling_results[i].memory_usage > 0)
    assert_true(scaling_results[i].efficiency > 0.0)
  }
  
  // CPU usage should increase with load
  for i in 1..scaling_results.length() {
    assert_true(scaling_results[i].cpu_usage >= scaling_results[i-1].cpu_usage)
  }
  
  // Memory usage should increase with load
  for i in 1..scaling_results.length() {
    assert_true(scaling_results[i].memory_usage >= scaling_results[i-1].memory_usage)
  }
}