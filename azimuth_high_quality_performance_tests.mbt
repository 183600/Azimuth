// Azimuth Telemetry System - High Quality Performance Tests
// This file contains comprehensive test cases for performance profiling and optimization

// Test 1: Algorithm Complexity and Big O Analysis
test "algorithm complexity and big o analysis" {
  // Test O(1) - Constant time operations
  let array = [1, 2, 3, 4, 5]
  let constant_time_start = PerformanceCounter::now()
  
  // Array access by index should be O(1)
  let value = array[2]
  assert_eq(value, 3)
  
  let constant_time_end = PerformanceCounter::now()
  let constant_time_duration = PerformanceCounter::duration_ms(constant_time_start, constant_time_end)
  assert_true(constant_time_duration < 1.0) // Should be very fast
  
  // Test O(n) - Linear time operations
  let large_array = [0; 10000]
  let linear_time_start = PerformanceCounter::now()
  
  // Summing all elements should be O(n)
  let mut sum = 0
  for element in large_array {
    sum = sum + element
  }
  
  let linear_time_end = PerformanceCounter::now()
  let linear_time_duration = PerformanceCounter::duration_ms(linear_time_start, linear_time_end)
  assert_true(linear_time_duration > constant_time_duration) // Should be slower than O(1)
  assert_true(linear_time_duration < 100.0) // But still reasonably fast
  
  // Test O(n²) - Quadratic time operations
  let small_array = [0; 100] // Smaller array for quadratic operation
  let quadratic_time_start = PerformanceCounter::now()
  
  // Nested loop should be O(n²)
  let mut nested_sum = 0
  for i = 0; i < small_array.length(); i = i + 1 {
    for j = 0; j < small_array.length(); j = j + 1 {
      nested_sum = nested_sum + small_array[i] * small_array[j]
    }
  }
  
  let quadratic_time_end = PerformanceCounter::now()
  let quadratic_time_duration = PerformanceCounter::duration_ms(quadratic_time_start, quadratic_time_end)
  assert_true(quadratic_time_duration > linear_time_duration) // Should be slower than O(n)
  
  // Test O(log n) - Logarithmic time operations
  let sorted_array = [0; 10000]
  for i = 0; i < sorted_array.length(); i = i + 1 {
    sorted_array[i] = i
  }
  
  let log_time_start = PerformanceCounter::now()
  
  // Binary search should be O(log n)
  let search_result = binary_search(sorted_array, 5000)
  assert_eq(search_result, Some(5000))
  
  let log_time_end = PerformanceCounter::now()
  let log_time_duration = PerformanceCounter::duration_ms(log_time_start, log_time_end)
  assert_true(log_time_duration < linear_time_duration) // Should be faster than O(n)
  
  // Verify complexity relationships
  assert_true(constant_time_duration < log_time_duration)
  assert_true(log_time_duration < linear_time_duration)
  assert_true(linear_time_duration < quadratic_time_duration)
}

// Test 2: Memory Allocation and Garbage Collection Performance
test "memory allocation and garbage collection performance" {
  // Test memory allocation speed
  let allocation_start = PerformanceCounter::now()
  
  let mut allocated_arrays = []
  for i = 0; i < 1000; i = i + 1 {
    let array = [0; 100] // Allocate 100 integers
    allocated_arrays.push(array)
  }
  
  let allocation_end = PerformanceCounter::now()
  let allocation_duration = PerformanceCounter::duration_ms(allocation_start, allocation_end)
  assert_true(allocation_duration < 1000.0) // Should complete within 1 second
  
  // Test memory deallocation (GC)
  let deallocation_start = PerformanceCounter::now()
  
  // Clear references to trigger garbage collection
  allocated_arrays = []
  
  // Force garbage collection if available
  System::gc()
  
  let deallocation_end = PerformanceCounter::now()
  let deallocation_duration = PerformanceCounter::duration_ms(deallocation_start, deallocation_end)
  assert_true(deallocation_duration < 5000.0) // Should complete within 5 seconds
  
  // Test memory usage patterns
  let initial_memory = System::memory_usage()
  
  // Allocate and deallocate in a pattern
  for i = 0; i < 100; i = i + 1 {
    let large_array = [0; 10000]
    // Use the array
    let sum = large_array.reduce(|acc, val| acc + val, 0)
    assert_eq(sum, 0)
    // Array goes out of scope and should be garbage collected
  }
  
  let final_memory = System::memory_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be minimal after GC
  assert_true(memory_increase < 1000000) // Less than 1MB increase
  
  // Test object pooling for performance
  let object_pool = ObjectPool::new(|| { 0 }) // Pool of zeros
  
  let pool_start = PerformanceCounter::now()
  
  // Use object pool
  for i = 0; i < 10000; i = i + 1 {
    let obj = ObjectPool::acquire(object_pool)
    let value = obj + 1
    ObjectPool::release(object_pool, value)
  }
  
  let pool_end = PerformanceCounter::now()
  let pool_duration = PerformanceCounter::duration_ms(pool_start, pool_end)
  
  // Compare with direct allocation
  let direct_start = PerformanceCounter::now()
  
  for i = 0; i < 10000; i = i + 1 {
    let obj = 0
    let value = obj + 1
    // obj goes out of scope
  }
  
  let direct_end = PerformanceCounter::now()
  let direct_duration = PerformanceCounter::duration_ms(direct_start, direct_end)
  
  // Object pooling should be faster or at least not significantly slower
  assert_true(pool_duration <= direct_duration * 2.0)
}

// Test 3: String Processing Performance
test "string processing performance" {
  // Test string concatenation performance
  let base_str = "hello"
  
  // Concatenation with + operator
  let concat_start = PerformanceCounter::now()
  
  let mut result = ""
  for i = 0; i < 1000; i = i + 1 {
    result = result + base_str
  }
  
  let concat_end = PerformanceCounter::now()
  let concat_duration = PerformanceCounter::duration_ms(concat_start, concat_end)
  
  // Concatenation with StringBuilder
  let builder_start = PerformanceCounter::now()
  
  let builder = StringBuilder::new()
  for i = 0; i < 1000; i = i + 1 {
    StringBuilder::append(builder, base_str)
  }
  let builder_result = StringBuilder::to_string(builder)
  
  let builder_end = PerformanceCounter::now()
  let builder_duration = PerformanceCounter::duration_ms(builder_start, builder_end)
  
  // StringBuilder should be significantly faster
  assert_true(builder_duration < concat_duration / 2.0)
  assert_eq(result.length(), builder_result.length())
  
  // Test string parsing performance
  let json_str = "{\"name\":\"test\",\"value\":42,\"active\":true,\"items\":[1,2,3,4,5]}"
  
  let parse_start = PerformanceCounter::now()
  
  for i = 0; i < 1000; i = i + 1 {
    let parsed = Json::parse(json_str)
    match parsed {
      Ok(obj) => assert_true(true) // Successfully parsed
      Error(_) => assert_true(false) // Should not reach here
    }
  }
  
  let parse_end = PerformanceCounter::now()
  let parse_duration = PerformanceCounter::duration_ms(parse_start, parse_end)
  assert_true(parse_duration < 1000.0) // Should complete within 1 second
  
  // Test regex matching performance
  let text = "The quick brown fox jumps over the lazy dog. The quick brown fox is quick."
  let pattern = Regex::compile("quick.*fox")
  
  let regex_start = PerformanceCounter::now()
  
  for i = 0; i < 10000; i = i + 1 {
    let matches = Regex::find_all(pattern, text)
    assert_eq(matches.length(), 2) // Should find 2 matches
  }
  
  let regex_end = PerformanceCounter::now()
  let regex_duration = PerformanceCounter::duration_ms(regex_start, regex_end)
  assert_true(regex_duration < 1000.0) // Should complete within 1 second
  
  // Test string splitting performance
  let csv_str = "value1,value2,value3,value4,value5,value6,value7,value8,value9,value10"
  
  let split_start = PerformanceCounter::now()
  
  for i = 0; i < 10000; i = i + 1 {
    let parts = csv_str.split(",")
    assert_eq(parts.length(), 10)
  }
  
  let split_end = PerformanceCounter::now()
  let split_duration = PerformanceCounter::duration_ms(split_start, split_end)
  assert_true(split_duration < 500.0) // Should be very fast
}

// Test 4: Collection Operations Performance
test "collection operations performance" {
  // Test array vs linked list performance
  let array_size = 10000
  let array = [0; array_size]
  let linked_list = LinkedList::new()
  
  // Populate linked list
  for i = 0; i < array_size; i = i + 1 {
    LinkedList::push_back(linked_list, i)
  }
  
  // Test sequential access
  let array_seq_start = PerformanceCounter::now()
  
  let mut array_sum = 0
  for element in array {
    array_sum = array_sum + element
  }
  
  let array_seq_end = PerformanceCounter::now()
  let array_seq_duration = PerformanceCounter::duration_ms(array_seq_start, array_seq_end)
  
  let list_seq_start = PerformanceCounter::now()
  
  let mut list_sum = 0
  for element in linked_list {
    list_sum = list_sum + element
  }
  
  let list_seq_end = PerformanceCounter::now()
  let list_seq_duration = PerformanceCounter::duration_ms(list_seq_start, list_seq_end)
  
  // Array should be faster for sequential access
  assert_true(array_seq_duration < list_seq_duration)
  assert_eq(array_sum, list_sum)
  
  // Test random access
  let array_rand_start = PerformanceCounter::now()
  
  let mut array_rand_sum = 0
  for i = 0; i < 1000; i = i + 1 {
    let index = Random::int_in_range(0, array_size - 1)
    array_rand_sum = array_rand_sum + array[index]
  }
  
  let array_rand_end = PerformanceCounter::now()
  let array_rand_duration = PerformanceCounter::duration_ms(array_rand_start, array_rand_end)
  
  let list_rand_start = PerformanceCounter::now()
  
  let mut list_rand_sum = 0
  for i = 0; i < 1000; i = i + 1 {
    let index = Random::int_in_range(0, array_size - 1)
    let value = LinkedList::get_at(linked_list, index)
    match value {
      Some(v) => list_rand_sum = list_rand_sum + v,
      None => assert_true(false) // Should not reach here
    }
  }
  
  let list_rand_end = PerformanceCounter::now()
  let list_rand_duration = PerformanceCounter::duration_ms(list_rand_start, list_rand_end)
  
  // Array should be much faster for random access
  assert_true(array_rand_duration < list_rand_duration / 5.0)
  
  // Test hash map performance
  let hash_map = HashMap::new()
  let keys = []
  
  // Populate hash map
  let populate_start = PerformanceCounter::now()
  
  for i = 0; i < 10000; i = i + 1 {
    let key = "key_" + i.to_string()
    let value = i * 2
    HashMap::insert(hash_map, key, value)
    keys.push(key)
  }
  
  let populate_end = PerformanceCounter::now()
  let populate_duration = PerformanceCounter::duration_ms(populate_start, populate_end)
  assert_true(populate_duration < 1000.0) // Should complete within 1 second
  
  // Test hash map lookups
  let lookup_start = PerformanceCounter::now()
  
  for key in keys {
    let value = HashMap::get(hash_map, key)
    match value {
      Some(v) => assert_true(v >= 0)
      None => assert_true(false) // Should not reach here
    }
  }
  
  let lookup_end = PerformanceCounter::now()
  let lookup_duration = PerformanceCounter::duration_ms(lookup_start, lookup_end)
  assert_true(lookup_duration < populate_duration) // Lookups should be faster than insertions
  
  // Test hash map iteration
  let iteration_start = PerformanceCounter::now()
  
  let mut iteration_count = 0
  for (key, value) in hash_map {
    iteration_count = iteration_count + 1
  }
  
  let iteration_end = PerformanceCounter::now()
  let iteration_duration = PerformanceCounter::duration_ms(iteration_start, iteration_end)
  assert_eq(iteration_count, 10000)
  assert_true(iteration_duration < 500.0) // Should be fast
}

// Test 5: Sorting Algorithm Performance
test "sorting algorithm performance" {
  // Generate test data
  let sizes = [100, 1000, 10000]
  
  for size in sizes {
    let random_data = generate_random_array(size)
    
    // Test quicksort performance
    let quick_data = Array::copy(random_data)
    let quick_start = PerformanceCounter::now()
    
    Array::quick_sort(quick_data)
    
    let quick_end = PerformanceCounter::now()
    let quick_duration = PerformanceCounter::duration_ms(quick_start, quick_end)
    
    // Verify sorted
    assert_true(is_sorted(quick_data))
    
    // Test merge sort performance
    let merge_data = Array::copy(random_data)
    let merge_start = PerformanceCounter::now()
    
    Array::merge_sort(merge_data)
    
    let merge_end = PerformanceCounter::now()
    let merge_duration = PerformanceCounter::duration_ms(merge_start, merge_end)
    
    // Verify sorted
    assert_true(is_sorted(merge_data))
    
    // Test heap sort performance
    let heap_data = Array::copy(random_data)
    let heap_start = PerformanceCounter::now()
    
    Array::heap_sort(heap_data)
    
    let heap_end = PerformanceCounter::now()
    let heap_duration = PerformanceCounter::duration_ms(heap_start, heap_end)
    
    // Verify sorted
    assert_true(is_sorted(heap_data))
    
    // Test built-in sort performance
    let builtin_data = Array::copy(random_data)
    let builtin_start = PerformanceCounter::now()
    
    Array::sort(builtin_data)
    
    let builtin_end = PerformanceCounter::now()
    let builtin_duration = PerformanceCounter::duration_ms(builtin_start, builtin_end)
    
    // Verify sorted
    assert_true(is_sorted(builtin_data))
    
    // Performance should be reasonable for all algorithms
    assert_true(quick_duration < 5000.0) // Should complete within 5 seconds
    assert_true(merge_duration < 5000.0)
    assert_true(heap_duration < 5000.0)
    assert_true(builtin_duration < 5000.0)
    
    // Built-in sort should be competitive
    assert_true(builtin_duration <= quick_duration * 2.0)
  }
  
  // Test sorting with special cases
  let sorted_data = []
  for i = 0; i < 1000; i = i + 1 {
    sorted_data.push(i)
  }
  
  let reverse_data = Array::copy(sorted_data)
  Array::reverse(reverse_data)
  
  let duplicate_data = []
  for i = 0; i < 1000; i = i + 1 {
    duplicate_data.push(i % 100) // Many duplicates
  }
  
  // Test all three cases
  let special_cases = [sorted_data, reverse_data, duplicate_data]
  let case_names = ["sorted", "reverse", "duplicates"]
  
  for i = 0; i < special_cases.length(); i = i + 1 {
    let data = special_cases[i]
    let name = case_names[i]
    
    let test_start = PerformanceCounter::now()
    Array::sort(data)
    let test_end = PerformanceCounter::now()
    let test_duration = PerformanceCounter::duration_ms(test_start, test_end)
    
    // Should still be reasonably fast even with special cases
    assert_true(test_duration < 1000.0) // Should complete within 1 second
    assert_true(is_sorted(data))
  }
}

// Test 6: I/O Performance
test "io performance" {
  // Test file write performance
  let test_data = "This is test data for I/O performance testing. ".repeat(1000)
  let file_path = "/tmp/performance_test.txt"
  
  let write_start = PerformanceCounter::now()
  
  let write_result = File::write_all_text(file_path, test_data)
  match write_result {
    Ok(_) => assert_true(true) // Success
    Error(_) => assert_true(false) // Should not reach here
  }
  
  let write_end = PerformanceCounter::now()
  let write_duration = PerformanceCounter::duration_ms(write_start, write_end)
  assert_true(write_duration < 1000.0) // Should complete within 1 second
  
  // Test file read performance
  let read_start = PerformanceCounter::now()
  
  let read_result = File::read_all_text(file_path)
  match read_result {
    Ok(content) => assert_eq(content.length(), test_data.length()),
    Error(_) => assert_true(false) // Should not reach here
  }
  
  let read_end = PerformanceCounter::now()
  let read_duration = PerformanceCounter::duration_ms(read_start, read_end)
  assert_true(read_duration < 1000.0) // Should complete within 1 second
  
  // Test binary file performance
  let binary_data = [0; 10000] // 10KB of zeros
  let binary_path = "/tmp/performance_test.bin"
  
  let binary_write_start = PerformanceCounter::now()
  
  let binary_write_result = File::write_all_bytes(binary_path, binary_data)
  match binary_write_result {
    Ok(_) => assert_true(true) // Success
    Error(_) => assert_true(false) // Should not reach here
  }
  
  let binary_write_end = PerformanceCounter::now()
  let binary_write_duration = PerformanceCounter::duration_ms(binary_write_start, binary_write_end)
  assert_true(binary_write_duration < 500.0) // Should be fast
  
  // Test network I/O performance (if available)
  let network_start = PerformanceCounter::now()
  
  let network_result = HttpClient::get("https://httpbin.org/json")
  match network_result {
    Ok(response) => {
      assert_true(response.contains("slideshow"))
      assert_true(response.contains("author"))
    }
    Error(_) => assert_true(true) // Network might not be available, that's OK
  }
  
  let network_end = PerformanceCounter::now()
  let network_duration = PerformanceCounter::duration_ms(network_start, network_end)
  
  // If network is available, should complete within reasonable time
  if network_result.is_ok() {
    assert_true(network_duration < 10000.0) // Should complete within 10 seconds
  }
  
  // Clean up test files
  File::delete(file_path)
  File::delete(binary_path)
}

// Test 7: Telemetry System Performance
test "telemetry system performance" {
  // Test metrics collection performance
  let metrics_collector = MetricsCollector::new()
  
  let metrics_start = PerformanceCounter::now()
  
  // Record many metrics
  for i = 0; i < 10000; i = i + 1 {
    let counter_name = "counter_" + (i % 100).to_string()
    let counter_value = i.to_float()
    MetricsCollector::record_counter(metrics_collector, counter_name, counter_value)
    
    if i % 10 == 0 {
      let gauge_name = "gauge_" + (i % 10).to_string()
      let gauge_value = (i * 0.1)
      MetricsCollector::record_gauge(metrics_collector, gauge_name, gauge_value)
    }
  }
  
  let metrics_end = PerformanceCounter::now()
  let metrics_duration = PerformanceCounter::duration_ms(metrics_start, metrics_end)
  assert_true(metrics_duration < 1000.0) // Should complete within 1 second
  
  // Test metrics aggregation performance
  let aggregation_start = PerformanceCounter::now()
  
  let aggregated = MetricsCollector::aggregate(metrics_collector)
  
  let aggregation_end = PerformanceCounter::now()
  let aggregation_duration = PerformanceCounter::duration_ms(aggregation_start, aggregation_end)
  assert_true(aggregation_duration < 500.0) // Should be fast
  
  // Test span creation performance
  let tracer = Tracer::new("performance-test")
  
  let span_start = PerformanceCounter::now()
  
  // Create many spans
  let spans = []
  for i = 0; i < 1000; i = i + 1 {
    let span_name = "span_" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    
    // Add some events
    for j = 0; j < 5; j = j + 1 {
      let event_name = "event_" + j.to_string()
      Span::add_event(span, event_name)
    }
    
    spans.push(span)
  }
  
  let span_end = PerformanceCounter::now()
  let span_duration = PerformanceCounter::duration_ms(span_start, span_end)
  assert_true(span_duration < 2000.0) // Should complete within 2 seconds
  
  // Test span export performance
  let export_start = PerformanceCounter::now()
  
  for span in spans {
    Span::end(span)
  }
  
  let export_end = PerformanceCounter::now()
  let export_duration = PerformanceCounter::duration_ms(export_start, export_end)
  assert_true(export_duration < 1000.0) // Should complete within 1 second
  
  // Test log recording performance
  let logger = Logger::new("performance-test")
  
  let log_start = PerformanceCounter::now()
  
  // Record many log entries
  for i = 0; i < 10000; i = i + 1 {
    let log_level = if i % 4 == 0 { Info } else if i % 4 == 1 { Warn } else if i % 4 == 2 { Error } else { Debug }
    let message = "Log message " + i.to_string()
    Logger::log(logger, log_level, message)
  }
  
  let log_end = PerformanceCounter::now()
  let log_duration = PerformanceCounter::duration_ms(log_start, log_end)
  assert_true(log_duration < 1000.0) // Should complete within 1 second
}

// Test 8: Concurrency Performance
test "concurrency performance" {
  // Test single-threaded vs multi-threaded performance
  let data_size = 100000
  let data = generate_random_array(data_size)
  
  // Single-threaded processing
  let single_start = PerformanceCounter::now()
  
  let mut single_result = 0
  for value in data {
    single_result = single_result + process_value(value)
  }
  
  let single_end = PerformanceCounter::now()
  let single_duration = PerformanceCounter::duration_ms(single_start, single_end)
  
  // Multi-threaded processing
  let thread_count = 4
  let chunk_size = data_size / thread_count
  let multi_start = PerformanceCounter::now()
  
  let futures = []
  for i = 0; i < thread_count; i = i + 1 {
    let start_index = i * chunk_size
    let end_index = if i == thread_count - 1 { data_size } else { (i + 1) * chunk_size }
    let chunk = data.slice(start_index, end_index)
    
    let task = || {
      let mut result = 0
      for value in chunk {
        result = result + process_value(value)
      }
      result
    }
    
    let future = ThreadPool::submit(thread_pool, task)
    futures.push(future)
  }
  
  let mut multi_result = 0
  for future in futures {
    let result = Future::get(future)
    multi_result = multi_result + result
  }
  
  let multi_end = PerformanceCounter::now()
  let multi_duration = PerformanceCounter::duration_ms(multi_start, multi_end)
  
  // Verify results are the same
  assert_eq(single_result, multi_result)
  
  // Multi-threaded should be faster (but not always, depending on overhead)
  // We'll just verify it completes in reasonable time
  assert_true(multi_duration < single_duration * 2.0) // Not significantly slower
  
  // Test concurrent map operations performance
  let concurrent_map = ConcurrentHashMap::new()
  
  let concurrent_start = PerformanceCounter::now()
  
  let concurrent_futures = []
  for i = 0; i < thread_count; i = i + 1 {
    let thread_id = i
    let task = || {
      for j = 0; j < 1000; j = j + 1 {
        let key = "key_" + (thread_id * 1000 + j).to_string()
        let value = thread_id * 1000 + j
        ConcurrentHashMap::insert(concurrent_map, key, value)
      }
    }
    
    let future = ThreadPool::submit(thread_pool, task)
    concurrent_futures.push(future)
  }
  
  for future in concurrent_futures {
    Future::get(future)
  }
  
  let concurrent_end = PerformanceCounter::now()
  let concurrent_duration = PerformanceCounter::duration_ms(concurrent_start, concurrent_end)
  assert_true(concurrent_duration < 5000.0) // Should complete within 5 seconds
  
  // Test atomic operations performance
  let atomic_counter = AtomicCounter::new(0)
  
  let atomic_start = PerformanceCounter::now()
  
  let atomic_futures = []
  for i = 0; i < thread_count; i = i + 1 {
    let task = || {
      for j = 0; j < 10000; j = j + 1 {
        AtomicCounter::fetch_add(atomic_counter, 1)
      }
    }
    
    let future = ThreadPool::submit(thread_pool, task)
    atomic_futures.push(future)
  }
  
  for future in atomic_futures {
    Future::get(future)
  }
  
  let atomic_end = PerformanceCounter::now()
  let atomic_duration = PerformanceCounter::duration_ms(atomic_start, atomic_end)
  assert_true(atomic_duration < 5000.0) // Should complete within 5 seconds
  
  let final_value = AtomicCounter::load(atomic_counter)
  assert_eq(final_value, thread_count * 10000)
}

// Test 9: Cache Performance
test "cache performance" {
  // Test LRU cache performance
  let cache = LRUCache::new(1000) // Capacity 1000
  
  // Warm up the cache
  for i = 0; i < 1000; i = i + 1 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    LRUCache::put(cache, key, value)
  }
  
  // Test cache hit performance
  let hit_start = PerformanceCounter::now()
  
  for i = 0; i < 10000; i = i + 1 {
    let key = "key_" + (i % 1000).to_string() // Should always hit
    let value = LRUCache::get(cache, key)
    match value {
      Some(v) => assert_true(v.contains("value_")),
      None => assert_true(false) // Should not reach here
    }
  }
  
  let hit_end = PerformanceCounter::now()
  let hit_duration = PerformanceCounter::duration_ms(hit_start, hit_end)
  assert_true(hit_duration < 1000.0) // Should be very fast
  
  // Test cache miss performance
  let miss_start = PerformanceCounter::now()
  
  for i = 10000; i < 11000; i = i + 1 {
    let key = "key_" + i.to_string() // Should always miss
    let value = LRUCache::get(cache, key)
    assert_eq(value, None)
  }
  
  let miss_end = PerformanceCounter::now()
  let miss_duration = PerformanceCounter::duration_ms(miss_start, miss_end)
  assert_true(miss_duration < 1000.0) // Should still be fast
  
  // Test cache eviction performance
  let eviction_start = PerformanceCounter::now()
  
  for i = 2000; i < 3000; i = i + 1 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    LRUCache::put(cache, key, value)
  }
  
  let eviction_end = PerformanceCounter::now()
  let eviction_duration = PerformanceCounter::duration_ms(eviction_start, eviction_end)
  assert_true(eviction_duration < 1000.0) // Should be fast
  
  // Test cache size
  assert_eq(LRUCache::size(cache), 1000) // Should be at capacity
  
  // Test function memoization performance
  let expensive_function = |x| {
    // Simulate expensive computation
    Thread::sleep(10)
    x * x
  }
  
  let memoized = Memoizer::new(expensive_function)
  
  // First call (cache miss)
  let first_start = PerformanceCounter::now()
  let result1 = Memoizer::call(memoized, 42)
  let first_end = PerformanceCounter::now()
  let first_duration = PerformanceCounter::duration_ms(first_start, first_end)
  
  // Second call (cache hit)
  let second_start = PerformanceCounter::now()
  let result2 = Memoizer::call(memoized, 42)
  let second_end = PerformanceCounter::now()
  let second_duration = PerformanceCounter::duration_ms(second_start, second_end)
  
  // Verify results are the same
  assert_eq(result1, result2)
  assert_eq(result1, 1764) // 42 * 42
  
  // Second call should be much faster
  assert_true(second_duration < first_duration / 5.0)
}

// Test 10: Profiling and Benchmarking
test "profiling and benchmarking" {
  // Test CPU profiling
  let profiler = Profiler::new()
  
  Profiler::start(profiler)
  
  // Run some CPU-intensive work
  let mut result = 0
  for i = 0; i < 1000000; i = i + 1 {
    result = result + (i * i) % 1000
  }
  
  Profiler::stop(profiler)
  
  let profile_data = Profiler::get_data(profiler)
  assert_true(profile_data.total_cpu_time > 0)
  assert_true(profile_data.function_calls > 0)
  
  // Test memory profiling
  let memory_profiler = MemoryProfiler::new()
  
  MemoryProfiler::start(memory_profiler)
  
  // Allocate and deallocate memory
  let allocations = []
  for i = 0; i < 1000; i = i + 1 {
    let array = [0; 100]
    allocations.push(array)
  }
  
  allocations = [] // Deallocate
  
  MemoryProfiler::stop(memory_profiler)
  
  let memory_profile = MemoryProfiler::get_data(memory_profiler)
  assert_true(memory_profile.peak_memory_usage > 0)
  assert_true(memory_profile.allocation_count > 0)
  
  // Test microbenchmarking
  let benchmark = Benchmark::new()
  
  // Benchmark array operations
  Benchmark::run(benchmark, "array_access", || {
    let array = [0; 10000]
    for i = 0; i < 1000; i = i + 1 {
      let index = i % 10000
      let value = array[index]
      assert_eq(value, 0)
    }
  })
  
  Benchmark::run(benchmark, "array_iteration", || {
    let array = [0; 10000]
    let mut sum = 0
    for value in array {
      sum = sum + value
    }
    assert_eq(sum, 0)
  })
  
  Benchmark::run(benchmark, "hash_map_operations", || {
    let map = HashMap::new()
    for i = 0; i < 1000; i = i + 1 {
      let key = "key_" + i.to_string()
      let value = i
      HashMap::insert(map, key, value)
    }
    
    for i = 0; i < 1000; i = i + 1 {
      let key = "key_" + i.to_string()
      let value = HashMap::get(map, key)
      match value {
        Some(v) => assert_eq(v, i),
        None => assert_true(false)
      }
    }
  })
  
  // Get benchmark results
  let results = Benchmark::get_results(benchmark)
  
  // Verify all benchmarks ran
  assert_true(results.length() >= 3)
  
  // Verify results contain timing information
  for result in results {
    assert_true(result.avg_time > 0)
    assert_true(result.min_time > 0)
    assert_true(result.max_time >= result.min_time)
    assert_true(result.iteration_count > 0)
  }
}

// Helper functions for testing
func generate_random_array(size : Int) -> Array[Int] {
  let array = []
  for i = 0; i < size; i = i + 1 {
    array.push(Random::int_in_range(0, 1000))
  }
  array
}

func binary_search(array : Array[Int], target : Int) -> Option[Int] {
  let mut low = 0
  let mut high = array.length() - 1
  
  while low <= high {
    let mid = (low + high) / 2
    let value = array[mid]
    
    if value == target {
      return Some(mid)
    } else if value < target {
      low = mid + 1
    } else {
      high = mid - 1
    }
  }
  
  None
}

func is_sorted(array : Array[Int]) -> Bool {
  for i = 1; i < array.length(); i = i + 1 {
    if array[i-1] > array[i] {
      return false
    }
  }
  true
}

func process_value(value : Int) -> Int {
  // Simulate some processing
  (value * 2 + 10) % 1000
}

// Mock implementations for testing
type PerformanceCounter
type System
type ObjectPool
type StringBuilder
type Json
type Regex
type LinkedList
type File
type HttpClient
type MetricsCollector
type Tracer
type Span
type Logger
type ThreadPool
type Future
type ConcurrentHashMap
type AtomicCounter
type LRUCache
type Memoizer
type Profiler
type MemoryProfiler
type Benchmark
type Thread
type Random

// Performance counter
func PerformanceCounter::now() -> PerformanceCounter { /* implementation */ }
func PerformanceCounter::duration_ms(start : PerformanceCounter, end : PerformanceCounter) -> Float { 0.0 }

// System utilities
func System::gc() -> Unit { /* implementation */ }
func System::memory_usage() -> Int { 0 }

// Object pool
func ObjectPool::new[T](factory : () -> T) -> ObjectPool[T] { /* implementation */ }
func ObjectPool::acquire[T](pool : ObjectPool[T]) -> T { /* implementation */ }
func ObjectPool::release[T](pool : ObjectPool[T], obj : T) -> Unit { /* implementation */ }

// String builder
func StringBuilder::new() -> StringBuilder { /* implementation */ }
func StringBuilder::append(builder : StringBuilder, str : String) -> Unit { /* implementation */ }
func StringBuilder::to_string(builder : StringBuilder) -> String { "" }

// JSON
func Json::parse(str : String) -> Result[JsonValue, String] { Ok(/* object */) }

// Regex
func Regex::compile(pattern : String) -> Regex { /* implementation */ }
func Regex::find_all(regex : Regex, text : String) -> Array[String] { [] }

// Linked list
func LinkedList::new[T]() -> LinkedList[T] { /* implementation */ }
func LinkedList::push_back[T](list : LinkedList[T], value : T) -> Unit { /* implementation */ }
func LinkedList::get_at[T](list : LinkedList[T], index : Int) -> Option[T] { Some(/* value */) }

// File operations
func File::write_all_text(path : String, content : String) -> Result[Unit, String] { Ok(()) }
func File::read_all_text(path : String) -> Result[String, String] { Ok("") }
func File::write_all_bytes(path : String, data : Array[Byte]) -> Result[Unit, String] { Ok(()) }
func File::delete(path : String) -> Unit { /* implementation */ }

// HTTP client
func HttpClient::get(url : String) -> Result[String, String] { Ok("{\"slideshow\": {\"author\": \"test\"}}") }

// Telemetry components
func MetricsCollector::new() -> MetricsCollector { /* implementation */ }
func MetricsCollector::record_counter(collector : MetricsCollector, name : String, value : Float) -> Unit { /* implementation */ }
func MetricsCollector::record_gauge(collector : MetricsCollector, name : String, value : Float) -> Unit { /* implementation */ }
func MetricsCollector::aggregate(collector : MetricsCollector) -> AggregatedMetrics { /* implementation */ }

func Tracer::new(service_name : String) -> Tracer { /* implementation */ }
func Tracer::start_span(tracer : Tracer, name : String) -> Span { /* implementation */ }

func Span::add_event(span : Span, name : String) -> Unit { /* implementation */ }
func Span::end(span : Span) -> Unit { /* implementation */ }

func Logger::new(name : String) -> Logger { /* implementation */ }
func Logger::log(logger : Logger, level : LogLevel, message : String) -> Unit { /* implementation */ }

// Thread pool
func ThreadPool::new(size : Int) -> ThreadPool { /* implementation */ }
func ThreadPool::submit[T](pool : ThreadPool, task : () -> T) -> Future[T] { /* implementation */ }

// Future
func Future::get[T](future : Future[T]) -> T { /* implementation */ }

// Atomic operations
func AtomicCounter::new(initial : Int) -> AtomicCounter { /* implementation */ }
func AtomicCounter::fetch_add(counter : AtomicCounter, value : Int) -> Int { 0 }
func AtomicCounter::load(counter : AtomicCounter) -> Int { 0 }

// LRU cache
func LRUCache::new[K, V](capacity : Int) -> LRUCache[K, V] { /* implementation */ }
func LRUCache::put[K, V](cache : LRUCache[K, V], key : K, value : V) -> Unit { /* implementation */ }
func LRUCache::get[K, V](cache : LRUCache[K, V], key : K) -> Option[V] { Some(/* value */) }
func LRUCache::size[K, V](cache : LRUCache[K, V]) -> Int { 0 }

// Memoizer
func Memoizer::new[T, R](func : T -> R) -> Memoizer[T, R] { /* implementation */ }
func Memoizer::call[T, R](memoizer : Memoizer[T, R], input : T) -> R { /* implementation */ }

// Profiling
func Profiler::new() -> Profiler { /* implementation */ }
func Profiler::start(profiler : Profiler) -> Unit { /* implementation */ }
func Profiler::stop(profiler : Profiler) -> Unit { /* implementation */ }
func Profiler::get_data(profiler : Profiler) -> ProfileData { /* implementation */ }

func MemoryProfiler::new() -> MemoryProfiler { /* implementation */ }
func MemoryProfiler::start(profiler : MemoryProfiler) -> Unit { /* implementation */ }
func MemoryProfiler::stop(profiler : MemoryProfiler) -> Unit { /* implementation */ }
func MemoryProfiler::get_data(profiler : MemoryProfiler) -> MemoryProfileData { /* implementation */ }

// Benchmark
func Benchmark::new() -> Benchmark { /* implementation */ }
func Benchmark::run(benchmark : Benchmark, name : String, func : () -> Unit) -> Unit { /* implementation */ }
func Benchmark::get_results(benchmark : Benchmark) -> Array[BenchmarkResult] { [] }

// Thread utilities
func Thread::sleep(ms : Int) -> Unit { /* implementation */ }

// Random utilities
func Random::int_in_range(min : Int, max : Int) -> Int { 0 }

// Array operations
func Array::copy[T](array : Array[T]) -> Array[T] { /* implementation */ }
func Array::quick_sort(array : Array[Int]) -> Unit { /* implementation */ }
func Array::merge_sort(array : Array[Int]) -> Unit { /* implementation */ }
func Array::heap_sort(array : Array[Int]) -> Unit { /* implementation */ }
func Array::sort(array : Array[Int]) -> Unit { /* implementation */ }
func Array::reverse[T](array : Array[T]) -> Unit { /* implementation */ }
func Array::slice[T](array : Array[T], start : Int, end : Int) -> Array[T] { /* implementation */ }

// Hash map operations
func HashMap::new[K, V]() -> HashMap[K, V] { /* implementation */ }
func HashMap::insert[K, V](map : HashMap[K, V], key : K, value : V) -> Unit { /* implementation */ }
func HashMap::get[K, V](map : HashMap[K, V], key : K) -> Option[V] { Some(/* value */) }
func HashMap::iter[K, V](map : HashMap[K, V]) -> Iterator[(K, V)] { /* implementation */ }

// Types
enum LogLevel { Debug, Info, Warn, Error, Fatal }

type JsonValue
type AggregatedMetrics
type ProfileData
type MemoryProfileData
type BenchmarkResult