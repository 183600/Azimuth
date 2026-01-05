// Azimuth Telemetry System - Performance Optimization Enhanced Tests
// This file contains comprehensive test cases for performance optimization techniques

// Test 1: Memory Pool Management
test "memory pool management for telemetry objects" {
  // Simple memory pool implementation
  type MemoryPool[T] = {
    objects: Array[T],
    available: Array[Int],
    create_fn: () -> T,
    reset_fn: (T) -> Unit
  }
  
  let create_pool = fn[T](size: Int, create_fn: () -> T, reset_fn: (T) -> Unit) -> MemoryPool[T] {
    let mut objects = []
    let mut available = []
    
    for i in 0..=size-1 {
      objects = objects + [create_fn()]
      available = available + [i]
    }
    
    { objects, available, create_fn, reset_fn }
  }
  
  let pool_acquire = fn[T](pool: MemoryPool[T]) -> (MemoryPool[T], Int) {
    let MemoryPool(objects, available, create_fn, reset_fn) = pool
    
    if available.length() > 0 {
      let index = available[0]
      let new_available = available.slice(1, available.length())
      ({ objects, available: new_available, create_fn, reset_fn }, index)
    } else {
      // Pool exhausted, create new object
      let new_objects = objects + [create_fn()]
      let new_index = objects.length()
      ({ objects: new_objects, available, create_fn, reset_fn }, new_index)
    }
  }
  
  let pool_release = fn[T](pool: MemoryPool[T], index: Int) -> MemoryPool[T] {
    let MemoryPool(objects, available, create_fn, reset_fn) = pool
    
    // Reset the object
    reset_fn(objects[index])
    
    // Add back to available list
    let new_available = available + [index]
    { objects, available: new_available, create_fn, reset_fn }
  }
  
  // Test with string objects
  let string_pool = create_pool(5, 
    fn() -> String { "" },
    fn(s: String) -> Unit { s.clear() }
  )
  
  let (pool1, index1) = pool_acquire(string_pool)
  let (pool2, index2) = pool_acquire(pool1)
  let (pool3, index3) = pool_acquire(pool2)
  
  // Verify we got different indices
  assert_true(index1 != index2)
  assert_true(index2 != index3)
  assert_true(index1 != index3)
  
  // Release objects back to pool
  let pool4 = pool_release(pool3, index1)
  let pool5 = pool_release(pool4, index2)
  
  // Acquire again, should get released indices
  let (pool6, index4) = pool_acquire(pool5)
  let (pool7, index5) = pool_acquire(pool6)
  
  // Should get the same indices that were released (in LIFO order)
  assert_eq(index4, index2)  // Last released
  assert_eq(index5, index1)  // First released
}

// Test 2: Efficient String Operations
test "efficient string operations for telemetry" {
  // Test string builder for efficient concatenation
  type StringBuilder = {
    buffer: Array[String],
    total_length: Int
  }
  
  let string_builder_new = fn() -> StringBuilder {
    { buffer: [], total_length: 0 }
  }
  
  let string_builder_append = fn(builder: StringBuilder, s: String) -> StringBuilder {
    { buffer: builder.buffer + [s], total_length: builder.total_length + s.length() }
  }
  
  let string_builder_build = fn(builder: StringBuilder) -> String {
    let mut result = ""
    for part in builder.buffer {
      result = result + part
    }
    result
  }
  
  // Test efficient string joining
  let join_strings = fn(strings: Array[String], separator: String) -> String {
    if strings.length() == 0 {
      return ""
    }
    
    let mut builder = string_builder_new()
    builder = string_builder_append(builder, strings[0])
    
    for i in 1..=strings.length()-1 {
      builder = string_builder_append(builder, separator)
      builder = string_builder_append(builder, strings[i])
    }
    
    string_builder_build(builder)
  }
  
  // Test string formatting for telemetry
  let format_telemetry_value = fn(prefix: String, key: String, value: String) -> String {
    let mut builder = string_builder_new()
    builder = string_builder_append(builder, prefix)
    builder = string_builder_append(builder, ".")
    builder = string_builder_append(builder, key)
    builder = string_builder_append(builder, "=")
    builder = string_builder_append(builder, value)
    string_builder_build(builder)
  }
  
  // Test string builder
  let builder1 = string_builder_new()
  let builder2 = string_builder_append(builder1, "Hello")
  let builder3 = string_builder_append(builder2, ", ")
  let builder4 = string_builder_append(builder3, "World")
  let result = string_builder_build(builder4)
  
  assert_eq(result, "Hello, World")
  
  // Test string joining
  let strings = ["apple", "banana", "cherry"]
  let joined = join_strings(strings, ", ")
  assert_eq(joined, "apple, banana, cherry")
  
  let empty_joined = join_strings([], ", ")
  assert_eq(empty_joined, "")
  
  let single_joined = join_strings(["single"], ", ")
  assert_eq(single_joined, "single")
  
  // Test telemetry formatting
  let formatted = format_telemetry_value("service", "name", "my-service")
  assert_eq(formatted, "service.name=my-service")
  
  let formatted2 = format_telemetry_value("metric", "counter.total", "42")
  assert_eq(formatted2, "metric.counter.total=42")
}

// Test 3: Efficient Data Structures
test "efficient data structures for telemetry" {
  // Test ring buffer for recent events
  type RingBuffer[T] = {
    buffer: Array[T],
    size: Int,
    head: Int,
    count: Int
  }
  
  let ring_buffer_new = fn[T](size: Int, default_value: T) -> RingBuffer[T] {
    let mut buffer = []
    for i in 0..=size-1 {
      buffer = buffer + [default_value]
    }
    { buffer, size, head: 0, count: 0 }
  }
  
  let ring_buffer_push = fn[T](rb: RingBuffer[T], item: T) -> RingBuffer[T] {
    let RingBuffer(buffer, size, head, count) = rb
    
    // Insert at head position
    let new_buffer = buffer.with(head, item)
    let new_head = (head + 1) % size
    let new_count = if count < size { count + 1 } else { size }
    
    { buffer: new_buffer, size, head: new_head, count: new_count }
  }
  
  let ring_buffer_get = fn[T](rb: RingBuffer[T], index: Int) -> T {
    let RingBuffer(buffer, size, head, count) = rb
    
    if index >= count {
      panic("Index out of bounds")
    }
    
    let actual_index = (head - 1 - index + size) % size
    buffer[actual_index]
  }
  
  // Test ring buffer with integers
  let rb1 = ring_buffer_new(3, 0)
  let rb2 = ring_buffer_push(rb1, 1)
  let rb3 = ring_buffer_push(rb2, 2)
  let rb4 = ring_buffer_push(rb3, 3)
  let rb5 = ring_buffer_push(rb4, 4)  // Should overwrite 1
  
  // Check contents (most recent first)
  assert_eq(ring_buffer_get(rb5, 0), 4)  // Most recent
  assert_eq(ring_buffer_get(rb5, 1), 3)
  assert_eq(ring_buffer_get(rb5, 2), 2)  // 1 was overwritten
  
  // Test with strings
  let string_rb1 = ring_buffer_new(2, "")
  let string_rb2 = ring_buffer_push(string_rb1, "first")
  let string_rb3 = ring_buffer_push(string_rb2, "second")
  let string_rb4 = ring_buffer_push(string_rb3, "third")  // Should overwrite "first"
  
  assert_eq(ring_buffer_get(string_rb4, 0), "third")
  assert_eq(ring_buffer_get(string_rb4, 1), "second")
}

// Test 4: Caching Mechanisms
test "caching mechanisms for telemetry data" {
  // Simple LRU cache implementation
  type LRUCache[K, V] = {
    capacity: Int,
    entries: Array[(K, V)],
    access_order: Array[K]
  }
  
  let lru_cache_new = fn[K, V](capacity: Int) -> LRUCache[K, V] {
    { capacity, entries: [], access_order: [] }
  }
  
  let lru_cache_get = fn[K: Eq, V](cache: LRUCache[K, V], key: K) -> (LRUCache[K, V], Option[V]) {
    let LRUCache(capacity, entries, access_order) = cache
    
    // Find the entry
    let mut found_index = -1
    for i in 0..=entries.length()-1 {
      if entries[i].0 == key {
        found_index = i
        break
      }
    }
    
    if found_index == -1 {
      return (cache, None)  // Not found
    }
    
    let value = entries[found_index].1
    
    // Update access order (move key to end)
    let mut new_access_order = []
    let mut found_in_order = false
    for k in access_order {
      if k == key {
        found_in_order = true
      } else {
        new_access_order = new_access_order + [k]
      }
    }
    new_access_order = new_access_order + [key]
    
    let new_cache = { capacity, entries, access_order: new_access_order }
    (new_cache, Some(value))
  }
  
  let lru_cache_put = fn[K: Eq, V](cache: LRUCache[K, V], key: K, value: V) -> LRUCache[K, V] {
    let LRUCache(capacity, entries, access_order) = cache
    
    // Check if key already exists
    let mut found_index = -1
    for i in 0..=entries.length()-1 {
      if entries[i].0 == key {
        found_index = i
        break
      }
    }
    
    let new_entries = if found_index != -1 {
      // Update existing entry
      entries.with(found_index, (key, value))
    } else if entries.length() < capacity {
      // Add new entry
      entries + [(key, value)]
    } else {
      // Cache is full, remove LRU entry
      let lru_key = access_order[0]
      let mut new_entries = []
      let mut found = false
      for (k, v) in entries {
        if k == lru_key {
          found = true
        } else {
          new_entries = new_entries + [(k, v)]
        }
      }
      new_entries + [(key, value)]
    }
    
    // Update access order
    let mut new_access_order = []
    let mut found_in_order = false
    for k in access_order {
      if k == key {
        found_in_order = true
      } else {
        new_access_order = new_access_order + [k]
      }
    }
    new_access_order = new_access_order + [key]
    
    { capacity, entries: new_entries, access_order: new_access_order }
  }
  
  // Test LRU cache
  let cache1 = lru_cache_new(3)
  let cache2 = lru_cache_put(cache1, "key1", "value1")
  let cache3 = lru_cache_put(cache2, "key2", "value2")
  let cache4 = lru_cache_put(cache3, "key3", "value3")
  
  // Get value and update access order
  let (cache5, value1) = lru_cache_get(cache4, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Add new entry, should evict key2 (LRU)
  let cache6 = lru_cache_put(cache5, "key4", "value4")
  let (cache7, value2) = lru_cache_get(cache6, "key2")
  match value2 {
    Some(_) => assert_true(false)  // Should have been evicted
    None => assert_true(true)
  }
  
  // key1 should still be in cache
  let (cache8, value1_again) = lru_cache_get(cache7, "key1")
  match value1_again {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
}

// Test 5: Batch Processing
test "batch processing for telemetry operations" {
  // Test batch processor
  type BatchProcessor[T] = {
    batch_size: Int,
    current_batch: Array[T],
    processor: (Array[T]) -> Unit
  }
  
  let batch_processor_new = fn[T](batch_size: Int, processor: (Array[T]) -> Unit) -> BatchProcessor[T] {
    { batch_size, current_batch: [], processor }
  }
  
  let batch_processor_add = fn[T](bp: BatchProcessor[T], item: T) -> BatchProcessor[T] {
    let BatchProcessor(batch_size, current_batch, processor) = bp
    
    let new_batch = current_batch + [item]
    
    if new_batch.length() >= batch_size {
      // Process the batch
      processor(new_batch)
      { batch_size, current_batch: [], processor }
    } else {
      { batch_size, current_batch: new_batch, processor }
    }
  }
  
  let batch_processor_flush = fn[T](bp: BatchProcessor[T]) -> BatchProcessor[T] {
    let BatchProcessor(batch_size, current_batch, processor) = bp
    
    if current_batch.length() > 0 {
      processor(current_batch)
    }
    
    { batch_size, current_batch: [], processor }
  }
  
  // Test batch processing
  let mut processed_batches = []
  
  let batch_processor = batch_processor_new(3, fn(batch: Array[String]) -> Unit {
    processed_batches = processed_batches + [batch]
  })
  
  // Add items
  let bp1 = batch_processor_add(batch_processor, "item1")
  let bp2 = batch_processor_add(bp1, "item2")
  let bp3 = batch_processor_add(bp2, "item3")  // Should trigger processing
  
  // Check that batch was processed
  assert_eq(processed_batches.length(), 1)
  assert_eq(processed_batches[0], ["item1", "item2", "item3"])
  
  // Add more items
  let bp4 = batch_processor_add(bp3, "item4")
  let bp5 = batch_processor_add(bp4, "item5")
  
  // Flush remaining items
  let bp6 = batch_processor_flush(bp5)
  
  // Check final batch
  assert_eq(processed_batches.length(), 2)
  assert_eq(processed_batches[1], ["item4", "item5"])
}

// Test 6: Lazy Evaluation
test "lazy evaluation for expensive telemetry operations" {
  // Test lazy value type
  type Lazy[T] = {
    evaluated: Bool,
    value: Option[T],
    evaluator: () -> T
  }
  
  let lazy_new = fn[T](evaluator: () -> T) -> Lazy[T] {
    { evaluated: false, value: None, evaluator }
  }
  
  let lazy_get = fn[T](lazy: Lazy[T]) -> (Lazy[T], T) {
    let Lazy(evaluated, value, evaluator) = lazy
    
    if evaluated {
      (lazy, value.unwrap())
    } else {
      let computed_value = evaluator()
      let new_lazy = { evaluated: true, value: Some(computed_value), evaluator }
      (new_lazy, computed_value)
    }
  }
  
  // Test with expensive computation
  let mut call_count = 0
  
  let expensive_computation = fn() -> Int {
    call_count = call_count + 1
    // Simulate expensive work
    let mut sum = 0
    for i in 0..=1000 {
      sum = sum + i
    }
    sum
  }
  
  let lazy_value = lazy_new(expensive_computation)
  
  // Value should not be computed yet
  assert_eq(call_count, 0)
  
  // Get the value
  let (lazy1, value1) = lazy_get(lazy_value)
  
  // Value should be computed once
  assert_eq(call_count, 1)
  assert_eq(value1, 500500)  // Sum of 0..1000
  
  // Get the value again
  let (_, value2) = lazy_get(lazy1)
  
  // Should not be computed again
  assert_eq(call_count, 1)
  assert_eq(value2, 500500)
}

// Test 7: Efficient Serialization
test "efficient serialization for telemetry data" {
  // Test compact binary representation
  let serialize_int = fn(value: Int) -> Array[Byte] {
    // Simple variable-length encoding
    let mut bytes = []
    let mut v = value
    
    while v >= 128 {
      bytes = bytes + [Byte::from_int(v % 128 + 128)]
      v = v / 128
    }
    
    bytes = bytes + [Byte::from_int(v)]
    bytes
  }
  
  let serialize_string = fn(s: String) -> Array[Byte] {
    // Length-prefixed string
    let length_bytes = serialize_int(s.length())
    let mut string_bytes = []
    
    for char in s.to_char_array() {
      string_bytes = string_bytes + [Byte::from_int(char.to_int())]
    }
    
    length_bytes + string_bytes
  }
  
  let serialize_attribute = fn(key: String, value: String) -> Array[Byte] {
    serialize_string(key) + serialize_string(value)
  }
  
  // Test serialization
  let int_bytes = serialize_int(42)
  assert_eq(int_bytes.length(), 1)
  assert_eq(int_bytes[0].to_int(), 42)
  
  let large_int_bytes = serialize_int(300)
  assert_eq(large_int_bytes.length(), 2)
  assert_eq(large_int_bytes[0].to_int(), 300 % 128 + 128)
  assert_eq(large_int_bytes[1].to_int(), 300 / 128)
  
  let string_bytes = serialize_string("hello")
  // Length (5) + characters
  assert_eq(string_bytes.length(), 6)
  assert_eq(string_bytes[0].to_int(), 5)  // Length
  
  let attr_bytes = serialize_attribute("key", "value")
  // Key length + key + value length + value
  assert_eq(attr_bytes.length(), 10)  // 1 + 3 + 1 + 5
}

// Test 8: Memory-Efficient Aggregations
test "memory-efficient aggregations for metrics" {
  // Test streaming aggregation
  type StreamingAggregation = {
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    sum_of_squares: Float
  }
  
  let aggregation_new = fn() -> StreamingAggregation {
    { count: 0, sum: 0.0, min: Float::infinity(), max: Float::neg_infinity(), sum_of_squares: 0.0 }
  }
  
  let aggregation_add = fn(agg: StreamingAggregation, value: Float) -> StreamingAggregation {
    let StreamingAggregation(count, sum, min, max, sum_of_squares) = agg
    
    {
      count: count + 1,
      sum: sum + value,
      min: if value < min { value } else { min },
      max: if value > max { value } else { max },
      sum_of_squares: sum_of_squares + value * value
    }
  }
  
  let aggregation_mean = fn(agg: StreamingAggregation) -> Float {
    if agg.count == 0 {
      0.0
    } else {
      agg.sum / Float::from_int(agg.count)
    }
  }
  
  let aggregation_variance = fn(agg: StreamingAggregation) -> Float {
    if agg.count <= 1 {
      0.0
    } else {
      let mean = aggregation_mean(agg)
      (agg.sum_of_squares / Float::from_int(agg.count)) - (mean * mean)
    }
  }
  
  let aggregation_merge = fn(agg1: StreamingAggregation, agg2: StreamingAggregation) -> StreamingAggregation {
    {
      count: agg1.count + agg2.count,
      sum: agg1.sum + agg2.sum,
      min: if agg1.min < agg2.min { agg1.min } else { agg2.min },
      max: if agg1.max > agg2.max { agg1.max } else { agg2.max },
      sum_of_squares: agg1.sum_of_squares + agg2.sum_of_squares
    }
  }
  
  // Test aggregation
  let values = [1.0, 2.0, 3.0, 4.0, 5.0]
  
  let mut agg = aggregation_new()
  for value in values {
    agg = aggregation_add(agg, value)
  }
  
  assert_eq(agg.count, 5)
  assert_eq(agg.sum, 15.0)
  assert_eq(agg.min, 1.0)
  assert_eq(agg.max, 5.0)
  assert_eq(agg.sum_of_squares, 55.0)
  
  let mean = aggregation_mean(agg)
  assert_eq(mean, 3.0)
  
  let variance = aggregation_variance(agg)
  assert_eq(variance, 2.0)
  
  // Test merging aggregations
  let agg1 = aggregation_new()
  let agg2 = aggregation_new()
  
  let mut final_agg1 = agg1
  let mut final_agg2 = agg2
  
  for i in 0..=2 {
    final_agg1 = aggregation_add(final_agg1, Float::from_int(i + 1))
  }
  
  for i in 3..=4 {
    final_agg2 = aggregation_add(final_agg2, Float::from_int(i + 1))
  }
  
  let merged = aggregation_merge(final_agg1, final_agg2)
  
  assert_eq(merged.count, 5)
  assert_eq(merged.sum, 15.0)
  assert_eq(merged.min, 1.0)
  assert_eq(merged.max, 5.0)
}

// Test 9: Efficient Sampling
test "efficient sampling strategies for telemetry" {
  // Test reservoir sampling
  let reservoir_sample = fn[T](stream: Array[T], k: Int) -> Array[T] {
    if stream.length() <= k {
      return stream
    }
    
    let mut reservoir = []
    
    // Fill reservoir with first k elements
    for i in 0..=k-1 {
      reservoir = reservoir + [stream[i]]
    }
    
    // Process remaining elements
    for i in k..=stream.length()-1 {
      let j = (i + 1).to_int()  // Random replacement position (simplified)
      if j < k {
        reservoir = reservoir.with(j, stream[i])
      }
    }
    
    reservoir
  }
  
  // Test systematic sampling
  let systematic_sample = fn[T](stream: Array[T], k: Int) -> Array[T] {
    if stream.length() <= k {
      return stream
    }
    
    let step = stream.length() / k
    let mut sample = []
    
    for i in 0..=k-1 {
      let index = i * step
      sample = sample + [stream[index]]
    }
    
    sample
  }
  
  // Test sampling
  let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let reservoir = reservoir_sample(data, 3)
  assert_eq(reservoir.length(), 3)
  
  let systematic = systematic_sample(data, 3)
  assert_eq(systematic.length(), 3)
  assert_eq(systematic, [1, 4, 7])  // Every 3rd element
  
  // Test with smaller k than data size
  let small_data = [1, 2, 3]
  let small_sample = reservoir_sample(small_data, 5)
  assert_eq(small_sample.length(), 3)  // Should return all data
}

// Test 10: Performance Monitoring
test "performance monitoring and optimization" {
  // Test simple performance timer
  type Timer = {
    start_time: Int,
    end_time: Option[Int]
  }
  
  let timer_start = fn() -> Timer {
    { start_time: 1000000, end_time: None }  // Mock start time
  }
  
  let timer_stop = fn(timer: Timer) -> Timer {
    { start_time: timer.start_time, end_time: Some(1001000) }  // Mock end time
  }
  
  let timer_elapsed = fn(timer: Timer) -> Int {
    match timer.end_time {
      Some(end) => end - timer.start_time
      None => 0  // Timer not stopped
    }
  }
  
  // Test performance budgeting
  type PerformanceBudget = {
    max_time: Int,
    current_time: Int
  }
  
  let budget_check = fn(budget: PerformanceBudget, operation_time: Int) -> Bool {
    budget.current_time + operation_time <= budget.max_time
  }
  
  let budget_update = fn(budget: PerformanceBudget, operation_time: Int) -> PerformanceBudget {
    {
      max_time: budget.max_time,
      current_time: budget.current_time + operation_time
    }
  }
  
  // Test timer
  let timer1 = timer_start()
  let timer2 = timer_stop(timer1)
  let elapsed = timer_elapsed(timer2)
  
  assert_eq(elapsed, 1000)  // 1001000 - 1000000
  
  // Test performance budget
  let budget = { max_time: 5000, current_time: 0 }
  
  assert_true(budget_check(budget, 1000))
  assert_true(budget_check(budget, 5000))
  assert_false(budget_check(budget, 6000))
  
  let budget1 = budget_update(budget, 1000)
  assert_eq(budget1.current_time, 1000)
  assert_true(budget_check(budget1, 3000))
  assert_false(budget_check(budget1, 5000))
  
  let budget2 = budget_update(budget1, 3000)
  assert_eq(budget2.current_time, 4000)
  assert_true(budget_check(budget2, 1000))
  assert_false(budget_check(budget2, 2000))
}