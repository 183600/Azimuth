// Performance Optimization Test Cases for Azimuth Telemetry System
// This file contains test cases for performance optimization techniques

// Test 1: Memory Pool Pattern
test "memory pool pattern for performance" {
  // Simple memory pool implementation for reusable objects
  type PoolItem {
    id : Int
    data : String
    in_use : Bool
  }
  
  type MemoryPool {
    items : Array[PoolItem]
    available_indices : Array[Int]
  }
  
  let create_pool = fn(size: Int) -> MemoryPool {
    let mut items = []
    let mut available_indices = []
    
    for i in 0..size {
      items.push(PoolItem {
        id: i,
        data: "",
        in_use: false
      })
      available_indices.push(i)
    }
    
    MemoryPool {
      items: items,
      available_indices: available_indices
    }
  }
  
  let acquire = fn(pool: MemoryPool) -> (Int, MemoryPool) {
    if pool.available_indices.length() == 0 {
      (-1, pool) // Pool exhausted
    } else {
      let index = pool.available_indices[0]
      let mut new_items = []
      let mut new_available = []
      
      // Update item to mark as in use
      for i in 0..pool.items.length() {
        if i == index {
          new_items.push(PoolItem {
            id: pool.items[i].id,
            data: pool.items[i].data,
            in_use: true
          })
        } else {
          new_items.push(pool.items[i])
        }
      }
      
      // Remove index from available list
      for i in 1..pool.available_indices.length() {
        new_available.push(pool.available_indices[i])
      }
      
      (index, MemoryPool {
        items: new_items,
        available_indices: new_available
      })
    }
  }
  
  let release = fn(pool: MemoryPool, index: Int) -> MemoryPool {
    if index < 0 || index >= pool.items.length() {
      return pool // Invalid index
    }
    
    let mut new_items = []
    let mut new_available = pool.available_indices
    
    // Update item to mark as not in use
    for i in 0..pool.items.length() {
      if i == index {
        new_items.push(PoolItem {
          id: pool.items[i].id,
          data: pool.items[i].data,
          in_use: false
        })
        new_available.push(index) // Add back to available list
      } else {
        new_items.push(pool.items[i])
      }
    }
    
    MemoryPool {
      items: new_items,
      available_indices: new_available
    }
  }
  
  let mut pool = create_pool(5)
  
  // Acquire items
  let (id1, pool1) = acquire(pool)
  let (id2, pool2) = acquire(pool1)
  let (id3, pool3) = acquire(pool2)
  
  assert_eq(id1, 0)
  assert_eq(id2, 1)
  assert_eq(id3, 2)
  
  // Release an item
  let pool4 = release(pool3, id1)
  
  // Acquire again - should get the released item
  let (id4, _) = acquire(pool4)
  assert_eq(id4, 0) // Should get the first available index
}

// Test 2: Lazy Initialization Pattern
test "lazy initialization pattern" {
  type LazyValue {
    initialized : Bool
    value : Int
    initializer : () -> Int
  }
  
  let create_lazy = fn(initializer: () -> Int) -> LazyValue {
    LazyValue {
      initialized: false,
      value: 0,
      initializer: initializer
    }
  }
  
  let get = fn(lazy: LazyValue) -> (Int, LazyValue) {
    if lazy.initialized {
      (lazy.value, lazy)
    } else {
      let computed_value = lazy.initializer()
      (computed_value, LazyValue {
        initialized: true,
        value: computed_value,
        initializer: lazy.initializer
      })
    }
  }
  
  // Expensive computation
  let expensive_computation = fn() -> Int {
    // Simulate expensive computation
    let mut sum = 0
    for i in 0..1000 {
      sum = sum + i
    }
    sum
  }
  
  let lazy_value = create_lazy(expensive_computation)
  
  // First access should compute the value
  let (value1, lazy1) = get(lazy_value)
  assert_eq(value1, 499500) // Sum of 0..999
  assert_true(lazy1.initialized)
  
  // Second access should return cached value
  let (value2, _) = get(lazy1)
  assert_eq(value2, 499500)
}

// Test 3: Batching Operations
test "batching operations for efficiency" {
  type Batch {
    operations : Array[String]
    max_size : Int
  }
  
  let create_batch = fn(max_size: Int) -> Batch {
    Batch {
      operations: [],
      max_size: max_size
    }
  }
  
  let add_operation = fn(batch: Batch, operation: String) -> Batch {
    if batch.operations.length() < batch.max_size {
      let mut new_operations = batch.operations
      new_operations.push(operation)
      Batch {
        operations: new_operations,
        max_size: batch.max_size
      }
    } else {
      batch // Batch is full
    }
  }
  
  let is_full = fn(batch: Batch) -> Bool {
    batch.operations.length() >= batch.max_size
  }
  
  let flush = fn(batch: Batch) -> (Array[String], Batch) {
    let operations = batch.operations
    (operations, create_batch(batch.max_size))
  }
  
  let mut batch = create_batch(3)
  
  // Add operations
  batch = add_operation(batch, "op1")
  batch = add_operation(batch, "op2")
  assert_false(is_full(batch))
  
  batch = add_operation(batch, "op3")
  assert_true(is_full(batch))
  
  // Flush the batch
  let (operations, new_batch) = flush(batch)
  assert_eq(operations.length(), 3)
  assert_eq(operations[0], "op1")
  assert_eq(operations[1], "op2")
  assert_eq(operations[2], "op3")
  assert_eq(new_batch.operations.length(), 0)
}

// Test 4: Caching Mechanism
test "caching mechanism for performance" {
  type CacheEntry {
    value : String
    timestamp : Int
  }
  
  type Cache {
    entries : Array[(String, CacheEntry)]
    max_size : Int
    current_time : Int
  }
  
  let create_cache = fn(max_size: Int) -> Cache {
    Cache {
      entries: [],
      max_size: max_size,
      current_time: 0
    }
  }
  
  let advance_time = fn(cache: Cache, seconds: Int) -> Cache {
    Cache {
      entries: cache.entries,
      max_size: cache.max_size,
      current_time: cache.current_time + seconds
    }
  }
  
  let get = fn(cache: Cache, key: String, ttl: Int) -> (Option[String], Cache) {
    for entry in cache.entries {
      if entry.0 == key {
        let age = cache.current_time - entry.1.timestamp
        if age <= ttl {
          return (Some(entry.1.value), cache)
        } else {
          // Entry expired, remove it
          let mut new_entries = []
          for e in cache.entries {
            if e.0 != key {
              new_entries.push(e)
            }
          }
          return (None, Cache {
            entries: new_entries,
            max_size: cache.max_size,
            current_time: cache.current_time
          })
        }
      }
    }
    (None, cache)
  }
  
  let put = fn(cache: Cache, key: String, value: String) -> Cache {
    // Remove existing entry if present
    let mut new_entries = []
    for e in cache.entries {
      if e.0 != key {
        new_entries.push(e)
      }
    }
    
    // Add new entry
    new_entries.push((key, CacheEntry {
      value: value,
      timestamp: cache.current_time
    }))
    
    // Remove oldest entries if over capacity
    while new_entries.length() > cache.max_size {
      let mut temp = []
      for i in 1..new_entries.length() {
        temp.push(new_entries[i])
      }
      new_entries = temp
    }
    
    Cache {
      entries: new_entries,
      max_size: cache.max_size,
      current_time: cache.current_time
    }
  }
  
  let mut cache = create_cache(2)
  
  // Add entries
  cache = put(cache, "key1", "value1")
  cache = put(cache, "key2", "value2")
  
  // Get entries
  let (value1, _) = get(cache, "key1", 10)
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Advance time and expire entry
  cache = advance_time(cache, 15)
  let (expired_value, cache2) = get(cache, "key1", 10)
  match expired_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Add new entry, should replace expired one
  cache2 = put(cache2, "key3", "value3")
  let (value3, _) = get(cache2, "key3", 10)
  match value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
}

// Test 5: Object Pooling for Strings
test "object pooling for strings" {
  type StringPool {
    strings : Array[String]
    in_use : Array[Bool]
  }
  
  let create_string_pool = fn(size: Int) -> StringPool {
    let mut strings = []
    let mut in_use = []
    
    for i in 0..size {
      strings.push("")
      in_use.push(false)
    }
    
    StringPool {
      strings: strings,
      in_use: in_use
    }
  }
  
  let acquire_string = fn(pool: StringPool) -> (String, StringPool) {
    for i in 0..pool.strings.length() {
      if !pool.in_use[i] {
        let mut new_in_use = []
        for j in 0..pool.in_use.length() {
          new_in_use.push(if j == i { true } else { pool.in_use[j] })
        }
        return ("", StringPool {
          strings: pool.strings,
          in_use: new_in_use
        })
      }
    }
    ("", pool) // Pool exhausted
  }
  
  let release_string = fn(pool: StringPool, index: Int) -> StringPool {
    if index < 0 || index >= pool.strings.length() {
      return pool
    }
    
    let mut new_in_use = []
    for i in 0..pool.in_use.length() {
      new_in_use.push(if i == index { false } else { pool.in_use[i] })
    }
    
    StringPool {
      strings: pool.strings,
      in_use: new_in_use
    }
  }
  
  let set_string = fn(pool: StringPool, index: Int, value: String) -> StringPool {
    if index < 0 || index >= pool.strings.length() {
      return pool
    }
    
    let mut new_strings = []
    for i in 0..pool.strings.length() {
      new_strings.push(if i == index { value } else { pool.strings[i] })
    }
    
    StringPool {
      strings: new_strings,
      in_use: pool.in_use
    }
  }
  
  let mut pool = create_string_pool(3)
  
  // Acquire strings
  let (str1, pool1) = acquire_string(pool)
  let (str2, pool2) = acquire_string(pool1)
  
  // Set values
  pool2 = set_string(pool2, 0, "first")
  pool2 = set_string(pool2, 1, "second")
  
  assert_eq(pool2.strings[0], "first")
  assert_eq(pool2.strings[1], "second")
  assert_true(pool2.in_use[0])
  assert_true(pool2.in_use[1])
  
  // Release strings
  pool2 = release_string(pool2, 0)
  assert_false(pool2.in_use[0])
  assert_true(pool2.in_use[1])
}

// Test 6: Resource Pooling with Cleanup
test "resource pooling with cleanup" {
  type Resource {
    id : Int
    data : String
    is_dirty : Bool
  }
  
  type ResourcePool {
    resources : Array[Resource]
    available : Array[Int]
  }
  
  let create_resource_pool = fn(size: Int) -> ResourcePool {
    let mut resources = []
    let mut available = []
    
    for i in 0..size {
      resources.push(Resource {
        id: i,
        data: "",
        is_dirty: false
      })
      available.push(i)
    }
    
    ResourcePool {
      resources: resources,
      available: available
    }
  }
  
  let acquire_resource = fn(pool: ResourcePool) -> (Int, ResourcePool) {
    if pool.available.length() == 0 {
      (-1, pool)
    } else {
      let index = pool.available[0]
      let mut new_available = []
      for i in 1..pool.available.length() {
        new_available.push(pool.available[i])
      }
      
      (index, ResourcePool {
        resources: pool.resources,
        available: new_available
      })
    }
  }
  
  let release_resource = fn(pool: ResourcePool, index: Int) -> ResourcePool {
    if index < 0 || index >= pool.resources.length() {
      return pool
    }
    
    // Mark resource as clean and add back to available
    let mut new_resources = []
    for i in 0..pool.resources.length() {
      if i == index {
        new_resources.push(Resource {
          id: pool.resources[i].id,
          data: pool.resources[i].data,
          is_dirty: false
        })
      } else {
        new_resources.push(pool.resources[i])
      }
    }
    
    let mut new_available = pool.available
    new_available.push(index)
    
    ResourcePool {
      resources: new_resources,
      available: new_available
    }
  }
  
  let mark_dirty = fn(pool: ResourcePool, index: Int) -> ResourcePool {
    if index < 0 || index >= pool.resources.length() {
      return pool
    }
    
    let mut new_resources = []
    for i in 0..pool.resources.length() {
      if i == index {
        new_resources.push(Resource {
          id: pool.resources[i].id,
          data: pool.resources[i].data,
          is_dirty: true
        })
      } else {
        new_resources.push(pool.resources[i])
      }
    }
    
    ResourcePool {
      resources: new_resources,
      available: pool.available
    }
  }
  
  let set_data = fn(pool: ResourcePool, index: Int, data: String) -> ResourcePool {
    if index < 0 || index >= pool.resources.length() {
      return pool
    }
    
    let mut new_resources = []
    for i in 0..pool.resources.length() {
      if i == index {
        new_resources.push(Resource {
          id: pool.resources[i].id,
          data: data,
          is_dirty: pool.resources[i].is_dirty
        })
      } else {
        new_resources.push(pool.resources[i])
      }
    }
    
    ResourcePool {
      resources: new_resources,
      available: pool.available
    }
  }
  
  let mut pool = create_resource_pool(3)
  
  // Acquire a resource
  let (index, pool1) = acquire_resource(pool)
  assert_eq(index, 0)
  
  // Use the resource
  pool1 = set_data(pool1, index, "important data")
  pool1 = mark_dirty(pool1, index)
  
  assert_eq(pool1.resources[index].data, "important data")
  assert_true(pool1.resources[index].is_dirty)
  
  // Release the resource (should be cleaned up)
  let pool2 = release_resource(pool1, index)
  assert_false(pool2.resources[index].is_dirty)
  
  // Resource should be available again
  let (new_index, _) = acquire_resource(pool2)
  assert_eq(new_index, 0)
}

// Test 7: Efficient String Concatenation
test "efficient string concatenation" {
  // Instead of repeated string concatenation, use array join
  let inefficient_concat = fn() -> String {
    let mut result = ""
    for i in 0..100 {
      result = result + "item" + i.to_string() + ","
    }
    result
  }
  
  let efficient_concat = fn() -> String {
    let mut parts = []
    for i in 0..100 {
      parts.push("item" + i.to_string())
      parts.push(",")
    }
    let mut result = ""
    for part in parts {
      result = result + part
    }
    result
  }
  
  // Both should produce the same result
  let result1 = inefficient_concat()
  let result2 = efficient_concat()
  
  // Check that they start the same way
  assert_eq(result1.substring(0, 20), "item0,item1,item2,item3")
  assert_eq(result2.substring(0, 20), "item0,item1,item2,item3")
  
  // Check that they end the same way
  let len1 = result1.length()
  let len2 = result2.length()
  assert_eq(len1, len2)
  assert_eq(result1.substring(len1 - 10, len1), "item99,")
  assert_eq(result2.substring(len2 - 10, len2), "item99,")
}

// Test 8: Memoization Pattern
test "memoization pattern for expensive computations" {
  type MemoCache {
    cache : Array[(Int, Int)]
  }
  
  let create_memo_cache = fn() -> MemoCache {
    MemoCache { cache: [] }
  }
  
  let fibonacci_memo = fn(n: Int, cache: MemoCache) -> (Int, MemoCache) {
    // Check if result is already cached
    for entry in cache.cache {
      if entry.0 == n {
        return (entry.1, cache)
      }
    }
    
    // Base cases
    if n <= 1 {
      return (n, cache)
    }
    
    // Recursive computation
    let (result1, cache1) = fibonacci_memo(n - 1, cache)
    let (result2, cache2) = fibonacci_memo(n - 2, cache1)
    let result = result1 + result2
    
    // Cache the result
    let mut new_cache = []
    for entry in cache2.cache {
      new_cache.push(entry)
    }
    new_cache.push((n, result))
    
    (result, MemoCache { cache: new_cache })
  }
  
  let cache = create_memo_cache()
  
  // First computation
  let (fib10, _) = fibonacci_memo(10, cache)
  assert_eq(fib10, 55)
  
  // Second computation should be faster due to caching
  let (fib10_again, _) = fibonacci_memo(10, cache)
  assert_eq(fib10_again, 55)
  
  // Different computation
  let (fib12, _) = fibonacci_memo(12, cache)
  assert_eq(fib12, 144)
}