// Azimuth Performance Optimization Tests
// This file contains tests for performance optimization techniques and patterns

// Test 1: Algorithm Complexity Analysis
test "algorithm complexity and efficiency" {
  // Linear search O(n)
  let linear_search = fn(arr : [Int], target : Int) {
    let mut found = false
    for item in arr {
      if item == target {
        found = true
        break
      }
    }
    found
  }
  
  // Binary search O(log n) - requires sorted array
  let binary_search = fn(arr : [Int], target : Int) {
    let mut left = 0
    let mut right = arr.length() - 1
    let mut found = false
    
    while left <= right {
      let mid = (left + right) / 2
      if arr[mid] == target {
        found = true
        break
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    found
  }
  
  let large_array = []
  for i in 0..=1000 {
    large_array = large_array.push(i)
  }
  
  // Test both searches find the same results
  let target = 500
  let linear_result = linear_search(large_array, target)
  let binary_result = binary_search(large_array, target)
  
  assert_eq(linear_result, true)
  assert_eq(binary_result, true)
  
  // Test non-existent target
  let non_existent = 1500
  let linear_not_found = linear_search(large_array, non_existent)
  let binary_not_found = binary_search(large_array, non_existent)
  
  assert_eq(linear_not_found, false)
  assert_eq(binary_not_found, false)
  
  // Verify binary search is more efficient for large arrays
  // (This is more of a conceptual test since we can't measure actual performance)
  assert_true(large_array.length() > 100)
}

// Test 2: Caching and Memoization Performance
test "caching and memoization for performance" {
  // Expensive computation simulation
  let expensive_fibonacci = fn(n : Int) {
    if n <= 1 {
      n
    } else {
      expensive_fibonacci(n - 1) + expensive_fibonacci(n - 2)
    }
  }
  
  // Memoized version
  let memoized_fibonacci = fn() {
    let cache = []
    fn(n : Int) {
      if n < cache.length() {
        cache[n]
      } else {
        let result = if n <= 1 {
          n
        } else {
          let f1 = memoized_fibonacci()(n - 1)
          let f2 = memoized_fibonacci()(n - 2)
          f1 + f2
        }
        // Extend cache (simplified approach)
        if n >= cache.length() {
          let mut new_cache = cache
          for i in cache.length()..=n {
            if i <= 1 {
              new_cache = new_cache.push(i)
            } else {
              new_cache = new_cache.push(new_cache[i-1] + new_cache[i-2])
            }
          }
          cache = new_cache
        }
        result
      }
    }
  }
  
  // Test both produce same results
  let fib_10 = expensive_fibonacci(10)
  let memo_fib_10 = memoized_fibonacci()(10)
  
  assert_eq(fib_10, 55)
  assert_eq(memo_fib_10, 55)
  
  // Test memoization consistency
  let memo_fib_10_again = memoized_fibonacci()(10)
  assert_eq(memo_fib_10_again, 55)
}

// Test 3: Lazy Evaluation for Performance
test "lazy evaluation performance benefits" {
  // Eager evaluation - computes all values immediately
  let eager_range = fn(start : Int, end : Int) {
    let mut result = []
    for i in start..=end {
      result = result.push(i * i) // Expensive computation
    }
    result
  }
  
  // Lazy evaluation - computes values only when needed
  let lazy_range = fn(start : Int, end : Int) {
    let mut current = start
    fn() {
      if current <= end {
        let result = current * current // Expensive computation
        current = current + 1
        Some(result)
      } else {
        None
      }
    }
  }
  
  // Test eager evaluation
  let eager_result = eager_range(1, 5)
  assert_eq(eager_result, [1, 4, 9, 16, 25])
  
  // Test lazy evaluation
  let lazy_generator = lazy_range(1, 5)
  let mut lazy_result = []
  
  // Only compute first 3 values
  for _ in 0..<3 {
    match lazy_generator() {
      Some(value) => lazy_result = lazy_result.push(value),
      None => break
    }
  }
  
  assert_eq(lazy_result, [1, 4, 9])
  assert_eq(lazy_result.length(), 3)
  
  // Verify lazy computation only computes what's needed
  assert_true(lazy_result.length() < eager_result.length())
}

// Test 4: Batch Processing Optimization
test "batch processing for performance" {
  // Individual processing
  let process_individual = fn(items : [Int]) {
    let mut results = []
    for item in items {
      // Simulate expensive operation
      let processed = item * item + item
      results = results.push(processed)
    }
    results
  }
  
  // Batch processing
  let process_batch = fn(items : [Int], batch_size : Int) {
    let mut results = []
    let mut i = 0
    
    while i < items.length() {
      let batch_end = if i + batch_size < items.length() {
        i + batch_size
      } else {
        items.length()
      }
      
      let batch = items.slice(i, batch_end)
      
      // Process batch efficiently
      let batch_results = batch.map(fn(item) { item * item + item })
      results = results.concat(batch_results)
      
      i = batch_end
    }
    
    results
  }
  
  let test_items = []
  for i in 1..=20 {
    test_items = test_items.push(i)
  }
  
  let individual_results = process_individual(test_items)
  let batch_results = process_batch(test_items, 5)
  
  assert_eq(individual_results, batch_results)
  assert_eq(individual_results.length(), 20)
  
  // Verify first few results
  assert_eq(individual_results[0], 2)   // 1*1 + 1
  assert_eq(individual_results[1], 6)   // 2*2 + 2
  assert_eq(individual_results[2], 12)  // 3*3 + 3
}

// Test 5: Memory Pool Pattern
test "memory pool pattern for object reuse" {
  // Simulate memory pool for expensive objects
  type ExpensiveObject = {
    id : Int
    data : String
    computed_value : Int
  }
  
  let create_expensive_object = fn(id : Int) {
    // Simulate expensive initialization
    let data = "Data for object " + id.to_string()
    let computed_value = id * id * id  // Expensive computation
    { id: id, data: data, computed_value: computed_value }
  }
  
  // Object pool simulation
  let mut object_pool = []
  let mut next_id = 0
  
  let get_from_pool = fn() {
    if object_pool.length() > 0 {
      let obj = object_pool[0]
      object_pool = object_pool.slice(1, object_pool.length())
      obj
    } else {
      create_expensive_object(next_id)
    }
  }
  
  let return_to_pool = fn(obj : ExpensiveObject) {
    object_pool = object_pool.push(obj)
  }
  
  // Test object reuse
  let obj1 = get_from_pool()
  next_id = next_id + 1
  assert_eq(obj1.id, 0)
  
  let obj2 = get_from_pool()
  next_id = next_id + 1
  assert_eq(obj2.id, 1)
  
  // Return objects to pool
  return_to_pool(obj1)
  return_to_pool(obj2)
  
  // Get objects from pool (should reuse)
  let obj3 = get_from_pool()
  assert_eq(obj3.id, 0) // Reused from pool
  
  let obj4 = get_from_pool()
  assert_eq(obj4.id, 1) // Reused from pool
}

// Test 6: String Optimization
test "string operations optimization" {
  // Inefficient string concatenation
  let inefficient_concat = fn(parts : [String]) {
    let mut result = ""
    for part in parts {
      result = result + part + " "  // Creates new string each time
    }
    result
  }
  
  // Efficient string building (simulated)
  let efficient_concat = fn(parts : [String]) {
    let mut result_parts = []
    for part in parts {
      result_parts = result_parts.push(part)
      result_parts = result_parts.push(" ")
    }
    
    // Single join operation
    let mut result = ""
    for part in result_parts {
      result = result + part
    }
    result
  }
  
  let string_parts = ["Hello", "world", "this", "is", "a", "test"]
  
  let inefficient_result = inefficient_concat(string_parts)
  let efficient_result = efficient_concat(string_parts)
  
  assert_eq(inefficient_result, efficient_result)
  assert_eq(inefficient_result, "Hello world this is a test ")
  
  // Test string builder pattern
  let string_builder = fn() {
    let mut parts = []
    fn(part : String) {
      parts = parts.push(part)
    },
    fn() {
      let mut result = ""
      for part in parts {
        result = result + part
      }
      result
    }
  }
  
  let (add_part, build) = string_builder()
  add_part("Hello")
  add_part(" ")
  add_part("world")
  
  let built_string = build()
  assert_eq(built_string, "Hello world")
}

// Test 7: Data Structure Selection Optimization
test "optimal data structure selection" {
  // Array-based operations (good for indexed access)
  let array_operations = fn() {
    let arr = [10, 20, 30, 40, 50]
    
    // Fast indexed access
    let first = arr[0]
    let last = arr[arr.length() - 1]
    
    // Slow search
    let contains_30 = arr.contains(30)
    
    (first, last, contains_30)
  }
  
  // Map-based operations (good for key-value lookup)
  let map_operations = fn() {
    // Simulate map with array of tuples
    let map = [("key1", 10), ("key2", 20), ("key3", 30)]
    
    // Fast lookup by key
    let find_value = fn(key : String) {
      let mut result = None
      for pair in map {
        if pair.0 == key {
          result = Some(pair.1)
          break
        }
      }
      result
    }
    
    let value1 = find_value("key2")
    let value2 = find_value("key4") // Non-existent
    
    (value1, value2)
  }
  
  let (arr_first, arr_last, arr_contains) = array_operations()
  let (map_value1, map_value2) = map_operations()
  
  assert_eq(arr_first, 10)
  assert_eq(arr_last, 50)
  assert_true(arr_contains)
  
  match map_value1 {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
  
  match map_value2 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 8: Early Exit Optimization
test "early exit optimization patterns" {
  // Find first match with early exit
  let find_first_match = fn(arr : [Int], predicate : (Int) -> Bool) {
    let mut result = None
    for item in arr {
      if predicate(item) {
        result = Some(item)
        break  // Early exit
      }
    }
    result
  }
  
  // Process all matches
  let find_all_matches = fn(arr : [Int], predicate : (Int) -> Bool) {
    let mut results = []
    for item in arr {
      if predicate(item) {
        results = results.push(item)
      }
    }
    results
  }
  
  let numbers = [1, 5, 10, 15, 20, 25, 30]
  
  // Find first even number
  let first_even = find_first_match(numbers, fn(x) { x % 2 == 0 })
  match first_even {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
  
  // Find all even numbers
  let all_evens = find_all_matches(numbers, fn(x) { x % 2 == 0 })
  assert_eq(all_evens, [10, 20, 30])
  
  // Short-circuit evaluation
  let short_circuit_and = fn(arr : [Int], threshold : Int) {
    let mut all_above = true
    for item in arr {
      if item <= threshold {
        all_above = false
        break  // Early exit
      }
    }
    all_above
  }
  
  let all_above_5 = short_circuit_and(numbers, 5)
  assert_false(all_above_5)  // 5 is not > 5
  
  let all_above_0 = short_circuit_and(numbers, 0)
  assert_true(all_above_0)   // All numbers > 0
}

// Test 9: Parallel Processing Simulation
test "parallel processing concepts" {
  // Simulate parallel processing with divide and conquer
  let parallel_sum = fn(arr : [Int]) {
    let sum_range = fn(start : Int, end : Int) {
      let mut total = 0
      for i in start..<end {
        total = total + arr[i]
      }
      total
    }
    
    let mid = arr.length() / 2
    let sum1 = sum_range(0, mid)
    let sum2 = sum_range(mid, arr.length())
    
    sum1 + sum2
  }
  
  // Sequential sum for comparison
  let sequential_sum = fn(arr : [Int]) {
    let mut total = 0
    for item in arr {
      total = total + item
    }
    total
  }
  
  let test_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let parallel_result = parallel_sum(test_array)
  let sequential_result = sequential_sum(test_array)
  
  assert_eq(parallel_result, 55)
  assert_eq(sequential_result, 55)
  assert_eq(parallel_result, sequential_result)
  
  // Test parallel map simulation
  let parallel_map = fn(arr : [Int], transform : (Int) -> Int) {
    let mid = arr.length() / 2
    let first_half = arr.slice(0, mid).map(transform)
    let second_half = arr.slice(mid, arr.length()).map(transform)
    
    first_half.concat(second_half)
  }
  
  let doubled_parallel = parallel_map(test_array, fn(x) { x * 2 })
  let doubled_sequential = test_array.map(fn(x) { x * 2 })
  
  assert_eq(doubled_parallel, [2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
  assert_eq(doubled_parallel, doubled_sequential)
}

// Test 10: Resource Pooling and Connection Management
test "resource pooling and connection management" {
  type Connection = {
    id : Int
    is_active : Bool
    created_at : Int
  }
  
  let create_connection = fn(id : Int) {
    Connection {
      id: id,
      is_active: true,
      created_at: 1000 + id
    }
  }
  
  // Connection pool simulation
  let mut connection_pool = []
  let mut next_connection_id = 0
  let mut active_connections = []
  
  let get_connection = fn() {
    if connection_pool.length() > 0 {
      let conn = connection_pool[0]
      connection_pool = connection_pool.slice(1, connection_pool.length())
      active_connections = active_connections.push(conn)
      conn
    } else {
      let new_conn = create_connection(next_connection_id)
      next_connection_id = next_connection_id + 1
      active_connections = active_connections.push(new_conn)
      new_conn
    }
  }
  
  let release_connection = fn(conn : Connection) {
    // Remove from active connections
    let mut new_active = []
    for active in active_connections {
      if active.id != conn.id {
        new_active = new_active.push(active)
      }
    }
    active_connections = new_active
    
    // Add back to pool
    let reused_conn = { conn | is_active: false }
    connection_pool = connection_pool.push(reused_conn)
  }
  
  // Test connection acquisition and release
  let conn1 = get_connection()
  assert_eq(conn1.id, 0)
  assert_true(conn1.is_active)
  assert_eq(active_connections.length(), 1)
  
  let conn2 = get_connection()
  assert_eq(conn2.id, 1)
  assert_eq(active_connections.length(), 2)
  
  // Release connection
  release_connection(conn1)
  assert_eq(active_connections.length(), 1)
  assert_eq(connection_pool.length(), 1)
  
  // Get connection from pool
  let conn3 = get_connection()
  assert_eq(conn3.id, 0) // Reused from pool
  assert_eq(active_connections.length(), 2)
  assert_eq(connection_pool.length(), 0)
  
  // Verify connection reuse
  assert_eq(conn3.created_at, conn1.created_at)
}