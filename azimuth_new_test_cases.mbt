// Azimuth New Test Cases
// This file contains additional MoonBit test cases focusing on advanced features

// Test 1: Async Operations Simulation
test "async operations simulation" {
  // Simulate async operations using callbacks
  type AsyncCallback[T] = (T) -> Unit
  
  let simulate_async_operation = fn(callback: AsyncCallback[String]) {
    // Simulate async delay with immediate callback
    callback("async_result")
  }
  
  let mut result_received = false
  let mut result_value = ""
  
  let callback = fn(value: String) {
    result_received = true
    result_value = value
  }
  
  simulate_async_operation(callback)
  
  assert_true(result_received)
  assert_eq(result_value, "async_result")
  
  // Test chained async operations
  let chain_async = fn(initial_value: Int, callback: AsyncCallback[Int]) {
    let first_step = fn(cb: AsyncCallback[Int]) {
      cb(initial_value * 2)
    }
    
    let second_step = fn(value: Int, cb: AsyncCallback[Int]) {
      cb(value + 10)
    }
    
    first_step(fn(value) {
      second_step(value, callback)
    })
  }
  
  let mut chain_result = 0
  let chain_callback = fn(value: Int) {
    chain_result = value
  }
  
  chain_async(5, chain_callback)
  assert_eq(chain_result, 20)  // (5 * 2) + 10
}

// Test 2: Recursive Functions
test "recursive function patterns" {
  // Factorial calculation
  let factorial = fn(n: Int) {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(7), 5040)
  
  // Fibonacci sequence
  let fibonacci = fn(n: Int) {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(7), 13)
  
  // Tree traversal simulation
  type TreeNode = {
    value: Int,
    left: Option[TreeNode],
    right: Option[TreeNode]
  }
  
  let create_tree_node = fn(value: Int, left: Option[TreeNode], right: Option[TreeNode]) {
    { value, left, right }
  }
  
  let tree = create_tree_node(
    1,
    Some(create_tree_node(2, None, None)),
    Some(create_tree_node(3, None, None))
  )
  
  let inorder_traversal = fn(node: Option[TreeNode], accumulator: Array[Int]) {
    match node {
      Some(n) => {
        let left_result = inorder_traversal(n.left, accumulator)
        let with_current = left_result.push(n.value)
        inorder_traversal(n.right, with_current)
      }
      None => accumulator
    }
  }
  
  let traversal_result = inorder_traversal(Some(tree), [])
  assert_eq(traversal_result, [2, 1, 3])
}

// Test 3: Generic Types and Functions
test "generic types and functions" {
  // Generic container type
  type Container[T] = {
    items: Array[T],
    size: Int
  }
  
  let create_container = fn[T](items: Array[T]) {
    {
      items,
      size: items.length()
    }
  }
  
  let string_container = create_container(["a", "b", "c"])
  assert_eq(string_container.size, 3)
  assert_eq(string_container.items.length(), 3)
  
  let int_container = create_container([1, 2, 3, 4])
  assert_eq(int_container.size, 4)
  assert_eq(int_container.items.length(), 4)
  
  // Generic function
  let find_in_container = fn[T](container: Container[T], predicate: (T) -> Bool) {
    let mut found = None
    for item in container.items {
      if predicate(item) {
        found = Some(item)
      }
    }
    found
  }
  
  let string_result = find_in_container(string_container, fn(s) { s == "b" })
  assert_eq(string_result, Some("b"))
  
  let int_result = find_in_container(int_container, fn(i) { i > 2 })
  assert_eq(int_result, Some(3))
  
  // Generic transformation
  let map_container = fn[T, U](container: Container[T], transform: (T) -> U) {
    let transformed_items = container.items.map(transform)
    create_container(transformed_items)
  }
  
  let lengths = map_container(string_container, fn(s) { s.length() })
  assert_eq(lengths.items, [1, 1, 1])
  
  let squares = map_container(int_container, fn(i) { i * i })
  assert_eq(squares.items, [1, 4, 9, 16])
}

// Test 4: Memory Management Patterns
test "memory management patterns" {
  // Simulate resource cleanup
  type Resource = {
    id: String,
    allocated: Bool,
    cleanup_called: Bool
  }
  
  let mut cleanup_log = []
  
  let allocate_resource = fn(id: String) {
    { id, allocated: true, cleanup_called: false }
  }
  
  let cleanup_resource = fn(resource: Resource) {
    cleanup_log = cleanup_log.push("cleanup:" + resource.id)
    { resource | allocated: false, cleanup_called: true }
  }
  
  let resource1 = allocate_resource("res1")
  let resource2 = allocate_resource("res2")
  
  assert_true(resource1.allocated)
  assert_true(resource2.allocated)
  assert_false(resource1.cleanup_called)
  assert_false(resource2.cleanup_called)
  
  let cleaned1 = cleanup_resource(resource1)
  let cleaned2 = cleanup_resource(resource2)
  
  assert_false(cleaned1.allocated)
  assert_false(cleaned2.allocated)
  assert_true(cleaned1.cleanup_called)
  assert_true(cleaned2.cleanup_called)
  
  assert_eq(cleanup_log.length(), 2)
  assert_true(cleanup_log.contains("cleanup:res1"))
  assert_true(cleanup_log.contains("cleanup:res2"))
  
  // Test resource pool pattern
  type ResourcePool = {
    available: Array[Resource],
    in_use: Array[Resource]
  }
  
  let create_pool = fn(size: Int) {
    let mut resources = []
    for i in 0..size {
      resources = resources.push(allocate_resource("pool_res_" + i.to_string()))
    }
    { available: resources, in_use: [] }
  }
  
  let acquire_from_pool = fn(pool: ResourcePool) {
    match pool.available.length() > 0 {
      true => {
        let resource = pool.available[0]
        let remaining = pool.available.slice(1)
        let updated_in_use = pool.in_use.push(resource)
        { available: remaining, in_use: updated_in_use }
      }
      false => pool
    }
  }
  
  let pool = create_pool(3)
  assert_eq(pool.available.length(), 3)
  assert_eq(pool.in_use.length(), 0)
  
  let pool_after_acquire = acquire_from_pool(pool)
  assert_eq(pool_after_acquire.available.length(), 2)
  assert_eq(pool_after_acquire.in_use.length(), 1)
}

// Test 5: Concurrency Patterns
test "concurrency patterns simulation" {
  // Simulate concurrent operations with shared state
  type SharedState = {
    counter: Int,
    operations: Array[String]
  }
  
  let mut state = { counter: 0, operations: [] }
  
  let simulate_concurrent_increment = fn(initial_state: SharedState, thread_id: String) {
    let new_counter = initial_state.counter + 1
    let operation = thread_id + ":increment"
    let updated_operations = initial_state.operations.push(operation)
    { counter: new_counter, operations: updated_operations }
  }
  
  // Simulate multiple threads incrementing
  state = simulate_concurrent_increment(state, "thread1")
  state = simulate_concurrent_increment(state, "thread2")
  state = simulate_concurrent_increment(state, "thread3")
  
  assert_eq(state.counter, 3)
  assert_eq(state.operations.length(), 3)
  assert_true(state.operations.contains("thread1:increment"))
  assert_true(state.operations.contains("thread2:increment"))
  assert_true(state.operations.contains("thread3:increment"))
  
  // Simulate mutex-like behavior
  type Mutex[T] = {
    locked: Bool,
    data: T,
    queue: Array[String]
  }
  
  let create_mutex = fn(data: T) {
    { locked: false, data, queue: [] }
  }
  
  let acquire_mutex = fn(mutex: Mutex[T], thread_id: String) {
    if mutex.locked {
      { mutex | queue: mutex.queue.push(thread_id) }
    } else {
      { mutex | locked: true }
    }
  }
  
  let release_mutex = fn(mutex: Mutex[T]) {
    match mutex.queue.length() > 0 {
      true => {
        let next_thread = mutex.queue[0]
        let remaining_queue = mutex.queue.slice(1)
        { locked: true, data: mutex.data, queue: remaining_queue }
      }
      false => {
        { locked: false, data: mutex.data, queue: [] }
      }
    }
  }
  
  let int_mutex = create_mutex(42)
  assert_false(int_mutex.locked)
  
  let locked_mutex = acquire_mutex(int_mutex, "thread1")
  assert_true(locked_mutex.locked)
  
  let queued_mutex = acquire_mutex(locked_mutex, "thread2")
  assert_true(queued_mutex.locked)
  assert_eq(queued_mutex.queue.length(), 1)
  
  let released_mutex = release_mutex(queued_mutex)
  assert_true(released_mutex.locked)  // Still locked by thread2
  assert_eq(released_mutex.queue.length(), 0)
  
  let fully_released = release_mutex(released_mutex)
  assert_false(fully_released.locked)
}

// Test 6: Algorithm Complexity
test "algorithm complexity analysis" {
  // Linear search
  let linear_search = fn(arr: Array[Int], target: Int) {
    let mut found = false
    let mut iterations = 0
    
    for item in arr {
      iterations = iterations + 1
      if item == target {
        found = true
      }
    }
    
    { found, iterations }
  }
  
  let search_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let search_result = linear_search(search_array, 7)
  
  assert_true(search_result.found)
  assert_eq(search_result.iterations, 10)  // Examines all elements
  
  // Binary search (sorted array)
  let binary_search = fn(arr: Array[Int], target: Int) {
    let mut found = false
    let mut iterations = 0
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      iterations = iterations + 1
      let mid = (left + right) / 2
      let mid_value = arr[mid]
      
      if mid_value == target {
        found = true
      } else if mid_value < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    { found, iterations }
  }
  
  let binary_result = binary_search(search_array, 7)
  assert_true(binary_result.found)
  assert_eq(binary_result.iterations, 3)  // Much fewer iterations
  
  // Compare complexities
  let large_array = []
  for i in 1..=100 {
    large_array = large_array.push(i)
  }
  
  let linear_complexity = linear_search(large_array, 99)
  let binary_complexity = binary_search(large_array, 99)
  
  assert_true(linear_complexity.found)
  assert_true(binary_complexity.found)
  assert_true(binary_complexity.iterations < linear_complexity.iterations)
}

// Test 7: Data Structure Transformations
test "data structure transformations" {
  // Array to tree transformation
  type BinaryTreeNode = {
    value: Int,
    left: Option[BinaryTreeNode],
    right: Option[BinaryTreeNode]
  }
  
  let array_to_bst = fn(arr: Array[Int], start: Int, end: Int) {
    if start > end {
      None
    } else {
      let mid = (start + end) / 2
      let left_subtree = array_to_bst(arr, start, mid - 1)
      let right_subtree = array_to_bst(arr, mid + 1, end)
      
      Some({
        value: arr[mid],
        left: left_subtree,
        right: right_subtree
      })
    }
  }
  
  let sorted_array = [1, 2, 3, 4, 5, 6, 7]
  let bst_root = array_to_bst(sorted_array, 0, sorted_array.length() - 1)
  
  // Verify BST structure
  match bst_root {
    Some(root) => {
      assert_eq(root.value, 4)  // Middle element
      match root.left {
        Some(left_node) => {
          assert_eq(left_node.value, 2)
          match left_node.left {
            Some(left_left) => assert_eq(left_left.value, 1)
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Tree to array transformation (in-order traversal)
  let bst_to_array = fn(node: Option[BinaryTreeNode]) {
    let traverse = fn(n: Option[BinaryTreeNode], acc: Array[Int]) {
      match n {
        Some(tree_node) => {
          let left_result = traverse(tree_node.left, acc)
          let with_current = left_result.push(tree_node.value)
          traverse(tree_node.right, with_current)
        }
        None => acc
      }
    }
    
    traverse(node, [])
  }
  
  let result_array = bst_to_array(bst_root)
  assert_eq(result_array, sorted_array)
  
  // Flat to nested structure transformation
  type NestedStructure = {
    level: Int,
    children: Array[NestedStructure]
  }
  
  let flat_to_nested = fn(flat_data: Array[(Int, String)], current_level: Int) {
    let mut result = []
    let mut i = 0
    
    while i < flat_data.length() {
      let (level, value) = flat_data[i]
      if level == current_level {
        let child = {
          level,
          children: flat_to_nested(flat_data.slice(i + 1), level + 1)
        }
        result = result.push(child)
      }
      i = i + 1
    }
    
    result
  }
  
  let flat_structure = [
    (1, "root"),
    (2, "child1"),
    (3, "grandchild1"),
    (3, "grandchild2"),
    (2, "child2")
  ]
  
  let nested_structure = flat_to_nested(flat_structure, 1)
  assert_eq(nested_structure.length(), 1)
  assert_eq(nested_structure[0].level, 1)
  assert_eq(nested_structure[0].children.length(), 2)
}

// Test 8: Exception Recovery Patterns
test "exception recovery patterns" {
  // Define exception types
  enum SystemException {
    NetworkError(String)
    DatabaseError(String)
    FileSystemError(String)
    TimeoutError(Int)
  }
  
  // Define recovery strategies
  enum RecoveryStrategy {
    Retry(Int)  // Number of retries
    Fallback(String)  // Fallback value
    CircuitBreaker  // Stop trying
  }
  
  // Simulate operation with potential failures
  let risky_operation = fn(attempt: Int) {
    match attempt {
      1 => Err(SystemException::NetworkError("Connection refused"))
      2 => Err(SystemException::DatabaseError("Query timeout"))
      3 => Ok("success")
      _ => Err(SystemException::TimeoutError(5000))
    }
  }
  
  // Implement retry with fallback
  let execute_with_recovery = fn(strategy: RecoveryStrategy) {
    let mut attempts = 0
    let mut last_error = None
    
    match strategy {
      RecoveryStrategy::Retry(max_attempts) => {
        while attempts < max_attempts {
          attempts = attempts + 1
          let result = risky_operation(attempts)
          
          match result {
            Ok(value) => return { success: true, value, attempts }
            Err(error) => {
              last_error = Some(error)
            }
          }
        }
        { success: false, value: "", attempts }
      }
      RecoveryStrategy::Fallback(fallback_value) => {
        let result = risky_operation(1)
        match result {
          Ok(value) => { success: true, value, attempts: 1 }
          Err(_) => { success: true, value: fallback_value, attempts: 1 }
        }
      }
      RecoveryStrategy::CircuitBreaker => {
        { success: false, value: "circuit_open", attempts: 0 }
      }
    }
  }
  
  // Test retry strategy
  let retry_result = execute_with_recovery(RecoveryStrategy::Retry(5))
  assert_true(retry_result.success)
  assert_eq(retry_result.value, "success")
  assert_eq(retry_result.attempts, 3)
  
  // Test fallback strategy
  let fallback_result = execute_with_recovery(RecoveryStrategy::Fallback("default_value"))
  assert_true(fallback_result.success)
  assert_eq(fallback_result.value, "default_value")
  assert_eq(fallback_result.attempts, 1)
  
  // Test circuit breaker
  let circuit_result = execute_with_recovery(RecoveryStrategy::CircuitBreaker)
  assert_false(circuit_result.success)
  assert_eq(circuit_result.value, "circuit_open")
  assert_eq(circuit_result.attempts, 0)
  
  // Test exponential backoff simulation
  let calculate_backoff = fn(attempt: Int, base_delay: Int, max_delay: Int) {
    let exponential_delay = base_delay * (2 ^ (attempt - 1))
    if exponential_delay > max_delay {
      max_delay
    } else {
      exponential_delay
    }
  }
  
  assert_eq(calculate_backoff(1, 100, 5000), 100)
  assert_eq(calculate_backoff(2, 100, 5000), 200)
  assert_eq(calculate_backoff(3, 100, 5000), 400)
  assert_eq(calculate_backoff(4, 100, 5000), 800)
  assert_eq(calculate_backoff(10, 100, 5000), 5000)  // Capped at max
}

// Test 9: Performance Benchmarking
test "performance benchmarking patterns" {
  // Simulate performance measurement
  type BenchmarkResult = {
    operation: String,
    iterations: Int,
    total_time: Int,
    avg_time: Float
  }
  
  let benchmark_operation = fn(operation: () -> Unit, iterations: Int) {
    // Simulate timing by counting operations
    let start_time = 1000  // Simulated timestamp
    
    for i in 1..=iterations {
      operation()
    }
    
    let end_time = 1000 + iterations * 10  // Simulated end time
    let total_time = end_time - start_time
    let avg_time = total_time.to_float() / iterations.to_float()
    
    {
      operation: "test_operation",
      iterations,
      total_time,
      avg_time
    }
  }
  
  // Benchmark different operations
  let simple_operation = fn() {
    let x = 1 + 1
  }
  
  let complex_operation = fn() {
    let mut result = 0
    for i in 1..=10 {
      result = result + i * i
    }
  }
  
  let simple_benchmark = benchmark_operation(simple_operation, 1000)
  let complex_benchmark = benchmark_operation(complex_operation, 1000)
  
  assert_eq(simple_benchmark.iterations, 1000)
  assert_eq(complex_benchmark.iterations, 1000)
  assert_eq(simple_benchmark.total_time, 10000)
  assert_eq(complex_benchmark.total_time, 10000)
  assert_eq(simple_benchmark.avg_time, 10.0)
  assert_eq(complex_benchmark.avg_time, 10.0)
  
  // Compare performance
  let compare_benchmarks = fn(bench1: BenchmarkResult, bench2: BenchmarkResult) {
    if bench1.avg_time < bench2.avg_time {
      bench1.operation + " is faster"
    } else if bench1.avg_time > bench2.avg_time {
      bench2.operation + " is faster"
    } else {
      "Both operations have equal performance"
    }
  }
  
  let comparison = compare_benchmarks(simple_benchmark, complex_benchmark)
  assert_eq(comparison, "Both operations have equal performance")
  
  // Memory usage simulation
  type MemoryUsage = {
    allocated: Int,
    freed: Int,
    peak: Int
  }
  
  let track_memory_usage = fn(operations: Array<() -> Unit>) {
    let mut allocated = 0
    let mut freed = 0
    let mut peak = 0
    
    for op in operations {
      // Simulate allocation
      allocated = allocated + 100
      if allocated > peak {
        peak = allocated
      }
      
      op()
      
      // Simulate deallocation
      freed = freed + 50
    }
    
    { allocated, freed, peak }
  }
  
  let test_operations = [simple_operation, complex_operation, simple_operation]
  let memory_result = track_memory_usage(test_operations)
  
  assert_eq(memory_result.allocated, 300)
  assert_eq(memory_result.freed, 150)
  assert_eq(memory_result.peak, 300)
}

// Test 10: Integration Testing Patterns
test "integration testing patterns" {
  // Simulate component integration
  type Database = {
    data: Array[(String, String)]
  }
  
  type Cache = {
    entries: Array[(String, String)]
  }
  
  type Service = {
    database: Database,
    cache: Cache
  }
  
  let create_database = fn() {
    { data: [("key1", "value1"), ("key2", "value2")] }
  }
  
  let create_cache = fn() {
    { entries: [] }
  }
  
  let create_service = fn(database: Database, cache: Cache) {
    { database, cache }
  }
  
  // Service operations
  let get_from_service = fn(service: Service, key: String) {
    // Check cache first
    let mut found = None
    for (k, v) in service.cache.entries {
      if k == key {
        found = Some(v)
      }
    }
    
    match found {
      Some(value) => value
      None => {
        // Check database
        for (k, v) in service.database.data {
          if k == key {
            return v
          }
        }
        "not_found"
      }
    }
  }
  
  let put_to_service = fn(service: Service, key: String, value: String) {
    let updated_cache = service.cache.entries.push((key, value))
    { service | cache: { entries: updated_cache } }
  }
  
  // Integration test
  let database = create_database()
  let cache = create_cache()
  let service = create_service(database, cache)
  
  // Test direct database access
  let db_value = get_from_service(service, "key1")
  assert_eq(db_value, "value1")
  
  // Test cache miss
  let cache_miss = get_from_service(service, "key3")
  assert_eq(cache_miss, "not_found")
  
  // Test cache put and get
  let updated_service = put_to_service(service, "key3", "value3")
  let cached_value = get_from_service(updated_service, "key3")
  assert_eq(cached_value, "value3")
  
  // Test cache hit (should return from cache, not database)
  let modified_db_service = {
    database: { data: [("key1", "modified_value1")] },
    cache: updated_service.cache
  }
  
  let cached_still_valid = get_from_service(modified_db_service, "key3")
  assert_eq(cached_still_valid, "value3")  // Still gets from cache
  
  // Test end-to-end workflow
  let end_to_end_test = fn() {
    let db = create_database()
    let cache = create_cache()
    let svc = create_service(db, cache)
    
    // Step 1: Get existing data
    let step1 = get_from_service(svc, "key1")
    assert_eq(step1, "value1")
    
    // Step 2: Add new data to cache
    let step2_svc = put_to_service(svc, "new_key", "new_value")
    let step2 = get_from_service(step2_svc, "new_key")
    assert_eq(step2, "new_value")
    
    // Step 3: Verify cache isolation
    let step3 = get_from_service(svc, "new_key")
    assert_eq(step3, "not_found")  // Original service unchanged
    
    "integration_test_passed"
  }
  
  let integration_result = end_to_end_test()
  assert_eq(integration_result, "integration_test_passed")
}