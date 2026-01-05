// Azimuth Telemetry System - New Concurrent Safety Stress Tests
// This file contains test cases for concurrent safety under stress conditions

// Test 1: Concurrent Counter Stress Test
test "concurrent counter stress test operations" {
  // Define thread-safe counter
  type AtomicCounter = {
    mut value: Int,
    mut lock: Bool,
    mut waiting_threads: Int
  }
  
  // Create counter
  let create_counter = fn(initial_value: Int) {
    { mut value: initial_value, mut lock: false, mut waiting_threads: 0 }
  }
  
  // Acquire lock with timeout
  let acquire_lock_with_timeout = fn(counter: AtomicCounter, max_attempts: Int) {
    let mut attempts = 0
    
    while counter.lock and attempts < max_attempts {
      counter.waiting_threads = counter.waiting_threads + 1
      attempts = attempts + 1
      // Simulate waiting
    }
    
    if not(counter.lock) {
      counter.lock = true
      counter.waiting_threads = 0
      true
    } else {
      counter.waiting_threads = counter.waiting_threads - 1
      false
    }
  }
  
  // Release lock
  let release_lock = fn(counter: AtomicCounter) {
    counter.lock = false
  }
  
  // Thread-safe increment
  let increment = fn(counter: AtomicCounter, amount: Int) {
    if acquire_lock_with_timeout(counter, 100) {
      counter.value = counter.value + amount
      release_lock(counter)
      true
    } else {
      false
    }
  }
  
  // Thread-safe decrement
  let decrement = fn(counter: AtomicCounter, amount: Int) {
    if acquire_lock_with_timeout(counter, 100) {
      counter.value = counter.value - amount
      release_lock(counter)
      true
    } else {
      false
    }
  }
  
  // Get value
  let get_value = fn(counter: AtomicCounter) {
    if acquire_lock_with_timeout(counter, 100) {
      let value = counter.value
      release_lock(counter)
      value
    } else {
      -1 // Error value
    }
  }
  
  // Simulate concurrent operations
  let simulate_concurrent_operations = fn(counter: AtomicCounter, operations: Array[String>) {
    let mut results = []
    let mut successful_operations = 0
    let mut failed_operations = 0
    
    for op in operations {
      let success = match op {
        "increment" => increment(counter, 1)
        "decrement" => decrement(counter, 1)
        "get" => get_value(counter) >= 0
        _ => false
      }
      
      results = results.push(success)
      
      if success {
        successful_operations = successful_operations + 1
      } else {
        failed_operations = failed_operations + 1
      }
    }
    
    {
      results: results,
      successful_operations: successful_operations,
      failed_operations: failed_operations,
      final_value: get_value(counter)
    }
  }
  
  // Test concurrent counter operations
  let counter = create_counter(0)
  
  // Create a mix of operations
  let operations = []
  let mut i = 0
  while i < 100 {
    operations = operations.push("increment")
    i = i + 1
  }
  
  i = 0
  while i < 30 {
    operations = operations.push("decrement")
    i = i + 1
  }
  
  i = 0
  while i < 20 {
    operations = operations.push("get")
    i = i + 1
  }
  
  // Simulate concurrent operations
  let result = simulate_concurrent_operations(counter, operations)
  
  // Verify results
  assert_eq(result.successful_operations + result.failed_operations, 150)
  assert_true(result.successful_operations > 100) // Most operations should succeed
  assert_eq(result.final_value, 70) // 100 increments - 30 decrements
  
  // Test with high contention
  let high_contention_counter = create_counter(0)
  let high_contention_ops = []
  
  i = 0
  while i < 200 {
    high_contention_ops = high_contention_ops.push("increment")
    i = i + 1
  }
  
  let high_contention_result = simulate_concurrent_operations(high_contention_counter, high_contention_ops)
  
  // Even with high contention, most operations should succeed
  assert_true(high_contention_result.successful_operations > 150)
  assert_eq(high_contention_result.final_value, high_contention_result.successful_operations)
}

// Test 2: Concurrent Queue Stress Test
test "concurrent queue stress test operations" {
  // Define thread-safe queue
  type ThreadSafeQueue = {
    mut items: Array[String],
    mut lock: Bool,
    mut waiting_producers: Int,
    mut waiting_consumers: Int,
    mut max_size: Int
  }
  
  // Create queue
  let create_queue = fn(max_size: Int) {
    { 
      mut items: [], 
      mut lock: false, 
      mut waiting_producers: 0, 
      mut waiting_consumers: 0,
      mut max_size: max_size
    }
  }
  
  // Acquire lock with timeout
  let acquire_queue_lock = fn(queue: ThreadSafeQueue, max_attempts: Int) {
    let mut attempts = 0
    
    while queue.lock and attempts < max_attempts {
      attempts = attempts + 1
      // Simulate waiting
    }
    
    if not(queue.lock) {
      queue.lock = true
      true
    } else {
      false
    }
  }
  
  // Release lock
  let release_queue_lock = fn(queue: ThreadSafeQueue) {
    queue.lock = false
  }
  
  // Thread-safe enqueue
  let enqueue = fn(queue: ThreadSafeQueue, item: String) {
    if acquire_queue_lock(queue, 100) {
      if queue.items.length() < queue.max_size {
        queue.items = queue.items.push(item)
        release_queue_lock(queue)
        true
      } else {
        release_queue_lock(queue)
        false // Queue is full
      }
    } else {
      false
    }
  }
  
  // Thread-safe dequeue
  let dequeue = fn(queue: ThreadSafeQueue) {
    if acquire_queue_lock(queue, 100) {
      if queue.items.length() > 0 {
        let item = queue.items[0]
        queue.items = queue.items.slice(1, queue.items.length())
        release_queue_lock(queue)
        Some(item)
      } else {
        release_queue_lock(queue)
        None
      }
    } else {
      None
    }
  }
  
  // Get queue size
  let size = fn(queue: ThreadSafeQueue) {
    if acquire_queue_lock(queue, 100) {
      let s = queue.items.length()
      release_queue_lock(queue)
      s
    } else {
      -1 // Error value
    }
  }
  
  // Simulate producer-consumer operations
  let simulate_producer_consumer = fn(queue: ThreadSafeQueue, producer_ops: Int, consumer_ops: Int) {
    let mut produced_items = []
    let mut consumed_items = []
    let mut successful_produces = 0
    let mut successful_consumes = 0
    let mut failed_produces = 0
    let mut failed_consumes = 0
    
    // Simulate producers
    let mut i = 0
    while i < producer_ops {
      let item = "item-" + i.to_string()
      if enqueue(queue, item) {
        produced_items = produced_items.push(item)
        successful_produces = successful_produces + 1
      } else {
        failed_produces = failed_produces + 1
      }
      i = i + 1
    }
    
    // Simulate consumers
    i = 0
    while i < consumer_ops {
      let item = dequeue(queue)
      match item {
        Some(value) => {
          consumed_items = consumed_items.push(value)
          successful_consumes = successful_consumes + 1
        }
        None => {
          failed_consumes = failed_consumes + 1
        }
      }
      i = i + 1
    }
    
    {
      successful_produces: successful_produces,
      failed_produces: failed_produces,
      successful_consumes: successful_consumes,
      failed_consumes: failed_consumes,
      final_size: size(queue),
      produced_items: produced_items,
      consumed_items: consumed_items
    }
  }
  
  // Test producer-consumer with balanced operations
  let queue = create_queue(10)
  let result = simulate_producer_consumer(queue, 15, 12)
  
  // Verify results
  assert_eq(result.successful_produces + result.failed_produces, 15)
  assert_eq(result.successful_consumes + result.failed_consumes, 12)
  assert_eq(result.final_size, result.successful_produces - result.successful_consumes)
  
  // Test with more consumers than producers
  let consumer_heavy_queue = create_queue(5)
  let consumer_heavy_result = simulate_producer_consumer(consumer_heavy_queue, 8, 15)
  
  // Should have more failed consumes due to empty queue
  assert_true(consumer_heavy_result.failed_consumes > 0)
  assert_eq(consumer_heavy_result.final_size, 
    consumer_heavy_result.successful_produces - consumer_heavy_result.successful_consumes)
  
  // Test with more producers than consumers
  let producer_heavy_queue = create_queue(5)
  let producer_heavy_result = simulate_producer_consumer(producer_heavy_queue, 15, 8)
  
  // Should have more failed produces due to full queue
  assert_true(producer_heavy_result.failed_produces > 0)
  assert_eq(producer_heavy_result.final_size, 
    producer_heavy_result.successful_produces - producer_heavy_result.successful_consumes)
}

// Test 3: Concurrent Map Stress Test
test "concurrent map stress test operations" {
  // Define thread-safe map entry
  type MapEntry = {
    key: String,
    value: String,
    version: Int
  }
  
  // Define thread-safe map
  type ThreadSafeMap = {
    mut entries: Array[MapEntry],
    mut lock: Bool,
    mut max_size: Int
  }
  
  // Create map
  let create_map = fn(max_size: Int) {
    { mut entries: [], mut lock: false, mut max_size: max_size }
  }
  
  // Acquire lock with timeout
  let acquire_map_lock = fn(map: ThreadSafeMap, max_attempts: Int) {
    let mut attempts = 0
    
    while map.lock and attempts < max_attempts {
      attempts = attempts + 1
      // Simulate waiting
    }
    
    if not(map.lock) {
      map.lock = true
      true
    } else {
      false
    }
  }
  
  // Release lock
  let release_map_lock = fn(map: ThreadSafeMap) {
    map.lock = false
  }
  
  // Find entry index
  let find_entry_index = fn(map: ThreadSafeMap, key: String) {
    let mut index = -1
    let mut i = 0
    
    while i < map.entries.length() {
      if map.entries[i].key == key {
        index = i
        break
      }
      i = i + 1
    }
    
    index
  }
  
  // Thread-safe put
  let put = fn(map: ThreadSafeMap, key: String, value: String) {
    if acquire_map_lock(map, 100) {
      let index = find_entry_index(map, key)
      
      if index >= 0 {
        // Update existing entry
        let old_entry = map.entries[index]
        map.entries[index] = {
          key: key,
          value: value,
          version: old_entry.version + 1
        }
        release_map_lock(map)
        true
      } else {
        // Add new entry if there's space
        if map.entries.length() < map.max_size {
          map.entries = map.entries.push({
            key: key,
            value: value,
            version: 1
          })
          release_map_lock(map)
          true
        } else {
          release_map_lock(map)
          false // Map is full
        }
      }
    } else {
      false
    }
  }
  
  // Thread-safe get
  let get = fn(map: ThreadSafeMap, key: String) {
    if acquire_map_lock(map, 100) {
      let index = find_entry_index(map, key)
      let value = if index >= 0 {
        Some(map.entries[index])
      } else {
        None
      }
      release_map_lock(map)
      value
    } else {
      None
    }
  }
  
  // Thread-safe remove
  let remove = fn(map: ThreadSafeMap, key: String) {
    if acquire_map_lock(map, 100) {
      let index = find_entry_index(map, key)
      let value = if index >= 0 {
        let entry = map.entries[index]
        map.entries = map.entries.slice(0, index) + 
                       map.entries.slice(index + 1, map.entries.length())
        Some(entry)
      } else {
        None
      }
      release_map_lock(map)
      value
    } else {
      None
    }
  }
  
  // Get map size
  let map_size = fn(map: ThreadSafeMap) {
    if acquire_map_lock(map, 100) {
      let s = map.entries.length()
      release_map_lock(map)
      s
    } else {
      -1
    }
  }
  
  // Simulate concurrent map operations
  let simulate_concurrent_map_ops = fn(map: ThreadSafeMap, operations: Array[(String, String, String)]) {
    let mut successful_puts = 0
    let mut successful_gets = 0
    let mut successful_removes = 0
    let mut failed_ops = 0
    
    for op in operations {
      let (op_type, key, value) = op
      
      let success = match op_type {
        "put" => put(map, key, value)
        "get" => {
          match get(map, key) {
            Some(_) => true
            None => false
          }
        }
        "remove" => {
          match remove(map, key) {
            Some(_) => true
            None => false
          }
        }
        _ => false
      }
      
      if success {
        match op_type {
          "put" => successful_puts = successful_puts + 1
          "get" => successful_gets = successful_gets + 1
          "remove" => successful_removes = successful_removes + 1
          _ => {}
        }
      } else {
        failed_ops = failed_ops + 1
      }
    }
    
    {
      successful_puts: successful_puts,
      successful_gets: successful_gets,
      successful_removes: successful_removes,
      failed_ops: failed_ops,
      final_size: map_size(map)
    }
  }
  
  // Test concurrent map operations
  let map = create_map(20)
  
  // Create a mix of operations
  let operations = []
  
  // Put operations
  let mut i = 0
  while i < 15 {
    operations = operations.push(("put", "key-" + i.to_string(), "value-" + i.to_string()))
    i = i + 1
  }
  
  // Get operations
  i = 0
  while i < 10 {
    operations = operations.push(("get", "key-" + i.to_string(), ""))
    i = i + 1
  }
  
  // Update operations
  i = 0
  while i < 5 {
    operations = operations.push(("put", "key-" + i.to_string(), "updated-value-" + i.to_string()))
    i = i + 1
  }
  
  // Remove operations
  i = 5
  while i < 8 {
    operations = operations.push(("remove", "key-" + i.to_string(), ""))
    i = i + 1
  }
  
  // Simulate concurrent operations
  let result = simulate_concurrent_map_ops(map, operations)
  
  // Verify results
  assert_eq(result.successful_puts + result.successful_gets + result.successful_removes + result.failed_ops, 30)
  assert_true(result.successful_puts > 15) // At least 15 puts (5 updates + 10 new)
  assert_true(result.successful_gets > 0) // Some gets should succeed
  assert_eq(result.final_size, result.successful_puts - result.successful_removes)
  
  // Test with map full condition
  let full_map = create_map(5)
  
  // Fill the map
  i = 0
  while i < 5 {
    put(full_map, "full-key-" + i.to_string(), "full-value-" + i.to_string())
    i = i + 1
  }
  
  // Try to add more entries
  let full_result = simulate_concurrent_map_ops(full_map, [
    ("put", "extra-key-1", "extra-value-1"),
    ("put", "extra-key-2", "extra-value-2"),
    ("put", "extra-key-3", "extra-value-3")
  ])
  
  // Most puts should fail due to map being full
  assert_true(full_result.successful_puts < 3)
}

// Test 4: Concurrent Resource Pool Stress Test
test "concurrent resource pool stress test operations" {
  // Define resource pool
  type ResourcePool = {
    mut resources: Array[String],
    mut allocated_resources: Array[String],
    mut lock: Bool,
    mut max_size: Int
  }
  
  // Create resource pool
  let create_resource_pool = fn(max_size: Int) {
    { 
      mut resources: [], 
      mut allocated_resources: [], 
      mut lock: false, 
      mut max_size: max_size 
    }
  }
  
  // Acquire lock with timeout
  let acquire_pool_lock = fn(pool: ResourcePool, max_attempts: Int) {
    let mut attempts = 0
    
    while pool.lock and attempts < max_attempts {
      attempts = attempts + 1
      // Simulate waiting
    }
    
    if not(pool.lock) {
      pool.lock = true
      true
    } else {
      false
    }
  }
  
  // Release lock
  let release_pool_lock = fn(pool: ResourcePool) {
    pool.lock = false
  }
  
  // Add resource to pool
  let add_resource = fn(pool: ResourcePool, resource: String) {
    if acquire_pool_lock(pool, 100) {
      if pool.resources.length() + pool.allocated_resources.length() < pool.max_size {
        pool.resources = pool.resources.push(resource)
        release_pool_lock(pool)
        true
      } else {
        release_pool_lock(pool)
        false // Pool is full
      }
    } else {
      false
    }
  }
  
  // Allocate resource from pool
  let allocate_resource = fn(pool: ResourcePool) {
    if acquire_pool_lock(pool, 100) {
      if pool.resources.length() > 0 {
        let resource = pool.resources[0]
        pool.resources = pool.resources.slice(1, pool.resources.length())
        pool.allocated_resources = pool.allocated_resources.push(resource)
        release_pool_lock(pool)
        Some(resource)
      } else {
        release_pool_lock(pool)
        None
      }
    } else {
      None
    }
  }
  
  // Release resource back to pool
  let release_resource = fn(pool: ResourcePool, resource: String) {
    if acquire_pool_lock(pool, 100) {
      if pool.allocated_resources.contains(resource) {
        pool.allocated_resources = pool.allocated_resources.filter_fn(r) { r != resource }
        pool.resources = pool.resources.push(resource)
        release_pool_lock(pool)
        true
      } else {
        release_pool_lock(pool)
        false
      }
    } else {
      false
    }
  }
  
  // Get pool statistics
  let get_pool_stats = fn(pool: ResourcePool) {
    if acquire_pool_lock(pool, 100) {
      let stats = {
        total_resources: pool.resources.length() + pool.allocated_resources.length(),
        available_resources: pool.resources.length(),
        allocated_resources: pool.allocated_resources.length(),
        max_size: pool.max_size
      }
      release_pool_lock(pool)
      stats
    } else {
      {
        total_resources: -1,
        available_resources: -1,
        allocated_resources: -1,
        max_size: pool.max_size
      }
    }
  }
  
  // Simulate concurrent resource pool operations
  let simulate_concurrent_pool_ops = fn(pool: ResourcePool, add_ops: Int, allocate_ops: Int, release_ops: Int) {
    let mut successful_adds = 0
    let mut successful_allocates = 0
    let mut successful_releases = 0
    let mut failed_ops = 0
    let mut allocated_resources = []
    
    // Add resources
    let mut i = 0
    while i < add_ops {
      let resource = "resource-" + i.to_string()
      if add_resource(pool, resource) {
        successful_adds = successful_adds + 1
      } else {
        failed_ops = failed_ops + 1
      }
      i = i + 1
    }
    
    // Allocate resources
    i = 0
    while i < allocate_ops {
      let resource = allocate_resource(pool)
      match resource {
        Some(r) => {
          allocated_resources = allocated_resources.push(r)
          successful_allocates = successful_allocates + 1
        }
        None => {
          failed_ops = failed_ops + 1
        }
      }
      i = i + 1
    }
    
    // Release some resources
    i = 0
    while i < release_ops and i < allocated_resources.length() {
      let resource = allocated_resources[i]
      if release_resource(pool, resource) {
        successful_releases = successful_releases + 1
      } else {
        failed_ops = failed_ops + 1
      }
      i = i + 1
    }
    
    let final_stats = get_pool_stats(pool)
    
    {
      successful_adds: successful_adds,
      successful_allocates: successful_allocates,
      successful_releases: successful_releases,
      failed_ops: failed_ops,
      final_stats: final_stats
    }
  }
  
  // Test concurrent resource pool operations
  let pool = create_resource_pool(10)
  
  // Add initial resources
  let mut i = 0
  while i < 5 {
    add_resource(pool, "initial-resource-" + i.to_string())
    i = i + 1
  }
  
  // Simulate concurrent operations
  let result = simulate_concurrent_pool_ops(pool, 8, 10, 5)
  
  // Verify results
  assert_eq(result.successful_adds + result.failed_ops, 8 + 10 + 5)
  assert_true(result.successful_adds > 0)
  assert_true(result.successful_allocates > 0)
  assert_true(result.successful_releases > 0)
  
  // Check final statistics
  assert_eq(result.final_stats.total_resources, 
    result.final_stats.available_resources + result.final_stats.allocated_resources)
  assert_true(result.final_stats.total_resources <= result.final_stats.max_size)
  
  // Test with high contention
  let high_contention_pool = create_resource_pool(3)
  
  // Add initial resources
  i = 0
  while i < 3 {
    add_resource(high_contention_pool, "hc-resource-" + i.to_string())
    i = i + 1
  }
  
  // Simulate high contention operations
  let high_contention_result = simulate_concurrent_pool_ops(high_contention_pool, 5, 15, 8)
  
  // With high contention, more operations should fail
  assert_true(high_contention_result.failed_ops > 0)
  assert_true(high_contention_result.final_stats.total_resources <= 3)
}

// Test 5: Concurrent Deadlock Detection Stress Test
test "concurrent deadlock detection stress test operations" {
  // Define resource
  type Resource = {
    id: String,
    mut owner: Option[String],
    mut waiting_queue: Array[String]
  }
  
  // Define process
  type Process = {
    id: String,
    mut held_resources: Array[String],
    mut waiting_for: Option[String]
  }
  
  // Define deadlock detector
  type DeadlockDetector = {
    mut resources: Array[Resource],
    mut processes: Array[Process],
    mut lock: Bool
  }
  
  // Create deadlock detector
  let create_detector = fn() {
    {
      mut resources: [],
      mut processes: [],
      mut lock: false
    }
  }
  
  // Acquire detector lock with timeout
  let acquire_detector_lock = fn(detector: DeadlockDetector, max_attempts: Int) {
    let mut attempts = 0
    
    while detector.lock and attempts < max_attempts {
      attempts = attempts + 1
      // Simulate waiting
    }
    
    if not(detector.lock) {
      detector.lock = true
      true
    } else {
      false
    }
  }
  
  // Release detector lock
  let release_detector_lock = fn(detector: DeadlockDetector) {
    detector.lock = false
  }
  
  // Add resource
  let add_resource = fn(detector: DeadlockDetector, resource_id: String) {
    if acquire_detector_lock(detector, 100) {
      detector.resources = detector.resources.push({
        id: resource_id,
        mut owner: None,
        mut waiting_queue: []
      })
      release_detector_lock(detector)
      true
    } else {
      false
    }
  }
  
  // Add process
  let add_process = fn(detector: DeadlockDetector, process_id: String) {
    if acquire_detector_lock(detector, 100) {
      detector.processes = detector.processes.push({
        id: process_id,
        mut held_resources: [],
        mut waiting_for: None
      })
      release_detector_lock(detector)
      true
    } else {
      false
    }
  }
  
  // Request resource
  let request_resource = fn(detector: DeadlockDetector, process_id: String, resource_id: String) {
    if acquire_detector_lock(detector, 100) {
      let resource_index = detector.resources.index_of_fn(r) { r.id == resource_id }
      let process_index = detector.processes.index_of_fn(p) { p.id == process_id }
      
      if resource_index >= 0 and process_index >= 0 {
        let resource = detector.resources[resource_index]
        
        match resource.owner {
          None => {
            // Resource is free
            detector.resources[resource_index].owner = Some(process_id)
            detector.processes[process_index].held_resources = 
              detector.processes[process_index].held_resources.push(resource_id)
            release_detector_lock(detector)
            true
          }
          Some(owner) => {
            if owner == process_id {
              // Process already owns the resource
              release_detector_lock(detector)
              true
            } else {
              // Resource is owned by another process
              detector.resources[resource_index].waiting_queue = 
                detector.resources[resource_index].waiting_queue.push(process_id)
              detector.processes[process_index].waiting_for = Some(resource_id)
              release_detector_lock(detector)
              false
            }
          }
        }
      } else {
        release_detector_lock(detector)
        false
      }
    } else {
      false
    }
  }
  
  // Release resource
  let release_resource = fn(detector: DeadlockDetector, process_id: String, resource_id: String) {
    if acquire_detector_lock(detector, 100) {
      let resource_index = detector.resources.index_of_fn(r) { r.id == resource_id }
      let process_index = detector.processes.index_of_fn(p) { p.id == process_id }
      
      if resource_index >= 0 and process_index >= 0 {
        // Check if process owns the resource
        if detector.resources[resource_index].owner == Some(process_id) {
          // Release the resource
          detector.resources[resource_index].owner = None
          
          // Remove from process's held resources
          detector.processes[process_index].held_resources = 
            detector.processes[process_index].held_resources.filter_fn(r) { r != resource_id }
          
          // Check waiting queue
          if detector.resources[resource_index].waiting_queue.length() > 0 {
            // Allocate to first waiting process
            let next_process = detector.resources[resource_index].waiting_queue[0]
            detector.resources[resource_index].owner = Some(next_process)
            detector.resources[resource_index].waiting_queue = 
              detector.resources[resource_index].waiting_queue.slice(1, detector.resources[resource_index].waiting_queue.length())
            
            // Update process's held resources
            let next_process_index = detector.processes.index_of_fn(p) { p.id == next_process }
            if next_process_index >= 0 {
              detector.processes[next_process_index].held_resources = 
                detector.processes[next_process_index].held_resources.push(resource_id)
              detector.processes[next_process_index].waiting_for = None
            }
          }
          
          release_detector_lock(detector)
          true
        } else {
          release_detector_lock(detector)
          false
        }
      } else {
        release_detector_lock(detector)
        false
      }
    } else {
      false
    }
  }
  
  // Detect deadlock
  let detect_deadlock = fn(detector: DeadlockDetector) {
    if acquire_detector_lock(detector, 100) {
      // Build wait graph
      let mut wait_edges = []
      
      for process in detector.processes {
        match process.waiting_for {
          Some(resource_id) => {
            let resource_index = detector.resources.index_of_fn(r) { r.id == resource_id }
            if resource_index >= 0 {
              match detector.resources[resource_index].owner {
                Some(owner_id) => {
                  wait_edges = wait_edges.push((process.id, owner_id))
                }
                None => {}
              }
            }
          }
          None => {}
        }
      }
      
      // Simple cycle detection
      let mut deadlocked_processes = []
      
      for edge in wait_edges {
        let (waiter, holder) = edge
        
        // Check if holder is waiting for waiter (direct cycle)
        let holder_waiting = wait_edges.some_fn(e) { 
          e.0 == holder and e.1 == waiter 
        }
        
        if holder_waiting {
          if not(deadlocked_processes.contains(waiter)) {
            deadlocked_processes = deadlocked_processes.push(waiter)
          }
          if not(deadlocked_processes.contains(holder)) {
            deadlocked_processes = deadlocked_processes.push(holder)
          }
        }
      }
      
      release_detector_lock(detector)
      deadlocked_processes
    } else {
      []
    }
  }
  
  // Simulate concurrent deadlock scenarios
  let simulate_deadlock_scenarios = fn(detector: DeadlockDetector, scenarios: Array[ Array[(String, String, String)] >) {
    let mut deadlock_counts = []
    let mut scenario_results = []
    
    for scenario in scenarios {
      // Reset detector for each scenario
      detector.resources = []
      detector.processes = []
      
      // Set up resources and processes
      let mut resources = []
      let mut processes = []
      
      for operation in scenario {
        let (op_type, id1, id2) = operation
        
        match op_type {
          "resource" => {
            resources = resources.push(id1)
          }
          "process" => {
            processes = processes.push(id1)
          }
          _ => {}
        }
      }
      
      // Add resources
      for resource in resources {
        add_resource(detector, resource)
      }
      
      // Add processes
      for process in processes {
        add_process(detector, process)
      }
      
      // Execute operations
      let mut successful_requests = 0
      let mut failed_requests = 0
      
      for operation in scenario {
        let (op_type, id1, id2) = operation
        
        match op_type {
          "request" => {
            if request_resource(detector, id1, id2) {
              successful_requests = successful_requests + 1
            } else {
              failed_requests = failed_requests + 1
            }
          }
          "release" => {
            release_resource(detector, id1, id2)
          }
          _ => {}
        }
      }
      
      // Check for deadlock
      let deadlocked = detect_deadlock(detector)
      
      scenario_results = scenario_results.push({
        successful_requests: successful_requests,
        failed_requests: failed_requests,
        deadlocked_processes: deadlocked.length(),
        deadlocked_process_ids: deadlocked
      })
    }
    
    scenario_results
  }
  
  // Test deadlock detection
  let detector = create_detector()
  
  // Define deadlock scenarios
  let scenarios = [
    // Scenario 1: Simple deadlock (P1 wants R2, P2 wants R1)
    [
      ("resource", "R1", ""),
      ("resource", "R2", ""),
      ("process", "P1", ""),
      ("process", "P2", ""),
      ("request", "P1", "R1"),
      ("request", "P2", "R2"),
      ("request", "P1", "R2"), // P1 waits for R2
      ("request", "P2", "R1")  // P2 waits for R1 - deadlock!
    ],
    
    // Scenario 2: No deadlock
    [
      ("resource", "R1", ""),
      ("resource", "R2", ""),
      ("process", "P1", ""),
      ("process", "P2", ""),
      ("request", "P1", "R1"),
      ("request", "P2", "R2"),
      ("release", "P1", "R1"), // P1 releases R1
      ("request", "P1", "R2")  // P1 requests R2 - no deadlock
    ],
    
    // Scenario 3: Complex deadlock (P1->R2, P2->R3, P3->R1)
    [
      ("resource", "R1", ""),
      ("resource", "R2", ""),
      ("resource", "R3", ""),
      ("process", "P1", ""),
      ("process", "P2", ""),
      ("process", "P3", ""),
      ("request", "P1", "R1"),
      ("request", "P2", "R2"),
      ("request", "P3", "R3"),
      ("request", "P1", "R2"), // P1 waits for R2
      ("request", "P2", "R3"), // P2 waits for R3
      ("request", "P3", "R1")  // P3 waits for R1 - deadlock cycle
    ]
  ]
  
  // Simulate deadlock scenarios
  let results = simulate_deadlock_scenarios(detector, scenarios)
  
  // Verify results
  // Scenario 1: Should detect deadlock
  assert_eq(results[0].successful_requests, 2) // P1->R1, P2->R2
  assert_eq(results[0].failed_requests, 2)    // P1->R2, P2->R1
  assert_eq(results[0].deadlocked_processes, 2)
  assert_true(results[0].deadlocked_process_ids.contains("P1"))
  assert_true(results[0].deadlocked_process_ids.contains("P2"))
  
  // Scenario 2: Should not detect deadlock
  assert_eq(results[1].successful_requests, 3) // P1->R1, P2->R2, P1->R2
  assert_eq(results[1].failed_requests, 0)
  assert_eq(results[1].deadlocked_processes, 0)
  
  // Scenario 3: Should detect deadlock
  assert_eq(results[2].successful_requests, 3) // P1->R1, P2->R2, P3->R3
  assert_eq(results[2].failed_requests, 3)    // P1->R2, P2->R3, P3->R1
  assert_eq(results[2].deadlocked_processes, 3)
  assert_true(results[2].deadlocked_process_ids.contains("P1"))
  assert_true(results[2].deadlocked_process_ids.contains("P2"))
  assert_true(results[2].deadlocked_process_ids.contains("P3"))
}