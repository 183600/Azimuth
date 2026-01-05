// Azimuth Concurrent Safety Thread Tests
// This file contains test cases for concurrent safety and thread management

// Test 1: Thread-Safe Counter
test "thread-safe counter" {
  // Define a thread-safe counter using atomic operations
  type AtomicCounter = {
    value: Int,
    lock_acquired: Bool
  }
  
  // Simulate atomic increment operation
  let atomic_increment = fn(counter: AtomicCounter) -> AtomicCounter {
    // In a real implementation, this would use actual atomic operations
    // For simulation, we'll assume the lock is acquired and released
    {
      value: counter.value + 1,
      lock_acquired: true
    }
  }
  
  // Simulate concurrent access to the counter
  let simulate_concurrent_access = fn(initial_value: Int, num_threads: Int, increments_per_thread: Int) -> Int {
    let mut counter = { value: initial_value, lock_acquired: false }
    let mut total_increments = 0
    
    // Simulate each thread performing increments
    for thread_id in 0..num_threads {
      for i in 0..increments_per_thread {
        // Each thread increments the counter
        counter = atomic_increment(counter)
        total_increments = total_increments + 1
      }
    }
    
    counter.value
  }
  
  // Test with 5 threads, each incrementing 10 times
  let result = simulate_concurrent_access(0, 5, 10)
  assert_eq(result, 50)  // 5 threads * 10 increments = 50
  
  // Test with non-zero initial value
  let result2 = simulate_concurrent_access(100, 3, 20)
  assert_eq(result2, 160)  // 100 + (3 * 20) = 160
  
  // Test with single thread (should still work)
  let result3 = simulate_concurrent_access(0, 1, 100)
  assert_eq(result3, 100)
}

// Test 2: Thread-Safe Queue
test "thread-safe queue" {
  // Define a thread-safe queue
  type ThreadSafeQueue[T] = {
    items: Array[T],
    head: Int,
    tail: Int,
    size: Int,
    capacity: Int,
    lock_acquired: Bool
  }
  
  // Create a new thread-safe queue
  let create_queue = fn[T](capacity: Int) -> ThreadSafeQueue[T] {
    {
      items: [],
      head: 0,
      tail: 0,
      size: 0,
      capacity: capacity,
      lock_acquired: false
    }
  }
  
  // Enqueue an item
  let enqueue = fn[T](queue: ThreadSafeQueue[T], item: T) -> ThreadSafeQueue[T] {
    if queue.size < queue.capacity {
      {
        items: queue.items.push(item),
        head: queue.head,
        tail: (queue.tail + 1) % queue.capacity,
        size: queue.size + 1,
        capacity: queue.capacity,
        lock_acquired: true
      }
    } else {
      // Queue is full, return unchanged
      queue
    }
  }
  
  // Dequeue an item
  let dequeue = fn[T](queue: ThreadSafeQueue[T]) -> (ThreadSafeQueue[T], Option[T]) {
    if queue.size > 0 {
      let item = queue.items[queue.head]
      let remaining_items = queue.items.slice(1, queue.items.length() - 1)
      
      ({
        items: remaining_items,
        head: (queue.head + 1) % queue.capacity,
        tail: queue.tail,
        size: queue.size - 1,
        capacity: queue.capacity,
        lock_acquired: true
      }, Some(item))
    } else {
      // Queue is empty
      (queue, None)
    }
  }
  
  // Test basic queue operations
  let queue1 = create_queue[String](5)
  assert_eq(queue1.size, 0)
  
  let queue2 = enqueue(queue1, "item1")
  assert_eq(queue2.size, 1)
  
  let queue3 = enqueue(queue2, "item2")
  assert_eq(queue3.size, 2)
  
  let (queue4, item1) = dequeue(queue3)
  assert_eq(queue4.size, 1)
  assert_eq(item1, Some("item1"))
  
  let (queue5, item2) = dequeue(queue4)
  assert_eq(queue5.size, 0)
  assert_eq(item2, Some("item2"))
  
  let (queue6, item3) = dequeue(queue5)
  assert_eq(queue6.size, 0)
  assert_eq(item3, None)
  
  // Test queue capacity limit
  let mut queue = create_queue[Int](3)
  queue = enqueue(queue, 1)
  queue = enqueue(queue, 2)
  queue = enqueue(queue, 3)
  assert_eq(queue.size, 3)
  
  // This should not add the item as the queue is full
  let full_queue = enqueue(queue, 4)
  assert_eq(full_queue.size, 3)
  
  // Test concurrent enqueue and dequeue simulation
  let simulate_concurrent_operations = fn(
    initial_queue: ThreadSafeQueue[Int],
    num_producers: Int,
    num_consumers: Int,
    items_per_producer: Int
  ) -> ThreadSafeQueue[Int] {
    let mut queue = initial_queue
    
    // Simulate producers adding items
    for producer_id in 0..num_producers {
      for i in 0..items_per_producer {
        let item = producer_id * 100 + i
        queue = enqueue(queue, item)
      }
    }
    
    // Simulate consumers removing items
    for consumer_id in 0..num_consumers {
      for i in 0..items_per_producer {
        let (new_queue, _) = dequeue(queue)
        queue = new_queue
      }
    }
    
    queue
  }
  
  let queue7 = create_queue[Int](100)
  let final_queue = simulate_concurrent_operations(queue7, 3, 2, 10)
  
  // 3 producers * 10 items = 30 items added
  // 2 consumers * 10 items = 20 items removed
  // Final size should be 10
  assert_eq(final_queue.size, 10)
}

// Test 3: Thread-Safe Map
test "thread-safe map" {
  // Define a thread-safe map
  type ThreadSafeMap[K, V] = {
    entries: Array[(K, V)>,
    lock_acquired: Bool
  }
  
  // Create a new thread-safe map
  let create_map = fn[K, V]() -> ThreadSafeMap[K, V] {
    {
      entries: [],
      lock_acquired: false
    }
  }
  
  // Get a value from the map
  let get = fn[K, V](map: ThreadSafeMap[K, V], key: K) -> Option[V] {
    for (k, v) in map.entries {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  // Set a value in the map
  let set = fn[K, V](map: ThreadSafeMap[K, V], key: K, value: V) -> ThreadSafeMap[K, V] {
    let mut updated = false
    let mut new_entries = []
    
    for (k, v) in map.entries {
      if k == key {
        new_entries = new_entries.push((k, value))
        updated = true
      } else {
        new_entries = new_entries.push((k, v))
      }
    }
    
    if not(updated) {
      new_entries = new_entries.push((key, value))
    }
    
    {
      entries: new_entries,
      lock_acquired: true
    }
  }
  
  // Remove a value from the map
  let remove = fn[K, V](map: ThreadSafeMap[K, V], key: K) -> ThreadSafeMap[K, V] {
    let new_entries = []
    
    for (k, v) in map.entries {
      if k != key {
        new_entries = new_entries.push((k, v))
      }
    }
    
    {
      entries: new_entries,
      lock_acquired: true
    }
  }
  
  // Test basic map operations
  let map1 = create_map[String, Int]()
  assert_eq(map1.entries.length(), 0)
  
  let map2 = set(map1, "key1", 100)
  assert_eq(map2.entries.length(), 1)
  assert_eq(get(map2, "key1"), Some(100))
  
  let map3 = set(map2, "key2", 200)
  assert_eq(map3.entries.length(), 2)
  assert_eq(get(map3, "key1"), Some(100))
  assert_eq(get(map3, "key2"), Some(200))
  
  let map4 = set(map3, "key1", 150)  // Update existing key
  assert_eq(map4.entries.length(), 2)
  assert_eq(get(map4, "key1"), Some(150))
  assert_eq(get(map4, "key2"), Some(200))
  
  let map5 = remove(map4, "key2")
  assert_eq(map5.entries.length(), 1)
  assert_eq(get(map5, "key1"), Some(150))
  assert_eq(get(map5, "key2"), None)
  
  // Test concurrent access simulation
  let simulate_concurrent_map_access = fn(
    initial_map: ThreadSafeMap[String, Int],
    num_threads: Int,
    operations_per_thread: Int
  ) -> ThreadSafeMap[String, Int] {
    let mut map = initial_map
    
    // Simulate each thread performing operations
    for thread_id in 0..num_threads {
      for i in 0..operations_per_thread {
        let key = "thread_" + thread_id.to_string() + "_key_" + i.to_string()
        let value = thread_id * 1000 + i
        
        // Alternate between set and get operations
        if i % 2 == 0 {
          map = set(map, key, value)
        } else {
          // Get operation (no modification)
          get(map, key)
        }
      }
    }
    
    map
  }
  
  let map6 = create_map[String, Int>()
  let final_map = simulate_concurrent_map_access(map6, 3, 5)
  
  // We expect approximately half of the operations to be sets
  // 3 threads * 5 operations = 15 operations
  // Approximately 7-8 set operations
  assert_true(final_map.entries.length() >= 7)
  assert_true(final_map.entries.length() <= 8)
}

// Test 4: Deadlock Prevention
test "deadlock prevention" {
  // Define a resource with a lock
  type Resource = {
    id: String,
    data: String,
    locked: Bool,
    lock_owner: Option[String]
  }
  
  // Define a thread that needs to acquire multiple resources
  type ThreadOperation = {
    id: String,
    required_resources: Array[String>,
    acquired_resources: Array[String>
  }
  
  // Create resources
  let create_resources = fn(resource_ids: Array[String>) -> Array[Resource] {
    resource_ids.map(fn(id) {
      {
        id: id,
        data: "data_for_" + id,
        locked: false,
        lock_owner: None
      }
    })
  }
  
  // Try to acquire a resource
  let try_acquire = fn(resources: Array[Resource], resource_id: String, thread_id: String) -> Array[Resource> {
    let mut updated_resources = []
    
    for resource in resources {
      if resource.id == resource_id and not(resource.locked) {
        updated_resources = updated_resources.push({
          id: resource.id,
          data: resource.data,
          locked: true,
          lock_owner: Some(thread_id)
        })
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    updated_resources
  }
  
  // Release a resource
  let release = fn(resources: Array[Resource>, resource_id: String, thread_id: String) -> Array[Resource> {
    let mut updated_resources = []
    
    for resource in resources {
      if resource.id == resource_id and resource.locked and resource.lock_owner == Some(thread_id) {
        updated_resources = updated_resources.push({
          id: resource.id,
          data: resource.data,
          locked: false,
          lock_owner: None
        })
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    updated_resources
  }
  
  // Check if all required resources are available
  let can_acquire_all = fn(resources: Array[Resource>, resource_ids: Array[String>) -> Bool {
    for resource_id in resource_ids {
      let mut found = false
      for resource in resources {
        if resource.id == resource_id {
          found = true
          if resource.locked {
            return false
          }
        }
      }
      if not(found) {
        return false
      }
    }
    true
  }
  
  // Simulate deadlock prevention by acquiring all resources at once
  let simulate_deadlock_prevention = fn(
    operations: Array[ThreadOperation],
    resources: Array[Resource>
  ) -> Array[ThreadOperation] {
    let mut updated_resources = resources
    let mut completed_operations = []
    
    for operation in operations {
      // Check if all required resources are available
      if can_acquire_all(updated_resources, operation.required_resources) {
        // Acquire all required resources
        let mut acquired_resources = []
        for resource_id in operation.required_resources {
          updated_resources = try_acquire(updated_resources, resource_id, operation.id)
          acquired_resources = acquired_resources.push(resource_id)
        }
        
        // Perform the operation (simulated)
        let completed_operation = {
          id: operation.id,
          required_resources: operation.required_resources,
          acquired_resources: acquired_resources
        }
        completed_operations = completed_operations.push(completed_operation)
        
        // Release all resources
        for resource_id in operation.required_resources {
          updated_resources = release(updated_resources, resource_id, operation.id)
        }
      } else {
        // Operation couldn't proceed due to resource contention
        let failed_operation = {
          id: operation.id,
          required_resources: operation.required_resources,
          acquired_resources: []
        }
        completed_operations = completed_operations.push(failed_operation)
      }
    }
    
    completed_operations
  }
  
  // Create test resources
  let resources = create_resources(["resource1", "resource2", "resource3"])
  
  // Create test operations that could potentially deadlock
  let operations = [
    {
      id: "thread1",
      required_resources: ["resource1", "resource2"],
      acquired_resources: []
    },
    {
      id: "thread2",
      required_resources: ["resource2", "resource3"],
      acquired_resources: []
    },
    {
      id: "thread3",
      required_resources: ["resource1", "resource3"],
      acquired_resources: []
    }
  ]
  
  // Execute operations with deadlock prevention
  let completed_operations = simulate_deadlock_prevention(operations, resources)
  
  // Verify that operations completed successfully
  assert_eq(completed_operations.length(), 3)
  
  // First operation should succeed
  assert_eq(completed_operations[0].id, "thread1")
  assert_eq(completed_operations[0].acquired_resources.length(), 2)
  assert_true(completed_operations[0].acquired_resources.contains("resource1"))
  assert_true(completed_operations[0].acquired_resources.contains("resource2"))
  
  // Second operation should succeed
  assert_eq(completed_operations[1].id, "thread2")
  assert_eq(completed_operations[1].acquired_resources.length(), 2)
  assert_true(completed_operations[1].acquired_resources.contains("resource2"))
  assert_true(completed_operations[1].acquired_resources.contains("resource3"))
  
  // Third operation should succeed
  assert_eq(completed_operations[2].id, "thread3")
  assert_eq(completed_operations[2].acquired_resources.length(), 2)
  assert_true(completed_operations[2].acquired_resources.contains("resource1"))
  assert_true(completed_operations[2].acquired_resources.contains("resource3"))
}

// Test 5: Thread Pool Management
test "thread pool management" {
  // Define a task
  type Task = {
    id: String,
    function: () -> String,
    status: String,  // "pending", "running", "completed"
    result: Option[String>
  }
  
  // Define a thread pool
  type ThreadPool = {
    threads: Array[String>,
    max_threads: Int,
    active_threads: Int,
    pending_tasks: Array[Task>,
    completed_tasks: Array[Task>
  }
  
  // Create a new thread pool
  let create_thread_pool = fn(max_threads: Int) -> ThreadPool {
    {
      threads: [],
      max_threads: max_threads,
      active_threads: 0,
      pending_tasks: [],
      completed_tasks: []
    }
  }
  
  // Add a task to the thread pool
  let submit_task = fn(pool: ThreadPool, task: Task) -> ThreadPool {
    {
      threads: pool.threads,
      max_threads: pool.max_threads,
      active_threads: pool.active_threads,
      pending_tasks: pool.pending_tasks.push(task),
      completed_tasks: pool.completed_tasks
    }
  }
  
  // Execute tasks from the thread pool
  let execute_tasks = fn(pool: ThreadPool) -> ThreadPool {
    let mut updated_pool = pool
    let mut tasks_to_execute = []
    let mut remaining_pending = []
    
    // Determine how many tasks we can execute
    let available_threads = updated_pool.max_threads - updated_pool.active_threads
    let tasks_to_run = if available_threads > 0 {
      if updated_pool.pending_tasks.length() < available_threads {
        updated_pool.pending_tasks.length()
      } else {
        available_threads
      }
    } else {
      0
    }
    
    // Select tasks to execute
    for i in 0..updated_pool.pending_tasks.length() {
      if i < tasks_to_run {
        tasks_to_execute = tasks_to_execute.push(updated_pool.pending_tasks[i])
      } else {
        remaining_pending = remaining_pending.push(updated_pool.pending_tasks[i])
      }
    }
    
    // Execute the selected tasks
    let mut completed = []
    for task in tasks_to_execute {
      let result = task.function()
      let completed_task = {
        id: task.id,
        function: task.function,
        status: "completed",
        result: Some(result)
      }
      completed = completed.push(completed_task)
    }
    
    // Update the pool
    {
      threads: updated_pool.threads,
      max_threads: updated_pool.max_threads,
      active_threads: updated_pool.active_threads + tasks_to_run,
      pending_tasks: remaining_pending,
      completed_tasks: updated_pool.completed_tasks.extend(completed)
    }
  }
  
  // Create test tasks
  let create_task = fn(id: String, value: String) -> Task {
    {
      id: id,
      function: fn() { "processed_" + value },
      status: "pending",
      result: None
    }
  }
  
  // Test thread pool operations
  let pool1 = create_thread_pool(3)
  assert_eq(pool1.max_threads, 3)
  assert_eq(pool1.active_threads, 0)
  assert_eq(pool1.pending_tasks.length(), 0)
  assert_eq(pool1.completed_tasks.length(), 0)
  
  // Add tasks to the pool
  let pool2 = submit_task(pool1, create_task("task1", "value1"))
  let pool3 = submit_task(pool2, create_task("task2", "value2"))
  let pool4 = submit_task(pool3, create_task("task3", "value3"))
  let pool5 = submit_task(pool4, create_task("task4", "value4"))
  let pool6 = submit_task(pool5, create_task("task5", "value5"))
  
  assert_eq(pool6.pending_tasks.length(), 5)
  assert_eq(pool6.completed_tasks.length(), 0)
  
  // Execute tasks (only 3 can run concurrently)
  let pool7 = execute_tasks(pool6)
  
  assert_eq(pool7.active_threads, 3)
  assert_eq(pool7.pending_tasks.length(), 2)  // 2 tasks still pending
  assert_eq(pool7.completed_tasks.length(), 3)  // 3 tasks completed
  
  // Verify completed tasks
  assert_eq(pool7.completed_tasks[0].result, Some("processed_value1"))
  assert_eq(pool7.completed_tasks[1].result, Some("processed_value2"))
  assert_eq(pool7.completed_tasks[2].result, Some("processed_value3"))
  
  // Execute remaining tasks
  let pool8 = execute_tasks(pool7)
  
  assert_eq(pool8.active_threads, 5)
  assert_eq(pool8.pending_tasks.length(), 0)  // No more pending tasks
  assert_eq(pool8.completed_tasks.length(), 5)  // All 5 tasks completed
  
  // Verify all completed tasks
  assert_eq(pool8.completed_tasks[3].result, Some("processed_value4"))
  assert_eq(pool8.completed_tasks[4].result, Some("processed_value5"))
  
  // Test with more tasks than thread pool capacity
  let pool9 = create_thread_pool(2)
  let pool10 = submit_task(pool9, create_task("task6", "value6"))
  let pool11 = submit_task(pool10, create_task("task7", "value7"))
  let pool12 = submit_task(pool11, create_task("task8", "value8"))
  let pool13 = submit_task(pool12, create_task("task9", "value9"))
  
  assert_eq(pool13.pending_tasks.length(), 4)
  
  // Execute tasks (only 2 can run concurrently)
  let pool14 = execute_tasks(pool13)
  
  assert_eq(pool14.active_threads, 2)
  assert_eq(pool14.pending_tasks.length(), 2)  // 2 tasks still pending
  assert_eq(pool14.completed_tasks.length(), 2)  // 2 tasks completed
  
  // Execute remaining tasks
  let pool15 = execute_tasks(pool14)
  
  assert_eq(pool15.active_threads, 4)
  assert_eq(pool15.pending_tasks.length(), 0)  // No more pending tasks
  assert_eq(pool15.completed_tasks.length(), 4)  // All 4 tasks completed
}