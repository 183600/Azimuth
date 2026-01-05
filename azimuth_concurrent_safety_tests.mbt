// Azimuth Concurrent Safety Tests
// This file contains test cases for concurrent safety functionality

// Test 1: Thread-Safe Counter
test "thread-safe counter implementation" {
  // Define a thread-safe counter structure
  type AtomicCounter = {
    mut value: Int,
    mut lock: Bool
  }
  
  // Create atomic counter
  let create_counter = fn(initial_value: Int) {
    { mut value: initial_value, mut lock: false }
  }
  
  // Acquire lock
  let acquire_lock = fn(counter: AtomicCounter) {
    let mut attempts = 0
    while counter.lock and attempts < 1000 {
      attempts = attempts + 1
      // Simulate waiting
    }
    counter.lock = true
    attempts < 1000  // Return true if lock acquired
  }
  
  // Release lock
  let release_lock = fn(counter: AtomicCounter) {
    counter.lock = false
  }
  
  // Thread-safe increment
  let increment = fn(counter: AtomicCounter) {
    if acquire_lock(counter) {
      counter.value = counter.value + 1
      release_lock(counter)
      true
    } else {
      false
    }
  }
  
  // Thread-safe decrement
  let decrement = fn(counter: AtomicCounter) {
    if acquire_lock(counter) {
      counter.value = counter.value - 1
      release_lock(counter)
      true
    } else {
      false
    }
  }
  
  // Thread-safe get
  let get_value = fn(counter: AtomicCounter) {
    if acquire_lock(counter) {
      let value = counter.value
      release_lock(counter)
      value
    } else {
      -1  // Error value
    }
  }
  
  // Test basic operations
  let counter = create_counter(0)
  
  assert_eq(get_value(counter), 0)
  assert_true(increment(counter))
  assert_eq(get_value(counter), 1)
  assert_true(increment(counter))
  assert_eq(get_value(counter), 2)
  assert_true(decrement(counter))
  assert_eq(get_value(counter), 1)
  
  // Test multiple operations
  let mut i = 0
  while i < 10 {
    assert_true(increment(counter))
    i = i + 1
  }
  assert_eq(get_value(counter), 11)
  
  // Test concurrent access simulation
  let concurrent_operations = fn(counter: AtomicCounter, operations: Array[String]) {
    let mut results = []
    
    for op in operations {
      match op {
        "increment" => results = results.push(increment(counter))
        "decrement" => results = results.push(decrement(counter))
        "get" => results = results.push(get_value(counter) >= 0)
        _ => results = results.push(false)
      }
    }
    
    results
  }
  
  let ops = ["increment", "increment", "get", "decrement", "increment", "get"]
  let results = concurrent_operations(counter, ops)
  
  assert_true(results.all(fn(r) { r }))
  assert_eq(get_value(counter), 12)  // 11 + 2 - 1 + 1 = 13, but one decrement
}

// Test 2: Thread-Safe Queue
test "thread-safe queue implementation" {
  // Define a thread-safe queue structure
  type ThreadSafeQueue = {
    mut items: Array[String],
    mut lock: Bool
  }
  
  // Create queue
  let create_queue = fn() {
    { mut items: [], mut lock: false }
  }
  
  // Acquire lock
  let acquire_queue_lock = fn(queue: ThreadSafeQueue) {
    let mut attempts = 0
    while queue.lock and attempts < 1000 {
      attempts = attempts + 1
    }
    queue.lock = true
    attempts < 1000
  }
  
  // Release lock
  let release_queue_lock = fn(queue: ThreadSafeQueue) {
    queue.lock = false
  }
  
  // Thread-safe enqueue
  let enqueue = fn(queue: ThreadSafeQueue, item: String) {
    if acquire_queue_lock(queue) {
      queue.items = queue.items.push(item)
      release_queue_lock(queue)
      true
    } else {
      false
    }
  }
  
  // Thread-safe dequeue
  let dequeue = fn(queue: ThreadSafeQueue) {
    if acquire_queue_lock(queue) {
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
  
  // Thread-safe peek
  let peek = fn(queue: ThreadSafeQueue) {
    if acquire_queue_lock(queue) {
      let item = if queue.items.length() > 0 {
        Some(queue.items[0])
      } else {
        None
      }
      release_queue_lock(queue)
      item
    } else {
      None
    }
  }
  
  // Thread-safe size
  let size = fn(queue: ThreadSafeQueue) {
    if acquire_queue_lock(queue) {
      let s = queue.items.length()
      release_queue_lock(queue)
      s
    } else {
      -1  // Error value
    }
  }
  
  // Test basic queue operations
  let queue = create_queue()
  
  assert_eq(size(queue), 0)
  assert_eq(peek(queue), None)
  assert_eq(dequeue(queue), None)
  
  // Test enqueue
  assert_true(enqueue(queue, "item1"))
  assert_eq(size(queue), 1)
  assert_eq(peek(queue), Some("item1"))
  
  assert_true(enqueue(queue, "item2"))
  assert_eq(size(queue), 2)
  assert_eq(peek(queue), Some("item1"))
  
  // Test dequeue
  assert_eq(dequeue(queue), Some("item1"))
  assert_eq(size(queue), 1)
  assert_eq(peek(queue), Some("item2"))
  
  assert_eq(dequeue(queue), Some("item2"))
  assert_eq(size(queue), 0)
  assert_eq(peek(queue), None)
  
  // Test multiple operations
  let mut i = 1
  while i <= 5 {
    assert_true(enqueue(queue, "item" + i.to_string()))
    i = i + 1
  }
  assert_eq(size(queue), 5)
  
  // Dequeue all items
  let mut items = []
  while size(queue) > 0 {
    match dequeue(queue) {
      Some(item) => items = items.push(item)
      None => break
    }
  }
  assert_eq(items.length(), 5)
  assert_eq(items[0], "item1")
  assert_eq(items[4], "item5")
}

// Test 3: Thread-Safe Map
test "thread-safe map implementation" {
  // Define a thread-safe map entry
  type MapEntry = {
    key: String,
    value: String
  }
  
  // Define a thread-safe map structure
  type ThreadSafeMap = {
    mut entries: Array[MapEntry],
    mut lock: Bool
  }
  
  // Create map
  let create_map = fn() {
    { mut entries: [], mut lock: false }
  }
  
  // Acquire lock
  let acquire_map_lock = fn(map: ThreadSafeMap) {
    let mut attempts = 0
    while map.lock and attempts < 1000 {
      attempts = attempts + 1
    }
    map.lock = true
    attempts < 1000
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
    if acquire_map_lock(map) {
      let index = find_entry_index(map, key)
      
      if index >= 0 {
        // Update existing entry
        map.entries[index] = { key: key, value: value }
      } else {
        // Add new entry
        map.entries = map.entries.push({ key: key, value: value })
      }
      
      release_map_lock(map)
      true
    } else {
      false
    }
  }
  
  // Thread-safe get
  let get = fn(map: ThreadSafeMap, key: String) {
    if acquire_map_lock(map) {
      let index = find_entry_index(map, key)
      let value = if index >= 0 {
        Some(map.entries[index].value)
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
    if acquire_map_lock(map) {
      let index = find_entry_index(map, key)
      let value = if index >= 0 {
        let value = map.entries[index].value
        map.entries = map.entries.slice(0, index) + map.entries.slice(index + 1, map.entries.length())
        Some(value)
      } else {
        None
      }
      release_map_lock(map)
      value
    } else {
      None
    }
  }
  
  // Thread-safe contains key
  let contains_key = fn(map: ThreadSafeMap, key: String) {
    if acquire_map_lock(map) {
      let exists = find_entry_index(map, key) >= 0
      release_map_lock(map)
      exists
    } else {
      false
    }
  }
  
  // Thread-safe size
  let map_size = fn(map: ThreadSafeMap) {
    if acquire_map_lock(map) {
      let s = map.entries.length()
      release_map_lock(map)
      s
    } else {
      -1  // Error value
    }
  }
  
  // Thread-safe keys
  let keys = fn(map: ThreadSafeMap) {
    if acquire_map_lock(map) {
      let all_keys = map.entries.map_fn(e) { e.key }
      release_map_lock(map)
      all_keys
    } else {
      []
    }
  }
  
  // Test basic map operations
  let map = create_map()
  
  assert_eq(map_size(map), 0)
  assert_false(contains_key(map, "key1"))
  assert_eq(get(map, "key1"), None)
  assert_eq(remove(map, "key1"), None)
  
  // Test put
  assert_true(put(map, "key1", "value1"))
  assert_eq(map_size(map), 1)
  assert_true(contains_key(map, "key1"))
  assert_eq(get(map, "key1"), Some("value1"))
  
  assert_true(put(map, "key2", "value2"))
  assert_eq(map_size(map), 2)
  assert_true(contains_key(map, "key2"))
  assert_eq(get(map, "key2"), Some("value2"))
  
  // Test update
  assert_true(put(map, "key1", "new_value1"))
  assert_eq(map_size(map), 2)
  assert_eq(get(map, "key1"), Some("new_value1"))
  
  // Test remove
  assert_eq(remove(map, "key1"), Some("new_value1"))
  assert_eq(map_size(map), 1)
  assert_false(contains_key(map, "key1"))
  assert_eq(get(map, "key1"), None)
  
  // Test keys
  let all_keys = keys(map)
  assert_eq(all_keys.length(), 1)
  assert_eq(all_keys[0], "key2")
}

// Test 4: Thread Pool Simulation
test "thread pool simulation" {
  // Define task structure
  type Task = {
    id: Int,
    work: () -> String,
    status: String,  // "pending", "running", "completed"
    result: Option[String]
  }
  
  // Define worker structure
  type Worker = {
    id: Int,
    status: String,  // "idle", "busy"
    current_task: Option[Int]
  }
  
  // Define thread pool structure
  type ThreadPool = {
    mut workers: Array[Worker],
    mut tasks: Array[Task],
    mut completed_tasks: Array[Task],
    mut lock: Bool
  }
  
  // Create thread pool
  let create_thread_pool = fn(worker_count: Int) {
    let mut workers = []
    let mut i = 0
    while i < worker_count {
      workers = workers.push({
        id: i,
        status: "idle",
        current_task: None
      })
      i = i + 1
    }
    
    {
      mut workers: workers,
      mut tasks: [],
      mut completed_tasks: [],
      mut lock: false
    }
  }
  
  // Acquire lock
  let acquire_pool_lock = fn(pool: ThreadPool) {
    let mut attempts = 0
    while pool.lock and attempts < 1000 {
      attempts = attempts + 1
    }
    pool.lock = true
    attempts < 1000
  }
  
  // Release lock
  let release_pool_lock = fn(pool: ThreadPool) {
    pool.lock = false
  }
  
  // Submit task
  let submit_task = fn(pool: ThreadPool, work: () -> String) {
    if acquire_pool_lock(pool) {
      let task_id = pool.tasks.length() + pool.completed_tasks.length()
      let task = {
        id: task_id,
        work: work,
        status: "pending",
        result: None
      }
      pool.tasks = pool.tasks.push(task)
      release_pool_lock(pool)
      task_id
    } else {
      -1  // Error
    }
  }
  
  // Find idle worker
  let find_idle_worker = fn(pool: ThreadPool) {
    let mut worker_index = -1
    let mut i = 0
    
    while i < pool.workers.length() {
      if pool.workers[i].status == "idle" {
        worker_index = i
        break
      }
      i = i + 1
    }
    
    worker_index
  }
  
  // Find pending task
  let find_pending_task = fn(pool: ThreadPool) {
    let mut task_index = -1
    let mut i = 0
    
    while i < pool.tasks.length() {
      if pool.tasks[i].status == "pending" {
        task_index = i
        break
      }
      i = i + 1
    }
    
    task_index
  }
  
  // Execute one task
  let execute_one_task = fn(pool: ThreadPool) {
    if acquire_pool_lock(pool) {
      let worker_index = find_idle_worker(pool)
      let task_index = find_pending_task(pool)
      
      if worker_index >= 0 and task_index >= 0 {
        // Assign task to worker
        pool.workers[worker_index].status = "busy"
        pool.workers[worker_index].current_task = Some(pool.tasks[task_index].id)
        pool.tasks[task_index].status = "running"
        
        // Execute task
        let result = pool.tasks[task_index].work()
        pool.tasks[task_index].result = Some(result)
        pool.tasks[task_index].status = "completed"
        
        // Move task to completed
        let completed_task = pool.tasks[task_index]
        pool.completed_tasks = pool.completed_tasks.push(completed_task)
        pool.tasks = pool.tasks.slice(0, task_index) + pool.tasks.slice(task_index + 1, pool.tasks.length())
        
        // Free worker
        pool.workers[worker_index].status = "idle"
        pool.workers[worker_index].current_task = None
        
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
  
  // Execute all tasks
  let execute_all_tasks = fn(pool: ThreadPool) {
    let mut executed = 0
    
    while execute_one_task(pool) {
      executed = executed + 1
    }
    
    executed
  }
  
  // Get pool status
  let get_pool_status = fn(pool: ThreadPool) {
    if acquire_pool_lock(pool) {
      let idle_workers = pool.workers.filter_fn(w) { w.status == "idle" }.length()
      let busy_workers = pool.workers.filter_fn(w) { w.status == "busy" }.length()
      let pending_tasks = pool.tasks.filter_fn(t) { t.status == "pending" }.length()
      let running_tasks = pool.tasks.filter_fn(t) { t.status == "running" }.length()
      let completed_tasks = pool.completed_tasks.length()
      
      release_pool_lock(pool)
      
      {
        idle_workers: idle_workers,
        busy_workers: busy_workers,
        pending_tasks: pending_tasks,
        running_tasks: running_tasks,
        completed_tasks: completed_tasks
      }
    } else {
      {
        idle_workers: -1,
        busy_workers: -1,
        pending_tasks: -1,
        running_tasks: -1,
        completed_tasks: -1
      }
    }
  }
  
  // Test thread pool
  let pool = create_thread_pool(3)
  
  // Check initial status
  let status = get_pool_status(pool)
  assert_eq(status.idle_workers, 3)
  assert_eq(status.busy_workers, 0)
  assert_eq(status.pending_tasks, 0)
  assert_eq(status.running_tasks, 0)
  assert_eq(status.completed_tasks, 0)
  
  // Submit tasks
  let task1_id = submit_task(pool, fn() { "Task 1 result" })
  let task2_id = submit_task(pool, fn() { "Task 2 result" })
  let task3_id = submit_task(pool, fn() { "Task 3 result" })
  let task4_id = submit_task(pool, fn() { "Task 4 result" })
  
  assert_true(task1_id >= 0)
  assert_true(task2_id >= 0)
  assert_true(task3_id >= 0)
  assert_true(task4_id >= 0)
  
  // Check status after submitting tasks
  let status_after_submit = get_pool_status(pool)
  assert_eq(status_after_submit.idle_workers, 3)
  assert_eq(status_after_submit.busy_workers, 0)
  assert_eq(status_after_submit.pending_tasks, 4)
  assert_eq(status_after_submit.running_tasks, 0)
  assert_eq(status_after_submit.completed_tasks, 0)
  
  // Execute all tasks
  let executed = execute_all_tasks(pool)
  assert_eq(executed, 4)
  
  // Check final status
  let final_status = get_pool_status(pool)
  assert_eq(final_status.idle_workers, 3)
  assert_eq(final_status.busy_workers, 0)
  assert_eq(final_status.pending_tasks, 0)
  assert_eq(final_status.running_tasks, 0)
  assert_eq(final_status.completed_tasks, 4)
}

// Test 5: Deadlock Detection
test "deadlock detection simulation" {
  // Define resource structure
  type Resource = {
    id: String,
    mut owner: Option[String],
    mut waiting_queue: Array[String]
  }
  
  // Define process structure
  type Process = {
    id: String,
    mut held_resources: Array[String],
    mut waiting_for: Option[String>
  }
  
  // Define deadlock detector structure
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
  
  // Add resource
  let add_resource = fn(detector: DeadlockDetector, resource_id: String) {
    if acquire_detector_lock(detector) {
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
    if acquire_detector_lock(detector) {
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
  
  // Acquire detector lock
  let acquire_detector_lock = fn(detector: DeadlockDetector) {
    let mut attempts = 0
    while detector.lock and attempts < 1000 {
      attempts = attempts + 1
    }
    detector.lock = true
    attempts < 1000
  }
  
  // Release detector lock
  let release_detector_lock = fn(detector: DeadlockDetector) {
    detector.lock = false
  }
  
  // Request resource
  let request_resource = fn(detector: DeadlockDetector, process_id: String, resource_id: String) {
    if acquire_detector_lock(detector) {
      let resource_index = find_resource_index(detector, resource_id)
      let process_index = find_process_index(detector, process_id)
      
      if resource_index >= 0 and process_index >= 0 {
        let resource = detector.resources[resource_index]
        
        match resource.owner {
          None => {
            // Resource is free, allocate to process
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
              // Resource is owned by another process, add to waiting queue
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
  
  // Find resource index
  let find_resource_index = fn(detector: DeadlockDetector, resource_id: String) {
    let mut index = -1
    let mut i = 0
    
    while i < detector.resources.length() {
      if detector.resources[i].id == resource_id {
        index = i
        break
      }
      i = i + 1
    }
    
    index
  }
  
  // Find process index
  let find_process_index = fn(detector: DeadlockDetector, process_id: String) {
    let mut index = -1
    let mut i = 0
    
    while i < detector.processes.length() {
      if detector.processes[i].id == process_id {
        index = i
        break
      }
      i = i + 1
    }
    
    index
  }
  
  // Release resource
  let release_resource = fn(detector: DeadlockDetector, process_id: String, resource_id: String) {
    if acquire_detector_lock(detector) {
      let resource_index = find_resource_index(detector, resource_id)
      let process_index = find_process_index(detector, process_id)
      
      if resource_index >= 0 and process_index >= 0 {
        // Check if process owns the resource
        if detector.resources[resource_index].owner == Some(process_id) {
          // Release the resource
          detector.resources[resource_index].owner = None
          
          // Remove from process's held resources
          let held = detector.processes[process_index].held_resources
          let mut updated = []
          for r in held {
            if r != resource_id {
              updated = updated.push(r)
            }
          }
          detector.processes[process_index].held_resources = updated
          
          // Check waiting queue
          if detector.resources[resource_index].waiting_queue.length() > 0 {
            // Allocate to first waiting process
            let next_process = detector.resources[resource_index].waiting_queue[0]
            detector.resources[resource_index].owner = Some(next_process)
            detector.resources[resource_index].waiting_queue = 
              detector.resources[resource_index].waiting_queue.slice(1, detector.resources[resource_index].waiting_queue.length())
            
            // Update process's held resources
            let next_process_index = find_process_index(detector, next_process)
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
    if acquire_detector_lock(detector) {
      // Build wait graph
      let mut wait_edges = []
      
      for process in detector.processes {
        match process.waiting_for {
          Some(resource_id) => {
            let resource_index = find_resource_index(detector, resource_id)
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
  
  // Test deadlock detection
  let detector = create_detector()
  
  // Add resources
  assert_true(add_resource(detector, "R1"))
  assert_true(add_resource(detector, "R2"))
  
  // Add processes
  assert_true(add_process(detector, "P1"))
  assert_true(add_process(detector, "P2"))
  
  // No deadlock initially
  let deadlocked = detect_deadlock(detector)
  assert_eq(deadlocked.length(), 0)
  
  // P1 requests R1
  assert_true(request_resource(detector, "P1", "R1"))
  
  // P2 requests R2
  assert_true(request_resource(detector, "P2", "R2"))
  
  // No deadlock yet
  let deadlocked_after_alloc = detect_deadlock(detector)
  assert_eq(deadlocked_after_alloc.length(), 0)
  
  // P1 requests R2 (waits)
  assert_false(request_resource(detector, "P1", "R2"))
  
  // P2 requests R1 (waits) - this creates a deadlock
  assert_false(request_resource(detector, "P2", "R1"))
  
  // Detect deadlock
  let deadlocked_final = detect_deadlock(detector)
  assert_eq(deadlocked_final.length(), 2)
  assert_true(deadlocked_final.contains("P1"))
  assert_true(deadlocked_final.contains("P2"))
  
  // Break deadlock by having P1 release R1
  assert_true(release_resource(detector, "P1", "R1"))
  
  // Check if deadlock is resolved
  let deadlocked_after_release = detect_deadlock(detector)
  assert_eq(deadlocked_after_release.length(), 0)
}