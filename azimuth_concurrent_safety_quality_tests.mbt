// Azimuth Concurrent Safety Test Suite
// This file contains high-quality test cases for concurrent safety in the telemetry system

// Test 1: Thread-Safe Counter for Metrics
test "thread-safe counter for metrics" {
  // Define atomic counter (simplified simulation)
  type AtomicCounter = {
    value: Int,
    pending_operations: Array[String]  // Simulate pending operations
  }
  
  // Create atomic counter
  let create_atomic_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      pending_operations: []
    }
  }
  
  // Simulate atomic increment
  let atomic_increment = fn(counter: AtomicCounter, thread_id: String) {
    // Add operation to pending queue
    let operation = "increment:" + thread_id
    let updated_pending = counter.pending_operations.push(operation)
    
    // Simulate processing operations in order
    let mut new_value = counter.value
    let mut remaining_ops = updated_pending
    
    while remaining_ops.length() > 0 {
      let op = remaining_ops[0]
      remaining_ops = remaining_ops.slice(1)
      
      if op.starts_with("increment:") {
        new_value = new_value + 1
      }
    }
    
    {
      value: new_value,
      pending_operations: []
    }
  }
  
  // Simulate atomic add
  let atomic_add = fn(counter: AtomicCounter, amount: Int, thread_id: String) {
    // Add operation to pending queue
    let operation = "add:" + amount.to_string() + ":" + thread_id
    let updated_pending = counter.pending_operations.push(operation)
    
    // Simulate processing operations in order
    let mut new_value = counter.value
    let mut remaining_ops = updated_pending
    
    while remaining_ops.length() > 0 {
      let op = remaining_ops[0]
      remaining_ops = remaining_ops.slice(1)
      
      if op.starts_with("add:") {
        let parts = op.split(":")
        let amount = parts[1].to_int()
        new_value = new_value + amount
      }
    }
    
    {
      value: new_value,
      pending_operations: []
    }
  }
  
  // Test atomic counter operations
  let counter = create_atomic_counter(0)
  assert_eq(counter.value, 0)
  
  // Simulate concurrent increments
  let counter1 = atomic_increment(counter, "thread-1")
  assert_eq(counter1.value, 1)
  
  let counter2 = atomic_increment(counter1, "thread-2")
  assert_eq(counter2.value, 2)
  
  let counter3 = atomic_increment(counter2, "thread-3")
  assert_eq(counter3.value, 3)
  
  // Test atomic add
  let counter4 = atomic_add(counter3, 5, "thread-4")
  assert_eq(counter4.value, 8)
  
  let counter5 = atomic_add(counter4, 10, "thread-5")
  assert_eq(counter5.value, 18)
  
  // Test mixed operations
  let counter6 = atomic_increment(counter5, "thread-6")
  assert_eq(counter6.value, 19)
  
  let counter7 = atomic_add(counter6, 1, "thread-7")
  assert_eq(counter7.value, 20)
}

// Test 2: Lock-Free Queue for Telemetry Events
test "lock-free queue for telemetry events" {
  // Define queue node
  type QueueNode = {
    value: String,
    next: Option[QueueNode]
  }
  
  // Define lock-free queue
  type LockFreeQueue = {
    head: QueueNode,
    tail: QueueNode
  }
  
  // Create queue node
  let create_node = fn(value: String) {
    {
      value: value,
      next: None
    }
  }
  
  // Create lock-free queue
  let create_lock_free_queue = fn() {
    let dummy = create_node("")
    {
      head: dummy,
      tail: dummy
    }
  }
  
  // Enqueue operation
  let enqueue = fn(queue: LockFreeQueue, value: String) {
    let new_node = create_node(value)
    
    // In a real lock-free implementation, this would use atomic operations
    // Here we simulate the behavior
    let updated_tail = { queue.tail | next: Some(new_node) }
    
    {
      head: queue.head,
      tail: new_node
    }
  }
  
  // Dequeue operation
  let dequeue = fn(queue: LockFreeQueue) {
    if queue.head.next.is_none() {
      // Queue is empty
      (queue, None)
    } else {
      // In a real lock-free implementation, this would use atomic operations
      let next_node = match queue.head.next {
        Some(node) => node
        None => { value: "", next: None }  // Shouldn't happen
      }
      
      let value = next_node.value
      let updated_head = next_node
      
      ({
        head: updated_head,
        tail: queue.tail
      }, Some(value))
    }
  }
  
  // Test lock-free queue
  let queue = create_lock_free_queue()
  
  // Test empty queue
  let (queue1, dequeued1) = dequeue(queue)
  assert_eq(dequeued1, None)
  
  // Enqueue items
  let queue2 = enqueue(queue1, "event-1")
  let queue3 = enqueue(queue2, "event-2")
  let queue4 = enqueue(queue3, "event-3")
  
  // Dequeue items
  let (queue5, dequeued2) = dequeue(queue4)
  assert_eq(dequeued2, Some("event-1"))
  
  let (queue6, dequeued3) = dequeue(queue5)
  assert_eq(dequeued3, Some("event-2"))
  
  let (queue7, dequeued4) = dequeue(queue6)
  assert_eq(dequeued4, Some("event-3"))
  
  // Queue should be empty again
  let (queue8, dequeued5) = dequeue(queue7)
  assert_eq(dequeued5, None)
  
  // Test enqueue/dequeue interleaving
  let queue9 = enqueue(queue8, "event-4")
  let (queue10, dequeued6) = dequeue(queue9)
  assert_eq(dequeued6, Some("event-4"))
  
  let queue11 = enqueue(queue10, "event-5")
  let queue12 = enqueue(queue11, "event-6")
  
  let (queue13, dequeued7) = dequeue(queue12)
  assert_eq(dequeued7, Some("event-5"))
  
  let (queue14, dequeued8) = dequeue(queue13)
  assert_eq(dequeued8, Some("event-6"))
}

// Test 3: Concurrent Map for Telemetry Attributes
test "concurrent map for telemetry attributes" {
  // Define map entry
  type MapEntry = {
    key: String,
    value: String,
    version: Int
  }
  
  // Define concurrent map
  type ConcurrentMap = {
    entries: Array[MapEntry],
    next_version: Int
  }
  
  // Create concurrent map
  let create_concurrent_map = fn() {
    {
      entries: [],
      next_version: 1
    }
  }
  
  // Get value from map
  let get = fn(map: ConcurrentMap, key: String) {
    let entry_opt = map.entries.find(fn(entry) { entry.key == key })
    match entry_opt {
      Some(entry) => Some(entry.value)
      None => None
    }
  }
  
  // Put value to map
  let put = fn(map: ConcurrentMap, key: String, value: String) {
    let existing_index = map.entries.find_index(fn(entry) { entry.key == key })
    
    match existing_index {
      Some(index) => {
        // Update existing entry
        let updated_entry = {
          key: key,
          value: value,
          version: map.next_version
        }
        
        let updated_entries = map.entries.update(index, updated_entry)
        
        {
          entries: updated_entries,
          next_version: map.next_version + 1
        }
      }
      None => {
        // Add new entry
        let new_entry = {
          key: key,
          value: value,
          version: map.next_version
        }
        
        let updated_entries = map.entries.push(new_entry)
        
        {
          entries: updated_entries,
          next_version: map.next_version + 1
        }
      }
    }
  }
  
  // Compare and swap (CAS) operation
  let compare_and_swap = fn(map: ConcurrentMap, key: String, expected_value: String, new_value: String) {
    let entry_opt = map.entries.find(fn(entry) { entry.key == key })
    
    match entry_opt {
      Some(entry) => {
        if entry.value == expected_value {
          // Values match, perform swap
          put(map, key, new_value)
        } else {
          // Values don't match, operation fails
          map
        }
      }
      None => {
        // Key doesn't exist, can't compare
        map
      }
    }
  }
  
  // Test concurrent map
  let map = create_concurrent_map()
  
  // Test empty map
  assert_eq(get(map, "service.name"), None)
  
  // Test put operations
  let map1 = put(map, "service.name", "payment-service")
  assert_eq(get(map1, "service.name"), Some("payment-service"))
  
  let map2 = put(map1, "service.version", "1.2.3")
  assert_eq(get(map2, "service.version"), Some("1.2.3"))
  assert_eq(get(map2, "service.name"), Some("payment-service"))  // Previous value still there
  
  // Test update
  let map3 = put(map2, "service.name", "updated-service")
  assert_eq(get(map3, "service.name"), Some("updated-service"))
  
  // Test CAS operation
  let map4 = compare_and_swap(map3, "service.name", "updated-service", "cas-service")
  assert_eq(get(map4, "service.name"), Some("cas-service"))
  
  // Test failed CAS
  let map5 = compare_and_swap(map4, "service.name", "wrong-value", "should-not-update")
  assert_eq(get(map5, "service.name"), Some("cas-service"))  // Should remain unchanged
  
  // Test CAS on non-existent key
  let map6 = compare_and_swap(map5, "non.existent.key", "expected", "new-value")
  assert_eq(get(map6, "non.existent.key"), None)  // Should remain None
  
  // Test version tracking
  assert_eq(map1.entries[0].version, 1)
  assert_eq(map2.entries[1].version, 2)
  assert_eq(map3.entries[0].version, 3)  // Updated service.name
  assert_eq(map4.entries[0].version, 4)  // CAS update
}

// Test 4: Read-Write Lock for Telemetry Configuration
test "read-write lock for telemetry configuration" {
  // Define lock state
  enum LockState {
    Unlocked
    ReadLocked(Int)  // Number of readers
    WriteLocked(String)  // Thread ID of writer
  }
  
  // Define read-write lock
  type ReadWriteLock = {
    state: LockState,
    waiting_readers: Int,
    waiting_writers: Int
  }
  
  // Define configuration
  type Configuration = {
    settings: Map[String, String],
    lock: ReadWriteLock
  }
  
  // Create read-write lock
  let create_read_write_lock = fn() {
    {
      state: Unlocked,
      waiting_readers: 0,
      waiting_writers: 0
    }
  }
  
  // Create configuration
  let create_configuration = fn() {
    {
      settings: [
        ("sampling.rate", "0.1"),
        ("batch.size", "100"),
        ("timeout.ms", "5000")
      ],
      lock: create_read_write_lock()
    }
  }
  
  // Acquire read lock
  let acquire_read_lock = fn(config: Configuration, thread_id: String) {
    match config.lock.state {
      Unlocked => {
        {
          settings: config.settings,
          lock: {
            state: ReadLocked(1),
            waiting_readers: config.lock.waiting_readers,
            waiting_writers: config.lock.waiting_writers
          }
        }
      }
      ReadLocked(count) => {
        {
          settings: config.settings,
          lock: {
            state: ReadLocked(count + 1),
            waiting_readers: config.lock.waiting_readers,
            waiting_writers: config.lock.waiting_writers
          }
        }
      }
      WriteLocked(_) => {
        // Would wait in real implementation
        config
      }
    }
  }
  
  // Release read lock
  let release_read_lock = fn(config: Configuration, thread_id: String) {
    match config.lock.state {
      ReadLocked(1) => {
        {
          settings: config.settings,
          lock: {
            state: Unlocked,
            waiting_readers: config.lock.waiting_readers,
            waiting_writers: config.lock.waiting_writers
          }
        }
      }
      ReadLocked(count) => {
        {
          settings: config.settings,
          lock: {
            state: ReadLocked(count - 1),
            waiting_readers: config.lock.waiting_readers,
            waiting_writers: config.lock.waiting_writers
          }
        }
      }
      _ => config  // Invalid state
    }
  }
  
  // Acquire write lock
  let acquire_write_lock = fn(config: Configuration, thread_id: String) {
    match config.lock.state {
      Unlocked => {
        {
          settings: config.settings,
          lock: {
            state: WriteLocked(thread_id),
            waiting_readers: config.lock.waiting_readers,
            waiting_writers: config.lock.waiting_writers
          }
        }
      }
      _ => {
        // Would wait in real implementation
        config
      }
    }
  }
  
  // Release write lock
  let release_write_lock = fn(config: Configuration, thread_id: String) {
    match config.lock.state {
      WriteLocked(id) => {
        if id == thread_id {
          {
            settings: config.settings,
            lock: {
              state: Unlocked,
              waiting_readers: config.lock.waiting_readers,
              waiting_writers: config.lock.waiting_writers
            }
          }
        } else {
          config  // Not the lock owner
        }
      }
      _ => config  // Invalid state
    }
  }
  
  // Read configuration
  let read_setting = fn(config: Configuration, key: String) {
    let setting_opt = config.settings.find(fn(pair) { pair.0 == key })
    match setting_opt {
      Some((_, value)) => Some(value)
      None => None
    }
  }
  
  // Write configuration
  let write_setting = fn(config: Configuration, key: String, value: String) {
    let existing_index = config.settings.find_index(fn(pair) { pair.0 == key })
    
    match existing_index {
      Some(index) => {
        let updated_settings = config.settings.update(index, (key, value))
        {
          settings: updated_settings,
          lock: config.lock
        }
      }
      None => {
        let updated_settings = config.settings.push((key, value))
        {
          settings: updated_settings,
          lock: config.lock
        }
      }
    }
  }
  
  // Test read-write lock
  let config = create_configuration()
  
  // Test initial state
  match config.lock.state {
    Unlocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test read operations
  let config1 = acquire_read_lock(config, "reader-1")
  match config1.lock.state {
    ReadLocked(1) => assert_true(true)
    _ => assert_true(false)
  }
  
  let sampling_rate = read_setting(config1, "sampling.rate")
  assert_eq(sampling_rate, Some("0.1"))
  
  // Test multiple readers
  let config2 = acquire_read_lock(config1, "reader-2")
  match config2.lock.state {
    ReadLocked(2) => assert_true(true)
    _ => assert_true(false)
  }
  
  let batch_size = read_setting(config2, "batch.size")
  assert_eq(batch_size, Some("100"))
  
  // Release readers
  let config3 = release_read_lock(config2, "reader-2")
  match config3.lock.state {
    ReadLocked(1) => assert_true(true)
    _ => assert_true(false)
  }
  
  let config4 = release_read_lock(config3, "reader-1")
  match config4.lock.state {
    Unlocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test write operations
  let config5 = acquire_write_lock(config4, "writer-1")
  match config5.lock.state {
    WriteLocked(id) => assert_eq(id, "writer-1")
    _ => assert_true(false)
  }
  
  let config6 = write_setting(config5, "sampling.rate", "0.2")
  let updated_rate = read_setting(config6, "sampling.rate")
  assert_eq(updated_rate, Some("0.2"))
  
  let config7 = release_write_lock(config6, "writer-1")
  match config7.lock.state {
    Unlocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test read after write
  let config8 = acquire_read_lock(config7, "reader-3")
  let rate_after_write = read_setting(config8, "sampling.rate")
  assert_eq(rate_after_write, Some("0.2"))
}

// Test 5: Thread Pool for Telemetry Processing
test "thread pool for telemetry processing" {
  // Define task
  type Task = {
    id: String,
    operation: String,
    data: String
  }
  
  // Define worker thread
  type WorkerThread = {
    id: String,
    busy: Bool,
    current_task: Option[Task]
  }
  
  // Define thread pool
  type ThreadPool = {
    workers: Array[WorkerThread],
    task_queue: Array[Task],
    completed_tasks: Array[Task]
  }
  
  // Create worker thread
  let create_worker = fn(id: String) {
    {
      id: id,
      busy: false,
      current_task: None
    }
  }
  
  // Create thread pool
  let create_thread_pool = fn(num_workers: Int) {
    let mut workers = []
    for i in 0..num_workers {
      workers = workers.push(create_worker("worker-" + i.to_string()))
    }
    
    {
      workers: workers,
      task_queue: [],
      completed_tasks: []
    }
  }
  
  // Submit task
  let submit_task = fn(pool: ThreadPool, task: Task) {
    {
      workers: pool.workers,
      task_queue: pool.task_queue.push(task),
      completed_tasks: pool.completed_tasks
    }
  }
  
  // Assign tasks to workers
  let assign_tasks = fn(pool: ThreadPool) {
    let mut updated_workers = pool.workers
    let mut remaining_tasks = pool.task_queue
    
    for i in 0..updated_workers.length() {
      if not(updated_workers[i].busy) and remaining_tasks.length() > 0 {
        let task = remaining_tasks[0]
        remaining_tasks = remaining_tasks.slice(1)
        
        updated_workers = updated_workers.update(i, {
          id: updated_workers[i].id,
          busy: true,
          current_task: Some(task)
        })
      }
    }
    
    {
      workers: updated_workers,
      task_queue: remaining_tasks,
      completed_tasks: pool.completed_tasks
    }
  }
  
  // Complete task
  let complete_task = fn(pool: ThreadPool, worker_id: String) {
    let worker_index = pool.workers.find_index(fn(worker) { worker.id == worker_id })
    
    match worker_index {
      Some(index) => {
        let worker = pool.workers[index]
        let task = match worker.current_task {
          Some(t) => t
          None => { id: "", operation: "", data: "" }  // Shouldn't happen
        }
        
        let updated_worker = {
          id: worker.id,
          busy: false,
          current_task: None
        }
        
        let updated_workers = pool.workers.update(index, updated_worker)
        let updated_completed = pool.completed_tasks.push(task)
        
        {
          workers: updated_workers,
          task_queue: pool.task_queue,
          completed_tasks: updated_completed
        }
      }
      None => pool
    }
  }
  
  // Test thread pool
  let pool = create_thread_pool(3)
  assert_eq(pool.workers.length(), 3)
  assert_eq(pool.task_queue.length(), 0)
  assert_eq(pool.completed_tasks.length(), 0)
  
  // Check all workers are initially idle
  for i in 0..pool.workers.length() {
    assert_false(pool.workers[i].busy)
  }
  
  // Submit tasks
  let task1 = { id: "task-1", operation: "process-span", data: "span-data-1" }
  let task2 = { id: "task-2", operation: "process-metric", data: "metric-data-1" }
  let task3 = { id: "task-3", operation: "process-log", data: "log-data-1" }
  let task4 = { id: "task-4", operation: "process-span", data: "span-data-2" }
  let task5 = { id: "task-5", operation: "process-metric", data: "metric-data-2" }
  
  let pool1 = submit_task(pool, task1)
  let pool2 = submit_task(pool1, task2)
  let pool3 = submit_task(pool2, task3)
  let pool4 = submit_task(pool3, task4)
  let pool5 = submit_task(pool4, task5)
  
  assert_eq(pool5.task_queue.length(), 5)
  
  // Assign tasks to workers
  let pool6 = assign_tasks(pool5)
  
  // 3 workers should be busy, 2 tasks should remain in queue
  let busy_workers = pool6.workers.filter(fn(worker) { worker.busy })
  assert_eq(busy_workers.length(), 3)
  assert_eq(pool6.task_queue.length(), 2)
  
  // Check which tasks are assigned
  let assigned_tasks = pool6.workers
    .filter(fn(worker) { worker.busy })
    .map(fn(worker) { 
      match worker.current_task {
        Some(task) => task.id
        None => ""
      }
    })
  
  assert_true(assigned_tasks.contains("task-1"))
  assert_true(assigned_tasks.contains("task-2"))
  assert_true(assigned_tasks.contains("task-3"))
  
  // Complete tasks
  let pool7 = complete_task(pool6, "worker-0")
  let pool8 = complete_task(pool7, "worker-1")
  
  // 2 workers should be idle, 1 should still be busy
  let idle_workers = pool8.workers.filter(fn(worker) { not(worker.busy) })
  assert_eq(idle_workers.length(), 2)
  
  // 2 tasks should be completed
  assert_eq(pool8.completed_tasks.length(), 2)
  
  // Assign remaining tasks
  let pool9 = assign_tasks(pool8)
  
  // All workers should be busy again
  let busy_workers2 = pool9.workers.filter(fn(worker) { worker.busy })
  assert_eq(busy_workers2.length(), 3)
  assert_eq(pool9.task_queue.length(), 0)
  
  // Complete all tasks
  let pool10 = complete_task(pool9, "worker-0")
  let pool11 = complete_task(pool10, "worker-1")
  let pool12 = complete_task(pool11, "worker-2")
  
  // All workers should be idle
  let idle_workers2 = pool12.workers.filter(fn(worker) { not(worker.busy) })
  assert_eq(idle_workers2.length(), 3)
  
  // All tasks should be completed
  assert_eq(pool12.completed_tasks.length(), 5)
}

// Test 6: Concurrent Batch Processing
test "concurrent batch processing" {
  // Define batch processor
  type ConcurrentBatchProcessor = {
    batch_size: Int,
    current_batch: Array[String],
    processing_batches: Array[Array[String]],
    completed_batches: Array[Array[String]],
    next_batch_id: Int
  }
  
  // Create concurrent batch processor
  let create_concurrent_batch_processor = fn(batch_size: Int) {
    {
      batch_size: batch_size,
      current_batch: [],
      processing_batches: [],
      completed_batches: [],
      next_batch_id: 1
    }
  }
  
  // Add item to batch
  let add_item = fn(processor: ConcurrentBatchProcessor, item: String) {
    let updated_batch = processor.current_batch.push(item)
    
    if updated_batch.length() >= processor.batch_size {
      // Move batch to processing
      {
        batch_size: processor.batch_size,
        current_batch: [],
        processing_batches: processor.processing_batches.push(updated_batch),
        completed_batches: processor.completed_batches,
        next_batch_id: processor.next_batch_id + 1
      }
    } else {
      {
        batch_size: processor.batch_size,
        current_batch: updated_batch,
        processing_batches: processor.processing_batches,
        completed_batches: processor.completed_batches,
        next_batch_id: processor.next_batch_id
      }
    }
  }
  
  // Complete batch processing
  let complete_batch = fn(processor: ConcurrentBatchProcessor, batch_index: Int) {
    if batch_index < processor.processing_batches.length() {
      let batch = processor.processing_batches[batch_index]
      
      // Remove from processing batches
      let updated_processing = processor.processing_batches
        .slice(0, batch_index) + processor.processing_batches.slice(batch_index + 1)
      
      // Add to completed batches
      let updated_completed = processor.completed_batches.push(batch)
      
      {
        batch_size: processor.batch_size,
        current_batch: processor.current_batch,
        processing_batches: updated_processing,
        completed_batches: updated_completed,
        next_batch_id: processor.next_batch_id
      }
    } else {
      processor
    }
  }
  
  // Test concurrent batch processing
  let processor = create_concurrent_batch_processor(3)
  
  // Add items
  let processor1 = add_item(processor, "item-1")
  let processor2 = add_item(processor1, "item-2")
  let processor3 = add_item(processor2, "item-3")
  
  // First batch should be created and moved to processing
  assert_eq(processor3.current_batch.length(), 0)
  assert_eq(processor3.processing_batches.length(), 1)
  assert_eq(processor3.processing_batches[0].length(), 3)
  assert_eq(processor3.completed_batches.length(), 0)
  
  // Add more items
  let processor4 = add_item(processor3, "item-4")
  let processor5 = add_item(processor4, "item-5")
  let processor6 = add_item(processor5, "item-6")
  
  // Second batch should be created
  assert_eq(processor6.current_batch.length(), 0)
  assert_eq(processor6.processing_batches.length(), 2)
  assert_eq(processor6.processing_batches[1].length(), 3)
  
  // Add items that don't fill a batch
  let processor7 = add_item(processor6, "item-7")
  let processor8 = add_item(processor7, "item-8")
  
  assert_eq(processor8.current_batch.length(), 2)
  assert_eq(processor8.processing_batches.length(), 2)
  
  // Complete first batch
  let processor9 = complete_batch(processor8, 0)
  
  assert_eq(processor9.processing_batches.length(), 1)
  assert_eq(processor9.completed_batches.length(), 1)
  assert_eq(processor9.completed_batches[0].length(), 3)
  
  // Complete second batch
  let processor10 = complete_batch(processor9, 0)
  
  assert_eq(processor10.processing_batches.length(), 0)
  assert_eq(processor10.completed_batches.length(), 2)
  
  // Flush remaining items
  let processor11 = {
    batch_size: processor10.batch_size,
    current_batch: [],
    processing_batches: processor10.processing_batches,
    completed_batches: processor10.completed_batches.push(processor10.current_batch),
    next_batch_id: processor10.next_batch_id
  }
  
  assert_eq(processor11.current_batch.length(), 0)
  assert_eq(processor11.completed_batches.length(), 3)
  assert_eq(processor11.completed_batches[2].length(), 2)
}