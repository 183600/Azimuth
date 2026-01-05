// Azimuth Concurrent Processing Comprehensive Test Suite
// This file contains comprehensive test cases for concurrent processing functionality

// Test 1: Thread Pool Implementation
test "thread pool implementation for telemetry processing" {
  // Define thread pool types
  type Task = {
    id: String,
    work: () -> String,
    created_at: Int
  }
  
  type Worker = {
    id: Int,
    current_task: Option[Task],
    busy: Bool,
    completed_tasks: Int
  }
  
  type ThreadPool = {
    workers: Array[Worker],
    task_queue: Array[Task],
    max_queue_size: Int
  }
  
  // Thread pool operations
  let create_worker = fn(id: Int) {
    {
      id,
      current_task: None,
      busy: false,
      completed_tasks: 0
    }
  }
  
  let create_thread_pool = fn(worker_count: Int, max_queue_size: Int) {
    let mut workers = []
    for i in 0..worker_count {
      workers = workers.push(create_worker(i))
    }
    
    {
      workers,
      task_queue: [],
      max_queue_size
    }
  }
  
  let find_available_worker = fn(pool: ThreadPool) {
    let mut available_worker = None
    
    for worker in pool.workers {
      if not(worker.busy) {
        available_worker = Some(worker)
        break
      }
    }
    
    available_worker
  }
  
  let submit_task = fn(pool: ThreadPool, task: Task) {
    if pool.task_queue.length() < pool.max_queue_size {
      { pool | task_queue: pool.task_queue.push(task) }
    } else {
      pool // Queue is full
    }
  }
  
  let assign_tasks = fn(pool: ThreadPool) {
    let mut updated_workers = pool.workers
    let mut remaining_tasks = pool.task_queue
    
    // Try to assign tasks to available workers
    for i in 0..updated_workers.length() {
      if not(updated_workers[i].busy) and remaining_tasks.length() > 0 {
        let task = remaining_tasks[0]
        remaining_tasks = remaining_tasks.slice(1, remaining_tasks.length())
        updated_workers = updated_workers.set(i, {
          id: updated_workers[i].id,
          current_task: Some(task),
          busy: true,
          completed_tasks: updated_workers[i].completed_tasks
        })
      }
    }
    
    {
      workers: updated_workers,
      task_queue: remaining_tasks,
      max_queue_size: pool.max_queue_size
    }
  }
  
  let complete_task = fn(pool: ThreadPool, worker_id: Int) {
    let mut updated_workers = pool.workers
    
    if worker_id < updated_workers.length() {
      let worker = updated_workers[worker_id]
      updated_workers = updated_workers.set(worker_id, {
        id: worker.id,
        current_task: None,
        busy: false,
        completed_tasks: worker.completed_tasks + 1
      })
    }
    
    {
      workers: updated_workers,
      task_queue: pool.task_queue,
      max_queue_size: pool.max_queue_size
    }
  }
  
  // Test thread pool creation
  let pool = create_thread_pool(3, 10)
  assert_eq(pool.workers.length(), 3)
  assert_eq(pool.task_queue.length(), 0)
  assert_eq(pool.max_queue_size, 10)
  
  // Test finding available workers
  let available_worker = find_available_worker(pool)
  assert_true(available_worker.is_some())
  
  match available_worker {
    Some(worker) => {
      assert_false(worker.busy)
      assert_eq(worker.completed_tasks, 0)
    }
    None => assert_true(false)
  }
  
  // Test submitting tasks
  let task1 = { id: "task-1", work: fn() { "result-1" }, created_at: 1640995200 }
  let task2 = { id: "task-2", work: fn() { "result-2" }, created_at: 1640995200 }
  let task3 = { id: "task-3", work: fn() { "result-3" }, created_at: 1640995200 }
  
  let pool1 = submit_task(pool, task1)
  let pool2 = submit_task(pool1, task2)
  let pool3 = submit_task(pool2, task3)
  
  assert_eq(pool3.task_queue.length(), 3)
  
  // Test task assignment
  let pool4 = assign_tasks(pool3)
  assert_eq(pool4.task_queue.length(), 0) // All tasks should be assigned
  assert_eq(pool4.workers[0].busy, true)
  assert_eq(pool4.workers[1].busy, true)
  assert_eq(pool4.workers[2].busy, true)
  
  // Test task completion
  let pool5 = complete_task(pool4, 0)
  assert_false(pool5.workers[0].busy)
  assert_eq(pool5.workers[0].completed_tasks, 1)
  assert_true(pool5.workers[1].busy)
  assert_true(pool5.workers[2].busy)
}

// Test 2: Concurrent Counter
test "concurrent counter implementation" {
  // Define counter types
  type AtomicCounter = {
    value: Int,
    lock: Bool
  }
  
  // Counter operations
  let create_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      lock: false
    }
  }
  
  let acquire_lock = fn(counter: AtomicCounter) {
    if not(counter.lock) {
      Some({ counter | lock: true })
    } else {
      None
    }
  }
  
  let release_lock = fn(counter: AtomicCounter) {
    { counter | lock: false }
  }
  
  let atomic_increment = fn(counter: AtomicCounter) {
    match acquire_lock(counter) {
      Some(locked_counter) => {
        let incremented = { locked_counter | value: locked_counter.value + 1 }
        release_lock(incremented)
      }
      None => counter // Lock not acquired
    }
  }
  
  let atomic_add = fn(counter: AtomicCounter, delta: Int) {
    match acquire_lock(counter) {
      Some(locked_counter) => {
        let added = { locked_counter | value: locked_counter.value + delta }
        release_lock(added)
      }
      None => counter // Lock not acquired
    }
  }
  
  let atomic_get = fn(counter: AtomicCounter) {
    counter.value
  }
  
  // Test counter creation
  let counter = create_counter(0)
  assert_eq(atomic_get(counter), 0)
  assert_false(counter.lock)
  
  // Test atomic increment
  let counter1 = atomic_increment(counter)
  assert_eq(atomic_get(counter1), 1)
  assert_false(counter1.lock)
  
  let counter2 = atomic_increment(counter1)
  assert_eq(atomic_get(counter2), 2)
  
  // Test atomic add
  let counter3 = atomic_add(counter2, 5)
  assert_eq(atomic_get(counter3), 7)
  
  let counter4 = atomic_add(counter3, -3)
  assert_eq(atomic_get(counter4), 4)
}

// Test 3: Producer-Consumer Pattern
test "producer-consumer pattern for telemetry data" {
  // Define producer-consumer types
  type TelemetryData = {
    id: String,
    type: String,
    data: String,
    timestamp: Int
  }
  
  type Buffer = {
    items: Array[TelemetryData],
    head: Int,
    tail: Int,
    size: Int,
    capacity: Int
  }
  
  type Producer = {
    id: String,
    produced_count: Int
  }
  
  type Consumer = {
    id: String,
    consumed_count: Int
  }
  
  type ProducerConsumerSystem = {
    buffer: Buffer,
    producers: Array[Producer],
    consumers: Array[Consumer]
  }
  
  // Buffer operations
  let create_buffer = fn(capacity: Int) {
    {
      items: Array::with_capacity(capacity),
      head: 0,
      tail: 0,
      size: 0,
      capacity
    }
  }
  
  let is_buffer_empty = fn(buffer: Buffer) {
    buffer.size == 0
  }
  
  let is_buffer_full = fn(buffer: Buffer) {
    buffer.size == buffer.capacity
  }
  
  let buffer_put = fn(buffer: Buffer, item: TelemetryData) {
    if not(is_buffer_full(buffer)) {
      let new_items = buffer.items.set(buffer.tail % buffer.capacity, item)
      {
        items: new_items,
        head: buffer.head,
        tail: (buffer.tail + 1) % buffer.capacity,
        size: buffer.size + 1,
        capacity: buffer.capacity
      }
    } else {
      buffer // Buffer is full
    }
  }
  
  let buffer_get = fn(buffer: Buffer) {
    if not(is_buffer_empty(buffer)) {
      let item = buffer.items[buffer.head % buffer.capacity]
      (Some(item), {
        items: buffer.items,
        head: (buffer.head + 1) % buffer.capacity,
        tail: buffer.tail,
        size: buffer.size - 1,
        capacity: buffer.capacity
      })
    } else {
      (None, buffer)
    }
  }
  
  // Producer-consumer operations
  let create_producer = fn(id: String) {
    { id, produced_count: 0 }
  }
  
  let create_consumer = fn(id: String) {
    { id, consumed_count: 0 }
  }
  
  let create_system = fn(buffer_capacity: Int, producer_count: Int, consumer_count: Int) {
    let mut producers = []
    for i in 0..producer_count {
      producers = producers.push(create_producer("producer-" + i.to_string()))
    }
    
    let mut consumers = []
    for i in 0..consumer_count {
      consumers = consumers.push(create_consumer("consumer-" + i.to_string()))
    }
    
    {
      buffer: create_buffer(buffer_capacity),
      producers,
      consumers
    }
  }
  
  let produce = fn(system: ProducerConsumerSystem, producer_id: String, data: TelemetryData) {
    let new_buffer = buffer_put(system.buffer, data)
    
    // Update producer count
    let mut updated_producers = system.producers
    for i in 0..updated_producers.length() {
      if updated_producers[i].id == producer_id {
        updated_producers = updated_producers.set(i, {
          id: updated_producers[i].id,
          produced_count: updated_producers[i].produced_count + 1
        })
      }
    }
    
    {
      buffer: new_buffer,
      producers: updated_producers,
      consumers: system.consumers
    }
  }
  
  let consume = fn(system: ProducerConsumerSystem, consumer_id: String) {
    let (item, new_buffer) = buffer_get(system.buffer)
    
    // Update consumer count
    let mut updated_consumers = system.consumers
    for i in 0..updated_consumers.length() {
      if updated_consumers[i].id == consumer_id {
        updated_consumers = updated_consumers.set(i, {
          id: updated_consumers[i].id,
          consumed_count: updated_consumers[i].consumed_count + (if item.is_some() { 1 } else { 0 })
        })
      }
    }
    
    {
      buffer: new_buffer,
      producers: system.producers,
      consumers: updated_consumers
    }
  }
  
  // Test system creation
  let system = create_system(5, 2, 2)
  assert_eq(system.buffer.capacity, 5)
  assert_eq(system.producers.length(), 2)
  assert_eq(system.consumers.length(), 2)
  assert_true(is_buffer_empty(system.buffer))
  assert_false(is_buffer_full(system.buffer))
  
  // Test production
  let data1 = { id: "data-1", type: "span", data: "span-data-1", timestamp: 1640995200 }
  let data2 = { id: "data-2", type: "metric", data: "metric-data-1", timestamp: 1640995200 }
  
  let system1 = produce(system, "producer-0", data1)
  assert_eq(system1.buffer.size, 1)
  assert_eq(system1.producers[0].produced_count, 1)
  assert_eq(system1.producers[1].produced_count, 0)
  
  let system2 = produce(system1, "producer-1", data2)
  assert_eq(system2.buffer.size, 2)
  assert_eq(system2.producers[0].produced_count, 1)
  assert_eq(system2.producers[1].produced_count, 1)
  
  // Test consumption
  let system3 = consume(system2, "consumer-0")
  assert_eq(system3.buffer.size, 1)
  assert_eq(system3.consumers[0].consumed_count, 1)
  assert_eq(system3.consumers[1].consumed_count, 0)
  
  let system4 = consume(system3, "consumer-1")
  assert_eq(system4.buffer.size, 0)
  assert_eq(system4.consumers[0].consumed_count, 1)
  assert_eq(system4.consumers[1].consumed_count, 1)
  
  // Test consuming from empty buffer
  let system5 = consume(system4, "consumer-0")
  assert_eq(system5.buffer.size, 0)
  assert_eq(system5.consumers[0].consumed_count, 1) // No increment
}

// Test 4: Read-Write Lock
test "read-write lock for telemetry configuration" {
  // Define lock types
  type LockState = 
    | Unlocked
    | ReadLocked(Int) // Int represents the number of readers
    | WriteLocked(String) // String represents the writer ID
  
  type ReadWriteLock = {
    state: LockState,
    waiting_readers: Int,
    waiting_writers: Int
  }
  
  type ConfigData = {
    settings: Array[(String, String)],
    version: Int
  }
  
  type ProtectedConfig = {
    data: ConfigData,
    lock: ReadWriteLock
  }
  
  // Lock operations
  let create_lock = fn() {
    {
      state: Unlocked,
      waiting_readers: 0,
      waiting_writers: 0
    }
  }
  
  let acquire_read_lock = fn(lock: ReadWriteLock) {
    match lock.state {
      Unlocked => Some({ lock | state: ReadLocked(1) })
      ReadLocked(count) => Some({ lock | state: ReadLocked(count + 1) })
      WriteLocked(_) => {
        // Writers have priority, so readers must wait
        { lock | waiting_readers: lock.waiting_readers + 1 }
      }
    }
  }
  
  let release_read_lock = fn(lock: ReadWriteLock) {
    match lock.state {
      ReadLocked(1) => { lock | state: Unlocked }
      ReadLocked(count) if count > 1 => { lock | state: ReadLocked(count - 1) }
      _ => lock // Invalid state
    }
  }
  
  let acquire_write_lock = fn(lock: ReadWriteLock, writer_id: String) {
    match lock.state {
      Unlocked => Some({ lock | state: WriteLocked(writer_id) })
      _ => {
        // Must wait for all readers and writers to finish
        { lock | waiting_writers: lock.waiting_writers + 1 }
      }
    }
  }
  
  let release_write_lock = fn(lock: ReadWriteLock) {
    match lock.state {
      WriteLocked(_) => { lock | state: Unlocked }
      _ => lock // Invalid state
    }
  }
  
  let is_read_locked = fn(lock: ReadWriteLock) {
    match lock.state {
      ReadLocked(_) => true
      _ => false
    }
  }
  
  let is_write_locked = fn(lock: ReadWriteLock) {
    match lock.state {
      WriteLocked(_) => true
      _ => false
    }
  }
  
  // Config operations
  let create_config = fn() {
    {
      data: {
        settings: [],
        version: 1
      },
      lock: create_lock()
    }
  }
  
  let read_setting = fn(config: ProtectedConfig, key: String) {
    match acquire_read_lock(config.lock) {
      Some(locked) => {
        let mut result = None
        for (k, v) in config.data.settings {
          if k == key {
            result = Some(v)
          }
        }
        let unlocked = release_read_lock(locked)
        (result, { config | lock: unlocked })
      }
      None => (None, config) // Lock not acquired
    }
  }
  
  let write_setting = fn(config: ProtectedConfig, key: String, value: String, writer_id: String) {
    match acquire_write_lock(config.lock, writer_id) {
      Some(locked) => {
        let mut new_settings = []
        let mut found = false
        
        for (k, v) in config.data.settings {
          if k == key {
            new_settings = new_settings.push((k, value))
            found = true
          } else {
            new_settings = new_settings.push((k, v))
          }
        }
        
        if not(found) {
          new_settings = new_settings.push((key, value))
        }
        
        let new_data = {
          settings: new_settings,
          version: config.data.version + 1
        }
        
        let unlocked = release_write_lock(locked)
        ({ config | data: new_data, lock: unlocked })
      }
      None => config // Lock not acquired
    }
  }
  
  // Test lock creation
  let lock = create_lock()
  match lock.state {
    Unlocked => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(lock.waiting_readers, 0)
  assert_eq(lock.waiting_writers, 0)
  
  // Test read lock acquisition
  let read_locked = acquire_read_lock(lock)
  assert_true(read_locked.is_some())
  
  match read_locked {
    Some(locked) => {
      assert_true(is_read_locked(locked))
      assert_false(is_write_locked(locked))
      
      // Test multiple readers
      let read_locked2 = acquire_read_lock(locked)
      assert_true(read_locked2.is_some())
      
      match read_locked2 {
        Some(locked2) => {
          match locked2.state {
            ReadLocked(count) => assert_eq(count, 2)
            _ => assert_true(false)
          }
          
          // Test read lock release
          let unlocked = release_read_lock(locked2)
          match unlocked.state {
            ReadLocked(count) => assert_eq(count, 1)
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test write lock acquisition
  let unlocked = create_lock()
  let write_locked = acquire_write_lock(unlocked, "writer-1")
  assert_true(write_locked.is_some())
  
  match write_locked {
    Some(locked) => {
      assert_false(is_read_locked(locked))
      assert_true(is_write_locked(locked))
      
      match locked.state {
        WriteLocked(writer_id) => assert_eq(writer_id, "writer-1")
        _ => assert_true(false)
      }
      
      // Test write lock release
      let unlocked = release_write_lock(locked)
      match unlocked.state {
        Unlocked => assert_true(true)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test config operations
  let config = create_config()
  assert_eq(config.data.settings.length(), 0)
  assert_eq(config.data.version, 1)
  
  // Test writing settings
  let config1 = write_setting(config, "service.name", "payment-service", "writer-1")
  assert_eq(config1.data.settings.length(), 1)
  assert_eq(config1.data.version, 2)
  assert_true(config1.data.settings.contains(("service.name", "payment-service")))
  
  let config2 = write_setting(config1, "service.version", "1.2.3", "writer-2")
  assert_eq(config2.data.settings.length(), 2)
  assert_eq(config2.data.version, 3)
  
  // Test reading settings
  let (value1, config3) = read_setting(config2, "service.name")
  assert_eq(value1, Some("payment-service"))
  
  let (value2, config4) = read_setting(config3, "service.version")
  assert_eq(value2, Some("1.2.3"))
  
  let (value3, config5) = read_setting(config4, "nonexistent")
  assert_eq(value3, None)
}

// Test 5: Concurrent Map
test "concurrent map for telemetry attributes" {
  // Define concurrent map types
  type MapEntry[K, V] = {
    key: K,
    value: V,
    version: Int
  }
  
  type MapSegment[K, V] = {
    entries: Array[MapEntry[K, V]],
    lock: Bool
  }
  
  type ConcurrentMap[K, V] = {
    segments: Array[MapSegment[K, V]],
    segment_count: Int
  }
  
  // Map operations
  let create_segment = fn() {
    {
      entries: [],
      lock: false
    }
  }
  
  let create_concurrent_map = fn(segment_count: Int) {
    let mut segments = []
    for i in 0..segment_count {
      segments = segments.push(create_segment())
    }
    
    {
      segments,
      segment_count
    }
  }
  
  let hash_key = fn(key: String, segment_count: Int) {
    let mut hash = 0
    for i in 0..key.length() {
      hash = (hash + key[i].to_int()) % segment_count
    }
    if hash < 0 { hash + segment_count } else { hash }
  }
  
  let acquire_segment_lock = fn(map: ConcurrentMap[K, V], segment_index: Int) {
    if segment_index < map.segments.length() and not(map.segments[segment_index].lock) {
      let updated_segments = map.segments.set(segment_index, {
        entries: map.segments[segment_index].entries,
        lock: true
      })
      Some({ map | segments: updated_segments })
    } else {
      None
    }
  }
  
  let release_segment_lock = fn(map: ConcurrentMap[K, V], segment_index: Int) {
    if segment_index < map.segments.length() {
      let updated_segments = map.segments.set(segment_index, {
        entries: map.segments[segment_index].entries,
        lock: false
      })
      { map | segments: updated_segments }
    } else {
      map
    }
  }
  
  let put = fn(map: ConcurrentMap[String, String], key: String, value: String) {
    let segment_index = hash_key(key, map.segment_count)
    
    match acquire_segment_lock(map, segment_index) {
      Some(locked_map) => {
        let segment = locked_map.segments[segment_index]
        let mut new_entries = []
        let mut found = false
        
        for entry in segment.entries {
          if entry.key == key {
            new_entries = new_entries.push({
              key,
              value,
              version: entry.version + 1
            })
            found = true
          } else {
            new_entries = new_entries.push(entry)
          }
        }
        
        if not(found) {
          new_entries = new_entries.push({
            key,
            value,
            version: 1
          })
        }
        
        let updated_segments = locked_map.segments.set(segment_index, {
          entries: new_entries,
          lock: false
        })
        
        { locked_map | segments: updated_segments }
      }
      None => map // Lock not acquired
    }
  }
  
  let get = fn(map: ConcurrentMap[String, String], key: String) {
    let segment_index = hash_key(key, map.segment_count)
    let segment = map.segments[segment_index]
    
    let mut result = None
    for entry in segment.entries {
      if entry.key == key {
        result = Some((entry.value, entry.version))
      }
    }
    
    result
  }
  
  let remove = fn(map: ConcurrentMap[String, String], key: String) {
    let segment_index = hash_key(key, map.segment_count)
    
    match acquire_segment_lock(map, segment_index) {
      Some(locked_map) => {
        let segment = locked_map.segments[segment_index]
        let mut new_entries = []
        
        for entry in segment.entries {
          if entry.key != key {
            new_entries = new_entries.push(entry)
          }
        }
        
        let updated_segments = locked_map.segments.set(segment_index, {
          entries: new_entries,
          lock: false
        })
        
        { locked_map | segments: updated_segments }
      }
      None => map // Lock not acquired
    }
  }
  
  // Test concurrent map creation
  let map = create_concurrent_map(4)
  assert_eq(map.segment_count, 4)
  assert_eq(map.segments.length(), 4)
  
  for segment in map.segments {
    assert_eq(segment.entries.length(), 0)
    assert_false(segment.lock)
  }
  
  // Test put operations
  let map1 = put(map, "service.name", "payment-service")
  let map2 = put(map1, "service.version", "1.2.3")
  let map3 = put(map2, "environment", "production")
  
  // Test get operations
  let (value1, version1) = get(map3, "service.name").unwrap()
  assert_eq(value1, "payment-service")
  assert_eq(version1, 1)
  
  let (value2, version2) = get(map3, "service.version").unwrap()
  assert_eq(value2, "1.2.3")
  assert_eq(version2, 1)
  
  let (value3, version3) = get(map3, "environment").unwrap()
  assert_eq(value3, "production")
  assert_eq(version3, 1)
  
  let none_result = get(map3, "nonexistent")
  assert_eq(none_result, None)
  
  // Test update operation
  let map4 = put(map3, "service.version", "2.0.0")
  let (updated_value, updated_version) = get(map4, "service.version").unwrap()
  assert_eq(updated_value, "2.0.0")
  assert_eq(updated_version, 2) // Version should be incremented
  
  // Test remove operation
  let map5 = remove(map4, "environment")
  let removed_result = get(map5, "environment")
  assert_eq(removed_result, None)
  
  // Other keys should still exist
  let remaining_value = get(map5, "service.name")
  assert_eq(remaining_value, Some(("payment-service", 1)))
}

// Test 6: Future and Promise
test "future and promise for async telemetry operations" {
  // Define future/promise types
  type FutureState[T] = 
    | Pending
    | Completed(T)
    | Failed(String)
  
  type Future[T] = {
    id: String,
    state: FutureState[T],
    callbacks: Array[T -> ()]
  }
  
  type Promise[T] = {
    future: Future[T]
  }
  
  // Future/promise operations
  let create_promise = fn(id: String) {
    {
      future: {
        id,
        state: Pending,
        callbacks: []
      }
    }
  }
  
  let add_callback = fn(future: Future[T], callback: T -> ()) {
    match future.state {
      Pending => {
        { future | callbacks: future.callbacks.push(callback) }
      }
      Completed(value) => {
        callback(value) // Execute immediately
        future
      }
      Failed(_) => future // Don't execute on failure
    }
  }
  
  let fulfill = fn(promise: Promise[T], value: T) {
    match promise.future.state {
      Pending => {
        // Execute all callbacks
        for callback in promise.future.callbacks {
          callback(value)
        }
        
        {
          future: {
            id: promise.future.id,
            state: Completed(value),
            callbacks: []
          }
        }
      }
      _ => promise.future // Already completed or failed
    }
  }
  
  let reject = fn(promise: Promise[T], error: String) {
    match promise.future.state {
      Pending => {
        {
          future: {
            id: promise.future.id,
            state: Failed(error),
            callbacks: []
          }
        }
      }
      _ => promise.future // Already completed or failed
    }
  }
  
  let is_completed = fn(future: Future[T]) {
    match future.state {
      Completed(_) => true
      _ => false
    }
  }
  
  let get_result = fn(future: Future[T]) {
    match future.state {
      Completed(value) => Some(value)
      _ => None
    }
  }
  
  // Test promise creation
  let promise = create_promise("future-1")
  assert_eq(promise.future.id, "future-1")
  match promise.future.state {
    Pending => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(promise.future.callbacks.length(), 0)
  assert_false(is_completed(promise.future))
  
  // Test adding callbacks
  let callback_results = { mut values: [] }
  
  let callback1 = fn(value: String) {
    callback_results.values = callback_results.values.push("callback1:" + value)
  }
  
  let callback2 = fn(value: String) {
    callback_results.values = callback_results.values.push("callback2:" + value)
  }
  
  let future_with_callbacks = add_callback(promise.future, callback1)
  let future_with_more_callbacks = add_callback(future_with_callbacks, callback2)
  
  assert_eq(future_with_more_callbacks.callbacks.length(), 2)
  assert_eq(callback_results.values.length(), 0) // No callbacks executed yet
  
  // Test fulfilling promise
  let fulfilled_promise = {
    future: fulfill(promise, "success")
  }
  
  assert_eq(callback_results.values.length(), 2) // Both callbacks should be executed
  assert_true(callback_results.values.contains("callback1:success"))
  assert_true(callback_results.values.contains("callback2:success"))
  
  assert_true(is_completed(fulfilled_promise.future))
  assert_eq(get_result(fulfilled_promise.future), Some("success"))
  
  // Test rejecting promise
  let rejected_promise = create_promise("future-2")
  let rejected = reject(rejected_promise, "Something went wrong")
  
  match rejected.future.state {
    Failed(error) => assert_eq(error, "Something went wrong")
    _ => assert_true(false)
  }
  
  assert_false(is_completed(rejected.future))
  assert_eq(get_result(rejected.future), None)
  
  // Test adding callback to already completed future
  let completed_promise = create_promise("future-3")
  let pre_fulfilled = fulfill(completed_promise, "already done")
  
  let callback_results2 = { mut values: [] }
  let callback3 = fn(value: String) {
    callback_results2.values = callback_results2.values.push("callback3:" + value)
  }
  
  let future_with_callback = add_callback(pre_fulfilled.future, callback3)
  
  assert_eq(callback_results2.values.length(), 1) // Callback should be executed immediately
  assert_eq(callback_results2.values[0], "callback3:already done")
}

// Test 7: Concurrent Batch Processing
test "concurrent batch processing for telemetry data" {
  // Define batch processing types
  type BatchProcessor = {
    id: String,
    batch_size: Int,
    processed_count: Int
  }
  
  type ProcessingTask = {
    id: String,
    data: Array[String],
    status: String
  }
  
  type ConcurrentBatchSystem = {
    processors: Array[BatchProcessor],
    pending_tasks: Array[ProcessingTask>,
    completed_tasks: Array[ProcessingTask]
  }
  
  // Batch processing operations
  let create_processor = fn(id: String, batch_size: Int) {
    {
      id,
      batch_size,
      processed_count: 0
    }
  }
  
  let create_task = fn(id: String, data: Array[String]) {
    {
      id,
      data,
      status: "pending"
    }
  }
  
  let create_system = fn(processor_count: Int, batch_size: Int) {
    let mut processors = []
    for i in 0..processor_count {
      processors = processors.push(create_processor("processor-" + i.to_string(), batch_size))
    }
    
    {
      processors,
      pending_tasks: [],
      completed_tasks: []
    }
  }
  
  let submit_task = fn(system: ConcurrentBatchSystem, task: ProcessingTask) {
    { system | pending_tasks: system.pending_tasks.push(task) }
  }
  
  let assign_tasks = fn(system: ConcurrentBatchSystem) {
    let mut updated_processors = system.processors
    let mut remaining_tasks = system.pending_tasks
    let mut assigned_tasks = []
    
    // Assign tasks to available processors
    for i in 0..updated_processors.length() {
      if remaining_tasks.length() > 0 {
        let task = remaining_tasks[0]
        remaining_tasks = remaining_tasks.slice(1, remaining_tasks.length())
        assigned_tasks = assigned_tasks.push((i, task))
      }
    }
    
    // Update processors with assigned tasks
    for (processor_index, task) in assigned_tasks {
      let processor = updated_processors[processor_index]
      updated_processors = updated_processors.set(processor_index, {
        id: processor.id,
        batch_size: processor.batch_size,
        processed_count: processor.processed_count + task.data.length()
      })
    }
    
    {
      processors: updated_processors,
      pending_tasks: remaining_tasks,
      completed_tasks: system.completed_tasks
    }
  }
  
  let complete_tasks = fn(system: ConcurrentBatchSystem) {
    let mut completed_tasks = system.completed_tasks
    
    // Move all pending tasks to completed
    for task in system.pending_tasks {
      completed_tasks = completed_tasks.push({
        id: task.id,
        data: task.data,
        status: "completed"
      })
    }
    
    {
      processors: system.processors,
      pending_tasks: [],
      completed_tasks
    }
  }
  
  // Test system creation
  let system = create_system(3, 10)
  assert_eq(system.processors.length(), 3)
  assert_eq(system.processors[0].batch_size, 10)
  assert_eq(system.pending_tasks.length(), 0)
  assert_eq(system.completed_tasks.length(), 0)
  
  // Test submitting tasks
  let task1 = create_task("task-1", ["data-1", "data-2", "data-3"])
  let task2 = create_task("task-2", ["data-4", "data-5"])
  let task3 = create_task("task-3", ["data-6", "data-7", "data-8", "data-9"])
  let task4 = create_task("task-4", ["data-10"])
  
  let system1 = submit_task(system, task1)
  let system2 = submit_task(system1, task2)
  let system3 = submit_task(system2, task3)
  let system4 = submit_task(system3, task4)
  
  assert_eq(system4.pending_tasks.length(), 4)
  
  // Test task assignment
  let system5 = assign_tasks(system4)
  assert_eq(system5.pending_tasks.length(), 1) // 3 tasks assigned, 1 remaining
  assert_eq(system5.processors[0].processed_count, 3)
  assert_eq(system5.processors[1].processed_count, 2)
  assert_eq(system5.processors[2].processed_count, 4)
  
  // Test completing tasks
  let system6 = complete_tasks(system5)
  assert_eq(system6.pending_tasks.length(), 0)
  assert_eq(system6.completed_tasks.length(), 4)
  assert_eq(system6.completed_tasks[0].status, "completed")
  assert_eq(system6.completed_tasks[1].status, "completed")
  assert_eq(system6.completed_tasks[2].status, "completed")
  assert_eq(system6.completed_tasks[3].status, "completed")
}

// Test 8: Concurrent Rate Limiter
test "concurrent rate limiter for telemetry data submission" {
  // Define rate limiter types
  type TokenBucket = {
    capacity: Int,
    tokens: Int,
    refill_rate: Int,
    last_refill: Int
  }
  
  type RateLimiter = {
    bucket: TokenBucket,
    lock: Bool
  }
  
  // Rate limiter operations
  let create_token_bucket = fn(capacity: Int, refill_rate: Int) {
    {
      capacity,
      tokens: capacity,
      refill_rate,
      last_refill: 1640995200 // Mock timestamp
    }
  }
  
  let create_rate_limiter = fn(capacity: Int, refill_rate: Int) {
    {
      bucket: create_token_bucket(capacity, refill_rate),
      lock: false
    }
  }
  
  let refill_tokens = fn(bucket: TokenBucket, current_time: Int) {
    let time_passed = current_time - bucket.last_refill
    let tokens_to_add = time_passed * bucket.refill_rate
    
    let new_tokens = if bucket.tokens + tokens_to_add > bucket.capacity {
      bucket.capacity
    } else {
      bucket.tokens + tokens_to_add
    }
    
    {
      capacity: bucket.capacity,
      tokens: new_tokens,
      refill_rate: bucket.refill_rate,
      last_refill: current_time
    }
  }
  
  let try_consume = fn(limiter: RateLimiter, tokens: Int, current_time: Int) {
    if not(limiter.lock) {
      let refilled_bucket = refill_tokens(limiter.bucket, current_time)
      
      if refilled_bucket.tokens >= tokens {
        let updated_bucket = {
          capacity: refilled_bucket.capacity,
          tokens: refilled_bucket.tokens - tokens,
          refill_rate: refilled_bucket.refill_rate,
          last_refill: refilled_bucket.last_refill
        }
        
        (true, {
          bucket: updated_bucket,
          lock: false
        })
      } else {
        (false, {
          bucket: refilled_bucket,
          lock: false
        })
      }
    } else {
      (false, limiter) // Lock not acquired
    }
  }
  
  let get_available_tokens = fn(limiter: RateLimiter, current_time: Int) {
    let refilled_bucket = refill_tokens(limiter.bucket, current_time)
    refilled_bucket.tokens
  }
  
  // Test rate limiter creation
  let limiter = create_rate_limiter(10, 2) // 10 tokens capacity, 2 tokens per second
  assert_eq(limiter.bucket.capacity, 10)
  assert_eq(limiter.bucket.tokens, 10)
  assert_eq(limiter.bucket.refill_rate, 2)
  assert_false(limiter.lock)
  
  // Test token consumption
  let (success1, limiter1) = try_consume(limiter, 5, 1640995200)
  assert_true(success1)
  assert_eq(limiter1.bucket.tokens, 5)
  
  let (success2, limiter2) = try_consume(limiter1, 3, 1640995200)
  assert_true(success2)
  assert_eq(limiter2.bucket.tokens, 2)
  
  let (success3, limiter3) = try_consume(limiter2, 3, 1640995200)
  assert_false(success3) // Not enough tokens
  assert_eq(limiter3.bucket.tokens, 2)
  
  // Test token refill
  let available_tokens1 = get_available_tokens(limiter3, 1640995202) // 2 seconds later
  assert_eq(available_tokens1, 6) // 2 remaining + 2*2 refilled
  
  let (success4, limiter4) = try_consume(limiter3, 5, 1640995202)
  assert_true(success4)
  assert_eq(limiter4.bucket.tokens, 1)
  
  // Test capacity limit
  let available_tokens2 = get_available_tokens(limiter4, 1640995210) // 8 seconds later
  assert_eq(available_tokens2, 10) // Should be capped at capacity
  
  let limiter5 = { limiter4 | bucket: refill_tokens(limiter4.bucket, 1640995210) }
  assert_eq(limiter5.bucket.tokens, 10)
}