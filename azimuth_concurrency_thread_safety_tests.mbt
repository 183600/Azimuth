// Azimuth Concurrency and Thread Safety Test Suite
// This file contains test cases for concurrency and thread safety functionality

// Test 1: Concurrent Counter Operations
test "concurrent counter operations" {
  type ConcurrentCounter = {
    value: Int,
    lock_acquired: Bool
  }
  
  let create_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(counter: ConcurrentCounter) {
    if not(counter.lock_acquired) {
      { counter | lock_acquired: true }
    } else {
      counter  // Lock already acquired
    }
  }
  
  let release_lock = fn(counter: ConcurrentCounter) {
    { counter | lock_acquired: false }
  }
  
  let increment = fn(counter: ConcurrentCounter) {
    if counter.lock_acquired {
      { counter | value: counter.value + 1 }
    } else {
      counter  // Cannot increment without lock
    }
  }
  
  let safe_increment = fn(counter: ConcurrentCounter) {
    let locked_counter = acquire_lock(counter)
    let incremented = increment(locked_counter)
    release_lock(incremented)
  }
  
  let counter = create_counter(0)
  assert_eq(counter.value, 0)
  assert_false(counter.lock_acquired)
  
  let locked_counter = acquire_lock(counter)
  assert_true(locked_counter.lock_acquired)
  
  let incremented = increment(locked_counter)
  assert_eq(incremented.value, 1)
  assert_true(incremented.lock_acquired)
  
  let released = release_lock(incremented)
  assert_eq(released.value, 1)
  assert_false(released.lock_acquired)
  
  // Test safe increment
  let safe_counter = safe_increment(released)
  assert_eq(safe_counter.value, 2)
  assert_false(safe_counter.lock_acquired)
  
  // Test increment without lock
  let unsafe_incremented = increment(safe_counter)
  assert_eq(unsafe_incremented.value, 2)  // Value unchanged
  assert_false(unsafe_incremented.lock_acquired)
}

// Test 2: Thread-Safe Queue Operations
test "thread-safe queue operations" {
  type QueueNode = {
    value: String,
    next: Option[QueueNode]
  }
  
  type ConcurrentQueue = {
    head: Option[QueueNode],
    tail: Option[QueueNode],
    size: Int,
    lock_acquired: Bool
  }
  
  let create_queue = fn() {
    {
      head: None,
      tail: None,
      size: 0,
      lock_acquired: false
    }
  }
  
  let create_node = fn(value: String) {
    {
      value,
      next: None
    }
  }
  
  let acquire_lock = fn(queue: ConcurrentQueue) {
    if not(queue.lock_acquired) {
      { queue | lock_acquired: true }
    } else {
      queue  // Lock already acquired
    }
  }
  
  let release_lock = fn(queue: ConcurrentQueue) {
    { queue | lock_acquired: false }
  }
  
  let enqueue = fn(queue: ConcurrentQueue, value: String) {
    if queue.lock_acquired {
      let new_node = create_node(value)
      
      match queue.tail {
        Some(tail_node) => {
          let updated_tail = { tail_node | next: Some(new_node) }
          {
            head: queue.head,
            tail: Some(new_node),
            size: queue.size + 1,
            lock_acquired: true
          }
        }
        None => {
          // Empty queue
          {
            head: Some(new_node),
            tail: Some(new_node),
            size: 1,
            lock_acquired: true
          }
        }
      }
    } else {
      queue  // Cannot enqueue without lock
    }
  }
  
  let dequeue = fn(queue: ConcurrentQueue) {
    if queue.lock_acquired && queue.size > 0 {
      match queue.head {
        Some(head_node) => {
          let new_head = head_node.next
          let new_size = queue.size - 1
          
          if new_size == 0 {
            // Queue becomes empty
            {
              head: None,
              tail: None,
              size: 0,
              lock_acquired: true
            }
          } else {
            {
              head: new_head,
              tail: queue.tail,
              size: new_size,
              lock_acquired: true
            }
          }
        }
        None => queue  // Shouldn't happen if size > 0
      }
    } else {
      queue  // Cannot dequeue without lock or if empty
    }
  }
  
  let safe_enqueue = fn(queue: ConcurrentQueue, value: String) {
    let locked_queue = acquire_lock(queue)
    let enqueued = enqueue(locked_queue, value)
    release_lock(enqueued)
  }
  
  let safe_dequeue = fn(queue: ConcurrentQueue) {
    let locked_queue = acquire_lock(queue)
    let dequeued = dequeue(locked_queue)
    release_lock(dequeued)
  }
  
  let queue = create_queue()
  assert_eq(queue.size, 0)
  assert_false(queue.lock_acquired)
  
  // Test safe enqueue
  let queue1 = safe_enqueue(queue, "item1")
  assert_eq(queue1.size, 1)
  assert_false(queue1.lock_acquired)
  
  let queue2 = safe_enqueue(queue1, "item2")
  assert_eq(queue2.size, 2)
  assert_false(queue2.lock_acquired)
  
  let queue3 = safe_enqueue(queue2, "item3")
  assert_eq(queue3.size, 3)
  assert_false(queue3.lock_acquired)
  
  // Test safe dequeue
  let queue4 = safe_dequeue(queue3)
  assert_eq(queue4.size, 2)
  assert_false(queue4.lock_acquired)
  
  let queue5 = safe_dequeue(queue4)
  assert_eq(queue5.size, 1)
  assert_false(queue5.lock_acquired)
  
  let queue6 = safe_dequeue(queue5)
  assert_eq(queue6.size, 0)
  assert_false(queue6.lock_acquired)
  
  // Test dequeue from empty queue
  let empty_queue = safe_dequeue(queue6)
  assert_eq(empty_queue.size, 0)
  assert_false(empty_queue.lock_acquired)
}

// Test 3: Concurrent Map Operations
test "concurrent map operations" {
  type MapEntry = {
    key: String,
    value: String
  }
  
  type ConcurrentMap = {
    entries: Array[MapEntry],
    lock_acquired: Bool
  }
  
  let create_map = fn() {
    {
      entries: [],
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(map: ConcurrentMap) {
    if not(map.lock_acquired) {
      { map | lock_acquired: true }
    } else {
      map  // Lock already acquired
    }
  }
  
  let release_lock = fn(map: ConcurrentMap) {
    { map | lock_acquired: false }
  }
  
  let get = fn(map: ConcurrentMap, key: String) {
    let mut found = None
    for entry in map.entries {
      if entry.key == key {
        found = Some(entry.value)
      }
    }
    found
  }
  
  let put = fn(map: ConcurrentMap, key: String, value: String) {
    if map.lock_acquired {
      let mut found = false
      let mut updated = []
      
      for entry in map.entries {
        if entry.key == key {
          updated = updated.push({ entry | value })
          found = true
        } else {
          updated = updated.push(entry)
        }
      }
      
      if not(found) {
        updated = updated.push({ key, value })
      }
      
      { map | entries: updated }
    } else {
      map  // Cannot put without lock
    }
  }
  
  let remove = fn(map: ConcurrentMap, key: String) {
    if map.lock_acquired {
      let filtered = map.entries.filter(fn(entry) { entry.key != key })
      { map | entries: filtered }
    } else {
      map  // Cannot remove without lock
    }
  }
  
  let safe_put = fn(map: ConcurrentMap, key: String, value: String) {
    let locked_map = acquire_lock(map)
    let updated = put(locked_map, key, value)
    release_lock(updated)
  }
  
  let safe_remove = fn(map: ConcurrentMap, key: String) {
    let locked_map = acquire_lock(map)
    let updated = remove(locked_map, key)
    release_lock(updated)
  }
  
  let map = create_map()
  assert_eq(map.entries.length(), 0)
  assert_false(map.lock_acquired)
  
  // Test safe put
  let map1 = safe_put(map, "key1", "value1")
  assert_eq(map1.entries.length(), 1)
  assert_eq(get(map1, "key1"), Some("value1"))
  
  let map2 = safe_put(map1, "key2", "value2")
  assert_eq(map2.entries.length(), 2)
  assert_eq(get(map2, "key1"), Some("value1"))
  assert_eq(get(map2, "key2"), Some("value2"))
  
  // Test update existing key
  let map3 = safe_put(map2, "key1", "updated_value1")
  assert_eq(map3.entries.length(), 2)
  assert_eq(get(map3, "key1"), Some("updated_value1"))
  assert_eq(get(map3, "key2"), Some("value2"))
  
  // Test safe remove
  let map4 = safe_remove(map3, "key2")
  assert_eq(map4.entries.length(), 1)
  assert_eq(get(map4, "key1"), Some("updated_value1"))
  assert_eq(get(map4, "key2"), None)
  
  let map5 = safe_remove(map4, "key1")
  assert_eq(map5.entries.length(), 0)
  assert_eq(get(map5, "key1"), None)
}

// Test 4: Concurrent Resource Pool
test "concurrent resource pool" {
  type Resource = {
    id: String,
    in_use: Bool
  }
  
  type ResourcePool = {
    resources: Array[Resource],
    lock_acquired: Bool
  }
  
  let create_pool = fn(resource_ids: Array[String]) {
    {
      resources: resource_ids.map(fn(id) { { id, in_use: false } }),
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(pool: ResourcePool) {
    if not(pool.lock_acquired) {
      { pool | lock_acquired: true }
    } else {
      pool  // Lock already acquired
    }
  }
  
  let release_lock = fn(pool: ResourcePool) {
    { pool | lock_acquired: false }
  }
  
  let acquire_resource = fn(pool: ResourcePool) {
    if pool.lock_acquired {
      let mut found = None
      let mut updated = []
      
      for resource in pool.resources {
        if not(resource.in_use) && found.is_none() {
          found = Some({ resource | in_use: true })
          updated = updated.push({ resource | in_use: true })
        } else {
          updated = updated.push(resource)
        }
      }
      
      match found {
        Some(res) => {
          {
            resources: updated,
            lock_acquired: true
          }
        }
        None => pool  // No available resources
      }
    } else {
      pool  // Cannot acquire without lock
    }
  }
  
  let release_resource = fn(pool: ResourcePool, resource_id: String) {
    if pool.lock_acquired {
      let updated = pool.resources.map(fn(resource) {
        if resource.id == resource_id {
          { resource | in_use: false }
        } else {
          resource
        }
      })
      
      { pool | resources: updated }
    } else {
      pool  // Cannot release without lock
    }
  }
  
  let safe_acquire_resource = fn(pool: ResourcePool) {
    let locked_pool = acquire_lock(pool)
    let result = acquire_resource(locked_pool)
    release_lock(result)
  }
  
  let safe_release_resource = fn(pool: ResourcePool, resource_id: String) {
    let locked_pool = acquire_lock(pool)
    let result = release_resource(locked_pool, resource_id)
    release_lock(result)
  }
  
  let pool = create_pool(["res1", "res2", "res3"])
  assert_eq(pool.resources.length(), 3)
  assert_true(pool.resources.all(fn(r) { not(r.in_use) }))
  
  // Test safe acquire
  let pool1 = safe_acquire_resource(pool)
  let in_use_count1 = pool1.resources.filter(fn(r) { r.in_use }).length()
  assert_eq(in_use_count1, 1)
  
  let pool2 = safe_acquire_resource(pool1)
  let in_use_count2 = pool2.resources.filter(fn(r) { r.in_use }).length()
  assert_eq(in_use_count2, 2)
  
  let pool3 = safe_acquire_resource(pool2)
  let in_use_count3 = pool3.resources.filter(fn(r) { r.in_use }).length()
  assert_eq(in_use_count3, 3)
  
  // All resources are in use now
  let pool4 = safe_acquire_resource(pool3)
  let in_use_count4 = pool4.resources.filter(fn(r) { r.in_use }).length()
  assert_eq(in_use_count4, 3)  // No change
  
  // Test safe release
  let pool5 = safe_release_resource(pool4, "res1")
  let in_use_count5 = pool5.resources.filter(fn(r) { r.in_use }).length()
  assert_eq(in_use_count5, 2)
  
  let pool6 = safe_release_resource(pool5, "res2")
  let in_use_count6 = pool6.resources.filter(fn(r) { r.in_use }).length()
  assert_eq(in_use_count6, 1)
  
  let pool7 = safe_release_resource(pool6, "res3")
  let in_use_count7 = pool7.resources.filter(fn(r) { r.in_use }).length()
  assert_eq(in_use_count7, 0)
}

// Test 5: Concurrent Task Execution
test "concurrent task execution" {
  type Task = {
    id: String,
    status: String,  // "pending", "running", "completed", "failed"
    result: Option[String]
  }
  
  type TaskExecutor = {
    tasks: Array[Task],
    running_tasks: Int,
    max_concurrent: Int,
    lock_acquired: Bool
  }
  
  let create_task = fn(id: String) {
    {
      id,
      status: "pending",
      result: None
    }
  }
  
  let create_executor = fn(max_concurrent: Int) {
    {
      tasks: [],
      running_tasks: 0,
      max_concurrent,
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(executor: TaskExecutor) {
    if not(executor.lock_acquired) {
      { executor | lock_acquired: true }
    } else {
      executor  // Lock already acquired
    }
  }
  
  let release_lock = fn(executor: TaskExecutor) {
    { executor | lock_acquired: false }
  }
  
  let submit_task = fn(executor: TaskExecutor, task: Task) {
    if executor.lock_acquired {
      {
        tasks: executor.tasks + [task],
        running_tasks: executor.running_tasks,
        max_concurrent: executor.max_concurrent,
        lock_acquired: true
      }
    } else {
      executor  // Cannot submit without lock
    }
  }
  
  let execute_next_task = fn(executor: TaskExecutor) {
    if executor.lock_acquired && executor.running_tasks < executor.max_concurrent {
      let mut found = false
      let mut updated = []
      
      for task in executor.tasks {
        if task.status == "pending" && not(found) {
          updated = updated.push({ task | status: "running" })
          found = true
        } else {
          updated = updated.push(task)
        }
      }
      
      if found {
        {
          tasks: updated,
          running_tasks: executor.running_tasks + 1,
          max_concurrent: executor.max_concurrent,
          lock_acquired: true
        }
      } else {
        executor  // No pending tasks
      }
    } else {
      executor  // Cannot execute without lock or at capacity
    }
  }
  
  let complete_task = fn(executor: TaskExecutor, task_id: String, result: String) {
    if executor.lock_acquired {
      let updated = executor.tasks.map(fn(task) {
        if task.id == task_id && task.status == "running" {
          { task | status: "completed", result: Some(result) }
        } else {
          task
        }
      })
      
      {
        tasks: updated,
        running_tasks: executor.running_tasks - 1,
        max_concurrent: executor.max_concurrent,
        lock_acquired: true
      }
    } else {
      executor  // Cannot complete without lock
    }
  }
  
  let safe_submit_task = fn(executor: TaskExecutor, task: Task) {
    let locked_executor = acquire_lock(executor)
    let result = submit_task(locked_executor, task)
    release_lock(result)
  }
  
  let safe_execute_next_task = fn(executor: TaskExecutor) {
    let locked_executor = acquire_lock(executor)
    let result = execute_next_task(locked_executor)
    release_lock(result)
  }
  
  let safe_complete_task = fn(executor: TaskExecutor, task_id: String, result: String) {
    let locked_executor = acquire_lock(executor)
    let updated = complete_task(locked_executor, task_id, result)
    release_lock(updated)
  }
  
  let executor = create_executor(2)
  assert_eq(executor.running_tasks, 0)
  assert_eq(executor.max_concurrent, 2)
  
  // Submit tasks
  let task1 = create_task("task1")
  let task2 = create_task("task2")
  let task3 = create_task("task3")
  
  let executor1 = safe_submit_task(executor, task1)
  assert_eq(executor1.tasks.length(), 1)
  
  let executor2 = safe_submit_task(executor1, task2)
  assert_eq(executor2.tasks.length(), 2)
  
  let executor3 = safe_submit_task(executor2, task3)
  assert_eq(executor3.tasks.length(), 3)
  
  // Execute tasks
  let executor4 = safe_execute_next_task(executor3)
  assert_eq(executor4.running_tasks, 1)
  
  let executor5 = safe_execute_next_task(executor4)
  assert_eq(executor5.running_tasks, 2)
  
  // At max capacity, cannot execute more
  let executor6 = safe_execute_next_task(executor5)
  assert_eq(executor6.running_tasks, 2)
  
  // Complete a task
  let executor7 = safe_complete_task(executor6, "task1", "result1")
  assert_eq(executor7.running_tasks, 1)
  
  // Can execute another task now
  let executor8 = safe_execute_next_task(executor7)
  assert_eq(executor8.running_tasks, 2)
  
  // Complete remaining tasks
  let executor9 = safe_complete_task(executor8, "task2", "result2")
  assert_eq(executor9.running_tasks, 1)
  
  let executor10 = safe_complete_task(executor9, "task3", "result3")
  assert_eq(executor10.running_tasks, 0)
}

// Test 6: Concurrent Event Handling
test "concurrent event handling" {
  type Event = {
    id: String,
    type: String,
    data: String,
    timestamp: Int
  }
  
  type EventHandler = {
    id: String,
    event_types: Array[String],
    processed_events: Array[String]
  }
  
  type EventBus = {
    handlers: Array[EventHandler],
    events: Array[Event],
    lock_acquired: Bool
  }
  
  let create_event = fn(id: String, type: String, data: String) {
    {
      id,
      type,
      data,
      timestamp: 1640995200
    }
  }
  
  let create_handler = fn(id: String, event_types: Array[String]) {
    {
      id,
      event_types,
      processed_events: []
    }
  }
  
  let create_event_bus = fn() {
    {
      handlers: [],
      events: [],
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(bus: EventBus) {
    if not(bus.lock_acquired) {
      { bus | lock_acquired: true }
    } else {
      bus  // Lock already acquired
    }
  }
  
  let release_lock = fn(bus: EventBus) {
    { bus | lock_acquired: false }
  }
  
  let register_handler = fn(bus: EventBus, handler: EventHandler) {
    if bus.lock_acquired {
      {
        handlers: bus.handlers + [handler],
        events: bus.events,
        lock_acquired: true
      }
    } else {
      bus  // Cannot register without lock
    }
  }
  
  let publish_event = fn(bus: EventBus, event: Event) {
    if bus.lock_acquired {
      {
        handlers: bus.handlers,
        events: bus.events + [event],
        lock_acquired: true
      }
    } else {
      bus  // Cannot publish without lock
    }
  }
  
  let process_events = fn(bus: EventBus) {
    if bus.lock_acquired {
      let mut updated_handlers = []
      
      for handler in bus.handlers {
        let mut processed = handler.processed_events
        
        for event in bus.events {
          if handler.event_types.contains(event.type) && 
             not(processed.contains(event.id)) {
            processed = processed + [event.id]
          }
        }
        
        updated_handlers = updated_handlers.push({ handler | processed_events: processed })
      }
      
      {
        handlers: updated_handlers,
        events: bus.events,
        lock_acquired: true
      }
    } else {
      bus  // Cannot process without lock
    }
  }
  
  let safe_register_handler = fn(bus: EventBus, handler: EventHandler) {
    let locked_bus = acquire_lock(bus)
    let result = register_handler(locked_bus, handler)
    release_lock(result)
  }
  
  let safe_publish_event = fn(bus: EventBus, event: Event) {
    let locked_bus = acquire_lock(bus)
    let result = publish_event(locked_bus, event)
    release_lock(result)
  }
  
  let safe_process_events = fn(bus: EventBus) {
    let locked_bus = acquire_lock(bus)
    let result = process_events(locked_bus)
    release_lock(result)
  }
  
  let bus = create_event_bus()
  assert_eq(bus.handlers.length(), 0)
  assert_eq(bus.events.length(), 0)
  
  // Register handlers
  let handler1 = create_handler("handler1", ["user_event", "system_event"])
  let handler2 = create_handler("handler2", ["user_event"])
  
  let bus1 = safe_register_handler(bus, handler1)
  assert_eq(bus1.handlers.length(), 1)
  
  let bus2 = safe_register_handler(bus1, handler2)
  assert_eq(bus2.handlers.length(), 2)
  
  // Publish events
  let event1 = create_event("event1", "user_event", "User logged in")
  let event2 = create_event("event2", "system_event", "System started")
  let event3 = create_event("event3", "order_event", "Order placed")
  
  let bus3 = safe_publish_event(bus2, event1)
  assert_eq(bus3.events.length(), 1)
  
  let bus4 = safe_publish_event(bus3, event2)
  assert_eq(bus4.events.length(), 2)
  
  let bus5 = safe_publish_event(bus4, event3)
  assert_eq(bus5.events.length(), 3)
  
  // Process events
  let bus6 = safe_process_events(bus5)
  
  // Check handler1 processed events
  let handler1_updated = bus6.handlers.filter(fn(h) { h.id == "handler1" })[0]
  assert_eq(handler1_updated.processed_events.length(), 2)
  assert_true(handler1_updated.processed_events.contains("event1"))
  assert_true(handler1_updated.processed_events.contains("event2"))
  assert_false(handler1_updated.processed_events.contains("event3"))
  
  // Check handler2 processed events
  let handler2_updated = bus6.handlers.filter(fn(h) { h.id == "handler2" })[0]
  assert_eq(handler2_updated.processed_events.length(), 1)
  assert_true(handler2_updated.processed_events.contains("event1"))
  assert_false(handler2_updated.processed_events.contains("event2"))
  assert_false(handler2_updated.processed_events.contains("event3"))
}

// Test 7: Concurrent Cache Operations
test "concurrent cache operations" {
  type CacheEntry = {
    key: String,
    value: String,
    timestamp: Int,
    ttl: Int  // Time to live in seconds
  }
  
  type ConcurrentCache = {
    entries: Array[CacheEntry],
    lock_acquired: Bool
  }
  
  let create_cache = fn() {
    {
      entries: [],
      lock_acquired: false
    }
  }
  
  let create_entry = fn(key: String, value: String, timestamp: Int, ttl: Int) {
    {
      key,
      value,
      timestamp,
      ttl
    }
  }
  
  let acquire_lock = fn(cache: ConcurrentCache) {
    if not(cache.lock_acquired) {
      { cache | lock_acquired: true }
    } else {
      cache  // Lock already acquired
    }
  }
  
  let release_lock = fn(cache: ConcurrentCache) {
    { cache | lock_acquired: false }
  }
  
  let get = fn(cache: ConcurrentCache, key: String, current_time: Int) {
    let mut found = None
    
    for entry in cache.entries {
      if entry.key == key && (current_time - entry.timestamp) < entry.ttl {
        found = Some(entry.value)
      }
    }
    
    found
  }
  
  let put = fn(cache: ConcurrentCache, key: String, value: String, current_time: Int, ttl: Int) {
    if cache.lock_acquired {
      let mut found = false
      let mut updated = []
      
      for entry in cache.entries {
        if entry.key == key {
          updated = updated.push(create_entry(key, value, current_time, ttl))
          found = true
        } else {
          updated = updated.push(entry)
        }
      }
      
      if not(found) {
        updated = updated.push(create_entry(key, value, current_time, ttl))
      }
      
      { cache | entries: updated }
    } else {
      cache  // Cannot put without lock
    }
  }
  
  let remove_expired = fn(cache: ConcurrentCache, current_time: Int) {
    if cache.lock_acquired {
      let filtered = cache.entries.filter(fn(entry) { 
        (current_time - entry.timestamp) < entry.ttl 
      })
      
      { cache | entries: filtered }
    } else {
      cache  // Cannot remove without lock
    }
  }
  
  let safe_put = fn(cache: ConcurrentCache, key: String, value: String, current_time: Int, ttl: Int) {
    let locked_cache = acquire_lock(cache)
    let updated = put(locked_cache, key, value, current_time, ttl)
    release_lock(updated)
  }
  
  let safe_remove_expired = fn(cache: ConcurrentCache, current_time: Int) {
    let locked_cache = acquire_lock(cache)
    let updated = remove_expired(locked_cache, current_time)
    release_lock(updated)
  }
  
  let cache = create_cache()
  assert_eq(cache.entries.length(), 0)
  
  let base_time = 1640995200
  
  // Test safe put
  let cache1 = safe_put(cache, "key1", "value1", base_time, 60)
  assert_eq(cache1.entries.length(), 1)
  assert_eq(get(cache1, "key1", base_time), Some("value1"))
  
  let cache2 = safe_put(cache1, "key2", "value2", base_time + 10, 30)
  assert_eq(cache2.entries.length(), 2)
  assert_eq(get(cache2, "key1", base_time + 10), Some("value1"))
  assert_eq(get(cache2, "key2", base_time + 10), Some("value2"))
  
  // Test update existing key
  let cache3 = safe_put(cache2, "key1", "updated_value1", base_time + 20, 60)
  assert_eq(cache3.entries.length(), 2)
  assert_eq(get(cache3, "key1", base_time + 20), Some("updated_value1"))
  assert_eq(get(cache3, "key2", base_time + 20), Some("value2"))
  
  // Test expiration
  let cache4 = safe_remove_expired(cache3, base_time + 40)  // key2 should be expired
  assert_eq(cache4.entries.length(), 1)
  assert_eq(get(cache4, "key1", base_time + 40), Some("updated_value1"))
  assert_eq(get(cache4, "key2", base_time + 40), None)
  
  // Test all expired
  let cache5 = safe_remove_expired(cache4, base_time + 100)  // All should be expired
  assert_eq(cache5.entries.length(), 0)
  assert_eq(get(cache5, "key1", base_time + 100), None)
}

// Test 8: Concurrent Rate Limiter
test "concurrent rate limiter" {
  type RateLimiter = {
    max_requests: Int,
    window_seconds: Int,
    request_timestamps: Array[Int],
    lock_acquired: Bool
  }
  
  let create_rate_limiter = fn(max_requests: Int, window_seconds: Int) {
    {
      max_requests,
      window_seconds,
      request_timestamps: [],
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(limiter: RateLimiter) {
    if not(limiter.lock_acquired) {
      { limiter | lock_acquired: true }
    } else {
      limiter  // Lock already acquired
    }
  }
  
  let release_lock = fn(limiter: RateLimiter) {
    { limiter | lock_acquired: false }
  }
  
  let is_allowed = fn(limiter: RateLimiter, current_time: Int) {
    if limiter.lock_acquired {
      // Remove timestamps outside the window
      let window_start = current_time - limiter.window_seconds
      let valid_timestamps = limiter.request_timestamps.filter(fn(ts) { ts >= window_start })
      
      if valid_timestamps.length() < limiter.max_requests {
        // Allow the request
        {
          max_requests: limiter.max_requests,
          window_seconds: limiter.window_seconds,
          request_timestamps: valid_timestamps + [current_time],
          lock_acquired: true
        }
      } else {
        // Deny the request
        limiter
      }
    } else {
      limiter  // Cannot check without lock
    }
  }
  
  let safe_is_allowed = fn(limiter: RateLimiter, current_time: Int) {
    let locked_limiter = acquire_lock(limiter)
    let result = is_allowed(locked_limiter, current_time)
    release_lock(result)
  }
  
  let limiter = create_rate_limiter(5, 60)  // 5 requests per 60 seconds
  assert_eq(limiter.max_requests, 5)
  assert_eq(limiter.window_seconds, 60)
  assert_eq(limiter.request_timestamps.length(), 0)
  
  let base_time = 1640995200
  
  // Test allowed requests
  let limiter1 = safe_is_allowed(limiter, base_time)
  assert_eq(limiter1.request_timestamps.length(), 1)
  
  let limiter2 = safe_is_allowed(limiter1, base_time + 10)
  assert_eq(limiter2.request_timestamps.length(), 2)
  
  let limiter3 = safe_is_allowed(limiter2, base_time + 20)
  assert_eq(limiter3.request_timestamps.length(), 3)
  
  let limiter4 = safe_is_allowed(limiter3, base_time + 30)
  assert_eq(limiter4.request_timestamps.length(), 4)
  
  let limiter5 = safe_is_allowed(limiter4, base_time + 40)
  assert_eq(limiter5.request_timestamps.length(), 5)
  
  // Test denied request (at limit)
  let limiter6 = safe_is_allowed(limiter5, base_time + 50)
  assert_eq(limiter6.request_timestamps.length(), 5)  // No change
  
  // Test after window passes
  let limiter7 = safe_is_allowed(limiter6, base_time + 70)  // 70 seconds later
  assert_eq(limiter7.request_timestamps.length(), 1)  // Only the new request
  
  // Test multiple requests in new window
  let limiter8 = safe_is_allowed(limiter7, base_time + 80)
  assert_eq(limiter8.request_timestamps.length(), 2)
  
  let limiter9 = safe_is_allowed(limiter8, base_time + 90)
  assert_eq(limiter9.request_timestamps.length(), 3)
}