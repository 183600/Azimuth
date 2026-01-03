// Azimuth Concurrent Safety Tests
// This file contains test cases for concurrent safety and thread safety

// Test 1: Thread-Safe Counter Operations
test "thread-safe counter operations" {
  // Simulate atomic counter operations
  let atomic_counter = 0
  
  let atomic_increment = fn(counter) {
    // Simulate atomic increment
    counter + 1
  }
  
  let atomic_decrement = fn(counter) {
    // Simulate atomic decrement
    counter - 1
  }
  
  let atomic_add = fn(counter, value) {
    // Simulate atomic add
    counter + value
  }
  
  // Test atomic operations
  let counter1 = atomic_increment(atomic_counter)
  assert_eq(counter1, 1)
  
  let counter2 = atomic_increment(counter1)
  assert_eq(counter2, 2)
  
  let counter3 = atomic_decrement(counter2)
  assert_eq(counter3, 1)
  
  let counter4 = atomic_add(counter3, 5)
  assert_eq(counter4, 6)
  
  // Test concurrent counter simulation
  let simulate_concurrent_increments = fn(initial_count, num_threads, increments_per_thread) {
    // Simulate concurrent increments
    initial_count + (num_threads * increments_per_thread)
  }
  
  let concurrent_result = simulate_concurrent_increments(0, 10, 5)
  assert_eq(concurrent_result, 50)
}

// Test 2: Thread-Safe Data Structures
test "thread-safe data structures" {
  // Test thread-safe array operations
  let thread_safe_array = []
  
  let thread_safe_push = fn(array, item) {
    // Simulate thread-safe push
    array.push(item)
  }
  
  let thread_safe_get = fn(array, index) {
    // Simulate thread-safe get with bounds checking
    if index >= 0 && index < array.length() {
      Some(array[index])
    } else {
      None
    }
  }
  
  // Test operations
  let array1 = thread_safe_push(thread_safe_array, "item1")
  let array2 = thread_safe_push(array1, "item2")
  let array3 = thread_safe_push(array2, "item3")
  
  assert_eq(array3.length(), 3)
  
  match thread_safe_get(array3, 0) {
    None => assert_true(false)
    Some(item) => assert_eq(item, "item1")
  }
  
  match thread_safe_get(array3, 2) {
    None => assert_true(false)
    Some(item) => assert_eq(item, "item3")
  }
  
  match thread_safe_get(array3, 5) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test thread-safe map operations
  let thread_safe_map = {}
  
  let thread_safe_put = fn(map, key, value) {
    // Simulate thread-safe put
    map.with(key, value)
  }
  
  let thread_safe_get_map = fn(map, key) {
    // Simulate thread-safe get
    match map.get(key) {
      None => None
      Some(value) => Some(value)
    }
  }
  
  let map1 = thread_safe_put(thread_safe_map, "key1", "value1")
  let map2 = thread_safe_put(map1, "key2", "value2")
  
  match thread_safe_get_map(map2, "key1") {
    None => assert_true(false)
    Some(value) => assert_eq(value, "value1")
  }
  
  match thread_safe_get_map(map2, "key3") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

// Test 3: Lock and Mutex Simulation
test "lock and mutex simulation" {
  // Simulate mutex lock
  let create_mutex = fn(initial_value) {
    { value = initial_value, locked = false }
  }
  
  let acquire_lock = fn(mutex) {
    if mutex.locked {
      { mutex = mutex, acquired = false }
    } else {
      { mutex = { value = mutex.value, locked = true }, acquired = true }
    }
  }
  
  let release_lock = fn(mutex) {
    { value = mutex.value, locked = false }
  }
  
  let with_lock = fn(mutex, operation) {
    let lock_result = acquire_lock(mutex)
    if lock_result.acquired {
      let result = operation(lock_result.mutex.value)
      let released_mutex = release_lock(lock_result.mutex)
      { result = result, mutex = released_mutex, success = true }
    } else {
      { result = None, mutex = mutex, success = false }
    }
  }
  
  // Test mutex operations
  let mutex = create_mutex(42)
  
  let operation_result = with_lock(mutex, fn(value) {
    value * 2
  })
  
  assert_true(operation_result.success)
  match operation_result.result {
    None => assert_true(false)
    Some(result) => assert_eq(result, 84)
  }
  assert_false(operation_result.mutex.locked)
  
  // Test with locked mutex
  let locked_mutex = { value = 100, locked = true }
  let failed_operation = with_lock(locked_mutex, fn(value) {
    value * 2
  })
  
  assert_false(failed_operation.success)
  match failed_operation.result {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

// Test 4: Read-Write Lock Simulation
test "read-write lock simulation" {
  // Simulate read-write lock
  let create_rwlock = fn(initial_value) {
    { value = initial_value, readers = 0, writer = false }
  }
  
  let acquire_read_lock = fn(rwlock) {
    if rwlock.writer {
      { rwlock = rwlock, acquired = false }
    } else {
      { rwlock = { value = rwlock.value, readers = rwlock.readers + 1, writer = false }, acquired = true }
    }
  }
  
  let release_read_lock = fn(rwlock) {
    { value = rwlock.value, readers = rwlock.readers - 1, writer = false }
  }
  
  let acquire_write_lock = fn(rwlock) {
    if rwlock.writer || rwlock.readers > 0 {
      { rwlock = rwlock, acquired = false }
    } else {
      { rwlock = { value = rwlock.value, readers = 0, writer = true }, acquired = true }
    }
  }
  
  let release_write_lock = fn(rwlock) {
    { value = rwlock.value, readers = 0, writer = false }
  }
  
  let with_read_lock = fn(rwlock, operation) {
    let lock_result = acquire_read_lock(rwlock)
    if lock_result.acquired {
      let result = operation(lock_result.rwlock.value)
      let released_rwlock = release_read_lock(lock_result.rwlock)
      { result = result, rwlock = released_rwlock, success = true }
    } else {
      { result = None, rwlock = rwlock, success = false }
    }
  }
  
  let with_write_lock = fn(rwlock, operation) {
    let lock_result = acquire_write_lock(rwlock)
    if lock_result.acquired {
      let new_value = operation(lock_result.rwlock.value)
      let updated_rwlock = { value = new_value, readers = 0, writer = true }
      let released_rwlock = release_write_lock(updated_rwlock)
      { result = new_value, rwlock = released_rwlock, success = true }
    } else {
      { result = None, rwlock = rwlock, success = false }
    }
  }
  
  // Test read-write lock operations
  let rwlock = create_rwlock("initial_value")
  
  // Test read operations
  let read_result = with_read_lock(rwlock, fn(value) {
    value.length()
  })
  
  assert_true(read_result.success)
  match read_result.result {
    None => assert_true(false)
    Some(result) => assert_eq(result, 13)  // "initial_value".length()
  }
  
  // Test write operations
  let write_result = with_write_lock(rwlock, fn(value) {
    value + "_modified"
  })
  
  assert_true(write_result.success)
  match write_result.result {
    None => assert_true(false)
    Some(result) => assert_eq(result, "initial_value_modified")
  }
  
  // Test concurrent readers
  let rwlock_with_readers = { value = "test", readers = 3, writer = false }
  let failed_write = with_write_lock(rwlock_with_readers, fn(value) {
    value + "_failed"
  })
  
  assert_false(failed_write.success)
  match failed_write.result {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test with active writer
  let rwlock_with_writer = { value = "test", readers = 0, writer = true }
  let failed_read = with_read_lock(rwlock_with_writer, fn(value) {
    value.length()
  })
  
  assert_false(failed_read.success)
  match failed_read.result {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

// Test 5: Concurrent Queue Simulation
test "concurrent queue simulation" {
  // Simulate thread-safe queue
  let create_queue = fn() {
    { items = [], head = 0, tail = 0 }
  }
  
  let enqueue = fn(queue, item) {
    // Simulate thread-safe enqueue
    { items = queue.items.push(item), head = queue.head, tail = queue.tail + 1 }
  }
  
  let dequeue = fn(queue) {
    // Simulate thread-safe dequeue
    if queue.head < queue.tail {
      let item = queue.items[queue.head]
      { queue = { items = queue.items, head = queue.head + 1, tail = queue.tail }, item = Some(item) }
    } else {
      { queue = queue, item = None }
    }
  }
  
  let is_empty = fn(queue) {
    queue.head >= queue.tail
  }
  
  // Test queue operations
  let queue = create_queue()
  assert_true(is_empty(queue))
  
  let queue1 = enqueue(queue, "item1")
  let queue2 = enqueue(queue1, "item2")
  let queue3 = enqueue(queue2, "item3")
  
  assert_false(is_empty(queue3))
  
  let dequeue_result1 = dequeue(queue3)
  match dequeue_result1.item {
    None => assert_true(false)
    Some(item) => assert_eq(item, "item1")
  }
  assert_false(is_empty(dequeue_result1.queue))
  
  let dequeue_result2 = dequeue(dequeue_result1.queue)
  match dequeue_result2.item {
    None => assert_true(false)
    Some(item) => assert_eq(item, "item2")
  }
  assert_false(is_empty(dequeue_result2.queue))
  
  let dequeue_result3 = dequeue(dequeue_result2.queue)
  match dequeue_result3.item {
    None => assert_true(false)
    Some(item) => assert_eq(item, "item3")
  }
  assert_true(is_empty(dequeue_result3.queue))
  
  let dequeue_result4 = dequeue(dequeue_result3.queue)
  match dequeue_result4.item {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

// Test 6: Concurrent Resource Pool
test "concurrent resource pool" {
  // Simulate thread-safe resource pool
  let create_concurrent_pool = fn(resources) {
    { available = resources, in_use = [] }
  }
  
  let acquire_resource = fn(pool) {
    // Simulate thread-safe resource acquisition
    if pool.available.length() > 0 {
      let resource = pool.available[0]
      let remaining = pool.available.slice(1)
      { 
        pool = { 
          available = remaining, 
          in_use = pool.in_use.push(resource) 
        }, 
        resource = Some(resource) 
      }
    } else {
      { pool = pool, resource = None }
    }
  }
  
  let release_resource = fn(pool, resource) {
    // Simulate thread-safe resource release
    let new_in_use = []
    for item in pool.in_use {
      if item != resource {
        new_in_use = new_in_use.push(item)
      }
    }
    { 
      available = pool.available.push(resource), 
      in_use = new_in_use 
    }
  }
  
  // Test pool operations
  let initial_resources = ["resource1", "resource2", "resource3"]
  let pool = create_concurrent_pool(initial_resources)
  
  assert_eq(pool.available.length(), 3)
  assert_eq(pool.in_use.length(), 0)
  
  // Acquire resources
  let acquire_result1 = acquire_resource(pool)
  match acquire_result1.resource {
    None => assert_true(false)
    Some(resource) => assert_eq(resource, "resource1")
  }
  assert_eq(acquire_result1.pool.available.length(), 2)
  assert_eq(acquire_result1.pool.in_use.length(), 1)
  
  let acquire_result2 = acquire_resource(acquire_result1.pool)
  match acquire_result2.resource {
    None => assert_true(false)
    Some(resource) => assert_eq(resource, "resource2")
  }
  assert_eq(acquire_result2.pool.available.length(), 1)
  assert_eq(acquire_result2.pool.in_use.length(), 2)
  
  // Release resources
  let released_pool1 = release_resource(acquire_result2.pool, "resource1")
  assert_eq(released_pool1.available.length(), 2)
  assert_eq(released_pool1.in_use.length(), 1)
  
  let released_pool2 = release_resource(released_pool1, "resource2")
  assert_eq(released_pool2.available.length(), 3)
  assert_eq(released_pool2.in_use.length(), 0)
  
  // Test resource exhaustion
  let acquire_result3 = acquire_resource(released_pool2)
  let acquire_result4 = acquire_resource(acquire_result3.pool)
  let acquire_result5 = acquire_resource(acquire_result4.pool)
  let acquire_result6 = acquire_resource(acquire_result5.pool)
  
  match acquire_result6.resource {
    None => assert_true(true)  // Should be exhausted
    Some(_) => assert_true(false)
  }
  assert_eq(acquire_result6.pool.available.length(), 0)
  assert_eq(acquire_result6.pool.in_use.length(), 3)
}

// Test 7: Concurrent Metrics Collection
test "concurrent metrics collection" {
  // Simulate thread-safe metrics collection
  let create_metrics_collector = fn() {
    { counters = {}, gauges = {}, histograms = {} }
  }
  
  let record_counter = fn(collector, name, value) {
    // Simulate thread-safe counter recording
    let current = match collector.counters.get(name) {
      None => 0
      Some(val) => val
    }
    let new_counters = collector.counters.with(name, current + value)
    { counters = new_counters, gauges = collector.gauges, histograms = collector.histograms }
  }
  
  let set_gauge = fn(collector, name, value) {
    // Simulate thread-safe gauge setting
    let new_gauges = collector.gauges.with(name, value)
    { counters = collector.counters, gauges = new_gauges, histograms = collector.histograms }
  }
  
  let get_counter = fn(collector, name) {
    // Simulate thread-safe counter reading
    match collector.counters.get(name) {
      None => 0
      Some(val) => val
    }
  }
  
  let get_gauge = fn(collector, name) {
    // Simulate thread-safe gauge reading
    match collector.gauges.get(name) {
      None => 0.0
      Some(val) => val
    }
  }
  
  // Test metrics collection
  let collector = create_metrics_collector()
  
  let collector1 = record_counter(collector, "http.requests", 1)
  let collector2 = record_counter(collector1, "http.requests", 1)
  let collector3 = record_counter(collector2, "http.requests", 1)
  
  assert_eq(get_counter(collector3, "http.requests"), 3)
  assert_eq(get_counter(collector3, "http.errors"), 0)
  
  let collector4 = set_gauge(collector3, "memory.usage", 1024.0)
  let collector5 = set_gauge(collector4, "memory.usage", 2048.0)
  
  assert_eq(get_gauge(collector5, "memory.usage"), 2048.0)
  assert_eq(get_gauge(collector5, "cpu.usage"), 0.0)
  
  // Test concurrent operations simulation
  let simulate_concurrent_counter_updates = fn(initial_collector, metric_name, num_threads, increments_per_thread) {
    // Simulate concurrent counter updates
    let mut result_collector = initial_collector
    for i in 0..<num_threads {
      for j in 0..<increments_per_thread {
        result_collector = record_counter(result_collector, metric_name, 1)
      }
    }
    result_collector
  }
  
  let concurrent_collector = simulate_concurrent_counter_updates(collector5, "concurrent.counter", 5, 10)
  assert_eq(get_counter(concurrent_collector, "concurrent.counter"), 50)
}

// Test 8: Concurrent Span Operations
test "concurrent span operations" {
  // Simulate thread-safe span operations
  let create_span_manager = fn() {
    { active_spans = {}, next_span_id = 1 }
  }
  
  let create_span = fn(manager, name, trace_id) {
    // Simulate thread-safe span creation
    let span_id = "span-" + manager.next_span_id.to_string()
    let span = Span({
      name = name,
      kind = Server,
      recording = true,
      span_context = SpanContext({
        trace_id = trace_id,
        span_id = span_id,
        sampled = true,
        trace_state = ""
      })
    })
    let new_active_spans = manager.active_spans.with(span_id, span)
    { 
      active_spans = new_active_spans, 
      next_span_id = manager.next_span_id + 1 
    }
  }
  
  let finish_span = fn(manager, span_id) {
    // Simulate thread-safe span finishing
    let new_active_spans = {}
    for (id, span) in manager.active_spans {
      if id != span_id {
        new_active_spans = new_active_spans.with(id, span)
      }
    }
    { active_spans = new_active_spans, next_span_id = manager.next_span_id }
  }
  
  let get_active_span = fn(manager, span_id) {
    // Simulate thread-safe span retrieval
    match manager.active_spans.get(span_id) {
      None => None
      Some(span) => Some(span)
    }
  }
  
  let get_active_span_count = fn(manager) {
    manager.active_spans.length()
  }
  
  // Test span operations
  let manager = create_span_manager()
  assert_eq(get_active_span_count(manager), 0)
  
  let manager1 = create_span(manager, "span1", "trace-123")
  assert_eq(get_active_span_count(manager1), 1)
  
  let manager2 = create_span(manager1, "span2", "trace-123")
  assert_eq(get_active_span_count(manager2), 2)
  
  let manager3 = create_span(manager2, "span3", "trace-456")
  assert_eq(get_active_span_count(manager3), 3)
  
  match get_active_span(manager3, "span-1") {
    None => assert_true(false)
    Some(span) => assert_eq(span.name, "span1")
  }
  
  match get_active_span(manager3, "span-3") {
    None => assert_true(false)
    Some(span) => assert_eq(span.name, "span3")
  }
  
  match get_active_span(manager3, "span-99") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  let manager4 = finish_span(manager3, "span-2")
  assert_eq(get_active_span_count(manager4), 2)
  
  match get_active_span(manager4, "span-2") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test concurrent span creation simulation
  let simulate_concurrent_span_creation = fn(initial_manager, trace_id, num_threads, spans_per_thread) {
    // Simulate concurrent span creation
    let mut result_manager = initial_manager
    for i in 0..<num_threads {
      for j in 0..<spans_per_thread {
        let span_name = "concurrent-span-" + i.to_string() + "-" + j.to_string()
        result_manager = create_span(result_manager, span_name, trace_id)
      }
    }
    result_manager
  }
  
  let concurrent_manager = simulate_concurrent_span_creation(manager4, "concurrent-trace", 3, 5)
  assert_eq(get_active_span_count(concurrent_manager), 17)  // 2 remaining + 15 new
}

// Test 9: Concurrent Context Propagation
test "concurrent context propagation" {
  // Simulate thread-safe context propagation
  let create_context = fn(trace_id, baggage) {
    { trace_id = trace_id, baggage = baggage }
  }
  
  let add_baggage_item = fn(context, key, value) {
    // Simulate thread-safe baggage addition
    let new_baggage = context.baggage.with(key, value)
    { trace_id = context.trace_id, baggage = new_baggage }
  }
  
  let get_baggage_item = fn(context, key) {
    // Simulate thread-safe baggage retrieval
    match context.baggage.get(key) {
      None => None
      Some(value) => Some(value)
    }
  }
  
  let copy_context = fn(context) {
    // Simulate thread-safe context copying
    { trace_id = context.trace_id, baggage = context.baggage }
  }
  
  // Test context operations
  let initial_baggage = {}
  let context = create_context("trace-123", initial_baggage)
  
  let context1 = add_baggage_item(context, "user.id", "12345")
  let context2 = add_baggage_item(context1, "request.id", "req-67890")
  let context3 = add_baggage_item(context2, "session.id", "sess-abc")
  
  match get_baggage_item(context3, "user.id") {
    None => assert_true(false)
    Some(value) => assert_eq(value, "12345")
  }
  
  match get_baggage_item(context3, "request.id") {
    None => assert_true(false)
    Some(value) => assert_eq(value, "req-67890")
  }
  
  match get_baggage_item(context3, "session.id") {
    None => assert_true(false)
    Some(value) => assert_eq(value, "sess-abc")
  }
  
  match get_baggage_item(context3, "nonexistent") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test context isolation
  let context_copy = copy_context(context3)
  let context4 = add_baggage_item(context_copy, "new.item", "new.value")
  
  // Original context should not have the new item
  match get_baggage_item(context3, "new.item") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Copied context should have the new item
  match get_baggage_item(context4, "new.item") {
    None => assert_true(false)
    Some(value) => assert_eq(value, "new.value")
  }
  
  // Test concurrent context modification simulation
  let simulate_concurrent_context_modification = fn(initial_context, num_threads, items_per_thread) {
    // Simulate concurrent context modifications
    let mut result_context = initial_context
    for i in 0..<num_threads {
      for j in 0..<items_per_thread {
        let key = "thread-" + i.to_string() + "-item-" + j.to_string()
        let value = "value-" + i.to_string() + "-" + j.to_string()
        result_context = add_baggage_item(result_context, key, value)
      }
    }
    result_context
  }
  
  let concurrent_context = simulate_concurrent_context_modification(context3, 3, 4)
  
  // Check that original items are still present
  match get_baggage_item(concurrent_context, "user.id") {
    None => assert_true(false)
    Some(value) => assert_eq(value, "12345")
  }
  
  // Check that new items are present
  match get_baggage_item(concurrent_context, "thread-1-item-2") {
    None => assert_true(false)
    Some(value) => assert_eq(value, "value-1-2")
  }
}

// Test 10: Race Condition Prevention
test "race condition prevention" {
  // Simulate race condition scenarios and prevention
  
  // Test 1: Check-then-act race condition
  let shared_resource = { value = 0, in_use = false }
  
  let unsafe_increment = fn(resource) {
    // Unsafe: check-then-act
    if !resource.in_use {
      let updated_resource = { value = resource.value + 1, in_use = true }
      // Simulate some work
      updated_resource
    } else {
      resource
    }
  }
  
  let safe_increment = fn(resource) {
    // Safe: atomic operation
    { value = resource.value + 1, in_use = resource.in_use }
  }
  
  // Test safe operation
  let resource1 = safe_increment(shared_resource)
  assert_eq(resource1.value, 1)
  
  let resource2 = safe_increment(resource1)
  assert_eq(resource2.value, 2)
  
  // Test 2: Lost update race condition
  let counter = { value = 0 }
  
  let unsafe_update = fn(counter, increment) {
    // Unsafe: read-modify-write
    let current = counter.value
    let new_value = current + increment
    { value = new_value }
  }
  
  let safe_update = fn(counter, increment) {
    // Safe: atomic update
    { value = counter.value + increment }
  }
  
  // Test safe update
  let counter1 = safe_update(counter, 5)
  assert_eq(counter1.value, 5)
  
  let counter2 = safe_update(counter1, 3)
  assert_eq(counter2.value, 8)
  
  // Test 3: ABA problem prevention
  let node = { value = "A", next = None }
  
  let unsafe_compare_and_swap = fn(node, expected, new_value) {
    // Unsafe: vulnerable to ABA problem
    if node.value == expected {
      { value = new_value, next = node.next }
    } else {
      node
    }
  }
  
  let safe_compare_and_swap = fn(node, expected, new_value, version) {
    // Safe: includes version check
    if node.value == expected && version == 1 {
      { value = new_value, next = node.next, version = version + 1 }
    } else {
      { value = node.value, next = node.next, version = version }
    }
  }
  
  // Test safe CAS
  let node1 = { value = "A", next = None, version = 1 }
  let node2 = safe_compare_and_swap(node1, "A", "B", 1)
  assert_eq(node2.value, "B")
  assert_eq(node2.version, 2)
  
  let node3 = safe_compare_and_swap(node2, "A", "C", 1)  // Should fail
  assert_eq(node3.value, "B")  // Should remain unchanged
  assert_eq(node3.version, 2)  // Should remain unchanged
  
  // Test 4: Deadlock prevention
  let create_ordered_locks = fn() {
    { lock1 = { id = 1, locked = false }, lock2 = { id = 2, locked = false } }
  }
  
  let acquire_ordered = fn(locks, lock_id) {
    // Always acquire locks in the same order to prevent deadlock
    if lock_id == 1 && !locks.lock1.locked {
      { lock1 = { id = 1, locked = true }, lock2 = locks.lock2 }
    } else if lock_id == 2 && !locks.lock1.locked && !locks.lock2.locked {
      { lock1 = locks.lock1, lock2 = { id = 2, locked = true } }
    } else {
      locks
    }
  }
  
  let release_ordered = fn(locks, lock_id) {
    if lock_id == 1 {
      { lock1 = { id = 1, locked = false }, lock2 = locks.lock2 }
    } else if lock_id == 2 {
      { lock1 = locks.lock1, lock2 = { id = 2, locked = false } }
    } else {
      locks
    }
  }
  
  // Test ordered lock acquisition
  let locks = create_ordered_locks()
  let locks1 = acquire_ordered(locks, 1)
  assert_true(locks1.lock1.locked)
  assert_false(locks1.lock2.locked)
  
  let locks2 = acquire_ordered(locks1, 2)
  assert_true(locks2.lock1.locked)
  assert_true(locks2.lock2.locked)
  
  let locks3 = release_ordered(locks2, 1)
  assert_false(locks3.lock1.locked)
  assert_true(locks3.lock2.locked)
  
  let locks4 = release_ordered(locks3, 2)
  assert_false(locks4.lock1.locked)
  assert_false(locks4.lock2.locked)
}