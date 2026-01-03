// Azimuth Async and Concurrent Tests
// This file contains tests for asynchronous and concurrent programming patterns

// Test 1: Promise/Future Pattern Simulation
test "promise/future pattern simulation" {
  // Simulate promise/future with callbacks
  type Promise[T] = {
    value : Option[T]
    callbacks : [(T) -> ()]
    is_resolved : Bool
  }
  
  let create_promise = fn() {
    Promise {
      value: None,
      callbacks: [],
      is_resolved: false
    }
  }
  
  let resolve_promise = fn[T](promise : Promise[T], value : T) {
    if !promise.is_resolved {
      let resolved_promise = { promise | value: Some(value), is_resolved: true }
      for callback in resolved_promise.callbacks {
        callback(value)
      }
      resolved_promise
    } else {
      promise
    }
  }
  
  let then_promise = fn[T, U](promise : Promise[T], callback : (T) -> U) {
    let mut result = None
    let new_callback = fn(value : T) {
      result = Some(callback(value))
    }
    
    let updated_promise = if promise.is_resolved {
      match promise.value {
        Some(v) => {
          new_callback(v)
          promise
        }
        None => promise
      }
    } else {
      { promise | callbacks: promise.callbacks.push(new_callback) }
    }
    
    (result, updated_promise)
  }
  
  // Test promise creation and resolution
  let promise = create_promise[Int]()
  assert_false(promise.is_resolved)
  
  let resolved = resolve_promise(promise, 42)
  assert_true(resolved.is_resolved)
  match resolved.value {
    Some(v) => assert_eq(v, 42),
    None => assert_true(false)
  }
  
  // Test promise chaining
  let promise2 = create_promise[String]()
  let (result1, promise_with_callback) = then_promise(promise2, fn(s) { s.length() })
  
  // Resolve the promise
  let resolved2 = resolve_promise(promise_with_callback, "hello")
  match result1 {
    Some(length) => assert_eq(length, 5),
    None => assert_true(false)
  }
}

// Test 2: Async/Await Pattern Simulation
test "async/await pattern simulation" {
  // Simulate async operations with state machines
  type AsyncState[T] = {
    is_completed : Bool
    result : Option[T]
    error : Option[String]
  }
  
  type AsyncOperation[T] = () -> AsyncState[T]
  
  let async_return = fn[T](value : T) {
    fn() {
      AsyncState {
        is_completed: true,
        result: Some(value),
        error: None
      }
    }
  }
  
  let async_delay = fn[T](value : T, delay_ms : Int) {
    let mut counter = 0
    fn() {
      counter = counter + 1
      if counter >= delay_ms {
        AsyncState {
          is_completed: true,
          result: Some(value),
          error: None
        }
      } else {
        AsyncState {
          is_completed: false,
          result: None,
          error: None
        }
      }
    }
  }
  
  let async_error = fn[T](error_message : String) {
    fn() {
      AsyncState {
        is_completed: true,
        result: None,
        error: Some(error_message)
      }
    }
  }
  
  // Simulate await by polling
  let await_async = fn[T](operation : AsyncOperation[T]) {
    let mut state = operation()
    while !state.is_completed {
      state = operation()
    }
    state
  }
  
  // Test successful async operation
  let success_op = async_return("success")
  let success_result = await_async(success_op)
  assert_true(success_result.is_completed)
  match success_result.result {
    Some(value) => assert_eq(value, "success"),
    None => assert_true(false)
  }
  
  // Test delayed async operation
  let delay_op = async_delay("delayed", 3)
  let delay_result = await_async(delay_op)
  assert_true(delay_result.is_completed)
  match delay_result.result {
    Some(value) => assert_eq(value, "delayed"),
    None => assert_true(false)
  }
  
  // Test error async operation
  let error_op = async_error[String]("something went wrong")
  let error_result = await_async(error_op)
  assert_true(error_result.is_completed)
  match error_result.error {
    Some(message) => assert_eq(message, "something went wrong"),
    None => assert_true(false)
  }
}

// Test 3: Concurrent Task Management
test "concurrent task management" {
  type Task[T] = {
    id : Int
    status : String  // "pending", "running", "completed", "failed"
    result : Option[T]
    error : Option[String]
  }
  
  type TaskManager[T] = {
    tasks : [Task[T]]
    next_id : Int
  }
  
  let create_task_manager = fn() {
    TaskManager {
      tasks: [],
      next_id: 0
    }
  }
  
  let submit_task = fn[T](manager : TaskManager[T], operation : () -> T) {
    let task_id = manager.next_id
    let new_task = Task {
      id: task_id,
      status: "pending",
      result: None,
      error: None
    }
    
    let updated_manager = {
      manager |
      tasks: manager.tasks.push(new_task),
      next_id: manager.next_id + 1
    }
    
    // Simulate task execution
    let updated_tasks = updated_manager.tasks.map(fn(task) {
      if task.id == task_id {
        match operation() {
          Ok(value) => { task | status: "completed", result: Some(value) },
          Err(error) => { task | status: "failed", error: Some(error) }
        }
      } else {
        task
      }
    })
    
    { updated_manager | tasks: updated_tasks }
  }
  
  let get_task_status = fn[T](manager : TaskManager[T], task_id : Int) {
    let mut status = "not_found"
    for task in manager.tasks {
      if task.id == task_id {
        status = task.status
        break
      }
    }
    status
  }
  
  let get_task_result = fn[T](manager : TaskManager[T], task_id : Int) {
    let mut result = None
    for task in manager.tasks {
      if task.id == task_id {
        result = task.result
        break
      }
    }
    result
  }
  
  // Test task management
  let manager = create_task_manager[Int]()
  
  // Submit tasks
  let manager1 = submit_task(manager, fn() { Ok(42) })
  let manager2 = submit_task(manager1, fn() { Ok(84) })
  let manager3 = submit_task(manager2, fn() { Err("task failed") })
  
  // Check task statuses
  assert_eq(get_task_status(manager3, 0), "completed")
  assert_eq(get_task_status(manager3, 1), "completed")
  assert_eq(get_task_status(manager3, 2), "failed")
  
  // Check task results
  match get_task_result(manager3, 0) {
    Some(value) => assert_eq(value, 42),
    None => assert_true(false)
  }
  
  match get_task_result(manager3, 1) {
    Some(value) => assert_eq(value, 84),
    None => assert_true(false)
  }
  
  match get_task_result(manager3, 2) {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
}

// Test 4: Producer-Consumer Pattern
test "producer-consumer pattern" {
  type Message = {
    content : String
    timestamp : Int
  }
  
  type Queue[T] = {
    items : [T]
    capacity : Int
  }
  
  let create_queue = fn(capacity : Int) {
    Queue { items: [], capacity: capacity }
  }
  
  let enqueue = fn[T](queue : Queue[T], item : T) {
    if queue.items.length() < queue.capacity {
      { queue | items: queue.items.push(item) }
    } else {
      queue // Queue full, drop item
    }
  }
  
  let dequeue = fn[T](queue : Queue[T]) {
    if queue.items.length() > 0 {
      let item = queue.items[0]
      let remaining = queue.items.slice(1, queue.items.length())
      (Some(item), { queue | items: remaining })
    } else {
      (None, queue)
    }
  }
  
  let is_empty = fn[T](queue : Queue[T]) {
    queue.items.length() == 0
  }
  
  let is_full = fn[T](queue : Queue[T]) {
    queue.items.length() >= queue.capacity
  }
  
  // Test queue operations
  let queue = create_queue<Message>(3)
  assert_true(is_empty(queue))
  assert_false(is_full(queue))
  
  let message1 = Message { content: "hello", timestamp: 1000 }
  let message2 = Message { content: "world", timestamp: 1001 }
  let message3 = Message { content: "test", timestamp: 1002 }
  let message4 = Message { content: "overflow", timestamp: 1003 }
  
  // Enqueue messages
  let queue1 = enqueue(queue, message1)
  let queue2 = enqueue(queue1, message2)
  let queue3 = enqueue(queue2, message3)
  
  assert_false(is_empty(queue3))
  assert_true(is_full(queue3))
  
  // Try to enqueue when full
  let queue4 = enqueue(queue3, message4)
  assert_eq(queue4.items.length(), 3) // Should still be 3
  
  // Dequeue messages
  let (msg1, queue5) = dequeue(queue4)
  match msg1 {
    Some(m) => assert_eq(m.content, "hello"),
    None => assert_true(false)
  }
  
  let (msg2, queue6) = dequeue(queue5)
  match msg2 {
    Some(m) => assert_eq(m.content, "world"),
    None => assert_true(false)
  }
  
  let (msg3, queue7) = dequeue(queue6)
  match msg3 {
    Some(m) => assert_eq(m.content, "test"),
    None => assert_true(false)
  }
  
  // Queue should be empty now
  assert_true(is_empty(queue7))
  
  // Try to dequeue from empty queue
  let (empty_msg, _) = dequeue(queue7)
  match empty_msg {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
}

// Test 5: Mutex and Lock Simulation
test "mutex and lock simulation" {
  type Mutex[T] = {
    value : T
    is_locked : Bool
    owner : Option[Int]
  }
  
  let create_mutex = fn[T](initial_value : T) {
    Mutex {
      value: initial_value,
      is_locked: false,
      owner: None
    }
  }
  
  let acquire_lock = fn[T](mutex : Mutex[T], thread_id : Int) {
    if !mutex.is_locked {
      Some({ mutex | is_locked: true, owner: Some(thread_id) })
    } else {
      None
    }
  }
  
  let release_lock = fn[T](mutex : Mutex[T], thread_id : Int) {
    match mutex.owner {
      Some(owner_id) => {
        if owner_id == thread_id {
          { mutex | is_locked: false, owner: None }
        } else {
          mutex // Not the owner
        }
      }
      None => mutex
    }
  }
  
  let with_lock = fn[T, U](mutex : Mutex[T], thread_id : Int, operation : (T) -> U) {
    match acquire_lock(mutex, thread_id) {
      Some(locked_mutex) => {
        let result = operation(locked_mutex.value)
        let unlocked_mutex = release_lock(locked_mutex, thread_id)
        (result, unlocked_mutex)
      }
      None => {
        // Lock acquisition failed
        ("lock_failed", mutex)
      }
    }
  }
  
  // Test mutex operations
  let mutex = create_mutex[Int](0)
  
  // Thread 1 acquires lock
  match acquire_lock(mutex, 1) {
    Some(locked) => {
      assert_true(locked.is_locked)
      match locked.owner {
        Some(owner) => assert_eq(owner, 1),
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Thread 2 tries to acquire lock
  let locked_mutex = { mutex | is_locked: true, owner: Some(1) }
  match acquire_lock(locked_mutex, 2) {
    Some(_) => assert_true(false), // Should not succeed
    None => assert_true(true)      // Should fail
  }
  
  // Thread 1 releases lock
  let unlocked_mutex = release_lock(locked_mutex, 1)
  assert_false(unlocked_mutex.is_locked)
  match unlocked_mutex.owner {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  // Thread 2 can now acquire lock
  match acquire_lock(unlocked_mutex, 2) {
    Some(locked_by_2) => {
      assert_true(locked_by_2.is_locked)
      match locked_by_2.owner {
        Some(owner) => assert_eq(owner, 2),
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test with_lock operation
  let mutex2 = create_mutex[String]("initial")
  let (result, final_mutex) = with_lock(mutex2, 1, fn(value) { value.length() })
  
  assert_eq(result, 8) // "initial" has 8 characters
  assert_false(final_mutex.is_locked)
  match final_mutex.owner {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
}

// Test 6: Thread Pool Simulation
test "thread pool simulation" {
  type Worker = {
    id : Int
    is_busy : Bool
    current_task : Option[Int]
  }
  
  type ThreadPool = {
    workers : [Worker]
    task_queue : [Int]
    next_task_id : Int
  }
  
  let create_thread_pool = fn(worker_count : Int) {
    let mut workers = []
    for i in 0..<worker_count {
      workers = workers.push(Worker {
        id: i,
        is_busy: false,
        current_task: None
      })
    }
    
    ThreadPool {
      workers: workers,
      task_queue: [],
      next_task_id: 0
    }
  }
  
  let submit_task_to_pool = fn(pool : ThreadPool, task_data : Int) {
    { pool | task_queue: pool.task_queue.push(pool.next_task_id) }
  }
  
  let assign_tasks = fn(pool : ThreadPool) {
    let mut updated_workers = pool.workers
    let mut remaining_tasks = pool.task_queue
    
    for i in 0..<updated_workers.length() {
      if !updated_workers[i].is_busy && remaining_tasks.length() > 0 {
        let task_id = remaining_tasks[0]
        remaining_tasks = remaining_tasks.slice(1, remaining_tasks.length())
        updated_workers[i] = {
          updated_workers[i] |
          is_busy: true,
          current_task: Some(task_id)
        }
      }
    }
    
    { pool | workers: updated_workers, task_queue: remaining_tasks }
  }
  
  let complete_task = fn(pool : ThreadPool, worker_id : Int) {
    let mut updated_workers = pool.workers
    if worker_id < updated_workers.length() {
      updated_workers[worker_id] = {
        updated_workers[worker_id] |
        is_busy: false,
        current_task: None
      }
    }
    { pool | workers: updated_workers }
  }
  
  let get_available_workers = fn(pool : ThreadPool) {
    let mut count = 0
    for worker in pool.workers {
      if !worker.is_busy {
        count = count + 1
      }
    }
    count
  }
  
  // Test thread pool operations
  let pool = create_thread_pool(3)
  assert_eq(pool.workers.length(), 3)
  assert_eq(get_available_workers(pool), 3)
  
  // Submit tasks
  let pool1 = submit_task_to_pool(pool, 100)
  let pool2 = submit_task_to_pool(pool1, 200)
  let pool3 = submit_task_to_pool(pool2, 300)
  let pool4 = submit_task_to_pool(pool3, 400) // Extra task
  
  assert_eq(pool4.task_queue.length(), 4)
  assert_eq(get_available_workers(pool4), 3)
  
  // Assign tasks to workers
  let pool_with_tasks = assign_tasks(pool4)
  assert_eq(pool_with_tasks.task_queue.length(), 1) // One task remains in queue
  assert_eq(get_available_workers(pool_with_tasks), 0) // All workers busy
  
  // Verify workers have tasks
  for worker in pool_with_tasks.workers {
    assert_true(worker.is_busy)
    match worker.current_task {
      Some(_) => assert_true(true),
      None => assert_true(false)
    }
  }
  
  // Complete a task
  let pool_after_complete = complete_task(pool_with_tasks, 0)
  assert_eq(get_available_workers(pool_after_complete), 1)
  assert_false(pool_after_complete.workers[0].is_busy)
  
  // Assign remaining task
  let final_pool = assign_tasks(pool_after_complete)
  assert_eq(final_pool.task_queue.length(), 0) // All tasks assigned
  assert_eq(get_available_workers(final_pool), 0) // All workers busy again
}

// Test 7: Async Channel Communication
test "async channel communication" {
  type Channel[T] = {
    buffer : [T]
    capacity : Int
    senders : Int
    receivers : Int
    is_closed : Bool
  }
  
  let create_channel = fn[T](capacity : Int) {
    Channel {
      buffer: [],
      capacity: capacity,
      senders: 0,
      receivers: 0,
      is_closed: false
    }
  }
  
  let channel_send = fn[T](channel : Channel[T], value : T) {
    if !channel.is_closed && channel.buffer.length() < channel.capacity {
      { channel | buffer: channel.buffer.push(value) }
    } else {
      channel // Send failed
    }
  }
  
  let channel_receive = fn[T](channel : Channel[T]) {
    if channel.buffer.length() > 0 {
      let value = channel.buffer[0]
      let remaining = channel.buffer.slice(1, channel.buffer.length())
      (Some(value), { channel | buffer: remaining })
    } else {
      (None, channel)
    }
  }
  
  let close_channel = fn[T](channel : Channel[T]) {
    { channel | is_closed: true }
  }
  
  // Test channel operations
  let channel = create_channel[String](2)
  assert_false(channel.is_closed)
  assert_eq(channel.buffer.length(), 0)
  
  // Send messages
  let channel1 = channel_send(channel, "message1")
  let channel2 = channel_send(channel1, "message2")
  let channel3 = channel_send(channel2, "message3") // Should fail (buffer full)
  
  assert_eq(channel2.buffer.length(), 2)
  assert_eq(channel3.buffer.length(), 2) // Still 2, third message dropped
  
  // Receive messages
  let (msg1, channel4) = channel_receive(channel3)
  match msg1 {
    Some(m) => assert_eq(m, "message1"),
    None => assert_true(false)
  }
  
  let (msg2, channel5) = channel_receive(channel4)
  match msg2 {
    Some(m) => assert_eq(m, "message2"),
    None => assert_true(false)
  }
  
  // Try to receive from empty channel
  let (empty_msg, _) = channel_receive(channel5)
  match empty_msg {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  // Test closed channel
  let closed_channel = close_channel(channel5)
  assert_true(closed_channel.is_closed)
  
  let closed_channel_after_send = channel_send(closed_channel, "should_fail")
  assert_eq(closed_channel_after_send.buffer.length(), 0) // Send failed
}

// Test 8: Race Condition Detection
test "race condition detection patterns" {
  type SharedCounter = {
    value : Int
    operations : [(Int, String)] // (thread_id, operation)
  }
  
  let create_counter = fn(initial_value : Int) {
    SharedCounter {
      value: initial_value,
      operations: []
    }
  }
  
  let atomic_increment = fn(counter : SharedCounter, thread_id : Int) {
    let new_value = counter.value + 1
    let new_operation = (thread_id, "increment")
    {
      counter |
      value: new_value,
      operations: counter.operations.push(new_operation)
    }
  }
  
  let atomic_decrement = fn(counter : SharedCounter, thread_id : Int) {
    let new_value = counter.value - 1
    let new_operation = (thread_id, "decrement")
    {
      counter |
      value: new_value,
      operations: counter.operations.push(new_operation)
    }
  }
  
  let detect_race_condition = fn(counter : SharedCounter) {
    let mut thread_values = {}
    let mut race_detected = false
    
    for operation in counter.operations {
      let (thread_id, op_type) = operation
      
      // Track operations per thread
      let current_ops = match thread_values.get(thread_id) {
        Some(ops) => ops,
        None => []
      }
      thread_values = thread_values.set(thread_id, current_ops.push(op_type))
    }
    
    // Check for interleaved operations (simplified race detection)
    if counter.operations.length() > 1 {
      let first_op = counter.operations[0]
      let second_op = counter.operations[1]
      
      if first_op.0 != second_op.0 {
        race_detected = true
      }
    }
    
    race_detected
  }
  
  // Test race condition detection
  let counter = create_counter(0)
  
  // Simulate concurrent operations
  let counter1 = atomic_increment(counter, 1)
  let counter2 = atomic_increment(counter1, 2)
  let counter3 = atomic_decrement(counter2, 1)
  let counter4 = atomic_increment(counter3, 2)
  
  assert_eq(counter4.value, 2) // 0 + 1 + 1 - 1 + 1 = 2
  assert_eq(counter4.operations.length(), 4)
  
  // Detect race condition
  let has_race = detect_race_condition(counter4)
  assert_true(has_race) // Different threads operated on shared data
  
  // Test single-threaded operations (no race)
  let single_counter = create_counter(10)
  let single1 = atomic_increment(single_counter, 1)
  let single2 = atomic_increment(single1, 1)
  let single3 = atomic_decrement(single2, 1)
  
  let single_race = detect_race_condition(single3)
  assert_false(single_race) // Same thread, no race condition
}

// Test 9: Deadlock Detection
test "deadlock detection patterns" {
  type Lock = {
    id : Int
    owner : Option[Int]
    waiting_queue : [Int]
  }
  
  type DeadlockDetector = {
    locks : [Lock]
    lock_graph : [(Int, Int)] // (thread, lock) ownership
    wait_graph : [(Int, Int)] // (thread, waiting_for_thread)
  }
  
  let create_detector = fn(lock_count : Int) {
    let mut locks = []
    for i in 0..<lock_count {
      locks = locks.push(Lock {
        id: i,
        owner: None,
        waiting_queue: []
      })
    }
    
    DeadlockDetector {
      locks: locks,
      lock_graph: [],
      wait_graph: []
    }
  }
  
  let acquire_lock = fn(detector : DeadlockDetector, thread_id : Int, lock_id : Int) {
    let mut updated_detector = detector
    
    // Find the lock
    for i in 0..<updated_detector.locks.length() {
      if updated_detector.locks[i].id == lock_id {
        let lock = updated_detector.locks[i]
        
        match lock.owner {
          None => {
            // Lock is available
            updated_detector.locks[i] = { lock | owner: Some(thread_id) }
            updated_detector.lock_graph = updated_detector.lock_graph.push((thread_id, lock_id))
          }
          Some(owner_id) => {
            // Lock is owned, add to waiting queue
            if owner_id != thread_id {
              updated_detector.locks[i] = {
                lock |
                waiting_queue: lock.waiting_queue.push(thread_id)
              }
              updated_detector.wait_graph = updated_detector.wait_graph.push((thread_id, owner_id))
            }
          }
        }
        break
      }
    }
    
    updated_detector
  }
  
  let release_lock = fn(detector : DeadlockDetector, thread_id : Int, lock_id : Int) {
    let mut updated_detector = detector
    
    // Find and update the lock
    for i in 0..<updated_detector.locks.length() {
      if updated_detector.locks[i].id == lock_id {
        let lock = updated_detector.locks[i]
        
        match lock.owner {
          Some(owner_id) => {
            if owner_id == thread_id {
              // Release the lock
              updated_detector.locks[i] = { lock | owner: None }
              
              // Remove from lock graph
              let mut new_lock_graph = []
              for (t, l) in updated_detector.lock_graph {
                if !(t == thread_id && l == lock_id) {
                  new_lock_graph = new_lock_graph.push((t, l))
                }
              }
              updated_detector.lock_graph = new_lock_graph
            }
          }
          None => () // Lock not owned by this thread
        }
        break
      }
    }
    
    updated_detector
  }
  
  let detect_deadlock = fn(detector : DeadlockDetector) {
    // Simple cycle detection in wait graph
    let has_cycle = fn(graph : [(Int, Int)], start : Int, visited : [Int]) {
      if visited.contains(start) {
        return true
      }
      
      let mut new_visited = visited.push(start)
      for (waiter, owner) in graph {
        if waiter == start {
          if has_cycle(graph, owner, new_visited) {
            return true
          }
        }
      }
      false
    }
    
    for (thread, _) in detector.wait_graph {
      if has_cycle(detector.wait_graph, thread, []) {
        return true
      }
    }
    false
  }
  
  // Test deadlock detection
  let detector = create_detector(2)
  
  // Thread 1 acquires lock 0
  let detector1 = acquire_lock(detector, 1, 0)
  
  // Thread 2 acquires lock 1
  let detector2 = acquire_lock(detector1, 2, 1)
  
  // Thread 1 tries to acquire lock 1 (waits for thread 2)
  let detector3 = acquire_lock(detector2, 1, 1)
  
  // Thread 2 tries to acquire lock 0 (waits for thread 1) - DEADLOCK!
  let detector4 = acquire_lock(detector3, 2, 0)
  
  // Detect deadlock
  let has_deadlock = detect_deadlock(detector4)
  assert_true(has_deadlock)
  
  // Test no deadlock scenario
  let detector5 = create_detector(2)
  
  // Thread 1 acquires lock 0
  let detector6 = acquire_lock(detector5, 1, 0)
  
  // Thread 1 acquires lock 1
  let detector7 = acquire_lock(detector6, 1, 1)
  
  // Thread 1 releases lock 0
  let detector8 = release_lock(detector7, 1, 0)
  
  // Thread 2 acquires lock 0
  let detector9 = acquire_lock(detector8, 2, 0)
  
  // No deadlock
  let no_deadlock = detect_deadlock(detector9)
  assert_false(no_deadlock)
}

// Test 10: Async Stream Processing
test "async stream processing" {
  type StreamItem[T] = {
    value : T
    timestamp : Int
    processed : Bool
  }
  
  type AsyncStream[T] = {
    items : [StreamItem[T]]
    position : Int
    is_completed : Bool
  }
  
  let create_stream = fn[T](items : [T]) {
    let stream_items = items.map(fn(item) {
      StreamItem {
        value: item,
        timestamp: 0,
        processed: false
      }
    })
    
    AsyncStream {
      items: stream_items,
      position: 0,
      is_completed: false
    }
  }
  
  let stream_next = fn[T](stream : AsyncStream[T]) {
    if stream.position < stream.items.length() {
      let item = stream.items[stream.position]
      (Some(item), { stream | position: stream.position + 1 })
    } else {
      (None, { stream | is_completed: true })
    }
  }
  
  let stream_map = fn[T, U](stream : AsyncStream[T], transform : (T) -> U) {
    let mapped_items = stream.items.map(fn(item) {
      { item | value: transform(item.value) }
    })
    { stream | items: mapped_items }
  }
  
  let stream_filter = fn[T](stream : AsyncStream[T], predicate : (T) -> Bool) {
    let filtered_items = stream.items.filter(fn(item) {
      predicate(item.value)
    })
    { stream | items: filtered_items }
  }
  
  let stream_collect = fn[T](stream : AsyncStream[T]) {
    stream.items.map(fn(item) { item.value })
  }
  
  // Test stream operations
  let numbers = [1, 2, 3, 4, 5]
  let stream = create_stream(numbers)
  
  assert_eq(stream.items.length(), 5)
  assert_eq(stream.position, 0)
  assert_false(stream.is_completed)
  
  // Stream iteration
  let (item1, stream1) = stream_next(stream)
  match item1 {
    Some(item) => assert_eq(item.value, 1),
    None => assert_true(false)
  }
  
  let (item2, stream2) = stream_next(stream1)
  match item2 {
    Some(item) => assert_eq(item.value, 2),
    None => assert_true(false)
  }
  
  assert_eq(stream2.position, 2)
  
  // Stream map
  let mapped_stream = stream_map(stream, fn(x) { x * 2 })
  let mapped_values = stream_collect(mapped_stream)
  assert_eq(mapped_values, [2, 4, 6, 8, 10])
  
  // Stream filter
  let filtered_stream = stream_filter(stream, fn(x) { x % 2 == 0 })
  let filtered_values = stream_collect(filtered_stream)
  assert_eq(filtered_values, [2, 4])
  
  // Stream to completion
  let mut current_stream = stream2
  let mut collected_values = []
  
  while true {
    let (item, next_stream) = stream_next(current_stream)
    match item {
      Some(stream_item) => {
        collected_values = collected_values.push(stream_item.value)
        current_stream = next_stream
      }
      None => {
        current_stream = next_stream
        break
      }
    }
  }
  
  assert_eq(collected_values, [3, 4, 5])
  assert_true(current_stream.is_completed)
}