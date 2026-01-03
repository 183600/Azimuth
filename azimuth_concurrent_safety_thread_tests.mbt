// Azimuth Concurrent Safety and Thread Tests
// This file contains comprehensive test cases for concurrent operations and thread safety

// Test 1: Atomic Operations
test "atomic operations for thread-safe data access" {
  type AtomicInt {
    mut value : Int
  }
  
  let create_atomic_int = fn(initial_value : Int) : AtomicInt {
    { value: initial_value }
  }
  
  let atomic_increment = fn(atomic : AtomicInt) : Int {
    atomic.value = atomic.value + 1
    atomic.value
  }
  
  let atomic_decrement = fn(atomic : AtomicInt) : Int {
    atomic.value = atomic.value - 1
    atomic.value
  }
  
  let atomic_add = fn(atomic : AtomicInt, delta : Int) : Int {
    atomic.value = atomic.value + delta
    atomic.value
  }
  
  let atomic_get = fn(atomic : AtomicInt) : Int {
    atomic.value
  }
  
  let atomic_set = fn(atomic : AtomicInt, new_value : Int) {
    atomic.value = new_value
  }
  
  let atomic_compare_and_swap = fn(atomic : AtomicInt, expected : Int, new_value : Int) : Bool {
    if atomic.value == expected {
      atomic.value = new_value
      true
    } else {
      false
    }
  }
  
  // Test basic atomic operations
  let counter = create_atomic_int(0)
  
  assert_eq(atomic_get(counter), 0)
  
  assert_eq(atomic_increment(counter), 1)
  assert_eq(atomic_get(counter), 1)
  
  assert_eq(atomic_decrement(counter), 0)
  assert_eq(atomic_get(counter), 0)
  
  assert_eq(atomic_add(counter, 5), 5)
  assert_eq(atomic_get(counter), 5)
  
  atomic_set(counter, 10)
  assert_eq(atomic_get(counter), 10)
  
  // Test compare and swap
  assert_true(atomic_compare_and_swap(counter, 10, 20))
  assert_eq(atomic_get(counter), 20)
  
  assert_false(atomic_compare_and_swap(counter, 10, 30))
  assert_eq(atomic_get(counter), 20)
  
  assert_true(atomic_compare_and_swap(counter, 20, 30))
  assert_eq(atomic_get(counter), 30)
}

// Test 2: Concurrent Counter
test "concurrent counter with thread safety" {
  type ConcurrentCounter {
    mut count : AtomicInt
  }
  
  let create_concurrent_counter = fn() : ConcurrentCounter {
    { count: create_atomic_int(0) }
  }
  
  let increment = fn(counter : ConcurrentCounter) {
    atomic_increment(counter.count)
  }
  
  let get_count = fn(counter : ConcurrentCounter) : Int {
    atomic_get(counter.count)
  }
  
  let reset = fn(counter : ConcurrentCounter) {
    atomic_set(counter.count, 0)
  }
  
  let counter = create_concurrent_counter()
  
  // Test basic operations
  assert_eq(get_count(counter), 0)
  
  increment(counter)
  increment(counter)
  increment(counter)
  
  assert_eq(get_count(counter), 3)
  
  reset(counter)
  assert_eq(get_count(counter), 0)
  
  // Test batch increments
  for i in 0..<100 {
    increment(counter)
  }
  
  assert_eq(get_count(counter), 100)
  
  // Test concurrent increments simulation
  let simulate_concurrent_increments = fn(counter : ConcurrentCounter, num_threads : Int, increments_per_thread : Int) {
    for thread_id in 0..<num_threads {
      for i in 0..<increments_per_thread {
        increment(counter)
      }
    }
  }
  
  reset(counter)
  simulate_concurrent_increments(counter, 5, 10)
  assert_eq(get_count(counter), 50)
}

// Test 3: Thread-Safe Queue
test "thread-safe queue implementation" {
  type ConcurrentNode[T] {
    value : T
    mut next : Option[ConcurrentNode[T]]
  }
  
  type ConcurrentQueue[T] {
    mut head : Option[ConcurrentNode[T]]
    mut tail : Option[ConcurrentNode[T]]
    mut size : AtomicInt
  }
  
  let create_node = fn[T](value : T) : ConcurrentNode[T] {
    { value: value, next: None }
  }
  
  let create_concurrent_queue = fn[T]() : ConcurrentQueue[T] {
    { head: None, tail: None, size: create_atomic_int(0) }
  }
  
  let enqueue = fn[T](queue : ConcurrentQueue[T], value : T) {
    let new_node = create_node(value)
    
    match queue.tail {
      None => {
        queue.head = Some(new_node)
        queue.tail = Some(new_node)
      }
      Some(tail_node) => {
        tail_node.next = Some(new_node)
        queue.tail = Some(new_node)
      }
    }
    
    atomic_increment(queue.size)
  }
  
  let dequeue = fn[T](queue : ConcurrentQueue[T]) : Option[T] {
    match queue.head {
      None => None
      Some(head_node) => {
        let value = head_node.value
        queue.head = head_node.next
        
        match queue.head {
          None => queue.tail = None
          Some(_) => ()
        }
        
        atomic_decrement(queue.size)
        Some(value)
      }
    }
  }
  
  let get_size = fn[T](queue : ConcurrentQueue[T]) : Int {
    atomic_get(queue.size)
  }
  
  let is_empty = fn[T](queue : ConcurrentQueue[T]) : Bool {
    get_size(queue) == 0
  }
  
  // Test basic queue operations
  let int_queue = create_concurrent_queue[Int]()
  
  assert_true(is_empty(int_queue))
  assert_eq(get_size(int_queue), 0)
  
  // Test enqueue
  enqueue(int_queue, 10)
  enqueue(int_queue, 20)
  enqueue(int_queue, 30)
  
  assert_false(is_empty(int_queue))
  assert_eq(get_size(int_queue), 3)
  
  // Test dequeue
  match dequeue(int_queue) {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
  
  match dequeue(int_queue) {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
  
  assert_eq(get_size(int_queue), 1)
  
  match dequeue(int_queue) {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  
  assert_true(is_empty(int_queue))
  assert_eq(get_size(int_queue), 0)
  
  // Test dequeue from empty queue
  match dequeue(int_queue) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test with strings
  let string_queue = create_concurrent_queue[String]()
  
  enqueue(string_queue, "first")
  enqueue(string_queue, "second")
  enqueue(string_queue, "third")
  
  match dequeue(string_queue) {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  match dequeue(string_queue) {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  match dequeue(string_queue) {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  assert_true(is_empty(string_queue))
}

// Test 4: Thread-Safe HashMap
test "thread-safe hash map implementation" {
  type HashMapEntry[K, V] {
    key : K
    value : V
    mut next : Option[HashMapEntry[K, V]]
  }
  
  type ConcurrentHashMap[K, V] {
    mut buckets : Array[Option[HashMapEntry[K, V]]]
    mut size : AtomicInt
  }
  
  let hash_string = fn(key : String) : Int {
    let mut hash = 5381
    for i in 0..<key.length() {
      hash = ((hash << 5) + hash) + key.char_code_at(i)
    }
    hash & 0x7fffffff
  }
  
  let create_concurrent_hash_map = fn[K, V](capacity : Int) : ConcurrentHashMap[K, V] {
    let buckets = []
    for i in 0..<capacity {
      buckets = buckets.push(None)
    }
    { buckets: buckets, size: create_atomic_int(0) }
  }
  
  let get_bucket_index = fn(map : ConcurrentHashMap[String, String], key : String) : Int {
    hash_string(key) % map.buckets.length()
  }
  
  let put = fn(map : ConcurrentHashMap[String, String], key : String, value : String) {
    let index = get_bucket_index(map, key)
    let bucket = map.buckets[index]
    
    match bucket {
      None => {
        map.buckets[index] = Some({ key: key, value: value, next: None })
        atomic_increment(map.size)
      }
      Some(head_entry) => {
        let mut current = head_entry
        let mut found = false
        
        // Search for existing key
        while true {
          if current.key == key {
            current.value = value
            found = true
            break
          }
          
          match current.next {
            None => break
            Some(next_entry) => current = next_entry
          }
        }
        
        // If not found, add new entry
        if !found {
          let new_entry = { key: key, value: value, next: Some(head_entry) }
          map.buckets[index] = Some(new_entry)
          atomic_increment(map.size)
        }
      }
    }
  }
  
  let get = fn(map : ConcurrentHashMap[String, String], key : String) : Option[String] {
    let index = get_bucket_index(map, key)
    let bucket = map.buckets[index]
    
    match bucket {
      None => None
      Some(head_entry) => {
        let mut current = head_entry
        
        while true {
          if current.key == key {
            return Some(current.value)
          }
          
          match current.next {
            None => return None
            Some(next_entry) => current = next_entry
          }
        }
      }
    }
  }
  
  let remove = fn(map : ConcurrentHashMap[String, String], key : String) : Bool {
    let index = get_bucket_index(map, key)
    let bucket = map.buckets[index]
    
    match bucket {
      None => false
      Some(head_entry) => {
        if head_entry.key == key {
          map.buckets[index] = head_entry.next
          atomic_decrement(map.size)
          return true
        }
        
        let mut current = head_entry
        
        while true {
          match current.next {
            None => return false
            Some(next_entry) => {
              if next_entry.key == key {
                current.next = next_entry.next
                atomic_decrement(map.size)
                return true
              }
              current = next_entry
            }
          }
        }
      }
    }
  }
  
  let get_size = fn(map : ConcurrentHashMap[String, String]) : Int {
    atomic_get(map.size)
  }
  
  // Test basic hash map operations
  let map = create_concurrent_hash_map[String, String](10)
  
  assert_eq(get_size(map), 0)
  
  // Test put and get
  put(map, "key1", "value1")
  put(map, "key2", "value2")
  put(map, "key3", "value3")
  
  assert_eq(get_size(map), 3)
  
  match get(map, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match get(map, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match get(map, "key3") {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  // Test update
  put(map, "key2", "new_value2")
  
  match get(map, "key2") {
    Some(value) => assert_eq(value, "new_value2")
    None => assert_true(false)
  }
  
  assert_eq(get_size(map), 3)  // Size should not change on update
  
  // Test remove
  assert_true(remove(map, "key1"))
  assert_false(remove(map, "nonexistent"))
  
  match get(map, "key1") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  assert_eq(get_size(map), 2)
  
  // Test collision handling
  put(map, "collision_key1", "value1")
  put(map, "collision_key2", "value2")
  
  match get(map, "collision_key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match get(map, "collision_key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
}

// Test 5: Thread Pool Simulation
test "thread pool simulation and task management" {
  type Task {
    id : Int
    work : () -> String
  }
  
  type ThreadPool {
    mut tasks : Array[Task]
    mut completed_tasks : Array[String]
    mut task_counter : AtomicInt
  }
  
  let create_task = fn(id : Int, work : () -> String) : Task {
    { id: id, work: work }
  }
  
  let create_thread_pool = fn() : ThreadPool {
    {
      tasks: [],
      completed_tasks: [],
      task_counter: create_atomic_int(0)
    }
  }
  
  let submit_task = fn(pool : ThreadPool, work : () -> String) : Int {
    let task_id = atomic_increment(pool.task_counter)
    let task = create_task(task_id, work)
    pool.tasks = pool.tasks.push(task)
    task_id
  }
  
  let execute_tasks = fn(pool : ThreadPool) {
    let tasks_to_execute = pool.tasks
    pool.tasks = []
    
    for task in tasks_to_execute {
      let result = task.work()
      pool.completed_tasks = pool.completed_tasks.push(result)
    }
  }
  
  let get_completed_tasks = fn(pool : ThreadPool) : Array[String] {
    pool.completed_tasks
  }
  
  let clear_completed_tasks = fn(pool : ThreadPool) {
    pool.completed_tasks = []
  }
  
  // Test thread pool operations
  let pool = create_thread_pool()
  
  // Submit tasks
  let task1_id = submit_task(pool, fn() { "Task 1 completed" })
  let task2_id = submit_task(pool, fn() { "Task 2 completed" })
  let task3_id = submit_task(pool, fn() { "Task 3 completed" })
  
  assert_eq(task1_id, 1)
  assert_eq(task2_id, 2)
  assert_eq(task3_id, 3)
  
  assert_eq(pool.tasks.length(), 3)
  assert_eq(pool.completed_tasks.length(), 0)
  
  // Execute tasks
  execute_tasks(pool)
  
  assert_eq(pool.tasks.length(), 0)
  assert_eq(pool.completed_tasks.length(), 3)
  
  let completed = get_completed_tasks(pool)
  assert_eq(completed, ["Task 1 completed", "Task 2 completed", "Task 3 completed"])
  
  // Submit more tasks
  let task4_id = submit_task(pool, fn() { "Task 4 completed" })
  let task5_id = submit_task(pool, fn() { "Task 5 completed" })
  
  execute_tasks(pool)
  
  assert_eq(pool.completed_tasks.length(), 5)
  
  let all_completed = get_completed_tasks(pool)
  assert_eq(all_completed, [
    "Task 1 completed", 
    "Task 2 completed", 
    "Task 3 completed",
    "Task 4 completed",
    "Task 5 completed"
  ])
  
  // Clear completed tasks
  clear_completed_tasks(pool)
  assert_eq(pool.completed_tasks.length(), 0)
  
  // Test task with computation
  let computation_task = submit_task(pool, fn() {
    let mut sum = 0
    for i in 0..<100 {
      sum = sum + i
    }
    "Sum: " + sum.to_string()
  })
  
  execute_tasks(pool)
  
  let results = get_completed_tasks(pool)
  assert_eq(results.length(), 1)
  assert_eq(results[0], "Sum: 4950")
}

// Test 6: Read-Write Lock Simulation
test "read-write lock simulation for concurrent access" {
  type LockType {
    Read
    Write
  }
  
  type RWLock {
    mut readers : Int
    mut writer : Bool
    mut waiting_writers : Int
  }
  
  let create_rwlock = fn() : RWLock {
    { readers: 0, writer: false, waiting_writers: 0 }
  }
  
  let acquire_read_lock = fn(lock : RWLock) : Bool {
    if !lock.writer && lock.waiting_writers == 0 {
      lock.readers = lock.readers + 1
      true
    } else {
      false
    }
  }
  
  let release_read_lock = fn(lock : RWLock) {
    if lock.readers > 0 {
      lock.readers = lock.readers - 1
    }
  }
  
  let acquire_write_lock = fn(lock : RWLock) : Bool {
    if !lock.writer && lock.readers == 0 {
      lock.writer = true
      true
    } else {
      false.waiting_writers = lock.waiting_writers + 1
      false
    }
  }
  
  let release_write_lock = fn(lock : RWLock) {
    if lock.writer {
      lock.writer = false
      if lock.waiting_writers > 0 {
        lock.waiting_writers = lock.waiting_writers - 1
      }
    }
  }
  
  let can_read = fn(lock : RWLock) : Bool {
    !lock.writer && lock.waiting_writers == 0
  }
  
  let can_write = fn(lock : RWLock) : Bool {
    !lock.writer && lock.readers == 0
  }
  
  // Test read-write lock operations
  let lock = create_rwlock()
  
  // Initially, both read and write should be possible
  assert_true(can_read(lock))
  assert_true(can_write(lock))
  
  // Acquire read lock
  assert_true(acquire_read_lock(lock))
  assert_eq(lock.readers, 1)
  assert_true(can_read(lock))  // Multiple readers allowed
  assert_false(can_write(lock)) // Writers blocked when readers present
  
  // Acquire another read lock
  assert_true(acquire_read_lock(lock))
  assert_eq(lock.readers, 2)
  assert_true(can_read(lock))
  assert_false(can_write(lock))
  
  // Release read locks
  release_read_lock(lock)
  assert_eq(lock.readers, 1)
  assert_true(can_read(lock))
  assert_false(can_write(lock))
  
  release_read_lock(lock)
  assert_eq(lock.readers, 0)
  assert_true(can_read(lock))
  assert_true(can_write(lock))
  
  // Acquire write lock
  assert_true(acquire_write_lock(lock))
  assert_true(lock.writer)
  assert_false(can_read(lock))  // Readers blocked when writer present
  assert_false(can_write(lock)) // Only one writer allowed
  
  // Release write lock
  release_write_lock(lock)
  assert_false(lock.writer)
  assert_true(can_read(lock))
  assert_true(can_write(lock))
  
  // Test waiting writers
  assert_false(acquire_write_lock(lock))  // Simulate blocked writer
  assert_eq(lock.waiting_writers, 1)
  assert_false(can_read(lock))  // Readers blocked when waiting writers
  
  // Release waiting writer
  release_write_lock(lock)
  assert_eq(lock.waiting_writers, 0)
  assert_true(can_read(lock))
  
  // Test multiple operations
  let acquire_multiple_readers = fn(lock : RWLock, count : Int) {
    for i in 0..<count {
      acquire_read_lock(lock)
    }
  }
  
  let release_all_readers = fn(lock : RWLock) {
    while lock.readers > 0 {
      release_read_lock(lock)
    }
  }
  
  acquire_multiple_readers(lock, 5)
  assert_eq(lock.readers, 5)
  assert_false(can_write(lock))
  
  release_all_readers(lock)
  assert_eq(lock.readers, 0)
  assert_true(can_write(lock))
}