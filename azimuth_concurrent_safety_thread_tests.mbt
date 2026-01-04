// Azimuth 并发安全和线程测试用例
// 专注于并发操作和线程安全测试

// 测试1: 原子操作测试
test "原子操作测试" {
  // 模拟原子计数器
  type AtomicCounter = {
    value: Int,
    lock: Bool
  }
  
  let create_counter = fn(initial_value: Int) {
    { value: initial_value, lock: false }
  }
  
  let atomic_increment = fn(counter: AtomicCounter) {
    if counter.lock {
      counter // 如果已锁定，返回原值
    } else {
      { counter | value: counter.value + 1, lock: true }
    }
  }
  
  let atomic_decrement = fn(counter: AtomicCounter) {
    if counter.lock {
      counter // 如果已锁定，返回原值
    } else {
      { counter | value: counter.value - 1, lock: true }
    }
  }
  
  let unlock = fn(counter: AtomicCounter) {
    { counter | lock: false }
  }
  
  // 测试原子操作
  let counter = create_counter(0)
  let locked_counter = atomic_increment(counter)
  assert_eq(locked_counter.value, 1)
  assert_true(locked_counter.lock)
  
  let unlocked_counter = unlock(locked_counter)
  assert_eq(unlocked_counter.value, 1)
  assert_false(unlocked_counter.lock)
  
  let counter2 = atomic_decrement(unlocked_counter)
  assert_eq(counter2.value, 0)
  assert_true(counter2.lock)
}

// 测试2: 互斥锁测试
test "互斥锁测试" {
  // 模拟互斥锁
  type Mutex = {
    locked: Bool,
    owner: Option[String]
  }
  
  let create_mutex = fn() {
    { locked: false, owner: None }
  }
  
  let lock = fn(mutex: Mutex, thread_id: String) {
    if mutex.locked {
      (false, mutex) // 锁已被占用
    } else {
      (true, { mutex | locked: true, owner: Some(thread_id) })
    }
  }
  
  let unlock = fn(mutex: Mutex, thread_id: String) {
    match mutex.owner {
      Some(owner) => {
        if owner == thread_id {
          { mutex | locked: false, owner: None }
        } else {
          mutex // 不是锁的拥有者，不能解锁
        }
      }
      None => mutex // 锁未被占用
    }
  }
  
  // 测试互斥锁
  let mutex = create_mutex()
  assert_false(mutex.locked)
  
  let (success1, locked_mutex) = lock(mutex, "thread-1")
  assert_true(success1)
  assert_true(locked_mutex.locked)
  assert_eq(locked_mutex.owner, Some("thread-1"))
  
  let (success2, still_locked) = lock(locked_mutex, "thread-2")
  assert_false(success2)
  assert_true(still_locked.locked)
  assert_eq(still_locked.owner, Some("thread-1"))
  
  let unlocked_mutex = unlock(still_locked, "thread-1")
  assert_false(unlocked_mutex.locked)
  assert_eq(unlocked_mutex.owner, None)
  
  let (success3, relocked_mutex) = lock(unlocked_mutex, "thread-2")
  assert_true(success3)
  assert_true(relocked_mutex.locked)
  assert_eq(relocked_mutex.owner, Some("thread-2"))
}

// 测试3: 信号量测试
test "信号量测试" {
  // 模拟信号量
  type Semaphore = {
    permits: Int,
    max_permits: Int,
    waiting: Array[String]
  }
  
  let create_semaphore = fn(max_permits: Int) {
    { permits: max_permits, max_permits, waiting: [] }
  }
  
  let acquire = fn(semaphore: Semaphore, thread_id: String) {
    if semaphore.permits > 0 {
      (true, { semaphore | permits: semaphore.permits - 1 })
    } else {
      (false, { semaphore | waiting: semaphore.waiting.push(thread_id) })
    }
  }
  
  let release = fn(semaphore: Semaphore, thread_id: String) {
    let new_permits = semaphore.permits + 1
    
    if semaphore.waiting.length() > 0 {
      let next_thread = semaphore.waiting[0]
      let new_waiting = semaphore.waiting.slice(1, semaphore.waiting.length() - 1)
      { permits: new_permits - 1, max_permits: semaphore.max_permits, waiting: new_waiting }
    } else {
      { permits: new_permits, max_permits: semaphore.max_permits, waiting: semaphore.waiting }
    }
  }
  
  // 测试信号量
  let semaphore = create_semaphore(2)
  assert_eq(semaphore.permits, 2)
  
  let (success1, sem1) = acquire(semaphore, "thread-1")
  assert_true(success1)
  assert_eq(sem1.permits, 1)
  
  let (success2, sem2) = acquire(sem1, "thread-2")
  assert_true(success2)
  assert_eq(sem2.permits, 0)
  
  let (success3, sem3) = acquire(sem2, "thread-3")
  assert_false(success3)
  assert_eq(sem3.permits, 0)
  assert_eq(sem3.waiting.length(), 1)
  assert_eq(sem3.waiting[0], "thread-3")
  
  let sem4 = release(sem3, "thread-1")
  assert_eq(sem4.permits, 0)
  assert_eq(sem4.waiting.length(), 0)
}

// 测试4: 读写锁测试
test "读写锁测试" {
  // 模拟读写锁
  type ReadWriteLock = {
    readers: Int,
    writer: Option[String],
    waiting_writers: Array[String]
  }
  
  let create_rwlock = fn() {
    { readers: 0, writer: None, waiting_writers: [] }
  }
  
  let acquire_read = fn(rwlock: ReadWriteLock, thread_id: String) {
    if rwlock.writer.is_none() and rwlock.waiting_writers.length() == 0 {
      (true, { rwlock | readers: rwlock.readers + 1 })
    } else {
      (false, rwlock)
    }
  }
  
  let release_read = fn(rwlock: ReadWriteLock, thread_id: String) {
    { rwlock | readers: rwlock.readers - 1 }
  }
  
  let acquire_write = fn(rwlock: ReadWriteLock, thread_id: String) {
    if rwlock.writer.is_none() and rwlock.readers == 0 {
      (true, { rwlock | writer: Some(thread_id) })
    } else {
      (false, { rwlock | waiting_writers: rwlock.waiting_writers.push(thread_id) })
    }
  }
  
  let release_write = fn(rwlock: ReadWriteLock, thread_id: String) {
    match rwlock.writer {
      Some(writer) => {
        if writer == thread_id {
          { rwlock | writer: None }
        } else {
          rwlock
        }
      }
      None => rwlock
    }
  }
  
  // 测试读写锁
  let rwlock = create_rwlock()
  
  // 多个读者可以同时获取锁
  let (success1, rwlock1) = acquire_read(rwlock, "reader-1")
  assert_true(success1)
  assert_eq(rwlock1.readers, 1)
  
  let (success2, rwlock2) = acquire_read(rwlock1, "reader-2")
  assert_true(success2)
  assert_eq(rwlock2.readers, 2)
  
  // 写者无法在有读者时获取锁
  let (success3, rwlock3) = acquire_write(rwlock2, "writer-1")
  assert_false(success3)
  assert_eq(rwlock3.waiting_writers.length(), 1)
  
  // 读者释放锁
  let rwlock4 = release_read(rwlock3, "reader-1")
  assert_eq(rwlock4.readers, 1)
  
  let rwlock5 = release_read(rwlock4, "reader-2")
  assert_eq(rwlock5.readers, 0)
  
  // 现在写者可以获取锁
  let (success4, rwlock6) = acquire_write(rwlock5, "writer-2")
  assert_true(success4)
  assert_eq(rwlock6.writer, Some("writer-2"))
  
  // 其他写者无法在有写者时获取锁
  let (success5, rwlock7) = acquire_write(rwlock6, "writer-3")
  assert_false(success5)
  
  // 读者无法在有写者时获取锁
  let (success6, rwlock8) = acquire_read(rwlock7, "reader-3")
  assert_false(success6)
  
  // 写者释放锁
  let rwlock9 = release_write(rwlock8, "writer-2")
  assert_eq(rwlock9.writer, None)
}

// 测试5: 条件变量测试
test "条件变量测试" {
  // 模拟条件变量
  type ConditionVariable = {
    waiting: Array[String],
    signaled: Bool
  }
  
  type MutexWithCondition = {
    mutex: Mutex,
    condition: ConditionVariable
  }
  
  type Mutex = {
    locked: Bool,
    owner: Option[String]
  }
  
  let create_mutex_with_condition = fn() {
    {
      mutex: { locked: false, owner: None },
      condition: { waiting: [], signaled: false }
    }
  }
  
  let lock_mutex = fn(mutex: Mutex, thread_id: String) {
    if mutex.locked {
      (false, mutex)
    } else {
      (true, { mutex | locked: true, owner: Some(thread_id) })
    }
  }
  
  let unlock_mutex = fn(mutex: Mutex, thread_id: String) {
    match mutex.owner {
      Some(owner) => {
        if owner == thread_id {
          { mutex | locked: false, owner: None }
        } else {
          mutex
        }
      }
      None => mutex
    }
  }
  
  let wait = fn(mutex_cond: MutexWithCondition, thread_id: String) {
    let unlocked_mutex = unlock_mutex(mutex_cond.mutex, thread_id)
    let new_waiting = mutex_cond.condition.waiting.push(thread_id)
    
    {
      mutex: unlocked_mutex,
      condition: { waiting: new_waiting, signaled: mutex_cond.condition.signaled }
    }
  }
  
  let signal = fn(mutex_cond: MutexWithCondition) {
    if mutex_cond.condition.waiting.length() > 0 {
      let first_waiting = mutex_cond.condition.waiting[0]
      let new_waiting = mutex_cond.condition.waiting.slice(1, mutex_cond.condition.waiting.length() - 1)
      
      {
        mutex: mutex_cond.mutex,
        condition: { waiting: new_waiting, signaled: true }
      }
    } else {
      {
        mutex: mutex_cond.mutex,
        condition: { waiting: [], signaled: true }
      }
    }
  }
  
  // 测试条件变量
  let mutex_cond = create_mutex_with_condition()
  
  let (success, locked_mutex) = lock_mutex(mutex_cond.mutex, "thread-1")
  assert_true(success)
  
  let mutex_cond2 = { mutex_cond | mutex: locked_mutex }
  let mutex_cond3 = wait(mutex_cond2, "thread-1")
  
  assert_false(mutex_cond3.mutex.locked)
  assert_eq(mutex_cond3.condition.waiting.length(), 1)
  assert_eq(mutex_cond3.condition.waiting[0], "thread-1")
  
  let mutex_cond4 = signal(mutex_cond3)
  assert_true(mutex_cond4.condition.signaled)
  assert_eq(mutex_cond4.condition.waiting.length(), 0)
}

// 测试6: 线程安全的队列测试
test "线程安全的队列测试" {
  // 模拟线程安全队列
  type ThreadSafeQueue = {
    elements: Array[String],
    lock: Bool,
    size: Int
  }
  
  let create_thread_safe_queue = fn() {
    { elements: [], lock: false, size: 0 }
  }
  
  let enqueue = fn(queue: ThreadSafeQueue, element: String) {
    if queue.lock {
      queue
    } else {
      {
        elements: queue.elements.push(element),
        lock: true,
        size: queue.size + 1
      }
    }
  }
  
  let dequeue = fn(queue: ThreadSafeQueue) {
    if queue.lock or queue.size == 0 {
      (None, queue)
    } else {
      let first = queue.elements[0]
      let remaining = queue.elements.slice(1, queue.size - 1)
      
      (Some(first), {
        elements: remaining,
        lock: true,
        size: queue.size - 1
      })
    }
  }
  
  let unlock = fn(queue: ThreadSafeQueue) {
    { queue | lock: false }
  }
  
  // 测试线程安全队列
  let queue = create_thread_safe_queue()
  
  let locked_queue1 = enqueue(queue, "item1")
  assert_eq(locked_queue1.size, 1)
  assert_true(locked_queue1.lock)
  
  let unlocked_queue1 = unlock(locked_queue1)
  assert_false(unlocked_queue1.lock)
  
  let locked_queue2 = enqueue(unlocked_queue1, "item2")
  assert_eq(locked_queue2.size, 2)
  
  let unlocked_queue2 = unlock(locked_queue2)
  
  let (item1, locked_queue3) = dequeue(unlocked_queue2)
  assert_eq(item1, Some("item1"))
  assert_eq(locked_queue3.size, 1)
  assert_true(locked_queue3.lock)
  
  let unlocked_queue3 = unlock(locked_queue3)
  
  let (item2, locked_queue4) = dequeue(unlocked_queue3)
  assert_eq(item2, Some("item2"))
  assert_eq(locked_queue4.size, 0)
}

// 测试7: 线程安全的计数器测试
test "线程安全的计数器测试" {
  // 模拟线程安全计数器
  type ThreadSafeCounter = {
    value: Int,
    lock: Bool
  }
  
  let create_thread_safe_counter = fn(initial_value: Int) {
    { value: initial_value, lock: false }
  }
  
  let increment = fn(counter: ThreadSafeCounter) {
    if counter.lock {
      counter
    } else {
      { counter | value: counter.value + 1, lock: true }
    }
  }
  
  let decrement = fn(counter: ThreadSafeCounter) {
    if counter.lock {
      counter
    } else {
      { counter | value: counter.value - 1, lock: true }
    }
  }
  
  let get_value = fn(counter: ThreadSafeCounter) {
    counter.value
  }
  
  let unlock = fn(counter: ThreadSafeCounter) {
    { counter | lock: false }
  }
  
  // 测试线程安全计数器
  let counter = create_thread_safe_counter(0)
  assert_eq(get_value(counter), 0)
  
  let locked_counter1 = increment(counter)
  assert_eq(get_value(locked_counter1), 1)
  assert_true(locked_counter1.lock)
  
  let unlocked_counter1 = unlock(locked_counter1)
  assert_false(unlocked_counter1.lock)
  
  let locked_counter2 = increment(unlocked_counter1)
  assert_eq(get_value(locked_counter2), 2)
  
  let unlocked_counter2 = unlock(locked_counter2)
  
  let locked_counter3 = decrement(unlocked_counter2)
  assert_eq(get_value(locked_counter3), 1)
  
  let unlocked_counter3 = unlock(locked_counter3)
  assert_eq(get_value(unlocked_counter3), 1)
}

// 测试8: 线程池测试
test "线程池测试" {
  // 模拟线程池
  type Task = {
    id: String,
    function: () -> String,
    status: String // "pending", "running", "completed"
  }
  
  type ThreadPool = {
    threads: Array[String],
    task_queue: Array[Task],
    max_threads: Int,
    active_threads: Int
  }
  
  let create_thread_pool = fn(max_threads: Int) {
    let mut threads = []
    for i in 0..max_threads {
      threads = threads.push("thread-" + i.to_string())
    }
    
    {
      threads,
      task_queue: [],
      max_threads,
      active_threads: 0
    }
  }
  
  let submit_task = fn(pool: ThreadPool, task_id: String, task_function: () -> String) {
    let new_task = {
      id: task_id,
      function: task_function,
      status: "pending"
    }
    
    { pool | task_queue: pool.task_queue.push(new_task) }
  }
  
  let execute_next_task = fn(pool: ThreadPool) {
    if pool.task_queue.length() > 0 and pool.active_threads < pool.max_threads {
      let next_task = pool.task_queue[0]
      let remaining_tasks = pool.task_queue.slice(1, pool.task_queue.length() - 1)
      
      let updated_task = { next_task | status: "running" }
      let result = updated_task.function()
      let completed_task = { updated_task | status: "completed" }
      
      ({
        threads: pool.threads,
        task_queue: remaining_tasks.push(completed_task),
        max_threads: pool.max_threads,
        active_threads: pool.active_threads + 1
      }, result)
    } else {
      (pool, None)
    }
  }
  
  // 测试线程池
  let pool = create_thread_pool(3)
  assert_eq(pool.max_threads, 3)
  assert_eq(pool.active_threads, 0)
  
  let task1 = fn() { "Task 1 completed" }
  let task2 = fn() { "Task 2 completed" }
  let task3 = fn() { "Task 3 completed" }
  
  let pool1 = submit_task(pool, "task-1", task1)
  assert_eq(pool1.task_queue.length(), 1)
  
  let pool2 = submit_task(pool1, "task-2", task2)
  assert_eq(pool2.task_queue.length(), 2)
  
  let pool3 = submit_task(pool2, "task-3", task3)
  assert_eq(pool3.task_queue.length(), 3)
  
  let (pool4, result1) = execute_next_task(pool3)
  assert_eq(result1, Some("Task 1 completed"))
  assert_eq(pool4.active_threads, 1)
  
  let (pool5, result2) = execute_next_task(pool4)
  assert_eq(result2, Some("Task 2 completed"))
  assert_eq(pool5.active_threads, 2)
  
  let (pool6, result3) = execute_next_task(pool5)
  assert_eq(result3, Some("Task 3 completed"))
  assert_eq(pool6.active_threads, 3)
}