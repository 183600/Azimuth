// Azimuth 并发和同步测试
// 专注于测试系统的并发处理能力和同步机制

// 测试1: 原子操作测试
test "原子操作测试" {
  // 模拟原子计数器
  type AtomicCounter {
    value : Int
    operations : Array<String>
  }
  
  // 创建原子计数器
  let create_counter = fn(initial : Int) -> AtomicCounter {
    { value: initial, operations: [] }
  }
  
  // 原子递增操作
  let atomic_increment = fn(counter : AtomicCounter, thread_id : String) -> AtomicCounter {
    { 
      value: counter.value + 1, 
      operations: counter.operations.push(thread_id + ":increment")
    }
  }
  
  // 原子递减操作
  let atomic_decrement = fn(counter : AtomicCounter, thread_id : String) -> AtomicCounter {
    { 
      value: counter.value - 1, 
      operations: counter.operations.push(thread_id + ":decrement")
    }
  }
  
  // 原子读取操作
  let atomic_read = fn(counter : AtomicCounter, thread_id : String) -> (Int, AtomicCounter) {
    (counter.value, { 
      value: counter.value, 
      operations: counter.operations.push(thread_id + ":read")
    })
  }
  
  // 测试原子操作
  let initial_counter = create_counter(0)
  
  // 模拟多个线程的原子操作
  let counter1 = atomic_increment(initial_counter, "thread1")
  let counter2 = atomic_increment(counter1, "thread2")
  let counter3 = atomic_decrement(counter2, "thread3")
  
  // 验证最终值
  assert_eq(counter3.value, 1)
  
  // 验证操作日志
  assert_eq(counter3.operations.length(), 3)
  assert_eq(counter3.operations[0], "thread1:increment")
  assert_eq(counter3.operations[1], "thread2:increment")
  assert_eq(counter3.operations[2], "thread3:decrement")
  
  // 测试原子读取
  let (read_value, counter4) = atomic_read(counter3, "thread4")
  assert_eq(read_value, 1)
  assert_eq(counter4.operations.length(), 4)
}

// 测试2: 互斥锁测试
test "互斥锁测试" {
  // 模拟互斥锁
  type Mutex {
    locked : Bool
    owner : Option<String>
    wait_queue : Array<String>
  }
  
  // 创建互斥锁
  let create_mutex = fn() -> Mutex {
    { locked: false, owner: None, wait_queue: [] }
  }
  
  // 尝试获取锁
  let try_lock = fn(mutex : Mutex, thread_id : String) -> (Bool, Mutex) {
    if mutex.locked {
      (false, { locked: true, owner: mutex.owner, wait_queue: mutex.wait_queue.push(thread_id) })
    } else {
      (true, { locked: true, owner: Some(thread_id), wait_queue: mutex.wait_queue })
    }
  }
  
  // 释放锁
  let unlock = fn(mutex : Mutex, thread_id : String) -> Mutex {
    match mutex.owner {
      Some(owner) => {
        if owner == thread_id {
          // 如果有等待的线程，将锁交给第一个等待者
          match mutex.wait_queue.get(0) {
            Some(next_thread) => {
              let new_queue = mutex.wait_queue.slice(1, mutex.wait_queue.length())
              { locked: true, owner: Some(next_thread), wait_queue: new_queue }
            }
            None => { locked: false, owner: None, wait_queue: [] }
          }
        } else {
          mutex  // 不是锁的所有者，无法释放
        }
      }
      None => mutex
    }
  }
  
  // 测试互斥锁操作
  let initial_mutex = create_mutex()
  assert_false(initial_mutex.locked)
  
  // 线程1获取锁
  let (success1, mutex1) = try_lock(initial_mutex, "thread1")
  assert_true(success1)
  assert_true(mutex1.locked)
  match mutex1.owner {
    Some(owner) => assert_eq(owner, "thread1")
    None => assert_true(false)
  }
  
  // 线程2尝试获取锁（失败）
  let (success2, mutex2) = try_lock(mutex1, "thread2")
  assert_false(success2)
  assert_eq(mutex2.wait_queue.length(), 1)
  assert_eq(mutex2.wait_queue[0], "thread2")
  
  // 线程1释放锁
  let mutex3 = unlock(mutex2, "thread1")
  assert_true(mutex3.locked)
  match mutex3.owner {
    Some(owner) => assert_eq(owner, "thread2")
    None => assert_true(false)
  }
  assert_eq(mutex3.wait_queue.length(), 0)
}

// 测试3: 信号量测试
test "信号量测试" {
  // 模拟信号量
  type Semaphore {
    permits : Int
    max_permits : Int
    wait_queue : Array<String>
  }
  
  // 创建信号量
  let create_semaphore = fn(initial_permits : Int) -> Semaphore {
    { permits: initial_permits, max_permits: initial_permits, wait_queue: [] }
  }
  
  // 获取许可
  let acquire = fn(semaphore : Semaphore, thread_id : String) -> (Bool, Semaphore) {
    if semaphore.permits > 0 {
      (true, { permits: semaphore.permits - 1, max_permits: semaphore.max_permits, wait_queue: semaphore.wait_queue })
    } else {
      (false, { permits: semaphore.permits, max_permits: semaphore.max_permits, wait_queue: semaphore.wait_queue.push(thread_id) })
    }
  }
  
  // 释放许可
  let release = fn(semaphore : Semaphore, thread_id : String) -> Semaphore {
    // 从等待队列中移除该线程（如果存在）
    let new_queue = semaphore.wait_queue.filter(fn(id) { id != thread_id })
    
    // 如果有等待的线程，直接将许可给它
    if new_queue.length() < semaphore.wait_queue.length() {
      { permits: semaphore.permits, max_permits: semaphore.max_permits, wait_queue: new_queue }
    } else {
      // 增加许可数量，但不超过最大值
      let new_permits = if semaphore.permits + 1 <= semaphore.max_permits { semaphore.permits + 1 } else { semaphore.max_permits }
      { permits: new_permits, max_permits: semaphore.max_permits, wait_queue: new_queue }
    }
  }
  
  // 测试信号量操作
  let initial_semaphore = create_semaphore(2)
  assert_eq(initial_semaphore.permits, 2)
  
  // 线程1获取许可
  let (success1, sem1) = acquire(initial_semaphore, "thread1")
  assert_true(success1)
  assert_eq(sem1.permits, 1)
  
  // 线程2获取许可
  let (success2, sem2) = acquire(sem1, "thread2")
  assert_true(success2)
  assert_eq(sem2.permits, 0)
  
  // 线程3尝试获取许可（失败）
  let (success3, sem3) = acquire(sem2, "thread3")
  assert_false(success3)
  assert_eq(sem3.permits, 0)
  assert_eq(sem3.wait_queue.length(), 1)
  assert_eq(sem3.wait_queue[0], "thread3")
  
  // 线程1释放许可
  let sem4 = release(sem3, "thread1")
  assert_eq(sem4.permits, 0)  // 许可直接给了等待的线程3
  assert_eq(sem4.wait_queue.length(), 0)
}

// 测试4: 条件变量测试
test "条件变量测试" {
  // 模拟条件变量
  type ConditionVariable {
    waiting_threads : Array<String>
    signaled : Bool
  }
  
  // 创建条件变量
  let create_condition = fn() -> ConditionVariable {
    { waiting_threads: [], signaled: false }
  }
  
  // 等待条件
  let wait = fn(condition : ConditionVariable, thread_id : String) -> ConditionVariable {
    { 
      waiting_threads: condition.waiting_threads.push(thread_id), 
      signaled: false 
    }
  }
  
  // 发送信号（唤醒一个等待的线程）
  let signal = fn(condition : ConditionVariable) -> ConditionVariable {
    if condition.waiting_threads.length() > 0 {
      let first_thread = condition.waiting_threads[0]
      let remaining_threads = condition.waiting_threads.slice(1, condition.waiting_threads.length())
      { 
        waiting_threads: remaining_threads, 
        signaled: true 
      }
    } else {
      condition
    }
  }
  
  // 广播信号（唤醒所有等待的线程）
  let broadcast = fn(condition : ConditionVariable) -> ConditionVariable {
    { 
      waiting_threads: [], 
      signaled: true 
    }
  }
  
  // 测试条件变量操作
  let initial_condition = create_condition()
  assert_eq(initial_condition.waiting_threads.length(), 0)
  assert_false(initial_condition.signaled)
  
  // 线程1等待条件
  let condition1 = wait(initial_condition, "thread1")
  assert_eq(condition1.waiting_threads.length(), 1)
  assert_eq(condition1.waiting_threads[0], "thread1")
  
  // 线程2等待条件
  let condition2 = wait(condition1, "thread2")
  assert_eq(condition2.waiting_threads.length(), 2)
  
  // 发送信号（唤醒一个线程）
  let condition3 = signal(condition2)
  assert_eq(condition3.waiting_threads.length(), 1)
  assert_eq(condition3.waiting_threads[0], "thread2")
  assert_true(condition3.signaled)
  
  // 广播信号（唤醒所有线程）
  let condition4 = broadcast(condition3)
  assert_eq(condition4.waiting_threads.length(), 0)
  assert_true(condition4.signaled)
}

// 测试5: 并发集合测试
test "并发集合测试" {
  // 模拟线程安全的集合
  type ConcurrentSet {
    data : Set<String>
    operations : Array<String>
  }
  
  // 创建并发集合
  let create_concurrent_set = fn() -> ConcurrentSet {
    { data: Set::empty(), operations: [] }
  }
  
  // 线程安全添加
  let concurrent_add = fn(set : ConcurrentSet, item : String, thread_id : String) -> ConcurrentSet {
    { 
      data: set.data.insert(item), 
      operations: set.operations.push(thread_id + ":add:" + item) 
    }
  }
  
  // 线程安全删除
  let concurrent_remove = fn(set : ConcurrentSet, item : String, thread_id : String) -> ConcurrentSet {
    { 
      data: set.data.remove(item), 
      operations: set.operations.push(thread_id + ":remove:" + item) 
    }
  }
  
  // 线程安全包含检查
  let concurrent_contains = fn(set : ConcurrentSet, item : String, thread_id : String) -> (Bool, ConcurrentSet) {
    let contains = set.data.contains(item)
    (contains, { 
      data: set.data, 
      operations: set.operations.push(thread_id + ":contains:" + item) 
    })
  }
  
  // 测试并发集合操作
  let initial_set = create_concurrent_set()
  assert_eq(initial_set.data.size(), 0)
  
  // 多个线程添加元素
  let set1 = concurrent_add(initial_set, "item1", "thread1")
  let set2 = concurrent_add(set1, "item2", "thread2")
  let set3 = concurrent_add(set2, "item3", "thread1")
  
  assert_eq(set3.data.size(), 3)
  assert_true(set3.data.contains("item1"))
  assert_true(set3.data.contains("item2"))
  assert_true(set3.data.contains("item3"))
  
  // 测试包含检查
  let (contains1, set4) = concurrent_contains(set3, "item2", "thread3")
  assert_true(contains1)
  
  let (contains2, set5) = concurrent_contains(set4, "item4", "thread3")
  assert_false(contains2)
  
  // 测试删除操作
  let set6 = concurrent_remove(set5, "item2", "thread2")
  assert_eq(set6.data.size(), 2)
  assert_false(set6.data.contains("item2"))
  assert_true(set6.data.contains("item1"))
  assert_true(set6.data.contains("item3"))
  
  // 验证操作日志
  assert_eq(set6.operations.length(), 6)
  assert_eq(set6.operations[0], "thread1:add:item1")
  assert_eq(set6.operations[1], "thread2:add:item2")
  assert_eq(set6.operations[2], "thread1:add:item3")
  assert_eq(set6.operations[3], "thread3:contains:item2")
  assert_eq(set6.operations[4], "thread3:contains:item4")
  assert_eq(set6.operations[5], "thread2:remove:item2")
}

// 测试6: 读写锁测试
test "读写锁测试" {
  // 模拟读写锁
  type ReadWriteLock {
    readers : Int
    writer : Option<String>
    waiting_writers : Array<String>
    waiting_readers : Array<String>
  }
  
  // 创建读写锁
  let create_rwlock = fn() -> ReadWriteLock {
    { readers: 0, writer: None, waiting_writers: [], waiting_readers: [] }
  }
  
  // 获取读锁
  let acquire_read = fn(lock : ReadWriteLock, thread_id : String) -> (Bool, ReadWriteLock) {
    match lock.writer {
      Some(_) => {
        // 有写锁，需要等待
        (false, { readers: lock.readers, writer: lock.writer, waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers.push(thread_id) })
      }
      None => {
        // 没有写锁，可以获取读锁
        (true, { readers: lock.readers + 1, writer: None, waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers })
      }
    }
  }
  
  // 获取写锁
  let acquire_write = fn(lock : ReadWriteLock, thread_id : String) -> (Bool, ReadWriteLock) {
    if lock.readers == 0 && lock.writer.is_none() {
      // 没有读者和写者，可以获取写锁
      (true, { readers: 0, writer: Some(thread_id), waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers })
    } else {
      // 有读者或写者，需要等待
      (false, { readers: lock.readers, writer: lock.writer, waiting_writers: lock.waiting_writers.push(thread_id), waiting_readers: lock.waiting_readers })
    }
  }
  
  // 释放读锁
  let release_read = fn(lock : ReadWriteLock, thread_id : String) -> ReadWriteLock {
    let new_readers = lock.readers - 1
    
    // 如果没有读者了，检查是否有等待的写者
    if new_readers == 0 && lock.waiting_writers.length() > 0 {
      let first_writer = lock.waiting_writers[0]
      let remaining_writers = lock.waiting_writers.slice(1, lock.waiting_writers.length())
      { readers: 0, writer: Some(first_writer), waiting_writers: remaining_writers, waiting_readers: lock.waiting_readers }
    } else {
      { readers: new_readers, writer: lock.writer, waiting_writers: lock.waiting_writers, waiting_readers: lock.waiting_readers }
    }
  }
  
  // 释放写锁
  let release_write = fn(lock : ReadWriteLock, thread_id : String) -> ReadWriteLock {
    match lock.writer {
      Some(writer) => {
        if writer == thread_id {
          // 释放写锁后，优先让等待的读者获取锁
          let reader_count = lock.waiting_readers.length()
          { readers: reader_count, writer: None, waiting_writers: lock.waiting_writers, waiting_readers: [] }
        } else {
          lock  // 不是写锁的所有者
        }
      }
      None => lock
    }
  }
  
  // 测试读写锁操作
  let initial_lock = create_rwlock()
  assert_eq(initial_lock.readers, 0)
  assert_eq(initial_lock.writer, None)
  
  // 线程1获取读锁
  let (success1, lock1) = acquire_read(initial_lock, "thread1")
  assert_true(success1)
  assert_eq(lock1.readers, 1)
  
  // 线程2获取读锁
  let (success2, lock2) = acquire_read(lock1, "thread2")
  assert_true(success2)
  assert_eq(lock2.readers, 2)
  
  // 线程3尝试获取写锁（失败）
  let (success3, lock3) = acquire_write(lock2, "thread3")
  assert_false(success3)
  assert_eq(lock3.waiting_writers.length(), 1)
  assert_eq(lock3.waiting_writers[0], "thread3")
  
  // 线程1释放读锁
  let lock4 = release_read(lock3, "thread1")
  assert_eq(lock4.readers, 1)
  assert_eq(lock4.writer, None)  // 还有读者，写者仍需等待
  
  // 线程2释放读锁
  let lock5 = release_read(lock4, "thread2")
  assert_eq(lock5.readers, 0)
  match lock5.writer {
    Some(writer) => assert_eq(writer, "thread3")  // 写者获取锁
    None => assert_true(false)
  }
  
  // 线程3释放写锁
  let lock6 = release_write(lock5, "thread3")
  assert_eq(lock6.readers, 0)
  assert_eq(lock6.writer, None)
}