// Azimuth Concurrency Control Test Suite
// 并发控制和同步机制测试用例

test "互斥锁基本操作" {
  // 测试互斥锁基本操作
  let mutex = @azimuth.Mutex::new()
  
  // 验证初始状态
  assert_true(mutex.is_unlocked())
  assert_false(mutex.is_locked())
  
  // 获取锁
  let lock_result = mutex.lock()
  match lock_result {
    @azimuth.LockResult::Acquired(lock_guard) => {
      assert_true(mutex.is_locked())
      assert_false(mutex.is_unlocked())
      
      // 释放锁
      mutex = lock_guard.unlock()
      assert_true(mutex.is_unlocked())
      assert_false(mutex.is_locked())
    }
    _ => assert_true(false)
  }
  
  // 测试尝试获取锁（非阻塞）
  let lock_result2 = mutex.try_lock()
  match lock_result2 {
    @azimuth.LockResult::Acquired(lock_guard) => {
      assert_true(mutex.is_locked())
      
      // 尝试再次获取锁（应该失败）
      let lock_result3 = mutex.try_lock()
      match lock_result3 {
        @azimuth.LockResult::WouldBlock => assert_true(true)
        _ => assert_true(false)
      }
      
      // 释放锁
      mutex = lock_guard.unlock()
    }
    _ => assert_true(false)
  }
}

test "读写锁操作" {
  // 测试读写锁操作
  let rwlock = @azimuth.RwLock::new()
  
  // 验证初始状态
  assert_eq(rwlock.readers_count(), 0)
  assert_false(rwlock.is_write_locked())
  
  // 获取读锁
  let read_lock1 = rwlock.read_lock()
  match read_lock1 {
    @azimuth.ReadLockResult::Acquired(read_guard) => {
      assert_eq(rwlock.readers_count(), 1)
      assert_false(rwlock.is_write_locked())
      
      // 再次获取读锁（应该成功）
      let read_lock2 = rwlock.read_lock()
      match read_lock2 {
        @azimuth.ReadLockResult::Acquired(read_guard2) => {
          assert_eq(rwlock.readers_count(), 2)
          assert_false(rwlock.is_write_locked())
          
          // 释放第一个读锁
          rwlock = read_guard.unlock()
          assert_eq(rwlock.readers_count(), 1)
          
          // 释放第二个读锁
          rwlock = read_guard2.unlock()
          assert_eq(rwlock.readers_count(), 0)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 获取写锁
  let write_lock = rwlock.write_lock()
  match write_lock {
    @azimuth.WriteLockResult::Acquired(write_guard) => {
      assert_eq(rwlock.readers_count(), 0)
      assert_true(rwlock.is_write_locked())
      
      // 尝试获取读锁（应该阻塞）
      let read_lock3 = rwlock.try_read_lock()
      match read_lock3 {
        @azimuth.ReadLockResult::WouldBlock => assert_true(true)
        _ => assert_true(false)
      }
      
      // 尝试获取写锁（应该阻塞）
      let write_lock2 = rwlock.try_write_lock()
      match write_lock2 {
        @azimuth.WriteLockResult::WouldBlock => assert_true(true)
        _ => assert_true(false)
      }
      
      // 释放写锁
      rwlock = write_guard.unlock()
      assert_eq(rwlock.readers_count(), 0)
      assert_false(rwlock.is_write_locked())
    }
    _ => assert_true(false)
  }
}

test "条件变量同步" {
  // 测试条件变量同步
  let mutex = @azimuth.Mutex::new()
  let condition = @azimuth.ConditionVariable::new()
  let shared_data = @azimuth.SharedData::new(0)
  
  // 生产者线程模拟
  let producer_result = {
    let lock_result = mutex.lock()
    match lock_result {
      @azimuth.LockResult::Acquired(lock_guard) => {
        // 修改共享数据
        shared_data = shared_data.set_value(42)
        
        // 通知等待的线程
        condition.notify_one()
        
        // 释放锁
        lock_guard.unlock()
        @azimuth.ThreadResult::Success
      }
      _ => @azimuth.ThreadResult::Failed("Failed to acquire lock")
    }
  }
  
  // 消费者线程模拟
  let consumer_result = {
    let lock_result = mutex.lock()
    match lock_result {
      @azimuth.LockResult::Acquired(lock_guard) => {
        // 等待条件满足
        let wait_result = condition.wait(lock_guard, fn() { shared_data.get_value() > 0 })
        
        match wait_result {
          @azimuth.WaitResult::Notified(new_guard) => {
            // 检查共享数据
            let value = shared_data.get_value()
            assert_eq(value, 42)
            
            // 释放锁
            new_guard.unlock()
            @azimuth.ThreadResult::Success
          }
          _ => @azimuth.ThreadResult::Failed("Wait failed")
        }
      }
      _ => @azimuth.ThreadResult::Failed("Failed to acquire lock")
    }
  }
  
  // 验证线程执行结果
  match producer_result {
    @azimuth.ThreadResult::Success => assert_true(true)
    _ => assert_true(false)
  }
  
  match consumer_result {
    @azimuth.ThreadResult::Success => assert_true(true)
    _ => assert_true(false)
  }
}

test "原子操作" {
  // 测试原子操作
  let atomic_int = @azimuth.AtomicInt::new(0)
  let atomic_bool = @azimuth.AtomicBool::new(false)
  
  // 验证初始值
  assert_eq(atomic_int.load(), 0)
  assert_false(atomic_bool.load())
  
  // 原子递增
  let old_value = atomic_int.fetch_add(5)
  assert_eq(old_value, 0)
  assert_eq(atomic_int.load(), 5)
  
  // 原子递减
  old_value = atomic_int.fetch_sub(2)
  assert_eq(old_value, 5)
  assert_eq(atomic_int.load(), 3)
  
  // 原子交换
  let old_bool = atomic_bool.swap(true)
  assert_false(old_bool)
  assert_true(atomic_bool.load())
  
  // 原子比较交换
  let cas_result = atomic_int.compare_exchange(3, 10)
  match cas_result {
    @azimuth.CASResult::Success(old) => {
      assert_eq(old, 3)
      assert_eq(atomic_int.load(), 10)
    }
    _ => assert_true(false)
  }
  
  // 失败的比较交换
  let cas_result2 = atomic_int.compare_exchange(5, 20)
  match cas_result2 {
    @azimuth.CASResult::Failed(old) => {
      assert_eq(old, 10)
      assert_eq(atomic_int.load(), 10)
    }
    _ => assert_true(false)
  }
  
  // 原位操作
  atomic_int.fetch_and(2) // 10 & 2 = 2
  assert_eq(atomic_int.load(), 2)
  
  atomic_int.fetch_or(1) // 2 | 1 = 3
  assert_eq(atomic_int.load(), 3)
  
  atomic_int.fetch_xor(3) // 3 ^ 3 = 0
  assert_eq(atomic_int.load(), 0)
}

test "线程池管理" {
  // 测试线程池管理
  let thread_pool = @azimuth.ThreadPool::new(4) // 4个线程
  
  // 验证线程池状态
  assert_eq(thread_pool.thread_count(), 4)
  assert_eq(thread_pool.active_count(), 0)
  assert_eq(thread_pool.task_count(), 0)
  
  // 提交任务
  let task1 = @azimuth.Task::new(fn() { 1 + 1 })
  let task2 = @azimuth.Task::new(fn() { 2 * 2 })
  let task3 = @azimuth.Task::new(fn() { 3 + 3 })
  
  let future1 = thread_pool.submit(task1)
  let future2 = thread_pool.submit(task2)
  let future3 = thread_pool.submit(task3)
  
  // 验证任务提交
  assert_eq(thread_pool.task_count(), 3)
  
  // 等待任务完成
  let result1 = future1.get()
  let result2 = future2.get()
  let result3 = future3.get()
  
  // 验证任务结果
  match result1 {
    @azimuth.FutureResult::Ready(value) => assert_eq(value, 2)
    _ => assert_true(false)
  }
  
  match result2 {
    @azimuth.FutureResult::Ready(value) => assert_eq(value, 4)
    _ => assert_true(false)
  }
  
  match result3 {
    @azimuth.FutureResult::Ready(value) => assert_eq(value, 6)
    _ => assert_true(false)
  }
  
  // 测试批量任务提交
  let tasks = []
  for i in 1..=10 {
    tasks = tasks.push(@azimuth.Task::new(fn() { i * i }))
  }
  
  let futures = thread_pool.submit_batch(tasks)
  assert_eq(futures.length(), 10)
  
  // 等待所有任务完成
  let results = []
  for future in futures {
    let result = future.get()
    match result {
      @azimuth.FutureResult::Ready(value) => results = results.push(value)
      _ => assert_true(false)
    }
  }
  
  // 验证批量任务结果
  assert_eq(results.length(), 10)
  assert_eq(results[0], 1) // 1*1
  assert_eq(results[4], 25) // 5*5
  assert_eq(results[9], 100) // 10*10
}

test "并发集合操作" {
  // 测试并发集合操作
  let concurrent_map = @azimuth.ConcurrentMap::new()
  let concurrent_queue = @azimuth.ConcurrentQueue::new()
  
  // 测试并发映射
  let insert_result1 = concurrent_map.insert("key1", "value1")
  assert_true(insert_result1)
  
  let insert_result2 = concurrent_map.insert("key2", "value2")
  assert_true(insert_result2)
  
  // 重复插入应该失败
  let insert_result3 = concurrent_map.insert("key1", "value1_new")
  assert_false(insert_result3)
  
  // 获取值
  match concurrent_map.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match concurrent_map.get("key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match concurrent_map.get("key3") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 更新值
  let update_result = concurrent_map.update("key1", "value1_updated")
  assert_true(update_result)
  
  match concurrent_map.get("key1") {
    Some(value) => assert_eq(value, "value1_updated")
    None => assert_true(false)
  }
  
  // 删除值
  let remove_result = concurrent_map.remove("key2")
  assert_true(remove_result)
  
  match concurrent_map.get("key2") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试并发队列
  assert_eq(concurrent_queue.size(), 0)
  assert_true(concurrent_queue.is_empty())
  
  // 入队
  concurrent_queue = concurrent_queue.enqueue("item1")
  concurrent_queue = concurrent_queue.enqueue("item2")
  concurrent_queue = concurrent_queue.enqueue("item3")
  
  assert_eq(concurrent_queue.size(), 3)
  assert_false(concurrent_queue.is_empty())
  
  // 出队
  let dequeue_result1 = concurrent_queue.dequeue()
  match dequeue_result1 {
    @azimuth.DequeueResult::Success(item) => assert_eq(item, "item1")
    _ => assert_true(false)
  }
  
  let dequeue_result2 = concurrent_queue.dequeue()
  match dequeue_result2 {
    @azimuth.DequeueResult::Success(item) => assert_eq(item, "item2")
    _ => assert_true(false)
  }
  
  assert_eq(concurrent_queue.size(), 1)
  
  // 查看队首元素
  match concurrent_queue.peek() {
    Some(item) => assert_eq(item, "item3")
    None => assert_true(false)
  }
  
  // 清空队列
  concurrent_queue = concurrent_queue.clear()
  assert_eq(concurrent_queue.size(), 0)
  assert_true(concurrent_queue.is_empty())
}

test "信号量控制" {
  // 测试信号量控制
  let semaphore = @azimuth.Semaphore::new(2) // 2个许可
  
  // 验证初始状态
  assert_eq(semaphore.available_permits(), 2)
  
  // 获取许可
  let acquire_result1 = semaphore.acquire()
  match acquire_result1 {
    @azimuth.SemaphoreResult::Acquired => assert_eq(semaphore.available_permits(), 1)
    _ => assert_true(false)
  }
  
  let acquire_result2 = semaphore.acquire()
  match acquire_result2 {
    @azimuth.SemaphoreResult::Acquired => assert_eq(semaphore.available_permits(), 0)
    _ => assert_true(false)
  }
  
  // 尝试获取许可（应该阻塞）
  let acquire_result3 = semaphore.try_acquire()
  match acquire_result3 {
    @azimuth.SemaphoreResult::WouldBlock => assert_true(true)
    _ => assert_true(false)
  }
  
  // 释放许可
  semaphore.release()
  assert_eq(semaphore.available_permits(), 1)
  
  // 现在应该可以获取许可
  let acquire_result4 = semaphore.try_acquire()
  match acquire_result4 {
    @azimuth.SemaphoreResult::Acquired => assert_eq(semaphore.available_permits(), 0)
    _ => assert_true(false)
  }
  
  // 释放所有许可
  semaphore.release()
  semaphore.release()
  assert_eq(semaphore.available_permits(), 2)
  
  // 测试批量获取
  let batch_acquire_result = semaphore.try_acquire(3)
  match batch_acquire_result {
    @azimuth.SemaphoreResult::WouldBlock => assert_true(true) // 只有2个许可，无法获取3个
    _ => assert_true(false)
  }
  
  let batch_acquire_result2 = semaphore.try_acquire(2)
  match batch_acquire_result2 {
    @azimuth.SemaphoreResult::Acquired => assert_eq(semaphore.available_permits(), 0)
    _ => assert_true(false)
  }
  
  // 批量释放
  semaphore.release(2)
  assert_eq(semaphore.available_permits(), 2)
}