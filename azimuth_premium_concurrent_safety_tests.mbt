// Azimuth 高级并发安全和线程安全测试用例
// 专注于高并发环境下的数据一致性和线程安全验证

// 测试1: 并发计数器原子操作
test "并发计数器原子操作测试" {
  let atomic_counter = AtomicCounter::new(0)
  let num_threads = 10
  let increments_per_thread = 1000
  
  // 创建多个线程并发增加计数器
  let threads = []
  for i in 0..num_threads {
    let thread = Thread::spawn(func() {
      for j in 0..increments_per_thread {
        AtomicCounter::increment(atomic_counter)
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证最终结果
  let expected_value = num_threads * increments_per_thread
  assert_eq(AtomicCounter::value(atomic_counter), expected_value)
}

// 测试2: 并发队列线程安全
test "并发队列线程安全测试" {
  let concurrent_queue = ConcurrentQueue::new(1000)
  let num_producers = 5
  let num_consumers = 3
  let items_per_producer = 200
  
  // 创建生产者线程
  let producer_threads = []
  for i in 0..num_producers {
    let thread = Thread::spawn(func() {
      for j in 0..items_per_producer {
        let item = "item_" + (i * items_per_producer + j).to_string()
        ConcurrentQueue::enqueue(concurrent_queue, item)
      }
    })
    producer_threads.push(thread)
  }
  
  // 创建消费者线程
  let consumed_items = AtomicCounter::new(0)
  let consumer_threads = []
  for i in 0..num_consumers {
    let thread = Thread::spawn(func() {
      let mut local_count = 0
      while local_count < (num_producers * items_per_producer) / num_consumers {
        match ConcurrentQueue::dequeue(concurrent_queue) {
          Some(item) => {
            local_count = local_count + 1
            AtomicCounter::increment(consumed_items)
          }
          None => {
            // 队列为空，短暂等待后重试
            Thread::sleep(Duration::from_millis(1))
          }
        }
      }
    })
    consumer_threads.push(thread)
  }
  
  // 等待所有生产者完成
  for thread in producer_threads {
    Thread::join(thread)
  }
  
  // 等待所有消费者完成
  for thread in consumer_threads {
    Thread::join(thread)
  }
  
  // 验证所有项目都被消费
  let expected_consumed = num_producers * items_per_producer
  assert_eq(AtomicCounter::value(consumed_items), expected_consumed)
}

// 测试3: 并发映射表操作
test "并发映射表操作测试" {
  let concurrent_map = ConcurrentMap::new()
  let num_threads = 8
  let operations_per_thread = 500
  
  // 创建并发操作线程
  let threads = []
  for i in 0..num_threads {
    let thread = Thread::spawn(func() {
      for j in 0..operations_per_thread {
        let key = "key_" + j.to_string()
        let value = "value_" + i.to_string() + "_" + j.to_string()
        
        // 插入操作
        ConcurrentMap::insert(concurrent_map, key, value)
        
        // 读取操作
        match ConcurrentMap::get(concurrent_map, key) {
          Some(v) => assert_eq(v, value)
          None => assert_true(false)
        }
        
        // 更新操作
        let new_value = value + "_updated"
        ConcurrentMap::insert(concurrent_map, key, new_value)
        
        // 删除操作（仅删除偶数键）
        if j % 2 == 0 {
          ConcurrentMap::remove(concurrent_map, key)
        }
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证映射表状态
  let remaining_keys = ConcurrentMap::keys(concurrent_map)
  for key in remaining_keys {
    let key_num = key.split("_")[1].to_int()
    assert_true(key_num % 2 == 1) // 只有奇数键应该保留
  }
}

// 测试4: 读写锁并发控制
test "读写锁并发控制测试" {
  let data = Ref::new(0)
  let rw_lock = RwLock::new()
  let num_readers = 10
  let num_writers = 3
  let operations_per_thread = 100
  
  // 创建读者线程
  let reader_threads = []
  let read_count = AtomicCounter::new(0)
  for i in 0..num_readers {
    let thread = Thread::spawn(func() {
      for j in 0..operations_per_thread {
        let guard = RwLock::read_lock(rw_lock)
        let current_value = Ref::get(data)
        assert_true(current_value >= 0)
        AtomicCounter::increment(read_count)
        RwLock::read_unlock(guard)
      }
    })
    reader_threads.push(thread)
  }
  
  // 创建写者线程
  let writer_threads = []
  for i in 0..num_writers {
    let thread = Thread::spawn(func() {
      for j in 0..operations_per_thread {
        let guard = RwLock::write_lock(rw_lock)
        let current_value = Ref::get(data)
        Ref::set(data, current_value + 1)
        RwLock::write_unlock(guard)
      }
    })
    writer_threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in reader_threads {
    Thread::join(thread)
  }
  for thread in writer_threads {
    Thread::join(thread)
  }
  
  // 验证最终状态
  let expected_writes = num_writers * operations_per_thread
  assert_eq(Ref::get(data), expected_writes)
  assert_eq(AtomicCounter::value(read_count), num_readers * operations_per_thread)
}

// 测试5: 并发条件变量同步
test "并发条件变量同步测试" {
  let mutex = Mutex::new()
  let condition = ConditionVariable::new()
  let shared_data = Ref::new(0)
  let num_producers = 3
  let num_consumers = 5
  let items_per_producer = 20
  
  // 创建生产者线程
  let producer_threads = []
  for i in 0..num_producers {
    let thread = Thread::spawn(func() {
      for j in 0..items_per_producer {
        let guard = Mutex::lock(mutex)
        
        // 等待缓冲区有空间
        while Ref::get(shared_data) >= 10 {
          ConditionVariable::wait(condition, guard)
          guard = Mutex::lock(mutex) // 重新获取锁
        }
        
        // 生产项目
        Ref::set(shared_data, Ref::get(shared_data) + 1)
        ConditionVariable::notify_all(condition)
        Mutex::unlock(guard)
      }
    })
    producer_threads.push(thread)
  }
  
  // 创建消费者线程
  let consumer_threads = []
  let consumed_count = AtomicCounter::new(0)
  for i in 0..num_consumers {
    let thread = Thread::spawn(func() {
      let mut local_consumed = 0
      while local_consumed < (num_producers * items_per_producer) / num_consumers {
        let guard = Mutex::lock(mutex)
        
        // 等待缓冲区有项目
        while Ref::get(shared_data) == 0 {
          ConditionVariable::wait(condition, guard)
          guard = Mutex::lock(mutex) // 重新获取锁
        }
        
        // 消费项目
        Ref::set(shared_data, Ref::get(shared_data) - 1)
        local_consumed = local_consumed + 1
        AtomicCounter::increment(consumed_count)
        ConditionVariable::notify_all(condition)
        Mutex::unlock(guard)
      }
    })
    consumer_threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in producer_threads {
    Thread::join(thread)
  }
  for thread in consumer_threads {
    Thread::join(thread)
  }
  
  // 验证所有项目都被消费
  assert_eq(Ref::get(shared_data), 0)
  assert_eq(AtomicCounter::value(consumed_count), num_producers * items_per_producer)
}

// 测试6: 并发原子引用计数
test "并发原子引用计数测试" {
  let shared_data = Arc::new("shared_resource")
  let num_threads = 8
  let clones_per_thread = 100
  
  // 创建并发克隆线程
  let threads = []
  let total_clones = AtomicCounter::new(0)
  
  for i in 0..num_threads {
    let thread = Thread::spawn(func() {
      let mut local_clones = []
      
      // 创建多个克隆
      for j in 0..clones_per_thread {
        let clone = Arc::clone(shared_data)
        local_clones.push(clone)
        AtomicCounter::increment(total_clones)
      }
      
      // 使用克隆
      for clone in local_clones {
        let value = Arc::get(clone)
        assert_eq(value, "shared_resource")
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证引用计数
  let expected_clones = num_threads * clones_per_thread
  assert_eq(AtomicCounter::value(total_clones), expected_clones)
}

// 测试7: 并发内存屏障
test "并发内存屏障测试" {
  let flag1 = AtomicBool::new(false)
  let flag2 = AtomicBool::new(false)
  let data1 = AtomicInt::new(0)
  let data2 = AtomicInt::new(0)
  
  // 线程1：设置数据1和标志1
  let thread1 = Thread::spawn(func() {
    AtomicInt::store(data1, 42)
    MemoryBarrier::store_store()
    AtomicBool::store(flag1, true)
  })
  
  // 线程2：等待标志1，设置数据2和标志2
  let thread2 = Thread::spawn(func() {
    while !AtomicBool::load(flag1) {
      Thread::yield()
    }
    MemoryBarrier::load_load()
    AtomicInt::store(data2, 84)
    MemoryBarrier::store_store()
    AtomicBool::store(flag2, true)
  })
  
  // 线程3：等待标志2，读取数据
  let thread3 = Thread::spawn(func() {
    while !AtomicBool::load(flag2) {
      Thread::yield()
    }
    MemoryBarrier::load_load()
    let value1 = AtomicInt::load(data1)
    let value2 = AtomicInt::load(data2)
    
    // 验证内存屏障确保的可见性
    assert_eq(value1, 42)
    assert_eq(value2, 84)
  })
  
  // 等待所有线程完成
  Thread::join(thread1)
  Thread::join(thread2)
  Thread::join(thread3)
}

// 测试8: 并发无锁数据结构
test "并发无锁数据结构测试" {
  let lock_free_stack = LockFreeStack::new()
  let num_threads = 6
  let items_per_thread = 1000
  
  // 创建并发推送线程
  let push_threads = []
  for i in 0..num_threads {
    let thread = Thread::spawn(func() {
      for j in 0..items_per_thread {
        let item = "item_" + i.to_string() + "_" + j.to_string()
        LockFreeStack::push(lock_free_stack, item)
      }
    })
    push_threads.push(thread)
  }
  
  // 等待所有推送线程完成
  for thread in push_threads {
    Thread::join(thread)
  }
  
  // 创建并发弹出线程
  let pop_threads = []
  let popped_count = AtomicCounter::new(0)
  for i in 0..num_threads {
    let thread = Thread::spawn(func() {
      let mut local_popped = 0
      while local_popped < items_per_thread {
        match LockFreeStack::pop(lock_free_stack) {
          Some(item) => {
            local_popped = local_popped + 1
            AtomicCounter::increment(popped_count)
          }
          None => {
            // 栈为空，检查是否所有项目都被弹出
            if AtomicCounter::value(popped_count) >= num_threads * items_per_thread {
              break
            }
            Thread::yield()
          }
        }
      }
    })
    pop_threads.push(thread)
  }
  
  // 等待所有弹出线程完成
  for thread in pop_threads {
    Thread::join(thread)
  }
  
  // 验证所有项目都被弹出
  let expected_popped = num_threads * items_per_thread
  assert_eq(AtomicCounter::value(popped_count), expected_popped)
  assert_true(LockFreeStack::is_empty(lock_free_stack))
}

// 测试9: 并发死锁检测
test "并发死锁检测测试" {
  let mutex1 = Mutex::new()
  let mutex2 = Mutex::new()
  let deadlock_detector = DeadlockDetector::new()
  
  // 线程1：按顺序获取锁
  let thread1 = Thread::spawn(func() {
    DeadlockDetector::register_thread(deadlock_detector, "thread1")
    
    let guard1 = Mutex::lock(mutex1)
    DeadlockDetector::acquire_lock(deadlock_detector, "mutex1")
    
    Thread::sleep(Duration::from_millis(10))
    
    let guard2 = Mutex::lock(mutex2)
    DeadlockDetector::acquire_lock(deadlock_detector, "mutex2")
    
    // 执行工作
    Thread::sleep(Duration::from_millis(10))
    
    DeadlockDetector::release_lock(deadlock_detector, "mutex2")
    Mutex::unlock(guard2)
    
    DeadlockDetector::release_lock(deadlock_detector, "mutex1")
    Mutex::unlock(guard1)
  })
  
  // 线程2：按相反顺序获取锁（可能导致死锁）
  let thread2 = Thread::spawn(func() {
    DeadlockDetector::register_thread(deadlock_detector, "thread2")
    
    let guard2 = Mutex::lock(mutex2)
    DeadlockDetector::acquire_lock(deadlock_detector, "mutex2")
    
    Thread::sleep(Duration::from_millis(10))
    
    // 检测潜在死锁
    if DeadlockDetector::would_deadlock(deadlock_detector, "mutex1") {
      // 避免死锁，释放已获取的锁
      DeadlockDetector::release_lock(deadlock_detector, "mutex2")
      Mutex::unlock(guard2)
      assert_true(true) // 成功避免死锁
    } else {
      let guard1 = Mutex::lock(mutex1)
      DeadlockDetector::acquire_lock(deadlock_detector, "mutex1")
      
      // 执行工作
      Thread::sleep(Duration::from_millis(10))
      
      DeadlockDetector::release_lock(deadlock_detector, "mutex1")
      Mutex::unlock(guard1)
      
      DeadlockDetector::release_lock(deadlock_detector, "mutex2")
      Mutex::unlock(guard2)
    }
  })
  
  // 等待所有线程完成
  Thread::join(thread1)
  Thread::join(thread2)
  
  // 验证没有发生死锁
  assert_false(DeadlockDetector::has_deadlock(deadlock_detector))
}

// 测试10: 并发性能压力测试
test "并发性能压力测试" {
  let concurrent_hash_map = ConcurrentHashMap::new()
  let num_threads = 12
  let operations_per_thread = 10000
  let key_range = 1000
  
  // 创建性能计时器
  let start_time = Time::now()
  
  // 创建并发操作线程
  let threads = []
  let operation_count = AtomicCounter::new(0)
  
  for i in 0..num_threads {
    let thread = Thread::spawn(func() {
      for j in 0..operations_per_thread {
        let key = (j % key_range).to_string()
        let value = "value_" + i.to_string() + "_" + j.to_string()
        
        // 随机操作：插入、读取、更新、删除
        let operation = j % 4
        match operation {
          0 => {
            // 插入
            ConcurrentHashMap::insert(concurrent_hash_map, key, value)
          }
          1 => {
            // 读取
            ConcurrentHashMap::get(concurrent_hash_map, key)
          }
          2 => {
            // 更新
            let new_value = value + "_updated"
            ConcurrentHashMap::insert(concurrent_hash_map, key, new_value)
          }
          3 => {
            // 删除
            ConcurrentHashMap::remove(concurrent_hash_map, key)
          }
          _ => {}
        }
        
        AtomicCounter::increment(operation_count)
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // 验证性能指标
  let total_operations = num_threads * operations_per_thread
  assert_eq(AtomicCounter::value(operation_count), total_operations)
  
  // 验证操作吞吐量（每秒操作数）
  let operations_per_second = total_operations.to_float() / duration.to_seconds()
  assert_true(operations_per_second > 10000.0) // 期望每秒至少10000次操作
  
  // 验证并发映射表的最终状态
  let final_size = ConcurrentHashMap::size(concurrent_hash_map)
  assert_true(final_size >= 0 && final_size <= key_range)
}