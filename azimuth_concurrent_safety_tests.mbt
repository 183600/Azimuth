// Azimuth 并发安全测试用例
// 专注于测试系统在并发环境下的数据一致性和线程安全性

// 测试1: 并发计数器原子性
test "并发计数器原子性测试" {
  // 创建共享计数器
  let counter = AtomicCounter::new(0)
  let thread_count = 10
  let increments_per_thread = 1000
  
  // 创建多个线程并发增加计数器
  let threads = []
  for i = 0; i < thread_count; i = i + 1 {
    threads.push(Thread::spawn(|| {
      for j = 0; j < increments_per_thread; j = j + 1 {
        counter.increment()
      }
    }))
  }
  
  // 等待所有线程完成
  for thread in threads {
    thread.join()
  }
  
  // 验证最终计数值
  let expected_value = thread_count * increments_per_thread
  assert_eq(counter.get(), expected_value)
}

// 测试2: 并发集合操作安全性
test "并发集合操作安全性测试" {
  // 创建共享集合
  let shared_set = ConcurrentSet::new()
  let thread_count = 8
  let items_per_thread = 125
  
  // 多个线程并发添加元素
  let add_threads = []
  for i = 0; i < thread_count; i = i + 1 {
    let thread_id = i
    add_threads.push(Thread::spawn(move || {
      for j = 0; j < items_per_thread; j = j + 1 {
        let item = "item-" + (thread_id * items_per_thread + j).to_string()
        shared_set.add(item)
      }
    }))
  }
  
  // 等待添加线程完成
  for thread in add_threads {
    thread.join()
  }
  
  // 验证集合大小
  let expected_size = thread_count * items_per_thread
  assert_eq(shared_set.size(), expected_size)
  
  // 验证所有元素都存在
  for i = 0; i < thread_count; i = i + 1 {
    for j = 0; j < items_per_thread; j = j + 1 {
      let item = "item-" + (i * items_per_thread + j).to_string()
      assert_true(shared_set.contains(item))
    }
  }
  
  // 多个线程并发读取和删除元素
  let remove_threads = []
  let removed_items = ConcurrentSet::new()
  
  for i = 0; i < thread_count / 2; i = i + 1 {
    remove_threads.push(Thread::spawn(move || {
      for j = 0; j < items_per_thread / 2; j = j + 1 {
        let item = "item-" + (i * items_per_thread + j).to_string()
        if shared_set.remove(item) {
          removed_items.add(item)
        }
      }
    }))
  }
  
  // 等待删除线程完成
  for thread in remove_threads {
    thread.join()
  }
  
  // 验证删除操作
  let expected_removed = (thread_count / 2) * (items_per_thread / 2)
  assert_eq(removed_items.size(), expected_removed)
  assert_eq(shared_set.size(), expected_size - expected_removed)
}

// 测试3: 并发映射操作一致性
test "并发映射操作一致性测试" {
  // 创建共享映射
  let shared_map = ConcurrentMap::new()
  let thread_count = 6
  let operations_per_thread = 500
  
  // 初始化一些键值对
  for i = 0; i < 100; i = i + 1 {
    shared_map.put("key-" + i.to_string(), "initial-value-" + i.to_string())
  }
  
  // 多个线程并发更新映射
  let update_threads = []
  for i = 0; i < thread_count; i = i + 1 {
    let thread_id = i
    update_threads.push(Thread::spawn(move || {
      for j = 0; j < operations_per_thread; j = j + 1 {
        let key = "key-" + (j % 100).to_string()
        let new_value = "thread-" + thread_id.to_string() + "-value-" + j.to_string()
        
        // 使用原子更新操作
        shared_map.put(key, new_value)
        
        // 偶尔读取一些值
        if j % 10 == 0 {
          let read_key = "key-" + ((j + 50) % 100).to_string()
          let value = shared_map.get(read_key)
          match value {
            Some(v) => {
              // 验证值不为空
              assert_true(v.length() > 0)
            }
            None => {
              // 某些情况下可能还没有值
            }
          }
        }
      }
    }))
  }
  
  // 等待所有更新线程完成
  for thread in update_threads {
    thread.join()
  }
  
  // 验证映射一致性
  assert_eq(shared_map.size(), 100)
  
  // 验证所有键都有值
  for i = 0; i < 100; i = i + 1 {
    let key = "key-" + i.to_string()
    let value = shared_map.get(key)
    match value {
      Some(v) => {
        assert_true(v.length() > 0)
      }
      None => {
        assert_true(false)  // 所有键都应该有值
      }
    }
  }
}

// 测试4: 并发队列操作安全性
test "并发队列操作安全性测试" {
  // 创建共享队列
  let shared_queue = ConcurrentQueue::new()
  let producer_count = 4
  let consumer_count = 3
  let items_per_producer = 300
  
  // 生产者线程
  let producer_threads = []
  for i = 0; i < producer_count; i = i + 1 {
    let producer_id = i
    producer_threads.push(Thread::spawn(move || {
      for j = 0; j < items_per_producer; j = j + 1 {
        let item = "producer-" + producer_id.to_string() + "-item-" + j.to_string()
        shared_queue.enqueue(item)
        
        // 随机延迟，模拟真实场景
        if j % 50 == 0 {
          Thread::sleep(1)  // 1ms
        }
      }
    }))
  }
  
  // 消费者线程
  let consumed_items = ConcurrentSet::new()
  let consumer_threads = []
  
  for i = 0; i < consumer_count; i = i + 1 {
    let consumer_id = i
    consumer_threads.push(Thread::spawn(move || {
      let local_consumed = []
      
      // 持续消费直到队列空且所有生产者完成
      while true {
        match shared_queue.dequeue() {
          Some(item) => {
            local_consumed.push(item)
          }
          None => {
            // 队列为空，稍等后重试
            Thread::sleep(10)  // 10ms
            
            // 检查是否所有生产者都完成了
            // 在实际实现中，应该有更好的同步机制
            if shared_queue.is_empty() && local_consumed.length() > 0 {
              break
            }
          }
        }
      }
      
      // 将本地消费的项添加到全局集合
      for item in local_consumed {
        consumed_items.add(item)
      }
    }))
  }
  
  // 等待所有生产者完成
  for thread in producer_threads {
    thread.join()
  }
  
  // 等待所有消费者完成
  for thread in consumer_threads {
    thread.join()
  }
  
  // 验证消费结果
  let expected_total_items = producer_count * items_per_producer
  assert_eq(consumed_items.size(), expected_total_items)
  
  // 验证队列已空
  assert_true(shared_queue.is_empty())
}

// 测试5: 并发锁竞争和死锁预防
test "并发锁竞争和死锁预防测试" {
  // 创建多个锁和共享资源
  let lock1 = Mutex::new(0)
  let lock2 = Mutex::new(0)
  let lock3 = Mutex::new(0)
  
  let thread_count = 5
  let operations_per_thread = 100
  let results = ConcurrentMap::new()
  
  // 创建多个线程，每个线程以不同的顺序获取锁
  let threads = []
  for i = 0; i < thread_count; i = i + 1 {
    let thread_id = i
    threads.push(Thread::spawn(move || {
      let local_result = 0
      
      for j = 0; j < operations_per_thread; j = j + 1 {
        // 根据线程ID决定锁获取顺序，避免死锁
        if thread_id % 3 == 0 {
          // 顺序: lock1 -> lock2 -> lock3
          let guard1 = lock1.lock()
          let guard2 = lock2.lock()
          let guard3 = lock3.lock()
          
          // 执行操作
          local_result = local_result + 1
          *guard1 = *guard1 + 1
          *guard2 = *guard2 + 1
          *guard3 = *guard3 + 1
          
        } else if thread_id % 3 == 1 {
          // 顺序: lock2 -> lock3 -> lock1
          let guard2 = lock2.lock()
          let guard3 = lock3.lock()
          let guard1 = lock1.lock()
          
          // 执行操作
          local_result = local_result + 1
          *guard1 = *guard1 + 1
          *guard2 = *guard2 + 1
          *guard3 = *guard3 + 1
          
        } else {
          // 顺序: lock3 -> lock1 -> lock2
          let guard3 = lock3.lock()
          let guard1 = lock1.lock()
          let guard2 = lock2.lock()
          
          // 执行操作
          local_result = local_result + 1
          *guard1 = *guard1 + 1
          *guard2 = *guard2 + 1
          *guard3 = *guard3 + 1
        }
        
        // 短暂释放锁，允许其他线程执行
        if j % 10 == 0 {
          Thread::sleep(1)  // 1ms
        }
      }
      
      results.put("thread-" + thread_id.to_string(), local_result)
    }))
  }
  
  // 设置超时，防止死锁导致测试永远不结束
  let timeout_thread = Thread::spawn(|| {
    Thread::sleep(10000)  // 10秒超时
    assert_true(false, "测试超时，可能发生死锁")
  })
  
  // 等待所有线程完成
  for thread in threads {
    thread.join()
  }
  
  // 取消超时线程
  timeout_thread.cancel()
  
  // 验证结果
  assert_eq(results.size(), thread_count)
  
  // 验证每个线程的操作计数
  for i = 0; i < thread_count; i = i + 1 {
    let key = "thread-" + i.to_string()
    let value = results.get(key)
    match value {
      Some(count) => {
        assert_eq(count, operations_per_thread)
      }
      None => {
        assert_true(false)
      }
    }
  }
  
  // 验证锁保护的计数器
  let guard1 = lock1.lock()
  let guard2 = lock2.lock()
  let guard3 = lock3.lock()
  
  let expected_total = thread_count * operations_per_thread
  assert_eq(*guard1, expected_total)
  assert_eq(*guard2, expected_total)
  assert_eq(*guard3, expected_total)
}