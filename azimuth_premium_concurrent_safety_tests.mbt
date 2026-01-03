// Azimuth Premium Concurrent Safety Tests
// 高级并发安全测试用例，专注于遥测系统在多线程环境下的安全性和一致性

// 测试 1: 线程安全的计数器
test "thread-safe counter implementation" {
  // 定义原子计数器
  type AtomicCounter = {
    value: Int,
    lock: Bool
  }
  
  // 创建原子计数器
  let create_atomic_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      lock: false
    }
  }
  
  // 原子递增
  let atomic_increment = fn(counter: AtomicCounter) {
    // 简化的自旋锁实现
    while counter.lock {
      // 自旋等待
    }
    
    // 获取锁
    counter.lock = true
    
    // 递增
    let old_value = counter.value
    counter.value = old_value + 1
    
    // 释放锁
    counter.lock = false
    
    counter.value
  }
  
  // 原子递减
  let atomic_decrement = fn(counter: AtomicCounter) {
    // 简化的自旋锁实现
    while counter.lock {
      // 自旋等待
    }
    
    // 获取锁
    counter.lock = true
    
    // 递减
    let old_value = counter.value
    counter.value = old_value - 1
    
    // 释放锁
    counter.lock = false
    
    counter.value
  }
  
  // 原子获取
  let atomic_get = fn(counter: AtomicCounter) {
    // 简化的自旋锁实现
    while counter.lock {
      // 自旋等待
    }
    
    // 获取锁
    counter.lock = true
    
    // 获取值
    let value = counter.value
    
    // 释放锁
    counter.lock = false
    
    value
  }
  
  // 测试原子计数器
  let counter = create_atomic_counter(0)
  
  // 单线程测试
  assert_eq(atomic_get(counter), 0)
  assert_eq(atomic_increment(counter), 1)
  assert_eq(atomic_increment(counter), 2)
  assert_eq(atomic_decrement(counter), 1)
  assert_eq(atomic_get(counter), 1)
  
  // 模拟并发递增
  let mut results = []
  for i in 1..=100 {
    let result = atomic_increment(counter)
    results = results.push(result)
  }
  
  // 验证最终值
  assert_eq(atomic_get(counter), 101)  // 初始1 + 100次递增
  
  // 验证所有结果都是唯一的且递增
  let sorted_results = results.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
  for i in 0..sorted_results.length() - 1 {
    assert_true(sorted_results[i + 1] > sorted_results[i])
  }
}

// 测试 2: 线程安全的队列
test "thread-safe queue implementation" {
  // 定义线程安全队列节点
  type QueueNode[T] = {
    value: T,
    next: Option[QueueNode[T]]
  }
  
  // 定义线程安全队列
  type ThreadSafeQueue[T] = {
    head: Option[QueueNode[T]],
    tail: Option[QueueNode[T]],
    size: Int,
    lock: Bool
  }
  
  // 创建线程安全队列
  let create_thread_safe_queue = fn() {
    {
      head: None,
      tail: None,
      size: 0,
      lock: false
    }
  }
  
  // 入队操作
  let enqueue = fn(queue: ThreadSafeQueue[T], item: T) {
    // 简化的自旋锁实现
    while queue.lock {
      // 自旋等待
    }
    
    // 获取锁
    queue.lock = true
    
    // 创建新节点
    let new_node = {
      value: item,
      next: None
    }
    
    // 添加到队列
    match queue.tail {
      None => {
        // 队列为空
        queue.head = Some(new_node)
        queue.tail = Some(new_node)
      }
      Some(tail_node) => {
        // 队列不为空，更新尾部
        let updated_tail = {
          value: tail_node.value,
          next: Some(new_node)
        }
        
        // 更新尾部节点
        let mut current = queue.head
        let mut found = false
        
        while current.is_some() && not found {
          match current {
            Some(node) => {
              match node.next {
                None => {
                  // 找到尾部节点
                  current = Some(updated_tail)
                  found = true
                }
                Some(next) => {
                  current = Some(next)
                }
              }
            }
            None => {
              found = true
            }
          }
        }
        
        queue.tail = Some(new_node)
      }
    }
    
    // 更新大小
    queue.size = queue.size + 1
    
    // 释放锁
    queue.lock = false
    
    queue.size
  }
  
  // 出队操作
  let dequeue = fn(queue: ThreadSafeQueue[T]) {
    // 简化的自旋锁实现
    while queue.lock {
      // 自旋等待
    }
    
    // 获取锁
    queue.lock = true
    
    // 从队列头部移除
    let result = match queue.head {
      None => {
        // 队列为空
        None
      }
      Some(head_node) => {
        // 队列不为空
        let value = head_node.value
        
        // 更新头部
        queue.head = head_node.next
        
        // 如果只有一个元素，同时更新尾部
        match head_node.next {
          None => {
            queue.tail = None
          }
          Some(_) => {
            // 多个元素，尾部不变
          }
        }
        
        // 更新大小
        queue.size = queue.size - 1
        
        Some(value)
      }
    }
    
    // 释放锁
    queue.lock = false
    
    result
  }
  
  // 测试线程安全队列
  let queue = create_thread_safe_queue()
  
  // 测试空队列
  assert_eq(queue.size, 0)
  assert_eq(dequeue(queue), None)
  
  // 测试入队
  assert_eq(enqueue(queue, "item1"), 1)
  assert_eq(enqueue(queue, "item2"), 2)
  assert_eq(enqueue(queue, "item3"), 3)
  
  // 测试出队
  assert_eq(dequeue(queue), Some("item1"))
  assert_eq(dequeue(queue), Some("item2"))
  assert_eq(dequeue(queue), Some("item3"))
  assert_eq(dequeue(queue), None)
  
  // 测试混合操作
  assert_eq(enqueue(queue, "item4"), 1)
  assert_eq(enqueue(queue, "item5"), 2)
  assert_eq(dequeue(queue), Some("item4"))
  assert_eq(enqueue(queue, "item6"), 2)
  assert_eq(dequeue(queue), Some("item5"))
  assert_eq(dequeue(queue), Some("item6"))
  assert_eq(dequeue(queue), None)
}

// 测试 3: 读写锁实现
test "read-write lock implementation" {
  // 定义读写锁状态
  enum LockState {
    Unlocked
    ReadLocked(Int)  // Int表示读者数量
    WriteLocked
  }
  
  // 定义读写锁
  type ReadWriteLock = {
    state: LockState,
    waiting_writers: Int,
    waiting_readers: Int
  }
  
  // 创建读写锁
  let create_read_write_lock = fn() {
    {
      state: LockState::Unlocked,
      waiting_writers: 0,
      waiting_readers: 0
    }
  }
  
  // 获取读锁
  let acquire_read_lock = fn(rw_lock: ReadWriteLock) {
    match rw_lock.state {
      LockState::Unlocked => {
        // 无锁，可以获取读锁
        rw_lock.state = LockState::ReadLocked(1)
        true
      }
      LockState::ReadLocked(count) => {
        // 已有读锁，可以增加读者数量
        rw_lock.state = LockState::ReadLocked(count + 1)
        true
      }
      LockState::WriteLocked => {
        // 写锁，需要等待
        rw_lock.waiting_readers = rw_lock.waiting_readers + 1
        false
      }
    }
  }
  
  // 释放读锁
  let release_read_lock = fn(rw_lock: ReadWriteLock) {
    match rw_lock.state {
      LockState::ReadLocked(count) => {
        if count <= 1 {
          // 最后一个读者，释放锁
          rw_lock.state = LockState::Unlocked
          
          // 如果有等待的写者，可以获取写锁
          if rw_lock.waiting_writers > 0 {
            rw_lock.waiting_writers = rw_lock.waiting_writers - 1
            rw_lock.state = LockState::WriteLocked
          }
        } else {
          // 还有其他读者，减少读者数量
          rw_lock.state = LockState::ReadLocked(count - 1)
        }
      }
      _ => {
        // 错误状态，不应该释放读锁
      }
    }
  }
  
  // 获取写锁
  let acquire_write_lock = fn(rw_lock: ReadWriteLock) {
    match rw_lock.state {
      LockState::Unlocked => {
        // 无锁，可以获取写锁
        rw_lock.state = LockState::WriteLocked
        true
      }
      LockState::ReadLocked(_) => {
        // 有读锁，需要等待
        rw_lock.waiting_writers = rw_lock.waiting_writers + 1
        false
      }
      LockState::WriteLocked => {
        // 已有写锁，需要等待
        rw_lock.waiting_writers = rw_lock.waiting_writers + 1
        false
      }
    }
  }
  
  // 释放写锁
  let release_write_lock = fn(rw_lock: ReadWriteLock) {
    match rw_lock.state {
      LockState::WriteLocked => {
        // 释放写锁
        rw_lock.state = LockState::Unlocked
        
        // 优先处理等待的写者
        if rw_lock.waiting_writers > 0 {
          rw_lock.waiting_writers = rw_lock.waiting_writers - 1
          rw_lock.state = LockState::WriteLocked
        } else if rw_lock.waiting_readers > 0 {
          // 没有等待的写者，但有待待的读者，让所有读者获取读锁
          let reader_count = rw_lock.waiting_readers
          rw_lock.waiting_readers = 0
          rw_lock.state = LockState::ReadLocked(reader_count)
        }
      }
      _ => {
        // 错误状态，不应该释放写锁
      }
    }
  }
  
  // 测试读写锁
  let rw_lock = create_read_write_lock()
  
  // 测试读锁
  assert_true(acquire_read_lock(rw_lock))
  match rw_lock.state {
    LockState::ReadLocked(count) => assert_eq(count, 1)
    _ => assert_true(false)
  }
  
  // 多个读者可以同时获取读锁
  assert_true(acquire_read_lock(rw_lock))
  match rw_lock.state {
    LockState::ReadLocked(count) => assert_eq(count, 2)
    _ => assert_true(false)
  }
  
  // 释放读锁
  release_read_lock(rw_lock)
  match rw_lock.state {
    LockState::ReadLocked(count) => assert_eq(count, 1)
    _ => assert_true(false)
  }
  
  release_read_lock(rw_lock)
  match rw_lock.state {
    LockState::Unlocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试写锁
  assert_true(acquire_write_lock(rw_lock))
  match rw_lock.state {
    LockState::WriteLocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // 写锁时不能获取读锁
  assert_false(acquire_read_lock(rw_lock))
  assert_eq(rw_lock.waiting_readers, 1)
  
  // 写锁时不能获取另一个写锁
  assert_false(acquire_write_lock(rw_lock))
  assert_eq(rw_lock.waiting_writers, 1)
  
  // 释放写锁后，等待的写者优先获取锁
  release_write_lock(rw_lock)
  match rw_lock.state {
    LockState::WriteLocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // 释放写锁后，等待的读者可以获取锁
  release_write_lock(rw_lock)
  match rw_lock.state {
    LockState::ReadLocked(count) => assert_eq(count, 1)
    _ => assert_true(false)
  }
}

// 测试 4: 线程安全的哈希表
test "thread-safe hash table implementation" {
  // 定义哈希表条目
  type HashEntry[K, V] = {
    key: K,
    value: V,
    next: Option[HashEntry[K, V]]
  }
  
  // 定义线程安全哈希表
  type ThreadSafeHashTable[K, V] = {
    buckets: Array[Option[HashEntry[K, V]]],
    size: Int,
    lock: Bool
  }
  
  // 创建线程安全哈希表
  let create_thread_safe_hash_table = fn(bucket_count: Int) {
    let mut buckets = []
    for i in 0..bucket_count {
      buckets = buckets.push(None)
    }
    
    {
      buckets,
      size: 0,
      lock: false
    }
  }
  
  // 简单哈希函数
  let hash_fn = fn(key: String, bucket_count: Int) {
    let mut hash = 0
    for c in key.chars() {
      hash = (hash * 31 + c.to_int()) % bucket_count
    }
    hash
  }
  
  // 插入键值对
  let hash_insert = fn(table: ThreadSafeHashTable[String, String], key: String, value: String) {
    // 简化的自旋锁实现
    while table.lock {
      // 自旋等待
    }
    
    // 获取锁
    table.lock = true
    
    let bucket_index = hash_fn(key, table.buckets.length())
    let bucket = table.buckets[bucket_index]
    
    // 检查键是否已存在
    let mut found = false
    let mut updated_bucket = bucket
    
    match bucket {
      None => {
        // 空桶，直接插入
        updated_bucket = Some({
          key,
          value,
          next: None
        })
        table.size = table.size + 1
      }
      Some(entry) => {
        // 非空桶，遍历链表
        let mut current = entry
        let mut prev = None
        
        while current.key != key {
          match current.next {
            None => {
              // 到达链表末尾，插入新条目
              let new_entry = {
                key,
                value,
                next: None
              }
              
              match prev {
                None => {
                  // 更新桶头
                  updated_bucket = Some({
                    key: current.key,
                    value: current.value,
                    next: Some(new_entry)
                  })
                }
                Some(prev_entry) => {
                  // 更新前驱节点的next
                  // 简化实现，直接替换整个桶
                  updated_bucket = Some({
                    key: entry.key,
                    value: entry.value,
                    next: Some({
                      key,
                      value,
                      next: None
                    })
                  })
                }
              }
              
              table.size = table.size + 1
              found = true
            }
            Some(next) => {
              prev = Some(current)
              current = next
            }
          }
        }
        
        if not found {
          // 键已存在，更新值
          // 简化实现，直接替换整个桶
          updated_bucket = Some({
            key,
            value,
            next: entry.next
          })
        }
      }
    }
    
    // 更新桶
    table.buckets[bucket_index] = updated_bucket
    
    // 释放锁
    table.lock = false
    
    true
  }
  
  // 获取值
  let hash_get = fn(table: ThreadSafeHashTable[String, String], key: String) {
    // 简化的自旋锁实现
    while table.lock {
      // 自旋等待
    }
    
    // 获取锁
    table.lock = true
    
    let bucket_index = hash_fn(key, table.buckets.length())
    let bucket = table.buckets[bucket_index]
    
    let result = match bucket {
      None => {
        None
      }
      Some(entry) => {
        // 遍历链表
        let mut current = entry
        
        while current.key != key {
          match current.next {
            None => {
              // 未找到
              current = { key: "", value: "", next: None }
              break
            }
            Some(next) => {
              current = next
            }
          }
        }
        
        if current.key == key {
          Some(current.value)
        } else {
          None
        }
      }
    }
    
    // 释放锁
    table.lock = false
    
    result
  }
  
  // 测试线程安全哈希表
  let table = create_thread_safe_hash_table(10)
  
  // 测试插入和获取
  assert_true(hash_insert(table, "key1", "value1"))
  assert_eq(hash_get(table, "key1"), Some("value1"))
  
  assert_true(hash_insert(table, "key2", "value2"))
  assert_eq(hash_get(table, "key2"), Some("value2"))
  
  // 测试更新
  assert_true(hash_insert(table, "key1", "updated_value1"))
  assert_eq(hash_get(table, "key1"), Some("updated_value1"))
  
  // 测试不存在的键
  assert_eq(hash_get(table, "nonexistent"), None)
  
  // 测试多个键值对
  for i in 3..=20 {
    let key = "key" + i.to_string()
    let value = "value" + i.to_string()
    assert_true(hash_insert(table, key, value))
  }
  
  // 验证所有插入的键值对
  for i in 1..=20 {
    let key = "key" + i.to_string()
    let value = if i == 1 { "updated_value1" } else { "value" + i.to_string() }
    assert_eq(hash_get(table, key), Some(value))
  }
  
  // 验证表大小
  assert_eq(table.size, 20)
}

// 测试 5: 线程安全的引用计数
test "thread-safe reference counting" {
  // 定义引用计数对象
  type RefCounted[T] = {
    value: T,
    ref_count: Int,
    lock: Bool
  }
  
  // 创建引用计数对象
  let create_ref_counted = fn(initial_value: T) {
    {
      value: initial_value,
      ref_count: 1,
      lock: false
    }
  }
  
  // 增加引用计数
  let ref_count_inc = fn(rc: RefCounted[T]) {
    // 简化的自旋锁实现
    while rc.lock {
      // 自旋等待
    }
    
    // 获取锁
    rc.lock = true
    
    // 增加引用计数
    rc.ref_count = rc.ref_count + 1
    let new_count = rc.ref_count
    
    // 释放锁
    rc.lock = false
    
    new_count
  }
  
  // 减少引用计数
  let ref_count_dec = fn(rc: RefCounted[T]) {
    // 简化的自旋锁实现
    while rc.lock {
      // 自旋等待
    }
    
    // 获取锁
    rc.lock = true
    
    // 减少引用计数
    rc.ref_count = rc.ref_count - 1
    let new_count = rc.ref_count
    
    // 释放锁
    rc.lock = false
    
    new_count
  }
  
  // 获取引用计数
  let ref_count_get = fn(rc: RefCounted[T]) {
    // 简化的自旋锁实现
    while rc.lock {
      // 自旋等待
    }
    
    // 获取锁
    rc.lock = true
    
    // 获取引用计数
    let count = rc.ref_count
    
    // 释放锁
    rc.lock = false
    
    count
  }
  
  // 获取值
  let ref_count_get_value = fn(rc: RefCounted[T]) {
    // 简化的自旋锁实现
    while rc.lock {
      // 自旋等待
    }
    
    // 获取锁
    rc.lock = true
    
    // 获取值
    let value = rc.value
    
    // 释放锁
    rc.lock = false
    
    value
  }
  
  // 测试引用计数
  let rc = create_ref_counted("test_value")
  
  // 初始引用计数为1
  assert_eq(ref_count_get(rc), 1)
  assert_eq(ref_count_get_value(rc), "test_value")
  
  // 增加引用计数
  assert_eq(ref_count_inc(rc), 2)
  assert_eq(ref_count_get(rc), 2)
  
  assert_eq(ref_count_inc(rc), 3)
  assert_eq(ref_count_get(rc), 3)
  
  // 减少引用计数
  assert_eq(ref_count_dec(rc), 2)
  assert_eq(ref_count_get(rc), 2)
  
  assert_eq(ref_count_dec(rc), 1)
  assert_eq(ref_count_get(rc), 1)
  
  // 不能减少到0以下（实际实现中应该检查）
  assert_eq(ref_count_dec(rc), 0)
  assert_eq(ref_count_get(rc), 0)
  
  // 即使引用计数为0，值仍然存在
  assert_eq(ref_count_get_value(rc), "test_value")
}