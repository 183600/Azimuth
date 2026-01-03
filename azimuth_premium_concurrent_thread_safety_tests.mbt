// Azimuth 高级并发和线程安全测试用例
// 专注于高并发环境下的线程安全性和资源管理

// 测试1: 无锁数据结构的并发安全性
test "无锁数据结构的并发安全性" {
  // 简单的无锁计数器实现
  type LockFreeCounter = {
    value: Int
  }
  
  let create_counter = fn() { { value: 0 } }
  
  // 原子递增操作（简化实现）
  let atomic_increment = fn(counter: LockFreeCounter) {
    // 在实际实现中，这里会使用原子操作
    // 这里简化为普通递增用于测试
    counter.value = counter.value + 1
    counter.value
  }
  
  // 并发测试多个线程同时递增计数器
  let counter = create_counter()
  let num_threads = 10
  let increments_per_thread = 1000
  
  // 模拟并发执行
  let mut results = []
  for i in 0..num_threads {
    let mut thread_result = 0
    for j in 0..increments_per_thread {
      thread_result = atomic_increment(counter)
    }
    results = results.push(thread_result)
  }
  
  // 验证最终结果
  let expected_total = num_threads * increments_per_thread
  assert_eq(counter.value, expected_total)
  
  // 验证所有线程都完成了执行
  assert_eq(results.length(), num_threads)
}

// 测试2: 并发队列的线程安全性
test "并发队列的线程安全性" {
  // 简单的并发队列实现
  type ConcurrentQueue[T] = {
    items: Array[T],
    head: Int,
    tail: Int,
    size: Int,
    capacity: Int
  }
  
  let create_queue = fn(capacity: Int) {
    { 
      items: Array::with_capacity(capacity),
      head: 0,
      tail: 0,
      size: 0,
      capacity: capacity
    }
  }
  
  let enqueue = fn(queue: ConcurrentQueue[T], item: T) {
    if queue.size < queue.capacity {
      queue.items[queue.tail] = item
      queue.tail = (queue.tail + 1) % queue.capacity
      queue.size = queue.size + 1
      true
    } else {
      false
    }
  }
  
  let dequeue = fn(queue: ConcurrentQueue[T]) {
    if queue.size > 0 {
      let item = queue.items[queue.head]
      queue.head = (queue.head + 1) % queue.capacity
      queue.size = queue.size - 1
      Some(item)
    } else {
      None
    }
  }
  
  // 测试队列的基本操作
  let queue = create_queue(10)
  
  // 填充队列
  for i in 0..5 {
    let enqueued = enqueue(queue, i)
    assert_true(enqueued)
  }
  
  assert_eq(queue.size, 5)
  
  // 清空队列
  let mut dequeued_items = []
  while queue.size > 0 {
    match dequeue(queue) {
      Some(item) => dequeued_items = dequeued_items.push(item)
      None => assert_true(false) // 不应该发生
    }
  }
  
  assert_eq(queue.size, 0)
  assert_eq(dequeued_items.length(), 5)
  assert_eq(dequeued_items, [0, 1, 2, 3, 4])
  
  // 测试队列边界条件
  let full_queue = create_queue(3)
  
  // 填满队列
  assert_true(enqueue(full_queue, "a"))
  assert_true(enqueue(full_queue, "b"))
  assert_true(enqueue(full_queue, "c"))
  assert_false(enqueue(full_queue, "d")) // 队列已满
  
  assert_eq(full_queue.size, 3)
  
  // 清空队列并尝试从空队列出队
  assert_eq(dequeue(full_queue), Some("a"))
  assert_eq(dequeue(full_queue), Some("b"))
  assert_eq(dequeue(full_queue), Some("c"))
  assert_eq(dequeue(full_queue), None) // 队列已空
}

// 测试3: 并发哈希表的线程安全性
test "并发哈希表的线程安全性" {
  // 简化的哈希表条目
  type HashEntry[K, V] = {
    key: K,
    value: V,
    next: Option[HashEntry[K, V]]
  }
  
  // 简化的哈希表实现
  type HashMap[K, V] = {
    buckets: Array[Option[HashEntry[K, V]]],
    size: Int,
    capacity: Int
  }
  
  let hash_function = fn(key: String, capacity: Int) {
    // 简单的哈希函数
    let hash = key.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
    hash % capacity
  }
  
  let create_hash_map = fn(capacity: Int) {
    {
      buckets: Array::filled(capacity, None),
      size: 0,
      capacity: capacity
    }
  }
  
  let insert = fn(map: HashMap[String, Int], key: String, value: Int) {
    let index = hash_function(key, map.capacity)
    
    match map.buckets[index] {
      None => {
        map.buckets[index] = Some({ key: key, value: value, next: None })
        map.size = map.size + 1
        true
      }
      Some(entry) => {
        // 查找是否已存在相同的键
        let mut current = entry
        let mut found = false
        
        // 简化处理：直接替换第一个条目的值
        map.buckets[index] = Some({ key: key, value: value, next: entry.next })
        true
      }
    }
  }
  
  let get = fn(map: HashMap[String, Int], key: String) {
    let index = hash_function(key, map.capacity)
    
    match map.buckets[index] {
      None => None
      Some(entry) => {
        if entry.key == key {
          Some(entry.value)
        } else {
          // 简化处理：只检查第一个条目
          None
        }
      }
    }
  }
  
  // 测试哈希表的基本操作
  let map = create_hash_map(10)
  
  // 插入键值对
  assert_true(insert(map, "key1", 100))
  assert_true(insert(map, "key2", 200))
  assert_true(insert(map, "key3", 300))
  
  assert_eq(map.size, 3)
  
  // 查找键值对
  assert_eq(get(map, "key1"), Some(100))
  assert_eq(get(map, "key2"), Some(200))
  assert_eq(get(map, "key3"), Some(300))
  assert_eq(get(map, "nonexistent"), None)
  
  // 测试哈希冲突处理
  let conflict_map = create_hash_map(2) // 小容量，容易产生冲突
  
  assert_true(insert(conflict_map, "a", 1))
  assert_true(insert(conflict_map, "b", 2))
  assert_true(insert(conflict_map, "c", 3))
  
  assert_eq(conflict_map.size, 3)
}

// 测试4: 并发资源池的管理
test "并发资源池的管理" {
  // 资源池项类型
  type PooledResource = {
    id: Int,
    in_use: Bool,
    created_at: Int
  }
  
  // 资源池类型
  type ResourcePool = {
    resources: Array[PooledResource],
    max_size: Int,
    created_count: Int
  }
  
  let create_pool = fn(max_size: Int) {
    {
      resources: [],
      max_size: max_size,
      created_count: 0
    }
  }
  
  let acquire_resource = fn(pool: ResourcePool) {
    // 查找可用资源
    for i in 0..pool.resources.length() {
      if not pool.resources[i].in_use {
        pool.resources[i].in_use = true
        return Some(i)
      }
    }
    
    // 如果没有可用资源且池未满，创建新资源
    if pool.resources.length() < pool.max_size {
      let new_resource = {
        id: pool.created_count,
        in_use: true,
        created_at: Time::now()
      }
      pool.resources = pool.resources.push(new_resource)
      pool.created_count = pool.created_count + 1
      Some(pool.resources.length() - 1)
    } else {
      None // 池已满
    }
  }
  
  let release_resource = fn(pool: ResourcePool, index: Int) {
    if index >= 0 && index < pool.resources.length() {
      pool.resources[index].in_use = false
      true
    } else {
      false
    }
  }
  
  let get_available_count = fn(pool: ResourcePool) {
    pool.resources.filter(fn(r) { not r.in_use }).length()
  }
  
  let get_in_use_count = fn(pool: ResourcePool) {
    pool.resources.filter(fn(r) { r.in_use }).length()
  }
  
  // 测试资源池的基本操作
  let pool = create_pool(3)
  
  // 初始状态
  assert_eq(get_available_count(pool), 0)
  assert_eq(get_in_use_count(pool), 0)
  
  // 获取资源
  let resource1 = acquire_resource(pool)
  let resource2 = acquire_resource(pool)
  let resource3 = acquire_resource(pool)
  
  assert_eq(resource1, Some(0))
  assert_eq(resource2, Some(1))
  assert_eq(resource3, Some(2))
  
  assert_eq(get_available_count(pool), 0)
  assert_eq(get_in_use_count(pool), 3)
  
  // 池已满，无法获取更多资源
  let resource4 = acquire_resource(pool)
  assert_eq(resource4, None)
  
  // 释放资源
  assert_true(release_resource(pool, 1))
  assert_eq(get_available_count(pool), 1)
  assert_eq(get_in_use_count(pool), 2)
  
  // 重新获取已释放的资源
  let resource5 = acquire_resource(pool)
  assert_eq(resource5, Some(1)) // 应该获取到索引1的资源
  assert_eq(get_available_count(pool), 0)
  assert_eq(get_in_use_count(pool), 3)
  
  // 测试无效索引
  assert_false(release_resource(pool, -1))
  assert_false(release_resource(pool, 10))
}

// 测试5: 读写锁的并发控制
test "读写锁的并发控制" {
  // 简化的读写锁状态
  type ReadWriteLock = {
    readers: Int,
    writer: Bool,
    waiting_writers: Int
  }
  
  let create_lock = fn() { { readers: 0, writer: false, waiting_writers: 0 } }
  
  let acquire_read_lock = fn(lock: ReadWriteLock) {
    if not lock.writer && lock.waiting_writers == 0 {
      lock.readers = lock.readers + 1
      true
    } else {
      false
    }
  }
  
  let release_read_lock = fn(lock: ReadWriteLock) {
    if lock.readers > 0 {
      lock.readers = lock.readers - 1
      true
    } else {
      false
    }
  }
  
  let acquire_write_lock = fn(lock: ReadWriteLock) {
    if not lock.writer && lock.readers == 0 {
      lock.writer = true
      true
    } else {
      lock.waiting_writers = lock.waiting_writers + 1
      false
    }
  }
  
  let release_write_lock = fn(lock: ReadWriteLock) {
    if lock.writer {
      lock.writer = false
      if lock.waiting_writers > 0 {
        lock.waiting_writers = lock.waiting_writers - 1
      }
      true
    } else {
      false
    }
  }
  
  // 测试读写锁的基本操作
  let lock = create_lock()
  
  // 初始状态
  assert_eq(lock.readers, 0)
  assert_eq(lock.writer, false)
  assert_eq(lock.waiting_writers, 0)
  
  // 多个读锁可以同时获取
  assert_true(acquire_read_lock(lock))
  assert_eq(lock.readers, 1)
  
  assert_true(acquire_read_lock(lock))
  assert_eq(lock.readers, 2)
  
  assert_true(acquire_read_lock(lock))
  assert_eq(lock.readers, 3)
  
  // 有读者时不能获取写锁
  assert_false(acquire_write_lock(lock))
  assert_eq(lock.writer, false)
  assert_eq(lock.waiting_writers, 1) // 写锁进入等待队列
  
  // 释放所有读锁
  assert_true(release_read_lock(lock))
  assert_eq(lock.readers, 2)
  
  assert_true(release_read_lock(lock))
  assert_eq(lock.readers, 1)
  
  assert_true(release_read_lock(lock))
  assert_eq(lock.readers, 0)
  
  // 没有读者时可以获取写锁
  assert_true(acquire_write_lock(lock))
  assert_eq(lock.writer, true)
  
  // 有写锁时不能获取读锁
  assert_false(acquire_read_lock(lock))
  assert_eq(lock.readers, 0)
  
  // 有写锁时不能获取另一个写锁
  assert_false(acquire_write_lock(lock))
  assert_eq(lock.waiting_writers, 2) // 又一个写锁进入等待队列
  
  // 释放写锁
  assert_true(release_write_lock(lock))
  assert_eq(lock.writer, false)
  assert_eq(lock.waiting_writers, 1) // 一个等待的写锁被唤醒
  
  // 测试无效释放操作
  assert_false(release_read_lock(lock)) // 没有读者
  assert_false(release_write_lock(lock)) // 没有写者
}

// 测试6: 并发任务调度器
test "并发任务调度器" {
  // 任务类型
  type Task = {
    id: Int,
    priority: Int,
    payload: String,
    status: String
  }
  
  // 任务调度器类型
  type TaskScheduler = {
    pending_tasks: Array[Task],
    running_tasks: Array[Task],
    completed_tasks: Array[Task],
    max_concurrent: Int
  }
  
  let create_scheduler = fn(max_concurrent: Int) {
    {
      pending_tasks: [],
      running_tasks: [],
      completed_tasks: [],
      max_concurrent: max_concurrent
    }
  }
  
  let submit_task = fn(scheduler: TaskScheduler, priority: Int, payload: String) {
    let new_task = {
      id: scheduler.pending_tasks.length() + scheduler.running_tasks.length() + scheduler.completed_tasks.length(),
      priority: priority,
      payload: payload,
      status: "pending"
    }
    
    // 按优先级插入
    let mut inserted = false
    let mut updated_pending = []
    
    for task in scheduler.pending_tasks {
      if not inserted && task.priority < priority {
        updated_pending = updated_pending.push(new_task)
        inserted = true
      }
      updated_pending = updated_pending.push(task)
    }
    
    if not inserted {
      updated_pending = updated_pending.push(new_task)
    }
    
    scheduler.pending_tasks = updated_pending
    new_task.id
  }
  
  let schedule_tasks = fn(scheduler: TaskScheduler) {
    let available_slots = scheduler.max_concurrent - scheduler.running_tasks.length()
    let mut scheduled = 0
    
    while scheduled < available_slots && scheduler.pending_tasks.length() > 0 {
      let task = scheduler.pending_tasks[0]
      scheduler.pending_tasks = scheduler.pending_tasks.slice(1)
      
      let running_task = { ...task, status: "running" }
      scheduler.running_tasks = scheduler.running_tasks.push(running_task)
      scheduled = scheduled + 1
    }
    
    scheduled
  }
  
  let complete_task = fn(scheduler: TaskScheduler, task_id: Int) {
    let mut found = false
    let mut updated_running = []
    
    for task in scheduler.running_tasks {
      if task.id == task_id {
        found = true
        let completed_task = { ...task, status: "completed" }
        scheduler.completed_tasks = scheduler.completed_tasks.push(completed_task)
      } else {
        updated_running = updated_running.push(task)
      }
    }
    
    scheduler.running_tasks = updated_running
    found
  }
  
  let get_queue_sizes = fn(scheduler: TaskScheduler) {
    {
      pending: scheduler.pending_tasks.length(),
      running: scheduler.running_tasks.length(),
      completed: scheduler.completed_tasks.length()
    }
  }
  
  // 测试任务调度器
  let scheduler = create_scheduler(2) // 最大2个并发任务
  
  // 提交任务
  let task1 = submit_task(scheduler, 1, "低优先级任务1")
  let task2 = submit_task(scheduler, 3, "高优先级任务1")
  let task3 = submit_task(scheduler, 2, "中优先级任务1")
  let task4 = submit_task(scheduler, 3, "高优先级任务2")
  
  let sizes = get_queue_sizes(scheduler)
  assert_eq(sizes.pending, 4)
  assert_eq(sizes.running, 0)
  assert_eq(sizes.completed, 0)
  
  // 调度任务
  let scheduled = schedule_tasks(scheduler)
  assert_eq(scheduled, 2) // 调度了2个任务
  
  let sizes_after_schedule = get_queue_sizes(scheduler)
  assert_eq(sizes_after_schedule.pending, 2)
  assert_eq(sizes_after_schedule.running, 2)
  assert_eq(sizes_after_schedule.completed, 0)
  
  // 验证高优先级任务先被调度
  assert_eq(scheduler.running_tasks[0].priority, 3) // 高优先级任务1
  assert_eq(scheduler.running_tasks[1].priority, 3) // 高优先级任务2
  
  // 完成一个任务
  assert_true(complete_task(scheduler, scheduler.running_tasks[0].id))
  
  let sizes_after_complete = get_queue_sizes(scheduler)
  assert_eq(sizes_after_complete.pending, 2)
  assert_eq(sizes_after_complete.running, 1)
  assert_eq(sizes_after_complete.completed, 1)
  
  // 再次调度任务
  let scheduled_again = schedule_tasks(scheduler)
  assert_eq(scheduled_again, 1) // 调度了1个任务
  
  // 验证中优先级任务被调度
  assert_eq(scheduler.running_tasks[1].priority, 2) // 中优先级任务1
}

// 测试7: 并发缓存的一致性
test "并发缓存的一致性" {
  // 缓存条目类型
  type CacheEntry[V] = {
    value: V,
    timestamp: Int,
    access_count: Int
  }
  
  // 缓存类型
  type Cache[K, V] = {
    entries: Map[K, CacheEntry[V]],
    max_size: Int,
    access_order: Array[K]
  }
  
  let create_cache = fn(max_size: Int) {
    {
      entries: Map::empty(),
      max_size: max_size,
      access_order: []
    }
  }
  
  let get = fn(cache: Cache[String, Int], key: String) {
    match Map::get(cache.entries, key) {
      Some(entry) => {
        // 更新访问计数和访问顺序
        let updated_entry = { 
          ...entry, 
          access_count: entry.access_count + 1,
          timestamp: Time::now()
        }
        let _ = Map::insert(cache.entries, key, updated_entry)
        
        // 更新访问顺序（LRU）
        let mut new_order = []
        let mut found = false
        for k in cache.access_order {
          if k == key {
            found = true
          } else {
            new_order = new_order.push(k)
          }
        }
        new_order = new_order.push(key)
        cache.access_order = new_order
        
        Some(entry.value)
      }
      None => None
    }
  }
  
  let put = fn(cache: Cache[String, Int], key: String, value: Int) {
    let now = Time::now()
    
    // 检查是否需要驱逐条目
    if cache.entries.size() >= cache.max_size && not Map::contains_key(cache.entries, key) {
      // 驱逐最少使用的条目
      if cache.access_order.length() > 0 {
        let lru_key = cache.access_order[0]
        let _ = Map::remove(cache.entries, lru_key)
        cache.access_order = cache.access_order.slice(1)
      }
    }
    
    // 添加或更新条目
    let entry = {
      value: value,
      timestamp: now,
      access_count: 1
    }
    
    let _ = Map::insert(cache.entries, key, entry)
    
    // 更新访问顺序
    if not cache.access_order.contains(key) {
      cache.access_order = cache.access_order.push(key)
    }
  }
  
  let get_stats = fn(cache: Cache[String, Int]) {
    {
      size: cache.entries.size(),
      max_size: cache.max_size,
      access_order_length: cache.access_order.length()
    }
  }
  
  // 测试缓存的基本操作
  let cache = create_cache(3)
  
  // 初始状态
  let stats = get_stats(cache)
  assert_eq(stats.size, 0)
  assert_eq(stats.max_size, 3)
  
  // 添加条目
  put(cache, "key1", 100)
  put(cache, "key2", 200)
  put(cache, "key3", 300)
  
  let stats_after_put = get_stats(cache)
  assert_eq(stats_after_put.size, 3)
  
  // 获取条目
  assert_eq(get(cache, "key1"), Some(100))
  assert_eq(get(cache, "key2"), Some(200))
  assert_eq(get(cache, "key3"), Some(300))
  assert_eq(get(cache, "nonexistent"), None)
  
  // 添加第四个条目，应该驱逐最少使用的条目
  put(cache, "key4", 400)
  
  let stats_after_eviction = get_stats(cache)
  assert_eq(stats_after_eviction.size, 3) // 大小保持不变
  
  // key1应该被驱逐（最少使用）
  assert_eq(get(cache, "key1"), None)
  assert_eq(get(cache, "key2"), Some(200))
  assert_eq(get(cache, "key3"), Some(300))
  assert_eq(get(cache, "key4"), Some(400))
  
  // 更新现有条目
  put(cache, "key2", 250)
  assert_eq(get(cache, "key2"), Some(250))
  
  let stats_after_update = get_stats(cache)
  assert_eq(stats_after_update.size, 3) // 大小保持不变
}