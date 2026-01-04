// Azimuth 并发处理测试用例
// 专注于并发编程、线程安全和资源管理

// 测试1: 并发任务执行和同步
test "并发任务执行和同步" {
  // 定义任务类型
  type Task = {
    id: Int,
    work: () -> String,
    result: Option[String],
    completed: Bool
  }
  
  // 创建任务
  let create_task = fn(id: Int, work: () -> String) {
    { id, work, result: None, completed: false }
  }
  
  // 执行任务
  let execute_task = fn(task: Task) {
    let result = task.work()
    { task | result: Some(result), completed: true }
  }
  
  // 创建多个任务
  let tasks = [
    create_task(1, fn() { "Task 1 completed" }),
    create_task(2, fn() { "Task 2 completed" }),
    create_task(3, fn() { "Task 3 completed" }),
    create_task(4, fn() { "Task 4 completed" }),
    create_task(5, fn() { "Task 5 completed" })
  ]
  
  // 模拟并发执行（在真实环境中会使用线程池）
  let mut completed_tasks = []
  for task in tasks {
    let completed_task = execute_task(task)
    completed_tasks = completed_tasks.push(completed_task)
  }
  
  // 验证所有任务都已完成
  assert_eq(completed_tasks.length(), 5)
  
  for task in completed_tasks {
    assert_true(task.completed)
    assert_true(task.result != None)
  }
  
  // 验证任务结果
  let results = completed_tasks.map(fn(t) { match t.result { Some(r) => r None => "" } })
  assert_true(results.contains("Task 1 completed"))
  assert_true(results.contains("Task 2 completed"))
  assert_true(results.contains("Task 3 completed"))
  assert_true(results.contains("Task 4 completed"))
  assert_true(results.contains("Task 5 completed"))
}

// 测试2: 生产者-消费者模式
test "生产者-消费者模式" {
  // 定义缓冲区类型
  type Buffer[T] = {
    items: Array[T],
    capacity: Int,
    head: Int,
    tail: Int,
    count: Int
  }
  
  // 创建缓冲区
  let create_buffer = fn(capacity: Int) {
    let mut items = []
    for i in 0..capacity {
      items = items.push(None)
    }
    { items, capacity, head: 0, tail: 0, count: 0 }
  }
  
  // 检查缓冲区是否为空
  let is_empty = fn(buffer: Buffer[T]) {
    buffer.count == 0
  }
  
  // 检查缓冲区是否已满
  let is_full = fn(buffer: Buffer[T]) {
    buffer.count == buffer.capacity
  }
  
  // 生产者添加数据
  let produce = fn(buffer: Buffer[String], item: String) {
    if not(is_full(buffer)) {
      buffer.items[buffer.tail] = Some(item)
      buffer.tail = (buffer.tail + 1) % buffer.capacity
      buffer.count = buffer.count + 1
      true
    } else {
      false
    }
  }
  
  // 消费者获取数据
  let consume = fn(buffer: Buffer[String]) {
    if not(is_empty(buffer)) {
      let item = buffer.items[buffer.head]
      buffer.items[buffer.head] = None
      buffer.head = (buffer.head + 1) % buffer.capacity
      buffer.count = buffer.count - 1
      item
    } else {
      None
    }
  }
  
  // 测试生产者-消费者模式
  let buffer = create_buffer(5)
  
  // 初始状态
  assert_true(is_empty(buffer))
  assert_false(is_full(buffer))
  
  // 生产者添加数据
  assert_true(produce(buffer, "item1"))
  assert_true(produce(buffer, "item2"))
  assert_true(produce(buffer, "item3"))
  
  assert_false(is_empty(buffer))
  assert_false(is_full(buffer))
  
  // 消费者获取数据
  let consumed1 = consume(buffer)
  let consumed2 = consume(buffer)
  
  assert_eq(consumed1, Some("item1"))
  assert_eq(consumed2, Some("item2"))
  
  // 继续生产和消费
  assert_true(produce(buffer, "item4"))
  assert_true(produce(buffer, "item5"))
  assert_true(produce(buffer, "item6"))
  
  assert_true(is_full(buffer))
  
  // 缓冲区已满，无法添加更多数据
  assert_false(produce(buffer, "item7"))
  
  // 消费所有数据
  let mut all_consumed = []
  while not(is_empty(buffer)) {
    match consume(buffer) {
      Some(item) => all_consumed = all_consumed.push(item)
      None => break
    }
  }
  
  assert_eq(all_consumed.length(), 4)
  assert_true(all_consumed.contains("item3"))
  assert_true(all_consumed.contains("item4"))
  assert_true(all_consumed.contains("item5"))
  assert_true(all_consumed.contains("item6"))
  
  assert_true(is_empty(buffer))
}

// 测试3: 读写锁模式
test "读写锁模式" {
  // 定义锁状态
  enum LockState {
    Unlocked
    ReadLocked(Int)  // 读锁数量
    WriteLocked(String)  // 写锁持有者
  }
  
  // 定义读写锁类型
  type ReadWriteLock = {
    state: LockState,
    waiting_writers: Array[String],
    waiting_readers: Int
  }
  
  // 创建读写锁
  let create_lock = fn() {
    {
      state: LockState::Unlocked,
      waiting_writers: [],
      waiting_readers: 0
    }
  }
  
  // 获取读锁
  let acquire_read_lock = fn(lock: ReadWriteLock, reader_id: String) {
    match lock.state {
      LockState::Unlocked => {
        lock.state = LockState::ReadLocked(1)
        true
      }
      LockState::ReadLocked(count) => {
        if lock.waiting_writers.length() == 0 {
          lock.state = LockState::ReadLocked(count + 1)
          true
        } else {
          lock.waiting_readers = lock.waiting_readers + 1
          false
        }
      }
      LockState::WriteLocked(_) => {
        lock.waiting_readers = lock.waiting_readers + 1
        false
      }
    }
  }
  
  // 释放读锁
  let release_read_lock = fn(lock: ReadWriteLock) {
    match lock.state {
      LockState::ReadLocked(count) => {
        if count == 1 {
          lock.state = LockState::Unlocked
          
          // 如果有等待的写者，优先分配写锁
          if lock.waiting_writers.length() > 0 {
            let writer = lock.waiting_writers[0]
            lock.waiting_writers = lock.waiting_writers.slice(1)
            lock.state = LockState::WriteLocked(writer)
          }
        } else {
          lock.state = LockState::ReadLocked(count - 1)
        }
      }
      _ => {}
    }
  }
  
  // 获取写锁
  let acquire_write_lock = fn(lock: ReadWriteLock, writer_id: String) {
    match lock.state {
      LockState::Unlocked => {
        lock.state = LockState::WriteLocked(writer_id)
        true
      }
      _ => {
        if not(lock.waiting_writers.contains(writer_id)) {
          lock.waiting_writers = lock.waiting_writers.push(writer_id)
        }
        false
      }
    }
  }
  
  // 释放写锁
  let release_write_lock = fn(lock: ReadWriteLock, writer_id: String) {
    match lock.state {
      LockState::WriteLocked(holder) => {
        if holder == writer_id {
          lock.state = LockState::Unlocked
          
          // 如果有等待的写者，优先分配写锁
          if lock.waiting_writers.length() > 0 {
            let next_writer = lock.waiting_writers[0]
            lock.waiting_writers = lock.waiting_writers.slice(1)
            lock.state = LockState::WriteLocked(next_writer)
          } else if lock.waiting_readers > 0 {
            // 如果有等待的读者，分配读锁
            lock.state = LockState::ReadLocked(lock.waiting_readers)
            lock.waiting_readers = 0
          }
        }
      }
      _ => {}
    }
  }
  
  // 测试读写锁
  let lock = create_lock()
  
  // 初始状态
  match lock.state {
    LockState::Unlocked => assert_true(true)
    _ => assert_true(false)
  }
  
  // 获取读锁
  assert_true(acquire_read_lock(lock, "reader1"))
  
  match lock.state {
    LockState::ReadLocked(1) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 另一个读者可以获取读锁
  assert_true(acquire_read_lock(lock, "reader2"))
  
  match lock.state {
    LockState::ReadLocked(2) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 写者必须等待
  assert_false(acquire_write_lock(lock, "writer1"))
  assert_eq(lock.waiting_writers.length(), 1)
  
  // 释放读锁
  release_read_lock(lock)
  
  match lock.state {
    LockState::ReadLocked(1) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 释放最后一个读锁，写者应该获取锁
  release_read_lock(lock)
  
  match lock.state {
    LockState::WriteLocked(holder) => assert_eq(holder, "writer1")
    _ => assert_true(false)
  }
  
  // 读者必须等待
  assert_false(acquire_read_lock(lock, "reader3"))
  assert_eq(lock.waiting_readers, 1)
  
  // 释放写锁
  release_write_lock(lock, "writer1")
  
  match lock.state {
    LockState::Unlocked => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试4: 线程安全的计数器
test "线程安全的计数器" {
  // 定义原子计数器类型
  type AtomicCounter = {
    value: Int,
    mutex: Bool  // 简化的互斥锁
  }
  
  // 创建原子计数器
  let create_counter = fn(initial_value: Int) {
    { value: initial_value, mutex: false }
  }
  
  // 获取锁
  let acquire_mutex = fn(counter: AtomicCounter) {
    if not(counter.mutex) {
      counter.mutex = true
      true
    } else {
      false
    }
  }
  
  // 释放锁
  let release_mutex = fn(counter: AtomicCounter) {
    counter.mutex = false
  }
  
  // 原子递增
  let atomic_increment = fn(counter: AtomicCounter) {
    if acquire_mutex(counter) {
      counter.value = counter.value + 1
      release_mutex(counter)
      true
    } else {
      false
    }
  }
  
  // 原子递减
  let atomic_decrement = fn(counter: AtomicCounter) {
    if acquire_mutex(counter) {
      counter.value = counter.value - 1
      release_mutex(counter)
      true
    } else {
      false
    }
  }
  
  // 原子获取值
  let atomic_get = fn(counter: AtomicCounter) {
    if acquire_mutex(counter) {
      let value = counter.value
      release_mutex(counter)
      value
    } else {
      -1  // 错误值
    }
  }
  
  // 原子设置值
  let atomic_set = fn(counter: AtomicCounter, new_value: Int) {
    if acquire_mutex(counter) {
      counter.value = new_value
      release_mutex(counter)
      true
    } else {
      false
    }
  }
  
  // 测试原子计数器
  let counter = create_counter(0)
  
  // 初始值
  assert_eq(atomic_get(counter), 0)
  
  // 递增操作
  assert_true(atomic_increment(counter))
  assert_eq(atomic_get(counter), 1)
  
  assert_true(atomic_increment(counter))
  assert_eq(atomic_get(counter), 2)
  
  assert_true(atomic_increment(counter))
  assert_eq(atomic_get(counter), 3)
  
  // 递减操作
  assert_true(atomic_decrement(counter))
  assert_eq(atomic_get(counter), 2)
  
  // 设置值
  assert_true(atomic_set(counter, 10))
  assert_eq(atomic_get(counter), 10)
  
  // 测试并发操作（模拟）
  let mut operations = []
  for i in 0..100 {
    if i % 2 == 0 {
      operations = operations.push(fn(c) { atomic_increment(c) })
    } else {
      operations = operations.push(fn(c) { atomic_decrement(c) })
    }
  }
  
  // 执行所有操作
  let mut success_count = 0
  for op in operations {
    if op(counter) {
      success_count = success_count + 1
    }
  }
  
  // 验证最终结果
  let final_value = atomic_get(counter)
  assert_eq(final_value, 10 + (success_count / 2) - (success_count - success_count / 2))
}

// 测试5: 并发任务队列
test "并发任务队列" {
  // 定义任务队列类型
  type TaskQueue[T] = {
    tasks: Array[T],
    workers: Array[String],
    pending_tasks: Int,
    completed_tasks: Int
  }
  
  // 创建任务队列
  let create_task_queue = fn() {
    {
      tasks: [],
      workers: [],
      pending_tasks: 0,
      completed_tasks: 0
    }
  }
  
  // 添加任务
  let enqueue_task = fn(queue: TaskQueue[String], task: String) {
    queue.tasks = queue.tasks.push(task)
    queue.pending_tasks = queue.pending_tasks + 1
  }
  
  // 添加工作线程
  let add_worker = fn(queue: TaskQueue[String], worker_id: String) {
    queue.workers = queue.workers.push(worker_id)
  }
  
  // 分配任务给工作线程
  let assign_task = fn(queue: TaskQueue[String]) {
    if queue.pending_tasks > 0 and queue.workers.length() > 0 {
      let task = queue.tasks[0]
      queue.tasks = queue.tasks.slice(1)
      queue.pending_tasks = queue.pending_tasks - 1
      
      let worker_id = queue.workers[0]
      queue.workers = queue.workers.slice(1)
      
      // 模拟任务执行
      queue.completed_tasks = queue.completed_tasks + 1
      
      // 工作线程重新变为可用
      queue.workers = queue.workers.push(worker_id)
      
      Some((worker_id, task))
    } else {
      None
    }
  }
  
  // 测试任务队列
  let queue = create_task_queue()
  
  // 添加工作线程
  add_worker(queue, "worker1")
  add_worker(queue, "worker2")
  add_worker(queue, "worker3")
  
  assert_eq(queue.workers.length(), 3)
  assert_eq(queue.pending_tasks, 0)
  assert_eq(queue.completed_tasks, 0)
  
  // 添加任务
  enqueue_task(queue, "task1")
  enqueue_task(queue, "task2")
  enqueue_task(queue, "task3")
  enqueue_task(queue, "task4")
  enqueue_task(queue, "task5")
  
  assert_eq(queue.pending_tasks, 5)
  
  // 分配任务
  let assignment1 = assign_task(queue)
  assert_eq(assignment1, Some(("worker1", "task1")))
  assert_eq(queue.pending_tasks, 4)
  assert_eq(queue.completed_tasks, 1)
  
  let assignment2 = assign_task(queue)
  assert_eq(assignment2, Some(("worker2", "task2")))
  assert_eq(queue.pending_tasks, 3)
  assert_eq(queue.completed_tasks, 2)
  
  let assignment3 = assign_task(queue)
  assert_eq(assignment3, Some(("worker3", "task3")))
  assert_eq(queue.pending_tasks, 2)
  assert_eq(queue.completed_tasks, 3)
  
  // 继续分配剩余任务
  let assignment4 = assign_task(queue)
  assert_eq(assignment4, Some(("worker1", "task4")))
  assert_eq(queue.pending_tasks, 1)
  assert_eq(queue.completed_tasks, 4)
  
  let assignment5 = assign_task(queue)
  assert_eq(assignment5, Some(("worker2", "task5")))
  assert_eq(queue.pending_tasks, 0)
  assert_eq(queue.completed_tasks, 5)
  
  // 没有更多任务
  let assignment6 = assign_task(queue)
  assert_eq(assignment6, None)
  
  // 验证所有任务都已完成
  assert_eq(queue.pending_tasks, 0)
  assert_eq(queue.completed_tasks, 5)
  assert_eq(queue.workers.length(), 3)
}

// 测试6: 并发资源池
test "并发资源池" {
  // 定义资源类型
  type Resource = {
    id: String,
    in_use: Bool,
    created_at: Int
  }
  
  // 定义资源池类型
  type ResourcePool = {
    resources: Array[Resource],
    max_size: Int,
    current_size: Int,
    available_count: Int
  }
  
  // 创建资源池
  let create_resource_pool = fn(max_size: Int) {
    {
      resources: [],
      max_size,
      current_size: 0,
      available_count: 0
    }
  }
  
  // 创建新资源
  let create_resource = fn(id: String) {
    {
      id,
      in_use: false,
      created_at: 1640995200
    }
  }
  
  // 添加资源到池中
  let add_resource = fn(pool: ResourcePool, resource_id: String) {
    if pool.current_size < pool.max_size {
      let resource = create_resource(resource_id)
      pool.resources = pool.resources.push(resource)
      pool.current_size = pool.current_size + 1
      pool.available_count = pool.available_count + 1
      true
    } else {
      false
    }
  }
  
  // 获取资源
  let acquire_resource = fn(pool: ResourcePool) {
    for i in 0..pool.resources.length() {
      let resource = pool.resources[i]
      if not(resource.in_use) {
        resource.in_use = true
        pool.available_count = pool.available_count - 1
        return Some(resource)
      }
    }
    None
  }
  
  // 释放资源
  let release_resource = fn(pool: ResourcePool, resource_id: String) {
    for i in 0..pool.resources.length() {
      let resource = pool.resources[i]
      if resource.id == resource_id and resource.in_use {
        resource.in_use = false
        pool.available_count = pool.available_count + 1
        return true
      }
    }
    false
  }
  
  // 测试资源池
  let pool = create_resource_pool(5)
  
  // 添加资源
  assert_true(add_resource(pool, "resource1"))
  assert_true(add_resource(pool, "resource2"))
  assert_true(add_resource(pool, "resource3"))
  
  assert_eq(pool.current_size, 3)
  assert_eq(pool.available_count, 3)
  
  // 获取资源
  let resource1 = acquire_resource(pool)
  assert_true(resource1 != None)
  assert_eq(pool.available_count, 2)
  
  match resource1 {
    Some(r) => assert_eq(r.id, "resource1")
    None => assert_true(false)
  }
  
  let resource2 = acquire_resource(pool)
  let resource3 = acquire_resource(pool)
  
  assert_eq(pool.available_count, 0)
  
  // 所有资源都在使用中
  let resource4 = acquire_resource(pool)
  assert_eq(resource4, None)
  
  // 释放资源
  assert_true(release_resource(pool, "resource1"))
  assert_eq(pool.available_count, 1)
  
  // 可以重新获取释放的资源
  let resource1_again = acquire_resource(pool)
  assert_true(resource1_again != None)
  
  match resource1_again {
    Some(r) => assert_eq(r.id, "resource1")
    None => assert_true(false)
  }
  
  // 添加更多资源
  assert_true(add_resource(pool, "resource4"))
  assert_true(add_resource(pool, "resource5"))
  
  assert_eq(pool.current_size, 5)
  assert_eq(pool.available_count, 1)
  
  // 池已满，无法添加更多资源
  assert_false(add_resource(pool, "resource6"))
  
  // 释放所有资源
  assert_true(release_resource(pool, "resource1"))
  assert_true(release_resource(pool, "resource2"))
  assert_true(release_resource(pool, "resource3"))
  assert_true(release_resource(pool, "resource4"))
  assert_true(release_resource(pool, "resource5"))
  
  assert_eq(pool.available_count, 5)
}

// 测试7: 并发缓存系统
test "并发缓存系统" {
  // 定义缓存条目类型
  type CacheEntry[T] = {
    key: String,
    value: T,
    expires_at: Int,
    access_count: Int,
    last_accessed: Int
  }
  
  // 定义缓存类型
  type Cache[T] = {
    entries: Array[CacheEntry[T]],
    max_size: Int,
    current_time: Int
  }
  
  // 创建缓存
  let create_cache = fn(max_size: Int) {
    {
      entries: [],
      max_size,
      current_time: 1640995200
    }
  }
  
  // 查找缓存条目
  let find_entry = fn(cache: Cache[T], key: String) {
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        return Some(i)
      }
    }
    None
  }
  
  // 检查条目是否过期
  let is_expired = fn(entry: CacheEntry[T], current_time: Int) {
    entry.expires_at > 0 and entry.expires_at <= current_time
  }
  
  // 获取缓存值
  let cache_get = fn(cache: Cache[T], key: String) {
    match find_entry(cache, key) {
      Some(index) => {
        let entry = cache.entries[index]
        
        if is_expired(entry, cache.current_time) {
          // 删除过期条目
          cache.entries = cache.entries.slice(0, index) + cache.entries.slice(index + 1)
          None
        } else {
          // 更新访问统计
          entry.access_count = entry.access_count + 1
          entry.last_accessed = cache.current_time
          Some(entry.value)
        }
      }
      None => None
    }
  }
  
  // 设置缓存值
  let cache_set = fn(cache: Cache[T], key: String, value: T, ttl: Int) {
    let expires_at = if ttl > 0 { cache.current_time + ttl } else { 0 }
    
    match find_entry(cache, key) {
      Some(index) => {
        // 更新现有条目
        let entry = cache.entries[index]
        entry.value = value
        entry.expires_at = expires_at
        entry.access_count = entry.access_count + 1
        entry.last_accessed = cache.current_time
      }
      None => {
        // 添加新条目
        if cache.entries.length() >= cache.max_size {
          // 简单的LRU策略：删除访问次数最少的条目
          let mut min_index = 0
          let mut min_count = cache.entries[0].access_count
          
          for i in 1..cache.entries.length() {
            if cache.entries[i].access_count < min_count {
              min_count = cache.entries[i].access_count
              min_index = i
            }
          }
          
          cache.entries = cache.entries.slice(0, min_index) + cache.entries.slice(min_index + 1)
        }
        
        let new_entry = {
          key,
          value,
          expires_at,
          access_count: 1,
          last_accessed: cache.current_time
        }
        
        cache.entries = cache.entries.push(new_entry)
      }
    }
  }
  
  // 测试缓存系统
  let cache = create_cache(3)
  
  // 设置缓存值
  cache_set(cache, "key1", "value1", 100)
  cache_set(cache, "key2", "value2", 200)
  cache_set(cache, "key3", "value3", 300)
  
  assert_eq(cache.entries.length(), 3)
  
  // 获取缓存值
  assert_eq(cache_get(cache, "key1"), Some("value1"))
  assert_eq(cache_get(cache, "key2"), Some("value2"))
  assert_eq(cache_get(cache, "key3"), Some("value3"))
  assert_eq(cache_get(cache, "key4"), None)
  
  // 更新缓存值
  cache_set(cache, "key1", "new_value1", 150)
  assert_eq(cache_get(cache, "key1"), Some("new_value1"))
  
  // 添加更多条目，触发LRU淘汰
  cache_set(cache, "key4", "value4", 400)
  
  // 验证key2被淘汰（访问次数最少）
  assert_eq(cache_get(cache, "key1"), Some("new_value1"))
  assert_eq(cache_get(cache, "key2"), None)  // 应该被淘汰
  assert_eq(cache_get(cache, "key3"), Some("value3"))
  assert_eq(cache_get(cache, "key4"), Some("value4"))
  
  assert_eq(cache.entries.length(), 3)
  
  // 测试过期
  cache.current_time = 1640995200 + 500  // 超过所有TTL
  assert_eq(cache_get(cache, "key1"), None)  // 应该过期
  assert_eq(cache_get(cache, "key3"), None)  // 应该过期
  assert_eq(cache_get(cache, "key4"), None)  // 应该过期
  
  assert_eq(cache.entries.length(), 0)
}

// 测试8: 并发事件系统
test "并发事件系统" {
  // 定义事件类型
  type Event = {
    id: String,
    type: String,
    data: String,
    timestamp: Int
  }
  
  // 定义事件监听器类型
  type EventListener = {
    id: String,
    event_type: String,
    handler: (Event) -> String
  }
  
  // 定义事件总线类型
  type EventBus = {
    listeners: Array[EventListener],
    event_queue: Array[Event],
    processing: Bool
  }
  
  // 创建事件总线
  let create_event_bus = fn() {
    {
      listeners: [],
      event_queue: [],
      processing: false
    }
  }
  
  // 添加事件监听器
  let add_listener = fn(bus: EventBus, listener_id: String, event_type: String, handler: (Event) -> String) {
    let listener = {
      id: listener_id,
      event_type,
      handler
    }
    bus.listeners = bus.listeners.push(listener)
  }
  
  // 发布事件
  let publish_event = fn(bus: EventBus, event_type: String, data: String) {
    let event = {
      id: "event-" + bus.event_queue.length().to_string(),
      type: event_type,
      data,
      timestamp: 1640995200
    }
    bus.event_queue = bus.event_queue.push(event)
  }
  
  // 处理事件
  let process_events = fn(bus: EventBus) {
    if not(bus.processing) {
      bus.processing = true
      
      let mut processed_events = []
      
      for event in bus.event_queue {
        let mut results = []
        
        for listener in bus.listeners {
          if listener.event_type == event.type {
            let result = listener.handler(event)
            results = results.push((listener.id, result))
          }
        }
        
        processed_events = processed_events.push((event, results))
      }
      
      bus.event_queue = []
      bus.processing = false
      
      processed_events
    } else {
      []
    }
  }
  
  // 测试事件系统
  let bus = create_event_bus()
  
  // 添加事件监听器
  add_listener(bus, "listener1", "user.login", fn(event) {
    "User logged in: " + event.data
  })
  
  add_listener(bus, "listener2", "user.login", fn(event) {
    "Login recorded at: " + event.timestamp.to_string()
  })
  
  add_listener(bus, "listener3", "order.created", fn(event) {
    "Order created: " + event.data
  })
  
  assert_eq(bus.listeners.length(), 3)
  
  // 发布事件
  publish_event(bus, "user.login", "user123")
  publish_event(bus, "order.created", "order456")
  publish_event(bus, "user.login", "user789")
  
  assert_eq(bus.event_queue.length(), 3)
  
  // 处理事件
  let processed = process_events(bus)
  
  assert_eq(processed.length(), 3)
  assert_eq(bus.event_queue.length(), 0)
  
  // 验证第一个事件（user.login）
  match processed[0] {
    (event, results) => {
      assert_eq(event.type, "user.login")
      assert_eq(event.data, "user123")
      assert_eq(results.length(), 2)
      
      let listener1_result = results.find(fn(r) { r.0 == "listener1" })
      assert_eq(listener1_result, Some(("listener1", "User logged in: user123")))
      
      let listener2_result = results.find(fn(r) { r.0 == "listener2" })
      assert_eq(listener2_result, Some(("listener2", "Login recorded at: 1640995200")))
    }
  }
  
  // 验证第二个事件（order.created）
  match processed[1] {
    (event, results) => {
      assert_eq(event.type, "order.created")
      assert_eq(event.data, "order456")
      assert_eq(results.length(), 1)
      
      let listener3_result = results.find(fn(r) { r.0 == "listener3" })
      assert_eq(listener3_result, Some(("listener3", "Order created: order456")))
    }
  }
  
  // 验证第三个事件（user.login）
  match processed[2] {
    (event, results) => {
      assert_eq(event.type, "user.login")
      assert_eq(event.data, "user789")
      assert_eq(results.length(), 2)
      
      let listener1_result = results.find(fn(r) { r.0 == "listener1" })
      assert_eq(listener1_result, Some(("listener1", "User logged in: user789")))
    }
  }
}