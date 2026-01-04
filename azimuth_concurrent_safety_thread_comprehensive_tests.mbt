// Azimuth 并发和线程安全综合测试用例
// 专注于并发操作、线程安全和同步机制

// 测试1: 线程安全计数器
test "线程安全计数器测试" {
  // 定义原子计数器
  type AtomicCounter = {
    value: Int,
    mutex: Mutex
  }
  
  // 创建原子计数器
  let create_atomic_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      mutex: Mutex::new()
    }
  }
  
  // 原子递增
  let atomic_increment = fn(counter: AtomicCounter) {
    Mutex::lock(counter.mutex)
    counter.value = counter.value + 1
    Mutex::unlock(counter.mutex)
    counter.value
  }
  
  // 原子递减
  let atomic_decrement = fn(counter: AtomicCounter) {
    Mutex::lock(counter.mutex)
    counter.value = counter.value - 1
    Mutex::unlock(counter.mutex)
    counter.value
  }
  
  // 原子获取值
  let atomic_get = fn(counter: AtomicCounter) {
    Mutex::lock(counter.mutex)
    let result = counter.value
    Mutex::unlock(counter.mutex)
    result
  }
  
  // 创建计数器
  let counter = create_atomic_counter(0)
  
  // 测试基本操作
  assert_eq(atomic_get(counter), 0)
  assert_eq(atomic_increment(counter), 1)
  assert_eq(atomic_get(counter), 1)
  assert_eq(atomic_decrement(counter), 0)
  assert_eq(atomic_get(counter), 0)
  
  // 模拟并发操作
  let num_threads = 10
  let increments_per_thread = 100
  
  // 创建线程数组
  let mut threads = []
  
  // 启动多个线程进行递增操作
  for i in 0..num_threads {
    let thread = Thread::spawn(fn() {
      for j in 0..increments_per_thread {
        atomic_increment(counter)
      }
    })
    threads = threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证最终值
  let expected_value = num_threads * increments_per_thread
  assert_eq(atomic_get(counter), expected_value)
  
  // 测试并发递增和递减
  let counter2 = create_atomic_counter(0)
  let mut threads2 = []
  
  // 启动递增线程
  for i in 0..5 {
    let thread = Thread::spawn(fn() {
      for j in 0..50 {
        atomic_increment(counter2)
      }
    })
    threads2 = threads2.push(thread)
  }
  
  // 启动递减线程
  for i in 0..3 {
    let thread = Thread::spawn(fn() {
      for j in 0..30 {
        atomic_decrement(counter2)
      }
    })
    threads2 = threads2.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads2 {
    Thread::join(thread)
  }
  
  // 验证最终值 (5*50 - 3*30 = 250 - 90 = 160)
  assert_eq(atomic_get(counter2), 160)
}

// 测试2: 线程安全缓存
test "线程安全缓存测试" {
  // 定义线程安全缓存项
  type CacheItem[T] = {
    value: T,
    last_accessed: Int
  }
  
  // 定义线程安全缓存
  type ThreadSafeCache[T] = {
    items: Map[String, CacheItem[T]],
    mutex: Mutex,
    max_size: Int
  }
  
  // 创建线程安全缓存
  let create_thread_safe_cache = fn(max_size: Int) {
    {
      items: Map::new(),
      mutex: Mutex::new(),
      max_size
    }
  }
  
  // 获取当前时间
  let current_time = fn() { 1640995200 }
  
  // 获取缓存项
  let cache_get = fn(cache: ThreadSafeCache[T], key: String) {
    Mutex::lock(cache.mutex)
    let item = cache.items.get(key)
    let result = match item {
      Some(cache_item) => {
        // 更新访问时间
        let updated_item = {
          value: cache_item.value,
          last_accessed: current_time()
        }
        cache.items = cache.items.set(key, updated_item)
        Some(cache_item.value)
      }
      None => None
    }
    Mutex::unlock(cache.mutex)
    result
  }
  
  // 设置缓存项
  let cache_put = fn(cache: ThreadSafeCache[T], key: String, value: T) {
    Mutex::lock(cache.mutex)
    
    // 检查是否需要驱逐
    if cache.items.size() >= cache.max_size and not(cache.items.contains(key)) {
      // 找到最久未访问的项
      let mut oldest_key = None
      let mut oldest_time = current_time()
      
      for (k, item) in cache.items {
        if item.last_accessed < oldest_time {
          oldest_time = item.last_accessed
          oldest_key = Some(k)
        }
      }
      
      match oldest_key {
        Some(key_to_evict) => {
          cache.items = cache.items.remove(key_to_evict)
        }
        None => {}
      }
    }
    
    // 添加新项
    let new_item = {
      value,
      last_accessed: current_time()
    }
    cache.items = cache.items.set(key, new_item)
    
    Mutex::unlock(cache.mutex)
  }
  
  // 创建线程安全缓存
  let cache = create_thread_safe_cache(5)
  
  // 测试基本操作
  cache_put(cache, "key1", "value1")
  cache_put(cache, "key2", "value2")
  cache_put(cache, "key3", "value3")
  
  assert_eq(cache_get(cache, "key1"), Some("value1"))
  assert_eq(cache_get(cache, "key2"), Some("value2"))
  assert_eq(cache_get(cache, "key3"), Some("value3"))
  assert_eq(cache_get(cache, "nonexistent"), None)
  
  // 测试并发访问
  let mut threads = []
  
  // 启动多个线程同时读写缓存
  for i in 0..10 {
    let thread = Thread::spawn(fn() {
      let thread_id = i
      for j in 0..20 {
        let key = "key" + ((j % 5) + 1).to_string()
        let value = "value_from_thread_" + thread_id.to_string() + "_iteration_" + j.to_string()
        
        // 写入
        cache_put(cache, key, value)
        
        // 读取
        let read_value = cache_get(cache, key)
        
        // 验证读取的值不为None
        assert_true(read_value != None)
      }
    })
    threads = threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证缓存大小不超过最大值
  Mutex::lock(cache.mutex)
  assert_true(cache.items.size() <= cache.max_size)
  Mutex::unlock(cache.mutex)
}

// 测试3: 生产者-消费者模式
test "生产者-消费者模式测试" {
  // 定义任务队列
  type TaskQueue[T] = {
    items: Array[T],
    mutex: Mutex,
    not_empty: ConditionVariable,
    not_full: ConditionVariable,
    max_size: Int
  }
  
  // 创建任务队列
  let create_task_queue = fn(max_size: Int) {
    {
      items: [],
      mutex: Mutex::new(),
      not_empty: ConditionVariable::new(),
      not_full: ConditionVariable::new(),
      max_size
    }
  }
  
  // 生产者：添加任务
  let produce = fn(queue: TaskQueue[T], item: T) {
    Mutex::lock(queue.mutex)
    
    // 等待队列不满
    while queue.items.length() >= queue.max_size {
      ConditionVariable::wait(queue.not_full, queue.mutex)
    }
    
    // 添加任务
    queue.items = queue.items.push(item)
    
    // 通知消费者
    ConditionVariable::signal(queue.not_empty)
    
    Mutex::unlock(queue.mutex)
  }
  
  // 消费者：获取任务
  let consume = fn(queue: TaskQueue[T]) {
    Mutex::lock(queue.mutex)
    
    // 等待队列不空
    while queue.items.length() == 0 {
      ConditionVariable::wait(queue.not_empty, queue.mutex)
    }
    
    // 获取任务
    let item = queue.items[0]
    queue.items = queue.items.slice(1, queue.items.length())
    
    // 通知生产者
    ConditionVariable::signal(queue.not_full)
    
    Mutex::unlock(queue.mutex)
    item
  }
  
  // 创建任务队列
  let queue = create_task_queue(10)
  
  // 定义任务类型
  type Task = {
    id: Int,
    data: String,
    producer_id: Int
  }
  
  // 创建结果收集器
  let results = { mut items: [], mutex: Mutex::new() }
  
  // 启动生产者线程
  let mut producer_threads = []
  for i in 0..3 {
    let thread = Thread::spawn(fn() {
      let producer_id = i
      for j in 0..20 {
        let task = {
          id: producer_id * 100 + j,
          data: "data_" + producer_id.to_string() + "_" + j.to_string(),
          producer_id
        }
        
        // 生产任务
        produce(queue, task)
        
        // 模拟生产延迟
        Thread::sleep(10)
      }
    })
    producer_threads = producer_threads.push(thread)
  }
  
  // 启动消费者线程
  let mut consumer_threads = []
  for i in 0..2 {
    let thread = Thread::spawn(fn() {
      let consumer_id = i
      let mut consumed_count = 0
      
      while consumed_count < 30 {
        // 消费任务
        let task = consume(queue)
        
        // 处理任务
        Mutex::lock(results.mutex)
        results.items = results.items.push({
          task,
          consumer_id,
          processed_at: current_time()
        })
        Mutex::unlock(results.mutex)
        
        consumed_count = consumed_count + 1
        
        // 模拟处理延迟
        Thread::sleep(15)
      }
    })
    consumer_threads = consumer_threads.push(thread)
  }
  
  // 等待所有生产者完成
  for thread in producer_threads {
    Thread::join(thread)
  }
  
  // 等待所有消费者完成
  for thread in consumer_threads {
    Thread::join(thread)
  }
  
  // 验证结果
  Mutex::lock(results.mutex)
  assert_eq(results.items.length(), 60)  // 3个生产者 × 20个任务
  
  // 验证所有任务都被处理
  let mut producer_counts = Map::new()
  for result in results.items {
    let producer_id = result.task.producer_id
    let count = producer_counts.get(producer_id)
    match count {
      Some(c) => producer_counts = producer_counts.set(producer_id, c + 1)
      None => producer_counts = producer_counts.set(producer_id, 1)
    }
  }
  
  assert_eq(producer_counts.get(0), Some(20))
  assert_eq(producer_counts.get(1), Some(20))
  assert_eq(producer_counts.get(2), Some(20))
  Mutex::unlock(results.mutex)
}

// 测试4: 读写锁模式
test "读写锁模式测试" {
  // 定义读写锁
  type ReadWriteLock = {
    readers: Int,
    writer: Bool,
    mutex: Mutex,
    read_condition: ConditionVariable,
    write_condition: ConditionVariable
  }
  
  // 创建读写锁
  let create_read_write_lock = fn() {
    {
      readers: 0,
      writer: false,
      mutex: Mutex::new(),
      read_condition: ConditionVariable::new(),
      write_condition: ConditionVariable::new()
    }
  }
  
  // 获取读锁
  let acquire_read_lock = fn(rw_lock: ReadWriteLock) {
    Mutex::lock(rw_lock.mutex)
    
    // 等待没有写者
    while rw_lock.writer {
      ConditionVariable::wait(rw_lock.read_condition, rw_lock.mutex)
    }
    
    rw_lock.readers = rw_lock.readers + 1
    
    Mutex::unlock(rw_lock.mutex)
  }
  
  // 释放读锁
  let release_read_lock = fn(rw_lock: ReadWriteLock) {
    Mutex::lock(rw_lock.mutex)
    
    rw_lock.readers = rw_lock.readers - 1
    
    // 如果没有读者了，通知可能的写者
    if rw_lock.readers == 0 {
      ConditionVariable::signal(rw_lock.write_condition)
    }
    
    Mutex::unlock(rw_lock.mutex)
  }
  
  // 获取写锁
  let acquire_write_lock = fn(rw_lock: ReadWriteLock) {
    Mutex::lock(rw_lock.mutex)
    
    // 等待没有读者和写者
    while rw_lock.readers > 0 or rw_lock.writer {
      ConditionVariable::wait(rw_lock.write_condition, rw_lock.mutex)
    }
    
    rw_lock.writer = true
    
    Mutex::unlock(rw_lock.mutex)
  }
  
  // 释放写锁
  let release_write_lock = fn(rw_lock: ReadWriteLock) {
    Mutex::lock(rw_lock.mutex)
    
    rw_lock.writer = false
    
    // 通知所有等待的读者和写者
    ConditionVariable::broadcast(rw_lock.read_condition)
    ConditionVariable::signal(rw_lock.write_condition)
    
    Mutex::unlock(rw_lock.mutex)
  }
  
  // 定义共享数据
  type SharedData = {
    value: Int,
    rw_lock: ReadWriteLock
  }
  
  // 创建共享数据
  let shared_data = {
    value: 0,
    rw_lock: create_read_write_lock()
  }
  
  // 创建操作日志
  let operation_log = { mut entries: [], mutex: Mutex::new() }
  
  // 启动读者线程
  let mut reader_threads = []
  for i in 0..5 {
    let thread = Thread::spawn(fn() {
      let reader_id = i
      for j in 0..10 {
        // 获取读锁
        acquire_read_lock(shared_data.rw_lock)
        
        // 读取数据
        let value = shared_data.value
        
        // 记录读操作
        Mutex::lock(operation_log.mutex)
        operation_log.entries = operation_log.entries.push({
          operation: "read",
          thread_id: reader_id,
          value,
          timestamp: current_time()
        })
        Mutex::unlock(operation_log.mutex)
        
        // 释放读锁
        release_read_lock(shared_data.rw_lock)
        
        // 模拟处理时间
        Thread::sleep(50)
      }
    })
    reader_threads = reader_threads.push(thread)
  }
  
  // 启动写者线程
  let mut writer_threads = []
  for i in 0..2 {
    let thread = Thread::spawn(fn() {
      let writer_id = i
      for j in 0..5 {
        // 获取写锁
        acquire_write_lock(shared_data.rw_lock)
        
        // 修改数据
        shared_data.value = shared_data.value + 1
        
        // 记录写操作
        Mutex::lock(operation_log.mutex)
        operation_log.entries = operation_log.entries.push({
          operation: "write",
          thread_id: writer_id,
          value: shared_data.value,
          timestamp: current_time()
        })
        Mutex::unlock(operation_log.mutex)
        
        // 释放写锁
        release_write_lock(shared_data.rw_lock)
        
        // 模拟处理时间
        Thread::sleep(100)
      }
    })
    writer_threads = writer_threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in reader_threads {
    Thread::join(thread)
  }
  
  for thread in writer_threads {
    Thread::join(thread)
  }
  
  // 验证结果
  Mutex::lock(operation_log.mutex)
  assert_eq(operation_log.entries.length(), 60)  // 5个读者 × 10次 + 2个写者 × 5次
  
  // 验证写操作次数
  let write_operations = operation_log.entries.filter(fn(e) { e.operation == "write" })
  assert_eq(write_operations.length(), 10)
  
  // 验证最终值
  assert_eq(shared_data.value, 10)  // 2个写者 × 5次写操作
  
  // 验证读操作期间值的一致性
  let read_operations = operation_log.entries.filter(fn(e) { e.operation == "read" })
  for read_op in read_operations {
    assert_true(read_op.value >= 0 and read_op.value <= 10)
  }
  Mutex::unlock(operation_log.mutex)
}

// 测试5: 线程池实现
test "线程池实现测试" {
  // 定义任务
  type Task = {
    id: Int,
    execute: () -> Any
  }
  
  // 定义线程池
  type ThreadPool = {
    workers: Array[Thread],
    task_queue: TaskQueue[Task],
    shutdown: Bool
  }
  
  // 创建线程池
  let create_thread_pool = fn(size: Int) {
    let task_queue = create_task_queue(100)
    let shutdown_flag = { mut value: false, mutex: Mutex::new() }
    
    // 创建工作线程
    let mut workers = []
    for i in 0..size {
      let thread = Thread::spawn(fn() {
        let worker_id = i
        
        while true {
          // 检查是否关闭
          Mutex::lock(shutdown_flag.mutex)
          let should_shutdown = shutdown_flag.value
          Mutex::unlock(shutdown_flag.mutex)
          
          if should_shutdown {
            break
          }
          
          // 获取任务
          let task = consume(task_queue)
          
          // 执行任务
          let result = task.execute()
          
          // 可以在这里处理结果
        }
      })
      workers = workers.push(thread)
    }
    
    {
      workers,
      task_queue,
      shutdown: false
    }
  }
  
  // 提交任务到线程池
  let submit_task = fn(pool: ThreadPool, task: Task) {
    produce(pool.task_queue, task)
  }
  
  // 关闭线程池
  let shutdown_thread_pool = fn(pool: ThreadPool) {
    // 这里应该设置关闭标志并等待所有工作线程完成
    // 简化实现
  }
  
  // 创建线程池
  let pool = create_thread_pool(3)
  
  // 创建结果收集器
  let results = { mut items: [], mutex: Mutex::new() }
  
  // 提交计算任务
  for i in 0..20 {
    let task = {
      id: i,
      execute: fn() {
        // 模拟计算密集型任务
        let mut sum = 0
        for j in 0..1000 {
          sum = sum + j
        }
        
        // 记录结果
        Mutex::lock(results.mutex)
        results.items = results.items.push({
          task_id: i,
          result: sum,
          thread_id: Thread::current_id(),
          timestamp: current_time()
        })
        Mutex::unlock(results.mutex)
        
        sum
      }
    }
    
    submit_task(pool, task)
  }
  
  // 等待所有任务完成
  Thread::sleep(2000)
  
  // 关闭线程池
  shutdown_thread_pool(pool)
  
  // 验证结果
  Mutex::lock(results.mutex)
  assert_eq(results.items.length(), 20)
  
  // 验证所有任务都被执行
  for result in results.items {
    assert_eq(result.result, 499500)  // 0到999的和
    assert_true(result.task_id >= 0 and result.task_id < 20)
  }
  
  // 验证多个线程参与了任务执行
  let thread_ids = results.items.map(fn(r) { r.thread_id })
  let unique_thread_ids = thread_ids.filter(fn(id, index) { thread_ids.index_of(id) == index })
  assert_true(unique_thread_ids.length() > 1)
  Mutex::unlock(results.mutex)
}

// 测试6: 原子操作和CAS
test "原子操作和CAS测试" {
  // 定义原子引用类型
  type AtomicRef[T] = {
    value: T,
    mutex: Mutex
  }
  
  // 创建原子引用
  let create_atomic_ref = fn(initial_value: T) {
    {
      value: initial_value,
      mutex: Mutex::new()
    }
  }
  
  // 比较并交换
  let compare_and_swap = fn(atomic_ref: AtomicRef[T], expected: T, new_value: T) {
    Mutex::lock(atomic_ref.mutex)
    let success = atomic_ref.value == expected
    if success {
      atomic_ref.value = new_value
    }
    let current_value = if success { new_value } else { atomic_ref.value }
    Mutex::unlock(atomic_ref.mutex)
    (success, current_value)
  }
  
  // 获取当前值
  let atomic_get_ref = fn(atomic_ref: AtomicRef[T]) {
    Mutex::lock(atomic_ref.mutex)
    let value = atomic_ref.value
    Mutex::unlock(atomic_ref.mutex)
    value
  }
  
  // 创建原子引用
  let atomic_int = create_atomic_ref(0)
  
  // 测试基本CAS操作
  let (success1, value1) = compare_and_swap(atomic_int, 0, 1)
  assert_true(success1)
  assert_eq(value1, 1)
  assert_eq(atomic_get_ref(atomic_int), 1)
  
  let (success2, value2) = compare_and_swap(atomic_int, 0, 2)
  assert_false(success2)
  assert_eq(value2, 1)
  assert_eq(atomic_get_ref(atomic_int), 1)
  
  let (success3, value3) = compare_and_swap(atomic_int, 1, 2)
  assert_true(success3)
  assert_eq(value3, 2)
  assert_eq(atomic_get_ref(atomic_int), 2)
  
  // 测试并发CAS操作
  let atomic_counter = create_atomic_ref(0)
  let num_threads = 10
  let increments_per_thread = 100
  
  // 创建线程数组
  let mut threads = []
  
  // 启动多个线程进行CAS递增
  for i in 0..num_threads {
    let thread = Thread::spawn(fn() {
      for j in 0..increments_per_thread {
        loop {
          let current = atomic_get_ref(atomic_counter)
          let new_value = current + 1
          let (success, _) = compare_and_swap(atomic_counter, current, new_value)
          
          if success {
            break
          }
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证最终值
  let expected_value = num_threads * increments_per_thread
  assert_eq(atomic_get_ref(atomic_counter), expected_value)
  
  // 测试复杂类型的CAS
  type Point = {
    x: Int,
    y: Int
  }
  
  let atomic_point = create_atomic_ref({ x: 0, y: 0 })
  
  // 并发更新点坐标
  let mut point_threads = []
  
  for i in 0..5 {
    let thread = Thread::spawn(fn() {
      for j in 0..10 {
        loop {
          let current = atomic_get_ref(atomic_point)
          let new_point = { x: current.x + 1, y: current.y + 1 }
          let (success, _) = compare_and_swap(atomic_point, current, new_point)
          
          if success {
            break
          }
        }
      }
    })
    point_threads = point_threads.push(thread)
  }
  
  for thread in point_threads {
    Thread::join(thread)
  }
  
  let final_point = atomic_get_ref(atomic_point)
  assert_eq(final_point.x, 50)
  assert_eq(final_point.y, 50)
}