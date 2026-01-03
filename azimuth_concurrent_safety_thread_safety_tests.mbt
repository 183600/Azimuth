// Azimuth 并发安全性和线程安全测试
// 测试系统在并发环境下的安全性和数据一致性

// 测试1: 并发计数器安全操作
test "并发计数器安全操作" {
  // 定义线程安全的计数器
  type AtomicCounter = {
    value: Int,
    lock: Bool
  }
  
  // 创建原子计数器
  let create_counter = fn(initial_value: Int) -> AtomicCounter {
    {
      value: initial_value,
      lock: false
    }
  }
  
  // 原子递增操作
  let atomic_increment = fn(counter: AtomicCounter) -> AtomicCounter {
    // 模拟获取锁
    if counter.lock {
      // 如果锁已被获取，返回原计数器(简化实现)
      return counter
    }
    
    // 获取锁，更新值，释放锁
    {
      value: counter.value + 1,
      lock: false
    }
  }
  
  // 原子递减操作
  let atomic_decrement = fn(counter: AtomicCounter) -> AtomicCounter {
    // 模拟获取锁
    if counter.lock {
      // 如果锁已被获取，返回原计数器(简化实现)
      return counter
    }
    
    // 获取锁，更新值，释放锁
    {
      value: counter.value - 1,
      lock: false
    }
  }
  
  // 创建计数器
  let mut counter = create_counter(0)
  
  // 模拟并发递增操作
  for i = 0; i < 100; i = i + 1 {
    counter = atomic_increment(counter)
  }
  
  // 验证最终值
  assert_eq(counter.value, 100)
  
  // 模拟并发递减操作
  for i = 0; i < 50; i = i + 1 {
    counter = atomic_decrement(counter)
  }
  
  // 验证最终值
  assert_eq(counter.value, 50)
}

// 测试2: 并发集合安全操作
test "并发集合安全操作" {
  // 定义线程安全的集合
  type ConcurrentSet[T] = {
    items: Array[T],
    lock: Bool
  }
  
  // 创建并发集合
  let create_concurrent_set = fn() -> ConcurrentSet[String] {
    {
      items: [],
      lock: false
    }
  }
  
  // 线程安全的添加操作
  let safe_add = fn(set: ConcurrentSet[String], item: String) -> ConcurrentSet[String] {
    // 模拟获取锁
    if set.lock {
      return set
    }
    
    // 检查项是否已存在
    let exists = set.items.any(fn(existing) { existing == item })
    
    if exists {
      return set
    }
    
    // 添加新项
    {
      items: set.items.push(item),
      lock: false
    }
  }
  
  // 线程安全的移除操作
  let safe_remove = fn(set: ConcurrentSet[String], item: String) -> ConcurrentSet[String] {
    // 模拟获取锁
    if set.lock {
      return set
    }
    
    // 移除项
    {
      items: set.items.filter(fn(existing) { existing != item }),
      lock: false
    }
  }
  
  // 线程安全的包含检查
  let safe_contains = fn(set: ConcurrentSet[String], item: String) -> Bool {
    // 模拟获取锁(读操作可以并发)
    set.items.any(fn(existing) { existing == item })
  }
  
  // 创建并发集合
  let mut concurrent_set = create_concurrent_set()
  
  // 添加多个项
  let items_to_add = ["item1", "item2", "item3", "item4", "item5"]
  for item in items_to_add {
    concurrent_set = safe_add(concurrent_set, item)
  }
  
  // 验证所有项都已添加
  assert_eq(concurrent_set.items.length(), 5)
  for item in items_to_add {
    assert_true(safe_contains(concurrent_set, item))
  }
  
  // 尝试添加重复项
  let original_length = concurrent_set.items.length()
  concurrent_set = safe_add(concurrent_set, "item2")  // 已存在的项
  assert_eq(concurrent_set.items.length(), original_length)  // 长度不应变化
  
  // 移除项
  concurrent_set = safe_remove(concurrent_set, "item3")
  assert_eq(concurrent_set.items.length(), 4)
  assert_false(safe_contains(concurrent_set, "item3"))
  assert_true(safe_contains(concurrent_set, "item1"))
  assert_true(safe_contains(concurrent_set, "item2"))
}

// 测试3: 并发映射安全操作
test "并发映射安全操作" {
  // 定义线程安全的映射
  type ConcurrentMap[K, V] = {
    entries: Array[(K, V)>,
    lock: Bool
  }
  
  // 创建并发映射
  let create_concurrent_map = fn() -> ConcurrentMap[String, Int] {
    {
      entries: [],
      lock: false
    }
  }
  
  // 线程安全的设置操作
  let safe_set = fn(map: ConcurrentMap[String, Int], key: String, value: Int) -> ConcurrentMap[String, Int] {
    // 模拟获取锁
    if map.lock {
      return map
    }
    
    // 检查键是否已存在
    let exists = map.entries.any(fn((k, _)) { k == key })
    
    if exists {
      // 更新现有键
      let new_entries = map.entries.map(fn((k, v)) {
        if k == key { (k, value) } else { (k, v) }
      })
      {
        entries: new_entries,
        lock: false
      }
    } else {
      // 添加新键值对
      {
        entries: map.entries.push((key, value)),
        lock: false
      }
    }
  }
  
  // 线程安全的获取操作
  let safe_get = fn(map: ConcurrentMap[String, Int], key: String) -> Option[Int] {
    // 模拟获取锁(读操作可以并发)
    for (k, v) in map.entries {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  // 线程安全的删除操作
  let safe_delete = fn(map: ConcurrentMap[String, Int], key: String) -> ConcurrentMap[String, Int] {
    // 模拟获取锁
    if map.lock {
      return map
    }
    
    {
      entries: map.entries.filter(fn((k, _)) { k != key }),
      lock: false
    }
  }
  
  // 创建并发映射
  let mut concurrent_map = create_concurrent_map()
  
  // 设置多个键值对
  concurrent_map = safe_set(concurrent_map, "key1", 100)
  concurrent_map = safe_set(concurrent_map, "key2", 200)
  concurrent_map = safe_set(concurrent_map, "key3", 300)
  
  // 验证键值对
  assert_eq(concurrent_map.entries.length(), 3)
  assert_eq(safe_get(concurrent_map, "key1"), Some(100))
  assert_eq(safe_get(concurrent_map, "key2"), Some(200))
  assert_eq(safe_get(concurrent_map, "key3"), Some(300))
  assert_eq(safe_get(concurrent_map, "key4"), None)
  
  // 更新现有键
  concurrent_map = safe_set(concurrent_map, "key2", 250)
  assert_eq(concurrent_map.entries.length(), 3)  // 长度不应变化
  assert_eq(safe_get(concurrent_map, "key2"), Some(250))
  
  // 删除键
  concurrent_map = safe_delete(concurrent_map, "key1")
  assert_eq(concurrent_map.entries.length(), 2)
  assert_eq(safe_get(concurrent_map, "key1"), None)
  assert_eq(safe_get(concurrent_map, "key2"), Some(250))
  assert_eq(safe_get(concurrent_map, "key3"), Some(300))
}

// 测试4: 并发队列安全操作
test "并发队列安全操作" {
  // 定义线程安全的队列
  type ConcurrentQueue[T] = {
    items: Array[T>,
    head: Int,
    tail: Int,
    lock: Bool
  }
  
  // 创建并发队列
  let create_concurrent_queue = fn(capacity: Int) -> ConcurrentQueue[String] {
    {
      items: Array::with_capacity(capacity),
      head: 0,
      tail: 0,
      lock: false
    }
  }
  
  // 线程安全的入队操作
  let safe_enqueue = fn(queue: ConcurrentQueue[String], item: String) -> ConcurrentQueue[String] {
    // 模拟获取锁
    if queue.lock {
      return queue
    }
    
    // 检查队列是否已满(简化实现)
    if queue.items.length() >= 100 {  // 假设最大容量为100
      return queue
    }
    
    // 添加项到队列尾部
    {
      items: queue.items.push(item),
      head: queue.head,
      tail: queue.tail + 1,
      lock: false
    }
  }
  
  // 线程安全的出队操作
  let safe_dequeue = fn(queue: ConcurrentQueue[String>) -> (ConcurrentQueue[String], Option[String]) {
    // 模拟获取锁
    if queue.lock {
      return (queue, None)
    }
    
    // 检查队列是否为空
    if queue.items.length() == 0 {
      return (queue, None)
    }
    
    // 移除队列头部项
    let item = queue.items[0]
    let new_items = queue.items.slice(1, queue.items.length())
    
    ({
      items: new_items,
      head: queue.head + 1,
      tail: queue.tail,
      lock: false
    }, Some(item))
  }
  
  // 创建并发队列
  let mut queue = create_concurrent_queue(10)
  
  // 入队多个项
  let items_to_enqueue = ["item1", "item2", "item3", "item4", "item5"]
  for item in items_to_enqueue {
    queue = safe_enqueue(queue, item)
  }
  
  // 验证队列长度
  assert_eq(queue.items.length(), 5)
  
  // 出队所有项并验证顺序
  let mut dequeued_items = []
  for i = 0; i < 5; i = i + 1 {
    let (new_queue, item) = safe_dequeue(queue)
    queue = new_queue
    match item {
      Some(value) => dequeued_items = dequeued_items.push(value)
      None => assert_true(false)  // 不应该为空
    }
  }
  
  // 验证出队顺序与入队顺序一致
  assert_eq(dequeued_items, items_to_enqueue)
  
  // 验证队列现在为空
  assert_eq(queue.items.length(), 0)
  
  // 尝试从空队列出队
  let (new_queue, item) = safe_dequeue(queue)
  queue = new_queue
  assert_eq(item, None)
}

// 测试5: 并发资源池管理
test "并发资源池管理" {
  // 定义资源
  type Resource = {
    id: String,
    in_use: Bool,
    created_at: Int
  }
  
  // 定义资源池
  type ResourcePool = {
    resources: Array[Resource>,
    max_size: Int,
    lock: Bool
  }
  
  // 创建资源池
  let create_resource_pool = fn(max_size: Int) -> ResourcePool {
    {
      resources: [],
      max_size: max_size,
      lock: false
    }
  }
  
  // 创建资源
  let create_resource = fn(id: String) -> Resource {
    {
      id: id,
      in_use: false,
      created_at: Time::now()
    }
  }
  
  // 从池中获取资源
  let acquire_resource = fn(pool: ResourcePool) -> (ResourcePool, Option[Resource]) {
    // 模拟获取锁
    if pool.lock {
      return (pool, None)
    }
    
    // 查找可用资源
    for resource in pool.resources {
      if !resource.in_use {
        // 标记资源为使用中
        let updated_resources = pool.resources.map(fn(r) {
          if r.id == resource.id {
            { id: r.id, in_use: true, created_at: r.created_at }
          } else {
            r
          }
        })
        
        return ({
          resources: updated_resources,
          max_size: pool.max_size,
          lock: false
        }, Some(resource))
      }
    }
    
    // 没有可用资源，检查是否可以创建新资源
    if pool.resources.length() < pool.max_size {
      let new_resource = create_resource("resource-" + pool.resources.length().to_string())
      let updated_resource = { id: new_resource.id, in_use: true, created_at: new_resource.created_at }
      
      return ({
        resources: pool.resources.push(updated_resource),
        max_size: pool.max_size,
        lock: false
      }, Some(updated_resource))
    }
    
    // 无法获取资源
    (pool, None)
  }
  
  // 释放资源回池中
  let release_resource = fn(pool: ResourcePool, resource_id: String) -> ResourcePool {
    // 模拟获取锁
    if pool.lock {
      return pool
    }
    
    // 查找资源并标记为未使用
    let updated_resources = pool.resources.map(fn(r) {
      if r.id == resource_id {
        { id: r.id, in_use: false, created_at: r.created_at }
      } else {
        r
      }
    })
    
    {
      resources: updated_resources,
      max_size: pool.max_size,
      lock: false
    }
  }
  
  // 创建资源池
  let mut pool = create_resource_pool(3)
  
  // 获取资源
  let (new_pool1, resource1) = acquire_resource(pool)
  pool = new_pool1
  match resource1 {
    Some(r) => {
      assert_eq(r.id, "resource-0")
      assert_true(r.in_use)
    }
    None => assert_true(false)
  }
  
  // 获取更多资源
  let (new_pool2, resource2) = acquire_resource(pool)
  pool = new_pool2
  match resource2 {
    Some(r) => {
      assert_eq(r.id, "resource-1")
      assert_true(r.in_use)
    }
    None => assert_true(false)
  }
  
  let (new_pool3, resource3) = acquire_resource(pool)
  pool = new_pool3
  match resource3 {
    Some(r) => {
      assert_eq(r.id, "resource-2")
      assert_true(r.in_use)
    }
    None => assert_true(false)
  }
  
  // 验证池中有3个资源，都在使用中
  assert_eq(pool.resources.length(), 3)
  assert_true(pool.resources.all(fn(r) { r.in_use }))
  
  // 尝试获取更多资源(应该失败，因为池已满)
  let (new_pool4, resource4) = acquire_resource(pool)
  pool = new_pool4
  assert_eq(resource4, None)
  
  // 释放一个资源
  pool = release_resource(pool, "resource-1")
  
  // 验证资源已释放
  let resource1_released = pool.resources.find(fn(r) { r.id == "resource-1" })
  match resource1_released {
    Some(r) => assert_false(r.in_use)
    None => assert_true(false)
  }
  
  // 再次尝试获取资源(应该成功，获取到已释放的资源)
  let (new_pool5, resource5) = acquire_resource(pool)
  pool = new_pool5
  match resource5 {
    Some(r) => assert_eq(r.id, "resource-1")  // 应该是之前释放的资源
    None => assert_true(false)
  }
  
  // 验证池中仍然有3个资源
  assert_eq(pool.resources.length(), 3)
}

// 测试6: 并发任务执行和同步
test "并发任务执行和同步" {
  // 定义任务状态
  enum TaskStatus {
    Pending
    Running
    Completed
    Failed
  }
  
  // 定义任务
  type Task = {
    id: String,
    status: TaskStatus,
    result: Option[String],
    error: Option[String]
  }
  
  // 定义任务执行器
  type TaskExecutor = {
    tasks: Array[Task>,
    max_concurrent: Int,
    running_count: Int,
    lock: Bool
  }
  
  // 创建任务执行器
  let create_task_executor = fn(max_concurrent: Int) -> TaskExecutor {
    {
      tasks: [],
      max_concurrent: max_concurrent,
      running_count: 0,
      lock: false
    }
  }
  
  // 创建任务
  let create_task = fn(id: String) -> Task {
    {
      id: id,
      status: TaskStatus::Pending,
      result: None,
      error: None
    }
  }
  
  // 提交任务
  let submit_task = fn(executor: TaskExecutor, task: Task) -> TaskExecutor {
    // 模拟获取锁
    if executor.lock {
      return executor
    }
    
    {
      tasks: executor.tasks.push(task),
      max_concurrent: executor.max_concurrent,
      running_count: executor.running_count,
      lock: false
    }
  }
  
  // 执行任务(简化实现)
  let execute_tasks = fn(executor: TaskExecutor) -> TaskExecutor {
    // 模拟获取锁
    if executor.lock {
      return executor
    }
    
    // 计算可以启动的任务数
    let pending_tasks = executor.tasks.filter(fn(t) {
      match t.status {
        TaskStatus::Pending => true
        _ => false
      }
    })
    
    let available_slots = executor.max_concurrent - executor.running_count
    let tasks_to_start = if available_slots > 0 {
      pending_tasks.take(available_slots)
    } else {
      []
    }
    
    // 更新任务状态
    let mut updated_tasks = executor.tasks
    let mut new_running_count = executor.running_count
    
    for task in tasks_to_start {
      // 模拟任务执行
      let task_result = "result-" + task.id
      let updated_task = {
        id: task.id,
        status: TaskStatus::Completed,
        result: Some(task_result),
        error: None
      }
      
      // 替换任务
      updated_tasks = updated_tasks.map(fn(t) {
        if t.id == task.id {
          updated_task
        } else {
          t
        }
      })
      
      new_running_count = new_running_count - 1  // 任务已完成
    }
    
    {
      tasks: updated_tasks,
      max_concurrent: executor.max_concurrent,
      running_count: new_running_count,
      lock: false
    }
  }
  
  // 创建任务执行器
  let mut executor = create_task_executor(2)
  
  // 提交多个任务
  let task1 = create_task("task1")
  let task2 = create_task("task2")
  let task3 = create_task("task3")
  let task4 = create_task("task4")
  
  executor = submit_task(executor, task1)
  executor = submit_task(executor, task2)
  executor = submit_task(executor, task3)
  executor = submit_task(executor, task4)
  
  // 验证任务已提交
  assert_eq(executor.tasks.length(), 4)
  assert_true(executor.tasks.all(fn(t) {
    match t.status {
      TaskStatus::Pending => true
      _ => false
    }
  }))
  
  // 执行任务
  executor = execute_tasks(executor)
  
  // 验证任务执行结果
  let completed_tasks = executor.tasks.filter(fn(t) {
    match t.status {
      TaskStatus::Completed => true
      _ => false
    }
  })
  
  let pending_tasks = executor.tasks.filter(fn(t) {
    match t.status {
      TaskStatus::Pending => true
      _ => false
    }
  })
  
  // 由于最大并发数为2，应该有2个任务完成，2个任务仍在等待
  assert_eq(completed_tasks.length(), 2)
  assert_eq(pending_tasks.length(), 2)
  
  // 再次执行任务
  executor = execute_tasks(executor)
  
  // 验证所有任务都已完成
  let all_completed = executor.tasks.all(fn(t) {
    match t.status {
      TaskStatus::Completed => true
      _ => false
    }
  })
  
  assert_true(all_completed)
  
  // 验证任务结果
  let task1_result = executor.tasks.find(fn(t) { t.id == "task1" })
  match task1_result {
    Some(t) => {
      match t.status {
        TaskStatus::Completed => {
          match t.result {
            Some(result) => assert_eq(result, "result-task1")
            None => assert_true(false)
          }
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}