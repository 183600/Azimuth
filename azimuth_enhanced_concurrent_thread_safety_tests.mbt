// Azimuth 并发和线程安全增强测试
// 专注于并发编程、线程安全、同步机制和资源管理

// 测试1: 并发任务执行和协调
test "并发任务执行和协调测试" {
  // 创建并发任务管理器
  let task_manager = ConcurrentTaskManager::new()
  
  // 配置任务执行器
  ConcurrentTaskManager::configure_executor(task_manager, {
    max_workers: 8,
    queue_capacity: 1000,
    thread_pool_type: "fixed",
    rejection_policy: "caller_runs"
  })
  
  // 创建测试任务
  let simple_task = fn() {
    let mut sum = 0
    for i in 0..=1000 {
      sum = sum + i
    }
    sum
  }
  
  let io_task = fn() {
    // 模拟IO操作
    let start_time = Time::now()
    while Time::now() - start_time < 100 {  // 100ms
      // 模拟等待
    }
    "io_operation_completed"
  }
  
  let cpu_intensive_task = fn() {
    // 模拟CPU密集型操作
    let mut result = 1
    for i in 1..=10000 {
      result = result * i % 1000000
    }
    result
  }
  
  // 提交单个任务
  let simple_task_future = ConcurrentTaskManager::submit_task(task_manager, simple_task)
  let io_task_future = ConcurrentTaskManager::submit_task(task_manager, io_task)
  let cpu_task_future = ConcurrentTaskManager::submit_task(task_manager, cpu_intensive_task)
  
  // 等待任务完成
  let simple_result = ConcurrentTaskManager::get_task_result(simple_task_future)
  let io_result = ConcurrentTaskManager::get_task_result(io_task_future)
  let cpu_result = ConcurrentTaskManager::get_task_result(cpu_task_future)
  
  // 验证任务结果
  assert_eq(simple_result, 500500)  // 0 + 1 + ... + 1000 = 500500
  assert_eq(io_result, "io_operation_completed")
  assert_true(cpu_result >= 0 and cpu_result < 1000000)
  
  // 批量提交任务
  let batch_tasks = []
  for i in 0..=50 {
    let task_id = i
    batch_tasks.push(fn() {
      let mut result = 1
      for j in 1..=1000 {
        result = result * j % (task_id + 1)
      }
      (task_id, result)
    })
  }
  
  let batch_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, batch_tasks)
  
  // 等待所有批量任务完成
  let batch_results = ConcurrentTaskManager::get_batch_task_results(batch_futures)
  
  // 验证批量任务结果
  assert_eq(batch_results.length(), 51)
  
  for (task_id, result) in batch_results {
    assert_true(task_id >= 0 and task_id <= 50)
    assert_true(result >= 0)
  }
  
  // 测试任务链式执行
  let task_chain = ConcurrentTaskManager::create_task_chain(task_manager)
  
  let step1 = fn() { 10 }
  let step2 = fn(x) { x * 2 }
  let step3 = fn(x) { x + 5 }
  let step4 = fn(x) { x / 3 }
  
  let chain_result = ConcurrentTaskManager::execute_chain(task_chain, [step1, step2, step3, step4])
  assert_eq(chain_result, 8)  // ((10 * 2) + 5) / 3 = 8.33，整数除法为8
  
  // 测试任务条件执行
  let conditional_task = fn(condition: Bool) {
    if condition {
      ConcurrentTaskManager::submit_task(task_manager, fn() { "condition_true" })
    } else {
      ConcurrentTaskManager::submit_task(task_manager, fn() { "condition_false" })
    }
  }
  
  let true_result = ConcurrentTaskManager::get_task_result(conditional_task(true))
  let false_result = ConcurrentTaskManager::get_task_result(conditional_task(false))
  
  assert_eq(true_result, "condition_true")
  assert_eq(false_result, "condition_false")
  
  // 测试任务超时
  let timeout_task = fn() {
    // 模拟长时间运行的任务
    let start_time = Time::now()
    while Time::now() - start_time < 5000 {  // 5秒
      // 模拟长时间工作
    }
    "long_task_completed"
  }
  
  let timeout_future = ConcurrentTaskManager::submit_task_with_timeout(task_manager, timeout_task, 1000)  // 1秒超时
  let timeout_result = ConcurrentTaskManager::get_task_result_with_timeout(timeout_future, 1000)
  
  // 由于任务超时，应该返回错误或默认值
  assert_true(timeout_result.is_error() or timeout_result == "timeout")
  
  // 验证任务执行统计
  let execution_stats = ConcurrentTaskManager::get_execution_stats(task_manager)
  assert_true(execution_stats.total_tasks_submitted > 0)
  assert_true(execution_stats.total_tasks_completed > 0)
  assert_true(execution_stats.avg_execution_time_ms > 0)
  assert_true(execution_stats.max_execution_time_ms >= execution_stats.avg_execution_time_ms)
}

// 测试2: 线程安全数据结构
test "线程安全数据结构测试" {
  // 创建线程安全计数器
  let atomic_counter = AtomicCounter::new(0)
  
  // 并发增加计数器
  let increment_tasks = []
  for i in 0..=100 {
    increment_tasks.push(fn() {
      for j in 0..=100 {
        AtomicCounter::increment(atomic_counter)
      }
    })
  }
  
  // 并发执行增加任务
  let task_manager = ConcurrentTaskManager::new()
  ConcurrentTaskManager::configure_executor(task_manager, {
    max_workers: 10,
    queue_capacity: 1000,
    thread_pool_type: "fixed",
    rejection_policy: "caller_runs"
  })
  
  let increment_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, increment_tasks)
  ConcurrentTaskManager::get_batch_task_results(increment_futures)
  
  // 验证计数器最终值
  let final_count = AtomicCounter::get_value(atomic_counter)
  assert_eq(final_count, 101 * 101)  // 101个任务，每个任务增加101次
  
  // 重置计数器
  AtomicCounter::reset(atomic_counter)
  assert_eq(AtomicCounter::get_value(atomic_counter), 0)
  
  // 测试原子操作
  let atomic_value = AtomicInt::new(42)
  
  // 原子增加
  let incremented = AtomicInt::add_and_get(atomic_value, 8)
  assert_eq(incremented, 50)
  assert_eq(AtomicInt::get(atomic_value), 50)
  
  // 原子减少
  let decremented = AtomicInt::subtract_and_get(atomic_value, 20)
  assert_eq(decremented, 30)
  assert_eq(AtomicInt::get(atomic_value), 30)
  
  // 比较并交换
  let cas_success = AtomicInt::compare_and_swap(atomic_value, 30, 100)
  assert_true(cas_success)
  assert_eq(AtomicInt::get(atomic_value), 100)
  
  let cas_failure = AtomicInt::compare_and_swap(atomic_value, 30, 200)
  assert_false(cas_failure)
  assert_eq(AtomicInt::get(atomic_value), 100)
  
  // 测试线程安全映射
  let concurrent_map = ConcurrentMap::new()
  
  // 并发插入键值对
  let insert_tasks = []
  for i in 0..=50 {
    let task_id = i
    insert_tasks.push(fn() {
      for j in 0..=10 {
        let key = "key-" + (task_id * 10 + j).to_string()
        let value = "value-" + (task_id * 10 + j).to_string()
        ConcurrentMap::put(concurrent_map, key, value)
      }
    })
  }
  
  // 并发执行插入任务
  let insert_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, insert_tasks)
  ConcurrentTaskManager::get_batch_task_results(insert_futures)
  
  // 验证映射大小
  let map_size = ConcurrentMap::size(concurrent_map)
  assert_eq(map_size, 511)  // 51个任务，每个任务插入11个键值对
  
  // 并发读取键值对
  let get_tasks = []
  for i in 0..=50 {
    let task_id = i
    get_tasks.push(fn() {
      let mut results = []
      for j in 0..=10 {
        let key = "key-" + (task_id * 10 + j).to_string()
        let value = ConcurrentMap::get(concurrent_map, key)
        results.push((key, value))
      }
      results
    })
  }
  
  // 并发执行读取任务
  let get_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, get_tasks)
  let get_results = ConcurrentTaskManager::get_batch_task_results(get_results)
  
  // 验证读取结果
  assert_eq(get_results.length(), 51)
  
  for task_results in get_results {
    assert_eq(task_results.length(), 11)
    for (key, value) in task_results {
      assert_true(key.starts_with("key-"))
      assert_true(value != None)
      match value {
        Some(v) => assert_eq(v, "value-" + key.substring(4))
        None => assert_true(false)
      }
    }
  }
  
  // 测试线程安全队列
  let concurrent_queue = ConcurrentQueue::new()
  
  // 并发入队
  let enqueue_tasks = []
  for i in 0..=20 {
    let task_id = i
    enqueue_tasks.push(fn() {
      for j in 0..=25 {
        let item = "item-" + (task_id * 25 + j).to_string()
        ConcurrentQueue::enqueue(concurrent_queue, item)
      }
    })
  }
  
  // 并发执行入队任务
  let enqueue_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, enqueue_tasks)
  ConcurrentTaskManager::get_batch_task_results(enqueue_futures)
  
  // 验证队列大小
  let queue_size = ConcurrentQueue::size(concurrent_queue)
  assert_eq(queue_size, 525)  // 21个任务，每个任务入队26个元素
  
  // 并发出队
  let dequeue_tasks = []
  for i in 0..=10 {
    dequeue_tasks.push(fn() {
      let mut results = []
      for j in 0..=25 {
        let item = ConcurrentQueue::dequeue(concurrent_queue)
        if item != None {
          results.push(item.unwrap())
        }
      }
      results
    })
  }
  
  // 并发执行出队任务
  let dequeue_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, dequeue_tasks)
  let dequeue_results = ConcurrentTaskManager::get_batch_task_results(dequeue_futures)
  
  // 验证出队结果
  let total_dequeued = dequeue_results.reduce(fn(acc, results) { acc + results.length() }, 0)
  assert_eq(total_dequeued, 275)  // 11个任务，每个任务出队26个元素
  
  // 验证队列剩余大小
  let remaining_size = ConcurrentQueue::size(concurrent_queue)
  assert_eq(remaining_size, 250)  // 525 - 275 = 250
  
  // 测试线程安全集合
  let concurrent_set = ConcurrentSet::new()
  
  // 并发添加元素（可能有重复）
  let add_tasks = []
  for i in 0..=20 {
    let task_id = i
    add_tasks.push(fn() {
      for j in 0..=25 {
        let element = "element-" + ((task_id + j) % 50).to_string()  // 故意制造重复
        ConcurrentSet::add(concurrent_set, element)
      }
    })
  }
  
  // 并发执行添加任务
  let add_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, add_tasks)
  ConcurrentTaskManager::get_batch_task_results(add_futures)
  
  // 验证集合大小（应该小于等于总添加次数）
  let set_size = ConcurrentSet::size(concurrent_set)
  assert_true(set_size <= 525)  // 最多525个元素
  assert_true(set_size >= 50)   // 至少50个不同元素
  
  // 验证集合包含性
  assert_true(ConcurrentSet::contains(concurrent_set, "element-0"))
  assert_true(ConcurrentSet::contains(concurrent_set, "element-25"))
  assert_true(ConcurrentSet::contains(concurrent_set, "element-49"))
}

// 测试3: 同步机制和锁
test "同步机制和锁测试" {
  // 创建互斥锁
  let mutex = Mutex::new()
  
  // 共享资源
  let shared_resource = { mut value: 0 }
  
  // 使用互斥锁保护共享资源
  let mutex_tasks = []
  for i in 0..=100 {
    let task_id = i
    mutex_tasks.push(fn() {
      Mutex::lock(mutex)
      // 临界区开始
      let current_value = shared_resource.value
      // 模拟一些处理
      let mut sum = 0
      for j in 0..=100 {
        sum = sum + j
      }
      shared_resource.value = current_value + 1
      // 临界区结束
      Mutex::unlock(mutex)
      task_id
    })
  }
  
  // 并发执行互斥锁任务
  let task_manager = ConcurrentTaskManager::new()
  ConcurrentTaskManager::configure_executor(task_manager, {
    max_workers: 10,
    queue_capacity: 1000,
    thread_pool_type: "fixed",
    rejection_policy: "caller_runs"
  })
  
  let mutex_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, mutex_tasks)
  let mutex_results = ConcurrentTaskManager::get_batch_task_results(mutex_futures)
  
  // 验证互斥锁保护结果
  assert_eq(shared_resource.value, 101)  // 101个任务，每个任务增加1
  assert_eq(mutex_results.length(), 101)
  
  // 测试读写锁
  let rwlock = RwLock::new()
  
  // 共享数据
  let shared_data = { mut readers: 0, mut writers: 0, mut value: "initial_value" }
  
  // 读任务
  let read_tasks = []
  for i in 0..=50 {
    let task_id = i
    read_tasks.push(fn() {
      RwLock::read_lock(rwlock)
      // 读临界区开始
      let current_value = shared_data.value
      shared_data.readers = shared_data.readers + 1
      // 模拟读操作
      let start_time = Time::now()
      while Time::now() - start_time < 10 {  // 10ms读操作
        // 模拟读处理
      }
      let result = current_value + "_read_" + task_id.to_string()
      shared_data.readers = shared_data.readers - 1
      // 读临界区结束
      RwLock::read_unlock(rwlock)
      result
    })
  }
  
  // 写任务
  let write_tasks = []
  for i in 0..=20 {
    let task_id = i
    write_tasks.push(fn() {
      RwLock::write_lock(rwlock)
      // 写临界区开始
      shared_data.writers = shared_data.writers + 1
      shared_data.value = "updated_by_writer_" + task_id.to_string()
      // 模拟写操作
      let start_time = Time::now()
      while Time::now() - start_time < 20 {  // 20ms写操作
        // 模拟写处理
      }
      let result = shared_data.value
      shared_data.writers = shared_data.writers - 1
      // 写临界区结束
      RwLock::write_unlock(rwlock)
      result
    })
  }
  
  // 并发执行读任务和写任务
  let all_tasks = read_tasks + write_tasks
  let all_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, all_tasks)
  let all_results = ConcurrentTaskManager::get_batch_task_results(all_futures)
  
  // 验证读写锁结果
  assert_eq(all_results.length(), 72)  // 51个读任务 + 21个写任务
  assert_eq(shared_data.readers, 0)    // 所有读者都已释放
  assert_eq(shared_data.writers, 0)    // 所有作者都已释放
  
  // 验证读结果
  let read_results = all_results.filter(fn(r) { r.contains("_read_") })
  assert_eq(read_results.length(), 51)
  
  // 验证写结果
  let write_results = all_results.filter(fn(r) { r.contains("updated_by_writer_") })
  assert_eq(write_results.length(), 21)
  
  // 测试条件变量
  let condition = ConditionVariable::new()
  let mutex_cv = Mutex::new()
  
  // 共享状态
  let shared_state = { mut ready: false, mut processed: 0, mut data: [] }
  
  // 生产者任务
  let producer_tasks = []
  for i in 0..=10 {
    let task_id = i
    producer_tasks.push(fn() {
      Mutex::lock(mutex_cv)
      // 等待消费者处理完之前的数据
      while shared_state.ready {
        ConditionVariable::wait(condition, mutex_cv)
      }
      
      // 生产数据
      shared_state.data = shared_state.data.push("data_" + task_id.to_string())
      shared_state.ready = true
      
      // 通知消费者
      ConditionVariable::notify_all(condition)
      Mutex::unlock(mutex_cv)
      task_id
    })
  }
  
  // 消费者任务
  let consumer_tasks = []
  for i in 0..=5 {
    let task_id = i
    consumer_tasks.push(fn() {
      let mut consumed = 0
      while consumed < 11 {  // 消费11个生产者的数据
        Mutex::lock(mutex_cv)
        
        // 等待数据准备就绪
        while not(shared_state.ready) {
          ConditionVariable::wait(condition, mutex_cv)
        }
        
        // 消费数据
        if shared_state.data.length() > 0 {
          let data = shared_state.data[0]
          shared_state.data = shared_state.data.slice(1)
          shared_state.processed = shared_state.processed + 1
          consumed = consumed + 1
          
          // 如果没有更多数据，标记为未就绪
          if shared_state.data.length() == 0 {
            shared_state.ready = false
          }
          
          // 通知生产者
          ConditionVariable::notify_all(condition)
        }
        
        Mutex::unlock(mutex_cv)
      }
      task_id
    })
  }
  
  // 并发执行生产者和消费者任务
  let cv_tasks = producer_tasks + consumer_tasks
  let cv_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, cv_tasks)
  let cv_results = ConcurrentTaskManager::get_batch_task_results(cv_futures)
  
  // 验证条件变量结果
  assert_eq(cv_results.length(), 17)  // 11个生产者 + 6个消费者
  assert_eq(shared_state.processed, 66)  // 11个数据 × 6个消费者
  assert_eq(shared_state.data.length(), 0)  // 所有数据都被消费
  
  // 测试信号量
  let semaphore = Semaphore::new(3)  // 最多3个并发访问
  
  // 信号量控制的任务
  let semaphore_tasks = []
  for i in 0..=20 {
    let task_id = i
    semaphore_tasks.push(fn() {
      Semaphore::acquire(semaphore)
      
      // 受信号量限制的临界区
      let start_time = Time::now()
      while Time::now() - start_time < 50 {  // 50ms操作
        // 模拟受限资源访问
      }
      let result = "task_" + task_id.to_string() + "_completed"
      
      Semaphore::release(semaphore)
      result
    })
  }
  
  // 并发执行信号量控制的任务
  let semaphore_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, semaphore_tasks)
  let semaphore_results = ConcurrentTaskManager::get_batch_task_results(semaphore_futures)
  
  // 验证信号量结果
  assert_eq(semaphore_results.length(), 21)
  
  for result in semaphore_results {
    assert_true(result.contains("_completed"))
  }
  
  // 验证信号量可用许可
  let available_permits = Semaphore::available_permits(semaphore)
  assert_eq(available_permits, 3)  // 所有任务都已释放许可
}

// 测试4: 并发集合操作
test "并发集合操作测试" {
  // 创建并发列表
  let concurrent_list = ConcurrentList::new()
  
  // 并发添加元素
  let add_tasks = []
  for i in 0..=50 {
    let task_id = i
    add_tasks.push(fn() {
      for j in 0..=10 {
        let element = "element_" + (task_id * 10 + j).to_string()
        ConcurrentList::add(concurrent_list, element)
      }
      task_id
    })
  }
  
  // 并发执行添加任务
  let task_manager = ConcurrentTaskManager::new()
  ConcurrentTaskManager::configure_executor(task_manager, {
    max_workers: 10,
    queue_capacity: 1000,
    thread_pool_type: "fixed",
    rejection_policy: "caller_runs"
  })
  
  let add_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, add_tasks)
  let add_results = ConcurrentTaskManager::get_batch_task_results(add_futures)
  
  // 验证添加结果
  assert_eq(add_results.length(), 51)
  assert_eq(ConcurrentList::size(concurrent_list), 561)  // 51个任务，每个任务添加11个元素
  
  // 并发迭代和修改
  let iterate_tasks = []
  for i in 0..=10 {
    let task_id = i
    iterate_tasks.push(fn() {
      let mut count = 0
      let iterator = ConcurrentList::iterator(concurrent_list)
      while Iterator::has_next(iterator) {
        let element = Iterator::next(iterator)
        if element.contains("element_") {
          count = count + 1
        }
        
        // 偶数任务添加新元素
        if task_id % 2 == 0 and count % 50 == 0 {
          ConcurrentList::add(concurrent_list, "new_element_" + task_id.to_string() + "_" + count.to_string())
        }
      }
      count
    })
  }
  
  // 并发执行迭代任务
  let iterate_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, iterate_tasks)
  let iterate_results = ConcurrentTaskManager::get_batch_task_results(iterate_futures)
  
  // 验证迭代结果
  assert_eq(iterate_results.length(), 11)
  
  for count in iterate_results {
    assert_true(count > 0)
  }
  
  // 验证列表大小（应该增加，因为有迭代任务添加了新元素）
  let final_size = ConcurrentList::size(concurrent_list)
  assert_true(final_size > 561)
  
  // 测试并发跳表（跳过列表）
  let concurrent_skip_list = ConcurrentSkipList::new()
  
  // 并发插入键值对
  let skip_list_tasks = []
  for i in 0..=30 {
    let task_id = i
    skip_list_tasks.push(fn() {
      for j in 0..=20 {
        let key = task_id * 20 + j
        let value = "value_" + key.to_string()
        ConcurrentSkipList::put(concurrent_skip_list, key, value)
      }
      task_id
    })
  }
  
  // 并发执行跳表插入任务
  let skip_list_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, skip_list_tasks)
  let skip_list_results = ConcurrentTaskManager::get_batch_task_results(skip_list_futures)
  
  // 验证跳表插入结果
  assert_eq(skip_list_results.length(), 31)
  assert_eq(ConcurrentSkipList::size(concurrent_skip_list), 631)  // 31个任务，每个任务插入21个键值对
  
  // 并发查找操作
  let search_tasks = []
  for i in 0..=20 {
    let task_id = i
    search_tasks.push(fn() {
      let mut found_count = 0
      for j in 0..=30 {
        let key = task_id * 30 + j
        let value = ConcurrentSkipList::get(concurrent_skip_list, key)
        if value != None {
          found_count = found_count + 1
        }
      }
      found_count
    })
  }
  
  // 并发执行跳表查找任务
  let search_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, search_tasks)
  let search_results = ConcurrentTaskManager::get_batch_task_results(search_futures)
  
  // 验证跳表查找结果
  assert_eq(search_results.length(), 21)
  
  let total_found = search_results.reduce(fn(acc, count) { acc + count }, 0)
  assert_true(total_found > 0)
  
  // 并发范围查询
  let range_tasks = []
  for i in 0..=10 {
    let start_key = i * 60
    let end_key = start_key + 50
    range_tasks.push(fn() {
      ConcurrentSkipList::range(concurrent_skip_list, start_key, end_key)
    })
  }
  
  // 并发执行范围查询任务
  let range_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, range_tasks)
  let range_results = ConcurrentTaskManager::get_batch_task_results(range_futures)
  
  // 验证范围查询结果
  assert_eq(range_results.length(), 11)
  
  for range_result in range_results {
    assert_true(range_result.length() >= 0)
    for (key, value) in range_result {
      assert_true(key >= 0)
      assert_true(value.starts_with("value_"))
    }
  }
  
  // 测试并发阻塞队列
  let blocking_queue = BlockingQueue::new(100)  // 容量100
  
  // 生产者任务
  let producer_tasks = []
  for i in 0..=10 {
    let task_id = i
    producer_tasks.push(fn() {
      for j in 0..=20 {
        let item = "item_" + (task_id * 20 + j).to_string()
        BlockingQueue::put(blocking_queue, item)  // 如果队列满，会阻塞
      }
      task_id
    })
  }
  
  // 消费者任务
  let consumer_tasks = []
  for i in 0..=5 {
    let task_id = i
    consumer_tasks.push(fn() {
      let mut consumed = 0
      while consumed < 35 {  // 每个消费者消费35个元素
        let item = BlockingQueue::take(blocking_queue)  // 如果队列空，会阻塞
        if item.contains("item_") {
          consumed = consumed + 1
        }
      }
      task_id
    })
  }
  
  // 并发执行生产者和消费者任务
  let queue_tasks = producer_tasks + consumer_tasks
  let queue_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, queue_tasks)
  let queue_results = ConcurrentTaskManager::get_batch_task_results(queue_futures)
  
  // 验证阻塞队列结果
  assert_eq(queue_results.length(), 17)  // 11个生产者 + 6个消费者
  assert_eq(BlockingQueue::size(blocking_queue), 35)  // 231个生产 - 210个消费 = 21剩余
  
  // 测试并发优先级队列
  let priority_queue = ConcurrentPriorityQueue::new()
  
  // 并发插入不同优先级的元素
  let priority_tasks = []
  for i in 0..=20 {
    let task_id = i
    priority_tasks.push(fn() {
      for j in 0..=10 {
        let priority = (task_id + j) % 10  // 优先级0-9
        let value = "task_" + task_id.to_string() + "_item_" + j.to_string()
        ConcurrentPriorityQueue::offer(priority_queue, priority, value)
      }
      task_id
    })
  }
  
  // 并发执行优先级队列插入任务
  let priority_futures = ConcurrentTaskManager::submit_batch_tasks(task_manager, priority_tasks)
  let priority_results = ConcurrentTaskManager::get_batch_task_results(priority_futures)
  
  // 验证优先级队列插入结果
  assert_eq(priority_results.length(), 21)
  assert_eq(ConcurrentPriorityQueue::size(priority_queue), 231)  // 21个任务，每个任务插入11个元素
  
  // 验证优先级顺序
  let mut last_priority = -1
  while ConcurrentPriorityQueue::size(priority_queue) > 0 {
    let (priority, value) = ConcurrentPriorityQueue::poll(priority_queue)
    assert_true(priority >= last_priority)  // 优先级应该非递减
    last_priority = priority
  }
}

// 测试5: 线程池和资源管理
test "线程池和资源管理测试" {
  // 创建固定大小线程池
  let fixed_thread_pool = ThreadPool::fixed(5)
  
  // 提交任务到固定线程池
  let fixed_tasks = []
  for i in 0..=50 {
    let task_id = i
    fixed_tasks.push(fn() {
      let mut result = 1
      for j in 1..=1000 {
        result = result * j % 10000
      }
      "fixed_task_" + task_id.to_string() + "_result_" + result.to_string()
    })
  }
  
  let fixed_futures = ThreadPool::submit_all(fixed_thread_pool, fixed_tasks)
  let fixed_results = ThreadPool::get_all_results(fixed_futures)
  
  // 验证固定线程池结果
  assert_eq(fixed_results.length(), 51)
  
  for result in fixed_results {
    assert_true(result.contains("fixed_task_"))
    assert_true(result.contains("_result_"))
  }
  
  // 获取线程池统计信息
  let fixed_stats = ThreadPool::get_stats(fixed_thread_pool)
  assert_eq(fixed_stats.core_pool_size, 5)
  assert_eq(fixed_stats.maximum_pool_size, 5)
  assert_true(fixed_stats.completed_task_count >= 51)
  assert_true(fixed_stats.active_count <= 5)
  
  // 创建缓存线程池
  let cached_thread_pool = ThreadPool::cached()
  
  // 提交大量短时任务到缓存线程池
  let cached_tasks = []
  for i in 0..=200 {
    let task_id = i
    cached_tasks.push(fn() {
      // 短时任务
      let mut sum = 0
      for j in 0..=50 {
        sum = sum + j
      }
      "cached_task_" + task_id.to_string() + "_sum_" + sum.to_string()
    })
  }
  
  let cached_futures = ThreadPool::submit_all(cached_thread_pool, cached_tasks)
  let cached_results = ThreadPool::get_all_results(cached_futures)
  
  // 验证缓存线程池结果
  assert_eq(cached_results.length(), 201)
  
  for result in cached_results {
    assert_true(result.contains("cached_task_"))
    assert_true(result.contains("_sum_"))
  }
  
  // 获取缓存线程池统计信息
  let cached_stats = ThreadPool::get_stats(cached_thread_pool)
  assert_eq(cached_stats.core_pool_size, 0)  // 缓存线程池核心大小为0
  assert_true(cached_stats.maximum_pool_size > 5)  // 最大线程数应该大于5
  assert_true(cached_stats.completed_task_count >= 201)
  assert_true(cached_stats.largest_pool_size >= 5)  // 应该创建了多个线程
  
  // 创建自定义线程池
  let custom_thread_pool = ThreadPool::custom({
    core_pool_size: 3,
    maximum_pool_size: 10,
    keep_alive_time: 60000,  // 60秒
    work_queue: "linked_blocking_queue",
    queue_capacity: 50,
    thread_factory: "default",
    rejected_execution_handler: "caller_runs_policy"
  })
  
  // 提交任务到自定义线程池
  let custom_tasks = []
  for i in 0..=100 {
    let task_id = i
    custom_tasks.push(fn() {
      // 模拟不同执行时间的任务
      let execution_time = 50 + (task_id % 200)  // 50-250ms
      let start_time = Time::now()
      while Time::now() - start_time < execution_time {
        // 模拟工作
      }
      "custom_task_" + task_id.to_string() + "_duration_" + execution_time.to_string()
    })
  }
  
  let custom_futures = ThreadPool::submit_all(custom_thread_pool, custom_tasks)
  let custom_results = ThreadPool::get_all_results(custom_futures)
  
  // 验证自定义线程池结果
  assert_eq(custom_results.length(), 101)
  
  for result in custom_results {
    assert_true(result.contains("custom_task_"))
    assert_true(result.contains("_duration_"))
  }
  
  // 获取自定义线程池统计信息
  let custom_stats = ThreadPool::get_stats(custom_thread_pool)
  assert_eq(custom_stats.core_pool_size, 3)
  assert_eq(custom_stats.maximum_pool_size, 10)
  assert_true(custom_stats.completed_task_count >= 101)
  
  // 测试资源清理
  ThreadPool::shutdown(fixed_thread_pool)
  ThreadPool::shutdown(cached_thread_pool)
  ThreadPool::shutdown(custom_thread_pool)
  
  // 验证线程池已关闭
  assert_true(ThreadPool::is_shutdown(fixed_thread_pool))
  assert_true(ThreadPool::is_shutdown(cached_thread_pool))
  assert_true(ThreadPool::is_shutdown(custom_thread_pool))
  
  // 测试资源泄漏检测
  let resource_tracker = ResourceTracker::new()
  
  // 创建资源密集型任务
  let resource_tasks = []
  for i in 0..=20 {
    let task_id = i
    resource_tasks.push(fn() {
      // 分配资源
      let resource_id = ResourceTracker::allocate(resource_tracker, "memory", 1024 * 1024)  // 1MB
      
      // 使用资源
      let mut data = []
      for j in 0..=1000 {
        data = data.push("data_" + j.to_string())
      }
      
      // 模拟处理
      let start_time = Time::now()
      while Time::now() - start_time < 100 {  // 100ms处理
        // 模拟工作
      }
      
      // 释放资源
      ResourceTracker::deallocate(resource_tracker, resource_id)
      "resource_task_" + task_id.to_string() + "_completed"
    })
  }
  
  // 创建新的线程池用于资源测试
  let resource_thread_pool = ThreadPool::fixed(5)
  
  // 提交资源密集型任务
  let resource_futures = ThreadPool::submit_all(resource_thread_pool, resource_tasks)
  let resource_results = ThreadPool::get_all_results(resource_futures)
  
  // 验证资源任务结果
  assert_eq(resource_results.length(), 21)
  
  for result in resource_results {
    assert_true(result.contains("resource_task_"))
    assert_true(result.contains("_completed"))
  }
  
  // 关闭资源线程池
  ThreadPool::shutdown(resource_thread_pool)
  
  // 验证资源清理
  let resource_stats = ResourceTracker::get_stats(resource_tracker)
  assert_eq(resource_stats.allocated_resources, 0)  // 所有资源应该已释放
  assert_eq(resource_stats.total_allocated, 21)     // 总共分配了21个资源
  assert_eq(resource_stats.total_deallocated, 21)   // 总共释放了21个资源
  
  // 测试线程安全资源池
  let resource_pool = ResourcePool::new(10, fn() { "resource_" + Time::now().to_string() })
  
  // 并发获取和释放资源
  let pool_tasks = []
  for i in 0..=50 {
    let task_id = i
    pool_tasks.push(fn() {
      let resource = ResourcePool::acquire(resource_pool)
      
      // 使用资源
      let start_time = Time::now()
      while Time::now() - start_time < 50 {  // 50ms使用时间
        // 模拟资源使用
      }
      
      // 释放资源
      ResourcePool::release(resource_pool, resource)
      task_id
    })
  }
  
  // 创建新的线程池用于资源池测试
  let pool_thread_pool = ThreadPool::fixed(5)
  
  // 提交资源池任务
  let pool_futures = ThreadPool::submit_all(pool_thread_pool, pool_tasks)
  let pool_results = ThreadPool::get_all_results(pool_futures)
  
  // 验证资源池任务结果
  assert_eq(pool_results.length(), 51)
  
  // 验证资源池状态
  let pool_stats = ResourcePool::get_stats(resource_pool)
  assert_eq(pool_stats.total_resources, 10)      // 总资源数
  assert_eq(pool_stats.available_resources, 10)  // 所有资源都已释放
  assert_eq(pool_stats.acquired_count, 51)       // 总共获取了51次
  assert_eq(pool_stats.released_count, 51)       // 总共释放了51次
  
  // 关闭资源池线程池
  ThreadPool::shutdown(pool_thread_pool)
}