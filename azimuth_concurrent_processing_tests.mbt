// Azimuth 并发处理测试用例
// 专注于并发安全、线程同步和资源管理

// 测试1: 基本并发操作
test "基本并发操作" {
  // 创建并发管理器
  let concurrent_manager = ConcurrentManager::new()
  
  // 创建共享计数器
  let counter = SharedCounter::new(0)
  
  // 启动多个并发任务增加计数器
  let tasks = []
  for i in 0..=10 {
    let task = ConcurrentManager::spawn(concurrent_manager, fn() {
      for j in 0..=100 {
        SharedCounter::increment(counter)
        // 模拟一些工作
        Thread::sleep(1)
      }
    })
    tasks = tasks.push(task)
  }
  
  // 等待所有任务完成
  for task in tasks {
    ConcurrentManager::join(concurrent_manager, task)
  }
  
  // 验证最终结果
  let final_count = SharedCounter::get(counter)
  assert_eq(final_count, 1100)  // 11个任务 × 100次递增
  
  // 测试并发原子操作
  let atomic_value = AtomicInt::new(0)
  
  let atomic_tasks = []
  for i in 0..=5 {
    let task = ConcurrentManager::spawn(concurrent_manager, fn() {
      let task_id = i
      for j in 0..=50 {
        AtomicInt::add(atomic_value, task_id)
        Thread::sleep(2)
      }
    })
    atomic_tasks = atomic_tasks.push(task)
  }
  
  // 等待所有原子任务完成
  for task in atomic_tasks {
    ConcurrentManager::join(concurrent_manager, task)
  }
  
  // 验证原子操作结果
  let expected_atomic_sum = 0 + 1 + 2 + 3 + 4 + 5  // 任务ID之和
  let final_atomic_value = AtomicInt::get(atomic_value)
  assert_eq(final_atomic_value, expected_atomic_sum * 50)  // 每个任务执行50次
}

// 测试2: 并发数据结构
test "并发数据结构" {
  // 创建并发队列
  let concurrent_queue = ConcurrentQueue::new()
  
  // 启动生产者任务
  let producer_tasks = []
  for i in 0..=3 {
    let task = ConcurrentManager::spawn(fn() {
      let producer_id = i
      for j in 0..=25 {
        let item = "item-" + producer_id.to_string() + "-" + j.to_string()
        ConcurrentQueue::enqueue(concurrent_queue, item)
        Thread::sleep(10)
      }
    })
    producer_tasks = producer_tasks.push(task)
  }
  
  // 启动消费者任务
  let consumed_items = SharedList::new()
  let consumer_tasks = []
  for i in 0..=2 {
    let task = ConcurrentManager::spawn(fn() {
      let consumer_id = i
      let mut items_consumed = 0
      
      while items_consumed < 35 {  // 每个消费者消费35个项目
        match ConcurrentQueue::try_dequeue(concurrent_queue) {
          Some(item) => {
            SharedList::add(consumed_items, item + "-by-" + consumer_id.to_string())
            items_consumed = items_consumed + 1
          }
          None => Thread::sleep(5)
        }
      }
    })
    consumer_tasks = consumer_tasks.push(task)
  }
  
  // 等待所有生产者完成
  for task in producer_tasks {
    ConcurrentManager::join(task)
  }
  
  // 等待所有消费者完成
  for task in consumer_tasks {
    ConcurrentManager::join(task)
  }
  
  // 验证消费的项目数量
  let final_consumed_count = SharedList::size(consumed_items)
  assert_eq(final_consumed_count, 105)  // 3个消费者 × 35个项目
  
  // 验证队列最终为空
  assert_true(ConcurrentQueue::is_empty(concurrent_queue))
  
  // 测试并发映射
  let concurrent_map = ConcurrentMap::new()
  
  // 启动多个任务并发更新映射
  let map_tasks = []
  for i in 0..=5 {
    let task = ConcurrentManager::spawn(fn() {
      let task_id = i
      for j in 0..=20 {
        let key = "key-" + (j % 10).to_string()  // 10个不同的键
        let value = task_id.to_string() + "-" + j.to_string()
        ConcurrentMap::insert(concurrent_map, key, value)
        Thread::sleep(1)
      }
    })
    map_tasks = map_tasks.push(task)
  }
  
  // 等待所有映射任务完成
  for task in map_tasks {
    ConcurrentManager::join(task)
  }
  
  // 验证映射大小
  let map_size = ConcurrentMap::size(concurrent_map)
  assert_eq(map_size, 10)  // 10个不同的键
  
  // 验证每个键都有值
  for i in 0..=9 {
    let key = "key-" + i.to_string()
    assert_true(ConcurrentMap::contains_key(concurrent_map, key))
    
    let value = ConcurrentMap::get(concurrent_map, key)
    assert_true(value != None)
  }
}

// 测试3: 锁和互斥
test "锁和互斥机制" {
  // 创建互斥锁保护的共享资源
  let protected_resource = {
    data: "initial",
    mutex: Mutex::new(),
    access_count: 0
  }
  
  // 启动多个任务并发访问受保护的资源
  let lock_tasks = []
  for i in 0..=5 {
    let task = ConcurrentManager::spawn(fn() {
      let task_id = i
      
      for j in 0..=10 {
        // 获取锁
        Mutex::lock(protected_resource.mutex)
        
        // 临界区：访问共享资源
        let old_data = protected_resource.data
        protected_resource.access_count = protected_resource.access_count + 1
        Thread::sleep(5)  # 模拟一些工作
        protected_resource.data = old_data + "-" + task_id.to_string()
        
        // 释放锁
        Mutex::unlock(protected_resource.mutex)
        
        Thread::sleep(10)
      }
    })
    lock_tasks = lock_tasks.push(task)
  }
  
  // 等待所有任务完成
  for task in lock_tasks {
    ConcurrentManager::join(task)
  }
  
  // 验证最终状态
  assert_eq(protected_resource.access_count, 60)  # 6个任务 × 10次访问
  
  // 验证数据完整性（应该包含所有任务ID）
  let final_data = protected_resource.data
  for i in 0..=5 {
    assert_true(final_data.contains(i.to_string()))
  }
  
  // 测试读写锁
  let rw_lock_resource = {
    data: [],
    lock: RWLock::new(),
    readers: 0,
    writers: 0
  }
  
  // 启动读者任务
  let reader_tasks = []
  for i in 0..=4 {
    let task = ConcurrentManager::spawn(fn() {
      let reader_id = i
      
      for j in 0..=5 {
        // 获取读锁
        RWLock::read_lock(rw_lock_resource.lock)
        
        // 读取数据
        rw_lock_resource.readers = rw_lock_resource.readers + 1
        let data_size = rw_lock_resource.data.length()
        
        // 模拟读取操作
        Thread::sleep(20)
        
        // 释放读锁
        RWLock::read_unlock(rw_lock_resource.lock)
        
        // 验证读取的数据大小合理
        assert_true(data_size >= 0)
      }
    })
    reader_tasks = reader_tasks.push(task)
  }
  
  // 启动写者任务
  let writer_tasks = []
  for i in 0..=2 {
    let task = ConcurrentManager::spawn(fn() {
      let writer_id = i
      
      for j in 0..=3 {
        // 获取写锁
        RWLock::write_lock(rw_lock_resource.lock)
        
        // 写入数据
        rw_lock_resource.writers = rw_lock_resource.writers + 1
        rw_lock_resource.data = rw_lock_resource.data.push("item-" + writer_id.to_string() + "-" + j.to_string())
        
        // 模拟写入操作
        Thread::sleep(30)
        
        // 释放写锁
        RWLock::write_unlock(rw_lock_resource.lock)
        
        Thread::sleep(50)
      }
    })
    writer_tasks = writer_tasks.push(task)
  }
  
  // 等待所有任务完成
  for task in reader_tasks {
    ConcurrentManager::join(task)
  }
  for task in writer_tasks {
    ConcurrentManager::join(task)
  }
  
  // 验证最终数据
  assert_eq(rw_lock_resource.data.length(), 9)  # 3个写者 × 3个项目
  assert_eq(rw_lock_resource.readers, 25)       # 5个读者 × 5次读取
  assert_eq(rw_lock_resource.writers, 9)        # 3个写者 × 3次写入
}

// 测试4: 条件变量
test "条件变量同步" {
  // 创建条件变量和共享状态
  let sync_state = {
    buffer: [],
    buffer_size: 5,
    mutex: Mutex::new(),
    not_empty: ConditionVariable::new(),
    not_full: ConditionVariable::new(),
    produced_count: 0,
    consumed_count: 0
  }
  
  // 启动生产者任务
  let producer_tasks = []
  for i in 0..=2 {
    let task = ConcurrentManager::spawn(fn() {
      let producer_id = i
      
      for j in 0..=10 {
        // 获取互斥锁
        Mutex::lock(sync_state.mutex)
        
        // 等待缓冲区不满
        while sync_state.buffer.length() >= sync_state.buffer_size {
          ConditionVariable::wait(sync_state.not_full, sync_state.mutex)
        }
        
        // 生产项目
        let item = "item-" + producer_id.to_string() + "-" + j.to_string()
        sync_state.buffer = sync_state.buffer.push(item)
        sync_state.produced_count = sync_state.produced_count + 1
        
        // 通知消费者
        ConditionVariable::notify_all(sync_state.not_empty)
        
        // 释放互斥锁
        Mutex::unlock(sync_state.mutex)
        
        Thread::sleep(50)
      }
    })
    producer_tasks = producer_tasks.push(task)
  }
  
  // 启动消费者任务
  let consumer_tasks = []
  for i in 0..=3 {
    let task = ConcurrentManager::spawn(fn() {
      let consumer_id = i
      
      for j in 0..=7 {  # 消费28个项目，总共生产33个
        // 获取互斥锁
        Mutex::lock(sync_state.mutex)
        
        // 等待缓冲区不空
        while sync_state.buffer.length() == 0 {
          ConditionVariable::wait(sync_state.not_empty, sync_state.mutex)
        }
        
        // 消费项目
        let item = sync_state.buffer[0]
        sync_state.buffer = sync_state.buffer.slice(1)
        sync_state.consumed_count = sync_state.consumed_count + 1
        
        // 通知生产者
        ConditionVariable::notify_all(sync_state.not_full)
        
        // 释放互斥锁
        Mutex::unlock(sync_state.mutex)
        
        // 模拟消费处理
        Thread::sleep(80)
      }
    })
    consumer_tasks = consumer_tasks.push(task)
  }
  
  // 等待所有生产者完成
  for task in producer_tasks {
    ConcurrentManager::join(task)
  }
  
  // 等待所有消费者完成
  for task in consumer_tasks {
    ConcurrentManager::join(task)
  }
  
  // 验证生产和消费计数
  assert_eq(sync_state.produced_count, 33)  # 3个生产者 × 11个项目
  assert_eq(sync_state.consumed_count, 28)  # 4个消费者 × 7个项目
  
  // 验证缓冲区中剩余项目
  assert_eq(sync_state.buffer.length(), 5)  # 33-28=5个剩余项目
}

// 测试5: 并发任务调度
test "并发任务调度" {
  // 创建任务调度器
  let scheduler = TaskScheduler::new(4)  # 4个工作线程
  
  // 创建任务结果收集器
  let results = ConcurrentMap::new()
  
  // 添加计算密集型任务
  for i in 0..=10 {
    let task_id = "compute-" + i.to_string()
    let task = Task::new(fn() {
      let mut sum = 0
      for j in 0..=1000 {
        sum = sum + j * j
      }
      Thread::sleep(100)  # 模拟计算时间
      sum
    })
    
    TaskScheduler::submit(scheduler, task_id, task, fn(task_id, result) {
      ConcurrentMap::insert(results, task_id, result)
    })
  }
  
  // 添加I/O密集型任务
  for i in 0..=5 {
    let task_id = "io-" + i.to_string()
    let task = Task::new(fn() {
      Thread::sleep(200)  # 模拟I/O等待时间
      "io-result-" + i.to_string()
    })
    
    TaskScheduler::submit(scheduler, task_id, task, fn(task_id, result) {
      ConcurrentMap::insert(results, task_id, result)
    })
  }
  
  // 添加快速任务
  for i in 0..=20 {
    let task_id = "fast-" + i.to_string()
    let task = Task::new(fn() {
      Thread::sleep(10)  # 快速任务
      i * 2
    })
    
    TaskScheduler::submit(scheduler, task_id, task, fn(task_id, result) {
      ConcurrentMap::insert(results, task_id, result)
    })
  }
  
  // 等待所有任务完成
  TaskScheduler::wait_for_all(scheduler)
  
  // 验证结果数量
  let results_count = ConcurrentMap::size(results)
  assert_eq(results_count, 37)  # 11 + 6 + 21个任务
  
  // 验证计算任务结果
  let expected_compute_sum = 0
  for j in 0..=1000 {
    expected_compute_sum = expected_compute_sum + j * j
  }
  
  for i in 0..=10 {
    let task_id = "compute-" + i.to_string()
    let result = ConcurrentMap::get(results, task_id)
    assert_true(result != None)
    assert_eq(result, expected_compute_sum)
  }
  
  // 验证I/O任务结果
  for i in 0..=5 {
    let task_id = "io-" + i.to_string()
    let result = ConcurrentMap::get(results, task_id)
    assert_true(result != None)
    assert_eq(result, "io-result-" + i.to_string())
  }
  
  // 验证快速任务结果
  for i in 0..=20 {
    let task_id = "fast-" + i.to_string()
    let result = ConcurrentMap::get(results, task_id)
    assert_true(result != None)
    assert_eq(result, i * 2)
  }
  
  // 获取调度统计
  let stats = TaskScheduler::get_stats(scheduler)
  assert_eq(stats.total_tasks, 37)
  assert_eq(stats.completed_tasks, 37)
  assert_eq(stats.failed_tasks, 0)
  assert_true(stats.total_execution_time > 0)
}

// 测试6: 并发限流和背压
test "并发限流和背压控制" {
  // 创建限流器
  let rate_limiter = RateLimiter::new(10)  # 每秒最多10个请求
  
  // 创建背压控制器
  let backpressure_controller = BackpressureController::new({
    max_queue_size: 50,
    high_watermark: 40,
    low_watermark: 20,
    rejection_strategy: "newest"
  })
  
  // 创建处理统计
  let processing_stats = {
    processed: AtomicInt::new(0),
    rejected: AtomicInt::new(0),
    queued: AtomicInt::new(0)
  }
  
  // 启动生产者任务
  let producer_task = ConcurrentManager::spawn(fn() {
    for i in 0..=100 {
      // 检查限流
      if RateLimiter::try_acquire(rate_limiter) {
        // 检查背压
        match BackpressureController::try_submit(backpressure_controller, i) {
          Success(_) => {
            AtomicInt::increment(processing_stats.queued)
          }
          Rejected(_) => {
            AtomicInt::increment(processing_stats.rejected)
          }
        }
      } else {
        AtomicInt::increment(processing_stats.rejected)
      }
      
      Thread::sleep(50)  # 每50ms一个请求，约20个请求/秒
    }
  })
  
  // 启动消费者任务
  let consumer_tasks = []
  for i in 0..=3 {
    let task = ConcurrentManager::spawn(fn() {
      let consumer_id = i
      
      while true {
        match BackpressureController::try_take(backpressure_controller) {
          Some(item) => {
            // 处理项目
            Thread::sleep(100)  # 模拟处理时间
            AtomicInt::increment(processing_stats.processed)
          }
          None => {
            Thread::sleep(10)
            
            // 检查是否所有生产者都完成了
            if ConcurrentManager::is_completed(producer_task) and 
               BackpressureController::queue_size(backpressure_controller) == 0 {
              break
            }
          }
        }
      }
    })
    consumer_tasks = consumer_tasks.push(task)
  }
  
  // 等待生产者完成
  ConcurrentManager::join(producer_task)
  
  // 等待所有消费者完成
  for task in consumer_tasks {
    ConcurrentManager::join(task)
  }
  
  // 验证处理统计
  let total_processed = AtomicInt::get(processing_stats.processed)
  let total_rejected = AtomicInt::get(processing_stats.rejected)
  let total_queued = AtomicInt::get(processing_stats.queued)
  
  assert_true(total_processed > 0)
  assert_true(total_rejected > 0)  # 由于限流和背压，应该有被拒绝的请求
  assert_eq(total_processed + BackpressureController::queue_size(backpressure_controller), total_queued)
  
  // 验证限流效果
  let rate_limiter_stats = RateLimiter::get_stats(rate_limiter)
  assert_true(rate_limiter_stats.permits_acquired <= 100)
  assert_true(rate_limiter_stats.permits_denied > 0)
  
  // 验证背压控制
  let backpressure_stats = BackpressureController::get_stats(backpressure_controller)
  assert_true(backpressure_stats.max_queue_size_reached >= 0)
  assert_true(backpressure_stats.rejected_count > 0)
}

// 测试7: 并发异常处理
test "并发异常处理" {
  // 创建异常处理管理器
  let exception_handler = ConcurrentExceptionHandler::new()
  
  // 设置异常处理策略
  ExceptionHandler::set_strategy(exception_handler, "retry", {
    max_retries: 3,
    backoff_strategy: "exponential",
    initial_delay: 100,
    max_delay: 1000
  })
  
  // 创建任务结果跟踪
  let task_results = ConcurrentMap::new()
  
  // 启动可能失败的任务
  let failing_tasks = []
  for i in 0..=10 {
    let task_id = "task-" + i.to_string()
    let task = ConcurrentManager::spawn(fn() {
      let task_id = i
      
      // 模拟不同的失败情况
      match task_id % 4 {
        0 => {
          // 总是失败的任务
          throw RuntimeError("Always failing task")
        }
        1 => {
          // 前几次失败，然后成功
          let mut attempt = 0
          while attempt < 2 {
            attempt = attempt + 1
            if attempt < 2 {
              throw RuntimeError("Temporary failure " + attempt.to_string())
            }
          }
          "success-after-retries"
        }
        2 => {
          // 随机失败
          if Math::random() > 0.5 {
            throw RuntimeError("Random failure")
          }
          "random-success"
        }
        _ => {
          // 总是成功的任务
          "always-success"
        }
      }
    })
    
    // 注册异常处理
    ExceptionHandler::handle_task(exception_handler, task_id, task, fn(task_id, result) {
      match result {
        Success(value) => {
          ConcurrentMap::insert(task_results, task_id + "-result", value)
        }
        Error(error) => {
          ConcurrentMap::insert(task_results, task_id + "-error", error.message)
        }
      }
    })
    
    failing_tasks = failing_tasks.push((task_id, task))
  }
  
  // 等待所有任务完成
  for (task_id, task) in failing_tasks {
    ConcurrentManager::join(task)
  }
  
  // 等待异常处理完成
  ExceptionHandler::wait_for_all(exception_handler)
  
  // 验证结果
  assert_eq(ConcurrentMap::size(task_results), 11)  # 所有任务都应该有结果
  
  // 检查总是失败的任务
  for i in 0..=2 {
    let task_id = "task-" + (i * 4).to_string()
    let error_key = task_id + "-error"
    let error = ConcurrentMap::get(task_results, error_key)
    assert_true(error != None)
    assert_true(error.contains("Always failing task"))
  }
  
  // 检查重试成功的任务
  let retry_task_id = "task-1"
  let retry_result = ConcurrentMap::get(task_results, retry_task_id + "-result")
  assert_true(retry_result != None)
  assert_eq(retry_result, "success-after-retries")
  
  // 检查总是成功的任务
  for i in 0..=2 {
    let task_id = "task-" + (i * 4 + 3).to_string()
    let success_result = ConcurrentMap::get(task_results, task_id + "-result")
    assert_true(success_result != None)
    assert_eq(success_result, "always-success")
  }
  
  // 获取异常处理统计
  let exception_stats = ExceptionHandler::get_stats(exception_handler)
  assert_true(exception_stats.total_tasks > 0)
  assert_true(exception_stats.failed_tasks > 0)
  assert_true(exception_stats.retry_attempts > 0)
}

// 测试8: 并发性能和资源监控
test "并发性能和资源监控" {
  // 创建性能监控器
  let performance_monitor = ConcurrentPerformanceMonitor::new()
  
  // 启动监控
  PerformanceMonitor::start(performance_monitor)
  
  // 创建资源使用跟踪
  let resource_tracker = ResourceTracker::new()
  
  // 启动CPU密集型任务
  let cpu_tasks = []
  let cpu_task_count = 4
  for i in 0..=cpu_task_count {
    let task = ConcurrentManager::spawn(fn() {
      let task_id = i
      let start_time = Time::now()
      
      // CPU密集型计算
      let mut result = 0
      for j in 0..=1000000 {
        result = result + j * j % 1000
      }
      
      let end_time = Time::now()
      let duration = end_time - start_time
      
      // 记录性能指标
      PerformanceMonitor::record_task_completion(performance_monitor, {
        task_id: "cpu-" + task_id.to_string(),
        task_type: "cpu_intensive",
        duration: duration,
        memory_usage: ResourceTracker::current_memory_usage(resource_tracker)
      })
      
      result
    })
    cpu_tasks = cpu_tasks.push(task)
  }
  
  // 启动内存密集型任务
  let memory_tasks = []
  let memory_task_count = 3
  for i in 0..=memory_task_count {
    let task = ConcurrentManager::spawn(fn() {
      let task_id = i
      let start_time = Time::now()
      
      // 内存密集型操作
      let large_arrays = []
      for j in 0..=100 {
        let large_array = []
        for k in 0..=10000 {
          large_array = large_array.push(k * j)
        }
        large_arrays = large_arrays.push(large_array)
      }
      
      let end_time = Time::now()
      let duration = end_time - start_time
      
      // 记录性能指标
      PerformanceMonitor::record_task_completion(performance_monitor, {
        task_id: "memory-" + task_id.to_string(),
        task_type: "memory_intensive",
        duration: duration,
        memory_usage: ResourceTracker::current_memory_usage(resource_tracker)
      })
      
      large_arrays.length()
    })
    memory_tasks = memory_tasks.push(task)
  }
  
  // 启动I/O密集型任务
  let io_tasks = []
  let io_task_count = 5
  for i in 0..=io_task_count {
    let task = ConcurrentManager::spawn(fn() {
      let task_id = i
      let start_time = Time::now()
      
      // I/O密集型操作（模拟）
      let io_results = []
      for j in 0..=20 {
        Thread::sleep(50)  # 模拟I/O等待
        io_results = io_results.push("io-result-" + j.to_string())
      }
      
      let end_time = Time::now()
      let duration = end_time - start_time
      
      // 记录性能指标
      PerformanceMonitor::record_task_completion(performance_monitor, {
        task_id: "io-" + task_id.to_string(),
        task_type: "io_intensive",
        duration: duration,
        memory_usage: ResourceTracker::current_memory_usage(resource_tracker)
      })
      
      io_results.length()
    })
    io_tasks = io_tasks.push(task)
  }
  
  // 等待所有任务完成
  for task in cpu_tasks {
    ConcurrentManager::join(task)
  }
  for task in memory_tasks {
    ConcurrentManager::join(task)
  }
  for task in io_tasks {
    ConcurrentManager::join(task)
  }
  
  // 停止监控
  PerformanceMonitor::stop(performance_monitor)
  
  // 获取性能报告
  let performance_report = PerformanceMonitor::generate_report(performance_monitor)
  
  // 验证性能报告
  assert_true(performance_report.total_tasks > 0)
  assert_eq(performance_report.task_counts.get("cpu_intensive"), Some(cpu_task_count + 1))
  assert_eq(performance_report.task_counts.get("memory_intensive"), Some(memory_task_count + 1))
  assert_eq(performance_report.task_counts.get("io_intensive"), Some(io_task_count + 1))
  
  // 验证任务类型性能差异
  let cpu_avg_duration = performance_report.average_durations.get("cpu_intensive")
  let memory_avg_duration = performance_report.average_durations.get("memory_intensive")
  let io_avg_duration = performance_report.average_durations.get("io_intensive")
  
  assert_true(cpu_avg_duration != None)
  assert_true(memory_avg_duration != None)
  assert_true(io_avg_duration != None)
  
  // I/O任务应该比CPU任务花费更长时间
  assert_true(io_avg_duration > cpu_avg_duration)
  
  // 内存任务应该使用更多内存
  let cpu_avg_memory = performance_report.average_memory_usage.get("cpu_intensive")
  let memory_avg_memory = performance_report.average_memory_usage.get("memory_intensive")
  
  assert_true(memory_avg_memory > cpu_avg_memory)
  
  // 获取资源使用统计
  let resource_stats = ResourceTracker::get_stats(resource_tracker)
  assert_true(resource_stats.peak_memory_usage > 0)
  assert_true(resource_stats.average_cpu_usage > 0)
  assert_true(resource_stats.total_execution_time > 0)
  
  // 验证并发度
  let concurrency_stats = PerformanceMonitor::get_concurrency_stats(performance_monitor)
  assert_true(concurrency_stats.max_concurrent_tasks > 0)
  assert_true(concurrency_stats.average_concurrent_tasks > 0)
}