// Azimuth Resource Lifecycle Management Tests
// 测试资源管理、生命周期控制和清理功能

test "内存资源管理" {
  // 测试内存资源管理
  let memory_manager = MemoryManager::new({
    "max_heap_size": 100 * 1024 * 1024,  // 100MB
    "gc_threshold": 0.8,  // 80%时触发GC
    "allocation_strategy": "generational"
  })
  
  // 测试内存分配和释放
  let allocated_objects = []
  
  for i in 0..=1000 {
    let large_object = {
      "id": "obj-" + i.to_string(),
      "data": "x" * 1024,  // 1KB对象
      "metadata": {
        "created_at": get_current_timestamp(),
        "type": "test_data",
        "priority": i % 10
      }
    }
    allocated_objects = allocated_objects.push(large_object)
    
    // 定期检查内存使用情况
    if i % 100 == 0 {
      let memory_stats = MemoryManager::get_stats(memory_manager)
      assert_true(memory_stats.used_heap < memory_stats.max_heap_size)
      
      // 如果内存使用超过阈值，触发垃圾回收
      if memory_stats.used_heap > memory_stats.max_heap_size * memory_manager.gc_threshold {
        MemoryManager::trigger_gc(memory_manager)
      }
    }
  }
  
  // 测试内存池管理
  let memory_pool = MemoryPool::new(1024 * 1024, 10)  // 1MB块，10个块
  
  let pooled_objects = []
  for i in 0..=15 {
    let pool allocation_result = MemoryPool::allocate(memory_pool, 100 * 1024)  // 100KB
    match allocation_result {
      Ok(block) => {
        let pooled_obj = {
          "block_id": block.id,
          "data": "pooled_data_" + i.to_string(),
          "size": 100 * 1024
        }
        pooled_objects = pooled_objects.push(pooled_obj)
      }
      Err(_) => {
        // 内存池耗尽时的处理
        assert_true(i >= 10)  // 前10个应该成功
      }
    }
  }
  
  // 释放部分内存池块
  for i in 0..=4 {
    if i < pooled_objects.length() {
      MemoryPool::deallocate(memory_pool, pooled_objects[i].block_id)
    }
  }
  
  // 验证内存池块被回收
  let new_allocation = MemoryPool::allocate(memory_pool, 100 * 1024)
  assert_true(new_allocation.is_ok())
  
  MemoryPool::cleanup(memory_pool)
}

test "连接资源管理" {
  // 测试连接资源管理
  let connection_manager = ConnectionManager::new({
    "max_connections": 20,
    "max_idle_time": 30000,  // 30秒
    "connection_timeout": 5000,  // 5秒
    "health_check_interval": 10000  // 10秒
  })
  
  // 模拟创建多个连接
  let connections = []
  for i in 0..=15 {
    let conn = ConnectionManager::create_connection(
      connection_manager,
      "endpoint-" + (i % 5).to_string() + ".example.com:8080"
    )
    connections = connections.push(conn)
  }
  
  // 验证连接池状态
  let pool_stats = ConnectionManager::get_pool_stats(connection_manager)
  assert_true(pool_stats.active_connections <= 20)
  assert_true(pool_stats.idle_connections >= 0)
  
  // 测试连接复用
  let reused_conn = ConnectionManager::get_connection(
    connection_manager,
    "endpoint-0.example.com:8080"
  )
  assert_true(reused_conn.is_some())
  
  // 测试连接健康检查
  let unhealthy_connections = ConnectionManager::check_health(connection_manager)
  for unhealthy_conn in unhealthy_connections {
    ConnectionManager::close_connection(connection_manager, unhealthy_conn.id)
  }
  
  // 测试连接超时清理
  ConnectionManager::simulate_idle_time(connection_manager, 35000)  // 35秒
  let idle_connections = ConnectionManager::cleanup_idle(connection_manager)
  assert_true(idle_connections > 0)
  
  // 关闭所有连接
  ConnectionManager::close_all(connection_manager)
  let final_stats = ConnectionManager::get_pool_stats(connection_manager)
  assert_eq(final_stats.active_connections, 0)
  assert_eq(final_stats.idle_connections, 0)
}

test "文件资源管理" {
  // 测试文件资源管理
  let file_manager = FileManager::new({
    "max_open_files": 100,
    "temp_dir": "/tmp/azimuth_test",
    "cleanup_on_exit": true
  })
  
  // 创建临时文件
  let temp_files = []
  for i in 0..=20 {
    let file_content = "test data " + i.to_string() + "\n" * 100
    let temp_file = FileManager::create_temp_file(
      file_manager,
      "test_" + i.to_string() + ".txt"
    )
    
    FileManager::write_content(file_manager, temp_file.path, file_content)
    temp_files = temp_files.push(temp_file)
  }
  
  // 验证文件创建
  assert_eq(temp_files.length(), 21)
  
  // 测试文件读取
  let first_file_content = FileManager::read_content(file_manager, temp_files[0].path)
  assert_true(first_file_content.contains("test data 0"))
  
  // 测试文件句柄管理
  let open_handles = []
  for i in 0..=15 {
    let handle = FileManager::open_file(file_manager, temp_files[i].path, "read")
    open_handles = open_handles.push(handle)
  }
  
  // 验证打开的文件句柄数量
  let handle_stats = FileManager::get_handle_stats(file_manager)
  assert_true(handle_stats.open_handles <= 100)
  
  // 关闭部分文件句柄
  for i in 0..=7 {
    FileManager::close_file(file_manager, open_handles[i])
  }
  
  // 测试文件清理
  let deleted_files = FileManager::cleanup_temp_files(file_manager)
  assert_true(deleted_files > 0)
  
  // 清理剩余文件
  FileManager::cleanup_all(file_manager)
}

test "线程和协程资源管理" {
  // 测试线程和协程资源管理
  let thread_manager = ThreadManager::new({
    "max_threads": 10,
    "thread_timeout": 30000,  // 30秒
    "stack_size": 1024 * 1024  // 1MB栈大小
  })
  
  let async_manager = AsyncManager::new({
    "max_coroutines": 100,
    "coroutine_timeout": 60000,  // 60秒
    "scheduler": "work_stealing"
  })
  
  // 创建多个线程任务
  let thread_tasks = []
  for i in 0..=8 {
    let task = ThreadManager::spawn(thread_manager, fn() {
      // 模拟工作负载
      let mut sum = 0
      for j in 0..=10000 {
        sum = sum + j
      }
      sum
    })
    thread_tasks = thread_tasks.push(task)
  }
  
  // 等待线程任务完成
  let thread_results = []
  for task in thread_tasks {
    let result = ThreadManager::wait_for_task(thread_manager, task)
    thread_results = thread_results.push(result)
  }
  
  assert_eq(thread_results.length(), 9)
  assert_true(thread_results.all(fn(r) { r > 0 }))
  
  // 创建多个协程任务
  let async_tasks = []
  for i in 0..=50 {
    let task = AsyncManager::spawn(async_manager, async fn() {
      // 模拟异步工作负载
      await async_sleep(10)  // 等待10ms
      i * 2
    })
    async_tasks = async_tasks.push(task)
  }
  
  // 等待协程任务完成
  let async_results = []
  for task in async_tasks {
    let result = AsyncManager::wait_for_task(async_manager, task)
    async_results = async_results.push(result)
  }
  
  assert_eq(async_results.length(), 51)
  assert_eq(async_results[0], 0)
  assert_eq(async_results[25], 50)
  assert_eq(async_results[50], 100)
  
  // 测试超时清理
  let timeout_tasks = []
  for i in 0..=5 {
    let task = ThreadManager::spawn(thread_manager, fn() {
      // 长时间运行的任务
      sleep(35000)  // 35秒，超过超时时间
      42
    })
    timeout_tasks = timeout_tasks.push(task)
  }
  
  // 等待超时清理
  sleep(32000)  // 32秒
  ThreadManager::cleanup_timeouts(thread_manager)
  
  let thread_stats = ThreadManager::get_stats(thread_manager)
  assert_true(thread_stats.active_threads <= thread_manager.max_threads)
  assert_true(thread_stats.timed_out_threads > 0)
  
  // 清理资源
  ThreadManager::shutdown(thread_manager)
  AsyncManager::shutdown(async_manager)
}

test "缓存资源管理" {
  // 测试缓存资源管理
  let cache_manager = CacheManager::new({
    "max_memory_usage": 50 * 1024 * 1024,  // 50MB
    "max_entries": 10000,
    "eviction_policy": "lru",
    "ttl": 3600000  // 1小时
  })
  
  // 创建多个缓存
  let caches = [
    CacheManager::create_cache(cache_manager, "user_cache", {
      "max_entries": 1000,
      "ttl": 1800000  // 30分钟
    }),
    CacheManager::create_cache(cache_manager, "session_cache", {
      "max_entries": 5000,
      "ttl": 900000  // 15分钟
    }),
    CacheManager::create_cache(cache_manager, "config_cache", {
      "max_entries": 100,
      "ttl": 7200000  // 2小时
    })
  ]
  
  // 填充缓存数据
  let user_cache = caches[0]
  let session_cache = caches[1]
  let config_cache = caches[2]
  
  for i in 0..=1200 {
    let user_data = {
      "id": "user-" + i.to_string(),
      "name": "User " + i.to_string(),
      "email": "user" + i.to_string() + "@example.com",
      "last_login": get_current_timestamp()
    }
    CacheManager::put(user_cache, "user-" + i.to_string(), user_data)
  }
  
  for i in 0..=6000 {
    let session_data = {
      "session_id": "sess-" + i.to_string(),
      "user_id": "user-" + (i % 100).to_string(),
      "created_at": get_current_timestamp(),
      "last_activity": get_current_timestamp()
    }
    CacheManager::put(session_cache, "sess-" + i.to_string(), session_data)
  }
  
  for i in 0..=150 {
    let config_data = {
      "key": "config." + i.to_string(),
      "value": "value_" + i.to_string(),
      "version": i % 10 + 1,
      "updated_at": get_current_timestamp()
    }
    CacheManager::put(config_cache, "config." + i.to_string(), config_data)
  }
  
  // 验证缓存大小限制
  let user_cache_stats = CacheManager::get_stats(user_cache)
  let session_cache_stats = CacheManager::get_stats(session_cache)
  let config_cache_stats = CacheManager::get_stats(config_cache)
  
  assert_true(user_cache_stats.entries <= 1000)
  assert_true(session_cache_stats.entries <= 5000)
  assert_true(config_cache_stats.entries <= 100)
  
  // 测试缓存过期清理
  CacheManager::simulate_time_passage(cache_manager, 2000000)  // 超过30分钟
  
  let expired_entries = CacheManager::cleanup_expired(cache_manager)
  assert_true(expired_entries > 0)
  
  // 测试内存使用限制
  let memory_usage = CacheManager::get_memory_usage(cache_manager)
  assert_true(memory_usage <= cache_manager.max_memory_usage)
  
  // 测试LRU淘汰策略
  CacheManager::fill_to_capacity(user_cache)  // 填满缓存
  let old_key = "user-1"  // 早期插入的键
  let new_key = "user-new"  // 新键
  
  CacheManager::put(user_cache, new_key, {"id": "user-new", "name": "New User"})
  
  let old_value = CacheManager::get(user_cache, old_key)
  let new_value = CacheManager::get(user_cache, new_key)
  
  assert_true(old_value.is_none())  // 旧值应该被淘汰
  assert_true(new_value.is_some())  // 新值应该存在
  
  // 清理缓存资源
  for cache in caches {
    CacheManager::clear(cache)
  }
  CacheManager::shutdown(cache_manager)
}

test "资源生命周期监控" {
  // 测试资源生命周期监控
  let resource_monitor = ResourceMonitor::new({
    "monitoring_interval": 1000,  // 1秒
    "alert_thresholds": {
      "memory_usage": 0.9,      // 90%
      "cpu_usage": 0.8,         // 80%
      "disk_usage": 0.85,       // 85%
      "connection_count": 0.9    // 90%
    },
    "retention_period": 86400000  // 24小时
  })
  
  // 启动监控
  ResourceMonitor::start(resource_monitor)
  
  // 创建资源使用场景
  let memory_consumer = MemoryConsumer::new(20 * 1024 * 1024)  // 20MB
  let cpu_consumer = CPUConsumer::new(0.7)  // 70% CPU
  let disk_consumer = DiskConsumer::new(100 * 1024 * 1024)  // 100MB磁盘
  
  // 模拟资源使用
  MemoryConsumer::start(memory_consumer)
  CPUConsumer::start(cpu_consumer)
  DiskConsumer::start(disk_consumer)
  
  // 等待监控数据收集
  sleep(5000)  // 5秒
  
  // 获取资源使用指标
  let metrics = ResourceMonitor::get_metrics(resource_monitor)
  
  assert_true(metrics.memory_usage > 0.0)
  assert_true(metrics.cpu_usage > 0.0)
  assert_true(metrics.disk_usage > 0.0)
  
  // 测试资源泄漏检测
  let leak_detector = LeakDetector::new({
    "scan_interval": 2000,  // 2秒
    "leak_threshold": {
      "memory_growth": 10 * 1024 * 1024,  // 10MB增长
      "connection_growth": 5,              // 5个连接增长
      "file_handle_growth": 10             // 10个文件句柄增长
    }
  })
  
  LeakDetector::start(leak_detector)
  
  // 模拟资源泄漏
  let leaker = ResourceLeaker::new()
  leaker.leak_memory(5 * 1024 * 1024)  // 泄漏5MB
  leaker.leak_connections(3)           // 泄漏3个连接
  leaker.leak_file_handles(5)          // 泄漏5个文件句柄
  
  // 等待泄漏检测
  sleep(3000)  // 3秒
  
  let leak_reports = LeakDetector::get_reports(leak_detector)
  assert_true(leak_reports.length() > 0)
  
  // 验证泄漏报告内容
  let memory_leak_detected = leak_reports.any(fn(report) {
    report.resource_type == "memory" && report.leak_amount > 0
  })
  let connection_leak_detected = leak_reports.any(fn(report) {
    report.resource_type == "connection" && report.leak_amount > 0
  })
  let file_handle_leak_detected = leak_reports.any(fn(report) {
    report.resource_type == "file_handle" && report.leak_amount > 0
  })
  
  assert_true(memory_leak_detected)
  assert_true(connection_leak_detected)
  assert_true(file_handle_leak_detected)
  
  // 测试资源清理建议
  let cleanup_suggestions = ResourceMonitor::generate_cleanup_suggestions(resource_monitor)
  assert_true(cleanup_suggestions.length() > 0)
  
  // 停止监控和清理资源
  MemoryConsumer::stop(memory_consumer)
  CPUConsumer::stop(cpu_consumer)
  DiskConsumer::stop(disk_consumer)
  leaker.cleanup()
  
  LeakDetector::stop(leak_detector)
  ResourceMonitor::stop(resource_monitor)
}