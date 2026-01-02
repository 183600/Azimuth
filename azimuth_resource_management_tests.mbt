// 资源管理测试
// 测试Azimuth遥测系统的资源管理功能

test "内存池管理和复用" {
  // 测试内存池的分配、复用和释放
  let memory_pool = MemoryPool::new(1024 * 1024)  // 1MB内存池
  let pool_monitor = MemoryPoolMonitor::new(memory_pool)
  
  pool_monitor.start_monitoring()
  
  // 测试内存分配
  let allocated_blocks = []
  let block_sizes = [1024, 2048, 4096, 8192, 16384]  // 1KB, 2KB, 4KB, 8KB, 16KB
  
  for round in 0..100 {
    for size in block_sizes {
      let block = memory_pool.allocate(size)
      assert_true(block.is_valid())
      assert_eq(block.get_size(), size)
      
      // 使用内存块
      block.fill_with_data(round.to_string())
      
      allocated_blocks = allocated_blocks.push(block)
    }
  }
  
  // 验证内存池状态
  let pool_stats = pool_monitor.get_statistics()
  assert_eq(pool_stats.get_allocated_blocks(), allocated_blocks.length())
  assert_true(pool_stats.get_used_memory() > 0)
  assert_true(pool_stats.get_used_memory() <= memory_pool.get_total_size())
  
  // 释放部分内存块
  let blocks_to_release = allocated_blocks.length() / 2
  for i in 0..blocks_to_release {
    let block = allocated_blocks.get(i)
    memory_pool.deallocate(block)
  }
  
  // 验证内存释放
  let after_release_stats = pool_monitor.get_statistics()
  assert_true(after_release_stats.get_allocated_blocks() < pool_stats.get_allocated_blocks())
  assert_true(after_release_stats.get_used_memory() < pool_stats.get_used_memory())
  
  // 测试内存复用
  let reused_blocks = []
  for size in block_sizes {
    let block = memory_pool.allocate(size)
    assert_true(block.is_valid())
    assert_eq(block.get_size(), size)
    
    // 验证复用的内存块是干净的
    assert_true(block.is_zeroed() || block.has_valid_signature())
    
    reused_blocks = reused_blocks.push(block)
  }
  
  // 验证复用统计
  let reuse_stats = pool_monitor.get_statistics()
  assert_true(reuse_stats.get_reuse_count() > 0)
  
  // 释放所有内存块
  for block in allocated_blocks {
    memory_pool.deallocate(block)
  }
  
  for block in reused_blocks {
    memory_pool.deallocate(block)
  }
  
  // 验证完全释放
  let final_stats = pool_monitor.get_statistics()
  assert_eq(final_stats.get_allocated_blocks(), 0)
  assert_eq(final_stats.get_used_memory(), 0)
  
  pool_monitor.stop_monitoring()
}

test "连接池管理和生命周期" {
  // 测试遥测数据导出器的连接池管理
  let connection_pool = ConnectionPool::new(
    "https://otlp.example.com:4317",
    10,  // 最大连接数
    30,  // 连接超时秒
    300  // 连接最大空闲时间秒
  )
  
  let pool_monitor = ConnectionPoolMonitor::new(connection_pool)
  
  // 测试连接获取
  let connections = []
  
  for i in 0..10 {
    let connection = connection_pool.get_connection()
    assert_true(connection.is_valid())
    assert_true(connection.is_active())
    
    connections = connections.push(connection)
  }
  
  // 验证连接池状态
  let pool_stats = pool_monitor.get_statistics()
  assert_eq(pool_stats.get_active_connections(), 10)
  assert_eq(pool_stats.get_idle_connections(), 0)
  assert_eq(pool_stats.get_total_connections(), 10)
  
  // 测试连接池耗尽情况
  let overflow_connection = connection_pool.get_connection()
  assert_true(overflow_connection.is_timeout() || overflow_connection.is_error())
  
  // 释放部分连接
  for i in 0..5 {
    let connection = connections.get(i)
    connection_pool.return_connection(connection)
  }
  
  // 验证连接释放后的状态
  let after_release_stats = pool_monitor.get_statistics()
  assert_eq(after_release_stats.get_active_connections(), 5)
  assert_eq(after_release_stats.get_idle_connections(), 5)
  
  // 测试连接复用
  let reused_connections = []
  for i in 0..5 {
    let connection = connection_pool.get_connection()
    assert_true(connection.is_valid())
    
    // 验证复用的连接仍然有效
    assert_true(connection.is_active())
    assert_true(connection.last_used_time() > 0)
    
    reused_connections = reused_connections.push(connection)
  }
  
  // 验证复用统计
  let reuse_stats = pool_monitor.get_statistics()
  assert_true(reuse_stats.get_reuse_count() > 0)
  
  // 释放所有连接
  for connection in connections {
    connection_pool.return_connection(connection)
  }
  
  for connection in reused_connections {
    connection_pool.return_connection(connection)
  }
  
  // 测试连接清理
  let cleanup_result = connection_pool.cleanup_idle_connections()
  assert_true(cleanup_result.get_cleaned_connections() > 0)
  
  // 验证清理后的状态
  let final_stats = pool_monitor.get_statistics()
  assert_eq(final_stats.get_active_connections(), 0)
  assert_eq(final_stats.get_idle_connections(), 0)
}

test "文件句柄管理" {
  // 测试遥测数据文件的句柄管理
  let file_manager = FileManager::new("/tmp/telemetry_test")
  let handle_monitor = FileHandleMonitor::new(file_manager)
  
  // 创建多个日志文件
  let log_files = []
  let file_count = 50
  
  for i in 0..file_count {
    let file_name = "telemetry_log_" + i.to_string() + ".log"
    let file_handle = file_manager.open_file(file_name, "write")
    
    assert_true(file_handle.is_valid())
    assert_true(file_handle.is_open())
    
    // 写入一些数据
    let log_entry = "Log entry " + i.to_string() + " at " + Time::now().to_string() + "\n"
    file_handle.write(log_entry)
    
    log_files = log_files.push((file_name, file_handle))
  }
  
  // 验证文件句柄状态
  let handle_stats = handle_monitor.get_statistics()
  assert_eq(handle_stats.get_open_handles(), file_count)
  assert_true(handle_stats.get_total_disk_usage() > 0)
  
  // 测试文件句柄限制
  let max_handles = file_manager.get_max_open_handles()
  let additional_files = []
  
  // 尝试打开超过限制的文件
  for i in 0..(max_handles + 10) {
    let file_name = "additional_log_" + i.to_string() + ".log"
    let file_handle = file_manager.open_file(file_name, "write")
    
    if file_handle.is_valid() {
      additional_files = additional_files.push((file_name, file_handle))
    }
  }
  
  // 验证句柄限制生效
  let limit_stats = handle_monitor.get_statistics()
  assert_true(limit_stats.get_open_handles() <= max_handles)
  
  // 关闭部分文件
  let files_to_close = log_files.length() / 2
  for i in 0..files_to_close {
    let (file_name, file_handle) = log_files.get(i)
    file_manager.close_file(file_handle)
  }
  
  // 验证文件关闭后的状态
  let after_close_stats = handle_monitor.get_statistics()
  assert_true(after_close_stats.get_open_handles() < limit_stats.get_open_handles())
  
  // 测试文件句柄复用
  let reused_files = []
  for i in 0..5 {
    let file_name = "reused_log_" + i.to_string() + ".log"
    let file_handle = file_manager.open_file(file_name, "write")
    
    if file_handle.is_valid() {
      reused_files = reused_files.push((file_name, file_handle))
    }
  }
  
  // 验证复用统计
  let reuse_stats = handle_monitor.get_statistics()
  assert_true(reuse_stats.get_handle_reuse_count() > 0)
  
  // 清理所有文件
  for (file_name, file_handle) in log_files {
    file_manager.close_file(file_handle)
    file_manager.delete_file(file_name)
  }
  
  for (file_name, file_handle) in additional_files {
    file_manager.close_file(file_handle)
    file_manager.delete_file(file_name)
  }
  
  for (file_name, file_handle) in reused_files {
    file_manager.close_file(file_handle)
    file_manager.delete_file(file_name)
  }
  
  // 验证完全清理
  let final_stats = handle_monitor.get_statistics()
  assert_eq(final_stats.get_open_handles(), 0)
  assert_eq(final_stats.get_total_disk_usage(), 0)
}

test "缓存资源管理" {
  // 测试遥测数据缓存的管理
  let cache_manager = CacheManager::new(
    100 * 1024 * 1024,  // 100MB缓存大小
    3600,               // 1小时缓存过期时间
    LRU                 // LRU淘汰策略
  )
  
  let cache_monitor = CacheMonitor::new(cache_manager)
  
  // 添加大量缓存项
  let cache_keys = []
  let cache_size = 10000
  
  for i in 0..cache_size {
    let key = "telemetry_data_" + i.to_string()
    let value = generate_large_telemetry_data(i)  // 生成大约10KB的数据
    
    let cache_result = cache_manager.put(key, value)
    assert_true(cache_result.is_success())
    
    cache_keys = cache_keys.push(key)
  }
  
  // 验证缓存状态
  let cache_stats = cache_monitor.get_statistics()
  assert_true(cache_stats.get_item_count() > 0)
  assert_true(cache_stats.get_memory_usage() > 0)
  assert_true(cache_stats.get_memory_usage() <= 100 * 1024 * 1024)
  
  // 测试缓存命中
  let hit_count = 0
  let miss_count = 0
  
  for i in 0..cache_size {
    let key = cache_keys.get(i)
    let cached_value = cache_manager.get(key)
    
    if cached_value.is_some() {
      hit_count = hit_count + 1
    } else {
      miss_count = miss_count + 1
    }
  }
  
  // 验证缓存命中率
  let hit_rate = hit_count.to_float() / (hit_count + miss_count).to_float()
  assert_true(hit_rate > 0.8)  // 命中率应该高于80%
  
  // 测试缓存淘汰
  let additional_items = 5000
  let eviction_count_before = cache_stats.get_eviction_count()
  
  for i in 0..additional_items {
    let key = "additional_data_" + i.to_string()
    let value = generate_large_telemetry_data(i + cache_size)
    
    cache_manager.put(key, value)
  }
  
  // 验证淘汰发生
  let eviction_stats = cache_monitor.get_statistics()
  assert_true(eviction_stats.get_eviction_count() > eviction_count_before)
  
  // 测试缓存清理
  let cleanup_result = cache_manager.cleanup_expired()
  assert_true(cleanup_result.get_cleaned_items() >= 0)
  
  // 测试缓存清空
  cache_manager.clear()
  
  // 验证清空后的状态
  let final_stats = cache_monitor.get_statistics()
  assert_eq(final_stats.get_item_count(), 0)
  assert_eq(final_stats.get_memory_usage(), 0)
}

test "线程资源管理" {
  // 测试遥测系统中的线程资源管理
  let thread_pool = ThreadPool::new(
    20,   // 核心线程数
    50,   // 最大线程数
    60    // 线程空闲时间秒
  )
  
  let thread_monitor = ThreadPoolMonitor::new(thread_pool)
  
  // 提交大量任务
  let task_count = 1000
  let completed_tasks = AtomicCounter::new(0)
  
  for i in 0..task_count {
    let task = TelemetryTask::new("task_" + i.to_string(), fn() {
      // 模拟遥测处理工作
      Thread::sleep(Duration::milliseconds(10 + (Math::random() * 40).to_int()))
      completed_tasks.increment()
    })
    
    thread_pool.submit(task)
  }
  
  // 等待所有任务完成
  while completed_tasks.get() < task_count {
    Thread::sleep(Duration::milliseconds(100))
  }
  
  // 验证线程池状态
  let pool_stats = thread_monitor.get_statistics()
  assert_true(pool_stats.get_completed_tasks() >= task_count)
  assert_true(pool_stats.get_active_threads() <= pool_stats.get_max_threads())
  
  // 测试线程池扩展
  let burst_tasks = 100
  let burst_completed = AtomicCounter::new(0)
  
  for i in 0..burst_tasks {
    let task = TelemetryTask::new("burst_task_" + i.to_string(), fn() {
      Thread::sleep(Duration::milliseconds(50))  // 较长的任务
      burst_completed.increment()
    })
    
    thread_pool.submit(task)
  }
  
  // 验证线程池扩展
  let during_burst_stats = thread_monitor.get_statistics()
  assert_true(during_burst_stats.get_active_threads() > pool_stats.get_core_threads())
  
  // 等待突发任务完成
  while burst_completed.get() < burst_tasks {
    Thread::sleep(Duration::milliseconds(100))
  }
  
  // 测试线程池收缩
  Thread::sleep(Duration::seconds(65))  // 超过空闲时间
  
  let after_idle_stats = thread_monitor.get_statistics()
  assert_true(after_idle_stats.get_active_threads() <= after_idle_stats.get_core_threads())
  
  // 测试线程池关闭
  thread_pool.shutdown()
  
  // 验证关闭后的状态
  let shutdown_stats = thread_monitor.get_statistics()
  assert_true(shutdown_stats.is_shutdown())
  assert_eq(shutdown_stats.get_active_threads(), 0)
}

test "资源泄漏检测和预防" {
  // 测试资源泄漏检测和预防机制
  let leak_detector = ResourceLeakDetector::new()
  leak_detector.start_monitoring()
  
  // 测试内存泄漏检测
  let memory_leak_test = fn() {
    let leaked_objects = []
    
    // 故意创建内存泄漏
    for i in 0..1000 {
      let large_object = String::repeat("leak_test_data_", 1000)
      leaked_objects = leaked_objects.push(large_object)
      
      if i % 100 == 0 {
        // 模拟部分清理
        leaked_objects = leaked_objects.slice(0, leaked_objects.length() / 2)
      }
    }
    
    // 不完全清理，留下一些泄漏
    leaked_objects = leaked_objects.slice(0, 100)
    
    return leaked_objects.length()
  }
  
  let leaked_count = memory_leak_test()
  assert_true(leaked_count > 0)
  
  // 检测内存泄漏
  let memory_leaks = leak_detector.detect_memory_leaks()
  assert_true(memory_leaks.get_leaked_objects() > 0)
  assert_true(memory_leaks.get_total_leaked_bytes() > 0)
  
  // 测试连接泄漏检测
  let connection_leak_test = fn() {
    let connection_pool = ConnectionPool::new("localhost:8080", 10, 10, 60)
    let leaked_connections = []
    
    // 故意创建连接泄漏
    for i in 0..50 {
      let connection = connection_pool.get_connection()
      
      if i % 5 != 0 {  // 每5个中有4个不归还，造成泄漏
        leaked_connections = leaked_connections.push(connection)
      } else {
        connection_pool.return_connection(connection)
      }
    }
    
    return leaked_connections.length()
  }
  
  let leaked_connections = connection_leak_test()
  assert_true(leaked_connections > 0)
  
  // 检测连接泄漏
  let connection_leaks = leak_detector.detect_connection_leaks()
  assert_true(connection_leaks.get_leaked_connections() > 0)
  
  // 测试文件句柄泄漏检测
  let file_leak_test = fn() {
    let file_manager = FileManager::new("/tmp/leak_test")
    let leaked_handles = []
    
    // 故意创建文件句柄泄漏
    for i in 0..20 {
      let file_handle = file_manager.open_file("leak_test_" + i.to_string() + ".tmp", "write")
      
      if i % 3 != 0 {  // 每3个中有2个不关闭，造成泄漏
        leaked_handles = leaked_handles.push(file_handle)
      } else {
        file_manager.close_file(file_handle)
        file_manager.delete_file("leak_test_" + i.to_string() + ".tmp")
      }
    }
    
    return leaked_handles.length()
  }
  
  let leaked_handles = file_leak_test()
  assert_true(leaked_handles > 0)
  
  // 检测文件句柄泄漏
  let file_leaks = leak_detector.detect_file_handle_leaks()
  assert_true(file_leaks.get_leaked_handles() > 0)
  
  // 测试自动泄漏修复
  let auto_fix_result = leak_detector.auto_fix_leaks()
  assert_true(auto_fix_result.get_fixed_memory_leaks() > 0)
  assert_true(auto_fix_result.get_fixed_connection_leaks() > 0)
  assert_true(auto_fix_result.get_fixed_file_leaks() > 0)
  
  // 验证修复效果
  let after_fix_memory = leak_detector.detect_memory_leaks()
  let after_fix_connections = leak_detector.detect_connection_leaks()
  let after_fix_files = leak_detector.detect_file_handle_leaks()
  
  assert_true(after_fix_memory.get_leaked_objects() < memory_leaks.get_leaked_objects())
  assert_true(after_fix_connections.get_leaked_connections() < connection_leaks.get_leaked_connections())
  assert_true(after_fix_files.get_leaked_handles() < file_leaks.get_leaked_handles())
  
  leak_detector.stop_monitoring()
  
  // 生成泄漏报告
  let leak_report = leak_detector.generate_leak_report()
  assert_true(leak_report.has_memory_leaks())
  assert_true(leak_report.has_connection_leaks())
  assert_true(leak_report.has_file_handle_leaks())
  assert_true(leak_report.get_auto_fix_success_rate() > 0.5)
}

test "资源配额和限制管理" {
  // 测试资源配额和限制管理
  let quota_manager = ResourceQuotaManager::new()
  
  // 设置各种资源配额
  quota_manager.set_memory_quota(500 * 1024 * 1024)      // 500MB内存配额
  quota_manager.set_cpu_quota(80.0)                       // 80% CPU配额
  quota_manager.set_connection_quota(50)                  // 50个连接配额
  quota_manager.set_file_handle_quota(200)                // 200个文件句柄配额
  quota_manager.set_thread_quota(100)                     // 100个线程配额
  
  // 测试内存配额
  let memory_user = ResourceUser::new("memory_test")
  let memory_allocation = 100 * 1024 * 1024  // 100MB
  
  for i in 0..6 {  // 尝试分配6次100MB，超过500MB配额
    let allocation_result = quota_manager.request_memory(memory_user, memory_allocation)
    
    if i < 5 {  // 前5次应该成功
      assert_true(allocation_result.is_granted())
    } else {    // 第6次应该失败
      assert_true(allocation_result.is_denied())
      assert_true(allocation_result.get_reason().contains("quota_exceeded"))
    }
  }
  
  // 测试CPU配额
  let cpu_user = ResourceUser::new("cpu_test")
  let cpu_usage = 25.0  // 25% CPU
  
  for i in 0..4 {  // 尝试使用4次25% CPU
    let cpu_request = quota_manager.request_cpu(cpu_user, cpu_usage)
    
    if i < 3 {  // 前3次应该成功
      assert_true(cpu_request.is_granted())
    } else {    // 第4次应该失败
      assert_true(cpu_request.is_denied())
      assert_true(cpu_request.get_reason().contains("quota_exceeded"))
    }
  }
  
  // 测试连接配额
  let connection_user = ResourceUser::new("connection_test")
  let connection_requests = []
  
  for i in 0..60 {  // 尝试创建60个连接，超过50个配额
    let connection_request = quota_manager.request_connection(connection_user)
    
    if i < 50 {  // 前50个应该成功
      assert_true(connection_request.is_granted())
      connection_requests = connection_requests.push(connection_request)
    } else {    // 后10个应该失败
      assert_true(connection_request.is_denied())
    }
  }
  
  // 测试资源释放
  for i in 0..10 {  // 释放10个连接
    let connection = connection_requests.get(i)
    quota_manager.release_connection(connection_user, connection)
  }
  
  // 释放后应该能够创建新连接
  let new_connection_request = quota_manager.request_connection(connection_user)
  assert_true(new_connection_request.is_granted())
  
  // 测试配额监控
  let quota_monitor = ResourceQuotaMonitor::new(quota_manager)
  let quota_report = quota_monitor.generate_report()
  
  assert_true(quota_report.get_memory_usage_percent() > 0.0)
  assert_true(quota_report.get_cpu_usage_percent() > 0.0)
  assert_true(quota_report.get_connection_usage_count() > 0)
  
  // 测试动态配额调整
  let adjustment_result = quota_manager.adjust_memory_quota(600 * 1024 * 1024)  // 增加到600MB
  assert_true(adjustment_result.is_successful())
  
  // 验证调整后的配额
  let adjusted_quota = quota_manager.get_memory_quota()
  assert_eq(adjusted_quota, 600 * 1024 * 1024)
  
  // 测试配额超限处理策略
  quota_manager.set_exceed_strategy("deny_new")  // 拒绝新请求
  quota_manager.set_exceed_strategy("priority_based")  // 基于优先级
  
  let high_priority_user = ResourceUser::new("high_priority").with_priority(10)
  let low_priority_user = ResourceUser::new("low_priority").with_priority(1)
  
  // 在配额紧张时，高优先级用户应该优先获得资源
  let high_priority_result = quota_manager.request_memory(high_priority_user, 50 * 1024 * 1024)
  let low_priority_result = quota_manager.request_memory(low_priority_user, 50 * 1024 * 1024)
  
  assert_true(high_priority_result.is_granted())
  // 低优先级用户的请求可能被拒绝，取决于当前资源使用情况
}