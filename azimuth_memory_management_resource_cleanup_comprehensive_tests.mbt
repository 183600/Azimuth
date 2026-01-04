// Azimuth 内存管理资源清理综合测试用例
// 专注于遥测系统的内存管理、垃圾回收、内存泄漏检测和资源清理

// 测试1: 内存分配和释放基础
test "内存分配和释放基础测试" {
  // 创建内存监控器
  let memory_monitor = MemoryMonitor::new()
  
  // 记录初始内存状态
  let initial_memory = memory_monitor.get_current_usage()
  assert_true(initial_memory.total_allocated > 0)
  assert_eq(initial_memory.total_freed, 0)
  
  // 测试基本对象创建和销毁
  let object_creation_test = fn() {
    let spans = []
    
    // 创建大量span对象
    for i in 0..=10000 {
      let span = Span::new("memory.test", Server, 
                          TraceContext::new("memory-trace", "memory-span-" + i.to_string(), true, ""))
      Span::set_attribute(span, "test.id", IntValue(i))
      Span::set_attribute(span, "test.data", StringValue("test data " + i.to_string()))
      spans = spans.push(span)
    }
    
    let creation_memory = memory_monitor.get_current_usage()
    
    // 销毁一半对象
    let remaining_spans = spans.slice(0, spans.length() / 2)
    spans = []  // 释放引用
    
    let destruction_memory = memory_monitor.get_current_usage()
    
    // 验证内存释放
    assert_true(destruction_memory.total_allocated >= creation_memory.total_allocated)
    assert_true(destruction_memory.total_freed > creation_memory.total_freed)
    
    remaining_spans
  }
  
  let remaining_spans = object_creation_test()
  
  // 完全清理剩余对象
  remaining_spans = []
  
  // 强制垃圾回收
  memory_monitor.force_gc()
  
  let final_memory = memory_monitor.get_current_usage()
  
  // 验证垃圾回收效果
  assert_true(final_memory.total_freed > initial_memory.total_freed)
  
  // 计算内存回收率
  let memory_recovered = final_memory.total_freed - initial_memory.total_freed
  let memory_allocated = final_memory.total_allocated - initial_memory.total_allocated
  let recovery_rate = memory_recovered.to_float() / memory_allocated.to_float()
  
  assert_true(recovery_rate > 0.8)  // 至少80%的内存应该被回收
}

// 测试2: 内存泄漏检测
test "内存泄漏检测测试" {
  // 创建内存泄漏检测器
  let leak_detector = MemoryLeakDetector::new()
  
  // 配置泄漏检测阈值
  leak_detector.set_leak_threshold(1024 * 1024)  // 1MB
  leak_detector.set_detection_interval(1000)     // 1秒检测间隔
  leak_detector.enable_continuous_monitoring(true)
  
  // 开始监控
  leak_detector.start_monitoring()
  
  // 创建可能导致内存泄漏的操作
  let potentially_leaky_operations = [
    fn() {
      // 循环引用测试
      let parent = RefObject::new("parent")
      let child = RefObject::new("child")
      parent.set_reference("child", child)
      child.set_reference("parent", parent)  // 创建循环引用
    },
    fn() {
      // 大对象未释放测试
      let large_objects = []
      for i in 0..=100 {
        let large_data = "x".repeat(10000)  // 10KB字符串
        large_objects = large_objects.push(large_data)
      }
      // 故意不释放large_objects
    },
    fn() {
      // 事件监听器未移除测试
      let event_emitter = EventEmitter::new()
      for i in 0..=1000 {
        let listener = fn(event) { /* 处理事件 */ }
        event_emitter.add_listener("test_event", listener)
        // 故意不移除监听器
      }
    }
  ]
  
  // 执行潜在泄漏操作
  for operation in potentially_leaky_operations {
    let pre_operation_memory = leak_detector.get_memory_snapshot()
    
    operation()
    
    let post_operation_memory = leak_detector.get_memory_snapshot()
    let memory_growth = post_operation_memory.heap_size - pre_operation_memory.heap_size
    
    // 检测内存增长
    if memory_growth > leak_detector.get_leak_threshold() {
      let leak_report = leak_detector.generate_leak_report(pre_operation_memory, post_operation_memory)
      assert_true(leak_report.potential_leak_detected)
      assert_true(leak_report.memory_growth > 0)
    }
  }
  
  // 强制垃圾回收
  leak_detector.force_gc()
  
  // 等待检测间隔
  Time::sleep(1100)
  
  // 获取最终泄漏报告
  let final_leak_report = leak_detector.get_leak_report()
  
  // 验证泄漏检测
  assert_true(final_leak_report.monitoring_duration > 0)
  
  // 如果检测到泄漏，验证报告详细信息
  if final_leak_report.potential_leaks_detected {
    assert_true(final_leak_report.leak_locations.length() > 0)
    
    for leak_location in final_leak_report.leak_locations {
      assert_true(leak_location.object_type.length() > 0)
      assert_true(leak_location.estimated_size > 0)
    }
  }
  
  // 停止监控
  leak_detector.stop_monitoring()
}

// 测试3: 内存池管理
test "内存池管理测试" {
  // 创建内存池管理器
  let pool_manager = MemoryPoolManager::new()
  
  // 配置不同类型的内存池
  pool_manager.create_pool("small_objects", {
    object_size: 1024,      // 1KB
    initial_count: 100,
    max_count: 1000,
    growth_factor: 1.5
  })
  
  pool_manager.create_pool("medium_objects", {
    object_size: 10240,     // 10KB
    initial_count: 50,
    max_count: 500,
    growth_factor: 1.5
  })
  
  pool_manager.create_pool("large_objects", {
    object_size: 102400,    // 100KB
    initial_count: 10,
    max_count: 100,
    growth_factor: 2.0
  })
  
  // 测试内存池分配
  let small_allocations = []
  let medium_allocations = []
  let large_allocations = []
  
  // 分配小对象
  for i in 0..=200 {
    let allocation = pool_manager.allocate("small_objects")
    assert_true(allocation.is_some)
    small_allocations = small_allocations.push(allocation)
  }
  
  // 分配中对象
  for i in 0..=75 {
    let allocation = pool_manager.allocate("medium_objects")
    assert_true(allocation.is_some)
    medium_allocations = medium_allocations.push(allocation)
  }
  
  // 分配大对象
  for i in 0..=15 {
    let allocation = pool_manager.allocate("large_objects")
    assert_true(allocation.is_some)
    large_allocations = large_allocations.push(allocation)
  }
  
  // 验证池状态
  let small_pool_stats = pool_manager.get_pool_stats("small_objects")
  let medium_pool_stats = pool_manager.get_pool_stats("medium_objects")
  let large_pool_stats = pool_manager.get_pool_stats("large_objects")
  
  assert_eq(small_pool_stats.allocated_count, 200)
  assert_eq(medium_pool_stats.allocated_count, 75)
  assert_eq(large_pool_stats.allocated_count, 15)
  
  assert_true(small_pool_stats.pool_size >= small_pool_stats.allocated_count)
  assert_true(medium_pool_stats.pool_size >= medium_pool_stats.allocated_count)
  assert_true(large_pool_stats.pool_size >= large_pool_stats.allocated_count)
  
  // 测试内存池释放
  // 释放一半的小对象
  for i in 0..=small_allocations.length() / 2 - 1 {
    pool_manager.deallocate("small_objects", small_allocations[i])
  }
  
  // 释放一半的中对象
  for i in 0..=medium_allocations.length() / 2 - 1 {
    pool_manager.deallocate("medium_objects", medium_allocations[i])
  }
  
  // 释放一半的大对象
  for i in 0..=large_allocations.length() / 2 - 1 {
    pool_manager.deallocate("large_objects", large_allocations[i])
  }
  
  // 验证释放后的池状态
  let small_pool_stats_after = pool_manager.get_pool_stats("small_objects")
  let medium_pool_stats_after = pool_manager.get_pool_stats("medium_objects")
  let large_pool_stats_after = pool_manager.get_pool_stats("large_objects")
  
  assert_eq(small_pool_stats_after.allocated_count, 100)
  assert_eq(medium_pool_stats_after.allocated_count, 37)
  assert_eq(large_pool_stats_after.allocated_count, 7)
  
  // 验证可用对象增加
  assert_true(small_pool_stats_after.available_count > small_pool_stats.available_count)
  assert_true(medium_pool_stats_after.available_count > medium_pool_stats.available_count)
  assert_true(large_pool_stats_after.available_count > large_pool_stats.available_count)
  
  // 测试内存池性能
  let pool_performance_test = fn(pool_name : String, allocation_count : Int) -> (Int, Int) {
    let start_time = Time::now()
    
    let allocations = []
    for i in 0..=allocation_count {
      let allocation = pool_manager.allocate(pool_name)
      allocations = allocations.push(allocation)
    }
    
    let allocation_time = Time::now()
    
    for allocation in allocations {
      pool_manager.deallocate(pool_name, allocation)
    }
    
    let deallocation_time = Time::now()
    
    (allocation_time - start_time, deallocation_time - allocation_time)
  }
  
  // 比较池分配与直接分配的性能
  let pool_allocation_time = pool_performance_test("small_objects", 1000)
  
  let direct_allocation_start = Time::now()
  let direct_allocations = []
  for i in 0..=1000 {
    let direct_allocation = DirectMemory::allocate(1024)
    direct_allocations = direct_allocations.push(direct_allocation)
  }
  let direct_allocation_time = Time::now() - direct_allocation_start
  
  let direct_deallocation_start = Time::now()
  for allocation in direct_allocations {
    DirectMemory::deallocate(allocation)
  }
  let direct_deallocation_time = Time::now() - direct_deallocation_start
  
  // 验证内存池性能优势
  assert_true(pool_allocation_time.0 < direct_allocation_time)  // 池分配应该更快
  assert_true(pool_allocation_time.1 < direct_deallocation_time)  // 池释放应该更快
  
  // 清理剩余分配
  for allocation in small_allocations.slice(small_allocations.length() / 2) {
    pool_manager.deallocate("small_objects", allocation)
  }
  
  for allocation in medium_allocations.slice(medium_allocations.length() / 2) {
    pool_manager.deallocate("medium_objects", allocation)
  }
  
  for allocation in large_allocations.slice(large_allocations.length() / 2) {
    pool_manager.deallocate("large_objects", allocation)
  }
}

// 测试4: 智能指针和引用计数
test "智能指针和引用计数测试" {
  // 创建智能指针管理器
  let smart_ptr_manager = SmartPointerManager::new()
  
  // 测试共享指针（SharedPtr）
  let shared_ptr_test = fn() {
    let data = "test data for shared pointer"
    let shared_ptr = smart_ptr_manager.create_shared_ptr(data)
    
    // 验证初始引用计数
    assert_eq(smart_ptr_manager.get_ref_count(shared_ptr), 1)
    
    // 创建额外的引用
    let shared_ref1 = smart_ptr_manager.add_shared_ref(shared_ptr)
    let shared_ref2 = smart_ptr_manager.add_shared_ref(shared_ptr)
    
    // 验证引用计数增加
    assert_eq(smart_ptr_manager.get_ref_count(shared_ptr), 3)
    
    // 释放引用
    smart_ptr_manager.release_shared_ref(shared_ref1)
    assert_eq(smart_ptr_manager.get_ref_count(shared_ptr), 2)
    
    smart_ptr_manager.release_shared_ref(shared_ref2)
    assert_eq(smart_ptr_manager.get_ref_count(shared_ptr), 1)
    
    // 验证数据访问
    assert_eq(smart_ptr_manager.deref_shared(shared_ptr), data)
    
    shared_ptr
  }
  
  let main_shared_ptr = shared_ptr_test()
  
  // 测试弱指针（WeakPtr）
  let weak_ptr_test = fn(shared_ptr) {
    let weak_ptr = smart_ptr_manager.create_weak_ptr(shared_ptr)
    
    // 验证弱指针有效性
    assert_true(smart_ptr_manager.is_weak_valid(weak_ptr))
    
    // 从弱指针创建共享指针
    let shared_from_weak = smart_ptr_manager.lock_weak(weak_ptr)
    assert_true(shared_from_weak.is_some)
    
    // 验证引用计数
    assert_eq(smart_ptr_manager.get_ref_count(shared_ptr), 2)
    
    // 释放从弱指针创建的共享指针
    smart_ptr_manager.release_shared_ref(shared_from_weak.unwrap())
    assert_eq(smart_ptr_manager.get_ref_count(shared_ptr), 1)
    
    weak_ptr
  }
  
  let weak_ptr = weak_ptr_test(main_shared_ptr)
  
  // 测试唯一指针（UniquePtr）
  let unique_ptr_test = fn() {
    let unique_data = "unique data"
    let unique_ptr = smart_ptr_manager.create_unique_ptr(unique_data)
    
    // 验证唯一指针数据访问
    assert_eq(smart_ptr_manager.deref_unique(unique_ptr), unique_data)
    
    // 移动唯一指针
    let moved_ptr = smart_ptr_manager.move_unique(unique_ptr)
    
    // 验证原指针不再有效
    assert_false(smart_ptr_manager.is_unique_valid(unique_ptr))
    
    // 验证移动后的指针有效
    assert_true(smart_ptr_manager.is_unique_valid(moved_ptr))
    assert_eq(smart_ptr_manager.deref_unique(moved_ptr), unique_data)
    
    moved_ptr
  }
  
  let unique_ptr = unique_ptr_test()
  
  // 测试引用计数循环检测
  let cycle_detection_test = fn() {
    let object_a = smart_ptr_manager.create_shared_ptr("object_a")
    let object_b = smart_ptr_manager.create_shared_ptr("object_b")
    
    // 创建循环引用
    smart_ptr_manager.set_shared_reference(object_a, "next", object_b)
    smart_ptr_manager.set_shared_reference(object_b, "prev", object_a)
    
    // 检测循环引用
    let cycles = smart_ptr_manager.detect_reference_cycles()
    assert_true(cycles.length() > 0)
    
    // 验证循环包含的对象
    let cycle_contains_a = cycles.any(fn(cycle) { 
      cycle.objects.any(fn(obj) { smart_ptr_manager.get_ptr_id(obj) == smart_ptr_manager.get_ptr_id(object_a) })
    })
    assert_true(cycle_contains_a)
    
    // 使用弱指针打破循环
    let weak_b = smart_ptr_manager.create_weak_ptr(object_b)
    smart_ptr_manager.set_weak_reference(object_a, "next_weak", weak_b)
    
    // 清理强引用
    smart_ptr_manager.clear_shared_reference(object_a, "next")
    smart_ptr_manager.clear_shared_reference(object_b, "prev")
    
    cycles
  }
  
  let detected_cycles = cycle_detection_test()
  
  // 验证循环检测报告
  assert_true(detected_cycles.length() > 0)
  for cycle in detected_cycles {
    assert_true(cycle.objects.length() >= 2)
    assert_true(cycle.cycle_length > 0)
  }
  
  // 清理所有指针
  smart_ptr_manager.release_shared_ref(main_shared_ptr)
  smart_ptr_manager.release_unique_ptr(unique_ptr)
  
  // 弱指针应该自动失效
  assert_false(smart_ptr_manager.is_weak_valid(weak_ptr))
}

// 测试5: 缓存内存管理
test "缓存内存管理测试" {
  // 创建缓存管理器
  let cache_manager = CacheManager::new()
  
  // 配置缓存策略
  cache_manager.configure_cache("span_cache", {
    max_size: 1000,           // 最多1000个条目
    max_memory: 10 * 1024 * 1024,  // 最多10MB
    eviction_policy: "lru",   // LRU淘汰策略
    ttl: 60000                // 60秒TTL
  })
  
  cache_manager.configure_cache("metric_cache", {
    max_size: 5000,
    max_memory: 50 * 1024 * 1024,  // 50MB
    eviction_policy: "lfu",   // LFU淘汰策略
    ttl: 30000                // 30秒TTL
  })
  
  // 测试缓存基本操作
  let cache_operations_test = fn() {
    // 添加缓存条目
    for i in 0..=1200 {
      let span_key = "span_" + i.to_string()
      let span_data = Span::new("cached.span", Server, 
                               TraceContext::new("cache-trace", "cache-span-" + i.to_string(), true, ""))
      Span::set_attribute(span_data, "cache.key", StringValue(span_key))
      Span::set_attribute(span_data, "cache.value", IntValue(i))
      
      cache_manager.put("span_cache", span_key, span_data)
    }
    
    // 验证缓存大小限制
    let span_cache_stats = cache_manager.get_stats("span_cache")
    assert_true(span_cache_stats.entry_count <= 1000)
    assert_true(span_cache_stats.memory_usage <= 10 * 1024 * 1024)
    
    // 验证缓存命中
    let hit_result = cache_manager.get("span_cache", "span_500")
    assert_true(hit_result.is_some)
    
    match hit_result {
      Some(cached_span) => {
        let cache_key = Span::get_attribute(cached_span, "cache.key")
        let cache_value = Span::get_attribute(cached_span, "cache.value")
        assert_eq(cache_key, Some(StringValue("span_500")))
        assert_eq(cache_value, Some(IntValue(500)))
      }
      None => assert_true(false)
    }
    
    // 验证缓存未命中
    let miss_result = cache_manager.get("span_cache", "nonexistent_key")
    assert_true(miss_result.is_none)
    
    // 测试缓存统计
    let final_stats = cache_manager.get_stats("span_cache")
    assert_true(final_stats.hits > 0)
    assert_true(final_stats.misses > 0)
    assert_true(final_stats.evictions > 0)  // 应该有淘汰
  }
  
  cache_operations_test()
  
  // 测试缓存内存压力
  let memory_pressure_test = fn() {
    let large_data = "x".repeat(10240)  // 10KB数据
    
    // 填充缓存直到内存限制
    let mut i = 0
    while cache_manager.get_stats("span_cache").memory_usage < 9 * 1024 * 1024 {
      let key = "large_data_" + i.to_string()
      cache_manager.put("span_cache", key, large_data)
      i = i + 1
    }
    
    let memory_pressure_stats = cache_manager.get_stats("span_cache")
    assert_true(memory_pressure_stats.memory_usage >= 9 * 1024 * 1024)
    
    // 添加更多数据触发淘汰
    for j in i..=i + 100 {
      let key = "pressure_data_" + j.to_string()
      cache_manager.put("span_cache", key, large_data)
    }
    
    // 验证内存使用仍在限制内
    let after_eviction_stats = cache_manager.get_stats("span_cache")
    assert_true(after_eviction_stats.memory_usage <= 10 * 1024 * 1024)
    assert_true(after_eviction_stats.evictions > memory_pressure_stats.evictions)
  }
  
  memory_pressure_test()
  
  // 测试缓存TTL过期
  let ttl_test = fn() {
    // 配置短TTL缓存用于测试
    cache_manager.configure_cache("ttl_test_cache", {
      max_size: 100,
      max_memory: 1024 * 1024,
      eviction_policy: "ttl",
      ttl: 2000  // 2秒TTL
    })
    
    // 添加缓存条目
    cache_manager.put("ttl_test_cache", "ttl_key", "ttl_value")
    
    // 立即访问应该命中
    let immediate_hit = cache_manager.get("ttl_test_cache", "ttl_key")
    assert_true(immediate_hit.is_some)
    
    // 等待TTL过期
    Time::sleep(2100)
    
    // 过期后访问应该未命中
    let expired_miss = cache_manager.get("ttl_test_cache", "ttl_key")
    assert_true(expired_miss.is_none)
    
    // 验证过期统计
    let ttl_stats = cache_manager.get_stats("ttl_test_cache")
    assert_true(ttl_stats.expirations > 0)
  }
  
  ttl_test()
  
  // 测试缓存清理
  let cleanup_test = fn() {
    // 获取清理前的缓存状态
    let before_cleanup = cache_manager.get_stats("span_cache")
    
    // 清理过期条目
    cache_manager.cleanup_expired("span_cache")
    
    // 清理所有缓存
    cache_manager.clear("span_cache")
    cache_manager.clear("metric_cache")
    cache_manager.clear("ttl_test_cache")
    
    // 验证清理效果
    let after_cleanup = cache_manager.get_stats("span_cache")
    assert_eq(after_cleanup.entry_count, 0)
    assert_eq(after_cleanup.memory_usage, 0)
  }
  
  cleanup_test()
}

// 测试6: 资源生命周期管理
test "资源生命周期管理测试" {
  // 创建资源生命周期管理器
  let lifecycle_manager = ResourceLifecycleManager::new()
  
  // 注册资源类型
  lifecycle_manager.register_resource_type("file_handle", {
    initializer: fn(path) { FileHandle::open(path) },
    finalizer: fn(handle) { FileHandle::close(handle) },
    validator: fn(handle) { FileHandle::is_valid(handle) }
  })
  
  lifecycle_manager.register_resource_type("network_connection", {
    initializer: fn(endpoint) { NetworkConnection::open(endpoint) },
    finalizer: fn(conn) { NetworkConnection::close(conn) },
    validator: fn(conn) { NetworkConnection::is_connected(conn) }
  })
  
  lifecycle_manager.register_resource_type("memory_buffer", {
    initializer: fn(size) { MemoryBuffer::allocate(size) },
    finalizer: fn(buffer) { MemoryBuffer::deallocate(buffer) },
    validator: fn(buffer) { MemoryBuffer::is_valid(buffer) }
  })
  
  // 测试资源创建和管理
  let resource_management_test = fn() {
    // 创建文件句柄资源
    let file_resource = lifecycle_manager.create_resource("file_handle", "/tmp/test.log")
    assert_true(lifecycle_manager.is_resource_valid(file_resource))
    
    // 创建网络连接资源
    let network_resource = lifecycle_manager.create_resource("network_connection", "tcp://localhost:8080")
    assert_true(lifecycle_manager.is_resource_valid(network_resource))
    
    // 创建内存缓冲区资源
    let memory_resource = lifecycle_manager.create_resource("memory_buffer", 1024 * 1024)  // 1MB
    assert_true(lifecycle_manager.is_resource_valid(memory_resource))
    
    // 获取资源状态
    let resource_status = lifecycle_manager.get_resource_status()
    assert_eq(resource_status.active_resources, 3)
    assert_eq(resource_status.total_resources_by_type.get("file_handle"), Some(1))
    assert_eq(resource_status.total_resources_by_type.get("network_connection"), Some(1))
    assert_eq(resource_status.total_resources_by_type.get("memory_buffer"), Some(1))
    
    // 测试资源自动清理
    lifecycle_manager.set_auto_cleanup(file_resource, true)
    lifecycle_manager.set_auto_cleanup(network_resource, true)
    lifecycle_manager.set_auto_cleanup(memory_resource, true)
    
    (file_resource, network_resource, memory_resource)
  }
  
  let (file_res, network_res, memory_res) = resource_management_test()
  
  // 测试资源池化
  let resource_pooling_test = fn() {
    // 创建文件句柄池
    lifecycle_manager.create_resource_pool("file_handle_pool", "file_handle", {
      initial_size: 5,
      max_size: 20,
      factory_param: "/tmp/pooled_test.log"
    })
    
    // 从池中获取资源
    let pooled_resources = []
    for i in 0..=10 {
      let resource = lifecycle_manager.acquire_from_pool("file_handle_pool")
      assert_true(lifecycle_manager.is_resource_valid(resource))
      pooled_resources = pooled_resources.push(resource)
    }
    
    // 验证池状态
    let pool_status = lifecycle_manager.get_pool_status("file_handle_pool")
    assert_eq(pool_status.active_resources, 10)
    assert_eq(pool_status.available_resources, 0)
    
    // 归还资源到池中
    for resource in pooled_resources.slice(0, 5) {
      lifecycle_manager.release_to_pool("file_handle_pool", resource)
    }
    
    // 验证归还后的池状态
    let after_release_status = lifecycle_manager.get_pool_status("file_handle_pool")
    assert_eq(after_release_status.active_resources, 5)
    assert_eq(after_release_status.available_resources, 5)
    
    pooled_resources.slice(5)  // 返回未归还的资源
  }
  
  let remaining_pooled_resources = resource_pooling_test()
  
  // 测试资源依赖关系
  let resource_dependency_test = fn() {
    // 创建有依赖关系的资源
    let parent_resource = lifecycle_manager.create_resource("memory_buffer", 2048)  // 2KB
    let child_resource = lifecycle_manager.create_resource("file_handle", "/tmp/dependent.log")
    
    // 设置依赖关系
    lifecycle_manager.add_dependency(parent_resource, child_resource)
    
    // 验证依赖关系
    let dependencies = lifecycle_manager.get_dependencies(parent_resource)
    assert_true(dependencies.contains(child_resource))
    
    let dependents = lifecycle_manager.get_dependents(child_resource)
    assert_true(dependents.contains(parent_resource))
    
    // 测试依赖清理
    lifecycle_manager.cleanup_with_dependencies(parent_resource)
    
    // 验证依赖资源也被清理
    assert_false(lifecycle_manager.is_resource_valid(parent_resource))
    assert_false(lifecycle_manager.is_resource_valid(child_resource))
  }
  
  resource_dependency_test()
  
  // 测试资源泄漏检测
  let resource_leak_test = fn() {
    // 获取初始资源状态
    let initial_status = lifecycle_manager.get_resource_status()
    
    // 创建资源但不设置自动清理
    let leaked_resources = []
    for i in 0..=10 {
      let resource = lifecycle_manager.create_resource("memory_buffer", 1024)  // 1KB
      lifecycle_manager.set_auto_cleanup(resource, false)  // 禁用自动清理
      leaked_resources = leaked_resources.push(resource)
    }
    
    // 检测资源泄漏
    let leak_detection_result = lifecycle_manager.detect_resource_leaks()
    assert_true(leak_detection_result.leaked_resources.length() > 0)
    
    // 验证泄漏报告
    for leaked_resource in leak_detection_result.leaked_resources {
      assert_eq(lifecycle_manager.get_auto_cleanup(leaked_resource), false)
    }
    
    // 手动清理泄漏的资源
    for resource in leaked_resources {
      lifecycle_manager.cleanup_resource(resource)
    }
    
    // 验证清理效果
    let final_status = lifecycle_manager.get_resource_status()
    assert_true(final_status.active_resources < initial_status.active_resources + 10)
  }
  
  resource_leak_test()
  
  // 清理剩余资源
  lifecycle_manager.cleanup_resource(file_res)
  lifecycle_manager.cleanup_resource(network_res)
  lifecycle_manager.cleanup_resource(memory_res)
  
  for resource in remaining_pooled_resources {
    lifecycle_manager.release_to_pool("file_handle_pool", resource)
  }
  
  lifecycle_manager.destroy_pool("file_handle_pool")
}

// 测试7: 内存压缩和整理
test "内存压缩和整理测试" {
  // 创建内存压缩管理器
  let compaction_manager = MemoryCompactionManager::new()
  
  // 配置压缩策略
  compaction_manager.configure_compaction({
    enable_auto_compaction: true,
    compaction_threshold: 0.7,      // 70%碎片率触发压缩
    compaction_interval: 30000,     // 30秒间隔
    max_compaction_time: 5000,      // 最大压缩时间5秒
    compaction_strategy: "generational"  // 分代压缩
  })
  
  // 创建内存碎片
  let create_fragmentation = fn() {
    let allocations = []
    
    // 创建不同大小的分配造成碎片
    for i in 0..=1000 {
      let size = if i % 3 == 0 { 512 } 
                else if i % 3 == 1 { 1024 } 
                else { 2048 }
      
      let allocation = compaction_manager.allocate(size)
      allocations = allocations.push(allocation)
    }
    
    // 随机释放一些分配增加碎片
    for i in 0..=allocations.length() - 1 {
      if i % 2 == 0 {
        compaction_manager.deallocate(allocations[i])
      }
    }
    
    allocations
  }
  
  let fragmented_allocations = create_fragmentation()
  
  // 获取压缩前的内存状态
  let before_compaction = compaction_manager.get_memory_stats()
  let initial_fragmentation = before_compaction.fragmentation_ratio
  
  // 手动触发压缩
  let compaction_start = Time::now()
  let compaction_result = compaction_manager.compact_memory()
  let compaction_end = Time::now()
  let compaction_duration = compaction_end - compaction_start
  
  // 验证压缩结果
  assert_true(compaction_result.success)
  assert_true(compaction_duration < compaction_manager.get_max_compaction_time())
  assert_true(compaction_result.fragments_compacted > 0)
  assert_true(compaction_result.memory_freed > 0)
  
  // 获取压缩后的内存状态
  let after_compaction = compaction_manager.get_memory_stats()
  let final_fragmentation = after_compaction.fragmentation_ratio
  
  // 验证碎片率降低
  assert_true(final_fragmentation < initial_fragmentation)
  let fragmentation_reduction = (initial_fragmentation - final_fragmentation) / initial_fragmentation
  assert_true(fragmentation_reduction > 0.1)  // 至少10%的碎片率降低
  
  // 测试分代压缩
  let generational_compaction_test = fn() {
    // 创建年轻代对象
    let young_objects = []
    for i in 0..=500 {
      let young_obj = compaction_manager.allocate_in_generation(1024, "young")
      young_objects = young_objects.push(young_obj)
    }
    
    // 创建老年代对象
    let old_objects = []
    for i in 0..=200 {
      let old_obj = compaction_manager.allocate_in_generation(2048, "old")
      old_objects = old_objects.push(old_obj)
    }
    
    // 释放一些年轻代对象
    for i in 0..=young_objects.length() / 2 - 1 {
      compaction_manager.deallocate(young_objects[i])
    }
    
    // 执行分代压缩
    let gen_compaction_result = compaction_manager.compact_generation("young")
    
    // 验证年轻代压缩效果
    assert_true(gen_compaction_result.success)
    assert_true(gen_compaction_result.generation == "young")
    
    // 清理剩余对象
    for obj in young_objects.slice(young_objects.length() / 2) {
      compaction_manager.deallocate(obj)
    }
    
    for obj in old_objects {
      compaction_manager.deallocate(obj)
    }
  }
  
  generational_compaction_test()
  
  // 测试内存整理
  let memory_defragmentation_test = fn() {
    // 创建大量小对象
    let small_objects = []
    for i in 0..=2000 {
      let small_obj = compaction_manager.allocate(256)  // 256字节
      small_objects = small_objects.push(small_obj)
    }
    
    // 随机释放造成不连续内存
    for i in 0..=small_objects.length() - 1 {
      if i % 3 != 0 {
        compaction_manager.deallocate(small_objects[i])
      }
    }
    
    let before_defrag = compaction_manager.get_memory_stats()
    let initial_largest_free_block = before_defrag.largest_free_block
    
    // 执行内存整理
    let defrag_result = compaction_manager.defragment_memory()
    
    // 验证整理效果
    assert_true(defrag_result.success)
    assert_true(defrag_result.blocks_moved > 0)
    
    let after_defrag = compaction_manager.get_memory_stats()
    
    // 验证最大空闲块增加
    assert_true(after_defrag.largest_free_block > initial_largest_free_block)
    
    // 清理剩余对象
    for obj in small_objects {
      if compaction_manager.is_allocated(obj) {
        compaction_manager.deallocate(obj)
      }
    }
  }
  
  memory_defragmentation_test()
  
  // 清理剩余分配
  for allocation in fragmented_allocations {
    if compaction_manager.is_allocated(allocation) {
      compaction_manager.deallocate(allocation)
    }
  }
}

// 测试8: 内存使用分析和优化
test "内存使用分析和优化测试" {
  // 创建内存分析器
  let memory_analyzer = MemoryAnalyzer::new()
  
  // 配置分析选项
  memory_analyzer.configure_analysis({
    enable_object_tracking: true,
    enable_allocation_tracing: true,
    enable_stack_trace_capture: true,
    analysis_depth: 10,
    sample_rate: 0.1  // 10%采样率
  })
  
  // 开始分析
  memory_analyzer.start_analysis()
  
  // 创建不同类型的内存使用模式
  let create_memory_patterns = fn() {
    // 大对象模式
    let large_objects = []
    for i in 0..=100 {
      let large_data = "x".repeat(10000)  // 10KB字符串
      large_objects = large_objects.push(large_data)
    }
    
    // 频繁分配释放模式
    for i in 0..=1000 {
      let temp_data = "temp".repeat(100)
      // 模拟使用后立即释放
    }
    
    // 递归分配模式
    let recursive_allocation = fn(depth : Int) -> Array<String> {
      if depth <= 0 {
        []
      } else {
        let data = "recursive".repeat(depth * 10)
        [data] + recursive_allocation(depth - 1)
      }
    }
    
    let recursive_data = recursive_allocation(50)
    
    // 长生命周期对象
    let long_lived_objects = []
    for i in 0..=50 {
      let persistent_data = "persistent_" + i.to_string()
      long_lived_objects = long_lived_objects.push(persistent_data)
    }
    
    (large_objects, recursive_data, long_lived_objects)
  }
  
  let (large_objs, recursive_data, long_lived_objs) = create_memory_patterns()
  
  // 停止分析并生成报告
  memory_analyzer.stop_analysis()
  let analysis_report = memory_analyzer.generate_report()
  
  // 验证分析报告
  assert_true(analysis_report.total_memory_allocated > 0)
  assert_true(analysis_report.total_memory_freed > 0)
  assert_true(analysis_report.peak_memory_usage > 0)
  assert_true(analysis_report.allocation_count > 0)
  assert_true(analysis_report.deallocation_count > 0)
  
  // 验证对象类型分析
  assert_true(analysis_report.object_types.length() > 0)
  
  let string_analysis = analysis_report.object_types.find(fn(t) { t.type_name == "String" })
  assert_true(string_analysis != None)
  
  match string_analysis {
    Some(string_type) => {
      assert_true(string_type.instance_count > 0)
      assert_true(string_type.total_memory > 0)
      assert_true(string_type.average_size > 0)
    }
    None => assert_true(false)
  }
  
  // 验证分配热点
  assert_true(analysis_report.allocation_hotspots.length() > 0)
  
  for hotspot in analysis_report.allocation_hotspots {
    assert_true(hotspot.function_name.length() > 0)
    assert_true(hotspot.allocation_count > 0)
    assert_true(hotspot.total_memory > 0)
  }
  
  // 测试内存优化建议
  let optimization_suggestions = memory_analyzer.generate_optimization_suggestions()
  assert_true(optimization_suggestions.length() > 0)
  
  // 验证建议类型
  let has_object_pool_suggestion = optimization_suggestions.any(fn(s) { 
    s.suggestion_type == "object_pooling" 
  })
  let has_memory_reduction_suggestion = optimization_suggestions.any(fn(s) { 
    s.suggestion_type == "memory_reduction" 
  })
  let has_allocation_pattern_suggestion = optimization_suggestions.any(fn(s) { 
    s.suggestion_type == "allocation_pattern" 
  })
  
  assert_true(has_object_pool_suggestion or has_memory_reduction_suggestion or has_allocation_pattern_suggestion)
  
  // 测试内存使用趋势分析
  let trend_analysis = memory_analyzer.analyze_memory_trends()
  assert_true(trend_analysis.trend_periods.length() > 0)
  
  for period in trend_analysis.trend_periods {
    assert_true(period.start_time > 0)
    assert_true(period.end_time > period.start_time)
    assert_true(period.average_memory_usage > 0)
    assert_true(period.peak_memory_usage >= period.average_memory_usage)
  }
  
  // 验证趋势预测
  if trend_analysis.trend_periods.length() >= 3 {
    let prediction = memory_analyzer.predict_memory_usage(trend_analysis)
    assert_true(prediction.predicted_usage > 0)
    assert_true(prediction.confidence_score > 0.0 and prediction.confidence_score <= 1.0)
  }
  
  // 测试内存效率评分
  let efficiency_score = memory_analyzer.calculate_efficiency_score()
  assert_true(efficiency_score.overall_score > 0.0 and efficiency_score.overall_score <= 1.0)
  assert_true(efficiency_score.allocation_efficiency > 0.0)
  assert_true(efficiency_score.garbage_collection_efficiency > 0.0)
  assert_true(efficiency_score.memory_utilization > 0.0)
  
  // 清理测试数据
  large_objs = []
  recursive_data = []
  long_lived_objs = []
}

// 测试9: 并发内存管理
test "并发内存管理测试" {
  // 创建并发内存管理器
  let concurrent_memory_manager = ConcurrentMemoryManager::new()
  
  // 配置并发策略
  concurrent_memory_manager.configure_concurrency({
    max_threads: 8,
    allocation_strategy: "thread_local",  // 线程本地分配
    gc_strategy: "concurrent",          // 并发垃圾回收
    lock_strategy: "lock_free"          // 无锁数据结构
  })
  
  // 创建线程池
  let thread_pool = ThreadPool::new(8)
  
  // 测试并发分配
  let concurrent_allocation_test = fn() {
    let futures = []
    
    for thread_id in 0..=7 {
      let future = thread_pool.spawn(fn() {
        let allocations = []
        
        // 每个线程分配1000个对象
        for i in 0..=1000 {
          let size = 512 + (i % 10) * 256  // 512-3072字节
          let allocation = concurrent_memory_manager.allocate(size)
          allocations = allocations.push(allocation)
        }
        
        // 随机释放一半对象
        for i in 0..=allocations.length() / 2 - 1 {
          concurrent_memory_manager.deallocate(allocations[i])
        }
        
        allocations.slice(allocations.length() / 2)  // 返回未释放的对象
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    let results = []
    for future in futures {
      let result = future.wait()
      results = results.push(result)
    }
    
    results
  }
  
  let concurrent_results = concurrent_allocation_test()
  
  // 验证并发分配结果
  assert_eq(concurrent_results.length(), 8)
  
  let total_remaining_allocations = concurrent_results.map(fn(r) { r.length() }).sum()
  assert_eq(total_remaining_allocations, 8 * 500)  // 每个线程剩余500个对象
  
  // 验证并发安全性
  let concurrent_stats = concurrent_memory_manager.get_concurrent_stats()
  assert_eq(concurrent_stats.active_threads, 8)
  assert_true(concurrent_stats.total_allocations > 0)
  assert_true(concurrent_stats.total_deallocations > 0)
  assert_eq(concurrent_stats.contention_events, 0)  // 无锁策略应该无竞争
  
  // 测试并发垃圾回收
  let concurrent_gc_test = fn() {
    // 创建大量对象用于GC测试
    let gc_futures = []
    
    for thread_id in 0..=7 {
      let future = thread_pool.spawn(fn() {
        let gc_objects = []
        
        // 创建大量短生命周期对象
        for i in 0..=5000 {
          let obj = "gc_test_object_" + i.to_string()
          gc_objects = gc_objects.push(obj)
          
          // 每100个对象释放一次
          if i % 100 == 99 {
            gc_objects = []
          }
        }
        
        thread_id  // 返回线程ID
      })
      gc_futures = gc_futures.push(future)
    }
    
    // 启动并发GC
    let gc_start = Time::now()
    concurrent_memory_manager.start_concurrent_gc()
    
    // 等待所有线程完成
    let thread_ids = []
    for future in gc_futures {
      let thread_id = future.wait()
      thread_ids = thread_ids.push(thread_id)
    }
    
    // 等待GC完成
    concurrent_memory_manager.wait_for_gc_completion()
    let gc_end = Time::now()
    
    (thread_ids, gc_end - gc_start)
  }
  
  let (gc_thread_ids, gc_duration) = concurrent_gc_test()
  
  // 验证并发GC
  assert_eq(gc_thread_ids.length(), 8)
  assert_true(gc_duration < 10000)  // GC应该在10秒内完成
  
  let gc_stats = concurrent_memory_manager.get_gc_stats()
  assert_true(gc_stats.concurrent_gc_runs > 0)
  assert_true(gc_stats.objects_collected > 0)
  assert_true(gc_stats.memory_reclaimed > 0)
  
  // 测试无锁数据结构性能
  let lock_free_performance_test = fn() {
    let lock_free_futures = []
    
    for thread_id in 0..=7 {
      let future = thread_pool.spawn(fn() {
        let operations = 0
        
        // 执行大量无锁操作
        for i in 0..=10000 {
          let allocation = concurrent_memory_manager.lock_free_allocate(256)
          concurrent_memory_manager.lock_free_deallocate(allocation)
          operations = operations + 1
        }
        
        operations
      })
      lock_free_futures = lock_free_futures.push(future)
    }
    
    let start_time = Time::now()
    
    let operation_counts = []
    for future in lock_free_futures {
      let count = future.wait()
      operation_counts = operation_counts.push(count)
    }
    
    let end_time = Time::now()
    let total_duration = end_time - start_time
    let total_operations = operation_counts.sum()
    let throughput = total_operations.to_float() / total_duration.to_float() * 1000.0
    
    (total_operations, throughput)
  }
  
  let (total_ops, throughput) = lock_free_performance_test()
  
  // 验证无锁性能
  assert_eq(total_ops, 8 * 10001)
  assert_true(throughput > 100000)  // 至少100K操作/秒
  
  // 清理剩余分配
  for thread_allocations in concurrent_results {
    for allocation in thread_allocations {
      concurrent_memory_manager.deallocate(allocation)
    }
  }
}

// 测试10: 内存压力和极限测试
test "内存压力和极限测试" {
  // 创建内存压力测试管理器
  let stress_test_manager = MemoryStressTestManager::new()
  
  // 配置压力测试参数
  stress_test_manager.configure_stress_test({
    max_memory_pressure: 0.9,      // 90%内存使用率
    stress_duration: 30000,         // 30秒压力测试
    allocation_burst_size: 1000,    // 每次突发分配1000个对象
    deallocation_ratio: 0.7,        // 70%释放率
    enable_emergency_gc: true,      // 启用紧急GC
    emergency_gc_threshold: 0.85    // 85%触发紧急GC
  })
  
  // 获取系统内存信息
  let system_memory = stress_test_manager.get_system_memory_info()
  assert_true(system_memory.total_memory > 0)
  assert_true(system_memory.available_memory > 0)
  assert_true(system_memory.used_memory > 0)
  
  // 测试内存压力场景
  let memory_pressure_test = fn() {
    let stress_start = Time::now()
    let pressure_allocations = []
    let emergency_gc_triggered = false
    
    // 持续分配内存直到达到压力阈值
    while stress_test_manager.get_memory_pressure() < 0.8 {
      let batch_allocations = []
      
      // 突发分配
      for i in 0..=stress_test_manager.get_allocation_burst_size() {
        let size = 1024 + (i % 10) * 512  // 1KB-6KB
        let allocation = stress_test_manager.stress_allocate(size)
        batch_allocations = batch_allocations.push(allocation)
      }
      
      // 按比例释放
      let deallocation_count = (batch_allocations.length().to_float() * 
                              stress_test_manager.get_deallocation_ratio()).to_int()
      
      for i in 0..=deallocation_count - 1 {
        stress_test_manager.stress_deallocate(batch_allocations[i])
      }
      
      // 保留未释放的分配
      pressure_allocations = pressure_allocations.concat(
        batch_allocations.slice(deallocation_count)
      )
      
      // 检查是否触发紧急GC
      if stress_test_manager.get_memory_pressure() > stress_test_manager.get_emergency_gc_threshold() {
        if not emergency_gc_triggered {
          stress_test_manager.trigger_emergency_gc()
          emergency_gc_triggered = true
        }
      }
      
      // 防止无限循环
      if Time::now() - stress_start > 60000 {  // 60秒超时
        break
      }
    }
    
    (pressure_allocations, emergency_gc_triggered)
  }
  
  let (pressure_allocs, emergency_triggered) = memory_pressure_test()
  
  // 验证压力测试结果
  assert_true(pressure_allocs.length() > 0)
  assert_true(stress_test_manager.get_memory_pressure() >= 0.8)
  assert_true(emergency_triggered)
  
  // 验证紧急GC统计
  let emergency_gc_stats = stress_test_manager.get_emergency_gc_stats()
  assert_true(emergency_gc_stats.trigger_count > 0)
  assert_true(emergency_gc_stats.memory_reclaimed > 0)
  assert_true(emergency_gc_stats.average_duration > 0)
  
  // 测试内存耗尽恢复
  let memory_exhaustion_recovery_test = fn() {
    // 模拟内存耗尽
    let exhaustion_allocations = []
    
    // 分配直到接近内存极限
    while stress_test_manager.get_memory_pressure() < 0.95 {
      let allocation = stress_test_manager.stress_allocate(1024)  // 1KB
      if allocation.is_valid {
        exhaustion_allocations = exhaustion_allocations.push(allocation)
      } else {
        break  // 分配失败，内存耗尽
      }
    }
    
    let max_pressure = stress_test_manager.get_memory_pressure()
    
    // 触发内存恢复程序
    let recovery_start = Time::now()
    let recovery_result = stress_test_manager.execute_memory_recovery()
    let recovery_end = Time::now()
    
    // 验证恢复效果
    assert_true(recovery_result.success)
    assert_true(recovery_end - recovery_start < 10000)  // 恢复应在10秒内完成
    assert_true(recovery_result.memory_recovered > 0)
    assert_true(recovery_result.allocations_freed > 0)
    
    let post_recovery_pressure = stress_test_manager.get_memory_pressure()
    assert_true(post_recovery_pressure < max_pressure)
    
    // 清理剩余分配
    for allocation in exhaustion_allocations {
      stress_test_manager.stress_deallocate(allocation)
    }
    
    recovery_result
  }
  
  let recovery_result = memory_exhaustion_recovery_test()
  assert_true(recovery_result.success)
  
  // 测试内存限制下的操作
  let memory_limit_test = fn() {
    // 设置内存限制
    let memory_limit = system_memory.available_memory / 2  // 使用50%可用内存
    stress_test_manager.set_memory_limit(memory_limit)
    
    let limit_allocations = []
    let allocation_failures = 0
    
    // 在限制下分配内存
    for i in 0..=10000 {
      let allocation = stress_test_manager.allocate_with_limit(10240)  // 10KB
      
      if allocation.is_valid {
        limit_allocations = limit_allocations.push(allocation)
      } else {
        allocation_failures = allocation_failures + 1
      }
      
      // 如果失败率过高，停止测试
      if allocation_failures > 100 {
        break
      }
    }
    
    // 验证限制效果
    let current_usage = stress_test_manager.get_current_memory_usage()
    assert_true(current_usage <= memory_limit)
    assert_true(allocation_failures > 0)  // 应该有分配失败
    
    // 清理分配
    for allocation in limit_allocations {
      stress_test_manager.stress_deallocate(allocation)
    }
    
    allocation_failures
  }
  
  let failure_count = memory_limit_test()
  assert_true(failure_count > 0)
  
  // 测试内存压力下的性能退化
  let performance_degradation_test = fn() {
    // 基准性能测试（正常内存压力下）
    let baseline_start = Time::now()
    let baseline_allocations = []
    
    for i in 0..=1000 {
      let allocation = stress_test_manager.stress_allocate(1024)
      baseline_allocations = baseline_allocations.push(allocation)
    }
    
    let baseline_end = Time::now()
    let baseline_duration = baseline_end - baseline_start
    
    // 清理基准测试分配
    for allocation in baseline_allocations {
      stress_test_manager.stress_deallocate(allocation)
    }
    
    // 高内存压力下的性能测试
    let pressure_allocations = []
    for i in 0..=5000 {
      let allocation = stress_test_manager.stress_allocate(2048)  // 2KB
      pressure_allocations = pressure_allocations.push(allocation)
    }
    
    let stress_start = Time::now()
    let stress_test_allocations = []
    
    for i in 0..=1000 {
      let allocation = stress_test_manager.stress_allocate(1024)
      stress_test_allocations = stress_test_allocations.push(allocation)
    }
    
    let stress_end = Time::now()
    let stress_duration = stress_end - stress_start
    
    // 清理压力测试分配
    for allocation in stress_test_allocations {
      stress_test_manager.stress_deallocate(allocation)
    }
    
    for allocation in pressure_allocations {
      stress_test_manager.stress_deallocate(allocation)
    }
    
    // 计算性能退化
    let performance_ratio = baseline_duration.to_float() / stress_duration.to_float()
    
    performance_ratio
  }
  
  let performance_ratio = performance_degradation_test()
  
  // 验证性能退化在可接受范围内
  assert_true(performance_ratio > 0.3)  // 性能退化不应超过70%
  
  // 清理压力测试分配
  for allocation in pressure_allocs {
    stress_test_manager.stress_deallocate(allocation)
  }
  
  // 生成压力测试报告
  let stress_report = stress_test_manager.generate_stress_test_report()
  
  // 验证压力测试报告
  assert_true(stress_report.test_duration > 0)
  assert_true(stress_report.max_memory_pressure > 0.8)
  assert_true(stress_report.emergency_gc_count > 0)
  assert_true(stress_report.memory_recovery_count > 0)
  assert_true(stress_report.performance_degradation <= 0.7)
}