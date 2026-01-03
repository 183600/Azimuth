// Azimuth Memory Management and Resource Cleanup Tests
// This file contains test cases for memory management and resource cleanup

// Test 1: Resource Lifecycle Management
test "资源生命周期管理" {
  // 定义资源类型
  type Resource[T] = {
    id: String,
    data: T,
    created_at: Int,
    last_accessed: Int,
    reference_count: Int
  }
  
  // 资源池
  type ResourcePool[T] = {
    resources: Map[String, Resource[T]],
    max_size: Int,
    cleanup_threshold: Int
  }
  
  // 创建资源池
  let create_resource_pool = fn(max_size: Int, cleanup_threshold: Int) {
    {
      resources: Map::empty(),
      max_size,
      cleanup_threshold
    }
  }
  
  // 添加资源到池
  let add_resource = fn(pool: ResourcePool[T], id: String, data: T) {
    let now = Time::now()
    let resource = {
      id,
      data,
      created_at: now,
      last_accessed: now,
      reference_count: 1
    }
    
    let updated_resources = Map::insert(pool.resources, id, resource)
    
    // 检查是否需要清理
    let needs_cleanup = updated_resources.size() > pool.cleanup_threshold
    let final_resources = if needs_cleanup {
      cleanup_old_resources(updated_resources, pool.max_size)
    } else {
      updated_resources
    }
    
    {
      ...pool,
      resources: final_resources
    }
  }
  
  // 清理旧资源
  let cleanup_old_resources = fn(resources: Map[String, Resource[T]], max_size: Int) {
    // 按最后访问时间排序
    let resource_list = resources.to_array()
      .sort(fn(a, b) {
        let (_, resource_a) = a
        let (_, resource_b) = b
        if resource_a.last_accessed < resource_b.last_accessed { -1 }
        else if resource_a.last_accessed > resource_b.last_accessed { 1 }
        else { 0 }
      })
    
    // 保留最新的max_size个资源
    let keep_count = if resource_list.length() > max_size {
      max_size
    } else {
      resource_list.length()
    }
    
    let kept_resources = resource_list.slice(0, keep_count)
    let result = Map::empty()
    
    for (id, resource) in kept_resources {
      let _ = Map::insert(result, id, resource)
    }
    
    result
  }
  
  // 获取资源
  let get_resource = fn(pool: ResourcePool[T], id: String) {
    match Map::get(pool.resources, id) {
      Some(resource) => {
        // 更新最后访问时间和引用计数
        let updated_resource = {
          ...resource,
          last_accessed: Time::now(),
          reference_count: resource.reference_count + 1
        }
        let updated_resources = Map::insert(pool.resources, id, updated_resource)
        
        {
          pool: {
            ...pool,
            resources: updated_resources
          },
          resource: Some(updated_resource)
        }
      }
      None => {
        {
          pool,
          resource: None
        }
      }
    }
  }
  
  // 释放资源
  let release_resource = fn(pool: ResourcePool[T], id: String) {
    match Map::get(pool.resources, id) {
      Some(resource) => {
        let new_ref_count = resource.reference_count - 1
        
        if new_ref_count <= 0 {
          // 完全移除资源
          let updated_resources = Map::remove(pool.resources, id)
          {
            ...pool,
            resources: updated_resources
          }
        } else {
          // 减少引用计数
          let updated_resource = {
            ...resource,
            reference_count: new_ref_count
          }
          let updated_resources = Map::insert(pool.resources, id, updated_resource)
          {
            ...pool,
            resources: updated_resources
          }
        }
      }
      None => pool
    }
  }
  
  // 测试资源池创建
  let pool = create_resource_pool(3, 5)
  assert_eq(pool.max_size, 3)
  assert_eq(pool.cleanup_threshold, 5)
  
  // 测试添加资源
  let pool_with_resource = add_resource(pool, "res1", "data1")
  assert_eq(pool_with_resource.resources.size(), 1)
  
  let pool_with_more_resources = add_resource(pool_with_resource, "res2", "data2")
  let pool_with_even_more = add_resource(pool_with_more_resources, "res3", "data3")
  
  // 测试获取资源
  let get_result = get_resource(pool_with_even_more, "res2")
  match get_result.resource {
    Some(resource) => {
      assert_eq(resource.id, "res2")
      assert_eq(resource.reference_count, 2)  // 初始为1，获取后+1
    }
    None => assert_true(false)
  }
  
  // 测试释放资源
  let pool_after_release = release_resource(get_result.pool, "res2")
  match Map::get(pool_after_release.resources, "res2") {
    Some(resource) => {
      assert_eq(resource.reference_count, 1)  // 从2减到1
    }
    None => assert_true(false)
  }
  
  // 测试完全释放
  let pool_after_full_release = release_resource(pool_after_release, "res2")
  match Map::get(pool_after_full_release.resources, "res2") {
    Some(_) => assert_true(false)  // 应该被移除
    None => assert_true(true)
  }
}

// Test 2: Memory Leak Detection
test "内存泄漏检测" {
  // 定义内存使用情况
  type MemoryUsage = {
    allocated: Int,
    freed: Int,
    peak_usage: Int,
    allocations: Array[Allocation]
  }
  
  type Allocation = {
    id: String,
    size: Int,
    timestamp: Int,
    freed: Bool
  }
  
  // 内存跟踪器
  type MemoryTracker = {
    usage: MemoryUsage,
    leak_threshold: Int
  }
  
  // 创建内存跟踪器
  let create_memory_tracker = fn(leak_threshold: Int) {
    {
      usage: {
        allocated: 0,
        freed: 0,
        peak_usage: 0,
        allocations: []
      },
      leak_threshold
    }
  }
  
  // 分配内存
  let allocate_memory = fn(tracker: MemoryTracker, size: Int) {
    let allocation_id = "alloc-" + Time::now().to_string()
    let allocation = {
      id: allocation_id,
      size,
      timestamp: Time::now(),
      freed: false
    }
    
    let new_allocated = tracker.usage.allocated + size
    let new_peak = if new_allocated > tracker.usage.peak_usage {
      new_allocated
    } else {
      tracker.usage.peak_usage
    }
    
    let updated_usage = {
      allocated: new_allocated,
      freed: tracker.usage.freed,
      peak_usage: new_peak,
      allocations: tracker.usage.allocations.push(allocation)
    }
    
    {
      tracker: {
        ...tracker,
        usage: updated_usage
      },
      allocation_id
    }
  }
  
  // 释放内存
  let free_memory = fn(tracker: MemoryTracker, allocation_id: String) {
    let updated_allocations = tracker.usage.allocations.map(fn(allocation) {
      if allocation.id == allocation_id && not allocation.freed {
        {
          ...allocation,
          freed: true
        }
      } else {
        allocation
      }
    })
    
    // 找到要释放的分配
    let allocation_to_free = tracker.usage.allocations.find(fn(a) { a.id == allocation_id })
    let freed_size = match allocation_to_free {
      Some(allocation) => if allocation.freed { 0 } else { allocation.size }
      None => 0
    }
    
    let updated_usage = {
      allocated: tracker.usage.allocated,
      freed: tracker.usage.freed + freed_size,
      peak_usage: tracker.usage.peak_usage,
      allocations: updated_allocations
    }
    
    {
      ...tracker,
      usage: updated_usage
    }
  }
  
  // 检测内存泄漏
  let detect_memory_leaks = fn(tracker: MemoryTracker) {
    let current_usage = tracker.usage.allocated - tracker.usage.freed
    let leaked_allocations = tracker.usage.allocations.filter(fn(allocation) {
      not allocation.freed
    })
    
    let has_leak = current_usage > tracker.leak_threshold
    
    {
      has_leak,
      current_usage,
      leaked_allocations,
      leak_count: leaked_allocations.length()
    }
  }
  
  // 测试内存跟踪
  let tracker = create_memory_tracker(100)
  
  // 分配内存
  let result1 = allocate_memory(tracker, 50)
  let result2 = allocate_memory(result1.tracker, 30)
  let result3 = allocate_memory(result2.tracker, 25)
  
  // 验证分配
  assert_eq(result3.tracker.usage.allocated, 105)  // 50 + 30 + 25
  assert_eq(result3.tracker.usage.peak_usage, 105)
  assert_eq(result3.tracker.usage.allocations.length(), 3)
  
  // 释放部分内存
  let tracker_after_free1 = free_memory(result3.tracker, result1.allocation_id)
  assert_eq(tracker_after_free1.usage.freed, 50)
  
  let tracker_after_free2 = free_memory(tracker_after_free1, result2.allocation_id)
  assert_eq(tracker_after_free2.usage.freed, 80)  // 50 + 30
  
  // 检测内存泄漏
  let leak_result = detect_memory_leaks(tracker_after_free2)
  assert_eq(leak_result.current_usage, 25)  // 105 - 80
  assert_false(leak_result.has_leak)  // 25 < 100 (阈值)
  assert_eq(leak_result.leak_count, 1)  // 只有一个未释放的分配
  
  // 创建泄漏情况
  let leaky_tracker = create_memory_tracker(10)
  let leaky_result1 = allocate_memory(leaky_tracker, 20)
  let leaky_result2 = allocate_memory(leaky_result1.tracker, 15)
  
  let leaky_detection = detect_memory_leaks(leaky_result2.tracker)
  assert_true(leaky_detection.has_leak)  // 35 > 10 (阈值)
  assert_eq(leaky_detection.leak_count, 2)  // 两个未释放的分配
}

// Test 3: Resource Pool with Automatic Cleanup
test "带自动清理的资源池" {
  // 定义可清理资源
  type CleanableResource = {
    id: String,
    data: String,
    created_at: Int,
    last_accessed: Int,
    cleanup_fn: Option[() -> Unit]
  }
  
  // 资源池配置
  type PoolConfig = {
    max_size: Int,
    idle_timeout_ms: Int,
    cleanup_interval_ms: Int
  }
  
  // 增强的资源池
  type EnhancedResourcePool = {
    resources: Map[String, CleanableResource],
    config: PoolConfig,
    last_cleanup: Int
  }
  
  // 创建增强资源池
  let create_enhanced_pool = fn(config: PoolConfig) {
    {
      resources: Map::empty(),
      config,
      last_cleanup: Time::now()
    }
  }
  
  // 添加资源
  let add_cleanable_resource = fn(pool: EnhancedResourcePool, id: String, data: String, cleanup_fn: Option[() -> Unit>) {
    let now = Time::now()
    let resource = {
      id,
      data,
      created_at: now,
      last_accessed: now,
      cleanup_fn
    }
    
    let updated_resources = Map::insert(pool.resources, id, resource)
    
    // 检查是否需要清理
    let needs_cleanup = (now - pool.last_cleanup) > pool.config.cleanup_interval_ms
    let final_resources = if needs_cleanup {
      cleanup_idle_resources(updated_resources, pool.config.idle_timeout_ms, now)
    } else {
      updated_resources
    }
    
    {
      resources: final_resources,
      config: pool.config,
      last_cleanup: if needs_cleanup { now } else { pool.last_cleanup }
    }
  }
  
  // 清理空闲资源
  let cleanup_idle_resources = fn(resources: Map[String, CleanableResource], idle_timeout_ms: Int, now: Int) {
    let result = Map::empty()
    
    for (id, resource) in resources {
      let idle_time = now - resource.last_accessed
      
      if idle_time > idle_timeout_ms {
        // 执行清理函数
        match resource.cleanup_fn {
          Some(cleanup_fn) => cleanup_fn()
          None => ()
        }
      } else {
        // 保留资源
        let _ = Map::insert(result, id, resource)
      }
    }
    
    result
  }
  
  // 获取资源
  let get_cleanable_resource = fn(pool: EnhancedResourcePool, id: String) {
    match Map::get(pool.resources, id) {
      Some(resource) => {
        let updated_resource = {
          ...resource,
          last_accessed: Time::now()
        }
        let updated_resources = Map::insert(pool.resources, id, updated_resource)
        
        {
          pool: {
            ...pool,
            resources: updated_resources
          },
          resource: Some(updated_resource)
        }
      }
      None => {
        {
          pool,
          resource: None
        }
      }
    }
  }
  
  // 测试增强资源池
  let config = {
    max_size: 5,
    idle_timeout_ms: 1000,
    cleanup_interval_ms: 500
  }
  
  let pool = create_enhanced_pool(config)
  
  // 创建清理计数器
  let mut cleanup_count = 0
  let test_cleanup_fn = fn() { cleanup_count = cleanup_count + 1 }
  
  // 添加资源
  let pool_with_resource = add_cleanable_resource(pool, "res1", "data1", Some(test_cleanup_fn))
  assert_eq(pool_with_resource.resources.size(), 1)
  
  // 模拟时间流逝（超过空闲超时）
  let future_time = Time::now() + 1500
  
  // 手动触发清理（模拟时间流逝）
  let resources_after_time = Map::empty()
  for (id, resource) in pool_with_resource.resources {
    let updated_resource = {
      ...resource,
      last_accessed: resource.created_at  // 保持旧的访问时间
    }
    let _ = Map::insert(resources_after_time, id, updated_resource)
  }
  
  let pool_before_cleanup = {
    ...pool_with_resource,
    resources: resources_after_time,
    last_cleanup: future_time - 600  // 触发清理
  }
  
  // 执行清理
  let cleaned_resources = cleanup_idle_resources(
    pool_before_cleanup.resources,
    pool_before_cleanup.config.idle_timeout_ms,
    future_time
  )
  
  // 验证资源被清理
  assert_eq(cleaned_resources.size(), 0)
  
  // 测试资源访问更新
  let pool_with_fresh_resource = add_cleanable_resource(pool, "res2", "data2", None)
  let access_result = get_cleanable_resource(pool_with_fresh_resource, "res2")
  
  match access_result.resource {
    Some(resource) => {
      assert_eq(resource.id, "res2")
      assert_true(resource.last_accessed > resource.created_at)
    }
    None => assert_true(false)
  }
}

// Test 4: Memory Pressure Handling
test "内存压力处理" {
  // 定义内存压力级别
  enum MemoryPressure {
    Normal
    Warning
    Critical
    Emergency
  }
  
  // 内存监控器
  type MemoryMonitor = {
    current_usage: Int,
    total_memory: Int,
    warning_threshold: Float,
    critical_threshold: Float,
    emergency_threshold: Float
  }
  
  // 创建内存监控器
  let create_memory_monitor = fn(total_memory: Int) {
    {
      current_usage: 0,
      total_memory,
      warning_threshold: 0.7,    // 70%
      critical_threshold: 0.85,  // 85%
      emergency_threshold: 0.95  // 95%
    }
  }
  
  // 更新内存使用情况
  let update_memory_usage = fn(monitor: MemoryMonitor, usage: Int) {
    {
      ...monitor,
      current_usage: usage
    }
  }
  
  // 获取内存压力级别
  let get_memory_pressure = fn(monitor: MemoryMonitor) {
    let usage_ratio = monitor.current_usage as Float / monitor.total_memory as Float
    
    if usage_ratio >= monitor.emergency_threshold {
      MemoryPressure::Emergency
    } else if usage_ratio >= monitor.critical_threshold {
      MemoryPressure::Critical
    } else if usage_ratio >= monitor.warning_threshold {
      MemoryPressure::Warning
    } else {
      MemoryPressure::Normal
    }
  }
  
  // 内存压力响应策略
  type PressureResponse = {
    should_gc: Bool,
    should_clear_cache: Bool,
    should_reject_allocations: Bool,
    should_force_cleanup: Bool
  }
  
  // 获取压力响应
  let get_pressure_response = fn(pressure: MemoryPressure) {
    match pressure {
      MemoryPressure::Normal => {
        {
          should_gc: false,
          should_clear_cache: false,
          should_reject_allocations: false,
          should_force_cleanup: false
        }
      }
      MemoryPressure::Warning => {
        {
          should_gc: true,
          should_clear_cache: false,
          should_reject_allocations: false,
          should_force_cleanup: false
        }
      }
      MemoryPressure::Critical => {
        {
          should_gc: true,
          should_clear_cache: true,
          should_reject_allocations: false,
          should_force_cleanup: false
        }
      }
      MemoryPressure::Emergency => {
        {
          should_gc: true,
          should_clear_cache: true,
          should_reject_allocations: true,
          should_force_cleanup: true
        }
      }
    }
  }
  
  // 测试内存监控
  let monitor = create_memory_monitor(1000)  // 1000单位内存
  
  // 正常使用情况
  let normal_monitor = update_memory_usage(monitor, 500)  // 50%
  let normal_pressure = get_memory_pressure(normal_monitor)
  assert_eq(normal_pressure, MemoryPressure::Normal)
  
  let normal_response = get_pressure_response(normal_pressure)
  assert_false(normal_response.should_gc)
  assert_false(normal_response.should_clear_cache)
  assert_false(normal_response.should_reject_allocations)
  
  // 警告级别
  let warning_monitor = update_memory_usage(monitor, 750)  // 75%
  let warning_pressure = get_memory_pressure(warning_monitor)
  assert_eq(warning_pressure, MemoryPressure::Warning)
  
  let warning_response = get_pressure_response(warning_pressure)
  assert_true(warning_response.should_gc)
  assert_false(warning_response.should_clear_cache)
  assert_false(warning_response.should_reject_allocations)
  
  // 关键级别
  let critical_monitor = update_memory_usage(monitor, 900)  // 90%
  let critical_pressure = get_memory_pressure(critical_monitor)
  assert_eq(critical_pressure, MemoryPressure::Critical)
  
  let critical_response = get_pressure_response(critical_pressure)
  assert_true(critical_response.should_gc)
  assert_true(critical_response.should_clear_cache)
  assert_false(critical_response.should_reject_allocations)
  
  // 紧急级别
  let emergency_monitor = update_memory_usage(monitor, 980)  // 98%
  let emergency_pressure = get_memory_pressure(emergency_monitor)
  assert_eq(emergency_pressure, MemoryPressure::Emergency)
  
  let emergency_response = get_pressure_response(emergency_pressure)
  assert_true(emergency_response.should_gc)
  assert_true(emergency_response.should_clear_cache)
  assert_true(emergency_response.should_reject_allocations)
  assert_true(emergency_response.should_force_cleanup)
}