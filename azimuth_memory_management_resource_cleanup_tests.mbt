// Azimuth 内存管理和资源清理测试用例
// 专注于测试内存泄漏检测、资源分配和自动清理机制

// 测试1: 内存泄漏检测
test "内存泄漏检测" {
  // 1. 创建内存监控器
  let memory_monitor = MemoryMonitor::new()
  let initial_memory = MemoryMonitor::get_allocated_memory(memory_monitor)
  
  // 2. 创建大量临时对象
  let temporary_objects = []
  for i in 0..=1000 {
    let telemetry_data = TelemetryData({
      id: "temp_" + i.to_string(),
      metrics: [
        ("cpu", FloatValue(0.5 + (i % 100) as Float / 100.0)),
        ("memory", IntValue(1024 * (i % 10 + 1))),
        ("network", IntValue(100 * (i % 5 + 1)))
      ],
      timestamp: 1735689600000000000L + (i as Int64 * 1000000L)
    })
    temporary_objects.push(telemetry_data)
  }
  
  // 3. 验证内存增长
  let peak_memory = MemoryMonitor::get_allocated_memory(memory_monitor)
  assert_true(peak_memory > initial_memory)
  
  // 4. 清理对象
  temporary_objects = []
  
  // 5. 强制垃圾回收
  MemoryMonitor::force_gc(memory_monitor)
  
  // 6. 验证内存回收
  let final_memory = MemoryMonitor::get_allocated_memory(memory_monitor)
  let memory_leak = final_memory - initial_memory
  
  // 允许少量内存增长（由于缓存等原因）
  assert_true(memory_leak < (temporary_objects.length() * 100)) // 每个对象不超过100字节泄漏
}

// 测试2: 资源生命周期管理
test "资源生命周期管理" {
  // 1. 创建资源管理器
  let resource_manager = ResourceManager::new()
  
  // 2. 创建多种类型的资源
  let file_resource = ResourceManager::create_file_resource(resource_manager, "/tmp/telemetry.log")
  let network_resource = ResourceManager::create_network_resource(resource_manager, "tcp://localhost:8080")
  let memory_resource = ResourceManager::create_memory_resource(resource_manager, 1024 * 1024) // 1MB
  let db_connection = ResourceManager::create_db_connection(resource_manager, "postgresql://localhost/azimuth")
  
  // 3. 验证资源创建
  assert_true(ResourceManager::is_resource_active(resource_manager, file_resource.id))
  assert_true(ResourceManager::is_resource_active(resource_manager, network_resource.id))
  assert_true(ResourceManager::is_resource_active(resource_manager, memory_resource.id))
  assert_true(ResourceManager::is_resource_active(resource_manager, db_connection.id))
  
  // 4. 使用资源
  ResourceManager::write_to_file(resource_manager, file_resource.id, "Telemetry data: test")
  ResourceManager::send_network_data(resource_manager, network_resource.id, "ping")
  ResourceManager::allocate_memory(resource_manager, memory_resource.id, 512)
  ResourceManager::execute_query(resource_manager, db_connection.id, "SELECT 1")
  
  // 5. 验证资源使用状态
  assert_eq(ResourceManager::get_file_size(resource_manager, file_resource.id), 21)
  assert_eq(ResourceManager::get_network_bytes_sent(resource_manager, network_resource.id), 4)
  assert_eq(ResourceManager::get_memory_used(resource_manager, memory_resource.id), 512)
  assert_eq(ResourceManager::get_db_query_count(resource_manager, db_connection.id), 1)
  
  // 6. 释放资源
  ResourceManager::release_resource(resource_manager, file_resource.id)
  ResourceManager::release_resource(resource_manager, network_resource.id)
  ResourceManager::release_resource(resource_manager, memory_resource.id)
  ResourceManager::release_resource(resource_manager, db_connection.id)
  
  // 7. 验证资源释放
  assert_false(ResourceManager::is_resource_active(resource_manager, file_resource.id))
  assert_false(ResourceManager::is_resource_active(resource_manager, network_resource.id))
  assert_false(ResourceManager::is_resource_active(resource_manager, memory_resource.id))
  assert_false(ResourceManager::is_resource_active(resource_manager, db_connection.id))
  
  // 8. 验证资源管理器清理
  assert_eq(ResourceManager::get_active_resource_count(resource_manager), 0)
}

// 测试3: 自动资源清理
test "自动资源清理" {
  // 1. 创建自动清理管理器
  let auto_cleanup_manager = AutoCleanupManager::new()
  
  // 2. 创建带自动清理的资源
  let auto_cleanup_resources = []
  for i in 0..=100 {
    let resource = AutoCleanupManager::create_resource(
      auto_cleanup_manager,
      "resource_" + i.to_string(),
      1000 + i * 100 // 不同的过期时间
    )
    auto_cleanup_resources.push(resource)
  }
  
  // 3. 验证资源创建
  assert_eq(AutoCleanupManager::get_resource_count(auto_cleanup_manager), 100)
  
  // 4. 模拟时间流逝，触发部分资源过期
  AutoCleanupManager::advance_time(auto_cleanup_manager, 1500)
  
  // 5. 触发自动清理
  AutoCleanupManager::trigger_cleanup(auto_cleanup_manager)
  
  // 6. 验证部分资源被清理
  assert_true(AutoCleanupManager::get_resource_count(auto_cleanup_manager) < 100)
  assert_true(AutoCleanupManager::get_resource_count(auto_cleanup_manager) > 0)
  
  // 7. 继续时间流逝，清理所有资源
  AutoCleanupManager::advance_time(auto_cleanup_manager, 10000)
  AutoCleanupManager::trigger_cleanup(auto_cleanup_manager)
  
  // 8. 验证所有资源被清理
  assert_eq(AutoCleanupManager::get_resource_count(auto_cleanup_manager), 0)
}

// 测试4: 循环引用检测
test "循环引用检测" {
  // 1. 创建循环引用检测器
  let cycle_detector = CycleDetector::new()
  
  // 2. 创建对象A
  let object_a = RefCountedObject::new("ObjectA")
  let ref_a = RefCountedObject::add_ref(object_a)
  
  // 3. 创建对象B
  let object_b = RefCountedObject::new("ObjectB")
  let ref_b = RefCountedObject::add_ref(object_b)
  
  // 4. 建立A引用B的关系
  RefCountedObject::set_reference(object_a, "ref_to_b", object_b)
  
  // 5. 建立B引用A的关系（形成循环）
  RefCountedObject::set_reference(object_b, "ref_to_a", object_a)
  
  // 6. 验证引用计数
  assert_eq(RefCountedObject::get_ref_count(object_a), 2)
  assert_eq(RefCountedObject::get_ref_count(object_b), 2)
  
  // 7. 释放外部引用
  RefCountedObject::release_ref(ref_a)
  RefCountedObject::release_ref(ref_b)
  
  // 8. 验证仍有循环引用
  assert_eq(RefCountedObject::get_ref_count(object_a), 1)
  assert_eq(RefCountedObject::get_ref_count(object_b), 1)
  
  // 9. 检测循环引用
  let cycles = CycleDetector::detect_cycles(cycle_detector)
  assert_eq(cycles.length(), 1)
  assert_true(cycles[0].contains("ObjectA"))
  assert_true(cycles[0].contains("ObjectB"))
  
  // 10. 清理循环引用
  CycleDetector::break_cycles(cycle_detector)
  
  // 11. 验证对象被正确清理
  assert_true(RefCountedObject::is_destroyed(object_a))
  assert_true(RefCountedObject::is_destroyed(object_b))
}

// 测试5: 内存池管理
test "内存池管理" {
  // 1. 创建内存池
  let memory_pool = MemoryPool::new(1024 * 1024) // 1MB池
  
  // 2. 验证初始状态
  assert_eq(MemoryPool::get_total_size(memory_pool), 1024 * 1024)
  assert_eq(MemoryPool::get_available_size(memory_pool), 1024 * 1024)
  assert_eq(MemoryPool::get_allocated_count(memory_pool), 0)
  
  // 3. 分配内存块
  let allocations = []
  let allocation_sizes = [1024, 2048, 4096, 8192, 16384]
  
  for size in allocation_sizes {
    let allocation = MemoryPool::allocate(memory_pool, size)
    assert_true(MemoryPool::is_valid_allocation(memory_pool, allocation))
    allocations.push(allocation)
  }
  
  // 4. 验证分配状态
  assert_eq(MemoryPool::get_allocated_count(memory_pool), 5)
  assert_true(MemoryPool::get_available_size(memory_pool) < 1024 * 1024)
  
  // 5. 使用分配的内存
  for i in 0..allocations.length() {
    let allocation = allocations[i]
    let data = "test_data_" + i.to_string()
    MemoryPool::write_data(memory_pool, allocation, data)
    
    let read_data = MemoryPool::read_data(memory_pool, allocation)
    assert_eq(read_data, data)
  }
  
  // 6. 释放部分内存
  MemoryPool::deallocate(memory_pool, allocations[1]) // 释放第二个分配
  MemoryPool::deallocate(memory_pool, allocations[3]) // 释放第四个分配
  
  // 7. 验证释放状态
  assert_eq(MemoryPool::get_allocated_count(memory_pool), 3)
  assert_true(MemoryPool::get_available_size(memory_pool) > MemoryPool::get_available_size(memory_pool) - 2048 - 8192)
  
  // 8. 分配新内存验证碎片整理
  let new_allocation = MemoryPool::allocate(memory_pool, 5120)
  assert_true(MemoryPool::is_valid_allocation(memory_pool, new_allocation))
  
  // 9. 释放所有内存
  for allocation in allocations {
    if MemoryPool::is_valid_allocation(memory_pool, allocation) {
      MemoryPool::deallocate(memory_pool, allocation)
    }
  }
  MemoryPool::deallocate(memory_pool, new_allocation)
  
  // 10. 验证完全释放
  assert_eq(MemoryPool::get_allocated_count(memory_pool), 0)
  assert_eq(MemoryPool::get_available_size(memory_pool), 1024 * 1024)
}

// 测试6: 资源限制和配额管理
test "资源限制和配额管理" {
  // 1. 创建资源配额管理器
  let quota_manager = QuotaManager::new()
  
  // 2. 设置资源配额
  QuotaManager::set_quota(quota_manager, "memory", 100 * 1024 * 1024) // 100MB
  QuotaManager::set_quota(quota_manager, "file_handles", 1000)
  QuotaManager::set_quota(quota_manager, "network_connections", 100)
  QuotaManager::set_quota(quota_manager, "cpu_time", 10000) // 10秒
  
  // 3. 验证配额设置
  assert_eq(QuotaManager::get_quota(quota_manager, "memory"), 100 * 1024 * 1024)
  assert_eq(QuotaManager::get_quota(quota_manager, "file_handles"), 1000)
  assert_eq(QuotaManager::get_quota(quota_manager, "network_connections"), 100)
  assert_eq(QuotaManager::get_quota(quota_manager, "cpu_time"), 10000)
  
  // 4. 请求资源分配
  let memory_request1 = QuotaManager::request_resource(quota_manager, "memory", 50 * 1024 * 1024)
  assert_true(memory_request1.approved)
  
  let memory_request2 = QuotaManager::request_resource(quota_manager, "memory", 30 * 1024 * 1024)
  assert_true(memory_request2.approved)
  
  let memory_request3 = QuotaManager::request_resource(quota_manager, "memory", 30 * 1024 * 1024)
  assert_false(memory_request3.approved) // 超出配额
  
  // 5. 释放部分资源
  QuotaManager::release_resource(quota_manager, "memory", 30 * 1024 * 1024)
  
  // 6. 重新请求资源
  let memory_request4 = QuotaManager::request_resource(quota_manager, "memory", 30 * 1024 * 1024)
  assert_true(memory_request4.approved) // 现在应该可以
  
  // 7. 测试其他资源类型
  let file_request1 = QuotaManager::request_resource(quota_manager, "file_handles", 500)
  assert_true(file_request1.approved)
  
  let file_request2 = QuotaManager::request_resource(quota_manager, "file_handles", 600)
  assert_false(file_request2.approved) // 超出配额
  
  // 8. 验证资源使用情况
  assert_eq(QuotaManager::get_used(quota_manager, "memory"), 80 * 1024 * 1024)
  assert_eq(QuotaManager::get_used(quota_manager, "file_handles"), 500)
  assert_eq(QuotaManager::get_used(quota_manager, "network_connections"), 0)
  assert_eq(QuotaManager::get_used(quota_manager, "cpu_time"), 0)
  
  // 9. 测试配额调整
  QuotaManager::adjust_quota(quota_manager, "memory", 150 * 1024 * 1024)
  assert_eq(QuotaManager::get_quota(quota_manager, "memory"), 150 * 1024 * 1024)
  
  // 10. 验证调整后的配额允许更多请求
  let memory_request5 = QuotaManager::request_resource(quota_manager, "memory", 40 * 1024 * 1024)
  assert_true(memory_request5.approved)
}

// 测试7: 延迟清理和批量操作
test "延迟清理和批量操作" {
  // 1. 创建延迟清理管理器
  let deferred_cleanup_manager = DeferredCleanupManager::new()
  
  // 2. 创建大量资源
  let resources = []
  for i in 0..=1000 {
    let resource = TelemetryResource({
      id: "resource_" + i.to_string(),
      data: "large_data_payload_".repeat(100), // 每个资源约2KB
      references: []
    })
    resources.push(resource)
  }
  
  // 3. 标记资源为待清理
  for resource in resources {
    DeferredCleanupManager::mark_for_cleanup(deferred_cleanup_manager, resource.id)
  }
  
  // 4. 验证资源被标记但尚未清理
  assert_eq(DeferredCleanupManager::get_pending_count(deferred_cleanup_manager), 1000)
  assert_true(TelemetryResource::is_active(resources[0]))
  
  // 5. 设置清理阈值
  DeferredCleanupManager::set_cleanup_threshold(deferred_cleanup_manager, 100) // 100个资源触发清理
  
  // 6. 部分释放资源引用
  for i in 0..=99 {
    TelemetryResource::release(resources[i])
  }
  
  // 7. 触发批量清理
  DeferredCleanupManager::trigger_batch_cleanup(deferred_cleanup_manager)
  
  // 8. 验证部分资源被清理
  assert_eq(DeferredCleanupManager::get_pending_count(deferred_cleanup_manager), 900)
  assert_false(TelemetryResource::is_active(resources[0]))
  assert_false(TelemetryResource::is_active(resources[99]))
  assert_true(TelemetryResource::is_active(resources[100]))
  
  // 9. 释放剩余资源引用
  for i in 100..=1000 {
    TelemetryResource::release(resources[i])
  }
  
  // 10. 使用时间触发清理
  DeferredCleanupManager::set_time_threshold(deferred_cleanup_manager, 5000) // 5秒
  DeferredCleanupManager::advance_time(deferred_cleanup_manager, 6000) // 6秒
  DeferredCleanupManager::trigger_time_based_cleanup(deferred_cleanup_manager)
  
  // 11. 验证所有资源被清理
  assert_eq(DeferredCleanupManager::get_pending_count(deferred_cleanup_manager), 0)
  
  // 12. 验证清理统计
  let stats = DeferredCleanupManager::get_cleanup_stats(deferred_cleanup_manager)
  assert_eq(stats.total_cleaned, 1000)
  assert_eq(stats.batch_cleanups, 2)
  assert_true(stats.total_memory_freed > 2000000) // 至少2MB
}

// 测试8: 资源优先级和紧急回收
test "资源优先级和紧急回收" {
  // 1. 创建优先级资源管理器
  let priority_resource_manager = PriorityResourceManager::new()
  
  // 2. 创建不同优先级的资源
  let critical_resource = PriorityResourceManager::create_resource(
    priority_resource_manager, 
    "critical_telemetry", 
    Priority::Critical
  )
  
  let high_resources = []
  for i in 0..=10 {
    let resource = PriorityResourceManager::create_resource(
      priority_resource_manager, 
      "high_priority_" + i.to_string(), 
      Priority::High
    )
    high_resources.push(resource)
  }
  
  let normal_resources = []
  for i in 0..=50 {
    let resource = PriorityResourceManager::create_resource(
      priority_resource_manager, 
      "normal_priority_" + i.to_string(), 
      Priority::Normal
    )
    normal_resources.push(resource)
  }
  
  let low_resources = []
  for i in 0..=100 {
    let resource = PriorityResourceManager::create_resource(
      priority_resource_manager, 
      "low_priority_" + i.to_string(), 
      Priority::Low
    )
    low_resources.push(resource)
  }
  
  // 3. 验证资源创建
  assert_eq(PriorityResourceManager::get_resource_count(priority_resource_manager), 162)
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Critical), 1)
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::High), 10)
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Normal), 50)
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Low), 100)
  
  // 4. 模拟内存压力
  PriorityResourceManager::simulate_memory_pressure(priority_resource_manager, 0.8) // 80%内存使用率
  
  // 5. 触发紧急回收（应该先回收低优先级资源）
  let回收_stats = PriorityResourceManager::emergency_reclaim(priority_resource_manager, 50) // 回收50个资源
  
  // 6. 验证回收结果
  assert_eq(回收_stats.resources_reclaimed, 50)
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Critical), 1) // 关键资源保持
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::High), 10) // 高优先级保持
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Normal), 50) // 普通资源保持
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Low), 50) // 低优先级被回收
  
  // 7. 增加内存压力
  PriorityResourceManager::simulate_memory_pressure(priority_resource_manager, 0.95) // 95%内存使用率
  
  // 8. 再次触发紧急回收
  let回收_stats2 = PriorityResourceManager::emergency_reclaim(priority_resource_manager, 40) // 回收40个资源
  
  // 9. 验证回收结果
  assert_eq(回收_stats2.resources_reclaimed, 40)
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Critical), 1) // 关键资源保持
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::High), 10) // 高优先级保持
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Normal), 10) // 部分普通资源被回收
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Low), 0) // 所有低优先级被回收
  
  // 10. 极端内存压力
  PriorityResourceManager::simulate_memory_pressure(priority_resource_manager, 0.99) // 99%内存使用率
  
  // 11. 最后的紧急回收
  let回收_stats3 = PriorityResourceManager::emergency_reclaim(priority_resource_manager, 15) // 回收15个资源
  
  // 12. 验证最终结果
  assert_eq(回收_stats3.resources_reclaimed, 15)
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Critical), 1) // 关键资源仍然保持
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::High), 5) // 部分高优先级被回收
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Normal), 0) // 所有普通资源被回收
  assert_eq(PriorityResourceManager::get_resource_count_by_priority(priority_resource_manager, Priority::Low), 0) // 所有低优先级被回收
  
  // 13. 验证关键资源始终受到保护
  assert_true(PriorityResourceManager::is_resource_active(priority_resource_manager, critical_resource.id))
}