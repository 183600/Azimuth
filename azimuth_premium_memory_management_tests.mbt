// Azimuth 高级内存管理和资源清理测试用例
// 专注于内存泄漏检测、资源生命周期管理和自动清理机制

// 测试1: 自动内存管理验证
test "自动内存管理验证测试" {
  let memory_tracker = MemoryTracker::new()
  let initial_memory = MemoryTracker::get_allocated_memory(memory_tracker)
  
  // 创建大量对象
  let objects = []
  for i in 0..10000 {
    let obj = LargeObject::new(1024) // 1KB对象
    objects.push(obj)
  }
  
  let peak_memory = MemoryTracker::get_allocated_memory(memory_tracker)
  assert_true(peak_memory > initial_memory + 10000 * 1024)
  
  // 清理对象引用
  objects.clear()
  
  // 触发垃圾回收
  GarbageCollector::collect()
  
  let final_memory = MemoryTracker::get_allocated_memory(memory_tracker)
  let memory_recovered = peak_memory - final_memory
  
  // 验证大部分内存被回收
  assert_true(memory_recovered > 10000 * 1024 * 0.8)
}

// 测试2: 资源生命周期管理
test "资源生命周期管理测试" {
  let resource_manager = ResourceManager::new()
  let resource_ids = []
  
  // 创建多个资源
  for i in 0..100 {
    let resource_id = ResourceManager::create_resource(resource_manager, "test_resource_" + i.to_string())
    resource_ids.push(resource_id)
    
    // 验证资源已创建
    assert_true(ResourceManager::resource_exists(resource_manager, resource_id))
  }
  
  // 验证所有资源都存在
  assert_eq(ResourceManager::get_resource_count(resource_manager), 100)
  
  // 删除一半资源
  for i in 0..50 {
    ResourceManager::delete_resource(resource_manager, resource_ids[i])
    assert_false(ResourceManager::resource_exists(resource_manager, resource_ids[i]))
  }
  
  // 验证资源计数
  assert_eq(ResourceManager::get_resource_count(resource_manager), 50)
  
  // 清理剩余资源
  ResourceManager::cleanup_all_resources(resource_manager)
  
  // 验证所有资源被清理
  assert_eq(ResourceManager::get_resource_count(resource_manager), 0)
}

// 测试3: 内存泄漏检测
test "内存泄漏检测测试" {
  let leak_detector = MemoryLeakDetector::new()
  LeakDetector::start_monitoring(leak_detector)
  
  // 模拟内存泄漏场景
  let leaked_objects = []
  for i in 0..1000 {
    let obj = LeakyObject::new(512) // 512B对象
    leaked_objects.push(obj)
  }
  
  // 故意不清理一些对象
  let intentionally_leaked = leaked_objects.slice(0, 100)
  let properly_managed = leaked_objects.slice(100, 1000)
  
  // 清理正确管理的对象
  properly_managed.clear()
  
  // 触发垃圾回收
  GarbageCollector::collect()
  
  // 检测内存泄漏
  let leak_report = LeakDetector::generate_report(leak_detector)
  let leaked_memory = LeakReport::get_leaked_memory(leak_report)
  
  // 验证检测到预期的内存泄漏
  assert_true(leaked_memory >= 100 * 512 * 0.8)
  
  LeakDetector::stop_monitoring(leak_detector)
}

// 测试4: 弱引用和循环引用处理
test "弱引用和循环引用处理测试" {
  let weak_ref_tracker = WeakReferenceTracker::new()
  
  // 创建循环引用对象
  let obj_a = CyclicObject::new("A")
  let obj_b = CyclicObject::new("B")
  
  // 建立循环引用
  CyclicObject::set_reference(obj_a, obj_b)
  CyclicObject::set_reference(obj_b, obj_a)
  
  // 创建弱引用
  let weak_a = WeakReference::new(obj_a)
  let weak_b = WeakReference::new(obj_b)
  
  // 验证弱引用有效
  assert_true(WeakReference::is_valid(weak_a))
  assert_true(WeakReference::is_valid(weak_b))
  
  // 清除强引用
  obj_a = None
  obj_b = None
  
  // 触发垃圾回收
  GarbageCollector::collect()
  
  // 验证循环引用被正确回收
  assert_false(WeakReference::is_valid(weak_a))
  assert_false(WeakReference::is_valid(weak_b))
}

// 测试5: 大对象内存管理
test "大对象内存管理测试" {
  let large_object_manager = LargeObjectManager::new()
  let object_sizes = [1024 * 1024, 2 * 1024 * 1024, 5 * 1024 * 1024] // 1MB, 2MB, 5MB
  let object_handles = []
  
  // 创建大对象
  for size in object_sizes {
    let handle = LargeObjectManager::allocate(large_object_manager, size)
    object_handles.push(handle)
    
    // 验证对象已分配
    assert_true(LargeObjectManager::is_allocated(large_object_manager, handle))
    
    // 填充数据
    let data = LargeObjectManager::get_data(large_object_manager, handle)
    for i in 0..size {
      data[i] = (i % 256).to_byte()
    }
  }
  
  // 验证内存使用
  let total_memory = LargeObjectManager::get_total_memory_usage(large_object_manager)
  let expected_memory = 1024 * 1024 + 2 * 1024 * 1024 + 5 * 1024 * 1024
  assert_true(total_memory >= expected_memory * 0.9)
  
  // 释放大对象
  for handle in object_handles {
    LargeObjectManager::deallocate(large_object_manager, handle)
    assert_false(LargeObjectManager::is_allocated(large_object_manager, handle))
  }
  
  // 验证内存被释放
  let final_memory = LargeObjectManager::get_total_memory_usage(large_object_manager)
  assert_true(final_memory < expected_memory * 0.1)
}

// 测试6: 内存池管理
test "内存池管理测试" {
  let memory_pool = MemoryPool::new(1024, 100) // 1KB块，100个块
  let allocated_blocks = []
  
  // 分配所有内存块
  for i in 0..100 {
    let block = MemoryPool::allocate(memory_pool)
    allocated_blocks.push(block)
    assert_true(MemoryPool::is_valid_block(memory_pool, block))
  }
  
  // 验证内存池已满
  assert_true(MemoryPool::is_full(memory_pool))
  
  // 尝试分配更多块（应该失败）
  let extra_block = MemoryPool::try_allocate(memory_pool)
  assert_eq(extra_block, None)
  
  // 释放一半块
  for i in 0..50 {
    MemoryPool::deallocate(memory_pool, allocated_blocks[i])
  }
  
  // 验证内存池有可用空间
  assert_false(MemoryPool::is_full(memory_pool))
  assert_eq(MemoryPool::get_available_count(memory_pool), 50)
  
  // 重新分配块
  for i in 0..50 {
    let block = MemoryPool::allocate(memory_pool)
    assert_true(MemoryPool::is_valid_block(memory_pool, block))
  }
  
  // 清理所有块
  for block in allocated_blocks {
    MemoryPool::deallocate(memory_pool, block)
  }
  
  // 验证所有块都可用
  assert_eq(MemoryPool::get_available_count(memory_pool), 100)
}

// 测试7: 智能指针资源管理
test "智能指针资源管理测试" {
  let resource_registry = ResourceRegistry::new()
  let smart_pointers = []
  
  // 创建智能指针管理的资源
  for i in 0..50 {
    let resource = ExpensiveResource::new("resource_" + i.to_string())
    let smart_ptr = SmartPointer::new(resource, resource_registry)
    smart_pointers.push(smart_ptr)
    
    // 验证资源被注册
    assert_true(ResourceRegistry::is_resource_registered(resource_registry, resource))
  }
  
  // 验证引用计数
  for ptr in smart_pointers {
    assert_eq(SmartPointer::get_reference_count(ptr), 1)
  }
  
  // 创建一些共享引用
  let shared_pointers = []
  for i in 0..25 {
    let shared_ptr = SmartPointer::clone(smart_pointers[i])
    shared_pointers.push(shared_ptr)
    assert_eq(SmartPointer::get_reference_count(smart_pointers[i]), 2)
  }
  
  // 清理共享指针
  shared_pointers.clear()
  
  // 验证引用计数减少
  for i in 0..25 {
    assert_eq(SmartPointer::get_reference_count(smart_pointers[i]), 1)
  }
  
  // 清理所有智能指针
  smart_pointers.clear()
  
  // 触发资源清理
  ResourceRegistry::cleanup_unreferenced_resources(resource_registry)
  
  // 验证所有资源被清理
  assert_eq(ResourceRegistry::get_registered_count(resource_registry), 0)
}

// 测试8: 缓冲区管理器测试
test "缓冲区管理器测试" {
  let buffer_manager = BufferManager::new()
  let buffer_handles = []
  
  // 分配不同大小的缓冲区
  let buffer_sizes = [256, 512, 1024, 2048, 4096]
  for size in buffer_sizes {
    for i in 0..10 {
      let handle = BufferManager::allocate_buffer(buffer_manager, size)
      buffer_handles.push(handle)
      
      // 验证缓冲区有效
      assert_true(BufferManager::is_buffer_valid(buffer_manager, handle))
      assert_eq(BufferManager::get_buffer_size(buffer_manager, handle), size)
      
      // 写入测试数据
      let buffer = BufferManager::get_buffer(buffer_manager, handle)
      for j in 0..size {
        buffer[j] = ((i + j) % 256).to_byte()
      }
    }
  }
  
  // 验证总内存使用
  let total_memory = BufferManager::get_total_memory_usage(buffer_manager)
  let expected_memory = (256 + 512 + 1024 + 2048 + 4096) * 10
  assert_true(total_memory >= expected_memory * 0.9)
  
  // 释放所有缓冲区
  for handle in buffer_handles {
    BufferManager::deallocate_buffer(buffer_manager, handle)
    assert_false(BufferManager::is_buffer_valid(buffer_manager, handle))
  }
  
  // 验证内存被释放
  let final_memory = BufferManager::get_total_memory_usage(buffer_manager)
  assert_true(final_memory < expected_memory * 0.1)
}

// 测试9: 延迟清理机制测试
test "延迟清理机制测试" {
  let deferred_cleanup_manager = DeferredCleanupManager::new()
  let cleanup_resources = []
  
  // 创建需要延迟清理的资源
  for i in 0..20 {
    let resource = CleanupResource::new("deferred_" + i.to_string())
    let cleanup_handle = DeferredCleanupManager::register_resource(
      deferred_cleanup_manager, 
      resource, 
      CleanupPriority::Low
    )
    cleanup_resources.push(cleanup_handle)
  }
  
  // 创建高优先级清理资源
  for i in 0..10 {
    let resource = CleanupResource::new("urgent_" + i.to_string())
    let cleanup_handle = DeferredCleanupManager::register_resource(
      deferred_cleanup_manager, 
      resource, 
      CleanupPriority::High
    )
    cleanup_resources.push(cleanup_handle)
  }
  
  // 验证资源被注册
  assert_eq(DeferredCleanupManager::get_pending_count(deferred_cleanup_manager), 30)
  
  // 触发高优先级清理
  DeferredCleanupManager::cleanup_by_priority(deferred_cleanup_manager, CleanupPriority::High)
  
  // 验证高优先级资源被清理
  assert_eq(DeferredCleanupManager::get_pending_count(deferred_cleanup_manager), 20)
  
  // 触发所有清理
  DeferredCleanupManager::cleanup_all(deferred_cleanup_manager)
  
  // 验证所有资源被清理
  assert_eq(DeferredCleanupManager::get_pending_count(deferred_cleanup_manager), 0)
}

// 测试10: 内存压力测试
test "内存压力测试" {
  let stress_test_manager = MemoryStressTestManager::new()
  let memory_limit = 100 * 1024 * 1024 // 100MB
  StressTestManager::set_memory_limit(stress_test_manager, memory_limit)
  
  let allocation_sizes = [1024, 4096, 16384, 65536] // 1KB, 4KB, 16KB, 64KB
  let allocated_objects = []
  let total_allocated = 0
  
  // 尝试分配直到接近内存限制
  for size in allocation_sizes {
    let mut continue_allocation = true
    while continue_allocation {
      let obj = StressTestManager::try_allocate(stress_test_manager, size)
      match obj {
        Some(object) => {
          allocated_objects.push(object)
          total_allocated = total_allocated + size
          
          // 检查是否接近内存限制
          if total_allocated > memory_limit * 0.9 {
            continue_allocation = false
          }
        }
        None => {
          continue_allocation = false
        }
      }
    }
  }
  
  // 验证内存使用在限制内
  let actual_memory_usage = StressTestManager::get_memory_usage(stress_test_manager)
  assert_true(actual_memory_usage <= memory_limit)
  
  // 随机释放一些对象
  let mut freed_count = 0
  for i in 0..allocated_objects.length() {
    if i % 3 == 0 {
      StressTestManager::deallocate(stress_test_manager, allocated_objects[i])
      freed_count = freed_count + 1
    }
  }
  
  // 验证内存减少
  let memory_after_cleanup = StressTestManager::get_memory_usage(stress_test_manager)
  assert_true(memory_after_cleanup < actual_memory_usage)
  
  // 清理所有对象
  for obj in allocated_objects {
    StressTestManager::deallocate(stress_test_manager, obj)
  }
  
  // 验证所有内存被释放
  let final_memory_usage = StressTestManager::get_memory_usage(stress_test_manager)
  assert_true(final_memory_usage < memory_limit * 0.1)
  
  // 生成压力测试报告
  let report = StressTestManager::generate_report(stress_test_manager)
  assert_true(StressTestReport::get_peak_memory(report) > 0)
  assert_true(StressTestReport::get_allocation_count(report) > 0)
  assert_true(StressTestReport::get_deallocation_count(report) > 0)
}