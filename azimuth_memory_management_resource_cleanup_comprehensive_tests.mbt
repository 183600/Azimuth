// Azimuth Memory Management and Resource Cleanup Comprehensive Test Suite
// 测试内存管理和资源清理功能，确保系统的内存使用效率和资源正确释放

test "内存分配和释放测试" {
  // 创建内存监控器
  let memory_monitor = MemoryMonitor::new()
  
  // 记录初始内存使用
  let initial_memory = MemoryMonitor::get_usage(memory_monitor)
  
  // 分配大量内存
  let large_data = []
  for i in 0..=10000 {
    large_data = large_data + ["data_" + i.to_string()]
  }
  
  // 记录峰值内存使用
  let peak_memory = MemoryMonitor::get_usage(memory_monitor)
  
  // 验证内存确实增加了
  assert_true(peak_memory > initial_memory)
  
  // 释放内存
  let large_data = []
  
  // 强制垃圾回收
  MemoryMonitor::force_gc(memory_monitor)
  
  // 记录释放后内存使用
  let cleanup_memory = MemoryMonitor::get_usage(memory_monitor)
  
  // 验证内存已释放（至少释放了80%）
  let memory_freed = peak_memory - cleanup_memory
  let memory_allocated = peak_memory - initial_memory
  assert_true(memory_freed > memory_allocated * 0.8)
}

test "对象生命周期管理测试" {
  // 创建对象生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 创建测试对象
  let test_objects = []
  for i in 0..=1000 {
    let obj = LifecycleManager::create_object(lifecycle_manager, "object_" + i.to_string())
    test_objects = test_objects + [obj]
  }
  
  // 验证对象已创建
  assert_eq(test_objects.length(), 1001)
  assert_eq(LifecycleManager::get_object_count(lifecycle_manager), 1001)
  
  // 销毁一半对象
  for i in 0..=500 {
    LifecycleManager::destroy_object(lifecycle_manager, test_objects[i])
  }
  
  // 验证对象已销毁
  assert_eq(LifecycleManager::get_object_count(lifecycle_manager), 500)
  
  // 销毁剩余对象
  for i in 501..=1000 {
    LifecycleManager::destroy_object(lifecycle_manager, test_objects[i])
  }
  
  // 验证所有对象已销毁
  assert_eq(LifecycleManager::get_object_count(lifecycle_manager), 0)
  
  // 强制垃圾回收
  LifecycleManager::force_gc(lifecycle_manager)
  
  // 验证内存已清理
  let memory_usage = LifecycleManager::get_memory_usage(lifecycle_manager)
  assert_true(memory_usage < 1024 * 1024) // 小于1MB
}

test "资源池管理测试" {
  // 创建资源池
  let resource_pool = ResourcePool::new(100) // 最大100个资源
  
  // 测试资源获取
  let resources = []
  for i in 0..=100 {
    let resource = ResourcePool::acquire(resource_pool)
    resources = resources + [resource]
  }
  
  // 验证所有资源已获取
  assert_eq(resources.length(), 101)
  assert_eq(ResourcePool::get_available_count(resource_pool), 0)
  
  // 测试资源释放
  for i in 0..=50 {
    ResourcePool::release(resource_pool, resources[i])
  }
  
  // 验证资源已释放
  assert_eq(ResourcePool::get_available_count(resource_pool), 51)
  
  // 测试资源重新获取
  let new_resources = []
  for i in 0..=51 {
    let resource = ResourcePool::acquire(resource_pool)
    new_resources = new_resources + [resource]
  }
  
  // 验证资源已重新获取
  assert_eq(new_resources.length(), 52)
  assert_eq(ResourcePool::get_available_count(resource_pool), 0)
  
  // 释放所有资源
  for resource in new_resources {
    ResourcePool::release(resource_pool, resource)
  }
  
  for i in 51..=100 {
    ResourcePool::release(resource_pool, resources[i])
  }
  
  // 验证所有资源已释放
  assert_eq(ResourcePool::get_available_count(resource_pool), 101)
}

test "内存泄漏检测测试" {
  // 创建内存泄漏检测器
  let leak_detector = LeakDetector::new()
  
  // 开始检测
  LeakDetector::start_detection(leak_detector)
  
  // 创建可能导致泄漏的代码
  let leaky_objects = []
  for i in 0..=1000 {
    let obj = {
      "id": i,
      "data": "leaky_data_" + i.to_string(),
      "children": []
    }
    leaky_objects = leaky_objects + [obj]
  }
  
  // 模拟泄漏：不释放对象引用
  let leaked_objects = leaky_objects.slice(0, 500) // 保留引用，模拟泄漏
  
  // 停止检测
  LeakDetector::stop_detection(leak_detector)
  
  // 获取泄漏报告
  let leak_report = LeakDetector::get_report(leak_detector)
  
  // 验证检测到泄漏
  assert_true(leak_report.detected_leaks)
  assert_true(leak_report.leaked_objects > 0)
  assert_true(leak_report.leaked_memory > 0)
  
  // 修复泄漏
  let leaked_objects = [] // 释放引用
  
  // 强制垃圾回收
  LeakDetector::force_gc(leak_detector)
  
  // 再次检测
  LeakDetector::start_detection(leak_detector)
  LeakDetector::stop_detection(leak_detector)
  
  // 获取修复后的报告
  let fixed_report = LeakDetector::get_report(leak_detector)
  
  // 验证泄漏已修复
  assert_false(fixed_report.detected_leaks)
  assert_eq(fixed_report.leaked_objects, 0)
}

test "缓存内存管理测试" {
  // 创建内存感知缓存
  let memory_cache = MemoryAwareCache::new(10 * 1024 * 1024) // 10MB限制
  
  // 添加大量数据到缓存
  for i in 0..=1000 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string() * 100 // 每个值约600字节
    MemoryAwareCache::set(memory_cache, key, value)
  }
  
  // 验证缓存大小不超过限制
  let cache_size = MemoryAwareCache::get_memory_usage(memory_cache)
  assert_true(cache_size <= 10 * 1024 * 1024)
  
  // 验证缓存项数量可能小于添加的数量（由于内存限制）
  let cache_count = MemoryAwareCache::size(memory_cache)
  assert_true(cache_count <= 1000)
  
  // 测试LRU淘汰
  let first_key = "key_0"
  let first_value = MemoryAwareCache::get(memory_cache, first_key)
  
  // 如果缓存已满，第一个项可能已被淘汰
  if cache_count < 1000 {
    match first_value {
      Some(_) => assert_true(false) // 应该被淘汰
      None => assert_true(true) // 正确被淘汰
    }
  }
  
  // 测试内存压力下的自动清理
  let large_value = "x" * 5 * 1024 * 1024 // 5MB
  MemoryAwareCache::set(memory_cache, "large_key", large_value)
  
  // 验证缓存大小仍然不超过限制
  let cache_size_after_large = MemoryAwareCache::get_memory_usage(memory_cache)
  assert_true(cache_size_after_large <= 10 * 1024 * 1024)
  
  // 验证大值已被添加（可能淘汰了其他项）
  let large_value_retrieved = MemoryAwareCache::get(memory_cache, "large_key")
  match large_value_retrieved {
    Some(value) => assert_eq(value.length(), 5 * 1024 * 1024)
    None => assert_true(false)
  }
}

test "循环引用内存管理测试" {
  // 创建循环引用检测器
  let circular_ref_detector = CircularRefDetector::new()
  
  // 创建具有循环引用的对象
  let obj_a = CircularRefDetector::create_object(circular_ref_detector, "A")
  let obj_b = CircularRefDetector::create_object(circular_ref_detector, "B")
  let obj_c = CircularRefDetector::create_object(circular_ref_detector, "C")
  
  // 创建循环引用：A -> B -> C -> A
  CircularRefDetector::add_reference(circular_ref_detector, obj_a, obj_b)
  CircularRefDetector::add_reference(circular_ref_detector, obj_b, obj_c)
  CircularRefDetector::add_reference(circular_ref_detector, obj_c, obj_a)
  
  // 检测循环引用
  let circular_refs = CircularRefDetector::detect_circular_references(circular_ref_detector)
  
  // 验证检测到循环引用
  assert_eq(circular_refs.length(), 1)
  assert_eq(circular_refs[0].length(), 3) // A -> B -> C -> A
  
  // 测试循环引用的内存清理
  CircularRefDetector::break_circular_references(circular_ref_detector)
  
  // 验证循环引用已断开
  let circular_refs_after = CircularRefDetector::detect_circular_references(circular_ref_detector)
  assert_eq(circular_refs_after.length(), 0)
  
  // 验证对象可以正常销毁
  CircularRefDetector::destroy_object(circular_ref_detector, obj_a)
  CircularRefDetector::destroy_object(circular_ref_detector, obj_b)
  CircularRefDetector::destroy_object(circular_ref_detector, obj_c)
  
  // 验证所有对象已销毁
  assert_eq(CircularRefDetector::get_object_count(circular_ref_detector), 0)
}

test "大对象内存管理测试" {
  // 创建大对象管理器
  let large_object_manager = LargeObjectManager::new()
  
  // 创建大对象（每个约1MB）
  let large_objects = []
  for i in 0..=10 {
    let large_data = "x" * 1024 * 1024 // 1MB
    let large_obj = LargeObjectManager::create_object(large_object_manager, "large_obj_" + i.to_string(), large_data)
    large_objects = large_objects + [large_obj]
  }
  
  // 验证大对象已创建
  assert_eq(large_objects.length(), 11)
  assert_eq(LargeObjectManager::get_object_count(large_object_manager), 11)
  
  // 验证内存使用
  let memory_usage = LargeObjectManager::get_memory_usage(large_object_manager)
  assert_true(memory_usage >= 11 * 1024 * 1024) // 至少11MB
  
  // 测试大对象压缩
  for obj in large_objects {
    LargeObjectManager::compress_object(large_object_manager, obj)
  }
  
  // 验证压缩后内存减少
  let compressed_memory = LargeObjectManager::get_memory_usage(large_object_manager)
  assert_true(compressed_memory < memory_usage)
  
  // 测试大对象流式处理
  for obj in large_objects {
    let stream = LargeObjectManager::create_stream(large_object_manager, obj)
    let processed_data = LargeObjectManager::process_stream(large_object_manager, stream, |data| {
      data.length() // 简单处理：返回数据长度
    })
    assert_true(processed_data > 0)
  }
  
  // 销毁大对象
  for obj in large_objects {
    LargeObjectManager::destroy_object(large_object_manager, obj)
  }
  
  // 验证大对象已销毁
  assert_eq(LargeObjectManager::get_object_count(large_object_manager), 0)
  
  // 验证内存已释放
  let final_memory = LargeObjectManager::get_memory_usage(large_object_manager)
  assert_true(final_memory < 1024 * 1024) // 小于1MB
}

test "内存压力测试" {
  // 创建内存压力测试器
  let pressure_tester = MemoryPressureTester::new()
  
  // 设置内存限制（50MB）
  MemoryPressureTester::set_memory_limit(pressure_tester, 50 * 1024 * 1024)
  
  // 开始压力测试
  MemoryPressureTester::start_test(pressure_tester)
  
  // 逐步增加内存使用
  let memory_blocks = []
  let block_size = 1024 * 1024 // 1MB
  let mut blocks_added = 0
  
  while MemoryPressureTester::get_memory_usage(pressure_tester) < 45 * 1024 * 1024 {
    let block = "x" * block_size
    memory_blocks = memory_blocks + [block]
    blocks_added = blocks_added + 1
    
    // 检查内存压力
    if MemoryPressureTester::is_under_pressure(pressure_tester) {
      // 执行内存清理
      MemoryPressureTester::trigger_cleanup(pressure_tester)
    }
  }
  
  // 验证添加的块数量
  assert_true(blocks_added >= 40) // 至少添加了40MB的数据
  
  // 测试内存压力下的操作
  let pressure_operations = []
  for i in 0..=1000 {
    let operation = || {
      // 模拟内存密集操作
      let temp_data = "y" * 10240 // 10KB
      temp_data.length()
    }
    pressure_operations = pressure_operations + [operation]
  }
  
  // 在内存压力下执行操作
  let results = MemoryPressureTester::execute_under_pressure(pressure_tester, pressure_operations)
  
  // 验证操作成功
  assert_eq(results.length(), 1001)
  for result in results {
    assert_eq(result, 10240)
  }
  
  // 停止压力测试
  MemoryPressureTester::stop_test(pressure_tester)
  
  // 获取压力测试报告
  let pressure_report = MemoryPressureTester::get_report(pressure_tester)
  
  // 验证压力测试报告
  assert_true(pressure_report.max_memory_usage > 40 * 1024 * 1024)
  assert_true(pressure_report.cleanup_triggered)
  assert_true(pressure_report.operations_executed >= 1000)
}

test "内存碎片整理测试" {
  // 创建内存碎片整理器
  defragmenter = MemoryDefragmenter::new()
  
  // 创建大量小对象（可能导致内存碎片）
  let small_objects = []
  for i in 0..=10000 {
    let small_obj = "small_data_" + i.to_string()
    small_objects = small_objects + [small_obj]
  }
  
  // 测量碎片化前的内存状态
  let before_fragmentation = MemoryDefragmenter::analyze_memory(defragmenter)
  
  // 删除一半对象（创建内存碎片）
  for i in 0..=5000 {
    small_objects[i] = "" // 清空对象内容
  }
  
  // 测量碎片化后的内存状态
  let after_fragmentation = MemoryDefragmenter::analyze_memory(defragmenter)
  
  // 验证内存碎片增加
  assert_true(after_fragmentation.fragmentation_ratio > before_fragmentation.fragmentation_ratio)
  
  // 执行内存碎片整理
  MemoryDefragmenter::defragment(defragmenter)
  
  // 测量整理后的内存状态
  let after_defragmentation = MemoryDefragmenter::analyze_memory(defragmenter)
  
  // 验证内存碎片减少
  assert_true(after_defragmentation.fragmentation_ratio < after_fragmentation.fragmentation_ratio)
  
  // 清理剩余对象
  for i in 5001..=10000 {
    small_objects[i] = ""
  }
  
  // 强制垃圾回收
  MemoryDefragmenter::force_gc(defragmenter)
  
  // 验证内存已清理
  let final_memory = MemoryDefragmenter::get_memory_usage(defragmenter)
  assert_true(final_memory < 10 * 1024 * 1024) // 小于10MB
}

test "资源清理策略测试" {
  // 创建资源清理策略管理器
  let cleanup_manager = CleanupStrategyManager::new()
  
  // 设置不同的清理策略
  CleanupStrategyManager::set_strategy(cleanup_manager, "aggressive", CleanupStrategy::Aggressive)
  CleanupStrategyManager::set_strategy(cleanup_manager, "moderate", CleanupStrategy::Moderate)
  CleanupStrategyManager::set_strategy(cleanup_manager, "conservative", CleanupStrategy::Conservative)
  
  // 创建测试资源
  let resources = []
  for i in 0..=1000 {
    let resource = {
      "id": i,
      "type": "test_resource",
      "data": "resource_data_" + i.to_string(),
      "last_accessed": Time::now()
    }
    resources = resources + [resource]
  }
  
  // 测试激进清理策略
  CleanupStrategyManager::apply_strategy(cleanup_manager, "aggressive", resources)
  let aggressive_count = CleanupStrategyManager::get_resource_count(cleanup_manager)
  assert_true(aggressive_count < 500) // 激进策略应清理大部分资源
  
  // 重新创建资源
  resources = []
  for i in 0..=1000 {
    let resource = {
      "id": i,
      "type": "test_resource",
      "data": "resource_data_" + i.to_string(),
      "last_accessed": Time::now()
    }
    resources = resources + [resource]
  }
  
  // 测试温和清理策略
  CleanupStrategyManager::apply_strategy(cleanup_manager, "moderate", resources)
  let moderate_count = CleanupStrategyManager::get_resource_count(cleanup_manager)
  assert_true(moderate_count >= 500 && moderate_count <= 800) // 温和策略应清理部分资源
  
  // 重新创建资源
  resources = []
  for i in 0..=1000 {
    let resource = {
      "id": i,
      "type": "test_resource",
      "data": "resource_data_" + i.to_string(),
      "last_accessed": Time::now()
    }
    resources = resources + [resource]
  }
  
  // 测试保守清理策略
  CleanupStrategyManager::apply_strategy(cleanup_manager, "conservative", resources)
  let conservative_count = CleanupStrategyManager::get_resource_count(cleanup_manager)
  assert_true(conservative_count > 800) // 保守策略应清理少量资源
  
  // 测试自动清理触发
  CleanupStrategyManager::set_auto_cleanup_threshold(cleanup_manager, 500) // 当资源数超过500时触发清理
  
  // 创建超过阈值数量的资源
  resources = []
  for i in 0..=1000 {
    let resource = {
      "id": i,
      "type": "test_resource",
      "data": "resource_data_" + i.to_string(),
      "last_accessed": Time::now()
    }
    resources = resources + [resource]
  }
  
  // 验证自动清理已触发
  let auto_cleanup_count = CleanupStrategyManager::get_resource_count(cleanup_manager)
  assert_true(auto_cleanup_count <= 500) // 应该被清理到阈值以下
  
  // 清理所有资源
  CleanupStrategyManager::cleanup_all(cleanup_manager)
  
  // 验证所有资源已清理
  let final_count = CleanupStrategyManager::get_resource_count(cleanup_manager)
  assert_eq(final_count, 0)
}