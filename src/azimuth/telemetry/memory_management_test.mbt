// 高级内存管理测试用例
// 测试内存泄漏检测、大对象生命周期管理、内存池优化等高级内存功能

test "memory_leak_detection_under_stress" {
  // 测试压力下的内存泄漏检测
  
  struct MemoryLeakDetector {
    allocated_objects: Array<(String, Int64, Int)>  // (object_id, timestamp, size_bytes)
    freed_objects: Array<String>                   // object_id
    total_allocated: Int64
    total_freed: Int64
    leak_threshold_bytes: Int64
    monitoring_window_ms: Int64
  }
  
  struct ObjectInfo {
    id: String
    size_bytes: Int
    allocation_time: Int64
    expected_lifetime_ms: Int64
    object_type: String
  }
  
  // 创建内存泄漏检测器
  let leak_detector = MemoryLeakDetector{
    allocated_objects: Array::empty(),
    freed_objects: Array::empty(),
    total_allocated: 0L,
    total_freed: 0L,
    leak_threshold_bytes: 10 * 1024 * 1024,  // 10MB泄漏阈值
    monitoring_window_ms: 60000               // 1分钟监控窗口
  }
  
  // 模拟对象分配
  let allocate_object = fn(
    detector: MemoryLeakDetector, 
    obj_info: ObjectInfo, 
    current_time: Int64
  ) -> MemoryLeakDetector {
    let mut new_allocated = detector.allocated_objects.to_array()
    new_allocated.push((obj_info.id, current_time, obj_info.size_bytes))
    
    { detector |
      allocated_objects: new_allocated,
      total_allocated: detector.total_allocated + obj_info.size_bytes.to_int64()
    }
  }
  
  // 模拟对象释放
  let free_object = fn(detector: MemoryLeakDetector, object_id: String) -> MemoryLeakDetector {
    let mut new_allocated = Array::empty<(String, Int64, Int)>()
    let mut freed_size = 0
    
    // 从分配列表中移除
    let mut i = 0
    while i < detector.allocated_objects.length() {
      let (id, timestamp, size) = detector.allocated_objects[i]
      if id != object_id {
        new_allocated.push((id, timestamp, size))
      } else {
        freed_size = size
      }
      i = i + 1
    }
    
    let mut new_freed = detector.freed_objects.to_array()
    new_freed.push(object_id)
    
    { detector |
      allocated_objects: new_allocated,
      freed_objects: new_freed,
      total_freed: detector.total_freed + freed_size.to_int64()
    }
  }
  
  // 检测内存泄漏
  let detect_leaks = fn(detector: MemoryLeakDetector, current_time: Int64) -> (Int64, Array<String>) {
    let mut leaked_bytes = 0L
    let mut leaked_objects = Array::empty<String>()
    
    let mut i = 0
    while i < detector.allocated_objects.length() {
      let (id, alloc_time, size) = detector.allocated_objects[i]
      
      // 检查对象是否超过预期生命周期
      let age = current_time - alloc_time
      if age > detector.monitoring_window_ms {
        leaked_bytes = leaked_bytes + size.to_int64()
        leaked_objects.push(id)
      }
      
      i = i + 1
    }
    
    (leaked_bytes, leaked_objects)
  }
  
  // 创建测试对象
  let current_time = 1640995200000000L
  
  let test_objects = [
    ObjectInfo{
      id: "span-1",
      size_bytes: 1024,
      allocation_time: current_time,
      expected_lifetime_ms: 5000,
      object_type: "span"
    },
    ObjectInfo{
      id: "metric-1", 
      size_bytes: 512,
      allocation_time: current_time + 1000L,
      expected_lifetime_ms: 10000,
      object_type: "metric"
    },
    ObjectInfo{
      id: "log-1",
      size_bytes: 2048,
      allocation_time: current_time + 2000L,
      expected_lifetime_ms: 3000,
      object_type: "log"
    },
    ObjectInfo{
      id: "large-batch-1",
      size_bytes: 1024 * 1024,  // 1MB
      allocation_time: current_time + 3000L,
      expected_lifetime_ms: 2000,
      object_type: "batch"
    }
  ]
  
  // 分配对象
  let mut detector_with_objects = leak_detector
  let mut i = 0
  while i < test_objects.length() {
    detector_with_objects = allocate_object(detector_with_objects, test_objects[i], current_time)
    i = i + 1
  }
  
  assert_eq(detector_with_objects.allocated_objects.length(), 4)
  assert_eq(detector_with_objects.total_allocated, 1024 + 512 + 2048 + 1024 * 1024)
  
  // 释放部分对象
  detector_with_objects = free_object(detector_with_objects, "span-1")
  detector_with_objects = free_object(detector_with_objects, "metric-1")
  
  assert_eq(detector_with_objects.allocated_objects.length(), 2)
  assert_eq(detector_with_objects.freed_objects.length(), 2)
  
  // 检测泄漏（在监控窗口内，不应该有泄漏）
  let (leaked_bytes_early, leaked_objects_early) = detect_leaks(detector_with_objects, current_time + 30000L)
  assert_eq(leaked_bytes_early, 0L)
  assert_eq(leaked_objects_early.length(), 0)
  
  // 检测泄漏（超过监控窗口，应该检测到泄漏）
  let (leaked_bytes_late, leaked_objects_late) = detect_leaks(detector_with_objects, current_time + 90000L)
  assert_eq(leaked_bytes_late > 0L, true)
  assert_eq(leaked_objects_late.length(), 2)
  assert_eq(leaked_bytes_late, 2048 + 1024 * 1024)  // log-1 + large-batch-1
  
  // 验证泄漏超过阈值
  assert_eq(leaked_bytes_late > leak_detector.leak_threshold_bytes, false)  // 应该不超过阈值
}

test "large_object_lifecycle_management" {
  // 测试大对象生命周期管理
  
  enum ObjectSize {
    Small(Int)      // < 1KB
    Medium(Int)     // 1KB - 100KB
    Large(Int)      // 100KB - 1MB
    XLarge(Int)     // > 1MB
  }
  
  struct LargeObject {
    id: String
    size_category: ObjectSize,
    actual_size_bytes: Int,
    creation_time: Int64,
    access_count: Int,
    last_access_time: Int64,
    is_pinned: Bool
  }
  
  struct ObjectLifecycleManager {
    objects: Array<LargeObject>
    max_memory_mb: Int
    current_memory_mb: Int
    gc_threshold_percentage: Float
    access_history: Array<(String, Int64)>  // (object_id, access_time)
  }
  
  // 创建大对象生命周期管理器
  let lifecycle_manager = ObjectLifecycleManager{
    objects: Array::empty(),
    max_memory_mb: 100,
    current_memory_mb: 0,
    gc_threshold_percentage: 0.8,
    access_history: Array::empty()
  }
  
  // 创建大对象
  let create_large_object = fn(
    manager: ObjectLifecycleManager, 
    id: String, 
    size_bytes: Int, 
    current_time: Int64
  ) -> ObjectLifecycleManager {
    let size_category = if size_bytes < 1024 {
      Small(size_bytes)
    } else if size_bytes < 100 * 1024 {
      Medium(size_bytes)
    } else if size_bytes < 1024 * 1024 {
      Large(size_bytes)
    } else {
      XLarge(size_bytes)
    }
    
    let new_object = LargeObject{
      id: id,
      size_category: size_category,
      actual_size_bytes: size_bytes,
      creation_time: current_time,
      access_count: 0,
      last_access_time: current_time,
      is_pinned: false
    }
    
    let new_memory_mb = manager.current_memory_mb + (size_bytes / (1024 * 1024))
    let mut new_objects = manager.objects.to_array()
    new_objects.push(new_object)
    
    { manager |
      objects: new_objects,
      current_memory_mb: new_memory_mb
    }
  }
  
  // 访问对象
  let access_object = fn(manager: ObjectLifecycleManager, object_id: String, current_time: Int64) -> ObjectLifecycleManager {
    let mut updated_objects = Array::empty<LargeObject>()
    let mut found = false
    
    let mut i = 0
    while i < manager.objects.length() {
      let obj = manager.objects[i]
      if obj.id == object_id {
        found = true
        let updated_obj = { obj |
          access_count: obj.access_count + 1,
          last_access_time: current_time
        }
        updated_objects.push(updated_obj)
      } else {
        updated_objects.push(obj)
      }
      i = i + 1
    }
    
    if found {
      let mut new_access_history = manager.access_history.to_array()
      new_access_history.push((object_id, current_time))
      
      { manager |
        objects: updated_objects,
        access_history: new_access_history
      }
    } else {
      manager
    }
  }
  
  // 垃圾回收策略
  let perform_gc = fn(manager: ObjectLifecycleManager, current_time: Int64) -> ObjectLifecycleManager {
    let memory_threshold = manager.max_memory_mb.to_float() * manager.gc_threshold_percentage
    
    if manager.current_memory_mb.to_float() < memory_threshold {
      return manager  // 不需要GC
    }
    
    // 按访问频率和最近访问时间排序
    let mut sorted_objects = manager.objects.to_array()
    
    // 简化排序：优先删除访问次数少且很久未访问的对象
    let mut objects_to_remove = Array::empty<String>()
    let mut freed_memory_mb = 0
    
    let mut i = 0
    while i < sorted_objects.length() && manager.current_memory_mb - freed_memory_mb > memory_threshold.to_int() {
      let obj = sorted_objects[i]
      let time_since_last_access = current_time - obj.last_access_time
      let access_frequency = obj.access_count.to_float() / (time_since_last_access / 1000000L).to_float()
      
      // 不删除固定对象和最近访问的对象
      if !obj.is_pinned && time_since_last_access > 60000000L && access_frequency < 0.001 {  // 1分钟未访问且访问频率低
        objects_to_remove.push(obj.id)
        freed_memory_mb = freed_memory_mb + (obj.actual_size_bytes / (1024 * 1024))
      }
      
      i = i + 1
    }
    
    // 移除选定的对象
    let mut remaining_objects = Array::empty<LargeObject>()
    let mut j = 0
    while j < sorted_objects.length() {
      let obj = sorted_objects[j]
      if !objects_to_remove.contains(obj.id) {
        remaining_objects.push(obj)
      }
      j = j + 1
    }
    
    { manager |
      objects: remaining_objects,
      current_memory_mb: manager.current_memory_mb - freed_memory_mb
    }
  }
  
  // 测试创建不同大小的对象
  let current_time = 1640995200000000L
  let manager_with_objects = create_large_object(lifecycle_manager, "small-obj", 512, current_time)
  let manager_with_objects = create_large_object(manager_with_objects, "medium-obj", 50 * 1024, current_time + 1000L)
  let manager_with_objects = create_large_object(manager_with_objects, "large-obj", 500 * 1024, current_time + 2000L)
  let manager_with_objects = create_large_object(manager_with_objects, "xlarge-obj", 2 * 1024 * 1024, current_time + 3000L)
  
  assert_eq(manager_with_objects.objects.length(), 4)
  assert_eq(manager_with_objects.current_memory_mb, 2 + 0 + 0 + 0)  // 2MB (向下取整)
  
  // 测试对象访问
  let manager_after_access = access_object(manager_with_objects, "large-obj", current_time + 10000L)
  let manager_after_access = access_object(manager_after_access, "large-obj", current_time + 20000L)
  let manager_after_access = access_object(manager_after_access, "small-obj", current_time + 15000L)
  
  let large_obj = manager_after_access.objects.find(fn(obj) { obj.id == "large-obj" })
  match large_obj {
    Some(obj) => {
      assert_eq(obj.access_count, 2)
      assert_eq(obj.last_access_time, current_time + 20000L)
    }
    None => assert_eq(false, true)
  }
  
  // 测试垃圾回收
  let manager_after_gc = perform_gc(manager_after_access, current_time + 120000L)  // 2分钟后
  
  // 由于内存使用量低，不应该触发GC
  assert_eq(manager_after_gc.objects.length(), manager_after_access.objects.length())
  
  // 创建更多对象触发GC
  let mut high_memory_manager = manager_after_access
  let mut i = 0
  while i < 50 {  // 创建50个大对象
    high_memory_manager = create_large_object(
      high_memory_manager, 
      "bulk-obj-" + i.to_string(), 
      1024 * 1024,  // 1MB each
      current_time + 3000L + i.to_int64() * 1000L
    )
    i = i + 1
  }
  
  // 现在应该触发GC
  let manager_after_bulk_gc = perform_gc(high_memory_manager, current_time + 200000L)
  
  // 验证GC效果
  assert_eq(manager_after_bulk_gc.objects.length() < high_memory_manager.objects.length(), true)
  assert_eq(manager_after_bulk_gc.current_memory_mb < high_memory_manager.current_memory_mb, true)
}

test "memory_pool_optimization_strategies" {
  // 测试内存池优化策略
  
  enum PoolType {
    FixedSize(Int)     // 固定大小池
    VariableSize       // 可变大小池
    Tiered             // 分层池
    Adaptive           // 自适应池
  }
  
  struct MemoryPool {
    pool_type: PoolType
    total_capacity_mb: Int
    used_capacity_mb: Int
    free_blocks: Array<(Int, Int64)>  // (size_bytes, last_free_time)
    allocated_blocks: Array<(String, Int, Int64)>  // (object_id, size_bytes, alloc_time)
    fragmentation_ratio: Float
    allocation_count: Int
    deallocation_count: Int
  }
  
  struct PoolMetrics {
    hit_rate: Float
    miss_rate: Float
    fragmentation_percentage: Float
    allocation_latency_ms: Int
    memory_efficiency: Float
  }
  
  // 创建不同类型的内存池
  let create_pool = fn(pool_type: PoolType, capacity_mb: Int) -> MemoryPool {
    MemoryPool{
      pool_type: pool_type,
      total_capacity_mb: capacity_mb,
      used_capacity_mb: 0,
      free_blocks: Array::empty(),
      allocated_blocks: Array::empty(),
      fragmentation_ratio: 0.0,
      allocation_count: 0,
      deallocation_count: 0
    }
  }
  
  // 从池中分配内存
  let allocate_from_pool = fn(pool: MemoryPool, size_bytes: Int, current_time: Int64) -> (MemoryPool, Bool) {
    if pool.used_capacity_mb + (size_bytes / (1024 * 1024)) > pool.total_capacity_mb {
      return (pool, false)  // 容量不足
    }
    
    // 查找合适的空闲块
    let mut suitable_block_index = -1
    let mut i = 0
    while i < pool.free_blocks.length() {
      let (block_size, _) = pool.free_blocks[i]
      if block_size >= size_bytes {
        suitable_block_index = i
        break
      }
      i = i + 1
    }
    
    let object_id = "obj-" + pool.allocation_count.to_string()
    let mut new_free_blocks = pool.free_blocks.to_array()
    let mut new_allocated_blocks = pool.allocated_blocks.to_array()
    
    if suitable_block_index >= 0 {
      // 使用现有空闲块
      let (block_size, _) = new_free_blocks[suitable_block_index]
      new_free_blocks.remove_at(suitable_block_index)
      
      // 如果块比需要的大，分割剩余部分
      if block_size > size_bytes {
        let remaining_size = block_size - size_bytes
        new_free_blocks.push((remaining_size, current_time))
      }
    } else {
      // 使用新空间
    }
    
    new_allocated_blocks.push((object_id, size_bytes, current_time))
    
    let new_pool = { pool |
      used_capacity_mb: pool.used_capacity_mb + (size_bytes / (1024 * 1024)),
      free_blocks: new_free_blocks,
      allocated_blocks: new_allocated_blocks,
      allocation_count: pool.allocation_count + 1
    }
    
    (new_pool, true)
  }
  
  // 释放内存到池
  let deallocate_to_pool = fn(pool: MemoryPool, object_id: String, current_time: Int64) -> MemoryPool {
    let mut new_allocated_blocks = Array::empty<(String, Int, Int64)>()
    let mut freed_block_size = 0
    
    // 查找并移除分配的块
    let mut i = 0
    while i < pool.allocated_blocks.length() {
      let (id, size, alloc_time) = pool.allocated_blocks[i]
      if id != object_id {
        new_allocated_blocks.push((id, size, alloc_time))
      } else {
        freed_block_size = size
      }
      i = i + 1
    }
    
    if freed_block_size > 0 {
      let mut new_free_blocks = pool.free_blocks.to_array()
      new_free_blocks.push((freed_block_size, current_time))
      
      { pool |
        allocated_blocks: new_allocated_blocks,
        free_blocks: new_free_blocks,
        used_capacity_mb: pool.used_capacity_mb - (freed_block_size / (1024 * 1024)),
        deallocation_count: pool.deallocation_count + 1
      }
    } else {
      pool  // 对象不存在
    }
  }
  
  // 计算池指标
  let calculate_pool_metrics = fn(pool: MemoryPool) -> PoolMetrics {
    let total_operations = pool.allocation_count + pool.deallocation_count
    let hit_rate = if pool.allocation_count > 0 {
      (pool.allocation_count - pool.free_blocks.length()).to_float() / pool.allocation_count.to_float()
    } else {
      0.0
    }
    
    let miss_rate = 1.0 - hit_rate
    
    // 简化的碎片率计算
    let fragmentation_percentage = if pool.free_blocks.length() > 0 {
      let total_free_size = pool.free_blocks.fold(0, fn(acc, block) { acc + block.0 })
      let largest_free = pool.free_blocks.fold(0, fn(acc, block) { @max(acc, block.0) })
      if total_free_size > 0 {
        (1.0 - (largest_free.to_float() / total_free_size.to_float())) * 100.0
      } else {
        0.0
      }
    } else {
      0.0
    }
    
    let memory_efficiency = if pool.total_capacity_mb > 0 {
      pool.used_capacity_mb.to_float() / pool.total_capacity_mb.to_float()
    } else {
      0.0
    }
    
    PoolMetrics{
      hit_rate: hit_rate,
      miss_rate: miss_rate,
      fragmentation_percentage: fragmentation_percentage,
      allocation_latency_ms: 1,  // 简化
      memory_efficiency: memory_efficiency
    }
  }
  
  // 测试不同类型的池
  let fixed_pool = create_pool(FixedSize(1024), 10)  // 10MB固定池
  let variable_pool = create_pool(VariableSize, 10)   // 10MB可变池
  let tiered_pool = create_pool(Tiered, 10)          // 10MB分层池
  let adaptive_pool = create_pool(Adaptive, 10)      // 10MB自适应池
  
  // 测试固定大小池分配
  let current_time = 1640995200000000L
  let (fixed_pool_after_alloc, alloc_success) = allocate_from_pool(fixed_pool, 1024, current_time)
  assert_eq(alloc_success, true)
  assert_eq(fixed_pool_after_alloc.allocation_count, 1)
  assert_eq(fixed_pool_after_alloc.used_capacity_mb, 1)
  
  // 测试释放
  let fixed_pool_after_free = deallocate_to_pool(fixed_pool_after_alloc, "obj-0", current_time + 10000L)
  assert_eq(fixed_pool_after_free.deallocation_count, 1)
  assert_eq(fixed_pool_after_free.used_capacity_mb, 0)
  assert_eq(fixed_pool_after_free.free_blocks.length(), 1)
  
  // 测试多次分配和释放
  let mut test_pool = variable_pool
  let allocation_sizes = [512, 1024, 2048, 4096, 8192, 16384]
  let mut i = 0
  while i < allocation_sizes.length() {
    let (updated_pool, success) = allocate_from_pool(test_pool, allocation_sizes[i], current_time + i.to_int64() * 1000L)
    assert_eq(success, true)
    test_pool = updated_pool
    i = i + 1
  }
  
  // 释放一些对象
  test_pool = deallocate_to_pool(test_pool, "obj-1", current_time + 10000L)
  test_pool = deallocate_to_pool(test_pool, "obj-3", current_time + 11000L)
  test_pool = deallocate_to_pool(test_pool, "obj-5", current_time + 12000L)
  
  // 计算池指标
  let metrics = calculate_pool_metrics(test_pool)
  
  assert_eq(metrics.hit_rate >= 0.0, true)
  assert_eq(metrics.miss_rate >= 0.0, true)
  assert_eq(metrics.fragmentation_percentage >= 0.0, true)
  assert_eq(metrics.memory_efficiency > 0.0, true)
  
  // 验证命中率与释放率的关系
  assert_eq(metrics.hit_rate + metrics.miss_rate, 1.0)
  
  // 测试碎片化
  let fragmented_pool = create_pool(VariableSize, 10)
  let (frag_pool_1, _) = allocate_from_pool(fragmented_pool, 1024, current_time)
  let (frag_pool_2, _) = allocate_from_pool(frag_pool_1, 1024, current_time + 1000L)
  let frag_pool_3 = deallocate_to_pool(frag_pool_2, "obj-0", current_time + 2000L)  // 创建碎片
  let frag_pool_4 = deallocate_to_pool(frag_pool_3, "obj-1", current_time + 3000L)
  
  let frag_metrics = calculate_pool_metrics(frag_pool_4)
  assert_eq(frag_metrics.fragmentation_percentage >= 0.0, true)
}

test "memory_pressure_response" {
  // 测试内存压力响应机制
  
  enum MemoryPressureLevel {
    Normal
    Warning
    Critical
    Emergency
  }
  
  struct MemoryPressureMonitor {
    current_level: MemoryPressureLevel
    threshold_warning_mb: Int
    threshold_critical_mb: Int
    threshold_emergency_mb: Int
    max_memory_mb: Int
    current_memory_mb: Int
    last_pressure_time: Int64
    response_actions: Array<String>
  }
  
  struct MemoryResponseAction {
    name: String
    trigger_level: MemoryPressureLevel
    memory_freed_mb: Int
    execution_time_ms: Int
    priority: Int
  }
  
  // 创建内存压力监控器
  let pressure_monitor = MemoryPressureMonitor{
    current_level: Normal,
    threshold_warning_mb: 512,
    threshold_critical_mb: 768,
    threshold_emergency_mb: 896,
    max_memory_mb: 1024,
    current_memory_mb: 256,
    last_pressure_time: 0L,
    response_actions: Array::empty()
  }
  
  // 定义响应动作
  let response_actions = [
    MemoryResponseAction{
      name: "clear_caches",
      trigger_level: Warning,
      memory_freed_mb: 64,
      execution_time_ms: 10,
      priority: 1
    },
    MemoryResponseAction{
      name: "compact_memory",
      trigger_level: Warning,
      memory_freed_mb: 32,
      execution_time_ms: 50,
      priority: 2
    },
    MemoryResponseAction{
      name: "force_gc",
      trigger_level: Critical,
      memory_freed_mb: 128,
      execution_time_ms: 100,
      priority: 1
    },
    MemoryResponseAction{
      name: "purge_old_data",
      trigger_level: Critical,
      memory_freed_mb: 256,
      execution_time_ms: 200,
      priority: 2
    },
    MemoryResponseAction{
      name: "emergency_cleanup",
      trigger_level: Emergency,
      memory_freed_mb: 384,
      execution_time_ms: 500,
      priority: 1
    }
  ]
  
  // 评估内存压力级别
  let evaluate_pressure_level = fn(monitor: MemoryPressureMonitor) -> MemoryPressureLevel {
    let usage_percentage = monitor.current_memory_mb.to_float() / monitor.max_memory_mb.to_float()
    
    if usage_percentage >= 0.875 {  // 87.5%
      Emergency
    } else if usage_percentage >= 0.75 {  // 75%
      Critical
    } else if usage_percentage >= 0.5 {   // 50%
      Warning
    } else {
      Normal
    }
  }
  
  // 执行响应动作
  let execute_response_actions = fn(
    monitor: MemoryPressureMonitor, 
    pressure_level: MemoryPressureLevel,
    actions: Array<MemoryResponseAction>
  ) -> MemoryPressureMonitor {
    let mut updated_monitor = monitor
    let mut applicable_actions = Array::empty<MemoryResponseAction>()
    
    // 筛选适用的动作
    let mut i = 0
    while i < actions.length() {
      let action = actions[i]
      match action.trigger_level {
        Normal => {}  // Normal级别不需要动作
        Warning => if pressure_level == Warning || pressure_level == Critical || pressure_level == Emergency {
          applicable_actions.push(action)
        }
        Critical => if pressure_level == Critical || pressure_level == Emergency {
          applicable_actions.push(action)
        }
        Emergency => if pressure_level == Emergency {
          applicable_actions.push(action)
        }
      }
      i = i + 1
    }
    
    // 按优先级排序
    applicable_actions.sort_by(fn(a, b) { a.priority - b.priority })
    
    // 执行动作
    let mut total_freed = 0
    let mut executed_actions = Array::empty<String>()
    
    let mut j = 0
    while j < applicable_actions.length() {
      let action = applicable_actions[j]
      total_freed = total_freed + action.memory_freed_mb
      executed_actions.push(action.name)
      j = j + 1
    }
    
    // 更新监控器状态
    let mut new_actions = updated_monitor.response_actions.to_array()
    let mut k = 0
    while k < executed_actions.length() {
      new_actions.push(executed_actions[k])
      k = k + 1
    }
    
    { updated_monitor |
      current_level: pressure_level,
      current_memory_mb: @max(0, updated_monitor.current_memory_mb - total_freed),
      response_actions: new_actions
    }
  }
  
  // 测试不同内存压力级别
  let test_scenarios = [
    (256, Normal),    // 25% 使用率
    (512, Warning),   // 50% 使用率
    (768, Critical),  // 75% 使用率
    (896, Emergency)  // 87.5% 使用率
  ]
  
  let mut i = 0
  while i < test_scenarios.length() {
    let (memory_mb, expected_level) = test_scenarios[i]
    
    let test_monitor = { pressure_monitor | current_memory_mb: memory_mb }
    let detected_level = evaluate_pressure_level(test_monitor)
    
    assert_eq(detected_level, expected_level)
    
    // 测试响应动作
    let monitor_after_response = execute_response_actions(test_monitor, detected_level, response_actions)
    
    match detected_level {
      Normal => assert_eq(monitor_after_response.response_actions.length(), 0)
      Warning => assert_eq(monitor_after_response.response_actions.length() >= 1, true)
      Critical => assert_eq(monitor_after_response.response_actions.length() >= 3, true)
      Emergency => assert_eq(monitor_after_response.response_actions.length() >= 5, true)
    }
    
    i = i + 1
  }
  
  // 测试内存释放效果
  let critical_monitor = { pressure_monitor | current_memory_mb: 800 }  // 临界状态
  let monitor_after_critical = execute_response_actions(critical_monitor, Critical, response_actions)
  
  assert_eq(monitor_after_critical.current_memory_mb < 800, true)  // 内存应该被释放
  assert_eq(monitor_after_critical.current_level, Critical)
  
  // 测试紧急情况处理
  let emergency_monitor = { pressure_monitor | current_memory_mb: 950 }  // 紧急状态
  let monitor_after_emergency = execute_response_actions(emergency_monitor, Emergency, response_actions)
  
  assert_eq(monitor_after_emergency.current_memory_mb < 950, true)
  assert_eq(monitor_after_emergency.current_level, Emergency)
  
  // 验证紧急情况下释放更多内存
  assert_eq(monitor_after_emergency.current_memory_mb < monitor_after_critical.current_memory_mb, true)
}