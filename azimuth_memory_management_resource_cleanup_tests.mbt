// Azimuth 内存管理和资源清理测试
// 专注于测试遥测系统中的内存使用效率和资源清理机制

// 测试1: 内存泄漏检测和资源释放验证
test "内存泄漏检测和资源释放验证" {
  // 定义资源管理数据结构
  type ResourceHandle = {
    resource_id: String,
    resource_type: String,
    allocation_time: Int,
    size_bytes: Int,
    is_active: Bool,
    cleanup_function: Option[() -> Unit]
  }
  
  type MemoryPool = {
    pool_id: String,
    total_capacity_bytes: Int,
    allocated_bytes: Int,
    active_resources: Array[ResourceHandle],
    max_resources: Int
  }
  
  // 创建内存池管理器
  let create_memory_pool = fn(pool_id: String, capacity_bytes: Int, max_resources: Int) -> MemoryPool {
    {
      pool_id: pool_id,
      total_capacity_bytes: capacity_bytes,
      allocated_bytes: 0,
      active_resources: [],
      max_resources: max_resources
    }
  }
  
  // 资源分配函数
  let allocate_resource = fn(
    pool: MemoryPool, 
    resource_type: String, 
    size_bytes: Int,
    cleanup_fn: Option[() -> Unit]
  ) -> (MemoryPool, Option[ResourceHandle]) {
    // 检查容量限制
    if pool.allocated_bytes + size_bytes > pool.total_capacity_bytes {
      return (pool, None)
    }
    
    // 检查资源数量限制
    if pool.active_resources.length() >= pool.max_resources {
      return (pool, None)
    }
    
    // 创建新资源
    let resource = {
      resource_id: "res-" + (pool.active_resources.length() as String),
      resource_type: resource_type,
      allocation_time: Time::now(),
      size_bytes: size_bytes,
      is_active: true,
      cleanup_function: cleanup_fn
    }
    
    // 更新内存池
    let updated_pool = {
      ...pool,
      allocated_bytes: pool.allocated_bytes + size_bytes,
      active_resources: pool.active_resources.push(resource)
    }
    
    (updated_pool, Some(resource))
  }
  
  // 资源释放函数
  let release_resource = fn(pool: MemoryPool, resource_id: String) -> MemoryPool {
    let mut updated_pool = pool
    let mut found_resource = false
    
    // 查找并释放资源
    let updated_resources = pool.active_resources.map(fn(resource) {
      if resource.resource_id == resource_id && resource.is_active {
        found_resource = true
        updated_pool.allocated_bytes = updated_pool.allocated_bytes - resource.size_bytes
        
        // 执行清理函数
        match resource.cleanup_function {
          Some(cleanup_fn) => cleanup_fn()
          None => ()
        }
        
        { ...resource, is_active: false }
      } else {
        resource
      }
    })
    
    if found_resource {
      { ...updated_pool, active_resources: updated_resources }
    } else {
      pool
    }
  }
  
  // 内存泄漏检测函数
  let detect_memory_leaks = fn(pool: MemoryPool) -> Array[ResourceHandle] {
    let current_time = Time::now()
    let leak_threshold_ms = 5000  // 5秒阈值
    
    pool.active_resources.filter(fn(resource) {
      if !resource.is_active {
        return false
      }
      
      // 检查资源是否长时间未释放
      let age_ms = current_time - resource.allocation_time
      age_ms > leak_threshold_ms
    })
  }
  
  // 测试资源分配和释放
  let initial_pool = create_memory_pool("test-pool", 1024 * 1024, 100)  // 1MB, 100个资源
  
  // 分配一些资源
  let (pool1, resource1) = allocate_resource(initial_pool, "buffer", 1024, None)
  let (pool2, resource2) = allocate_resource(pool1, "connection", 512, None)
  let (pool3, resource3) = allocate_resource(pool2, "cache", 2048, None)
  
  // 验证资源分配成功
  assert_true(resource1.is_some())
  assert_true(resource2.is_some())
  assert_true(resource3.is_some())
  
  // 验证内存使用量
  assert_eq(pool3.allocated_bytes, 1024 + 512 + 2048)
  assert_eq(pool3.active_resources.length(), 3)
  
  // 释放一个资源
  let pool4 = release_resource(pool3, resource1.unwrap().resource_id)
  
  // 验证资源释放
  assert_eq(pool4.allocated_bytes, 512 + 2048)
  
  // 检查内存泄漏（应该没有泄漏，因为资源刚分配）
  let leaks = detect_memory_leaks(pool4)
  assert_eq(leaks.length(), 0)
}

// 测试2: 缓冲区管理和内存池优化
test "缓冲区管理和内存池优化" {
  // 定义缓冲区数据结构
  type Buffer = {
    buffer_id: String,
    data: Array[Byte],
    capacity: Int,
    used_bytes: Int,
    is_dirty: Bool,
    last_access_time: Int
  }
  
  type BufferPool = {
    pool_id: String,
    buffers: Map[String, Buffer],
    free_buffers: Array[String],
    total_memory_bytes: Int,
    used_memory_bytes: Int,
    buffer_count: Int
  }
  
  // 创建缓冲区池
  let create_buffer_pool = fn(pool_id: String, total_memory: Int) -> BufferPool {
    {
      pool_id: pool_id,
      buffers: Map::new(),
      free_buffers: [],
      total_memory_bytes: total_memory,
      used_memory_bytes: 0,
      buffer_count: 0
    }
  }
  
  // 分配缓冲区
  let allocate_buffer = fn(pool: BufferPool, size: Int) -> (BufferPool, Option[String]) {
    // 检查内存限制
    if pool.used_memory_bytes + size > pool.total_memory_bytes {
      return (pool, None)
    }
    
    // 尝试重用空闲缓冲区
    match pool.free_buffers.pop() {
      Some(buffer_id) => {
        let buffer = pool.buffers.get(buffer_id).unwrap()
        if buffer.capacity >= size {
          let updated_buffer = {
            ...buffer,
            used_bytes: size,
            is_dirty: true,
            last_access_time: Time::now()
          }
          
          let updated_buffers = pool.buffers.set(buffer_id, updated_buffer)
          let updated_pool = {
            ...pool,
            buffers: updated_buffers,
            used_memory_bytes: pool.used_memory_bytes - buffer.used_bytes + size
          }
          
          return (updated_pool, Some(buffer_id))
        }
      }
      None => ()
    }
    
    // 创建新缓冲区
    let buffer_id = "buf-" + (pool.buffer_count as String)
    let new_buffer = {
      buffer_id: buffer_id,
      data: Array::make(size, 0),
      capacity: size,
      used_bytes: size,
      is_dirty: true,
      last_access_time: Time::now()
    }
    
    let updated_buffers = pool.buffers.set(buffer_id, new_buffer)
    let updated_pool = {
      ...pool,
      buffers: updated_buffers,
      used_memory_bytes: pool.used_memory_bytes + size,
      buffer_count: pool.buffer_count + 1
    }
    
    (updated_pool, Some(buffer_id))
  }
  
  // 释放缓冲区
  let release_buffer = fn(pool: BufferPool, buffer_id: String) -> BufferPool {
    match pool.buffers.get(buffer_id) {
      Some(buffer) => {
        let updated_buffer = {
          ...buffer,
          is_dirty: false,
          last_access_time: Time::now()
        }
        
        let updated_buffers = pool.buffers.set(buffer_id, updated_buffer)
        let updated_pool = {
          ...pool,
          buffers: updated_buffers,
          free_buffers: pool.free_buffers.push(buffer_id)
        }
        
        updated_pool
      }
      None => pool
    }
  }
  
  // 内存碎片整理
  let defragment_memory = fn(pool: BufferPool) -> BufferPool {
    let current_time = Time::now()
    let idle_threshold_ms = 10000  // 10秒未访问
    
    // 找出长时间未使用的缓冲区
    let idle_buffers = pool.buffers.filter(fn(_, buffer) {
      !buffer.is_dirty && (current_time - buffer.last_access_time) > idle_threshold_ms
    })
    
    // 释放空闲缓冲区
    let mut updated_pool = pool
    for (buffer_id, _) in idle_buffers {
      let buffer = pool.buffers.get(buffer_id).unwrap()
      updated_pool = {
        ...updated_pool,
        used_memory_bytes: updated_pool.used_memory_bytes - buffer.capacity,
        buffers: updated_pool.buffers.remove(buffer_id),
        free_buffers: updated_pool.free_buffers.filter(fn(id) { id != buffer_id })
      }
    }
    
    updated_pool
  }
  
  // 测试缓冲区管理
  let initial_pool = create_buffer_pool("test-buffer-pool", 10240)  // 10KB
  
  // 分配一些缓冲区
  let (pool1, buf1_id) = allocate_buffer(initial_pool, 1024)
  let (pool2, buf2_id) = allocate_buffer(pool1, 2048)
  let (pool3, buf3_id) = allocate_buffer(pool2, 4096)
  
  // 验证分配成功
  assert_true(buf1_id.is_some())
  assert_true(buf2_id.is_some())
  assert_true(buf3_id.is_some())
  
  // 验证内存使用
  assert_eq(pool3.used_memory_bytes, 1024 + 2048 + 4096)
  assert_eq(pool3.buffer_count, 3)
  
  // 释放一个缓冲区
  let pool4 = release_buffer(pool3, buf1_id.unwrap())
  
  // 验证释放后状态
  assert_eq(pool4.used_memory_bytes, 1024 + 2048 + 4096)  // 内存仍在使用
  assert_eq(pool4.free_buffers.length(), 1)
  
  // 重用释放的缓冲区
  let (pool5, reused_buf_id) = allocate_buffer(pool4, 512)
  
  // 验证重用成功
  assert_true(reused_buf_id.is_some())
  assert_eq(reused_buf_id.unwrap(), buf1_id.unwrap())  // 应该重用同一个缓冲区
  assert_eq(pool5.used_memory_bytes, 512 + 2048 + 4096)  // 内存使用量应该减少
}

// 测试3: 资源生命周期管理
test "资源生命周期管理" {
  // 定义资源生命周期状态
  type ResourceLifecycle = {
    resource_id: String,
    created_at: Int,
    initialized_at: Option[Int],
    active_at: Option[Int],
    deactivating_at: Option[Int],
    destroyed_at: Option[Int],
    current_state: String
  }
  
  type LifecycleManager = {
    resources: Map[String, ResourceLifecycle],
    state_transitions: Map[String, Array[String]],
    transition_rules: Map[String, Array[String]]
  }
  
  // 创建生命周期管理器
  let create_lifecycle_manager = fn() -> LifecycleManager {
    let transition_rules = Map::from_array([
      ("created", ["initialized"]),
      ("initialized", ["active"]),
      ("active", ["deactivating"]),
      ("deactivating", ["destroyed"]),
      ("destroyed", [])
    ])
    
    {
      resources: Map::new(),
      state_transitions: Map::new(),
      transition_rules: transition_rules
    }
  }
  
  // 状态转换验证
  let can_transition = fn(manager: LifecycleManager, from_state: String, to_state: String) -> Bool {
    match manager.transition_rules.get(from_state) {
      Some(allowed_states) => allowed_states.contains(to_state),
      None => false
    }
  }
  
  // 执行状态转换
  let transition_state = fn(
    manager: LifecycleManager, 
    resource_id: String, 
    new_state: String
  ) -> LifecycleManager {
    match manager.resources.get(resource_id) {
      Some(lifecycle) => {
        let current_state = lifecycle.current_state
        
        if can_transition(manager, current_state, new_state) {
          let current_time = Time::now()
          let updated_lifecycle = match new_state {
            "initialized" => {
              ...lifecycle,
              initialized_at: Some(current_time),
              current_state: new_state
            }
            "active" => {
              ...lifecycle,
              active_at: Some(current_time),
              current_state: new_state
            }
            "deactivating" => {
              ...lifecycle,
              deactivating_at: Some(current_time),
              current_state: new_state
            }
            "destroyed" => {
              ...lifecycle,
              destroyed_at: Some(current_time),
              current_state: new_state
            }
            _ => lifecycle
          }
          
          // 记录状态转换
          let transitions = match manager.state_transitions.get(resource_id) {
            Some(existing_transitions) => existing_transitions.push(new_state)
            None => [new_state]
          }
          
          let updated_resources = manager.resources.set(resource_id, updated_lifecycle)
          let updated_transitions = manager.state_transitions.set(resource_id, transitions)
          
          {
            ...manager,
            resources: updated_resources,
            state_transitions: updated_transitions
          }
        } else {
          manager  // 无效转换，返回原状态
        }
      }
      None => manager
    }
  }
  
  // 创建资源
  let create_resource = fn(manager: LifecycleManager, resource_id: String) -> LifecycleManager {
    let current_time = Time::now()
    let lifecycle = {
      resource_id: resource_id,
      created_at: current_time,
      initialized_at: None,
      active_at: None,
      deactivating_at: None,
      destroyed_at: None,
      current_state: "created"
    }
    
    let updated_resources = manager.resources.set(resource_id, lifecycle)
    let updated_transitions = manager.state_transitions.set(resource_id, ["created"])
    
    {
      ...manager,
      resources: updated_resources,
      state_transitions: updated_transitions
    }
  }
  
  // 测试生命周期管理
  let initial_manager = create_lifecycle_manager()
  
  // 创建资源
  let manager1 = create_resource(initial_manager, "resource-1")
  
  // 验证初始状态
  let resource1 = manager1.resources.get("resource-1").unwrap()
  assert_eq(resource1.current_state, "created")
  assert_true(resource1.initialized_at.is_none())
  
  // 转换到初始化状态
  let manager2 = transition_state(manager1, "resource-1", "initialized")
  
  // 验证初始化状态
  let resource2 = manager2.resources.get("resource-1").unwrap()
  assert_eq(resource2.current_state, "initialized")
  assert_true(resource2.initialized_at.is_some())
  
  // 转换到活跃状态
  let manager3 = transition_state(manager2, "resource-1", "active")
  
  // 验证活跃状态
  let resource3 = manager3.resources.get("resource-1").unwrap()
  assert_eq(resource3.current_state, "active")
  assert_true(resource3.active_at.is_some())
  
  // 尝试无效转换（从active直接到created）
  let manager4 = transition_state(manager3, "resource-1", "created")
  
  // 验证无效转换被拒绝
  let resource4 = manager4.resources.get("resource-1").unwrap()
  assert_eq(resource4.current_state, "active")  // 状态应该保持不变
  
  // 正常的生命周期转换
  let manager5 = transition_state(manager4, "resource-1", "deactivating")
  let manager6 = transition_state(manager5, "resource-1", "destroyed")
  
  // 验证最终状态
  let resource6 = manager6.resources.get("resource-1").unwrap()
  assert_eq(resource6.current_state, "destroyed")
  assert_true(resource6.destroyed_at.is_some())
  
  // 验证状态转换历史
  let transitions = manager6.state_transitions.get("resource-1").unwrap()
  assert_eq(transitions.length(), 4)  // created -> initialized -> active -> deactivating -> destroyed
}

// 测试4: 内存压力测试和资源限制
test "内存压力测试和资源限制" {
  // 定义内存压力测试数据结构
  type MemoryStressTest = {
    test_id: String,
    target_memory_mb: Int,
    allocation_step_kb: Int,
    max_allocations: Int,
    allocations: Array[AllocationRecord]
  }
  
  type AllocationRecord = {
    allocation_id: String,
    size_bytes: Int,
    allocation_time: Int,
    deallocation_time: Option[Int],
    is_active: Bool
  }
  
  type StressTestResult = {
    test_id: String,
    total_allocated_mb: Float,
    peak_memory_mb: Float,
    allocation_count: Int,
    deallocation_count: Int,
    memory_efficiency: Float,
    test_duration_ms: Int
  }
  
  // 创建内存压力测试
  let create_stress_test = fn(
    test_id: String, 
    target_memory_mb: Int, 
    allocation_step_kb: Int
  ) -> MemoryStressTest {
    {
      test_id: test_id,
      target_memory_mb: target_memory_mb,
      allocation_step_kb: allocation_step_kb,
      max_allocations: (target_memory_mb * 1024) / allocation_step_kb,
      allocations: []
    }
  }
  
  // 执行内存分配
  let allocate_memory = fn(test: MemoryStressTest, size_bytes: Int) -> MemoryStressTest {
    let current_allocated = test.allocations.filter(fn(a) { a.is_active })
      .reduce(0, fn(acc, a) { acc + a.size_bytes })
    
    let current_allocated_mb = (current_allocated as Float) / (1024.0 * 1024.0)
    
    // 检查是否超过目标内存
    if current_allocated_mb >= (test.target_memory_mb as Float) {
      return test
    }
    
    // 创建新的分配记录
    let allocation = {
      allocation_id: "alloc-" + (test.allocations.length() as String),
      size_bytes: size_bytes,
      allocation_time: Time::now(),
      deallocation_time: None,
      is_active: true
    }
    
    {
      ...test,
      allocations: test.allocations.push(allocation)
    }
  }
  
  // 执行内存释放
  let deallocate_memory = fn(test: MemoryStressTest, allocation_id: String) -> MemoryStressTest {
    let updated_allocations = test.allocations.map(fn(allocation) {
      if allocation.allocation_id == allocation_id && allocation.is_active {
        {
          ...allocation,
          deallocation_time: Some(Time::now()),
          is_active: false
        }
      } else {
        allocation
      }
    })
    
    { ...test, allocations: updated_allocations }
  }
  
  // 计算测试结果
  let calculate_test_result = fn(test: MemoryStressTest, start_time: Int) -> StressTestResult {
    let end_time = Time::now()
    let test_duration = end_time - start_time
    
    let active_allocations = test.allocations.filter(fn(a) { a.is_active })
    let total_allocated = active_allocations.reduce(0, fn(acc, a) { acc + a.size_bytes })
    let total_allocated_mb = (total_allocated as Float) / (1024.0 * 1024.0)
    
    let peak_memory = test.allocations.reduce(0, fn(acc, a) { 
      if a.is_active { acc + a.size_bytes } else { acc }
    })
    let peak_memory_mb = (peak_memory as Float) / (1024.0 * 1024.0)
    
    let allocation_count = test.allocations.length()
    let deallocation_count = test.allocations.filter(fn(a) { !a.is_active }).length()
    
    // 计算内存效率（实际使用/分配总量）
    let memory_efficiency = if allocation_count > 0 {
      (total_allocated as Float) / (test.allocations.reduce(0, fn(acc, a) { acc + a.size_bytes }) as Float)
    } else {
      0.0
    }
    
    {
      test_id: test.test_id,
      total_allocated_mb: total_allocated_mb,
      peak_memory_mb: peak_memory_mb,
      allocation_count: allocation_count,
      deallocation_count: deallocation_count,
      memory_efficiency: memory_efficiency,
      test_duration_ms: test_duration
    }
  }
  
  // 执行压力测试
  let start_time = Time::now()
  let initial_test = create_stress_test("stress-test-1", 10, 64)  // 10MB目标，64KB步长
  
  // 模拟内存分配循环
  let mut current_test = initial_test
  for i in 0..100 {
    current_test = allocate_memory(current_test, 64 * 1024)  // 64KB分配
    
    // 随机释放一些内存
    if i % 3 == 0 && current_test.allocations.length() > 5 {
      let allocation_to_free = current_test.allocations[i % current_test.allocations.length()]
      current_test = deallocate_memory(current_test, allocation_to_free.allocation_id)
    }
  }
  
  // 计算测试结果
  let result = calculate_test_result(current_test, start_time)
  
  // 验证测试结果
  assert_true(result.total_allocated_mb <= 10.0)  // 不应该超过目标内存
  assert_true(result.allocation_count > 0)  // 应该有分配
  assert_true(result.memory_efficiency >= 0.0 && result.memory_efficiency <= 1.0)  // 效率应该在合理范围
  assert_true(result.test_duration_ms >= 0)  // 测试时间应该为正数
}