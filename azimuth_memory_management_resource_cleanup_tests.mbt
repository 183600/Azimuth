// Azimuth 内存管理和资源清理测试用例
// 专注于测试遥测系统的内存管理、资源分配和清理机制

// 测试1: 内存分配和释放
test "内存分配和释放" {
  // 模拟内存池管理
  let mut memory_pool = {
    total_size: 1024 * 1024,  // 1MB
    allocated_size: 0,
    free_blocks: [{ start: 0, size: 1024 * 1024 }],
    allocated_blocks: []
  }
  
  // 内存分配函数
  func allocate_memory(pool : { total_size : Int, allocated_size : Int, free_blocks : Array[{ start : Int, size : Int }], allocated_blocks : Array[{ start : Int, size : Int }] }, requested_size : Int) -> { success : Bool, address : Int, updated_pool : { total_size : Int, allocated_size : Int, free_blocks : Array[{ start : Int, size : Int }], allocated_blocks : Array[{ start : Int, size : Int }] } } {
    // 查找合适的空闲块
    let mut suitable_block_index = -1
    let mut updated_free_blocks = pool.free_blocks
    
    for i in range(0, pool.free_blocks.length()) {
      let block = pool.free_blocks[i]
      if block.size >= requested_size {
        suitable_block_index = i
        break
      }
    }
    
    if suitable_block_index == -1 {
      return { success: false, address: -1, updated_pool: pool }
    }
    
    // 分配内存
    let suitable_block = pool.free_blocks[suitable_block_index]
    let allocated_address = suitable_block.start
    
    // 更新空闲块列表
    let mut new_free_blocks = []
    for i in range(0, pool.free_blocks.length()) {
      if i != suitable_block_index {
        new_free_blocks = new_free_blocks.push(pool.free_blocks[i])
      } else {
        let block = pool.free_blocks[i]
        if block.size > requested_size {
          // 分割块
          let remaining_block = { start: block.start + requested_size, size: block.size - requested_size }
          new_free_blocks = new_free_blocks.push(remaining_block)
        }
      }
    }
    
    // 更新已分配块列表
    let new_allocated_block = { start: allocated_address, size: requested_size }
    let new_allocated_blocks = pool.allocated_blocks.push(new_allocated_block)
    
    // 更新内存池状态
    let updated_pool = {
      total_size: pool.total_size,
      allocated_size: pool.allocated_size + requested_size,
      free_blocks: new_free_blocks,
      allocated_blocks: new_allocated_blocks
    }
    
    return { success: true, address: allocated_address, updated_pool: updated_pool }
  }
  
  // 内存释放函数
  func free_memory(pool : { total_size : Int, allocated_size : Int, free_blocks : Array[{ start : Int, size : Int }], allocated_blocks : Array[{ start : Int, size : Int }] }, address : Int, size : Int) -> { success : Bool, updated_pool : { total_size : Int, allocated_size : Int, free_blocks : Array[{ start : Int, size : Int }], allocated_blocks : Array[{ start : Int, size : Int }] } } {
    // 查找要释放的已分配块
    let mut block_found = false
    let mut updated_allocated_blocks = []
    
    for block in pool.allocated_blocks {
      if block.start == address && block.size == size {
        block_found = true
      } else {
        updated_allocated_blocks = updated_allocated_blocks.push(block)
      }
    }
    
    if not block_found {
      return { success: false, updated_pool: pool }
    }
    
    // 添加回空闲块列表
    let freed_block = { start: address, size: size }
    let new_free_blocks = pool.free_blocks.push(freed_block)
    
    // 更新内存池状态
    let updated_pool = {
      total_size: pool.total_size,
      allocated_size: pool.allocated_size - size,
      free_blocks: new_free_blocks,
      allocated_blocks: updated_allocated_blocks
    }
    
    return { success: true, updated_pool: updated_pool }
  }
  
  // 测试内存分配
  let alloc_result1 = allocate_memory(memory_pool, 1024)  // 分配1KB
  assert_true(alloc_result1.success)
  assert_eq(alloc_result1.address, 0)
  assert_eq(alloc_result1.updated_pool.allocated_size, 1024)
  assert_eq(alloc_result1.updated_pool.free_blocks.length(), 1)  // 原始块被分割
  
  memory_pool = alloc_result1.updated_pool
  
  let alloc_result2 = allocate_memory(memory_pool, 2048)  // 分配2KB
  assert_true(alloc_result2.success)
  assert_eq(alloc_result2.address, 1024)  // 紧接着第一个块
  assert_eq(alloc_result2.updated_pool.allocated_size, 1024 + 2048)
  
  memory_pool = alloc_result2.updated_pool
  
  // 测试内存释放
  let free_result1 = free_memory(memory_pool, 1024, 1024)  // 释放第一个块
  assert_true(free_result1.success)
  assert_eq(free_result1.updated_pool.allocated_size, 2048)  // 只剩下第二个块
  
  memory_pool = free_result1.updated_pool
  
  // 测试释放不存在的块
  let free_result_invalid = free_memory(memory_pool, 9999, 1024)
  assert_false(free_result_invalid.success)
  
  // 测试过度分配
  let alloc_result_oversize = allocate_memory(memory_pool, memory_pool.total_size - memory_pool.allocated_size + 1)
  assert_false(alloc_result_oversize.success)
}

// 测试2: 内存泄漏检测
test "内存泄漏检测" {
  // 模拟资源分配跟踪
  let mut resource_tracker = {
    allocations: [],
    deallocations: [],
    current_allocations: {}
  }
  
  // 记录内存分配
  func track_allocation(tracker : { allocations : Array[{ id : String, size : Int, timestamp : Int }], deallocations : Array[String], current_allocations : Map[String, { size : Int, timestamp : Int }] }, allocation_id : String, size : Int, timestamp : Int) -> { allocations : Array[{ id : String, size : Int, timestamp : Int }], deallocations : Array[String], current_allocations : Map[String, { size : Int, timestamp : Int }] } {
    let allocation = { id: allocation_id, size: size, timestamp: timestamp }
    let new_allocations = tracker.allocations.push(allocation)
    let new_current_allocations = tracker.current_allocations.set(allocation_id, { size: size, timestamp: timestamp })
    
    return {
      allocations: new_allocations,
      deallocations: tracker.deallocations,
      current_allocations: new_current_allocations
    }
  }
  
  // 记录内存释放
  func track_deallocation(tracker : { allocations : Array[{ id : String, size : Int, timestamp : Int }], deallocations : Array[String], current_allocations : Map[String, { size : Int, timestamp : Int }] }, allocation_id : String) -> { allocations : Array[{ id : String, size : Int, timestamp : Int }], deallocations : Array[String], current_allocations : Map[String, { size : Int, timestamp : Int }] } {
    let new_deallocations = tracker.deallocations.push(allocation_id)
    let new_current_allocations = tracker.current_allocations.delete(allocation_id)
    
    return {
      allocations: tracker.allocations,
      deallocations: new_deallocations,
      current_allocations: new_current_allocations
    }
  }
  
  // 检测内存泄漏
  func detect_memory_leaks(tracker : { allocations : Array[{ id : String, size : Int, timestamp : Int }], deallocations : Array[String], current_allocations : Map[String, { size : Int, timestamp : Int }] }) -> { has_leaks : Bool, leaked_allocations : Array[{ id : String, size : Int, timestamp : Int }], total_leaked_size : Int } {
    let leaked_allocations = []
    let mut total_leaked_size = 0
    
    for allocation in tracker.allocations {
      let is_deallocated = tracker.deallocations.contains(allocation.id)
      if not is_deallocated {
        leaked_allocations = leaked_allocations.push(allocation)
        total_leaked_size = total_leaked_size + allocation.size
      }
    }
    
    return {
      has_leaks: leaked_allocations.length() > 0,
      leaked_allocations: leaked_allocations,
      total_leaked_size: total_leaked_size
    }
  }
  
  // 模拟资源分配和释放
  resource_tracker = track_allocation(resource_tracker, "alloc_001", 1024, 1640995200)
  resource_tracker = track_allocation(resource_tracker, "alloc_002", 2048, 1640995210)
  resource_tracker = track_allocation(resource_tracker, "alloc_003", 512, 1640995220)
  
  // 释放部分资源
  resource_tracker = track_deallocation(resource_tracker, "alloc_002")
  
  // 检测内存泄漏
  let leak_result = detect_memory_leaks(resource_tracker)
  assert_true(leak_result.has_leaks)
  assert_eq(leak_result.leaked_allocations.length(), 2)
  assert_eq(leak_result.total_leaked_size, 1024 + 512)
  
  // 验证泄漏的资源
  let leaked_ids = leak_result.leaked_allocations.map(a => a.id)
  assert_true(leaked_ids.contains("alloc_001"))
  assert_true(leaked_ids.contains("alloc_003"))
  assert_false(leaked_ids.contains("alloc_002"))  // 已释放
  
  // 释放剩余资源
  resource_tracker = track_deallocation(resource_tracker, "alloc_001")
  resource_tracker = track_deallocation(resource_tracker, "alloc_003")
  
  // 再次检测内存泄漏
  let no_leak_result = detect_memory_leaks(resource_tracker)
  assert_false(no_leak_result.has_leaks)
  assert_eq(no_leak_result.leaked_allocations.length(), 0)
  assert_eq(no_leak_result.total_leaked_size, 0)
}

// 测试3: 垃圾回收机制
test "垃圾回收机制" {
  // 模拟对象引用关系
  let mut objects = {
    "obj_001": { size: 1024, references: ["obj_002", "obj_003"], is_root: true },
    "obj_002": { size: 512, references: ["obj_004"], is_root: false },
    "obj_003": { size: 256, references: [], is_root: false },
    "obj_004": { size: 128, references: ["obj_005"], is_root: false },
    "obj_005": { size: 64, references: [], is_root: false },
    "obj_006": { size: 32, references: [], is_root: false }  // 不可达对象
  }
  
  // 标记可达对象
  func mark_reachable(objects : Map[String, { size : Int, references : Array[String], is_root : Bool }]) -> Map[String, Bool] {
    let mut reachable = {}  // 对象ID -> 是否可达
    let mut to_visit = []
    
    // 添加根对象到访问列表
    for (obj_id, obj_info) in objects.to_array() {
      if obj_info.is_root {
        to_visit = to_visit.push(obj_id)
        reachable = reachable.set(obj_id, true)
      }
    }
    
    // 遍历引用图
    while to_visit.length() > 0 {
      let current_obj = to_visit[0]
      to_visit = to_visit.slice(1)  // 移除第一个元素
      
      match objects.get(current_obj) {
        Some(obj_info) => {
          for ref_id in obj_info.references {
            match reachable.get(ref_id) {
              Some(_) => (),  // 已标记
              None => {
                reachable = reachable.set(ref_id, true)
                to_visit = to_visit.push(ref_id)
              }
            }
          }
        }
        None => ()
      }
    }
    
    return reachable
  }
  
  // 清理不可达对象
  func sweep_unreachable(objects : Map[String, { size : Int, references : Array[String], is_root : Bool }], reachable : Map[String, Bool]) -> { cleaned_objects : Map[String, { size : Int, references : Array[String], is_root : Bool }], reclaimed_size : Int } {
    let mut cleaned_objects = {}
    let mut reclaimed_size = 0
    
    for (obj_id, obj_info) in objects.to_array() {
      match reachable.get(obj_id) {
        Some(is_reachable) => {
          if is_reachable {
            cleaned_objects = cleaned_objects.set(obj_id, obj_info)
          } else {
            reclaimed_size = reclaimed_size + obj_info.size
          }
        }
        None => {
          // 不可达对象，清理
          reclaimed_size = reclaimed_size + obj_info.size
        }
      }
    }
    
    return { cleaned_objects: cleaned_objects, reclaimed_size: reclaimed_size }
  }
  
  // 执行垃圾回收
  let reachable_objects = mark_reachable(objects)
  
  // 验证可达对象
  assert_true(reachable_objects.get("obj_001").unwrap())  // 根对象
  assert_true(reachable_objects.get("obj_002").unwrap())  // 被obj_001引用
  assert_true(reachable_objects.get("obj_003").unwrap())  // 被obj_001引用
  assert_true(reachable_objects.get("obj_004").unwrap())  // 被obj_002引用
  assert_true(reachable_objects.get("obj_005").unwrap())  // 被obj_004引用
  assert_false(reachable_objects.get("obj_006").unwrap()) // 不可达
  
  // 清理不可达对象
  let gc_result = sweep_unreachable(objects, reachable_objects)
  
  // 验证清理结果
  assert_eq(gc_result.cleaned_objects.size(), 5)  // 5个可达对象
  assert_eq(gc_result.reclaimed_size, 32)        // obj_006被清理，回收32字节
  
  // 验证清理后的对象集合
  assert_true(gc_result.cleaned_objects.contains("obj_001"))
  assert_true(gc_result.cleaned_objects.contains("obj_002"))
  assert_true(gc_result.cleaned_objects.contains("obj_003"))
  assert_true(gc_result.cleaned_objects.contains("obj_004"))
  assert_true(gc_result.cleaned_objects.contains("obj_005"))
  assert_false(gc_result.cleaned_objects.contains("obj_006"))
  
  // 测试循环引用检测
  let mut cyclic_objects = {
    "cycle_001": { size: 128, references: ["cycle_002"], is_root: true },
    "cycle_002": { size: 256, references: ["cycle_003"], is_root: false },
    "cycle_003": { size: 512, references: ["cycle_001"], is_root: false },  // 循环引用回到cycle_001
    "orphan_001": { size: 64, references: [], is_root: false }  // 孤立对象
  }
  
  // 执行垃圾回收处理循环引用
  let cyclic_reachable = mark_reachable(cyclic_objects)
  let cyclic_gc_result = sweep_unreachable(cyclic_objects, cyclic_reachable)
  
  // 验证循环引用处理
  assert_true(cyclic_reachable.get("cycle_001").unwrap())  // 根对象
  assert_true(cyclic_reachable.get("cycle_002").unwrap())  // 被cycle_001引用
  assert_true(cyclic_reachable.get("cycle_003").unwrap())  // 被cycle_002引用
  assert_false(cyclic_reachable.get("orphan_001").unwrap()) // 不可达
  
  assert_eq(cyclic_gc_result.cleaned_objects.size(), 3)  // 3个循环引用对象被保留
  assert_eq(cyclic_gc_result.reclaimed_size, 64)        // orphan_001被清理
}

// 测试4: 资源池管理
test "资源池管理" {
  // 模拟连接池
  let mut connection_pool = {
    max_connections: 10,
    active_connections: [],
    idle_connections: [],
    created_connections: 0,
    total_requests: 0,
    successful_requests: 0
  }
  
  // 从池中获取连接
  func acquire_connection(pool : { max_connections : Int, active_connections : Array[{ id : String, created_at : Int }], idle_connections : Array[{ id : String, created_at : Int }], created_connections : Int, total_requests : Int, successful_requests : Int }, timestamp : Int) -> { success : Bool, connection_id : String, updated_pool : { max_connections : Int, active_connections : Array[{ id : String, created_at : Int }], idle_connections : Array[{ id : String, created_at : Int }], created_connections : Int, total_requests : Int, successful_requests : Int } } {
    // 检查是否有空闲连接
    if pool.idle_connections.length() > 0 {
      let connection = pool.idle_connections[0]
      let remaining_idle = pool.idle_connections.slice(1)
      let new_active = pool.active_connections.push(connection)
      
      let updated_pool = {
        max_connections: pool.max_connections,
        active_connections: new_active,
        idle_connections: remaining_idle,
        created_connections: pool.created_connections,
        total_requests: pool.total_requests + 1,
        successful_requests: pool.successful_requests + 1
      }
      
      return { success: true, connection_id: connection.id, updated_pool: updated_pool }
    }
    
    // 检查是否可以创建新连接
    if pool.active_connections.length() < pool.max_connections {
      let new_connection_id = "conn_" + (pool.created_connections + 1).to_string()
      let new_connection = { id: new_connection_id, created_at: timestamp }
      let new_active = pool.active_connections.push(new_connection)
      
      let updated_pool = {
        max_connections: pool.max_connections,
        active_connections: new_active,
        idle_connections: pool.idle_connections,
        created_connections: pool.created_connections + 1,
        total_requests: pool.total_requests + 1,
        successful_requests: pool.successful_requests + 1
      }
      
      return { success: true, connection_id: new_connection_id, updated_pool: updated_pool }
    }
    
    // 无法获取连接
    let updated_pool = {
      max_connections: pool.max_connections,
      active_connections: pool.active_connections,
      idle_connections: pool.idle_connections,
      created_connections: pool.created_connections,
      total_requests: pool.total_requests + 1,
      successful_requests: pool.successful_requests
    }
    
    return { success: false, connection_id: "", updated_pool: updated_pool }
  }
  
  // 释放连接回池中
  func release_connection(pool : { max_connections : Int, active_connections : Array[{ id : String, created_at : Int }], idle_connections : Array[{ id : String, created_at : Int }], created_connections : Int, total_requests : Int, successful_requests : Int }, connection_id : String) : { max_connections : Int, active_connections : Array[{ id : String, created_at : Int }], idle_connections : Array[{ id : String, created_at : Int }], created_connections : Int, total_requests : Int, successful_requests : Int } {
    let mut new_active = []
    let mut released_connection = { id: "", created_at: 0 }
    let mut found = false
    
    for connection in pool.active_connections {
      if connection.id == connection_id {
        released_connection = connection
        found = true
      } else {
        new_active = new_active.push(connection)
      }
    }
    
    if found {
      let new_idle = pool.idle_connections.push(released_connection)
      
      return {
        max_connections: pool.max_connections,
        active_connections: new_active,
        idle_connections: new_idle,
        created_connections: pool.created_connections,
        total_requests: pool.total_requests,
        successful_requests: pool.successful_requests
      }
    }
    
    // 连接未找到，返回原始池
    return pool
  }
  
  // 测试连接获取和释放
  let acquire_result1 = acquire_connection(connection_pool, 1640995200)
  assert_true(acquire_result1.success)
  assert_eq(acquire_result1.connection_id, "conn_1")
  assert_eq(acquire_result1.updated_pool.active_connections.length(), 1)
  assert_eq(acquire_result1.updated_pool.total_requests, 1)
  
  connection_pool = acquire_result1.updated_pool
  
  let acquire_result2 = acquire_connection(connection_pool, 1640995210)
  assert_true(acquire_result2.success)
  assert_eq(acquire_result2.connection_id, "conn_2")
  assert_eq(acquire_result2.updated_pool.active_connections.length(), 2)
  
  connection_pool = acquire_result2.updated_pool
  
  // 释放连接
  connection_pool = release_connection(connection_pool, "conn_1")
  assert_eq(connection_pool.active_connections.length(), 1)
  assert_eq(connection_pool.idle_connections.length(), 1)
  assert_eq(connection_pool.idle_connections[0].id, "conn_1")
  
  // 再次获取连接（应该重用空闲连接）
  let acquire_result3 = acquire_connection(connection_pool, 1640995220)
  assert_true(acquire_result3.success)
  assert_eq(acquire_result3.connection_id, "conn_1")  // 重用连接
  assert_eq(acquire_result3.updated_pool.active_connections.length(), 2)
  assert_eq(acquire_result3.updated_pool.idle_connections.length(), 0)
  
  connection_pool = acquire_result3.updated_pool
  
  // 测试连接池满的情况
  let mut temp_pool = connection_pool
  
  // 填满连接池
  for i in range(0, 8) {  // 已有2个活跃连接，再添加8个达到上限
    let result = acquire_connection(temp_pool, 1640995230 + i)
    assert_true(result.success)
    temp_pool = result.updated_pool
  }
  
  assert_eq(temp_pool.active_connections.length(), 10)  // 达到最大连接数
  assert_eq(temp_pool.idle_connections.length(), 0)
  
  // 尝试获取超出限制的连接
  let overflow_result = acquire_connection(temp_pool, 1640995300)
  assert_false(overflow_result.success)
  assert_eq(overflow_result.updated_pool.total_requests, temp_pool.total_requests + 1)
  assert_eq(overflow_result.updated_pool.successful_requests, temp_pool.successful_requests)  // 成功请求数不变
  
  // 测试连接池统计
  let success_rate = connection_pool.successful_requests.to_float() / connection_pool.total_requests.to_float() * 100.0
  assert_eq(success_rate, 100.0)  // 所有请求都成功
}

// 测试5: 缓存管理
test "缓存管理" {
  // 模拟LRU缓存
  let mut lru_cache = {
    capacity: 3,
    entries: {},
    access_order: []
  }
  
  // 缓存访问函数
  func cache_access(cache : { capacity : Int, entries : Map[String, { value : String, size : Int, last_access : Int }], access_order : Array[String] }, key : String, value : String, size : Int, timestamp : Int) -> { value : String, hit : Bool, updated_cache : { capacity : Int, entries : Map[String, { value : String, size : Int, last_access : Int }], access_order : Array[String] } } {
    // 检查缓存命中
    match cache.entries.get(key) {
      Some(entry) => {
        // 缓存命中，更新访问时间
        let updated_entry = { value: entry.value, size: entry.size, last_access: timestamp }
        let updated_entries = cache.entries.set(key, updated_entry)
        
        // 更新访问顺序（移到末尾）
        let mut new_access_order = []
        for k in cache.access_order {
          if k != key {
            new_access_order = new_access_order.push(k)
          }
        }
        new_access_order = new_access_order.push(key)
        
        let updated_cache = {
          capacity: cache.capacity,
          entries: updated_entries,
          access_order: new_access_order
        }
        
        return { value: entry.value, hit: true, updated_cache: updated_cache }
      }
      None => {
        // 缓存未命中，添加新条目
        let new_entry = { value: value, size: size, last_access: timestamp }
        let mut updated_entries = cache.entries.set(key, new_entry)
        let mut new_access_order = cache.access_order.push(key)
        
        // 检查是否需要淘汰条目
        if updated_entries.size() > cache.capacity {
          // 淘汰最久未使用的条目（访问顺序的第一个）
          let lru_key = new_access_order[0]
          new_access_order = new_access_order.slice(1)
          updated_entries = updated_entries.delete(lru_key)
        }
        
        let updated_cache = {
          capacity: cache.capacity,
          entries: updated_entries,
          access_order: new_access_order
        }
        
        return { value: value, hit: false, updated_cache: updated_cache }
      }
    }
  }
  
  // 测试缓存访问
  let result1 = cache_access(lru_cache, "key1", "value1", 10, 1640995200)
  assert_false(result1.hit)
  assert_eq(result1.value, "value1")
  assert_eq(result1.updated_cache.entries.size(), 1)
  
  lru_cache = result1.updated_cache
  
  let result2 = cache_access(lru_cache, "key2", "value2", 20, 1640995210)
  assert_false(result2.hit)
  assert_eq(result2.value, "value2")
  assert_eq(result2.updated_cache.entries.size(), 2)
  
  lru_cache = result2.updated_cache
  
  let result3 = cache_access(lru_cache, "key3", "value3", 30, 1640995220)
  assert_false(result3.hit)
  assert_eq(result3.value, "value3")
  assert_eq(result3.updated_cache.entries.size(), 3)
  
  lru_cache = result3.updated_cache
  
  // 缓存命中
  let result4 = cache_access(lru_cache, "key2", "value2", 20, 1640995230)
  assert_true(result4.hit)
  assert_eq(result4.value, "value2")
  assert_eq(result4.updated_cache.entries.size(), 3)
  
  lru_cache = result4.updated_cache
  
  // 添加新条目，触发淘汰
  let result5 = cache_access(lru_cache, "key4", "value4", 40, 1640995240)
  assert_false(result5.hit)
  assert_eq(result5.value, "value4")
  assert_eq(result5.updated_cache.entries.size(), 3)
  
  // 验证key1被淘汰（最久未使用）
  assert_false(result5.updated_cache.entries.contains("key1"))
  assert_true(result5.updated_cache.entries.contains("key2"))
  assert_true(result5.updated_cache.entries.contains("key3"))
  assert_true(result5.updated_cache.entries.contains("key4"))
  
  lru_cache = result5.updated_cache
  
  // 测试缓存统计
  func get_cache_stats(cache : { capacity : Int, entries : Map[String, { value : String, size : Int, last_access : Int }], access_order : Array[String] }) -> { size : Int, capacity : Int, usage_percent : Float, total_memory_kb : Int } {
    let mut total_memory = 0
    
    for (_, entry) in cache.entries.to_array() {
      total_memory = total_memory + entry.size
    }
    
    return {
      size: cache.entries.size(),
      capacity: cache.capacity,
      usage_percent: cache.entries.size().to_float() / cache.capacity.to_float() * 100.0,
      total_memory_kb: total_memory
    }
  }
  
  let cache_stats = get_cache_stats(lru_cache)
  assert_eq(cache_stats.size, 3)
  assert_eq(cache_stats.capacity, 3)
  assert_eq(cache_stats.usage_percent, 100.0)
  assert_eq(cache_stats.total_memory_kb, 20 + 30 + 40)  // key2, key3, key4的大小
}

// 测试6: 资源生命周期管理
test "资源生命周期管理" {
  // 模拟资源生命周期状态
  type ResourceState = "initializing" | "active" | "idle" | "disposing" | "disposed"
  
  let mut resources = {
    "res_001": { state: "active", created_at: 1640995200, last_accessed: 1640995250, size: 1024 },
    "res_002": { state: "idle", created_at: 1640995100, last_accessed: 1640995200, size: 512 },
    "res_003": { state: "active", created_at: 1640995300, last_accessed: 1640995350, size: 2048 },
    "res_004": { state: "idle", created_at: 1640995000, last_accessed: 1640995100, size: 256 }
  }
  
  // 更新资源状态
  func update_resource_state(resources : Map[String, { state : ResourceState, created_at : Int, last_accessed : Int, size : Int }], resource_id : String, new_state : ResourceState, timestamp : Int) : Map[String, { state : ResourceState, created_at : Int, last_accessed : Int, size : Int }> {
    match resources.get(resource_id) {
      Some(resource) => {
        let updated_resource = {
          state: new_state,
          created_at: resource.created_at,
          last_accessed: timestamp,
          size: resource.size
        }
        return resources.set(resource_id, updated_resource)
      }
      None => resources
    }
  }
  
  // 查找空闲资源
  func find_idle_resources(resources : Map[String, { state : ResourceState, created_at : Int, last_accessed : Int, size : Int }], max_idle_time_ms : Int, current_time : Int) : Array[String] {
    let mut idle_resources = []
    
    for (resource_id, resource) in resources.to_array() {
      if resource.state == "idle" && (current_time - resource.last_accessed) > max_idle_time_ms {
        idle_resources = idle_resources.push(resource_id)
      }
    }
    
    return idle_resources
  }
  
  // 清理空闲资源
  func cleanup_idle_resources(resources : Map[String, { state : ResourceState, created_at : Int, last_accessed : Int, size : Int }], max_idle_time_ms : Int, current_time : Int) : { cleaned_resources : Map[String, { state : ResourceState, created_at : Int, last_accessed : Int, size : Int }], disposed_resources : Array[String], reclaimed_size : Int } {
    let idle_resources = find_idle_resources(resources, max_idle_time_ms, current_time)
    let mut cleaned_resources = resources
    let mut reclaimed_size = 0
    
    for resource_id in idle_resources {
      // 标记为 disposing
      cleaned_resources = update_resource_state(cleaned_resources, resource_id, "disposing", current_time)
      
      // 计算回收大小
      match cleaned_resources.get(resource_id) {
        Some(resource) => reclaimed_size = reclaimed_size + resource.size,
        None => ()
      }
      
      // 标记为 disposed
      cleaned_resources = update_resource_state(cleaned_resources, resource_id, "disposed", current_time)
      
      // 从资源集合中移除
      cleaned_resources = cleaned_resources.delete(resource_id)
    }
    
    return {
      cleaned_resources: cleaned_resources,
      disposed_resources: idle_resources,
      reclaimed_size: reclaimed_size
    }
  }
  
  // 测试资源状态更新
  resources = update_resource_state(resources, "res_001", "idle", 1640995400)
  assert_eq(resources.get("res_001").unwrap().state, "idle")
  assert_eq(resources.get("res_001").unwrap().last_accessed, 1640995400)
  
  // 测试空闲资源查找
  let idle_resources = find_idle_resources(resources, 300000, 1640995600)  // 5分钟空闲时间
  assert_eq(idle_resources.length(), 2)  // res_002和res_04空闲超过5分钟
  
  // 测试资源清理
  let cleanup_result = cleanup_idle_resources(resources, 300000, 1640995600)
  
  // 验证清理结果
  assert_eq(cleanup_result.disposed_resources.length(), 2)
  assert_true(cleanup_result.disposed_resources.contains("res_002"))
  assert_true(cleanup_result.disposed_resources.contains("res_004"))
  assert_eq(cleanup_result.reclaimed_size, 512 + 256)  // res_002和res_004的大小
  
  // 验证清理后的资源集合
  assert_eq(cleanup_result.cleaned_resources.size(), 2)  // 只剩下res_001和res_003
  assert_true(cleanup_result.cleaned_resources.contains("res_001"))
  assert_true(cleanup_result.cleaned_resources.contains("res_003"))
  
  // 测试资源生命周期统计
  func get_resource_lifecycle_stats(resources : Map[String, { state : ResourceState, created_at : Int, last_accessed : Int, size : Int }]) -> { total_resources : Int, active_resources : Int, idle_resources : Int, total_size_kb : Int } {
    let mut active_count = 0
    let mut idle_count = 0
    let mut total_size = 0
    
    for (_, resource) in resources.to_array() {
      total_size = total_size + resource.size
      
      match resource.state {
        "active" => active_count = active_count + 1,
        "idle" => idle_count = idle_count + 1,
        _ => ()
      }
    }
    
    return {
      total_resources: resources.size(),
      active_resources: active_count,
      idle_resources: idle_count,
      total_size_kb: total_size
    }
  }
  
  let original_stats = get_resource_lifecycle_stats(resources)
  assert_eq(original_stats.total_resources, 4)
  assert_eq(original_stats.active_resources, 2)
  assert_eq(original_stats.idle_resources, 2)
  assert_eq(original_stats.total_size_kb, 1024 + 512 + 2048 + 256)
  
  let cleaned_stats = get_resource_lifecycle_stats(cleanup_result.cleaned_resources)
  assert_eq(cleaned_stats.total_resources, 2)
  assert_eq(cleaned_stats.active_resources, 1)  // res_001现在是idle
  assert_eq(cleaned_stats.idle_resources, 1)    // res_003仍然是active
  assert_eq(cleaned_stats.total_size_kb, 1024 + 2048)
}