// Azimuth Premium Memory Management Tests
// 高级内存管理测试用例，专注于遥测系统的内存优化和资源管理

// 测试 1: 内存池管理
test "memory pool management" {
  // 定义内存块大小分类
  enum MemoryBlockSize {
    Small    // 64字节
    Medium   // 256字节
    Large    // 1KB
    XLarge   // 4KB
  }
  
  // 定义内存块
  type MemoryBlock = {
    id: Int,
    size: MemoryBlockSize,
    data: Array[Int],
    in_use: Bool,
    allocated_at: Int,
    last_accessed: Int
  }
  
  // 定义内存池
  type MemoryPool = {
    small_blocks: Array[MemoryBlock],
    medium_blocks: Array[MemoryBlock],
    large_blocks: Array[MemoryBlock],
    xlarge_blocks: Array[MemoryBlock],
    total_allocated: Int,
    total_in_use: Int,
    next_id: Int
  }
  
  // 创建内存池
  let create_memory_pool = fn(small_count: Int, medium_count: Int, large_count: Int, xlarge_count: Int) {
    let mut small_blocks = []
    let mut medium_blocks = []
    let mut large_blocks = []
    let mut xlarge_blocks = []
    
    // 创建小内存块
    for i in 0..small_count {
      small_blocks = small_blocks.push({
        id: i,
        size: MemoryBlockSize::Small,
        data: Array::make(16, 0),  // 16 * 4 = 64字节
        in_use: false,
        allocated_at: 0,
        last_accessed: 0
      })
    }
    
    // 创建中等内存块
    for i in 0..medium_count {
      medium_blocks = medium_blocks.push({
        id: small_count + i,
        size: MemoryBlockSize::Medium,
        data: Array::make(64, 0),  // 64 * 4 = 256字节
        in_use: false,
        allocated_at: 0,
        last_accessed: 0
      })
    }
    
    // 创建大内存块
    for i in 0..large_count {
      large_blocks = large_blocks.push({
        id: small_count + medium_count + i,
        size: MemoryBlockSize::Large,
        data: Array::make(256, 0),  // 256 * 4 = 1KB
        in_use: false,
        allocated_at: 0,
        last_accessed: 0
      })
    }
    
    // 创建超大内存块
    for i in 0..xlarge_count {
      xlarge_blocks = xlarge_blocks.push({
        id: small_count + medium_count + large_count + i,
        size: MemoryBlockSize::XLarge,
        data: Array::make(1024, 0),  // 1024 * 4 = 4KB
        in_use: false,
        allocated_at: 0,
        last_accessed: 0
      })
    }
    
    {
      small_blocks,
      medium_blocks,
      large_blocks,
      xlarge_blocks,
      total_allocated: 0,
      total_in_use: 0,
      next_id: small_count + medium_count + large_count + xlarge_count
    }
  }
  
  // 分配内存块
  let allocate_block = fn(pool: MemoryPool, size: MemoryBlockSize, current_time: Int) {
    let mut blocks = match size {
      MemoryBlockSize::Small => pool.small_blocks
      MemoryBlockSize::Medium => pool.medium_blocks
      MemoryBlockSize::Large => pool.large_blocks
      MemoryBlockSize::XLarge => pool.xlarge_blocks
    }
    
    // 查找空闲块
    let mut found_index = -1
    for i in 0..blocks.length() {
      if not blocks[i].in_use {
        found_index = i
        break
      }
    }
    
    if found_index >= 0 {
      // 找到空闲块，标记为使用中
      let block = blocks[found_index]
      let updated_block = {
        id: block.id,
        size: block.size,
        data: block.data,
        in_use: true,
        allocated_at: current_time,
        last_accessed: current_time
      }
      
      blocks[found_index] = updated_block
      
      // 更新池状态
      let updated_pool = match size {
        MemoryBlockSize::Small => {
          {
            small_blocks: blocks,
            medium_blocks: pool.medium_blocks,
            large_blocks: pool.large_blocks,
            xlarge_blocks: pool.xlarge_blocks,
            total_allocated: pool.total_allocated + 1,
            total_in_use: pool.total_in_use + 1,
            next_id: pool.next_id
          }
        }
        MemoryBlockSize::Medium => {
          {
            small_blocks: pool.small_blocks,
            medium_blocks: blocks,
            large_blocks: pool.large_blocks,
            xlarge_blocks: pool.xlarge_blocks,
            total_allocated: pool.total_allocated + 1,
            total_in_use: pool.total_in_use + 1,
            next_id: pool.next_id
          }
        }
        MemoryBlockSize::Large => {
          {
            small_blocks: pool.small_blocks,
            medium_blocks: pool.medium_blocks,
            large_blocks: blocks,
            xlarge_blocks: pool.xlarge_blocks,
            total_allocated: pool.total_allocated + 1,
            total_in_use: pool.total_in_use + 1,
            next_id: pool.next_id
          }
        }
        MemoryBlockSize::XLarge => {
          {
            small_blocks: pool.small_blocks,
            medium_blocks: pool.medium_blocks,
            large_blocks: pool.large_blocks,
            xlarge_blocks: blocks,
            total_allocated: pool.total_allocated + 1,
            total_in_use: pool.total_in_use + 1,
            next_id: pool.next_id
          }
        }
      }
      
      (updated_pool, Some(updated_block))
    } else {
      // 没有可用块
      (pool, None)
    }
  }
  
  // 释放内存块
  let deallocate_block = fn(pool: MemoryPool, block_id: Int, current_time: Int) {
    // 在所有块类型中查找
    let mut found = false
    let mut updated_pool = pool
    
    // 检查小块
    if not found {
      let mut blocks = pool.small_blocks
      for i in 0..blocks.length() {
        if blocks[i].id == block_id && blocks[i].in_use {
          let block = blocks[i]
          let updated_block = {
            id: block.id,
            size: block.size,
            data: block.data,
            in_use: false,
            allocated_at: block.allocated_at,
            last_accessed: current_time
          }
          
          blocks[i] = updated_block
          updated_pool = {
            small_blocks: blocks,
            medium_blocks: pool.medium_blocks,
            large_blocks: pool.large_blocks,
            xlarge_blocks: pool.xlarge_blocks,
            total_allocated: pool.total_allocated,
            total_in_use: pool.total_in_use - 1,
            next_id: pool.next_id
          }
          found = true
          break
        }
      }
    }
    
    // 检查中等块
    if not found {
      let mut blocks = pool.medium_blocks
      for i in 0..blocks.length() {
        if blocks[i].id == block_id && blocks[i].in_use {
          let block = blocks[i]
          let updated_block = {
            id: block.id,
            size: block.size,
            data: block.data,
            in_use: false,
            allocated_at: block.allocated_at,
            last_accessed: current_time
          }
          
          blocks[i] = updated_block
          updated_pool = {
            small_blocks: pool.small_blocks,
            medium_blocks: blocks,
            large_blocks: pool.large_blocks,
            xlarge_blocks: pool.xlarge_blocks,
            total_allocated: pool.total_allocated,
            total_in_use: pool.total_in_use - 1,
            next_id: pool.next_id
          }
          found = true
          break
        }
      }
    }
    
    // 检查大块
    if not found {
      let mut blocks = pool.large_blocks
      for i in 0..blocks.length() {
        if blocks[i].id == block_id && blocks[i].in_use {
          let block = blocks[i]
          let updated_block = {
            id: block.id,
            size: block.size,
            data: block.data,
            in_use: false,
            allocated_at: block.allocated_at,
            last_accessed: current_time
          }
          
          blocks[i] = updated_block
          updated_pool = {
            small_blocks: pool.small_blocks,
            medium_blocks: pool.medium_blocks,
            large_blocks: blocks,
            xlarge_blocks: pool.xlarge_blocks,
            total_allocated: pool.total_allocated,
            total_in_use: pool.total_in_use - 1,
            next_id: pool.next_id
          }
          found = true
          break
        }
      }
    }
    
    // 检查超大块
    if not found {
      let mut blocks = pool.xlarge_blocks
      for i in 0..blocks.length() {
        if blocks[i].id == block_id && blocks[i].in_use {
          let block = blocks[i]
          let updated_block = {
            id: block.id,
            size: block.size,
            data: block.data,
            in_use: false,
            allocated_at: block.allocated_at,
            last_accessed: current_time
          }
          
          blocks[i] = updated_block
          updated_pool = {
            small_blocks: pool.small_blocks,
            medium_blocks: pool.medium_blocks,
            large_blocks: pool.large_blocks,
            xlarge_blocks: blocks,
            total_allocated: pool.total_allocated,
            total_in_use: pool.total_in_use - 1,
            next_id: pool.next_id
          }
          found = true
          break
        }
      }
    }
    
    updated_pool
  }
  
  // 测试内存池
  let pool = create_memory_pool(5, 3, 2, 1)
  let mut current_time = 1000
  
  // 初始状态
  assert_eq(pool.total_allocated, 0)
  assert_eq(pool.total_in_use, 0)
  
  // 分配小块
  let (pool1, block1) = allocate_block(pool, MemoryBlockSize::Small, current_time)
  assert_true(block1.is_some())
  assert_eq(pool1.total_allocated, 1)
  assert_eq(pool1.total_in_use, 1)
  
  // 分配更多块
  current_time = current_time + 100
  let (pool2, block2) = allocate_block(pool1, MemoryBlockSize::Small, current_time)
  let (pool3, block3) = allocate_block(pool2, MemoryBlockSize::Medium, current_time)
  let (pool4, block4) = allocate_block(pool3, MemoryBlockSize::Large, current_time)
  
  assert_eq(pool4.total_allocated, 3)
  assert_eq(pool4.total_in_use, 3)
  
  // 释放块
  current_time = current_time + 100
  match block1 {
    Some(b) => {
      let pool5 = deallocate_block(pool4, b.id, current_time)
      assert_eq(pool5.total_in_use, 2)
    }
    None => assert_true(false)
  }
  
  // 重新分配块（应该重用已释放的块）
  current_time = current_time + 100
  let (pool6, block5) = allocate_block(pool4, MemoryBlockSize::Small, current_time)
  assert_true(block5.is_some())
  
  // 分配所有小块
  let mut pool_current = pool6
  for i in 0..3 {
    current_time = current_time + 100
    let (new_pool, block) = allocate_block(pool_current, MemoryBlockSize::Small, current_time)
    pool_current = new_pool
    assert_true(block.is_some())
  }
  
  // 尝试分配超出限制的块
  let (pool_final, block_excess) = allocate_block(pool_current, MemoryBlockSize::Small, current_time)
  assert_true(block_excess.is_none())
}

// 测试 2: 垃圾回收机制
test "garbage collection mechanism" {
  // 定义对象引用
  type ObjectRef = {
    id: Int,
    ref_count: Int,
    size: Int,
    last_accessed: Int,
    marked: Bool
  }
  
  // 定义垃圾收集器
  type GarbageCollector = {
    objects: Map[Int, ObjectRef],
    roots: Set[Int],
    total_memory: Int,
    gc_threshold: Int,
    last_gc_time: Int
  }
  
  // 创建垃圾收集器
  let create_garbage_collector = fn(gc_threshold: Int) {
    {
      objects: Map::empty(),
      roots: Set::empty(),
      total_memory: 0,
      gc_threshold,
      last_gc_time: 0
    }
  }
  
  // 创建对象
  let create_object = fn(gc: GarbageCollector, size: Int, current_time: Int) {
    let id = gc.objects.size()
    let obj_ref = {
      id,
      ref_count: 1,
      size,
      last_accessed: current_time,
      marked: false
    }
    
    let _ = Map::insert(gc.objects, id, obj_ref)
    
    {
      objects: gc.objects,
      roots: gc.roots,
      total_memory: gc.total_memory + size,
      gc_threshold: gc.gc_threshold,
      last_gc_time: gc.last_gc_time
    }
  }
  
  // 添加引用
  let add_reference = fn(gc: GarbageCollector, obj_id: Int) {
    match Map::get(gc.objects, obj_id) {
      Some(obj) => {
        let updated_obj = {
          id: obj.id,
          ref_count: obj.ref_count + 1,
          size: obj.size,
          last_accessed: obj.last_accessed,
          marked: obj.marked
        }
        
        let _ = Map::insert(gc.objects, obj_id, updated_obj)
        
        {
          objects: gc.objects,
          roots: gc.roots,
          total_memory: gc.total_memory,
          gc_threshold: gc.gc_threshold,
          last_gc_time: gc.last_gc_time
        }
      }
      None => {
        gc
      }
    }
  }
  
  // 移除引用
  let remove_reference = fn(gc: GarbageCollector, obj_id: Int) {
    match Map::get(gc.objects, obj_id) {
      Some(obj) => {
        let updated_obj = {
          id: obj.id,
          ref_count: obj.ref_count - 1,
          size: obj.size,
          last_accessed: obj.last_accessed,
          marked: obj.marked
        }
        
        let _ = Map::insert(gc.objects, obj_id, updated_obj)
        
        {
          objects: gc.objects,
          roots: gc.roots,
          total_memory: gc.total_memory,
          gc_threshold: gc.gc_threshold,
          last_gc_time: gc.last_gc_time
        }
      }
      None => {
        gc
      }
    }
  }
  
  // 添加根对象
  let add_root = fn(gc: GarbageCollector, obj_id: Int) {
    {
      objects: gc.objects,
      roots: Set::add(gc.roots, obj_id),
      total_memory: gc.total_memory,
      gc_threshold: gc.gc_threshold,
      last_gc_time: gc.last_gc_time
    }
  }
  
  // 执行垃圾回收
  let perform_gc = fn(gc: GarbageCollector, current_time: Int) {
    // 标记阶段
    let mut marked_objects = Set::empty()
    let mut work_list = Set::to_array(gc.roots)
    
    while work_list.length() > 0 {
      let obj_id = work_list[0]
      work_list = work_list.slice(1)
      
      if not Set::contains(marked_objects, obj_id) {
        marked_objects = Set::add(marked_objects, obj_id)
        
        // 在实际实现中，这里会遍历对象的引用并添加到工作列表
        // 简化实现，我们只标记对象本身
      }
    }
    
    // 清除阶段
    let mut new_objects = gc.objects
    let mut freed_memory = 0
    let mut objects_to_remove = []
    
    for (obj_id, obj) in gc.objects {
      if not Set::contains(marked_objects, obj_id) && obj.ref_count <= 0 {
        objects_to_remove = objects_to_remove.push(obj_id)
        freed_memory = freed_memory + obj.size
      }
    }
    
    for obj_id in objects_to_remove {
      let _ = Map::remove(new_objects, obj_id)
    }
    
    {
      objects: new_objects,
      roots: gc.roots,
      total_memory: gc.total_memory - freed_memory,
      gc_threshold: gc.gc_threshold,
      last_gc_time: current_time
    }
  }
  
  // 检查是否需要垃圾回收
  let should_gc = fn(gc: GarbageCollector) {
    gc.total_memory > gc.gc_threshold
  }
  
  // 测试垃圾收集器
  let gc = create_garbage_collector(1000)  // 1KB阈值
  let mut current_time = 1000
  
  // 创建对象
  let gc1 = create_object(gc, 100, current_time)
  let gc2 = create_object(gc1, 200, current_time)
  let gc3 = create_object(gc2, 300, current_time)
  
  // 添加根对象
  let gc4 = add_root(gc3, 0)
  let gc5 = add_root(gc4, 2)
  
  // 添加引用
  let gc6 = add_reference(gc5, 1)
  
  // 移除引用
  let gc7 = remove_reference(gc6, 1)
  
  // 检查内存使用
  assert_eq(gc7.total_memory, 600)  // 100 + 200 + 300
  assert_false(should_gc(gc7))
  
  // 创建更多对象直到超过阈值
  let gc8 = create_object(gc7, 500, current_time)
  
  // 现在应该需要垃圾回收
  assert_eq(gc8.total_memory, 1100)  // 600 + 500
  assert_true(should_gc(gc8))
  
  // 执行垃圾回收
  let gc9 = perform_gc(gc8, current_time)
  
  // 对象1（ID=1）引用计数为0且不是根对象，应该被回收
  assert_eq(gc9.total_memory, 900)  // 1100 - 200
  assert_false(should_gc(gc9))
}

// 测试 3: 内存泄漏检测
test "memory leak detection" {
  // 定义内存分配记录
  type AllocationRecord = {
    id: Int,
    size: Int,
    allocated_at: Int,
    freed_at: Option[Int],
    stack_trace: Array[String]
  }
  
  // 定义内存泄漏检测器
  type MemoryLeakDetector = {
    allocations: Map[Int, AllocationRecord],
    next_id: Int,
    total_allocated: Int,
    total_freed: Int,
    current_usage: Int,
    peak_usage: Int
  }
  
  // 创建内存泄漏检测器
  let create_memory_leak_detector = fn() {
    {
      allocations: Map::empty(),
      next_id: 0,
      total_allocated: 0,
      total_freed: 0,
      current_usage: 0,
      peak_usage: 0
    }
  }
  
  // 记录分配
  let record_allocation = fn(detector: MemoryLeakDetector, size: Int, current_time: Int, stack_trace: Array[String>) {
    let id = detector.next_id
    let record = {
      id,
      size,
      allocated_at: current_time,
      freed_at: None,
      stack_trace
    }
    
    let _ = Map::insert(detector.allocations, id, record)
    
    let new_current_usage = detector.current_usage + size
    let new_peak_usage = new_current_usage.max(detector.peak_usage)
    
    {
      allocations: detector.allocations,
      next_id: detector.next_id + 1,
      total_allocated: detector.total_allocated + size,
      total_freed: detector.total_freed,
      current_usage: new_current_usage,
      peak_usage: new_peak_usage
    }
  }
  
  // 记录释放
  let record_deallocation = fn(detector: MemoryLeakDetector, id: Int, current_time: Int) {
    match Map::get(detector.allocations, id) {
      Some(record) => {
        let updated_record = {
          id: record.id,
          size: record.size,
          allocated_at: record.allocated_at,
          freed_at: Some(current_time),
          stack_trace: record.stack_trace
        }
        
        let _ = Map::insert(detector.allocations, id, updated_record)
        
        {
          allocations: detector.allocations,
          next_id: detector.next_id,
          total_allocated: detector.total_allocated,
          total_freed: detector.total_freed + record.size,
          current_usage: detector.current_usage - record.size,
          peak_usage: detector.peak_usage
        }
      }
      None => {
        detector
      }
    }
  }
  
  // 检测内存泄漏
  let detect_leaks = fn(detector: MemoryLeakDetector, current_time: Int) {
    let mut leaks = []
    
    for (_, record) in detector.allocations {
      match record.freed_at {
        None => {
          // 未释放的分配
          leaks = leaks.push({
            id: record.id,
            size: record.size,
            lifetime: current_time - record.allocated_at,
            stack_trace: record.stack_trace
          })
        }
        Some(_) => {
          // 已释放，不视为泄漏
        }
      }
    }
    
    leaks
  }
  
  // 获取内存统计
  let get_memory_stats = fn(detector: MemoryLeakDetector) {
    {
      total_allocated: detector.total_allocated,
      total_freed: detector.total_freed,
      current_usage: detector.current_usage,
      peak_usage: detector.peak_usage,
      leak_count: detector.allocations.filter(fn(_, record) { record.freed_at.is_none() }).size()
    }
  }
  
  // 测试内存泄漏检测
  let detector = create_memory_leak_detector()
  let mut current_time = 1000
  
  // 记录分配
  let detector1 = record_allocation(detector, 100, current_time, ["function1", "line1"])
  let detector2 = record_allocation(detector1, 200, current_time + 100, ["function2", "line2"])
  let detector3 = record_allocation(detector2, 150, current_time + 200, ["function3", "line3"])
  
  // 检查内存统计
  let stats1 = get_memory_stats(detector3)
  assert_eq(stats1.total_allocated, 450)
  assert_eq(stats1.total_freed, 0)
  assert_eq(stats1.current_usage, 450)
  assert_eq(stats1.peak_usage, 450)
  assert_eq(stats1.leak_count, 3)
  
  // 释放一些分配
  let detector4 = record_deallocation(detector3, 1, current_time + 300)
  let detector5 = record_deallocation(detector4, 3, current_time + 400)
  
  // 检查内存统计
  let stats2 = get_memory_stats(detector5)
  assert_eq(stats2.total_allocated, 450)
  assert_eq(stats2.total_freed, 250)  // 100 + 150
  assert_eq(stats2.current_usage, 200)  // 450 - 250
  assert_eq(stats2.peak_usage, 450)
  assert_eq(stats2.leak_count, 1)
  
  // 检测内存泄漏
  current_time = current_time + 500
  let leaks = detect_leaks(detector5, current_time)
  
  assert_eq(leaks.length(), 1)
  assert_eq(leaks[0].id, 2)
  assert_eq(leaks[0].size, 200)
  assert_eq(leaks[0].lifetime, 400)  // 从1100到1500
  assert_eq(leaks[0].stack_trace, ["function2", "line2"])
}

// 测试 4: 内存使用优化
test "memory usage optimization" {
  // 定义内存使用优化器
  type MemoryOptimizer = {
    buffer_pools: Map[Int, Array[Array[Int]]>,  // 大小 -> 缓冲区池
    string_interns: Map[String, String>,         // 字符串驻留
    compressed_data: Map[String, Array[Int>>,    // 压缩数据
    memory_savings: Int
  }
  
  // 创建内存优化器
  let create_memory_optimizer = fn() {
    {
      buffer_pools: Map::empty(),
      string_interns: Map::empty(),
      compressed_data: Map::empty(),
      memory_savings: 0
    }
  }
  
  // 获取缓冲区
  let get_buffer = fn(optimizer: MemoryOptimizer, size: Int) {
    match Map::get(optimizer.buffer_pools, size) {
      Some(pool) => {
        if pool.length() > 0 {
          // 从池中取出缓冲区
          let buffer = pool[0]
          let remaining_pool = pool.slice(1)
          let _ = Map::insert(optimizer.buffer_pools, size, remaining_pool)
          
          // 模拟内存节省：重用缓冲区节省了分配新缓冲区的开销
          let memory_savings = size * 4  // 假设每个Int占4字节
          
          ({
            buffer_pools: optimizer.buffer_pools,
            string_interns: optimizer.string_interns,
            compressed_data: optimizer.compressed_data,
            memory_savings: optimizer.memory_savings + memory_savings
          }, buffer)
        } else {
          // 池中没有可用缓冲区，创建新的
          let new_buffer = Array::make(size, 0)
          
          (optimizer, new_buffer)
        }
      }
      None => {
        // 没有对应大小的池，创建新的缓冲区和池
        let new_buffer = Array::make(size, 0)
        let _ = Map::insert(optimizer.buffer_pools, size, [])
        
        (optimizer, new_buffer)
      }
    }
  }
  
  // 返回缓冲区
  let return_buffer = fn(optimizer: MemoryOptimizer, buffer: Array[Int>) {
    let size = buffer.length()
    
    match Map::get(optimizer.buffer_pools, size) {
      Some(pool) => {
        // 将缓冲区添加到池中
        let updated_pool = pool.push(buffer)
        let _ = Map::insert(optimizer.buffer_pools, size, updated_pool)
        
        {
          buffer_pools: optimizer.buffer_pools,
          string_interns: optimizer.string_interns,
          compressed_data: optimizer.compressed_data,
          memory_savings: optimizer.memory_savings
        }
      }
      None => {
        // 创建新的池并添加缓冲区
        let _ = Map::insert(optimizer.buffer_pools, size, [buffer])
        
        {
          buffer_pools: optimizer.buffer_pools,
          string_interns: optimizer.string_interns,
          compressed_data: optimizer.compressed_data,
          memory_savings: optimizer.memory_savings
        }
      }
    }
  }
  
  // 字符串驻留
  let intern_string = fn(optimizer: MemoryOptimizer, s: String) {
    match Map::get(optimizer.string_interns, s) {
      Some(interned) => {
        // 字符串已驻留，节省内存
        let memory_savings = s.length() * 2  // 假设每个字符占2字节
        
        ({
          buffer_pools: optimizer.buffer_pools,
          string_interns: optimizer.string_interns,
          compressed_data: optimizer.compressed_data,
          memory_savings: optimizer.memory_savings + memory_savings
        }, interned)
      }
      None => {
        // 字符串未驻留，添加到驻留池
        let _ = Map::insert(optimizer.string_interns, s, s)
        
        (optimizer, s)
      }
    }
  }
  
  // 简化的压缩函数（行程编码）
  let compress_data = fn(data: Array[Int]) {
    if data.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut current = data[0]
    let mut count = 1
    
    for i in 1..data.length() {
      if data[i] == current && count < 255 {
        count = count + 1
      } else {
        result = result.push(current)
        result = result.push(count)
        current = data[i]
        count = 1
      }
    }
    
    // 添加最后一组
    result = result.push(current)
    result = result.push(count)
    
    result
  }
  
  // 存储压缩数据
  let store_compressed = fn(optimizer: MemoryOptimizer, key: String, data: Array[Int>) {
    let compressed = compress_data(data)
    let _ = Map::insert(optimizer.compressed_data, key, compressed)
    
    // 计算节省的内存
    let original_size = data.length() * 4  // 假设每个Int占4字节
    let compressed_size = compressed.length() * 4
    let memory_savings = original_size - compressed_size
    
    {
      buffer_pools: optimizer.buffer_pools,
      string_interns: optimizer.string_interns,
      compressed_data: optimizer.compressed_data,
      memory_savings: optimizer.memory_savings + memory_savings.max(0)
    }
  }
  
  // 测试内存优化
  let optimizer = create_memory_optimizer()
  
  // 测试缓冲区池
  let (optimizer1, buffer1) = get_buffer(optimizer, 100)
  let (optimizer2, buffer2) = get_buffer(optimizer1, 100)
  
  // 返回缓冲区
  let optimizer3 = return_buffer(optimizer2, buffer1)
  let optimizer4 = return_buffer(optimizer3, buffer2)
  
  // 再次获取缓冲区，应该重用
  let (optimizer5, buffer3) = get_buffer(optimizer4, 100)
  
  // 验证内存节省
  assert_true(optimizer5.memory_savings > 0)
  
  // 测试字符串驻留
  let (optimizer6, str1) = intern_string(optimizer5, "common_string")
  let (optimizer7, str2) = intern_string(optimizer6, "common_string")
  let (optimizer8, str3) = intern_string(optimizer7, "unique_string")
  
  // 验证字符串驻留节省
  assert_true(optimizer8.memory_savings > optimizer5.memory_savings)
  
  // 测试数据压缩
  let test_data = [1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5]
  let optimizer9 = store_compressed(optimizer8, "test_key", test_data)
  
  // 验证压缩节省
  assert_true(optimizer9.memory_savings > optimizer8.memory_savings)
  
  // 验证总内存节省
  assert_true(optimizer9.memory_savings > 0)
}

// 测试 5: 内存使用监控
test "memory usage monitoring" {
  // 定义内存使用快照
  type MemorySnapshot = {
    timestamp: Int,
    heap_used: Int,
    heap_total: Int,
    non_heap_used: Int,
    non_heap_total: Int,
    direct_memory: Int,
    gc_count: Int,
    gc_time: Int
  }
  
  // 定义内存监控器
  type MemoryMonitor = {
    snapshots: Array[MemorySnapshot],
    alert_thresholds: {
      heap_usage: Float,
      gc_frequency: Float,
      memory_growth: Float
    },
    alerts: Array[String]
  }
  
  // 创建内存监控器
  let create_memory_monitor = fn(heap_threshold: Float, gc_threshold: Float, growth_threshold: Float) {
    {
      snapshots: [],
      alert_thresholds: {
        heap_usage: heap_threshold,
        gc_frequency: gc_threshold,
        memory_growth: growth_threshold
      },
      alerts: []
    }
  }
  
  // 记录内存快照
  let record_snapshot = fn(monitor: MemoryMonitor, timestamp: Int, heap_used: Int, heap_total: Int, 
                           non_heap_used: Int, non_heap_total: Int, direct_memory: Int, 
                           gc_count: Int, gc_time: Int) {
    let snapshot = {
      timestamp,
      heap_used,
      heap_total,
      non_heap_used,
      non_heap_total,
      direct_memory,
      gc_count,
      gc_time
    }
    
    let updated_snapshots = monitor.snapshots.push(snapshot)
    
    // 检查警报条件
    let mut new_alerts = monitor.alerts
    
    // 检查堆使用率
    let heap_usage_ratio = (heap_used as Float) / (heap_total as Float)
    if heap_usage_ratio > monitor.alert_thresholds.heap_usage {
      new_alerts = new_alerts.push("堆内存使用率过高: " + (heap_usage_ratio * 100.0).to_string() + "%")
    }
    
    // 检查GC频率（如果有多个快照）
    if updated_snapshots.length() >= 2 {
      let prev_snapshot = updated_snapshots[updated_snapshots.length() - 2]
      let gc_diff = gc_count - prev_snapshot.gc_count
      let time_diff = timestamp - prev_snapshot.timestamp
      
      if time_diff > 0 {
        let gc_rate = (gc_diff as Float) / (time_diff as Float / 1000.0)  // GC次数/秒
        if gc_rate > monitor.alert_thresholds.gc_frequency {
          new_alerts = new_alerts.push("GC频率过高: " + gc_rate.to_string() + " 次/秒")
        }
      }
      
      // 检查内存增长率
      let heap_growth = heap_used - prev_snapshot.heap_used
      let growth_rate = (heap_growth as Float) / (time_diff as Float / 1000.0)  // 字节/秒
      
      if growth_rate > monitor.alert_thresholds.memory_growth {
        new_alerts = new_alerts.push("内存增长过快: " + growth_rate.to_string() + " 字节/秒")
      }
    }
    
    {
      snapshots: updated_snapshots,
      alert_thresholds: monitor.alert_thresholds,
      alerts: new_alerts
    }
  }
  
  // 获取内存使用趋势
  let get_memory_trend = fn(monitor: MemoryMonitor, window_size: Int) {
    if monitor.snapshots.length() < window_size {
      return {
        heap_growth_rate: 0.0,
        gc_frequency: 0.0,
        average_heap_usage: 0.0
      }
    }
    
    let recent_snapshots = monitor.snapshots.slice(monitor.snapshots.length() - window_size)
    
    // 计算堆增长率
    let first_snapshot = recent_snapshots[0]
    let last_snapshot = recent_snapshots[recent_snapshots.length() - 1]
    let time_diff = last_snapshot.timestamp - first_snapshot.timestamp
    let heap_growth = last_snapshot.heap_used - first_snapshot.heap_used
    
    let heap_growth_rate = if time_diff > 0 {
      (heap_growth as Float) / (time_diff as Float / 1000.0)  // 字节/秒
    } else {
      0.0
    }
    
    // 计算GC频率
    let gc_diff = last_snapshot.gc_count - first_snapshot.gc_count
    let gc_frequency = if time_diff > 0 {
      (gc_diff as Float) / (time_diff as Float / 1000.0)  // GC次数/秒
    } else {
      0.0
    }
    
    // 计算平均堆使用率
    let mut total_heap_usage = 0.0
    for snapshot in recent_snapshots {
      total_heap_usage = total_heap_usage + ((snapshot.heap_used as Float) / (snapshot.heap_total as Float))
    }
    let average_heap_usage = total_heap_usage / (recent_snapshots.length() as Float)
    
    {
      heap_growth_rate,
      gc_frequency,
      average_heap_usage
    }
  }
  
  // 测试内存监控
  let monitor = create_memory_monitor(0.8, 5.0, 1024000.0)  // 80%堆使用率，5次GC/秒，1MB/秒增长
  
  // 记录初始快照
  let monitor1 = record_snapshot(monitor, 1000, 500, 1000, 200, 300, 100, 0, 0)
  
  // 记录正常使用快照
  let monitor2 = record_snapshot(monitor1, 2000, 600, 1000, 200, 300, 100, 1, 100)
  let monitor3 = record_snapshot(monitor2, 3000, 700, 1000, 200, 300, 100, 2, 200)
  
  // 验证没有警报
  assert_eq(monitor3.alerts.length(), 0)
  
  // 记录高堆使用率快照
  let monitor4 = record_snapshot(monitor3, 4000, 850, 1000, 200, 300, 100, 3, 300)
  
  // 验证堆使用率警报
  assert_true(monitor4.alerts.length() > 0)
  assert_true(monitor4.alerts.any(fn(alert) { alert.contains("堆内存使用率过高") }))
  
  // 记录高GC频率快照
  let monitor5 = record_snapshot(monitor4, 5000, 800, 1000, 200, 300, 100, 10, 400)
  
  // 验证GC频率警报
  assert_true(monitor5.alerts.any(fn(alert) { alert.contains("GC频率过高") }))
  
  // 记录内存增长过快快照
  let monitor6 = record_snapshot(monitor5, 6000, 2000, 1000, 200, 300, 100, 11, 500)
  
  // 验证内存增长警报
  assert_true(monitor6.alerts.any(fn(alert) { alert.contains("内存增长过快") }))
  
  // 获取内存趋势
  let trend = get_memory_trend(monitor6, 3)
  
  // 验证趋势计算
  assert_true(trend.heap_growth_rate > 0.0)
  assert_true(trend.gc_frequency > 0.0)
  assert_true(trend.average_heap_usage > 0.0)
}