// Azimuth 性能优化高质量测试用例
// 专注于性能优化、资源管理和效率提升

// 测试1: 内存池管理优化
test "内存池管理优化测试" {
  // 定义内存块
  type MemoryBlock = {
    id: Int,
    size: Int,
    data: Array[Byte],
    in_use: Bool,
    last_used: Int
  }
  
  // 定义内存池
  type MemoryPool = {
    blocks: Array[MemoryBlock],
    free_blocks: Array[Int],
    used_blocks: Array[Int],
    total_memory: Int,
    used_memory: Int,
    allocation_count: Int,
    deallocation_count: Int
  }
  
  // 创建内存池
  let create_memory_pool = fn(total_blocks: Int, block_size: Int) {
    let mut blocks = []
    let mut free_blocks = []
    
    for i in 0..total_blocks {
      let block = {
        id: i,
        size: block_size,
        data: Array::new(block_size),
        in_use: false,
        last_used: 0
      }
      blocks = blocks.push(block)
      free_blocks = free_blocks.push(i)
    }
    
    {
      blocks,
      free_blocks,
      used_blocks: [],
      total_memory: total_blocks * block_size,
      used_memory: 0,
      allocation_count: 0,
      deallocation_count: 0
    }
  }
  
  // 分配内存块
  let allocate_block = fn(pool: MemoryPool, size: Int) {
    if pool.free_blocks.length() == 0 {
      (pool, None)  // 没有可用块
    } else {
      // 获取第一个空闲块
      let block_id = pool.free_blocks[0]
      let block = pool.blocks[block_id]
      
      if block.size >= size {
        // 块足够大，标记为使用中
        let updated_block = { block | in_use: true, last_used: 1640995200 }
        let updated_blocks = pool.blocks.set(block_id, updated_block)
        
        // 更新空闲和已使用列表
        let new_free_blocks = pool.free_blocks.slice(1, pool.free_blocks.length())
        let new_used_blocks = pool.used_blocks.push(block_id)
        
        let updated_pool = {
          blocks: updated_blocks,
          free_blocks: new_free_blocks,
          used_blocks: new_used_blocks,
          total_memory: pool.total_memory,
          used_memory: pool.used_memory + block.size,
          allocation_count: pool.allocation_count + 1,
          deallocation_count: pool.deallocation_count
        }
        
        (updated_pool, Some(block_id))
      } else {
        // 块太小，寻找更大的块
        let mut suitable_block_id = None
        let mut min_suitable_size = Int::max_value()
        
        for free_id in pool.free_blocks {
          let free_block = pool.blocks[free_id]
          if free_block.size >= size and free_block.size < min_suitable_size {
            suitable_block_id = Some(free_id)
            min_suitable_size = free_block.size
          }
        }
        
        match suitable_block_id {
          Some(id) => {
            let suitable_block = pool.blocks[id]
            let updated_block = { suitable_block | in_use: true, last_used: 1640995200 }
            let updated_blocks = pool.blocks.set(id, updated_block)
            
            // 更新空闲和已使用列表
            let new_free_blocks = pool.free_blocks.filter(fn(free_id) { free_id != id })
            let new_used_blocks = pool.used_blocks.push(id)
            
            let updated_pool = {
              blocks: updated_blocks,
              free_blocks: new_free_blocks,
              used_blocks: new_used_blocks,
              total_memory: pool.total_memory,
              used_memory: pool.used_memory + suitable_block.size,
              allocation_count: pool.allocation_count + 1,
              deallocation_count: pool.deallocation_count
            }
            
            (updated_pool, Some(id))
          }
          None => (pool, None)  // 没有合适大小的块
        }
      }
    }
  }
  
  // 释放内存块
  let deallocate_block = fn(pool: MemoryPool, block_id: Int) {
    let block = pool.blocks[block_id]
    if not(block.in_use) {
      pool  // 块已经空闲
    } else {
      // 标记块为空闲
      let updated_block = { block | in_use: false }
      let updated_blocks = pool.blocks.set(block_id, updated_block)
      
      // 更新空闲和已使用列表
      let new_free_blocks = pool.free_blocks.push(block_id)
      let new_used_blocks = pool.used_blocks.filter(fn(used_id) { used_id != block_id })
      
      {
        blocks: updated_blocks,
        free_blocks: new_free_blocks,
        used_blocks: new_used_blocks,
        total_memory: pool.total_memory,
        used_memory: pool.used_memory - block.size,
        allocation_count: pool.allocation_count,
        deallocation_count: pool.deallocation_count + 1
      }
    }
  }
  
  // 压缩内存池（整理碎片）
  let compact_memory_pool = fn(pool: MemoryPool) {
    // 简单的压缩策略：将所有已使用的块移到前面
    let mut compacted_blocks = pool.blocks
    let mut compacted_used_blocks = []
    let mut compacted_free_blocks = []
    
    // 首先添加所有已使用的块
    for block_id in pool.used_blocks {
      compacted_used_blocks = compacted_used_blocks.push(block_id)
    }
    
    // 然后添加所有空闲块
    for block_id in pool.free_blocks {
      compacted_free_blocks = compacted_free_blocks.push(block_id)
    }
    
    {
      blocks: compacted_blocks,
      free_blocks: compacted_free_blocks,
      used_blocks: compacted_used_blocks,
      total_memory: pool.total_memory,
      used_memory: pool.used_memory,
      allocation_count: pool.allocation_count,
      deallocation_count: pool.deallocation_count
    }
  }
  
  // 获取内存池统计信息
  let get_pool_stats = fn(pool: MemoryPool) {
    {
      total_blocks: pool.blocks.length(),
      free_blocks: pool.free_blocks.length(),
      used_blocks: pool.used_blocks.length(),
      utilization_ratio: pool.used_memory.to_float() / pool.total_memory.to_float(),
      allocation_count: pool.allocation_count,
      deallocation_count: pool.deallocation_count,
      fragmentation_ratio: if pool.used_blocks.length() > 0 {
        pool.free_blocks.length().to_float() / (pool.free_blocks.length() + pool.used_blocks.length()).to_float()
      } else {
        0.0
      }
    }
  }
  
  // 创建内存池
  let mut pool = create_memory_pool(100, 1024)  // 100个块，每个1024字节
  
  // 测试基本分配和释放
  let (pool1, block1) = allocate_block(pool, 512)
  assert_true(block1 != None)
  
  let (pool2, block2) = allocate_block(pool1, 1024)
  assert_true(block2 != None)
  
  let pool3 = deallocate_block(pool2, block1.unwrap())
  
  // 验证统计信息
  let stats1 = get_pool_stats(pool3)
  assert_eq(stats1.used_blocks, 1)
  assert_eq(stats1.free_blocks, 99)
  assert_eq(stats1.allocation_count, 2)
  assert_eq(stats1.deallocation_count, 1)
  
  // 测试大量分配和释放
  let mut current_pool = pool3
  let mut allocated_blocks = []
  
  // 分配50个块
  for i in 0..50 {
    let (new_pool, block) = allocate_block(current_pool, 512)
    match block {
      Some(id) => {
        allocated_blocks = allocated_blocks.push(id)
        current_pool = new_pool
      }
      None => assert_true(false)
    }
  }
  
  // 验证分配结果
  let stats2 = get_pool_stats(current_pool)
  assert_eq(stats2.used_blocks, 51)  // 之前1个 + 新分配50个
  assert_eq(stats2.free_blocks, 49)
  assert_eq(stats2.allocation_count, 52)  // 之前2个 + 新分配50个
  
  // 释放一半的块
  for i in 0..25 {
    let block_id = allocated_blocks[i]
    current_pool = deallocate_block(current_pool, block_id)
  }
  
  // 验证释放结果
  let stats3 = get_pool_stats(current_pool)
  assert_eq(stats3.used_blocks, 26)  // 51 - 25
  assert_eq(stats3.free_blocks, 74)  // 49 + 25
  assert_eq(stats3.deallocation_count, 26)  // 之前1个 + 新释放25个
  
  // 压缩内存池
  let compacted_pool = compact_memory_pool(current_pool)
  let stats4 = get_pool_stats(compacted_pool)
  
  // 验证压缩后的统计信息
  assert_eq(stats4.used_blocks, 26)
  assert_eq(stats4.free_blocks, 74)
  assert_eq(stats4.allocation_count, 52)
  assert_eq(stats4.deallocation_count, 26)
  
  // 验证内存利用率
  assert_true(stats4.utilization_ratio > 0.25)  // 至少25%的内存被使用
  assert_true(stats4.utilization_ratio < 0.75)  // 不超过75%的内存被使用
}

// 测试2: 对象池模式优化
test "对象池模式优化测试" {
  // 定义池化对象
  type PooledObject = {
    id: String,
    data: String,
    created_at: Int,
    last_used: Int,
    use_count: Int,
    is_valid: Bool
  }
  
  // 定义对象工厂
  type ObjectFactory = {
    create: () -> PooledObject,
    reset: PooledObject -> PooledObject,
    validate: PooledObject -> Bool
  }
  
  // 定义对象池
  type ObjectPool = {
    objects: Array[PooledObject],
    available_objects: Array[Int],
    used_objects: Array[Int],
    factory: ObjectFactory,
    max_size: Int,
    min_size: Int,
    create_count: Int,
    reuse_count: Int
  }
  
  // 创建对象工厂
  let create_object_factory = fn() {
    {
      create: fn() {
        {
          id: "obj-" + Random::string(8),
          data: "initial_data",
          created_at: 1640995200,
          last_used: 1640995200,
          use_count: 0,
          is_valid: true
        }
      },
      reset: fn(obj: PooledObject) {
        {
          id: obj.id,
          data: "reset_data",
          created_at: obj.created_at,
          last_used: 1640995200,
          use_count: obj.use_count + 1,
          is_valid: true
        }
      },
      validate: fn(obj: PooledObject) {
        obj.is_valid and obj.use_count < 100  // 最多使用100次
      }
    }
  }
  
  // 创建对象池
  let create_object_pool = fn(min_size: Int, max_size: Int, factory: ObjectFactory) {
    let mut objects = []
    let mut available_objects = []
    
    // 创建最小数量的对象
    for i in 0..min_size {
      let obj = factory.create()
      objects = objects.push(obj)
      available_objects = available_objects.push(i)
    }
    
    {
      objects,
      available_objects,
      used_objects: [],
      factory,
      max_size,
      min_size,
      create_count: min_size,
      reuse_count: 0
    }
  }
  
  // 获取对象
  let acquire_object = fn(pool: ObjectPool) {
    if pool.available_objects.length() > 0 {
      // 使用现有对象
      let obj_index = pool.available_objects[0]
      let obj = pool.objects[obj_index]
      
      // 验证对象
      if pool.factory.validate(obj) {
        // 重置对象
        let reset_obj = pool.factory.reset(obj)
        let updated_objects = pool.objects.set(obj_index, reset_obj)
        
        // 更新可用和已使用列表
        let new_available = pool.available_objects.slice(1, pool.available_objects.length())
        let new_used = pool.used_objects.push(obj_index)
        
        let updated_pool = {
          objects: updated_objects,
          available_objects: new_available,
          used_objects: new_used,
          factory: pool.factory,
          max_size: pool.max_size,
          min_size: pool.min_size,
          create_count: pool.create_count,
          reuse_count: pool.reuse_count + 1
        }
        
        (updated_pool, Some(reset_obj))
      } else {
        // 对象无效，移除并创建新对象
        let new_obj = pool.factory.create()
        let updated_objects = pool.objects.set(obj_index, new_obj)
        
        let new_available = pool.available_objects.slice(1, pool.available_objects.length())
        let new_used = pool.used_objects.push(obj_index)
        
        let updated_pool = {
          objects: updated_objects,
          available_objects: new_available,
          used_objects: new_used,
          factory: pool.factory,
          max_size: pool.max_size,
          min_size: pool.min_size,
          create_count: pool.create_count + 1,
          reuse_count: pool.reuse_count
        }
        
        (updated_pool, Some(new_obj))
      }
    } else if pool.objects.length() < pool.max_size {
      // 创建新对象
      let new_obj = pool.factory.create()
      let obj_index = pool.objects.length()
      let updated_objects = pool.objects.push(new_obj)
      let new_used = pool.used_objects.push(obj_index)
      
      let updated_pool = {
        objects: updated_objects,
        available_objects: pool.available_objects,
        used_objects: new_used,
        factory: pool.factory,
        max_size: pool.max_size,
        min_size: pool.min_size,
        create_count: pool.create_count + 1,
        reuse_count: pool.reuse_count
      }
      
      (updated_pool, Some(new_obj))
    } else {
      // 池已满，无法获取对象
      (pool, None)
    }
  }
  
  // 释放对象
  let release_object = fn(pool: ObjectPool, obj: PooledObject) {
    // 找到对象索引
    let mut obj_index = None
    for i in 0..pool.objects.length() {
      if pool.objects[i].id == obj.id {
        obj_index = Some(i)
        break
      }
    }
    
    match obj_index {
      Some(index) => {
        // 更新对象使用信息
        let updated_obj = { obj | last_used: 1640995200 }
        let updated_objects = pool.objects.set(index, updated_obj)
        
        // 更新可用和已使用列表
        let new_available = pool.available_objects.push(index)
        let new_used = pool.used_objects.filter_fn(used_index) { used_index != index }
        
        {
          objects: updated_objects,
          available_objects: new_available,
          used_objects: new_used,
          factory: pool.factory,
          max_size: pool.max_size,
          min_size: pool.min_size,
          create_count: pool.create_count,
          reuse_count: pool.reuse_count
        }
      }
      None => pool  // 对象不在池中
    }
  }
  
  // 清理无效对象
  let cleanup_invalid_objects = fn(pool: ObjectPool) {
    let mut valid_objects = pool.objects
    let mut valid_available = pool.available_objects
    let mut valid_used = pool.used_objects
    let mut removed_count = 0
    
    // 检查可用对象
    for index in pool.available_objects {
      let obj = pool.objects[index]
      if not(pool.factory.validate(obj)) {
        // 移除无效对象
        valid_available = valid_available.filter_fn(i) { i != index }
        valid_used = valid_used.filter_fn(i) { i != index }
        removed_count = removed_count + 1
      }
    }
    
    // 创建新对象以保持最小大小
    let mut final_objects = valid_objects
    let mut final_available = valid_available
    
    while final_objects.length() < pool.min_size {
      let new_obj = pool.factory.create()
      let obj_index = final_objects.length()
      final_objects = final_objects.push(new_obj)
      final_available = final_available.push(obj_index)
    }
    
    {
      objects: final_objects,
      available_objects: final_available,
      used_objects: valid_used,
      factory: pool.factory,
      max_size: pool.max_size,
      min_size: pool.min_size,
      create_count: pool.create_count + removed_count,
      reuse_count: pool.reuse_count
    }
  }
  
  // 创建对象工厂
  let factory = create_object_factory()
  
  // 创建对象池
  let mut pool = create_object_pool(5, 10, factory)
  
  // 验证初始状态
  assert_eq(pool.objects.length(), 5)
  assert_eq(pool.available_objects.length(), 5)
  assert_eq(pool.used_objects.length(), 0)
  assert_eq(pool.create_count, 5)
  assert_eq(pool.reuse_count, 0)
  
  // 获取对象
  let (pool1, obj1) = acquire_object(pool)
  assert_true(obj1 != None)
  
  let (pool2, obj2) = acquire_object(pool1)
  assert_true(obj2 != None)
  
  // 验证获取后的状态
  assert_eq(pool2.objects.length(), 5)
  assert_eq(pool2.available_objects.length(), 3)
  assert_eq(pool2.used_objects.length(), 2)
  assert_eq(pool2.create_count, 5)
  assert_eq(pool2.reuse_count, 2)
  
  // 释放对象
  let pool3 = release_object(pool2, obj1.unwrap())
  
  // 验证释放后的状态
  assert_eq(pool3.objects.length(), 5)
  assert_eq(pool3.available_objects.length(), 4)
  assert_eq(pool3.used_objects.length(), 1)
  assert_eq(pool3.create_count, 5)
  assert_eq(pool3.reuse_count, 2)
  
  // 获取更多对象，触发创建新对象
  let mut current_pool = pool3
  let mut acquired_objects = []
  
  for i in 0..6 {
    let (new_pool, obj) = acquire_object(current_pool)
    match obj {
      Some(o) => {
        acquired_objects = acquired_objects.push(o)
        current_pool = new_pool
      }
      None => assert_true(false)
    }
  }
  
  // 验证扩展后的状态
  assert_eq(current_pool.objects.length(), 10)  // 达到最大大小
  assert_eq(current_pool.available_objects.length(), 2)
  assert_eq(current_pool.used_objects.length(), 7)
  assert_true(current_pool.create_count > 5)  # 创建了新对象
  assert_eq(current_pool.reuse_count, 8)  # 之前2个 + 新获取6个
  
  // 释放所有对象
  for obj in acquired_objects {
    current_pool = release_object(current_pool, obj)
  }
  
  // 验证全部释放后的状态
  assert_eq(current_pool.objects.length(), 10)
  assert_eq(current_pool.available_objects.length(), 10)
  assert_eq(current_pool.used_objects.length(), 0)
  
  // 测试清理无效对象
  // 模拟一些对象变为无效
  let invalid_objects = current_pool.objects.map(fn(obj) {
    if Random::float() > 0.7 {
      { obj | is_valid: false, use_count: 101 }  # 超过最大使用次数
    } else {
      obj
    }
  })
  
  let pool_with_invalid = { current_pool | objects: invalid_objects }
  let cleaned_pool = cleanup_invalid_objects(pool_with_invalid)
  
  // 验证清理后的状态
  assert_eq(cleaned_pool.objects.length(), 10)  # 保持最小大小
  assert_eq(cleaned_pool.available_objects.length(), 10)
  assert_eq(cleaned_pool.used_objects.length(), 0)
  
  // 验证所有对象都有效
  for obj in cleaned_pool.objects {
    assert_true(cleaned_pool.factory.validate(obj))
  }
}

// 测试3: 批处理优化策略
test "批处理优化策略测试" {
  // 定义批处理配置
  type BatchConfig = {
    max_batch_size: Int,
    max_wait_time_ms: Int,
    min_batch_size: Int,
    flush_on_error: Bool
  }
  
  // 定义批处理项
  type BatchItem[T] = {
    id: String,
    data: T,
    timestamp: Int,
    retry_count: Int
  }
  
  // 定义批处理结果
  type BatchResult[T] = {
    success_count: Int,
    failure_count: Int,
    processed_items: Array[BatchItem[T]],
    errors: Array[String]
  }
  
  // 定义批处理器
  type BatchProcessor[T] = {
    config: BatchConfig,
    pending_items: Array[BatchItem[T]],
    processing_items: Array[BatchItem[T]],
    last_flush_time: Int,
    total_processed: Int,
    total_failed: Int
  }
  
  // 创建批处理配置
  let create_batch_config = fn(max_batch_size: Int, max_wait_time_ms: Int, min_batch_size: Int, flush_on_error: Bool) {
    {
      max_batch_size,
      max_wait_time_ms,
      min_batch_size,
      flush_on_error
    }
  }
  
  // 创建批处理器
  let create_batch_processor = fn(config: BatchConfig) {
    {
      config,
      pending_items: [],
      processing_items: [],
      last_flush_time: 1640995200,
      total_processed: 0,
      total_failed: 0
    }
  }
  
  // 添加批处理项
  let add_batch_item = fn(processor: BatchProcessor[T], item: T) {
    let batch_item = {
      id: "item-" + Random::string(8),
      data: item,
      timestamp: 1640995200,
      retry_count: 0
    }
    
    { processor | pending_items: processor.pending_items.push(batch_item) }
  }
  
  // 检查是否应该刷新批处理
  let should_flush = fn(processor: BatchProcessor[T]) {
    let now = 1640995200
    let time_elapsed = now - processor.last_flush_time
    
    processor.pending_items.length() >= processor.config.max_batch_size or
    (processor.pending_items.length() >= processor.config.min_batch_size and time_elapsed >= processor.config.max_wait_time_ms)
  }
  
  // 处理批处理
  let process_batch = fn(processor: BatchProcessor[T], process_fn: Array[BatchItem[T]] -> BatchResult[T]) {
    if processor.pending_items.length() == 0 {
      processor
    } else {
      // 移动待处理项到处理中
      let processing_items = processor.pending_items
      let updated_processor = {
        pending_items: [],
        processing_items: processing_items,
        last_flush_time: 1640995200,
        total_processed: processor.total_processed,
        total_failed: processor.total_failed
      }
      
      // 处理批处理
      let result = process_fn(processing_items)
      
      // 更新统计信息
      {
        config: updated_processor.config,
        pending_items: [],
        processing_items: [],
        last_flush_time: updated_processor.last_flush_time,
        total_processed: updated_processor.total_processed + result.success_count,
        total_failed: updated_processor.total_failed + result.failure_count
      }
    }
  }
  
  // 强制刷新所有待处理项
  let flush_all = fn(processor: BatchProcessor[T], process_fn: Array[BatchItem[T]] -> BatchResult[T]) {
    if processor.pending_items.length() > 0 {
      process_batch(processor, process_fn)
    } else {
      processor
    }
  }
  
  // 创建批处理配置
  let config = create_batch_config(10, 5000, 3, true)
  
  // 创建批处理器
  let mut processor = create_batch_processor(config)
  
  // 添加批处理项
  for i in 0..5 {
    processor = add_batch_item(processor, "data_" + i.to_string())
  }
  
  // 验证待处理项
  assert_eq(processor.pending_items.length(), 5)
  assert_false(should_flush(processor))  # 未达到最大批大小，但达到最小批大小
  
  // 添加更多项直到达到最大批大小
  for i in 5..10 {
    processor = add_batch_item(processor, "data_" + i.to_string())
  }
  
  // 验证应该刷新
  assert_eq(processor.pending_items.length(), 10)
  assert_true(should_flush(processor))
  
  // 定义处理函数
  let process_items = fn(items: Array[BatchItem[String]]) {
    let mut success_count = 0
    let mut failure_count = 0
    let mut errors = []
    
    for item in items {
      // 模拟90%成功率
      if Random::float() > 0.1 {
        success_count = success_count + 1
      } else {
        failure_count = failure_count + 1
        errors = errors.push("Failed to process item: " + item.id)
      }
    }
    
    {
      success_count,
      failure_count,
      processed_items: items,
      errors
    }
  }
  
  // 处理批处理
  processor = process_batch(processor, process_items)
  
  // 验证处理结果
  assert_eq(processor.pending_items.length(), 0)
  assert_eq(processor.processing_items.length(), 0)
  assert_eq(processor.total_processed, 10)
  assert_true(processor.total_failed >= 0)
  
  // 测试基于时间的刷新
  let time_config = create_batch_config(100, 1000, 5, true)  # 1秒超时
  let mut time_processor = create_batch_processor(time_config)
  
  // 添加少量项
  for i in 0..6 {
    time_processor = add_batch_item(time_processor, "time_data_" + i.to_string())
  }
  
  // 模拟时间流逝
  let old_time_processor = { time_processor | last_flush_time: 1640995200 - 2000 }  # 2秒前
  
  // 验证应该刷新（基于时间）
  assert_eq(old_time_processor.pending_items.length(), 6)
  assert_true(should_flush(old_time_processor))
  
  // 处理基于时间的刷新
  time_processor = process_batch(old_time_processor, process_items)
  
  // 验证处理结果
  assert_eq(time_processor.pending_items.length(), 0)
  assert_eq(time_processor.total_processed, 6)
  
  // 测试强制刷新
  let mut force_processor = create_batch_processor(config)
  
  // 添加少量项（不足最小批大小）
  for i in 0..2 {
    force_processor = add_batch_item(force_processor, "force_data_" + i.to_string())
  }
  
  // 验证不应该自动刷新
  assert_eq(force_processor.pending_items.length(), 2)
  assert_false(should_flush(force_processor))
  
  // 强制刷新
  force_processor = flush_all(force_processor, process_items)
  
  // 验证强制刷新结果
  assert_eq(force_processor.pending_items.length(), 0)
  assert_eq(force_processor.total_processed, 2)
}

// 测试4: 缓存预热和预加载策略
test "缓存预热和预加载策略测试" {
  // 定义缓存项
  type CacheItem[T] = {
    key: String,
    value: T,
    access_count: Int,
    last_accessed: Int,
    created_at: Int,
    size: Int,
    preload_priority: Int
  }
  
  // 定义预加载策略
  enum PreloadStrategy {
    None
    MostAccessed
    RecentAccessed
    PriorityBased
    Predictive
  }
  
  // 定义缓存配置
  type CacheConfig = {
    max_items: Int,
    max_memory: Int,
    preload_strategy: PreloadStrategy,
    preload_percentage: Float,
    preload_threshold: Int
  }
  
  // 定义缓存
  type Cache[T] = {
    items: Map[String, CacheItem[T]],
    config: CacheConfig,
    current_memory: Int,
    access_history: Array[String],
    preload_candidates: Array[String]
  }
  
  // 创建缓存配置
  let create_cache_config = fn(max_items: Int, max_memory: Int, preload_strategy: PreloadStrategy, preload_percentage: Float, preload_threshold: Int) {
    {
      max_items,
      max_memory,
      preload_strategy,
      preload_percentage,
      preload_threshold
    }
  }
  
  // 创建缓存
  let create_cache = fn(config: CacheConfig) {
    {
      items: Map::new(),
      config,
      current_memory: 0,
      access_history: [],
      preload_candidates: []
    }
  }
  
  // 计算项大小
  let calculate_item_size = fn[T](value: T) {
    match value {
      String(s) => s.length(),
      Int(_) => 8,
      Float(_) => 8,
      _ => 16
    }
  }
  
  // 添加缓存项
  let put_cache_item = fn(cache: Cache[T], key: String, value: T, preload_priority: Int) {
    let size = calculate_item_size(value)
    let now = 1640995200
    
    let item = {
      key,
      value,
      access_count: 1,
      last_accessed: now,
      created_at: now,
      size,
      preload_priority
    }
    
    // 检查是否需要驱逐
    let needs_eviction = cache.items.size() >= cache.config.max_items or
                        cache.current_memory + size > cache.config.max_memory
    
    let updated_cache = if needs_eviction {
      // 简化的LRU驱逐策略
      let mut oldest_key = None
      let mut oldest_time = now
      
      for (k, v) in cache.items {
        if v.last_accessed < oldest_time {
          oldest_time = v.last_accessed
          oldest_key = Some(k)
        }
      }
      
      match oldest_key {
        Some(key_to_evict) => {
          let evicted_item = cache.items.get(key_to_evict)
          match evicted_item {
            Some(item) => {
              let new_items = cache.items.remove(key_to_evict)
              {
                items: new_items,
                config: cache.config,
                current_memory: cache.current_memory - item.size,
                access_history: cache.access_history,
                preload_candidates: cache.preload_candidates
              }
            }
            None => cache
          }
        }
        None => cache
      }
    } else {
      cache
    }
    
    // 添加新项
    let new_items = updated_cache.items.set(key, item)
    let new_history = updated_cache.access_history.push(key)
    
    {
      items: new_items,
      config: updated_cache.config,
      current_memory: updated_cache.current_memory + size,
      access_history: new_history,
      preload_candidates: updated_cache.preload_candidates
    }
  }
  
  // 获取缓存项
  let get_cache_item = fn(cache: Cache[T], key: String) {
    let item = cache.items.get(key)
    match item {
      Some(cache_item) => {
        let now = 1640995200
        let updated_item = {
          key: cache_item.key,
          value: cache_item.value,
          access_count: cache_item.access_count + 1,
          last_accessed: now,
          created_at: cache_item.created_at,
          size: cache_item.size,
          preload_priority: cache_item.priority
        }
        
        let new_items = cache.items.set(key, updated_item)
        let new_history = cache.access_history.push(key)
        
        let updated_cache = {
          items: new_items,
          config: cache.config,
          current_memory: cache.current_memory,
          access_history: new_history,
          preload_candidates: cache.preload_candidates
        }
        
        (updated_cache, Some(cache_item.value))
      }
      None => (cache, None)
    }
  }
  
  // 识别预加载候选项
  let identify_preload_candidates = fn(cache: Cache[T]) {
    match cache.config.preload_strategy {
      PreloadStrategy::MostAccessed => {
        // 基于访问次数排序
        let mut items = []
        for (k, v) in cache.items {
          items = items.push((k, v.access_count))
        }
        
        // 按访问次数降序排序
        let sorted_items = items.sort(fn(a, b) { b.1 > a.1 })
        
        // 选择前N%作为候选项
        let candidate_count = (sorted_items.length().to_float() * cache.config.preload_percentage).to_int()
        sorted_items.slice(0, candidate_count).map(fn(item) { item.0 })
      }
      PreloadStrategy::RecentAccessed => {
        // 基于最近访问时间排序
        let mut items = []
        for (k, v) in cache.items {
          items = items.push((k, v.last_accessed))
        }
        
        // 按访问时间降序排序
        let sorted_items = items.sort(fn(a, b) { b.1 > a.1 })
        
        // 选择前N%作为候选项
        let candidate_count = (sorted_items.length().to_float() * cache.config.preload_percentage).to_int()
        sorted_items.slice(0, candidate_count).map(fn(item) { item.0 })
      }
      PreloadStrategy::PriorityBased => {
        // 基于优先级排序
        let mut items = []
        for (k, v) in cache.items {
          items = items.push((k, v.preload_priority))
        }
        
        // 按优先级降序排序
        let sorted_items = items.sort(fn(a, b) { b.1 > a.1 })
        
        // 选择前N%作为候选项
        let candidate_count = (sorted_items.length().to_float() * cache.config.preload_percentage).to_int()
        sorted_items.slice(0, candidate_count).map(fn(item) { item.0 })
      }
      PreloadStrategy::Predictive => {
        // 基于访问历史预测（简化实现）
        let mut access_frequency = Map::new()
        
        // 计算访问频率
        for key in cache.access_history {
          let count = access_frequency.get(key)
          match count {
            Some(c) => access_frequency = access_frequency.set(key, c + 1)
            None => access_frequency = access_frequency.set(key, 1)
          }
        }
        
        // 按频率排序
        let mut items = []
        for (k, v) in access_frequency {
          items = items.push((k, v))
        }
        
        let sorted_items = items.sort(fn(a, b) { b.1 > a.1 })
        
        // 选择前N%作为候选项
        let candidate_count = (sorted_items.length().to_float() * cache.config.preload_percentage).to_int()
        sorted_items.slice(0, candidate_count).map(fn(item) { item.0 })
      }
      PreloadStrategy::None => []
    }
  }
  
  // 预热缓存
  let warm_up_cache = fn(cache: Cache[T], data_source: Map[String, T], priority_calculator: String -> Int) {
    let mut warmed_cache = cache
    
    // 计算所有数据的优先级
    let mut prioritized_data = []
    for (key, value) in data_source {
      let priority = priority_calculator(key)
      prioritized_data = prioritized_data.push((key, value, priority))
    }
    
    // 按优先级排序
    let sorted_data = prioritized_data.sort(fn(a, b) { b.2 > a.2 })
    
    // 预加载高优先级数据
    let preload_count = (sorted_data.length().to_float() * cache.config.preload_percentage).to_int()
    
    for i in 0..preload_count {
      let (key, value, priority) = sorted_data[i]
      warmed_cache = put_cache_item(warmed_cache, key, value, priority)
    }
    
    // 识别预加载候选项
    let candidates = identify_preload_candidates(warmed_cache)
    
    {
      items: warmed_cache.items,
      config: warmed_cache.config,
      current_memory: warmed_cache.current_memory,
      access_history: warmed_cache.access_history,
      preload_candidates: candidates
    }
  }
  
  // 创建缓存配置
  let config = create_cache_config(100, 10000, PreloadStrategy::MostAccessed, 0.3, 5)
  
  // 创建缓存
  let mut cache = create_cache(config)
  
  // 创建数据源
  let mut data_source = Map::new()
  for i in 0..50 {
    data_source = data_source.set("key_" + i.to_string(), "value_" + i.to_string())
  }
  
  // 定义优先级计算器
  let priority_calculator = fn(key: String) {
    // 简单的优先级计算：基于键的哈希值
    let hash = key.length() % 10
    hash
  }
  
  // 预热缓存
  cache = warm_up_cache(cache, data_source, priority_calculator)
  
  // 验证预热结果
  assert_eq(cache.items.size(), 15)  # 50 * 0.3 = 15
  assert_eq(cache.preload_candidates.length(), 4)  # 15 * 0.3 ≈ 4
  
  // 测试访问模式
  for i in 0..20 {
    let key = "key_" + (i % 10).to_string()  # 访问前10个键多次
    let (updated_cache, _) = get_cache_item(cache, key)
    cache = updated_cache
  }
  
  // 重新识别预加载候选项
  let new_candidates = identify_preload_candidates(cache)
  
  // 验证候选项更新
  assert_true(new_candidates.length() > 0)
  
  // 测试不同预加载策略
  let priority_config = create_cache_config(100, 10000, PreloadStrategy::PriorityBased, 0.2, 5)
  let priority_cache = create_cache(priority_config)
  
  // 预热优先级缓存
  let priority_warmed_cache = warm_up_cache(priority_cache, data_source, priority_calculator)
  
  // 验证优先级预热结果
  assert_eq(priority_warmed_cache.items.size(), 10)  # 50 * 0.2 = 10
  
  // 验证高优先级项被优先加载
  let mut found_high_priority = false
  for (key, item) in priority_warmed_cache.items {
    if item.preload_priority >= 7 {
      found_high_priority = true
      break
    }
  }
  assert_true(found_high_priority)
}

// 测试5: 资源池化和复用优化
test "资源池化和复用优化测试" {
  // 定义资源类型
  enum ResourceType {
    DatabaseConnection
    NetworkSocket
    FileHandle
    Thread
  }
  
  // 定义资源状态
  enum ResourceState {
    Available
    InUse
    Exhausted
    Maintenance
  }
  
  // 定义资源
  type Resource = {
    id: String,
    resource_type: ResourceType,
    state: ResourceState,
    created_at: Int,
    last_used: Int,
    usage_count: Int,
    max_uses: Int,
    metadata: Map[String, String]
  }
  
  // 定义资源池配置
  type ResourcePoolConfig = {
    resource_type: ResourceType,
    min_resources: Int,
    max_resources: Int,
    max_idle_time_ms: Int,
    max_uses_per_resource: Int,
    creation_timeout_ms: Int,
    health_check_interval_ms: Int
  }
  
  // 定义资源池
  type ResourcePool = {
    config: ResourcePoolConfig,
    resources: Array[Resource],
    available_resources: Array[Int],
    used_resources: Array[Int],
    waiting_requests: Array[String],
    total_created: Int,
    total_destroyed: Int,
    total_acquired: Int,
    total_released: Int
  }
  
  // 创建资源池配置
  let create_resource_pool_config = fn(resource_type: ResourceType, min_resources: Int, max_resources: Int, max_idle_time_ms: Int, max_uses_per_resource: Int) {
    {
      resource_type,
      min_resources,
      max_resources,
      max_idle_time_ms,
      max_uses_per_resource,
      creation_timeout_ms: 5000,
      health_check_interval_ms: 30000
    }
  }
  
  // 创建资源
  let create_resource = fn(resource_type: ResourceType, id: String, max_uses: Int) {
    {
      id,
      resource_type,
      state: ResourceState::Available,
      created_at: 1640995200,
      last_used: 1640995200,
      usage_count: 0,
      max_uses,
      metadata: Map::new()
    }
  }
  
  // 创建资源池
  let create_resource_pool = fn(config: ResourcePoolConfig) {
    let mut resources = []
    let mut available_resources = []
    
    // 创建最小数量的资源
    for i in 0..config.min_resources {
      let resource = create_resource(config.resource_type, "res-" + i.to_string(), config.max_uses_per_resource)
      resources = resources.push(resource)
      available_resources = available_resources.push(i)
    }
    
    {
      config,
      resources,
      available_resources,
      used_resources: [],
      waiting_requests: [],
      total_created: config.min_resources,
      total_destroyed: 0,
      total_acquired: 0,
      total_released: 0
    }
  }
  
  // 检查资源健康状态
  let check_resource_health = fn(resource: Resource) {
    let now = 1640995200
    let idle_time = now - resource.last_used
    let is_exhausted = resource.usage_count >= resource.max_uses
    
    if is_exhausted {
      ResourceState::Exhausted
    } else if idle_time > 3600000 {  # 1小时
      ResourceState::Maintenance
    } else {
      ResourceState::Available
    }
  }
  
  // 获取资源
  let acquire_resource = fn(pool: ResourcePool) {
    if pool.available_resources.length() > 0 {
      // 使用现有资源
      let resource_index = pool.available_resources[0]
      let resource = pool.resources[resource_index]
      
      // 检查资源健康状态
      let health = check_resource_health(resource)
      
      match health {
        ResourceState::Available => {
          // 资源可用，标记为使用中
          let updated_resource = {
            id: resource.id,
            resource_type: resource.resource_type,
            state: ResourceState::InUse,
            created_at: resource.created_at,
            last_used: 1640995200,
            usage_count: resource.usage_count + 1,
            max_uses: resource.max_uses,
            metadata: resource.metadata
          }
          
          let updated_resources = pool.resources.set(resource_index, updated_resource)
          let new_available = pool.available_resources.slice(1, pool.available_resources.length())
          let new_used = pool.used_resources.push(resource_index)
          
          let updated_pool = {
            config: pool.config,
            resources: updated_resources,
            available_resources: new_available,
            used_resources: new_used,
            waiting_requests: pool.waiting_requests,
            total_created: pool.total_created,
            total_destroyed: pool.total_destroyed,
            total_acquired: pool.total_acquired + 1,
            total_released: pool.total_released
          }
          
          (updated_pool, Some(updated_resource))
        }
        _ => {
          // 资源不可用，移除并创建新资源
          let new_available = pool.available_resources.slice(1, pool.available_resources.length())
          let temp_pool = { pool | available_resources: new_available }
          
          if pool.resources.length() < pool.config.max_resources {
            // 创建新资源
            let new_resource = create_resource(pool.config.resource_type, "res-" + Random::string(8), pool.config.max_uses_per_resource)
            let resource_index = pool.resources.length()
            let updated_resources = pool.resources.push(new_resource)
            let new_used = pool.used_resources.push(resource_index)
            
            let updated_pool = {
              config: pool.config,
              resources: updated_resources,
              available_resources: temp_pool.available_resources,
              used_resources: new_used,
              waiting_requests: pool.waiting_requests,
              total_created: pool.total_created + 1,
              total_destroyed: pool.total_destroyed,
              total_acquired: pool.total_acquired + 1,
              total_released: pool.total_released
            }
            
            (updated_pool, Some(new_resource))
          } else {
            // 无法创建更多资源
            (temp_pool, None)
          }
        }
      }
    } else if pool.resources.length() < pool.config.max_resources {
      // 创建新资源
      let new_resource = create_resource(pool.config.resource_type, "res-" + Random::string(8), pool.config.max_uses_per_resource)
      let resource_index = pool.resources.length()
      let updated_resources = pool.resources.push(new_resource)
      let new_used = pool.used_resources.push(resource_index)
      
      let updated_pool = {
        config: pool.config,
        resources: updated_resources,
        available_resources: pool.available_resources,
        used_resources: new_used,
        waiting_requests: pool.waiting_requests,
        total_created: pool.total_created + 1,
        total_destroyed: pool.total_destroyed,
        total_acquired: pool.total_acquired + 1,
        total_released: pool.total_released
      }
      
      (updated_pool, Some(new_resource))
    } else {
      // 资源池已满，添加到等待队列
      let request_id = "req-" + Random::string(8)
      let updated_pool = { pool | waiting_requests: pool.waiting_requests.push(request_id) }
      
      (updated_pool, None)
    }
  }
  
  // 释放资源
  let release_resource = fn(pool: ResourcePool, resource: Resource) {
    // 找到资源索引
    let mut resource_index = None
    for i in 0..pool.resources.length() {
      if pool.resources[i].id == resource.id {
        resource_index = Some(i)
        break
      }
    }
    
    match resource_index {
      Some(index) => {
        // 更新资源状态
        let updated_resource = {
          id: resource.id,
          resource_type: resource.resource_type,
          state: ResourceState::Available,
          created_at: resource.created_at,
          last_used: 1640995200,
          usage_count: resource.usage_count,
          max_uses: resource.max_uses,
          metadata: resource.metadata
        }
        
        let updated_resources = pool.resources.set(index, updated_resource)
        
        // 更新可用和已使用列表
        let new_available = pool.available_resources.push(index)
        let new_used = pool.used_resources.filter_fn(i) { i != index }
        
        {
          config: pool.config,
          resources: updated_resources,
          available_resources: new_available,
          used_resources: new_used,
          waiting_requests: pool.waiting_requests,
          total_created: pool.total_created,
          total_destroyed: pool.total_destroyed,
          total_acquired: pool.total_acquired,
          total_released: pool.total_released + 1
        }
      }
      None => pool  # 资源不在池中
    }
  }
  
  // 清理过期资源
  let cleanup_expired_resources = fn(pool: ResourcePool) {
    let mut valid_resources = pool.resources
    let mut valid_available = pool.available_resources
    let mut valid_used = pool.used_resources
    let mut expired_count = 0
    
    // 检查所有资源
    for i in 0..pool.resources.length() {
      let resource = pool.resources[i]
      let health = check_resource_health(resource)
      
      if health == ResourceState::Exhausted or health == ResourceState::Maintenance {
        // 移除过期资源
        valid_available = valid_available.filter_fn(index) { index != i }
        valid_used = valid_used.filter_fn(index) { index != i }
        expired_count = expired_count + 1
      }
    }
    
    // 创建新资源以保持最小数量
    let mut final_resources = valid_resources
    let mut final_available = valid_available
    
    while final_resources.length() < pool.config.min_resources {
      let new_resource = create_resource(pool.config.resource_type, "res-" + Random::string(8), pool.config.max_uses_per_resource)
      let resource_index = final_resources.length()
      final_resources = final_resources.push(new_resource)
      final_available = final_available.push(resource_index)
    }
    
    {
      config: pool.config,
      resources: final_resources,
      available_resources: final_available,
      used_resources: valid_used,
      waiting_requests: pool.waiting_requests,
      total_created: pool.total_created + expired_count,
      total_destroyed: pool.total_destroyed + expired_count,
      total_acquired: pool.total_acquired,
      total_released: pool.total_released
    }
  }
  
  // 创建资源池配置
  let config = create_resource_pool_config(ResourceType::DatabaseConnection, 5, 10, 300000, 1000)
  
  // 创建资源池
  let mut pool = create_resource_pool(config)
  
  // 验证初始状态
  assert_eq(pool.resources.length(), 5)
  assert_eq(pool.available_resources.length(), 5)
  assert_eq(pool.used_resources.length(), 0)
  assert_eq(pool.total_created, 5)
  
  // 获取资源
  let (pool1, resource1) = acquire_resource(pool)
  assert_true(resource1 != None)
  
  let (pool2, resource2) = acquire_resource(pool1)
  assert_true(resource2 != None)
  
  // 验证获取后的状态
  assert_eq(pool2.resources.length(), 5)
  assert_eq(pool2.available_resources.length(), 3)
  assert_eq(pool2.used_resources.length(), 2)
  assert_eq(pool2.total_acquired, 2)
  
  // 释放资源
  let pool3 = release_resource(pool2, resource1.unwrap())
  
  // 验证释放后的状态
  assert_eq(pool3.resources.length(), 5)
  assert_eq(pool3.available_resources.length(), 4)
  assert_eq(pool3.used_resources.length(), 1)
  assert_eq(pool3.total_released, 1)
  
  // 获取更多资源，触发创建新资源
  let mut current_pool = pool3
  let mut acquired_resources = []
  
  for i in 0..8 {
    let (new_pool, resource) = acquire_resource(current_pool)
    match resource {
      Some(r) => {
        acquired_resources = acquired_resources.push(r)
        current_pool = new_pool
      }
      None => assert_true(false)
    }
  }
  
  // 验证扩展后的状态
  assert_eq(current_pool.resources.length(), 10)  # 达到最大大小
  assert_eq(current_pool.available_resources.length(), 2)
  assert_eq(current_pool.used_resources.length(), 8)
  assert_true(current_pool.total_created > 5)  # 创建了新资源
  
  // 释放所有资源
  for resource in acquired_resources {
    current_pool = release_resource(current_pool, resource)
  }
  
  // 验证全部释放后的状态
  assert_eq(current_pool.resources.length(), 10)
  assert_eq(current_pool.available_resources.length(), 10)
  assert_eq(current_pool.used_resources.length(), 0)
  
  // 模拟资源过期
  let expired_resources = current_pool.resources.map(fn(resource) {
    if Random::float() > 0.7 {
      { resource | usage_count: 1001 }  # 超过最大使用次数
    } else {
      resource
    }
  })
  
  let pool_with_expired = { current_pool | resources: expired_resources }
  let cleaned_pool = cleanup_expired_resources(pool_with_expired)
  
  // 验证清理后的状态
  assert_eq(cleaned_pool.resources.length(), 10)  # 保持最小大小
  assert_eq(cleaned_pool.available_resources.length(), 10)
  assert_eq(cleaned_pool.used_resources.length(), 0)
  assert_true(cleaned_pool.total_destroyed > 0)  # 有资源被清理
  
  // 验证所有资源都可用
  for resource in cleaned_pool.resources {
    assert_eq(check_resource_health(resource), ResourceState::Available)
  }
}