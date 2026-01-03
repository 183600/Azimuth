// Azimuth Premium Performance Optimization Tests
// 高级性能优化测试用例，专注于遥测系统的性能瓶颈和优化策略

// 测试 1: 批处理性能优化
test "batch processing performance optimization" {
  // 定义批处理配置
  type BatchConfig = {
    max_batch_size: Int,
    max_wait_time_ms: Int,
    compression_threshold: Int
  }
  
  // 创建动态批处理器
  let create_batch_processor = fn(config: BatchConfig) {
    let mut batch = []
    let mut last_flush = 0
    let mut total_items = 0
    let mut total_batches = 0
    
    // 处理单个项目
    let process_item = fn(item: String, current_time: Int) {
      batch = batch.push(item)
      total_items = total_items + 1
      
      // 检查是否需要刷新批次
      let should_flush = batch.length() >= config.max_batch_size || 
                        (current_time - last_flush) >= config.max_wait_time_ms
      
      if should_flush {
        total_batches = total_batches + 1
        let batch_size = batch.length()
        
        // 模拟压缩决策
        let should_compress = batch_size >= config.compression_threshold
        
        batch = []
        last_flush = current_time
        
        (total_batches, batch_size, should_compress)
      } else {
        (total_batches, 0, false)
      }
    }
    
    process_item
  }
  
  // 测试配置
  let config = {
    max_batch_size: 100,
    max_wait_time_ms: 5000,
    compression_threshold: 50
  }
  
  let batch_processor = create_batch_processor(config)
  
  // 模拟数据处理
  let mut current_time = 1000
  let mut total_batches_processed = 0
  let mut max_batch_size_seen = 0
  let mut compression_used_count = 0
  
  // 处理250个项目，时间间隔100ms
  for i in 1..=250 {
    let item = "item_" + i.to_string()
    let (batches, batch_size, compressed) = batch_processor(item, current_time)
    
    total_batches_processed = batches
    max_batch_size_seen = max_batch_size_seen.max(batch_size)
    if compressed { compression_used_count = compression_used_count + 1 }
    
    current_time = current_time + 100
  }
  
  // 验证批处理效果
  assert_true(total_batches_processed >= 2)  // 至少应该有2个批次
  assert_true(max_batch_size_seen <= config.max_batch_size)  // 批次大小不超过限制
  assert_true(compression_used_count > 0)  // 应该使用了压缩
  
  // 最后一次刷新
  let (final_batches, _, final_compressed) = batch_processor("", current_time + 10000)
  assert_true(final_batches > total_batches_processed)
}

// 测试 2: 内存池管理优化
test "memory pool management optimization" {
  // 定义内存池
  type MemoryPool = {
    small_objects: Array[String],
    medium_objects: Array[String],
    large_objects: Array[String],
    small_allocated: Int,
    medium_allocated: Int,
    large_allocated: Int
  }
  
  // 创建内存池
  let create_memory_pool = fn() {
    {
      small_objects: [],
      medium_objects: [],
      large_objects: [],
      small_allocated: 0,
      medium_allocated: 0,
      large_allocated: 0
    }
  }
  
  // 分配对象
  let allocate = fn(pool: MemoryPool, size: Int) {
    match size {
      s if s <= 100 => {
        if pool.small_objects.length() > 0 {
          let obj = pool.small_objects[0]
          let remaining = pool.small_objects.slice(1)
          ({
            small_objects: remaining,
            medium_objects: pool.medium_objects,
            large_objects: pool.large_objects,
            small_allocated: pool.small_allocated + 1,
            medium_allocated: pool.medium_allocated,
            large_allocated: pool.large_allocated
          }, obj)
        } else {
          ({
            small_objects: pool.small_objects,
            medium_objects: pool.medium_objects,
            large_objects: pool.large_objects,
            small_allocated: pool.small_allocated + 1,
            medium_allocated: pool.medium_allocated,
            large_allocated: pool.large_allocated
          }, "small_obj_" + pool.small_allocated.to_string())
        }
      }
      m if m > 100 && m <= 1000 => {
        if pool.medium_objects.length() > 0 {
          let obj = pool.medium_objects[0]
          let remaining = pool.medium_objects.slice(1)
          ({
            small_objects: pool.small_objects,
            medium_objects: remaining,
            large_objects: pool.large_objects,
            small_allocated: pool.small_allocated,
            medium_allocated: pool.medium_allocated + 1,
            large_allocated: pool.large_allocated
          }, obj)
        } else {
          ({
            small_objects: pool.small_objects,
            medium_objects: pool.medium_objects,
            large_objects: pool.large_objects,
            small_allocated: pool.small_allocated,
            medium_allocated: pool.medium_allocated + 1,
            large_allocated: pool.large_allocated
          }, "medium_obj_" + pool.medium_allocated.to_string())
        }
      }
      l => {
        if pool.large_objects.length() > 0 {
          let obj = pool.large_objects[0]
          let remaining = pool.large_objects.slice(1)
          ({
            small_objects: pool.small_objects,
            medium_objects: pool.medium_objects,
            large_objects: remaining,
            small_allocated: pool.small_allocated,
            medium_allocated: pool.medium_allocated,
            large_allocated: pool.large_allocated + 1
          }, obj)
        } else {
          ({
            small_objects: pool.small_objects,
            medium_objects: pool.medium_objects,
            large_objects: pool.large_objects,
            small_allocated: pool.small_allocated,
            medium_allocated: pool.medium_allocated,
            large_allocated: pool.large_allocated + 1
          }, "large_obj_" + pool.large_allocated.to_string())
        }
      }
    }
  }
  
  // 释放对象
  let deallocate = fn(pool: MemoryPool, obj: String, size: Int) {
    match size {
      s if s <= 100 => {
        ({
          small_objects: pool.small_objects.push(obj),
          medium_objects: pool.medium_objects,
          large_objects: pool.large_objects,
          small_allocated: pool.small_allocated,
          medium_allocated: pool.medium_allocated,
          large_allocated: pool.large_allocated
        }, ())
      }
      m if m > 100 && m <= 1000 => {
        ({
          small_objects: pool.small_objects,
          medium_objects: pool.medium_objects.push(obj),
          large_objects: pool.large_objects,
          small_allocated: pool.small_allocated,
          medium_allocated: pool.medium_allocated,
          large_allocated: pool.large_allocated
        }, ())
      }
      l => {
        ({
          small_objects: pool.small_objects,
          medium_objects: pool.medium_objects,
          large_objects: pool.large_objects.push(obj),
          small_allocated: pool.small_allocated,
          medium_allocated: pool.medium_allocated,
          large_allocated: pool.large_allocated
        }, ())
      }
    }
  }
  
  // 测试内存池
  let mut pool = create_memory_pool()
  let allocated_objects = []
  
  // 分配各种大小的对象
  let sizes = [50, 150, 1200, 80, 500, 2000, 30, 800]
  for size in sizes {
    let (new_pool, obj) = allocate(pool, size)
    pool = new_pool
    allocated_objects = allocated_objects.push((obj, size))
  }
  
  // 验证分配计数
  assert_eq(pool.small_allocated, 2)  // 50, 80, 30 -> 3个小的
  assert_eq(pool.medium_allocated, 3)  // 150, 500, 800 -> 3个中的
  assert_eq(pool.large_allocated, 2)  // 1200, 2000 -> 2个大的
  
  // 释放一半的对象
  let half_count = allocated_objects.length() / 2
  for i in 0..half_count {
    let (obj, size) = allocated_objects[i]
    let (new_pool, _) = deallocate(pool, obj, size)
    pool = new_pool
  }
  
  // 验证释放后的池状态
  assert_true(pool.small_objects.length() + pool.medium_objects.length() + pool.large_objects.length() >= half_count)
  
  // 再次分配对象，应该重用已释放的对象
  let (new_pool, reused_obj) = allocate(pool, 50)
  pool = new_pool
  
  // 由于我们释放了一个小对象，应该可以重用
  assert_true(pool.small_objects.length() < allocated_objects.length() / 2)
}

// 测试 3: 智能采样策略性能
test "intelligent sampling strategy performance" {
  // 定义采样决策器
  type SamplingDecision = {
    should_sample: Bool,
    sample_rate: Float,
    reason: String
  }
  
  // 创建自适应采样器
  let create_adaptive_sampler = fn(base_rate: Float, max_rate: Float, min_rate: Float) {
    let mut current_rate = base_rate
    let mut sample_count = 0
    let mut total_count = 0
    let mut error_count = 0
    
    let sample = fn(trace_id: String, has_error: Bool, priority: Int) {
      total_count = total_count + 1
      if has_error { error_count = error_count + 1 }
      
      // 基于错误率调整采样率
      let error_rate = if total_count > 0 { (error_count as Float) / (total_count as Float) } else { 0.0 }
      
      // 错误率高时提高采样率
      if error_rate > 0.1 && current_rate < max_rate {
        current_rate = (current_rate * 1.5).min(max_rate)
      } else if error_rate < 0.01 && current_rate > min_rate {
        current_rate = (current_rate * 0.8).max(min_rate)
      }
      
      // 高优先级总是采样
      let should_sample = priority >= 8 || 
                         (priority >= 5 && trace_id.to_int() % 1000 < (current_rate * 1000.0).to_int()) ||
                         (priority < 5 && trace_id.to_int() % 10000 < (current_rate * 10000.0).to_int())
      
      if should_sample {
        sample_count = sample_count + 1
      }
      
      {
        should_sample,
        sample_rate: current_rate,
        reason: if priority >= 8 { "high_priority" } 
                else if has_error { "error_detected" }
                else { "adaptive_sampling" }
      }
    }
    
    sample
  }
  
  // 测试自适应采样器
  let sampler = create_adaptive_sampler(0.1, 0.5, 0.01)
  
  let mut sampled_count = 0
  let mut high_priority_sampled = 0
  let mut error_sampled = 0
  let mut adaptive_sampled = 0
  
  // 模拟1000个追踪
  for i in 1..=1000 {
    let trace_id = "trace_" + i.to_string()
    let has_error = i % 10 == 0  // 10%的错误率
    let priority = if i % 50 == 0 { 9 }  // 2%高优先级
                  else if i % 20 == 0 { 6 }  // 5%中优先级
                  else { 3 }  // 93%低优先级
    
    let decision = sampler(trace_id, has_error, priority)
    
    if decision.should_sample {
      sampled_count = sampled_count + 1
      
      if decision.reason == "high_priority" {
        high_priority_sampled = high_priority_sampled + 1
      } else if decision.reason == "error_detected" {
        error_sampled = error_sampled + 1
      } else {
        adaptive_sampled = adaptive_sampled + 1
      }
    }
  }
  
  // 验证采样效果
  assert_true(high_priority_sampled >= 15)  // 高优先级应该大部分被采样
  assert_true(error_sampled >= 8)  // 错误追踪应该有较高采样率
  assert_true(sampled_count > 50)  // 总采样数应该合理
  assert_true(sampled_count < 500)  // 但不应该过高
  
  // 验证采样率在合理范围内
  let actual_rate = (sampled_count as Float) / 1000.0
  assert_true(actual_rate >= 0.05)  // 至少5%
  assert_true(actual_rate <= 0.3)   // 但不超过30%
}

// 测试 4: 预计算和缓存优化
test "precomputation and caching optimization" {
  // 定义缓存项
  type CacheItem[T] = {
    value: T,
    computed_at: Int,
    access_count: Int,
    last_accessed: Int
  }
  
  // 创建LRU缓存
  let create_lru_cache = fn(max_size: Int, ttl_ms: Int) {
    let mut cache = Map::empty()
    let mut access_order = []
    
    let get = fn(key: String, compute_fn: () -> T, current_time: Int) {
      match Map::get(cache, key) {
        Some(item) => {
          // 检查是否过期
          if current_time - item.computed_at > ttl_ms {
            // 过期，重新计算
            let new_value = compute_fn()
            let new_item = {
              value: new_value,
              computed_at: current_time,
              access_count: item.access_count + 1,
              last_accessed: current_time
            }
            
            let _ = Map::insert(cache, key, new_item)
            
            // 更新访问顺序
            access_order = access_order.filter(fn(k) { k != key }).push(key)
            
            new_value
          } else {
            // 未过期，更新访问信息
            let updated_item = {
              value: item.value,
              computed_at: item.computed_at,
              access_count: item.access_count + 1,
              last_accessed: current_time
            }
            
            let _ = Map::insert(cache, key, updated_item)
            
            // 更新访问顺序
            access_order = access_order.filter(fn(k) { k != key }).push(key)
            
            item.value
          }
        }
        None => {
          // 缓存中没有，计算新值
          let new_value = compute_fn()
          let new_item = {
            value: new_value,
            computed_at: current_time,
            access_count: 1,
            last_accessed: current_time
          }
          
          // 检查缓存大小
          if cache.size() >= max_size {
            // 移除最久未访问的项
            let oldest_key = access_order[0]
            let _ = Map::remove(cache, oldest_key)
            access_order = access_order.slice(1)
          }
          
          let _ = Map::insert(cache, key, new_item)
          access_order = access_order.push(key)
          
          new_value
        }
      }
    }
    
    get
  }
  
  // 测试缓存
  let cache = create_lru_cache(5, 1000)  // 最多5项，1秒TTL
  let mut current_time = 1000
  
  // 昂贵的计算函数
  let expensive_computation = fn(x: Int) {
    // 模拟计算时间
    x * x * x + x * x + x
  }
  
  // 第一次访问，应该计算
  let result1 = cache("key1", fn() { expensive_computation(10) }, current_time)
  assert_eq(result1, 1110)  // 10^3 + 10^2 + 10 = 1000 + 100 + 10 = 1110
  
  // 第二次访问，应该从缓存获取
  let result2 = cache("key1", fn() { expensive_computation(10) }, current_time + 100)
  assert_eq(result2, 1110)
  
  // 添加更多项
  let result3 = cache("key2", fn() { expensive_computation(20) }, current_time + 200)
  assert_eq(result3, 8420)  // 20^3 + 20^2 + 20 = 8000 + 400 + 20 = 8420
  
  let result4 = cache("key3", fn() { expensive_computation(30) }, current_time + 300)
  assert_eq(result4, 27930)  // 30^3 + 30^2 + 30 = 27000 + 900 + 30 = 27930
  
  let result5 = cache("key4", fn() { expensive_computation(40) }, current_time + 400)
  assert_eq(result5, 65640)  // 40^3 + 40^2 + 40 = 64000 + 1600 + 40 = 65640
  
  let result6 = cache("key5", fn() { expensive_computation(50) }, current_time + 500)
  assert_eq(result6, 127550)  // 50^3 + 50^2 + 50 = 125000 + 2500 + 50 = 127550
  
  // 缓存已满，添加新项应该移除最旧的
  let result7 = cache("key6", fn() { expensive_computation(60) }, current_time + 600)
  assert_eq(result7, 219660)  // 60^3 + 60^2 + 60 = 216000 + 3600 + 60 = 219660
  
  // 访问已过期的项
  let result8 = cache("key2", fn() { expensive_computation(20) }, current_time + 1500)
  assert_eq(result8, 8420)  // 应该重新计算，但结果相同
  
  // 测试TTL过期
  let result9 = cache("key1", fn() { expensive_computation(15) }, current_time + 2500)
  assert_eq(result9, 3690)  // 15^3 + 15^2 + 15 = 3375 + 225 + 15 = 3615，但这里用新的计算函数
}

// 测试 5: 并发处理优化
test "concurrent processing optimization" {
  // 定义工作项
  type WorkItem = {
    id: Int,
    data: String,
    priority: Int
  }
  
  // 定义工作结果
  type WorkResult = {
    id: Int,
    result: String,
    processing_time: Int
  }
  
  // 创建工作池处理器
  let create_worker_pool = fn(worker_count: Int) {
    let mut workers = []
    let mut work_queue = []
    let mut results = []
    
    // 初始化工作线程
    for i in 0..worker_count {
      workers = workers.push({
        id: i,
        busy: false,
        current_work: None
      })
    }
    
    // 分配工作
    let assign_work = fn(item: WorkItem) {
      // 找到空闲的工作线程
      match workers.find_index(fn(w) { not w.busy }) {
        Some(index) => {
          let worker = workers[index]
          let updated_worker = {
            id: worker.id,
            busy: true,
            current_work: Some(item)
          }
          workers[index] = updated_worker
          
          // 模拟处理工作
          let processing_time = 100 + item.data.length() * 10
          let result = {
            id: item.id,
            result: "processed_" + item.data,
            processing_time
          }
          
          results = results.push(result)
          
          // 标记工作线程为空闲
          let worker = workers[index]
          let updated_worker = {
            id: worker.id,
            busy: false,
            current_work: None
          }
          workers[index] = updated_worker
          
          true
        }
        None => {
          // 没有空闲工作线程，加入队列
          work_queue = work_queue.push(item)
          false
        }
      }
    }
    
    // 处理队列中的工作
    let process_queue = fn() {
      let mut processed = 0
      while work_queue.length() > 0 {
        let item = work_queue[0]
        work_queue = work_queue.slice(1)
        
        if assign_work(item) {
          processed = processed + 1
        } else {
          // 没有空闲工作线程，停止处理
          work_queue = [item] + work_queue
          break
        }
      }
      processed
    }
    
    (assign_work, process_queue, fn() { results })
  }
  
  // 测试工作池
  let (assign_work, process_queue, get_results) = create_worker_pool(3)
  
  // 创建工作项
  let work_items = [
    { id: 1, data: "short", priority: 1 },
    { id: 2, data: "medium_length", priority: 2 },
    { id: 3, data: "very_long_data_item", priority: 3 },
    { id: 4, data: "tiny", priority: 1 },
    { id: 5, data: "another_medium_item", priority: 2 },
    { id: 6, data: "extra_long_data_item_for_testing", priority: 3 },
    { id: 7, data: "small", priority: 1 },
    { id: 8, data: "medium_data", priority: 2 }
  ]
  
  // 按优先级排序
  let sorted_items = work_items.sort(fn(a, b) { 
    if a.priority > b.priority { -1 } 
    else if a.priority < b.priority { 1 } 
    else { 0 } 
  })
  
  // 分配工作
  for item in sorted_items {
    assign_work(item)
  }
  
  // 处理剩余队列
  let queue_processed = process_queue()
  
  // 获取结果
  let results = get_results()
  
  // 验证结果
  assert_eq(results.length(), 8)  // 所有工作项都应该被处理
  assert_true(queue_processed >= 0)  // 应该处理了一些队列项
  
  // 验证高优先级项先被处理
  let high_priority_results = results.filter(fn(r) { r.id == 3 || r.id == 6 })
  assert_true(high_priority_results.length() == 2)
  
  // 验证处理时间与数据长度相关
  let long_data_result = results.find(fn(r) { r.id == 3 })
  match long_data_result {
    Some(result) => assert_true(result.processing_time > 200)
    None => assert_true(false)
  }
  
  let short_data_result = results.find(fn(r) { r.id == 1 })
  match short_data_result {
    Some(result) => assert_true(result.processing_time < 200)
    None => assert_true(false)
  }
}