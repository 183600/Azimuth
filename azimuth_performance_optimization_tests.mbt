// Azimuth 性能优化测试用例
// 专注于性能优化和资源管理

// 测试1: 内存池管理优化
test "内存池管理优化" {
  // 模拟内存池实现
  let create_memory_pool = fn(initial_size: Int) {
    {
      pool_size: initial_size,
      allocated_blocks: [],
      free_blocks: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].slice(0, initial_size),
      allocation_count: 0,
      deallocation_count: 0
    }
  }
  
  let allocate_block = fn(pool: { pool_size: Int, allocated_blocks: Array[Int], free_blocks: Array[Int], allocation_count: Int, deallocation_count: Int }) {
    if pool.free_blocks.length() > 0 {
      let block_id = pool.free_blocks[0]
      let updated_pool = {
        pool |
        allocated_blocks: pool.allocated_blocks.push(block_id),
        free_blocks: pool.free_blocks.slice(1, pool.free_blocks.length()),
        allocation_count: pool.allocation_count + 1
      }
      (Some(block_id), updated_pool)
    } else {
      (None, pool)
    }
  }
  
  let deallocate_block = fn(pool: { pool_size: Int, allocated_blocks: Array[Int], free_blocks: Array[Int], allocation_count: Int, deallocation_count: Int }, block_id: Int) {
    if pool.allocated_blocks.contains(block_id) {
      let updated_allocated = []
      for id in pool.allocated_blocks {
        if id != block_id {
          updated_allocated = updated_allocated.push(id)
        }
      }
      
      let updated_pool = {
        pool |
        allocated_blocks: updated_allocated,
        free_blocks: pool.free_blocks.push(block_id),
        deallocation_count: pool.deallocation_count + 1
      }
      updated_pool
    } else {
      pool  // 块未分配，返回原池
    }
  }
  
  // 测试内存池分配和释放
  let mut pool = create_memory_pool(10)
  
  // 分配多个块
  let (block1, pool1) = allocate_block(pool)
  let (block2, pool2) = allocate_block(pool1)
  let (block3, pool3) = allocate_block(pool2)
  
  assert_eq(block1, Some(0))
  assert_eq(block2, Some(1))
  assert_eq(block3, Some(2))
  assert_eq(pool3.allocated_blocks.length(), 3)
  assert_eq(pool3.free_blocks.length(), 7)
  assert_eq(pool3.allocation_count, 3)
  
  // 释放一个块
  let pool4 = deallocate_block(pool3, 2)
  assert_eq(pool4.allocated_blocks.length(), 2)
  assert_eq(pool4.free_blocks.length(), 8)
  assert_eq(pool4.deallocation_count, 1)
  assert_true(pool4.free_blocks.contains(2))
  
  // 重新分配应该复用释放的块
  let (block_reused, pool5) = allocate_block(pool4)
  assert_eq(block_reused, Some(2))  // 应该复用刚释放的块
  assert_eq(pool5.allocated_blocks.length(), 3)
  assert_eq(pool5.free_blocks.length(), 7)
  
  // 测试池耗尽情况
  let mut exhausted_pool = create_memory_pool(2)
  let (b1, p1) = allocate_block(exhausted_pool)
  let (b2, p2) = allocate_block(p1)
  let (b3, p3) = allocate_block(p2)  // 应该失败
  
  assert_eq(b1, Some(0))
  assert_eq(b2, Some(1))
  assert_eq(b3, None)  // 池已耗尽
  assert_eq(p3.free_blocks.length(), 0)
}

// 测试2: 缓存策略优化
test "缓存策略优化" {
  // 模拟LRU缓存实现
  let create_lru_cache = fn(capacity: Int) {
    {
      capacity,
      items: [],  // 存储键值对
      order: [],  // 记录访问顺序
      hits: 0,
      misses: 0
    }
  }
  
  let cache_get = fn(cache: { capacity: Int, items: Array[(String, String)>, order: Array[String], hits: Int, misses: Int }, key: String) {
    let mut found = false
    let mut value = ""
    
    // 查找值
    for (k, v) in cache.items {
      if k == key {
        found = true
        value = v
        break
      }
    }
    
    if found {
      // 更新访问顺序
      let mut new_order = []
      for k in cache.order {
        if k != key {
          new_order = new_order.push(k)
        }
      }
      new_order = new_order.push(key)
      
      let updated_cache = {
        cache |
        order: new_order,
        hits: cache.hits + 1
      }
      
      (Some(value), updated_cache)
    } else {
      let updated_cache = { cache | misses: cache.misses + 1 }
      (None, updated_cache)
    }
  }
  
  let cache_put = fn(cache: { capacity: Int, items: Array[(String, String)>, order: Array[String], hits: Int, misses: Int }, key: String, value: String) {
    // 检查是否已存在
    let mut exists = false
    for (k, _) in cache.items {
      if k == key {
        exists = true
        break
      }
    }
    
    if exists {
      // 更新现有值
      let mut new_items = []
      for (k, v) in cache.items {
        if k == key {
          new_items = new_items.push((k, value))
        } else {
          new_items = new_items.push((k, v))
        }
      }
      
      // 更新访问顺序
      let mut new_order = []
      for k in cache.order {
        if k != key {
          new_order = new_order.push(k)
        }
      }
      new_order = new_order.push(key)
      
      {
        cache |
        items: new_items,
        order: new_order
      }
    } else if cache.items.length() < cache.capacity {
      // 有空间，直接添加
      {
        cache |
        items: cache.items.push((key, value)),
        order: cache.order.push(key)
      }
    } else {
      // 缓存已满，移除最久未使用的项
      let lru_key = cache.order[0]
      
      // 移除最久未使用的项
      let mut new_items = []
      for (k, v) in cache.items {
        if k != lru_key {
          new_items = new_items.push((k, v))
        }
      }
      new_items = new_items.push((key, value))
      
      // 更新访问顺序
      let new_order = cache.order.slice(1, cache.order.length()).push(key)
      
      {
        cache |
        items: new_items,
        order: new_order
      }
    }
  }
  
  // 测试LRU缓存
  let mut cache = create_lru_cache(3)
  
  // 添加三个项
  cache = cache_put(cache, "key1", "value1")
  cache = cache_put(cache, "key2", "value2")
  cache = cache_put(cache, "key3", "value3")
  
  assert_eq(cache.items.length(), 3)
  assert_eq(cache.order, ["key1", "key2", "key3"])
  
  // 访问key1，使其成为最近使用的
  let (val1, cache1) = cache_get(cache, "key1")
  assert_eq(val1, Some("value1"))
  assert_eq(cache1.order, ["key2", "key3", "key1"])
  assert_eq(cache1.hits, 1)
  assert_eq(cache1.misses, 0)
  
  // 添加新项，应该移除key2（最久未使用）
  let cache2 = cache_put(cache1, "key4", "value4")
  assert_eq(cache2.items.length(), 3)
  assert_eq(cache2.order, ["key3", "key1", "key4"])
  
  // key2应该已被移除
  let (val2, cache3) = cache_get(cache2, "key2")
  assert_eq(val2, None)
  assert_eq(cache3.misses, 1)
  
  // key1和key3应该仍在
  let (val1_again, _) = cache_get(cache3, "key1")
  let (val3, _) = cache_get(cache3, "key3")
  
  assert_eq(val1_again, Some("value1"))
  assert_eq(val3, Some("value3"))
  
  // 计算命中率
  let hit_rate = cache3.hits.to_float() / (cache3.hits + cache3.misses).to_float()
  assert_true(hit_rate > 0.6)  // 应该有较高的命中率
}

// 测试3: 批处理优化
test "批处理优化" {
  // 模拟批处理操作
  let process_single = fn(item: Int) {
    // 模拟单个处理操作
    item * 2
  }
  
  let process_batch = fn(items: Array[Int>, batch_size: Int) {
    let mut results = []
    let mut i = 0
    
    while i < items.length() {
      let mut batch = []
      let mut j = i
      let batch_end = if i + batch_size < items.length() { i + batch_size } else { items.length() }
      
      while j < batch_end {
        batch = batch.push(items[j])
        j = j + 1
      }
      
      // 处理当前批次
      let mut batch_results = []
      for item in batch {
        batch_results = batch_results.push(process_single(item))
      }
      
      // 将批次结果添加到总结果
      for result in batch_results {
        results = results.push(result)
      }
      
      i = i + batch_size
    }
    
    results
  }
  
  // 测试批处理
  let items = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let batch_size = 3
  
  let batch_results = process_batch(items, batch_size)
  let expected = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
  
  assert_eq(batch_results, expected)
  
  // 测试不同批次大小
  let batch_results_5 = process_batch(items, 5)
  assert_eq(batch_results_5, expected)
  
  let batch_results_10 = process_batch(items, 10)
  assert_eq(batch_results_10, expected)
  
  let batch_results_20 = process_batch(items, 20)  // 批次大小大于数据量
  assert_eq(batch_results_20, expected)
  
  // 测试空数组
  let empty_results = process_batch([], 5)
  assert_eq(empty_results, [])
  
  // 测试性能优化：比较批处理与单个处理
  let simulate_processing_time = fn(item_count: Int, batch_size: Int) {
    // 模拟处理时间：单个处理每个单位时间，批处理每个批次固定时间+每个项目0.1单位时间
    let single_time = item_count * 1.0
    
    let batch_count = (item_count + batch_size - 1) / batch_size  // 向上取整
    let batch_time = batch_count.to_float() * 2.0 + item_count.to_float() * 0.1
    
    (single_time, batch_time)
  }
  
  let (single_time_100, batch_time_100) = simulate_processing_time(100, 10)
  let (single_time_1000, batch_time_1000) = simulate_processing_time(1000, 50)
  
  assert_true(batch_time_100 < single_time_100)  // 批处理应该更快
  assert_true(batch_time_1000 < single_time_1000)  // 大数据集批处理优势更明显
}

// 测试4: 连接池优化
test "连接池优化" {
  // 模拟数据库连接池
  let create_connection_pool = fn(max_connections: Int) {
    {
      max_connections,
      active_connections: [],
      idle_connections: [],
      total_created: 0,
      total_closed: 0
    }
  }
  
  let get_connection = fn(pool: { max_connections: Int, active_connections: Array[String>, idle_connections: Array[String>, total_created: Int, total_closed: Int }) {
    if pool.idle_connections.length() > 0 {
      // 复用空闲连接
      let conn_id = pool.idle_connections[0]
      let updated_pool = {
        pool |
        idle_connections: pool.idle_connections.slice(1, pool.idle_connections.length()),
        active_connections: pool.active_connections.push(conn_id)
      }
      (Some(conn_id), updated_pool)
    } else if pool.active_connections.length() < pool.max_connections {
      // 创建新连接
      let new_conn_id = "conn-" + pool.total_created.to_string()
      let updated_pool = {
        pool |
        active_connections: pool.active_connections.push(new_conn_id),
        total_created: pool.total_created + 1
      }
      (Some(new_conn_id), updated_pool)
    } else {
      // 连接池已满
      (None, pool)
    }
  }
  
  let release_connection = fn(pool: { max_connections: Int, active_connections: Array[String], idle_connections: Array[String], total_created: Int, total_closed: Int }, conn_id: String) {
    if pool.active_connections.contains(conn_id) {
      // 从活动连接中移除
      let mut new_active = []
      for conn in pool.active_connections {
        if conn != conn_id {
          new_active = new_active.push(conn)
        }
      }
      
      // 添加到空闲连接
      let updated_pool = {
        pool |
        active_connections: new_active,
        idle_connections: pool.idle_connections.push(conn_id)
      }
      updated_pool
    } else {
      pool  // 连接不在活动列表中
    }
  }
  
  let close_connection = fn(pool: { max_connections: Int, active_connections: Array[String], idle_connections: Array[String], total_created: Int, total_closed: Int }, conn_id: String) {
    let mut updated_pool = release_connection(pool, conn_id)
    
    if updated_pool.idle_connections.contains(conn_id) {
      // 从空闲连接中移除
      let mut new_idle = []
      for conn in updated_pool.idle_connections {
        if conn != conn_id {
          new_idle = new_idle.push(conn)
        }
      }
      
      updated_pool = {
        updated_pool |
        idle_connections: new_idle,
        total_closed: updated_pool.total_closed + 1
      }
    }
    
    updated_pool
  }
  
  // 测试连接池
  let mut pool = create_connection_pool(5)
  
  // 获取连接
  let (conn1, pool1) = get_connection(pool)
  let (conn2, pool2) = get_connection(pool1)
  let (conn3, pool3) = get_connection(pool2)
  
  assert_eq(conn1, Some("conn-0"))
  assert_eq(conn2, Some("conn-1"))
  assert_eq(conn3, Some("conn-2"))
  assert_eq(pool3.active_connections.length(), 3)
  assert_eq(pool3.idle_connections.length(), 0)
  assert_eq(pool3.total_created, 3)
  
  // 释放连接
  let pool4 = release_connection(pool3, "conn-1")
  assert_eq(pool4.active_connections.length(), 2)
  assert_eq(pool4.idle_connections.length(), 1)
  assert_true(pool4.idle_connections.contains("conn-1"))
  
  // 再次获取连接应该复用刚释放的连接
  let (conn_reused, pool5) = get_connection(pool4)
  assert_eq(conn_reused, Some("conn-1"))  // 应该复用
  assert_eq(pool5.active_connections.length(), 3)
  assert_eq(pool5.idle_connections.length(), 0)
  
  // 测试连接池耗尽
  let mut exhausted_pool = create_connection_pool(2)
  
  // 获取所有连接
  let (c1, p1) = get_connection(exhausted_pool)
  let (c2, p2) = get_connection(p1)
  let (c3, p3) = get_connection(p2)  // 应该失败
  
  assert_eq(c1, Some("conn-0"))
  assert_eq(c2, Some("conn-1"))
  assert_eq(c3, None)  // 连接池已满
  assert_eq(p3.active_connections.length(), 2)
  
  // 释放一个连接后再获取
  let p4 = release_connection(p3, "conn-0")
  let (c4, p5) = get_connection(p4)
  
  assert_eq(c4, Some("conn-0"))  // 复用连接
  assert_eq(p5.active_connections.length(), 2)
  
  // 测试连接关闭
  let p6 = close_connection(p5, "conn-1")
  assert_eq(p6.active_connections.length(), 1)
  assert_eq(p6.idle_connections.length(), 0)
  assert_eq(p6.total_closed, 1)
}

// 测试5: 数据压缩优化
test "数据压缩优化" {
  // 模拟简单数据压缩算法（行程编码）
  let compress_rle = fn(data: Array[Int]) {
    if data.length() == 0 {
      return []
    }
    
    let mut compressed = []
    let mut current = data[0]
    let mut count = 1
    
    for i in 1..data.length() {
      if data[i] == current {
        count = count + 1
      } else {
        compressed = compressed.push((current, count))
        current = data[i]
        count = 1
      }
    }
    
    // 添加最后一组
    compressed = compressed.push((current, count))
    compressed
  }
  
  let decompress_rle = fn(compressed: Array[(Int, Int)>) {
    let mut decompressed = []
    
    for (value, count) in compressed {
      let mut i = 0
      while i < count {
        decompressed = decompressed.push(value)
        i = i + 1
      }
    }
    
    decompressed
  }
  
  // 测试压缩和解压缩
  let data1 = [1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 5, 5]
  let compressed1 = compress_rle(data1)
  let decompressed1 = decompress_rle(compressed1)
  
  assert_eq(compressed1, [(1, 3), (2, 2), (3, 4), (4, 1), (5, 2)])
  assert_eq(decompressed1, data1)
  
  // 测试无重复数据
  let data2 = [1, 2, 3, 4, 5]
  let compressed2 = compress_rle(data2)
  let decompressed2 = decompress_rle(compressed2)
  
  assert_eq(compressed2, [(1, 1), (2, 1), (3, 1), (4, 1), (5, 1)])
  assert_eq(decompressed2, data2)
  
  // 测试空数组
  let data3 = []
  let compressed3 = compress_rle(data3)
  let decompressed3 = decompress_rle(compressed3)
  
  assert_eq(compressed3, [])
  assert_eq(decompressed3, [])
  
  // 测试单一值数组
  let data4 = [7, 7, 7, 7, 7]
  let compressed4 = compress_rle(data4)
  let decompressed4 = decompress_rle(compressed4)
  
  assert_eq(compressed4, [(7, 5)])
  assert_eq(decompressed4, data4)
  
  // 计算压缩率
  let calculate_compression_ratio = fn(original: Array[Int], compressed: Array[(Int, Int)>) {
    let original_size = original.length()
    let compressed_size = compressed.length() * 2  // 每个压缩项有值和计数
    
    if original_size == 0 {
      1.0
    } else {
      compressed_size.to_float() / original_size.to_float()
    }
  }
  
  let ratio1 = calculate_compression_ratio(data1, compressed1)
  let ratio2 = calculate_compression_ratio(data2, compressed2)
  let ratio4 = calculate_compression_ratio(data4, compressed4)
  
  assert_true(ratio1 < 1.0)  // 数据1应该有压缩效果
  assert_true(ratio2 > 1.0)  // 数据2压缩后可能更大
  assert_true(ratio4 < 1.0)  // 数据4应该有显著压缩效果
  
  // 测试字典压缩
  let create_dictionary = fn(data: Array[String>) {
    let mut dictionary = []
    let mut index = 0
    
    for item in data {
      if not(dictionary.contains(item)) {
        dictionary = dictionary.push(item)
        index = index + 1
      }
    }
    
    dictionary
  }
  
  let compress_with_dictionary = fn(data: Array[String>, dictionary: Array[String>) {
    let mut compressed = []
    
    for item in data {
      let mut found_index = -1
      for i in 0..dictionary.length() {
        if dictionary[i] == item {
          found_index = i
          break
        }
      }
      
      if found_index >= 0 {
        compressed = compressed.push(found_index)
      }
    }
    
    compressed
  }
  
  let decompress_with_dictionary = fn(compressed: Array[Int>, dictionary: Array[String>) {
    let mut decompressed = []
    
    for index in compressed {
      if index >= 0 && index < dictionary.length() {
        decompressed = decompressed.push(dictionary[index])
      }
    }
    
    decompressed
  }
  
  // 测试字典压缩
  let string_data = ["error", "warning", "info", "error", "error", "warning", "debug", "info"]
  let dictionary = create_dictionary(string_data)
  let compressed_dict = compress_with_dictionary(string_data, dictionary)
  let decompressed_dict = decompress_with_dictionary(compressed_dict, dictionary)
  
  assert_eq(dictionary, ["error", "warning", "info", "debug"])
  assert_eq(compressed_dict, [0, 1, 2, 0, 0, 1, 3, 2])
  assert_eq(decompressed_dict, string_data)
  
  // 计算字典压缩率
  let original_string_size = string_data.reduce(fn(acc, s) { acc + s.length() }, 0)
  let dictionary_size = dictionary.reduce(fn(acc, s) { acc + s.length() }, 0)
  let compressed_dict_size = compressed_dict.length()  // 每个索引占1个单位
  
  let dict_compression_ratio = (dictionary_size + compressed_dict_size).to_float() / original_string_size.to_float()
  assert_true(dict_compression_ratio < 1.0)  // 字典压缩应该有效果
}